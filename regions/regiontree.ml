open Printf
open Log

type region_id = int

module type In =
sig
  type element_contents
  type region_contents
end

module type Out =
sig
  type element_contents
  type region_contents

  type region = {
    id: region_id;
    children: (region * region) option;
    data: region_contents;
  }

  type t
  type elt

  val create : 
    diffusion_period: int ->
    fraction_diffused: float ->
    post_move: (region -> region -> elt -> unit) ->
    region -> t

  val elt_contents : elt -> element_contents
  val elt_score : elt -> float

  val add_element : t -> region_id -> float -> element_contents -> elt
    (** Create an element and add it to the tree.
        Once added, an element cannot be removed from the tree. *)

  val update_score : t -> elt -> float -> unit
    (** Update the score of an element. *)

  val iter : t -> (region -> unit) -> unit
  val fold : t -> 'acc -> (region -> 'acc -> 'acc) -> 'acc
end

module Make (Param : In) : Out
  with type element_contents = Param.element_contents
  and type region_contents = Param.region_contents =
struct
  type element_contents = Param.element_contents
  type region_contents = Param.region_contents

  type region = {
    id: region_id;
    children: (region * region) option;
    data: region_contents;
  }

  type element_id = int

  type elt = {
    elt_id : element_id;
    elt_contents : element_contents;
    mutable elt_region : node;
    mutable elt_path : region_id list;
    mutable elt_prio : float;
  }

  and node = {
    region : region;
    mutable parent : node option;
    node_children : (node * node) option;
    mutable up_queue : elt Prioqueue.t;
    mutable left_queue : elt Prioqueue.t;
    mutable right_queue : elt Prioqueue.t;
    mutable incoming : (region * elt) list;
    mutable up_remainder : float;
    mutable left_remainder : float;
    mutable right_remainder : float;
    mutable region_element_count : int;
  }

  type t = {
    mutable ticker : int;
    diffusion_period : int;
    fraction_diffused : float;
    total_element_count : int;
    root : node;
    mutable post_move : (region -> region -> elt -> unit);
    mutable element_counter : int;
    region_table : (region_id, node) Hashtbl.t;
  }

  let elt_contents x = x.elt_contents
  let elt_score x = x.elt_prio

  let rec map_region_to_node x =
    let node_children =
      match x.children with
          None -> None
        | Some (a, b) -> Some (map_region_to_node a, map_region_to_node b)
    in
    let node = {
      region = x;
      parent = None;
      node_children;
      up_queue = Prioqueue.empty;
      left_queue = Prioqueue.empty;
      right_queue = Prioqueue.empty;
      incoming = [];
      up_remainder = 0.;
      left_remainder = 0.;
      right_remainder = 0.;
      region_element_count = 0;
    }
    in
    (match node_children with
         None -> ()
       | Some (a, b) ->
           a.parent <- Some node;
           b.parent <- Some node);
    node

  let rec fold_nodes node acc f =
    let acc = f node acc in
    match node.node_children with
        None -> acc
      | Some (a, b) ->
          let acc = fold_nodes a acc f in
          fold_nodes b acc f

  let rec iter_nodes node f =
    f node;
    match node.node_children with
        None -> ()
      | Some (a, b) ->
          iter_nodes a f;
          iter_nodes b f

  let fold root acc f =
    fold_nodes root.root acc (fun node acc -> f node.region acc)

  let rec iter root f =
    iter_nodes root.root (fun node -> f node.region)

  let create 
      ~diffusion_period
      ~fraction_diffused
      ~post_move
      root_region =
    assert (diffusion_period >= 1);
    assert (fraction_diffused >= 0.);
    assert (fraction_diffused < 0.333334);
    let root_node = map_region_to_node root_region in
    let region_count = fold_nodes root_node 0 (fun _ n -> n + 1) in
    let region_table = Hashtbl.create (2 * region_count) in
    iter_nodes root_node (
      fun node ->
        let id = node.region.id in
        assert (not (Hashtbl.mem region_table id));
        Hashtbl.add region_table id node
    );
    {
      ticker = 0;
      diffusion_period;
      fraction_diffused;
      total_element_count = 0;
      root = root_node;
      post_move;
      element_counter = 0;
      region_table;
    }

  let rec take_top acc q n f =
    assert (n >= 0);
    if n = 0 then (acc, q)
    else
      match Prioqueue.pop_max q with
          None -> (acc, q)
        | Some (prio, id, x, q) -> take_top (f prio x :: acc) q (n - 1) f

  let rec take_bottom acc q n f =
    assert (n >= 0);
    if n = 0 then (acc, q)
    else
      match Prioqueue.pop_min q with
          None -> (acc, q)
        | Some (prio, id, x, q) -> take_bottom (f prio x :: acc) q (n - 1) f

  (* move top/bottom items to parent/child *)
  let move_out root node =
    let base = root.fraction_diffused *. float node.region_element_count in

    (* maximum number of elements to move out of each gate (up/left/right) *)
    let up_quota = base +. node.up_remainder in
    let left_quota = base +. node.left_remainder in
    let right_quota = base +. node.right_remainder in

    (* up *)
    begin
      match node.parent with
          None -> ()
        | Some p ->
            let n = truncate up_quota in
            let acc, q =
              take_top p.incoming node.up_queue n
                (fun prio x ->
                   node.region_element_count <- node.region_element_count - 1;
                   assert (node.region_element_count >= 0);

                   node.left_queue <-
                     Prioqueue.remove prio x.elt_id node.left_queue;
                   node.right_queue <-
                     Prioqueue.remove prio x.elt_id node.right_queue;

                   (node.region, x)
                )
            in
            p.incoming <- acc;
            node.up_queue <- q;
            node.up_remainder <- up_quota -. float n
    end;

    (* left *)
    begin
      match node.node_children with
          None -> ()
        | Some (left, _) ->
            let n = truncate left_quota in
            let acc, q =
              take_bottom left.incoming node.left_queue n
                (fun prio x ->
                   node.region_element_count <- node.region_element_count - 1;
                   assert (node.region_element_count >= 0);

                   node.up_queue <-
                     Prioqueue.remove prio x.elt_id node.up_queue;
                   node.right_queue <-
                     Prioqueue.remove prio x.elt_id node.right_queue;
                   (node.region, x)
                )
            in
            left.incoming <- acc;
            node.left_queue <- q;
            node.left_remainder <- left_quota -. float n
    end;

    (* right *)
    begin
      match node.node_children with
          None -> ()
        | Some (_, right) ->
            let n = truncate right_quota in
            let acc, q =
              take_bottom right.incoming node.right_queue n
                (fun prio x ->
                   node.region_element_count <- node.region_element_count - 1;
                   assert (node.region_element_count >= 0);

                   node.up_queue <-
                     Prioqueue.remove prio x.elt_id node.up_queue;
                   node.left_queue <-
                     Prioqueue.remove prio x.elt_id node.left_queue;
                   (node.region, x)
                )
            in
            right.incoming <- acc;
            node.right_queue <- q;
            node.right_remainder <- right_quota -. float n
    end

  let add_element_to_region x =
    (* note: elt_region and elt_path must be already up-to-date *)
    let node = x.elt_region in

    (* put element into the up queue if parent exists *)
    (match node.parent with
         None -> ()
       | Some _ ->
           node.up_queue <-
             Prioqueue.add x.elt_prio x.elt_id x node.up_queue
    );
    (* put element into left or right queue if path allows *)
    (match x.elt_path with
         [] -> ()
       | child_id :: _ ->
           match node.node_children with
               None -> assert false
             | Some (left, right) ->
                 if left.region.id = child_id then
                   node.left_queue <-
                     Prioqueue.add x.elt_prio x.elt_id x node.left_queue
                 else (
                   assert (right.region.id = child_id);
                   node.right_queue <-
                     Prioqueue.add x.elt_prio x.elt_id x node.right_queue
                 )
    );
    node.region_element_count <- node.region_element_count + 1


  (* put incoming into prioqueue *)
  let move_in root node =
    let region_id = node.region.id in
    List.iter (
      fun (from_region, x) ->
        (* run hook safely *)
        (try root.post_move from_region node.region x
         with e -> log_exn e);
        (* update element path *)
        let updated_path =
           match x.elt_path with
               id :: tl when id = region_id -> (* pop *) tl
             | l -> (* push *) from_region.id :: l
        in
        x.elt_region <- node;
        x.elt_path <- updated_path;
        add_element_to_region x

    ) node.incoming;
    node.incoming <- []

  let tick x =
    x.ticker <- (x.ticker + 1) mod x.diffusion_period;
    if x.ticker = 0 then (
      iter_nodes x.root (move_out x);
      iter_nodes x.root (move_in x);
    )

  let add_element root region_id elt_prio elt_contents =
    tick root;
    let elt_id = root.element_counter in
    root.element_counter <- root.element_counter + 1;
    let node =
      try Hashtbl.find root.region_table region_id
      with Not_found -> failwith (sprintf "No such region ID: %i" region_id)
    in
    let elt = {
      elt_id;
      elt_contents;
      elt_region = node;
      elt_path = [];
      elt_prio;
    }
    in
    add_element_to_region elt;
    elt

  let update_score root elt prio =
    tick root;
    let id = elt.elt_id in
    let prio0 = elt.elt_prio in
    let node = elt.elt_region in
    node.up_queue <- Prioqueue.remove prio0 id node.up_queue;
    node.left_queue <- Prioqueue.remove prio0 id node.left_queue;
    node.right_queue <- Prioqueue.remove prio0 id node.right_queue;
    elt.elt_prio <- prio;
    add_element_to_region elt
end

let test () =
  let module T = Make (
    struct
      type element_contents = int
      type region_contents = (int, float) Hashtbl.t
    end
  )
  in
  let r000 = {
    T.id = 3;
    children = None;
    data = Hashtbl.create 100;
  }
  in
  let r001 = {
    T.id = 4;
    children = None;
    data = Hashtbl.create 100;
  }
  in
  let r010 = {
    T.id = 5;
    children = None;
    data = Hashtbl.create 100;
  }
  in
  let r011 = {
    T.id = 6;
    children = None;
    data = Hashtbl.create 100;
  }
  in
  let r00 = {
    T.id = 1;
    children = Some (r000, r001);
    data = Hashtbl.create 100;
  }
  in
  let r01 = {
    T.id = 2;
    children = Some (r010, r011);
    data = Hashtbl.create 100;
  }
  in
  let r0 = {
    T.id = 0;
    children = Some (r00, r01);
    data = Hashtbl.create 100;
  }
  in
  let remove region x =
    let tbl = region.T.data in
    assert (Hashtbl.mem tbl x);
    Hashtbl.remove tbl x
  in
  let add region x score =
    let tbl = region.T.data in
    assert (not (Hashtbl.mem tbl x));
    Hashtbl.add tbl x score
  in
  let post_move src dst elt =
    let x = T.elt_contents elt in
    let score = T.elt_score elt in
    printf "elt:%i:%g src:%i -> dst:%i\n%!" x score src.T.id dst.T.id;
    remove src x;
    add dst x score
  in

  let t =
    T.create
      ~diffusion_period: 10
      ~fraction_diffused: 0.05
      ~post_move
      r0
  in
  
  for i = 1 to 1000 do
    let region =
      match i mod 4 with
          0 -> r000
        | 1 -> r001
        | 2 -> r010
        | 3 -> r011
        | _ -> assert false
    in
    let score = mod_float (float i /. 10.) 1. in
    printf "add elt:%i:%g\n%!" i score;
    add region i score;
    ignore (T.add_element t region.T.id score i)
  done;

  let all =
    T.fold t [] (
      fun r acc ->
        let l = Hashtbl.fold (fun k v acc -> (k, v) :: acc) r.T.data [] in
        let sum = List.fold_left (fun acc (_, x) -> acc +. x) 0. l in
        let n = List.length l in
        let mean = sum /. float n in
        printf "region:%i n:%i mean:%g\n" r.T.id n mean;
        acc @ l
    )
  in
  assert (List.length all = 1000)
