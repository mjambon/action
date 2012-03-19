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

  val add_element : t -> region_id -> element_contents -> float -> elt
    (** Create an element and add it to the tree.
        Once added, an element cannot be removed from the tree. *)

  val update_score : elt -> float -> unit
    (** Update the score of an element,
        possibly moving it to another region. *)

  val iter_region : t -> region_id -> (elt -> unit) -> unit
  val fold_region : region -> 'acc -> (elt -> 'acc -> 'acc) -> 'acc
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

  type elt = {
    elt_contents : element_contents;
    mutable elt_path : region_id list;
  }

  type node = {
    region : region;
    node_children : (node * node) option;
    mutable up_queue : elt Prioqueue.t;
    mutable left_queue : elt Prioqueue.t;
    mutable right_queue : elt Prioqueue.t;
    mutable bottom_queue : elt Prioqueue.t;
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
  }

  let rec map_region_to_node x =
    let node_children =
      match x.children with
          None -> None
        | Some (a, b) -> Some (map_region_to_node a, map_region_to_node b)
    in
    {
      region = x;
      node_children;
      prioqueue = Prioqueue.empty;
      bottom = Prioqueue.empty;
      incoming = [];
      up_remainder = 0.;
      down_remainder = 0.;
    }

  let create 
      ~diffusion_period
      ~fraction_diffused
      ~post_move
      root_region =
    assert (diffusion_period >= 1);
    assert (fraction_diffused >= 0.);
    assert (fraction_diffused <= 1.);
    let root_node = map_region_to_node root_region in
    {
      ticker = 0;
      diffusion_period;
      fraction_diffused;
      total_element_count = 0;
      root = root_node;
      post_move;
    }

  (* move top/bottom items to parent/child *)
  let move_out root parent node =
    let base = root.fraction_diffused *. float node.region_element_count in

    (* maximum number of elements to move out of each gate (up/left/right) *)
    let up_quota = base +. node.up_remainder in
    let left_quota = base +. node.left_remainder in
    let right_quota = base +. node.right_remainder in

    (* up *)
    (match parent with
         None -> ()
       | Some p ->
           ...
    )

  (* put incoming into prioqueue *)
  let move_in root parent node = failwith "not implemented"

  let rec iter parent node f =
    f parent node;
    match node.node_children with
        None -> ()
      | Some (a, b) ->
          let new_parent = Some node in
          iter new_parent a f;
          iter new_parent b f

  let tick x =
    x.ticker <- (x.ticker + 1) mod x.diffusion_period;
    if x.ticker = 0 then (
      iter None x.root (move_out x);
      iter None x.root (move_in x);
    )
end
