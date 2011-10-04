open Printf

module Z = Lazylist

type seq =
    Atom of (string * (unit -> unit))
  | Seq of (action * action)

and action = {
  ac_id : int;
  ac_seq : seq;
  ac_length : int;
  ac_hash : int;
}

type action_base = action array

let rec fold_left f acc x =
  match x.ac_seq with
      Atom (s, _) -> f acc s
    | Seq (a, b) ->
        let acc = fold_left f acc a in
        fold_left f acc b

let rec fold_right f x acc =
  match x.ac_seq with
      Atom (s, _) -> f s acc
    | Seq (a, b) ->
        let acc = fold_right f b acc in
        fold_right f a acc


let flatten x = fold_right (fun s acc -> s :: acc) x []

let iter f x = fold_left (fun () s -> f s) () x

let rec to_lazylist_aux x tail =
  match x.ac_seq with
      Atom (s, _) -> Z.Cell (s, tail)
    | Seq (a, b) ->
        let tail = lazy (to_lazylist_aux b tail) in
        to_lazylist_aux a tail

let to_lazylist x = lazy (to_lazylist_aux x Z.empty)

let compare a b =
  Lazylist.compare String.compare (to_lazylist a) (to_lazylist b)

let really_hash x =
  Lazylist.hash Hashtbl.hash (to_lazylist x)

let hash x = x.ac_hash

let equal a b =
  a.ac_hash = b.ac_hash && compare a b = 0

let unique_id =
  let n = ref 0 in
  fun () ->
    let i = !n in
    if i < 0 then
      failwith "Seq.unique_id overflow"
    else (
      incr n;
      i
    )

let atom s f =
  let incomplete = {
    ac_id = unique_id ();
    ac_seq = Atom (s, f);
    ac_length = 1;
    ac_hash = 0;
  }
  in
  { incomplete with ac_hash = really_hash incomplete }

let seq a b =
  let incomplete = {
    ac_id = unique_id ();
    ac_seq = Seq (a, b);
    ac_length = a.ac_length + b.ac_length;
    ac_hash = 0;
  }
  in
  { incomplete with ac_hash = really_hash incomplete }


let print_seq x =
  let l = flatten x in
  print_string (Yojson.Safe.prettify (Seq_j.string_of_string_list l))

let test () =
  let mk name =
    atom name (fun () -> printf "%s\n" name)
  in
  let a = [| mk "up"; mk "down"; mk "left"; mk "right"; mk "nop" |] in
  let x = seq (seq a.(0) a.(0)) a.(1) in
  let y = seq x x in
  print_seq y;
  print_newline ();

  let all = Picker.create 100 in
  List.iter (fun x -> Picker.replace all x.ac_id x)
    ([ x; y ] @ Array.to_list a);
  for i = 1 to 20 do
    printf "[%i] " i;
    match Picker.pick all with
        None -> assert false
      | Some (k, v) ->
          printf "#%i " k; print_seq v;
          print_string "\n";
  done
