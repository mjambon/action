type seq = private
    Atom of (string * (unit -> unit))
  | Seq of (action * action)

(* Main type *)
and action = private {
  ac_id : int; (* unique node ID *)
  ac_seq : seq; (* node or leaf *)
  ac_length : int; (* precomputed sequence length *)
  ac_hash : int; (* precomputed sequence hash *)
}

(* Constructors *)
val atom : string -> (unit -> unit) -> action
val seq : action -> action -> action

(* Iterators *)
val fold_left : ('a -> string -> 'a) -> 'a -> action -> 'a
val fold_right : (string -> 'a -> 'a) -> action -> 'a -> 'a
val flatten : action -> string list
val iter : (string -> unit) -> action -> unit

(* Comparison and hashing *)
val compare : action -> action -> int
val hash : action -> int
val equal : action -> action -> bool

(* Printing and debugging *)
val print_seq : action -> unit
val test : unit -> unit
