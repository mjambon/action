type seq = private
    Atom of string
  | Seq of (action * action)

(* Main type *)
and action = private {
  ac_id : int; (* unique node ID *)
  ac_seq : seq; (* node or leaf *)
  ac_score : int ref; (* score based on global feedback *)
  ac_lowest_score : int ref; (* negative *)
  ac_boost_count : int ref; (* number of times it got positive feedback *)
  ac_length : int; (* precomputed sequence length *)
  ac_hash : int; (* precomputed sequence hash *)
  ac_exec : (action -> unit);
    (* function that actually performs the action
       (ideally the same function for all actions) *)
}

(* Constructors *)
val atom : string -> (action -> unit) -> action
val seq : ?exec:(action -> unit) -> action -> action -> action
  (* reuses exec function from first argument if not specified *)

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
