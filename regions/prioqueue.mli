(* Functional priority queue *)

type 'a t
val empty : 'a t
val add : float -> 'a -> 'a t -> 'a t
val pop_min : 'a t -> (float * 'a * 'a t) option
val pop_max : 'a t -> (float * 'a * 'a t) option
