(* Functional priority queue *)

type id = int

type 'a t
val empty : 'a t
val add : float -> id -> 'a -> 'a t -> 'a t
val remove : float -> id -> 'a t -> 'a t
val pop_min : 'a t -> (float * id * 'a * 'a t) option
val pop_max : 'a t -> (float * id * 'a * 'a t) option
