type ('a, 'b) picker

val create : int -> ('a, 'b) picker
val get : ('a, 'b) picker -> 'a -> 'b option
val remove : ('a, 'b) picker -> 'a -> unit
val replace : ('a, 'b) picker -> 'a -> 'b -> unit
val pick : ('a, 'b) picker -> ('a * 'b) option
