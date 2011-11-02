type ('a, 'b) picker

val create : int -> ('a, 'b) picker
val get : ('a, 'b) picker -> 'a -> 'b option
val mem : ('a, 'b) picker -> 'a -> bool
val remove : ('a, 'b) picker -> 'a -> unit
val replace : ('a, 'b) picker -> 'a -> 'b -> unit
val pick : ('a, 'b) picker -> ('a * 'b) option

val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) picker -> 'c -> 'c
val iter : ('a -> 'b -> unit) -> ('a, 'b) picker -> unit
