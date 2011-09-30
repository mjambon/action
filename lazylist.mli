type 'a lazy_list = 'a forced_list lazy_t

and 'a forced_list = Cell of 'a * 'a lazy_list | Empty

val empty : 'a lazy_list

val compare : ('a -> 'a -> int) -> 'a lazy_list -> 'a lazy_list -> int

val hash : ('a -> int) -> 'a lazy_list -> int
