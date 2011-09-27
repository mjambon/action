type tristate

type view = Positive | Zero | Negative

val boost : t -> int -> unit
val next : t -> unit
val view : t -> view
