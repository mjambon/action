type tristate

type view = Positive | Zero | Negative

val boost : tristate -> int -> unit
val next : tristate -> unit
val view : tristate -> view
