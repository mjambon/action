(**
  Multi-timer
*)

type 'a timer

val create : unit -> 'a timer
val count : 'a timer -> int
  (** Return the number of items in constant time *)

val put : 'a timer -> int -> 'a -> unit
  (** [put timer t x] puts an item [x] into [timer].
      Element [x] will expire and be removed after exactly [t] ticks.
  *)

val tick : 'a timer -> 'a list
  (** A clock tick.
      All elements that expire are removed from the timer and returned.
  *)
