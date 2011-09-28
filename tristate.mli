type tristate

type view = Positive | Zero | Negative

val create : int -> tristate
  (* [create ceiling] creates an initial state of 0.
     The argument is the ceiling (inclusive upper bound).
     The floor is its opposite (inclusive lower bound). *)

val boost : tristate -> int -> unit
  (* Add positive or negative value to the internal state but stay
     within the bounds imposed by floor and ceiling. *)

val next : tristate -> unit
  (* Increment or decrement the state value to bring it closer to 0. *)

val view : tristate -> view
  (* Return whether the state value is positive, null or negative. *)
