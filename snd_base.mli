val base : Seq.action list
  (*
    Predefined base actions,
    which either add a note to the end of the current sequence
    or apply a special effect to the current sequence.
  *)

val exec : Seq.action -> unit
  (*
    Execute the given action, stored as a tree.
  *)
