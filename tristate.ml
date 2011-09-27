type tristate = int ref
    (* Defines how much time is left in that state.

       Each clock tick brings the value closer to zero by 1.
    *)

type view = Positive | Zero | Negative

let boost x n =
  x := !x + n

let next x =
  if !x < 0 then
    incr x
  else if !x > 0 then
    decr x

let view x =
  if !x < 0 then Negative
  else if !x > 0 then Positive
  else Zero
