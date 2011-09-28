open Printf

type tristate = {
  mutable value : int;
  ceiling : int;
  floor : int;
}
    (* Defines how much time is left in that state.

       Each clock tick brings the value closer to zero by 1.
    *)

type view = Positive | Zero | Negative

let create ceiling =
  if ceiling <= 0 then
    invalid_arg (sprintf "Tristate.create %i" ceiling)
  else
    {
      value = 0;
      ceiling = ceiling;
      floor = -ceiling;
    }

let boost x n =
  let old = x.value in
  let new_ = old + n in
  if n >= 0 then (
    if new_ > x.ceiling || new_ < old (* overflow *) then
      x.value <- x.ceiling
    else
      x.value <- new_
  )
  else (
    if new_ < x.floor || new_ > old (* overflow *) then
      x.value <- x.floor
    else
      x.value <- new_
  )

let next x =
  let old = x.value in
  if old < 0 then
    x.value <- old + 1
  else if old > 0 then
    x.value <- old - 1

let view x =
  let v = x.value in
  if v < 0 then Negative
  else if v > 0 then Positive
  else Zero
