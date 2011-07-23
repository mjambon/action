type slice = {
  offset : int;
  votes : float array;
}


let create len = Array.make len 0.0

let clear a =
  for i = 0 to Array.length a - 1 do
    a.(i) <- 0.0
  done

let vote all {offset; votes} =
  assert (offset >= 0);
  assert (offset + Array.length votes <= Array.length all);
  Array.iteri (
    fun i x ->
      assert (x >= 0. && x <= 1.);
      let j = offset + i in
      all.(j) <- all.(j) +. x
  ) votes

let choose all =
  assert (Array.length all >= 1);
  let best = ref all.(0) in
  let pos = ref 0 in
  for i = 1 to Array.length all - 1 do
    let x = all.(i) in
    if x > !best then (
      pos := i;
      best := x
    )
  done;
  !pos
