module M =
  Map.Make (
    struct
      type t = float
      let compare = compare
    end
  )

type 'a t = 'a list M.t

let empty = M.empty

let is_normal x =
  match classify_float x with
      FP_normal
    | FP_zero -> true
    | FP_subnormal
    | FP_infinite
    | FP_nan -> false

let add k v m =
  assert (is_normal k);
  try
    let l = M.find k m in
    M.add k (v :: l) m
  with Not_found ->
    M.add k [v] m

let pop_min m =
  try
    let k, l = M.min_binding m in
    match l with
        [] -> assert false
      | [v] -> Some (k, v, M.remove k m)
      | v :: l -> Some (k, v, M.add k l m)
  with Not_found -> None

let pop_max m =
  try
    let k, l = M.max_binding m in
    match l with
        [] -> assert false
      | [v] -> Some (k, v, M.remove k m)
      | v :: l -> Some (k, v, M.add k l m)
  with Not_found -> None

let test () =
  let m = empty in
  assert (pop_min m = None);
  assert (pop_max m = None);
  let m = add 1. 1 m in
  let m = add 2. 2 m in
  let m = add 2. 3 m in
  assert (match pop_min m with Some (1., 1, _) -> true | _ -> false);
  assert (match pop_max m with Some (2., 3, _) -> true | _ -> false);
  let m =
    match pop_max m with
        Some (2., 3, m) -> m
      | _ -> assert false
  in
  let m =
    match pop_min m with
        Some (1., 1, m) -> m
      | _ -> assert false
  in
  assert (pop_min m = pop_max m);
  let m =
    match pop_min m with
        Some (2., 2, m) -> m
      | _ -> assert false
  in
  assert (pop_min m = None);
  assert (pop_max m = None) 
