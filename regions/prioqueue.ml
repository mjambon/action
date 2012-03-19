type id = int

module M =
  Map.Make (
    struct
      type t = float * id
      let compare (prio1, id1) (prio2, id2) =
        let c = compare (prio1 : float) prio2 in
        if c <> 0 then c
        else
          compare (id1 : int) id2
    end
  )

type 'a t = 'a M.t

let empty = M.empty

let is_normal x =
  match classify_float x with
      FP_normal
    | FP_zero -> true
    | FP_subnormal
    | FP_infinite
    | FP_nan -> false

let add prio id v m =
  assert (is_normal prio);
  M.add (prio, id) v m

let pop_min m =
  try
    let ((prio, id) as k), v = M.min_binding m in
    Some (prio, id, v, M.remove k m)
  with Not_found -> None

let pop_max m =
  try
    let ((prio, id) as k), v = M.max_binding m in
    Some (prio, id, v, M.remove k m)
  with Not_found -> None

let remove prio id m =
  M.remove (prio, id) m

let test () =
  let m = empty in
  assert (pop_min m = None);
  assert (pop_max m = None);
  let m = add 1. 1 1 m in
  let m = add 2. 2 2 m in
  let m = add 2. 3 3 m in
  assert (match pop_min m with Some (1., 1, 1, _) -> true | _ -> false);
  assert (match pop_max m with Some (2., _, _, _) -> true | _ -> false);
  let m =
    match pop_max m with
        Some (2., _, _, m) -> m
      | _ -> assert false
  in
  let m =
    match pop_min m with
        Some (1., _, _, m) -> m
      | _ -> assert false
  in
  assert (pop_min m = pop_max m);
  let m =
    match pop_min m with
        Some (2., _, _, m) -> m
      | _ -> assert false
  in
  assert (pop_min m = None);
  assert (pop_max m = None) 
