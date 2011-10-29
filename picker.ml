open Printf

type 'a ext_array = {
  mutable ext_ar : 'a option array;
  mutable ext_count : int;
}

let ext_create len = {
  ext_ar = Array.make len None;
  ext_count = 0;
}

let ext_add ext x =
  let pos = ext.ext_count in
  let len = Array.length ext.ext_ar in
  if pos >= len then (
    let new_len = min Sys.max_array_length (max (2 * len) 1) in
    if new_len <= len then
      failwith "Picker.ext_add: array is full";
    let a = Array.make new_len None in
    Array.blit ext.ext_ar 0 a 0 ext.ext_count;
    ext.ext_ar <- a;
  );
  ext.ext_ar.(pos) <- Some x;
  ext.ext_count <- pos + 1;
  pos

let ext_remove ext i =
  let n = ext.ext_count in
  if i < 0 || i >= n then
    invalid_arg (sprintf "Picker.ext_remove %i" i)
  else (
    ext.ext_count <- n - 1;
    if i = n - 1 then
      ext.ext_ar.(i) <- None
    else (
      let last = ext.ext_ar.(n-1) in
      ext.ext_ar.(i) <- last;
      ext.ext_ar.(n-1) <- None
    )
  )

type ('a, 'b) picker = {
  p_ext : ('a * 'b) ext_array;
  p_tbl : ('a, ('b * int)) Hashtbl.t;
}

let create len = {
  p_ext = ext_create len;
  p_tbl = Hashtbl.create (2 * len);
}

let get p k =
  try Some (fst (Hashtbl.find p.p_tbl k))
  with Not_found -> None

let remove p k =
  try
    let v, i = Hashtbl.find p.p_tbl k in
    ext_remove p.p_ext i;
    Hashtbl.remove p.p_tbl k;
    match p.p_ext.ext_ar.(i) (* legal only if array never shrinks *) with
        None -> ()
      | Some (k', v') ->
          Hashtbl.replace p.p_tbl k' (v', i)
  with Not_found ->
    ()

let replace p k v =
  try
    let old_v, i = Hashtbl.find p.p_tbl k in
    p.p_ext.ext_ar.(i) <- Some (k, v);
    Hashtbl.replace p.p_tbl k (v, i)
  with Not_found ->
    let i = ext_add p.p_ext (k, v) in
    Hashtbl.add p.p_tbl k (v, i)

let pick p =
  let a = p.p_ext.ext_ar in
  let n = p.p_ext.ext_count in
  if n = 0 then
    None
  else
    a.(Random.int n)

let fold f p acc =
  let a = p.p_ext.ext_ar in
  let len = p.p_ext.ext_count in
  assert (len <= Array.length a);
  let acc = ref acc in
  for i = 0 to len - 1 do
    match a.(i) with
        None -> assert false
      | Some (k, v) -> acc := f k v !acc
  done;
  !acc

let iter f p =
  fold (fun k v acc -> f k v) p ()


let test () =
  let p = create 0 in
  assert (pick p = None);
  replace p "a" 1;
  assert (pick p = Some ("a", 1));
  replace p "b" 2;
  for i = 1 to 20; do
    ignore (pick p)
  done;
  replace p "a" 3;
  iter (fun s n -> printf "(%S, %i)\n" s n) p;
  for i = 1 to 20 do
    match pick p with
        Some ("a", 3) -> ()
      | Some ("b", 2) -> ()
      | _ -> assert false
  done;
  remove p "a";
  for i = 1 to 20 do
    assert (pick p = Some ("b", 2))
  done;
  remove p "b";
  assert (pick p = None);
  Array.iter (fun x -> assert (x = None)) p.p_ext.ext_ar;
  assert (Hashtbl.length p.p_tbl = 0)
