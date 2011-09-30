open Printf

type 'a lazy_list = 'a forced_list lazy_t

and 'a forced_list = Cell of 'a * 'a lazy_list | Empty

let empty = lazy Empty

let rec compare cmp a b =
  match Lazy.force a, Lazy.force b with
      Empty, Empty -> 0
    | Cell _, Empty -> 1
    | Empty, Cell _ -> -1
    | Cell (ha, ta), Cell (hb, tb) ->
        let c = cmp ha hb in
        if c <> 0 then c
        else
          compare cmp ta tb

let rec hash_aux n h x =
  if n <= 0 then 0
  else
    match Lazy.force x with
        Empty -> 0
      | Cell (hx, tx) -> h hx + 223 * hash_aux (n-1) h tx

let hash h x = hash_aux 10 h x

let test () =
  let cell x tail =
    lazy (
      printf "%i\n%!" x;
      Cell (x, tail)
    )
  in
  let l =
    let acc = ref empty in
    for i = 20 downto 1 do
      acc := cell i !acc
    done;
    !acc
  in
  let a = hash Hashtbl.hash l in
  printf "hash = %i\n%!" a;
  let b = hash Hashtbl.hash l in
  printf "hash = %i\n%!" b;
  assert (a = b)


