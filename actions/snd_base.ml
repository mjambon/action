open Printf
open Snd_gen

let wedge = Rel ("wedge", (fun x -> 1. -. 2. *. abs_float (x -. 0.5)))

let tu = 0.25 (* time unit *)

let base_notes =
  List.flatten (
    Array.to_list (
      Array.init 8 (
        fun i ->
          let freq = 440. *. (1. +. log (float (i + 1)) /. log 8.) in
          let name = sprintf "%.1fHz" freq in
          [ 
            name, sy wedge (atom tu name (sine freq));
            name ^ "-", sy wedge (atom (0.5 *. tu) name (sine freq));
            name ^ "+", sy wedge (atom (2. *. tu) name (sine freq));
          ]
      )
    )
  )
  @ [ "blank", blank tu ]

let wavetable =
  let tbl = Hashtbl.create 20 in
  List.iter (fun (k, v) -> Hashtbl.add tbl k v) base_notes;
  tbl

let seq a b =
  seq ~pause:0.05 a b

let translate s =
  try Hashtbl.find wavetable s
  with Not_found -> invalid_arg ("Snd_base.translate: " ^ s)

let rec seq_list l =
  match l with
      [] -> blank 0.
    | [x] -> translate x
    | x :: tl -> seq (translate x) (seq_list tl)

let exec x =
  let l = Seq.flatten x in
  List.iter (fun s -> printf "%s " s) l;
  print_newline ();
  let sample = seq_list l in
  Snd_gen.play sample

let base =
  List.map (fun (name, wave) -> Seq.atom name exec) base_notes
