open Printf
open Snd_gen

let wedge = Rel ("wedge", (fun x -> 1. -. 2. *. abs_float (x -. 0.5)))

let base_notes =
  Array.to_list (
    Array.init 8 (
      fun i ->
        let freq = 440. *. (1. +. log (float i) /. log 8.) in
        let name = sprintf "%.1f Hz" freq in
        (name ^ " wedge"), sy wedge (atom 0.25 name (sine freq))
    )
  )

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
  let sample = seq_list l in
  Snd_gen.play sample

let base =
  List.map (fun (name, wave) -> Seq.atom name exec) base_notes
