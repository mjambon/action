open Printf

type 'a seq =
    Atom
  | Seq of ('a * 'a)

type action = {
  action_name : string option;
  action_length : int;
  action_seq : action seq;
  action_func : (unit -> unit) option;
}

type action_base = action array

let seq a b =
  {
    action_name = None;
    action_length = a.action_length + b.action_length;
    action_seq = Seq (a, b);
    action_func = None;
  }

let rec flatten x tl =
  match x.action_name, x.action_seq with
      Some s, _ -> s :: tl
    | None, Seq (a, b) ->
        let bl = flatten b tl in
        flatten a bl
    | None, Atom ->
        assert false

let print_seq x =
  let l = flatten x [] in
  print_string (Yojson.Safe.prettify (Seq_j.string_of_string_list l))

let test () =
  let mk name =
    {
      action_name = Some name;
      action_length = 1;
      action_seq = Atom;
      action_func = Some (fun () -> printf "%s\n" name);
    }
  in
  let a = [| mk "up"; mk "down"; mk "left"; mk "right"; mk "nop" |] in
  let x = seq (seq a.(0) a.(0)) a.(1) in
  let y = seq x x in
  print_seq y;
  print_newline ()
