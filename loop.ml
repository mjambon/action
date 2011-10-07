(*
let init () =
  let active = Picker.create 100 in
  let disabled = Timer.create () in
  List.iter (fun x -> Picker.replace all x.ac_id x) base_actions;
  (active, disabled)

let loop (active, disabled) =
  (* Get list of disabled actions whose score just reached 0
     and add them to the active set. *)
  let out = Timer.tick disabled in
  List.iter (
    fun x ->
      x.ac_score := 0;
      Picker.replace all x.ac_id x;
  ) out;
  
  (* Substract 1 from each active action with score > 0 *)
  
  (* Pick two active actions and compose them into a sequence if it does
     not already exist. *)

  (* Pick an active action *)

  (* Play the action *)

  (* Collect feedback *)

  (* Apply feedback *)
*)
