open Seq

let init () =
  let active = Picker.create 100 in
  let disabled = Timer.create () in
  List.iter (fun x -> Picker.replace active x.ac_id x) Snd_base.base;
  (active, disabled)

let loop (active, disabled) =
  (* Get list of disabled actions whose score just reached 0
     and add them to the active set. *)
  let out = Timer.tick disabled in
  List.iter (
    fun x ->
      x.ac_score := 0;
      Picker.replace active x.ac_id x;
  ) out;
  
  (* Substract 1 from each active action with score > 0 *)
  Picker.iter (
    fun id x ->
      let score = !(x.ac_score) in
      if score > 0 then
        x.ac_score := score - 1
  ) active;
  
  (* Pick two active actions and compose them into a sequence if it does
     not already exist. *)

  (* Pick an active action *)

  (* Play the action *)

  (* Collect feedback *)

  (* Apply feedback *)
