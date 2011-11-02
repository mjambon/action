open Seq


module Action_tbl =
  Hashtbl.Make (
    struct
      type t = Seq.action
      let hash = Seq.hash
      let equal = Seq.equal
    end
  )

type env = {
  all_actions : unit Action_tbl.t;
  active_actions : (int, Seq.action) Picker.picker;
  disabled_actions : Seq.action Timer.timer;
}

let init () =
  let all_actions = Action_tbl.create 100 in
  let active_actions = Picker.create 100 in
  let disabled_actions = Timer.create () in
  List.iter (
    fun x ->
      Action_tbl.replace all_actions x ();
      Picker.replace active_actions x.ac_id x;
  ) Snd_base.base;
  {
    all_actions;
    active_actions;
    disabled_actions;
  }


let play_and_get_feedback () =
  (* Play the action *)
  a.ac_exec a;
  
  (* Collect feedback *)
  print_endline "Was it good?";
  print_endline "[Y]es [N]o [R]eplay [S]kip";
  let rec loop () =
    match read_line () with
        "Y" | "y" -> Some true
      | "N" | "n" -> Some false
      | "R" | "r" -> a.ac_exec a; loop ()
      | "S" | "s" -> None
      | _ -> loop ()
  in
  loop ()

let loop { all_actions; active_actions; disabled_actions } =
  (* Get list of disabled actions whose score just reached 0
     and add them to the active set. *)
  let out = Timer.tick disabled_actions in
  List.iter (
    fun x ->
      x.ac_score := 0;
      Picker.replace active_actions x.ac_id x;
  ) out;
  
  (* Substract 1 from each active action with score > 0 *)
  Picker.iter (
    fun id x ->
      let score = !(x.ac_score) in
      if score > 0 then
        x.ac_score := score - 1
  ) active_actions;
  
  (* Pick two active actions and compose them into a sequence if it does
     not already exist. *)
  (match Picker.pick active_actions, Picker.pick active_actions with
       Some a1, Some a2 ->
         let a3 = Seq.seq a1 a2 in
         if not (Action_tbl.mem all_actions a3) then (
           Action_tbl.add all_actions a3 ();
           Picker.replace active_actions a3.ac_id a3;
         );
     | _ -> ()
  );
  
  (* Pick an active action *)
  match Picker.pick active_actions with
      None -> () (* all actions are disabled due to bad feedback,
                    continue with next cycle *)
    | Some a ->
        let opt_feedback = play_and_get_feedback () in
  
        (* Apply feedback *)
        match opt_feedback with
            None -> ()
          | Some true -> ...
          | Some false -> ...
