open Printf
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

let set_term () =
  let fd = Unix.descr_of_in_channel stdin in
  let x = Unix.tcgetattr fd in
  Unix.tcsetattr fd Unix.TCSANOW
    {
      x with
        Unix.c_icanon = false;
        c_vtime = 0;
        c_vmin = 1;
    }

let init () =
  set_term ();
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


let play_and_get_feedback a =
  (* Play the action *)
  a.ac_exec a;
  
  (* Collect feedback *)
  print_endline "Was it good?";
  print_endline "[Y]es [N]o [R]eplay [S]kip";
  let rec loop () =
    let c = input_char stdin in
    if c <> '\n' then
      print_newline ();
    match c with
        'Y' | 'y' -> Some true
      | 'N' | 'n' -> Some false
      | 'R' | 'r' -> a.ac_exec a; loop ()
      | 'S' | 's' -> None
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
       Some (_, a1), Some (_, a2) ->
         let a3 = Seq.seq a1 a2 in
         if !(a1.ac_boost_count) > 0 
           && !(a2.ac_boost_count) > 0
           && not (Action_tbl.mem all_actions a3) then (
             Action_tbl.add all_actions a3 ();
             Picker.replace active_actions a3.ac_id a3;
           )
     | _ -> ()
  );
  
  (* Pick an active action *)
  match Picker.pick active_actions with
      None -> () (* all actions are disabled due to bad feedback,
                    continue with next cycle *)
    | Some (_, x) ->
        let opt_feedback = play_and_get_feedback x in
  
        (* Apply feedback *)
        let prev_score = !(x.ac_score) in
        printf "Score %i -> " prev_score;
        match opt_feedback with
            None -> ()
          | Some true ->
              let score = prev_score + 100 in
              x.ac_score := score;
              incr x.ac_boost_count;
              printf "%i [up]\n%!" score;
          | Some false ->
              let score = prev_score / 2 in
              if score < 10 then (
                let score = 2 * !(x.ac_lowest_score) in
                assert (score < 0);
                x.ac_score := score;
                x.ac_lowest_score := score;
                Picker.remove active_actions x.ac_id;
                Timer.put disabled_actions (-score) x;
                printf "%i [disabled]\n%!" score;
              )
              else (
                x.ac_score := score;
                printf "%i [down]\n%!" score;
              )

let run () =
  let env = init () in
  while true do
    loop env
  done
