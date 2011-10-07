type 'a box = {
  mutable remaining : int;
  mutable expiring_elements : 'a list;
  mutable other_elements : (int * 'a) list;
}

type 'a timer = {
  mutable boxes : 'a box list;
  mutable item_count : int;
}

let create () = {
  boxes = [];
  item_count = 0;
}

let count x = x.item_count

(*
  Find a box where the time tb remaining for that box is such that:

    tb <= time_remaining < 2 * tb
*)
let rec insert boxes time_remaining x =
  match boxes with
      [] ->
        [
          {
            remaining = time_remaining;
            expiring_elements = [x];
            other_elements = [];
          }
        ]
    | b :: bl ->
        let tb = b.remaining in
        if time_remaining < tb then
          {
            remaining = time_remaining;
            expiring_elements = [x];
            other_elements = [];
          } :: boxes
        else if time_remaining < 2 * tb then (
          if time_remaining = tb then
            b.expiring_elements <- x :: b.expiring_elements
          else
            b.other_elements <- (time_remaining - tb, x) :: b.other_elements;
          boxes
        )
        else
          let tail = insert bl time_remaining x in
          if tail == bl then
            boxes
          else
            b :: tail


let relocate timer t x =
  if t <= 0 then
    invalid_arg ("Timer.put: " ^ string_of_int t)
  else
    let boxes = insert timer.boxes t x in
    timer.boxes <- boxes

let put timer t x =
  relocate timer t x;
  timer.item_count <- timer.item_count + 1;
  if timer.item_count <= 0 then
    failwith "Timer.put: int overflow"

let tick timer =
  List.iter (fun x -> x.remaining <- x.remaining - 1) timer.boxes;
  if List.exists (fun x -> x.remaining = 0) timer.boxes then

    (* Remove the expired boxes *)
    let expired_boxes, keep =
      List.partition (fun x -> x.remaining = 0) timer.boxes in
    timer.boxes <- keep;

    (* Take expired items from the expired boxes and build the
       result list *)
    let expired_items =
      List.fold_left (
        fun acc box -> List.rev_append box.expiring_elements acc
      ) [] expired_boxes
    in

    (* Take the non-expired items from the expired items and 
       insert them into other boxes, creating new boxes when needed. *)
    List.iter (
      fun box ->
        List.iter (
          fun (time_remaining_at_exit, x) ->
            relocate timer time_remaining_at_exit x
        ) box.other_elements
    ) expired_boxes;
    
    let n = List.length expired_items in
    timer.item_count <- timer.item_count - n;
    assert (timer.item_count >= 0);

    expired_items

  else
    (* No box expires therefore no item expires either. *)
    []


let test () =
  let timer = create () in
  put timer 1 "1";
  assert (tick timer = ["1"]);

  put timer 10 "10";
  assert (tick timer = []);
  put timer 8 "8";
  assert (tick timer = []);
  put timer 11 "11";
  put timer 16 "16";
  assert (tick timer = []);
  put timer 15 "15";
  put timer 40 "40";

  for i = 1 to 5 do assert (tick timer = []) done;

  assert (tick timer = ["8"]);
  assert (tick timer = ["10"]);
  assert (timer.item_count = 4);

  for i = 1 to 2 do assert (tick timer = []) done;
  assert (tick timer = ["11"]);

  for i = 1 to 4 do assert (tick timer = []) done;
  assert (List.sort compare (tick timer) = ["15";"16"]);
  assert (timer.item_count = 1);

  for i = 1 to 24 do assert (tick timer = []) done;
  assert (tick timer = ["40"]);
  assert (timer.item_count = 0);

  assert (tick timer = []);

  timer
;;
(*
test ()
*)
