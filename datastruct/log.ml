open Printf

let logger = ref (fun s -> prerr_string s; flush stderr)

let log level s =
  let msg =
    match level with
        `Err -> sprintf "[err] %s\n" s
      | `Info -> sprintf "[info] %s\n" s
  in
  !logger msg

let logf level msgf =
  kprintf (log level) msgf

let rec scan_lines tbl buf s pos ellipsis =
  let len = String.length s in
  let h = ref 0 in
  let pos2 = ref pos in
  (try
     for i = pos to len - 1 do
       match s.[i] with
           '\n' ->
             pos2 := i + 1;
             raise Exit
         | c -> h := 223 * !h + (Char.code c)
     done;
     pos2 := len
   with Exit -> ());
  let ellipsis2 = 
    if Hashtbl.mem tbl !h then
      ellipsis + 1
    else (
      Hashtbl.add tbl !h ();
      if ellipsis > 0 then
        bprintf buf "... (omitting %i lines)\n" ellipsis;
      Buffer.add_substring buf s pos (!pos2 - pos);
      0
    )
  in
  if !pos2 < len then
    scan_lines tbl buf s !pos2 ellipsis2
  else
    if ellipsis2 > 0 then
      bprintf buf "... (omitting %i lines)\n" ellipsis2

(* Replace any sequence of lines that already occurred by "...\n" *)
let make_compact s =
  let tbl = Hashtbl.create 20 in
  let buf = Buffer.create 1000 in
  scan_lines tbl buf s 0 0;
  Buffer.contents buf

let exn e =
  if Printexc.backtrace_status () then
    make_compact (Printexc.get_backtrace ()) ^ Printexc.to_string e
  else
    Printexc.to_string e

let log_exn e =
  logf `Err "Exception %s" (exn e)
