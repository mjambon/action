val logger : (string -> unit) ref
val log : [ `Err | `Info ] -> string -> unit
val logf : [ `Err | `Info ] -> ('a, unit, string, unit) format4 -> 'a
val exn : exn -> string
val log_exn : exn -> unit
