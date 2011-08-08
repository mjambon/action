(*
  Simplistic sound generation.
*)

type name = string
type time = float
type y_modifier =
    Const of float
      (* Constant scaling *)

  | Abs of name * (time -> float)
      (* Absolute mask (input range: [0, infinity[) *)

  | Rel of name * (float -> float)
      (* Relative mask (input range: [0, 1]) *)

type wave = private
    Blank of time

  | Atom of time * name * (time -> float)
      (* Abstract waveform of given duration *)

  | Seq of time * wave * wave
      (* Sequence of two waveforms, with optional positive pause *)

  | Add of time * wave * wave
      (* Addition of two waveforms, with optional positive delay *)

  | Sx of time * float * wave
      (* Horizontal scaling (duration multiplier) *)

  | Sy of time * y_modifier * wave
      (* Vertical scaling (fading, etc.) *)

val length : wave -> time

val blank : time -> wave
val atom : time -> name -> (time -> float) -> wave
val seq : ?pause:time -> wave -> wave -> wave
val add : ?delay:time -> wave -> wave -> wave
val sx : float -> wave -> wave
val sy : float -> wave -> wave

val eval : wave -> time -> float
