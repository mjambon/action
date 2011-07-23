(* Range voting *)

val create : int -> float array
val clear : float array -> unit

type slice = {
  offset : int;
  votes : float array;
}

val vote : float array -> slice -> unit
val choose : float array -> int
