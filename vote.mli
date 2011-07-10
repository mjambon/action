(* Range voting *)

val create : int -> float array
val reset : float array -> unit

type slice = {
  offset : int;
  votes : float array;
}

val vote : float array -> slice -> unit
val choose : 'a array -> int
