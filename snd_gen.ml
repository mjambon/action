open Printf

type name = string
type time = float

type y_modifier =
    Const of float
  | Abs of name * (time -> float)
  | Rel of name * (float -> float)

type wave =
    Blank of time
  | Atom of time * name * (time -> float)
  | Seq of time * wave * wave 
  | Add of time * wave * wave
  | Sx of time * float * wave
  | Sy of time * y_modifier * wave

let length = function
    Blank dt
  | Atom (dt, _, _)
  | Seq (dt, _, _)
  | Add (dt, _, _)
  | Sx (dt, _, _)
  | Sy (dt, _, _) -> dt

let blank dt = Blank dt

let atom dt name f = Atom (dt, name, f)

let seq ?pause a b =
  match pause with
      None -> Seq (length a +. length b, a, b)
    | Some dt ->
        if not (dt >= 0.) then
          invalid_arg "Snd_gen.seq"
        else
          let b' = Seq (dt +. length b, Blank dt, b) in
          Seq (length a +. length b', a, b')

let add ?delay a b =
  match delay with
      None -> Add (max (length a) (length b), a, b)
    | Some dt ->
        if not (dt >= 0.) then
          invalid_arg "Snd_gen.add"
        else
          let b' = Seq (dt +. length b, Blank dt, b) in
          Add (max (length a) (length b'), a, b')

let sx r w =
  if not (r >= 0.) then
    invalid_arg "Snd_gen.sx"
  else
    Sx (length w /. r, r, w)

let sy m w = Sy (length w, m, w)

let rec eval w t =
  if not (t >= 0.) then
    invalid_arg "Snd_gen.sx"
  else
    let dt = length w in
    if t > dt then
      0.
    else
      match w with
          Blank _ -> 0.
        | Atom (dt, _, f) ->
            if t <= dt then f t
            else 0.
        | Seq (_, a, b) ->
            let dt1 = length a in
            if t <= dt1 then
              eval a t
            else
              eval b (t -. dt1)
        | Add (_, a, b) ->
            eval a t +. eval b t
        | Sx (_, r, w) ->
            eval w (r *. t)
        | Sy (_, m, w) ->
            (match m with
                 Const r -> r *. eval w t
               | Abs (_, f) ->
                   let x = eval w t in 
                   if x = 0. then
                     0.
                   else
                     f t *. x
               | Rel (_, f) ->
                   let x = eval w t in
                   if x = 0. then
                     0.
                   else
                     f (t /. dt) *. x
            )

let pi = acos (-1.)
let twopi = 2. *. pi

let sine freq t =
  sin (freq *. twopi *. t)

let sin440 = sine 440.

let w_sin440 = atom 1. "sin440" sin440
let w_sin880 = sx 2. (seq w_sin440 w_sin440)

let wedge = Rel ("wedge", (fun x -> 1. -. 2. *. abs_float (x -. 0.5)))

let w = seq ~pause:1. (sy wedge w_sin440) (sy wedge w_sin880)

let play x =
  let fname = "/dev/shm/randomtask_sound.wav" in
  Snd_wav.save_wav fname (length x) (eval x);
  ignore (Sys.command "time aplay test.wav")

let test () = play w

