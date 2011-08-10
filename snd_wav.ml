(*
  WAV sound format
  
  Warning: this is old, messy and incomplete code.
*)

open Printf

type fmt = 
    { datasize : int;
      wFormatTag : int; (* short | should be 1 *) 
      wChannels : int; (* unsigned short | mono=1, stereo=2 *) 
      dwSamplesPerSec : float; (* unsigned long | frames per second *) 
      dwAvgBytesPerSec : float; (* unsigned long | bytes per second *)
      wBlockAlign : int; (* unsigned short | bytes per frame *) 
      wBitsPerSample : int; (* unsigned short | bits per sample point *)
      input : in_channel -> float }

(* nfloats: values between -1 and +1 *)

let two15 = 2. ** 15.
let two16 = 2. ** 16.
let two31 = 2. ** 31.
let two32 = 2. ** 32.

let input_chars ic n =
  let buf = String.create n in
  really_input ic buf 0 n;
  buf

let rec output_bits oc n x = 
  for pos = n - 1 downto 0 do
    output_char oc (if (x lsr pos) land 1 = 0 then '0' else '1')
  done

let output_string_bits oc s =
  String.iter (fun c -> output_bits oc 8 (Char.code c); print_char ' ') s

(* low endian signed/unsigned long -> int32 *)
let input_int32_int32 ic =
  let inp ic = Int32.of_int (input_byte ic) in
  let ( ++ ) a b = Int32.logor (Int32.shift_left a 8) b in
  let b0 = inp ic in
  let b1 = inp ic in
  let b2 = inp ic in
  let b3 = inp ic in
  b3 ++ b2 ++ b1 ++ b0


(* low endian unsigned long -> float *)
let input_int32u_float ic =
  let ( ++ ) a b = (a lsl 8) lor b in
  let b0 = input_byte ic in
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  (float (b3 ++ b2 ++ b1) *. 256. +. float b0)


(* low endian positive signed long -> unsigned int *)
let input_int32_intu ic =
  let ( ++ ) a b = (a lsl 8) lor b in
  let b0 = input_byte ic in
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  b3 ++ b2 ++ b1 ++ b0

let input_int32_int ic =
  let x = input_int32_intu ic in
  if x < 0 then 
    (output_bits stderr 32 x;
     invalid_arg "Wav.input_int32_int: out of range")
  else x

(* low endian unsigned short -> int *)
let input_int16u ic =
  let b0 = input_byte ic in
  let b1 = input_byte ic in
  (b1 lsl 8) lor b0

(* low endian signed short -> int *)
let input_int16s ic =
  let b0 = input_byte ic in
  let b1 = input_byte ic in
  let x = (b1 lsl 8) lor b0 in
  if b1 lsr 7 = 0 then x
  else (lnot 0xff_ff) lor x

(* unsigned byte -> int *)
let input_int8u = input_byte

(* signed byte -> int *)
let input_int8s ic =
  let b = input_byte ic in
  if b lsr 7 = 0 then b
  else (lnot 0xff) lor b

let char0 oc x = output_byte oc (x land 0xff)
let char1 oc x = output_byte oc ((x lsr 8) land 0xff)
let char2 oc x = output_byte oc ((x lsr 16) land 0xff)
let char3 oc x = output_byte oc ((x lsr 24) land 0xff)

(* unsigned int -> int32 *)
let output_int_int32 oc x =
  char0 oc x;
  char1 oc x;
  char2 oc x;
  char3 oc x

let output_int_int16u oc x =
  char0 oc x;
  char1 oc x

let output_int_int16s = output_int_int16u

let check_range x =
  if x < -1. || x > 1. then
    invalid_arg (sprintf "check_range: %f" x)

let output_nfloat_int8u oc x =
  check_range x;
  char0 oc (truncate (128. *. (x +. 1.)))

let output_nfloat_int16s oc x =
  check_range x;
  let y = truncate (two15 *. x) in
  char0 oc y;
  char1 oc y

let output_nfloat_int32u oc x =
  check_range x;
  let y = two31 *. (x +. 1.) in
  char0 oc (truncate (mod_float y 256.));
  let i = truncate (y /. 256.) in
  char0 oc i;
  char1 oc i;
  char2 oc i

let output_float_int32u oc y =
  char0 oc (truncate (mod_float y 256.));
  let i = truncate (y /. 256.) in
  char0 oc i;
  char1 oc i;
  char2 oc i


let output_int_int8u = output_byte
let output_int_int8s = output_byte


let open_chunk ic =
  let chunk_id = input_chars ic 4 in
  let chunk_size = input_int32_int ic in
  (chunk_id, chunk_size)

let skip ic size =
  seek_in ic (pos_in ic + size + size land 1)

let input_mono8 ic = input_int8u ic
let input_stereo8 ic =
  let a = input_int8u ic in
  let b = input_int8u ic in
  (a + b) / 2

let input_mono16 ic = input_int16s ic
let input_stereo16 ic =
  let a = input_int16s ic in
  let b = input_int16s ic in
  (a + b) / 2

let input_mono8_nfloat ic = float (input_int8u ic) /. 256. -. 0.5
let input_stereo8_nfloat ic = 
  (float (input_int8u ic + input_int8u ic)) /. 256. -. 1.

let input_mono16_nfloat ic = float (input_int16s ic) /. two15
let input_stereo16_nfloat ic =
  (float (input_int16s ic + input_int16s ic)) /. two16

let output_nfloat_mono8 = output_nfloat_int8u
let output_nfloat_stereo8 oc a b = 
  output_nfloat_int8u oc a;
  output_nfloat_int8u oc b

let output_nfloat_mono16 = output_nfloat_int16s
let output_nfloat_stereo16 oc a b = 
  output_nfloat_int16s oc a;
  output_nfloat_int16s oc b


let get_input ~wChannels ~wBitsPerSample =
  match wChannels, wBitsPerSample with
      1, 8 -> input_mono8_nfloat
    | 1, 16 -> input_mono16_nfloat
    | 2, 8 -> input_stereo8_nfloat
    | 2, 16 -> input_stereo16_nfloat
    | _ -> failwith "not implemented"

let rec get_fmt ic =
  let id, fmtsize = open_chunk ic in
  if id <> "fmt " then 
    (skip ic fmtsize;
     get_fmt ic)
  else 
    let wFormatTag = input_int16s ic in
    let wChannels = input_int16u ic in
    let dwSamplesPerSec = input_int32u_float ic in
    let dwAvgBytesPerSec = input_int32u_float ic in
    let wBlockAlign = input_int16u ic in
    let wBitsPerSample = input_int16u ic in
    let input = get_input ~wChannels ~wBitsPerSample in
    if wFormatTag <> 1 then 
      failwith "WAV compression: not supported";
    { datasize = 0;
      wFormatTag = wFormatTag;
      wChannels = wChannels;
      dwSamplesPerSec = dwSamplesPerSec;
      dwAvgBytesPerSec = dwAvgBytesPerSec;
      wBlockAlign = wBlockAlign;
      wBitsPerSample = wBitsPerSample;
      input = input }

let print_fmt fmt =
  printf "\
datasize = %i;
wFormatTag = %i;
wChannels = %i;
dwSamplesPerSec = %g;
dwAvgBytesPerSec = %g;
wBlockAlign = %i;
wBitsPerSample = %i;\n"
    fmt.datasize
    fmt.wFormatTag fmt.wChannels fmt.dwSamplesPerSec fmt.dwAvgBytesPerSec 
    fmt.wBlockAlign fmt.wBitsPerSample





let rec get_wave ic =
  let fmt = get_fmt ic in
  let rec loop ic =
    let id, datasize = open_chunk ic in
    if id <> "data" then 
      (skip ic datasize;
       loop ic)
    else
      datasize in
  let datasize = loop ic in
  { fmt with datasize = datasize }


let open_in_wav file =
  let ic = open_in_bin file in
  if input_chars ic 4 <> "RIFF" then
    failwith (sprintf "Malformed WAV file %s (1)" file);
  let size = input_int32_int ic in
  if input_chars ic 4 <> "WAVE" then
    failwith (sprintf "Malformed WAV file %s (2)" file);
  let fmt = get_wave ic in
  printf "size: 8 + %i\n" size;
  print_fmt fmt;
  flush stdout;
  (fmt, ic)

let mono16_fmt fmt =
  let datasize = (fmt.datasize / fmt.wChannels) * 16 / fmt.wBitsPerSample in
  { fmt with
      datasize = datasize;
      wFormatTag = 1;
      wChannels = 1;
      dwAvgBytesPerSec = fmt.dwAvgBytesPerSec /. float fmt.wChannels;
      wBlockAlign = 2;
      wBitsPerSample = 16 }

let full_size fmt =
  4 +  (* WAVE *)
  8 +  (* fmt chunk ID and size *)
  16 + (* fmt chunk *)
  8 +  (* data chunk ID and size *)
  fmt.datasize (* data chunk *)


(* mono output only *)
let open_out_wav fmt file =
  let oc = open_out_bin file in
  fprintf oc "RIFF%aWAVE" output_int_int32 (full_size fmt);
  fprintf oc "fmt %a" output_int_int32 16;
  fprintf oc "%a%a%a%a%a%a" 
    output_int_int16s fmt.wFormatTag
    output_int_int16u fmt.wChannels
    output_float_int32u fmt.dwSamplesPerSec
    output_float_int32u fmt.dwAvgBytesPerSec
    output_int_int16u fmt.wBlockAlign
    output_int_int16u fmt.wBitsPerSample;
  fprintf oc "data%a" output_int_int32 fmt.datasize;
  oc



let save_wav fname tmax f =
  let sampling_rate = 44100. in
  let nsamples = truncate (sampling_rate *. tmax) in
  let fmt = {
    datasize = 2 * nsamples;
    wFormatTag = 1; (* should be 1 *)
    wChannels = 1; (* mono *)
    dwSamplesPerSec = sampling_rate; (* frames per second *) 
    dwAvgBytesPerSec = 2. *. sampling_rate; (* bytes per second *)
    wBlockAlign = 2; (* bytes per frame *) 
    wBitsPerSample = 16; (* bits per sample point *)
    input = (fun ic -> assert false)
  }
  in
  let oc = open_out_wav fmt fname in
  try
    for i = 0 to nsamples - 1 do
      let t = float i /. sampling_rate in
      output_nfloat_int16s oc (f t)
    done;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e
