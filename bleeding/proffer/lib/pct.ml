(* Invalid percent escapes are preserved rather than rejected, which is where this
   differs from [Uriz.percent_decode]. Ranges without escapes take the direct substring
   path. Sources are read at [local]. A decoded result is a heap string, except from the
   [_local] forms, which build it in the caller's region. *)

module Scanner = Httpz_uri.Scanner

let copy (s : string @ local) off len =
  let b = Bytes.create len in
  Bytes.unsafe_blit_string s off b 0 len;
  Bytes.unsafe_to_string b
;;

let copy_all (s : string @ local) = copy s 0 (String.length s)

(* The stub allocates in the caller's region, never on the heap, and cannot
   raise once the length is checked, so it is declared [noalloc] as Base
   declares its own. *)
external unsafe_create_local : int -> bytes @ local @@ portable
  = "caml_create_local_bytes"
[@@noalloc]

let[@zero_alloc] create_local len = exclave_
  if len > Sys.max_string_length then invalid_arg "Pct.create_local";
  unsafe_create_local len
;;

let[@zero_alloc] sub_local (s : string @ local) off len = exclave_
  let b = create_local len in
  Bytes.unsafe_blit_string s off b 0 len;
  Bytes.unsafe_to_string b
;;

let[@zero_alloc] escaped ~plus (s : string @ local) off stop =
  Scanner.needs_percent_decode s ~pos:off ~len:(stop - off) ~plus_as_space:plus
;;

(* [-1] when the escape at [i] is malformed or cut off by [stop]. *)
let[@zero_alloc] escape_at (s : string @ local) i stop =
  if i + 2 < stop
  then (
    let hi = Scanner.hex_val (String.unsafe_get s (i + 1)) in
    let lo = Scanner.hex_val (String.unsafe_get s (i + 2)) in
    if hi >= 0 && lo >= 0 then (hi * 16) + lo else -1)
  else -1
;;

let[@zero_alloc] rec decoded_len (s : string @ local) i stop acc =
  if i >= stop
  then acc
  else if Char.equal (String.unsafe_get s i) '%' && escape_at s i stop >= 0
  then decoded_len s (i + 3) stop (acc + 1)
  else decoded_len s (i + 1) stop (acc + 1)
;;

let[@zero_alloc] rec decode_into ~plus (s : string @ local) i stop (b : bytes @ local) j =
  if i < stop
  then (
    let c = String.unsafe_get s i in
    if plus && Char.equal c '+'
    then (
      Bytes.unsafe_set b j ' ';
      decode_into ~plus s (i + 1) stop b (j + 1))
    else if Char.equal c '%'
    then (
      let v = escape_at s i stop in
      if v >= 0
      then (
        Bytes.unsafe_set b j (Char.unsafe_chr v);
        decode_into ~plus s (i + 3) stop b (j + 1))
      else (
        Bytes.unsafe_set b j c;
        decode_into ~plus s (i + 1) stop b (j + 1)))
    else (
      Bytes.unsafe_set b j c;
      decode_into ~plus s (i + 1) stop b (j + 1)))
;;

(* The scanner refuses a window holding a malformed triplet, so one it refuses is redone
   byte by byte with the escape left as it stands. [decoded_len] sizes [b] for either
   writer: it counts a well-formed triplet as the one byte the scanner writes, and a
   malformed one as the byte the fallback leaves. *)
let[@zero_alloc] decode_escaped_into ~plus (s : string @ local) off stop (b : bytes @ local)
  =
  let written =
    Scanner.percent_decode_into
      s
      ~pos:off
      ~len:(stop - off)
      ~dst:b
      ~dst_pos:0
      ~plus_as_space:plus
  in
  if written < 0 then decode_into ~plus s off stop b 0
;;

(* [decode_escaped_sub] and [decode_escaped_local] are [decode_sub] and [decode_local] for
   a window already known to hold an escape, so a caller that has just tested for one does
   not scan it twice. *)
let decode_escaped_sub ~plus (s : string @ local) off len =
  let stop = off + len in
  let b = Bytes.create (decoded_len s off stop 0) in
  decode_escaped_into ~plus s off stop b;
  Bytes.unsafe_to_string b
;;

let[@zero_alloc] decode_escaped_local ~plus (s : string @ local) off len = exclave_
  let stop = off + len in
  let b = create_local (decoded_len s off stop 0) in
  decode_escaped_into ~plus s off stop b;
  Bytes.unsafe_to_string b
;;

let decode_sub ~plus (s : string @ local) off len =
  if not (escaped ~plus s off (off + len))
  then copy s off len
  else decode_escaped_sub ~plus s off len
;;

let[@zero_alloc] decode_local ~plus (s : string @ local) off len = exclave_
  if not (escaped ~plus s off (off + len))
  then sub_local s off len
  else decode_escaped_local ~plus s off len
;;

let decode ~plus (s : string @ local) = decode_sub ~plus s 0 (String.length s)

let segments (path : string @ local) =
  let n = String.length path in
  let rec go start i =
    if i >= n
    then if i > start then [ decode_sub ~plus:false path start (i - start) ] else []
    else if String.unsafe_get path i = '/'
    then
      if i > start
      then decode_sub ~plus:false path start (i - start) :: go (i + 1) (i + 1)
      else go (i + 1) (i + 1)
    else go start (i + 1)
  in
  let segments = go 0 0 in
  segments
;;

let pairs = Httpz_media.Urlencoded.decode

let[@zero_alloc] rec same_bytes (s : string @ local) off stop (name : string @ local) =
  off = stop
  ||
  let at = off - stop + String.length name in
  Char.equal (String.unsafe_get s off) (String.unsafe_get name at)
  && same_bytes s (off + 1) stop name
;;

(* Compared after decoding because [%41] is one decoded byte, and decoded into the
   region so the comparison allocates nothing. *)
let[@zero_alloc] key_is ~plus (s : string @ local) off stop (name : string @ local) =
  if escaped ~plus s off stop
  then (
    let local_ decoded = decode_escaped_local ~plus s off (stop - off) in
    String.equal decoded name)
  else stop - off = String.length name && same_bytes s off stop name
;;

let[@zero_alloc] rec index_from (s : string @ local) i stop c =
  if i >= stop
  then -1
  else if Char.equal (String.unsafe_get s i) c
  then i
  else index_from s (i + 1) stop c
;;

let rec param_from ~plus (s : string @ local) (name : string @ local) start n =
  if start > n
  then None
  else (
    let stop =
      match index_from s start n '&' with
      | -1 -> n
      | i -> i
    in
    let next = stop + 1 in
    if stop <= start
    then if next > n then None else param_from ~plus s name next n
    else (
      let eq = index_from s start stop '=' in
      if eq = -1
      then
        if key_is ~plus s start stop name
        then Some ""
        else if next > n
        then None
        else param_from ~plus s name next n
      else if key_is ~plus s start eq name
      then Some (decode_sub ~plus s (eq + 1) (stop - eq - 1))
      else if next > n
      then None
      else param_from ~plus s name next n))
;;

let param ~plus (s : string @ local) (name : string @ local) =
  param_from ~plus s name 0 (String.length s)
;;

(* Route matching walks the original path and decodes only captures. *)
let[@zero_alloc] rec seg_start (path : string @ local) i n =
  if i >= n
  then n
  else if Char.equal (String.unsafe_get path i) '/'
  then seg_start path (i + 1) n
  else i
;;

let[@zero_alloc] rec seg_stop (path : string @ local) i n =
  if i >= n
  then n
  else if Char.equal (String.unsafe_get path i) '/'
  then i
  else seg_stop path (i + 1) n
;;

let[@zero_alloc] seg_is (path : string @ local) off stop lit =
  if escaped ~plus:false path off stop
  then (
    let local_ decoded = decode_escaped_local ~plus:false path off (stop - off) in
    String.equal decoded lit)
  else stop - off = String.length lit && same_bytes path off stop lit
;;

let[@zero_alloc] rec seg_list_local (path : string @ local) i n = exclave_
  let off = seg_start path i n in
  if off >= n
  then []
  else (
    let stop = seg_stop path off n in
    decode_local ~plus:false path off (stop - off) :: seg_list_local path stop n)
;;

(* Each callback's decoded strings live in one iteration's region. *)
let iter_pairs (s : string @ local)
    (f : (string @ local -> string @ local -> unit) @ local) =
  let len = String.length s in
  let rec next start =
    if start < len then begin
      let rec stop i = if i = len || s.[i] = '&' then i else stop (i + 1) in
      let last = stop start in
      if last > start then begin
        let rec equal i = if i = last || s.[i] = '=' then i else equal (i + 1) in
        let eq = equal start in
        let local_ key = decode_local ~plus:true s start (eq - start) in
        let local_ value =
          if eq = last then "" else decode_local ~plus:true s (eq + 1) (last - eq - 1)
        in
        f key value
      end;
      next (last + 1)
    end
  in
  let () = next 0 in ()

let iter_segments (s : string @ local) (f @ local) =
  let len = String.length s in
  let rec next start =
    if start < len then begin
      let rec stop i = if i = len || s.[i] = '/' then i else stop (i + 1) in
      let last = stop start in
      if last > start then begin
        let local_ value = decode_local ~plus:false s start (last - start) in
        f value
      end;
      next (last + 1)
    end
  in
  let () = next 0 in ()
