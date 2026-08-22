(* Percent-decoding, written once and shared by path segments, query strings
   and form bodies. An invalid escape decodes to the bytes as written rather
   than raising, so a malformed target still reaches a handler that can answer
   404 instead of failing the connection.

   Everything works over a range of the original string rather than over
   pieces cut out of it first. Splitting into substrings and then decoding
   each one copied every byte of the target twice, and paid for a [Buffer] per
   piece, when almost no real target holds an escape at all. A range that
   needs no decoding is taken with one [String.sub]; only a range that holds
   a '%', or a '+' where one means space, goes through a buffer. *)

let hex c =
  match c with
  | '0' .. '9' -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
  | 'A' .. 'F' -> Some (Char.code c - Char.code 'A' + 10)
  | _ -> None

(* [escaped ~plus s off stop] is whether the range holds anything decoding
   would change. It reads the range once and answers false for almost every
   real target, which is what makes the fast path worth having. *)
let rec escaped ~plus s off stop =
  off < stop
  &&
  let c = String.unsafe_get s off in
  c = '%' || (plus && c = '+') || escaped ~plus s (off + 1) stop

(* [decode_sub ~plus s off len] is the range decoded. The buffer path is the
   same loop the whole-string decoder always ran, bounded to the range. *)
let decode_sub ~plus s off len =
  let stop = off + len in
  if not (escaped ~plus s off stop) then String.sub s off len
  else begin
    let b = Buffer.create len in
    let i = ref off in
    while !i < stop do
      let c = String.unsafe_get s !i in
      if plus && c = '+' then (
        Buffer.add_char b ' ';
        incr i)
      else if c = '%' && !i + 2 < stop then
        match (hex s.[!i + 1], hex s.[!i + 2]) with
        | Some hi, Some lo ->
            Buffer.add_char b (Char.chr ((hi * 16) + lo));
            i := !i + 3
        | _ ->
            Buffer.add_char b c;
            incr i
      else (
        Buffer.add_char b c;
        incr i)
    done;
    Buffer.contents b
  end

let decode ~plus s = decode_sub ~plus s 0 (String.length s)

(* [segments path] is [path] split on '/', with empty pieces dropped and each
   piece percent-decoded. '+' is literal in a path. Built by recursing to the
   end and consing on the way out, so there is no reversal and no intermediate
   list of undecoded pieces. *)
let segments path =
  let n = String.length path in
  let rec go start i =
    if i >= n then
      if i > start then [ decode_sub ~plus:false path start (i - start) ]
      else []
    else if String.unsafe_get path i = '/' then
      if i > start then
        decode_sub ~plus:false path start (i - start) :: go (i + 1) (i + 1)
      else go (i + 1) (i + 1)
    else go start (i + 1)
  in
  go 0 0

(* [pairs s] is an application/x-www-form-urlencoded string decoded into an
   association list. '+' means space. A piece with no '=' has an empty value. *)
let pairs s =
  let n = String.length s in
  let piece start stop =
    if stop <= start then None
    else
      let rec eq i =
        if i >= stop then None
        else if String.unsafe_get s i = '=' then Some i
        else eq (i + 1)
      in
      match eq start with
      | None -> Some (decode_sub ~plus:true s start (stop - start), "")
      | Some i ->
          Some
            ( decode_sub ~plus:true s start (i - start),
              decode_sub ~plus:true s (i + 1) (stop - i - 1) )
  in
  let rec go start i =
    if i >= n then match piece start i with Some p -> [ p ] | None -> []
    else if String.unsafe_get s i = '&' then
      match piece start i with
      | Some p -> p :: go (i + 1) (i + 1)
      | None -> go (i + 1) (i + 1)
    else go start (i + 1)
  in
  go 0 0

(* [param ~plus s name] is the first value in [s] whose key decodes to [name],
   without building the association list. Almost every request reads no query
   parameter at all, and one that does usually reads one or two, so scanning
   for the key beats decoding every pair into a list first.

   A key that holds no escape is compared where it lies. Only a key that does
   is decoded, and only the value that matched is. *)
(* Every function below takes what it needs as an argument rather than
   closing over it. A local recursive function that captures the string, the
   name and the mode allocates a closure on each call, which for a scan this
   short is most of the cost. *)

let rec same_bytes s off stop name =
  off = stop
  ||
  let at = off - stop + String.length name in
  Char.equal (String.unsafe_get s off) (String.unsafe_get name at)
  && same_bytes s (off + 1) stop name

(* The escape check comes first. An escaped key's encoded length is not its
   decoded length, so comparing lengths before decoding would answer no for
   every key that carries an escape: [%41] is one byte once decoded and three
   before. *)
let key_is ~plus s off stop name =
  if escaped ~plus s off stop then
    String.equal (decode_sub ~plus s off (stop - off)) name
  else stop - off = String.length name && same_bytes s off stop name

let rec index_from s i stop c =
  if i >= stop then -1
  else if Char.equal (String.unsafe_get s i) c then i
  else index_from s (i + 1) stop c

(* [param ~plus s name] is the first value in [s] whose key decodes to [name],
   without building the association list. Almost every request reads no query
   parameter at all, and one that does usually reads one or two, so scanning
   for the key beats decoding every pair into a list first.

   A key that holds no escape is compared where it lies. Only a key that does
   is decoded, and only the value that matched is. *)
let rec param_from ~plus s name start n =
  if start > n then None
  else
    let stop = match index_from s start n '&' with -1 -> n | i -> i in
    let next = stop + 1 in
    if stop <= start then
      if next > n then None else param_from ~plus s name next n
    else
      let eq = index_from s start stop '=' in
      if eq = -1 then
        if key_is ~plus s start stop name then Some ""
        else if next > n then None
        else param_from ~plus s name next n
      else if key_is ~plus s start eq name then
        Some (decode_sub ~plus s (eq + 1) (stop - eq - 1))
      else if next > n then None
      else param_from ~plus s name next n

let param ~plus s name = param_from ~plus s name 0 (String.length s)

(* Walking a path without cutting it up.

   Dispatch used to run over a [string list] built for every request, so a
   route made only of literal segments still paid a substring and a cons per
   segment before it could compare anything. These let the matcher walk the
   path where it lies and allocate only for what a capture actually binds. *)

(* [seg_start path i n] is the offset of the next non-empty segment at or
   after [i], or [n] when the path is exhausted. Empty pieces are skipped,
   which is what dropping them from the old list did. *)
let rec seg_start path i n =
  if i >= n then n
  else if Char.equal (String.unsafe_get path i) '/' then
    seg_start path (i + 1) n
  else i

(* [seg_stop path i n] is the offset one past the segment starting at [i]. *)
let rec seg_stop path i n =
  if i >= n then n
  else if Char.equal (String.unsafe_get path i) '/' then i
  else seg_stop path (i + 1) n

(* [seg_is path off stop lit] is whether the segment decodes to [lit]. A
   segment holding no escape is compared where it lies; only one that does is
   decoded, so [/%6eotes] still matches the literal [notes] as it did when
   every segment was decoded up front. *)
let seg_is path off stop lit =
  if escaped ~plus:false path off stop then
    String.equal (decode_sub ~plus:false path off (stop - off)) lit
  else stop - off = String.length lit && same_bytes path off stop lit

(* [seg_list path i n] is every remaining segment, decoded. Only a [rest]
   capture materialises one. *)
let rec seg_list path i n =
  let off = seg_start path i n in
  if off >= n then []
  else
    let stop = seg_stop path off n in
    decode_sub ~plus:false path off (stop - off) :: seg_list path stop n
