open Base
module I16 = Stdlib_stable.Int16_u
module I64 = Stdlib_upstream_compatible.Int64_u

let[@inline always] i16 x = I16.of_int x
let[@inline always] to_int x = I16.to_int x

type t =
  #{ meth : Method.t
   ; target : Span.t
   ; path : Span.t
   ; query : Span.t
   ; version : Version.t
   ; body_off : int16#
   ; content_length : int64#
   ; is_chunked : bool
   ; keep_alive : bool
   ; connection_upgrade : bool
   ; expect_continue : bool
   ; unsupported_expectation : bool
   }

(* A negative body length represents an absent or zero-length body. *)
let[@inline] body_bounds ~(len : int16#) (req : t @ local) : #(int * int * bool) =
  let cl = req.#content_length in
  let buf_len = to_int len in
  if I64.compare cl #0L <= 0
  then #(-1, 0, false)
  else (
    let body_off = to_int req.#body_off in
    (* A body larger than an OCaml [int], or one whose exclusive end would
       overflow it, can never be wholly resident in this caller-owned buffer.
       Saturate the advisory arithmetic instead of truncating [int64] and
       accidentally reporting it complete. *)
    if I64.compare cl (I64.of_int (Stdlib.max_int - body_off)) > 0
    then #(Stdlib.max_int, Stdlib.max_int, false)
    else (
      let body_len = I64.to_int cl in
      let body_end = body_off + body_len in
      #(body_len, body_end, body_end <= buf_len)))
;;

let[@zero_alloc] body_in_buffer ~(len : int16#) (req : t @ local) =
  if req.#is_chunked
  then false
  else (
    let #(body_len, _, in_buffer) = body_bounds ~len req in
    body_len < 0 || in_buffer)
;;

(* [opt]: this is zero-alloc only once [Span.make] is inlined, which the dev profile's
   [-opaque] prevents. Checked in release/optimized builds. *)
let[@zero_alloc opt] body_span ~(len : int16#) (req : t @ local) =
  if req.#is_chunked
  then Span.make ~off:(i16 0) ~len:(i16 (-1))
  else (
    let #(body_len, _, in_buffer) = body_bounds ~len req in
    if body_len < 0
    then Span.make ~off:req.#body_off ~len:(i16 0)
    else if in_buffer
    then Span.make ~off:req.#body_off ~len:(i16 body_len)
    else Span.make ~off:(i16 0) ~len:(i16 (-1)))
;;

let[@zero_alloc] body_bytes_needed ~(len : int16#) (req : t @ local) : int16# =
  if req.#is_chunked
  then i16 (-1)
  else (
    let #(body_len, body_end, in_buffer) = body_bounds ~len req in
    if body_len < 0 || in_buffer
    then i16 0
    else i16 (Stdlib.min 32767 (body_end - to_int len)))
;;

let[@inline] write_request_line dst ~off ~meth ~target version =
  let off = Buf_write.string dst ~off meth in
  let off = Buf_write.char dst ~off ' ' in
  let off = Buf_write.string dst ~off target in
  let off = Buf_write.char dst ~off ' ' in
  let off = Buf_write.string dst ~off (Version.to_string version) in
  Buf_write.crlf dst ~off
;;

let pp_with_buf (buf : bytes) fmt (req : t) =
  Stdlib.Format.fprintf
    fmt
    "%s %s %s"
    (Method.to_string req.#meth)
    (Span.to_string buf req.#target)
    (Version.to_string req.#version)
;;

let pp fmt (req : t) =
  Stdlib.Format.fprintf
    fmt
    "#{ meth = %a; target = #{ off = %d; len = %d }; version = %a; body_off = %d; \
     content_length = %Ld; is_chunked = %b; keep_alive = %b; connection_upgrade = %b; \
     expect_continue = %b; \
     unsupported_expectation = %b }"
    Method.pp
    req.#meth
    (Span.off req.#target)
    (Span.len req.#target)
    Version.pp
    req.#version
    (to_int req.#body_off)
    (I64.to_int64 req.#content_length)
    req.#is_chunked
    req.#keep_alive
    req.#connection_upgrade
    req.#expect_continue
    req.#unsupported_expectation
;;
