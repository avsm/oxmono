(* req.ml - HTTP request type *)

open Base

module I16 = Stdlib_stable.Int16_u
module I64 = Stdlib_upstream_compatible.Int64_u

(* int16# conversion and arithmetic helpers *)
let[@inline always] i16 x = I16.of_int x
let[@inline always] to_int x = I16.to_int x

type t =
  #{ meth : Method.t
   ; target : Span.t
   ; version : Version.t
   ; body_off : int16#
   ; content_length : int64#
   ; is_chunked : bool
   ; keep_alive : bool
   ; expect_continue : bool
   }

(* Helper to get body length and end position for non-chunked requests.
   Returns None if content_length <= 0, Some (body_len, body_end) otherwise. *)
let[@inline] body_bounds ~(len : int16#) (req : t @ local) =
  let cl = req.#content_length in
  let buf_len = to_int len in
  if I64.compare cl #0L <= 0 then None
  else
    let body_len = I64.to_int cl in
    let body_end = to_int req.#body_off + body_len in
    Some (body_len, body_end, body_end <= buf_len)
;;

let body_in_buffer ~(len : int16#) (req : t @ local) =
  if req.#is_chunked then false
  else match body_bounds ~len req with
    | None -> true
    | Some (_, _, in_buffer) -> in_buffer
;;

let body_span ~(len : int16#) (req : t @ local) =
  if req.#is_chunked then Span.make ~off:(i16 0) ~len:(i16 (-1))
  else match body_bounds ~len req with
    | None -> Span.make ~off:req.#body_off ~len:(i16 0)
    | Some (body_len, _, true) -> Span.make ~off:req.#body_off ~len:(i16 body_len)
    | Some (_, _, false) -> Span.make ~off:(i16 0) ~len:(i16 (-1))
;;

let body_bytes_needed ~(len : int16#) (req : t @ local) : int16# =
  if req.#is_chunked then i16 (-1)
  else match body_bounds ~len req with
    | None -> i16 0
    | Some (_, _, true) -> i16 0
    | Some (_, body_end, false) -> i16 (body_end - to_int len)
;;

let pp_with_buf (buf : bytes) fmt (req : t) =
  Stdlib.Format.fprintf fmt "%s %s %s"
    (Method.to_string req.#meth)
    (Span.to_string buf req.#target)
    (Version.to_string req.#version)
;;

let pp fmt (req : t) =
  Stdlib.Format.fprintf fmt
    "#{ meth = %a; target = #{ off = %d; len = %d }; version = %a; body_off = %d; content_length = %Ld; is_chunked = %b; keep_alive = %b; expect_continue = %b }"
    Method.pp req.#meth
    (Span.off req.#target) (Span.len req.#target)
    Version.pp req.#version
    (to_int req.#body_off)
    (I64.to_int64 req.#content_length)
    req.#is_chunked
    req.#keep_alive
    req.#expect_continue
;;
