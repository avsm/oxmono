(* parser.ml - HTTP/1.1 request parser *)

open Base

module I16 = Stdlib_stable.Int16_u
module I32 = Int32_u
module I64 = Stdlib_upstream_compatible.Int64_u

exception Parse_error = Err.Parse_error

type pstate = #{ buf : bytes; len : int16# }

(* Method constants as unboxed int32# for zero-alloc comparison (little-endian) *)
let get_int32 : int32# = I32.of_int32 0x00544547l  (* "GET" masked *)
let put_int32 : int32# = I32.of_int32 0x00545550l  (* "PUT" masked *)
let method_3byte_mask : int32# = I32.of_int32 0x00FFFFFFl
let post_int32 : int32# = I32.of_int32 0x54534F50l (* "POST" *)
let head_int32 : int32# = I32.of_int32 0x44414548l (* "HEAD" *)

(* HTTP version as unboxed int64# for zero-alloc comparison (little-endian) *)
let http11_int64 : int64# = I64.of_int64 0x312E312F50545448L (* "HTTP/1.1" *)
let http10_int64 : int64# = I64.of_int64 0x302E312F50545448L (* "HTTP/1.0" *)

(* int16# arithmetic helpers *)
let[@inline always] add16 a b = I16.add a b
let[@inline always] sub16 a b = I16.sub a b
let[@inline always] gte16 a b = I16.compare a b >= 0
let[@inline always] i16 x = I16.of_int x
let[@inline always] to_int x = I16.to_int x
let one16 : int16# = i16 1

let[@inline] make buf ~(len : int16#) : pstate = #{ buf; len }

let[@inline] at_end st ~(pos : int16#) = gte16 pos st.#len

let[@inline] char (c : char#) st ~(pos : int16#) : int16# =
  Err.partial_when @@ at_end st ~pos;
  Err.malformed_when @@ Buf_read.( <>. ) (Buf_read.peek st.#buf pos) c;
  add16 pos one16

let[@inline] take_while (f : char# -> bool) st ~(pos : int16#) : #(Span.t * int16#) =
  let start = pos in
  let mutable p = pos in
  while not (at_end st ~pos:p) && f (Buf_read.peek st.#buf p) do
    p <- add16 p one16
  done;
  #(Span.make ~off:start ~len:(sub16 p start), p)

let[@inline] skip_while (f : char# -> bool) st ~(pos : int16#) : int16# =
  let mutable p = pos in
  while not (at_end st ~pos:p) && f (Buf_read.peek st.#buf p) do
    p <- add16 p one16
  done;
  p

(* ----- HTTP-Specific Parsing ----- *)

let[@inline] crlf st ~(pos : int16#) : int16# =
  let pos = char #'\r' st ~pos in
  char #'\n' st ~pos

let[@inline] sp st ~(pos : int16#) : int16# =
  char #' ' st ~pos

let[@inline] token st ~(pos : int16#) : #(Span.t * int16#) =
  let #(sp, pos) = take_while Buf_read.is_token_char st ~pos in
  Err.malformed_when (Span.len sp = 0);
  #(sp, pos)

let[@inline] ows st ~(pos : int16#) : int16# =
  skip_while Buf_read.is_space st ~pos

let[@inline] http_version st ~(pos : int16#) : #(Version.t * int16#) =
  Err.partial_when (to_int (sub16 st.#len pos) < 8);
  let v64 : int64# = I64.of_int64 (Bytes.unsafe_get_int64 st.#buf (to_int pos)) in
  let new_pos = add16 pos (i16 8) in
  let version =
    if I64.equal v64 http11_int64 then Version.Http_1_1
    else if I64.equal v64 http10_int64 then Version.Http_1_0
    else Err.fail Err.Invalid_version
  in
  #(version, new_pos)

let[@inline] parse_method st ~(pos : int16#) : #(Method.t * int16#) =
  let #(sp, pos) = token st ~pos in
  let len = Span.len sp in
  let off = Span.off sp in
  let meth = match len with
  | 3 ->
    let v : int32# = I32.bit_and (I32.of_int32 (Bytes.unsafe_get_int32 st.#buf off)) method_3byte_mask in
    if I32.equal v get_int32 then Method.Get
    else if I32.equal v put_int32 then Method.Put
    else Err.fail Err.Invalid_method
  | 4 ->
    let v : int32# = I32.of_int32 (Bytes.unsafe_get_int32 st.#buf off) in
    if I32.equal v post_int32 then Method.Post
    else if I32.equal v head_int32 then Method.Head
    else Err.fail Err.Invalid_method
  | 5 ->
    if Span.equal st.#buf sp "PATCH" then Method.Patch
    else if Span.equal st.#buf sp "TRACE" then Method.Trace
    else Err.fail Err.Invalid_method
  | 6 ->
    if Span.equal st.#buf sp "DELETE" then Method.Delete
    else Err.fail Err.Invalid_method
  | 7 ->
    if Span.equal st.#buf sp "OPTIONS" then Method.Options
    else if Span.equal st.#buf sp "CONNECT" then Method.Connect
    else Err.fail Err.Invalid_method
  | _ -> Err.fail Err.Invalid_method
  in
  #(meth, pos)

let[@inline] parse_target st ~(pos : int16#) : #(Span.t * int16#) =
  let #(sp, pos) = take_while (fun c ->
    Buf_read.( <>. ) c #' ' && Buf_read.( <>. ) c #'\r') st ~pos
  in
  Err.when_ (Span.len sp = 0) Err.Invalid_target;
  #(sp, pos)

let[@inline] request_line st ~(pos : int16#) : #(Method.t * Span.t * Version.t * int16#) =
  let #(meth, pos) = parse_method st ~pos in
  let pos = sp st ~pos in
  let #(target, pos) = parse_target st ~pos in
  let pos = sp st ~pos in
  let #(version, pos) = http_version st ~pos in
  let pos = crlf st ~pos in
  #(meth, target, version, pos)

let[@inline] parse_header st ~(pos : int16#) : #(Header_name.t * Span.t * Span.t * int16# * bool) =
  let #(name_span, pos) = token st ~pos in
  let pos = char #':' st ~pos in
  let pos = ows st ~pos in
  let value_start = pos in
  let #(crlf_pos, has_bare_cr) = Buf_read.find_crlf_check_bare_cr st.#buf ~pos ~len:st.#len in
  Err.partial_when (to_int crlf_pos < 0);
  let mutable value_end = crlf_pos in
  while I16.compare value_end value_start > 0 &&
        Buf_read.is_space (Buf_read.peek st.#buf (sub16 value_end one16)) do
    value_end <- sub16 value_end one16
  done;
  let value_span = Span.make ~off:value_start ~len:(sub16 value_end value_start) in
  let pos = add16 crlf_pos (i16 2) in
  let name = Header_name.of_span st.#buf name_span in
  #(name, name_span, value_span, pos, has_bare_cr)

let[@inline] is_headers_end st ~(pos : int16#) : bool =
  if to_int (sub16 st.#len pos) < 2 then false
  else
    Buf_read.( =. ) (Buf_read.peek st.#buf pos) #'\r' &&
    Buf_read.( =. ) (Buf_read.peek st.#buf (add16 pos one16)) #'\n'

let[@inline] end_headers st ~(pos : int16#) : int16# =
  crlf st ~pos
