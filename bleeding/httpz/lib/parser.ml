open Base
module I16 = Stdlib_stable.Int16_u
module I32 = Int32_u
module I64 = Stdlib_upstream_compatible.Int64_u
module Char_u = Stdlib_stable.Char_u

exception Parse_error = Err.Parse_error

type pstate =
  #{ buf : bytes
   ; len : int16#
   }

let get_int32 : int32# = I32.of_int32 0x00544547l (* "GET" masked *)
let put_int32 : int32# = I32.of_int32 0x00545550l (* "PUT" masked *)
let method_3byte_mask : int32# = I32.of_int32 0x00FFFFFFl
let post_int32 : int32# = I32.of_int32 0x54534F50l (* "POST" *)
let head_int32 : int32# = I32.of_int32 0x44414548l (* "HEAD" *)
let copy_int32 : int32# = I32.of_int32 0x59504F43l (* "COPY" *)
let lock_int32 : int32# = I32.of_int32 0x4B434F4Cl (* "LOCK" *)
let move_int32 : int32# = I32.of_int32 0x45564F4Dl (* "MOVE" *)
let http11_int64 : int64# = I64.of_int64 0x312E312F50545448L (* "HTTP/1.1" *)
let http10_int64 : int64# = I64.of_int64 0x302E312F50545448L (* "HTTP/1.0" *)
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
;;

let[@inline] skip_while (f : char# -> bool) st ~(pos : int16#) : int16# =
  let mutable p = pos in
  while (not (at_end st ~pos:p)) && f (Buf_read.peek st.#buf p) do
    p <- add16 p one16
  done;
  p
;;

let[@inline] crlf st ~(pos : int16#) : int16# =
  let pos = char #'\r' st ~pos in
  char #'\n' st ~pos
;;

let[@inline] sp st ~(pos : int16#) : int16# = char #' ' st ~pos

(* A token that reaches the end of the buffer has not met its delimiter, so more of it may
   still arrive: "GE" is a prefix of "GET", not an unknown method. Only a token stopped by
   a byte the caller can see is complete enough to judge. *)
let[@inline] token st ~(pos : int16#) : #(Span.t * int16#) =
  let stop =
    i16 (Buf_read.skip_token st.#buf ~pos:(to_int pos) ~limit:(to_int st.#len))
  in
  Err.partial_when (gte16 stop st.#len);
  let sp = Span.make ~off:pos ~len:(sub16 stop pos) in
  Err.malformed_when (Span.len sp = 0);
  #(sp, stop)
;;

let[@inline] ows st ~(pos : int16#) : int16# = skip_while Buf_read.is_space st ~pos

let[@inline] http_version st ~(pos : int16#) : #(Version.t * int16#) =
  let available = to_int (sub16 st.#len pos) in
  (* A short prefix can be partial, but only while every byte received so far
     can still begin an HTTP/1.x version. In particular, the CR in
     ["GET / \r\n"] makes the line invalid immediately. *)
  let prefix = "HTTP/1." in
  let prefix_len = min available (String.length prefix) in
  for i = 0 to prefix_len - 1 do
    Err.when_
      (Buf_read.( <>. )
         (Buf_read.peek st.#buf (add16 pos (i16 i)))
         (Char_u.of_char (String.unsafe_get prefix i)))
      Err.Invalid_version
  done;
  Err.partial_when (available < 8);
  let v64 : int64# = I64.of_int64 (Bytes.unsafe_get_int64 st.#buf (to_int pos)) in
  let new_pos = add16 pos (i16 8) in
  let version =
    if I64.equal v64 http11_int64
    then Version.Http_1_1
    else if I64.equal v64 http10_int64
    then Version.Http_1_0
    else if Span.equal st.#buf (Span.make ~off:pos ~len:(i16 7)) "HTTP/1."
            &&
            match Buf_read.peek st.#buf (add16 pos (i16 7)) with
            | #'2' .. #'9' -> true
            | _ -> false
    then Version.Http_1_1
    else Err.fail Err.Invalid_version
  in
  #(version, new_pos)
;;

let[@inline] parse_method st ~(pos : int16#) : #(Method.t * int16#) =
  let #(sp, pos) = token st ~pos in
  let unknown () =
    Err.fail
      (if Buf_read.( =. ) (Buf_read.peek st.#buf pos) #' '
       then Err.Unsupported_method
       else Err.Invalid_method)
  in
  let len = Span.len sp in
  let off = Span.off sp in
  let meth =
    match len with
    | 3 ->
      let v : int32# =
        I32.bit_and (I32.of_int32 (Bytes.unsafe_get_int32 st.#buf off)) method_3byte_mask
      in
      if I32.equal v get_int32
      then Method.Get
      else if I32.equal v put_int32
      then Method.Put
      else unknown ()
    | 4 ->
      let v : int32# = I32.of_int32 (Bytes.unsafe_get_int32 st.#buf off) in
      if I32.equal v post_int32
      then Method.Post
      else if I32.equal v head_int32
      then Method.Head
      else if I32.equal v copy_int32
      then Method.Copy
      else if I32.equal v lock_int32
      then Method.Lock
      else if I32.equal v move_int32
      then Method.Move
      else unknown ()
    | 5 ->
      if Span.equal st.#buf sp "PATCH"
      then Method.Patch
      else if Span.equal st.#buf sp "TRACE"
      then Method.Trace
      else if Span.equal st.#buf sp "MKCOL"
      then Method.Mkcol
      else unknown ()
    | 6 ->
      if Span.equal st.#buf sp "DELETE"
      then Method.Delete
      else if Span.equal st.#buf sp "REPORT"
      then Method.Report
      else if Span.equal st.#buf sp "UNLOCK"
      then Method.Unlock
      else unknown ()
    | 7 ->
      if Span.equal st.#buf sp "OPTIONS"
      then Method.Options
      else if Span.equal st.#buf sp "CONNECT"
      then Method.Connect
      else unknown ()
    | 8 -> if Span.equal st.#buf sp "PROPFIND" then Method.Propfind else unknown ()
    | 9 -> if Span.equal st.#buf sp "PROPPATCH" then Method.Proppatch else unknown ()
    | _ -> unknown ()
  in
  #(meth, pos)
;;

(* [Scan.find_sp_or_cr] only finds where the target ends; it says nothing about the bytes
   in between, which is how a control character or a truncated percent-triplet would
   otherwise reach the router. [Target.parse] checks them against the RFC 3986 grammar and
   reports rather than raises, so the rejection is made here.

   The split is returned along with the span. Every caller that dispatches on a path or a
   query needs it, and it is already computed. *)
(* RFC 9112 §3.2.3 confines authority-form to CONNECT, §3.2.4 confines asterisk-form to
   OPTIONS, and §3.2 gives CONNECT no other form. Accepting a form the method does not
   define lets a request mean one thing to httpz and another to a proxy in front of it. *)
let[@inline] form_allows (meth : Method.t) (form : Target.form) =
  match form, meth with
  | Target.Origin, Method.Connect | Target.Absolute, Method.Connect -> false
  | Target.Origin, _ | Target.Absolute, _ -> true
  | Target.Authority, Method.Connect -> true
  | Target.Asterisk, Method.Options -> true
  | (Target.Authority | Target.Asterisk | Target.Invalid), _ -> false
;;

let[@inline] parse_target
  st
  ~(pos : int16#)
  ~(meth : Method.t)
  ~(limits : Buf_read.limits)
  : #(Span.t * Target.t * int16#)
  =
  let stop = i16 (Scan.find_sp_or_cr st.#buf ~pos:(to_int pos) ~limit:(to_int st.#len)) in
  let sp = Span.make ~off:pos ~len:(sub16 stop pos) in
  (* Bound the length first: the target is already over budget whether or not the rest of
     it has arrived, so there is nothing to wait for. *)
  Err.when_ (I16.compare (sub16 stop pos) limits.#max_target_length > 0) Err.Uri_too_long;
  (* No SP or CR yet means the target is still arriving. A prefix of a valid target need
     not itself be valid — "/a%4" is a truncated triplet, not a bad one, and the empty
     prefix is not an empty target — so judging it here would fail any request whose read
     split inside the request line. *)
  Err.partial_when (gte16 stop st.#len);
  Err.when_ (Span.len sp = 0) Err.Invalid_target;
  let target = Target.parse st.#buf sp in
  Err.when_ (not (Target.is_valid target)) Err.Invalid_target;
  Err.when_ (not (form_allows meth (Target.form target))) Err.Invalid_target;
  #(sp, target, stop)
;;

let[@inline] request_line st ~(pos : int16#) ~(limits : Buf_read.limits)
  : #(Method.t * Span.t * Target.t * Version.t * int16#)
  =
  (* RFC 9112 section 2.2 recommends that a server ignore at least one empty line before a
     request-line. Skip the complete leading CRLF sequence, but keep a lone trailing CR
     partial so a fragmented empty line is retried. *)
  let rec skip_empty_lines count pos =
    if at_end st ~pos
    then pos
    else if Buf_read.( <>. ) (Buf_read.peek st.#buf pos) #'\r'
    then pos
    else (
      Err.partial_when (to_int (sub16 st.#len pos) < 2);
      if Buf_read.( =. ) (Buf_read.peek st.#buf (add16 pos one16)) #'\n'
      then begin
        Err.when_ (count >= 8) Err.Invalid_method;
        skip_empty_lines (count + 1) (add16 pos (i16 2))
      end
      else pos)
  in
  let pos = skip_empty_lines 0 pos in
  let #(meth, pos) = parse_method st ~pos in
  let pos = sp st ~pos in
  let #(target, target_parsed, pos) = parse_target st ~pos ~meth ~limits in
  let pos = sp st ~pos in
  let #(version, pos) = http_version st ~pos in
  let pos = crlf st ~pos in
  #(meth, target, target_parsed, version, pos)
;;

(* The status code is exactly three digits (RFC 9112 s4). The reason phrase is
   informational bytes up to CRLF and may be empty; some servers also omit the SP that
   precedes it, which every deployed client accepts, so it is accepted here. A bare CR
   inside the phrase is refused as it is in a header value. *)
let[@inline] status_line st ~(pos : int16#) : #(Version.t * int16# * Span.t * int16#) =
  let #(version, pos) = http_version st ~pos in
  let pos = sp st ~pos in
  Err.partial_when (to_int (sub16 st.#len pos) < 3);
  let d1 = Buf_read.digit_value (Buf_read.peek st.#buf pos) in
  let d2 = Buf_read.digit_value (Buf_read.peek st.#buf (add16 pos one16)) in
  let d3 = Buf_read.digit_value (Buf_read.peek st.#buf (add16 pos (i16 2))) in
  Err.when_ (d1 < 0 || d2 < 0 || d3 < 0) Err.Invalid_status;
  let code = i16 ((d1 * 100) + (d2 * 10) + d3) in
  let pos = add16 pos (i16 3) in
  Err.partial_when (at_end st ~pos);
  (* A fourth digit means the code was not three digits; refuse it rather than truncate
     "2000" to 200. *)
  Err.when_ (Buf_read.digit_value (Buf_read.peek st.#buf pos) >= 0) Err.Invalid_status;
  let pos =
    if Buf_read.( =. ) (Buf_read.peek st.#buf pos) #' ' then add16 pos one16 else pos
  in
  let #(crlf_pos, has_bare_cr) =
    Buf_read.find_crlf_check_bare_cr st.#buf ~pos ~len:st.#len
  in
  Err.partial_when (to_int crlf_pos < 0);
  Err.when_ has_bare_cr Err.Bare_cr_detected;
  Err.when_
    (not (Buf_read.valid_field_value st.#buf ~pos ~len:crlf_pos))
    Err.Invalid_status;
  let reason = Span.make ~off:pos ~len:(sub16 crlf_pos pos) in
  let pos = add16 crlf_pos (i16 2) in
  #(version, code, reason, pos)
;;

let[@inline] parse_header st ~(pos : int16#)
  : #(Header_name.t * Span.t * Span.t * int16#)
  =
  let #(name_span, pos) = token st ~pos in
  let pos = char #':' st ~pos in
  let pos = ows st ~pos in
  let value_start = pos in
  let #(first_crlf, first_has_bare_cr) =
    Buf_read.find_crlf_check_bare_cr st.#buf ~pos ~len:st.#len
  in
  Err.partial_when (to_int first_crlf < 0);
  (* RFC 9112 section 5.2 requires clients to unfold obsolete response field lines before
     interpreting them. The same normalization is an allowed server-side recovery, so
     doing it in the shared parser keeps framing fields and ordinary fields on one path.

     Do not mutate until the complete folded field is present. A caller retries [parse]
     after [Partial], and changing a CRLF into spaces before then would make that retry
     see a different grammar. *)
  let mutable crlf_pos = first_crlf in
  let mutable has_bare_cr = first_has_bare_cr in
  let mutable scanning = true in
  while scanning do
    let next = add16 crlf_pos (i16 2) in
    Err.partial_when (at_end st ~pos:next);
    if Buf_read.is_space (Buf_read.peek st.#buf next)
    then (
      let #(next_crlf, next_has_bare_cr) =
        Buf_read.find_crlf_check_bare_cr st.#buf ~pos:next ~len:st.#len
      in
      Err.partial_when (to_int next_crlf < 0);
      has_bare_cr <- has_bare_cr || next_has_bare_cr;
      crlf_pos <- next_crlf)
    else scanning <- false
  done;
  (* Replace each CRLF plus the continuation's leading whitespace with SP. The resulting
     value remains one span into the caller's buffer. *)
  let mutable fold = first_crlf in
  while I16.compare fold crlf_pos < 0 do
    Bytes.unsafe_set st.#buf (to_int fold) ' ';
    Bytes.unsafe_set st.#buf (to_int (add16 fold one16)) ' ';
    let mutable part = add16 fold (i16 2) in
    while
      I16.compare part crlf_pos < 0 && Buf_read.is_space (Buf_read.peek st.#buf part)
    do
      Bytes.unsafe_set st.#buf (to_int part) ' ';
      part <- add16 part one16
    done;
    let #(next_crlf, _) =
      Buf_read.find_crlf_check_bare_cr st.#buf ~pos:part ~len:st.#len
    in
    fold <- next_crlf
  done;
  (* Do not make safety depend on the low-level caller remembering to inspect
     the returned flag.  Message parsers still report the more specific
     Bare_cr_detected status because this is the status raised here. *)
  Err.when_ has_bare_cr Err.Bare_cr_detected;
  Err.when_
    (not (Buf_read.valid_field_value st.#buf ~pos:value_start ~len:crlf_pos))
    Err.Invalid_header;
  let mutable value_end = crlf_pos in
  while
    I16.compare value_end value_start > 0
    && Buf_read.is_space (Buf_read.peek st.#buf (sub16 value_end one16))
  do
    value_end <- sub16 value_end one16
  done;
  let value_span = Span.make ~off:value_start ~len:(sub16 value_end value_start) in
  let pos = add16 crlf_pos (i16 2) in
  let name = Header_name.of_span st.#buf name_span in
  #(name, name_span, value_span, pos)
;;

(* A lone CR is the start of the terminating CRLF as readily as it is the start of nothing
   at all, so fewer than two bytes is not an answer of [false] but no answer yet. *)
let[@inline] is_headers_end st ~(pos : int16#) : bool =
  Err.partial_when (to_int (sub16 st.#len pos) < 2);
  Buf_read.( =. ) (Buf_read.peek st.#buf pos) #'\r'
  && Buf_read.( =. ) (Buf_read.peek st.#buf (add16 pos one16)) #'\n'
;;

let[@inline] end_headers st ~(pos : int16#) : int16# = crlf st ~pos

(* Connection and Content-Length folding is identical for requests and responses, so both
   message parsers share the state carried here. *)
type conn_value =
  | Conn_default
  | Conn_close
  | Conn_keep_alive

(* "close" wins over "keep-alive" whichever order the two arrive in, and once seen it
   cannot be withdrawn by a later field line. *)
let[@inline] parse_connection_value (local_ (buf : bytes)) value_span ~default =
  Err.when_ (not (Span.token_list_valid buf value_span)) Err.Invalid_header;
  if Span.token_list_contains buf value_span "close"
  then Conn_close
  else if phys_equal default Conn_close
  then Conn_close
  else if Span.token_list_contains buf value_span "keep-alive"
  then Conn_keep_alive
  else default
;;

let[@inline] content_length_value
  (local_ (buf : bytes))
  value_span
  ~has_cl
  ~(current : int64#)
  ~(max_content_length : int64#)
  : int64#
  =
  let #(parsed_len, overflow, conflicting) = Span.parse_content_length buf value_span in
  Err.when_ overflow Err.Content_length_overflow;
  Err.when_ conflicting Err.Ambiguous_framing;
  Err.when_ (I64.compare parsed_len #0L < 0) Err.Invalid_header;
  Err.when_ (has_cl && not (I64.equal parsed_len current)) Err.Ambiguous_framing;
  Err.when_ (I64.compare parsed_len max_content_length > 0) Err.Content_length_overflow;
  parsed_len
;;
