(* target.ml - RFC 9112 request-target parsing *)

open Base

module I16 = Stdlib_stable.Int16_u
module Char_u = Stdlib_stable.Char_u
module Raw = Uriz.Raw

let[@inline] i16 x = I16.of_int x

(* Unboxed char helpers *)
let[@inline always] peek buf pos = Buf_read.peek buf (i16 pos)
let[@inline always] peek_str s i = Char_u.of_char (String.unsafe_get s i)
let ( =. ) = Buf_read.( =. )
let ( <>. ) = Buf_read.( <>. )

type form =
  | Origin
  | Absolute
  | Authority
  | Asterisk
  | Invalid

type t =
  #{ form : form
   ; path : Span.t
   ; query : Span.t
   ; scheme : Span.t
   ; host : Span.t
   ; port : int
   ; err : int
   }

let empty_span : Span.t = Span.make ~off:(i16 0) ~len:(i16 0)

(* [Span.t] offsets are signed 16-bit, so every offset handed to [span] must
   have come from inside the parse buffer, whose length {!Buf_read.buffer_size}
   bounds at 32768. The [-1] that {!Uriz.Raw} uses for an absent component is
   filtered out at each use site rather than being narrowed here. *)
let[@inline always] span ~off ~len : Span.t = Span.make ~off:(i16 off) ~len:(i16 len)

let[@inline always] invalid ~err : t =
  #{ form = Invalid
   ; path = empty_span
   ; query = empty_span
   ; scheme = empty_span
   ; host = empty_span
   ; port = -1
   ; err
   }
;;

(* ----- Origin-form ----- *)

(* "//x" is an absolute-path with an empty first segment, but RFC 3986 reads a
   leading "//" as an authority. Handing [parse_sub] a window that begins at
   the last of the leading slashes keeps it on the path grammar; the slashes
   dropped from the window are all legal empty segments, so nothing about the
   remaining bytes changes, and the span returned to the caller is widened back
   over them. *)
let[@inline] parse_origin (local_ buf : bytes) (local_ s : string) ~toff ~tlen : t =
  let mutable k = 1 in
  while k < tlen && peek buf (toff + k) =. #'/' do
    k <- k + 1
  done;
  let skip = k - 1 in
  let sp = Raw.parse_sub s ~pos:(toff + skip) ~len:(tlen - skip) in
  (* [frag_off] is the offset of the fragment text; the byte at fault is the
     ['#'] before it. *)
  if sp.#err <> 0 then invalid ~err:(sp.#err - 1)
  else if sp.#frag_off >= 0 then invalid ~err:(sp.#frag_off - 1)
  else (
    let path = span ~off:toff ~len:(sp.#path_off + sp.#path_len - toff) in
    let query =
      if sp.#query_off < 0 then empty_span
      else span ~off:sp.#query_off ~len:sp.#query_len
    in
    #{ form = Origin
     ; path
     ; query
     ; scheme = empty_span
     ; host = empty_span
     ; port = -1
     ; err = -1
     })
;;

(* ----- Absolute-form ----- *)

let[@inline] parse_absolute ~toff (sp : Raw.spans) : t =
  (* A userinfo in a request-target is deprecated by RFC 9110 §4.2.4 and is a
     spoofing vector for anything that logs or re-issues the target, so it is
     refused rather than ignored. *)
  if sp.#userinfo_off >= 0 then invalid ~err:sp.#userinfo_off
  else if sp.#frag_off >= 0 then invalid ~err:(sp.#frag_off - 1)
  else if sp.#port_val > 65535 then invalid ~err:sp.#port_off
  else (
    let query =
      if sp.#query_off < 0 then empty_span
      else span ~off:sp.#query_off ~len:sp.#query_len
    in
    #{ form = Absolute
     ; path = span ~off:sp.#path_off ~len:sp.#path_len
     ; query
     ; scheme = span ~off:toff ~len:sp.#scheme_len
     ; host = span ~off:sp.#host_off ~len:sp.#host_len
     ; port = sp.#port_val
     ; err = -1
     })
;;

(* ----- Authority-form ----- *)

(* [host:port] is not a URI-reference: with a reg-name it parses as a scheme
   followed by a rootless path, and with an IPv4 literal or an IP-literal it
   does not parse at all. The grammar is small enough to take directly. *)

(* reg-name = *( unreserved / pct-encoded / sub-delims ). Returns the offset
   just past the name, or [-1] on a byte that starts nothing legal. *)
let[@inline] reg_name_end (local_ s : string) ~pos ~limit : int =
  let mutable i = pos in
  let mutable go = true in
  let mutable bad = false in
  while go && i < limit do
    let c = String.unsafe_get s i in
    if Raw.is_unreserved c || Raw.is_sub_delim c
    then i <- i + 1
    else if Char.equal c '%'
    then
      if i + 2 < limit
         && Raw.is_hexdig (String.unsafe_get s (i + 1))
         && Raw.is_hexdig (String.unsafe_get s (i + 2))
      then i <- i + 3
      else (
        bad <- true;
        go <- false)
    else go <- false
  done;
  if bad then -1 else i
;;

(* The URI host shared by authority-form and Host field parsing. The span of
   an IP-literal excludes its brackets, matching {!Uriz.Raw}. [after] is the
   first byte after the host, or [-1] when the host is malformed. *)
let[@inline] parse_host (local_ s : string) ~pos ~limit =
  if Char.equal (String.unsafe_get s pos) '['
  then (
    let e6 = Raw.ipv6_end s (pos + 1) limit in
    let e =
      if e6 >= 0 && e6 < limit && Char.equal (String.unsafe_get s e6) ']'
      then e6
      else (
        let ef = Raw.ipvfuture_end s (pos + 1) limit in
        if ef >= 0 && ef < limit && Char.equal (String.unsafe_get s ef) ']'
        then ef
        else -1)
    in
    if e < 0 then #(0, 0, -1) else #(pos + 1, e - pos - 1, e + 1))
  else (
    let e = reg_name_end s ~pos ~limit in
    if e <= pos then #(0, 0, -1) else #(pos, e - pos, e))
;;

(* Scan the non-empty decimal port shared by authority-form and Host. The
   result is [#(stop, value)], with a negative value for malformed input. *)
let[@inline] parse_port (local_ s : string) ~pos ~limit =
  let mutable i = pos in
  let mutable value = 0 in
  let mutable valid = true in
  while valid && i < limit do
    let digit = Char.to_int (String.unsafe_get s i) - 48 in
    if digit < 0 || digit > 9 || value > 6553
    then valid <- false
    else (
      value <- (value * 10) + digit;
      i <- i + 1)
  done;
  #(i, if valid && i > pos && value <= 65535 then value else -1)
;;

let[@inline] parse_authority (local_ s : string) ~toff ~tlen : t =
  let limit = toff + tlen in
  (* [#(host_off, host_len, after)]: [after] is the offset of the byte that
     must be the ':', or [-1] when the host itself is malformed. An IP-literal
     reports the span inside the brackets, as {!Uriz.Raw} does. *)
  let #(host_off, host_len, after) = parse_host s ~pos:toff ~limit in
  if after < 0
  then invalid ~err:toff
  else if after >= limit || not (Char.equal (String.unsafe_get s after) ':')
  then invalid ~err:after
  else (
    (* port = 1*DIGIT. RFC 3986 allows it to be empty but RFC 9112 §3.2.3
       requires it here, and a port above 65535 addresses nothing. *)
    let #(port_end, port) = parse_port s ~pos:(after + 1) ~limit in
    if port < 0
    then invalid ~err:port_end
    else
      #{ form = Authority
       ; path = empty_span
       ; query = empty_span
       ; scheme = empty_span
       ; host = span ~off:host_off ~len:host_len
       ; port
       ; err = -1
       })
;;

(* ----- Entry point ----- *)

let asterisk_form : t =
  #{ form = Asterisk
   ; path = empty_span
   ; query = empty_span
   ; scheme = empty_span
   ; host = empty_span
   ; port = -1
   ; err = -1
   }
;;

(* [Stdlib.Bytes.unsafe_to_string]: every scanner reached from here reads
   [buf] and none writes to it, so the alias cannot observe a mutation of an
   immutable string. {!Range.parse_string} takes the same view in reverse. *)
(* [opt]: allocation-free only once [Span.make] and the [Uriz.Raw] scanners are
   inlined, which the dev profile's [-opaque] prevents. Checked in
   release/optimized builds, as {!Req.body_span} is. *)
let[@zero_alloc opt] parse (local_ buf : bytes) (target : Span.t) : t =
  let toff = Span.off target in
  let tlen = Span.len target in
  if tlen = 0
  then invalid ~err:toff
  else (
    let local_ s = Stdlib.Bytes.unsafe_to_string buf in
    let c0 = peek buf toff in
    (* [s] borrows the enclosing region, so each call below is bound rather
       than left in tail position. The results are unboxed. *)
    if c0 =. #'/'
    then (
      let r = parse_origin buf s ~toff ~tlen in
      r)
    else if tlen = 1 && c0 =. #'*'
    then asterisk_form
    else (
      let sp = Raw.parse_sub s ~pos:toff ~len:tlen in
      (* "host:8080" is a URI-reference too — a scheme with a rootless path —
         so a scheme alone does not make a target absolute-form. Requiring a
         non-empty authority separates the two, and rejects the "http:/x" and
         "http://" that the RFC 3986 grammar admits but no origin server can
         serve. *)
      if sp.#err = 0 && sp.#scheme_len >= 0 && sp.#host_off >= 0 && sp.#host_len > 0
      then parse_absolute ~toff sp
      else (
        let r = parse_authority s ~toff ~tlen in
        r)))
;;

let[@inline] form (t : t) = t.#form
let[@inline] path (t : t) = t.#path
let[@inline] query (t : t) = t.#query
let[@inline] scheme (t : t) = t.#scheme
let[@inline] host (t : t) = t.#host
let[@inline] port (t : t) = t.#port
let[@inline] has_query (t : t) = Span.len t.#query > 0

let[@inline] is_valid (t : t) =
  match t.#form with
  | Invalid -> false
  | Origin | Absolute | Authority | Asterisk -> true
;;

let[@inline] error_offset (t : t) = t.#err

(* Host = uri-host [ ":" port ]. Keep this beside the request-target
   authority parser so the two uses of RFC 3986 cannot drift apart. A comma is
   rejected even though it is a URI sub-delimiter: Host is not a list field,
   and accepting a comma-separated-looking value creates routing ambiguity
   across intermediaries. *)
let valid_host (local_ buf : bytes) (value : Span.t) =
  let off = Span.off value in
  let limit = off + Span.len value in
  let local_ s = Stdlib.Bytes.unsafe_to_string buf in
  if off = limit
  then true
  else (
    let mutable comma = false in
    let mutable i = off in
    while not comma && i < limit do
      comma <- Char.equal (String.unsafe_get s i) ',';
      i <- i + 1
    done;
    if comma
    then false
    else
      let #(_host_off, host_len, after) = parse_host s ~pos:off ~limit in
      if host_len <= 0
      then false
      else if after = limit
      then true
      else if after < limit && Char.equal (String.unsafe_get s after) ':'
      then (
        let #(port_end, port) = parse_port s ~pos:(after + 1) ~limit in
        port_end = limit && port >= 0)
      else false)
;;

(** {1 Path Segment Matching} *)

(** Match literal segment. Returns #(matched, remaining). *)
let[@inline] match_segment (local_ buf : bytes) (path : Span.t) (expected : string) : #(bool * Span.t) =
  let plen = Span.len path in
  if plen = 0 then #(false, empty_span)
  else
    let poff = Span.off path in
    let elen = String.length expected in
    (* Find next '/' *)
    let mutable i = 0 in
    while i < plen && peek buf (poff + i) <>. #'/' do
      i <- i + 1
    done;
    (* Check if segment matches *)
    if i <> elen then #(false, empty_span)
    else (
      (* Manual char-by-char comparison using unboxed chars *)
      let mutable j = 0 in
      let mutable eq = true in
      while eq && j < elen do
        if not (peek buf (poff + j) =. peek_str expected j)
        then eq <- false
        else j <- j + 1
      done;
      if not eq then #(false, empty_span)
      else
        (* Skip the '/' separator if present *)
        let rest_off = if i < plen then poff + i + 1 else poff + i in
        let rest_len = if i < plen then plen - i - 1 else 0 in
        #(true, Span.make ~off:(i16 rest_off) ~len:(i16 rest_len)))
;;

(** Match any segment (capture). Returns #(matched, segment, remaining). *)
let[@inline] match_param (local_ buf : bytes) (path : Span.t) : #(bool * Span.t * Span.t) =
  let plen = Span.len path in
  if plen = 0 then #(false, empty_span, empty_span)
  else
    let poff = Span.off path in
    (* Find next '/' *)
    let mutable i = 0 in
    while i < plen && peek buf (poff + i) <>. #'/' do
      i <- i + 1
    done;
    let seg = Span.make ~off:(i16 poff) ~len:(i16 i) in
    let rest_off = if i < plen then poff + i + 1 else poff + i in
    let rest_len = if i < plen then plen - i - 1 else 0 in
    let rest = Span.make ~off:(i16 rest_off) ~len:(i16 rest_len) in
    #(true, seg, rest)
;;

(** Check if path is empty (complete match). *)
let[@inline] is_empty (path : Span.t) : bool = Span.len path = 0

(** {1 Query Parameter Lookup}

    The cursor below mirrors {!Uriz.query_step}: one parameter runs to the next
    ['&'] or to the end of the query, and the first ['='] inside it splits key
    from value. A trailing ['&'] therefore introduces one final parameter with
    an empty key, exactly as uriz reports it.

    Where uriz distinguishes an absent value ([-1]) from a present empty one,
    the span here is zero-length in both cases: a [Span.t] has no offset that
    means "absent", and every offset it can hold is one a caller may read
    through. This matches {!Uriz.find_query}, which also collapses a parameter
    with no ['='] to [""]. *)

(* Boundaries of the parameter starting at [pos], as
   [#(key, value, next)]. [next] is [-1] once the query is exhausted. *)
let[@inline] step (local_ buf : bytes) (query : Span.t) ~pos : #(Span.t * Span.t * int) =
  let qend = Span.off query + Span.len query in
  let mutable e = pos in
  while e < qend && peek buf e <>. #'&' do
    e <- e + 1
  done;
  let mutable eq = pos in
  while eq < e && peek buf eq <>. #'=' do
    eq <- eq + 1
  done;
  let next = if e < qend then e + 1 else -1 in
  if eq < e
  then #(span ~off:pos ~len:(eq - pos), span ~off:(eq + 1) ~len:(e - eq - 1), next)
  else #(span ~off:pos ~len:(e - pos), span ~off:e ~len:0, next)
;;

(* [-1] for an empty query, so that "" and "?" yield no parameters at all. *)
let[@inline] cursor (query : Span.t) = if Span.len query = 0 then -1 else Span.off query

(** Find query parameter by name. Returns #(found, value_span). *)
let find_query_param (local_ buf : bytes) (query : Span.t) (name : string) : #(bool * Span.t) =
  let mutable pos = cursor query in
  let mutable found = false in
  let mutable value = empty_span in
  while pos >= 0 do
    let #(k, v, next) = step buf query ~pos in
    if Span.equal buf k name
    then (
      found <- true;
      value <- v;
      pos <- -1)
    else pos <- next
  done;
  #(found, value)
;;

(** Fold over query parameters. *)
let fold_query_params (local_ buf : bytes) (query : Span.t) ~init ~f =
  let mutable pos = cursor query in
  let mutable acc = init in
  while pos >= 0 do
    let #(k, v, next) = step buf query ~pos in
    acc <- f acc k v;
    pos <- next
  done;
  acc
;;

(** Convert query to string pairs (allocates). *)
let query_to_string_pairs (local_ buf : bytes) (query : Span.t) : (string * string) list =
  fold_query_params buf query ~init:[] ~f:(fun acc key value ->
      (Span.to_string buf key, Span.to_string buf value) :: acc)
  |> List.rev
;;
