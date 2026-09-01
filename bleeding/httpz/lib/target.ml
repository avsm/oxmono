open Base

module I16 = Stdlib_stable.Int16_u
module Char_u = Stdlib_stable.Char_u
module Scanner = Httpz_uri.Scanner

let[@inline] i16 x = I16.of_int x

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
  let sp = Scanner.parse_sub s ~pos:(toff + skip) ~len:(tlen - skip) in
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

let[@inline] parse_absolute ~toff (sp : Scanner.spans) : t =
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

(* [host:port] is not a URI-reference: with a reg-name it parses as a scheme
   followed by a rootless path, and with an IPv4 literal or an IP-literal it
   does not parse at all. The grammar is small enough to take directly. *)

(* HTTP routing treats an authority as an opaque boundary, not as a generic
   URI component. In particular, accepting a comma makes the same bytes look
   like a field-value list to some intermediaries, and accepting pct-encoding
   gives two spellings for a routing name. Keep this conservative profile in
   the one scanner shared by authority-form, Host, and authority comparison. *)
let[@inline] reg_name_end (local_ s : string) ~pos ~limit : int =
  let mutable i = pos in
  let mutable go = true in
  while go && i < limit do
    let c = String.unsafe_get s i in
    if Scanner.is_unreserved c
       || (Scanner.is_sub_delim c && not (Char.equal c ','))
    then i <- i + 1
    else go <- false
  done;
  i
;;

(* The URI host shared by authority-form and Host field parsing. The span of
   an IP-literal excludes its brackets, matching [Httpz_uri.Scanner]. [after] is the
   first byte after the host, or [-1] when the host is malformed. *)
let[@inline] parse_host (local_ s : string) ~pos ~limit =
  if Char.equal (String.unsafe_get s pos) '['
  then (
    let e6 = Scanner.ipv6_end s (pos + 1) limit in
    let e =
      if e6 >= 0 && e6 < limit && Char.equal (String.unsafe_get s e6) ']'
      then e6
      else (
        let ef = Scanner.ipvfuture_end s (pos + 1) limit in
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
     reports the span inside the brackets, as [Httpz_uri.Scanner] does. *)
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
   immutable string. [Range.parse_string] takes the same view in reverse. *)
(* [opt]: allocation-free only once [Span.make] and the [Httpz_uri.Scanner] functions are
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
      let sp = Scanner.parse_sub s ~pos:toff ~len:tlen in
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

let[@inline] is_absolute (t : t) =
  match t.#form with
  | Absolute -> true
  | Origin | Authority | Asterisk | Invalid -> false
;;

(* Host = uri-host [ ":" port ]. Keep this beside the request-target
   authority parser so the two uses cannot drift apart. *)
let valid_host (local_ buf : bytes) (value : Span.t) =
  let off = Span.off value in
  let limit = off + Span.len value in
  let local_ s = Stdlib.Bytes.unsafe_to_string buf in
  (* RFC 9110 4.2.1 gives no http(s) URI an empty host, so an empty Host field
     names nothing; RFC 9112 3.2 asks for 400 rather than a guess. *)
  if off = limit
  then false
  else (
    (* The port suffix is spelled out rather than shared with a closure: [s]
       is local, so a closure over it cannot be called in tail position. *)
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

(* Host is compared without ASCII case and the port exactly, so that an absent
   port matches only an absent port: a scheme default is not filled in, because
   an intermediary that does fill it in would then route the two spellings the
   same way this server does not. Both sides drop IP-literal brackets, which
   [parse_host] already does for the target. *)
let authority_matches (local_ buf : bytes) (t : t) (value : Span.t) =
  let off = Span.off value in
  let limit = off + Span.len value in
  let local_ s = Stdlib.Bytes.unsafe_to_string buf in
  if off >= limit
  then false
  else (
    let #(host_off, host_len, after) = parse_host s ~pos:off ~limit in
    if after < 0
    then false
    else (
      (* [-2] for a malformed suffix, which no port value can equal. *)
      let port =
        if after = limit
        then -1
        else if Char.equal (String.unsafe_get s after) ':'
        then (
          let #(port_end, port) = parse_port s ~pos:(after + 1) ~limit in
          if port_end = limit then port else -2)
        else -2
      in
      if port <> t.#port || host_len <> Span.len t.#host
      then false
      else (
        let toff = Span.off t.#host in
        let mutable i = 0 in
        let mutable eq = true in
        while eq && i < host_len do
          if Buf_read.to_lower (peek buf (toff + i))
             =. Buf_read.to_lower (peek buf (host_off + i))
          then i <- i + 1
          else eq <- false
        done;
        eq)))
;;

(* A path segment ends at the next '/' or at the end of the path. The
   separator itself belongs to neither the segment nor the remainder. *)
let[@inline] segment_end (local_ buf : bytes) ~poff ~plen =
  let mutable i = 0 in
  while i < plen && peek buf (poff + i) <>. #'/' do
    i <- i + 1
  done;
  i
;;

let[@inline] segment_rest ~poff ~plen ~seg_len =
  let rest_off = if seg_len < plen then poff + seg_len + 1 else poff + seg_len in
  let rest_len = if seg_len < plen then plen - seg_len - 1 else 0 in
  Span.make ~off:(i16 rest_off) ~len:(i16 rest_len)
;;

let[@inline] match_segment
  (local_ buf : bytes)
  (path : Span.t)
  (expected : string)
  : #(bool * Span.t)
  =
  let plen = Span.len path in
  if plen = 0 then #(false, empty_span)
  else
    let poff = Span.off path in
    let elen = String.length expected in
    let seg_len = segment_end buf ~poff ~plen in
    if seg_len <> elen then #(false, empty_span)
    else (
      let mutable j = 0 in
      let mutable eq = true in
      while eq && j < elen do
        if not (peek buf (poff + j) =. peek_str expected j)
        then eq <- false
        else j <- j + 1
      done;
      if not eq then #(false, empty_span)
      else #(true, segment_rest ~poff ~plen ~seg_len))
;;

let[@inline] match_param (local_ buf : bytes) (path : Span.t)
  : #(bool * Span.t * Span.t)
  =
  let plen = Span.len path in
  if plen = 0 then #(false, empty_span, empty_span)
  else
    let poff = Span.off path in
    let seg_len = segment_end buf ~poff ~plen in
    let seg = Span.make ~off:(i16 poff) ~len:(i16 seg_len) in
    #(true, seg, segment_rest ~poff ~plen ~seg_len)
;;

let[@inline] is_empty (path : Span.t) : bool = Span.len path = 0

(* A parameter ends at [&], and its first [=] separates key from value. A
   missing and an empty value both become zero-length spans. *)

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

let find_query_param (local_ buf : bytes) (query : Span.t) (name : string)
  : #(bool * Span.t)
  =
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

let query_to_string_pairs (local_ buf : bytes) (query : Span.t)
  : (string * string) list
  =
  fold_query_params buf query ~init:[] ~f:(fun acc key value ->
      (Span.to_string buf key, Span.to_string buf value) :: acc)
  |> List.rev
;;
