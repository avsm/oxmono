(*
 * Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2012-2014 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

module Raw = Uriz_raw

let hex_upper = "0123456789ABCDEF"

let[@inline] lower c =
  if c >= 'A' && c <= 'Z' then Char.unsafe_chr (Char.code c + 32) else c

(* [String.sub] wants its argument global, which would stop every accessor
   from reading a stack-allocated URI.  [Bytes.blit_string] does accept a
   local source, so this is the same function with a wider domain.  The result
   is a fresh heap string, hence global. *)
let sub (s : string @ local) off len =
  let b = Bytes.create len in
  Bytes.blit_string s off b 0 len;
  Bytes.unsafe_to_string b

(* [Hashtbl.hash] is global-only too.  FNV-1a over the bytes keeps {!hash}
   usable on a local URI.  It is not [Hashtbl.hash], only a function that
   agrees with {!equal}. *)
let string_hash (s : string @ local) =
  let n = String.length s in
  let mutable h = 0x811c9dc5 in
  let mutable i = 0 in
  while i < n do
    h <- (h lxor Char.code (String.unsafe_get s i)) * 0x01000193 land max_int;
    i <- i + 1
  done;
  h

(* A URI is its canonical text plus an index into it.  Every field but [raw]
   is an immediate, so this is a single flat block. *)
type t = {
  raw : string;
  scheme_len : int;
  userinfo_off : int;
  userinfo_len : int;
  host_off : int;
  host_len : int;
  host_kind : int;
  port_off : int;
  port_len : int;
  port_val : int;
  path_off : int;
  path_len : int;
  query_off : int;
  query_len : int;
  frag_off : int;
  frag_len : int;
}

(* The local instantiation builds the record in the caller's frame, so a URI
   that shares its canonical string with a local input never touches the heap
   at all.  Canonicalized text can also be allocated in the caller's region;
   the same record constructor accepts either lifetime.  Every field
   is bound first because [exclave_if_local] only accepts a record built from
   identifiers. *)
let%template[@mode m = (global, local)] some_of_spans (raw : string @ m)
    (v : Raw.spans) : t or_null @ m =
  let scheme_len = Raw.scheme_len v in
  let userinfo_off = Raw.userinfo_off v in
  let userinfo_len = Raw.userinfo_len v in
  let host_off = Raw.host_off v in
  let host_len = Raw.host_len v in
  let host_kind = Raw.host_kind v in
  let port_off = Raw.port_off v in
  let port_len = Raw.port_len v in
  let port_val = Raw.port_val v in
  let path_off = Raw.path_off v in
  let path_len = Raw.path_len v in
  let query_off = Raw.query_off v in
  let query_len = Raw.query_len v in
  let frag_off = Raw.frag_off v in
  let frag_len = Raw.frag_len v in
  This
    { raw; scheme_len; userinfo_off; userinfo_len; host_off; host_len;
      host_kind; port_off; port_len; port_val; path_off; path_len; query_off;
      query_len; frag_off; frag_len }
  [@exclave_if_local m]

(* The same builder without the [or_null], for the producers that know they
   have a valid URI.  The field extraction is repeated rather than shared
   because [exclave_if_local] only accepts a record literal of identifiers or
   a tailcall, and neither [This (t_of_spans ...)] nor a [match] on the result
   of [some_of_spans] is one of those. *)
let%template[@mode m = (global, local)] t_of_spans (raw : string @ m)
    (v : Raw.spans) : t @ m =
  let scheme_len = Raw.scheme_len v in
  let userinfo_off = Raw.userinfo_off v in
  let userinfo_len = Raw.userinfo_len v in
  let host_off = Raw.host_off v in
  let host_len = Raw.host_len v in
  let host_kind = Raw.host_kind v in
  let port_off = Raw.port_off v in
  let port_len = Raw.port_len v in
  let port_val = Raw.port_val v in
  let path_off = Raw.path_off v in
  let path_len = Raw.path_len v in
  let query_off = Raw.query_off v in
  let query_len = Raw.query_len v in
  let frag_off = Raw.frag_off v in
  let frag_len = Raw.frag_len v in
  { raw; scheme_len; userinfo_off; userinfo_len; host_off; host_len;
    host_kind; port_off; port_len; port_val; path_off; path_len; query_off;
    query_len; frag_off; frag_len }
  [@exclave_if_local m]

(* {2 Canonicalization}

   Write [s\[off, off+len)] into [dst] at [k], applying RFC 3986 §6.2.2:
   percent-triplets that encode an unreserved character are decoded, the hex
   of the ones that survive is uppercased, and, when [low] is set for the
   scheme and the host, ASCII letters are folded to lowercase.  The input
   has already been validated, so every ['%'] here is a well-formed triplet.
   Returns the new write position. *)
let write_run (dst : bytes @ local) k (s : string @ local) off len low =
  let mutable k = k in
  let mutable i = off in
  let e = off + len in
  while i < e do
    let c = String.unsafe_get s i in
    if c = '%' then begin
      let v =
        (Raw.hex_val (String.unsafe_get s (i + 1)) * 16)
        + Raw.hex_val (String.unsafe_get s (i + 2))
      in
      let d = Char.unsafe_chr v in
      if Raw.is_unreserved d then begin
        Bytes.unsafe_set dst k (if low then lower d else d);
        k <- k + 1
      end
      else begin
        Bytes.unsafe_set dst k '%';
        Bytes.unsafe_set dst (k + 1) (String.unsafe_get hex_upper (v lsr 4));
        Bytes.unsafe_set dst (k + 2) (String.unsafe_get hex_upper (v land 15));
        k <- k + 3
      end;
      i <- i + 3
    end
    else begin
      Bytes.unsafe_set dst k (if low then lower c else c);
      k <- k + 1;
      i <- i + 1
    end
  done;
  k

(* The canonical form is never longer than the input, and the scanner told us
   exactly how much shorter, so one exact-size [Bytes] suffices. *)
let[@zero_alloc] canonicalize_into (out : bytes @ local) (s : string @ local) (v : Raw.spans) =
  let mutable k = 0 in
  let scheme_len = Raw.scheme_len v in
  if scheme_len >= 0 then begin
    k <- write_run out k s 0 scheme_len true;
    Bytes.unsafe_set out k ':';
    k <- k + 1
  end;
  let host_off = Raw.host_off v in
  if host_off >= 0 then begin
    Bytes.unsafe_set out k '/';
    Bytes.unsafe_set out (k + 1) '/';
    k <- k + 2;
    let ui_off = Raw.userinfo_off v in
    if ui_off >= 0 then begin
      k <- write_run out k s ui_off (Raw.userinfo_len v) false;
      Bytes.unsafe_set out k '@';
      k <- k + 1
    end;
    let kind = Raw.host_kind v in
    if kind = Raw.host_ipv6 || kind = Raw.host_ipvfuture then begin
      Bytes.unsafe_set out k '[';
      k <- k + 1;
      k <- write_run out k s host_off (Raw.host_len v) true;
      Bytes.unsafe_set out k ']';
      k <- k + 1
    end
    else k <- write_run out k s host_off (Raw.host_len v) true;
    let port_off = Raw.port_off v in
    if port_off >= 0 then begin
      Bytes.unsafe_set out k ':';
      k <- k + 1;
      let port_len = Raw.port_len v in
      Bytes.blit_string s port_off out k port_len;
      k <- k + port_len
    end
  end;
  k <- write_run out k s (Raw.path_off v) (Raw.path_len v) false;
  let query_off = Raw.query_off v in
  if query_off >= 0 then begin
    Bytes.unsafe_set out k '?';
    k <- k + 1;
    k <- write_run out k s query_off (Raw.query_len v) false
  end;
  let frag_off = Raw.frag_off v in
  if frag_off >= 0 then begin
    Bytes.unsafe_set out k '#';
    k <- k + 1;
    k <- write_run out k s frag_off (Raw.frag_len v) false
  end;
  ()

let canonicalize (s : string @ local) v =
  let out = Bytes.create (String.length s - Raw.shrink v) in
  canonicalize_into out s v;
  Bytes.unsafe_to_string out

let[@zero_alloc] canonicalize__local (s : string @ local) v = exclave_
  let out = Base.Bytes.create_local (String.length s - Raw.shrink v) in
  canonicalize_into out s v;
  Bytes.unsafe_to_string out

let%template[@mode m = (global, local)] of_string (s : string @ m)
    : t or_null @ m =
  let v = Raw.parse s in
  if not (Raw.is_valid v) then Null
  else if not (Raw.needs_normalization v) then
    (* Already canonical: share the caller's string, so the only allocation
       is the record itself.  At mode local, not even that. *)
    (some_of_spans [@mode m]) s v [@exclave_if_local m]
  else begin
    let raw = (canonicalize [@mode m]) s v in
    let v = Raw.parse raw in
    if Raw.is_valid v then let result = (some_of_spans [@mode m]) raw v in result
    else Null
  end [@exclave_if_local m ~reasons:[ May_return_local ]]

(* Parse without ever normalizing: input that is not already canonical is
   rejected rather than rewritten.  Nothing can allocate, so the record goes
   on the stack, the canonical string {i is} the caller's string, and
   [@zero_alloc] proves the whole call heap-free.  This is the hot-path entry
   point: parse, read spans, drop.  Fall back to {!of_string} on [Null]. *)
let[@zero_alloc] of_string_canonical (s : string @ local) : t or_null @ local =
  let v = Raw.parse s in
  if (not (Raw.is_valid v)) || Raw.needs_normalization v then Null
  else exclave_ (some_of_spans [@mode local]) s v

let%template[@mode m = (global, local)] of_string_exn (s : string @ m) : t @ m =
  let v = Raw.parse s in
  if not (Raw.is_valid v) then
    (* A local input cannot safely escape through the global exception. *)
    invalid_arg "Uriz.of_string_exn: not a URI reference"
  else if not (Raw.needs_normalization v) then
    (t_of_spans [@mode m]) s v [@exclave_if_local m]
  else begin
    let raw = (canonicalize [@mode m]) s v in
    let v = Raw.parse raw in
    if Raw.is_valid v then
      let result = (t_of_spans [@mode m]) raw v in result
    else
      invalid_arg "Uriz.of_string_exn: normalization produced an invalid URI"
  end [@exclave_if_local m ~reasons:[ May_return_local ]]

(* {2 Output and identity} *)

(* The [raw] field of a local record reads back local, so this really is
   mode-polymorphic rather than merely local-accepting. *)
let%template[@mode m = (global, local)] to_string (t : t @ m) : string @ m =
  t.raw

let pp ppf (t : t @ local) = Format.pp_print_string ppf (sub t.raw 0 (String.length t.raw))
let equal (a : t @ local) (b : t @ local) = String.equal a.raw b.raw
let compare (a : t @ local) (b : t @ local) = String.compare a.raw b.raw
let hash (t : t @ local) = string_hash t.raw

(* {2 Component access}

   All read-only, so all local-accepting.  The results are freshly allocated
   substrings and therefore global. *)

let[@inline] opt_sub (t : t @ local) off len =
  if off < 0 then Null else This (sub t.raw off len)

let scheme (t : t @ local) =
  if t.scheme_len < 0 then Null else This (sub t.raw 0 t.scheme_len)

let userinfo (t : t @ local) = opt_sub t t.userinfo_off t.userinfo_len
let host (t : t @ local) = opt_sub t t.host_off t.host_len
let path (t : t @ local) = sub t.raw t.path_off t.path_len
let query (t : t @ local) = opt_sub t t.query_off t.query_len
let fragment (t : t @ local) = opt_sub t t.frag_off t.frag_len
let port (t : t @ local) = if t.port_val < 0 then Null else This t.port_val
let has_port (t : t @ local) = t.port_off >= 0
let has_authority (t : t @ local) = t.host_off >= 0
let is_absolute (t : t @ local) = t.scheme_len >= 0
let[@zero_alloc] has_query (t : t @ local) = t.query_off >= 0
let[@zero_alloc] has_fragment (t : t @ local) = t.frag_off >= 0
let[@zero_alloc] has_userinfo (t : t @ local) = t.userinfo_off >= 0
let[@zero_alloc] encoded_path_span (t : t @ local) = #(t.path_off, t.path_len)

let encoded_path_and_query (t : t @ local) =
  let finish =
    if t.query_off < 0 then t.path_off + t.path_len else t.query_off + t.query_len
  in
  sub t.raw t.path_off (finish - t.path_off)

let port_int (t : t @ local) = t.port_val
let[@zero_alloc] sub__local (s : string @ local) off len = exclave_
  let b = Base.Bytes.create_local len in
  Bytes.blit_string s off b 0 len;
  Bytes.unsafe_to_string b

let opt_sub__local (t : t @ local) off len = exclave_
  if off < 0 then Null else This (sub__local t.raw off len)

let scheme__local (t : t @ local) = exclave_
  if t.scheme_len < 0 then Null else This (sub__local t.raw 0 t.scheme_len)

let userinfo__local (t : t @ local) = exclave_ opt_sub__local t t.userinfo_off t.userinfo_len
let host__local (t : t @ local) = exclave_ opt_sub__local t t.host_off t.host_len
let query__local (t : t @ local) = exclave_ opt_sub__local t t.query_off t.query_len
let fragment__local (t : t @ local) = exclave_ opt_sub__local t t.frag_off t.frag_len
let path__local (t : t @ local) = exclave_ sub__local t.raw t.path_off t.path_len


let host_kind (t : t @ local) =
  if t.host_kind = Raw.host_reg_name then `Reg_name
  else if t.host_kind = Raw.host_ipv4 then `Ipv4
  else if t.host_kind = Raw.host_ipv6 then `Ipv6
  else if t.host_kind = Raw.host_ipvfuture then `Ipvfuture
  else `None

let scheme_span (t : t @ local) = if t.scheme_len < 0 then #(-1, 0) else #(0, t.scheme_len)
let userinfo_span (t : t @ local) = #(t.userinfo_off, t.userinfo_len)
let host_span (t : t @ local) = #(t.host_off, t.host_len)
let port_span (t : t @ local) = #(t.port_off, t.port_len)
let path_span (t : t @ local) = #(t.path_off, t.path_len)
let query_span (t : t @ local) = #(t.query_off, t.query_len)
let fragment_span (t : t @ local) = #(t.frag_off, t.frag_len)

(* {2 Percent codecs} *)

type component =
  [ `Userinfo
  | `Host
  | `Path
  | `Segment
  | `Query
  | `Query_value
  | `Fragment
  | `Unreserved
  ]

let table_of_pred f =
  String.init 256 (fun i -> if f (Char.unsafe_chr i) then '\001' else '\000')

let[@inline] base_safe c = Raw.is_unreserved c || Raw.is_sub_delim c

let tbl_unreserved = table_of_pred Raw.is_unreserved
let tbl_userinfo = table_of_pred (fun c -> base_safe c || c = ':')
let tbl_host = table_of_pred base_safe
let tbl_segment = table_of_pred (fun c -> base_safe c || c = ':' || c = '@')

let tbl_path =
  table_of_pred (fun c -> base_safe c || c = ':' || c = '@' || c = '/')

let tbl_query =
  table_of_pred (fun c ->
      base_safe c || c = ':' || c = '@' || c = '/' || c = '?')

let tbl_query_value =
  table_of_pred (fun c ->
      (base_safe c && c <> '&' && c <> '=' && c <> '+')
      || c = ':' || c = '@' || c = '/' || c = '?')

let table_of = function
  | `Userinfo -> tbl_userinfo
  | `Host -> tbl_host
  | `Path -> tbl_path
  | `Segment -> tbl_segment
  | `Query -> tbl_query
  | `Query_value -> tbl_query_value
  | `Fragment -> tbl_query
  | `Unreserved -> tbl_unreserved

let[@inline] safe tbl c = String.unsafe_get tbl (Char.code c) <> '\000'

let encode_with tbl s =
  let n = String.length s in
  let mutable unsafe_count = 0 in
  let mutable i = 0 in
  while i < n do
    if not (safe tbl (String.unsafe_get s i)) then
      unsafe_count <- unsafe_count + 1;
    i <- i + 1
  done;
  if unsafe_count = 0 then s
  else begin
    let out = Bytes.create (n + (2 * unsafe_count)) in
    let mutable k = 0 in
    let mutable i = 0 in
    while i < n do
      let c = String.unsafe_get s i in
      if safe tbl c then begin
        Bytes.unsafe_set out k c;
        k <- k + 1
      end
      else begin
        let v = Char.code c in
        Bytes.unsafe_set out k '%';
        Bytes.unsafe_set out (k + 1) (String.unsafe_get hex_upper (v lsr 4));
        Bytes.unsafe_set out (k + 2) (String.unsafe_get hex_upper (v land 15));
        k <- k + 3
      end;
      i <- i + 1
    done;
    Bytes.unsafe_to_string out
  end

let pct_encode ?(component = `Path) s = encode_with (table_of component) s

(* Decode [s\[off, off+len)], which the caller guarantees is well formed.
   [plus] folds ['+'] to a space, which does not change the length and so is
   settled separately from the triplet count. *)
let decode_span ~plus (s : string @ local) off len =
  let e = off + len in
  let mutable i = off in
  let mutable triplets = 0 in
  let mutable pluses = false in
  while i < e do
    let c = String.unsafe_get s i in
    if c = '%' then begin
      triplets <- triplets + 1;
      i <- i + 3
    end
    else begin
      if plus && c = '+' then pluses <- true;
      i <- i + 1
    end
  done;
  if triplets = 0 && not pluses then sub s off len
  else begin
    let out = Bytes.create (len - (2 * triplets)) in
    let mutable k = 0 in
    let mutable i = off in
    while i < e do
      let c = String.unsafe_get s i in
      if c = '%' then begin
        Bytes.unsafe_set out k
          (Char.unsafe_chr
             ((Raw.hex_val (String.unsafe_get s (i + 1)) * 16)
             + Raw.hex_val (String.unsafe_get s (i + 2))));
        k <- k + 1;
        i <- i + 3
      end
      else begin
        Bytes.unsafe_set out k (if plus && c = '+' then ' ' else c);
        k <- k + 1;
        i <- i + 1
      end
    done;
    Bytes.unsafe_to_string out
  end

let pct_decode ?(plus_as_space = false) s =
  let n = String.length s in
  let mutable i = 0 in
  let mutable bad = false in
  let mutable triplets = 0 in
  let mutable pluses = false in
  while i < n do
    let c = String.unsafe_get s i in
    if c = '%' then
      if
        i + 2 < n
        && Raw.is_hexdig (String.unsafe_get s (i + 1))
        && Raw.is_hexdig (String.unsafe_get s (i + 2))
      then begin
        triplets <- triplets + 1;
        i <- i + 3
      end
      else begin
        bad <- true;
        i <- n
      end
    else begin
      if plus_as_space && c = '+' then pluses <- true;
      i <- i + 1
    end
  done;
  if bad then Null
  else if triplets = 0 && not pluses then This s
  else This (decode_span ~plus:plus_as_space s 0 n)

(* {2 Decoded access} *)

let path_decoded (t : t @ local) =
  decode_span ~plus:false t.raw t.path_off t.path_len

let path_unencoded (t : t @ local) = path_decoded t

let fragment_decoded (t : t @ local) =
  if t.frag_off < 0 then Null
  else This (decode_span ~plus:false t.raw t.frag_off t.frag_len)

let userinfo_decoded (t : t @ local) =
  if t.userinfo_off < 0 then Null
  else This (decode_span ~plus:false t.raw t.userinfo_off t.userinfo_len)

let decoded_host (t : t @ local) =
  if t.host_off < 0 then Null
  else This (decode_span ~plus:false t.raw t.host_off t.host_len)

(* {2 Query} *)

let query_cursor (t : t @ local) = if t.query_off < 0 || t.query_len = 0 then -1 else t.query_off

let query_step (t : t @ local) pos =
  if pos < 0 then #(-1, 0, -1, 0, -1)
  else begin
    let qend = t.query_off + t.query_len in
    let mutable e = pos in
    while e < qend && String.unsafe_get t.raw e <> '&' do
      e <- e + 1
    done;
    let mutable eq = pos in
    while eq < e && String.unsafe_get t.raw eq <> '=' do
      eq <- eq + 1
    done;
    let next = if e < qend then e + 1 else -1 in
    if eq < e then #(pos, eq - pos, eq + 1, e - eq - 1, next)
    else #(pos, e - pos, -1, 0, next)
  end

let query_iter ?(plus_as_space = false) (t : t @ local) (f @ local) =
  let plus = plus_as_space in
  let mutable pos = query_cursor t in
  let mutable go = pos >= 0 in
  while go do
    let #(koff, klen, voff, vlen, next) = query_step t pos in
    let key = decode_span ~plus t.raw koff klen in
    let value =
      if voff < 0 then Null else This (decode_span ~plus t.raw voff vlen)
    in
    f ~key ~value;
    if next < 0 then go <- false else pos <- next
  done

let query_params ?(plus_as_space = false) (t : t @ local) =
  let plus = plus_as_space in
  let mutable pos = query_cursor t in
  let mutable acc = [] in
  while pos >= 0 do
    let #(koff, klen, voff, vlen, next) = query_step t pos in
    let key = decode_span ~plus t.raw koff klen in
    let value =
      if voff < 0 then None else Some (decode_span ~plus t.raw voff vlen)
    in
    acc <- (key, value) :: acc;
    pos <- next
  done;
  List.rev acc

(* Compare a percent-encoded span against a plain string without decoding it
   into a fresh buffer first.  Decodes exactly as [decode_span] does, so the
   two agree on ['+'] as well. *)
let span_decodes_to ~plus (s : string @ local) off len (key : string @ local) =
  let e = off + len in
  let kn = String.length key in
  let mutable i = off in
  let mutable j = 0 in
  let mutable eq = true in
  while eq && i < e do
    if j >= kn then eq <- false
    else begin
      let c = String.unsafe_get s i in
      let d =
        if c = '%' then
          Char.unsafe_chr
            ((Raw.hex_val (String.unsafe_get s (i + 1)) * 16)
            + Raw.hex_val (String.unsafe_get s (i + 2)))
        else if plus && c = '+' then ' '
        else c
      in
      if d <> String.unsafe_get key j then eq <- false
      else begin
        i <- (if c = '%' then i + 3 else i + 1);
        j <- j + 1
      end
    end
  done;
  eq && j = kn

let find_query ?(plus_as_space = false) (t : t @ local) (key : string @ local) =
  let plus = plus_as_space in
  let mutable pos = query_cursor t in
  let mutable res = Null in
  let mutable go = pos >= 0 in
  while go do
    let #(koff, klen, voff, vlen, next) = query_step t pos in
    if span_decodes_to ~plus t.raw koff klen key then begin
      res <- This (if voff < 0 then "" else decode_span ~plus t.raw voff vlen);
      go <- false
    end
    else if next < 0 then go <- false
    else pos <- next
  done;
  res

(* {2 RFC 3986 §5.2.4 remove_dot_segments}

   Writes the normalized form of [src\[off, off+len)] into [dst] from [k0] and
   returns the end position.  The output is never longer than the input, so
   [dst] only needs [k0 + len] bytes. *)

let[@zero_alloc] drop_last_segment (dst : bytes @ local) k0 k =
  let mutable j = k - 1 in
  let mutable res = k0 in
  let mutable go = true in
  while go && j >= k0 do
    if Bytes.unsafe_get dst j = '/' then begin
      res <- j;
      go <- false
    end
    else j <- j - 1
  done;
  res

let[@zero_alloc] remove_dot_segments (src : string @ local) off len (dst : bytes @ local) k0 =
  let e = off + len in
  let mutable i = off in
  let mutable k = k0 in
  while i < e do
    let r = e - i in
    let c0 = String.unsafe_get src i in
    let c1 = if r >= 2 then String.unsafe_get src (i + 1) else '\000' in
    let c2 = if r >= 3 then String.unsafe_get src (i + 2) else '\000' in
    let c3 = if r >= 4 then String.unsafe_get src (i + 3) else '\000' in
    if r >= 3 && c0 = '.' && c1 = '.' && c2 = '/' then i <- i + 3
    else if r >= 2 && c0 = '.' && c1 = '/' then i <- i + 2
    else if r >= 3 && c0 = '/' && c1 = '.' && c2 = '/' then i <- i + 2
    else if r = 2 && c0 = '/' && c1 = '.' then begin
      Bytes.unsafe_set dst k '/';
      k <- k + 1;
      i <- e
    end
    else if r >= 4 && c0 = '/' && c1 = '.' && c2 = '.' && c3 = '/' then begin
      k <- drop_last_segment dst k0 k;
      i <- i + 3
    end
    else if r = 3 && c0 = '/' && c1 = '.' && c2 = '.' then begin
      k <- drop_last_segment dst k0 k;
      Bytes.unsafe_set dst k '/';
      k <- k + 1;
      i <- e
    end
    else if r = 1 && c0 = '.' then i <- e
    else if r = 2 && c0 = '.' && c1 = '.' then i <- e
    else begin
      if c0 = '/' then begin
        Bytes.unsafe_set dst k '/';
        k <- k + 1;
        i <- i + 1
      end;
      while i < e && String.unsafe_get src i <> '/' do
        Bytes.unsafe_set dst k (String.unsafe_get src i);
        k <- k + 1;
        i <- i + 1
      done
    end
  done;
  k

(* {2 Assembling a URI in one scratch buffer}

   [resolve] and [normalize] compose their result into a single over-allocated
   buffer and cut the exact string out of it at the end.  Sizing the result
   exactly up front would need the dot-segment output length, which needs the
   pass, so one scratch is the floor.

   Every helper here takes its destination at mode [local], so one copy of the
   composition code serves both a heap [Bytes.create] and a region
   [Base.Bytes.create_local]. *)

let[@inline] [@zero_alloc] put_char (b : bytes @ local) k c =
  Bytes.unsafe_set b k c;
  k + 1

let[@inline] [@zero_alloc] put_sub (b : bytes @ local) k (s : string @ local) off len =
  Bytes.blit_string s off b k len;
  k + len

(* Which §5.3 recomposition guard [b\[pstart, pend)] needs as a path: 1 for
   ["/."], 2 for ["./"], 0 for none. *)
let[@zero_alloc] path_guard (b : bytes @ local) pstart pend ~has_scheme ~has_auth =
  let len = pend - pstart in
  if
    (not has_auth) && len >= 2
    && Bytes.unsafe_get b pstart = '/'
    && Bytes.unsafe_get b (pstart + 1) = '/'
  then 1
  else if (not has_scheme) && (not has_auth) && len > 0 then begin
    let mutable i = pstart in
    let mutable colon = false in
    while i < pend && Bytes.unsafe_get b i <> '/' do
      if Bytes.unsafe_get b i = ':' then colon <- true;
      i <- i + 1
    done;
    if colon then 2 else 0
  end
  else 0

(* Shift the path right by two and write the guard in front of it. *)
let[@zero_alloc] insert_guard (b : bytes @ local) pstart pend g =
  Bytes.blit b pstart b (pstart + 2) (pend - pstart);
  if g = 1 then begin
    Bytes.unsafe_set b pstart '/';
    Bytes.unsafe_set b (pstart + 1) '.'
  end
  else begin
    Bytes.unsafe_set b pstart '.';
    Bytes.unsafe_set b (pstart + 1) '/'
  end;
  pend + 2

(* [remove_dot_segments] never writes ahead of where it is reading, so it can
   run over the scratch in place. *)
let[@inline] [@zero_alloc] strip_dots_in_place (b : bytes @ local) pstart pend =
  (* not a tail call: the aliased string must not outlive this frame *)
  let k =
    remove_dot_segments (Bytes.unsafe_to_string b) pstart (pend - pstart) b
      pstart
  in
  k

(* Whether any '/'-delimited segment of [s\[off, off+len)] is ["."] or [".."].
   When none is, dot-segment removal is the identity and {!normalize} can hand
   back its argument without allocating. *)
let[@zero_alloc] path_has_dot_segment (s : string @ local) off len =
  let e = off + len in
  let mutable i = off in
  let mutable found = false in
  while i < e && not found do
    let st = i in
    while i < e && String.unsafe_get s i <> '/' do
      i <- i + 1
    done;
    let sl = i - st in
    if
      (sl = 1 && String.unsafe_get s st = '.')
      || (sl = 2
          && String.unsafe_get s st = '.'
          && String.unsafe_get s (st + 1) = '.')
    then found <- true;
    if i < e then i <- i + 1
  done;
  found

(* Composition uses canonical spans, canonical encoded components and canonical
   delimiters. Validate that invariant before constructing the URI index. *)
let%template[@mode m = (global, local)] of_canonical_exn (raw : string @ m)
    : t @ m =
  let v = Raw.parse raw in
  if Raw.err v <> 0 || Raw.needs_normalization v then
    invalid_arg "Uriz: composed URI is not canonical"
  else (t_of_spans [@mode m]) raw v [@exclave_if_local m]

(* Upper bound on the composed length.  Every byte written is either copied
   from one of the two canonical strings or one of a bounded number of
   delimiters, and dot-segment removal only shrinks.  Sixteen covers the
   delimiters and the two guard bytes. *)
let[@inline] [@zero_alloc] resolve_cap (base : t @ local) (r : t @ local) =
  String.length base.raw + String.length r.raw + 16

(* RFC 3986 §5.2.2, composed into a caller-supplied buffer.  Shared by the two
   [resolve] variants, which differ only in where that buffer lives. *)
let[@zero_alloc] compose_resolve (b : bytes @ local) ~(base : t @ local) (r : t @ local) =
  let mutable k = 0 in
  let has_scheme = r.scheme_len >= 0 || base.scheme_len >= 0 in
  if r.scheme_len >= 0 then begin
    k <- put_sub b k r.raw 0 r.scheme_len;
    k <- put_char b k ':'
  end
  else if base.scheme_len >= 0 then begin
    k <- put_sub b k base.raw 0 base.scheme_len;
    k <- put_char b k ':'
  end;
  let r_defines_auth = r.scheme_len >= 0 || r.host_off >= 0 in
  let a = if r_defines_auth then r else base in
  let has_auth = a.host_off >= 0 in
  if has_auth then begin
    k <- put_char b k '/';
    k <- put_char b k '/';
    if a.userinfo_off >= 0 then begin
      k <- put_sub b k a.raw a.userinfo_off a.userinfo_len;
      k <- put_char b k '@'
    end;
    if a.host_kind = Raw.host_ipv6 || a.host_kind = Raw.host_ipvfuture then begin
      k <- put_char b k '[';
      k <- put_sub b k a.raw a.host_off a.host_len;
      k <- put_char b k ']'
    end
    else k <- put_sub b k a.raw a.host_off a.host_len;
    if a.port_off >= 0 then begin
      k <- put_char b k ':';
      k <- put_sub b k a.raw a.port_off a.port_len
    end
  end;
  (* Path and query travel together: §5.2.2 only inherits the base query in
     the one case where the reference contributes no path either. *)
  let take_base_path = (not r_defines_auth) && r.path_len = 0 in
  if take_base_path then begin
    k <- put_sub b k base.raw base.path_off base.path_len;
    if r.query_off >= 0 then begin
      k <- put_char b k '?';
      k <- put_sub b k r.raw r.query_off r.query_len
    end
    else if base.query_off >= 0 then begin
      k <- put_char b k '?';
      k <- put_sub b k base.raw base.query_off base.query_len
    end
  end
  else begin
    let pstart = k in
    let use_merge =
      (not r_defines_auth)
      && r.path_len > 0
      && String.unsafe_get r.raw r.path_off <> '/'
    in
    (* §5.2.3 merge, written straight into the scratch *)
    if use_merge then
      if base.host_off >= 0 && base.path_len = 0 then k <- put_char b k '/'
      else begin
        let mutable j = base.path_off + base.path_len - 1 in
        let mutable cut = base.path_off in
        let mutable go = true in
        while go && j >= base.path_off do
          if String.unsafe_get base.raw j = '/' then begin
            cut <- j + 1;
            go <- false
          end
          else j <- j - 1
        done;
        k <- put_sub b k base.raw base.path_off (cut - base.path_off)
      end;
    k <- put_sub b k r.raw r.path_off r.path_len;
    k <- strip_dots_in_place b pstart k;
    let g = path_guard b pstart k ~has_scheme ~has_auth in
    if g <> 0 then k <- insert_guard b pstart k g;
    if r.query_off >= 0 then begin
      k <- put_char b k '?';
      k <- put_sub b k r.raw r.query_off r.query_len
    end
  end;
  if r.frag_off >= 0 then begin
    k <- put_char b k '#';
    k <- put_sub b k r.raw r.frag_off r.frag_len
  end;
  k

(* §6.2.2.3 applied to the path, composed into a caller-supplied buffer. *)
let[@zero_alloc] compose_normalize (b : bytes @ local) (t : t @ local) =
  let n = String.length t.raw in
  let mutable k = put_sub b 0 t.raw 0 t.path_off in
  let pstart = k in
  k <- put_sub b k t.raw t.path_off t.path_len;
  k <- strip_dots_in_place b pstart k;
  let g =
    path_guard b pstart k ~has_scheme:(t.scheme_len >= 0)
      ~has_auth:(t.host_off >= 0)
  in
  if g <> 0 then k <- insert_guard b pstart k g;
  let tail = t.path_off + t.path_len in
  put_sub b k t.raw tail (n - tail)

(* The two instantiations of [resolve] and [normalize] diverge past the shared
   composition: the global one cuts a heap string out of a heap scratch, the
   local one composes in a region buffer and freezes an exact-size region copy
   into a local string, so it never touches the heap at all.  That is more
   divergence than [ppx_template] can express from one body, so the [__local]
   variants are written out by hand under the names the [@mode local]
   instantiation resolves to.

   Both carry a checked [@zero_alloc].  The region buffer must come from
   [Base.Bytes.create_local], which Base annotates [@zero_alloc].  A bare
   external such as the stdlib [Bytes.create__stack] is treated as possibly
   allocating and loses the proof. *)

let resolve ~(base : t @ local) (r : t @ local) =
  let b = Bytes.create (resolve_cap base r) in
  let k = compose_resolve b ~base r in
  of_canonical_exn (Bytes.sub_string b 0 k)

let[@zero_alloc] resolve__local ~(base : t @ local) (r : t @ local) =
  exclave_
  let b = Base.Bytes.create_local (resolve_cap base r) in
  let k = compose_resolve b ~base r in
  let e = Base.Bytes.create_local k in
  Bytes.blit b 0 e 0 k;
  (of_canonical_exn [@mode local]) (Bytes.unsafe_to_string e)

(* Global argument, because the no-change case hands the argument straight
   back and a global caller must not receive a local URI. *)
let[@inline] [@zero_alloc] normalize_is_identity (t : t @ local) =
  (t.scheme_len < 0 && t.host_off < 0)
  || not (path_has_dot_segment t.raw t.path_off t.path_len)

let normalize (t : t) =
  if normalize_is_identity t then t
  else begin
    let b = Bytes.create (String.length t.raw + 2) in
    let k = compose_normalize b t in
    of_canonical_exn (Bytes.sub_string b 0 k)
  end

let[@zero_alloc] normalize__local (t : t) =
  exclave_
  if normalize_is_identity t then t
  else begin
    let b = Base.Bytes.create_local (String.length t.raw + 2) in
    let k = compose_normalize b t in
    let e = Base.Bytes.create_local k in
    Bytes.blit b 0 e 0 k;
    (of_canonical_exn [@mode local]) (Bytes.unsafe_to_string e)
  end

(* A region URI dies with its region.  This is the way out for a caller that
   decides to keep one. *)
let globalize (t : t @ local) =
  { raw = sub t.raw 0 (String.length t.raw);
    scheme_len = t.scheme_len;
    userinfo_off = t.userinfo_off;
    userinfo_len = t.userinfo_len;
    host_off = t.host_off;
    host_len = t.host_len;
    host_kind = t.host_kind;
    port_off = t.port_off;
    port_len = t.port_len;
    port_val = t.port_val;
    path_off = t.path_off;
    path_len = t.path_len;
    query_off = t.query_off;
    query_len = t.query_len;
    frag_off = t.frag_off;
    frag_len = t.frag_len }

(* {2 Construction from components} *)

let valid_scheme s = Raw.scheme_end s 0 (String.length s) = String.length s

(* Component windows borrow the old URI text. Replacing one component needs
   only the final string and index, without copying all the unchanged fields. *)
type piece = #{ text : string; off : int; len : int; canonical : bool }
let piece (text @ local) = #{ text; off = 0; len = String.length text; canonical = false }
let absent = #{ text = ""; off = 0; len = -1; canonical = true }
let optional_piece (s @ local) = exclave_ match s with Null -> absent | This s -> piece s
let present (p : piece @ local) = p.#len >= 0
let at (p : piece @ local) i = String.unsafe_get p.#text (p.#off + i)

type parts = {
  scheme : piece; userinfo : piece; host : piece; port : piece;
  path : piece; query : piece; fragment : piece;
  (* 0: registered name; 1: literal needing brackets; 2: supplied brackets. *)
  host_form : int;
}

(* RFC 3986 section 3.2.2 brackets only an IP-literal.  The IPvFuture
   production also matches ordinary registered names such as ["v6.example.com"],
   so the discriminator is a [':'], which every IP-literal needs and no
   registered name may carry. *)
let has_colon (s : string @ local) =
  let n = String.length s in
  let mutable i = 0 in
  let mutable found = false in
  while (not found) && i < n do
    if String.unsafe_get s i = ':' then found <- true else i <- i + 1
  done;
  found

let host_form (host : string @ local) =
  let n = String.length host in
  if n >= 2 && host.[0] = '[' && host.[n - 1] = ']' then 2
  else if has_colon host && (Raw.is_ipv6 host || Raw.ipvfuture_end host 0 n = n)
  then 1
  else 0

(* Count and write the canonical encoded spelling in the same traversal.
   A counting pass uses an empty destination and never writes to it. *)
let[@zero_alloc] encode_piece (dst : bytes @ local) ~write k tbl low
    (p : piece @ local) =
  if p.#canonical then begin
    if write then Bytes.blit_string p.#text p.#off dst k p.#len;
    k + p.#len
  end else
  let mutable k = k in
  let mutable i = 0 in
  while i < p.#len do
    let c = at p i in
    let triplet = tbl <> "" && c = '%' && i + 2 < p.#len
      && Raw.is_hexdig (at p (i + 1)) && Raw.is_hexdig (at p (i + 2)) in
    if triplet then begin
      let v = Raw.hex_val (at p (i + 1)) * 16 + Raw.hex_val (at p (i + 2)) in
      let c = Char.unsafe_chr v in
      if Raw.is_unreserved c then begin
        if write then Bytes.unsafe_set dst k (if low then lower c else c);
        k <- k + 1
      end else begin
        if write then begin
          Bytes.unsafe_set dst k '%';
          Bytes.unsafe_set dst (k + 1) hex_upper.[v lsr 4];
          Bytes.unsafe_set dst (k + 2) hex_upper.[v land 15]
        end;
        k <- k + 3
      end;
      i <- i + 3
    end else begin
      if tbl = "" || safe tbl c then begin
        if write then Bytes.unsafe_set dst k (if low then lower c else c);
        k <- k + 1
      end else begin
        let v = Char.code c in
        if write then begin
          Bytes.unsafe_set dst k '%';
          Bytes.unsafe_set dst (k + 1) hex_upper.[v lsr 4];
          Bytes.unsafe_set dst (k + 2) hex_upper.[v land 15]
        end;
        k <- k + 3
      end;
      i <- i + 1
    end
  done;
  k

let[@zero_alloc] compose_parts (dst : bytes @ local) ~write (p : parts @ local) =
  let local_ k = ref 0 in
  let local_ add c = if write then Bytes.unsafe_set dst !k c; incr k in
  let local_ emit tbl low text = k := encode_piece dst ~write !k tbl low text in
  let has_scheme = present p.scheme in
  (* RFC 3986 section 3.2: an authority is introduced by a host.  Userinfo alone
     coerces one with an empty host, which {!with_userinfo} documents; a
     port alone does not, since {!with_port} must not invent a host the caller
     never supplied. *)
  let has_auth = present p.host || present p.userinfo in
  if has_scheme then begin emit "" true p.scheme; add ':' end;
  if has_auth then begin
    add '/'; add '/';
    if present p.userinfo then begin emit tbl_userinfo false p.userinfo; add '@' end;
    if present p.host then begin
      if p.host_form = 1 then add '[';
      emit (if p.host_form = 0 then tbl_host else "") true p.host;
      if p.host_form = 1 then add ']'
    end;
    if present p.port then begin add ':'; emit "" false p.port end
  end;
  if p.path.#len > 0 then begin
    if has_auth then begin if at p.path 0 <> '/' then add '/' end
    else if p.path.#len >= 2 && at p.path 0 = '/' && at p.path 1 = '/' then begin
      add '/'; add '.'
    end else if not has_scheme then begin
      let mutable i = 0 in
      let mutable colon = false in
      while i < p.path.#len && at p.path i <> '/' do
        if at p.path i = ':' then colon <- true;
        i <- i + 1
      done;
      if colon then begin add '.'; add '/' end
    end;
    emit tbl_path false p.path
  end;
  if present p.query then begin add '?'; emit tbl_query false p.query end;
  if present p.fragment then begin add '#'; emit tbl_query false p.fragment end;
  !k

let[@inline] check_authority (p : parts @ local) =
  if present p.port && not (present p.host || present p.userinfo)
  then invalid_arg "Uriz: a port needs an authority with a host"

let build_parts (p : parts @ local) =
  check_authority p;
  let size = compose_parts (Bytes.unsafe_of_string "") ~write:false p in
  let dst = Bytes.create size in
  let _ = compose_parts dst ~write:true p in
  of_canonical_exn (Bytes.unsafe_to_string dst)

let[@zero_alloc] build_parts__local (p : parts @ local) = exclave_
  check_authority p;
  let size = compose_parts (Bytes.unsafe_of_string "") ~write:false p in
  let dst = Base.Bytes.create_local size in
  let _ = compose_parts dst ~write:true p in
  of_canonical_exn__local (Bytes.unsafe_to_string dst)

let parts_of_uri (t : t @ local) = exclave_
  let view off len = #{ text = t.raw; off; len = if off < 0 then -1 else len; canonical = true } in
  { scheme = view 0 t.scheme_len;
    userinfo = view t.userinfo_off t.userinfo_len;
    host = view t.host_off t.host_len;
    host_form = (if t.host_kind = Raw.host_ipv6 || t.host_kind = Raw.host_ipvfuture then 1 else 0);
    port = view t.port_off t.port_len;
    path = view t.path_off t.path_len;
    query = view t.query_off t.query_len;
    fragment = view t.frag_off t.frag_len }

let[@inline] of_opt = function None -> Null | Some x -> This x

let encoded_port_of_int = function
  | Null -> Null
  | This p ->
    if p < 0 then invalid_arg "Uriz: negative port";
    This (string_of_int p)

let%template[@mode m = (global, local)] make ?scheme ?userinfo ?host ?port
    ?path ?query ?fragment () : t @ m =
  begin
  (match scheme with Some s when not (valid_scheme s) -> invalid_arg "Uriz: invalid scheme" | _ -> ());
  let p = {
    scheme = optional_piece (of_opt scheme);
    userinfo = optional_piece (of_opt userinfo);
    host = optional_piece (of_opt host);
    host_form = (match host with None -> 0 | Some h -> host_form h);
    port = optional_piece (encoded_port_of_int (of_opt port));
    path = piece (Option.value path ~default:"");
    query = optional_piece (of_opt query);
    fragment = optional_piece (of_opt fragment);
  } in
  let result = (build_parts [@mode m]) p in
  result
  end
  [@exclave_if_local m ~reasons:[ May_return_local ]]

let%template[@mode m = (global, local)] with_scheme (t : t @ local) scheme : t @ m =
  begin
  (match scheme with This s when not (valid_scheme s) -> invalid_arg "Uriz: invalid scheme" | _ -> ());
  let p = parts_of_uri t in
  let p = { p with scheme = optional_piece scheme } in
  let result = (build_parts [@mode m]) p in
  result
  end
  [@exclave_if_local m ~reasons:[ May_return_local ]]

let%template[@mode m = (global, local)] with_userinfo (t : t @ local) userinfo : t @ m =
  begin
  let p = parts_of_uri t in
  let p = { p with userinfo = optional_piece userinfo } in
  let result = (build_parts [@mode m]) p in
  result
  end
  [@exclave_if_local m ~reasons:[ May_return_local ]]

(* Dropping the host drops the whole authority. *)
let%template[@mode m = (global, local)] with_host (t : t @ local) host : t @ m =
  begin
  let p = parts_of_uri t in
  let p = match host with
    | Null -> { p with host = absent; userinfo = absent; port = absent; host_form = 0 }
    | This h -> { p with host = piece h; host_form = host_form h }
  in
  let result = (build_parts [@mode m]) p in
  result
  end
  [@exclave_if_local m ~reasons:[ May_return_local ]]

let%template[@mode m = (global, local)] with_port (t : t @ local) port : t @ m =
  begin
  let p = parts_of_uri t in
  let p = { p with port = optional_piece (encoded_port_of_int port) } in
  let result = (build_parts [@mode m]) p in
  result
  end
  [@exclave_if_local m ~reasons:[ May_return_local ]]

let%template[@mode m = (global, local)] with_path (t : t @ local) path : t @ m =
  begin
  let p = parts_of_uri t in
  let p = { p with path = piece path } in
  let result = (build_parts [@mode m]) p in
  result
  end
  [@exclave_if_local m ~reasons:[ May_return_local ]]

let%template[@mode m = (global, local)] with_query (t : t @ local) query : t @ m =
  begin
  let p = parts_of_uri t in
  let p = { p with query = optional_piece query } in
  let result = (build_parts [@mode m]) p in
  result
  end
  [@exclave_if_local m ~reasons:[ May_return_local ]]

let%template[@mode m = (global, local)] with_fragment (t : t @ local) fragment : t @ m =
  begin
  let p = parts_of_uri t in
  let p = { p with fragment = optional_piece fragment } in
  let result = (build_parts [@mode m]) p in
  result
  end
  [@exclave_if_local m ~reasons:[ May_return_local ]]
(* Query updates preserve the encoded spelling and order of parameters that
   remain. Keys are compared in decoded form, just as [find_query] compares
   them. A newly added parameter is appended, preserving caller order. *)

let remove_query_param ?(plus_as_space = false) (t : t) (key : string @ local) =
  let mutable pos = query_cursor t in
  let mutable found = false in
  while pos >= 0 do
    let #(koff, klen, _voff, _vlen, next) = query_step t pos in
    if span_decodes_to ~plus:plus_as_space t.raw koff klen key then found <- true;
    pos <- next
  done;
  if not found then t
  else begin
    let out = Buffer.create t.query_len in
    let mutable first = true in
    let mutable pos = query_cursor t in
    while pos >= 0 do
      let #(koff, klen, voff, vlen, next) = query_step t pos in
      if not (span_decodes_to ~plus:plus_as_space t.raw koff klen key) then begin
        if first then first <- false else Buffer.add_char out '&';
        Buffer.add_substring out t.raw koff klen;
        if voff >= 0 then begin
          Buffer.add_char out '=';
          Buffer.add_substring out t.raw voff vlen
        end
      end;
      pos <- next
    done;
    let query = if first then Null else This (Buffer.contents out) in
    with_query t query
  end

let add_query_param (t : t @ local) ~key ~value =
  let key = pct_encode ~component:`Query_value key in
  let value = pct_encode ~component:`Query_value value in
  let binding = key ^ "=" ^ value in
  let query =
    match query t with
    | Null | This "" -> binding
    | This query -> query ^ "&" ^ binding
  in
  with_query t (This query)

let add_query_params (t : t) bindings =
  match bindings with
  | [] -> t
  | _ ->
    let out = Buffer.create 64 in
    (match query t with
     | Null | This "" -> ()
     | This query -> Buffer.add_string out query);
    List.iter
      (fun (key, value) ->
        if Buffer.length out > 0 then Buffer.add_char out '&';
        Buffer.add_string out (pct_encode ~component:`Query_value key);
        Buffer.add_char out '=';
        Buffer.add_string out (pct_encode ~component:`Query_value value))
      bindings;
    with_query t (This (Buffer.contents out))

let set_query_params ?(plus_as_space = false) (t : t) params =
  match params with
  | [] -> t
  | _ ->
    let out = Buffer.create (max 32 t.query_len) in
    let first = ref true in
    let separator () = if !first then first := false else Buffer.add_char out '&' in
    let mutable pos = query_cursor t in
    while pos >= 0 do
      let #(koff, klen, voff, vlen, next) = query_step t pos in
      if not (List.exists (fun (key, _) ->
        span_decodes_to ~plus:plus_as_space t.raw koff klen key) params) then begin
        separator ();
        let len = if voff < 0 then klen else voff + vlen - koff in
        Buffer.add_substring out t.raw koff len
      end;
      pos <- next
    done;
    List.iter (fun (key, value) ->
      separator ();
      Buffer.add_string out (pct_encode ~component:`Query_value key);
      Buffer.add_char out '=';
      Buffer.add_string out (pct_encode ~component:`Query_value value)) params;
    with_query t (This (Buffer.contents out))
