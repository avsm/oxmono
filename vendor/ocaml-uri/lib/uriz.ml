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
   at all.  A local record may hold global fields, so the canonicalized path,
   where [raw] is a fresh heap string, templates just as well.  Every field
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
let write_run dst k (s : string @ local) off len low =
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
let canonicalize (s : string @ local) (v : Raw.spans) =
  let out = Bytes.create (String.length s - Raw.shrink v) in
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
      k <- write_run out k s host_off (Raw.host_len v) (kind = Raw.host_ipv6);
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
    let raw = canonicalize s v in
    let v = Raw.parse raw in
    if Raw.is_valid v then (some_of_spans [@mode m]) raw v [@exclave_if_local m]
    else Null
  end

(* Parse without ever normalizing: input that is not already canonical is
   rejected rather than rewritten.  Nothing can allocate, so the record goes
   on the stack, the canonical string {i is} the caller's string, and
   [@zero_alloc] proves the whole call heap-free.  This is the hot-path entry
   point: parse, read spans, drop.  Fall back to {!of_string} on [Null]. *)
let[@zero_alloc] of_string_canonical (s : string @ local) : t or_null @ local =
  let v = Raw.parse s in
  if (not (Raw.is_valid v)) || Raw.needs_normalization v then Null
  else exclave_ (some_of_spans [@mode local]) s v

(* [of_string] phrased as tailcalls to the record builder instead of a match
   on an [or_null], which is what lets the result be exclaved.  Every internal
   producer finishes here, so they all inherit the mode polymorphism. *)
let%template[@mode m = (global, local)] of_string_exn (s : string @ m) : t @ m =
  let v = Raw.parse s in
  if Raw.err v <> 0 then
    invalid_arg
      ("Uriz.of_string_exn: not a URI reference: " ^ sub s 0 (String.length s))
  else if not (Raw.needs_normalization v) then
    (t_of_spans [@mode m]) s v [@exclave_if_local m]
  else begin
    let raw = canonicalize s v in
    let v = Raw.parse raw in
    (t_of_spans [@mode m]) raw v [@exclave_if_local m]
  end

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
let port_int (t : t @ local) = t.port_val
let has_authority (t : t @ local) = t.host_off >= 0
let is_absolute (t : t @ local) = t.scheme_len >= 0

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
      (base_safe c && c <> '&' && c <> '=' && c <> '+' && c <> ';')
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

(* Like [encode_with], but a well-formed "%XX" triplet is passed through
   rather than being re-encoded as "%25XX".  This is what lets [make] and the
   [with_*] functions accept already-encoded component text. *)
let encode_preserving tbl s =
  let n = String.length s in
  let[@inline] triplet_at i =
    i + 2 < n
    && String.unsafe_get s i = '%'
    && Raw.is_hexdig (String.unsafe_get s (i + 1))
    && Raw.is_hexdig (String.unsafe_get s (i + 2))
  in
  let mutable unsafe_count = 0 in
  let mutable i = 0 in
  while i < n do
    if triplet_at i then i <- i + 3
    else begin
      if not (safe tbl (String.unsafe_get s i)) then
        unsafe_count <- unsafe_count + 1;
      i <- i + 1
    end
  done;
  if unsafe_count = 0 then s
  else begin
    let out = Bytes.create (n + (2 * unsafe_count)) in
    let mutable k = 0 in
    let mutable i = 0 in
    while i < n do
      if triplet_at i then begin
        Bytes.blit_string s i out k 3;
        k <- k + 3;
        i <- i + 3
      end
      else begin
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
      end
    done;
    Bytes.unsafe_to_string out
  end

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

let fragment_decoded (t : t @ local) =
  if t.frag_off < 0 then Null
  else This (decode_span ~plus:false t.raw t.frag_off t.frag_len)

let userinfo_decoded (t : t @ local) =
  if t.userinfo_off < 0 then Null
  else This (decode_span ~plus:false t.raw t.userinfo_off t.userinfo_len)

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

(* RFC 3986 §5.3 leaves two recomposition hazards to the caller: a path
   starting with "//" would be re-read as an authority, and a first segment
   containing ':' would be re-read as a scheme.  Both are fixed by a prefix. *)
let add_path_guarded buf ~has_scheme ~has_auth d k =
  if (not has_auth) && k >= 2 && Bytes.get d 0 = '/' && Bytes.get d 1 = '/' then
    Buffer.add_string buf "/."
  else if (not has_scheme) && (not has_auth) && k > 0 then begin
    let mutable i = 0 in
    let mutable colon = false in
    while i < k && Bytes.unsafe_get d i <> '/' do
      if Bytes.unsafe_get d i = ':' then colon <- true;
      i <- i + 1
    done;
    if colon then Buffer.add_string buf "./"
  end;
  Buffer.add_subbytes buf d 0 k

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

(* A composed result is canonical by construction: every byte is copied from a
   canonical string, dot-segment removal only deletes whole segments, and the
   guards are themselves canonical.  Checking rather than re-canonicalizing
   keeps the local path free of the heap [Bytes] that {!canonicalize} would
   allocate. *)
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

let valid_scheme s =
  let n = String.length s in
  n > 0
  && Raw.is_alpha (String.unsafe_get s 0)
  &&
  let mutable i = 1 in
  let mutable ok = true in
  while i < n do
    let c = String.unsafe_get s i in
    if not (Raw.is_alpha c || Raw.is_digit c || c = '+' || c = '-' || c = '.')
    then ok <- false;
    i <- i + 1
  done;
  ok

(* [build] and its callers only move the record: the component text goes
   through [encode_preserving] and [Buffer], which allocate heap strings
   whatever the mode, so the [__local] variants save the record and nothing
   else. *)
let%template[@mode m = (global, local)] build ~scheme ~userinfo ~host ~port
    ~path ~query ~fragment : t @ m =
  let buf = Buffer.create 64 in
  (match scheme with
  | Null -> ()
  | This s ->
    if not (valid_scheme s) then invalid_arg ("Uriz: invalid scheme: " ^ s);
    Buffer.add_string buf s;
    Buffer.add_char buf ':');
  let has_auth =
    match host with
    | This _ -> true
    | Null -> ( match userinfo, port with Null, Null -> false | _ -> true)
  in
  if has_auth then begin
    Buffer.add_string buf "//";
    (match userinfo with
    | Null -> ()
    | This u ->
      Buffer.add_string buf (encode_preserving tbl_userinfo u);
      Buffer.add_char buf '@');
    (match host with
    | Null -> ()
    | This h ->
      let n = String.length h in
      if n >= 2 && h.[0] = '[' && h.[n - 1] = ']' then Buffer.add_string buf h
      else if Raw.is_ipv6 h then begin
        Buffer.add_char buf '[';
        Buffer.add_string buf h;
        Buffer.add_char buf ']'
      end
      else Buffer.add_string buf (encode_preserving tbl_host h));
    match port with
    | Null -> ()
    | This p ->
      if p < 0 then invalid_arg "Uriz: negative port";
      Buffer.add_char buf ':';
      Buffer.add_string buf (string_of_int p)
  end;
  let path = encode_preserving tbl_path path in
  let plen = String.length path in
  if plen > 0 then
    if has_auth then begin
      if path.[0] <> '/' then Buffer.add_char buf '/';
      Buffer.add_string buf path
    end
    else begin
      let d = Bytes.unsafe_of_string path in
      add_path_guarded buf ~has_scheme:(scheme <> Null) ~has_auth d plen
    end;
  (match query with
  | Null -> ()
  | This q ->
    Buffer.add_char buf '?';
    Buffer.add_string buf (encode_preserving tbl_query q));
  (match fragment with
  | Null -> ()
  | This f ->
    Buffer.add_char buf '#';
    Buffer.add_string buf (encode_preserving tbl_query f));
  let raw = Buffer.contents buf in
  (of_string_exn [@mode m]) raw [@exclave_if_local m]

let[@inline] of_opt = function None -> Null | Some x -> This x

let%template[@mode m = (global, local)] make ?scheme ?userinfo ?host ?port
    ?path ?query ?fragment () : t @ m =
  let scheme = of_opt scheme in
  let userinfo = of_opt userinfo in
  let host = of_opt host in
  let port = of_opt port in
  let path = match path with None -> "" | Some p -> p in
  let query = of_opt query in
  let fragment = of_opt fragment in
  (build [@mode m]) ~scheme ~userinfo ~host ~port ~path ~query ~fragment
  [@exclave_if_local m]

(* The [with_*] functions decompose into the encoded component views and
   rebuild.  [encode_preserving] leaves the views untouched, since they are
   already legal. *)
let cur_scheme t = scheme t
let cur_userinfo t = userinfo t
let cur_host t = host t
let cur_port t = if t.port_val < 0 then Null else This t.port_val
let cur_query t = query t
let cur_fragment t = fragment t

let%template[@mode m = (global, local)] with_scheme (t : t @ local) scheme
    : t @ m =
  let userinfo = cur_userinfo t in
  let host = cur_host t in
  let port = cur_port t in
  let path = path t in
  let query = cur_query t in
  let fragment = cur_fragment t in
  (build [@mode m]) ~scheme ~userinfo ~host ~port ~path ~query ~fragment
  [@exclave_if_local m]

let%template[@mode m = (global, local)] with_userinfo (t : t @ local) userinfo
    : t @ m =
  let scheme = cur_scheme t in
  let host = cur_host t in
  let port = cur_port t in
  let path = path t in
  let query = cur_query t in
  let fragment = cur_fragment t in
  (build [@mode m]) ~scheme ~userinfo ~host ~port ~path ~query ~fragment
  [@exclave_if_local m]

(* Dropping the host drops the whole authority, since RFC 3986 has no
   authority without one. *)
let%template[@mode m = (global, local)] with_host (t : t @ local) host : t @ m =
  let scheme = cur_scheme t in
  let userinfo = match host with Null -> Null | This _ -> cur_userinfo t in
  let port = match host with Null -> Null | This _ -> cur_port t in
  let path = path t in
  let query = cur_query t in
  let fragment = cur_fragment t in
  (build [@mode m]) ~scheme ~userinfo ~host ~port ~path ~query ~fragment
  [@exclave_if_local m]

let%template[@mode m = (global, local)] with_port (t : t @ local) port : t @ m =
  let scheme = cur_scheme t in
  let userinfo = cur_userinfo t in
  let host = cur_host t in
  let path = path t in
  let query = cur_query t in
  let fragment = cur_fragment t in
  (build [@mode m]) ~scheme ~userinfo ~host ~port ~path ~query ~fragment
  [@exclave_if_local m]

let%template[@mode m = (global, local)] with_path (t : t @ local) path : t @ m =
  let scheme = cur_scheme t in
  let userinfo = cur_userinfo t in
  let host = cur_host t in
  let port = cur_port t in
  let query = cur_query t in
  let fragment = cur_fragment t in
  (build [@mode m]) ~scheme ~userinfo ~host ~port ~path ~query ~fragment
  [@exclave_if_local m]

let%template[@mode m = (global, local)] with_query (t : t @ local) query
    : t @ m =
  let scheme = cur_scheme t in
  let userinfo = cur_userinfo t in
  let host = cur_host t in
  let port = cur_port t in
  let path = path t in
  let fragment = cur_fragment t in
  (build [@mode m]) ~scheme ~userinfo ~host ~port ~path ~query ~fragment
  [@exclave_if_local m]

let%template[@mode m = (global, local)] with_fragment (t : t @ local) fragment
    : t @ m =
  let scheme = cur_scheme t in
  let userinfo = cur_userinfo t in
  let host = cur_host t in
  let port = cur_port t in
  let path = path t in
  let query = cur_query t in
  (build [@mode m]) ~scheme ~userinfo ~host ~port ~path ~query ~fragment
  [@exclave_if_local m]
