(*
 * Copyright (c) 2012-2026 Anil Madhavapeddy <anil@recoil.org>
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

type spans = #{
  err : int;
  flags : int;
  shrink : int;
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

let host_none = 0
let host_reg_name = 1
let host_ipv4 = 2
let host_ipv6 = 3
let host_ipvfuture = 4

let flag_dirty = 1

(* {2 Character classification}

   The ABNF character sets, one predicate each.  These are the definitions;
   the byte tables below are built from them. *)

let[@inline] is_alpha c = match c with 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false
let[@inline] is_digit c = match c with '0' .. '9' -> true | _ -> false

let[@inline] is_unreserved c =
  match c with
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' -> true
  | _ -> false

let[@inline] is_sub_delim c =
  match c with
  | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' -> true
  | _ -> false

let[@inline] is_upper c = match c with 'A' .. 'Z' -> true | _ -> false
let[@inline] is_lower_hex c = match c with 'a' .. 'f' -> true | _ -> false
let[@inline] is_upper_hex c = match c with 'A' .. 'F' -> true | _ -> false

(* {2 Byte tables}

   Every scanner below asks the same question once per input byte: does this
   byte belong to some character set?  As a pattern match that is a decision
   tree of range comparisons whose branches follow the input byte and so
   mispredict, and the set is picked by a [cls] argument the scanners take at
   run time, which puts an indirect jump in front of the tree.  One byte per
   code point with one bit per set reduces the whole question to a load, an
   AND and a branch, with the set chosen by a mask the caller hoists out of
   its loop.

   Both tables are derived from the predicates above, so neither can drift
   away from the grammar. *)

let cls_userinfo = 0 (* unreserved / sub-delims / ":"                        *)
let cls_reg_name = 1 (* unreserved / sub-delims                              *)
let cls_pchar = 2 (* unreserved / sub-delims / ":" / "@"                     *)
let cls_qf = 3 (* pchar / "/" / "?"                                          *)
let cls_seg_nz_nc = 4 (* unreserved / sub-delims / "@"                       *)
let cls_ipvfuture = 5 (* unreserved / sub-delims / ":"                       *)

(* The two bits left over after the six classes.  They answer what the run
   scanners go on to ask about a byte they have just accepted. *)
let bit_upper = 1 lsl 6
let bit_unreserved = 1 lsl 7

let class_of c =
  let base = is_unreserved c || is_sub_delim c in
  let bit b cls = if b then 1 lsl cls else 0 in
  bit (base || c = ':') cls_userinfo
  lor bit base cls_reg_name
  lor bit (base || c = ':' || c = '@') cls_pchar
  lor bit (base || c = ':' || c = '@' || c = '/' || c = '?') cls_qf
  lor bit (base || c = '@') cls_seg_nz_nc
  lor bit (base || c = ':') cls_ipvfuture
  lor (if is_upper c then bit_upper else 0)
  lor (if is_unreserved c then bit_unreserved else 0)

let class_table =
  String.init 256 (fun i -> Char.unsafe_chr (class_of (Char.unsafe_chr i)))

(* The bit set of [c].  Test it against [1 lsl cls], [bit_upper] or
   [bit_unreserved], or against any [lor] of those in a single AND. *)
let[@inline] class_bits c = Char.code (String.unsafe_get class_table (Char.code c))

let[@inline] in_class cls c = class_bits c land (1 lsl cls) <> 0

(* Hex digit values, biased by one so that zero means "not a hex digit". *)

let hex_of c =
  match c with
  | '0' .. '9' -> Char.code c - 48
  | 'A' .. 'F' -> Char.code c - 55
  | 'a' .. 'f' -> Char.code c - 87
  | _ -> -1

let hex_table =
  String.init 256 (fun i -> Char.unsafe_chr (hex_of (Char.unsafe_chr i) + 1))

let[@inline] hex_val c = Char.code (String.unsafe_get hex_table (Char.code c)) - 1

let[@inline] is_hexdig c =
  Char.code (String.unsafe_get hex_table (Char.code c)) <> 0

(* {2 Combinators}

   A parser is a position transformer over immediates:

   {[ string -> int (* pos *) -> int (* limit *) -> int (* new pos, or -1 *) ]}

   Every combinator is [@inline always] with its parser arguments at mode
   [local].  The intermediate closures are at worst stack allocations, which
   plain [@zero_alloc] permits, and flambda2 erases them at the
   statically-applied call sites below.

   Use combinators for structure and direct computation for leaves that would
   otherwise backtrack.  Ordered choice re-scans from the start, so a leaf
   such as [dec-octet] written as its five ABNF branches costs twice what a
   direct value test does.

   These stay internal.  [@zero_alloc] cannot be attached to a combinator
   itself, because it calls an unknown closure.  The guarantee exists only at
   a fully inlined call site, so exporting them would advertise a proof that
   does not exist. *)

let[@inline always] lit c s i limit =
  if i < limit && String.unsafe_get s i = c then i + 1 else -1

let[@inline always] range lo hi s i limit =
  if i < limit then begin
    let c = String.unsafe_get s i in
    if c >= lo && c <= hi then i + 1 else -1
  end
  else -1

(* [p] then [q]. *)
let[@inline always] seq (p @ local) (q @ local) s i limit =
  let j = p s i limit in
  if j < 0 then -1 else q s j limit

(* Ordered choice: [p], else [q] from the same start. *)
let[@inline always] alt (p @ local) (q @ local) s i limit =
  let j = p s i limit in
  if j < 0 then q s i limit else j

(* Zero or more, greedy.  [p] must consume input when it succeeds. *)
let[@inline always] star (p @ local) s i limit =
  let mutable pos = i in
  let mutable go = true in
  while go do
    let j = p s pos limit in
    if j < 0 then go <- false else pos <- j
  done;
  pos

let[@inline always] plus (p @ local) s i limit =
  let j = p s i limit in
  if j < 0 then -1 else star p s j limit

(* Exactly [n] repetitions. *)
let[@inline always] count n (p @ local) s i limit =
  let mutable k = 0 in
  let mutable pos = i in
  while k < n && pos >= 0 do
    pos <- p s pos limit;
    k <- k + 1
  done;
  pos

(* Between [lo] and [hi] repetitions, greedy. *)
let[@inline always] between lo hi (p @ local) s i limit =
  let mutable k = 0 in
  let mutable pos = i in
  let mutable go = true in
  while go && k < hi do
    let j = p s pos limit in
    if j < 0 then go <- false
    else begin
      pos <- j;
      k <- k + 1
    end
  done;
  if k < lo then -1 else pos

(* Negative lookahead: succeed without consuming iff [p] fails. *)
let[@inline always] nfb (p @ local) s i limit =
  if p s i limit < 0 then i else -1

let[@inline always] digit s i limit = range '0' '9' s i limit
let[@inline always] hexdig s i limit = if i < limit && is_hexdig (String.unsafe_get s i) then i + 1 else -1

(* {2 IPv4}

   [dec-octet] is strict: 1-3 digits, no leading zeros, value <= 255.  The
   value test is direct rather than the RFC's five-way ordered choice, which
   backtracks.  The two accept the same language, checked exhaustively over
   the digit/dot alphabet.

   [ipv4_end] returns the offset just past a complete dotted-quad, or -1. *)

let[@inline always] [@zero_alloc] dec_octet s i limit =
  if i >= limit || not (is_digit (String.unsafe_get s i)) then -1
  else begin
    let d0 = Char.code (String.unsafe_get s i) - 48 in
    if d0 = 0 then i + 1
    else begin
      let mutable v = d0 in
      let mutable q = i + 1 in
      while q < limit && q - i < 3 && is_digit (String.unsafe_get s q) do
        v <- (v * 10) + (Char.code (String.unsafe_get s q) - 48);
        q <- q + 1
      done;
      if v > 255 then -1 else q
    end
  end

(* IPv4address = dec-octet 3*( "." dec-octet ), with a trailing digit refused
   so that an over-long octet cannot be truncated into a match. *)
let[@zero_alloc] ipv4_end s i limit =
  let p =
    seq
      (fun s i l -> count 3 (fun s i l -> seq dec_octet (lit '.') s i l) s i l)
      dec_octet s i limit
  in
  if p < 0 then -1 else nfb digit s p limit

(* {2 IPv6}

   [h16 = 1*4HEXDIG] is a combinator.  The surrounding control flow is not.

   The RFC writes [IPv6address] as nine alternatives whose optional prefixes
   overlap ([ *5( h16 ":" ) h16 ] "::" h16, and so on).  Transcribing that
   literally as ordered choice is wrong, because a PEG cannot backtrack into
   an alternative that already succeeded: for ["1::"] the greedy [*n( h16
   ":" )] consumes ["1:"], the following [h16] then fails, [opt] yields the
   empty prefix, and ["::"] no longer matches.  This production therefore
   uses the equivalent group-counting algorithm: 1-4 hex digits per group, at
   most one [::], a trailing dotted quad worth two groups, exactly 8 groups
   without [::] and strictly fewer than 8 with it.

   Returns the offset just past the address, or -1. *)

let[@inline always] [@zero_alloc] h16 s i limit =
  between 1 4 hexdig s i limit

let[@zero_alloc] ipv6_end s i limit =
  let mutable p = i in
  let mutable groups = 0 in
  let mutable comp = -1 in
  let mutable bad = false in
  let mutable more = false in
  if p < limit && String.unsafe_get s p = ':' then
    if p + 1 < limit && String.unsafe_get s (p + 1) = ':' then begin
      comp <- 0;
      p <- p + 2
    end
    else bad <- true;
  if not bad then more <- p < limit && is_hexdig (String.unsafe_get s p);
  while more && not bad do
    let v4 = ipv4_end s p limit in
    if v4 >= 0 then begin
      groups <- groups + 2;
      p <- v4;
      more <- false
    end
    else begin
      let q = h16 s p limit in
      if q < 0 || (q < limit && is_hexdig (String.unsafe_get s q)) then bad <- true
      else begin
        groups <- groups + 1;
        p <- q;
        if p < limit && String.unsafe_get s p = ':' then
          if p + 1 < limit && String.unsafe_get s (p + 1) = ':' then
            if comp >= 0 then bad <- true
            else begin
              comp <- groups;
              p <- p + 2;
              more <- p < limit && is_hexdig (String.unsafe_get s p)
            end
          else begin
            p <- p + 1;
            if p < limit && is_hexdig (String.unsafe_get s p) then more <- true
            else bad <- true
          end
        else more <- false
      end
    end
  done;
  if bad then -1
  else if comp >= 0 then if groups < 8 then p else -1
  else if groups = 8 then p
  else -1

(* {2 IPvFuture}

   [IPvFuture = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )].  A
   direct transcription; ABNF string literals are case-insensitive (RFC 5234
   §2.3), hence the ['v'] / ['V'] choice. *)

let[@inline always] ipvfuture_char s i limit =
  if i < limit && in_class cls_ipvfuture (String.unsafe_get s i) then i + 1
  else -1

let[@zero_alloc] ipvfuture_end s i limit =
  seq
    (fun s i l -> alt (lit 'v') (lit 'V') s i l)
    (fun s i l ->
      seq
        (fun s i l -> plus hexdig s i l)
        (fun s i l -> seq (lit '.') (fun s i l -> plus ipvfuture_char s i l) s i l)
        s i l)
    s i limit

(* {2 Run scanners}

   [scan_run] consumes a maximal run of class-[cls] characters and
   percent-encoded triplets.  It returns the stop offset together with the two
   normalization observations the canonicaliser needs:

   - [dirty] = 1 if the run is not already in canonical form (lowercase hex in
     a triplet, a triplet that encodes an unreserved character, or, when
     [low] = 1, an uppercase letter that host case-normalization must fold).
   - [shrink] = the number of bytes canonicalization will remove, i.e. two per
     triplet that decodes to an unreserved character.

   [bad] = 1 marks a malformed percent-encoding, with the stop offset pointing
   at the offending '%'.

   [mask] selects the class once, outside the loop.  [fold] is [bit_upper]
   when [low] = 1 and 0 otherwise, so the case-folding question costs an AND
   against the byte's bits rather than a test of [low] and a second
   classification.  ['%'] is in no class, so the class test can come first and
   the ordinary byte never pays for the triplet check. *)

let[@zero_alloc] scan_run s i limit cls low =
  let mask = 1 lsl cls in
  let fold = if low = 1 then bit_upper else 0 in
  let mutable p = i in
  let mutable dirty = 0 in
  let mutable shrink = 0 in
  let mutable bad = 0 in
  let mutable go = true in
  while go do
    if p >= limit then go <- false
    else begin
      let c = String.unsafe_get s p in
      let b = class_bits c in
      if b land mask <> 0 then begin
        if b land fold <> 0 then dirty <- 1;
        p <- p + 1
      end
      else if c = '%' then
        if p + 2 < limit then begin
          let h = hex_val (String.unsafe_get s (p + 1)) in
          let l = hex_val (String.unsafe_get s (p + 2)) in
          if h < 0 || l < 0 then begin
            bad <- 1;
            go <- false
          end
          else begin
            let dec = class_bits (Char.unsafe_chr ((h * 16) + l)) in
            if dec land (bit_unreserved lor fold) <> 0 then begin
              dirty <- 1;
              if dec land bit_unreserved <> 0 then shrink <- shrink + 2
            end
            else if
              is_lower_hex (String.unsafe_get s (p + 1))
              || is_lower_hex (String.unsafe_get s (p + 2))
            then dirty <- 1;
            p <- p + 3
          end
        end
        else begin
          bad <- 1;
          go <- false
        end
      else go <- false
    end
  done;
  #(p, dirty, shrink, bad)

(* Path scanning.  [nc] = 1 selects the [segment-nz-nc] charset (no ':') until
   the first '/', which is what [path-noscheme] requires of a relative
   reference with no scheme.  That switch is the only thing [mask] tracks, so
   it replaces the charset test and the "am I still in the first segment"
   test together.  Neither ['/'] nor ['%'] belongs to a path charset, so both
   fall out of the class test and are handled after it. *)

let[@zero_alloc] scan_path s i limit nc =
  let pchar = 1 lsl cls_pchar in
  let mutable mask = if nc = 1 then 1 lsl cls_seg_nz_nc else pchar in
  let mutable p = i in
  let mutable dirty = 0 in
  let mutable shrink = 0 in
  let mutable bad = 0 in
  let mutable go = true in
  while go do
    if p >= limit then go <- false
    else begin
      let c = String.unsafe_get s p in
      if class_bits c land mask <> 0 then p <- p + 1
      else if c = '/' then begin
        mask <- pchar;
        p <- p + 1
      end
      else if c = '%' then
        if p + 2 < limit then begin
          let h = hex_val (String.unsafe_get s (p + 1)) in
          let l = hex_val (String.unsafe_get s (p + 2)) in
          if h < 0 || l < 0 then begin
            bad <- 1;
            go <- false
          end
          else begin
            if class_bits (Char.unsafe_chr ((h * 16) + l)) land bit_unreserved <> 0
            then begin
              dirty <- 1;
              shrink <- shrink + 2
            end
            else if
              is_lower_hex (String.unsafe_get s (p + 1))
              || is_lower_hex (String.unsafe_get s (p + 2))
            then dirty <- 1;
            p <- p + 3
          end
        end
        else begin
          bad <- 1;
          go <- false
        end
      else go <- false
    end
  done;
  #(p, dirty, shrink, bad)

(* {2 Scheme and port}

   [scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )] and
   [port = *DIGIT], both straight transcriptions.  [port_end] only delimits
   the digits.  The numeric value is computed separately, since a combinator
   returns a position and nothing else. *)

let[@inline always] alpha s i limit =
  if i < limit && is_alpha (String.unsafe_get s i) then i + 1 else -1

let[@inline always] scheme_char s i limit =
  if i < limit then begin
    let c = String.unsafe_get s i in
    if is_alpha c || is_digit c || c = '+' || c = '-' || c = '.' then i + 1
    else -1
  end
  else -1

let[@zero_alloc] scheme_end s i limit =
  seq alpha (fun s i l -> star scheme_char s i l) s i limit

let[@zero_alloc] port_end s i limit = star digit s i limit

(* {2 The scanner} *)

let[@zero_alloc] parse_sub s ~pos ~len =
  (* [n] is the exclusive limit every combinator is given, so no read can
     leave the window.  An out-of-range window fails at once with [err = 1];
     [n] is then never consulted. *)
  let n = pos + len in
  let mutable dirty = 0 in
  let mutable shrink = 0 in
  let mutable err =
    if pos < 0 || len < 0 || pos > String.length s - len then 1 else 0
  in
  let mutable p = pos in
  let mutable scheme_len = -1 in
  let mutable userinfo_off = -1 in
  let mutable userinfo_len = 0 in
  let mutable host_off = -1 in
  let mutable host_len = 0 in
  let mutable host_kind = 0 in
  let mutable port_off = -1 in
  let mutable port_len = 0 in
  let mutable port_val = -1 in
  let mutable path_off = pos in
  let mutable path_len = 0 in
  let mutable query_off = -1 in
  let mutable query_len = 0 in
  let mutable frag_off = -1 in
  let mutable frag_len = 0 in
  if err = 0 then begin
    (* scheme, when followed by ":".  [scheme_len] is a length, so it is the
       one field measured from [pos] rather than from the start of [s]. *)
    let q = scheme_end s pos n in
    if q >= 0 && q < n && String.unsafe_get s q = ':' then begin
      scheme_len <- q - pos;
      p <- q + 1;
      let mutable k = pos in
      while k < q do
        if is_upper (String.unsafe_get s k) then dirty <- 1;
        k <- k + 1
      done
    end;
    (* authority = [ userinfo "@" ] host [ ":" port ], introduced by "//" *)
    if
      p + 1 < n
      && String.unsafe_get s p = '/'
      && String.unsafe_get s (p + 1) = '/'
    then begin
      p <- p + 2;
      let #(q, d, sh, bad) = scan_run s p n cls_userinfo 0 in
      if bad = 0 && q < n && String.unsafe_get s q = '@' then begin
        userinfo_off <- p;
        userinfo_len <- q - p;
        dirty <- dirty lor d;
        shrink <- shrink + sh;
        p <- q + 1
      end;
      if p < n && String.unsafe_get s p = '[' then begin
        let e6 = ipv6_end s (p + 1) n in
        if e6 >= 0 && e6 < n && String.unsafe_get s e6 = ']' then begin
          host_kind <- host_ipv6;
          host_off <- p + 1;
          host_len <- e6 - p - 1;
          let mutable k = host_off in
          while k < e6 do
            if is_upper_hex (String.unsafe_get s k) then dirty <- 1;
            k <- k + 1
          done;
          p <- e6 + 1
        end
        else begin
          let ef = ipvfuture_end s (p + 1) n in
          if ef >= 0 && ef < n && String.unsafe_get s ef = ']' then begin
            host_kind <- host_ipvfuture;
            host_off <- p + 1;
            host_len <- ef - p - 1;
            p <- ef + 1
          end
          else err <- p + 1
        end
      end
      else begin
        let #(q, d, sh, bad) = scan_run s p n cls_reg_name 1 in
        if bad = 1 then err <- q + 1
        else begin
          dirty <- dirty lor d;
          shrink <- shrink + sh;
          host_off <- p;
          host_len <- q - p;
          host_kind <- (if q > p && ipv4_end s p q = q then host_ipv4 else host_reg_name);
          p <- q
        end
      end;
      if err = 0 && p < n && String.unsafe_get s p = ':' then begin
        let q = port_end s (p + 1) n in
        let mutable v = 0 in
        let mutable ov = 0 in
        let mutable k = p + 1 in
        while k < q do
          let d = Char.code (String.unsafe_get s k) - 48 in
          if v > (max_int - d) / 10 then ov <- 1 else v <- (v * 10) + d;
          k <- k + 1
        done;
        port_off <- p + 1;
        port_len <- q - p - 1;
        if port_len = 0 then port_val <- -1
        else if ov = 1 then err <- p + 2
        else port_val <- v;
        p <- q
      end
    end
  end;
  (* path *)
  if err = 0 then begin
    path_off <- p;
    if host_off >= 0 then begin
      (* path-abempty: empty, or every segment introduced by '/' *)
      let c = if p < n then String.unsafe_get s p else '\000' in
      if p < n && c <> '/' && c <> '?' && c <> '#' then err <- p + 1
      else begin
        let #(q, d, sh, bad) = scan_path s p n 0 in
        if bad = 1 then err <- q + 1
        else begin
          dirty <- dirty lor d;
          shrink <- shrink + sh;
          path_len <- q - p;
          p <- q
        end
      end
    end
    else begin
      let nc = if scheme_len < 0 then 1 else 0 in
      let #(q, d, sh, bad) = scan_path s p n nc in
      if bad = 1 then err <- q + 1
      else begin
        dirty <- dirty lor d;
        shrink <- shrink + sh;
        path_len <- q - p;
        p <- q
      end
    end
  end;
  (* query *)
  if err = 0 && p < n && String.unsafe_get s p = '?' then begin
    let #(q, d, sh, bad) = scan_run s (p + 1) n cls_qf 0 in
    if bad = 1 then err <- q + 1
    else begin
      dirty <- dirty lor d;
      shrink <- shrink + sh;
      query_off <- p + 1;
      query_len <- q - p - 1;
      p <- q
    end
  end;
  (* fragment *)
  if err = 0 && p < n && String.unsafe_get s p = '#' then begin
    let #(q, d, sh, bad) = scan_run s (p + 1) n cls_qf 0 in
    if bad = 1 then err <- q + 1
    else begin
      dirty <- dirty lor d;
      shrink <- shrink + sh;
      frag_off <- p + 1;
      frag_len <- q - p - 1;
      p <- q
    end
  end;
  if err = 0 && p < n then err <- p + 1;
  #{ err = err;
     flags = if dirty = 1 then flag_dirty else 0;
     shrink = shrink;
     scheme_len = scheme_len;
     userinfo_off = userinfo_off;
     userinfo_len = userinfo_len;
     host_off = host_off;
     host_len = host_len;
     host_kind = host_kind;
     port_off = port_off;
     port_len = port_len;
     port_val = port_val;
     path_off = path_off;
     path_len = path_len;
     query_off = query_off;
     query_len = query_len;
     frag_off = frag_off;
     frag_len = frag_len }

let[@zero_alloc] parse s = parse_sub s ~pos:0 ~len:(String.length s)

(* {2 Accessors} *)

let[@inline] [@zero_alloc] err (v : spans) = v.#err
let[@inline] [@zero_alloc] is_valid (v : spans) = v.#err = 0
let[@inline] [@zero_alloc] error_offset (v : spans) = v.#err - 1
let[@inline] [@zero_alloc] needs_normalization (v : spans) = v.#flags land flag_dirty <> 0
let[@inline] [@zero_alloc] shrink (v : spans) = v.#shrink
let[@inline] [@zero_alloc] scheme_len (v : spans) = v.#scheme_len
let[@inline] [@zero_alloc] userinfo_off (v : spans) = v.#userinfo_off
let[@inline] [@zero_alloc] userinfo_len (v : spans) = v.#userinfo_len
let[@inline] [@zero_alloc] host_off (v : spans) = v.#host_off
let[@inline] [@zero_alloc] host_len (v : spans) = v.#host_len
let[@inline] [@zero_alloc] host_kind (v : spans) = v.#host_kind
let[@inline] [@zero_alloc] port_off (v : spans) = v.#port_off
let[@inline] [@zero_alloc] port_len (v : spans) = v.#port_len
let[@inline] [@zero_alloc] port_val (v : spans) = v.#port_val
let[@inline] [@zero_alloc] path_off (v : spans) = v.#path_off
let[@inline] [@zero_alloc] path_len (v : spans) = v.#path_len
let[@inline] [@zero_alloc] query_off (v : spans) = v.#query_off
let[@inline] [@zero_alloc] query_len (v : spans) = v.#query_len
let[@inline] [@zero_alloc] frag_off (v : spans) = v.#frag_off
let[@inline] [@zero_alloc] frag_len (v : spans) = v.#frag_len

let[@zero_alloc] is_ipv4 s = ipv4_end s 0 (String.length s) = String.length s
let[@zero_alloc] is_ipv6 s = ipv6_end s 0 (String.length s) = String.length s

(* {2 Percent decoding into a caller's buffer}

   The decoded form is never longer than the input, so [len] bytes of room at
   [dst_pos] always suffice.  Both window and destination are checked once at
   entry; the loop then runs unchecked. *)

let[@zero_alloc] needs_decode s ~pos ~len ~plus_as_space =
  let n = String.length s in
  if pos < 0 || len < 0 || pos > n - len then false
  else begin
    let e = pos + len in
    let mutable i = pos in
    let mutable found = false in
    while (not found) && i < e do
      let c = String.unsafe_get s i in
      if c = '%' || (plus_as_space && c = '+') then found <- true else i <- i + 1
    done;
    found
  end

let[@zero_alloc] pct_decode_into s ~pos ~len ~dst ~dst_pos ~plus_as_space =
  if
    pos < 0 || len < 0
    || pos > String.length s - len
    || dst_pos < 0
    || dst_pos > Bytes.length dst - len
  then -1
  else begin
    let e = pos + len in
    let mutable i = pos in
    let mutable k = dst_pos in
    let mutable bad = false in
    while (not bad) && i < e do
      let c = String.unsafe_get s i in
      if c = '%' then
        if i + 2 < e then begin
          let h = hex_val (String.unsafe_get s (i + 1)) in
          let l = hex_val (String.unsafe_get s (i + 2)) in
          if h < 0 || l < 0 then bad <- true
          else begin
            Bytes.unsafe_set dst k (Char.unsafe_chr ((h * 16) + l));
            k <- k + 1;
            i <- i + 3
          end
        end
        else bad <- true
      else begin
        Bytes.unsafe_set dst k (if plus_as_space && c = '+' then ' ' else c);
        k <- k + 1;
        i <- i + 1
      end
    done;
    if bad then -1 else k - dst_pos
  end
