open Base

(* A resolver reaches an IPv4 address through inet_aton(3), which accepts far
   more spellings than the dotted quad: hexadecimal and octal parts, and one
   to four of them. Policy that compares host strings must recognize the same
   set the resolver does, so this is a parser rather than a character class. *)

let max_ipv4 : int = 0xFFFF_FFFF

let[@inline] digit_value c =
  match c with
  | '0' .. '9' -> Char.to_int c - 48
  | 'a' .. 'f' -> Char.to_int c - 87
  | 'A' .. 'F' -> Char.to_int c - 55
  | _ -> -1
;;

(* One part of a dotted address, [-1] when it is not a number in any of the
   three bases. A part wider than the whole address is rejected as it is
   scanned, so the accumulator cannot wrap. *)
let scan_part s ~off ~limit =
  if off >= limit
  then -1
  else (
    let base, start =
      if
        limit - off >= 2
        && Char.equal (String.get s off) '0'
        && (Char.equal (String.get s (off + 1)) 'x'
            || Char.equal (String.get s (off + 1)) 'X')
      then 16, off + 2
      else if limit - off >= 2 && Char.equal (String.get s off) '0'
      then 8, off + 1
      else 10, off
    in
    if start >= limit
    then -1
    else (
      let value = ref 0 in
      let pos = ref start in
      let bad = ref false in
      while (not !bad) && !pos < limit do
        let d = digit_value (String.get s !pos) in
        if d < 0 || d >= base
        then bad := true
        else (
          value := (!value * base) + d;
          if !value > max_ipv4 then bad := true else Stdlib.incr pos)
      done;
      if !bad then -1 else !value))
;;

let ipv4_of_string s =
  let raw_len = String.length s in
  if raw_len = 0
  then None
  else (
    (* Some inet_aton/getaddrinfo implementations accept one final root dot.
       Classify that spelling as an address on every platform so a name policy
       cannot become weaker when the executable is moved between libcs. *)
    let n =
      if raw_len > 1 && Char.equal (String.get s (raw_len - 1)) '.'
      then raw_len - 1
      else raw_len
    in
    let dots = ref 0 in
    for i = 0 to n - 1 do
      if Char.equal (String.get s i) '.' then Stdlib.incr dots
    done;
    if !dots > 3
    then None
    else (
      let count = !dots + 1 in
      (* The last part fills every byte the earlier ones left, so it is the
         only one allowed above 255. *)
      let tail_shift = 8 * (5 - count) in
      let acc = ref 0 in
      let idx = ref 0 in
      let off = ref 0 in
      let bad = ref false in
      while (not !bad) && !idx < count do
        let limit = ref !off in
        while !limit < n && not (Char.equal (String.get s !limit) '.') do
          Stdlib.incr limit
        done;
        let value = scan_part s ~off:!off ~limit:!limit in
        if value < 0
        then bad := true
        else if !idx < count - 1
        then
          if value > 255
          then bad := true
          else (
            acc := (!acc lsl 8) lor value;
            off := !limit + 1;
            Stdlib.incr idx)
        else if tail_shift < 32 && value >= 1 lsl tail_shift
        then bad := true
        else (
          acc := (!acc lsl tail_shift) lor value;
          off := !limit;
          Stdlib.incr idx)
      done;
      if !bad || !off <> n then None else Some !acc))
;;

let ipv4_canonical s =
  match ipv4_of_string s with
  | None -> None
  | Some v ->
    let byte shift = Int.to_string ((v lsr shift) land 0xFF) in
    Some (byte 24 ^ "." ^ byte 16 ^ "." ^ byte 8 ^ "." ^ byte 0)
;;

let is_ipv4_literal s =
  match ipv4_of_string s with
  | None -> false
  | Some _ -> true
;;

let is_ipv6_literal s =
  let n = String.length s in
  let value =
    if
      n >= 2
      && Char.equal (String.get s 0) '['
      && Char.equal (String.get s (n - 1)) ']'
    then String.sub s ~pos:1 ~len:(n - 2)
    else s
  in
  match Stdlib.String.index_opt value '%' with
  | None -> Httpz_uri.Scanner.is_ipv6 value
  | Some zone ->
    (* Raw socket APIs use [%zone], while RFC 6874 URI literals encode the
       delimiter as [%25zone]. This is intentionally a classifier, not an
       interface-name validator: recognizing too much fails a name allowlist
       closed, while missing a libc-accepted spelling can bypass it. *)
    zone > 0
    && zone + 1 < String.length value
    && Httpz_uri.Scanner.is_ipv6 (String.sub value ~pos:0 ~len:zone)
;;

let is_literal s = is_ipv4_literal s || is_ipv6_literal s
