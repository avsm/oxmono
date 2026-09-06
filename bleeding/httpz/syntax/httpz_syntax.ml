open Base
module Char_u = Stdlib_stable.Char_u

let[@inline always] is_token_char (c : char#) =
  match c with
  | #'a' .. #'z' | #'A' .. #'Z' | #'0' .. #'9' -> true
  | #'!' | #'#' | #'$' | #'%' | #'&' | #'\'' | #'*' | #'+' | #'-' | #'.' -> true
  | #'^' | #'_' | #'`' | #'|' | #'~' -> true
  | _ -> false
;;

let[@inline always] is_space (c : char#) =
  match c with
  | #' ' | #'\t' -> true
  | _ -> false
;;

let[@inline always] is_field_value_char (c : char#) =
  let code = Char_u.code c in
  code = 0x09 || (code >= 0x20 && code <> 0x7f)
;;

let[@inline always] is_qdtext_char (c : char#) =
  let code = Char_u.code c in
  code = 0x09
  || code = 0x20
  || code = 0x21
  || (code >= 0x23 && code <= 0x5b)
  || (code >= 0x5d && code <= 0x7e)
  || code >= 0x80
;;

let[@inline always] is_quoted_pair_char (c : char#) = is_field_value_char c

let[@inline always] is_digit (c : char#) =
  match c with
  | #'0' .. #'9' -> true
  | _ -> false
;;

let[@inline always] digit_value (c : char#) : int =
  match c with
  | #'0' .. #'9' -> Char_u.code c - 48
  | _ -> -1
;;

let[@inline always] to_lower (c : char#) : char# =
  match c with
  | #'A' .. #'Z' -> Char_u.chr (Char_u.code c + 32)
  | _ -> c
;;

let[@inline always] char_at (local_ s : string) i =
  Char_u.of_char (String.unsafe_get s i)
;;

let[@inline] valid_bounds (local_ s : string) ~pos ~len =
  let total = String.length s in
  pos >= 0 && len >= 0 && len <= total && pos <= total - len
;;

let[@zero_alloc] is_token_sub (local_ s : string) ~pos ~len =
  if len = 0 || not (valid_bounds s ~pos ~len)
  then false
  else (
    let stop = pos + len in
    let mutable i = pos in
    while i < stop && is_token_char (char_at s i) do
      i <- i + 1
    done;
    i = stop)
;;

let[@zero_alloc] is_token (local_ s : string) =
  is_token_sub s ~pos:0 ~len:(String.length s)
;;

let[@zero_alloc] is_field_value (local_ s : string) =
  let mutable i = 0 in
  let mutable valid = true in
  while valid && i < String.length s do
    valid <- is_field_value_char (char_at s i);
    i <- i + 1
  done;
  valid
;;

let[@zero_alloc] is_quoted_string_sub (local_ s : string) ~pos ~len =
  if len < 2
     || not (valid_bounds s ~pos ~len)
     || not (Char_u.equal (char_at s pos) #'"')
     || not (Char_u.equal (char_at s (pos + len - 1)) #'"')
  then false
  else (
    let stop = pos + len - 1 in
    let mutable i = pos + 1 in
    let mutable valid = true in
    while valid && i < stop do
      let c = char_at s i in
      if Char_u.equal c #'\\'
      then
        if i + 1 < stop && is_quoted_pair_char (char_at s (i + 1))
        then i <- i + 2
        else valid <- false
      else if is_qdtext_char c
      then i <- i + 1
      else valid <- false
    done;
    valid)
;;

let quote_string (local_ s : string) =
  if not (is_field_value s) then
    invalid_arg "Httpz.Header.Syntax.quote_string: byte forbidden in a field value";
  let len = String.length s in
  let mutable size = len + 2 in
  for i = 0 to len - 1 do
    let c = String.unsafe_get s i in
    if Char.equal c '"' || Char.equal c '\\' then size <- size + 1
  done;
  let out = Stdlib.Bytes.create size in
  Stdlib.Bytes.unsafe_set out 0 '"';
  let mutable k = 1 in
  for i = 0 to len - 1 do
    let c = String.unsafe_get s i in
    if Char.equal c '"' || Char.equal c '\\' then begin
      Stdlib.Bytes.unsafe_set out k '\\'; k <- k + 1
    end;
    Stdlib.Bytes.unsafe_set out k c; k <- k + 1
  done;
  Stdlib.Bytes.unsafe_set out k '"';
  Stdlib.Bytes.unsafe_to_string out
;;

let unquote_string (local_ s : string) =
  let len = String.length s in
  if not (is_quoted_string_sub s ~pos:0 ~len) then None
  else begin
    let mutable size = 0 in
    let mutable i = 1 in
    while i < len - 1 do
      i <- i + (if Char.equal (String.unsafe_get s i) '\\' then 2 else 1);
      size <- size + 1
    done;
    let out = Stdlib.Bytes.create size in
    let mutable i = 1 in
    for k = 0 to size - 1 do
      if Char.equal (String.unsafe_get s i) '\\' then i <- i + 1;
      Stdlib.Bytes.unsafe_set out k (String.unsafe_get s i);
      i <- i + 1
    done;
    Some (Stdlib.Bytes.unsafe_to_string out)
  end
;;

let[@zero_alloc] qvalue_sub (local_ s : string) ~pos ~len =
  if len < 1 || len > 5 || not (valid_bounds s ~pos ~len)
  then -1
  else
    let first = char_at s pos in
    if len = 1
    then if Char_u.equal first #'0' then 0 else if Char_u.equal first #'1' then 1000 else -1
    else if not (Char_u.equal (char_at s (pos + 1)) #'.')
    then -1
    else (
      let stop = pos + len in
      let mutable i = pos + 2 in
      let mutable value = 0 in
      let mutable scale = 100 in
      let mutable valid = true in
      while valid && i < stop do
        let c = char_at s i in
        let digit = Char_u.code c - Char_u.code #'0' in
        if digit < 0 || digit > 9 || (Char_u.equal first #'1' && digit <> 0)
        then valid <- false
        else (
          value <- value + (digit * scale);
          scale <- scale / 10;
          i <- i + 1)
      done;
      if not valid
      then -1
      else if Char_u.equal first #'0'
      then value
      else if Char_u.equal first #'1'
      then 1000
      else -1)
;;

let rec skip_space (local_ s : string) i stop =
  if i < stop && is_space (char_at s i) then skip_space s (i + 1) stop else i
;;

let rec trim_space (local_ s : string) start i =
  if i > start && is_space (char_at s (i - 1))
  then trim_space s start (i - 1)
  else i
;;

let rec equal_from
    (local_ a : string) a0 (local_ b : string) b0 i len =
  i = len
  || (Char_u.equal
        (to_lower (char_at a (a0 + i)))
        (to_lower (char_at b (b0 + i)))
      && equal_from a a0 b b0 (i + 1) len)
;;

let[@inline] equal_ci (local_ a : string) a0 (local_ b : string) b0 len =
  equal_from a a0 b b0 0 len
;;
