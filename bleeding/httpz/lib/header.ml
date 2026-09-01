open Base
module Name = Header_name
module Char_u = Stdlib_stable.Char_u

module Syntax = struct
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
      while i < stop && Buf_read.is_token_char (char_at s i) do
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
      valid <- Buf_read.is_field_value_char (char_at s i);
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
          if i + 1 < stop && Buf_read.is_quoted_pair_char (char_at s (i + 1))
          then i <- i + 2
          else valid <- false
        else if Buf_read.is_qdtext_char c
        then i <- i + 1
        else valid <- false
      done;
      valid)
  ;;

  let quote_string (local_ s : string) =
    if not (is_field_value s)
    then invalid_arg "Httpz.Header.Syntax.quote_string: forbidden control byte";
    let b = Buffer.create (String.length s + 2) in
    Buffer.add_char b '"';
    let mutable i = 0 in
    while i < String.length s do
      let c = String.unsafe_get s i in
      if Char.equal c '"' || Char.equal c '\\' then Buffer.add_char b '\\';
      Buffer.add_char b c;
      i <- i + 1
    done;
    Buffer.add_char b '"';
    Buffer.contents b
  ;;

  let unquote_string (local_ s : string) =
    let len = String.length s in
    if not (is_quoted_string_sub s ~pos:0 ~len)
    then None
    else (
      let b = Buffer.create (len - 2) in
      let mutable i = 1 in
      while i < len - 1 do
        let c = String.unsafe_get s i in
        if Char.equal c '\\'
        then (
          i <- i + 1;
          Buffer.add_char b (String.unsafe_get s i))
        else Buffer.add_char b c;
        i <- i + 1
      done;
      Some (Buffer.contents b))
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
end

type t =
  { name : Name.t
  ; name_span : Span.t
  ; value : Span.t
  }

let rec find (headers : t list @ local) name = exclave_
  match headers with
  | [] -> None
  | hdr :: rest ->
    let matches =
      match name, hdr.name with
      | Name.Other, _ | _, Name.Other -> false
      | n1, n2 -> phys_equal n1 n2
    in
    if matches then Some hdr else find rest name
;;

(* Every field carries the name span it was parsed from, which lets recognized and
   extension names take one path. *)
let rec find_lowercase (local_ (buf : bytes)) (headers : t list @ local) name = exclave_
  match headers with
  | [] -> None
  | hdr :: rest ->
    if Span.equal_caseless buf hdr.name_span name
    then Some hdr
    else find_lowercase buf rest name
;;

(* [Span.equal_caseless] folds only the buffer side, so [name] is lowered once here rather
   than once per field. *)
let find_string (local_ (buf : bytes)) (headers : t list @ local) name = exclave_
  find_lowercase buf headers (String.lowercase name)
;;

let to_string_pair (buf : bytes) t =
  let name =
    match t.name with
    | Name.Other -> Span.to_string buf t.name_span
    | known -> Name.canonical known
  in
  let value = Span.to_string buf t.value in
  name, value
;;

let to_string_pairs (buf : bytes) headers = List.map headers ~f:(to_string_pair buf)

let rec to_string_pairs_local (buf : bytes) (headers : t list @ local) =
  match headers with
  | [] -> []
  | hdr :: rest ->
    let pair = to_string_pair buf hdr in
    pair :: to_string_pairs_local buf rest
;;

let pp_with_buf (buf : bytes) fmt t =
  let name, value = to_string_pair buf t in
  Stdlib.Format.fprintf fmt "%s: %s" name value
;;

let pp fmt t =
  Stdlib.Format.fprintf
    fmt
    "{ name = %a; name_span = #{ off = %d; len = %d }; value = #{ off = %d; len = %d } }"
    Name.pp
    t.name
    (Span.off t.name_span)
    (Span.len t.name_span)
    (Span.off t.value)
    (Span.len t.value)
;;
