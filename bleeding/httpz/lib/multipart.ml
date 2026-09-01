open Base
module Char_u = Stdlib_stable.Char_u

type part =
  { name : string
  ; filename : string option
  ; content_type : string option
  ; headers : (string * string) list
  ; off : int
  ; len : int
  }

(* [sub] reads its source at [local], which [String.sub] does not. *)
let sub (s : string @ local) ~pos ~len =
  let b = Stdlib.Bytes.create len in
  Stdlib.Bytes.unsafe_blit_string s pos b 0 len;
  Stdlib.Bytes.unsafe_to_string b
;;

let content (body : string @ local) p = sub body ~pos:p.off ~len:p.len

let max_part_headers = 32
let max_header_line = 8192
let default_max_parts = 256

let[@inline] is_ows (c : char) = Buf_read.is_space (Char_u.of_char c)

(* RFC 9110 token, which a field name and an unquoted parameter both are. *)
let[@inline] is_tchar (c : char) = Buf_read.is_token_char (Char_u.of_char c)

(* RFC 2046 bcharsnospace, plus SP everywhere but the last position. *)
let[@inline] is_bchar_nospace (c : char) =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | '\'' | '(' | ')' | '+' | '_' | ',' | '-' | '.' | '/' | ':' | '=' | '?' ->
    true
  | _ -> false
;;

let valid_boundary b =
  let n = String.length b in
  if n < 1 || n > 70 || Char.equal (String.get b (n - 1)) ' '
  then false
  else (
    let ok = ref true in
    let i = ref 0 in
    while !ok && !i < n do
      let c = String.get b !i in
      if not (is_bchar_nospace c || Char.equal c ' ') then ok := false;
      Stdlib.incr i
    done;
    !ok)
;;

let[@inline] lower (c : char) = Char.lowercase c

(* [lit] must already be lowercase. *)
let equal_ci_window (s : string @ local) ~off ~len lit =
  if len <> String.length lit
  then false
  else (
    let same = ref true in
    let i = ref 0 in
    while !same && !i < len do
      if not (Char.equal (lower (String.get s (off + !i))) (String.get lit !i))
      then same := false;
      Stdlib.incr i
    done;
    !same)
;;

let equal_window (s : string @ local) ~off lit =
  let m = String.length lit in
  if off + m > String.length s
  then false
  else (
    let same = ref true in
    let i = ref 0 in
    while !same && !i < m do
      if not (Char.equal (String.get s (off + !i)) (String.get lit !i))
      then same := false;
      Stdlib.incr i
    done;
    !same)
;;

(* Plain scan. The delimiter shares no prefix with itself beyond the leading
   CRLF, so the quadratic worst case needs a body built to provoke it. *)
let find_sub (body : string @ local) ~sub ~from =
  let n = String.length body in
  let m = String.length sub in
  if m = 0
  then -1
  else (
    let c0 = String.get sub 0 in
    let last = n - m in
    let i = ref (if from < 0 then 0 else from) in
    let found = ref (-1) in
    while !found < 0 && !i <= last do
      if Char.equal (String.get body !i) c0
      then (
        let j = ref 1 in
        while !j < m && Char.equal (String.get body (!i + !j)) (String.get sub !j) do
          Stdlib.incr j
        done;
        if !j = m then found := !i else Stdlib.incr i)
      else Stdlib.incr i
    done;
    !found)
;;

let rec assoc_first ps key =
  match ps with
  | [] -> None
  | (k, v) :: rest -> if String.equal k key then Some v else assoc_first rest key
;;

let[@inline] valid_qdtext c = Buf_read.is_qdtext_char (Char_u.of_char c)
let[@inline] valid_quoted_pair c = Buf_read.is_quoted_pair_char (Char_u.of_char c)

(* Percent-decoding for an RFC 8187 value. A malformed escape is kept as it
   stands rather than failing the whole part. *)
let percent_decode (s : string @ local) ~off ~len =
  if len <= 0
  then ""
  else if not (Httpz_uri.Scanner.needs_percent_decode s ~pos:off ~len ~plus_as_space:false)
  then sub s ~pos:off ~len
  else (
    let dst = Bytes.create len in
    let written =
      Httpz_uri.Scanner.percent_decode_into s ~pos:off ~len ~dst ~dst_pos:0 ~plus_as_space:false
    in
    if written >= 0
    then Bytes.To_string.sub dst ~pos:0 ~len:written
    else sub s ~pos:off ~len)
;;

(* RFC 8187 ext-value: charset "'" [language] "'" value-chars. Only UTF-8 is
   accepted; ISO-8859-1 was removed by RFC 8187 and anything else is unknown. *)
let ext_value v =
  let n = String.length v in
  let q1 = ref 0 in
  while !q1 < n && not (Char.equal (String.get v !q1) '\'') do
    Stdlib.incr q1
  done;
  if !q1 >= n
  then None
  else (
    let q2 = ref (!q1 + 1) in
    while !q2 < n && not (Char.equal (String.get v !q2) '\'') do
      Stdlib.incr q2
    done;
    if !q2 >= n || not (equal_ci_window v ~off:0 ~len:!q1 "utf-8")
    then None
    else Some (percent_decode v ~off:(!q2 + 1) ~len:(n - !q2 - 1)))
;;

(* Parameters of a field value: ";" name "=" (token / quoted-string), with
   quoted pairs unescaped. Names are lowercased. The complete suffix must fit;
   accepting a valid prefix would turn [boundary=a:b] into [boundary=a]. *)
let parse_params (s : string @ local) ~pos ~stop =
  let acc = ref [] in
  let i = ref pos in
  let valid = ref true in
  let going = ref true in
  while !going && !valid do
    while !i < stop && is_ows (String.get s !i) do
      Stdlib.incr i
    done;
    if !i >= stop then going := false
    else if not (Char.equal (String.get s !i) ';')
    then valid := false
    else (
      Stdlib.incr i;
      while !i < stop && is_ows (String.get s !i) do
        Stdlib.incr i
      done;
      let n0 = !i in
      while !i < stop && is_tchar (String.get s !i) do
        Stdlib.incr i
      done;
      let n1 = !i in
      if n1 = n0 || !i >= stop || not (Char.equal (String.get s !i) '=')
      then valid := false
      else (
        Stdlib.incr i;
        let value =
          if !i < stop && Char.equal (String.get s !i) '"'
          then (
            Stdlib.incr i;
            let b = Buffer.create 32 in
            let closed = ref false in
            while !valid && (not !closed) && !i < stop do
              let c = String.get s !i in
              if Char.equal c '\\' && !i + 1 < stop
              then (
                let escaped = String.get s (!i + 1) in
                if valid_quoted_pair escaped
                then (
                  Buffer.add_char b escaped;
                  i := !i + 2)
                else valid := false)
              else if Char.equal c '"'
              then (
                Stdlib.incr i;
                closed := true)
              else if valid_qdtext c
              then (
                Buffer.add_char b c;
                Stdlib.incr i)
              else valid := false
            done;
            if !valid && !closed then Some (Buffer.contents b) else None)
          else (
            let v0 = !i in
            while !i < stop && is_tchar (String.get s !i) do
              Stdlib.incr i
            done;
            if !i = v0 then None else Some (sub s ~pos:v0 ~len:(!i - v0)))
        in
        match value with
        | None -> valid := false
        | Some v ->
          let name = String.lowercase (sub s ~pos:n0 ~len:(n1 - n0)) in
          if List.exists !acc ~f:(fun (existing, _) -> String.equal existing name)
          then valid := false
          else acc := (name, v) :: !acc))
  done;
  if !valid then Some (List.rev !acc) else None
;;

(* The media type of a field value ends at the first parameter. *)
let type_bounds (s : string @ local) ~stop =
  let semi = ref 0 in
  while !semi < stop && not (Char.equal (String.get s !semi) ';') do
    Stdlib.incr semi
  done;
  let first = ref 0 in
  while !first < !semi && is_ows (String.get s !first) do
    Stdlib.incr first
  done;
  let last = ref !semi in
  while !last > !first && is_ows (String.get s (!last - 1)) do
    Stdlib.decr last
  done;
  !first, !last, !semi
;;

(* Check the form needed by response validation without materialising parameter
   names or values. A repeated boundary is ambiguous; other parameters only
   need to be syntactically valid. *)
let[@zero_alloc] has_boundary ?(media_type = "multipart/form-data")
    (ct : string @ local) =
  let n = String.length ct in
  let mutable semi = 0 in
  while semi < n && not (Char.equal (String.get ct semi) ';') do
    semi <- semi + 1
  done;
  let mutable first = 0 in
  while first < semi && is_ows (String.get ct first) do
    first <- first + 1
  done;
  let mutable last = semi in
  while last > first && is_ows (String.get ct (last - 1)) do
    last <- last - 1
  done;
  let mutable valid = equal_ci_window ct ~off:first ~len:(last - first) media_type in
  let mutable seen = false in
  let mutable boundary_ok = false in
  let mutable i = semi in
  while valid && i < n do
    while i < n && is_ows (String.get ct i) do i <- i + 1 done;
    if i >= n
    then ()
    else if not (Char.equal (String.get ct i) ';')
    then valid <- false
    else (
      i <- i + 1;
      while i < n && is_ows (String.get ct i) do i <- i + 1 done;
      let name_start = i in
      while i < n && is_tchar (String.get ct i) do i <- i + 1 done;
      let name_stop = i in
      if name_stop = name_start || i >= n || not (Char.equal (String.get ct i) '=')
      then valid <- false
      else (
        let is_boundary =
          equal_ci_window ct ~off:name_start ~len:(name_stop - name_start) "boundary"
        in
        if is_boundary && seen
        then valid <- false
        else (
          i <- i + 1;
          let mutable value_len = 0 in
          let mutable value_ok = true in
          let mutable last_space = false in
          if i < n && Char.equal (String.get ct i) '"'
          then (
            i <- i + 1;
            let mutable closed = false in
            while valid && not closed && i < n do
              let c = String.get ct i in
              if Char.equal c '\\'
              then
                if i + 1 < n && valid_quoted_pair (String.get ct (i + 1))
                then (
                  let escaped = String.get ct (i + 1) in
                  value_len <- value_len + 1;
                  value_ok <- value_ok && (is_bchar_nospace escaped || Char.equal escaped ' ');
                  last_space <- Char.equal escaped ' ';
                  i <- i + 2)
                else valid <- false
              else if Char.equal c '"'
              then (
                closed <- true;
                i <- i + 1)
              else if valid_qdtext c
              then (
                value_len <- value_len + 1;
                value_ok <- value_ok && (is_bchar_nospace c || Char.equal c ' ');
                last_space <- Char.equal c ' ';
                i <- i + 1)
              else valid <- false
            done;
            if not closed then valid <- false)
          else (
            let value_start = i in
            while i < n && is_tchar (String.get ct i) do
              let c = String.get ct i in
              value_len <- value_len + 1;
              value_ok <- value_ok && (is_bchar_nospace c || Char.equal c ' ');
              last_space <- Char.equal c ' ';
              i <- i + 1
            done;
            if i = value_start then valid <- false);
          if is_boundary
          then (
            seen <- true;
            boundary_ok <- value_len >= 1 && value_len <= 70 && value_ok && not last_space))))
  done;
  valid && seen && boundary_ok
;;

let boundary_of_content_type ?(media_type = "multipart/form-data")
    (ct : string @ local) =
  let n = String.length ct in
  let first, last, semi = type_bounds ct ~stop:n in
  if not (equal_ci_window ct ~off:first ~len:(last - first) media_type)
  then None
  else (
    match Option.bind (parse_params ct ~pos:semi ~stop:n)
            ~f:(fun ps -> assoc_first ps "boundary") with
    | None -> None
    | Some b -> if valid_boundary b then Some b else None)
;;

(* What follows a boundary token: the close marker, the CRLF that opens a part,
   or neither, in which case the token was ordinary content. *)
type follow =
  | Fol_close
  | Fol_part of int
  | Fol_bare_lf
  | Fol_none

let follow (body : string @ local) pos =
  let n = String.length body in
  if pos + 1 < n
     && Char.equal (String.get body pos) '-'
     && Char.equal (String.get body (pos + 1)) '-'
  then (
    let i = ref (pos + 2) in
    while !i < n && is_ows (String.get body !i) do Stdlib.incr i done;
    if !i + 1 < n && Char.equal (String.get body !i) '\r'
       && Char.equal (String.get body (!i + 1)) '\n'
    then Fol_close
    else if !i < n && Char.equal (String.get body !i) '\n' then Fol_bare_lf
    else Fol_none)
  else (
    let i = ref pos in
    while !i < n && is_ows (String.get body !i) do
      Stdlib.incr i
    done;
    if !i + 1 < n
       && Char.equal (String.get body !i) '\r'
       && Char.equal (String.get body (!i + 1)) '\n'
    then Fol_part (!i + 2)
    else if !i < n && Char.equal (String.get body !i) '\n'
    then Fol_bare_lf
    else Fol_none)
;;

(* [--boundaryX] is a different boundary, not this one followed by content, so
   an occurrence whose tail is not a delimiter is skipped. *)
let rec find_valid_delim (body : string @ local) delim from =
  let idx = find_sub body ~sub:delim ~from in
  if idx < 0
  then -1
  else (
    match follow body (idx + String.length delim) with
    | Fol_none -> find_valid_delim body delim (idx + 1)
    | Fol_close | Fol_part _ | Fol_bare_lf -> idx)
;;

(* Header lines of one part, ending at the empty line. Returns the fields and
   the offset of the first content byte. *)
let parse_headers (body : string @ local) pos =
  let n = String.length body in
  let acc = ref [] in
  let count = ref 0 in
  let cursor = ref pos in
  let result = ref None in
  while Option.is_none !result do
    let p = !cursor in
    if p >= n
    then result := Some (Error "truncated part header")
    else if Char.equal (String.get body p) '\n'
    then result := Some (Error "bare LF in a part header")
    else if Char.equal (String.get body p) '\r'
    then
      if p + 1 < n && Char.equal (String.get body (p + 1)) '\n'
      then result := Some (Ok (List.rev !acc, p + 2))
      else result := Some (Error "bare CR in a part header")
    else if is_ows (String.get body p)
    then result := Some (Error "obsolete line folding in a part header")
    else (
      let name_stop = ref p in
      while !name_stop < n && is_tchar (String.get body !name_stop) do
        Stdlib.incr name_stop
      done;
      if !name_stop = p
         || !name_stop >= n
         || not (Char.equal (String.get body !name_stop) ':')
      then result := Some (Error "malformed part header")
      else (
        let value_start = !name_stop + 1 in
        let scan = ref value_start in
        let line_end = ref (-1) in
        while !line_end < 0 && Option.is_none !result do
          if !scan >= n
          then result := Some (Error "truncated part header")
          else (
            let c = String.get body !scan in
            if Char.equal c '\r'
            then
              if !scan + 1 < n && Char.equal (String.get body (!scan + 1)) '\n'
              then line_end := !scan
              else result := Some (Error "bare CR in a part header")
            else if Char.equal c '\n'
            then result := Some (Error "bare LF in a part header")
            else if Char.equal c '\000'
            then result := Some (Error "NUL in a part header")
            else if not (Buf_read.is_field_value_char (Char_u.of_char c))
            then result := Some (Error "control byte in a part header")
            else Stdlib.incr scan)
        done;
        if !line_end >= 0
        then
          if !line_end + 2 - p > max_header_line
          then result := Some (Error "part header line is too long")
          else (
            Stdlib.incr count;
            if !count > max_part_headers
            then result := Some (Error "too many part headers")
            else (
              let first = ref value_start in
              while !first < !line_end && is_ows (String.get body !first) do
                Stdlib.incr first
              done;
              let last = ref !line_end in
              while !last > !first && is_ows (String.get body (!last - 1)) do
                Stdlib.decr last
              done;
              let name =
                String.lowercase (sub body ~pos:p ~len:(!name_stop - p))
              in
              let value = sub body ~pos:!first ~len:(!last - !first) in
              acc := (name, value) :: !acc;
              cursor := !line_end + 2))))
  done;
  match !result with
  | Some r -> r
  | None -> Error "truncated part header"
;;

let part_of_headers headers ~off ~len =
  match assoc_first headers "content-disposition" with
  | None -> Error "a part has no Content-Disposition"
  | Some cd ->
    let stop = String.length cd in
    let first, last, semi = type_bounds cd ~stop in
    if not (equal_ci_window cd ~off:first ~len:(last - first) "form-data")
    then Error "a part is not Content-Disposition: form-data"
    else (
      match parse_params cd ~pos:semi ~stop with
      | None -> Error "malformed Content-Disposition parameters"
      | Some params -> (match assoc_first params "name" with
      | None -> Error "a part has no name parameter"
      | Some name ->
        let filename =
          match assoc_first params "filename*" with
          | Some v ->
            (match ext_value v with
             | Some d -> Some d
             | None -> assoc_first params "filename")
          | None -> assoc_first params "filename"
        in
        let content_type = assoc_first headers "content-type" in
        Ok { name; filename; content_type; headers; off; len }))
;;

let parse ?(max_parts = default_max_parts) ~boundary (body : string @ local) =
  if max_parts < 0 then
    invalid_arg "Httpz.Multipart.parse: max_parts is negative"
  else if not (valid_boundary boundary)
  then Error "invalid boundary"
  else (
    let dash = "--" ^ boundary in
    let delim = "\r\n" ^ dash in
    let dlen = String.length delim in
    let result = ref None in
    let parts = ref [] in
    let count = ref 0 in
    let cursor = ref 0 in
    (* The first delimiter may drop its leading CRLF when no preamble runs
       ahead of it. A preamble that merely begins with the same bytes is not
       one, so the CRLF-prefixed search still runs behind it. *)
    let opening = ref (-1) in
    if equal_window body ~off:0 dash
    then (
      match follow body (String.length dash) with
      | Fol_none -> ()
      | Fol_close | Fol_part _ | Fol_bare_lf -> opening := String.length dash);
    if !opening >= 0
    then cursor := !opening
    else (
      let idx = find_valid_delim body delim 0 in
      if idx < 0
      then result := Some (Error "no multipart delimiter")
      else cursor := idx + dlen);
    while Option.is_none !result do
      match follow body !cursor with
      | Fol_close -> result := Some (Ok (List.rev !parts))
      | Fol_bare_lf -> result := Some (Error "bare LF in the multipart framing")
      | Fol_none -> result := Some (Error "malformed multipart delimiter")
      | Fol_part header_off ->
        (match parse_headers body header_off with
         | Error msg -> result := Some (Error msg)
         | Ok (headers, content_off) ->
           let idx = find_valid_delim body delim content_off in
           if idx < 0
           then result := Some (Error "missing closing delimiter")
           else (
             Stdlib.incr count;
             if !count > max_parts
             then result := Some (Error "too many parts")
             else (
               match
                 part_of_headers headers ~off:content_off ~len:(idx - content_off)
               with
               | Error msg -> result := Some (Error msg)
               | Ok p ->
                 parts := p :: !parts;
                 cursor := idx + dlen)))
    done;
    match !result with
    | Some r -> r
    | None -> Error "malformed multipart body")
;;
