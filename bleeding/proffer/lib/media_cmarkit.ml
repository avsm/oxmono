module Media = Httpz.Media

let utf8 = [ ("charset", "utf-8") ]

(* Cmarkit predates modes. The three narrow assertions below wrap pure,
   reentrant functions and capture only scalar options. Audit them when
   updating Cmarkit. *)

(* Optional lexical input restriction. This is not a parser-work bound:
   code spans and Markdown link nesting do not follow a literal bracket scan.
   Untrusted decoding requires the upstream Cmarkit parser correction. *)
let deeper_than max s =
  let n = String.length s in
  let rec scan i depth =
    if i >= n then false
    else
      match String.unsafe_get s i with
      | '\\' -> scan (i + 2) depth
      | '[' -> if depth >= max then true else scan (i + 1) (depth + 1)
      | ']' -> scan (i + 1) (if depth > 0 then depth - 1 else 0)
      | _ -> scan (i + 1) depth
  in
  scan 0 0

let markdown ?(strict = false) ?(max_bracket_depth = 16) () =
  if max_bracket_depth < 1 then
    invalid_arg "Markdown.markdown: max_bracket_depth must be positive";
  Media.of_strings ~params:utf8 ~accept:[ "text/x-markdown" ] "text/markdown"
    ~encode:Cmarkit_commonmark.of_doc
    ~decode:(fun s ->
      if deeper_than max_bracket_depth s then
          Error
            (Printf.sprintf "bracket nesting deeper than %d" max_bracket_depth)
      else Ok (Cmarkit.Doc.of_string ~strict s))

let hex_value = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' as c -> Char.code c - Char.code 'A' + 10
  | _ -> -1

(* Cmarkit's safety check runs before its HTML renderer percent-encodes
   control bytes. Check the generated href/src value again after reversing
   percent triplets and removing the ASCII whitespace/control bytes that URL
   consumers can ignore around a scheme. Raw HTML is absent in safe mode, so
   these exact double-quoted attributes are all renderer-owned. *)
let unsafe_rendered_url value =
  let len = String.length value in
  let normalized = Buffer.create len in
  let rec loop i =
    if i < len then begin
      let byte, next =
        if i + 2 < len && value.[i] = '%' then
          let hi = hex_value value.[i + 1] and lo = hex_value value.[i + 2] in
          if hi >= 0 && lo >= 0 then ((hi lsl 4) lor lo, i + 3)
          else (Char.code value.[i], i + 1)
        else (Char.code value.[i], i + 1)
      in
      if byte > 0x20 && byte <> 0x7f then
        Buffer.add_char normalized (Char.chr byte);
      loop next
    end
  in
  loop 0;
  Cmarkit.Inline.Link.is_unsafe (Buffer.contents normalized)

let rec equal_at text literal offset index =
  index = String.length literal
  || (text.[offset + index] = literal.[index]
      && equal_at text literal offset (index + 1))

let sanitize_url_attributes html =
  let len = String.length html in
  let output = Buffer.create len in
  let starts_at i literal =
    i <= len - String.length literal && equal_at html literal i 0
  in
  let rec scan copied i =
    if i >= len then Buffer.add_substring output html copied (len - copied)
    else
      let prefix_len =
        if starts_at i " href=\"" then 7
        else if starts_at i " src=\"" then 6
        else 0
      in
      if prefix_len = 0 then scan copied (i + 1)
      else
        let value_start = i + prefix_len in
        match String.index_from_opt html value_start '"' with
        | None -> Buffer.add_substring output html copied (len - copied)
        | Some value_end ->
            Buffer.add_substring output html copied (value_start - copied);
            let value = String.sub html value_start (value_end - value_start) in
            if not (unsafe_rendered_url value) then Buffer.add_string output value;
            Buffer.add_char output '"';
            scan (value_end + 1) (value_end + 1)
  in
  scan 0 0;
  Buffer.contents output

let html ?(safe = true) () =
  Media.encoder ~params:utf8 "text/html"
    (fun doc ->
       let rendered = Cmarkit_html.of_doc ~safe doc in
       if safe then sanitize_url_attributes rendered else rendered)
