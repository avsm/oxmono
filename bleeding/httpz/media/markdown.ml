let utf8 = [ ("charset", "utf-8") ]

(* Optional lexical input restriction. This is not a parser-work bound:
   code spans and Markdown link nesting do not follow a literal bracket scan.
   Untrusted decoding requires the upstream Cmarkit parser correction. *)
let deeper_than max s =
  let n = String.length s in
  let rec scan i depth =
    if i >= n then false
    else
      (* Safe: the [i >= n] guard above covers both [i = n] from the
         ordinary increment and the [i = n + 1] the backslash branch can
         produce, so [i] is always in bounds here. *)
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
   these exact double-quoted attributes are all renderer-owned. The scan
   below depends on cmarkit escaping a double quote as [&quot;] in every
   safe-mode text and attribute position, and percent-encoding it to [%22]
   in URLs: were that ever not so, a stray quote in text could desynchronise
   the scan and let a real href pass unchecked. *)
let unsafe_rendered_url value first last =
  let rec next i =
    if i >= last then #(-1, i)
    else
      let #(byte, following) =
        if i + 2 < last && value.[i] = '%' then
          let hi = hex_value value.[i + 1] and lo = hex_value value.[i + 2] in
          if hi >= 0 && lo >= 0 then #((hi lsl 4) lor lo, i + 3)
          else #(Char.code value.[i], i + 1)
        else #(Char.code value.[i], i + 1)
      in
      if byte <= 0x20 || byte = 0x7f then next following else #(byte, following)
  in
  let starts prefix =
    let rec compare i k =
      if k = String.length prefix then true else
      let #(byte, following) = next i in
      byte >= 0 && Char.lowercase_ascii (Char.chr byte) = prefix.[k]
      && compare following (k + 1)
    in
    compare first 0
  in
  starts "javascript:" || starts "vbscript:" || starts "file:"
  || (starts "data:" &&
      (* Cmarkit owns the allowed data media types. Only this uncommon path
         needs a normalized owned string for its global API. *)
      let normalized = Buffer.create (last - first) in
      let rec write i =
        let #(byte, following) = next i in
        if byte >= 0 then begin Buffer.add_char normalized (Char.chr byte); write following end
      in
      write first;
      Cmarkit.Inline.Link.is_unsafe (Buffer.contents normalized))

let rec equal_at text literal offset index =
  index = String.length literal
  || (text.[offset + index] = literal.[index]
      && equal_at text literal offset (index + 1))

let sanitize_url_attributes html =
  let len = String.length html in
  let output = ref None in
  let buffer () =
    match !output with
    | Some out -> out
    | None -> let out = Buffer.create len in output := Some out; out
  in
  let starts_at i literal =
    i <= len - String.length literal && equal_at html literal i 0
  in
  (* [scan copied i] is the offset from which the unscanned tail still has to
     be appended, or [-1] once the document has been truncated and no more of
     it may be emitted. *)
  let rec scan copied i =
    if i >= len then copied
    else
      let prefix_len = if starts_at i " href=\"" then 7 else if starts_at i " src=\"" then 6 else 0 in
      if prefix_len = 0 then scan copied (i + 1)
      else
        let first = i + prefix_len in
        match String.index_from_opt html first '"' with
        | None ->
            (* An href/src attribute with no closing quote should be
               unreachable from cmarkit's safe-mode renderer. Fail closed:
               keep everything copied so far and drop the unterminated value
               and the rest of the document, rather than emit an unchecked
               URL verbatim. *)
            Buffer.add_substring (buffer ()) html copied (first - copied);
            -1
        | Some last ->
            if unsafe_rendered_url html first last then begin
              Buffer.add_substring (buffer ()) html copied (first - copied);
              scan last (last + 1)
            end else scan copied (last + 1)
  in
  let copied = scan 0 0 in
  match !output with
  | None -> html
  | Some out ->
      if copied >= 0 then Buffer.add_substring out html copied (len - copied);
      Buffer.contents out

let html ?(safe = true) () =
  Media.encoder ~params:utf8 "text/html"
    (fun doc ->
       let rendered = Cmarkit_html.of_doc ~safe doc in
       if safe then sanitize_url_attributes rendered else rendered)
