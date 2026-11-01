(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* ------------------------------------------------------------------ *)
(* Reducing HTML to its text                                           *)
(* ------------------------------------------------------------------ *)

let is_alnum c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

(* Whether [s] holds [sub] at [i], letters compared without case. HTML element
   names are case insensitive, and a page written with [<SCRIPT>] hides its
   script from a comparison that is not. *)
let at s i sub =
  let n = String.length sub in
  i + n <= String.length s
  && String.lowercase_ascii (String.sub s i n) = String.lowercase_ascii sub

(* The index just past the first [sub] at or after [i], or the end of [s]. An
   element that is never closed takes the rest of the document, which is what a
   browser does with it too. *)
let past s i sub =
  let n = String.length s in
  let rec go i =
    if i >= n then n
    else if at s i sub then i + String.length sub
    else go (i + 1)
  in
  go i

(* The lower-cased name of the element a [<] at [i] opens, if it opens one. *)
let element s i =
  let n = String.length s in
  let j = i + 1 in
  let rec take k = if k < n && is_alnum s.[k] then take (k + 1) else k in
  let k = take j in
  if k = j then None else Some (String.lowercase_ascii (String.sub s j (k - j)))

(* Every tag becomes a space rather than nothing, so that the words either side
   of one do not run together. *)
let tagless s =
  let n = String.length s in
  let b = Buffer.create n in
  let rec go i =
    if i >= n then ()
    else if s.[i] <> '<' then begin
      Buffer.add_char b s.[i];
      go (i + 1)
    end
    else if at s i "<!--" then go (past s (i + 4) "-->")
    else
      match element s i with
      | Some (("script" | "style") as name) ->
          Buffer.add_char b ' ';
          go (past s (past s i ("</" ^ name)) ">")
      | Some _ | None ->
          Buffer.add_char b ' ';
          go (past s (i + 1) ">")
  in
  go 0;
  Buffer.contents b

(* The entities a page of prose actually uses, and the numeric forms. A name
   this does not know is left as it was written, since a reader can see what it
   said where a dropped one leaves a hole. *)
let named = function
  | "amp" -> Some "&"
  | "lt" -> Some "<"
  | "gt" -> Some ">"
  | "quot" -> Some "\""
  | "apos" -> Some "'"
  (* A non-breaking space becomes an ordinary one, so that the collapse below
     treats it as the space it is drawn as. *)
  | "nbsp" -> Some " "
  | "mdash" -> Some "\xe2\x80\x94"
  | "ndash" -> Some "\xe2\x80\x93"
  | "hellip" -> Some "\xe2\x80\xa6"
  | _ -> None

let numeric body =
  let code =
    if String.length body > 1 && (body.[0] = 'x' || body.[0] = 'X') then
      int_of_string_opt ("0x" ^ String.sub body 1 (String.length body - 1))
    else int_of_string_opt body
  in
  match code with
  | Some c when Uchar.is_valid c ->
      let b = Buffer.create 4 in
      Buffer.add_utf_8_uchar b (Uchar.of_int c);
      Some (Buffer.contents b)
  | Some _ | None -> None

let entities s =
  let n = String.length s in
  let b = Buffer.create n in
  let rec go i =
    if i >= n then ()
    else if s.[i] <> '&' then begin
      Buffer.add_char b s.[i];
      go (i + 1)
    end
    else
      (* A reference is short. Anything longer than this is an ampersand in the
         prose followed by words, not an entity missing its semicolon. *)
      let stop = min n (i + 12) in
      let rec semi k =
        if k >= stop then None else if s.[k] = ';' then Some k else semi (k + 1)
      in
      match semi (i + 1) with
      | None ->
          Buffer.add_char b '&';
          go (i + 1)
      | Some k -> (
          let body = String.sub s (i + 1) (k - i - 1) in
          let decoded =
            if String.length body > 0 && body.[0] = '#' then
              numeric (String.sub body 1 (String.length body - 1))
            else named body
          in
          match decoded with
          | Some text ->
              Buffer.add_string b text;
              go (k + 1)
          | None ->
              Buffer.add_char b '&';
              go (i + 1))
  in
  go 0;
  Buffer.contents b

let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r' || c = '\012'

(* A run of whitespace becomes one newline where it held one and one space
   otherwise, so a document keeps roughly the lines its source had and loses the
   indentation that markup is laid out with. Nothing here knows which elements
   are blocks, which is one of the ways this is a reduction. *)
let collapse s =
  let n = String.length s in
  let b = Buffer.create n in
  let rec go i =
    if i >= n then ()
    else if not (is_space s.[i]) then begin
      Buffer.add_char b s.[i];
      go (i + 1)
    end
    else
      let rec run k newline =
        if k < n && is_space s.[k] then run (k + 1) (newline || s.[k] = '\n')
        else (k, newline)
      in
      let k, newline = run i false in
      if Buffer.length b > 0 && k < n then
        Buffer.add_char b (if newline then '\n' else ' ');
      go k
  in
  go 0;
  Buffer.contents b

let text_of_html s = collapse (entities (tagless s))

(* ------------------------------------------------------------------ *)
(* The answers                                                         *)
(* ------------------------------------------------------------------ *)

let content_type = function "" -> "no content type" | t -> t

(* Stated above the body rather than below it. A model that reads the document
   first has already believed it. *)
let not_2xx status =
  if status >= 200 && status < 300 then ""
  else
    "This status is not a 2xx, so what follows is whatever the server sent \
     instead of the document, such as an error page. Read it as that.\n"

let fetch ~status ~url ~content_type:ct ~bytes ~body ~reduced =
  Printf.sprintf "%d %s\n%s, %d bytes%s\n%s\n%s" status url (content_type ct)
    bytes
    (if reduced then
       Printf.sprintf ", reduced to %d bytes of text" (String.length body)
     else "")
    (not_2xx status) body

let head ~status ~url ~content_type:ct ~length =
  Printf.sprintf "%d %s\n%s, %s\n%s" status url (content_type ct)
    (match length with
    | Some n -> Printf.sprintf "%d bytes declared" n
    | None -> "no size declared")
    (not_2xx status)

let refused ~url ~size ~bound =
  Printf.sprintf
    "%s\n\
     Nothing of the body is returned. A document cut off at a bound reads as \
     the whole of it, which is the mistake this refusal exists to prevent. Ask \
     for head to see what it is, raise max_bytes if this machine can hold it, \
     or fetch a narrower page."
    (match size with
    | Some n ->
        Printf.sprintf
          "Refused: %s is %d bytes, over the %d byte bound this fetch was \
           given."
          url n bound
    | None ->
        Printf.sprintf
          "Refused: %s passed the %d byte bound this fetch was given, and the \
           server did not say how large it is."
          url bound)

let ran ~program ~status ~output ~total =
  let outcome =
    match status with
    | `Exited 0 -> Printf.sprintf "%s exited 0" program
    | `Exited n -> Printf.sprintf "%s exited %d" program n
    | `Signaled n -> Printf.sprintf "%s was killed by signal %d" program n
  in
  let dropped = total - String.length output in
  Printf.sprintf "%s%s\n%s" outcome
    (if dropped > 0 then
       Printf.sprintf
         ", and wrote %d bytes, of which the first %d are below and %d were \
          dropped"
         total (String.length output) dropped
     else "")
    output

let failed ~what ~url ~code ~said =
  Printf.sprintf "%s %s failed: curl exited %d.\n%s" what url code
    (match String.trim said with
    | "" -> "curl said nothing about why."
    | s -> s)

let no_program ~program ~reason =
  Printf.sprintf
    "%S could not be run: %s\n\
     A program is looked for on numptyd's PATH and run directly, with no \
     shell, so there is no quoting, no globbing and no pipeline. Name the \
     program alone and pass each argument as its own element of args."
    program reason

let no_curl =
  "numptyd found no curl to run, so this tool has nothing to reach the network \
   with. Put curl on the PATH and start numpty again."
