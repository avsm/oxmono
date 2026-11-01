(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar

let printable s =
  String.is_valid_utf_8 s
  && not
       (String.exists
          (fun c -> (Char.code c < 0x20 && c <> '\n') || c = '\127')
          s)

(* [sanitize] and [strip] give printable UTF-8 and are idempotent; [strip]
   leaves no escape; [strip ~keep_styles] leaves nothing on after it: text
   printed after it is drawn as it would be on its own. *)
let test_laws s =
  let san = Console_text.sanitize s and str = Console_text.strip s in
  check (printable san && printable str);
  check (not (String.contains str '\027'));
  check_eq ~pp:Fmt.Dump.string san (Console_text.sanitize san);
  check_eq ~pp:Fmt.Dump.string str (Console_text.strip str);
  check
    (not (String.contains (Console_text.sanitize ~keep_newlines:false s) '\n'))

let test_styles_isolated s =
  let draw prefix =
    let vte = Console_vte.v ~rows:2 ~cols:8 () in
    Console_vte.feed vte ("\027[2J\027[H" ^ prefix ^ "\r\n!");
    let g = Console_vte.grid vte in
    let i = Grid.idx g ~x:0 ~y:1 in
    Ansi.Color.(to_rgba (Console_vte.fg g i), to_rgba (Console_vte.bg g i))
  in
  let styled = Console_text.strip ~keep_newlines:false ~keep_styles:true s in
  check (draw styled = draw "")

(* A line ends at LF, VT, FF, at NEL (U+0085, which UTF-8 writes C2 85),
   and at a CR that none of them follows once the characters [sanitize]
   removes are gone: [sanitize] keeps one line feed for each. After a CR, a
   variation selector has no base and is removed too. *)
let removed c =
  c = 0x061c || c = 0x200b || c = 0x200e || c = 0x200f
  || (c >= 0x202a && c <= 0x202e)
  || (c >= 0x2060 && c <= 0x2064)
  || (c >= 0x2066 && c <= 0x2069)
  || c = 0xfeff
  || (c >= 0xfe00 && c <= 0xfe0f)
  || (c >= 0xfff9 && c <= 0xfffb)
  || (c >= 0xe0000 && c <= 0xe007f)
  || (c >= 0xe0100 && c <= 0xe01ef)

let line_ends s =
  let n = String.length s in
  let nel i = i + 1 < n && s.[i] = '\xc2' && s.[i + 1] = '\x85' in
  let feed i = i < n && (String.contains "\n\011\012" s.[i] || nel i) in
  let rec next i =
    if i >= n then i
    else
      let d = String.get_utf_8_uchar s i in
      if
        Uchar.utf_decode_is_valid d
        && removed (Uchar.to_int (Uchar.utf_decode_uchar d))
      then next (i + Uchar.utf_decode_length d)
      else i
  in
  let count = ref 0 in
  String.iteri
    (fun i c ->
      if feed i || (c = '\r' && not (feed (next (i + 1)))) then incr count)
    s;
  !count

let test_line_ends s =
  let lines = String.fold_left (fun n c -> if c = '\n' then n + 1 else n) 0 in
  check_eq ~pp:Fmt.int (line_ends s) (lines (Console_text.sanitize s))

(* The controls a terminal acts on without printing, CAN and SUB among them:
   put between two characters of text that holds no escape, they leave it as
   it was. *)
let silent =
  List.filter
    (fun c -> not (List.mem c [ 0x09; 0x0a; 0x0b; 0x0c; 0x0d; 0x1b ]))
    (List.init 0x20 Fun.id)
  @ [ 0x7f ]

let test_silent_discarded s p k =
  let s = Console_text.strip s in
  let rec boundary i =
    if i >= String.length s then String.length s
    else if Uchar.utf_decode_is_valid (String.get_utf_8_uchar s i) then i
    else boundary (i + 1)
  in
  let i = boundary (if s = "" then 0 else p mod (String.length s + 1)) in
  let c =
    String.make 1 (Char.chr (List.nth silent (k mod List.length silent)))
  in
  let with_c = String.sub s 0 i ^ c ^ String.sub s i (String.length s - i) in
  check_eq ~pp:Fmt.Dump.string s (Console_text.strip with_c)

(* Zeros inside an extended colour are components or palette indices,
   not resets. Relaying a tool's colours keeps those colours on its text
   and restores the default style for the caller's next character. *)
let test_extended_colours r g k =
  let sequence =
    match k mod 4 with
    | 0 -> Fmt.str "\027[1;38;2;%d;%d;0m" r g
    | 1 -> Fmt.str "\027[48;2;%d;%d;0m" r g
    | 2 -> "\027[38;5;0m"
    | _ -> "\027[48;5;0m"
  in
  let source = sequence ^ "x" in
  let output = Console_text.strip ~keep_styles:true source in
  let expected = Console_vte.v ~rows:1 ~cols:4 ()
  and actual = Console_vte.v ~rows:1 ~cols:4 () in
  Console_vte.feed expected (source ^ "\027[0m!");
  Console_vte.feed actual (output ^ "!");
  for i = 0 to 1 do
    if
      not
        (Grid.cells_equal
           (Console_vte.grid expected)
           i (Console_vte.grid actual) i)
    then failf "extended colour changed cell %d or leaked after %S" i source
  done;
  check_eq ~pp:Fmt.Dump.string output
    (Console_text.strip ~keep_styles:true output)

(* The characters of [s] as [String.get_utf_8_uchar] decodes them: where
   each starts and ends, and whether it is valid. *)
let decodes s =
  let n = String.length s in
  let rec go i acc =
    if i >= n then List.rev acc
    else
      let d = String.get_utf_8_uchar s i in
      let next = i + Uchar.utf_decode_length d in
      go next ((i, next, d) :: acc)
  in
  go 0 []

(* [boundary s] falls where a decode of [s] starts, or at its end. Without
   an escape code it is the end of [s], or the start of its last decode if
   that one is invalid: a character cut short goes to the next piece. *)
let test_boundary s =
  let n = String.length s and k = Console_text.boundary s in
  let ds = decodes s in
  check (k = n || List.exists (fun (i, _, _) -> i = k) ds);
  let escape (_, _, d) =
    Uchar.utf_decode_is_valid d
    &&
    let c = Uchar.to_int (Uchar.utf_decode_uchar d) in
    c = 0x1b || (c >= 0x80 && c <= 0x9f)
  in
  if not (List.exists escape ds) then
    let expected =
      match List.rev ds with
      | (i, _, d) :: _ when not (Uchar.utf_decode_is_valid d) -> i
      | _ -> n
    in
    check_eq ~pp:Fmt.int expected k

(* Characters whose keeping depends on their neighbours: tags, the black
   flag, the cancel tag, selectors and what may come before one. *)
let pieces =
  [|
    "a"; " "; "\r"; "\n"; "\027[1m"; "\u{1F3F4}"; "\u{E0067}"; "\u{E0062}";
    "\u{E007F}"; "\u{E0001}"; "\u{FE0F}"; "\u{E0100}"; "\u{200D}";
    "\u{200F}"; "\u{200B}"; "\u{845B}";
  |]
[@@ocamlformat "disable"]

let neighbours =
  map
    [ list uint8 ]
    (fun l ->
      String.concat ""
        (List.map (fun k -> pieces.(k mod Array.length pieces)) l))

(* No two selectors are adjacent in what [sanitize] keeps, a tag is kept
   only inside a whole emoji tag sequence, and the laws hold. *)
let test_neighbours s =
  test_laws s;
  test_line_ends s;
  let out = Console_text.sanitize s in
  let cps =
    List.map
      (fun (_, _, d) -> Uchar.to_int (Uchar.utf_decode_uchar d))
      (decodes out)
  in
  let selector c = (c >= 0xfe00 && c <= 0xfe0f) || (c >= 0xe0100 && c <= 0xe01ef)
  and tag c = c >= 0xe0020 && c <= 0xe007e in
  let rec walk = function
    | a :: b :: _ when selector a && selector b ->
        failf "two selectors kept in %S" out
    | 0x1f3f4 :: rest when match rest with c :: _ -> tag c | [] -> false ->
        let rec tags = function
          | c :: rest when tag c -> tags rest
          | 0xe007f :: rest -> walk rest
          | _ -> failf "a tag sequence cut short in %S" out
        in
        tags rest
    | c :: _ when tag c || c = 0xe007f -> failf "a stray tag kept in %S" out
    | _ :: rest -> walk rest
    | [] -> ()
  in
  walk cps

let suite =
  ( "console_text",
    [
      test_case "selectors and tags by their neighbours" [ neighbours ]
        test_neighbours;
      test_case "a boundary falls between two decoded characters" [ bytes ]
        test_boundary;
      test_case "extended colours survive and leave the next cell plain"
        [ uint8; uint8; uint8 ] test_extended_colours;
      test_case "sanitize and strip" [ bytes ] test_laws;
      test_case "one line feed per line end" [ bytes ] test_line_ends;
      test_case "silent controls print nothing" [ bytes; uint8; uint8 ]
        test_silent_discarded;
      test_case "strip ~keep_styles leaves nothing on" [ bytes ]
        test_styles_isolated;
    ] )
