(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A camel that says things.

   The camel faces right, towards its own words, which run in a column beside
   it. A model streams its reply a few characters at a time, so {!speaker}
   gathers those into lines and prints each one as it completes, against the
   next line of the camel. Once the camel runs out the text carries on at the
   same indent, and a reply too short to reach its feet is padded so that the
   whole animal is drawn. *)

let art =
  [
    {|     \__|};
    {|     \->|};
    {|   aa \\|};
    {|/camelll|};
    {|  ll  ll|};
    {|  ^^  ^^|};
  ]

let art_width = List.fold_left (fun w l -> max w (String.length l)) 0 art
let default_width = 64
let gap = "  "

(* OCaml's orange, as a 256-colour escape. The camel is coloured and its words
   are not, so the text stays whatever the terminal renders normally. *)
let orange = "\027[38;5;208m"
let reset = "\027[0m"

(* Trailing spaces come from padding a short camel line, and serve no purpose
   once the text beside it has been placed. *)
let rtrim s =
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = ' ' do
    decr n
  done;
  String.sub s 0 !n

type speaker = { pending : Buffer.t; mutable emitted : int; color : bool }

let speaker ?(color = true) () =
  { pending = Buffer.create 256; emitted = 0; color }

(* The camel occupies the left column for as long as it lasts. *)
let gutter t =
  if t.emitted < List.length art then
    let picture = Printf.sprintf "%-*s" art_width (List.nth art t.emitted) in
    if t.color then orange ^ picture ^ reset ^ gap else picture ^ gap
  else String.make art_width ' ' ^ gap

let emit t buf line =
  (* Trim the text, not the whole line: a coloured gutter ends in an escape
     sequence that must survive. *)
  let line = rtrim line in
  Buffer.add_string buf
    (if line = "" then rtrim (gutter t) else gutter t ^ line);
  Buffer.add_char buf '\n';
  t.emitted <- t.emitted + 1

let take t n ~drop =
  let s = Buffer.contents t.pending in
  let line = String.sub s 0 n in
  let rest = String.sub s (n + drop) (String.length s - n - drop) in
  Buffer.clear t.pending;
  Buffer.add_string t.pending rest;
  line

(* Emit every line that is already complete, leaving any partial one pending. *)
let rec drain t ~width buf =
  let s = Buffer.contents t.pending in
  match String.index_opt s '\n' with
  | Some i when i <= width ->
      emit t buf (take t i ~drop:1);
      drain t ~width buf
  | _ when String.length s > width ->
      (* Break at the last space that fits. A word longer than the whole width
         has nowhere to break, so it is cut. *)
      let rec back i =
        if i <= 0 then None else if s.[i] = ' ' then Some i else back (i - 1)
      in
      (match back width with
      | Some i -> emit t buf (take t i ~drop:1)
      | None -> emit t buf (take t width ~drop:0));
      drain t ~width buf
  | _ -> ()

let speak ?(width = default_width) t chunk =
  if chunk = "" then ""
  else begin
    Buffer.add_string t.pending chunk;
    let buf = Buffer.create (String.length chunk + 64) in
    drain t ~width buf;
    Buffer.contents buf
  end

let finish t =
  let buf = Buffer.create 128 in
  let rest = String.trim (Buffer.contents t.pending) in
  Buffer.clear t.pending;
  if rest <> "" then emit t buf rest;
  (* Draw whatever of the camel the reply was too short to reach. *)
  if t.emitted > 0 then
    while t.emitted < List.length art do
      emit t buf ""
    done;
  t.emitted <- 0;
  Buffer.contents buf

let say ?(width = default_width) ?color text =
  let t = speaker ?color () in
  let body = speak ~width t text in
  body ^ finish t
