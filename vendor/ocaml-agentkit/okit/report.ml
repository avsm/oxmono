(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Dune_rpc.Private

(* [to_text d] is [d] as the model sees it, ending in a newline. A location
   gives the File header dune prints, which is not part of the message. A
   severity that the message does not already open with gives a line of its
   own. *)
let to_text (d : Diagnostic.t) =
  let buf = Buffer.create 256 in
  (match d.loc with
  | None -> ()
  | Some l ->
      let s = Loc.start l and e = Loc.stop l in
      Buffer.add_string buf
        (Printf.sprintf "File %S, line %d, characters %d-%d:\n" s.pos_fname
           s.pos_lnum (s.pos_cnum - s.pos_bol) (e.pos_cnum - e.pos_bol)));
  let body = Format.asprintf "%a" Pp.to_fmt (Diagnostic.message d) in
  (match d.severity with
  | Some sev ->
      let word =
        match sev with Diagnostic.Error -> "Error" | Warning -> "Warning"
      in
      if not (String.starts_with ~prefix:word body) then begin
        Buffer.add_string buf word;
        Buffer.add_char buf '\n'
      end
  | None -> ());
  Buffer.add_string buf body;
  if not (String.ends_with ~suffix:"\n" body) then Buffer.add_char buf '\n';
  List.iter
    (fun (p : Diagnostic.Promotion.t) ->
      Buffer.add_string buf
        (Printf.sprintf "wrote %s (run promote to accept)\n" p.in_source))
    d.promotion;
  Buffer.contents buf

(* A build of a broken workspace reports hundreds of diagnostics, and a model
   that reads the first few and the number of the rest knows as much as one
   that reads them all and far more cheaply. *)
let max_diagnostics = 10

(* The file a write just saved is the one its author is looking at, so fewer
   of its diagnostics still say what went wrong. *)
let max_own_diagnostics = 5

(* How far an answer listing what a file or a module holds may run. The count
   is what a reader can take in, and the byte bound is the agent's: a result
   over its limit on a tool result has its middle removed, and a list cut that
   way says nothing about what went. Stopping short of that bound and saying
   how much was left leaves the rest reachable. *)
let max_outline = 200
let max_answer_bytes = Ds4.Toolbox.page_bytes

let render buf ~limit ds =
  List.iteri (fun i d -> if i < limit then Buffer.add_string buf (to_text d)) ds;
  let beyond = List.length ds - limit in
  if beyond > 0 then
    Buffer.add_string buf (Printf.sprintf "… and %d more.\n" beyond)

let targets s =
  let space c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
  let flat = String.map (fun c -> if space c then ' ' else c) s in
  match List.filter (fun t -> t <> "") (String.split_on_char ' ' flat) with
  | [] -> [ "." ]
  | l -> l

let build ~what (r : Session.build) =
  let buf = Buffer.create 512 in
  render buf ~limit:max_diagnostics r.diagnostics;
  Buffer.add_string buf (if r.ok then what ^ " ok" else what ^ " failed");
  Buffer.contents buf

(* A dune file is named rather than suffixed. *)
let built path =
  let base = Filename.basename path in
  base = "dune" || base = "dune-project"
  || List.exists
       (fun ext -> Filename.check_suffix path ext)
       [ ".ml"; ".mli"; ".mld" ]

(* A path written as "./lib/fix.ml", or with a separator doubled, names the
   file that "lib/fix.ml" names, and dune answers with the plain form. Reduce a
   path to it, since a written file whose own diagnostics went unmatched would
   have them counted among the rest of the workspace's and their text
   dropped. *)
let plain path =
  String.split_on_char '/' path
  |> List.filter (fun s -> s <> "" && s <> Filename.current_dir_name)
  |> String.concat "/"

(* Whether a diagnostic is about the file that was written. Dune reports an
   absolute path and the call named one relative to a capability, so what they
   share is the tail, cut at a directory boundary so that "fix.ml" does not
   claim "prefix.ml". *)
let about ~path (d : Diagnostic.t) =
  let path = plain path in
  match d.loc with
  | None -> false
  | Some l ->
      let file = (Loc.start l).pos_fname in
      file = path || String.ends_with ~suffix:("/" ^ path) file

let after_write ~path (r : Session.build) =
  let buf = Buffer.create 512 in
  let mine, elsewhere = List.partition (about ~path) r.diagnostics in
  render buf ~limit:max_own_diagnostics mine;
  if elsewhere <> [] then
    Buffer.add_string buf
      (Printf.sprintf "and %d elsewhere\n" (List.length elsewhere));
  (* Nothing about this file is only good news if the build reached its
     targets, so the outcome is stated rather than assumed. *)
  if mine = [] then
    Buffer.add_string buf (if r.ok then "build ok" else "build failed");
  Buffer.contents buf

(* The whole count, nested declarations included, so that an outline which
   stopped early can say how much of the file it stands for. *)
let rec count_items items =
  List.fold_left
    (fun n (i : Merlin.outline_item) -> n + 1 + count_items i.children)
    0 items

let outline items =
  let buf = Buffer.create 512 in
  let shown = ref 0 in
  let rec go level items =
    List.iter
      (fun (i : Merlin.outline_item) ->
        if !shown < max_outline && Buffer.length buf < max_answer_bytes then begin
          incr shown;
          Buffer.add_string buf
            (Printf.sprintf "%s%s %s%s\n"
               (String.make (level * 2) ' ')
               i.kind i.name
               (match i.typ with Some t -> " : " ^ t | None -> ""));
          go (level + 1) i.children
        end)
      items
  in
  go 0 items;
  let total = count_items items in
  if !shown < total then
    Buffer.add_string buf
      (Printf.sprintf
         "\n\
          … %d declarations of %d. Read the file itself for the rest, which \
          read and read_lines return a page at a time.\n"
         !shown total);
  if Buffer.length buf = 0 then "the file declares nothing"
  else Buffer.contents buf

(* A file with many uses, a prefix with many completions, and a broken file
   with many errors are each answered with the head of the list and the count
   of what was left, as a build is. *)
let max_uses = 100
let max_completions = 60

let problems (ps : Merlin.problem list) =
  let buf = Buffer.create 512 in
  List.iteri
    (fun i (p : Merlin.problem) ->
      if i < max_diagnostics then
        Buffer.add_string buf
          (Printf.sprintf "%s %s: %s\n"
             (match p.at with
             | Some at -> Printf.sprintf "%d:%d" at.line at.col
             | None -> "?:?")
             (if p.warning then "warning" else "error")
             (* A type error runs to several lines, and the ones after the
                first are indented so that the next problem's position still
                begins a line. *)
             (String.concat "\n  " (String.split_on_char '\n' p.message))))
    ps;
  let beyond = List.length ps - max_diagnostics in
  if beyond > 0 then
    Buffer.add_string buf (Printf.sprintf "… and %d more.\n" beyond);
  if Buffer.length buf = 0 then "no errors or warnings" else Buffer.contents buf

let trim_slash s =
  let n = String.length s in
  if n > 1 && s.[n - 1] = '/' then String.sub s 0 (n - 1) else s

let roots d =
  let d = trim_slash d in
  match Unix.realpath d with
  | real when trim_slash real <> d -> [ d; trim_slash real ]
  | _ | (exception Unix.Unix_error _) -> [ d ]

let under ~roots file =
  let strip r =
    let p = r ^ "/" in
    if String.starts_with ~prefix:p file then
      Some
        (String.sub file (String.length p)
           (String.length file - String.length p))
    else None
  in
  Option.value ~default:file (List.find_map strip roots)

let place ~roots (p : Merlin.place) =
  Printf.sprintf "%s:%d:%d" (under ~roots p.file) p.pos.line p.pos.col

let occurrences ~roots places =
  let buf = Buffer.create 512 in
  List.iteri
    (fun i p ->
      if i < max_uses then Buffer.add_string buf (place ~roots p ^ "\n"))
    places;
  let beyond = List.length places - max_uses in
  if beyond > 0 then
    Buffer.add_string buf (Printf.sprintf "… and %d more.\n" beyond);
  if Buffer.length buf = 0 then
    "nothing at that position is a name, so there are no uses to report"
  else Buffer.contents buf

(* Merlin reports a value once for each path it can be reached by, so a name in
   both an interface and the stdlib's aliases of it comes back several times
   over. The query's own limit counts those repeats, so they are dropped here
   rather than left to fill the answer. *)
let search ~roots hits =
  let buf = Buffer.create 512 in
  let seen = Hashtbl.create 16 in
  List.iter
    (fun (h : Merlin.hit) ->
      if not (Hashtbl.mem seen (h.name, h.typ)) then begin
        Hashtbl.replace seen (h.name, h.typ) ();
        Buffer.add_string buf
          (Printf.sprintf "%s : %s  %s\n" h.name h.typ (place ~roots h.place))
      end)
    hits;
  if Buffer.length buf = 0 then
    "nothing of that type. Write the query as a type expression, such as \"int \
     -> string\"."
  else Buffer.contents buf

(* Merlin wraps a long type across several lines, and a list of names is read by
   the line, so each entry is put back on one. *)
let one_line s =
  let words =
    List.filter
      (fun w -> w <> "")
      (String.split_on_char ' '
         (String.map
            (fun c -> if c = '\n' || c = '\r' || c = '\t' then ' ' else c)
            s))
  in
  String.concat " " words

(* The shape the outline uses, so that what a module offers reads alike however
   it was asked for. *)
let completions cs =
  let buf = Buffer.create 512 in
  let shown = ref 0 in
  List.iter
    (fun (c : Merlin.completion) ->
      if !shown < max_completions && Buffer.length buf < max_answer_bytes then begin
        incr shown;
        Buffer.add_string buf
          (Printf.sprintf "%s %s%s\n" c.kind c.name
             (match c.typ with "" -> "" | t -> " : " ^ one_line t))
      end)
    cs;
  let total = List.length cs in
  if !shown < total then
    Buffer.add_string buf
      (Printf.sprintf
         "\n\
          … %d of %d names. Give more of the prefix to narrow it, since what \
          is past this is not reported.\n"
         !shown total);
  if Buffer.length buf = 0 then "nothing in scope begins with that prefix"
  else Buffer.contents buf
