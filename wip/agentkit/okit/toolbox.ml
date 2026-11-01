(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tools over a dune workspace. Each is one call to the okitd that holds the
   session, so nothing here runs a program and nothing here forks: that is the
   whole point of the split, since a fork of a process holding a model costs
   minutes on macOS.

   A tool still reaches the filesystem only through the capabilities it was
   given. okitd reads no file of the workspace, so a write saves and a merlin
   query loads through the capability here and sends the text on.

   These tools run programs in okitd, which the filesystem tools do not. A
   build rule executes whatever command the workspace's dune files name, so
   granting a build is trusting those files with the process's authority. *)

module Tool = Ds4.Tool
module Caps = Ds4.Toolbox.Caps

(* How long okitd has to answer before it is killed and the session degraded.
   A build, a test run and a shell command are the workspace's own work and are
   given five minutes. The rest are one exchange with a dune that is already
   running, or one merlin, and are given one minute. Exceeding either ends the
   session for good, so these bound a wedge rather than a slow machine, and are
   set well above what the operation takes when it is working. *)
let long = 300.
let short = 60.

(* What okitd answers is the tool's result, refusals included. An [Error] is the
   death of the session, which is a result too: a model told that okit's server
   is gone can say so and work another way, where an empty answer would send it
   looking for a wall it cannot see. The traces of a call go to the channel the
   session's own traces go to, so an interface has one column to draw. *)
let ask client ~timeout op =
  match Client.call client ~timeout op ~on_trace:(Client.trace client) with
  | Ok output -> output
  | Error e -> e

(* Report a failure as text the model can act on. An exception would end the
   turn, where a message lets the model correct itself and try again. *)
let guard f = try f () with Eio.Exn.Io _ as e -> Printexc.to_string e

let cap_description =
  "capability to resolve under, written with angle brackets as open_dir \
   returns it, such as \"<lib>\". Use an empty string for the starting \
   directory."

let unknown_cap caps name =
  Printf.sprintf "unknown capability %S. Held: %s" name
    (String.concat ", " (Caps.names caps))

(* Resolve the capability a call names, then run [f] under it. The wording is
   the plain toolbox's, since the model holds one set of capabilities and both
   toolboxes name them alike. *)
let with_cap caps name f =
  match Caps.find caps name with
  | Some d -> guard (fun () -> f d)
  | None -> unknown_cap caps name

(* As [with_cap], but for a tool that takes a path. Every capability held is a
   directory Eio opened, so it resolves the path itself and refuses one that
   leaves it, an absolute path and one climbing out through ".." alike. Nothing
   here reads the path to decide that. A test that did would be a second opinion
   on the capability's, and would let through whatever it had not thought of:
   the check this replaced passed "../.." because it is relative. *)
let with_path caps name path f =
  match Caps.find caps name with
  | None -> unknown_cap caps name
  | Some d -> (
      try f d with
      | Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) ->
          (* A path that leaves the capability and a file within it that the
             process may not open are refused alike, and the backend names
             which it was in terms this cannot match on without binding itself
             to that backend. Both leave the model in the same position, so both
             are answered with what it can do next. *)
          Printf.sprintf
            "%S is outside this capability, or cannot be opened under it. Use \
             open_dir to ask for access to the directory holding it, then pass \
             the name it returns as cap."
            path
      | Eio.Exn.Io _ as e -> Printexc.to_string e)

(* ------------------------------------------------------------------ *)
(* Building                                                            *)
(* ------------------------------------------------------------------ *)

let build ~client =
  let codec =
    let open Dsml.Codec in
    Invoke.map "build" (fun targets -> targets)
    |> Invoke.param ~enc:Fun.id ~default:"." "targets" string
         ~description:
           "targets to build, separated by spaces. A path: \".\" for the whole \
            workspace, \"lib\" for a directory, \"src/foo.exe\" for one file. \
            Or an alias as dune's command line writes it: \"@check\", \
            \"@lib/runtest\". Left out, the whole workspace is built."
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "Build the workspace and report the errors and warnings dune finds. \
       Reach for this after changing a file by any means other than write or \
       edit, each of which builds on its own." codec (fun targets ->
      (* The string goes over whole. okitd splits it, so the same words decide
         the targets wherever the tool is driven from. *)
      ask client ~timeout:long (Proto.Build { targets }))

let test ~client =
  let codec =
    let open Dsml.Codec in
    Invoke.map "test" () |> Invoke.seal
  in
  Tool.v
    ~description:
      "Run the workspace's tests and report what they say. A test whose output \
       differs from what is recorded names the file to promote." codec
    (fun () -> ask client ~timeout:long Proto.Test)

let promote ~client =
  let codec =
    let open Dsml.Codec in
    Invoke.map "promote" (fun path -> path)
    |> Invoke.param ~enc:Fun.id "path" string
         ~description:
           "the source file to accept, written as the diagnostic that offered \
            the promotion names it"
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "Accept the file a test built in place of one in the source tree, which \
       is what a diagnostic offering a promotion asks for." codec (fun path ->
      ask client ~timeout:short (Proto.Promote { path }))

(* Save through the capability, then answer with what dune then says about the
   file. The save is done here and only the build is asked of okitd, which
   reads no file of the workspace: a capability is not something to hand across
   a pipe.

   A whole file replaced and a passage of one are the same operation to dune
   and different ones to a reader, so the call carries both the tool's name,
   which is what okitd traces, and the word the answer opens with. *)
let saved client ~dir ~path ~tool ~past content =
  Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(dir / path) content;
  (* Asked for unconditionally: okitd answers with nothing for a file dune does
     not compile, and asking is what puts the call on the trace. *)
  Printf.sprintf "%s %s\n%s" past path
    (ask client ~timeout:long (Proto.After_write { path; verb = tool }))

let write ~client ~caps =
  let codec =
    let open Dsml.Codec in
    Invoke.map "write" (fun cap path content -> (cap, path, content))
    |> Invoke.param
         ~enc:(fun (c, _, _) -> c)
         "cap" string ~description:cap_description
    |> Invoke.param
         ~enc:(fun (_, p, _) -> p)
         "path" string ~description:"destination path"
    |> Invoke.param
         ~enc:(fun (_, _, c) -> c)
         "content" string ~description:"new file contents"
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "Create a UTF-8 text file, or replace the whole of one, and, when it is \
       a file dune compiles, build the workspace and report what it then says \
       about that file. Reach for edit instead to change part of a file, which \
       this would have you send whole." codec (fun (cap, path, content) ->
      with_path caps cap path @@ fun dir ->
      saved client ~dir ~path ~tool:"write" ~past:"wrote" content)

let edit ~client ~caps =
  (* The tool the plain toolbox offers, with the build appended, so a model
     cannot tell which of the two it holds. Its parameters and the account of
     what [old] must match are that tool's. *)
  let codec =
    let open Dsml.Codec in
    Invoke.map "edit" (fun cap path old new_ -> (cap, path, old, new_))
    |> Invoke.param
         ~enc:(fun (c, _, _, _) -> c)
         "cap" string ~description:cap_description
    |> Invoke.param
         ~enc:(fun (_, p, _, _) -> p)
         "path" string ~description:"path of the file to change"
    |> Invoke.param
         ~enc:(fun (_, _, o, _) -> o)
         "old" string
         ~description:
           "the passage to replace, copied from the file exactly, whitespace \
            and all. It must appear once in the file, so include the lines \
            around it where the passage alone would match twice."
    |> Invoke.param
         ~enc:(fun (_, _, _, n) -> n)
         "new" string ~description:"what to put in its place"
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "Replace one passage of a text file, leaving the rest of the file alone, \
       and report what dune then says about it. This is the tool for changing \
       code: reach for write only to create a file or to replace the whole of \
       one, since it has you send every line you are not changing. A passage \
       that is not there, or that is there more than once, changes nothing and \
       says which it was." codec (fun (cap, path, old, new_) ->
      with_path caps cap path @@ fun dir ->
      let source = Eio.Path.load Eio.Path.(dir / path) in
      match Ds4.Toolbox.substitute ~path ~old ~new_ source with
      | Error e -> e
      | Ok updated ->
          saved client ~dir ~path ~tool:"edit" ~past:"edited" updated)

let project ~client =
  let codec =
    let open Dsml.Codec in
    Invoke.map "project" (fun module_ -> module_)
    |> Invoke.param ~enc:Fun.id ~default:"" "module" string
         ~description:
           "one module to ask about, written as OCaml writes it, \"Report\", \
            or as the file holding it, \"okit/report.ml\". Left out, the whole \
            workspace is described."
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "Describe the workspace: each library and executable, the directory it \
       is built from, the libraries it requires and the modules it is made of. \
       Named a module, describe only the component that holds it, which is how \
       to find what a module may refer to. Reach for this before looking for \
       where something lives." codec (fun module_ ->
      ask client ~timeout:short (Proto.Project { module_ }))

(* ------------------------------------------------------------------ *)
(* Asking merlin                                                       *)
(* ------------------------------------------------------------------ *)

(* The starting capability is the workspace, which is where merlin runs and
   what a path is written from. Both of the names it goes by are kept, since a
   file under the workspace that is written as an absolute path sends a model
   looking outside it. *)
let workspace caps =
  match Option.bind (Caps.find caps "") Eio.Path.native with
  | None -> []
  | Some d -> Report.roots d

(* Merlin runs in the workspace root, so a query names its file from there. A
   capability narrowed to a subdirectory would otherwise send merlin looking
   for the configuration of the wrong directory. *)
let from_root caps dir path =
  match Eio.Path.native dir with
  | None -> path
  | Some d ->
      let d = Report.trim_slash d in
      let rel = Report.under ~roots:(workspace caps) d in
      if rel = d then path else Filename.concat rel path

(* Merlin answers about the text it is handed rather than about what is on
   disk, and okitd reads no file of the workspace, so the file is read through
   the capability here and sent in the call. [dir] is passed on for the tools
   that look at a second file, which they must do through the same capability.
*)
let with_source caps cap path f =
  with_path caps cap path @@ fun dir ->
  let source = Eio.Path.load Eio.Path.(dir / path) in
  f ~dir ~path:(from_root caps dir path) ~source

let file_codec tool =
  let open Dsml.Codec in
  Invoke.map tool (fun cap path -> (cap, path))
  |> Invoke.param ~enc:fst "cap" string ~description:cap_description
  |> Invoke.param ~enc:snd "path" string
       ~description:"path of the OCaml file to ask about"
  |> Invoke.seal

let position_codec tool =
  let open Dsml.Codec in
  Invoke.map tool (fun cap path line col -> (cap, path, line, col))
  |> Invoke.param
       ~enc:(fun (c, _, _, _) -> c)
       "cap" string ~description:cap_description
  |> Invoke.param
       ~enc:(fun (_, p, _, _) -> p)
       "path" string ~description:"path of the OCaml file to ask about"
  |> Invoke.param
       ~enc:(fun (_, _, l, _) -> l)
       "line" int ~description:"line to ask about, counting from 1"
  |> Invoke.param
       ~enc:(fun (_, _, _, c) -> c)
       "col" int ~description:"column to ask about, counting from 0"
  |> Invoke.seal

(* The interface states a module's shape once and the implementation restates
   it among everything else, so a reader who asked about the implementation is
   told the interface is there before it reads on. Only that direction: a
   reader already looking at an interface is where it should be. *)
let interface_note dir path =
  if not (Filename.check_suffix path ".ml") then ""
  else
    let mli = path ^ "i" in
    if not (Eio.Path.is_file Eio.Path.(dir / mli)) then ""
    else
      Printf.sprintf
        "%s is this module's interface, which states what it offers and \
         nothing else. Outline that before reading on.\n\n"
        mli

let outline ~client ~caps =
  Tool.v
    ~description:
      "List what an OCaml file declares, with the type of each declaration and \
       the contents of each module. Ask about a module's .mli first, since \
       that is everything the module offers and is far shorter than its .ml. \
       This is how to learn what an OCaml file holds: reach for read or \
       read_lines on one only once this has told you which declaration you \
       want and you need to see how it is written." (file_codec "outline")
    (fun (cap, path) ->
      with_source caps cap path @@ fun ~dir ~path:from_root ~source ->
      interface_note dir path
      ^ ask client ~timeout:short (Proto.Outline { path = from_root; source }))

let type_at ~client ~caps =
  Tool.v
    ~description:
      "Report the type of the smallest expression at a line and column of an \
       OCaml file, as the compiler prints it. This is the compiler's own \
       answer, so reach for it rather than working a type out by reading the \
       code that produced it." (position_codec "type_at")
    (fun (cap, path, line, col) ->
      with_source caps cap path @@ fun ~dir:_ ~path ~source ->
      ask client ~timeout:short (Proto.Type_at { path; source; line; col }))

(* How much of a definition [locate] shows. A short function fits whole, and a
   longer one says where it stopped so that the rest is read deliberately. *)
let max_definition_lines = 20

(* A tab counts as one column, which is wrong for a file that mixes tabs with
   spaces and right for one that uses either alone. What is compared is one
   line's indentation against another's in the same file. *)
let indent line =
  let n = String.length line in
  let rec go i =
    if i < n && (line.[i] = ' ' || line.[i] = '\t') then go (i + 1) else i
  in
  go 0

(* A definition runs from its first line to the last one indented further than
   it, blank lines included, which is how OCaml lays one out at every level of
   nesting. What follows at the same indentation or less is the next
   declaration, whatever the two of them are. *)
let definition lines ~first =
  let margin = indent lines.(first) in
  let rec take i acc =
    if i >= Array.length lines || i - first >= max_definition_lines then
      (List.rev acc, i - first >= max_definition_lines)
    else
      let line = lines.(i) in
      if i > first && String.trim line <> "" && indent line <= margin then
        (List.rev acc, false)
      else take (i + 1) ((i + 1, line) :: acc)
  in
  let numbered, cut = take first [] in
  (* A blank line before the next declaration belongs to neither. *)
  let rec trim = function
    | (_, line) :: rest when String.trim line = "" -> trim rest
    | rest -> rest
  in
  (List.rev (trim (List.rev numbered)), cut)

(* Read the definition okitd located, through the capability rather than in
   okitd, which holds this process's whole authority and reads no file of the
   workspace. The starting capability is the workspace, which is what the
   answer's path is written from. *)
let excerpt caps file ~line =
  match Caps.find caps "" with
  | None -> ""
  | Some root -> (
      let lines =
        Array.of_list
          (String.split_on_char '\n' (Eio.Path.load Eio.Path.(root / file)))
      in
      match line - 1 with
      | first when first < 0 || first >= Array.length lines -> ""
      | first ->
          let numbered, cut = definition lines ~first in
          String.concat ""
            (List.map (fun (n, l) -> Printf.sprintf "%6d  %s\n" n l) numbered)
          ^
          if cut then
            Printf.sprintf
              "… the first %d lines. Use read_lines from there for the rest.\n"
              max_definition_lines
          else "")

(* okitd answers a definition it found as [file:line:column], and one it could
   not reach with merlin's own account of why, which does not end in two
   numbers. *)
let split_place answer =
  match List.rev (String.split_on_char ':' (String.trim answer)) with
  | col :: line :: rest -> (
      match (int_of_string_opt col, int_of_string_opt line) with
      | Some col, Some line ->
          Some (String.concat ":" (List.rev rest), line, col)
      | _ -> None)
  | _ -> None

let outside =
  "That is outside this workspace, in a library installed elsewhere. Use \
   open_dir on the directory holding it to read it, or complete to see what \
   its module offers."

let locate ~client ~caps =
  Tool.v
    ~description:
      "Report where the name at a line and column of an OCaml file is defined, \
       as file:line:column, with the definition itself. It follows the name \
       through every module alias and open, so reach for this rather than grep \
       for the name or a guess at which file holds it."
    (position_codec "locate") (fun (cap, path, line, col) ->
      with_source caps cap path @@ fun ~dir:_ ~path ~source ->
      let answer =
        ask client ~timeout:short (Proto.Locate { path; source; line; col })
      in
      match split_place answer with
      | None -> answer
      | Some (file, line, _) ->
          answer ^ "\n"
          ^
          if Filename.is_relative file then excerpt caps file ~line else outside)

let errors ~client ~caps =
  Tool.v
    ~description:
      "Report what the compiler makes of one OCaml file, as it stands on disk, \
       without building the workspace. Each error and warning is given with \
       the line and column it starts at. Reach for this to check a file you \
       have changed by some other means than write." (file_codec "errors")
    (fun (cap, path) ->
      with_source caps cap path @@ fun ~dir:_ ~path ~source ->
      ask client ~timeout:short (Proto.Errors { path; source }))

let occurrences ~client ~caps =
  Tool.v
    ~description:
      "List every use of the name at a line and column of an OCaml file, \
       across the whole workspace, as file:line:column. This is the tool for \
       finding where an identifier is used: it knows the identifier rather \
       than the text, so it finds none of what a search for the same letters \
       would turn up in comments and unrelated names. The definition itself is \
       one of the uses reported. Reach for this before changing anything a \
       caller can see." (position_codec "occurrences")
    (fun (cap, path, line, col) ->
      with_source caps cap path @@ fun ~dir:_ ~path ~source ->
      (* Long, because the workspace's index is built before the query. *)
      ask client ~timeout:long (Proto.Occurrences { path; source; line; col }))

(* How many values a search answers with. Merlin ranks them, so the ones past
   this are the ones that fit the query least. *)
let search_limit = 10

let search ~client ~caps =
  let codec =
    let open Dsml.Codec in
    Invoke.map "search" (fun cap path query -> (cap, path, query))
    |> Invoke.param
         ~enc:(fun (c, _, _) -> c)
         "cap" string ~description:cap_description
    |> Invoke.param
         ~enc:(fun (_, p, _) -> p)
         "path" string
         ~description:
           "an OCaml file to search from. It decides which libraries are in \
            scope, so name one in the component you are working in."
    |> Invoke.param
         ~enc:(fun (_, _, q) -> q)
         "query" string
         ~description:
           "the type to look for, written as a type expression such as \"int \
            -> string\" or \"'a list -> 'a option\""
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "Find values by their type, in this workspace and every library it may \
       refer to, best match first. A value taking its arguments in another \
       order, or taking more of them, still matches. Reach for this when you \
       know the shape of what you want but not its name." codec
    (fun (cap, path, query) ->
      with_source caps cap path @@ fun ~dir:_ ~path ~source ->
      ask client ~timeout:short
        (Proto.Search { path; source; query; limit = search_limit }))

let complete ~client ~caps =
  let codec =
    let open Dsml.Codec in
    Invoke.map "complete" (fun cap path line col prefix ->
        (cap, path, line, col, prefix))
    |> Invoke.param
         ~enc:(fun (c, _, _, _, _) -> c)
         "cap" string ~description:cap_description
    |> Invoke.param
         ~enc:(fun (_, p, _, _, _) -> p)
         "path" string ~description:"path of the OCaml file to ask from"
    |> Invoke.param
         ~enc:(fun (_, _, l, _, _) -> l)
         "line" int ~description:"line to ask from, counting from 1"
    |> Invoke.param
         ~enc:(fun (_, _, _, c, _) -> c)
         "col" int ~description:"column to ask from, counting from 0"
    |> Invoke.param
         ~enc:(fun (_, _, _, _, p) -> p)
         "prefix" string
         ~description:
           "what the name begins with, qualified as it would be written at \
            that position: \"List.\" for everything that module offers, \
            \"List.f\" to narrow it, \"fo\" for what is in scope beginning \
            with those letters."
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "List what can be named at a point in an OCaml file, with the type of \
       each. Reach for this to learn what an installed library offers, whose \
       source is not in this workspace and so has no file to outline." codec
    (fun (cap, path, line, col, prefix) ->
      with_source caps cap path @@ fun ~dir:_ ~path ~source ->
      ask client ~timeout:short
        (Proto.Complete { path; source; line; col; prefix }))

(* ------------------------------------------------------------------ *)
(* The shell                                                           *)
(* ------------------------------------------------------------------ *)

(* The same tool the model has always had, run in okitd rather than here. Its
   name, its argument and its description are {!Ds4.Toolbox.bash}'s, since
   the model must not have to know which process the shell ran in. *)
let bash ~client =
  let codec =
    let open Dsml.Codec in
    Invoke.map "bash" (fun command -> command)
    |> Invoke.param ~enc:Fun.id "command" string
         ~description:"shell command line, run with 'bash -c'"
    |> Invoke.seal
  in
  Tool.v ~description:"Run a shell command and capture its combined output."
    codec (fun command -> ask client ~timeout:long (Proto.Bash { command }))
