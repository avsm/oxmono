(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Ready-made tools. Each closes over an Eio capability, so its reach is what
   the caller granted and no more.

   The filesystem tools reach the disk through a capability held in [Caps],
   named by the tool call. Every such capability comes from
   [Eio.Path.open_subtree], so Eio rejects ".." and symlinks that leave it and
   no path handling here can get it wrong. A model that narrows its own reach
   with [open_dir] cannot widen it again, since minting is always relative to a
   capability it already holds.

   [bash] is the exception, since a shell runs with the whole process's
   authority. The other tools exist so that it is rarely needed. *)

type dir = Eio.Fs.dir_ty Eio.Path.t

(* The capabilities an agent holds, each under a name it can quote back.

   Names come from Eio's own printer, so a capability is called what Eio calls
   it. That is a label rather than a path, so two directories with the same
   basename would print alike and the second is suffixed to keep names
   distinct. *)
module Caps = struct
  type t = {
    sw : Eio.Switch.t;
    fs : dir; (* opens directories outside the start, once approved *)
    approve : string -> bool;
    root_name : string;
    mutable held : (string * dir) list;
  }

  let label d = Format.asprintf "%a" Eio.Path.pp d

  let rec fresh held base n =
    let candidate = if n = 1 then base else Printf.sprintf "%s~%d" base n in
    if List.mem_assoc candidate held then fresh held base (n + 1) else candidate

  (* The start is opened here rather than held as it was handed over, so that
     the confinement above is this module's to keep rather than every caller's.
     A start taken straight from [Eio.Stdenv.fs] resolves "/etc/passwd", and
     "../.." alike, against the whole filesystem, and every tool reaching
     through it would inherit that. *)
  let create ~sw ~fs ?(approve = fun _ -> true) start =
    let start = (Eio.Path.open_subtree ~sw start :> dir) in
    let root_name = label start in
    { sw; fs :> dir; approve; root_name; held = [ (root_name, start) ] }

  let root_name t = t.root_name
  let names t = List.map fst t.held

  (* An empty name is the start directory, so that a model can always make its
     first call without having been told a name. *)
  let find t name =
    List.assoc_opt (if name = "" then t.root_name else name) t.held

  (* Native paths let a directory be matched against the capabilities held, so
     a listing can say which of its entries already have one. *)
  let native_dir d =
    match Eio.Path.native d with
    | None -> None
    | Some s ->
        let n = String.length s in
        Some (if n > 1 && s.[n - 1] = '/' then String.sub s 0 (n - 1) else s)

  let name_for t d =
    match native_dir d with
    | None -> None
    | Some target ->
        List.find_map
          (fun (n, c) ->
            match native_dir c with
            | Some s when s = target -> Some n
            | _ -> None)
          t.held

  let describe t =
    String.concat "\n"
      (List.map
         (fun (n, d) ->
           Printf.sprintf "%s  %s" n
             (Option.value ~default:"(unnamed)" (Eio.Path.native d)))
         t.held)

  (* A leading ~ is what a person writes and so what a model copies. Expanding
     it here saves a request for a directory that does not exist. *)
  let expand path =
    match (path, Sys.getenv_opt "HOME") with
    | "~", Some home -> home
    | _, Some home
      when String.length path > 1 && path.[0] = '~' && path.[1] = '/' ->
        Filename.concat home (String.sub path 2 (String.length path - 2))
    | _ -> path

  type refusal = Unknown_parent of string | Denied of string

  let register t d =
    let name = fresh t.held (label d) 1 in
    t.held <- t.held @ [ (name, (d :> dir)) ];
    name

  (* A relative path narrows a capability already held, which needs no
     permission because it is already reachable. An absolute path asks for
     somewhere new and has to be approved. *)
  let mint t ~parent ~path =
    let path = expand path in
    if Filename.is_relative path then
      match find t parent with
      | None -> Error (Unknown_parent parent)
      | Some p ->
          Ok (register t (Eio.Path.open_subtree ~sw:t.sw Eio.Path.(p / path)))
    else if not (t.approve path) then Error (Denied path)
    else Ok (register t (Eio.Path.open_subtree ~sw:t.sw Eio.Path.(t.fs / path)))
end

(* Every tool bounds its own output. The agent also caps each result, but only
   after the work is done, so a search of a large tree would still read every
   file. Stopping early is cheaper, and the note left in place of what was
   dropped says what to do next rather than leaving the model to assume it saw
   everything. *)
(* Directories a summary or a search reports but does not descend into. A build
   tree mirrors the source, so walking it doubles every result and buries what
   was being looked for. Names beginning with a dot are treated the same way. *)
let opaque_dirs = [ "_build"; "_opam"; "node_modules"; "target"; "dist" ]

let descend_into name =
  (not (String.length name > 0 && name.[0] = '.'))
  && not (List.mem name opaque_dirs)

let max_entries = 200
let max_matches = 100
let max_depth = 12
let max_lines = 500
let max_scan_bytes = 1_000_000

(* How much of a file one result carries. The agent shortens a result over its
   own limit, 4000 characters by default, by removing the middle, and the lines
   that go that way cannot be asked for again: nothing in the answer says which
   they were. A page stops short of that limit and names the line it stopped
   at, so a file of any length can be read whole, a page at a time. *)
let page_bytes = 3500

(* The first whole lines of [s] that fit in [page_bytes], with how many were
   taken and how many the whole of [s] has. Cutting at a line boundary is what
   makes the line to continue from exact, and keeps the text returned the
   file's own, which an edit copies its [old] out of. *)
let head_page s =
  let lines = String.split_on_char '\n' s in
  (* A file ending in a newline splits with an empty last element, which is not
     a line of it. *)
  let lines =
    match List.rev lines with "" :: rest -> List.rev rest | _ -> lines
  in
  let buf = Buffer.create page_bytes in
  let shown = ref 0 in
  let stop = ref false in
  List.iter
    (fun line ->
      if not !stop then
        (* The first line is taken however long it is, so that a file of one
           very long line still advances rather than answering with nothing. *)
        if !shown > 0 && Buffer.length buf + String.length line + 1 > page_bytes
        then stop := true
        else begin
          incr shown;
          Buffer.add_string buf line;
          Buffer.add_char buf '\n'
        end)
    lines;
  (Buffer.contents buf, !shown, List.length lines)

let note_truncated buf ~shown ~what =
  Buffer.add_string buf
    (Printf.sprintf "\n… truncated at %d %s. Narrow the query for more.\n" shown
       what)

let kind_name : Eio.File.Stat.kind -> string = function
  | `Directory -> "dir"
  | `Regular_file -> "file"
  | `Symbolic_link -> "symlink"
  | `Fifo -> "fifo"
  | `Socket -> "socket"
  | `Block_device -> "block"
  | `Character_special -> "char"
  | `Unknown -> "unknown"

(* Report a failure as text the model can act on. An exception would end the
   turn, where a message lets the model correct itself and try again. *)
let guard f = try f () with Eio.Exn.Io _ as e -> Printexc.to_string e

(* Files worth searching. Matches inside a binary are noise, and reading a very
   large file to find nothing is wasted work. *)
let scannable (st : Eio.File.Stat.t) =
  st.kind = `Regular_file && Optint.Int63.to_int st.size <= max_scan_bytes

let looks_binary s =
  let n = min (String.length s) 8000 in
  let rec at i = i < n && (s.[i] = '\000' || at (i + 1)) in
  at 0

(* A matching line is shown as it is, so that it can be quoted back to [edit],
   unless it is so long that one match would crowd out the rest. *)
let max_line_bytes = 400

let clip_line line =
  if String.length line <= max_line_bytes then line
  else
    Printf.sprintf "%s… (%d bytes in all)"
      (String.sub line 0 max_line_bytes)
      (String.length line)

(* Replace a file by writing its new contents beside it and renaming them over
   it, so that a failure part way leaves the old file whole rather than cut
   short. The mode of the file it replaces is kept. A symlink, or a file with
   more than one link, is written in place instead, since a rename would
   replace the link with a file or separate the names. *)
let replace_file dir path content =
  let target = Eio.Path.(dir / path) in
  let in_place perm =
    Eio.Path.save ~create:(`Or_truncate perm) target content
  in
  match Eio.Path.stat ~follow:false target with
  | st when st.kind = `Symbolic_link || Int64.compare st.nlink 1L > 0 ->
      in_place 0o644
  | st -> (
      let perm = if st.kind = `Regular_file then st.perm else 0o644 in
      let tmp =
        Filename.concat (Filename.dirname path)
          ("." ^ Filename.basename path ^ ".ds4-tmp")
      in
      let tmp = Eio.Path.(dir / tmp) in
      try
        Eio.Path.save ~create:(`Or_truncate perm) tmp content;
        Eio.Path.rename tmp target
      with e ->
        (try Eio.Path.unlink tmp with Eio.Exn.Io _ -> ());
        raise e)
  | exception Eio.Exn.Io _ -> in_place 0o644

let contains ~needle s =
  let n = String.length s and m = String.length needle in
  if m = 0 then true
  else
    let rec at i = i + m <= n && (String.sub s i m = needle || at (i + 1)) in
    at 0

(* The first occurrence of [needle] in [s] at or after [i]. *)
let index_from s needle i =
  let n = String.length s and m = String.length needle in
  let rec at i =
    if i + m > n then None
    else if String.sub s i m = needle then Some i
    else at (i + 1)
  in
  at i

(* Enough of a passage to name it in a refusal. The whole of it would be the
   file back again, and its first line is what a reader recognises. *)
let sketch s =
  let line =
    match String.index_opt s '\n' with None -> s | Some i -> String.sub s 0 i
  in
  if String.length line <= 60 then Printf.sprintf "%S" line
  else Printf.sprintf "%S…" (String.sub line 0 60)

(* A passage is replaced only where it names one place. Zero matches and two
   are both refused, and both say what to do, because the alternative is a tool
   that edits the wrong one of two look-alike passages and reports success. The
   model has no way to see that from an empty answer. *)
let substitute ~path ~old ~new_ source =
  if old = "" then
    Error
      (Printf.sprintf
         "refusing to replace the empty string in %s. Give the text to \
          replace, copied from the file."
         path)
  else
    let rec count i n =
      match index_from source old i with
      | None -> n
      | Some j -> count (j + String.length old) (n + 1)
    in
    match count 0 0 with
    | 0 ->
        Error
          (Printf.sprintf
             "%s is not in %s. Copy the text to replace out of a read of the \
              file, whitespace and all."
             (sketch old) path)
    | 1 ->
        let j = Option.get (index_from source old 0) in
        let m = String.length old in
        Ok
          (String.sub source 0 j ^ new_
          ^ String.sub source (j + m) (String.length source - j - m))
    | n ->
        Error
          (Printf.sprintf
             "%s appears %d times in %s. Give more of the lines around the one \
              you mean, so that it matches once."
             (sketch old) n path)

(* Walk the subtree depth first, calling [f] for each entry until [stop ()] is
   true. Symlinks are reported but never followed, so a link back to an ancestor
   cannot make this loop. *)
let walk ~dir ~stop f =
  let rec go rel depth =
    if depth <= max_depth && not (stop ()) then
      let here = if rel = "" then dir else Eio.Path.(dir / rel) in
      match Eio.Path.read_dir_entries here with
      | exception Eio.Exn.Io _ ->
          () (* unreadable directory: skip, keep going *)
      | entries ->
          List.iter
            (fun (kind, name) ->
              if not (stop ()) then begin
                let child =
                  if rel = "" then name else Filename.concat rel name
                in
                f child kind;
                if kind = `Directory && descend_into name then
                  go child (depth + 1)
              end)
            entries
  in
  go "" 0

(* Resolve the capability a call names, then run [f] under it. An unknown name
   is reported with the ones that are held, so the model can correct itself. *)
let with_cap caps name f =
  match Caps.find caps name with
  | Some d -> guard (fun () -> f d)
  | None ->
      Printf.sprintf "unknown capability %S. Held: %s" name
        (String.concat ", " (Caps.names caps))

(* As [with_cap], but for the tools that take a path. An absolute path asks to
   leave the capability, which these tools cannot grant. Saying so plainly
   matters: resolving it fails somewhere inside Eio and reads back as an empty
   directory, which sends a model looking for a way around rather than asking
   for access. *)
let with_path caps name path f =
  if not (Filename.is_relative path) then
    Printf.sprintf
      "%S is outside this capability. Use open_dir to ask for access to it, \
       then pass the name it returns as cap."
      path
  else with_cap caps name f

(* Resolve a path under a capability. An empty or "." path is the capability's
   own directory. *)
let at d path = if path = "." || path = "" then d else Eio.Path.(d / path)

let open_dir ~caps =
  let codec =
    let open Dsml.Codec in
    Invoke.map "open_dir" (fun cap path -> (cap, path))
    |> Invoke.param ~enc:fst "cap" string
         ~description:
           "the capability the new one is taken from, written with angle \
            brackets such as \"<lib>\". Use an empty string for the starting \
            directory."
    |> Invoke.param ~enc:snd "path" string
         ~description:
           "directory to open. A relative path is taken under cap. \
            An             absolute one asks for a directory outside it, and a \
            leading ~             stands for the home directory, so use \
            \"~/x\" rather than             guessing where home is."
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "Open a directory as a new capability, confined to that directory and \
       its contents. Returns a name in angle brackets, such as \"<lib>\", to \
       pass verbatim as the cap argument of later calls. A capability can only \
       ever be narrowed, never widened." codec (fun (cap, path) ->
      guard @@ fun () ->
      match Caps.mint caps ~parent:cap ~path with
      | Error (Caps.Unknown_parent p) ->
          Printf.sprintf "unknown capability %S. Held: %s" p
            (String.concat ", " (Caps.names caps))
      | Error (Caps.Denied p) -> Printf.sprintf "access to %s was refused" p
      | Ok name -> name)

let caps ~caps =
  let codec =
    let open Dsml.Codec in
    Invoke.map "caps" (fun _ -> ())
    |> Invoke.param
         ~enc:(fun () -> "")
         "unused" string ~description:"ignored, pass an empty string"
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "List the capabilities currently held, with the directory each one \
       covers."
    codec (fun () -> Caps.describe caps)

let list ~caps =
  let codec =
    let open Dsml.Codec in
    Invoke.map "list" (fun cap path -> (cap, path))
    |> Invoke.param ~enc:fst "cap" string
         ~description:
           "capability to resolve under, written with angle brackets as \
            open_dir returns it, such as \"<lib>\". Use an empty string for \
            the starting directory."
    |> Invoke.param ~enc:snd "path" string
         ~description:"directory to list. Use \".\" for the capability itself."
    |> Invoke.seal
  in
  Tool.v ~description:"List a directory's entries with their kind and size."
    codec (fun (cap, path) ->
      with_path caps cap path @@ fun dir ->
      let target = at dir path in
      let entries = Eio.Path.read_dir_entries target in
      let buf = Buffer.create 512 in
      let shown = ref 0 in
      List.iter
        (fun (kind, name) ->
          if !shown < max_entries then begin
            incr shown;
            let size =
              match kind with
              | `Regular_file -> (
                  match
                    Eio.Path.stat ~follow:false Eio.Path.(target / name)
                  with
                  | st -> Printf.sprintf "%d" (Optint.Int63.to_int st.size)
                  | exception Eio.Exn.Io _ -> "?")
              | _ -> "-"
            in
            let mark =
              if kind = `Directory then
                match Caps.name_for caps Eio.Path.(target / name) with
                | Some n -> "  " ^ n
                | None -> ""
              else ""
            in
            Buffer.add_string buf
              (Printf.sprintf "%-8s %10s  %s%s\n" (kind_name kind) size name
                 mark)
          end)
        entries;
      if List.length entries > max_entries then
        note_truncated buf ~shown:!shown ~what:"entries";
      if Buffer.length buf = 0 then "(empty directory)" else Buffer.contents buf)

(* One call in place of walking a tree with repeated [list]s, which is what an
   agent otherwise does to orient itself and what costs it the most context. *)
let tree ~caps =
  let codec =
    let open Dsml.Codec in
    Invoke.map "tree" (fun cap path depth -> (cap, path, depth))
    |> Invoke.param
         ~enc:(fun (c, _, _) -> c)
         "cap" string
         ~description:
           "capability to resolve under, written with angle brackets as \
            open_dir returns it, such as \"<lib>\". Use an empty string for \
            the starting directory."
    |> Invoke.param
         ~enc:(fun (_, p, _) -> p)
         "path" string
         ~description:"directory to summarise. Use \".\" for the capability."
    |> Invoke.param
         ~enc:(fun (_, _, d) -> d)
         "depth" int
         ~description:"how many levels to descend. 0 or less means 3."
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "Summarise a directory tree in one call, showing each entry indented by \
       depth and each directory with the number of entries it holds. A \
       directory that already has a capability is shown with its name in angle \
       brackets. Build directories are listed but not descended into. Prefer \
       this to repeated list calls when getting your bearings." codec
    (fun (cap, path, depth) ->
      with_path caps cap path @@ fun dir ->
      let limit = if depth <= 0 then 3 else min depth max_depth in
      let root = at dir path in
      let buf = Buffer.create 1024 in
      let shown = ref 0 in
      (* Entries a summary should report, which is everything but the dot names
         the recursive tools skip. *)
      (* Order what a reader came for first. A name beginning with a dot or an
         underscore is machinery rather than source, so it sorts last and, when
         the entry budget runs out, is what gets cut. *)
      let interesting n =
        not (String.length n > 0 && (n.[0] = '.' || n.[0] = '_'))
      in
      let visible d =
        match Eio.Path.read_dir_entries d with
        | exception Eio.Exn.Io _ -> None
        | entries ->
            entries
            |> List.filter (fun (_, n) ->
                not (String.length n > 0 && n.[0] = '.'))
            |> List.stable_sort (fun (_, a) (_, b) ->
                match (interesting a, interesting b) with
                | true, false -> -1
                | false, true -> 1
                | _ -> String.compare a b)
            |> Option.some
      in
      (* Mark a directory that already has a capability, so that it is plain
         which parts of the tree can be reached without opening anything. *)
      let held d =
        match Caps.name_for caps d with
        | Some n -> Printf.sprintf " %s" n
        | None -> ""
      in
      (* Every directory carries its entry count, so a directory listed with
         nothing under it is readably different from an empty one: the count
         says how much was left out, whether the depth ran out or the whole
         summary hit its limit. *)
      let rec go here level =
        match visible here with
        | None -> ()
        | Some entries ->
            List.iter
              (fun (kind, name) ->
                if !shown < max_entries then begin
                  incr shown;
                  let pad = String.make (level * 2) ' ' in
                  match kind with
                  | `Directory ->
                      let child = Eio.Path.(here / name) in
                      let n =
                        match visible child with
                        | Some e -> List.length e
                        | None -> 0
                      in
                      Buffer.add_string buf
                        (Printf.sprintf "%s%s/ (%d)%s\n" pad name n (held child));
                      if level + 1 < limit && n > 0 && descend_into name then
                        go child (level + 1)
                  | _ ->
                      Buffer.add_string buf (Printf.sprintf "%s%s\n" pad name)
                end)
              entries
      in
      match visible root with
      | None -> Printf.sprintf "cannot read %s" path
      | Some entries ->
          Buffer.add_string buf
            (Printf.sprintf "%s (%d)\n" path (List.length entries));
          go root 0;
          if !shown >= max_entries then
            note_truncated buf ~shown:!shown ~what:"entries";
          if entries = [] then Printf.sprintf "%s is empty" path
          else Buffer.contents buf)

let read ~caps =
  let codec =
    let open Dsml.Codec in
    Invoke.map "read" (fun cap path -> (cap, path))
    |> Invoke.param ~enc:fst "cap" string
         ~description:
           "capability to resolve under, written with angle brackets as \
            open_dir returns it, such as \"<lib>\". Use an empty string for \
            the starting directory."
    |> Invoke.param ~enc:snd "path" string
         ~description:"path of the file to read"
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "Read a UTF-8 text file. A file longer than one result holds comes back \
       as its first lines and the number of the line it stopped at, so nothing \
       is lost without saying so: call read_lines from that line for the rest, \
       as many times as it takes. This is the most expensive answer to any \
       question, since what comes back stays in the conversation from then on. \
       Reach for a tool that answers the question you actually have, and for \
       read_lines once you know which part of the file holds it." codec
    (fun (cap, path) ->
      with_path caps cap path @@ fun dir ->
      let source = Eio.Path.load Eio.Path.(dir / path) in
      if String.length source <= page_bytes then source
      else
        let body, shown, total = head_page source in
        if shown >= total then source
        else
          Printf.sprintf
            "%s\n\
             … lines 1-%d of %d. Read from line %d with read_lines for the rest.\n"
            body shown total (shown + 1))

(* A separate tool rather than optional arguments on [read], because codec
   parameters are all required and reading a whole file is the common case. *)
let read_lines ~caps =
  let codec =
    let open Dsml.Codec in
    Invoke.map "read_lines" (fun cap path start count ->
        (cap, path, start, count))
    |> Invoke.param
         ~enc:(fun (c, _, _, _) -> c)
         "cap" string
         ~description:
           "capability to resolve under, written with angle brackets as \
            open_dir returns it, such as \"<lib>\". Use an empty string for \
            the starting directory."
    |> Invoke.param
         ~enc:(fun (_, p, _, _) -> p)
         "path" string ~description:"path of the file to read"
    |> Invoke.param
         ~enc:(fun (_, _, s, _) -> s)
         "start" int ~description:"first line to return, counting from 1"
    |> Invoke.param
         ~enc:(fun (_, _, _, c) -> c)
         "count" int
         ~description:"how many lines to return. 0 or less reads to the end."
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "Read a range of lines from a text file, numbered as they are in it. \
       Prefer this to read whenever another tool has already told you which \
       lines matter, and widen the range rather than reading the whole file \
       when it turns out to be too narrow. A range longer than one result \
       holds stops early and names the line it stopped at, so reading again \
       from there reaches the rest of the file however long it is." codec
    (fun (cap, path, start, count) ->
      with_path caps cap path @@ fun dir ->
      let start = max 1 start in
      (* What was asked for, kept apart from what will be returned, so that a
         window ending because the caller's count ran out is not reported as
         one this tool cut short. *)
      let asked = count in
      let count = if count <= 0 then max_lines else min count max_lines in
      let buf = Buffer.create 512 in
      let shown = ref 0 and last = ref 0 and total = ref 0 in
      let full = ref false in
      (* Stream the file rather than load it, so that reading from the middle
         of a large file costs no more memory than reading its start. The whole
         of it is walked even once the page is full, since the count of what is
         left is what tells the model where it has got to. *)
      Eio.Path.with_lines
        Eio.Path.(dir / path)
        (fun lines ->
          Seq.iteri
            (fun i line ->
              let n = i + 1 in
              total := n;
              if n >= start && (not !full) && !shown < count then
                (* The first line of a page is taken however long it is, so
                   that a file of very long lines still advances. Eight
                   characters cover the number it is written with. *)
                if
                  !shown > 0
                  && Buffer.length buf + String.length line + 8 > page_bytes
                then full := true
                else begin
                  incr shown;
                  last := n;
                  Buffer.add_string buf (Printf.sprintf "%6d  %s\n" n line)
                end)
            lines);
      if !shown = 0 then
        Printf.sprintf "no lines at or after %d in %s, which has %d lines" start
          path !total
      else begin
        (* A window that ran out is what the caller asked for and needs no
           instruction. One this tool's own bound ended does. *)
        let cut =
          !full || (!shown = count && (asked <= 0 || asked > max_lines))
        in
        if !last < !total then
          Buffer.add_string buf
            (Printf.sprintf "\n… lines %d-%d of %d.%s\n" start !last !total
               (if cut then
                  Printf.sprintf " Read from line %d for the rest." (!last + 1)
                else ""));
        Buffer.contents buf
      end)

let find ~caps =
  let codec =
    let open Dsml.Codec in
    Invoke.map "find" (fun cap path substring -> (cap, path, substring))
    |> Invoke.param
         ~enc:(fun (c, _, _) -> c)
         "cap" string
         ~description:
           "capability to resolve under, written with angle brackets as \
            open_dir returns it, such as \"<lib>\". Use an empty string for \
            the starting directory."
    |> Invoke.param
         ~enc:(fun (_, p, _) -> p)
         "path" string ~description:"directory to search under"
    |> Invoke.param
         ~enc:(fun (_, _, s) -> s)
         "substring" string
         ~description:
           "match entries whose path contains this text. An empty string \
            matches all."
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "Find files and directories under a path whose name contains a \
       substring, searching recursively." codec (fun (cap, path, substring) ->
      with_path caps cap path @@ fun dir ->
      let root = at dir path in
      let buf = Buffer.create 512 in
      let shown = ref 0 in
      walk ~dir:root
        ~stop:(fun () -> !shown >= max_entries)
        (fun rel kind ->
          if contains ~needle:substring rel then begin
            incr shown;
            Buffer.add_string buf
              (Printf.sprintf "%-8s %s\n" (kind_name kind) rel)
          end);
      if !shown = 0 then
        Printf.sprintf "nothing matching %S under %s" substring path
      else begin
        if !shown >= max_entries then
          note_truncated buf ~shown:!shown ~what:"matches";
        Buffer.contents buf
      end)

let grep ~caps =
  let codec =
    let open Dsml.Codec in
    Invoke.map "grep" (fun cap path substring -> (cap, path, substring))
    |> Invoke.param
         ~enc:(fun (c, _, _) -> c)
         "cap" string
         ~description:
           "capability to resolve under, written with angle brackets as \
            open_dir returns it, such as \"<lib>\". Use an empty string for \
            the starting directory."
    |> Invoke.param
         ~enc:(fun (_, p, _) -> p)
         "path" string ~description:"file or directory to search"
    |> Invoke.param
         ~enc:(fun (_, _, s) -> s)
         "substring" string
         ~description:
           "literal text to search for. This is not a regular expression."
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "Search file contents for a literal string, recursively, reporting each \
       match as path:line: text. The text is the line as it is in the file, so \
       it can be quoted to edit. Binary files and files over 1 MB are not \
       searched, and the answer counts them." codec
    (fun (cap, path, substring) ->
      with_path caps cap path @@ fun dir ->
      if substring = "" then "refusing to search for the empty string"
      else begin
        let buf = Buffer.create 512 in
        let shown = ref 0 in
        let large = ref 0 and binary = ref 0 and unreadable = ref 0 in
        let search rel target =
          match Eio.Path.stat ~follow:false target with
          | exception Eio.Exn.Io _ -> incr unreadable
          | st when st.kind <> `Regular_file -> ()
          | st when not (scannable st) -> incr large
          | _ -> (
              match Eio.Path.load target with
              | exception Eio.Exn.Io _ -> incr unreadable
              | body when looks_binary body -> incr binary
              | body ->
                  List.iteri
                    (fun i line ->
                      if !shown < max_matches && contains ~needle:substring line
                      then begin
                        incr shown;
                        Buffer.add_string buf
                          (Printf.sprintf "%s:%d: %s\n" rel (i + 1)
                             (clip_line line))
                      end)
                    (String.split_on_char '\n' body))
        in
        let root = at dir path in
        (* A root that is not there is an error rather than a search that
           found nothing, since the two call for different next steps. *)
        (match Eio.Path.stat ~follow:false root with
        | st when st.kind = `Regular_file -> search path root
        | _ ->
            walk ~dir:root
              ~stop:(fun () -> !shown >= max_matches)
              (fun rel kind ->
                if kind = `Regular_file then search rel Eio.Path.(root / rel)));
        if !shown = 0 then
          Buffer.add_string buf
            (Printf.sprintf "no matches for %S under %s\n" substring path)
        else if !shown >= max_matches then
          note_truncated buf ~shown:!shown ~what:"matches";
        (* A search that passed over files says so, since what it did not read
           may be where the text is. *)
        let skipped =
          List.filter_map
            (fun (n, what) ->
              if n = 0 then None else Some (Printf.sprintf "%d %s" n what))
            [
              (!large, "larger than 1 MB");
              (!binary, "binary");
              (!unreadable, "unreadable");
            ]
        in
        if skipped <> [] then
          Buffer.add_string buf
            (Printf.sprintf "Not searched: %s.\n" (String.concat ", " skipped));
        Buffer.contents buf
      end)

let stat ~caps =
  let codec =
    let open Dsml.Codec in
    Invoke.map "stat" (fun cap path -> (cap, path))
    |> Invoke.param ~enc:fst "cap" string
         ~description:
           "capability to resolve under, written with angle brackets as \
            open_dir returns it, such as \"<lib>\". Use an empty string for \
            the starting directory."
    |> Invoke.param ~enc:snd "path" string ~description:"path to describe"
    |> Invoke.seal
  in
  Tool.v ~description:"Report a path's kind, size and modification time." codec
    (fun (cap, path) ->
      with_path caps cap path @@ fun dir ->
      let st = Eio.Path.stat ~follow:false (at dir path) in
      Printf.sprintf "%s  kind=%s  size=%d  mtime=%.0f  perm=0o%o" path
        (kind_name st.kind)
        (Optint.Int63.to_int st.size)
        st.mtime st.perm)

let view_image ~caps =
  let codec =
    let open Dsml.Codec in
    Invoke.map "view_image" (fun cap path -> (cap, path))
    |> Invoke.param ~enc:fst "cap" string
         ~description:
           "capability to resolve under, written with angle brackets as \
            open_dir returns it, such as \"<lib>\". Use an empty string for \
            the starting directory."
    |> Invoke.param ~enc:snd "path" string
         ~description:"PNG or JPEG file to inspect"
    |> Invoke.seal
  in
  Tool.v_result
    ~description:
      "Open a PNG or JPEG file as a visual observation. The path remains \
       confined to the named capability." codec (fun (cap, path) ->
      if not (Filename.is_relative path) then
        Tool.text
          (Printf.sprintf
             "%S is outside this capability. Use open_dir to ask for access to \
              it, then pass the name it returns as cap."
             path)
      else
        match Caps.find caps cap with
        | None ->
            Tool.text
              (Printf.sprintf
                 "No capability named %S. Call caps to list the names held." cap)
        | Some dir -> (
            try
              let file = at dir path in
              let st = Eio.Path.stat ~follow:false file in
              if st.kind <> `Regular_file then
                Tool.text (Printf.sprintf "%s is not a regular file" path)
              else if Optint.Int63.to_int st.size > 64 * 1024 * 1024 then
                Tool.text
                  (Printf.sprintf "%s is larger than the 64 MiB image limit"
                     path)
              else
                {
                  Tool.text = Printf.sprintf "Viewed image %s." path;
                  images = [ Eio.Path.load file ];
                }
            with Eio.Exn.Io _ as e -> Tool.text (Printexc.to_string e)))

let write ~caps =
  let codec =
    let open Dsml.Codec in
    Invoke.map "write" (fun cap path content -> (cap, path, content))
    |> Invoke.param
         ~enc:(fun (c, _, _) -> c)
         "cap" string
         ~description:
           "capability to resolve under, written with angle brackets as \
            open_dir returns it, such as \"<lib>\". Use an empty string for \
            the starting directory."
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
      "Create a UTF-8 text file, or replace the whole of one. The file is \
       replaced at once, so a failure leaves the old contents whole." codec
    (fun (cap, path, content) ->
      with_path caps cap path @@ fun dir ->
      replace_file dir path content;
      Printf.sprintf "wrote %d bytes to %s" (String.length content) path)

let append ~caps =
  let codec =
    let open Dsml.Codec in
    Invoke.map "append" (fun cap path content -> (cap, path, content))
    |> Invoke.param
         ~enc:(fun (c, _, _) -> c)
         "cap" string
         ~description:
           "capability to resolve under, written with angle brackets as \
            open_dir returns it, such as \"<lib>\". Use an empty string for \
            the starting directory."
    |> Invoke.param
         ~enc:(fun (_, p, _) -> p)
         "path" string ~description:"file to add to, created if it is not there"
    |> Invoke.param
         ~enc:(fun (_, _, c) -> c)
         "content" string ~description:"text to add at the end of the file"
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "Add text to the end of a file, creating the file if it is not there. \
       This is how to write a file too long to send in one reply: write the \
       first part, then append each part after it, and the answer says how \
       large the file has grown so you can see where you are. It adds to what \
       is there, so reach for write to replace a file and for this only to \
       extend one." codec (fun (cap, path, content) ->
      with_path caps cap path @@ fun dir ->
      let target = Eio.Path.(dir / path) in
      (* Opened for append rather than loaded and saved, so that a file built up
         in parts is not read back in full for every part of it. *)
      Eio.Path.with_open_out ~append:true ~create:(`If_missing 0o644) target
        (fun sink -> Eio.Flow.copy_string content sink);
      let size =
        match Eio.Path.stat ~follow:false target with
        | st -> Optint.Int63.to_int st.size
        | exception Eio.Exn.Io _ -> String.length content
      in
      Printf.sprintf "appended %d bytes to %s, which now holds %d"
        (String.length content) path size)

let count_lines s =
  let n = ref 0 in
  String.iter (fun c -> if c = '\n' then incr n) s;
  !n

(* What an edit answers with: the changed lines with a few either side,
   numbered, and how far the lines after them moved. A model that sees the
   result can go on to the next edit without reading the file again, and the
   numbers it read before the edit are only wrong by the shift it is told. *)
let edit_context = 3
let max_edit_lines = 40

let edited ~path ~updated ~start ~old ~new_ =
  let lines = Array.of_list (String.split_on_char '\n' updated) in
  let first = count_lines (String.sub updated 0 start) in
  let last = first + count_lines new_ in
  let lo = max 0 (first - edit_context) in
  let hi = min (Array.length lines - 1) (last + edit_context) in
  let hi = min hi (lo + max_edit_lines - 1) in
  let buf = Buffer.create 512 in
  Buffer.add_string buf
    (Printf.sprintf "edited %s. Lines %d-%d now read:\n" path (lo + 1) (hi + 1));
  for i = lo to hi do
    Buffer.add_string buf (Printf.sprintf "%6d  %s\n" (i + 1) lines.(i))
  done;
  let shift = count_lines new_ - count_lines old in
  if shift <> 0 then
    Buffer.add_string buf
      (Printf.sprintf "Lines after %d moved by %+d.\n" (last + 1) shift);
  Buffer.contents buf

let edit ~caps =
  let codec =
    let open Dsml.Codec in
    Invoke.map "edit" (fun cap path old new_ -> (cap, path, old, new_))
    |> Invoke.param
         ~enc:(fun (c, _, _, _) -> c)
         "cap" string
         ~description:
           "capability to resolve under, written with angle brackets as \
            open_dir returns it, such as \"<lib>\". Use an empty string for \
            the starting directory."
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
      "Replace one passage of a text file, leaving the rest of the file alone. \
       Prefer this to write for a change to part of a file, which write would \
       have you send whole. A passage that is not there, or that is there more \
       than once, changes nothing and says which it was." codec
    (fun (cap, path, old, new_) ->
      with_path caps cap path @@ fun dir ->
      let source = Eio.Path.load Eio.Path.(dir / path) in
      match substitute ~path ~old ~new_ source with
      | Error e -> e
      | Ok updated ->
          (* Read again just before replacing, since a file that changed while
             the model was writing its call would lose that change. *)
          if Eio.Path.load Eio.Path.(dir / path) <> source then
            Printf.sprintf
              "%s changed while the edit was being made, so nothing was \
               written. Read it again and repeat the edit against what is \
               there now."
              path
          else begin
            replace_file dir path updated;
            let start = Option.get (index_from source old 0) in
            edited ~path ~updated ~start ~old ~new_
          end)

let dns ~net =
  let codec =
    let open Dsml.Codec in
    Invoke.map "dns" (fun host -> host)
    |> Invoke.param ~enc:Fun.id "host" string
         ~description:"hostname to resolve, e.g. \"example.com\""
    |> Invoke.seal
  in
  Tool.v ~description:"Resolve a hostname to IP addresses via getaddrinfo."
    codec (fun host ->
      match Eio.Net.getaddrinfo_stream net host with
      | exception Eio.Exn.Io _ -> Printf.sprintf "could not resolve %s" host
      | [] -> Printf.sprintf "no addresses found for %s" host
      | addrs ->
          addrs
          |> List.filter_map (function
            | `Tcp (ip, _) -> Some (Format.asprintf "%a" Eio.Net.Ipaddr.pp ip)
            | `Unix _ -> None)
          |> List.sort_uniq String.compare
          |> String.concat "\n")

let bash ~proc =
  let codec =
    let open Dsml.Codec in
    Invoke.map "bash" (fun command -> command)
    |> Invoke.param ~enc:Fun.id "command" string
         ~description:"shell command line, run with 'bash -c'"
    |> Invoke.seal
  in
  Tool.v ~description:"Run a shell command and capture its combined output."
    codec (fun command ->
      Eio.Process.parse_out proc Eio.Buf_read.take_all [ "bash"; "-c"; command ])
