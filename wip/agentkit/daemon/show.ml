(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Journal = Agentkit.Journal
module Memory = Agentkit.Memory

(* The rendering is Agentkit.Show's, so a record reads the same here as under
   any other command that prints a journal. *)
let record_line = Agentkit.Show.record_line ~agent:"numpty"
let brief = Agentkit.Show.brief

let log ?since ?kinds ?task ?run dir emit =
  (* The task filter needs the [wake] records to know which wake-up a record
     belongs to, so the kind filter is applied here rather than by the reader. *)
  let current = ref None in
  Journal.iter dir (fun (r : Journal.record) ->
      (match r.Journal.kind with
      | Journal.Wake w -> current := Some w.Journal.task
      | _ -> ());
      let keep =
        Agentkit.Show.keep ?since ?run r
        && (match task with None -> true | Some id -> !current = Some id)
        &&
        match kinds with
        | None -> true
        | Some ks -> List.mem (Journal.kind_name r.Journal.kind) ks
      in
      if keep then emit (record_line r))

(* Memory. *)

let entry_text (e : Memory.entry) =
  Printf.sprintf "## %s (%s)%s\ncreated %d, updated %d\n%s\n\n%s\n" e.Memory.id
    (Memory.kind_name e.Memory.kind)
    (match e.Memory.tags with
    | [] -> ""
    | tags -> "  [" ^ String.concat " " tags ^ "]")
    e.Memory.created e.Memory.updated e.Memory.title e.Memory.body

let snapshot_text (s : Memory.snapshot) =
  Printf.sprintf
    "memory version %d, after version %d, written %s from journal record %d\n\
     cause: %s\n\n\
     %s"
    s.Memory.version s.Memory.parent s.Memory.time s.Memory.seq s.Memory.cause
    (match s.Memory.entries with
    | [] -> "It holds no entries.\n"
    | entries -> String.concat "\n" (List.map entry_text entries))

let show m ~at =
  match at with
  | Some v -> snapshot_text (Memory.read m v)
  | None -> (
      match Memory.read_current m with
      | Some s -> snapshot_text s
      | None ->
          "Memory holds nothing. No version has been written, so no wake-up \
           has written anything down.\n")

let history m =
  match Memory.versions m with
  | [] -> "Memory holds no versions.\n"
  | versions ->
      String.concat ""
        (List.map
           (fun v ->
             let s = Memory.read m v in
             Printf.sprintf "%06d  after %06d  %s  seq %-6d %d entr%s  %s\n"
               s.Memory.version s.Memory.parent s.Memory.time s.Memory.seq
               (List.length s.Memory.entries)
               (if List.length s.Memory.entries = 1 then "y" else "ies")
               (brief s.Memory.cause))
           versions)

let diff m v w =
  let entries x = (Memory.read m x).Memory.entries in
  let before = entries v and after = entries w in
  let find id es =
    List.find_opt (fun (e : Memory.entry) -> e.Memory.id = id) es
  in
  let ids es = List.map (fun (e : Memory.entry) -> e.Memory.id) es in
  let all = List.sort_uniq String.compare (ids before @ ids after) in
  let lines =
    List.filter_map
      (fun id ->
        match (find id before, find id after) with
        | None, Some e ->
            Some
              (Printf.sprintf "+ %s (%s) %s" id
                 (Memory.kind_name e.Memory.kind)
                 e.Memory.title)
        | Some e, None ->
            Some
              (Printf.sprintf "- %s (%s) %s" id
                 (Memory.kind_name e.Memory.kind)
                 e.Memory.title)
        | Some a, Some b when a <> b ->
            Some
              (Printf.sprintf "~ %s (%s) %s" id
                 (Memory.kind_name b.Memory.kind)
                 b.Memory.title)
        | _ -> None)
      all
  in
  match lines with
  | [] -> Printf.sprintf "Versions %d and %d hold the same entries.\n" v w
  | lines ->
      Printf.sprintf "From version %d to version %d:\n%s" v w
        (String.concat "\n" lines ^ "\n")
