(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A log line is read by eye, so text is cut to one line. The full text of a
   record is in the file, which is one JSON object per line and the thing to
   read when a line here is not enough. *)
let brief ?(limit = 120) s =
  let first =
    match String.index_opt s '\n' with None -> s | Some i -> String.sub s 0 i
  in
  let cut =
    String.trim
      (if String.length first <= limit then first else String.sub first 0 limit)
  in
  if String.trim s = cut then cut
  else Printf.sprintf "%s… (%d bytes)" cut (String.length s)

let json_text json =
  match Jsont_bytesrw.encode_string Jsont.json json with
  | Ok s -> brief s
  | Error e -> "(could not be shown: " ^ e ^ ")"

let summary ?agent (k : Journal.kind) =
  match k with
  | Journal.Run_start r ->
      (* A record does not say which program wrote it. The directory it was
         read from does, which is what [agent] carries. *)
      Printf.sprintf "pid %d, %s%s on %s, ctx %d, %s" r.Journal.pid
        (match agent with Some a -> a ^ " " | None -> "v")
        r.Journal.version r.Journal.backend r.Journal.ctx_size r.Journal.model
  | Journal.Run_stop why -> brief why
  | Journal.Wake w ->
      Printf.sprintf "%s, due %s, %s%s" w.Journal.task w.Journal.due
        w.Journal.why
        (match w.Journal.serial with
        | None -> ""
        | Some n -> Printf.sprintf " serial %d" n)
  | Journal.Brief b ->
      Printf.sprintf "from memory version %d, %d open item%s, %d bytes"
        b.Journal.version b.Journal.open_items
        (if b.Journal.open_items = 1 then "" else "s")
        b.Journal.bytes
  | Journal.Prompt t | Journal.Reasoning t | Journal.Content t -> brief t
  | Journal.Tool_call tc ->
      Printf.sprintf "#%d %s %s" tc.Journal.call tc.Journal.name
        (brief tc.Journal.arguments)
  | Journal.Tool_result tr ->
      Printf.sprintf "#%d %s in %.1fs%s: %s" tr.Journal.call tr.Journal.name
        tr.Journal.seconds
        (if tr.Journal.truncated then " (truncated for the model)" else "")
        (brief tr.Journal.output)
  | Journal.Stats s ->
      Printf.sprintf "ctx %d/%d, %d turn%s, %d tool call%s, %d tokens in %.1fs"
        s.Agent.ctx_used s.Agent.ctx_size s.Agent.turns
        (if s.Agent.turns = 1 then "" else "s")
        s.Agent.tool_calls
        (if s.Agent.tool_calls = 1 then "" else "s")
        s.Agent.generated s.Agent.generate_seconds
  | Journal.Expanded n -> Printf.sprintf "the context grew to %d tokens" n
  | Journal.Squeezed n -> Printf.sprintf "only %d tokens to reply in" n
  | Journal.Cut_off c ->
      Printf.sprintf "the reply stopped at its %d token ceiling%s"
        c.Journal.tokens
        (if c.Journal.tool_call then ", discarding the tool call it was writing"
         else "")
  | Journal.Compacted c ->
      Printf.sprintf "%d to %d tokens: %s" c.Agent.before c.Agent.after
        (brief c.Agent.summary)
  | Journal.Continued c ->
      Printf.sprintf "%s, session %d after session %d" c.Journal.task
        c.Journal.session c.Journal.previous
  | Journal.Memory_write mw ->
      Printf.sprintf "%d to %d, %s: %s" mw.Journal.from mw.Journal.to_
        mw.Journal.entry (brief mw.Journal.why)
  | Journal.Handover v -> Printf.sprintf "at memory version %d" v
  | Journal.Error e ->
      Printf.sprintf "%s: %s" e.Journal.where (brief e.Journal.what)
  | Journal.Schedule_load s ->
      Printf.sprintf "read %s%s"
        (match s.Journal.tasks with
        | [] -> "no tasks"
        | tasks -> String.concat ", " tasks)
        (match s.Journal.changed with
        | [] -> ""
        | changed -> ", changed " ^ String.concat ", " changed)
  | Journal.Unknown u ->
      (* A kind this build does not know is shown as it was written. A reader
         asking what happened must be told even where this program cannot say
         what it means. *)
      json_text u.Journal.json

let record_line ?agent (r : Journal.record) =
  Printf.sprintf "%6d  %s  run %-4d %-14s %s" r.Journal.seq r.Journal.time
    r.Journal.run
    (Journal.kind_name r.Journal.kind)
    (summary ?agent r.Journal.kind)

let check_kinds kinds =
  match List.filter (fun k -> not (List.mem k Journal.kind_names)) kinds with
  | [] -> ()
  | unknown ->
      failwith
        (Printf.sprintf "no record has the kind %s. This build writes %s"
           (String.concat " or " (List.map (Printf.sprintf "%S") unknown))
           (String.concat ", " Journal.kind_names))

let keep ?since ?run (r : Journal.record) =
  (match since with
    | None -> true
    | Some t -> (
        match Utc.of_rfc3339 r.Journal.time with
        | Some rt -> rt >= t
        | None -> true))
  && match run with None -> true | Some n -> r.Journal.run = n

let log ?agent ?since ?kinds ?run dir emit =
  Option.iter check_kinds kinds;
  Journal.iter ?kinds dir (fun (r : Journal.record) ->
      if keep ?since ?run r then emit (record_line ?agent r))
