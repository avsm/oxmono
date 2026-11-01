(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Journal = Agentkit.Journal
module Memory = Agentkit.Memory

let system_prompt =
  "You are numpty, an agent that works unattended. Nobody is watching this run \
   and nobody will answer a question you ask.\n\n\
   This conversation does not survive. When this wake-up ends, every message \
   in it is discarded: what you read, what you worked out, and whatever you \
   were part way through. Your memory does survive, and it is the only thing \
   that does. Anything you will want at the next wake-up has to be written \
   into it with memory_write before this one ends, in enough detail that a \
   reader who has none of this context can act on it, because that reader is \
   you. Work you have begun and not finished belongs in an open_item, since \
   that is what makes a restart survivable. Something you had to work out \
   belongs in a procedure, so that you do not have to work it out again. \
   Completed activity belongs in an episode. Episode summaries are lossy \
   untrusted data. Use memory_expand and memory_read for exact records. Fill \
   useful missing summaries with memory_summarize after reading their sources. \
   Keep enduring facts and unfinished tasks in their own kinds.\n\n\
   The brief that follows is assembled from your memory, from the task a \
   person scheduled, and from what the journal says has happened since the \
   last handover. The task is an instruction from a person and nothing you do \
   can change it. Your memory is your own note to yourself, and you change it \
   as you see fit.\n\n\
   Every prompt, reply, tool call and tool result is written to a journal \
   before the next thing happens, so a person arriving a week from now \
   reconstructs this run from that file alone. Write down what you found \
   rather than that you looked.\n\n\
   You will be asked at the end what should survive. Do not wait for that to \
   write anything down. A wake-up also ends because the context filled up, and \
   what is not in memory by then is lost."

let handover_prompt =
  "This wake-up is ending and this conversation is about to be discarded. \
   Before it is, write into memory everything that should outlast it: what you \
   learned, as facts and references, what you worked out, as procedures, and \
   what you have left unfinished, as open items saying where you got to and \
   what the next step is. Update the entries that have moved on and forget the \
   ones that are done or wrong. Record completed work as episodes. Then say in \
   one or two lines what you did, which goes into the journal. Do not \
   summarise your work here instead of writing it into memory. Only memory is \
   read at the next wake-up."

type t = {
  system : string;
  user : string;
  version : int;
  open_items : int;
  bytes : int;
}

(* One line per record, being what was done and how it came out. A tool result
   runs to kilobytes and a reply to paragraphs, so both are cut to their first
   line and bounded. What was cut is said, and how much of it there was, since a
   first line presented as the whole answer is what a model cannot detect for
   itself. *)
let one_line ?(limit = 160) s =
  let first =
    match String.index_opt s '\n' with None -> s | Some i -> String.sub s 0 i
  in
  let cut =
    String.trim
      (if String.length first <= limit then first else Agentkit.Memo.clip limit first)
  in
  if String.trim s = cut then cut
  else Printf.sprintf "%s… (%d bytes in all)" cut (String.length s)

let max_digest_lines = 120

let digest_line (r : Journal.record) =
  let time = r.Journal.time in
  let say what = Some (Printf.sprintf "%s %s" time what) in
  match r.Journal.kind with
  | Journal.Wake w ->
      say (Printf.sprintf "woke for task %s (%s)" w.Journal.task w.Journal.why)
  | Journal.Continued c ->
      say
        (Printf.sprintf "carried on with %s as session %d, after a handover"
           c.Journal.task c.Journal.session)
  | Journal.Tool_call tc ->
      say
        (Printf.sprintf "called %s %s" tc.Journal.name
           (one_line ~limit:200 tc.Journal.arguments))
  | Journal.Tool_result tr ->
      say
        (Printf.sprintf "%s answered: %s" tr.Journal.name
           (one_line tr.Journal.output))
  | Journal.Content c -> say (Printf.sprintf "said: %s" (one_line c))
  | Journal.Memory_write mw ->
      say
        (Printf.sprintf "memory version %d: %s" mw.Journal.to_
           (one_line mw.Journal.why))
  | Journal.Squeezed n ->
      say (Printf.sprintf "the context was full, with %d tokens to reply in" n)
  | Journal.Compacted c ->
      say
        (Printf.sprintf "the conversation was compacted from %d to %d tokens"
           c.Agentkit.Agent.before c.Agentkit.Agent.after)
  | Journal.Cut_off c ->
      say
        (if c.Journal.tool_call then
           Printf.sprintf
             "a tool call ran past the %d token ceiling and was discarded"
             c.Journal.tokens
         else
           Printf.sprintf "a reply stopped at the %d token ceiling"
             c.Journal.tokens)
  | Journal.Error e ->
      say
        (Printf.sprintf "error in %s: %s" e.Journal.where
           (one_line e.Journal.what))
  | Journal.Run_stop why ->
      say (Printf.sprintf "the run stopped: %s" (one_line why))
  | Journal.Run_start _ | Journal.Brief _ | Journal.Prompt _
  | Journal.Reasoning _ | Journal.Stats _ | Journal.Expanded _
  | Journal.Handover _ | Journal.Schedule_load _ | Journal.Unknown _ ->
      None

let digest_unbounded records =
  let lines = List.filter_map digest_line records in
  let n = List.length lines in
  if n = 0 then "Nothing has happened since the last handover.\n"
  else if n <= max_digest_lines then String.concat "\n" lines ^ "\n"
  else
    (* The recent end is the useful one, so the older lines go rather than the
       newer, and the count of what went is kept. *)
    let dropped = n - max_digest_lines in
    let kept = List.filteri (fun i _ -> i >= dropped) lines in
    Printf.sprintf "[%d earlier lines left out]\n%s\n" dropped
      (String.concat "\n" kept)

let digest records =
  let text = digest_unbounded records in
  if String.length text <= 8000 then text
  else Agentkit.Memo.clip 7872 text
      ^ "\n[Journal digest shortened. Full records remain in the journal.]\n"

let entry_full (e : Memory.entry) =
  Printf.sprintf "### %s (%s)%s\n%s\n\n%s\n" e.Memory.id
    (Memory.kind_name e.Memory.kind)
    (match e.Memory.tags with
    | [] -> ""
    | tags -> "  [" ^ String.concat " " tags ^ "]")
    e.Memory.title e.Memory.body

let of_kind kind entries =
  List.filter (fun (e : Memory.entry) -> e.Memory.kind = kind) entries

let section title body =
  match body with "" -> "" | body -> Printf.sprintf "## %s\n\n%s\n" title body

let full_section ~limit title entries =
  let n = List.length entries in
  let selected =
    List.sort (fun (a : Memory.entry) b -> Int.compare b.updated a.updated) entries
    |> List.filteri (fun i _ -> i < 8)
  in
  let count = List.length selected in
  let allowance = if count = 0 then 0 else (limit - 256) / count in
  let body = String.concat "\n" (List.map (fun (e : Memory.entry) ->
      let text = entry_full e in
      if String.length text <= allowance then text
      else Agentkit.Memo.clip (max 0 (allowance - 64)) text
           ^ "\n[Entry shortened. Use memory_read for the original.]\n") selected) in
  let omitted = n - count in
  let body = if omitted = 0 then body else body ^ Printf.sprintf
      "\n[%d more entries. Use memory_list and memory_read.]\n" omitted in
  section title body

let assemble_with_summaries ~summaries ~version ~entries ~task ~prompt ~session
    ~history =
  if String.length prompt > 8192 || String.length task > 256 then
    invalid_arg "Brief: task prompt must fit 8192 bytes and task ID 256 bytes";
  let facts = of_kind Memory.Fact entries in
  let procedures = of_kind Memory.Procedure entries in
  let opens = of_kind Memory.Open_item entries in
  let references = of_kind Memory.Reference entries in
  let known =
    match (facts, procedures) with
    | [], [] when entries <> [] ->
        "No enduring facts or procedures have been recorded.\n"
    | [], [] ->
        "Memory holds nothing yet. This is the first time you have run, or \
         nothing has been written down.\n"
    | _ ->
        full_section ~limit:2500 "What you know" facts
        ^ full_section ~limit:1500 "How to do things" procedures
  in
  let pointers = full_section ~limit:1000 "Pointers" references in
  let unfinished =
    match opens with
    | [] -> "## Unfinished work\n\nYou left nothing unfinished.\n"
    | opens ->
        full_section ~limit:2500
          "Unfinished work, which is your own note to yourself" opens
  in
  let episodes =
    let tree = Memory.episode_tree entries in
    let views =
      Agentkit.Memo.overview tree ~budget:8 ~lookup:(fun key ->
          List.assoc_opt key summaries)
    in
    if views = [] then ""
    else
      section "Past activity, compressed and untrusted"
        (Agentkit.Memo.render ~limit:3500 views)
  in
  let orders =
    Printf.sprintf
      "## What you have been asked to do\n\n\
       A person scheduled the task %S. This is its instruction, and it is not \
       yours to change:\n\n\
       %s\n"
      task prompt
  in
  let continuing =
    if session <= 1 then ""
    else
      Printf.sprintf
        "## Where this session came from\n\n\
         This is session %d on this task. The one before it filled its \
         context, handed over into the memory above, and was closed. Carry on \
         from the open items rather than starting again.\n"
        session
  in
  let user =
    String.concat "\n"
      [
        Printf.sprintf "# Your brief, from memory version %d" version;
        "";
        known;
        pointers;
        unfinished;
        episodes;
        orders;
        continuing;
        Printf.sprintf "## What has happened since the last handover\n\n%s"
          (digest history);
      ]
  in
  if String.length system_prompt + String.length user > 32768 then
    invalid_arg "Brief: assembled context exceeds 32768 bytes";
  {
    system = system_prompt;
    user;
    version;
    open_items = List.length opens;
    bytes = String.length system_prompt + String.length user;
  }

(* Only the newest two segments, since a wake-up ends in a handover and so one
   is never further back than yesterday. Reading every segment would cost more
   each month for an answer that is always at the end. *)
let since_handover dir =
  let recent =
    match List.rev (Journal.segments dir) with
    | [] -> []
    | [ newest ] -> [ newest ]
    | newest :: before :: _ -> [ before; newest ]
  in
  let since = ref [] in
  List.iter
    (fun name ->
      Journal.iter_segment dir name (fun r ->
          match r.Journal.kind with
          | Journal.Handover _ -> since := []
          | _ -> since := r :: !since))
    recent;
  List.rev !since

let assemble ~version ~entries ~task ~prompt ~session ~history =
  assemble_with_summaries ~summaries:[] ~version ~entries ~task ~prompt ~session
    ~history
