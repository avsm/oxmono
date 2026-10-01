(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* agentkit: browse the journals the agents keep.

   humpty, numpty and dumpty each append their account to a journal of their
   own, under the same codec. This command holds no model and reads them all:
   [list] says which journals exist and what they hold, [runs] gives one line
   per run, [log] prints records through the same renderer the agents use, and
   [show] lays one run out as the conversation it was. *)

module Journal = Agentkit.Journal
module Show = Agentkit.Show
module Utc = Agentkit.Utc

let version = "0.1"

(* The known journals, each under its agent's state directory. A source that
   is not an agent's name is taken as the path of a journal directory, which
   is how a store copied off a machine is read. *)
let agents = [ "humpty"; "numpty"; "dumpty" ]

let journal_of ~fs = function
  | "humpty" -> Eio.Path.(Xdge.state_dir (Xdge.create fs "ds4") / "journal")
  | "numpty" -> Eio.Path.(Xdge.state_dir (Xdge.create fs "numpty") / "journal")
  | "dumpty" -> Eio.Path.(Xdge.state_dir (Xdge.create fs "dumpty") / "journal")
  | path -> Eio.Path.(fs / path)

let native p = Option.value ~default:"?" (Eio.Path.native p)

(* Colour, for a terminal alone. The stream form of every command is the same
   text without it, so a pipe reads what a person read. *)
let tty = Unix.isatty Unix.stdout
let style code s = if tty then Printf.sprintf "\027[%sm%s\027[0m" code s else s
let bold = style "1"
let dim = style "2"
let cyan = style "36"
let red = style "31"
let green = style "32"

let run f =
  try Ok (Eio_main.run @@ fun env -> f env) with
  | Failure message -> Error message
  | exn -> Error (Printexc.to_string exn)

(* ---- what the journals hold --------------------------------------------- *)

type survey = {
  records : int;
  runs : int;
  first : string option;
  last : string option;
}

let survey dir =
  let records = ref 0 and runs = ref 0 in
  let first = ref None and last = ref None in
  Journal.iter dir (fun (r : Journal.record) ->
      incr records;
      (match r.Journal.kind with Journal.Run_start _ -> incr runs | _ -> ());
      if !first = None then first := Some r.Journal.time;
      last := Some r.Journal.time);
  { records = !records; runs = !runs; first = !first; last = !last }

let list () =
  run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  Printf.printf "%-8s %8s %5s  %-20s %-20s %s\n" "AGENT" "RECORDS" "RUNS"
    "FIRST" "LAST" "JOURNAL";
  List.iter
    (fun agent ->
      let dir = journal_of ~fs agent in
      if Journal.segments dir = [] then
        Printf.printf "%-8s %s\n" agent
          (dim (Printf.sprintf "nothing recorded at %s" (native dir)))
      else
        let s = survey dir in
        Printf.printf "%-8s %8d %5d  %-20s %-20s %s\n" agent s.records s.runs
          (Option.value ~default:"-" s.first)
          (Option.value ~default:"-" s.last)
          (native dir))
    agents

(* ---- runs ---------------------------------------------------------------- *)

type acc = {
  mutable started : string;
  mutable model : string;
  mutable stopped : string option;  (** the run_stop reason *)
  mutable stop_time : string option;
  mutable calls : int;
  mutable turns : int;
}

(* One line per run, oldest first. The order of first appearance is the order
   of the runs, since a journal is append only. *)
let scan_runs dir =
  let order = ref [] in
  let by_run = Hashtbl.create 16 in
  Journal.iter dir (fun (r : Journal.record) ->
      let acc =
        match Hashtbl.find_opt by_run r.Journal.run with
        | Some a -> a
        | None ->
            let a =
              {
                started = r.Journal.time;
                model = "?";
                stopped = None;
                stop_time = None;
                calls = 0;
                turns = 0;
              }
            in
            Hashtbl.add by_run r.Journal.run a;
            order := r.Journal.run :: !order;
            a
      in
      match r.Journal.kind with
      | Journal.Run_start rs -> acc.model <- Filename.basename rs.Journal.model
      | Journal.Run_stop why ->
          acc.stopped <- Some why;
          acc.stop_time <- Some r.Journal.time
      | Journal.Tool_call _ -> acc.calls <- acc.calls + 1
      | Journal.Stats s -> acc.turns <- s.Agentkit.Agent.turns
      | _ -> ())
  |> ignore;
  List.rev_map (fun run -> (run, Hashtbl.find by_run run)) !order

let duration a =
  match (Utc.of_rfc3339 a.started, Option.map Utc.of_rfc3339 a.stop_time) with
  | Some t0, Some (Some t1) ->
      let s = int_of_float (t1 -. t0) in
      if s >= 3600 then Printf.sprintf "%dh%02dm" (s / 3600) (s mod 3600 / 60)
      else if s >= 60 then Printf.sprintf "%dm%02ds" (s / 60) (s mod 60)
      else Printf.sprintf "%ds" s
  | _ -> "-"

let print_runs agent dir =
  match scan_runs dir with
  | [] ->
      Printf.printf "%s\n"
        (dim (Printf.sprintf "%s: nothing recorded at %s" agent (native dir)))
  | runs ->
      List.iter
        (fun (run, a) ->
          let stop =
            match a.stopped with
            | Some why -> Show.brief ~limit:60 why
            | None -> red "no run_stop: still going, or it never got to say"
          in
          Printf.printf "%-8s %4d  %s  %7s %5d %5d  %-30s %s\n" agent run
            a.started (duration a) a.turns a.calls a.model stop)
        runs

let runs source =
  run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  Printf.printf "%-8s %4s  %-20s %7s %5s %5s  %-30s %s\n" "AGENT" "RUN"
    "STARTED" "TOOK" "TURNS" "CALLS" "MODEL" "STOPPED";
  match source with
  | Some s -> print_runs s (journal_of ~fs s)
  | None -> List.iter (fun a -> print_runs a (journal_of ~fs a)) agents

(* ---- log ----------------------------------------------------------------- *)

let parse_since = function
  | None -> None
  | Some s -> Some (Result.fold ~ok:Fun.id ~error:failwith (Utc.of_since s))

let log source since kinds run_opt =
  run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let since = parse_since since in
  let kinds = match kinds with [] -> None | k -> Some k in
  Option.iter Show.check_kinds kinds;
  let one agent =
    let dir = journal_of ~fs agent in
    if Journal.segments dir = [] then
      Printf.printf "%s\n"
        (dim (Printf.sprintf "%s: nothing recorded at %s" agent (native dir)))
    else begin
      Printf.printf "%s\n" (bold (Printf.sprintf "%s — %s" agent (native dir)));
      Show.log ~agent ?since ?kinds ?run:run_opt dir print_endline
    end
  in
  match source with Some s -> one s | None -> List.iter one agents

(* ---- show ---------------------------------------------------------------- *)

(* One run laid out as the conversation it was: the prompts and replies whole,
   the reasoning dimmed, the tool traffic marked and cut, and what the run
   cost at the foot. The record stream carries everything shown here, so this
   is an arrangement of the account rather than another account. *)
let show_run source run_n full =
  run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = journal_of ~fs source in
  let cut s = if full then s else Show.brief ~limit:200 s in
  let seen = ref false in
  Journal.iter dir (fun (r : Journal.record) ->
      if r.Journal.run = run_n then begin
        seen := true;
        match r.Journal.kind with
        | Journal.Run_start rs ->
            Printf.printf "%s\n"
              (bold
                 (Printf.sprintf "run %d of %s — %s, pid %d, %s, ctx %d" run_n
                    source r.Journal.time rs.Journal.pid rs.Journal.backend
                    rs.Journal.ctx_size));
            Printf.printf "%s\n\n" (dim rs.Journal.model)
        | Journal.Run_stop why ->
            Printf.printf "\n%s %s\n" (green "stopped:") why
        | Journal.Prompt t -> Printf.printf "%s %s\n" (bold ">") t
        | Journal.Reasoning t -> Printf.printf "%s\n" (dim (cut t))
        | Journal.Content t -> Printf.printf "%s\n" t
        | Journal.Tool_call tc ->
            Printf.printf "%s\n"
              (cyan
                 (Printf.sprintf "→ #%d %s %s" tc.Journal.call tc.Journal.name
                    (cut tc.Journal.arguments)))
        | Journal.Tool_result tr ->
            Printf.printf "%s\n"
              (cyan
                 (Printf.sprintf "← #%d %s (%.1fs)%s %s" tr.Journal.call
                    tr.Journal.name tr.Journal.seconds
                    (if tr.Journal.truncated then " truncated" else "")
                    (cut tr.Journal.output)))
        | Journal.Error e ->
            Printf.printf "%s\n"
              (red
                 (Printf.sprintf "error in %s: %s" e.Journal.where
                    e.Journal.what))
        | Journal.Stats _ when not full -> ()
        | k -> Printf.printf "%s\n" (dim (Show.summary ~agent:source k))
      end);
  if not !seen then
    failwith
      (Printf.sprintf
         "the journal at %s holds no run %d. 'agentkit runs %s' lists them"
         (native dir) run_n source)

(* ---- cmdliner ------------------------------------------------------------ *)

open Cmdliner

let source_doc =
  "$(docv) is $(b,humpty), $(b,numpty) or $(b,dumpty), or the path of a \
   journal directory, which is how a store copied off another machine is read."

let source_opt =
  Arg.(
    value & pos 0 (some string) None & info [] ~docv:"SOURCE" ~doc:source_doc)

let source_req =
  Arg.(
    required & pos 0 (some string) None & info [] ~docv:"SOURCE" ~doc:source_doc)

let list_cmd =
  let doc = "List the journals the agents keep and what each holds." in
  Cmd.v (Cmd.info "list" ~doc) Term.(const list $ const ())

let runs_cmd =
  let doc = "One line per run: when it started, what it cost, how it ended." in
  Cmd.v (Cmd.info "runs" ~doc) Term.(const runs $ source_opt)

let since_arg =
  Arg.(
    value
    & opt (some string) None
    & info [ "since" ] ~docv:"WHEN"
        ~doc:
          "Show records from $(docv) on: a timestamp such as \
           2026-08-08T09:14:07Z, a date, which is midnight UTC on it, or how \
           far back to go, such as 2h or 7d.")

let kind_arg =
  Arg.(
    value & opt_all string []
    & info [ "kind" ] ~docv:"KIND"
        ~doc:"Show records of $(docv) alone. Repeatable.")

let run_arg =
  Arg.(
    value
    & opt (some int) None
    & info [ "run" ] ~docv:"N" ~doc:"Show the records of run $(docv) alone.")

let log_cmd =
  let doc = "Print journal records, every agent's or one source's." in
  Cmd.v (Cmd.info "log" ~doc)
    Term.(const log $ source_opt $ since_arg $ kind_arg $ run_arg)

let full_arg =
  Arg.(
    value & flag
    & info [ "full" ]
        ~doc:
          "Show reasoning and tool traffic whole rather than cut to a line, \
           and the statistics of every turn.")

let run_pos =
  Arg.(
    required
    & pos 1 (some int) None
    & info [] ~docv:"RUN" ~doc:"The run to show. 'agentkit runs' lists them.")

let show_cmd =
  let doc = "Lay one run out as the conversation it was." in
  Cmd.v (Cmd.info "show" ~doc)
    Term.(const show_run $ source_req $ run_pos $ full_arg)

let () =
  let doc = "Browse the journals the agents keep." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Every agent in this repository records what it did into an \
         append-only journal: $(b,humpty) each interactive session, \
         $(b,numpty) each unattended run, and $(b,dumpty) each one-shot \
         exchange. The records share one codec, and this command reads them \
         all without loading a model.";
      `P
        "$(b,list) says which journals exist and what they hold. $(b,runs) \
         gives one line per run. $(b,log) prints records, filtered by \
         $(b,--since), $(b,--kind) and $(b,--run). $(b,show) lays one run out \
         as the conversation it was.";
    ]
  in
  let info = Cmd.info "agentkit" ~version ~doc ~man in
  exit
    (Cmd.eval_result (Cmd.group info [ list_cmd; runs_cmd; log_cmd; show_cmd ]))
