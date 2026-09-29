(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Line = Agentkit.Line

type job = {
  task : string;
  started : string;
  session : int;
  ctx_used : int;
  ctx_size : int;
  turns : int;
  tool_calls : int;
  tool : string option;
  prefilled : int;
  prefill_total : int;
}

type due = { task : string; next : string option; waiting : bool }

type t = {
  run : int;
  since : string;
  model : string;
  backend : string;
  netd : string;
  version : int;
  job : job option;
  tasks : due list;
}

type running = {
  r_run : int;
  r_since : string;
  r_model : string;
  r_backend : string;
  r_netd : string;
  r_version : int;
  r_job : string option;
}

type jobs = { j_job : job option; j_tasks : due list }
type memory = { m_version : int; m_text : string }
type lines = { lines : string list }

let running (t : t) =
  {
    r_run = t.run;
    r_since = t.since;
    r_model = t.model;
    r_backend = t.backend;
    r_netd = t.netd;
    r_version = t.version;
    r_job = Option.map (fun (j : job) -> j.task) t.job;
  }

let jobs (t : t) = { j_job = t.job; j_tasks = t.tasks }

(* A model path and a fault message are bytes this process did not choose, so
   every string is scrubbed on the way out for the reason the journal's is. *)
let string = Jsont.map ~dec:Fun.id ~enc:Line.utf_8 Jsont.string

let job_jsont =
  Jsont.Object.map ~kind:"job"
    (fun
      task
      started
      session
      ctx_used
      ctx_size
      turns
      tool_calls
      tool
      prefilled
      prefill_total
      :
      job
    ->
      {
        task;
        started;
        session;
        ctx_used;
        ctx_size;
        turns;
        tool_calls;
        tool;
        prefilled;
        prefill_total;
      })
  |> Jsont.Object.mem "task" string ~enc:(fun (j : job) -> j.task)
  |> Jsont.Object.mem "started" string ~enc:(fun (j : job) -> j.started)
  |> Jsont.Object.mem "session" Jsont.int ~enc:(fun (j : job) -> j.session)
  |> Jsont.Object.mem "ctx_used" Jsont.int ~enc:(fun (j : job) -> j.ctx_used)
  |> Jsont.Object.mem "ctx_size" Jsont.int ~enc:(fun (j : job) -> j.ctx_size)
  |> Jsont.Object.mem "turns" Jsont.int ~enc:(fun (j : job) -> j.turns)
  |> Jsont.Object.mem "tool_calls" Jsont.int ~enc:(fun (j : job) ->
      j.tool_calls)
  |> Jsont.Object.opt_mem "tool" string ~enc:(fun (j : job) -> j.tool)
  |> Jsont.Object.mem "prefilled" Jsont.int ~enc:(fun (j : job) -> j.prefilled)
  |> Jsont.Object.mem "prefill_total" Jsont.int ~enc:(fun (j : job) ->
      j.prefill_total)
  |> Jsont.Object.finish

let due_jsont =
  Jsont.Object.map ~kind:"due" (fun task next waiting : due ->
      { task; next; waiting })
  |> Jsont.Object.mem "task" string ~enc:(fun (d : due) -> d.task)
  |> Jsont.Object.opt_mem "next" string ~enc:(fun (d : due) -> d.next)
  |> Jsont.Object.mem "waiting" Jsont.bool ~enc:(fun (d : due) -> d.waiting)
  |> Jsont.Object.finish

let running_jsont =
  Jsont.Object.map ~kind:"status"
    (fun r_run r_since r_model r_backend r_netd r_version r_job : running ->
      { r_run; r_since; r_model; r_backend; r_netd; r_version; r_job })
  |> Jsont.Object.mem "run" Jsont.int ~enc:(fun (r : running) -> r.r_run)
  |> Jsont.Object.mem "since" string ~enc:(fun (r : running) -> r.r_since)
  |> Jsont.Object.mem "model" string ~enc:(fun (r : running) -> r.r_model)
  |> Jsont.Object.mem "backend" string ~enc:(fun (r : running) -> r.r_backend)
  |> Jsont.Object.mem "netd" string ~enc:(fun (r : running) -> r.r_netd)
  |> Jsont.Object.mem "version" Jsont.int ~enc:(fun (r : running) ->
      r.r_version)
  |> Jsont.Object.opt_mem "job" string ~enc:(fun (r : running) -> r.r_job)
  |> Jsont.Object.finish

let jobs_jsont =
  Jsont.Object.map ~kind:"jobs" (fun j_job j_tasks : jobs -> { j_job; j_tasks })
  |> Jsont.Object.opt_mem "job" job_jsont ~enc:(fun (j : jobs) -> j.j_job)
  |> Jsont.Object.mem "tasks" (Jsont.list due_jsont) ~enc:(fun (j : jobs) ->
      j.j_tasks)
  |> Jsont.Object.finish

let memory_jsont =
  Jsont.Object.map ~kind:"memory" (fun m_version m_text : memory ->
      { m_version; m_text })
  |> Jsont.Object.mem "version" Jsont.int ~enc:(fun (m : memory) -> m.m_version)
  |> Jsont.Object.mem "text" string ~enc:(fun (m : memory) -> m.m_text)
  |> Jsont.Object.finish

let lines_jsont =
  Jsont.Object.map ~kind:"lines" (fun lines : lines -> { lines })
  |> Jsont.Object.mem "lines" (Jsont.list string) ~enc:(fun (l : lines) ->
      l.lines)
  |> Jsont.Object.finish
