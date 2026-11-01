(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type metric = Spin | Count of int * int | Bytes of int64 * int64
type outcome = [ `Succeeded | `Failed | `Cancelled ]
type level = [ `App | `Error | `Warning | `Info | `Debug ]
type kind = Output | Logs of level
type line = { gap : float; kind : kind; text : string; seq : int }
type memo = ..

type scope = {
  current : unit -> t option;
  within : t -> (unit -> unit) -> unit;
  cancelled : exn -> bool;
}

and change =
  | Started of t
  | Progressed of t
  | Renamed of t
  | Line of t * line
  | Ended of t

and work = {
  now : unit -> float;
  start : float;
  summary : string option;
  scope : scope;
  by_key : (string, t) Hashtbl.t;
  mutable next_id : int;
  mutable next_line : int;
  mutable started : int;
  mutable finished : int;
  mutable failed : int;
  mutable cancelled : int;
  mutable count_cell : int;
  mutable rate_cell : int;
  mutable label_cells : int array;
  mutable watchers : (change -> unit) list;
  mutable ending : t -> unit;
  mutable changes : int;
}

and t = {
  id : int;
  key : string option;
  work : work;
  parent : t option;
  log_lines : int;
  window : line Queue.t;
  output : line Queue.t;
  mutable output_bytes : int;
  mutable dropped : int;
  mutable log_prev : float;
  mutable label : string;
  mutable metric : metric;
  transient : bool;
  started_at : float;
  mutable ended_at : float option;
  mutable message : string option;
  mutable outcome : outcome option;
  mutable children : t list;
  mutable child_limit : int;
  mutable gone : bool;
  mutable held : int;
  mutable weight : int;
  mutable shed : int;
  mutable last_cur : float;
  mutable last_t : float;
  mutable rate : float;
  mutable version : int;
  mutable memo : memo list;
}

let is_root r = r.id < 0
let is_running r = r.ended_at = None

let elapsed r =
  match r.ended_at with
  | Some f -> f -. r.started_at
  | None -> r.work.now () -. r.started_at

let since_start w = w.now () -. w.start
let rec depth r = match r.parent with None -> 0 | Some p -> 1 + depth p
let children r = List.rev (List.filter (fun c -> not c.gone) r.children)
let running_children r = List.filter is_running r.children

(* The labels from the top task down to [r]; the root has none. *)
let path r =
  let rec up acc r =
    if is_root r then acc
    else
      match r.parent with
      | None -> r.label :: acc
      | Some p -> up (r.label :: acc) p
  in
  up [] r

let rec root_of r = match r.parent with None -> r | Some p -> root_of p
let changed r = r.version <- r.version + 1
let keep r m = r.memo <- m

let notify w change =
  w.changes <- w.changes + 1;
  (match change with
  | Started r | Progressed r | Renamed r | Line (r, _) | Ended r -> changed r);
  List.iter (fun f -> f change) w.watchers

let watch w f = w.watchers <- w.watchers @ [ f ]
let on_ending r f = r.work.ending <- f

let local () =
  let stack = ref [] in
  {
    current = (fun () -> match !stack with r :: _ -> Some r | [] -> None);
    within =
      (fun r f ->
        let saved = !stack in
        stack := r :: saved;
        Fun.protect f ~finally:(fun () -> stack := saved));
    cancelled = (fun _ -> false);
  }

let row w ~id ?key ?parent ~log_lines ~transient label =
  let now = w.now () in
  {
    id;
    key;
    work = w;
    parent;
    log_lines;
    window = Queue.create ();
    output = Queue.create ();
    output_bytes = 0;
    dropped = 0;
    log_prev = now;
    label = Task_text.label label;
    metric = Spin;
    transient;
    started_at = now;
    ended_at = None;
    message = None;
    outcome = None;
    children = [];
    child_limit = 16;
    gone = false;
    held = 0;
    weight = 0;
    shed = 0;
    last_cur = 0.;
    last_t = now;
    rate = 0.;
    version = 0;
    memo = [];
  }

let root ?label ?scope ~now () =
  let start = now () in
  let w =
    {
      now;
      start;
      summary = Option.map Task_text.label label;
      scope = Option.value scope ~default:(local ());
      by_key = Hashtbl.create 16;
      next_id = 0;
      next_line = 0;
      started = 0;
      finished = 0;
      failed = 0;
      cancelled = 0;
      count_cell = 0;
      rate_cell = 0;
      label_cells = [||];
      watchers = [];
      ending = ignore;
      changes = 0;
    }
  in
  row w ~id:(-1) ~log_lines:0 ~transient:true
    (Option.value w.summary ~default:"")

(* The task of [w] where it is asked, if it is not the root: the one that
   [within] made current, running or not. *)
let scoped w =
  match w.scope.current () with
  | Some r when r.work == w && not (is_root r) -> Some r
  | _ -> None

let current w =
  match scoped w with Some r when is_running r -> Some r | _ -> None

let adopt p r =
  p.children <- r :: p.children;
  p.child_limit <- p.child_limit - 1;
  if p.child_limit <= 0 then begin
    (* The children that are gone are forgotten each time the list doubles. *)
    p.children <- List.filter (fun c -> not c.gone) p.children;
    p.child_limit <- max 16 (List.length p.children)
  end

let rec forget r =
  changed r;
  r.memo <- [];
  r.gone <- true;
  List.iter forget r.children;
  r.children <- [];
  Queue.clear r.output;
  r.output_bytes <- 0;
  r.held <- 0

let rec size r =
  List.fold_left (fun n c -> if c.gone then n else n + size c) 1 r.children

(* The tasks that ended and wait under a task are bounded: past 1 MiB, the
   oldest are dropped, down to half of it, never the latest. *)
let held_cap = 1_048_576

(* A success lets go of its output once it ended. *)
let weight r =
  256 + String.length r.label
  + Option.fold ~none:0 ~some:String.length r.message
  + (if r.outcome = Some `Succeeded then 0 else r.output_bytes)
  + r.held

let shed p latest =
  let keep c =
    if
      p.held > held_cap / 2 && c != latest && (not (is_running c)) && not c.gone
    then begin
      p.held <- p.held - c.weight;
      p.shed <- p.shed + size c;
      changed p;
      forget c;
      false
    end
    else true
  in
  p.children <- List.rev (List.filter keep (List.rev p.children))

let hold r =
  match r.parent with
  | None -> ()
  | Some p ->
      r.weight <- weight r;
      p.held <- p.held + r.weight;
      if p.held > held_cap then shed p r

let child_outcome = function
  | `Succeeded -> `Succeeded
  | `Failed | `Cancelled -> `Cancelled

(* The tasks under one that ends end first, in the order they started: they
   succeed with it, and are cancelled when it fails or is cancelled. *)
let rec finish outcome ?message r =
  if is_running r then begin
    let w = r.work in
    w.ending r;
    List.iter (finish (child_outcome outcome)) (List.rev (running_children r));
    Queue.clear r.window;
    r.ended_at <- Some (w.now ());
    r.message <- Option.map Task_text.label message;
    r.outcome <- Some outcome;
    if not (is_root r) then begin
      w.finished <- w.finished + 1;
      match outcome with
      | `Failed -> w.failed <- w.failed + 1
      | `Cancelled -> w.cancelled <- w.cancelled + 1
      | `Succeeded -> ()
    end;
    Option.iter (fun k -> Hashtbl.remove w.by_key k) r.key;
    hold r;
    (* A success folds its output away, once its end was seen. *)
    Fun.protect
      (fun () -> notify w (Ended r))
      ~finally:(fun () ->
        if outcome = `Succeeded then begin
          Queue.clear r.output;
          r.output_bytes <- 0
        end)
  end

let succeed ?message r = finish `Succeeded ?message r
let fail ?message r = finish `Failed ?message r
let cancel ?message r = finish `Cancelled ?message r

(* The label column only widens: it keeps the widest label of each depth. *)
let widen_label r =
  let w = r.work and d = depth r in
  if d >= Array.length w.label_cells then begin
    let cells = Array.make (d + 1) 0 in
    Array.blit w.label_cells 0 cells 0 (Array.length w.label_cells);
    w.label_cells <- cells
  end;
  w.label_cells.(d) <- max w.label_cells.(d) (Width.string_width r.label)

let start ?key ?(log_lines = 8) ?(transient = false) parent label =
  if log_lines < 0 then invalid_arg "Console.Task.start: negative log_lines";
  (match key with
  | Some "" -> invalid_arg "Console.Task.start: empty key"
  | _ -> ());
  let w = parent.work in
  match Option.bind key (Hashtbl.find_opt w.by_key) with
  | Some r -> (r, false)
  | None ->
      let id = w.next_id in
      w.next_id <- id + 1;
      let r = row w ~id ?key ~parent ~log_lines ~transient label in
      w.started <- w.started + 1;
      Option.iter (fun k -> Hashtbl.replace w.by_key k r) key;
      if not parent.gone then adopt parent r;
      notify w (Started r);
      (* A task started under one that ended takes its ending. *)
      (match parent.outcome with
      | None -> ()
      | Some `Succeeded -> succeed r
      | Some (`Failed | `Cancelled) -> cancel r);
      (r, true)

let within r f =
  let value = ref None in
  r.work.scope.within r (fun () -> value := Some (f ()));
  Option.get !value

(* A task found by its key ends as the call that started it says. *)
let run ?key ?log_lines ?transient parent label f =
  match start ?key ?log_lines ?transient parent label with
  | r, false -> within r (fun () -> f r)
  | r, true -> (
      let value = ref None in
      match r.work.scope.within r (fun () -> value := Some (f r)) with
      | () ->
          succeed r;
          Option.get !value
      | exception e ->
          let bt = Printexc.get_raw_backtrace () in
          if r.work.scope.cancelled e then cancel r
          else fail ~message:(r.label ^ ": " ^ Printexc.to_string e) r;
          Printexc.raise_with_backtrace e bt)

let set_label r label =
  if is_running r && not (is_root r) then begin
    r.label <- Task_text.label label;
    notify r.work (Renamed r)
  end

let count_text = function
  | Spin -> ""
  | Count (c, tot) -> string_of_int c ^ "/" ^ string_of_int tot
  | Bytes (c, tot) -> Task_text.bytes c ^ " / " ^ Task_text.bytes tot

(* The widest the count of a task can get, known from its total: a count
   of items as wide as its total twice, and at least [999/999]; a count of
   bytes below a total of a KiB or more as wide as "1023.99 KiB" in front
   of it. *)
let count_width = function
  | Spin -> 0
  | Count (_, total) ->
      max (String.length "999/999")
        (String.length (count_text (Count (total, total))))
  | Bytes (cur, total) ->
      let whole = Task_text.bytes total in
      let widest =
        if Int64.compare total 1024L < 0 then whole else "1023.99 KiB"
      in
      max
        (String.length (count_text (Bytes (cur, total))))
        (String.length widest + 3 + String.length whole)

let rate_text metric rate =
  match metric with
  | Bytes _ when rate > 1. -> Task_text.bytes (Int64.of_float rate) ^ "/s"
  | Spin | Count _ | Bytes _ -> ""

let same_metric a b =
  match (a, b) with
  | Spin, Spin -> true
  | Count (c, n), Count (c', n') -> c = c' && n = n'
  | Bytes (c, n), Bytes (c', n') -> Int64.equal c c' && Int64.equal n n'
  | _ -> false

(* The rate weighs the past by the time since it was measured, not by the
   number of reports: a count reported often or seldom gives the same rate
   for the same work. *)
let tau = 2.

let measure r cur =
  let now = r.work.now () in
  let dt = now -. r.last_t in
  if dt > 0. then begin
    let inst = Float.max 0. ((cur -. r.last_cur) /. dt) in
    r.rate <-
      (if r.rate <= 0. then inst
       else
         let keep = Float.exp (-.dt /. tau) in
         (keep *. r.rate) +. ((1. -. keep) *. inst));
    r.last_cur <- cur;
    r.last_t <- now
  end

(* The columns widen at most once for each kind of progress: the first
   task of bytes widens the count column to the widest count of bytes and
   opens the rate column at the widest rate, so that no later total and no
   first rate widens them again. A count of items is as wide as its total
   needs. *)
let widest_bytes = String.length "1023.99 KiB / 1023.99 TiB"
let widest_rate = String.length "1023.99 MiB/s"

let set_metric r metric =
  let moved = not (same_metric r.metric metric) in
  changed r;
  r.metric <- metric;
  let w = r.work in
  w.count_cell <- max w.count_cell (count_width metric);
  (match metric with
  | Bytes _ ->
      w.count_cell <- max w.count_cell widest_bytes;
      w.rate_cell <-
        max w.rate_cell
          (max widest_rate (String.length (rate_text metric r.rate)))
  | Spin | Count _ -> ());
  if moved then notify w (Progressed r)

let set_items r ~count ~total =
  if count < 0 || total < 0 then
    invalid_arg "Console.Task.set_items: negative progress";
  if is_running r && not (is_root r) then
    match r.metric with
    | Bytes _ -> invalid_arg "Console.Task.set_items: the task counts bytes"
    | Spin ->
        measure r (float_of_int count);
        set_metric r (Count (count, max count total))
    | Count (c, n) ->
        let count = max c count in
        measure r (float_of_int count);
        set_metric r (Count (count, max count (max n total)))

let set_bytes r ~count ~total =
  if count < 0L || total < 0L then
    invalid_arg "Console.Task.set_bytes: negative progress";
  if is_running r && not (is_root r) then begin
    let count, total =
      match r.metric with
      | Count _ -> invalid_arg "Console.Task.set_bytes: the task counts items"
      | Spin -> (count, Int64.max count total)
      | Bytes (c, n) ->
          let count = Int64.max c count in
          (count, Int64.max count (Int64.max n total))
    in
    measure r (Int64.to_float count);
    set_metric r (Bytes (count, total))
  end

let fraction = function
  | Spin -> None
  | Count (cur, total) -> Some (float cur /. float (max 1 total))
  | Bytes (cur, total) ->
      Some (Int64.to_float cur /. Int64.to_float (Int64.max 1L total))

let eta r =
  let left =
    match r.metric with
    | Spin -> None
    | Count (cur, total) -> Some (float (total - cur))
    | Bytes (cur, total) -> Some (Int64.to_float (Int64.sub total cur))
  in
  match left with
  | Some left when r.rate > 0. && is_running r -> Some (left /. r.rate)
  | _ -> None

(* The lines a task keeps for its failure, the oldest dropped first. *)
let output_cap = 1_048_576

let keep_output r line =
  Queue.add line r.output;
  r.output_bytes <- r.output_bytes + String.length line.text + 1;
  while r.output_bytes > output_cap do
    let old = Queue.take r.output in
    r.output_bytes <- r.output_bytes - String.length old.text - 1;
    r.dropped <- r.dropped + 1
  done

(* A window holds the latest lines its task shows. *)
let push r line =
  if r.log_lines > 0 then begin
    Queue.add line r.window;
    if Queue.length r.window > r.log_lines then ignore (Queue.take r.window)
  end

(* A running task keeps each line, capped, in its window and for its
   failure; every line is a change, whole. *)
let add r kind text =
  let w = r.work in
  List.iter
    (fun text ->
      let now = w.now () in
      let line = { gap = now -. r.log_prev; kind; text; seq = w.next_line } in
      w.next_line <- w.next_line + 1;
      r.log_prev <- now;
      if is_running r then begin
        let kept =
          let text = Task_text.capped line.text in
          if text == line.text then line else { line with text }
        in
        push r kept;
        keep_output r kept
      end;
      notify w (Line (r, line)))
    (Task_text.lines text)

(* A line of the root is output of no task. *)
let above root kind text =
  List.iter
    (fun text ->
      notify root.work (Line (root, { gap = 0.; kind; text; seq = 0 })))
    (Task_text.lines text)

let kind_of = function None -> Output | Some l -> Logs l

let log ?level r text =
  if is_root r then above r (kind_of level) text
  else if is_running r then add r (kind_of level) text

let report root level text =
  match scoped root.work with
  | Some r -> add r (Logs level) text
  | None -> above root (Logs level) text

let event (ch : change) =
  let task r = if is_root r then None else Some (r.id, path r) in
  let parent r =
    match r.parent with Some p when not (is_root p) -> Some p.id | _ -> None
  in
  fun ~time ->
    match ch with
    | Started r -> Some (Event.start ?parent:(parent r) ~time (r.id, path r))
    | Progressed r -> (
        let progress count total unit =
          Some (Event.progress ~time ~count ~total ~unit (r.id, path r))
        in
        match r.metric with
        | Spin -> None
        | Count (cur, total) ->
            progress (Int64.of_int cur) (Int64.of_int total) `Items
        | Bytes (cur, total) -> progress cur total `Bytes)
    | Renamed r -> Some (Event.label ~time (r.id, path r) r.label)
    | Line (r, line) ->
        let level =
          match line.kind with
          | Output -> None
          | Logs l -> Some (l :> Event.level)
        in
        Some (Event.output ?task:(task r) ?level ~time line.text)
    | Ended r ->
        let e =
          match Option.value r.outcome ~default:`Cancelled with
          | `Succeeded -> Event.succeeded
          | `Failed -> Event.failed
          | `Cancelled -> Event.cancelled
        in
        Some (e ?task:(task r) ?message:r.message ~time ~elapsed:(elapsed r) ())

let on_event r f =
  let w = r.work in
  watch w (fun ch ->
      match event ch ~time:(since_start w) with Some e -> f e | None -> ())

let state r : [ `Running | outcome ] =
  match r.outcome with
  | None -> `Running
  | Some o -> (o :> [ `Running | outcome ])

let pp ppf r =
  Fmt.pf ppf "<task %d %S%s>" r.id r.label
    (match r.outcome with
    | None -> ""
    | Some `Succeeded -> " succeeded"
    | Some `Failed -> " failed"
    | Some `Cancelled -> " cancelled")

let id r = r.id
let work r = r.work
let parent r = r.parent
let label r = r.label
let metric r = r.metric
let transient r = r.transient
let message r = r.message
let outcome r = r.outcome
let window r = r.window
let output r = r.output
let dropped r = r.dropped
let gone r = r.gone
let shed r = r.shed
let rate r = r.rate
let version r = r.version
let memo r = r.memo

module Work = struct
  let now (w : work) = w.now
  let summary (w : work) = w.summary
  let started (w : work) = w.started
  let finished (w : work) = w.finished
  let failed (w : work) = w.failed
  let cancelled (w : work) = w.cancelled
  let count_cell (w : work) = w.count_cell
  let rate_cell (w : work) = w.rate_cell

  let label_cell (w : work) depth =
    if depth < Array.length w.label_cells then w.label_cells.(depth) else 0

  let depths (w : work) = Array.length w.label_cells
end
