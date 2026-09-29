(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Journal = Agentkit.Journal
module Schedule = Agentkit.Schedule

type stopped = Signalled of string | Faulted of string

(* A signal handler runs between two safe points of a fiber that may be inside
   the engine, so it does the least it can: it says what arrived. The loop and
   the wake-up both read it where they can act on it. *)
let asked_to_stop = Atomic.make None
let asked_to_reread = Atomic.make false
let stopping () = Atomic.get asked_to_stop

let with_signals f =
  let handle atomic name =
    Sys.Signal_handle (fun _ -> Atomic.set atomic (Some name))
  in
  let previous =
    [
      (Sys.sigterm, Sys.signal Sys.sigterm (handle asked_to_stop "SIGTERM"));
      (Sys.sigint, Sys.signal Sys.sigint (handle asked_to_stop "SIGINT"));
      ( Sys.sighup,
        Sys.signal Sys.sighup
          (Sys.Signal_handle (fun _ -> Atomic.set asked_to_reread true)) );
    ]
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun (n, old) -> Sys.set_signal n old) previous)
    f

let ids (s : Schedule.t) =
  List.map (fun (t : Schedule.task) -> t.Schedule.id) s.Schedule.tasks

(* What a person's edit changed, which is what the journal records. A task that
   is gone counts as changed, since a reader asking why it stopped firing is
   asking about this record. *)
let changed (before : Schedule.t) (after : Schedule.t) =
  let find id (s : Schedule.t) =
    List.find_opt
      (fun (t : Schedule.task) -> t.Schedule.id = id)
      s.Schedule.tasks
  in
  let moved =
    List.filter (fun id -> find id before <> find id after) (ids after)
  in
  moved @ List.filter (fun id -> find id after = None) (ids before)

let run ~clock ~store ~schedule ~tick ~publish_tasks ~wake =
  with_signals @@ fun () ->
  let journal = Store.journal store in
  let append k = ignore (Journal.append journal k) in
  let history = History.read (Store.journal_dir (Store.root store)) in
  let current = ref { Schedule.tasks = [] } in
  let stamp = ref None in
  let since = ref None in
  (* The file is stat-ed rather than read on every tick, since a tick is a
     second and a schedule is read by a person's edit and not by the clock. *)
  let stat () =
    match Eio.Path.stat ~follow:true schedule with
    | st -> Some (st.Eio.File.Stat.mtime, st.Eio.File.Stat.size)
    | exception Eio.Exn.Io _ -> None
  in
  let load ~force =
    let now = stat () in
    if force || now <> !stamp then begin
      stamp := now;
      match Schedule.read schedule with
      | Ok s ->
          let changed = changed !current s in
          current := s;
          append (Journal.Schedule_load { Journal.tasks = ids s; changed })
      | Error e ->
          (* The schedule in force stays in force. A person mid-edit should not
             lose a daemon, and an empty schedule would stop every task at
             once. *)
          Logs.err (fun m ->
              m "the schedule was not read, so the one in force stays: %s" e);
          append (Journal.Error { Journal.where = "schedule"; what = e })
    end
  in
  let fire (task : Schedule.task) (f : Schedule.firing) =
    append
      (Journal.Wake
         {
           Journal.task = task.Schedule.id;
           due = Journal.rfc3339 f.Schedule.due;
           why = Schedule.why_name f.Schedule.why;
           serial = f.Schedule.serial;
         });
    (* Noted before the wake-up runs, so a task whose wake-up raises is not
       fired again by the next tick of the same run. The journal already says it
       fired, which is what a restart reads. *)
    History.note history task.Schedule.id
      {
        History.due = f.Schedule.due;
        serial =
          (match f.Schedule.serial with
          | Some n -> n
          | None -> (
              match History.fired history task.Schedule.id with
              | Some p -> p.History.serial
              | None -> 0));
      };
    Logs.info (fun m ->
        m "waking for %s (%s)" task.Schedule.id
          (Schedule.why_name f.Schedule.why));
    wake ~task:task.Schedule.id ~prompt:task.Schedule.prompt
  in
  let rec loop () =
    match Atomic.get asked_to_stop with
    | Some why -> Signalled why
    | None -> (
        load ~force:(Atomic.exchange asked_to_reread false);
        let now = Eio.Time.now clock in
        let polled =
          List.map
            (fun (task : Schedule.task) ->
              let last, serial =
                match History.fired history task.Schedule.id with
                | Some p -> (Some p.History.due, p.History.serial)
                | None -> (None, 0)
              in
              ( task,
                Schedule.poll ~zone:Schedule.system task ~last ~serial
                  ~since:!since ~now ))
            !current.Schedule.tasks
        in
        (* Next fire times are the daemon's own arithmetic rather than a file
           being read back, so this is where a client learns them. *)
        let entries =
          List.map
            (fun ((task : Schedule.task), (st : Schedule.status)) ->
              {
                Status.task = task.Schedule.id;
                next = Option.map Journal.rfc3339 st.Schedule.next;
                waiting = st.Schedule.fire <> None;
              })
            polled
        in
        publish_tasks entries;
        let due =
          List.filter_map
            (fun (task, (st : Schedule.status)) ->
              match st.Schedule.fire with
              | Some f -> Some (task, f)
              | None -> None)
            polled
        in
        (* Sequentially, since a process holds one engine, and stopping between
           two of them rather than starting the next. *)
        let rec fire_each fired = function
          | [] -> None
          | (task, f) :: rest -> (
              (* A task leaves the published list once it fires. The one running
                 is the job rather than a task waiting, and one that has run has
                 no next fire time until the next tick computes it. *)
              let fired = task.Schedule.id :: fired in
              publish_tasks
                (List.filter
                   (fun (e : Status.due) -> not (List.mem e.Status.task fired))
                   entries);
              match fire task f with
              | Wake.Faulted why -> Some (Faulted why)
              | Wake.Finished -> (
                  match Atomic.get asked_to_stop with
                  | Some why -> Some (Signalled why)
                  | None -> fire_each fired rest))
        in
        match fire_each [] due with
        | Some stopped -> stopped
        | None ->
            (* Set after the poll, so that the first poll of a run sees [None]
               and a [skip] task does not fire for a due time nobody was there
               for. *)
            since := Some now;
            Eio.Time.sleep clock tick;
            loop ())
  in
  loop ()
