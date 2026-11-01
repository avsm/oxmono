module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module E = Matrix_client.Encryption

type report = {
  event_id : Id.Event_id.t;
  cause : E.utd_cause;
  time_to_decrypt : float option;
  event_age : int64 option;
  event_local_age : int64 option;
  event_local_age_millis : int64 option;
  user_trusts_own_identity : bool;
  sender : Id.User_id.t;
  sender_homeserver : string;
  own_homeserver : string option;
}

type pending = { started : float; report : report }

type t = {
  sw : Eio.Switch.t;
  clock : float Eio.Time.clock_ty Eio.Std.r;
  store : Matrix_client.Store.t option;
  on_utd : report -> unit;
  mark_reported : report -> unit;
  own_homeserver : string option;
  device_created_at : Ptime.t option;
  max_delay : float option;
  mutable reported : Id.Event_id.t list;
  reported_set : (string, unit) Hashtbl.t;
  pending : (string, pending) Hashtbl.t;
  mutex : Eio.Mutex.t;
}

let ring_size = 4096
let ring_codec = Jsont.list Id.Event_id.jsont

let ring_slot =
  Matrix_client.Store.Slot.v ~name:"matrix.ui.utd_hook.reported" ring_codec

let key id = Id.Event_id.to_string id

let load store =
  match Matrix_client.Store.Slot.find store ring_slot with
  | Ok (Some ids) ->
      let ids =
        (* A malformed/oversized historical ring should not make a client
           unusable. Keep its newest bounded suffix and remove duplicates. *)
        let ids =
          if List.length ids > ring_size then
            List.filteri (fun i _ -> i >= List.length ids - ring_size) ids
          else ids
        in
        List.fold_left
          (fun acc id ->
            if List.exists (Id.Event_id.equal id) acc then acc else id :: acc)
          [] (List.rev ids)
      in
      ids
  | Ok None -> []
  | Error err ->
      Logs.warn (fun m ->
          m "ui: unable to load UTD report deduplication state: %a"
            Matrix_client.Error.pp err);
      []

let create ~sw ~clock ?store ?max_delay ?own_homeserver ?device_created_at
    ~on_utd () =
  let reported = match store with None -> [] | Some store -> load store in
  let reported_set = Hashtbl.create ((List.length reported * 2) + 1) in
  List.iter (fun id -> Hashtbl.replace reported_set (key id) ()) reported;
  let pending = Hashtbl.create 32 in
  let max_delay = Option.map (Float.max 0.) max_delay in
  let mark_reported_ref = ref (fun (_ : report) -> ()) in
  let t =
    {
      sw;
      clock;
      store;
      on_utd;
      mark_reported = (fun report -> !mark_reported_ref report);
      own_homeserver;
      device_created_at;
      max_delay;
      reported;
      reported_set;
      pending;
      mutex = Eio.Mutex.create ();
    }
  in
  let mark_reported report =
    let id = key report.event_id in
    t.reported <- t.reported @ [ report.event_id ];
    Hashtbl.replace t.reported_set id ();
    while List.length t.reported > ring_size do
      match t.reported with
      | [] -> ()
      | oldest :: rest ->
          t.reported <- rest;
          Hashtbl.remove t.reported_set (key oldest)
    done;
    Option.iter
      (fun store ->
        match Matrix_client.Store.Slot.set store ring_slot t.reported with
        | Error err ->
            Logs.warn (fun m ->
                m "ui: unable to persist UTD report deduplication state: %a"
                  Matrix_client.Error.pp err)
        | Ok () -> (
            try
              match Matrix_client.Store.flush store with
              | Ok () -> ()
              | Error err ->
                  Logs.warn (fun m ->
                      m "ui: unable to flush UTD report deduplication state: %a"
                        Matrix_client.Error.pp err)
            with
            | Eio.Cancel.Cancelled _ as exn ->
                let bt = Printexc.get_raw_backtrace () in
                Printexc.raise_with_backtrace exn bt
            | Eio.Io _ as exn ->
                let contextual =
                  Eio.Exn.add_context exn
                    "flushing UTD report deduplication state"
                in
                Logs.warn (fun m ->
                    m "ui: unable to flush UTD report deduplication state: %a"
                      Eio.Exn.pp contextual)))
      t.store
  in
  mark_reported_ref := mark_reported;
  t

let on_utd t ~event ~cause ?event_local_age ~user_trusts_own_identity () =
  match event.Event.Raw_event.event_id with
  | None -> ()
  | Some event_id ->
      let id = key event_id in
      let ready =
        Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
            if Hashtbl.mem t.reported_set id || Hashtbl.mem t.pending id then
              None
            else
              let event_local_age =
                match event_local_age with
                | Some _ -> event_local_age
                | None ->
                    Option.map
                      (fun created ->
                        Int64.sub
                          (Event.Timestamp.to_ms event.origin_server_ts)
                          (Event.Timestamp.to_ms
                             (Event.Timestamp.of_ptime created)))
                      t.device_created_at
              in
              let report =
                {
                  event_id;
                  cause;
                  time_to_decrypt = None;
                  event_age = Option.bind event.unsigned Event.Unsigned.age;
                  event_local_age;
                  event_local_age_millis = event_local_age;
                  user_trusts_own_identity;
                  sender = event.sender;
                  sender_homeserver =
                    Id.Server_name.to_string
                      (Id.User_id.server_name event.sender);
                  own_homeserver = t.own_homeserver;
                }
              in
              match t.max_delay with
              | None ->
                  t.mark_reported report;
                  Some report
              | Some delay ->
                  Hashtbl.replace t.pending id
                    { started = Eio.Time.now t.clock; report };
                  Eio.Fiber.fork ~sw:t.sw (fun () ->
                      Eio.Time.sleep t.clock delay;
                      let ready =
                        Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
                            match Hashtbl.find_opt t.pending id with
                            | None -> None
                            | Some pending ->
                                Hashtbl.remove t.pending id;
                                if Hashtbl.mem t.reported_set id then None
                                else
                                  let report =
                                    {
                                      pending.report with
                                      time_to_decrypt = None;
                                    }
                                  in
                                  t.mark_reported report;
                                  Some report)
                      in
                      Option.iter t.on_utd ready);
                  None)
      in
      Option.iter t.on_utd ready

let on_late_decrypt t event_id =
  let id = key event_id in
  let ready =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        match Hashtbl.find_opt t.pending id with
        | None -> None
        | Some pending ->
            Hashtbl.remove t.pending id;
            if Hashtbl.mem t.reported_set id then None
            else
              let report =
                {
                  pending.report with
                  time_to_decrypt =
                    Some
                      (Float.max 0. (Eio.Time.now t.clock -. pending.started));
                }
              in
              t.mark_reported report;
              Some report)
  in
  Option.iter t.on_utd ready
