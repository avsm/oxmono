let src = Logs.Src.create "crowthebot" ~doc:"Crow runtime diagnostics"

module Log = (val Logs.src_log src : Logs.LOG)

let tools_src = Logs.Src.create "crowthebot.tools" ~doc:"Crow tool activity"

module Tools = (val Logs.src_log tools_src : Logs.LOG)

let configure ~verbose =
  Logs.set_level (Some Logs.Warning);
  Logs.Src.set_level src (Some (if verbose then Logs.Info else Logs.Warning));
  Logs.Src.set_level tools_src (Some Logs.Info)

let enabled () =
  match Logs.Src.level src with
  | Some (Logs.App | Logs.Info | Logs.Debug) -> true
  | _ -> false

let matrix_error (error : Matrix_eio.Error.err) =
  match error with
  | Network _ -> "Matrix network error"
  | Policy_denied _ -> "Matrix capability denied"
  | Tls _ -> "Matrix TLS error"
  | Http { status; _ } -> Printf.sprintf "Matrix HTTP %d" status
  | Json _ -> "Matrix JSON error"
  | Matrix { errcode; _ } -> (
      "Matrix "
      ^
      match errcode with
      | Matrix_client.Error.M_UNKNOWN_CODE _ -> "unknown error code"
      | code -> Matrix_client.Error.errcode_to_string code)
  | Not_logged_in -> "Matrix session missing"
  | No_content -> "Matrix response empty"
  | Cancelled -> "cancelled"

exception Model_output_limit

let error = function
  | Model_output_limit -> "model output token limit reached"
  | Eio.Io (Matrix_eio.Error.E e, _) -> matrix_error e
  | Eio.Io (Openrouter.E (Openrouter.Http_error { status; _ }), _) ->
      Printf.sprintf "model HTTP %d" status
  | Eio.Io (Openrouter.E _, _) -> "model protocol error"
  | Eio.Time.Timeout -> "timeout"
  | Eio.Cancel.Cancelled _ -> "cancelled"
  | Failure _ -> "operation failed"
  | Invalid_argument _ -> "invalid input or configuration"
  | Eio.Io _ -> "I/O error"
  | _ -> "unexpected exception"

let sent = function
  | Matrix_bot.Sent.Sent _ -> "sent"
  | Uploaded _ -> "unexpected upload result"
  | Failed None -> "send failed"
  | Failed (Some e) -> matrix_error (Matrix_eio.Error.of_client_error e)
  | Cancelled -> "send cancelled"
  | Timed_out -> "send confirmation timed out"

let sync = function
  | Matrix_ui.Runtime.Not_started -> "not started"
  | Syncing -> "waiting for first sync"
  | Live _ -> "live"
  | Failed _ -> "failed"
  | Offline -> "offline; retrying"
  | Stopped -> "stopped"

let timeline ~self events =
  let pending = ref 0 and malformed = ref 0 in
  Array.iter
    (fun event ->
      let p =
        Matrix_ui.Presentation.of_event (Matrix_ui.Event_cache.effective event)
      in
      if not (Matrix_proto.Id.User_id.equal p.sender self) then
        match p.content with
        | Matrix_ui.Presentation.Unable_to_decrypt -> incr pending
        | Malformed _ -> incr malformed
        | _ -> ())
    events;
  (Array.length events, !pending, !malformed)

let watch_room ~sw ~self ~cache ~room =
  let module O = Matrix_ui.Observable.List in
  let events = Matrix_ui.Event_cache.events cache room in
  let initial, subscription = O.subscribe ~sw events in
  let previous = ref None in
  let report events =
    let counts = timeline ~self events in
    if !previous <> Some counts then begin
      previous := Some counts;
      let total, pending, malformed = counts in
      Log.info (fun m ->
          m "Timeline room=%S cached=%d encrypted_pending=%d malformed=%d"
            (Matrix_proto.Id.Room_id.to_string room)
            total pending malformed);
      if pending > 0 then
        Log.info (fun m ->
            m
              "Room %S has messages waiting for decryption; they cannot reach \
               Crow until room keys arrive"
              (Matrix_proto.Id.Room_id.to_string room))
    end
  in
  report initial;
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec loop () =
        match O.next subscription with
        | None -> `Stop_daemon
        | Some _ ->
            report (O.snapshot events);
            loop ()
      in
      loop ());
  fun () -> O.unsubscribe subscription
