type mode = Discovering | Sliding | Classic

type response =
  | Sliding of Matrix_proto.Sliding_sync.Response.t
  | Classic of Matrix_proto.Sync.Response.t

type stop_reason = User_stop | Fallback

let default_on_response _ = Sync.Continue
let default_on_mode _ = ()

let resolve_once resolved finish reason =
  if not !resolved then begin
    resolved := true;
    Eio.Promise.resolve finish reason
  end

let action_after_response ~resolved ~finish on_response response =
  match on_response response with
  | Sync.Stop as action ->
      resolve_once resolved finish User_stop;
      action
  | (Sync.Continue | Sync.Retry_after _) as action -> action

let action_after_error ~resolved ~finish ~unsupported on_error error =
  if unsupported then begin
    resolve_once resolved finish Fallback;
    Sync.Stop
  end
  else
    match on_error error with
    | Sync.Stop as action ->
        resolve_once resolved finish User_stop;
        action
    | (Sync.Continue | Sync.Retry_after _) as action -> action

let run_sliding ~clock client ~service ~controller ?initial_pos ?timeout_ms
    ?txn_id ?set_presence ?thread_subscription_store ?encryption ?verification
    ?on_encryption_error ~on_response ~on_error ~on_change () =
  let finished, finish = Eio.Promise.create () in
  let resolved = ref false in
  Eio.Switch.run @@ fun phase_sw ->
  let callbacks : Matrix_proto.Sliding_sync.Response.t Sync.callbacks =
    {
      on_response =
        (fun response ->
          action_after_response ~resolved ~finish on_response (Sliding response));
      on_error =
        (fun error ->
          action_after_error ~resolved ~finish
            ~unsupported:(Sliding_sync.is_unsupported error)
            on_error error);
    }
  in
  Sliding_sync.sync_forever_controlled ~sw:phase_sw ~clock client ~service
    ?initial_pos ?timeout_ms ?txn_id ?set_presence ?thread_subscription_store
    ?encryption ?verification ?on_encryption_error ~on_change ~callbacks
    controller;
  Eio.Promise.await finished

let classic_params_with_presence ?set_presence
    (classic_params : Matrix_client.Sync.params) =
  match set_presence with
  | None -> classic_params
  | Some set_presence ->
      { classic_params with set_presence = Some set_presence }

let run_classic ~clock client ~service ?params ?encryption ?verification
    ?on_encryption_error ~on_response ~on_error ~on_change () =
  let finished, finish = Eio.Promise.create () in
  let resolved = ref false in
  Eio.Switch.run @@ fun phase_sw ->
  let on_response response =
    action_after_response ~resolved ~finish on_response (Classic response)
  in
  let on_error error =
    action_after_error ~resolved ~finish ~unsupported:false on_error error
  in
  Sync_service.run ~sw:phase_sw ~clock client service ?params ?encryption
    ?verification ?on_encryption_error ~on_response ~on_error ~on_change ();
  Eio.Promise.await finished

let run ~sw ~clock client ~service ?(request = Sliding_sync.Request.v ())
    ?controller ?initial_pos ?timeout_ms ?txn_id ?set_presence
    ?thread_subscription_store ?classic_params ?encryption ?verification
    ?on_encryption_error ?(on_mode = default_on_mode)
    ?(on_response = default_on_response) ?on_error ~on_change () =
  let classic_params =
    classic_params_with_presence ?set_presence
      (Option.value classic_params ~default:Matrix_client.Sync.default_params)
  in
  let controller =
    match controller with
    | Some controller -> controller
    | None -> Sliding_sync.Controller.create request
  in
  let reset_backoff, on_error =
    match on_error with
    | Some on_error -> ((fun () -> ()), on_error)
    | None ->
        let backoff = ref 0.5 in
        ( (fun () -> backoff := 0.5),
          fun _ ->
            let delay = !backoff in
            backoff := Float.min 60. (delay *. 2.);
            Sync.Retry_after delay )
  in
  let on_response response =
    reset_backoff ();
    on_response response
  in
  Eio.Fiber.fork ~sw (fun () ->
      on_mode Discovering;
      let rec discover () =
        try Some (Sliding_sync.is_available client)
        with Eio.Io (Error.E error, _) -> (
          match on_error error with
          | Sync.Continue -> discover ()
          | Sync.Stop -> None
          | Sync.Retry_after delay ->
              Eio.Time.sleep clock delay;
              discover ())
      in
      match discover () with
      | None -> ()
      | Some false ->
          reset_backoff ();
          on_mode Classic;
          ignore
            (run_classic ~clock client ~service ~params:classic_params
               ?encryption ?verification ?on_encryption_error ~on_response
               ~on_error ~on_change ())
      | Some true -> (
          reset_backoff ();
          on_mode Sliding;
          match
            run_sliding ~clock client ~service ~controller ?initial_pos
              ?timeout_ms ?txn_id ?set_presence ?thread_subscription_store
              ?encryption ?verification ?on_encryption_error ~on_response
              ~on_error ~on_change ()
          with
          | User_stop -> ()
          | Fallback ->
              on_mode Classic;
              ignore
                (run_classic ~clock client ~service ~params:classic_params
                   ?encryption ?verification ?on_encryption_error ~on_response
                   ~on_error ~on_change ())))
