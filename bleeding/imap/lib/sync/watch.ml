type retry_error =
  | Connect_failed of Error.t
  | Connect_timed_out
  | Scan_failed of Error.t
  | Scan_timed_out
  | Idle_failed of Imap_eio.Error.t

type error =
  | Invalid_configuration of string
  | Fatal_scan of Error.t

let needs_rescan (cursor : Imap.Mirror.cursor)
    (info : Imap.Response.select_metadata) =
  let epoch_changed = match cursor.uidvalidity with
    | None -> true
    | Some value -> Imap.Uidvalidity.to_int64 value <> info.uidvalidity in
  let frontier_changed = info.uidnext <> Int64.succ cursor.frontier in
  let modseq_changed = match cursor.anchor,info.highestmodseq with
    | None,_ -> false
    | Some _,None -> true
    | Some anchor,Some observed ->
        info.nomodseq || Imap.Modseq.to_int64 anchor <> observed in
  epoch_changed || frontier_changed || modseq_changed

let run ~clock ~connect ~next_stage_id ~on_publish
    ?(on_retry=fun _ -> ()) ?(poll_seconds=60.) ?(retry_seconds=5.)
    ?(max_retry_seconds=300.) ?(connect_timeout_seconds=30.)
    ?(scan_timeout_seconds=3600.) ?(idle_renew_seconds=1500.) () =
  if not (Float.is_finite poll_seconds && poll_seconds > 0. &&
          Float.is_finite retry_seconds && retry_seconds > 0. &&
          Float.is_finite max_retry_seconds &&
          max_retry_seconds >= retry_seconds &&
          Float.is_finite connect_timeout_seconds &&
          connect_timeout_seconds > 0. &&
          Float.is_finite scan_timeout_seconds &&
          scan_timeout_seconds > 0. &&
          Float.is_finite idle_renew_seconds && idle_renew_seconds > 0. &&
          idle_renew_seconds <= 1740.) then
    Error (Invalid_configuration
      "watch intervals must be finite and positive, IDLE renewal at most \
       1740 seconds, and maximum retry at least initial retry")
  else
    let sleep seconds = Eio.Time.sleep clock seconds in
    let next_delay delay = min max_retry_seconds (delay *. 2.) in
    let connect_bounded ~sw =
      try Some (Eio.Time.with_timeout_exn clock connect_timeout_seconds
        (fun () -> connect ~sw))
      with Eio.Time.Timeout -> None in
    let scan () =
      try Eio.Time.with_timeout_exn clock scan_timeout_seconds
        (fun () -> Eio.Switch.run @@ fun sw ->
          match connect_bounded ~sw with
          | None -> Error Connect_timed_out
          | Some (Error error) -> Error (Connect_failed error)
          | Some (Ok (ctx:Ctx.t)) ->
              let idle=Imap_eio.Client.has ctx.client Imap.Capability.Idle in
              match Engine.scan_once ~ctx ~stage_id:(next_stage_id ()) () with
              | Ok receipt -> Ok (receipt,idle)
              | Error error -> Error (Scan_failed error))
      with Eio.Time.Timeout -> Error Scan_timed_out in
    let wait_for_change cursor = Eio.Switch.run @@ fun sw ->
      match connect_bounded ~sw with
      | None -> Error Connect_timed_out
      | Some (Error error) -> Error (Connect_failed error)
      | Some (Ok (ctx:Ctx.t)) ->
          let renew_at = Eio.Time.now clock +. idle_renew_seconds in
          let watch_once () =
            Imap_eio.Client.with_mailbox ctx.client ~mode:`Read_only
              ctx.mailbox
              (fun selected ->
                match Imap_eio.Selected.info selected with
                | Error _ as error -> error
                | Ok info when needs_rescan cursor info -> Ok `Changed
                | Ok _ ->
                    match Result.bind (Imap_eio.Selected.Idle.require selected)
                        Imap_eio.Selected.Idle.wait_for_change with
                    | Ok _ -> Ok `Woken
                    | Error _ as error -> error) in
          let rec watch () =
            let remaining = renew_at -. Eio.Time.now clock in
            if remaining <= 0. then Ok ()
            else
              match Eio.Time.with_timeout_exn clock remaining watch_once with
              | Ok `Woken -> watch ()
              | Ok `Changed -> Ok ()
              | Error error -> Error (Idle_failed error)
              | exception Eio.Time.Timeout -> Ok () in
          watch () in
    let fatal = function
      | Error.Invalid_scope _ | Error.Limit _ | Error.Mirror _
      | Error.Uidvalidity_changed -> true
      | _ -> false in
    let rec loop delay =
      match scan () with
      | Error (Connect_failed error | Scan_failed error) when fatal error ->
          Error (Fatal_scan error)
      | Error issue ->
          on_retry issue;
          sleep delay;
          loop (next_delay delay)
      | Ok (receipt,idle) ->
          on_publish receipt;
          let waited = if idle then wait_for_change receipt.cursor
            else (sleep poll_seconds; Ok ()) in
          (match waited with
           | Ok () -> loop retry_seconds
           | Error issue ->
               on_retry issue;
               sleep delay;
               loop (next_delay delay)) in
    loop retry_seconds
