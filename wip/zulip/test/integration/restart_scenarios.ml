open Zulip_eio

let ok = function
  | Ok x -> x
  | Error e -> Alcotest.fail (Error.error_to_string e)

let run ~client ~sender ~clock ~channel =
  let helper =
    match Sys.getenv_opt "ZULIP_TEST_RESTART_HELPER" with
    | Some s -> s
    | None ->
        Alcotest.fail
          "Use zulip.sh run or eval the output of zulip.sh env for restart \
           testing"
  in
  Eio.Switch.run @@ fun sw ->
  Eio.Time.with_timeout_exn clock 180. @@ fun () ->
  let first, first_wake = Eio.Promise.create () in
  let replaced, replaced_wake = Eio.Promise.create () in
  let completed, completed_wake = Eio.Promise.create () in
  let registrations = ref 0 and recoveries = ref 0 in
  let content =
    Printf.sprintf "after restart %d %.6f" (Unix.getpid ())
      (Unix.gettimeofday ())
  in
  Eio.Fiber.fork ~sw (fun () ->
      let result =
        Event_queue.iter client
          ~event_types:[ Zulip.Event_type.Message ]
          ~on_recover:(fun _ -> incr recoveries)
          ~on_registered:(fun queue ->
            incr registrations;
            if !registrations = 1 then Eio.Promise.resolve first_wake queue
            else if !registrations = 2 then Eio.Promise.resolve replaced_wake ())
          (fun event ->
            match Zulip.Event_payload.of_event event with
            | Ok (Message m) when Zulip.Message.content m.message = content ->
                Event_queue.Stop
            | _ -> Event_queue.Continue)
      in
      Eio.Promise.resolve completed_wake result);
  let await_live promise =
    Eio.Fiber.first
      (fun () -> Eio.Promise.await promise)
      (fun () ->
        ignore (Eio.Promise.await completed |> ok);
        Alcotest.fail "collector exited before expected registration")
  in
  let original_queue = await_live first in
  Eio_unix.run_in_systhread (fun () ->
      let pid =
        Unix.create_process helper [| helper; "restart" |] Unix.stdin
          Unix.stdout Unix.stderr
      in
      match snd (Unix.waitpid [] pid) with
      | Unix.WEXITED 0 -> ()
      | _ -> failwith "local test-server restart failed");
  (* Zulip may preserve queues across graceful restarts. Explicitly expire the
     old queue as well, so both reconnection and fresh registration are checked. *)
  (match Event_queue.delete original_queue client with
  | Ok () -> ()
  | Error error when Error.is_bad_queue error -> ()
  | Error error -> ignore (ok (Error error)));
  await_live replaced;
  Messages.send_channel_id sender ~channel_id:channel ~topic:"restart test"
    ~content ()
  |> ok |> ignore;
  Eio.Promise.await completed |> ok;
  Alcotest.(check bool) "replacement queue registered" true (!registrations >= 2);
  Alcotest.(check bool) "collector reported recovery" true (!recoveries >= 1)
