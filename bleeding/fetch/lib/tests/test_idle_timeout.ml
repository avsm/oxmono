open Fetch

let check = Alcotest.check Alcotest.bool

let source actions =
  let flow = Eio_mock.Flow.make "idle-timeout" in
  Eio_mock.Flow.on_read flow actions;
  (flow :> Eio.Flow.source_ty Eio.Resource.t)

let read flow =
  let buffer = Cstruct.create 16 in
  Eio.Flow.single_read flow buffer

let held = `Run (fun () -> Eio.Fiber.await_cancel ())

let test_stalled_read () =
  Eio_mock.Backend.run_full @@ fun env ->
  let flow = source [ `Return "a"; held ] in
  let flow = with_idle_timeout ~clock:env#clock ~seconds:1. flow in
  check "first read completes" true (read flow = 1);
  match read flow with
  | exception Idle_timeout seconds ->
      check "duration retained" true (Float.equal seconds 1.)
  | _ -> Alcotest.fail "a stalled read did not time out"

let test_progress_resets_timeout () =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = env#clock in
  let delayed text =
    `Run (fun () ->
      Eio.Time.sleep clock 0.75;
      text)
  in
  let flow = source [ delayed "a"; delayed "b" ] in
  let flow = with_idle_timeout ~clock ~seconds:1. flow in
  check "first delayed read" true (read flow = 1);
  check "second gets a fresh deadline" true (read flow = 1)

let test_arguments () =
  Eio_mock.Backend.run_full @@ fun env ->
  List.iter
    (fun seconds ->
      check "invalid duration" true
        (match
           with_idle_timeout ~clock:env#clock ~seconds
             (Eio.Flow.string_source "")
         with
        | exception Invalid_argument _ -> true
        | _ -> false))
    [ -1.; Float.infinity; Float.nan ]

let () =
  Alcotest.run "fetch-idle-timeout"
    [
      ( "source",
        [
          Alcotest.test_case "stalled read" `Quick test_stalled_read;
          Alcotest.test_case "progress resets timeout" `Quick
            test_progress_resets_timeout;
          Alcotest.test_case "arguments" `Quick test_arguments;
        ] );
    ]
