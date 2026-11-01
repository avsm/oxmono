open! Core
open Bonsai_term
open Bonsai_test
module Effect = Bonsai_term.Effect

let settle h =
  for _ = 1 to 10 do
    Handle.recompute_view h
  done

let key h key =
  Bonsai_term_test.send_event h (Event.Key_press { key; mods = [] });
  settle h

let image h = Bonsai_term_test.last_view h |> View.Private.notty_image

let () =
  let calls = ref 0 and exited = ref false in
  let raw =
    {|{"time":0,"monotonic":1,"runtime":[{"session":"test","running":1,"heartbeat":0,"connections":3,"queued":2,"transfers":"[]"}],"totals":[{"requests":4,"hits":3,"misses":1,"shared":0,"served_bytes":1234,"errors":0}],"traffic":[{"fetched_bytes":999}],"requests":[{"cache":"HIT","status":206,"method":"GET","range_request":1,"served_bytes":42,"duration_us":100,"path":"/a/zarr.json"}],"downloads":[]}|}
  in
  let h =
    Bonsai_term_test.create_handle
      ~initial_dimensions:{ width = 100; height = 22 }
      (Zarrtop_ui.app ~interval:1.
         ~poll:(fun () ->
           Effect.of_sync_fun
             (fun () ->
               incr calls;
               Ok raw)
             ())
         ~exit:(fun () -> Effect.of_sync_fun (fun () -> exited := true) ()))
  in
  settle h;
  assert (!calls = 1);
  let request_view = image h in
  key h Tab;
  assert (not (Notty.I.equal request_view (image h)));
  key h Tab;
  key h Tab;
  List.iter [ Event.Key.ASCII '/'; ASCII 'q'; Enter ] ~f:(fun key ->
      Bonsai_term_test.send_event h (Event.Key_press { key; mods = [] }));
  settle h;
  assert (not !exited);
  assert (not (Notty.I.equal request_view (image h)));
  key h (ASCII '/');
  key h Backspace;
  key h Enter;
  key h (ASCII ' ');
  let count = !calls in
  Handle.advance_clock_by h (Time_ns.Span.of_sec 2.);
  settle h;
  assert (!calls = count);
  key h (ASCII ' ');
  Handle.advance_clock_by h (Time_ns.Span.of_sec 2.);
  settle h;
  assert (!calls > count);
  List.iter
    [
      { Dimensions.width = 18; height = 5 };
      { width = 1; height = 1 };
      { width = 80; height = 24 };
    ]
    ~f:(fun dimensions ->
      Bonsai_term_test.set_dimensions h dimensions;
      settle h;
      let view = Bonsai_term_test.last_view h in
      assert (View.width view <= dimensions.width);
      assert (View.height view <= dimensions.height));
  key h (ASCII 'q');
  assert !exited;
  let error_h =
    Bonsai_term_test.create_handle
      (Zarrtop_ui.app ~interval:1.
         ~poll:(fun () -> Effect.return (Error "Database unavailable"))
         ~exit:(fun () -> Effect.Ignore))
  in
  settle error_h;
  Bonsai_term_test.print_view (Bonsai_term_test.last_view h);
  print_endline
    "zarrtop polling, pause, views, filters, resizing and quit passed."
