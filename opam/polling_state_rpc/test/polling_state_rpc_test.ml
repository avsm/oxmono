open! Core
open! Async

module T = struct
  type t =
    { foo : string
    ; bar : int
    }
  [@@deriving sexp, legacy_diff, bin_io]
end

module T_incompatible = struct
  type t =
    { foo : string
    ; bar : int
    ; added_field : int
    }
  [@@deriving sexp, legacy_diff, bin_io]
end

let rpc =
  Polling_state_rpc.create
    ~name:"foo"
    ~version:0
    ~query_equal:[%equal: string]
    ~bin_query:[%bin_type_class: string]
    (module T)
;;

let incompatible_rpc =
  Polling_state_rpc.create
    ~name:"foo"
    ~version:0
    ~query_equal:[%equal: string]
    ~bin_query:[%bin_type_class: string]
    (module T_incompatible)
;;

let where_to_listen_for_unit_test =
  Tcp.Where_to_listen.bind_to Localhost On_port_chosen_by_os
;;

let make_server ?server_received_request ?(block = fun () -> Deferred.unit) () =
  let implementation =
    let items : T.t list =
      [ { foo = "abc"; bar = 32 }; { foo = "def"; bar = 25 }; { foo = "ghi"; bar = 22 } ]
    in
    let number_of_queries = ref 0 in
    let implementation _ query =
      let count = !number_of_queries in
      Int.incr number_of_queries;
      let found = List.find_exn items ~f:(fun { foo; _ } -> String.equal foo query) in
      Deferred.return { found with bar = found.bar + count }
    in
    Polling_state_rpc.implement
      rpc
      ~on_client_and_server_out_of_sync:
        (Expect_test_helpers_core.print_s ~hide_positions:true)
      ~for_first_request:implementation
      (fun _ query ->
         let%bind () =
           match server_received_request with
           | Some server_received_request -> Mvar.put server_received_request ()
           | None -> return ()
         in
         let%bind () = block () in
         implementation () query)
  in
  let open Deferred.Or_error.Let_syntax in
  let%map server =
    Deferred.ok
    @@ Rpc.Connection.serve
         ~implementations:
           (Rpc.Implementations.create_exn
              ~implementations:[ implementation ]
              ~on_unknown_rpc:`Close_connection
              ~on_exception:Log_on_background_exn)
         ~initial_connection_state:(fun _addr conn -> (), conn)
         ~where_to_listen:where_to_listen_for_unit_test
         ()
  in
  server, Tcp.Where_to_connect.of_inet_address (Tcp.Server.listening_on_address server)
;;

let make_client ?initial_query () =
  let introspect prev query diff =
    print_s
      [%message
        (prev : T.t option)
          (query : string)
          (diff : T.t Polling_state_rpc.Private_for_testing.Response.t)]
  in
  Polling_state_rpc.Private_for_testing.create_client ?initial_query rpc ~introspect
;;

let make_client_printing_terse_diffs ?initial_query () =
  let introspect _prev _query diff =
    let diff_bin_size =
      [%bin_size: T.t Polling_state_rpc.Private_for_testing.Response.t] diff
    in
    let is_fresh = Polling_state_rpc.Private_for_testing.Response.is_fresh diff in
    print_s [%message (is_fresh : bool) (diff_bin_size : int)]
  in
  Polling_state_rpc.Private_for_testing.create_client ?initial_query rpc ~introspect
;;

let make_incompatible_client ?initial_query () =
  let introspect prev query diff =
    print_s
      [%message
        (prev : T_incompatible.t option)
          (query : string)
          (diff : T_incompatible.t Polling_state_rpc.Private_for_testing.Response.t)]
  in
  Polling_state_rpc.Private_for_testing.create_client
    ?initial_query
    incompatible_rpc
    ~introspect
;;

let%expect_test "basic operations" =
  let open Deferred.Or_error.Let_syntax in
  Deferred.Or_error.ok_exn
  @@
  let%bind server, where_to_connect = make_server () in
  let%bind connection =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
  in
  let client = make_client () in
  let%bind response = Polling_state_rpc.Client.dispatch client connection "abc" in
  print_s [%sexp (response : T.t)];
  [%expect
    {|
    ((prev ()) (query abc) (diff (Fresh ((foo abc) (bar 32)))))
    ((foo abc) (bar 32))
    |}];
  let%bind response = Polling_state_rpc.Client.dispatch client connection "def" in
  print_s [%sexp (response : T.t)];
  [%expect
    {|
    ((prev (((foo abc) (bar 32)))) (query def)
     (diff (Update ((Foo def) (Bar 26)))))
    ((foo def) (bar 26))
    |}];
  let%bind response = Polling_state_rpc.Client.redispatch client connection in
  print_s [%sexp (response : T.t)];
  [%expect
    {|
    ((prev (((foo def) (bar 26)))) (query def) (diff (Update ((Bar 27)))))
    ((foo def) (bar 27))
    |}];
  let%bind () = Deferred.ok (Async.Tcp.Server.close server) in
  let%bind () = Deferred.ok (Async.Tcp.Server.close_finished server) in
  Deferred.ok (Async.Scheduler.yield_until_no_jobs_remain ())
;;

let%expect_test "terse diffs" =
  let open Deferred.Or_error.Let_syntax in
  Deferred.Or_error.ok_exn
  @@
  let%bind server, where_to_connect = make_server () in
  let%bind connection =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
  in
  let client = make_client_printing_terse_diffs () in
  let%bind _ = Polling_state_rpc.Client.dispatch client connection "abc" in
  [%expect {| ((is_fresh true) (diff_bin_size 9)) |}];
  let%bind _ = Polling_state_rpc.Client.dispatch client connection "def" in
  [%expect {| ((is_fresh false) (diff_bin_size 12)) |}];
  let%bind _ = Polling_state_rpc.Client.redispatch client connection in
  [%expect {| ((is_fresh false) (diff_bin_size 7)) |}];
  let%bind () = Deferred.ok (Async.Tcp.Server.close server) in
  let%bind () = Deferred.ok (Async.Tcp.Server.close_finished server) in
  Deferred.ok (Async.Scheduler.yield_until_no_jobs_remain ())
;;

let%expect_test "initial-query" =
  let open Deferred.Or_error.Let_syntax in
  Deferred.Or_error.ok_exn
  @@
  let%bind server, where_to_connect = make_server () in
  let%bind connection =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
  in
  let client = make_client ~initial_query:"abc" () in
  let%bind response = Polling_state_rpc.Client.redispatch client connection in
  print_s [%sexp (response : T.t)];
  [%expect
    {|
    ((prev ()) (query abc) (diff (Fresh ((foo abc) (bar 32)))))
    ((foo abc) (bar 32))
    |}];
  let%bind () = Deferred.ok (Async.Tcp.Server.close server) in
  let%bind () = Deferred.ok (Async.Tcp.Server.close_finished server) in
  Deferred.ok (Async.Scheduler.yield_until_no_jobs_remain ())
;;

let%expect_test "simulate server restarting " =
  let open Deferred.Or_error.Let_syntax in
  (* use two servers to simulate the server restarting *)
  Deferred.Or_error.ok_exn
  @@
  let%bind _server1, where_to_connect1 = make_server () in
  let%bind _server2, where_to_connect2 = make_server () in
  let%bind connection1 =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect1)
  in
  let%bind connection2 =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect2)
  in
  let client = make_client () in
  (* using the first server *)
  let%bind response = Polling_state_rpc.Client.dispatch client connection1 "abc" in
  print_s [%sexp (response : T.t)];
  [%expect
    {|
    ((prev ()) (query abc) (diff (Fresh ((foo abc) (bar 32)))))
    ((foo abc) (bar 32))
    |}];
  (* using the second server *)
  let%bind response = Polling_state_rpc.Client.redispatch client connection2 in
  print_s [%sexp (response : T.t)];
  [%expect
    {|
    ((prev (((foo abc) (bar 32)))) (query abc)
     (diff (Fresh ((foo abc) (bar 32)))))
    ((foo abc) (bar 32))
    |}];
  (* notice that "bar" didn't change to 33 even though it otherwise would have been *)
  Deferred.ok (Async.Scheduler.yield_until_no_jobs_remain ())
;;

let%expect_test "multiple clients on the same connection" =
  let open Deferred.Or_error.Let_syntax in
  Deferred.Or_error.ok_exn
  @@
  let%bind server, where_to_connect = make_server () in
  let%bind connection =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
  in
  let client1 = make_client () in
  let client2 = make_client () in
  let%bind response = Polling_state_rpc.Client.dispatch client1 connection "abc" in
  print_s [%sexp (response : T.t)];
  [%expect
    {|
    ((prev ()) (query abc) (diff (Fresh ((foo abc) (bar 32)))))
    ((foo abc) (bar 32))
    |}];
  let%bind response = Polling_state_rpc.Client.dispatch client2 connection "abc" in
  print_s [%sexp (response : T.t)];
  [%expect
    {|
    ((prev ()) (query abc) (diff (Fresh ((foo abc) (bar 33)))))
    ((foo abc) (bar 33))
    |}];
  let%bind response = Polling_state_rpc.Client.dispatch client1 connection "def" in
  print_s [%sexp (response : T.t)];
  [%expect
    {|
    ((prev (((foo abc) (bar 32)))) (query def)
     (diff (Update ((Foo def) (Bar 27)))))
    ((foo def) (bar 27))
    |}];
  let%bind response = Polling_state_rpc.Client.dispatch client2 connection "def" in
  print_s [%sexp (response : T.t)];
  [%expect
    {|
    ((prev (((foo abc) (bar 33)))) (query def)
     (diff (Update ((Foo def) (Bar 28)))))
    ((foo def) (bar 28))
    |}];
  let%bind () = Deferred.ok (Async.Tcp.Server.close server) in
  let%bind () = Deferred.ok (Async.Tcp.Server.close_finished server) in
  Deferred.ok (Async.Scheduler.yield_until_no_jobs_remain ())
;;

let%expect_test "simulate client disconnecting and reconnecting" =
  let open Deferred.Or_error.Let_syntax in
  Deferred.Or_error.ok_exn
  @@
  let%bind server, where_to_connect = make_server () in
  let%bind connection =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
  in
  let client1 = make_client () in
  let%bind response = Polling_state_rpc.Client.dispatch client1 connection "abc" in
  print_s [%sexp (response : T.t)];
  [%expect
    {|
    ((prev ()) (query abc) (diff (Fresh ((foo abc) (bar 32)))))
    ((foo abc) (bar 32))
    |}];
  let%bind () = Deferred.ok (Rpc.Connection.close connection) in
  let%bind () = Deferred.ok (Rpc.Connection.close_finished connection) in
  let%bind () = Deferred.ok (Async.Scheduler.yield_until_no_jobs_remain ()) in
  let%bind connection =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
  in
  let%bind response = Polling_state_rpc.Client.dispatch client1 connection "def" in
  print_s [%sexp (response : T.t)];
  (* notice that it's fresh now. This implies that the user was removed from the cache. *)
  [%expect
    {|
    ((prev (((foo abc) (bar 32)))) (query def)
     (diff (Fresh ((foo def) (bar 26)))))
    ((foo def) (bar 26))
    |}];
  let%bind response = Polling_state_rpc.Client.dispatch client1 connection "ghi" in
  print_s [%sexp (response : T.t)];
  [%expect
    {|
    ((prev (((foo def) (bar 26)))) (query ghi)
     (diff (Update ((Foo ghi) (Bar 24)))))
    ((foo ghi) (bar 24))
    |}];
  let%bind () = Deferred.ok (Async.Tcp.Server.close server) in
  let%bind () = Deferred.ok (Async.Tcp.Server.close_finished server) in
  Deferred.ok (Async.Scheduler.yield_until_no_jobs_remain ())
;;

let%expect_test "server throws exception" =
  let open Deferred.Or_error.Let_syntax in
  Deferred.Or_error.ok_exn
  @@
  let%bind server, where_to_connect = make_server () in
  let%bind connection =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
  in
  let client1 = make_client () in
  let%bind response =
    Deferred.ok (Polling_state_rpc.Client.dispatch client1 connection "xyz")
  in
  let response_is_error = Or_error.is_error response in
  print_s [%message (response_is_error : bool)];
  [%expect {| (response_is_error true) |}];
  let%bind () = Deferred.ok (Async.Tcp.Server.close server) in
  let%bind () = Deferred.ok (Async.Tcp.Server.close_finished server) in
  Deferred.ok (Async.Scheduler.yield_until_no_jobs_remain ())
;;

let%expect_test "change query at a time that would otherwise cause blocking" =
  let open Deferred.Or_error.Let_syntax in
  Deferred.Or_error.ok_exn
  @@
  let%bind server, where_to_connect =
    (* The server always blocks forever on the second (and third, and fourth, and ...)
       requests for the same query. *)
    make_server ~block:(fun () -> Deferred.never ()) ()
  in
  let%bind connection =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
  in
  let client = make_client () in
  let%bind response = Polling_state_rpc.Client.dispatch client connection "abc" in
  print_s [%sexp (response : T.t)];
  [%expect
    {|
    ((prev ()) (query abc) (diff (Fresh ((foo abc) (bar 32)))))
    ((foo abc) (bar 32))
    |}];
  (* Dispatching again on the same client changes the query. Thus, the fact that we get a
     response from this second dispatch shows that the server is going through the "first
     request" code path for the latest poll request. *)
  let%bind response = Polling_state_rpc.Client.dispatch client connection "def" in
  print_s [%sexp (response : T.t)];
  [%expect
    {|
    ((prev (((foo abc) (bar 32)))) (query def)
     (diff (Update ((Foo def) (Bar 26)))))
    ((foo def) (bar 26))
    |}];
  let%bind () = Deferred.ok (Async.Tcp.Server.close server) in
  let%bind () = Deferred.ok (Async.Tcp.Server.close_finished server) in
  Deferred.ok (Async.Scheduler.yield_until_no_jobs_remain ())
;;

let%expect_test "demonstrate that changing the query will cancel an ongoing request." =
  let open Deferred.Or_error.Let_syntax in
  let block = Bvar.create () in
  Deferred.Or_error.ok_exn
  @@
  let%bind server, where_to_connect =
    make_server ~block:(fun _query -> Bvar.wait block) ()
  in
  let%bind connection =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
  in
  let client = make_client () in
  let%bind response = Polling_state_rpc.Client.dispatch client connection "abc" in
  print_s [%sexp (response : T.t)];
  [%expect
    {|
    ((prev ()) (query abc) (diff (Fresh ((foo abc) (bar 32)))))
    ((foo abc) (bar 32))
    |}];
  (* Fire off another rpc with the same query to get the blocking behavior. *)
  let unbound_response = Polling_state_rpc.Client.dispatch client connection "abc" in
  let%bind response = Polling_state_rpc.Client.dispatch client connection "def" in
  print_s [%sexp (response : T.t)];
  [%expect
    {|
    ((prev (((foo abc) (bar 32)))) (query def)
     (diff (Update ((Foo def) (Bar 26)))))
    ((foo def) (bar 26))
    |}];
  let%bind.Deferred earlier_response = unbound_response in
  print_s [%sexp (earlier_response : T.t Or_error.t)];
  [%expect {| (Error "Request aborted") |}];
  Bvar.broadcast block ();
  let%bind () = Deferred.ok (Async.Tcp.Server.close server) in
  let%bind () = Deferred.ok (Async.Tcp.Server.close_finished server) in
  Deferred.ok (Async.Scheduler.yield_until_no_jobs_remain ())
;;

(* This test may seem silly at first glance, but there was previously a bug which caused a
   race condition in the following logic. Soak-testing this inline test failed previously,
   so we leave this test case for further soak-testing. *)

let%expect_test "redispatch does not race with dispatch if dispatch is called \
                 immediately afterwards"
  =
  let run_once () =
    let open Deferred.Or_error.Let_syntax in
    Deferred.Or_error.ok_exn
    @@
    let%bind server, where_to_connect = make_server () in
    let%bind connection =
      Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
    in
    let client = make_client () in
    (* Fire off an rpc so that redispatch can be called *)
    let%bind response = Polling_state_rpc.Client.dispatch client connection "abc" in
    print_s [%sexp (response : T.t)];
    [%expect
      {|
      ((prev ()) (query abc) (diff (Fresh ((foo abc) (bar 32)))))
      ((foo abc) (bar 32))
      |}];
    (* We fire off a redispatch and then a dispatch request. We expect that after these
       requests are completed, the query should be that of the dispatch operation: "def". *)
    let redispatch_response = Polling_state_rpc.Client.redispatch client connection in
    let dispatch_response = Polling_state_rpc.Client.dispatch client connection "def" in
    (* Both of the responses look as expected. *)
    let%bind.Deferred redispatch_response in
    print_s [%sexp (redispatch_response : T.t Or_error.t)];
    [%expect {| (Error "Request aborted") |}];
    let%bind.Deferred dispatch_response in
    print_s [%sexp (dispatch_response : T.t Or_error.t)];
    [%expect
      {|
      ((prev (((foo abc) (bar 32)))) (query def)
       (diff (Update ((Foo def) (Bar 27)))))
      (Ok ((foo def) (bar 27)))
      |}];
    (* And doing one more redispatch shows us that the query was not modified by the
       redispatch query. *)
    let%bind.Deferred response = Polling_state_rpc.Client.redispatch client connection in
    print_s [%sexp (response : T.t Or_error.t)];
    [%expect
      {|
      ((prev (((foo def) (bar 27)))) (query def) (diff (Update ((Bar 28)))))
      (Ok ((foo def) (bar 28)))
      |}];
    let%bind () = Deferred.ok (Async.Tcp.Server.close server) in
    let%bind () = Deferred.ok (Async.Tcp.Server.close_finished server) in
    Deferred.ok (Async.Scheduler.yield_until_no_jobs_remain ())
  in
  (* The test is run 10 times to increase the chance of catching a future regression *)
  let i = ref 0 in
  while%bind return (!i < 10) do
    incr i;
    run_once ()
  done
;;

let%expect_test "[forget_on_server] does not clear the current query." =
  let open Deferred.Or_error.Let_syntax in
  Deferred.Or_error.ok_exn
  @@
  let%bind server, where_to_connect = make_server () in
  let%bind connection =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
  in
  let client = make_client () in
  let%bind _response = Polling_state_rpc.Client.dispatch client connection "abc" in
  [%expect {| ((prev ()) (query abc) (diff (Fresh ((foo abc) (bar 32))))) |}];
  let%bind () = Polling_state_rpc.Client.forget_on_server client connection in
  let%bind.Deferred response = Polling_state_rpc.Client.redispatch client connection in
  print_s [%sexp (response : T.t Or_error.t)];
  [%expect
    {|
    ((prev (((foo abc) (bar 32)))) (query abc)
     (diff (Fresh ((foo abc) (bar 33)))))
    (Ok ((foo abc) (bar 33)))
    |}];
  let%bind () = Deferred.ok (Async.Tcp.Server.close server) in
  let%bind () = Deferred.ok (Async.Tcp.Server.close_finished server) in
  Deferred.ok (Async.Scheduler.yield_until_no_jobs_remain ())
;;

let%expect_test "[forget_on_server] cancels an ongoing request." =
  let open Deferred.Or_error.Let_syntax in
  let block = Bvar.create () in
  Deferred.Or_error.ok_exn
  @@
  let%bind server, where_to_connect =
    make_server ~block:(fun _query -> Bvar.wait block) ()
  in
  let%bind connection =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
  in
  let client = make_client () in
  let%bind response = Polling_state_rpc.Client.dispatch client connection "abc" in
  print_s [%sexp (response : T.t)];
  [%expect
    {|
    ((prev ()) (query abc) (diff (Fresh ((foo abc) (bar 32)))))
    ((foo abc) (bar 32))
    |}];
  (* Fire off another rpc with the same query to get the blocking behavior. *)
  let unbound_response = Polling_state_rpc.Client.dispatch client connection "abc" in
  let%bind () = Polling_state_rpc.Client.forget_on_server client connection in
  let%bind.Deferred earlier_response = unbound_response in
  print_s [%sexp (earlier_response : T.t Or_error.t)];
  [%expect {| (Error "Request aborted") |}];
  Bvar.broadcast block ();
  let%bind () = Deferred.ok (Async.Tcp.Server.close server) in
  let%bind () = Deferred.ok (Async.Tcp.Server.close_finished server) in
  Deferred.ok (Async.Scheduler.yield_until_no_jobs_remain ())
;;

let%expect_test "[forget_on_server] does not affect any other clients." =
  let open Deferred.Or_error.Let_syntax in
  Deferred.Or_error.ok_exn
  @@
  let%bind server, where_to_connect = make_server () in
  let%bind connection =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
  in
  let client1 = make_client () in
  let client2 = make_client () in
  let%bind response = Polling_state_rpc.Client.dispatch client1 connection "abc" in
  print_s [%sexp (response : T.t)];
  [%expect
    {|
    ((prev ()) (query abc) (diff (Fresh ((foo abc) (bar 32)))))
    ((foo abc) (bar 32))
    |}];
  let%bind response = Polling_state_rpc.Client.dispatch client2 connection "abc" in
  print_s [%sexp (response : T.t)];
  [%expect
    {|
    ((prev ()) (query abc) (diff (Fresh ((foo abc) (bar 33)))))
    ((foo abc) (bar 33))
    |}];
  let%bind () = Polling_state_rpc.Client.forget_on_server client1 connection in
  let%bind response = Polling_state_rpc.Client.dispatch client1 connection "abc" in
  print_s [%sexp (response : T.t)];
  [%expect
    {|
    ((prev (((foo abc) (bar 32)))) (query abc)
     (diff (Fresh ((foo abc) (bar 34)))))
    ((foo abc) (bar 34))
    |}];
  let%bind response = Polling_state_rpc.Client.dispatch client2 connection "abc" in
  print_s [%sexp (response : T.t)];
  [%expect
    {|
    ((prev (((foo abc) (bar 33)))) (query abc) (diff (Update ((Bar 35)))))
    ((foo abc) (bar 35))
    |}];
  let%bind () = Deferred.ok (Async.Tcp.Server.close server) in
  let%bind () = Deferred.ok (Async.Tcp.Server.close_finished server) in
  Deferred.ok (Async.Scheduler.yield_until_no_jobs_remain ())
;;

module Block : sig
  type t

  val create : unit -> t
  val block : t -> unit
  val unblock : t -> unit Deferred.t
  val unblocked : t -> unit Deferred.t
end = struct
  type t =
    { mutable is_blocked : bool
    ; unblocked_mvar : unit Mvar.Read_write.t
    }

  let create () = { is_blocked = false; unblocked_mvar = Mvar.create () }
  let block t = t.is_blocked <- true

  let unblock t =
    t.is_blocked <- false;
    Mvar.put t.unblocked_mvar ()
  ;;

  let unblocked t = if t.is_blocked then Mvar.take t.unblocked_mvar else return ()
end

type connection =
  { connection : Rpc.Connection.t
  ; reader_block : Block.t
  ; writer_block : Block.t
  }

let make_connection where_to_connect =
  let reader_block = Block.create () in
  let writer_block = Block.create () in
  let reader_r, reader_w = Pipe.create () in
  let writer_r, writer_w = Pipe.create () in
  let%bind reader_r = Reader.of_pipe (Info.of_string "reader_r") reader_r in
  let%bind writer_w, _ = Writer.of_pipe (Info.of_string "writer_w") writer_w in
  let make_transport fd ~max_message_size =
    let fd_reader = Reader.create fd in
    let fd_writer = Writer.create fd in
    don't_wait_for
      (Pipe.iter (Reader.pipe fd_reader) ~f:(fun message ->
         let%bind () = Block.unblocked reader_block in
         Pipe.write reader_w message));
    don't_wait_for
      (Pipe.iter writer_r ~f:(fun message ->
         let%bind () = Block.unblocked writer_block in
         Pipe.write (Writer.pipe fd_writer) message));
    Rpc.Transport.of_reader_writer ~max_message_size reader_r writer_w
  in
  let%map connection =
    Rpc.Connection.client ~make_transport where_to_connect
    |> Deferred.Or_error.of_exn_result
    >>| Or_error.ok_exn
  in
  { connection; writer_block; reader_block }
;;

let%expect_test "demonstrate that changing the query will abort any un-dispatched RPCs." =
  let bvar = Bvar.create () in
  let server_received_request = Mvar.create () in
  let%bind server, where_to_connect =
    make_server ~server_received_request ~block:(fun _query -> Bvar.wait bvar) ()
    >>| Or_error.ok_exn
  in
  let%bind { connection; writer_block; _ } = make_connection where_to_connect in
  let client = make_client () in
  let%bind response = Polling_state_rpc.Client.dispatch client connection "abc" in
  print_s [%sexp (response : T.t Or_error.t)];
  [%expect
    {|
    ((prev ()) (query abc) (diff (Fresh ((foo abc) (bar 32)))))
    (Ok ((foo abc) (bar 32)))
    |}];
  (* Fire off another rpc with the same query to get the blocking behavior. *)
  let response1 = Polling_state_rpc.Client.dispatch client connection "abc" in
  let%bind () = Mvar.take server_received_request in
  let response2 = Polling_state_rpc.Client.dispatch client connection "abc" in
  Block.block writer_block;
  let response3 = Polling_state_rpc.Client.dispatch client connection "def" in
  Bvar.broadcast bvar ();
  [%expect {| |}];
  let%bind response1 in
  [%expect {| ((prev (((foo abc) (bar 32)))) (query abc) (diff (Update ((Bar 33))))) |}];
  print_s [%sexp (response1 : T.t Or_error.t)];
  [%expect {| (Ok ((foo abc) (bar 33))) |}];
  let%bind () = Block.unblock writer_block in
  let%bind response2 in
  print_s [%sexp (response2 : T.t Or_error.t)];
  [%expect {| (Error "Request aborted") |}];
  let%bind response3 in
  print_s [%sexp (response3 : T.t Or_error.t)];
  [%expect
    {|
    ((prev (((foo abc) (bar 33)))) (query def)
     (diff (Update ((Foo def) (Bar 27)))))
    (Ok ((foo def) (bar 27)))
    |}];
  let%bind () = Async.Tcp.Server.close server in
  let%bind () = Async.Tcp.Server.close_finished server in
  Async.Scheduler.yield_until_no_jobs_remain ()
;;

let%expect_test "demonstrate that an [rpc_error] triggers a bug message." =
  let%bind server, where_to_connect = make_server () >>| Or_error.ok_exn in
  let%bind connection =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
    >>| Or_error.ok_exn
  in
  let client = make_incompatible_client () in
  let%bind response = Polling_state_rpc.Client.dispatch client connection "abc" in
  Expect_test_helpers_core.print_s
    [%sexp (response : T_incompatible.t Or_error.t)]
    ~hide_positions:true;
  [%expect
    {|
    (Error (
      (rpc_error (
        Bin_io_exn (
          (location "client-side rpc response un-bin-io'ing")
          (exn (
            Failure
            "lib/async_rpc/kernel/src/rpc.ml:LINE:COL message length (9) did not match expected length (8)")))))
      (connection_description ("Client connected via TCP" 127.0.0.1:PORT))
      (rpc_name    foo)
      (rpc_version 0)))
    |}];
  let%bind response1 = Polling_state_rpc.Client.dispatch client connection "abc" in
  Expect_test_helpers_core.print_s
    [%sexp (response1 : T_incompatible.t Or_error.t)]
    ~hide_positions:true;
  [%expect
    {|
    (lib/polling_state_rpc/src/polling_state_rpc.ml:LINE:COL
     "A polling state RPC client has requested a fresh response, but the server expected it to have the seqnum of the latest diffs. The server will send a fresh response as requested. This likely means that the client had trouble receiving the last RPC response."
     (rpc_name    foo)
     (rpc_version 0))
    (Error (
      (rpc_error (
        Bin_io_exn (
          (location "client-side rpc response un-bin-io'ing")
          (exn (
            Failure
            "lib/async_rpc/kernel/src/rpc.ml:LINE:COL message length (9) did not match expected length (8)")))))
      (connection_description ("Client connected via TCP" 127.0.0.1:PORT))
      (rpc_name    foo)
      (rpc_version 0)))
    |}];
  let%bind response2 = Polling_state_rpc.Client.dispatch client connection "def" in
  Expect_test_helpers_core.print_s
    [%sexp (response2 : T_incompatible.t Or_error.t)]
    ~hide_positions:true;
  [%expect
    {|
    (lib/polling_state_rpc/src/polling_state_rpc.ml:LINE:COL
     "A polling state RPC client has requested a fresh response, but the server expected it to have the seqnum of the latest diffs. The server will send a fresh response as requested. This likely means that the client had trouble receiving the last RPC response."
     (rpc_name    foo)
     (rpc_version 0))
    (Error (
      (rpc_error (
        Bin_io_exn (
          (location "client-side rpc response un-bin-io'ing")
          (exn (
            Failure
            "lib/async_rpc/kernel/src/rpc.ml:LINE:COL message length (9) did not match expected length (8)")))))
      (connection_description ("Client connected via TCP" 127.0.0.1:PORT))
      (rpc_name    foo)
      (rpc_version 0)))
    |}];
  let%bind () = Async.Tcp.Server.close server in
  let%bind () = Async.Tcp.Server.close_finished server in
  Async.Scheduler.yield_until_no_jobs_remain ()
;;

let make_server_with_client_state
  ?server_received_request
  ?(block = fun () -> Deferred.unit)
  ()
  =
  let make_counter () =
    let count = ref 0 in
    fun () ->
      count := !count + 1;
      !count
  in
  let implementation =
    let items : T.t list =
      [ { foo = "abc"; bar = 32 }; { foo = "def"; bar = 25 }; { foo = "ghi"; bar = 22 } ]
    in
    let number_of_queries = ref 0 in
    let implementation _ _ query =
      let count = !number_of_queries in
      incr number_of_queries;
      let found = List.find_exn items ~f:(fun { foo; _ } -> String.equal foo query) in
      Deferred.return { found with bar = found.bar + count }
    in
    Polling_state_rpc.implement_with_client_state
      rpc
      ~on_client_and_server_out_of_sync:
        (Expect_test_helpers_core.print_s ~hide_positions:true)
      ~create_client_state:(fun (connection_number, client_counter) ->
        let client_number = client_counter () in
        let client_string = [%string "%{connection_number#Int}:%{client_number#Int}"] in
        print_string [%string "%{client_string} created.\n"];
        client_string)
      ~on_client_forgotten:(fun client_string ->
        print_string [%string "%{client_string} forgotten.\n"])
      ~for_first_request:implementation
      (fun _ _ query ->
        let%bind () =
          match server_received_request with
          | Some server_received_request -> Mvar.put server_received_request ()
          | None -> return ()
        in
        let%bind () = block () in
        implementation () () query)
  in
  let connection_counter = make_counter () in
  Pipe_transport_server.create
    ~implementations:
      (Rpc.Implementations.create_exn
         ~implementations:[ implementation ]
         ~on_unknown_rpc:`Close_connection
         ~on_exception:Log_on_background_exn)
    ~initial_connection_state:(fun conn -> (connection_counter (), make_counter ()), conn)
;;

let%expect_test "demonstrate client state with destructor" =
  let server = make_server_with_client_state () in
  let%bind connection1 = Pipe_transport_server.client_connection server in
  let client_1_1 = make_client () in
  let client_1_2 = make_client () in
  let client_2_1 = make_client () in
  let%bind response = Polling_state_rpc.Client.dispatch client_1_1 connection1 "abc" in
  print_s [%sexp (response : T.t Or_error.t)];
  [%expect
    {|
    1:1 created.
    ((prev ()) (query abc) (diff (Fresh ((foo abc) (bar 32)))))
    (Ok ((foo abc) (bar 32)))
    |}];
  let%bind response = Polling_state_rpc.Client.dispatch client_1_2 connection1 "def" in
  print_s [%sexp (response : T.t Or_error.t)];
  [%expect
    {|
    1:2 created.
    ((prev ()) (query def) (diff (Fresh ((foo def) (bar 26)))))
    (Ok ((foo def) (bar 26)))
    |}];
  let%bind connection2 = Pipe_transport_server.client_connection server in
  let%bind response = Polling_state_rpc.Client.dispatch client_2_1 connection2 "ghi" in
  print_s [%sexp (response : T.t Or_error.t)];
  [%expect
    {|
    2:1 created.
    ((prev ()) (query ghi) (diff (Fresh ((foo ghi) (bar 24)))))
    (Ok ((foo ghi) (bar 24)))
    |}];
  let%bind () =
    Polling_state_rpc.Client.forget_on_server client_1_1 connection1 >>| Or_error.ok_exn
  in
  [%expect {| 1:1 forgotten. |}];
  let%bind () = Rpc.Connection.close connection1 in
  let%bind () = Rpc.Connection.close_finished connection1 in
  let%bind () = Async.Scheduler.yield_until_no_jobs_remain () in
  [%expect {| 1:2 forgotten. |}];
  Pipe_transport_server.shutdown server;
  let%bind () = Async.Scheduler.yield_until_no_jobs_remain () in
  [%expect {| 2:1 forgotten. |}];
  return ()
;;

module%test [@name "implement_via_bus"] _ = struct
  module Response = struct
    include Int
    include Legacy_diffable.Atomic.Make (Int)
  end

  let rpc =
    Polling_state_rpc.create
      ~name:"foo"
      ~version:0
      ~query_equal:[%equal: bool]
      ~bin_query:[%bin_type_class: bool]
      (module Response)
  ;;

  let with_connection_to_server implementation ~f =
    let%bind server, where_to_connect =
      let%map server =
        Rpc.Connection.serve
          ~implementations:
            (Rpc.Implementations.create_exn
               ~implementations:[ implementation ]
               ~on_unknown_rpc:`Close_connection
               ~on_exception:Log_on_background_exn)
          ~initial_connection_state:(fun _addr conn -> (), conn)
          ~where_to_listen:where_to_listen_for_unit_test
          ()
      in
      ( server
      , Tcp.Where_to_connect.of_inet_address (Tcp.Server.listening_on_address server) )
    in
    let%bind connection =
      Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
      |> Deferred.Or_error.ok_exn
    in
    let%bind () = f connection in
    let%bind () = Async.Tcp.Server.close server in
    let%bind () = Async.Tcp.Server.close_finished server in
    Async.Scheduler.yield_until_no_jobs_remain ()
  ;;

  let rec actually_yield_until_no_jobs_remain () =
    let%bind () = Async.Scheduler.yield_until_no_jobs_remain () in
    match Async.Scheduler.num_pending_jobs () with
    | 0 -> return ()
    | _ -> actually_yield_until_no_jobs_remain ()
  ;;

  let make_client () =
    let introspect prev query diff =
      print_s
        [%message
          (prev : int option)
            (query : bool)
            (diff : int Polling_state_rpc.Private_for_testing.Response.t)]
    in
    Polling_state_rpc.Private_for_testing.create_client rpc ~introspect
  ;;

  let%expect_test "single client, single query" =
    let bus =
      Bus.create_exn
        ~on_subscription_after_first_write:Allow_and_send_last_value_if_global
        ~on_callback_raise:(fun error -> print_s [%message (error : Error.t)])
        ()
    in
    let implementation =
      Polling_state_rpc.implement_via_bus
        ~create_client_state:(fun _connection_state -> ())
        ~on_client_and_server_out_of_sync:
          (Expect_test_helpers_core.print_s ~hide_positions:true)
        rpc
        (fun _connection_state _client_state _query -> return bus)
    in
    with_connection_to_server implementation ~f:(fun connection ->
      let client = make_client () in
      let query () = Polling_state_rpc.Client.dispatch client connection true in
      let push n = Bus.write bus n in
      let response = query () in
      let%bind () = actually_yield_until_no_jobs_remain () in
      push 0;
      [%expect {| |}];
      let%bind response in
      print_s [%sexp (response : int Or_error.t)];
      [%expect
        {|
        ((prev ()) (query true) (diff (Fresh 0)))
        (Ok 0)
        |}];
      let response = query () in
      let%bind () = actually_yield_until_no_jobs_remain () in
      [%expect {| |}];
      push 1;
      let%bind response in
      print_s [%sexp (response : int Or_error.t)];
      [%expect
        {|
        ((prev (0)) (query true) (diff (Update (1))))
        (Ok 1)
        |}];
      push 2;
      push 3;
      let%bind response = query () in
      print_s [%sexp (response : int Or_error.t)];
      [%expect
        {|
        ((prev (1)) (query true) (diff (Update (3))))
        (Ok 3)
        |}];
      let response1 = query () in
      push 4;
      push 5;
      let%bind () = actually_yield_until_no_jobs_remain () in
      let response2 = query () in
      let%bind response1 in
      print_s [%sexp (response1 : int Or_error.t)];
      [%expect
        {|
        ((prev (3)) (query true) (diff (Update (5))))
        (Ok 5)
        |}];
      let%bind () = actually_yield_until_no_jobs_remain () in
      push 6;
      let%bind response2 in
      print_s [%sexp (response2 : int Or_error.t)];
      [%expect
        {|
        ((prev (5)) (query true) (diff (Update (6))))
        (Ok 6)
        |}];
      return ())
  ;;

  let%expect_test "single client, multiple queries" =
    let true_bus =
      Bus.create_exn
        ~on_subscription_after_first_write:Allow_and_send_last_value_if_global
        ~on_callback_raise:(fun error -> print_s [%message (error : Error.t)])
        ()
    in
    let false_bus =
      Bus.create_exn
        ~on_subscription_after_first_write:Allow_and_send_last_value_if_global
        ~on_callback_raise:(fun error -> print_s [%message (error : Error.t)])
        ()
    in
    let implementation =
      Polling_state_rpc.implement_via_bus
        ~create_client_state:(fun _connection_state -> ())
        ~on_client_and_server_out_of_sync:
          (Expect_test_helpers_core.print_s ~hide_positions:true)
        rpc
        (fun _connection_state _client_state query ->
          if query then return true_bus else return false_bus)
    in
    with_connection_to_server implementation ~f:(fun connection ->
      let query client query =
        Polling_state_rpc.Client.dispatch client connection query
      in
      let push query n = Bus.write (if query then true_bus else false_bus) n in
      let client = make_client () in
      (* make an initial query *)
      push true 0;
      let%bind response = query client true in
      print_s [%sexp (response : int Or_error.t)];
      [%expect
        {|
        ((prev ()) (query true) (diff (Fresh 0)))
        (Ok 0)
        |}];
      (* redispatch and wait for a response *)
      let response = query client true in
      push true 1;
      let%bind response in
      print_s [%sexp (response : int Or_error.t)];
      [%expect
        {|
        ((prev (0)) (query true) (diff (Update (1))))
        (Ok 1)
        |}];
      (* switch to a different query *)
      let response = query client false in
      push false 2;
      let%bind () = actually_yield_until_no_jobs_remain () in
      [%expect {| ((prev (1)) (query false) (diff (Update (2)))) |}];
      let%bind response in
      print_s [%sexp (response : int Or_error.t)];
      [%expect {| (Ok 2) |}];
      (* redispatch a query, but abort before it resolves *)
      let aborted = query client false in
      let%bind () =
        (* wait for the server to receive the response that we're about to abort *)
        Async.Scheduler.yield_until_no_jobs_remain ()
      in
      let%bind response = query client true in
      print_s [%sexp (response : int Or_error.t)];
      [%expect
        {|
        ((prev (2)) (query true) (diff (Update (1))))
        (Ok 1)
        |}];
      let response = query client true in
      push true 3;
      let%bind response in
      print_s [%sexp (response : int Or_error.t)];
      [%expect
        {|
        ((prev (1)) (query true) (diff (Update (3))))
        (Ok 3)
        |}];
      (* ensure that the aborted request eventually resolves *)
      let%bind aborted in
      print_s [%sexp (aborted : int Or_error.t)];
      [%expect {| (Error "Request aborted") |}];
      return ())
  ;;

  let%expect_test "multiple clients, single query" =
    let bus =
      Bus.create_exn
        ~on_subscription_after_first_write:Allow_and_send_last_value_if_global
        ~on_callback_raise:(fun error -> print_s [%message (error : Error.t)])
        ()
    in
    let implementation =
      Polling_state_rpc.implement_via_bus
        ~create_client_state:(fun _connection_state -> ())
        ~on_client_and_server_out_of_sync:
          (Expect_test_helpers_core.print_s ~hide_positions:true)
        rpc
        (fun _connection_state _client_state _query -> return bus)
    in
    with_connection_to_server implementation ~f:(fun connection ->
      let query client = Polling_state_rpc.Client.dispatch client connection true in
      let push n = Bus.write bus n in
      let client1 = make_client () in
      let client2 = make_client () in
      push 0;
      let%bind response1 = query client1 in
      print_s [%sexp (response1 : int Or_error.t)];
      [%expect
        {|
        ((prev ()) (query true) (diff (Fresh 0)))
        (Ok 0)
        |}];
      let response2 = query client1 in
      let%bind () = actually_yield_until_no_jobs_remain () in
      [%expect {| |}];
      let%bind response3 = query client2 in
      print_s [%sexp (response3 : int Or_error.t)];
      [%expect
        {|
        ((prev ()) (query true) (diff (Fresh 0)))
        (Ok 0)
        |}];
      push 1;
      let%bind response2 in
      print_s [%sexp (response2 : int Or_error.t)];
      [%expect
        {|
        ((prev (0)) (query true) (diff (Update (1))))
        (Ok 1)
        |}];
      return ())
  ;;

  let%expect_test "multiple clients, multiple queries" =
    let true_bus =
      Bus.create_exn
        ~on_subscription_after_first_write:Allow_and_send_last_value_if_global
        ~on_callback_raise:(fun error -> print_s [%message (error : Error.t)])
        ()
    in
    let false_bus =
      Bus.create_exn
        ~on_subscription_after_first_write:Allow_and_send_last_value_if_global
        ~on_callback_raise:(fun error -> print_s [%message (error : Error.t)])
        ()
    in
    let implementation =
      Polling_state_rpc.implement_via_bus
        ~create_client_state:(fun _connection_state -> ())
        ~on_client_and_server_out_of_sync:
          (Expect_test_helpers_core.print_s ~hide_positions:true)
        rpc
        (fun _connection_state _client_state query ->
          if query then return true_bus else return false_bus)
    in
    with_connection_to_server implementation ~f:(fun connection ->
      let query client query =
        Polling_state_rpc.Client.dispatch client connection query
      in
      let client1 = make_client () in
      let client2 = make_client () in
      Bus.write true_bus 0;
      let%bind response1 = query client1 true in
      print_s [%sexp (response1 : int Or_error.t)];
      [%expect
        {|
        ((prev ()) (query true) (diff (Fresh 0)))
        (Ok 0)
        |}];
      let response2 = query client1 false in
      let%bind () = actually_yield_until_no_jobs_remain () in
      [%expect {| |}];
      let%bind response3 = query client2 true in
      print_s [%sexp (response3 : int Or_error.t)];
      [%expect
        {|
        ((prev ()) (query true) (diff (Fresh 0)))
        (Ok 0)
        |}];
      Bus.write false_bus 1;
      let%bind response2 in
      print_s [%sexp (response2 : int Or_error.t)];
      [%expect
        {|
        ((prev (0)) (query false) (diff (Update (1))))
        (Ok 1)
        |}];
      let%bind response4 = query client2 false in
      print_s [%sexp (response4 : int Or_error.t)];
      [%expect
        {|
        ((prev (0)) (query false) (diff (Update (1))))
        (Ok 1)
        |}];
      return ())
  ;;

  let%expect_test "bus gets created with [Allow]" =
    let bus =
      Bus.create_exn
        ~on_subscription_after_first_write:Allow
        ~on_callback_raise:(fun error -> print_s [%message (error : Error.t)])
        ()
    in
    let implementation =
      Polling_state_rpc.implement_via_bus
        ~create_client_state:(fun _client_state -> ())
        ~on_client_and_server_out_of_sync:
          (Expect_test_helpers_core.print_s ~hide_positions:true)
        rpc
        (fun _connection_state _client_state _query -> return bus)
    in
    with_connection_to_server implementation ~f:(fun connection ->
      let query client query =
        Polling_state_rpc.Client.dispatch client connection query
      in
      let client = make_client () in
      Bus.write bus 0;
      let response = query client true in
      let%bind () = actually_yield_until_no_jobs_remain () in
      [%expect {| |}];
      Bus.write bus 1;
      let%bind response in
      print_s [%sexp (response : int Or_error.t)];
      [%expect
        {|
        ((prev ()) (query true) (diff (Fresh 1)))
        (Ok 1)
        |}];
      return ())
  ;;

  let%expect_test "bus gets created with [Raise]" =
    (* This test demonstrates a way in which you might not satisfy the constraints of the
       API. However, disallowing this usage with the type is difficult or impossible, and
       the exeption you get back is good enough that we don't need to add any extra logic
       to polling-state RPC to tell the user of their mistake. *)
    let bus =
      Bus.create_exn
        ~on_subscription_after_first_write:Raise
        ~on_callback_raise:(fun error -> print_s [%message (error : Error.t)])
        ()
    in
    let implementation =
      Polling_state_rpc.implement_via_bus
        ~create_client_state:(fun _connection_state -> ())
        ~on_client_and_server_out_of_sync:
          (Expect_test_helpers_core.print_s ~hide_positions:true)
        rpc
        (fun _connection_state _client_state _query -> return bus)
    in
    with_connection_to_server implementation ~f:(fun connection ->
      let query client query =
        Polling_state_rpc.Client.dispatch client connection query
      in
      let client = make_client () in
      Bus.write bus 0;
      let%bind response = query client true in
      Expect_test_helpers_core.print_s
        ~hide_positions:true
        (Expect_test_helpers_core.remove_backtraces [%sexp (response : int Or_error.t)]);
      [%expect
        {|
        (Error (
          (rpc_error (
            Uncaught_exn (
              (location "server-side rpc computation")
              (exn (
                monitor.ml.Error
                ("Bus.subscribe_exn called after first write"
                 ((created_from
                   lib/polling_state_rpc/test/polling_state_rpc_test.ml:LINE:COL)
                  (on_subscription_after_first_write Raise)
                  (state                             Ok_to_write)
                  (write_ever_called                 true)
                  (subscribers ()))
                 lib/polling_state_rpc/src/bus_state.ml:LINE:COL)
                ("ELIDED BACKTRACE"))))))
          (connection_description ("Client connected via TCP" 127.0.0.1:PORT))
          (rpc_name    foo)
          (rpc_version 0)))
        |}];
      return ())
  ;;

  let%expect_test "client_state" =
    let bus =
      Bus.create_exn
        ~on_subscription_after_first_write:Allow_and_send_last_value_if_global
        ~on_callback_raise:(fun error -> print_s [%message (error : Error.t)])
        ()
    in
    let implementation =
      Polling_state_rpc.implement_via_bus
        ~on_client_forgotten:(fun client_state ->
          print_s [%message "on_client_forgotten" (!client_state : int)])
        ~create_client_state:(fun _connection_state -> ref 0)
        ~on_client_and_server_out_of_sync:
          (Expect_test_helpers_core.print_s ~hide_positions:true)
        rpc
        (fun _connection_state client_state _query ->
          incr client_state;
          print_s [%message (!client_state : int)];
          return bus)
    in
    with_connection_to_server implementation ~f:(fun connection ->
      let query client query =
        Polling_state_rpc.Client.dispatch client connection query
      in
      let client1 = make_client () in
      Bus.write bus 0;
      let%bind response = query client1 true in
      Expect_test_helpers_core.print_s
        ~hide_positions:true
        (Expect_test_helpers_core.remove_backtraces [%sexp (response : int Or_error.t)]);
      [%expect
        {|
        (!client_state 1)
        ((prev ()) (query true) (diff (Fresh 0)))
        (Ok 0)
        |}];
      let client2 = make_client () in
      let%bind response = query client2 true in
      print_s [%sexp (response : int Or_error.t)];
      [%expect
        {|
        (!client_state 1)
        ((prev ()) (query true) (diff (Fresh 0)))
        (Ok 0)
        |}];
      Bus.write bus 1;
      let%bind response = query client2 false in
      print_s [%sexp (response : int Or_error.t)];
      [%expect
        {|
        (!client_state 2)
        ((prev (0)) (query false) (diff (Update (1))))
        (Ok 1)
        |}];
      let%bind forget_response =
        Polling_state_rpc.Client.forget_on_server client1 connection
      in
      print_s [%message (forget_response : unit Or_error.t)];
      [%expect
        {|
        (on_client_forgotten (!client_state 1))
        (forget_response (Ok ()))
        |}];
      return ())
  ;;

  let%expect_test "async callback" =
    let bus =
      Bus.create_exn
        ~on_subscription_after_first_write:Allow_and_send_last_value_if_global
        ~on_callback_raise:(fun error -> print_s [%message (error : Error.t)])
        ()
    in
    let mvar = Mvar.create () in
    let implementation =
      Polling_state_rpc.implement_via_bus
        ~create_client_state:(fun _connection_state -> ())
        ~on_client_and_server_out_of_sync:
          (Expect_test_helpers_core.print_s ~hide_positions:true)
        rpc
        (fun _connection_state _client_state _query ->
          let%bind () = Mvar.take mvar in
          return bus)
    in
    with_connection_to_server implementation ~f:(fun connection ->
      let client = make_client () in
      let query () = Polling_state_rpc.Client.dispatch client connection true in
      let push n = Bus.write bus n in
      let response = query () in
      (* Even after pushing, the result will not be resolved yet. *)
      push 0;
      let%bind () = actually_yield_until_no_jobs_remain () in
      print_s [%sexp (Deferred.peek response : int Or_error.t option)];
      [%expect {| () |}];
      (* Filling the mvar causes the response to become resolved. *)
      push 1;
      Mvar.set mvar ();
      let%bind () = actually_yield_until_no_jobs_remain () in
      let%bind response in
      print_s [%sexp (response : int Or_error.t)];
      [%expect
        {|
        ((prev ()) (query true) (diff (Fresh 1)))
        (Ok 1)
        |}];
      (* We already have the bus, so we do not call the callback again, and therefore we
         do not block on the mvar getting filled again. *)
      push 2;
      let%bind response = query () in
      print_s [%sexp (response : int Or_error.t)];
      [%expect
        {|
        ((prev (1)) (query true) (diff (Update (2))))
        (Ok 2)
        |}];
      return ())
  ;;

  let%expect_test "sync callback - different behavior with Eager_deferred" =
    (* This is the same test as "async callback" above. The behavior differences are
       important to note. *)
    let bus =
      Bus.create_exn
        ~on_subscription_after_first_write:Allow_and_send_last_value_if_global
        ~on_callback_raise:(fun error -> print_s [%message (error : Error.t)])
        ()
    in
    let implementation =
      Polling_state_rpc.implement_via_bus'
        ~create_client_state:(fun _connection_state -> ())
        ~on_client_and_server_out_of_sync:
          (Expect_test_helpers_core.print_s ~hide_positions:true)
        rpc
        (fun _connection_state _client_state _query -> bus)
    in
    with_connection_to_server implementation ~f:(fun connection ->
      let client = make_client () in
      let query () = Polling_state_rpc.Client.dispatch client connection true in
      let push n = Bus.write bus n in
      let response = query () in
      (* The bus itself is eager_deferred internally, so as soon as you push it's active. *)
      push 0;
      let%bind () = actually_yield_until_no_jobs_remain () in
      print_s [%sexp (Deferred.peek response : int Or_error.t option)];
      [%expect
        {|
        ((prev ()) (query true) (diff (Fresh 0)))
        ((Ok 0))
        |}];
      push 1;
      let%bind () = actually_yield_until_no_jobs_remain () in
      let%bind response in
      print_s [%sexp (response : int Or_error.t)];
      [%expect {| (Ok 0) |}];
      let%bind () = actually_yield_until_no_jobs_remain () in
      let%bind response = query () in
      print_s [%sexp (response : int Or_error.t)];
      [%expect
        {|
        ((prev (0)) (query true) (diff (Update (1))))
        (Ok 1)
        |}];
      return ())
  ;;
end

module Mdx_prelude = struct
  include Core
  include Async
  include Async_rpc_kernel
  include Babel_fixtures
end
