open! Core
open! Async
include Babel_fixtures

let rpc version ~query_equal ~bin_query response_module =
  Polling_state_rpc.Expert.create
    ~name:"my-rpc"
    ~version
    ~query_equal
    ~bin_query
    response_module
;;

let v1 =
  rpc
    1
    ~query_equal:Stable.V1.Query.equal
    ~bin_query:Stable.V1.Query.bin_t
    (module Stable.V1.Response)

and v2 =
  rpc
    2
    ~query_equal:Stable.V2.Query.equal
    ~bin_query:Stable.V2.Query.bin_t
    (module Stable.V2.Response)
;;

let callee_v1_only = Polling_state_rpc.Babel.Callee.singleton v1
let callee_v2_only = Polling_state_rpc.Babel.Callee.singleton v2

let callee_v1_compatible_with_v2 =
  callee_v2_only
  |> Polling_state_rpc.Babel.Callee.map_query ~f:Stable.V2.Query.to_v1_t
  |> Polling_state_rpc.Babel.Callee.map_response ~f:Stable.V2.Response.of_v1_t
  |> Polling_state_rpc.Babel.Callee.map_update ~f:Stable.V2.Response.Update.of_v1_t
  |> Polling_state_rpc.Babel.Callee.add ~rpc:v1
;;

let callee_v2_compatible_with_v1 =
  callee_v1_only
  |> Polling_state_rpc.Babel.Callee.map_query ~f:Stable.V2.Query.of_v1_t
  |> Polling_state_rpc.Babel.Callee.map_response ~f:Stable.V2.Response.to_v1_t
  |> Polling_state_rpc.Babel.Callee.map_update ~f:Stable.V2.Response.Update.to_v1_t
  |> Polling_state_rpc.Babel.Callee.add ~rpc:v2
;;

let caller_v1_only = Polling_state_rpc.Babel.Caller.singleton v1
let caller_v2_only = Polling_state_rpc.Babel.Caller.singleton v2

let caller_v1_compatible_with_v2 =
  caller_v2_only
  |> Polling_state_rpc.Babel.Caller.map_query ~f:Stable.V2.Query.of_v1_t
  |> Polling_state_rpc.Babel.Caller.map_response
       ~update_fn:Stable.V1.Response.update
       ~f:Stable.V2.Response.to_v1_t
       ~upgrade_update:Stable.V2.Response.Update.to_v1_t
       ~downgrade_update:Stable.V2.Response.Update.of_v1_t
  |> Polling_state_rpc.Babel.Caller.add ~rpc:v1
;;

let caller_v2_compatible_with_v1 =
  caller_v1_only
  |> Polling_state_rpc.Babel.Caller.map_query ~f:Stable.V2.Query.to_v1_t
  |> Polling_state_rpc.Babel.Caller.map_response
       ~update_fn:Stable.V2.Response.update
       ~f:Stable.V2.Response.of_v1_t
       ~upgrade_update:Stable.V2.Response.Update.of_v1_t
       ~downgrade_update:Stable.V2.Response.Update.to_v1_t
  |> Polling_state_rpc.Babel.Caller.add ~rpc:v2
;;

let v1_implementations ?(callee = callee_v1_compatible_with_v2) () =
  let f =
    Polling_state_rpc.Expert.Implementation.create_with_client_state
      ~on_client_and_server_out_of_sync:(fun _ -> ())
      ~create_client_state:(fun _ -> ref 0)
      ~response:(module Stable.V1.Response)
      ~query_equal:Stable.V1.Query.equal
      (fun () `Authorized state { Stable.V1.Query.inc } ->
        if inc then Int.incr state;
        return { Stable.V1.Response.c = !state })
    |> Polling_state_rpc.Expert.Implementation.to_babel_implementation
  in
  Babel.Callee.implement_multi_exn callee ~f
;;

let v2_implementations ?(callee = callee_v2_compatible_with_v1) () =
  let f =
    Polling_state_rpc.Expert.Implementation.create_with_client_state
      ~on_client_and_server_out_of_sync:(fun _ -> ())
      ~create_client_state:(fun _ -> ref 0)
      ~response:(module Stable.V2.Response)
      ~query_equal:Stable.V2.Query.equal
      (fun () `Authorized state { Stable.V2.Query.add } ->
        state := !state + add;
        return { Stable.V2.Response.count = !state })
    |> Polling_state_rpc.Expert.Implementation.to_babel_implementation
  in
  Babel.Callee.implement_multi_exn callee ~f
;;

let where_to_listen_for_unit_test =
  Tcp.Where_to_listen.bind_to Localhost On_port_chosen_by_os
;;

let make_server ?(implementations = v2_implementations ()) () =
  let open Deferred.Or_error.Let_syntax in
  let%map server =
    Deferred.ok
    @@ Rpc.Connection.serve
         ~implementations:
           (Rpc.Implementations.create_exn
              ~implementations
              ~on_unknown_rpc:`Close_connection
              ~on_exception:Log_on_background_exn)
         ~initial_connection_state:(fun _addr conn -> (), conn)
         ~where_to_listen:where_to_listen_for_unit_test
         ()
  in
  server, Tcp.Where_to_connect.of_inet_address (Tcp.Server.listening_on_address server)
;;

let make_client_v1
  ?initial_query
  ?(caller = caller_v1_compatible_with_v2)
  ?(introspect = true)
  connection_with_menu
  =
  let introspect =
    if introspect
    then (fun prev query response_payload ->
      print_s [%message (prev : Stable.V1.Response.t option) (query : Stable.V1.Query.t)];
      print_s
        [%message
          (response_payload
           : Stable.V1.Response.t Polling_state_rpc.Private_for_testing.Response.t)])
    else fun _ _ _ -> ()
  in
  let%map.Or_error resolver =
    Polling_state_rpc.Babel.Caller.dispatch_multi caller connection_with_menu
  in
  Polling_state_rpc.Private_for_testing.negotiate ?initial_query resolver ~introspect
;;

let make_client_v2
  ?initial_query
  ?(caller = caller_v2_compatible_with_v1)
  ?(introspect = true)
  connection_with_menu
  =
  let introspect =
    if introspect
    then (fun prev query response_payload ->
      print_s [%message (prev : Stable.V2.Response.t option) (query : Stable.V2.Query.t)];
      print_s
        [%message
          (response_payload
           : Stable.V2.Response.t Polling_state_rpc.Private_for_testing.Response.t)])
    else fun _ _ _ -> ()
  in
  let%map.Or_error resolver =
    Polling_state_rpc.Babel.Caller.dispatch_multi caller connection_with_menu
  in
  Polling_state_rpc.Private_for_testing.negotiate ?initial_query resolver ~introspect
;;

let%expect_test "v2 client interacting with v2 server" =
  let open Deferred.Or_error.Let_syntax in
  Deferred.Or_error.ok_exn
  @@
  let%bind server, where_to_connect = make_server () in
  let%bind connection =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
  in
  let%bind connection_with_menu = Versioned_rpc.Connection_with_menu.create connection in
  let%bind client = make_client_v2 connection_with_menu |> Deferred.return in
  let%bind response = Polling_state_rpc.Client.dispatch client connection { add = 1 } in
  print_s [%message (response : Stable.V2.Response.t)];
  [%expect
    {|
    ((prev ()) (query ((add 1))))
    (response_payload (Fresh ((count 1))))
    (response ((count 1)))
    |}];
  let%bind response = Polling_state_rpc.Client.dispatch client connection { add = 2 } in
  print_s [%message (response : Stable.V2.Response.t)];
  [%expect
    {|
    ((prev (((count 1)))) (query ((add 2))))
    (response_payload (Update ((count_delta 2))))
    (response ((count 3)))
    |}];
  let%bind () = Deferred.ok (Async.Tcp.Server.close server) in
  let%bind () = Deferred.ok (Async.Tcp.Server.close_finished server) in
  Deferred.ok (Async.Scheduler.yield_until_no_jobs_remain ())
;;

let%expect_test "v1 client interacting with v2 server compatible with v1" =
  let open Deferred.Or_error.Let_syntax in
  Deferred.Or_error.ok_exn
  @@
  let%bind server, where_to_connect = make_server () in
  let%bind connection =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
  in
  let%bind connection_with_menu = Versioned_rpc.Connection_with_menu.create connection in
  let%bind client = make_client_v1 connection_with_menu |> Deferred.return in
  let%bind response =
    Polling_state_rpc.Client.dispatch client connection { inc = true }
  in
  print_s [%message (response : Stable.V1.Response.t)];
  [%expect
    {|
    ((prev ()) (query ((inc true))))
    (response_payload (Fresh ((c 1))))
    (response ((c 1)))
    |}];
  let%bind response =
    Polling_state_rpc.Client.dispatch client connection { inc = true }
  in
  print_s [%message (response : Stable.V1.Response.t)];
  [%expect
    {|
    ((prev (((c 1)))) (query ((inc true))))
    (response_payload (Update 1))
    (response ((c 2)))
    |}];
  let%bind () = Deferred.ok (Async.Tcp.Server.close server) in
  let%bind () = Deferred.ok (Async.Tcp.Server.close_finished server) in
  Deferred.ok (Async.Scheduler.yield_until_no_jobs_remain ())
;;

let%expect_test "v2 client interacting with v1 server compatible with v2" =
  let open Deferred.Or_error.Let_syntax in
  Deferred.Or_error.ok_exn
  @@
  let%bind server, where_to_connect =
    make_server
      ~implementations:(v1_implementations ~callee:callee_v1_compatible_with_v2 ())
      ()
  in
  let%bind connection =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
  in
  let%bind connection_with_menu = Versioned_rpc.Connection_with_menu.create connection in
  let%bind client = make_client_v2 connection_with_menu |> Deferred.return in
  let%bind response = Polling_state_rpc.Client.dispatch client connection { add = 1 } in
  print_s [%message (response : Stable.V2.Response.t)];
  [%expect
    {|
    ((prev ()) (query ((add 1))))
    (response_payload (Fresh ((count 1))))
    (response ((count 1)))
    |}];
  let%bind response = Polling_state_rpc.Client.dispatch client connection { add = 100 } in
  print_s [%message (response : Stable.V2.Response.t)];
  (* Note: we only increment by one because the v2 -> v1 query conversion (and v1
     implementation) loses the cardinality in the query (int -> bool) *)
  [%expect
    {|
    ((prev (((count 1)))) (query ((add 100))))
    (response_payload (Update ((count_delta 1))))
    (response ((count 2)))
    |}];
  let%bind () = Deferred.ok (Async.Tcp.Server.close server) in
  let%bind () = Deferred.ok (Async.Tcp.Server.close_finished server) in
  Deferred.ok (Async.Scheduler.yield_until_no_jobs_remain ())
;;

let%expect_test "v1 client interacting with v2 only server" =
  let open Deferred.Or_error.Let_syntax in
  Deferred.Or_error.ok_exn
  @@
  let%bind server, where_to_connect =
    make_server ~implementations:(v2_implementations ~callee:callee_v2_only ()) ()
  in
  let%bind connection =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
  in
  let%bind connection_with_menu = Versioned_rpc.Connection_with_menu.create connection in
  let%bind client = make_client_v1 connection_with_menu |> Deferred.return in
  let%bind response =
    Polling_state_rpc.Client.dispatch client connection { inc = true }
  in
  print_s [%message (response : Stable.V1.Response.t)];
  [%expect
    {|
    ((prev ()) (query ((inc true))))
    (response_payload (Fresh ((c 1))))
    (response ((c 1)))
    |}];
  let%bind response =
    Polling_state_rpc.Client.dispatch client connection { inc = true }
  in
  print_s [%message (response : Stable.V1.Response.t)];
  [%expect
    {|
    ((prev (((c 1)))) (query ((inc true))))
    (response_payload (Update ((count_delta 1))))
    (response ((c 2)))
    |}];
  let%bind () = Deferred.ok (Async.Tcp.Server.close server) in
  let%bind () = Deferred.ok (Async.Tcp.Server.close_finished server) in
  Deferred.ok (Async.Scheduler.yield_until_no_jobs_remain ())
;;

let%expect_test "v2 client interacting with v1 only server" =
  let open Deferred.Or_error.Let_syntax in
  Deferred.Or_error.ok_exn
  @@
  let%bind server, where_to_connect =
    make_server ~implementations:(v1_implementations ~callee:callee_v1_only ()) ()
  in
  let%bind connection =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
  in
  let%bind connection_with_menu = Versioned_rpc.Connection_with_menu.create connection in
  let%bind client = make_client_v2 connection_with_menu |> Deferred.return in
  let%bind response = Polling_state_rpc.Client.dispatch client connection { add = 1 } in
  print_s [%message (response : Stable.V2.Response.t)];
  [%expect
    {|
    ((prev ()) (query ((add 1))))
    (response_payload (Fresh ((count 1))))
    (response ((count 1)))
    |}];
  let%bind response = Polling_state_rpc.Client.dispatch client connection { add = 100 } in
  print_s [%message (response : Stable.V2.Response.t)];
  (* Note: we only increment by one because the v2 -> v1 query conversion (and v1
     implementation) loses the cardinality in the query (int -> bool) *)
  [%expect
    {|
    ((prev (((count 1)))) (query ((add 100))))
    (response_payload (Update 1))
    (response ((count 2)))
    |}];
  let%bind () = Deferred.ok (Async.Tcp.Server.close server) in
  let%bind () = Deferred.ok (Async.Tcp.Server.close_finished server) in
  Deferred.ok (Async.Scheduler.yield_until_no_jobs_remain ())
;;

let%expect_test "error case: version negociation failure (server is v1 only, client is \
                 v2 only)"
  =
  let open Deferred.Or_error.Let_syntax in
  Deferred.Or_error.ok_exn
  @@
  let%bind server, where_to_connect =
    make_server ~implementations:(v1_implementations ~callee:callee_v1_only ()) ()
  in
  let%bind connection =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
  in
  let%bind connection_with_menu = Versioned_rpc.Connection_with_menu.create connection in
  let client_or_error =
    make_client_v2 ~introspect:false connection_with_menu ~caller:caller_v2_only
  in
  assert (Or_error.is_error client_or_error);
  let%bind () = Deferred.ok (Async.Tcp.Server.close server) in
  let%bind () = Deferred.ok (Async.Tcp.Server.close_finished server) in
  Deferred.ok (Async.Scheduler.yield_until_no_jobs_remain ())
;;

let%expect_test "ensure that same version communication keeps responses correctly cut off"
  =
  let open Deferred.Or_error.Let_syntax in
  Deferred.Or_error.ok_exn
  @@
  let%bind server, where_to_connect = make_server () in
  let%bind connection =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
  in
  let%bind connection_with_menu = Versioned_rpc.Connection_with_menu.create connection in
  let%bind client =
    make_client_v2 ~introspect:false connection_with_menu |> Deferred.return
  in
  let%bind response = Polling_state_rpc.Client.dispatch client connection { add = 1 } in
  let%bind same_response =
    Polling_state_rpc.Client.dispatch client connection { add = 0 }
  in
  assert (phys_equal response same_response);
  let%bind () = Deferred.ok (Async.Tcp.Server.close server) in
  let%bind () = Deferred.ok (Async.Tcp.Server.close_finished server) in
  Deferred.ok (Async.Scheduler.yield_until_no_jobs_remain ())
;;

let%expect_test "ensure that server-side downgrade query then upgrade response \
                 communication keeps responses correctly cut off"
  =
  let open Deferred.Or_error.Let_syntax in
  Deferred.Or_error.ok_exn
  @@
  let%bind server, where_to_connect =
    make_server ~implementations:(v1_implementations ()) ()
  in
  let%bind connection =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
  in
  let%bind connection_with_menu = Versioned_rpc.Connection_with_menu.create connection in
  let%bind client =
    make_client_v2 ~introspect:false connection_with_menu |> Deferred.return
  in
  let%bind response = Polling_state_rpc.Client.dispatch client connection { add = 1 } in
  let%bind same_response =
    Polling_state_rpc.Client.dispatch client connection { add = 0 }
  in
  assert (phys_equal response same_response);
  let%bind () = Deferred.ok (Async.Tcp.Server.close server) in
  let%bind () = Deferred.ok (Async.Tcp.Server.close_finished server) in
  Deferred.ok (Async.Scheduler.yield_until_no_jobs_remain ())
;;

let%expect_test "ensure that client-side downgrade query then upgrade response \
                 communication keeps responses correctly cut off"
  =
  let open Deferred.Or_error.Let_syntax in
  Deferred.Or_error.ok_exn
  @@
  let%bind server, where_to_connect =
    make_server ~implementations:(v1_implementations ~callee:callee_v1_only ()) ()
  in
  let%bind connection =
    Deferred.Or_error.of_exn_result (Rpc.Connection.client where_to_connect)
  in
  let%bind connection_with_menu = Versioned_rpc.Connection_with_menu.create connection in
  let%bind client =
    make_client_v2 ~introspect:false connection_with_menu |> Deferred.return
  in
  let%bind response = Polling_state_rpc.Client.dispatch client connection { add = 1 } in
  let%bind same_response =
    Polling_state_rpc.Client.dispatch client connection { add = 0 }
  in
  assert (phys_equal response same_response);
  let%bind () = Deferred.ok (Async.Tcp.Server.close server) in
  let%bind () = Deferred.ok (Async.Tcp.Server.close_finished server) in
  Deferred.ok (Async.Scheduler.yield_until_no_jobs_remain ())
;;
