open! Core
open! Async_kernel
open! Async_rpc_kernel

module Seqnum = struct
  include Unique_id.Int ()

  let forget = of_int_exn (-1)
end

module Query_dispatch_id = Unique_id.Int ()

(* A Client_id.t is unique per Client.t for a given connection. They are not guaranteed to
   be unique between connections. We need this because a user could have multiple Client.t
   for the same Rpc. *)
module Client_id = Unique_id.Int ()

module Cache : sig
  type ('a, 'client_state) t
  type ('a, 'client_state) per_client

  val create : on_client_forgotten:('client_state -> unit) -> ('a, 'client_state) t

  val find
    :  ('a, 'client_state) t
    -> connection_state:'connection_state
    -> connection:Rpc.Connection.t
    -> client_id:Client_id.t
    -> create_client_state:('connection_state -> 'client_state)
    -> ('a, 'client_state) per_client

  val remove_and_trigger_cancel
    :  ('a, 'client_state) t
    -> connection:Rpc.Connection.t
    -> client_id:Client_id.t
    -> unit

  val last_response : ('a, 'client_state) per_client -> ('a * Seqnum.t) option
  val set : ('a, 'client_state) per_client -> 'a -> Seqnum.t
  val wait_for_cancel : ('a, 'client_state) per_client -> unit Deferred.t
  val trigger_cancel : ('a, 'client_state) per_client -> unit
  val client_state : ('a, 'client_state) per_client -> 'client_state
end = struct
  type ('a, 'client_state) per_client =
    { mutable last_response : ('a * Seqnum.t) option
    ; mutable cancel : unit Ivar.t
    ; client_state : 'client_state
    }

  type ('a, 'client_state) per_connection =
    ('a, 'client_state) per_client Client_id.Table.t

  type ('a, 'client_state) t =
    { connections : (Rpc.Connection.t * ('a, 'client_state) per_connection) Bag.t
    ; on_client_forgotten : 'client_state -> unit
    }

  let create ~on_client_forgotten = { connections = Bag.create (); on_client_forgotten }

  let find_by_connection t ~connection =
    Bag.find t ~f:(fun (conn, _) -> phys_equal connection conn) |> Option.map ~f:snd
  ;;

  let find t ~connection_state ~connection ~client_id ~create_client_state =
    let per_connection =
      match find_by_connection t.connections ~connection with
      | Some per_connection -> per_connection
      | None ->
        let result = Client_id.Table.create () in
        let elt = Bag.add t.connections (connection, result) in
        Deferred.upon (Rpc.Connection.close_finished connection) (fun () ->
          Hashtbl.iter result ~f:(fun { client_state; _ } ->
            t.on_client_forgotten client_state);
          Bag.remove t.connections elt);
        result
    in
    Hashtbl.find_or_add per_connection client_id ~default:(fun () ->
      { last_response = None
      ; cancel = Ivar.create ()
      ; client_state = create_client_state connection_state
      })
  ;;

  let remove_and_trigger_cancel t ~connection ~client_id =
    Option.iter (find_by_connection t.connections ~connection) ~f:(fun per_connection ->
      Option.iter (Hashtbl.find_and_remove per_connection client_id) ~f:(fun per_client ->
        t.on_client_forgotten per_client.client_state;
        Ivar.fill_exn per_client.cancel ()))
  ;;

  let last_response per_client = per_client.last_response

  let set per_client data =
    let seqnum = Seqnum.create () in
    per_client.last_response <- Some (data, seqnum);
    seqnum
  ;;

  let wait_for_cancel per_client = Ivar.read per_client.cancel

  let trigger_cancel per_client =
    Ivar.fill_exn per_client.cancel ();
    per_client.cancel <- Ivar.create ()
  ;;

  let client_state per_client = per_client.client_state
end

module Request = struct
  (* We maintain two variations of the request type so that we can change one of them
     while keeping the other fixed. The stable type should be used only as an intermediate
     step of (de)serialization.

     Changing the unstable type requires figuring out how to encode/decode any additional
     data into/out-of the old stable type using the [unstable_of_stable] and
     [stable_of_unstable]. *)

  module Unstable = struct
    type 'query t =
      | Query of
          { last_seqnum : Seqnum.t option
          ; query : 'query
          ; client_id : Client_id.t
          }
      | Cancel_ongoing of Client_id.t
      | Forget_client of
          { query : 'query
          ; client_id : Client_id.t
          }

    let map ~f = function
      | Query { last_seqnum; query; client_id } ->
        Query { last_seqnum; query = f query; client_id }
      | Cancel_ongoing client_id -> Cancel_ongoing client_id
      | Forget_client { query; client_id } -> Forget_client { query = f query; client_id }
    ;;
  end

  module Stable = struct
    type 'query t =
      | Query of
          { last_seqnum : Seqnum.t option
          ; query : 'query
          ; client_id : Client_id.t
          }
      | Cancel_ongoing of Client_id.t
    [@@deriving bin_io, sexp_of]

    let%expect_test _ =
      print_endline [%bin_digest: unit t];
      [%expect {| 4eb554fadd7eded37e4da89efd208c52 |}]
    ;;

    let map ~f = function
      | Query { last_seqnum; query; client_id } ->
        Query { last_seqnum; query = f query; client_id }
      | Cancel_ongoing client_id -> Cancel_ongoing client_id
    ;;
  end

  let unstable_of_stable : 'query Stable.t -> 'query Unstable.t = function
    | Query { last_seqnum = Some last_seqnum; query; client_id }
      when [%equal: Seqnum.t] last_seqnum Seqnum.forget ->
      Forget_client { query; client_id }
    | Query { last_seqnum; query; client_id } -> Query { last_seqnum; query; client_id }
    | Cancel_ongoing client_id -> Cancel_ongoing client_id
  ;;

  let stable_of_unstable : 'query Unstable.t -> 'query Stable.t = function
    | Query { last_seqnum; query; client_id } -> Query { last_seqnum; query; client_id }
    | Cancel_ongoing client_id -> Cancel_ongoing client_id
    | Forget_client { query; client_id } ->
      Query { last_seqnum = Some Seqnum.forget; query; client_id }
  ;;
end

module Response = struct
  type ('response, 'update) t =
    | Fresh of 'response
    | Update of 'update
  [@@deriving bin_io, sexp_of]

  let%expect_test _ =
    print_endline [%bin_digest: (int, string) t];
    [%expect {| 13ef8c5223a0ea284c72512be32e5c09 |}]
  ;;

  type ('response, 'update) pair =
    | Response of
        { new_seqnum : Seqnum.t
        ; response : ('response, 'update) t
        }
    | Cancellation_successful
  [@@deriving bin_io, sexp_of]

  let map_response ~f = function
    | Response { new_seqnum; response = Fresh r } ->
      Response { new_seqnum; response = Fresh (f r) }
    | Response { new_seqnum; response = Update u } ->
      Response { new_seqnum; response = Update u }
    | Cancellation_successful -> Cancellation_successful
  ;;

  let map_update ~f = function
    | Response { new_seqnum; response = Fresh r } ->
      Response { new_seqnum; response = Fresh r }
    | Response { new_seqnum; response = Update u } ->
      Response { new_seqnum; response = Update (f u) }
    | Cancellation_successful -> Cancellation_successful
  ;;

  let%expect_test _ =
    print_endline [%bin_digest: (int, string) pair];
    [%expect {| 8bc63a85561d87b693d15e78c64e1008 |}]
  ;;
end

module type Response = sig
  (** This is a subset of the diffable signature (without [to_diffs] and [of_diffs]) and
      with the diffable type also supporting bin_io *)

  type t [@@deriving bin_io]

  module Update : sig
    type t [@@deriving bin_io, sexp_of]
  end

  val diffs : from:t -> to_:t -> Update.t
  val update : t -> Update.t -> t
end

module Expert' = struct
  type ('query, 'response, 'update) t =
    { response_module :
        (module Response with type t = 'response and type Update.t = 'update)
    ; query_equal : 'query -> 'query -> bool
    ; underlying_rpc :
        ('query Request.Stable.t, ('response, 'update) Response.pair) Rpc.Rpc.t
    }

  let name { underlying_rpc; _ } = Rpc.Rpc.name underlying_rpc
  let version { underlying_rpc; _ } = Rpc.Rpc.version underlying_rpc
  let babel_generic_rpc { underlying_rpc; _ } = Babel.Generic_rpc.Rpc underlying_rpc
  let description t = Rpc.Rpc.description t.underlying_rpc
  let shapes t = Rpc.Rpc.shapes t.underlying_rpc

  let create
    (type response update)
    ?(preserve_legacy_versioned_polling_state_rpc_caller_identity = false)
    ~name
    ~version
    ~query_equal
    ~bin_query
    (module M : Response with type t = response and type Update.t = update)
    =
    let bin_query, m_bin_t, m_update_bin_t =
      if preserve_legacy_versioned_polling_state_rpc_caller_identity
      then (
        let add_uuid uuid_str bin_t =
          { bin_t with
            Bin_prot.Type_class.shape =
              Bin_prot.Shape.annotate
                (Bin_prot.Shape.Uuid.of_string uuid_str)
                bin_t.Bin_prot.Type_class.shape
          }
        in
        let bin_query = add_uuid {|8266b996-7a30-436c-b045-00c02f7f3db7|} bin_query in
        let m_bin_t = add_uuid {|3e367da4-b851-422a-b4b0-6a1833eb941c|} M.bin_t in
        let m_update_bin_t =
          add_uuid {|db6dae4d-cb00-4727-ab1b-f0f34b37945c|} M.Update.bin_t
        in
        bin_query, m_bin_t, m_update_bin_t)
      else bin_query, M.bin_t, M.Update.bin_t
    in
    let bin_response = Response.bin_pair m_bin_t m_update_bin_t in
    let bin_query = Request.Stable.bin_t bin_query in
    { query_equal
    ; response_module = (module M)
    ; underlying_rpc =
        Rpc.Rpc.create
          ~name
          ~version
          ~bin_query
          ~bin_response
          ~include_in_error_count:Only_on_exn
    }
  ;;

  module Implementation = struct
    type ('query, 'response, 'update) base =
      'query Request.Stable.t -> ('response, 'update) Response.pair Deferred.t

    type ('connection_state, 'authed, 'query, 'response, 'update) t =
      'connection_state * Rpc.Connection.t
      -> Rpc.Description.t
      -> 'authed
      -> ('query, 'response, 'update) base

    let to_rpc_implementation_with_auth
      ~(here : [%call_pos])
      ?on_exception
      t
      ~check_auth
      ~rpc
      =
      let { underlying_rpc; _ } = rpc in
      let description = Rpc.Rpc.description underlying_rpc in
      let f (conn_state, conn) query =
        let query_opt =
          match query with
          | Request.Stable.Query { query; _ } -> `Query query
          | Cancel_ongoing _ -> `Cancel_ongoing
        in
        match%bind check_auth conn_state query_opt with
        | Or_not_authorized.Authorized authed ->
          let%bind res = t (conn_state, conn) description authed query in
          return (Or_not_authorized.Authorized res)
        | Not_authorized error -> return (Or_not_authorized.Not_authorized error)
      in
      Rpc.Rpc.implement_with_auth ?on_exception ~here underlying_rpc f
    ;;

    let to_rpc_implementation ~(here : [%call_pos]) ?on_exception t ~rpc =
      let { underlying_rpc; _ } = rpc in
      let description = Rpc.Rpc.description underlying_rpc in
      let f (conn_state, conn) query =
        t (conn_state, conn) description `Authorized query
      in
      Rpc.Rpc.implement ?on_exception ~here underlying_rpc f
    ;;

    let to_babel_implementation t (conn_state, conn) description query =
      t (conn_state, conn) description `Authorized query
    ;;

    let create_with_client_state
      (type response update)
      ?(on_client_forgotten = ignore)
      ?for_first_request
      f
      ~on_client_and_server_out_of_sync
      ~create_client_state
      ~response:(module M : Response with type t = response and type Update.t = update)
      ~query_equal
      =
      let for_first_request = Option.value for_first_request ~default:f in
      let init connection_state authed client_state query =
        let%map response = for_first_request connection_state authed client_state query in
        response, response
      in
      let updates
        connection_state
        authed
        client_state
        ~prev:(prev_query, prev_response)
        query
        =
        let f = if query_equal prev_query query then f else for_first_request in
        let%map new_ = f connection_state authed client_state query in
        let diff = M.diffs ~from:prev_response ~to_:new_ in
        Response.Update diff, new_
      in
      let cache = Cache.create ~on_client_forgotten in
      fun (connection_state, connection)
        ({ name = rpc_name; version = rpc_version } : Rpc.Description.t)
        authed
        request ->
        match Request.unstable_of_stable request with
        | Cancel_ongoing client_id ->
          let per_client =
            Cache.find cache ~connection_state ~connection ~client_id ~create_client_state
          in
          Cache.trigger_cancel per_client;
          return Response.Cancellation_successful
        | Forget_client { query = _; client_id } ->
          Cache.remove_and_trigger_cancel cache ~connection ~client_id;
          return Response.Cancellation_successful
        | Query { last_seqnum; query; client_id } ->
          let per_client =
            Cache.find cache ~connection_state ~connection ~client_id ~create_client_state
          in
          let prev =
            match Cache.last_response per_client, last_seqnum with
            | Some (prev, prev_seqnum), Some last_seqnum ->
              (match Seqnum.equal prev_seqnum last_seqnum with
               | true -> Some prev
               | false ->
                 on_client_and_server_out_of_sync
                   [%message
                     [%here]
                       "A polling state RPC client has requested diffs from a seqnum \
                        that the server does not have, so the server is sending a fresh \
                        response instead. This likely means that the client had trouble \
                        receiving the last RPC response."
                       (rpc_name : string)
                       (rpc_version : int)];
                 None)
            | None, Some _ | None, None -> None
            | Some _, None ->
              on_client_and_server_out_of_sync
                [%message
                  [%here]
                    "A polling state RPC client has requested a fresh response, but the \
                     server expected it to have the seqnum of the latest diffs. The \
                     server will send a fresh response as requested. This likely means \
                     that the client had trouble receiving the last RPC response."
                    (rpc_name : string)
                    (rpc_version : int)];
              None
          in
          let response =
            let client_state = Cache.client_state per_client in
            match prev with
            | Some prev ->
              let%map response, userdata =
                updates connection_state authed client_state ~prev query
              in
              response, userdata
            | None ->
              let%map response, userdata =
                init connection_state authed client_state query
              in
              Response.Fresh response, userdata
          in
          let response_or_cancelled =
            choose
              [ choice response (fun r -> `Response r)
              ; choice (Cache.wait_for_cancel per_client) (fun () -> `Cancelled)
              ]
          in
          (match%map response_or_cancelled with
           | `Response (response, userdata) ->
             let new_seqnum = Cache.set per_client (query, userdata) in
             Cache.trigger_cancel per_client;
             Response.Response { new_seqnum; response }
           | `Cancelled -> Response.Cancellation_successful)
    ;;

    let create
      ?for_first_request
      f
      ~on_client_and_server_out_of_sync
      ~response
      ~query_equal
      =
      let f connection_state authed _client_state query =
        f connection_state authed query
      in
      let for_first_request =
        Option.map
          for_first_request
          ~f:(fun f connection_state authed _client_state query ->
            f connection_state authed query)
      in
      create_with_client_state
        ?for_first_request
        f
        ~on_client_and_server_out_of_sync
        ~create_client_state:(fun _ _ -> ())
        ~response
        ~query_equal
    ;;

    let create_via_bus
      ?on_client_forgotten
      f
      ~on_client_and_server_out_of_sync
      ~create_client_state
      ~response
      ~query_equal
      =
      create_with_client_state
        ~on_client_and_server_out_of_sync
        ~create_client_state:(fun connection_state ->
          Bus_state.create (), create_client_state connection_state)
        ~on_client_forgotten:(fun (bus_state, client_state) ->
          Bus_state.unsubscribe bus_state;
          Option.iter on_client_forgotten ~f:(fun on_client_forgotten ->
            on_client_forgotten client_state))
        ~response
        ~query_equal
        ~for_first_request:(fun connection_state authed (bus_state, client_state) query ->
          (* Set up the bus subscription to the new query. *)
          let%bind.Eager_deferred bus = f connection_state authed client_state query in
          (* Subscribe to the new bus, unsubscribing to the previous bus if necessary. It
             is good to subscribe to the bus as immediately after receiving the bus via
             Eager_deferred, such that the publisher to the bus has an accurate count of
             subscribers from [Bus.num_subscribers]. *)
          Bus_state.subscribe bus_state bus;
          (* Wait for the bus to publish something to the [Bus_state.t] so we can return
             it as the response. *)
          Bus_state.take bus_state)
        (fun _connection_state _authed (bus_state, _client_state) _query ->
          (* For all polls except the first one for each query, we can simply wait for the
             current bus to publish a new response. We ignore the query, because we know
             that the bus subscription only ever corresponds to the current query. *)
          Bus_state.take bus_state)
    ;;

    let create_via_bus'
      ?on_client_forgotten
      f
      ~on_client_and_server_out_of_sync
      ~create_client_state
      ~response
      ~query_equal
      =
      create_via_bus
        ~on_client_and_server_out_of_sync
        ~create_client_state
        ?on_client_forgotten
        ~response
        ~query_equal
        (fun connection_state authed client_state query ->
           f connection_state authed client_state query |> Deferred.return)
    ;;
  end
end

type ('query, 'response) t =
  | T : ('query, 'response, 'update) Expert'.t -> ('query, 'response) t

let name (T expert) = Expert'.name expert
let version (T expert) = Expert'.version expert
let babel_generic_rpc (T expert) = Expert'.babel_generic_rpc expert

let create
  (type response)
  ~name
  ~version
  ~query_equal
  ~bin_query
  (module M : Response with type t = response)
  =
  T (Expert'.create ~name ~version ~query_equal ~bin_query (module M))
;;

let omit_auth f connection_state `Authorized = f connection_state

let implement_with_client_state
  ~(here : [%call_pos])
  ~on_client_and_server_out_of_sync
  ~create_client_state
  ?on_client_forgotten
  ?for_first_request
  (T ({ response_module; query_equal; _ } as rpc))
  f
  =
  let f = omit_auth f in
  let for_first_request = Option.map for_first_request ~f:omit_auth in
  Expert'.Implementation.create_with_client_state
    ~on_client_and_server_out_of_sync
    ~create_client_state
    ?on_client_forgotten
    ?for_first_request
    ~response:response_module
    ~query_equal
    f
  |> Expert'.Implementation.to_rpc_implementation ~here ~rpc
;;

let implement
  ~(here : [%call_pos])
  ~on_client_and_server_out_of_sync
  ?for_first_request
  (T ({ response_module; query_equal; _ } as rpc))
  f
  =
  let f = omit_auth f in
  let for_first_request = Option.map for_first_request ~f:omit_auth in
  Expert'.Implementation.create
    ~on_client_and_server_out_of_sync
    ?for_first_request
    ~response:response_module
    ~query_equal
    f
  |> Expert'.Implementation.to_rpc_implementation ~here ~rpc
;;

let implement_via_bus
  ~(here : [%call_pos])
  ~on_client_and_server_out_of_sync
  ~create_client_state
  ?on_client_forgotten
  (T ({ response_module; query_equal; _ } as rpc))
  f
  =
  let f = omit_auth f in
  Expert'.Implementation.create_via_bus
    ~on_client_and_server_out_of_sync
    ~create_client_state
    ?on_client_forgotten
    ~response:response_module
    ~query_equal
    f
  |> Expert'.Implementation.to_rpc_implementation ~here ~rpc
;;

let implement_via_bus'
  ~(here : [%call_pos])
  ~on_client_and_server_out_of_sync
  ~create_client_state
  ?on_client_forgotten
  (T ({ response_module; query_equal; _ } as rpc))
  f
  =
  let f = omit_auth f in
  Expert'.Implementation.create_via_bus'
    ~on_client_and_server_out_of_sync
    ~create_client_state
    ?on_client_forgotten
    ~response:response_module
    ~query_equal
    f
  |> Expert'.Implementation.to_rpc_implementation ~here ~rpc
;;

module Client = struct
  module Response' = struct
    type 'response t =
      | Fresh : 'response -> 'response t
      | Update :
          { t : 'update
          ; sexp_of : 'update -> Sexp.t
          ; bin_size_t : 'update -> int
          }
          -> 'response t

    let sexp_of_t sexp_of_a = function
      | Fresh a -> [%sexp Fresh (sexp_of_a a : Sexp.t)]
      | Update { t; sexp_of; _ } -> [%sexp Update (sexp_of t : Sexp.t)]
    ;;

    let base_bin_size_t = [%bin_size: [ `Fresh | `Update ]]

    let bin_size_t bin_size_a = function
      | Fresh a -> base_bin_size_t `Fresh + bin_size_a a
      | Update { t; bin_size_t; _ } -> base_bin_size_t `Update + bin_size_t t
    ;;

    let is_fresh = function
      | Fresh _ -> true
      | Update _ -> false
    ;;

    let of_response resp ~sexp_of_update ~bin_size_update =
      match resp with
      | Response.Fresh r -> Fresh r
      | Update diffs ->
        Update { t = diffs; sexp_of = sexp_of_update; bin_size_t = bin_size_update }
    ;;
  end

  type ('a, 'b) x = ('a, 'b) t

  type ('query, 'response, 'diff) unpacked =
    { mutable last_seqnum : Seqnum.t option
    ; mutable last_query : 'query option
    ; mutable last_query_dispatch_id : Query_dispatch_id.t
    ; mutable out : 'response option
    ; mutable sequencer : unit Sequencer.t
    ; cleaning_sequencer : unit Sequencer.t
    ; client_id : Client_id.t
    ; bus : ('query -> 'response -> unit) Bus.Read_write.t
    ; query_equal : 'query -> 'query -> bool
    ; dispatch_underlying :
        Async_rpc_kernel_private.Connection.t
        -> 'query Request.Unstable.t
        -> ('response, 'diff) Response.pair Or_error.t Deferred.t
    ; fold : 'response option -> 'query -> ('response, 'diff) Response.t -> 'response
    ; sexp_of_update : 'diff -> Sexp.t
    ; bin_size_update : 'diff -> int
    }

  type ('query, 'response) t =
    | T : ('query, 'response, 'diff) unpacked -> ('query, 'response) t

  let cancel_current_if_query_changed t q connection =
    (* use this cleaning_sequencer as a lock to prevent two subsequent queries from
       obliterating the same sequencer. *)
    Throttle.enqueue' t.cleaning_sequencer (fun () ->
      match t.last_query with
      (* If there was no previous query, we can keep the existing sequencer because its
         totally empty. *)
      | None -> Deferred.Or_error.ok_unit
      (* If the sequencer isn't running anything right now, then we can co-opt it for this
         query. *)
      | Some _ when Throttle.num_jobs_running t.sequencer = 0 -> Deferred.Or_error.ok_unit
      (* If the current query is the same as the last one, then we're fine because the
         sequencer is already running that kind of query. *)
      | Some q' when t.query_equal q q' -> Deferred.Or_error.ok_unit
      (* Otherwise, we need to cancel the current request, kill every task currently in
         the sequencer, and make a new sequencer for this query. *)
      | _ ->
        Throttle.kill t.sequencer;
        let%bind cancel_response =
          t.dispatch_underlying connection (Cancel_ongoing t.client_id)
        in
        let%map () = Throttle.cleaned t.sequencer in
        t.sequencer <- Sequencer.create ~continue_on_error:true ();
        (match cancel_response with
         | Ok (Response _) ->
           [%message "BUG" [%here] "regular response caused by cancellation"]
           |> Or_error.error_s
         | Ok Cancellation_successful -> Ok ()
         | Error e -> Error e))
  ;;

  module Aborted_error : sig
    val the_one_and_only : Error.t
    val test : Error.t -> bool
  end = struct
    exception Aborted [@@deriving sexp_of]

    let the_one_and_only = Error.of_exn Aborted
    let test = phys_equal the_one_and_only
  end

  let dispatch'
    (type query response diff)
    (t : (query, response, diff) unpacked)
    connection
    query
    =
    t.last_query <- Some query;
    let%bind response =
      let last_seqnum = t.last_seqnum in
      let client_id = t.client_id in
      t.dispatch_underlying connection (Query { query; last_seqnum; client_id })
    in
    match response with
    | Ok (Response { response; new_seqnum }) ->
      let new_out = t.fold t.out query response in
      t.last_seqnum <- Some new_seqnum;
      t.out <- Some new_out;
      Bus.write2 t.bus query new_out;
      let underlying_diff =
        lazy
          (Response'.of_response
             response
             ~sexp_of_update:t.sexp_of_update
             ~bin_size_update:t.bin_size_update)
      in
      return (Ok (new_out, underlying_diff))
    | Ok Cancellation_successful -> return (Error Aborted_error.the_one_and_only)
    | Error e -> return (Error e)
  ;;

  (* These are errors defined by sequencer; squish them into Or_error.t *)
  let collapse_sequencer_error = function
    | `Ok result_or_error -> result_or_error
    | `Aborted -> Error (Error.of_string "Request aborted")
    | `Raised exn -> Error (Error.of_exn exn)
  ;;

  let fix_sequencer_error = function
    | `Ok (Error e) when Aborted_error.test e -> `Aborted
    | other -> other
  ;;

  (* Use a sequencer to ensure that there aren't any sequential outgoing requests *)
  let dispatch_with_underlying_diff ?(on_dispatch = Fn.id) (T t) connection query =
    let query_dispatch_id = Query_dispatch_id.create () in
    t.last_query_dispatch_id <- query_dispatch_id;
    match%bind cancel_current_if_query_changed t query connection with
    | (`Aborted | `Raised _ | `Ok (Error _)) as result -> return result
    | `Ok (Ok ()) ->
      if not (Query_dispatch_id.equal t.last_query_dispatch_id query_dispatch_id)
      then return `Aborted
      else (
        let%map result =
          Throttle.enqueue' t.sequencer (fun () ->
            let d = dispatch' t connection query in
            on_dispatch ();
            d)
        in
        fix_sequencer_error result)
  ;;

  let dispatch_with_underlying_diff_as_sexp
    ?sexp_of_response
    ?on_dispatch
    (T t)
    connection
    query
    =
    let%map.Deferred x =
      dispatch_with_underlying_diff ?on_dispatch (T t) connection query
    in
    match x with
    | `Aborted -> `Aborted
    | `Raised exn -> `Raised exn
    | `Ok ok ->
      `Ok
        (let%map.Or_error response, raw_response = ok in
         let raw_response =
           let%map.Lazy raw_response in
           let sexp_of_response = Option.value sexp_of_response ~default:sexp_of_opaque in
           [%sexp_of: response Response'.t] raw_response
         in
         response, raw_response)
  ;;

  let dispatch rpc connection query =
    let%map.Deferred.Or_error response, (_ : 'response Response'.t lazy_t) =
      dispatch_with_underlying_diff rpc connection query >>| collapse_sequencer_error
    in
    response
  ;;

  let redispatch (T t) connection =
    let%map result =
      Throttle.enqueue' t.sequencer (fun () ->
        match t.last_query with
        | Some q ->
          let%map.Deferred.Or_error response, (_ : 'response Response'.t lazy_t) =
            dispatch' t connection q
          in
          response
        | None ->
          "[redispatch] called before a query was set or a regular dispatch had completed"
          |> Error.of_string
          |> Error
          |> Deferred.return)
    in
    collapse_sequencer_error (fix_sequencer_error result)
  ;;

  let forget_on_server (T t) connection =
    t.last_query_dispatch_id <- Query_dispatch_id.create ();
    match t.last_query with
    (* If there was no previous query, then the server has nothing to forget. *)
    | None -> Deferred.Or_error.ok_unit
    | Some query ->
      Throttle.enqueue' t.cleaning_sequencer (fun () ->
        Throttle.kill t.sequencer;
        let%bind forget_response =
          t.dispatch_underlying
            connection
            (Forget_client { query; client_id = t.client_id })
        in
        let%map () = Throttle.cleaned t.sequencer in
        t.sequencer <- Sequencer.create ~continue_on_error:true ();
        match forget_response with
        | Ok (Response _) ->
          (* It is possible that new clients of old servers will get spammed with this
             message if [forget_on_server] is called a lot. However, this sounds like an
             unlikely case to me, since old servers probably won't exist for much longer;
             in addition, spamming this message is not terrible. *)
          Or_error.error_s
            [%message
              "BUG"
                [%here]
                {|Regular response caused by forget request. This can also happen if the server is old and does not support forget requests, in which case this is not a bug.|}]
        | Ok Cancellation_successful -> Ok ()
        | Error e -> Error e)
      >>| collapse_sequencer_error
  ;;

  let query (T { last_query; _ }) = last_query
  let bus (T { bus; _ }) = Bus.read_only bus

  let create'
    ?initial_query
    ~query_equal
    ~dispatch_underlying
    ~update
    ~sexp_of_update
    ~bin_size_update
    ()
    =
    let f prev _query = function
      | Response.Fresh r -> r
      | Update diffs ->
        (match prev with
         | None ->
           raise_s
             [%message
               "BUG" [%here] "received an update without receiving any previous values"]
         | Some prev -> update prev diffs)
    in
    let bus =
      Bus.create_exn
        ~on_callback_raise:(fun error ->
          let tag = "exception thrown from inside of polling-state-rpc bus handler" in
          error |> Error.tag ~tag |> [%sexp_of: Error.t] |> eprint_s)
        ~on_subscription_after_first_write:Allow_and_send_last_value_if_global
        ()
    in
    let sequencer = Sequencer.create ~continue_on_error:true () in
    let cleaning_sequencer = Sequencer.create ~continue_on_error:true () in
    T
      { bus
      ; sequencer
      ; cleaning_sequencer
      ; last_seqnum = None
      ; last_query = initial_query
      ; last_query_dispatch_id = Query_dispatch_id.create ()
      ; out = None
      ; query_equal
      ; dispatch_underlying
      ; fold = f
      ; client_id = Client_id.create ()
      ; sexp_of_update
      ; bin_size_update
      }
  ;;

  let create (type query response) ?initial_query (T expert : (query, response) x) =
    let ({ underlying_rpc; response_module = (module M); query_equal } : _ Expert'.t) =
      expert
    in
    let dispatch_underlying connection request =
      Rpc.Rpc.dispatch underlying_rpc connection (Request.stable_of_unstable request)
    in
    create'
      ?initial_query
      ~dispatch_underlying
      ~query_equal
      ~update:M.update
      ~sexp_of_update:M.Update.sexp_of_t
      ~bin_size_update:M.Update.bin_size_t
      ()
  ;;

  module For_introspection = struct
    module Response = Response'

    let collapse_sequencer_error = collapse_sequencer_error
    let dispatch_with_underlying_diff = dispatch_with_underlying_diff
    let dispatch_with_underlying_diff_as_sexp = dispatch_with_underlying_diff_as_sexp
  end
end

module Expert = struct
  include Expert'

  let to_simple expert = T expert

  module Client_resolver = struct
    type ('query, 'response, 'update) rpc = ('query, 'response, 'update) t

    type ('query, 'response, 'update) t =
      { sexp_of_update : 'update -> Sexp.t
      ; bin_size_update : 'update -> int
      ; update : 'response -> 'update -> 'response
      ; query_equal : 'query -> 'query -> bool
      ; dispatch_underlying :
          Async_rpc_kernel.Rpc.Connection.t
          -> 'query Request.Unstable.t
          -> ('response, 'update) Response.pair Or_error.t Deferred.t
      }

    let of_rpc
      (type response update)
      ~metadata
      ({ response_module =
           (module M : Response with type t = response and type Update.t = update)
       ; query_equal
       ; underlying_rpc
       } :
        _ rpc)
      =
      let dispatch_underlying connection request =
        Rpc.Rpc.Expert.dispatch_bin_prot_with_metadata'
          ~metadata
          underlying_rpc
          connection
          (Request.stable_of_unstable request)
        >>| Result.map_error
              ~f:
                (Rpc_error.to_error
                   ~rpc_description:(Rpc.Rpc.description underlying_rpc)
                   ~connection_description:(Rpc.Connection.description connection)
                   ~connection_close_started:
                     (Rpc.Connection.close_reason connection ~on_close:`started))
      in
      { update = M.update
      ; sexp_of_update = M.Update.sexp_of_t
      ; bin_size_update = M.Update.bin_size_t
      ; query_equal
      ; dispatch_underlying
      }
    ;;

    let negotiate
      ?initial_query
      { update; sexp_of_update; bin_size_update; query_equal; dispatch_underlying }
      =
      Client.create'
        ?initial_query
        ~update
        ~sexp_of_update
        ~bin_size_update
        ~query_equal
        ~dispatch_underlying
        ()
    ;;

    let map_query
      { update; sexp_of_update; bin_size_update; query_equal; dispatch_underlying }
      ~f
      =
      let dispatch_underlying conn query =
        dispatch_underlying conn (Request.Unstable.map query ~f)
      in
      let query_equal a b = query_equal (f a) (f b) in
      { update; sexp_of_update; bin_size_update; dispatch_underlying; query_equal }
    ;;

    let map_response
      ?downgrade_update
      { update = _; sexp_of_update; bin_size_update; query_equal; dispatch_underlying }
      ~update_fn
      ~f
      ~upgrade_update
      =
      let dispatch_underlying conn query =
        let%map.Deferred.Or_error res = dispatch_underlying conn query in
        Response.map_response ~f res |> Response.map_update ~f:upgrade_update
      in
      let update res diff = update_fn res diff in
      let sexp_of_update, bin_size_update =
        match downgrade_update with
        | None -> sexp_of_opaque, Fn.const 0
        | Some f -> (fun u -> sexp_of_update (f u)), fun u -> bin_size_update (f u)
      in
      { update; sexp_of_update; bin_size_update; dispatch_underlying; query_equal }
    ;;
  end

  module For_tests = struct
    let response_module t = t.response_module
    let query_equal t = t.query_equal
  end
end

module Babel = struct
  module Callee = struct
    let singleton rpc = Babel.Callee.Rpc.singleton rpc.Expert.underlying_rpc
    let add t ~rpc = Babel.Callee.Rpc.add t ~rpc:rpc.Expert.underlying_rpc
    let map_query t ~f = Babel.Callee.Rpc.map_query t ~f:(Request.Stable.map ~f)
    let map_response t ~f = Babel.Callee.Rpc.map_response t ~f:(Response.map_response ~f)
    let map_update t ~f = Babel.Callee.Rpc.map_response t ~f:(Response.map_update ~f)
  end

  module Caller = struct
    let dispatch_multi t connection_with_menu =
      let menu = Versioned_rpc.Connection_with_menu.menu connection_with_menu in
      let%map.Or_error f = Babel.Caller.to_dispatch_fun t menu in
      f (Versioned_rpc.Connection_with_menu.connection connection_with_menu)
    ;;

    let dispatch_multi_and_negotiate ?initial_query t conn =
      let%map.Or_error resolver = dispatch_multi t conn in
      Expert.Client_resolver.negotiate ?initial_query resolver
    ;;

    let singleton ?(metadata = Rpc_metadata.V2.empty) rpc =
      Babel.Caller.Expert.return
        (Expert.babel_generic_rpc rpc)
        (Expert.Client_resolver.of_rpc ~metadata rpc)
    ;;

    let add t ~rpc = Babel.Caller.of_list_decreasing_preference [ singleton rpc; t ]
    let map_query t ~f = Babel.Caller.map t ~f:(Expert.Client_resolver.map_query ~f)

    let map_response t ~f ~upgrade_update ~downgrade_update ~update_fn =
      Babel.Caller.map
        t
        ~f:
          (Expert.Client_resolver.map_response
             ~f
             ~upgrade_update
             ~downgrade_update
             ~update_fn)
    ;;
  end
end

module Private_for_testing = struct
  module Response' = Client.Response'

  let make_fold ~introspect ~fold ~sexp_of_update ~bin_size_update =
    let fold prev query resp =
      let resp' = Response'.of_response resp ~sexp_of_update ~bin_size_update in
      introspect prev query resp';
      fold prev query resp
    in
    fold
  ;;

  let create_client ?initial_query t ~introspect =
    let (T client) = Client.create ?initial_query t in
    let new_fold =
      make_fold
        ~introspect
        ~fold:client.fold
        ~sexp_of_update:client.sexp_of_update
        ~bin_size_update:client.bin_size_update
    in
    Client.T { client with fold = new_fold }
  ;;

  let negotiate
    ?initial_query
    { Expert.Client_resolver.update
    ; sexp_of_update
    ; bin_size_update
    ; query_equal
    ; dispatch_underlying
    }
    ~introspect
    =
    let (T client) =
      Client.create'
        ?initial_query
        ~update
        ~sexp_of_update
        ~bin_size_update
        ~query_equal
        ~dispatch_underlying
        ()
    in
    let new_fold =
      make_fold
        ~introspect
        ~fold:client.fold
        ~sexp_of_update:client.sexp_of_update
        ~bin_size_update:client.bin_size_update
    in
    Client.T { client with fold = new_fold }
  ;;

  module Response = Response'
end
