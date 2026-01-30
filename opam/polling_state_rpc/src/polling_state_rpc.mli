open! Core
open! Async_kernel
open! Async_rpc_kernel

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

(** The type representing the RPC itself. Similar to Rpc.Rpc.t and its friends. *)
type ('query, 'response) t

val name : ('query, 'response) t -> string
val version : ('query, 'response) t -> int
val babel_generic_rpc : _ t -> Babel.Generic_rpc.t

val create
  :  name:string
  -> version:int
  -> query_equal:('query -> 'query -> bool)
  -> bin_query:'query Bin_prot.Type_class.t
  -> (module Response with type t = 'response)
  -> ('query, 'response) t

(** Handling the first request and handling the remaining requests is different because
    the first request you likely want to compute immediately, but for subsequent requests,
    it's more likely that you'll want to block until your data store changes and updates
    are made available. *)
val implement
  :  here:[%call_pos]
  -> on_client_and_server_out_of_sync:(Sexp.t -> unit)
       (** [on_client_and_server_out_of_sync] gets called when the client asks for either
           diffs from a point different than the server expected or a fresh response when
           the server was expecting it to ask for diffs.

           This can happen when the response type's bin_shape changes, which means that
           the client will receive responses but won't be able to deserialize them. It can
           also happen when the client's heartbeat times out.

           In both of these cases the server behaves reasonably (it falls back to sending
           a fresh response), but passes a message to [on_client_and_server_out_of_sync]
           because the situation is undesirable, so server owners should be made aware so
           that it can be addressed.

           Some good values for this parameter include:
           - [Log.Global.info_s] - the mismatch is logged, but otherwise not handled.
           - [raise_s] - the server will get to skip sending the whole state across the
             wire and will instead merely send an error. This is often a good choice
             because usually if clients have trouble receiving one response, they continue
             to have trouble receiving more responses (for example, bin_shape errors don't
             go away naturally). *)
  -> ?for_first_request:('connection_state -> 'query -> 'response Deferred.t)
       (** When provided, [for_first_request] is called instead of the primary
           implementation function for the first_request of a particular query. This is so
           that you can respond immediately to the first request, but block on subsequent
           requests until there's an update to send. *)
  -> ('query, 'response) t
  -> ('connection_state -> 'query -> 'response Deferred.t)
  -> ('connection_state * Rpc.Connection.t) Rpc.Implementation.t

(** Similar to [implement], but with support for per-client state. This allows you to
    support multiple clients, each with different server-side state, that share a single
    [Rpc.Connection.t].

    Note that, similar to the connection state, the client state cannot be shared between
    multiple connections - if a client reconnects after disconnecting, it will be given a
    new client state.

    [on_client_forgotten] is called when the client calls [Client.forget_on_server], or
    when the underlying connection is closed. *)
val implement_with_client_state
  :  here:[%call_pos]
  -> on_client_and_server_out_of_sync:(Sexp.t -> unit)
  -> create_client_state:('connection_state -> 'client_state)
  -> ?on_client_forgotten:('client_state -> unit)
  -> ?for_first_request:
       ('connection_state -> 'client_state -> 'query -> 'response Deferred.t)
  -> ('query, 'response) t
  -> ('connection_state -> 'client_state -> 'query -> 'response Deferred.t)
  -> ('connection_state * Rpc.Connection.t) Rpc.Implementation.t

(** Like [implement], except the callback is invoked only when the query changes, and it
    must return a bus instead of a single result. Each time a client polls, it will
    receive the newest response that it has not yet seen. If it has seen the newest
    response, then the RPC implementation will block until there is a newer response.

    This function immediately subscribes to the returned bus and cancels any previous
    subscriptions each time it invokes the callback. It is recommended that the bus use
    [Allow_and_send_last_value], so that each new query doesn't miss the first response
    for each query (or even raise, if [on_subscription_after_first_write] is set to
    [Raise]). *)
val implement_via_bus
  :  here:[%call_pos]
  -> on_client_and_server_out_of_sync:(Sexp.t -> unit)
  -> create_client_state:('connection_state -> 'client_state)
  -> ?on_client_forgotten:('client_state -> unit)
  -> ('query, 'response) t
  -> ('connection_state
      -> 'client_state
      -> 'query
      -> ('response -> unit, [> read ]) Bus.t Deferred.t)
  -> ('connection_state * Rpc.Connection.t) Rpc.Implementation.t

(** Like [implement_via_bus], except that the callback does not return a deferred.

    [implement_via_bus'] will call [Bus.subscribe] immediately after the callback returns.
    When the client is no longer polling for the given query, this will call
    [Bus.unsubscribe'], meaning that [Bus.num_subscribers] will count the client as a
    subscriber iff the client is still polling for this query. *)
val implement_via_bus'
  :  here:[%call_pos]
  -> on_client_and_server_out_of_sync:(Sexp.t -> unit)
  -> create_client_state:('connection_state -> 'client_state)
  -> ?on_client_forgotten:('client_state -> unit)
  -> ('query, 'response) t
  -> ('connection_state
      -> 'client_state
      -> 'query
      -> ('response -> unit, [> read ]) Bus.t)
  -> ('connection_state * Rpc.Connection.t) Rpc.Implementation.t

module Client : sig
  type ('query, 'response) rpc := ('query, 'response) t

  (** A Client.t is the method by which polling-state-rpcs are dispatched to the server,
      and their results collected by the client. Clients independently track responses and
      perform diff updates, so if you have two state-rpcs that share the same underlying
      rpc, you should make a new client for each one. *)
  type ('query, 'response) t

  val create : ?initial_query:'query -> ('query, 'response) rpc -> ('query, 'response) t

  (** Dispatch will call the rpc and return the corresponding response. If you're
      listening for responses via [bus], the response will also be communicated there. *)
  val dispatch
    :  ('query, 'response) t
    -> Rpc.Connection.t
    -> 'query
    -> 'response Deferred.Or_error.t

  (** Same as [dispatch] but reusing the previous query. This function returns an Error if
      the query was not set with [dispatch] or [create ~initial_query] beforehand. *)
  val redispatch
    :  ('query, 'response) t
    -> Rpc.Connection.t
    -> 'response Deferred.Or_error.t

  (** Asks the server to forget any state related to the specified client. Use this
      function on clients that might not be used again, so that the server can free up
      memory. If the server side of the RPC was built using [implement_with_client_state]
      then a `on_client_forgotten` function (if provided) will be called.

      In addition, any queued and ongoing [dispatch]es will get cancelled.

      Calling [dispatch] after [forget_on_server] works just fine, but will require the
      server to send the entire response, rather than merely the diff from the previous
      response. In other words, clearing a client only affects speed/memory; it should
      have no effect on the results returned by subsequent calls to [dispatch] or
      [redispatch]. *)
  val forget_on_server
    :  ('query, 'response) t
    -> Rpc.Connection.t
    -> unit Deferred.Or_error.t

  (** Returns the most recent query. *)
  val query : ('query, _) t -> 'query option

  (** Receives a [bus] which forwards all the responses that come from this client
      alongside the query which requested them. *)
  val bus : ('query, 'response) t -> ('query -> 'response -> unit) Bus.Read_only.t

  module For_introspection : sig
    module Response : sig
      type 'response t [@@deriving sexp_of]

      val bin_size_t : ('response -> int) -> 'response t -> int
      val is_fresh : _ t -> bool
    end

    val collapse_sequencer_error : 'a Or_error.t Throttle.outcome -> 'a Or_error.t

    (** Like [dispatch], but also returns the underlying diff sent over the wire. *)
    val dispatch_with_underlying_diff
      :  ?on_dispatch:(unit -> unit)
      -> ('query, 'response) t
      -> Rpc.Connection.t
      -> 'query
      -> ('response * 'response Response.t lazy_t) Or_error.t Throttle.outcome Deferred.t

    val dispatch_with_underlying_diff_as_sexp
      :  ?sexp_of_response:('response -> Sexp.t)
      -> ?on_dispatch:(unit -> unit)
      -> ('query, 'response) t
      -> Rpc.Connection.t
      -> 'query
      -> ('response * Sexp.t lazy_t) Or_error.t Throttle.outcome Deferred.t
  end
end

(** [Polling_state_rpc.Expert] allows creating versioned rpcs using the [Babel] helpers
    and use authenticated implementations. *)
module Expert : sig
  type ('query, 'response) simple := ('query, 'response) t

  (** [Polling_state_rpc.Expert.t] is equivalent to [Polling_state_rpc.t] except that it
      materializes the ['update] arg type so we can take it in account in versioning. *)
  type ('query, 'response, 'update) t

  (** Creates a polling state rpc compatible with versioning and authorizations.

      {[
        let v1 =
          Polling_state_rpc.Expert.create
            ~name:"my-rpc"
            ~version:1
            ~query_equal:Stable.V1.Query.equal
            ~bin_query:Stable.V1.Query.bin_t
            (module Stable.V1.Response)
        ;;

        let v2 =
          Polling_state_rpc.Expert.create
            ~name:"my-rpc"
            ~version:2
            ~query_equal:Stable.V2.Query.equal
            ~bin_query:Stable.V2.Query.bin_t
            (module Stable.V2.Response)
        ;;
      ]} *)
  val create
    :  ?preserve_legacy_versioned_polling_state_rpc_caller_identity:bool
    -> name:string
    -> version:int
    -> query_equal:('query -> 'query -> bool)
    -> bin_query:'query Bin_prot.Type_class.t
    -> (module Response with type t = 'response and type Update.t = 'update)
    -> ('query, 'response, 'update) t

  val name : _ t -> string
  val version : _ t -> int
  val description : _ t -> Rpc.Description.t
  val shapes : _ t -> Rpc_shapes.t

  (** Convert to a simple version of this rpc *)
  val to_simple : ('query, 'response, _) t -> ('query, 'response) simple

  (** Produce implementations that can either be used with [to_babel_implementation] and
      [Babel.Callee.implement_multi_exn] or used in a single version implementation with
      [to_rpc_implementation].

      The implementation styles offered are the same as with the [implement*] apis above.

      {[
        let v2_impl =
          Polling_state_rpc.Expert.Implementation.create_with_client_state
            ~on_client_and_server_out_of_sync:(fun _ -> ())
            ~create_client_state:(fun _ -> ref 0)
            ~response:(module Stable.V2.Response)
            ~query_equal:Stable.V2.Query.equal
            (fun ( (* connection_state *) )
              `Authorized
              client_state
              (query : Stable.V2.Query.t) ->
              client_state := !client_state + query.add;
              return ({ count = !client_state } : Stable.V2.Response.t))
        ;;
      ]}

      Use the [Babel.Callee] api seen later to implement all versioned implementations at
      once. *)
  module Implementation : sig
    type ('query, 'response, 'update) rpc := ('query, 'response, 'update) t
    type ('query, 'response, 'update) base

    (** Represents an authenticable implementation *)
    type ('connection_state, 'authed, 'query, 'response, 'update) t =
      'connection_state * Rpc.Connection.t
      -> Rpc.Description.t
      -> 'authed
      -> ('query, 'response, 'update) base

    val create
      :  ?for_first_request:
           ('connection_state -> 'authed -> 'query -> 'response Deferred.t)
      -> ('connection_state -> 'authed -> 'query -> 'response Deferred.t)
      -> on_client_and_server_out_of_sync:(Sexp.t -> unit)
      -> response:(module Response with type t = 'response and type Update.t = 'update)
      -> query_equal:('query -> 'query -> bool)
      -> ('connection_state, 'authed, 'query, 'response, 'update) t

    val create_with_client_state
      :  ?on_client_forgotten:('client_state -> unit)
      -> ?for_first_request:
           ('connection_state
            -> 'authed
            -> 'client_state
            -> 'query
            -> 'response Deferred.t)
      -> ('connection_state -> 'authed -> 'client_state -> 'query -> 'response Deferred.t)
      -> on_client_and_server_out_of_sync:(Sexp.t -> unit)
      -> create_client_state:('connection_state -> 'client_state)
      -> response:(module Response with type t = 'response and type Update.t = 'update)
      -> query_equal:('query -> 'query -> bool)
      -> ('connection_state, 'authed, 'query, 'response, 'update) t

    val create_via_bus
      :  ?on_client_forgotten:('client_state -> unit)
      -> ('connection_state
          -> 'authed
          -> 'client_state
          -> 'query
          -> ('response -> unit, [> read ]) Bus.t Deferred.t)
      -> on_client_and_server_out_of_sync:(Sexp.t -> unit)
      -> create_client_state:('connection_state -> 'client_state)
      -> response:(module Response with type t = 'response and type Update.t = 'update)
      -> query_equal:('query -> 'query -> bool)
      -> ('connection_state, 'authed, 'query, 'response, 'update) t

    val create_via_bus'
      :  ?on_client_forgotten:('client_state -> unit)
      -> ('connection_state
          -> 'authed
          -> 'client_state
          -> 'query
          -> ('response -> unit, [> read ]) Bus.t)
      -> on_client_and_server_out_of_sync:(Sexp.t -> unit)
      -> create_client_state:('connection_state -> 'client_state)
      -> response:(module Response with type t = 'response and type Update.t = 'update)
      -> query_equal:('query -> 'query -> bool)
      -> ('connection_state, 'authed, 'query, 'response, 'update) t

    (** Converts the implementation to a single version rpc that doesn't check for
        authorization.

        You can pattern-match the ['authed] argument of the function with [`Authorized] *)
    val to_rpc_implementation
      :  here:[%call_pos]
      -> ?on_exception:Rpc.On_exception.t
      -> ('connection_state, [ `Authorized ], 'query, 'response, 'update) t
      -> rpc:('query, 'response, 'update) rpc
      -> ('connection_state * Rpc.Connection.t) Rpc.Implementation.t

    (** Converts the implementation to a single version rpc that checks for authorization. *)
    val to_rpc_implementation_with_auth
      :  here:[%call_pos]
      -> ?on_exception:Rpc.On_exception.t
      -> ('connection_state, 'authed, 'query, 'response, 'update) t
      -> check_auth:
           ('connection_state
            -> [ `Query of 'query | `Cancel_ongoing ]
            -> 'authed Or_not_authorized.t Deferred.t)
      -> rpc:('query, 'response, 'update) rpc
      -> ('connection_state * Rpc.Connection.t) Rpc.Implementation.t

    (** Converts the implementation to a [Babel.Callee] implementation.

        As babel does not support (yet) authorized calls, we set authed to [`Authorized]. *)
    val to_babel_implementation
      :  ('connection_state, [ `Authorized ], 'query, 'response, 'update) t
      -> ('connection_state * Rpc.Connection.t
          -> Rpc.Description.t
          -> ('query, 'response, 'update) base)
  end

  (** Client resolvers are used as dispatches in callers. They are meant to be
      "instantiated" to clients in by calling [negotiate] on them. *)
  module Client_resolver : sig
    type ('query, 'response, 'update) t

    val negotiate
      :  ?initial_query:'query
      -> ('query, 'response, _) t
      -> ('query, 'response) Client.t
  end

  module For_tests : sig
    val response_module
      :  (_, 'response, 'update) t
      -> (module Response with type t = 'response and type Update.t = 'update)

    val query_equal : ('query, _, _) t -> ('query -> 'query -> bool)
  end
end

(** A set of apis compatible with [Babel.Caller] and [Babel.Callee] types. *)
module Babel : sig
  (** Represents the different versions for an rpc and the type transitions between them
      on the server-side.
      {[
        let callee =
          Polling_state_rpc.Babel.Callee.singleton v1
          |> Polling_state_rpc.Babel.Callee.map_query ~f:Stable.V2.Query.of_v1_t
          |> Polling_state_rpc.Babel.Callee.map_response ~f:Stable.V2.Response.to_v1_t
          |> Polling_state_rpc.Babel.Callee.map_update
               ~f:Stable.V2.Response.Update.to_v1_t
          |> Polling_state_rpc.Babel.Callee.add ~rpc:v2
        ;;

        let implementations =
          Babel.Callee.implement_multi_exn
            callee
            ~f:(Polling_state_rpc.Expert.Implementation.to_babel_implementation v2_impl)
        ;;
      ]}

      Implementing with the callee will implement the two versions at once:
      {[
        # List.length implementations
        - : int = 2
      ]} *)
  module Callee : sig
    type ('query, 'response, 'update) implementation :=
      ('query, 'response, 'update) Expert.Implementation.base

    type 'a t := 'a Babel.Callee.t

    (** Create a callee which can implement a given rpc. *)
    val singleton
      :  ('query, 'response, 'update) Expert.t
      -> ('query, 'response, 'update) implementation t

    (** Extend a callee to be able to implement a given rpc. *)
    val add
      :  ('query, 'response, 'update) implementation t
      -> rpc:('query, 'response, 'update) Expert.t
      -> ('query, 'response, 'update) implementation t

    (** Map over the query type of a callee. *)
    val map_query
      :  ('query1, 'response, 'update) implementation t
      -> f:('query1 -> 'query2)
      -> ('query2, 'response, 'update) implementation t

    (** Map over the response type of a callee. *)
    val map_response
      :  ('query, 'response1, 'update) implementation t
      -> f:('response2 -> 'response1)
      -> ('query, 'response2, 'update) implementation t

    (** Map over the update type of a callee. *)
    val map_update
      :  ('query, 'response, 'update1) implementation t
      -> f:('update2 -> 'update1)
      -> ('query, 'response, 'update2) implementation t
  end

  (** Represents the different versions for an rpc and the type transitions between them
      on the client-side.
      {[
        let caller =
          Polling_state_rpc.Babel.Caller.singleton v1
          |> Polling_state_rpc.Babel.Caller.map_query ~f:Stable.V2.Query.to_v1_t
          |> Polling_state_rpc.Babel.Caller.map_response
               ~f:Stable.V2.Response.of_v1_t
               ~update_fn:Stable.V2.Response.update
               ~upgrade_update:Stable.V2.Response.Update.of_v1_t
               ~downgrade_update:Stable.V2.Response.Update.to_v1_t
          |> Polling_state_rpc.Babel.Caller.add ~rpc:v2
        ;;
      ]}

      Once you have a caller, you can negotiate a dispatch with a Connection with menu:
      {[
        # let dispatch = Polling_state_rpc.Babel.Caller.dispatch_multi_and_negotiate caller
        val dispatch :
          Versioned_rpc.Connection_with_menu.t ->
          (Stable.V2.Query.t, Stable.V2.Response.t) Polling_state_rpc.Client.t
          Core.Or_error.t = <fun>
      ]} *)
  module Caller : sig
    type ('query, 'response, 'update) dispatch :=
      ('query, 'response, 'update) Expert.Client_resolver.t

    type 'a t := 'a Babel.Caller.t

    val dispatch_multi
      :  ('query, 'response, 'update) dispatch t
      -> Versioned_rpc.Connection_with_menu.t
      -> ('query, 'response, 'update) dispatch Or_error.t

    val dispatch_multi_and_negotiate
      :  ?initial_query:'query
      -> ('query, 'response, _) dispatch t
      -> Versioned_rpc.Connection_with_menu.t
      -> ('query, 'response) Client.t Or_error.t

    val singleton
      :  ?metadata:Rpc_metadata.V2.t
      -> ('query, 'response, 'update) Expert.t
      -> ('query, 'response, 'update) dispatch t

    val add
      :  ('query, 'response, 'update) dispatch t
      -> rpc:('query, 'response, 'update) Expert.t
      -> ('query, 'response, 'update) dispatch t

    (** A specialization of [map] for the query type of a protocol. *)
    val map_query
      :  ('query1, 'response, 'update) dispatch t
      -> f:('query2 -> 'query1)
      -> ('query2, 'response, 'update) dispatch t

    (** A specialization of [map] for the response and update types of a protocol. This
        mapping needs to be bi-directional and aware of the diff application to correctly
        apply minimal diffs with minimal type conversion: we only map fresh responses and
        apply diffs in the upgraded version space.

        This means that this upgrade conserves physical equality whenever the diff update
        conserves it. *)
    val map_response
      :  ('query, 'response1, 'update1) dispatch t
      -> f:('response1 -> 'response2)
      -> upgrade_update:('update1 -> 'update2)
      -> downgrade_update:('update2 -> 'update1)
      -> update_fn:('response2 -> 'update2 -> 'response2)
      -> ('query, 'response2, 'update2) dispatch t
  end
end

module Private_for_testing : sig
  module Response = Client.For_introspection.Response

  val create_client
    :  ?initial_query:'query
    -> ('query, 'response) t
    -> introspect:('response option -> 'query -> 'response Response.t -> unit)
    -> ('query, 'response) Client.t

  val negotiate
    :  ?initial_query:'query
    -> ('query, 'response, 'update) Expert.Client_resolver.t
    -> introspect:('response option -> 'query -> 'response Response.t -> unit)
    -> ('query, 'response) Client.t
end
