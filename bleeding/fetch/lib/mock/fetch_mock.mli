(** A mock backend for {!Fetch}.

    Any [Fetch.Middleware.request -> Fetch.response] function becomes a
    client, so code written against {!Fetch.type-t} can be tested
    without sockets. Run it under [Eio_mock.Backend.run], or under
    [run_full] whose auto-advancing clocks suit testing
    {!Fetch.with_limits} and {!Fetch.with_retry}. *)

val client : (Fetch.Middleware.request -> Fetch.response) -> Fetch.plain
(** [client fn] is a client answering every request with [fn]. *)

val respond :
  ?status:int ->
  ?headers:Http.Header.t ->
  ?version:Fetch.version ->
  string ->
  Fetch.Middleware.request -> Fetch.response
(** [respond body req] is a canned response to [req] carrying [body],
    with status 200 and [req]'s URL unless given otherwise. *)
