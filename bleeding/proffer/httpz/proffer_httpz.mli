(** A {!Proffer} backend on the httpz parser and Eio.

    One listening socket, one domain, a fiber per connection. The wire is
    owned here: dispatch, conditional requests and HEAD are decided by
    {!Proffer.Backend}, which the mock backend runs too.

    {[
      Eio_main.run @@ fun stdenv ->
      Eio.Switch.run @@ fun sw ->
      Proffer_httpz.run ~sw
        ~net:(Eio.Stdenv.net stdenv)
        ~clock:(Eio.Stdenv.clock stdenv)
        ~addr:(`Tcp (Eio.Net.Ipaddr.V4.loopback, 8380))
        ~on_error:(fun exn -> prerr_endline (Printexc.to_string exn))
        ~env My_site.env My_site.compiled
    ]} *)

type config = {
  backlog : int;
      (** Depth of the kernel accept queue. A connection that arrives while
          the server is at [max_connections] waits here, and the kernel
          refuses it once the queue is full. *)
  max_connections : int;
      (** How many connections may be open at once. The accept loop stops
          accepting at this number and resumes as connections end, so an
          overloaded server queues rather than degrades. Each open connection
          holds about 96KB of buffers, which is what makes this the bound on
          the server's memory. *)
  idle_timeout : float;
      (** Seconds a connection may sit with no request in progress, counted
          from when it is accepted and again after each response. On expiry
          the connection is closed with no reply, there being nothing a
          silent client would read. *)
  request_timeout : float;
      (** Seconds a request has to arrive in full, head and body, counted
          from its first byte. On expiry the client gets 408 and the
          connection is closed. *)
}
(** Limits on serving. This is a record, so a field added to it breaks any
    caller that builds one literally. Write [{ default_config with backlog =
    128 }] instead, which is the intended idiom. *)

val default_config : config
(** [default_config] is a backlog of 64, at most 512 connections, a 75 second
    idle timeout and a 15 second request timeout.

    The idle timeout is longer than the keep-alive timeout a fronting proxy
    usually runs, so that the proxy retires an idle connection first and
    never sends a request down one this server is closing. The request
    timeout is short because {!run} caps a request at about 32KB, so a peer
    that cannot finish one in 15 seconds is not sending in good faith. *)

type event = {
  remote_addr : string;
      (** ["addr:port"] for TCP, the socket path for a Unix socket. This is
          the peer, so behind a proxy it is the proxy. *)
  meth : Proffer.Method.t;  (** The method from the request line. *)
  target : string;
      (** The request target as it was sent, still percent-encoded. *)
  path : string;
      (** The part of [target] before any ['?'], still percent-encoded. This
          is empty for a request refused before it reached a route, which is
          one answered 408, 411 or 413. *)
  request_headers : (string * string) list;
      (** Every request field the parser did not consume, in the order they
          arrived. Content-Length, Transfer-Encoding, Connection and Expect
          are consumed for framing and never appear. Host does. A name is
          spelled as the parser gives it, canonically for a field it knows
          and as sent for one it does not, so a reader folds case to match. A
          log derives Host, User-Agent, Referer and the forwarding fields
          from here. *)
  status : Proffer.Status.t;  (** The status sent to the client. *)
  response_content_type : string option;
      (** The Content-Type the handler set, or [None] when it set none, as a
          304 does. A request refused before it reached a route is also
          [None], its reply being the server's own. *)
  cache_status : string option;
      (** What the handler put in X-Cache, or [None] when it set nothing.
          The field is the site's own, so what a value means is too. *)
  body_size : int;  (** Bytes of body sent, so zero for HEAD and 304. *)
  duration_us : int;
      (** Microseconds from the parse of the request line to the last byte of
          the response. *)
}
(** What one served request is worth recording. *)

val run :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  addr:Eio.Net.Sockaddr.stream ->
  ?config:config ->
  ?on_listening:(Eio.Net.Sockaddr.stream -> unit) ->
  ?on_event:(event -> unit) ->
  on_error:(exn -> unit) ->
  env:'env ->
  'env Proffer.Compiled.t ->
  unit
(** [run ~sw ~net ~clock ~addr ~on_error ~env compiled] listens on [addr] and
    serves [compiled] until [sw] is cancelled, which is the only way it
    returns. [clock] measures the timeouts in [config].

    Serving is single-domain: connections are fibers, so [env] may hold state
    bound to this domain.

    [on_listening] is called once with the bound address, after the socket is
    listening and before the first connection is accepted. Pass port 0 in
    [addr] and read the port from it. A caller that must not race the listen
    has no other signal, since [run] does not return.

    [on_error] receives an exception raised by a handler, in which case the
    client gets a plain 500, and one raised by a connection, in which case
    that connection is dropped. [on_event] is called once per request whose
    request line parsed, including one answered 408, 411 or 413 because of
    its body. A request that does not parse at all is answered 400 or 413
    without an event, there being no method or target to report.

    {2 Limits}

    A whole request, its head and its body together, must fit in about 32KB.
    A larger one is answered 413 and the connection is closed. The bound is
    not configurable, because the parse buffer is the only place a body goes.
    Put anything that accepts a real upload behind a different backend.

    A chunked request body is answered 411 rather than dechunked.

    The timeouts in [config] bound reading. A peer that stops reading its
    response is bounded by [max_connections] alone, since a blocked write has
    no deadline of its own.

    {2 Framing}

    An unknown-length {!Proffer.Body.Stream} is sent chunked to an HTTP/1.1
    client. An HTTP/1.0 client, which has no chunked encoding, gets the
    stream raw and the connection closed to delimit it. A stream that
    declares its length is sent with Content-Length to either.

    @raise Invalid_argument if [config.max_connections] is not positive. *)
