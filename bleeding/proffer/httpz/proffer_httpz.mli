(** A {!Proffer} backend on the httpz parser and Eio.

    One listening socket, one domain, a fiber per connection. The wire is
    owned here: dispatch, conditional requests and HEAD are decided by
    {!Proffer.Serve}, which the mock backend runs too.

    {[
      Eio_main.run @@ fun stdenv ->
      Eio.Switch.run @@ fun sw ->
      Proffer_httpz.run ~sw
        ~net:(Eio.Stdenv.net stdenv)
        ~addr:(`Tcp (Eio.Net.Ipaddr.V4.loopback, 8380))
        ~on_error:(fun exn -> prerr_endline (Printexc.to_string exn))
        ~env My_site.env My_site.compiled
    ]} *)

type config = { backlog : int }
(** Listening socket options. [backlog] is the accept queue depth. *)

val default_config : config
(** [default_config] has a backlog of 64. *)

module Log : sig
  type event = {
    remote_addr : string;
        (** ["addr:port"] for TCP, the socket path for a Unix socket. *)
    meth : Proffer.Method.t;
    target : string;
    status : Proffer.Status.t;
    body_size : int;  (** Bytes of body sent, so zero for HEAD and 304. *)
    duration_us : int;
  }
  (** What one served request is worth recording. *)
end

val run :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  addr:Eio.Net.Sockaddr.stream ->
  ?config:config ->
  ?on_event:(Log.event -> unit) ->
  on_error:(exn -> unit) ->
  env:'env ->
  'env Proffer.Compiled.t ->
  unit
(** [run ~sw ~net ~addr ~on_error ~env compiled] listens on [addr] and serves
    [compiled] until [sw] is cancelled, which is the only way it returns.

    Serving is single-domain: connections are fibers, so [env] may hold state
    bound to this domain.

    [on_error] receives an exception raised by a handler, in which case the
    client gets a plain 500, and one raised by a connection, in which case
    that connection is dropped. [on_event] is called once per request whose
    request line parsed, including one answered 411 or 413 because of its
    body. A request that does not parse at all is answered 400 or 413 without
    an event, there being no method or target to report.

    An unknown-length {!Proffer.Body.Stream} is sent chunked to an HTTP/1.1
    client. An HTTP/1.0 client, which has no chunked encoding, gets the
    stream raw and the connection closed to delimit it. *)
