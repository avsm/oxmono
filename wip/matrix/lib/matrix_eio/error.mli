(** error — failures, as Eio exceptions.

    Every call in this library that would return [Error e] raises
    [Eio.Io (E e', _)] instead, where [e'] is the {!type-err} that
    {!of_client_error} maps [e] to. The exception is registered with
    [Eio.Exn.register_pp], so it prints as a Matrix error rather than as an
    opaque one. *)

(** This exception boundary is deliberate: [matrix-chat.eio] is the raising
    convenience layer. Code that needs explicit error values can call the
    corresponding {!Matrix_client} function with {!Client.base}; it does not
    need a second generated [*_result] wrapper surface. *)

(** What went wrong. Mirrors {!Matrix_client.Error.t}, with {!Cancelled} added
    for what arises on this side of the boundary. *)
type err =
  | Network of string  (** The request did not complete. *)
  | Policy_denied of string
      (** A local capability policy refused the request before it was sent. *)
  | Tls of string  (** TLS setup, validation or transport failed. *)
  | Http of { status : int; body : string }
      (** A non-2xx response whose body is not a Matrix error. *)
  | Json of string  (** A body could not be encoded or decoded. *)
  | Matrix of {
      errcode : Matrix_client.Error.errcode;
      error : string;
      retry_after_ms : int option;
    }  (** The homeserver answered with an error object. *)
  | Not_logged_in  (** The call needs a session and the client has none. *)
  | No_content
      (** The request succeeded and returned nothing the call could decode. *)
  | Cancelled  (** The fiber was cancelled. *)

type Eio.Exn.err += E of err  (** The Eio error a failure here is carried in. *)

val err : err -> exn
(** [err e] is the [Eio.Io] exception carrying [e], for a module that raises
    without a {!Matrix_client.Error.t} to start from. *)

val pp_err : Format.formatter -> err -> unit
(** [pp_err ppf e] renders [e] on one line, without a trailing newline. *)

val of_client_error : Matrix_client.Error.t -> err
(** [of_client_error e] is the {!type-err} for [e]. *)

val with_context : string -> (unit -> 'a) -> 'a
(** [with_context context fn] runs [fn]. If it raises [Eio.Io], the same
    exception is re-raised with [context] and its original raw backtrace.
    Cancellation and every non-I/O exception pass through unchanged. Context
    must describe the operation and must not contain credentials or message
    content. *)

val raise_client_error : ?context:string -> Matrix_client.Error.t -> 'a
(** [raise_client_error ?context e] raises [Eio.Io] with
    [E (of_client_error e)]. When supplied, [context] is attached using Eio's
    structured error context. It never returns. *)

val unwrap : ?context:string -> ('a, Matrix_client.Error.t) result -> 'a
(** [unwrap ?context r] is [v] for [Ok v]. Every wrapper in this library applies
    it to the result its {!Matrix_client} counterpart returns. Raises [Eio.Io]
    with [E (of_client_error e)] for [Error e], attaching [context] when
    supplied. Use {!with_context} around the whole operation as well when
    evaluating [r] can itself raise an I/O exception. *)

val is_retryable : err -> bool
(** [is_retryable e] is [true] when the same request is worth sending again,
    which is so for a transport failure, a 5xx, a 429 and [M_LIMIT_EXCEEDED]. It
    is [false] for everything else, including policy and TLS failures and a body
    that would not decode. *)
