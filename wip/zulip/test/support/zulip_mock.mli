(** Helpers for Zulip tests that run a client against a mock transport. *)

val ok : ('a, Zulip_eio.Error.t) result -> 'a
(** [ok r] is the value of [r]. It fails the current test on [Error]. *)

val headers : Http.Header.t
(** [headers] declares a JSON response body. *)

val success : string
(** [success] is the body of a bare successful Zulip response. *)

val read_body : Fetch.Middleware.request -> string
(** [read_body request] is the whole body of [request], or [""] if it has none.
    A streamed body is read up to 64 KiB. *)

val form : Fetch.Middleware.request -> (string * string) list
(** [form request] decodes the URL-encoded body of [request]. *)

val with_client :
  (Fetch.Middleware.request -> Fetch.response) ->
  (Zulip_eio.Client.t -> 'a) ->
  'a
(** [with_client handler run] runs [run] on a client for bot@zulip.test at
    https://zulip.test whose requests are answered by [handler], inside a mock
    Eio backend. *)

val decode : 'a Jsont.t -> string -> 'a
(** [decode codec text] decodes [text] with [codec]. It fails the current test
    on a decoding error. *)

val has_member : string -> Jsont.json -> bool
(** [has_member name json] is [true] if [json] is an object with a member called
    [name]. *)
