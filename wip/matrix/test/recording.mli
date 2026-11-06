(** Mock HTTP clients that record the requests they answer. *)

type recorded = {
  meth : string;  (** The method, such as ["GET"]. *)
  url : string;  (** The full request URL. *)
  headers : Http.Header.t;
  body : string option;
      (** [None] for a request with no body, and ["<stream>"] for a streamed
          body. *)
}
(** A request as the backend saw it. *)

val body_of_request : Fetch.Middleware.request -> string option
(** [body_of_request req] is the body of [req] as {!recorded} stores it. *)

val record : recorded list ref -> Fetch.Middleware.request -> unit
(** [record log req] pushes [req] onto [log], newest first. *)

val mock :
  (Fetch.Middleware.request -> Fetch.response) ->
  recorded list ref * Fetch.plain
(** [mock handler] is [(log, client)] where [client] answers each request with
    [handler] after pushing it onto [log], newest first. *)
