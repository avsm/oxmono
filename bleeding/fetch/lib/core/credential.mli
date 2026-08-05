(** How a credential travels. See {!Fetch.with_credentials}. *)

type t =
  | Bearer of (unit -> string)
      (** [Authorization] set to ["Bearer " ^ token ()]. *)
  | Header of string * (Middleware.request -> string)
      (** The named header, an API-key header such as [X-Api-Key],
          set to the function's result. *)
  | Query of (string * string) list
      (** Query parameters bound on the request URL, replacing a
          caller binding of the same name and keeping the rest of
          the query. *)
