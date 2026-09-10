(** Private operator configuration and scoped calendar connections. *)

type t

type identity = {
  account : string;
  username : string;
  key : string;
  page_size : int;
}

val configuration : Tool_config.t

val initialize :
  sw:Eio.Switch.t ->
  fetch:_ Fetch.t ->
  clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  Jsont.json ->
  t
(** [initialize ~sw ~fetch ~clock settings] closes over one bearer credential.
    Discovery is lazy. Runtime requests are confined to the configured origin,
    the selected account and the library's calendar read API. *)

val identity : t -> identity
(** [identity t] discovers and pins the calendar account and authenticated user.
*)

val download : t -> blob:string -> string
(** [download t ~blob] downloads one account-scoped blob within the response
    cap. *)

val mirror_source :
  t ->
  Jmap_eio.Calendars.kind ->
  (Jmap_eio.Calendars.item, Jmap_eio.Calendars.receipt) Jmap_eio.Mirror.source
(** [mirror_source t kind] supplies library-owned sync operations. *)
