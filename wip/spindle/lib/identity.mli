(** Handles and avatars of accounts, for showing people by name.

    A handle is shown only once it is verified both ways: the DID document
    claims it and the handle names the DID back. Answers are kept, handles in
    memory and in the spindle's store so that they survive a restart, and
    avatars in memory only. Every lookup is bounded in time, and a failure is
    remembered briefly, so a page that names many accounts stays quick. *)

type t

val v :
  store:Store.t ->
  network:Network.t ->
  http:Fetch.plain ->
  ?plc:string ->
  clock:_ Eio.Time.clock ->
  unit ->
  t
(** [v ~store ~network ~http ~plc ~clock ()] verifies handles with
    {!Xrpc.Identity.verified_handle} through [http] and the PLC directory [plc],
    and finds avatars through [network] and [http]. Both should refuse private
    addresses, since accounts choose where these lead. *)

val handle : t -> string -> string option
(** [handle t did] is [did]'s verified handle, if it has one. Unless kept, it
    asks the network, for at most five seconds. *)

val handles : t -> string list -> unit
(** [handles t dids] looks up the handles of [dids] at once, eight at a time, so
    that later calls of {!handle} are answered from what is kept. *)

val remember : t -> string -> string -> unit
(** [remember t did handle] keeps [handle] as [did]'s, as an OAuth login has
    verified it. *)

val avatar : t -> string -> (string * string) option
(** [avatar t did] is the media type and bytes of [did]'s Bluesky profile
    picture, if it has one: a PNG, JPEG, WebP or GIF of at most 1 MiB, read from
    the account's own PDS. *)
