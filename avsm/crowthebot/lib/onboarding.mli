(** Admin DM approval for group rooms and live SAS confirmation. *)

type t
(** The onboarding state for one running profile. *)

val create :
  store:Store.t -> self:string ->
  check_room:(string -> bool) -> room_label:(string -> string) ->
  send_admin:(string -> unit) ->
  verify:(unit -> unit) -> respond:(flow_id:string -> accept:bool -> unit) -> t
(** [create ~store ~self ~check_room ~room_label ~send_admin ~verify ~respond] creates
    an admin controller. [check_room] must establish current group membership
    for Crow and the admin. [room_label] returns a room's alias, name or ID.
    [send_admin] must use a private room containing only these accounts. Verification callbacks use the running Matrix service. *)

val request : t -> room:string -> unit
(** [request t ~room] records an eligible room, notifies the admin once per run
    and starts verification once per run. Pending decisions survive restarts.
    A failed notification may be retried. *)

val prompt :
  t -> flow_id:string -> Matrix_eio.Verification_service.prompt -> unit
(** [prompt t ~flow_id p] posts the admin's SAS to the private admin DM, then
    confirms Crow's side. The admin compares and confirms in their Matrix
    client. A failed send does not confirm. Other accounts are rejected. *)

val result : t -> Matrix_eio.Verification_service.result -> unit
(** [result t outcome] tells the admin the verification outcome.
    Device verification does not enable a group room. *)

val handle :
  t -> direct:bool -> send:(string -> unit) -> Engine.event -> bool
(** [handle t ~direct ~send event] handles literal [rooms], [room approve|reject
    ROOM] and [verify] commands outside the model. Commands require
    the primary admin in a currently validated DM. [ROOM] is a pending room ID
    or a unique alias or name. It returns whether the text
    names an onboarding command, including unauthorized and duplicate events.
    Ordinary conversation returns [false]. *)
