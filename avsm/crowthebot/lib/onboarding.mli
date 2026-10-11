(** Admin DM approval for group rooms and live SAS confirmation. *)

type t
(** The onboarding state for one running profile. *)

val create :
  store:Store.t -> self:string -> now:(unit -> float) ->
  check_room:(string -> bool) -> send_admin:(string -> unit) ->
  verify:(unit -> unit) -> respond:(flow_id:string -> accept:bool -> unit) -> t
(** [create ~store ~self ~now ~check_room ~send_admin ~verify ~respond] creates
    an admin controller. [check_room] must establish current group membership
    for Crow and the admin. [send_admin] must use a private room containing only
    these accounts. Verification callbacks use the running Matrix service. *)

val request : t -> room:string -> unit
(** [request t ~room] records an eligible room, notifies the admin once per run
    and starts verification once per run. Pending decisions survive restarts.
    A failed notification may be retried. *)

val prompt :
  t -> flow_id:string -> Matrix_eio.Verification_service.prompt -> unit
(** [prompt t ~flow_id p] sends the admin's SAS and exact confirmation commands.
    Other accounts are rejected. Prompts expire after ten minutes. *)

val result : t -> Matrix_eio.Verification_service.result -> unit
(** [result t outcome] clears pending prompts and tells the admin the outcome.
    Device verification does not enable a group room. *)

val handle :
  t -> direct:bool -> send:(string -> unit) -> Engine.event -> bool
(** [handle t ~direct ~send event] handles literal [rooms], [room approve|reject
    ID] and [verify [yes|no FLOW]] commands outside the model. Commands require
    the primary admin in a currently validated DM. It returns whether the text
    names an onboarding command, including unauthorized and duplicate events.
    Ordinary conversation returns [false]. *)
