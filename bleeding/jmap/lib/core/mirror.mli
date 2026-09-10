@@ portable

(** Restartable, storage-independent JMAP mirroring.

    A mirror starts with a complete snapshot, catches up from an anchor state,
    then follows [/changes]. Persist each update and its cursor in one
    transaction. Keep the previous complete snapshot visible while staging a
    replacement. Use a compare-and-swap revision when workers may overlap. Never
    checkpoint a later [/get] state in place of [/changes.newState]. *)

type phase = New | Listing | Catching_up | Live

type cursor = private {
  phase : phase;
  state : string option;
  position : int;
  query_state : string option;
}

val cursor :
  phase:phase ->
  ?state:string ->
  ?position:int ->
  ?query_state:string ->
  unit ->
  cursor
(** [cursor ~phase ()] restores a checkpoint. Invalid combinations raise
    [Invalid_argument] before any protocol reads. [position] defaults to zero.
    It must fit both a machine integer and an unsigned JMAP Int53.
*)

val cursor_jsont : cursor Jsont.t
(** [cursor_jsont] codes checkpoints and validates their phase invariants. *)

val initial : cursor
(** [initial] starts a new staged snapshot. *)

type ('item, 'receipt) fetched = {
  state : string;
  items : 'item list;
  not_found : string list;
  receipts : 'receipt list;
}

type 'receipt changes = {
  old_state : string;
  new_state : string;
  more : bool;
  created : string list;
  updated : string list;
  destroyed : string list;
  receipts : 'receipt list;
}

type 'receipt page = {
  query_state : string;
  position : int;
  ids : string list;
  total : int option;
  receipts : 'receipt list;
}

type ('item, 'receipt) source = {
  get : ids:string list option -> ('item, 'receipt) fetched;
  changes : since:string -> ('receipt changes, 'receipt list) result;
  page : (position:int -> 'receipt page) option;
  id : 'item -> string;
  batch_size : int;
}
(** [source] supplies bounded protocol reads for one account and data type.
    [changes] returns [Error receipts] only for [cannotCalculateChanges]. Other
    failures raise and leave the cursor unchanged. With [page=None],
    [get ~ids:None] must return the complete collection. With [page=Some query],
    the query must include every base object with stable ordering and no
    filters. [get ~ids:(Some [])] supplies the initial anchor state.
    [batch_size] is the server's maximum number of IDs permitted in a get
    request, or a lower cap. The caller must enforce response size limits and
    account scoping. *)

type ('item, 'receipt) update = {
  cursor : cursor;
  items : 'item list;
  destroyed : string list;
  receipts : 'receipt list;
  more : bool;
  publish : bool;
}

type ('item, 'receipt) step =
  | Restart of 'receipt list
  | Update of ('item, 'receipt) update

val step : ('item, 'receipt) source -> cursor -> ('item, 'receipt) step
(** [step source cursor] performs at most one query or change page and its
    batched gets. [Restart] requires discarding the staged snapshot and saving
    [initial], while retaining the previously published snapshot. [publish]
    atomically exposes a completed staged snapshot. Live updates apply directly
    to the published snapshot. Short query pages advance by their actual count,
    and a changed query state restarts the snapshot. Deleted and not-found IDs
    are removed. A failed call yields no update to commit. *)
