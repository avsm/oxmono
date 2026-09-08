(** crypto_store — the encryption machine's state on disk.

    {!Encryption} holds its cryptographic state in memory. An
    {!Encryption.type-snapshot} of it is written to a profile directory and read
    back, reusing {!Profile_store}'s files and adding [crypto_state.json] for
    what they have no shape for. Nothing is encrypted at rest. The files are
    created [0600] and atomically replaced through
    {!Profile_store.atomic_write}; the file and its native parent directory are
    synced. The security boundary is the filesystem. A client that loses this
    state cannot read its own history and must publish a new device.

    @see <https://spec.matrix.org/v1.11/client-server-api/#end-to-end-encryption>
      End-to-end encryption *)

type snapshot = Encryption.snapshot
(** The type for what one profile holds. It is {!Encryption.type-snapshot}. *)

type t
(** The type for handles on one profile directory. *)

val create : xdg:Xdge.t -> profile:string -> t
(** [create ~xdg ~profile] opens, creating if need be, the same profile
    directory {!Profile_store.create} uses, so that one directory holds both the
    session and the crypto state.

    Raises [Eio.Io] if the directory cannot be created. *)

val exists : t -> bool
(** [exists t] is [true] when this profile holds an Olm account or an
    interrupted transaction that must be recovered with {!load}. *)

val load : t -> (snapshot option, Error.t) result
(** [load t] reads the whole snapshot back. It is [Ok None] when the profile
    holds no Olm account, the normal state before the first {!save}. A session
    that fails to unpickle is dropped rather than failing the load, so that one
    corrupt Megolm session cannot cost the device its identity. [Error] means
    the account itself is present but unreadable. Loading is serialized with
    saves and refreshes this handle's generation marker. A complete pending redo
    journal is replayed before reading any component. Malformed or inconsistent
    journals fail closed. Older profiles need no migration; an interrupted
    legacy save without a journal still requires explicit recovery.

    Raises [Eio.Io] on a filesystem failure. *)

val save : t -> snapshot -> (unit, Error.t) result
(** [save t snapshot] serializes the complete snapshot to a private redo journal
    before replacing any component. Each file is atomically replaced and synced;
    an even generation commits the transaction. A crash or I/O failure after
    journal creation is recovered by the next {!load}, including in a new
    process, without requiring the old in-memory machine. Successful saves
    remove the journal. Native directory sync orders the journal, components and
    commit marker. Sessions that fail to pickle retain the existing skip policy.

    Saves are serialized with other crypto operations and reject stale handles
    with {!Error.Policy_denied}. Files remain unencrypted, including the
    journal. Raw {!Profile_store} component reads do not perform recovery; use
    {!load} for coherent crypto snapshots. Errors carry the first failure;
    filesystem failures can raise [Eio.Io]. *)

val clear : t -> (unit, Error.t) result
(** [clear t] deletes every file this module and {!Profile_store} write for the
    profile, while serializing with other crypto operations and advancing the
    persistent generation marker. Deletion is journalled so {!load} completes an
    interrupted clear rather than resurrecting the account. A stale handle is
    rejected with {!Error.Policy_denied}, so it cannot clear newer state.

    Raises [Eio.Io] if a file cannot be removed. *)
