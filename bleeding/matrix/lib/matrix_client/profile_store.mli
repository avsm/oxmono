(** profile_store — the directory a profile's files live in.

    A profile is [$XDG_DATA_HOME/matrix/profiles/<name>/] and holds the session,
    the device identity, the one-time key pool and the Olm and Megolm sessions
    as separate JSON files, each created 0600. Encryption at rest is deferred:
    tokens, private keys and journals are unencrypted, and there is no local
    unlock prompt or keyring integration. Filesystem access controls protect the
    profile and any copies/backups. There is no export format. A profile is
    bound to the device that made it and holds one account. {!Session} defines
    what the files contain.

    This module owns the directory. {!Crypto_store} and {!Store} keep files of
    their own beside these, addressing the directory through {!dir}.

    @see <https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html>
      XDG Base Directory Specification *)

type t
(** The type for profile directories. *)

val validate_profile_name : string -> unit
(** [validate_profile_name name] rejects names that are not one non-empty
    relative path component. It raises [Invalid_argument] when rejected. *)

val create : xdg:Xdge.t -> profile:string -> t
(** [create ~xdg ~profile] is the store for [profile], creating its directory
    mode 0700 if it does not exist. An existing directory is left as it is.

    By default the directory is [$XDG_DATA_HOME/matrix/profiles/<profile>/],
    through [xdg]. Use {!create_at} when an explicit filesystem root is needed.

    Raises [Invalid_argument] for an unsafe profile name, or [Eio.Io] if the
    directory cannot be created. *)

val create_at : root:Eio.Fs.dir_ty Eio.Path.t -> profile:string -> t
(** [create_at ~root ~profile] is the store for [profile] rooted at
    [root/profiles/<profile>/]. [root] replaces [Xdge.data_dir] and is useful
    for tests and embedders that must not depend on or modify the process's
    HOME/XDG environment. The directory is created mode 0700 if it does not
    exist; an existing directory is left as it is.

    Raises [Invalid_argument] for an unsafe profile name, or [Eio.Io] if the
    directory cannot be created. *)

val dir : t -> Eio.Fs.dir_ty Eio.Path.t
(** The profile's directory. *)

val exists : t -> bool
(** [exists t] is [true] when the profile holds a session file, so an account is
    logged in. *)

val with_lock : t -> (unit -> 'a) -> ('a, Error.t) result
(** [with_lock t fn] runs [fn] while holding the profile's cross-process lock.

    The lock is shared by all handles for the same native profile path in the
    current process and by other processes through a persistent advisory lock
    inode named [.profile.lock]. The inode is never removed. Waiting for the
    process-local lock is cancellable; once entered, cancellation is deferred
    until cleanup has released both locks. Exceptions raised by [fn] propagate
    after cleanup. Non-native Eio filesystems return {!Error.Policy_denied},
    since they cannot provide a cross-process lock; native lock acquisition
    failures return {!Error.Network_error}. *)

val with_dir_lock :
  Eio.Fs.dir_ty Eio.Path.t -> (unit -> 'a) -> ('a, Error.t) result
(** [with_dir_lock dir fn] is {!with_lock} for an exact profile directory. It is
    provided to the base and crypto stores, which address the profile by
    directory rather than by a {!Profile_store.t} handle. *)

val atomic_write : path:_ Eio.Path.t -> data:string -> (unit, Error.t) result
(** [atomic_write ~path ~data] writes [data] to a unique same-directory
    temporary file, with mode 0600, syncs the file, and atomically renames it
    over [path]. Temporary files are removed on every failure, including
    cancellation. It returns a typed error if [path] has no basename or a unique
    temporary name cannot be reserved. Other filesystem failures raise [Eio.Io].
    On native filesystems the parent directory is also synced after rename; a
    directory-sync failure returns {!Error.Network_error}. Non-native Eio
    filesystems provide only the file-sync guarantee. *)

(** {1 Reading and writing}

    A [load_] is [Ok None] when the file is absent and [Error] when it is there
    but does not parse, so a caller can tell a fresh profile from a damaged one.
    A [save_] writes through {!atomic_write}, so a crash mid-write leaves the
    previous file intact rather than truncated, and then replaces the file. Both
    fail with {!Error.Json_error} carrying the codec's message, and neither
    catches a filesystem failure.

    Raises [Eio.Io] on a filesystem failure. *)

val load_session : t -> (Session.Session_file.t option, Error.t) result
(** [load_session t] is the session file in [t]. *)

val save_session : t -> Session.Session_file.t -> (unit, Error.t) result
(** [save_session t s] writes [s] as the session file in [t]. *)

val save_login :
  t ->
  clock:_ Eio.Time.clock ->
  Session.Session_file.t ->
  (unit, Error.t) result
(** [save_login t ~clock session] persists credentials from a successful new
    login while holding the refresh and profile locks. Only after the session is
    durable does it remove any previous refresh marker, including malformed
    markers, and sync the directory. A failed session write leaves the marker
    intact. Waiting for the refresh lock is cancellable; the commit is
    protected.

    Use this only for freshly issued login credentials. Ordinary sync/metadata
    updates must use {!update_session}; they must not clear an uncertain
    refresh. Do not call this from inside a refresh callback. Filesystem
    failures may raise [Eio.Io]. *)

val update_session :
  t ->
  (Session.Session_file.t -> Session.Session_file.t) ->
  (unit, Error.t) result
(** [update_session t f] reloads the current session while holding {!with_lock},
    saves [f current] atomically under the same lock, and thereby avoids
    overwriting changes made by another profile handle. It returns a typed error
    when the session is absent or cannot be decoded. Exceptions raised by [f]
    propagate after the lock is released. *)

val refresh_session_prepared :
  t ->
  clock:_ Eio.Time.clock ->
  expected:Session.Session_file.t ->
  prepare:
    (Session.Session_file.t ->
    (unit -> (Session.Auth.t, Error.t) result, Error.t) result) ->
  (Session.Session_file.t, Error.t) result
(** [refresh_session_prepared t ~clock ~expected ~prepare] has the coordination
    and persistence contract of {!refresh_session}, with a read-only preparation
    phase. After reloading and checking the profile, [prepare latest] performs
    discovery/validation and returns an exchange thunk. Preparation must never
    consume a refresh token. Its errors and cancellation leave no uncertainty
    marker, so a later attempt can retry it. The marker is made durable before
    invoking the exchange thunk; exchange failures remain conservative.

    Preparation and exchange hold the refresh lock, but not the profile lock. An
    already rotated session is adopted without calling [prepare]. *)

val refresh_session :
  t ->
  clock:_ Eio.Time.clock ->
  expected:Session.Session_file.t ->
  refresh:(Session.Session_file.t -> (Session.Auth.t, Error.t) result) ->
  (Session.Session_file.t, Error.t) result
(** [refresh_session t ~clock ~expected ~refresh] serializes token exchanges
    across processes using [.refresh.lock]. It reloads the profile after waiting
    and adopts already rotated tokens without calling [refresh]. A successful
    exchange is persisted before returning, preserving concurrent sync metadata.
    A missing or different login is never overwritten. Both lock waiting and the
    exchange are cancellable; short profile commits defer cancellation.

    An on-disk marker prevents retrying potentially consumed tokens after an
    interrupted exchange or an error. Such a session requires a new login unless
    a completed rotation was already saved. Persist fresh login credentials with
    {!save_login} to retire a previous marker. This conservative policy also
    applies to transient errors returned by [refresh]. Callers must not perform
    another exchange or persist stale credentials in an update callback. *)

val load_device_keys : t -> (Session.Device_keys.t option, Error.t) result
(** [load_device_keys t] is the device keys file in [t]. *)

val save_device_keys : t -> Session.Device_keys.t -> (unit, Error.t) result
(** [save_device_keys t k] writes [k] as the device keys file in [t]. *)

val load_one_time_keys :
  t -> (Session.One_time_keys_file.t option, Error.t) result
(** [load_one_time_keys t] is the one-time-keys file in [t]. *)

val save_one_time_keys :
  t -> Session.One_time_keys_file.t -> (unit, Error.t) result
(** [save_one_time_keys t k] writes [k] as the one-time-keys file in [t]. *)

val load_olm_sessions :
  t -> (Session.Olm_sessions_file.t option, Error.t) result
(** [load_olm_sessions t] is the Olm sessions file in [t]. *)

val save_olm_sessions :
  t -> Session.Olm_sessions_file.t -> (unit, Error.t) result
(** [save_olm_sessions t s] writes [s] as the Olm sessions file in [t]. *)

val load_megolm_inbound :
  t -> (Session.Megolm_inbound_file.t option, Error.t) result
(** [load_megolm_inbound t] is the inbound Megolm sessions file in [t]. *)

val save_megolm_inbound :
  t -> Session.Megolm_inbound_file.t -> (unit, Error.t) result
(** [save_megolm_inbound t s] writes [s] as the inbound Megolm sessions file in
    [t]. *)

val load_megolm_outbound :
  t -> (Session.Megolm_outbound_file.t option, Error.t) result
(** [load_megolm_outbound t] is the outbound Megolm sessions file in [t]. *)

val save_megolm_outbound :
  t -> Session.Megolm_outbound_file.t -> (unit, Error.t) result
(** [save_megolm_outbound t s] writes [s] as the outbound Megolm sessions file
    in [t]. *)

val clear : t -> unit
(** [clear t] deletes every file this module writes, leaving the directory.
    Files other modules keep in the same profile are untouched.

    Raises [Eio.Io] if a file cannot be removed. *)
