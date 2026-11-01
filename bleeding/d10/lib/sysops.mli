(** Filesystem, subprocess and HTTP operations through Eio.

    {!v} selects [gtar] when available, otherwise [tar]. Subprocesses inherit
    the environment captured at construction with non-interactive Git settings.
    HTTP downloads use Fetch with the Curl backend.

    [OI_CMD_TIMEOUT] and [OI_HTTP_TIMEOUT] set timeouts in seconds, defaulting
    to 600. Zero disables the corresponding timeout. Command timeouts raise
    [Failure]. HTTP timeouts return [false]. Cancellation propagates.
    [OI_FORCE_HTTP1], when non-empty and different from [0], forces HTTP/1.1. *)

(** {1 Initialisation} *)

type t
(** System operations context with pre-resolved tool paths. *)

val pp : t Fmt.t
(** [pp ppf t] prints an opaque context identifier. *)

type Eio.Exn.err +=
  | Cmd_failed of {
      argv : string list;
      status : [ `Exited of int | `Signaled of int ];
      output : string;
    }
        (** Raised inside [Eio.Io] by {!Cmd.run} and {!Cmd.run_inherit} for
            non-zero exits and signals. Output is captured only by [run].
            Output-reading commands use Eio's process errors instead. *)

val v :
  ?stdout:_ Eio.Flow.sink ->
  ?stderr:_ Eio.Flow.sink ->
  proc_mgr:_ Eio.Process.mgr ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  unit ->
  t
(** [v ~proc_mgr ~fs ~net ~clock ()] selects tar and captures the child
    environment. Supply [stdout] and [stderr] to stream {!Cmd.run_inherit}
    output. [fs] and [net] are retained as API parameters but currently unused.
    [clock] drives subprocess and HTTP timeouts. *)

(** {1 File queries} *)

val file_exists : _ Eio.Path.t -> bool
(** [file_exists path] is [true] if [path] exists (follows symlinks). *)

(** {1 File copying} *)

val copy_tree : t -> src:_ Eio.Path.t -> dst:_ Eio.Path.t -> unit
(** [copy_tree t ~src ~dst] copies a directory tree. Attempts CoW clone
    ([cp -ac]) first (zero-copy on APFS), falling back to [cp -a]. [dst] must be
    disposable: the fallback removes it before retrying. *)

val link_tree : t -> src:_ Eio.Path.t -> dst:_ Eio.Path.t -> unit
(** [link_tree t ~src ~dst] hardlinks all files from [src] into [dst]
    recursively via [cp -Rfl]. Falls back to [cp -Rfa] when hardlinking fails,
    including across filesystems. Creates [dst] if needed. *)

(** {1 Archive operations} *)

module Tar : sig
  val extract :
    t -> archive:_ Eio.Path.t -> dst:_ Eio.Path.t -> ?strip:int -> unit -> unit
  (** [extract t ~archive ~dst ?strip ()] extracts [archive] into [dst]. Uses
      [gtar] on macOS if available. *)

  val create_zstd : t -> src:_ Eio.Path.t -> dst:_ Eio.Path.t -> unit
  (** [create_zstd t ~src ~dst] creates a zstd-compressed tar archive at [dst]
      from the contents of directory [src]. *)
end

module Http : sig
  val fetch :
    ?on_progress:(received:int64 -> total:int64 option -> unit) ->
    t ->
    url:string ->
    dst:_ Eio.Path.t ->
    bool
  (** [fetch t ~url ~dst] downloads [url] to [dst] via the in-process HTTP
      client (Fetch with the Curl backend). Follows redirects, requires a 2xx
      response. A rejected HTTP status leaves [dst] unchanged. A transport
      failure after headers may leave a partial file. Returns [true] on success,
      [false] on any error (HTTP 4xx/5xx, network failure, TLS handshake error,
      timeout). Caller cancellation propagates.

      [on_progress] is invoked periodically (throttled to ~20Hz) with the
      running byte count. [total] is [Some n] when the server sent a
      [Content-Length] header, [None] for chunked-transfer responses where the
      size isn't known up front. A final invocation reports the byte count after
      a completed transfer. Failed transfers need not emit a final callback. *)

  val head : t -> url:string -> bool
  (** [head t ~url] issues a HEAD request via the in-process HTTP client.
      Returns [true] on a 2xx response, [false] on any other status, network
      failure, TLS error, or timeout. Caller cancellation propagates. Useful as
      a presence probe for content-addressed remote objects where the body is
      irrelevant. *)

  (** {2 Shared HTTP sessions}

      Sessions reuse a Curl client across requests. Connection reuse and
      protocol negotiation depend on the server and Curl configuration. *)

  type session
  (** A client scoped to an Eio switch. Use from fibers in one domain. *)

  val with_session : sw:Eio.Switch.t -> t -> (session -> 'a) -> 'a
  (** [with_session ~sw t f] creates a session bound to [sw] and runs
      [f session]. Connection pools live for [sw]'s lifetime. *)

  val fetch_session :
    ?on_progress:(received:int64 -> total:int64 option -> unit) ->
    session ->
    url:string ->
    dst:_ Eio.Path.t ->
    bool
  (** Same contract as {!fetch} but reuses [session]'s connection pool. *)
end

(** {1 Low-level command execution} *)

module Cmd : sig
  val run : t -> string list -> unit
  (** [run t args] executes [args] as a subprocess, raising
      [Eio.Io (Cmd_failed _, _)] on non-zero exit or signal. Stdout and stderr
      are captured silently. *)

  val run_out : t -> string list -> string
  (** [run_out t args] executes [args] and returns its trimmed stdout.
      Subprocess stderr is left attached to the parent's stderr. Chatty commands
      like [git ls-remote] (redirect warnings) will leak there. Use
      {!run_out_quiet} when that's not desired. *)

  val run_out_quiet : t -> string list -> string
  (** Like {!run_out} but routes the subprocess's stderr to a throwaway buffer
      so warnings / progress lines don't reach the parent's stderr. Useful for
      query-shaped commands where we only care about stdout. *)

  val run_inherit : t -> string list -> unit
  (** Like {!run} but the subprocess inherits the parent's stdout and stderr so
      any progress or error output is shown to the user as the command runs. The
      failure message is short ("git exited 128") because git's own output
      already explained the problem. Requires [t] to have been created with
      [~stdout] / [~stderr]. Falls back silently to {!run} when they aren't set. *)
end
