(** Optional HTTP registry for prepared source archives.

    Archives are addressed by SHA-256 at
    [<base_url>/d10ir-archives/<sha>.tar.zst] and downloaded to
    [<cache_root>/d10ir/archives/<sha>.tar.zst]. No index is required.

    Callers serialize cache writes across processes. Concurrent fibers share
    in-flight downloads by SHA, so use one cache root within one Eio domain. *)

type remote = [ `Http_remote of string ]
(** [`Http_remote base_url] fetches as
    [<base_url>/d10ir-archives/<sha>.tar.zst]. *)

val pull :
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  session:D10.Sysops.Http.session ->
  cache_root:string ->
  remote:remote ->
  sha:string ->
  bool
(** [pull ~clock ~fs ~session ~cache_root ~remote ~sha] downloads and verifies
    an archive before renaming it into place. Returns [true] when available.
    Existing files are trusted without rechecking. HTTP failures and checksum
    mismatches return [false]. Cancellation propagates.

    Failed downloads are retried three times with exponential backoff starting
    at two seconds. [OI_ARCHIVE_RETRIES] sets the attempt count. An optional
    JSON sidecar is fetched when available. *)

type prefetch_summary = {
  fetched : int;  (** Newly downloaded. *)
  cached : int;  (** Already present locally. *)
  failed : int;  (** Fetch attempts that failed. *)
  missing : string list;
      (** sha256s that are still missing locally after the fetch attempts. The
          caller should hard-error or fall back. *)
}

val prefetch :
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  session:D10.Sysops.Http.session ->
  cache_root:string ->
  remote:remote ->
  ?max_fibers:int ->
  string list ->
  prefetch_summary
(** [prefetch ~clock ~fs ~session ~cache_root ~remote shas] downloads missing
    archives and reports counts and missing hashes. [max_fibers] must be
    positive. It defaults to [OI_HTTP_PARALLEL] when that is a positive integer,
    otherwise two. *)
