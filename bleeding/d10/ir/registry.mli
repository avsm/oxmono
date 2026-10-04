(** Remote archive registry for d10ir consolidated source archives.

    Mirrors {!D10.Layer}'s remote-fetch pattern: each archive is
    content-addressed by its sha256 and lives at
    [<base_url>/d10ir-archives/<sha>.tar.zst]. Fetched archives drop into
    [<cache>/d10ir/archives/<sha>.tar.zst] where {!D10ir.Direct} can pick them
    up.

    The registry is a static HTTP server (CDN, S3 bucket, GitHub release tree,
    …). No index file is needed — clients query by sha directly, since the
    d10ir.Plan.t already lists every archive sha we need. *)

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
(** [pull] downloads the archive with the given sha into
    [<cache>/d10ir/archives/<sha>.tar.zst]. Verifies the downloaded content's
    sha256 against [sha] before renaming into place. Returns [true] on success.

    No-op (returns [true]) if the archive already exists locally. Returns
    [false] on HTTP failure or sha mismatch — caller decides whether to fall
    back to inline bake or hard-error. *)

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
(** [prefetch] pulls every archive in parallel, skipping those already present.
    Used by the recipe runner to make all required sources available locally
    before kicking off the d10ir build (which itself does no network I/O). *)
