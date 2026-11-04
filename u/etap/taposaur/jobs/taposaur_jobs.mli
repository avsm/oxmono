(** Durable acquisition, conversion and catalogue refresh work.

    {1 NAME}
    Taposaur_jobs - typed Duraml queues for the paper archive.

    {1 SYNOPSIS}
    {[
      let queues = Taposaur_jobs.Queues.v store in
      let entries = Taposaur_jobs.Plan.review ~configured:["elsevier"]
        ~converters:[Grobid; Pub2tei] ~target:Fulltext ~revision:1
        ~requested_by:"avsm2" reviews ~snapshot:2 in
      ignore (Taposaur_jobs.Submit.entries queues entries);
      Taposaur_jobs.Worker.acquire ~sw ~mono_clock:env#mono_clock
        acquisition queues ~strategy:"elsevier"
    ]}

    {1 DESCRIPTION}
    Work is described by small, versioned JSON payloads that name a paper,
    the strategy or route applied to it and the configuration revision
    that makes a repeat deliberate. Credentials, HTTP clients, staging
    trees and archive capabilities are worker inputs, never payloads.

    Three families of queues exist in one Duraml store:
    - One acquisition queue per publisher strategy, so that a single
      worker per queue can pace requests to that publisher.
    - One conversion queue per route, GROBID for PDFs and Pub2TEI for
      publisher XML, with its own bounded worker pool.
    - One catalogue refresh queue that follows every archive publication.

    Keys derive from the semantic identity of the work. Submitting the same
    paper, strategy and revision again returns the existing job instead of
    creating more work. A review snapshot is attribution carried in the
    payload for audit and inspection. It does not alter review membership.

    {1 OUTCOMES}
    Handlers return typed completions. Busy publishers, converters and
    transports become scheduled retries with the server's [Retry-After]
    when it was supplied. Refusals, invalid content and unconfigured
    credentials become permanent failures with stable codes. Every
    attempt is audited through the supplied session, and the durable
    Duraml events are bridged to the audit by {!Audit_bridge}.

    Execution is at least once. Publication into the archive is idempotent
    through {!Taposaur_accept}, which reports content already held instead
    of duplicating it. Successors are published atomically with the
    completion that produced them.

    Each handler runs inside one audit operation stamped with its job and
    attempt from {!Duraml_client.attempt}, whose result carries the outcome
    and a note with the retry reason or failure code. The handler passes
    that operation's session to the download, staging, acceptance and
    catalogue libraries, so their records nest beneath it and the audit
    reconstructs why a job retried or failed.

    {1 CONCURRENCY}
    Worker concurrency is per invocation. {!Worker.own_queue} takes a host
    lock so one process serves a publisher queue at a time, which with the
    paced clients supplied by the caller bounds the request rate to that
    publisher. Conversion and refresh pools are independent. Archive
    directory locks are held only while files are published. *)

(** {1 Payloads} *)

module Attribution : sig
  type t = { slug : string; snapshot : int }
  (** The review snapshot that requested the work. *)
  val jsont : t Jsont.t
end

module Artifact : sig
  type t = {
    file : string;
    format : Taposaur.Manifest.Format.t;
    sha256 : string;
    bytes : int64;
  }
  (** A content reference: a filename in the paper directory plus the
      digest and size that make it verifiable. *)
  val jsont : t Jsont.t
  val of_manifest : Taposaur.Manifest.Artifact.t -> t option
  (** [of_manifest a] is [None] when [a] records no digest. *)
end

module Acquisition : sig
  type request = {
    uid : Taposaur.Uid.t;
    strategy : string;  (** One of {!strategies}. *)
    revision : int;  (** Strategy configuration revision, positive. *)
    publisher : string;  (** Publisher evidence used for routing. *)
    review : Attribution.t option;
    requested_by : string;  (** Actor who planned the work. *)
  }
  type result = {
    artifacts : Artifact.t list;  (** Accepted into the archive. *)
    attempts : (string * string) list;
        (** Source name and failure message for each unsuccessful source. *)
  }
  val strategies : string list
  (** [strategies] is {!Taposaur_download.strategies} plus [oa], the
      open-access and CORE fallbacks for publishers without a strategy. *)
  val key : request -> string
  (** [key r] is [acquire:UID:STRATEGY:vREVISION]. Raises
      [Invalid_argument] when the key exceeds Duraml's 256-byte limit. *)
  val queue : strategy:string -> string
  (** [queue ~strategy] is [acquire.STRATEGY]. *)
  val request_jsont : request Jsont.t
  val result_jsont : result Jsont.t
end

module Conversion : sig
  type request = {
    uid : Taposaur.Uid.t;
    input : Artifact.t;  (** The archived PDF or publisher XML. *)
    route : Taposaur.Manifest.Route.t;  (** [Grobid] or [Pub2tei]. *)
    converter : string;
        (** Converter and configuration identity, such as [grobid-1]. *)
  }
  (** Conversions are shared between reviews, so the payload is exactly
      the key's identity and carries no attribution. A successor staged by
      two acquisitions therefore reconciles instead of conflicting. *)
  type result = {
    output : Artifact.t;  (** The TEI accepted into the archive. *)
    publisher : string option;  (** Publisher named by the TEI header. *)
  }
  val key : request -> string
  (** [key r] is [convert:UID:ROUTE:INPUT_SHA256:CONVERTER]. Raises
      [Invalid_argument] when the key exceeds Duraml's 256-byte limit. *)
  val queue : Taposaur.Manifest.Route.t -> string
  (** [queue route] is [convert.ROUTE]. *)
  val request_jsont : request Jsont.t
  val result_jsont : result Jsont.t
end

module Refresh : sig
  type request = {
    uid : Taposaur.Uid.t;
    manifest_sha256 : string;  (** The publication that needs indexing. *)
  }
  type result = {
    index : string;  (** [added], [updated] or [unchanged]. *)
    catalogue : string;  (** [updated] or [partial]. *)
  }
  val key : request -> string
  (** [key r] is [refresh:UID:MANIFEST_SHA256]. *)
  val queue : string
  (** [queue] is [catalogue.refresh]. *)
  val request_jsont : request Jsont.t
  val result_jsont : result Jsont.t
end

(** {1 Queues} *)

module Queues : sig
  type t
  (** Typed bindings for every queue family in one store. *)
  val v : _ Duraml.t -> t
  (** [v store] binds the queues. It creates no jobs or workers. *)
  val store : t -> Duraml.plain
  val acquisition : t -> strategy:string ->
    (Acquisition.request, Acquisition.result) Duraml_client.Queue.t
  (** [acquisition t ~strategy] raises [Invalid_argument] for a strategy
      outside {!Acquisition.strategies}. *)
  val conversion : t -> Taposaur.Manifest.Route.t ->
    (Conversion.request, Conversion.result) Duraml_client.Queue.t
  (** [conversion t route] raises [Invalid_argument] for [S2orc]. *)
  val refresh : t -> (Refresh.request, Refresh.result) Duraml_client.Queue.t
  val names : string list
  (** [names] lists every queue name, for inspection and ownership. *)
end

(** {1 Planning and submission} *)

module Plan : sig
  type target = Fulltext | Tei
  (** [Fulltext] selects papers without a PDF or publisher XML.
      [Tei] selects papers with such an input and no TEI. *)
  type decision =
    | Acquire of Acquisition.request
    | Convert of Conversion.request
    | Held  (** The requested artifact already exists. *)
    | Refused of string
        (** The archive records a permanent refusal by the strategy that
            would be used, reported only with [skip_refused]. *)
    | Unsupported of string
        (** No implemented strategy or route, with the reason. *)
    | Unconfigured of string
        (** An implemented strategy whose credentials are not configured. *)
    | Invalid of string  (** The DOI or manifest could not be used. *)
  type entry = { doi : string; publisher : string; decision : decision }
  type summary = {
    total : int; acquire : int; convert : int; held : int; refused : int;
    unsupported : int; unconfigured : int; invalid : int;
  }
  val summarize : entry list -> summary
  val review : ?limit:int -> ?members:Taposaur.Uid.t list ->
    ?catalog:Taposaur_catalog.read Taposaur_catalog.t ->
    ?archive:Taposaur_archive.t -> ?skip_refused:bool ->
    configured:string list -> converters:Taposaur.Manifest.Route.t list ->
    target:target -> revision:int -> requested_by:string ->
    _ Taposaur_review_store.t -> snapshot:int -> entry list
  (** [review store ~snapshot ...] reads the frozen snapshot rows in DOI
      order and decides each member. [configured] names strategies whose
      credentials the caller has, so an implemented but unconfigured
      strategy is reported rather than queued. A member whose observation
      names no publisher is routed by the owner of its DOI prefix
      ({!Taposaur_download.prefix_publisher}), so an unindexed member on a
      Wiley, Elsevier, Springer, PLOS, Taylor & Francis or Cambridge
      prefix is acquirable; without that it is [Unsupported]. A publisher
      without a strategy is queued on [oa] when that is configured, which
      needs the metadata lookup service, and is otherwise [Unsupported]. [converters] names routes
      with a configured service. With [catalog], holdings come from the
      catalogue row, or from the frozen snapshot for an unindexed member,
      and no manifest is read for [Fulltext] planning. Without it, the
      current [archive] manifest decides, else the frozen snapshot. The
      catalogue is a cache, so a paper acquired since its last refresh is
      planned again and the worker then succeeds with the held artifacts
      without fetching. [Tei] targets read the [archive] manifest for the
      input digest and are reported as [Invalid] without it. With
      [skip_refused] (default false) the [archive] manifest of each member
      that would be acquired is read, and a permanent refusal recorded
      there for the chosen strategy makes the member [Refused]; this costs
      one manifest read per such member. [members] restricts decisions to
      those snapshot members, so a small selection costs no manifest reads
      for the rest; a member outside the snapshot raises
      [Invalid_argument]. [limit] bounds the rows examined. Snapshot rows
      are never modified. *)
  val entry_jsont : entry Jsont.t
  (** [entry_jsont] encodes decisions for CLI reports. *)
end

module Submit : sig
  type outcome = {
    added : int; existing : int; conflicts : int; skipped : int;
  }
  (** [skipped] counts entries whose decision is not work. *)
  val entries : ?not_before:Ptime.t -> ?lane:string -> Queues.t ->
    Plan.entry list -> outcome * (Plan.entry * Duraml.Id.t option) list
  (** [entries queues plan] submits each [Acquire] and [Convert] decision
      with its semantic key. Each submission is a separate transaction.
      Repeating a submission reconciles rather than duplicates. A conflict
      means the same work was already requested with different
      attribution, such as another review or actor, and that existing job
      is returned. [None] identifies only skipped entries. [lane] is the
      Duraml fair-share lane of every submitted job; successors staged by
      the workers inherit it. Name it by batch, such as the review
      snapshot, so a later single request is not queued behind the
      batch. *)
end

(** {1 Workers} *)

module Worker : sig
  type acquisition = {
    downloader : Taposaur_download.t;
        (** Configure with [busy_retries = 0] so busy responses return as
            scheduled retries instead of sleeping on a leased worker. *)
    reader : Taposaur_archive.t;  (** Read access to the archive. *)
    staging_root : Eio.Fs.dir_ty Eio.Path.t;
        (** The staging tree. Each attempt opens its own
            {!Taposaur_store} on it, enclosed in the attempt's audit. *)
    sw : Eio.Switch.t;  (** Owns the staging stores the handler opens. *)
    archive : Taposaur_accept.t;
    converters : Taposaur.Manifest.Route.t list;
        (** Routes to stage as successors for accepted inputs. *)
    converter : Taposaur.Manifest.Route.t -> string;
        (** Converter identity for each staged route. *)
    clock : float Eio.Time.clock_ty Eio.Resource.t;
    audit : Taposaur_audit.session option;
    cmd : string;  (** Writer name recorded in archive events. *)
  }
  val acquisition_handler : acquisition -> Queues.t ->
    sw:Eio.Switch.t -> Acquisition.request ->
    Acquisition.result Duraml_client.Completion.t
  (** [acquisition_handler c queues] fetches into staging with
      {!Taposaur_download.fetch}, accepts every validated artifact into the
      archive, and stages one conversion per accepted input plus one
      catalogue refresh as atomic successors. Once the strategy's own
      source has refused the paper, transient failures of the open-access
      and CORE fallbacks no longer schedule a retry, so a refusing
      publisher is not asked again on the fallbacks' account; the [oa]
      strategy treats every fallback as its own. When the archive already
      holds a PDF or publisher XML for the paper, the handler succeeds
      with those artifacts without fetching, so a replayed attempt does
      not download watermarked bytes twice. Busy or transport failures
      with no artifact retry after the publisher's [Retry-After], else
      five minutes for busy responses and one minute for transport
      failures and timeouts. Refusals fail with codes such as [http_403],
      [invalid_content], [unconfigured] and [no_source]. A staging tree or
      archive directory that refuses the paper fails with
      [staging_refused] or [archive_refused]. A permanent failure other
      than missing credentials is also recorded in the archive manifest
      with {!Taposaur_accept.record_refusal}, so later planning with
      [skip_refused] leaves the paper alone. An artifact from any source
      counts as success even when the publisher refused. Retry delays are
      clamped to between 30 seconds and one day. I/O failures retry.
      Audit failures and programming errors propagate and stop the pool. *)

  type conversion = {
    reader : Taposaur_archive.t;
    archive : Taposaur_accept.t;
    grobid : Taposaur_services.Grobid.t option;
    pub2tei : Taposaur_services.Pub2tei.t option;
    timeout : float;  (** Seconds per converter request. *)
    max_bytes : int;  (** Input and output byte limit. *)
    clock : float Eio.Time.clock_ty Eio.Resource.t;
    mono_clock : Eio.Time.Mono.ty Eio.Resource.t;
        (** Bounds converter requests. [clock] stamps records. *)
    audit : Taposaur_audit.session option;
    cmd : string;
  }
  val conversion_handler : conversion -> Queues.t ->
    sw:Eio.Switch.t -> Conversion.request ->
    Conversion.result Duraml_client.Completion.t
  (** [conversion_handler c queues] reads the input from the archive,
      verifies its digest, calls the route's converter and validates the
      response before acceptance. A TEI already recorded for the same
      input, route and converter identity succeeds without another call,
      because converter output is not byte-stable across runs. A valid
      response is well-formed TEI whose header names the paper's DOI, or
      for Pub2TEI names no DOI at all, since its input is the paper's own
      publisher XML and older articles carry none in the header. HTTP
      429, 502, 503 and 504, timeouts and transport failures retry. HTTP
      204, other error statuses, error documents, malformed TEI and
      identity mismatches are permanent: the manifest records the failure
      and the job fails with [no_content], [converter_error],
      [invalid_tei], [empty_body] or [identity_mismatch]. A missing converter fails with
      [unconfigured], an input beyond [max_bytes] with [input_too_large],
      and a publisher envelope without exactly one article with
      [invalid_input]. Publisher XML is passed through
      {!Taposaur_document.Publisher_xml.unwrap} before Pub2TEI. Success
      stages a catalogue refresh. *)

  type refresh = {
    reader : Taposaur_archive.t;
    archive : Taposaur_accept.t;
    catalog : Taposaur_catalog.write Taposaur_catalog.t;
    publisher_lookup : Taposaur.Doi.t ->
      Taposaur.Publisher_evidence.t option;
    abstract_lookup : Taposaur.Doi.t ->
      Taposaur.Abstract_evidence.t option;
        (** External evidence applied by the resolver when the archive
            supplies no abstract, so a refresh keeps a filled one. *)
    clock : float Eio.Time.clock_ty Eio.Resource.t;
    audit : Taposaur_audit.session option;
  }
  val refresh_handler : refresh -> Queues.t ->
    sw:Eio.Switch.t -> Refresh.request ->
    Refresh.result Duraml_client.Completion.t
  (** [refresh_handler c queues] rewrites the paper's index row in its
      shard file, publishes that one row into the catalogue with
      {!Taposaur_catalog.Shard.upsert}, importing the whole shard only when
      the catalogue has never seen it, and resolves the paper's metadata
      with {!Taposaur_indexing.Enrich.run}. An unavailable manifest or a
      superseded snapshot retries. Catalogue transactions stay short, so
      other catalogue writers are not starved. *)

  val own_queue : ?component:string -> prefix:string -> string ->
    Taposaur_lock.t
  (** [own_queue ~prefix queue] takes the host lock [PREFIX.QUEUE.lock],
      so that one process serves [queue] of the store at [prefix] at a
      time. Use the queue database path as [prefix]. Raises
      {!Taposaur_lock.Busy} when another worker owns it. Release it after
      the worker pool stops. *)

  val acquire : sw:Eio.Switch.t -> mono_clock:_ Eio.Time.Mono.t ->
    ?concurrency:int -> ?lease:Ptime.Span.t -> ?max_attempts:int ->
    ?stop:unit Eio.Promise.t -> acquisition -> Queues.t ->
    strategy:string -> unit
  (** [acquire ~sw ... c queues ~strategy] starts the acquisition pool for
      one strategy in [sw]. Defaults are concurrency 1, a 300-second lease
      and 8 attempts. *)
  val convert : sw:Eio.Switch.t -> mono_clock:_ Eio.Time.Mono.t ->
    ?concurrency:int -> ?lease:Ptime.Span.t -> ?max_attempts:int ->
    ?stop:unit Eio.Promise.t -> conversion -> Queues.t ->
    Taposaur.Manifest.Route.t -> unit
  (** [convert ~sw ... c queues route] starts a conversion pool. Defaults
      are concurrency 2, a 600-second lease and 5 attempts. *)
  val refresh : sw:Eio.Switch.t -> mono_clock:_ Eio.Time.Mono.t ->
    ?concurrency:int -> ?lease:Ptime.Span.t -> ?max_attempts:int ->
    ?stop:unit Eio.Promise.t -> refresh -> Queues.t -> unit
  (** [refresh ~sw ... c queues] starts the refresh pool. Defaults are
      concurrency 1, a 300-second lease and 10 attempts. Concurrency above
      one shares a single catalogue write handle. *)
end

(** {1 Inspection} *)

module Inspect : sig
  type counts = {
    queue : string; queued : int; running : int; done_ : int;
    failed : int; cancelled : int;
  }
  val counts : _ Duraml.t -> counts list
  (** [counts store] counts jobs per queue and state with
      {!Duraml.count}, reading no job records. *)
  val state_of_string : string -> Duraml.state option
  (** [state_of_string name] inverts {!Duraml.state_name}. *)
  type job = {
    id : string; queue : string; key : string option; state : string;
    attempt : int; uid : string option; review : Attribution.t option;
    created : Ptime.t; updated : Ptime.t; available : Ptime.t;
    outcome : string option;
        (** [succeeded], [retry: REASON] or [failed CODE: DETAIL]. *)
    spec : Duraml.Spec.t;  (** The stored specification. *)
    parent : string option;
        (** The job whose completion staged this one, if any. *)
    lane : string;  (** The Duraml dispatch lane, [""] when unnamed. *)
  }
  val describe : Duraml.job -> job
  (** [describe j] decodes the payload to attribute the job to a paper
      and review. Undecodable payloads leave those fields empty. *)
  val list : ?queue:string -> ?state:Duraml.state -> ?after:Duraml.Id.t ->
    ?limit:int -> _ Duraml.t -> job list
  (** [list store] pages jobs in identifier order, at most [limit]
      (default 100, maximum 1000). *)
  val find : _ Duraml.t -> Duraml.Id.t -> job option
  val children : ?limit:int -> _ Duraml.t -> Duraml.Id.t -> job list
  (** [children store id] lists the successors staged by job [id], at most
      [limit] (default 100). *)
  val paper_url : job -> string option
  (** [paper_url j] is the website path of the job's paper, if attributed. *)
end

(** {1 Audit} *)

module Audit_bridge : sig
  val deliver : ?limit:int -> cursor:string -> _ Duraml.t ->
    Taposaur_audit.session -> int
  (** [deliver ~cursor store session] appends new Duraml events to the
      audit as [queue.event] notices and returns the number delivered.
      [cursor] is a local file holding the store identity and the last
      delivered sequence, guarded by the lock [CURSOR.lock] so concurrent
      deliveries for one store are refused with {!Taposaur_lock.Busy}.
      Before delivering, the newest audit records for this store are
      consulted so an event already recorded after a lost cursor write is
      not repeated. Each record carries the store identity,
      sequence, queue, job, attempt, kind, detail, key and, when decodable,
      the paper and review attribution. [limit] bounds one call, default
      1000. *)
  val run : mono_clock:_ Eio.Time.Mono.t -> ?interval:Ptime.Span.t ->
    ?stop:unit Eio.Promise.t -> cursor:string -> _ Duraml.t ->
    Taposaur_audit.session -> unit
  (** [run ~mono_clock ~cursor store session] delivers repeatedly every
      [interval] (default 5 seconds) until [stop] resolves. *)
end
