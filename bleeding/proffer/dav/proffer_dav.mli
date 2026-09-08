(** Explicit WebDAV server exports. The outbound client is [Fetch_dav]. *)

module Path : sig
  type t
  val of_segments : string list -> t
  val segments : t -> string list
  val child : t -> string -> t
  val parent : t -> t option
  val under : prefix:t -> t -> bool
  val equal : t -> t -> bool
end

type kind = File | Collection
type entry = {
  kind : kind;
  length : int64;
  etag : string option;
  modified : float;
  properties : Httpz_dav.element list;
}
exception Error of int * Httpz_dav.name list
(** [Error (status, conditions)] is a sanitized store rejection. *)

module Reader : sig
  type t
  val v : stat:(Path.t -> entry option) ->
    list:(Path.t -> (string * entry) list) ->
    read:(Path.t ->
      (entry -> (Proffer.Body.Sink.t -> unit) -> unit) @ local -> unit) -> t
  (** [v ~stat ~list ~read] is a reader capability. The callbacks must bound
      work and keep the opened file alive throughout [read]'s callback. *)
end

type lease = {
  path : Path.t;
  token : Httpz_dav.Token.t;
  principal : string;
  depth : Httpz_dav.tree_depth;
  expires : float;
  owner : Httpz_dav.element option;
}
type operation =
  | Put of Path.t * Proffer.Req.Input.t
  | Mkcol of Path.t
  | Delete of Path.t
  | Copy of { src : Path.t; dst : Path.t; overwrite : bool;
              depth : Httpz_dav.tree_depth }
  | Move of { src : Path.t; dst : Path.t; overwrite : bool }
  | Proppatch of Path.t * Httpz_dav.update list
  | Check of Path.t
  | Lock of Path.t * string * Httpz_dav.tree_depth * int *
      Httpz_dav.element option
  | Refresh of Path.t * string * Httpz_dav.Token.t * int
  | Unlock of Path.t * string * Httpz_dav.Token.t

type result = Completed of int | Written of int * entry | Locked of int * lease

module Writer : sig
  type t
  val v : reader:Reader.t -> now:(unit -> float) ->
    leases:(unit -> lease list) ->
    mutate:(operation -> guard:(Reader.t -> lease list -> unit) -> result) -> t
  (** [mutate operation ~guard] stages any input before its transaction,
      runs [guard] against current state under serialization, then commits.
      [Check] runs the guard and verifies existence without changing state.
      A failed guard must have no public effect. This provider interface is
      for trusted storage implementations. *)
  val reader : t -> Reader.t
end

type access = Read_only | Read_write
module Security : sig
  type t
  val authenticated : realm:string ->
    authenticate:(string -> string option) ->
    authorize:(string -> access option) -> t
  (** The authenticator receives one Authorization field and returns a stable
      principal. Authorization is checked again before committing a write. *)
  val public_read_only : t
end

type limits = {
  max_xml_bytes : int;
  max_xml_nodes : int;
  max_xml_depth : int;
  max_resources : int;
  max_response_bytes : int;
  max_file_bytes : int64;
  max_active : int;
}
val default_limits : limits

type t
val read_only : ?limits:limits -> ?allow_insecure_loopback:bool ->
  origin:string -> at:string list -> security:Security.t -> Reader.t -> t
val read_write : limits:limits -> ?allow_insecure_loopback:bool ->
  origin:string -> at:string list -> security:Security.t -> Writer.t -> t
(** Constructors neither open a listener nor register a route. Writers require
    explicit limits and authenticated security. Origins use HTTPS unless the
    explicit loopback-only HTTP option is supplied. *)

val mount : at:string list -> ('env -> t) @ portable ->
  'env Proffer.Site.t -> 'env Proffer.Site.t
(** [mount ~at export site] reserves [at] for the export selected from the
    handler environment. Its configured prefix must equal [at]. No default
    Proffer site invokes this function. *)
