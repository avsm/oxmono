(** This module controls redirect following and credential-scope extension. *)

type decision =
  | Follow (** Follow using the credential scope already in force. *)
  | Follow_within_scope
  (** Follow and offer the target origin to extensible credential wrappers. *)
  | Stop (** Return the redirect response with its body unread. *)

(** A [config] bounds a redirect walk and decides each valid Location hop. *)
type config =
  { max_hops : int
  ; allow_downgrade : bool
  ; on_hop :
      from:Middleware.Url.t -> to_:Middleware.Url.t -> Middleware.response -> decision
  }

(** [v ()] follows at most ten hops, rejects HTTPS downgrades, and follows without
    extending credential scope. *)
val v
  :  ?max_hops:int
  -> ?allow_downgrade:bool
  -> ?on_hop:
       (from:Middleware.Url.t -> to_:Middleware.Url.t -> Middleware.response -> decision)
  -> unit
  -> config

(** [default] is [v ()]. *)
val default : config

(** [same_site ~from ~to_] is [true] when both URLs are HTTPS on the same port
    and their hosts are equal or share a registrable domain according to the
    Public Suffix List. Distinct IP literals are never same-site. This is
    intentionally narrower than browser SameSite because the result can extend
    the scope of origin credentials. *)
val same_site : from:Middleware.Url.t -> to_:Middleware.Url.t -> bool

(** [within_site] extends credential scope across same-site hops and follows other
    permitted hops without extension. *)
val within_site : config
