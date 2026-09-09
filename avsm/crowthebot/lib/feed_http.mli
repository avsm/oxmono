(** Bounded, credential-free HTTP reads for feed documents. *)

val normalize : string -> string
val public_address : Ipaddr.t -> bool

val public_connect : _ Eio.Net.t -> Fetch_httpz.connect
(** [public_connect net] checks resolved addresses before connecting. Use it for
    the feed client's transport so names and redirects cannot reach private
    addresses. The callback retains only [net]. *)

type response =
  | Unchanged
  | Document of {
      body : string;
      url : string;
      etag : string option;
      last_modified : string option;
    }

type t =
  url:string -> etag:string option -> last_modified:string option -> response

val create : fetch:_ Fetch.t -> clock:_ Eio.Time.Mono.t -> t
(** [create ~fetch ~clock] permits GET only, three redirects without HTTPS
    downgrades, at most 2 MiB and a 30-second deadline. [fetch] must use the
    public-address connector and carry no cookies or credentials. *)
