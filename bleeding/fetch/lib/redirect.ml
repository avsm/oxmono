type decision = Follow | Follow_within_scope | Stop

type config = {
  max_hops : int;
  allow_downgrade : bool;
  on_hop :
    from:Middleware.Url.t ->
    to_:Middleware.Url.t ->
    Middleware.response ->
    decision;
}

let follow ~from:_ ~to_:_ _ = Follow

let v ?(max_hops = 10) ?(allow_downgrade = false) ?(on_hop = follow) () =
  if max_hops < 0 then
    invalid_arg "Fetch.Redirect.v: max_hops must be non-negative";
  { max_hops; allow_downgrade; on_hop }

let default = v ()

(* An address has no registrable domain, so it is never same-site with
   anything but itself. Recognition must be the resolver's: a dotted-quad
   test would take "0x7f.1" for a name and hand it a public suffix. The
   [zero_alloc] claim the dotted-quad scan carried does not survive the call
   into [Httpz.Ip]. *)
let is_ip_literal host =
  Httpz.Ip.is_literal host

let same_site ~from ~to_ =
  if
    Middleware.Url.scheme from <> `Https
    || Middleware.Url.scheme to_ <> `Https
    || Middleware.Url.port from <> Middleware.Url.port to_
  then false
  else
    let from_host = Middleware.Url.host from in
    let to_host = Middleware.Url.host to_ in
    if String.equal from_host to_host then true
    else if is_ip_literal from_host || is_ip_literal to_host then false
    else
      match
        #( Pubsuffix.registrable_domain from_host,
           Pubsuffix.registrable_domain to_host )
      with
      | #(Ok from_site, Ok to_site) -> String.equal from_site to_site
      | _ -> false

let within_site =
  { default with
    on_hop =
      (fun ~from ~to_ _ ->
        if same_site ~from ~to_ then Follow_within_scope else Follow) }
