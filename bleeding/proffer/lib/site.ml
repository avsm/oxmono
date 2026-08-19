(* A site is its routes, its fallback, and a decorator. A wrapper such as
   [with_auth] cannot rewrite the routes, because a route hides its handler
   behind a matcher, so it composes onto [decorate] instead. [Backend] applies
   [decorate] to the handler it selects, and to the fallback, passing the
   request's path segments so a wrapper can act on a subtree alone. *)

type 'env t = {
  routes : 'env Route.t list;
  fallback : 'env Route.handler @@ portable;
  decorate :
    (string list -> 'env Route.handler -> 'env Route.handler) @@ portable;
  (* Whether a wrapper has composed onto [decorate]. [mount] reads it, because
     it takes a sub-site's routes and nothing else, and a decoration silently
     dropped from a gated sub-site would serve it unauthenticated. *)
  decorated : bool;
}

let default_fallback _env _req = Resp.text ~status:`Not_found "Not Found\n"
let no_decoration _segs h = h

let of_routes routes =
  {
    routes;
    fallback = default_fallback;
    decorate = no_decoration;
    decorated = false;
  }

let with_fallback (fallback @ portable) t = { t with fallback }

(* A wrapper runs outside the wrappers already applied, so the site's own
   decoration is what it wraps. Stacking [with_headers] over [with_auth] puts
   the headers on the challenge too. *)

let with_headers extra t =
  let decorate segs h =
    let inner = t.decorate segs h in
    fun env req -> Resp.add_headers extra (inner env req)
  in
  { t with decorate; decorated = true }

(* [under scope segs] is whether [segs] starts with one of the prefixes in
   [scope]. An empty prefix matches every path, which is how a caller gates a
   whole site. *)
let under scope segs =
  let rec starts pfx s =
    match (pfx, s) with
    | [], _ -> true
    | pc :: pt, sc :: st -> String.equal pc sc && starts pt st
    | _ :: _, [] -> false
  in
  List.exists (fun pfx -> starts pfx segs) scope

let with_auth ~scope ~realm ~(check @ portable) t =
  (* An empty scope gates nothing, so the wrapper would serve the site open
     while reading as a gate. The prefix that gates everything is [[]], one
     keystroke away, so the empty list is refused rather than obeyed. *)
  if scope = [] then
    invalid_arg
      "Proffer.Site.with_auth: an empty scope gates nothing, so pass [[]] to \
       gate the whole site";
  (* [%S] quotes and escapes the realm, which is what a quoted-string wants.
     A realm holding a backslash or a double quote would need HTTP's escaping
     rather than OCaml's, so it is rejected here instead. *)
  if String.exists (fun c -> c = '"' || c = '\\') realm then
    invalid_arg
      (Printf.sprintf "Proffer.Site.with_auth: realm %S is not quotable" realm);
  let field = Printf.sprintf "Basic realm=%S" realm in
  (* The challenge is built per rejection rather than once, because a response
     is not portable and so cannot be captured by the decorator. *)
  let challenge () =
    Resp.add_headers
      [ ("WWW-Authenticate", field) ]
      (Resp.text ~status:`Unauthorized "Unauthorized\n")
  in
  let decorate segs h =
    let inner = t.decorate segs h in
    if under scope segs then fun env req ->
      if check (Req.header req "authorization") then inner env req
      else challenge ()
    else inner
  in
  { t with decorate; decorated = true }

(* Only the routes of [sub] are taken. Its fallback belongs to it alone, and
   its decorator would have to run under this site's, a composition the caller
   writes directly by wrapping the mounted result. Taking the routes of a
   decorated sub-site would drop a gate the caller believes is in place, so
   that is refused rather than documented. *)
let mount ~at sub t =
  if sub.decorated then
    invalid_arg
      "Proffer.Site.mount: the sub-site is wrapped, so wrap the result of \
       mount instead";
  let prefixed = List.map (fun r -> Route.prefix at r) sub.routes in
  { t with routes = t.routes @ prefixed }
