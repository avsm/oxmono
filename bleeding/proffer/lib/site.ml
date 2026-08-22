(* A site is its routes, its fallback, and a decorator. A wrapper such as
   [with_auth] cannot rewrite the routes, because a route hides its handler
   behind a matcher, so it composes onto [decorate] instead. [Backend] applies
   [decorate] to the handler it selects, and to the fallback, passing the
   request's path segments so a wrapper can act on a subtree alone. *)

module St = Httpz.Res

type 'env t = {
  routes : 'env Route.t list;
  fallback : 'env Route.handler @@ portable;
  decorate :
    (string -> 'env Route.handler -> 'env Route.handler) @@ portable;
  (* Whether a wrapper has composed onto [decorate]. [mount] reads it, because
     it takes a sub-site's routes and nothing else, and a decoration silently
     dropped from a gated sub-site would serve it unauthenticated. *)
  decorated : bool;
}

let default_fallback _env (_req : Req.t @ local)
    (respond : Resp.respond @ local) =
  Resp.text respond ~status:St.Not_found "Not Found\n"
let no_decoration _path h = h

let of_routes routes =
  {
    routes;
    fallback = default_fallback;
    decorate = no_decoration;
    decorated = false;
  }

let with_fallback (fallback : _ Route.handler @ portable) t =
  { t with fallback }

(* A wrapper runs outside the wrappers already applied, so the site's own
   decoration is what it wraps. Stacking [with_headers] over [with_auth] puts
   the headers on the challenge too. *)

(* The decorator extends the block on its way past rather than rebuilding a
   response, since a handler no longer returns one. [Headers.cat] puts the
   joined block in the caller's region, so a site-wide header costs no heap.
   Appended rather than merged, so a name a handler already set is the copy a
   client reads first. The names and values go through the same check [Resp.v]
   applies, which is what stops a decorator injecting a response split, and it
   runs once here rather than on every response. *)
let with_headers extra t =
  let extra = Headers.of_list extra in
  Headers.iter Resp.check_header extra;
  let decorate segs h =
    let inner = t.decorate segs h in
    fun env (req : Req.t @ local) (respond : Resp.respond @ local) ->
      let local_ decorated : Resp.respond =
       fun d ->
        let local_ d =
          { d with Resp.headers = Headers.cat d.Resp.headers extra }
        in
        let () = respond d in
        ()
      in
      let () = inner env req decorated in
      ()
  in
  { t with decorate; decorated = true }

(* [under scope path] is whether [path] starts with one of the prefixes in
   [scope]. An empty prefix matches every path, which is how a caller gates a
   whole site. The prefix is walked against the path where it lies, for the
   same reason dispatch is: a gate that ran on every request should not build
   a list to do it. *)
let under scope path =
  let n = String.length path in
  let rec starts pfx i =
    match pfx with
    | [] -> true
    | pc :: pt ->
        let off = Pct.seg_start path i n in
        off < n
        &&
        let stop = Pct.seg_stop path off n in
        Pct.seg_is path off stop pc && starts pt stop
  in
  List.exists (fun pfx -> starts pfx 0) scope

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
  let challenge (respond : Resp.respond @ local) =
    let () =
      Resp.v respond ~status:St.Unauthorized
        ~headers:
          (stack_
             [ Headers.h_local Httpz.Header_name.Www_authenticate field ])
        ~content_type:"text/plain; charset=utf-8"
        (Body.String "Unauthorized\n")
    in
    ()
  in
  let decorate segs h =
    let inner = t.decorate segs h in
    if under scope segs then
      fun env (req : Req.t @ local) (respond : Resp.respond @ local) ->
        if check (Req.header req Httpz.Header_name.Authorization) then
          let () = inner env req respond in
          ()
        else challenge respond
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
