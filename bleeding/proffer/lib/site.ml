(* Wrappers compose through [decorate] because routes hide handlers behind matchers. The
   backend decorates matched handlers and generated responses. *)

module St = Httpz.Res

type 'env t =
  { routes : 'env Route.t list
  ; fallback : 'env Route.handler @@ portable
  ; (* A decorator runs the handler rather than returning a wrapped one, which
       would be a heap closure on every response of a decorated site. *)
    decorate :
      string @ local
      -> 'env Route.handler @ local
      -> 'env
      -> Req.t @ local
      -> Resp.respond @ local
      -> unit
      @@ portable
  ; (* Mounting a decorated sub-site is rejected to avoid dropping wrappers. *)
    decorated : bool
  }

let default_fallback _env (_req : Req.t @ local) (respond : Resp.respond @ local) =
  Resp.text respond ~status:St.Not_found "Not Found\n"
;;

let no_decoration _path (h : _ Route.handler @ local) env (req : Req.t @ local)
    (respond : Resp.respond @ local) =
  let () = (h env) req respond in
  ()
;;

let of_routes routes =
  { routes; fallback = default_fallback; decorate = no_decoration; decorated = false }
;;

let with_fallback (fallback : _ Route.handler @ portable) t = { t with fallback }

(* A wrapper runs outside the wrappers already applied, so the site's own decoration is
   what it wraps. Stacking [with_headers] over [with_auth] puts the headers on the
   challenge too. *)

(* Validate field syntax once; response-specific overlap checks run when the
   decorated response is known. Handler fields come first when a name repeats. *)
let with_headers extra t =
  let extra = Headers.of_list extra in
  Resp.check_headers extra;
  let decorate (segs : string @ local) (h : _ Route.handler @ local) env
    (req : Req.t @ local) (respond : Resp.respond @ local) =
    let local_ decorated : Resp.respond =
      fun d ->
      let local_ d = Resp.with_headers d extra in
      let () = respond d in
      ()
    in
    let () = t.decorate segs h env req decorated in
    ()
  in
  { t with decorate; decorated = true }
;;

(* Plain recursion rather than closures over [path], so a scope test
   allocates nothing. *)
let rec starts (path : string @ local) n pfx i =
  match pfx with
  | [] -> true
  | pc :: pt ->
    let off = Pct.seg_start path i n in
    off < n
    &&
    let stop = Pct.seg_stop path off n in
    Pct.seg_is path off stop pc && starts path n pt stop
;;

let rec under scope (path : string @ local) =
  match scope with
  | [] -> false
  | pfx :: rest -> starts path (String.length path) pfx 0 || under rest path
;;

let with_auth ~scope ~realm ~(check : (string option @ local -> bool) @ portable) t =
  (* Refuse a likely typo that would otherwise leave the site unprotected. *)
  if scope = []
  then
    invalid_arg
      "Proffer.Site.with_auth: an empty scope gates nothing, so pass [[]] to gate the \
       whole site";
  let invalid_segment segment =
    String.equal segment "" || String.equal segment "."
    || String.equal segment ".."
    || String.contains segment '/' || String.contains segment '\\'
    || String.exists
         (fun c ->
           let code = Char.code c in
           code < 0x20 || code = 0x7f)
         segment
  in
  List.iter
    (List.iter (fun segment ->
       if invalid_segment segment then
         invalid_arg
           (Printf.sprintf
              "Proffer.Site.with_auth: scope segment %S is ambiguous or invalid"
              segment)))
    scope;
  let invalid_realm_char c =
    let n = Char.code c in
    c = '"' || c = '\\' || (n < 0x20 && c <> '\t') || n = 0x7f
  in
  if String.exists invalid_realm_char realm
  then
    invalid_arg (Printf.sprintf "Proffer.Site.with_auth: realm %S is not quotable" realm);
  let field = "Basic realm=\"" ^ realm ^ "\"" in
  let challenge (respond : Resp.respond @ local) =
    let () =
      Resp.v
        respond
        ~status:St.Unauthorized
        ~headers:(stack_ [ Headers.h_local Httpz.Header_name.Www_authenticate field ])
        ~content_type:(This "text/plain; charset=utf-8")
        (Body.String "Unauthorized\n")
    in
    ()
  in
  let decorate (segs : string @ local) (h : _ Route.handler @ local) env
      (req : Req.t @ local) (respond : Resp.respond @ local) =
    let rec authorization_count count = function
      | [] -> count
      | (field : Headers.field) :: rest ->
        authorization_count
          (if Headers.same_name field.name Httpz.Header_name.Authorization
           then count + 1
           else count)
          rest
    in
    if
      (not (under scope segs))
      || (authorization_count 0 (Req.headers req) <= 1
          && check (Req.header req Httpz.Header_name.Authorization))
    then (
      let () = t.decorate segs h env req respond in
      ())
    else challenge respond
  in
  { t with decorate; decorated = true }
;;

(* Mount only routes. Reject wrappers that would otherwise be silently lost. *)
let mount ~at sub t =
  if sub.decorated
  then
    invalid_arg
      "Proffer.Site.mount: the sub-site is wrapped, so wrap the result of mount instead";
  let prefixed = List.map (fun r -> Route.prefix at r) sub.routes in
  { t with routes = t.routes @ prefixed }
;;

let routes t = t.routes
let fallback t = t.fallback
let decorate t = t.decorate
