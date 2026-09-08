module St = Httpz.Res

type admitted = {
  max_body : int64;
  run : (Req.t @ local -> Resp.respond @ local -> unit);
}
type 'env admission_handler =
  'env -> (Req.t @ local -> Resp.respond @ local -> admitted option) @ local
type 'env endpoint = {
  at : string list;
  admit : 'env admission_handler @@ portable;
}
type admission = Ordinary | Responded | Accepted of admitted

let accept ~max_body (run : Req.t @ local -> Resp.respond @ local -> unit) =
  if max_body < 0L then invalid_arg "Proffer.Site.accept: negative body limit";
  { max_body; run }

type 'env t =
  { routes : 'env Route.t list
  ; endpoints : 'env endpoint list
  ; fallback : 'env Route.handler @@ portable
  ; (* A wrapper runs the handler rather than returning a wrapped one, which
       would be a heap closure on every response of a site with wrappers. *)
    run_with_wrappers :
      string @ local
      -> 'env Route.handler @ local
      -> 'env
      -> Req.t @ local
      -> Resp.respond @ local
      -> unit
      @@ portable
  ; (* Mounting would discard these wrappers, so reject such sub-sites. *)
    has_wrappers : bool
  }

let default_fallback _env (_req : Req.t @ local) (respond : Resp.respond @ local) =
  Resp.text respond ~status:St.Not_found "Not Found\n"
;;

let run_without_wrappers _path (h : _ Route.handler @ local) env (req : Req.t @ local)
    (respond : Resp.respond @ local) =
  let () = (h env) req respond in
  ()
;;

let of_routes routes =
  { routes
  ; endpoints = []
  ; fallback = default_fallback
  ; run_with_wrappers = run_without_wrappers
  ; has_wrappers = false
  }
;;

let with_fallback (fallback : _ Route.handler @ portable) t = { t with fallback }

(* Validate field syntax once; response-specific overlap checks run when the
   combined response is known. Handler fields come first when a name repeats. *)
let with_headers extra t =
  let extra = Headers.of_list extra in
  Resp.check_headers extra;
  let run_with_wrappers (segs : string @ local) (h : _ Route.handler @ local) env
    (req : Req.t @ local) (respond : Resp.respond @ local) =
    let local_ wrapped_respond : Resp.respond =
      fun d ->
      let local_ d = Resp.with_headers d extra in
      let () = respond d in
      ()
    in
    let () = t.run_with_wrappers segs h env req wrapped_respond in
    ()
  in
  { t with run_with_wrappers; has_wrappers = true }
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
  List.iter
    (List.iter (fun segment ->
       if Static.invalid_segment segment then
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
        ~content_type:(This Resp.text_type)
        (Body.String "Unauthorized\n")
    in
    ()
  in
  let run_with_wrappers (segs : string @ local) (h : _ Route.handler @ local) env
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
      let () = t.run_with_wrappers segs h env req respond in
      ())
    else challenge respond
  in
  { t with run_with_wrappers; has_wrappers = true }
;;

(* Mount only routes. Reject wrappers that would otherwise be silently lost. *)
let mount ~at sub t =
  if sub.has_wrappers || sub.endpoints <> []
  then
    invalid_arg
      "Proffer.Site.mount: the sub-site is wrapped, so wrap the result of mount instead";
  List.iter
    (fun segment ->
      if Static.invalid_segment segment then
        invalid_arg
          (Printf.sprintf
             "Proffer.Site.mount: prefix segment %S is ambiguous or invalid"
             segment))
    at;
  let prefixed = List.map (fun r -> Route.prefix at r) sub.routes in
  { t with routes = t.routes @ prefixed }
;;

let routes t = t.routes
let fallback t = t.fallback
let run_with_wrappers t = t.run_with_wrappers

let with_endpoint ~at ~(admit : _ admission_handler @ portable) t =
  List.iter (fun s -> if Static.invalid_segment s then
    invalid_arg "Proffer.Site.with_endpoint: invalid prefix") at;
  let rec prefix a b = match a, b with
    | [], _ -> true
    | x :: xs, y :: ys when x = y -> prefix xs ys
    | _ -> false in
  if List.exists (fun e -> prefix at e.at || prefix e.at at) t.endpoints then
    invalid_arg "Proffer.Site.with_endpoint: overlapping endpoints";
  { t with endpoints = { at; admit } :: t.endpoints }

let admit t env (req : Req.t @ local) (respond : Resp.respond @ local) =
  let rec find = function
    | [] -> Ordinary
    | endpoint :: rest ->
        if under [endpoint.at] (Req.path req) then begin
          let accepted = ref None in
          let () = t.run_with_wrappers (Req.path req)
            (fun env req respond ->
              accepted := endpoint.admit env req respond) env req respond in
          match !accepted with
          | None -> Responded
          | Some accepted ->
              Accepted { accepted with run = (fun req respond ->
                let () = t.run_with_wrappers (Req.path req)
                  (fun _env req respond ->
                    let () = accepted.run req respond in ())
                  env req respond in ()) }
        end else find rest in
  let result = find t.endpoints in
  result
