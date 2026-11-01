(* Route selection and wrappers. Captured strings and the cold 405 Allow
   value retain their existing explicit allocation boundaries. *)
module M = Httpz.Method
module H = Httpz.Header_name
let text_type = Resp.text_type

let[@inline never][@zero_alloc assume] call_run_with_wrappers site (path : string @ local)
    (h : 'e Route.handler @ local) env (req : Req.t @ local)
    (respond : Resp.respond @ local) =
  (Site.run_with_wrappers site) path h env req respond

(* A matcher allocates only for a captured segment, which the handler
   receives as an owned string like the request body. *)
let[@inline never][@zero_alloc assume] match_route (r : 'e Route.t)
    (path : string @ local) =
  exclave_ Route.run r path

let[@zero_alloc] meth_matches meth route_meth =
  Method.equal route_meth meth
  || (Method.equal meth M.Head && Method.equal route_meth M.Get)

(* The method is tested first because [Route.run] decodes and allocates one
   string per captured segment, which a method mismatch would then discard. *)
let[@zero_alloc] rec find_handler routes meth (path : string @ local) = exclave_
  match routes with
  | [] -> Null
  | r :: rest ->
      if meth_matches meth (Route.meth r) then
        match match_route r path with
        | This h -> This h
        | Null -> find_handler rest meth path
      else find_handler rest meth path

let[@zero_alloc] method_not_allowed allow (respond : Resp.respond @ local) =
  (* Through [Resp.v] rather than [Resp.text ~headers], because the optional
     argument would put the block on the heap. *)
  let () =
    Resp.v respond ~status:Httpz.Res.Method_not_allowed
      ~headers:(stack_ [ Headers.h_local H.Allow allow ])
      ~content_type:(This text_type) (Body.String "Method Not Allowed\n")
  in
  ()

(* HEAD is answered from the GET route, so it belongs in Allow whenever GET
   does. Order follows the route list, which is stable across runs. *)
let allow_value allowed =
  let names = List.map Method.to_string allowed in
  let names =
    if
      List.exists (fun m -> Method.equal m M.Get) allowed
      && not (List.exists (fun m -> Method.equal m M.Head) allowed)
    then
      names @ [ "HEAD" ]
    else names
  in
  String.concat ", " names

(* A field value is a global string, so the Allow value of a 405 cannot be
   built in the region. It and the handler over it are the one response-path
   allocation here outside the callback boundaries. *)
let[@cold][@zero_alloc assume] unrouted site (path : string @ local) : 'e Route.handler =
  let rec allowed routes acc =
    match routes with
    | [] -> List.rev acc
    | r :: rest -> (
        match Route.run r path with
        | Null -> allowed rest acc
        | This _ ->
            let rm = Route.meth r in
            if List.exists (Method.equal rm) acc then allowed rest acc
            else allowed rest (rm :: acc))
  in
  match allowed (Site.routes site) [] with
  | [] -> Site.fallback site
  | allowed ->
      let allow = allow_value allowed in
      fun _env (_req : Req.t @ local) (respond : Resp.respond @ local) ->
        method_not_allowed allow respond

(* A repeated Content-Type is refused before routing, but through the site's
   wrapper, so the 400 carries a wrapper's fields as any other response
   does. *)
let duplicate_content_type : 'e Route.handler =
 fun _env (_req : Req.t @ local) (respond : Resp.respond @ local) ->
  Resp.bad_request respond ()

let[@zero_alloc] rec repeated_content_type (headers : Headers.t @ local) seen =
  match headers with
  | [] -> false
  | field :: rest ->
      if Headers.same_name field.Headers.name H.Content_type then
        seen || repeated_content_type rest true
      else repeated_content_type rest seen

let[@inline always][@zero_alloc] run site env (req : Req.t @ local) (respond : Resp.respond @ local) =
  let path = Req.path req in
  let local_ h =
    if repeated_content_type (Req.headers req) false then duplicate_content_type
    else match find_handler (Site.routes site) (Req.meth req) path with
      | This h -> h
      | Null -> unrouted site path
  in
  let () = call_run_with_wrappers site path h env req respond in
  ()
