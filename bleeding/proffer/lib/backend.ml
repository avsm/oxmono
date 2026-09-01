(* Shared dispatch, conditional-request, and HEAD processing for backends.

   The response path is checked free of heap allocation. The [call_*]
   functions below are the boundaries the checker does not see through. They
   run the application's callbacks and the route matcher. *)

module M = Httpz.Method
module H = Httpz.Header_name
module I64 = Stdlib_upstream_compatible.Int64_u
module F64 = Stdlib_upstream_compatible.Float_u

type body =
  | Empty
  | String of string @@ global
  | Stream of {
      length : int64 option;
      write : (Body.Sink.t -> unit) @@ global;
      trailers : Headers.t;
    }
  | Handoff of {
      kind : Body.handoff_kind;
      run : (Body.Socket.t -> unit) @@ global;
    }

type outcome = {
  status : Status.t;
  headers : Headers.t;
  last_modified : float option;
  body : body;
  content_length : int64 option;
}

type writer = outcome @ local -> unit

let sink ?emit_sub emit = Body.Sink.v ?emit_sub emit
let socket ~read ~write ~shutdown = Body.Socket.v ~read ~write ~shutdown

let[@zero_alloc] request ~meth ~version ~connection_upgrade
    ~(target : string @ local)
    ~(path : string @ local) ~(query : string @ local)
    (headers : Headers.t @ local) ~(body : string @ local) =
  exclave_
  Req.backend ~meth ~version ~connection_upgrade ~target ~path ~query headers
    ~body
;;

let text_type = "text/plain; charset=utf-8"

(* The writer and the handler run under [run]'s guard, where a raise counts as
   an ordinary path for the checker, so those two are assumed clean on every
   path. *)
let[@inline never][@zero_alloc assume strict] call_writer
    (write : writer @ local) (o : outcome @ local) =
  write o

let[@inline never][@zero_alloc assume] call_error (on_error : exn -> unit) exn
    =
  try on_error exn with _ -> ()

let[@inline never][@zero_alloc assume strict] call_describe
    (describe : (Resp.respond @ local -> unit) @ local)
    (respond : Resp.respond @ local) =
  describe respond

let[@inline never][@zero_alloc assume] call_gen (gen : unit -> string) = gen ()

let[@inline never][@zero_alloc assume] call_decorate site (path : string @ local)
    (h : 'e Route.handler @ local) env (req : Req.t @ local)
    (respond : Resp.respond @ local) =
  (Site.decorate site) path h env req respond

(* A matcher allocates only for a captured segment, which the handler
   receives as an owned string like the request body. *)
let[@inline never][@zero_alloc assume] match_route (r : 'e Route.t)
    (path : string @ local) =
  exclave_ Route.run r path

let[@zero_alloc] meth_matches meth route_meth =
  Method.equal route_meth meth
  || (Method.equal meth M.Head && Method.equal route_meth M.Get)

let[@zero_alloc] rec find_handler routes meth (path : string @ local) = exclave_
  match routes with
  | [] -> Null
  | r :: rest -> (
      match match_route r path with
      | This h when meth_matches meth (Route.meth r) -> This h
      | _ -> find_handler rest meth path)

let[@zero_alloc] is_ows c = Char.equal c ' ' || Char.equal c '\t'

let[@zero_alloc] rec skip_ows (v : string @ local) i j =
  if i < j && is_ows (String.unsafe_get v i) then skip_ows v (i + 1) j else i

let[@zero_alloc] rec trim_ows (v : string @ local) i j =
  if j > i && is_ows (String.unsafe_get v (j - 1)) then trim_ows v i (j - 1)
  else j

let[@zero_alloc] rec same_bytes (a : string @ local) i (b : string @ local) j n =
  n = 0
  || Char.equal (String.unsafe_get a i) (String.unsafe_get b j)
     && same_bytes a (i + 1) b (j + 1) (n - 1)

(* Repeated field lines combine into one comma-joined value (RFC 9110 section
   5.3), which is not a valid date, so a repeated date field is [Null]. *)
let[@zero_alloc] rec only_field (t : Headers.t @ local) name
    (found : string or_null @ local) = exclave_
  match t with
  | [] -> found
  | f :: tl ->
      if Headers.same_name f.Headers.name name then
        match found with
        | This _ -> Null
        | Null -> only_field tl name (This f.Headers.value)
      else only_field tl name found

let[@zero_alloc] imf_date ~has_now (now : float#) (t : Headers.t @ local) name
    : #(bool * float#) =
  match only_field t name Null with
  | Null -> #(false, #0.)
  | This v ->
      let #(valid, parsed) = Date.parse_imf ~has_now now v in
      #(valid, parsed)

(* Strong comparison, as RFC 9110 section 8.8.3.2 defines it for If-Match. A
   weak tag on either side never matches. Weak comparison ignores strength. *)
let[@zero_alloc] item_matches (v : string @ local) i j ~strong
    (etag : Etag.t @ local) =
  let weak_item =
    j - i >= 2
    && Char.equal (String.unsafe_get v i) 'W'
    && Char.equal (String.unsafe_get v (i + 1)) '/'
  in
  let i = if weak_item then i + 2 else i in
  let n = j - i in
  n >= 2
  && Char.equal (String.unsafe_get v i) '"'
  && Char.equal (String.unsafe_get v (j - 1)) '"'
  && ((not strong) || not (weak_item || Etag.is_weak etag))
  &&
  let opaque = Etag.opaque etag in
  String.length opaque = n - 2 && same_bytes opaque 0 v (i + 1) (n - 2)

(* An entity-tag may contain a comma inside its quotes, so items are split on
   commas outside them. *)
let[@zero_alloc] rec any_item (v : string @ local) ~strong (etag : Etag.t @ local)
    ~start ~i ~quoted =
  let n = String.length v in
  if i = n then
    let a = skip_ows v start n in
    item_matches v a (trim_ows v a n) ~strong etag
  else
    let c = String.unsafe_get v i in
    if Char.equal c '"' then
      any_item v ~strong etag ~start ~i:(i + 1) ~quoted:(not quoted)
    else if Char.equal c ',' && not quoted then
      (let a = skip_ows v start i in
       item_matches v a (trim_ows v a i) ~strong etag)
      || any_item v ~strong etag ~start:(i + 1) ~i:(i + 1) ~quoted:false
    else any_item v ~strong etag ~start ~i:(i + 1) ~quoted

(* An entity-tag condition is a list, so repeated fields combine. *)
let[@zero_alloc] rec any_field (t : Headers.t @ local) name ~strong
    (etag : Etag.t @ local) =
  match t with
  | [] -> false
  | f :: tl ->
      Headers.same_name f.Headers.name name
      && any_item f.Headers.value ~strong etag ~start:0 ~i:0 ~quoted:false
      || any_field tl name ~strong etag

let[@zero_alloc] is_star (v : string @ local) =
  let n = String.length v in
  let a = skip_ows v 0 n in
  let b = trim_ows v a n in
  b - a = 1 && Char.equal (String.unsafe_get v a) '*'

let[@zero_alloc] is_star_range (v : string @ local) a b =
  let a = skip_ows v a b in
  let b = trim_ows v a b in
  b - a = 1 && Char.equal (String.unsafe_get v a) '*'

let[@zero_alloc] rec any_star_item (v : string @ local) ~start ~i ~quoted =
  let n = String.length v in
  if i = n then
    is_star_range v start n
  else
    let c = String.unsafe_get v i in
    if Char.equal c '"' then
      any_star_item v ~start ~i:(i + 1) ~quoted:(not quoted)
    else if Char.equal c ',' && not quoted then
      is_star_range v start i
      || any_star_item v ~start:(i + 1) ~i:(i + 1) ~quoted:false
    else any_star_item v ~start ~i:(i + 1) ~quoted

let[@zero_alloc] rec any_star_field (t : Headers.t @ local) name =
  match t with
  | [] -> false
  | f :: rest ->
      Headers.same_name f.Headers.name name
      && any_star_item f.Headers.value ~start:0 ~i:0 ~quoted:false
      || any_star_field rest name

(* A 2xx response describes a current representation, so [*] matches whatever
   the handler produced whether or not it carried an entity-tag. A tag list
   against a response with no entity-tag matches nothing. *)
let[@zero_alloc] condition_matches (t : Headers.t @ local) name ~strong
    (etag : Etag.t option @ local) =
  any_star_field t name
  || match etag with Some e -> any_field t name ~strong e | None -> false

(* IMF-fixdate has whole-second resolution. *)
let[@zero_alloc] not_after (a : float @ local) (b : float#) =
  F64.compare (F64.floor (F64.of_float a)) (F64.floor b) <= 0

type precondition = Proceed | Revalidated | Failed

let[@zero_alloc] is_conditional_read (req : Req.t @ local) =
  let meth = Req.meth req in
  Method.equal meth M.Get || Method.equal meth M.Head

(* A generic handler exposes validators only after it has run, which is too
   late to protect a mutation. Refuse conditional writes before dispatch rather
   than claim a post-state comparison prevented an effect. Invalid or repeated
   date fields are ignored, as RFC 9110 requires. *)
let[@zero_alloc] reject_conditional_write ~has_now (now : float#)
    (req : Req.t @ local) =
  (not (is_conditional_read req))
  &&
  let headers = Req.headers req in
  Headers.mem headers H.If_match
  || Headers.mem headers H.If_none_match
  ||
  let #(valid, _) = imf_date ~has_now now headers H.If_unmodified_since in
  valid

(* RFC 9110 section 13.2.2 fixes this order. Unsafe conditional requests have
   already been refused above, before dispatch; this evaluates GET and HEAD.
   If-Range is not evaluated because this library does not serve ranges. *)
let[@zero_alloc] precondition ~has_now (now : float#) (req : Req.t @ local)
    (d : Resp.description @ local) =
  let code = Status.code d.Resp.status in
  if code < 200 || code >= 300 then Proceed
  else
    let headers = Req.headers req in
    let safe = is_conditional_read req in
    let has_if_match = Headers.mem headers H.If_match in
    if
      has_if_match
      && not (condition_matches headers H.If_match ~strong:true d.Resp.etag)
    then Failed
    else
      let unmodified_since_failed =
        (not has_if_match)
        &&
        match d.Resp.last_modified with
        | None -> false
        | Some lm ->
            let #(valid, until) =
              imf_date ~has_now now headers H.If_unmodified_since
            in
            valid && not (not_after lm until)
      in
      if unmodified_since_failed then Failed
      else if Headers.mem headers H.If_none_match then
        if condition_matches headers H.If_none_match ~strong:false d.Resp.etag
        then if safe then Revalidated else Failed
        else Proceed
      else
        match d.Resp.last_modified with
        | Some lm when safe ->
            let #(valid, since) =
              imf_date ~has_now now headers H.If_modified_since
            in
            if not valid then Proceed
            (* A date the server has not reached yet is meaningless and would
               let a client pin 304s, so it is ignored (section 13.1.3). *)
            else if has_now && F64.compare (F64.floor since) (F64.floor now) > 0
            then Proceed
            else if not_after lm since then Revalidated
            else Proceed
        | _ -> Proceed

(* [exclave_], so the option and the [Int64] box it holds are built in the
   caller's region rather than on the heap. Without it a top-level function
   returns a global value and the whole point of the local outcome is lost. *)
let[@zero_alloc] len s = exclave_ Some (Int64.of_int (String.length s))

let[@zero_alloc] upgrade_offered (req : Req.t @ local)
    (selected : string @ local) =
  match Headers.combined (Req.headers req) H.Upgrade with
  | None -> false
  | Some offer ->
      let matched = Httpz.Upgrade.matches_offer ~offer ~selected in
      matched
;;

let[@zero_alloc] valid_upgrade_request (req : Req.t @ local)
    (protocol : string @ local) =
  Req.version req = Httpz.Version.Http_1_1
  && Req.connection_upgrade req
  && upgrade_offered req protocol
;;

let trailer_name = "Trailer"

let[@zero_alloc] rec trailer_declarations (trailers : Headers.t @ local) =
  exclave_
  match trailers with
  | [] -> []
  | { Headers.spelling; _ } :: rest ->
      Headers.other_local trailer_name spelling :: trailer_declarations rest

(* Every field is built with [h_local], so the block is entirely in the
   region. Last-Modified is not a field here. It travels in the outcome as a
   time the backend writes itself. *)
let[@zero_alloc] block (d : Resp.description @ local) = exclave_
  let local_ extra = [] in
  let local_ extra =
    match d.Resp.body with
    | Body.Handoff { kind = Body.Upgrade protocol; _ } ->
        Headers.h_local H.Upgrade protocol :: extra
    | _ -> extra
  in
  let local_ extra =
    match d.Resp.etag with
    | None -> extra
    | Some e -> Headers.h_local H.Etag (Etag.to_string e) :: extra
  in
  let local_ extra =
    match d.Resp.cache with
    | None -> extra
    | Some c -> Headers.h_local H.Cache_control c :: extra
  in
  let local_ extra =
    match d.Resp.content_type with
    | Null -> extra
    | This ct -> Headers.h_local H.Content_type ct :: extra
  in
  let local_ headers = Headers.cat d.Resp.headers extra in
  match d.Resp.body with
  | Body.Stream { trailers = _ :: _ as trailers; _ } ->
      let local_ declarations = trailer_declarations trailers in
      Headers.cat headers declarations
  | _ -> headers

(* A 304 carries only the metadata needed to update a stored response. The
   name is httpz's constructor, so this is a comparison of an immediate
   rather than a case-folding walk over a string. *)
let[@zero_alloc] is_revalidation_name (name : Headers.name @ local) =
  match name with
  | H.Etag | H.Last_modified | H.Cache_control | H.Content_location | H.Expires
  | H.Vary ->
      true
  | _ -> false

let[@zero_alloc] rec revalidation (b : Headers.t @ local) = exclave_
  match b with
  | [] -> []
  | { Headers.name; spelling; value } :: tl ->
      if is_revalidation_name name then
        { Headers.name; spelling; value } :: revalidation tl
      else revalidation tl

let[@zero_alloc] without_trailer (b : Headers.t @ local) = exclave_
  Headers.without b H.Trailer
;;

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

let[@zero_alloc] send (write : writer @ local) status
    (headers : Headers.t @ local) ~(last_modified : float option @ local)
    (body : body @ local)
    (content_length : int64 option @ local) =
  let local_ o = { status; headers; last_modified; body; content_length } in
  (* Not a tail call, since [o] lives in this frame. *)
  let () = call_writer write o in
  ()

(* Built once. [h_local] can raise, and a raise under [run]'s guard counts as
   an ordinary path for the checker. *)
let precondition_failed_headers = [ Headers.h H.Content_type text_type ]

let[@zero_alloc] write_precondition_failed (req : Req.t @ local)
    (write : writer @ local) =
  let message = "Precondition Failed\n" in
  let headers = precondition_failed_headers in
  let local_ body =
    if Method.equal (Req.meth req) M.Head then Empty else String message
  in
  let local_ content_length = len message in
  let () =
    send write Httpz.Res.Precondition_failed headers ~last_modified:None body
      content_length
  in
  ()

let[@zero_alloc] decide ~has_now (now : float#) (req : Req.t @ local)
    (d : Resp.description @ local) (write : writer @ local) =
  let local_ b = block d in
  let last_modified = d.Resp.last_modified in
  match precondition ~has_now now req d with
  | Failed -> write_precondition_failed req write
  | Revalidated ->
      let local_ headers = revalidation b in
      let () =
        send write Httpz.Res.Not_modified headers ~last_modified Empty None
      in
      ()
  | Proceed -> (
      let status = d.Resp.status in
      let code = Status.code status in
      match d.Resp.body with
      | Body.Handoff { kind = Body.Tunnel; run }
        when Method.equal (Req.meth req) M.Connect && code >= 200 && code < 300
        ->
          let local_ body = Handoff { kind = Body.Tunnel; run } in
          let () = send write status b ~last_modified body None in
          ()
      | Body.Handoff { kind = Body.Upgrade protocol; run }
        when code = 101 && valid_upgrade_request req protocol ->
          let local_ body = Handoff { kind = Body.Upgrade protocol; run } in
          let () = send write status b ~last_modified body None in
          ()
      | Body.Handoff _ ->
          invalid_arg
            "Proffer.Resp: a tunnel needs a successful CONNECT response and an \
             upgrade needs status 101 and a matching HTTP/1.1 Upgrade offer"
      | _ when (code >= 100 && code < 200) || code = 204 ->
        let local_ headers = without_trailer b in
        let () = send write status headers ~last_modified Empty None in
        ()
      | _ when code = 205 ->
        let local_ headers = without_trailer b in
        let () = send write status headers ~last_modified Empty (Some 0L) in
        ()
      | _ when code = 304 ->
        let local_ headers = revalidation b in
        let local_ content_length = Body.declared_length d.Resp.body in
        let () = send write status headers ~last_modified Empty content_length in
        ()
      | _ when Method.equal (Req.meth req) M.Head ->
        let local_ headers = without_trailer b in
        let local_ content_length = Body.declared_length d.Resp.body in
        let () = send write status headers ~last_modified Empty content_length in
        ()
      | _ ->
        match d.Resp.body with
        | Body.Empty ->
            let () = send write status b ~last_modified Empty (Some 0L) in
            ()
        | Body.String s ->
            let local_ content_length = len s in
            let () =
              send write status b ~last_modified (String s) content_length
            in
            ()
        | Body.Stream { length; write = w; trailers } ->
            let local_ body = Stream { length; write = w; trailers } in
            let local_ content_length =
              match trailers with [] -> length | _ :: _ -> None
            in
            let () =
              send write status b ~last_modified body content_length
            in
            ()
        | Body.Delayed { length; gen } ->
            (* Run here, so HEAD and contentless statuses never pay for a body
               they drop. The generator runs under [handle]'s guard, which is
               why it is handed back rather than run in the handler. *)
            let s = call_gen gen in
            (* [Int64.equal] takes its arguments at global, and the declared
               length is read out of a local description. *)
            (match length with
            | Some expected
              when not
                     (I64.equal (I64.of_int64 expected)
                        (I64.of_int (String.length s))) ->
                invalid_arg
                  "Proffer.Resp: a delayed body did not produce its declared \
                   length"
            | _ -> ());
            let local_ content_length = len s in
            let () =
              send write status b ~last_modified (String s) content_length
            in
            ()
        | Body.Handoff _ -> assert false)

(* Built once, since they are reported rather than raised. *)
let responded_twice =
  Invalid_argument "Proffer.Backend: the handler responded more than once"

let never_responded =
  Invalid_argument "Proffer.Backend: the handler returned without responding"

(* [run ?on_error req describe write] gives [describe] a responder and writes
   what it responds with. [handle] is this plus dispatch, and a test reaches it
   through [proffer.mock] to exercise one response without a site. *)
let[@zero_alloc] run_core ~on_error ~has_now (now : float#)
    (req : Req.t @ local)
    (describe : (Resp.respond @ local -> unit) @ local)
    (write : writer @ local) =
  (* [responded] detects missing or duplicate responder calls. [sent] is
     separate because a delayed body can fail after the response is described
     but before anything reaches the writer. *)
  let local_ responded = ref false in
  let local_ sent = ref false in
  let local_ w : writer =
   fun o ->
    sent := true;
    call_writer write o
  in
  let local_ respond : Resp.respond =
   fun d ->
    if !responded then call_error on_error responded_twice
    else begin
      responded := true;
      let () = decide ~has_now now req d w in
      ()
    end
  in
  (match
     if reject_conditional_write ~has_now now req then begin
       responded := true;
       write_precondition_failed req w
     end
     else call_describe describe respond
   with
  | () -> if not !responded then call_error on_error never_responded
  | exception exn -> call_error on_error exn);
  if not !sent then begin
    let message = "Internal Server Error\n" in
    let local_ body =
      if Method.equal (Req.meth req) M.Head then Empty else String message
    in
    let local_ content_length = len message in
    let local_ headers =
      stack_ [ Headers.h_local H.Content_type text_type ]
    in
    (match
       send w Httpz.Res.Internal_server_error headers ~last_modified:None body
         content_length
     with
    | () -> ()
    | exception exn -> call_error on_error exn)
  end

let ignore_error _ = ()

let run ?on_error ?now (req : Req.t @ local)
    (describe : (Resp.respond @ local -> unit) @ local)
    (write : writer @ local) =
  let on_error = Option.value on_error ~default:ignore_error in
  let #(has_now, now) =
    match now with
    | None -> #(false, #0.)
    | Some now -> #(true, F64.of_float now)
  in
  let () = run_core ~on_error ~has_now now req describe write in
  ()

let[@zero_alloc] handle_core ~on_error ~has_now (now : float#) site env
    (req : Req.t @ local) (write : writer @ local) =
  let path = Req.path req in
  (* Decorating generated 404 and 405 responses prevents protected routes from
     being inferred by comparing their unauthenticated responses. *)
  let local_ describe (r : Resp.respond @ local) =
    let rec content_types (headers : Headers.t @ local) count =
      match headers with
      | [] -> count
      | field :: rest ->
          content_types rest
            (if Headers.same_name field.Headers.name H.Content_type
             then count + 1
             else count)
    in
    if content_types (Req.headers req) 0 > 1
    then Resp.bad_request r ()
    else
      let local_ h =
        match find_handler (Site.routes site) (Req.meth req) path with
        | This h -> h
        | Null -> unrouted site path
      in
      let () = call_decorate site path h env req r in
      ()
  in
  let () = run_core ~on_error ~has_now now req describe write in
  ()

let[@zero_alloc] handle_unboxed ~on_error ~(now : float#) site env
    (req : Req.t @ local) (write : writer @ local) =
  handle_core ~on_error ~has_now:true now site env req write

let handle ?on_error ?now site env (req : Req.t @ local)
    (write : writer @ local) =
  let on_error = Option.value on_error ~default:ignore_error in
  let #(has_now, now) =
    match now with
    | None -> #(false, #0.)
    | Some now -> #(true, F64.of_float now)
  in
  let () = handle_core ~on_error ~has_now now site env req write in
  ()
