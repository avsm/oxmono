(* The part of a backend that is not wire code: dispatch, conditional GET and
   HEAD. Writing it once here is what lets the mock tests exercise the same
   code a socket backend runs.

   A handler is given a responder and [handle] is given a writer, so a response
   travels down through both and never back up. Everything that describes it
   lives in the region [handle] runs the handler in: the description record,
   the header block and the outcome are all local, and only the bodies and the
   strings inside the block are heap values that were already there. *)

module M = Httpz.Method

(* A declared variant with [global_] payloads rather than a polymorphic one
   in a [global_] field. The distinction is worth 8 words on every response:
   the socket needs the string and the writer at global, but it does not need
   the block holding them, and [global_] on the field forced the whole thing
   to the heap. The payloads carry the modality instead, so the block is built
   in the region and what comes out of it is still global. *)
type body =
  | Empty
  | String of string @@ global
  | Stream of {
      length : int64 option;
      write : (Body.Sink.t -> unit) @@ global;
    }

type outcome = {
  status : Status.t;
  headers : Headers.t;
  body : body;
  content_length : int64 option;
}

type writer = outcome @ local -> unit

let sink ?emit_sub emit = Body.Sink.v ?emit_sub emit
let text_type = "text/plain; charset=utf-8"
(* HEAD is answered from the GET route, so it belongs in Allow whenever GET
   does. Order follows the route list, which is stable across runs. *)
let allow_value allowed =
  let names = List.map Method.to_string allowed in
  let names =
    if List.exists (fun m -> Method.equal m M.Get) allowed then
      names @ [ "HEAD" ]
    else names
  in
  String.concat ", " names

(* [dispatch compiled meth path] is the handler of the first route matching
   both the path and the method, or else the methods that matched the path
   alone, which is what a 405 must report.

   Every function here takes what it needs as an argument and the scan returns
   into the caller's region. Written the obvious way, with [matches] and [go]
   closing over [meth] and [path] and returning a heap tuple, this cost 13
   words on every request before a route was even looked at, which was the
   largest single allocation left on the serving path. A closure that captures
   is a heap block; a parameter is not. *)
let meth_matches meth route_meth =
  Method.equal route_meth meth
  || (Method.equal meth M.Head && Method.equal route_meth M.Get)

let rec mem_meth m allowed =
  match allowed with
  | [] -> false
  | x :: tl -> Method.equal x m || mem_meth m tl

(* The result is passed to a continuation rather than returned. A returned
   [(handler option * Method.t list)] cannot be local, because the handler
   inside it is a heap closure and a local option would make the compiler
   treat it as one; and it cannot be global without allocating the option and
   the tuple on every request. Handing it on costs neither. *)
let rec scan routes meth path allowed
    ~(found : ('e Route.handler -> 'e Route.handler) @ local)
    ~(none : (Method.t list -> 'e Route.handler) @ local) =
  match routes with
  | [] -> none (List.rev allowed)
  | r :: rest -> (
      match Route.run r path with
      | Null -> scan rest meth path allowed ~found ~none
      | This h ->
          let rm = Route.meth r in
          if meth_matches meth rm then found h
          else if mem_meth rm allowed then scan rest meth path allowed ~found
              ~none
          else scan rest meth path (rm :: allowed) ~found ~none)

let dispatch compiled meth path
    ~(found : ('e Route.handler -> 'e Route.handler) @ local)
    ~(none : (Method.t list -> 'e Route.handler) @ local) =
  scan (Compiled.routes compiled) meth path [] ~found ~none

(* An entity-tag may contain a comma inside its quotes, so the If-None-Match
   list is split on commas outside them. *)
let split_field_value v =
  let out = ref [] in
  let buf = Buffer.create 32 in
  let quoted = ref false in
  String.iter
    (fun c ->
      if c = '"' then (
        quoted := not !quoted;
        Buffer.add_char buf c)
      else if c = ',' && not !quoted then (
        out := Buffer.contents buf :: !out;
        Buffer.clear buf)
      else Buffer.add_char buf c)
    v;
  List.rev (Buffer.contents buf :: !out)

let if_none_match v etag =
  if String.equal (String.trim v) "*" then true
  else
    List.exists
      (fun item ->
        match Etag.of_field_value item with
        | Some t -> Etag.weak_equal t etag
        | None -> false)
      (split_field_value v)

(* [not_modified req resp] is whether the request's preconditions let the
   backend answer 304 instead of [resp].

   Only a successful GET or HEAD is revalidated. RFC 9110 answers a failed
   precondition on any other method with 412, which v1 does not implement, so
   those requests simply get the full response. If-Modified-Since is consulted
   only when If-None-Match is absent, as RFC 9110 section 13.1.3 requires. *)
let not_modified (req : Req.t @ local) (d : Resp.description @ local) =
  let meth = Req.meth req in
  if not (Method.equal meth Httpz.Method.Get || Method.equal meth
    Httpz.Method.Head) then false
  else if Status.code d.Resp.status <> 200 then false
  else
    match Req.header req Httpz.Header_name.If_none_match with
    | Some v -> (
        match d.Resp.etag with Some e -> if_none_match v e | None -> false)
    | None -> (
        match (d.Resp.last_modified, Req.header req
          Httpz.Header_name.If_modified_since)
          with
        | Some lm, Some v -> (
            match Date.of_imf v with
            | None -> false
            | Some since ->
                (* Compared at whole-second resolution. An IMF-fixdate cannot
                   express anything finer, so a sub-second difference is not a
                   modification the client can observe. *)
                Float.floor lm <= Float.floor since)
        | _ -> false)

(* [exclave_], so the option and the [Int64] box it holds are built in the
   caller's region rather than on the heap. Without it a top-level function
   returns a global value and the whole point of the local outcome is lost. *)
let len s = exclave_ Some (Int64.of_int (String.length s))

(* The fields a typed argument owns are rendered here rather than in
   [Resp.v], because this is where it is known whether the response is being
   sent at all. A 304 keeps only the revalidation fields, and does not pay for
   a block it would discard. The order is the one [Resp.v] used to build:
   the caller's block, then Content-Type, Cache-Control, ETag, Last-Modified. *)
(* Every field is built with [h_local], so the block a backend writes is
   entirely in the region. [h] would put each record on the heap, which on
   this path is every response. *)
let block (d : Resp.description @ local) = exclave_
  let local_ extra = [] in
  let local_ extra =
    match d.Resp.last_modified with
    | None -> extra
    | Some t -> Headers.h_local Httpz.Header_name.Last_modified (Date.to_imf t)
        :: extra
  in
  let local_ extra =
    match d.Resp.etag with
    | None -> extra
    | Some e ->
        Headers.h_local Httpz.Header_name.Etag (Etag.to_string e) :: extra
  in
  let local_ extra =
    match d.Resp.cache with
    | None -> extra
    | Some c -> Headers.h_local Httpz.Header_name.Cache_control c :: extra
  in
  let local_ extra =
    match d.Resp.content_type with
    | None -> extra
    | Some ct -> Headers.h_local Httpz.Header_name.Content_type ct :: extra
  in
  Headers.cat d.Resp.headers extra

(* A 304 carries only what a client revalidates against. *)
let revalidation (b : Headers.t @ local) = exclave_
  let rec go (b : Headers.t @ local) = exclave_
    match b with
    | [] -> []
    | { Headers.name; spelling; value } :: tl ->
        (* A 304 carries only what a client revalidates against. The name is
           httpz's constructor, so this is four comparisons of an immediate
           rather than four case-folding walks over a string. *)
        let keep =
          match name with
          | Httpz.Header_name.Etag | Httpz.Header_name.Last_modified
          | Httpz.Header_name.Cache_control | Httpz.Header_name.Vary ->
              true
          | _ -> false
        in
        if keep then { Headers.name; spelling; value } :: go tl else go tl
  in
  go b

let method_not_allowed allowed (respond : Resp.respond @ local) =
  (* Through [Resp.v] rather than [Resp.text ~headers], because the optional
     argument would put the block on the heap. *)
  let () =
    Resp.v respond ~status:Httpz.Res.Method_not_allowed
      ~headers:
        (stack_
           [ Headers.h_local Httpz.Header_name.Allow (allow_value allowed) ])
      ~content_type:text_type (Body.String "Method Not Allowed\n")
  in
  ()

(* [decide req d write] turns one description into the outcome a backend
   writes. It is where the protocol mechanics that need no socket happen. *)
let decide (req : Req.t @ local) (d : Resp.description @ local)
    (write : writer @ local) =
  let local_ b = block d in
  if not_modified req d then
    let local_ o =
      { status = Httpz.Res.Not_modified; headers = revalidation b; body = Empty;
        content_length = None }
    in
    let () = write o in
    ()
  else if Method.equal (Req.meth req) Httpz.Method.Head then
    let local_ o =
      { status = d.Resp.status; headers = b; body = Empty;
        content_length = Body.declared_length d.Resp.body }
    in
    let () = write o in
    ()
  else
    match d.Resp.body with
    | Body.Empty ->
        let local_ o =
          { status = d.Resp.status; headers = b; body = Empty;
            content_length = Some 0L }
        in
        let () = write o in
        ()
    | Body.String s ->
        let local_ o =
          { status = d.Resp.status; headers = b; body = String s;
            content_length = len s }
        in
        let () = write o in
        ()
    | Body.Stream { length; write = w } ->
        let local_ o =
          { status = d.Resp.status; headers = b;
            body = Stream { length; write = w }; content_length = length }
        in
        let () = write o in
        ()
    | Body.Delayed { gen; _ } ->
        (* Run here, so a HEAD and a 304 never pay for a body they drop. The
           generator runs under [handle]'s guard, which is why it is handed
           back rather than run in the handler. *)
        let s = gen () in
        let local_ o =
          { status = d.Resp.status; headers = b; body = String s;
            content_length = len s }
        in
        let () = write o in
        ()

(* [run ?on_error req describe write] gives [describe] a responder and writes
   what it responds with. [handle] is this plus dispatch, and a test reaches it
   through [proffer.mock] to exercise one response without a site. *)
let run ?on_error (req : Req.t @ local)
    (describe : (Resp.respond @ local -> unit) @ local)
    (write : writer @ local) =
  let report exn = match on_error with None -> () | Some f -> f exn in
  (* Two heap words on the path, and they decide four things a site depends
     on. [responded] records that the handler called its responder, so a
     handler that returns without responding is a bug that gets a 500 and a
     second call is dropped rather than writing twice. [sent] records that
     bytes actually reached the writer, which is not the same thing: a
     [Body.Delayed] generator runs inside [decide], so a generator that raises
     has responded but not sent, and must still get a 500. A handler that
     raises after sending gets nothing further, because the bytes have gone. *)
  let responded = ref false in
  let sent = ref false in
  let local_ w : writer =
   fun o ->
    sent := true;
    write o
  in
  let local_ respond : Resp.respond =
   fun d ->
    if !responded then
      report
        (Invalid_argument
           "Proffer.Backend: the handler responded more than once")
    else begin
      responded := true;
      let () = decide req d w in
      ()
    end
  in
  (match describe respond with
  | () ->
      if not !responded then
        report
          (Invalid_argument
             "Proffer.Backend: the handler returned without responding")
  | exception exn -> report exn);
  if not !sent then begin
    let local_ d =
      { Resp.status = Httpz.Res.Internal_server_error; headers = Headers.empty;
        etag = None; last_modified = None; cache = None;
        content_type = Some text_type;
        body = Body.String "Internal Server Error\n" }
    in
    let () = decide req d w in
    ()
  end

let handle ?on_error compiled env (req : Req.t @ local)
    (write : writer @ local) =
  (* Every answer to a request goes through the site's decoration, including
     the 404 and the 405 the library writes itself. A 405 that escaped the
     decoration would tell an unauthenticated caller that a route exists under
     a gated path, which a 404 outside the route table would not, and that
     difference enumerates the protected route table. *)
  let decorate = Compiled.decorate compiled in
  let path = Req.path req in
  let h =
    dispatch compiled (Req.meth req) path
      ~found:(fun h -> decorate path h)
      ~none:(fun allowed ->
        match allowed with
        | [] -> decorate path (Compiled.fallback compiled)
        | allowed ->
            decorate path (fun _env _req (r : Resp.respond @ local) ->
                method_not_allowed allowed r))
  in
  let local_ describe (r : Resp.respond @ local) = h env req r in
  let () = run ?on_error req describe write in
  ()
