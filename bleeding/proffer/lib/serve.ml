(* The part of a backend that is not wire code: dispatch, conditional GET and
   HEAD. Writing it once here is what lets the mock tests exercise the same
   code a socket backend runs. *)

type outcome = {
  status : Status.t;
  headers : (string * string) list;
  body :
    [ `Empty
    | `String of string
    | `Stream of int64 option * (Body.Sink.t -> unit) ];
  content_length : int64 option;
}

let sink emit = Body.Sink.v emit
let text_type = "text/plain; charset=utf-8"

let internal_error () =
  Resp.v ~status:`Internal_server_error ~content_type:text_type
    (Body.String "Internal Server Error\n")

(* HEAD is answered from the GET route, so it belongs in Allow whenever GET
   does. Order follows the route list, which is stable across runs. *)
let allow_value allowed =
  let names = List.map Method.to_string allowed in
  let names =
    if List.exists (fun m -> Method.equal m `GET) allowed then
      names @ [ "HEAD" ]
    else names
  in
  String.concat ", " names

let method_not_allowed allowed =
  Resp.v ~status:`Method_not_allowed
    ~headers:[ ("Allow", allow_value allowed) ]
    ~content_type:text_type
    (Body.String "Method Not Allowed\n")

(* [dispatch compiled meth segs] is the handler of the first route matching
   both the path and the method, or else the methods that matched the path
   alone, which is what a 405 must report. *)
let dispatch compiled meth segs =
  let matches route_meth =
    Method.equal route_meth meth
    || (Method.equal meth `HEAD && Method.equal route_meth `GET)
  in
  let rec go routes allowed =
    match routes with
    | [] -> (None, List.rev allowed)
    | r :: rest -> (
        match Route.run r segs with
        | None -> go rest allowed
        | Some h ->
            let rm = Route.meth r in
            if matches rm then (Some h, [])
            else if List.exists (fun m -> Method.equal m rm) allowed then
              go rest allowed
            else go rest (rm :: allowed))
  in
  go (Compiled.routes compiled) []

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
let not_modified req resp =
  let meth = Req.meth req in
  if not (Method.equal meth `GET || Method.equal meth `HEAD) then false
  else if Status.code (Resp.status resp) <> 200 then false
  else
    match Req.header req "if-none-match" with
    | Some v -> (
        match Resp.etag resp with Some e -> if_none_match v e | None -> false)
    | None -> (
        match (Resp.last_modified resp, Req.header req "if-modified-since") with
        | Some lm, Some v -> (
            match Date.of_imf v with
            | None -> false
            | Some since ->
                (* Compared at whole-second resolution. An IMF-fixdate cannot
                   express anything finer, so a sub-second difference is not a
                   modification the client can observe. *)
                Float.floor lm <= Float.floor since)
        | _ -> false)

let revalidation_headers resp =
  List.filter
    (fun (n, _) ->
      match String.lowercase_ascii n with
      | "etag" | "last-modified" | "cache-control" | "vary" -> true
      | _ -> false)
    (Resp.headers resp)

let len s = Some (Int64.of_int (String.length s))

let of_resp req resp =
  let status = Resp.status resp and headers = Resp.headers resp in
  if not_modified req resp then
    {
      status = `Not_modified;
      headers = revalidation_headers resp;
      body = `Empty;
      content_length = None;
    }
  else if Method.equal (Req.meth req) `HEAD then
    {
      status;
      headers;
      body = `Empty;
      content_length = Body.declared_length (Resp.body resp);
    }
  else
    match Resp.body resp with
    | Body.Empty -> { status; headers; body = `Empty; content_length = Some 0L }
    | Body.String s ->
        { status; headers; body = `String s; content_length = len s }
    | Body.Delayed { gen; _ } ->
        let s = gen () in
        { status; headers; body = `String s; content_length = len s }
    | Body.Stream { length; write } ->
        {
          status;
          headers;
          body = `Stream (length, write);
          content_length = length;
        }

let handle ?on_error compiled env req =
  let report exn = match on_error with None -> () | Some f -> f exn in
  let run h =
    match h env req with
    | resp -> resp
    | exception exn ->
        report exn;
        internal_error ()
  in
  match dispatch compiled (Req.meth req) (Req.segments req) with
  | Some h, _ -> of_resp req (run h)
  | None, [] -> of_resp req (run (Compiled.fallback compiled))
  | None, allowed -> of_resp req (method_not_allowed allowed)
