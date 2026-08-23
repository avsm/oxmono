(* A handler is given a responder rather than returning a value. Nothing on the
   response path then has to outlive the call, so a backend can build the whole
   description in the region it runs the handler in.

   [respond] is always used at [local]. A handler that stashed it would be
   holding a closure over a connection about to be reused, and the mode is what
   stops that. Values flow down into the responder and never back up, which is
   why this is a continuation rather than a returned record: a returned local
   value would need [exclave_] at every frame between the constructor and the
   backend, and every combinator that transforms a response would need it too.

   The typed fields travel beside the block rather than inside it. [Backend]
   renders them into fields when it knows whether it is sending the response at
   all, which is also where a conditional request is answered, so a 304 does
   not pay for a block it discards. *)

(* The description is one record rather than a run of labelled arguments.
   Currying and locality do not mix: a curried function used at [local] groups
   its arrows, so [respond ~status ~headers ...] reads as complete after the
   first argument and the compiler rejects the rest. One argument has no
   arrows to group.

   The record travels at [local], so a backend never pays heap for it. Only
   [headers] is left at the record's own mode, because the block is the part
   worth keeping on the stack. Every other field holds a heap value that has to
   be readable at global to be written to a socket, so each is [global_]. *)
module H = Httpz.Header_name

type description = {
  status : Status.t;
  headers : Headers.t;
  global_ etag : Etag.t option;
  global_ last_modified : float option;
  global_ cache : Cache_control.t option;
  global_ content_type : string or_null;
  global_ body : Body.t;
}

type respond = description @ local -> unit

let html_type = "text/html; charset=utf-8"
let text_type = "text/plain; charset=utf-8"

(* Everything below funnels through [v], so validating there is what stops a
   header value carrying a CR or an LF from splitting the response, and an
   entity-tag carrying a double quote from ending its own quoted string. The
   check raises rather than sanitising: a caller that builds such a field has a
   bug, and the backend's handler guard turns the exception into a 500 reported
   at the point it happened.

   The strings reaching these checks are global even when the block holding
   them is local, because [Headers.field] declares both of its fields
   [global_]. That is what lets the checks stay written against the stdlib. *)

let is_tchar = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_' | '`'
  | '|' | '~' ->
      true
  | _ -> false

let check_name name =
  if String.equal name "" then
    invalid_arg "Proffer.Resp.v: header name is empty"
  else if not (String.for_all is_tchar name) then
    invalid_arg
      (Printf.sprintf "Proffer.Resp.v: header name %S is not a token" name)

let has_control value =
  String.exists (fun c -> c = '\r' || c = '\n' || c = '\000') value

let check_value what value =
  if has_control value then
    invalid_arg
      (Printf.sprintf "Proffer.Resp.v: %s value %S contains CR, LF or NUL" what
         value)

(* A known name is an RFC 9110 token by construction and no caller can spell
   it wrong, so only [Other] is checked. That is most of what the name ADT
   buys: the validation that used to run on every field of every response now
   runs on the few a site invents. *)
let check_header (name : Headers.name @ local) spelling value =
  (match name with
  | H.Other -> check_name spelling
  | _ -> ());
  if has_control value then
    invalid_arg
      (Printf.sprintf
         "Proffer.Resp.v: value %S of header %S contains CR, LF or NUL" value
         spelling)

let check_etag e =
  let opaque = Etag.opaque e in
  if
    String.exists
      (fun c -> c = '"' || c = '\r' || c = '\n' || c = '\000')
      opaque
  then
    invalid_arg
      (Printf.sprintf
         "Proffer.Resp.v: etag value %S contains a double quote, CR, LF or NUL"
         opaque)

let check_last_modified t =
  if not (Date.representable t) then
    invalid_arg
      (Printf.sprintf
         "Proffer.Resp.v: last_modified %g is not a representable HTTP date" t)

(* A typed argument owns its field. Were [headers] allowed to name it too, the
   block would carry the field twice, and the copy a client reads first would
   not be the one [Backend] evaluates a conditional request against. *)
(* A direct scan rather than [Headers.exists], which would take a closure over
   [name] and so a heap block. This runs once per typed argument given, on
   every response, and the closure was most of what naming a content type
   cost. *)
let rec names_field (t : Headers.t @ local) (name : Headers.name @ local) =
  match t with
  | [] -> false
  | { Headers.name = n; _ } :: tl ->
      Headers.same_name n name || names_field tl name

let check_no_overlap (headers : Headers.t @ local) (name : Headers.name) set =
  if set && names_field headers name then
    invalid_arg
      (Printf.sprintf
         "Proffer.Resp.v: header %S is already set by its own argument"
         (Httpz.Header_name.canonical name))

(* [headers] is a required argument rather than an optional one. An optional
   argument is passed as an allocated [Some], which for a local block would be
   a local option, and a local option cannot cross into the optional-argument
   protocol. The friendly constructors below take the optional form and forward
   to the required one. *)
(* [content_type] is required, and [or_null] rather than an option, for the
   same reason [headers] is required: an optional argument's payload arrives
   local, and a local string cannot reach a [global_] field. [or_null] adds no
   box on top, so a caller naming a content type pays nothing for it. *)
let v (respond : respond @ local) ?(status = Httpz.Res.Success)
    ~(headers : Headers.t @ local) ?etag ?last_modified ?cache
    ~(content_type : string or_null) body =
  Headers.iter check_header headers;
  (match content_type with
  | Null -> ()
  | This ct -> check_value "content_type" ct);
  Option.iter check_etag etag;
  Option.iter check_last_modified last_modified;
  check_no_overlap headers Httpz.Header_name.Content_type
    (match content_type with Null -> false | This _ -> true);
  check_no_overlap headers Httpz.Header_name.Cache_control (cache <> None);
  check_no_overlap headers Httpz.Header_name.Etag (etag <> None);
  check_no_overlap headers Httpz.Header_name.Last_modified (last_modified <>
    None);
  let local_ d =
    { status; headers; etag; last_modified; cache; content_type; body }
  in
  (* Not a tail call: [d] lives in this frame, so the call must return here
     for the region to be torn down after the backend has consumed it. *)
  let () = respond d in
  ()

let h = Headers.h
let other = Headers.other
let h_local = Headers.h_local

let html (respond : respond @ local) ?status ?etag ?cache
    ?(headers : Headers.t @ local = Headers.empty) s =
  v respond ?status ?etag ?cache ~headers ~content_type:(This html_type)
    (Body.String s)

let text (respond : respond @ local) ?status
    ?(headers : Headers.t @ local = Headers.empty) s =
  v respond ?status ~headers ~content_type:(This text_type) (Body.String s)

let media (respond : respond @ local) ?status ?etag ?cache
    ?(headers : Headers.t @ local = Headers.empty) ct s =
  v respond ?status ?etag ?cache ~headers ~content_type:(This ct)
    (Body.String s)

(* [write] is taken at the caller's mode rather than [local], since the
   backend runs it after the description has been consumed, which is past the
   point where a local closure would still be alive. *)
let stream (respond : respond @ local) ?status ?cache
    ?(headers : Headers.t @ local = Headers.empty) ?length ct write =
  v respond ?status ?cache ~headers ~content_type:(This ct)
    (Body.Stream { length; write })

let empty (respond : respond @ local) ?(status = Httpz.Res.Success)
    ?(headers : Headers.t @ local = Headers.empty) () =
  v respond ~status ~headers ~content_type:Null Body.Empty

let see_other (respond : respond @ local) location =
  let () =
    v respond ~status:Httpz.Res.See_other ~content_type:Null
      ~headers:(stack_ [ h_local Httpz.Header_name.Location location ])
      Body.Empty
  in
  ()

let redirect (respond : respond @ local) ?(permanent = false) location =
  let () =
    v respond
      ~status:
        (if permanent then Httpz.Res.Moved_permanently else Httpz.Res.Found)
      ~content_type:Null
      ~headers:(stack_ [ h_local Httpz.Header_name.Location location ])
      Body.Empty
  in
  ()

let not_found (respond : respond @ local)
    ?(html = "<!doctype html>\n<title>Not Found</title>\n") () =
  v respond ~status:Httpz.Res.Not_found ~headers:Headers.empty
    ~content_type:(This html_type) (Body.String html)

let bad_request (respond : respond @ local)
    ?(html = "<!doctype html>\n<title>Bad Request</title>\n") () =
  v respond ~status:Httpz.Res.Bad_request ~headers:Headers.empty
    ~content_type:(This html_type) (Body.String html)
