type t = {
  status : Status.t;
  headers : Headers.t;
  etag : Etag.t option;
  last_modified : float option;
  cache : Cache_control.t option;
  body : Body.t;
}

let html_type = "text/html; charset=utf-8"
let text_type = "text/plain; charset=utf-8"

(* Everything below funnels through [v], so validating there is what stops a
   header value carrying a CR or an LF from splitting the response, and an
   entity-tag carrying a double quote from ending its own quoted string. The
   check raises rather than sanitising: a caller that builds such a field has a
   bug, and the backend's handler guard turns the exception into a 500 reported
   at the point it happened. *)

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

let check_header name value =
  check_name name;
  if has_control value then
    invalid_arg
      (Printf.sprintf
         "Proffer.Resp.v: value %S of header %S contains CR, LF or NUL" value
         name)

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
let check_no_overlap headers name set =
  if set && List.exists (fun (n, _) -> Headers.same n name) headers then
    invalid_arg
      (Printf.sprintf
         "Proffer.Resp.v: header %S is already set by its own argument" name)

let v ?(status = `OK) ?(headers = []) ?etag ?last_modified ?cache ?content_type
    body =
  List.iter (fun (n, value) -> check_header n value) headers;
  Option.iter (check_value "content_type") content_type;
  Option.iter check_etag etag;
  Option.iter check_last_modified last_modified;
  check_no_overlap headers "Content-Type" (content_type <> None);
  check_no_overlap headers "Cache-Control" (cache <> None);
  check_no_overlap headers "ETag" (etag <> None);
  check_no_overlap headers "Last-Modified" (last_modified <> None);
  let extra =
    List.concat
      [
        (match content_type with
        | None -> []
        | Some ct -> [ ("Content-Type", ct) ]);
        (match cache with
        | None -> []
        | Some c -> [ ("Cache-Control", Cache_control.to_string c) ]);
        (match etag with
        | None -> []
        | Some e -> [ ("ETag", Etag.to_string e) ]);
        (match last_modified with
        | None -> []
        | Some t -> [ ("Last-Modified", Date.to_imf t) ]);
      ]
  in
  {
    status;
    headers = Headers.of_list (headers @ extra);
    etag;
    last_modified;
    cache;
    body;
  }

let html ?status ?etag ?cache s =
  v ?status ?etag ?cache ~content_type:html_type (Body.String s)

let text ?status s = v ?status ~content_type:text_type (Body.String s)

let media ?status ?etag ?cache ct s =
  v ?status ?etag ?cache ~content_type:ct (Body.String s)

let see_other location =
  v ~status:`See_other ~headers:[ ("Location", location) ] Body.Empty

let redirect ?(permanent = false) location =
  v
    ~status:(if permanent then `Moved_permanently else `Found)
    ~headers:[ ("Location", location) ]
    Body.Empty

let not_found ?(html = "<!doctype html>\n<title>Not Found</title>\n") () =
  v ~status:`Not_found ~content_type:html_type (Body.String html)

let bad_request ?(html = "<!doctype html>\n<title>Bad Request</title>\n") () =
  v ~status:`Bad_request ~content_type:html_type (Body.String html)

(* The field is rewritten rather than appended to, so repeated calls leave one
   Vary rather than two. The name is checked here because this is the one
   constructor that does not funnel through [v]. *)
let vary name t =
  if String.equal name "" || not (String.for_all is_tchar name) then
    invalid_arg
      (Printf.sprintf "Proffer.Resp.vary: %S is not a header name" name);
  let value =
    match Headers.find t.headers "Vary" with
    | None -> name
    | Some existing -> existing ^ ", " ^ name
  in
  let others =
    List.filter
      (fun (n, _) -> not (Headers.same n "Vary"))
      (Headers.to_list t.headers)
  in
  { t with headers = Headers.of_list (others @ [ ("Vary", value) ]) }

let status t = t.status
let headers t = t.headers
let body t = t.body
let etag t = t.etag
let last_modified t = t.last_modified
let cache t = t.cache
