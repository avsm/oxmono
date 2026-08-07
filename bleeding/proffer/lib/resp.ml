type t = {
  status : Status.t;
  headers : (string * string) list;
  etag : Etag.t option;
  last_modified : float option;
  cache : Cache_control.t option;
  body : Body.t;
}

let html_type = "text/html; charset=utf-8"
let text_type = "text/plain; charset=utf-8"

let v ?(status = `OK) ?(headers = []) ?etag ?last_modified ?cache ?content_type
    body =
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
  { status; headers = headers @ extra; etag; last_modified; cache; body }

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

let status t = t.status
let headers t = t.headers
let body t = t.body
let etag t = t.etag
let last_modified t = t.last_modified
let cache t = t.cache
