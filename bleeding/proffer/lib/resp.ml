module H = Httpz.Header_name
module I64 = Stdlib_upstream_compatible.Int64_u
module F64 = Stdlib_upstream_compatible.Float_u
module Bytes = Bytesrw.Bytes

type description = {
  status : Status.t;
  headers : Headers.t;
  etag : Etag.t option;
  last_modified : float option;
  cache : Cache_control.t option;
  content_type : string or_null;
  body : Body.t;
}

type respond = description @ local -> unit

let html_type = "text/html; charset=utf-8"
let text_type = "text/plain; charset=utf-8"

(* Central validation prevents response splitting and malformed entity-tags.
   Invalid application output is rejected and becomes a reported 500. *)

let check_name (name : string @ local) =
  if String.equal name "" then
    invalid_arg "Proffer.Resp.v: header name is empty"
  else if not (Httpz.Header.Syntax.is_token name) then
    invalid_arg
      (Printf.sprintf "Proffer.Resp.v: header name %S is not a token"
         (Pct.copy_all name))

let check_upgrade_protocol (protocol : string @ local) =
  if not (Httpz.Upgrade.valid_protocol protocol) then
    invalid_arg
      (Printf.sprintf
         "Proffer.Resp.v: upgrade protocol %S is not a protocol token"
         (Pct.copy_all protocol))

let check_value (what : string @ local) (value : string @ local) =
  if not (Httpz.Header.Syntax.is_field_value value) then
    invalid_arg
      (Printf.sprintf
         "Proffer.Resp.v: %s value %S contains a forbidden control byte"
         (Pct.copy_all what) (Pct.copy_all value))

(* Known names are valid tokens by construction. *)
let check_header (name : Headers.name @ local) (spelling : string @ local)
    (value : string @ local) =
  (match name with
  | (H.Content_length | H.Transfer_encoding | H.Connection | H.Trailer) as name
    ->
      invalid_arg
        (Printf.sprintf "Proffer.Resp.v: %s is set by the response backend"
           (H.canonical name))
  | H.Other -> check_name spelling
  | _ -> ());
  check_value spelling value

let[@zero_alloc] rec all_etagc s i =
  i = String.length s
  ||
  let n = Char.code (String.unsafe_get s i) in
  (n = 0x21 || (n >= 0x23 && n <= 0x7e) || n >= 0x80) && all_etagc s (i + 1)

let check_etag (e : Etag.t @ local) =
  let opaque = Etag.opaque e in
  if not (all_etagc opaque 0) then
    invalid_arg
      (Printf.sprintf
         "Proffer.Resp.v: etag value %S contains a byte forbidden by RFC 9110"
         opaque)

let check_last_modified (t : float @ local) =
  if not (Date.representable t) then
    invalid_arg
      (Printf.sprintf
         "Proffer.Resp.v: last_modified %g is not a representable HTTP date"
         (F64.to_float (F64.of_float t)))

(* The length is read out of a body the caller holds at [local], and
   [Int64.compare] takes its arguments at global. Unboxing the comparison
   through [Int64_u] avoids copying the box to the heap to test it. *)
let check_body_length (body : Body.t @ local) =
  let local_ length =
    match body with
    | Body.Delayed { length; _ } | Body.Stream { length; _ } -> length
    | Body.Empty | Body.String _ | Body.Handoff _ -> None
  in
  match length with
  | None -> ()
  | Some n ->
      let n = I64.of_int64 n in
      if I64.compare n (I64.of_int 0) < 0 then
        invalid_arg "Proffer.Resp.v: body length is negative"
      else if I64.compare n (I64.of_int max_int) > 0 then
        invalid_arg "Proffer.Resp.v: body length does not fit in an OCaml int"

let[@zero_alloc] rec singleton_header (headers : Headers.t @ local) name label
    (found : string or_null @ local) = exclave_
  match headers with
  | [] -> found
  | field :: rest ->
    if Headers.same_name field.Headers.name name
    then
      match found with
      | Null -> singleton_header rest name label (This field.Headers.value)
      | This _ ->
        invalid_arg (Printf.sprintf "Proffer.Resp.v: %s must not be repeated" label)
    else singleton_header rest name label found

(* Compared byte by byte rather than through [String.sub]: the value comes
   from a local description and a substring of it cannot escape. *)
let is_byteranges (local_ ct : string) =
  let prefix = "multipart/byteranges" in
  let n = String.length prefix in
  let rec same i =
    i = n
    || (Char.equal
          (Char.lowercase_ascii (String.unsafe_get ct i))
          (String.unsafe_get prefix i)
       && same (i + 1))
  in
  String.length ct >= n && same 0
  && (String.length ct = n || Char.equal ct.[n] ';' || Char.equal ct.[n] ' ')

(* RFC 9110 s14.6: a [multipart/byteranges] body cannot be parsed without the
   boundary its own parts are delimited by. *)
let has_boundary_param (local_ ct : string) =
  Httpz.Multipart.has_boundary ~media_type:"multipart/byteranges" ct

let[@zero_alloc] check_partial_content (status : Status.t)
    (headers : Headers.t @ local)
    (content_type : string or_null @ local) =
  match status with
  | Httpz.Res.Partial_content ->
      let ct =
        match content_type with
        | This ct -> This ct
        | Null -> Headers.find_or_null headers H.Content_type
      in
      (match ct with
       | This ct when is_byteranges ct ->
         if not (has_boundary_param ct) then
           invalid_arg
             "Proffer.Resp.v: a multipart/byteranges 206 response needs a \
              boundary parameter";
         (* Each part carries its own Content-Range; a top-level one would
            describe a single range this multipart body does not have. *)
         if Headers.mem headers H.Content_range then
           invalid_arg
             "Proffer.Resp.v: a multipart/byteranges 206 response must not \
              also carry a top-level Content-Range header"
       | _ ->
         (match singleton_header headers H.Content_range "Content-Range" Null with
          | Null ->
            invalid_arg
              "Proffer.Resp.v: a 206 response needs a Content-Range header \
               or a multipart/byteranges content type"
          | This value
            when Httpz.Range.Content.kind ~unit:"bytes" value
                 <> Httpz.Range.Content.Satisfied ->
            invalid_arg
              (Printf.sprintf
                 "Proffer.Resp.v: Content-Range %S is not a valid RFC 9110 \
                  byte range for a 206 response"
                 (Pct.copy_all value))
          | This _ -> ()))
  | _ -> ()

(* Statuses whose meaning depends on a response field must carry it. *)
let check_status_requires (status : Status.t) (headers : Headers.t @ local) =
  let requires name reason =
    if not (Headers.mem headers name) then
      invalid_arg
        (Printf.sprintf "Proffer.Resp.v: a %d response needs a %s header"
           (Status.code status) reason)
  in
  match status with
  | Httpz.Res.Unauthorized ->
    let () = requires H.Www_authenticate "WWW-Authenticate" in
    ()
  | Httpz.Res.Method_not_allowed ->
    let () = requires H.Allow "Allow" in
    ()
  | Httpz.Res.Proxy_authentication_required ->
    let () = requires H.Proxy_authenticate "Proxy-Authenticate" in
    ()
  | Httpz.Res.Range_not_satisfiable ->
    (match singleton_header headers H.Content_range "Content-Range" Null with
     | Null ->
       let () = requires H.Content_range "Content-Range" in
       ()
     | This value ->
       if
         Httpz.Range.Content.kind ~unit:"bytes" value
         <> Httpz.Range.Content.Unsatisfied
       then
         invalid_arg
           "Proffer.Resp.v: a 416 response needs Content-Range: bytes */length")
  | Httpz.Res.Upgrade_required ->
    let () = requires H.Upgrade "Upgrade" in
    ()
  | _ -> ()

(* Typed arguments own their fields so clients and conditional processing see
   the same value. *)
let check_no_overlap (headers : Headers.t @ local) (name : Headers.name) set =
  if set && Headers.mem headers name then
    invalid_arg
      (Printf.sprintf
         "Proffer.Resp.v: header %S is already set by its own argument"
         (Httpz.Header_name.canonical name))

let[@zero_alloc] rec check_trailer_fields (trailers : Headers.t @ local) =
  match trailers with
  | [] -> ()
  | { Headers.name; spelling; value } :: rest ->
      check_header name spelling value;
      if Httpz.Chunk.is_forbidden_trailer name then
        invalid_arg
          (Printf.sprintf
             "Proffer.Resp.v: %s is forbidden in a trailer section"
             (Pct.copy_all spelling));
      check_trailer_fields rest

let[@zero_alloc] check_stream_trailers (headers : Headers.t @ local)
    (body : Body.t @ local) =
  match body with
  | Body.Stream { trailers; _ } ->
      let has_other_trailer =
        match Headers.find_other headers "trailer" with
        | Some _ -> true
        | None -> false
      in
      (match trailers with
       | _ :: _ when
           Headers.mem headers H.Trailer
           || has_other_trailer ->
           invalid_arg
             "Proffer.Resp.v: Trailer is set by the streamed body's trailers"
       | _ -> ());
      check_trailer_fields trailers
  | _ -> ()

let[@zero_alloc] check_handoff (status : Status.t)
    (headers : Headers.t @ local) (body : Body.t @ local) =
  match body with
  | Body.Handoff { kind = Body.Upgrade protocol; _ } ->
      check_upgrade_protocol protocol;
      if Headers.mem headers H.Upgrade then
        invalid_arg "Proffer.Resp.v: Upgrade is set by the handoff's protocol";
      if status <> Httpz.Res.Switching_protocols then
        invalid_arg "Proffer.Resp.v: an upgrade handoff needs status 101"
  | Body.Handoff { kind = Body.Tunnel; _ } ->
      let code = Status.code status in
      if code < 200 || code >= 300 then
        invalid_arg "Proffer.Resp.v: a tunnel handoff needs a 2xx status"
  | _ ->
      let code = Status.code status in
      if code >= 100 && code < 200 then
        invalid_arg
          "Proffer.Resp.v: an informational status cannot be a final response"

let[@zero_alloc] check_upgrade_header (status : Status.t)
    (headers : Headers.t @ local) =
  match Headers.combined headers H.Upgrade with
  | None -> ()
  | Some value ->
    if not (Httpz.Upgrade.valid_protocol_list value) then
      invalid_arg "Proffer.Resp.v: Upgrade is not a valid protocol list";
    if status <> Httpz.Res.Upgrade_required then
      invalid_arg
        "Proffer.Resp.v: an application-supplied Upgrade header is only valid \
         on status 426"

(* [headers] is a required argument rather than an optional one. An optional
   argument is passed as an allocated [Some], which for a local block would be
   a local option, and a local option cannot cross into the optional-argument
   protocol. The friendly constructors below take the optional form and forward
   to the required one. *)
(* [content_type] is required, and [or_null] rather than an option, for the
   same reason [headers] is required: an optional argument's payload arrives
   local, and a local string cannot reach a [global_] field. [or_null] adds no
   box on top, so a caller naming a content type pays nothing for it. *)
let[@zero_alloc] rec check_headers (headers : Headers.t @ local) =
  match headers with
  | [] -> ()
  | { Headers.name; spelling; value } :: tl ->
      check_header name spelling value;
      check_headers tl

let[@zero_alloc] is_some (o : _ option @ local) =
  match o with None -> false | Some _ -> true

(* A site decorator runs after [v], so it must reapply every invariant that
   depends on the combined header block rather than merely validating the new
   fields in isolation. *)
let[@zero_alloc] with_headers (d : description @ local)
    (extra : Headers.t @ local) : description @ local = exclave_
  check_headers extra;
  check_no_overlap extra H.Content_type
    (match d.content_type with Null -> false | This _ -> true);
  check_no_overlap extra H.Cache_control (is_some d.cache);
  check_no_overlap extra H.Etag (is_some d.etag);
  check_no_overlap extra H.Last_modified (is_some d.last_modified);
  let local_ headers = Headers.cat d.headers extra in
  check_stream_trailers headers d.body;
  check_handoff d.status headers d.body;
  check_upgrade_header d.status headers;
  check_partial_content d.status headers d.content_type;
  check_status_requires d.status headers;
  { d with headers }

(* The responder belongs to the backend, so the checker cannot see into it. *)
let[@inline never][@zero_alloc assume] call_respond (respond : respond @ local)
    (d : description @ local) =
  respond d

let[@zero_alloc] v (respond : respond @ local) ?(status = Httpz.Res.Success)
    ~(headers : Headers.t @ local) ?(etag : Etag.t option @ local)
    ?(last_modified : float option @ local)
    ?(cache : Cache_control.t option @ local)
    ~(content_type : string or_null @ local) (body : Body.t @ local) =
  check_headers headers;
  (match content_type with
  | Null -> ()
  | This ct -> check_value "content_type" ct);
  (match etag with None -> () | Some e -> check_etag e);
  (match last_modified with None -> () | Some t -> check_last_modified t);
  check_body_length body;
  check_stream_trailers headers body;
  check_handoff status headers body;
  check_upgrade_header status headers;
  check_no_overlap headers Httpz.Header_name.Content_type
    (match content_type with Null -> false | This _ -> true);
  check_no_overlap headers Httpz.Header_name.Cache_control (is_some cache);
  check_no_overlap headers Httpz.Header_name.Etag (is_some etag);
  check_no_overlap headers Httpz.Header_name.Last_modified
    (is_some last_modified);
  check_partial_content status headers content_type;
  check_status_requires status headers;
  let local_ d =
    { status; headers; etag; last_modified; cache; content_type; body }
  in
  (* Not a tail call: [d] lives in this frame, so the call must return here
     for the region to be torn down after the backend has consumed it. *)
  let () = call_respond respond d in
  ()

let h = Headers.h
let other = Headers.other
let h_local = Headers.h_local

let html (respond : respond @ local) ?status ?(etag : Etag.t option @ local) ?(cache : Cache_control.t option @ local)
    ?(headers : Headers.t @ local = Headers.empty) s =
  let () =
    v respond ?status ?etag ?cache ~headers ~content_type:(This html_type)
      (stack_ (Body.String s))
  in
  ()

let text (respond : respond @ local) ?status
    ?(headers : Headers.t @ local = Headers.empty) s =
  let () =
    v respond ?status ~headers ~content_type:(This text_type)
      (stack_ (Body.String s))
  in
  ()

let media (respond : respond @ local) ?status ?(etag : Etag.t option @ local) ?(cache : Cache_control.t option @ local)
    ?(headers : Headers.t @ local = Headers.empty) ct s =
  let () =
    v respond ?status ?etag ?cache ~headers ~content_type:(This ct)
      (stack_ (Body.String s))
  in
  ()

(* [write] is taken at the caller's mode rather than [local], since the
   backend runs it after the description has been consumed, which is past the
   point where a local closure would still be alive. *)
let stream (respond : respond @ local) ?status ?(cache : Cache_control.t option @ local)
    ?(headers : Headers.t @ local = Headers.empty) ?length
    ?(trailers : Headers.t @ local = Headers.empty) ct write =
  let () =
    v respond ?status ?cache ~headers ~content_type:(This ct)
      (stack_ (Body.Stream { length; write; trailers }))
  in
  ()

let tunnel (respond : respond @ local) ?(status = Httpz.Res.Success)
    ?(headers : Headers.t @ local = Headers.empty) run =
  let () =
    v respond ~status ~headers ~content_type:Null
      (stack_ (Body.Handoff { kind = Body.Tunnel; run }))
  in
  ()

let upgrade (respond : respond @ local)
    ?(headers : Headers.t @ local = Headers.empty) ~protocol run =
  let () =
    v respond ~status:Httpz.Res.Switching_protocols ~headers ~content_type:Null
      (stack_ (Body.Handoff { kind = Body.Upgrade protocol; run }))
  in
  ()

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

let encode (respond : respond @ local) ?status ?(etag : Etag.t option @ local) ?(cache : Cache_control.t option @ local)
    ?(headers : Headers.t @ local = Headers.empty) codec x =
  let body = Httpz.Media.encode codec x in
  let content_type = Httpz.Media.content_type codec in
  let () =
    v respond ?status ?etag ?cache ~headers ~content_type:(This content_type)
      (stack_ (Body.String body))
  in
  ()

let encode_seq (respond : respond @ local) ?status ?(cache : Cache_control.t option @ local)
    ?(headers : Headers.t @ local = Headers.empty) sq items =
  let write sink =
    let writer =
      Bytes.Writer.make (fun slice ->
        if Bytes.Slice.length slice <> 0 then
          Body.Sink.write_sub sink (Bytes.Slice.bytes slice)
            ~off:(Bytes.Slice.first slice) ~len:(Bytes.Slice.length slice))
    in
    let codec = Httpz.Media.item sq in
    let rec loop items =
      match items () with
      | Seq.Nil -> ()
      | Seq.Cons (x, rest) ->
          Httpz.Media.encode_writer codec x writer;
          Body.Sink.write sink "\n";
          loop rest
    in
    loop items
  in
  let content_type = Httpz.Media.seq_content_type sq in
  let () =
    v respond ?status ?cache ~headers ~content_type:(This content_type)
      (stack_ (Body.Stream
                 { length = None; write; trailers = Headers.empty }))
  in
  ()
