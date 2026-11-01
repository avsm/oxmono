(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type error =
  | Http_error of int * string
  | Jmap_error of Jmap.Proto.Error.Request_error.t
  | Method_error of Jmap.Proto.Error.Method_error.t
  | Json_error of Jsont.Error.t
  | Session_error of string
  | Transport of Fetch.error * string
  | Timeout of float

let pp_escaped = Jmap.Proto.Error.pp_escaped

(* The body is bounded by [max_body], which can still be a whole HTML page. *)
let first_line ?(limit = 120) s =
  let s =
    match String.index_opt s '\n' with Some i -> String.sub s 0 i | None -> s
  in
  let s = String.trim s in
  if String.length s <= limit then s else String.sub s 0 limit ^ "..."

let pp_error fmt = function
  | Http_error (code, msg) ->
      Format.fprintf fmt "HTTP error %d: %a" code pp_escaped (first_line msg)
  | Jmap_error err ->
      Format.fprintf fmt "JMAP error: %a" Jmap.Proto.Error.Request_error.pp err
  | Method_error err -> Jmap.Proto.Error.Method_error.pp fmt err
  | Json_error err ->
      Format.fprintf fmt "JSON error: %a" pp_escaped (Jsont.Error.to_string err)
  | Session_error msg -> Format.fprintf fmt "Session error: %a" pp_escaped msg
  (* The text is the one {!Eio.Exn.pp} gives the failure, which names the
     operation and the host as well as the error itself; the [Fetch.error]
     beside it is what a caller matches on to decide whether to try again. *)
  | Transport (_, msg) ->
      Format.fprintf fmt "Connection error: %a" pp_escaped msg
  | Timeout seconds ->
      Format.fprintf fmt "Connection error: no response within %gs" seconds

let error_to_string err = Format.asprintf "%a" pp_error err

exception Jmap_client_error of error

let ok_exn = function Ok v -> v | Error e -> raise (Jmap_client_error e)

let () =
  Printexc.register_printer (function
    | Jmap_client_error error ->
        Some ("Jmap_eio.Client.Jmap_client_error: " ^ error_to_string error)
    | _ -> None)

(* A URL origin represented by Fetch's canonical URL type, so credential scope
   and origin equality use exactly the same normalization as requests. *)
module Origin : sig
  type t

  val of_url : string -> t option
  val of_fetch_url : Fetch.Middleware.Url.t -> t
  val to_scope : t -> string
  val equal : t -> t -> bool
end = struct
  type t = Fetch.Middleware.Url.t

  let of_url url = Result.to_option (Fetch.Middleware.Url.of_string url)
  let of_fetch_url url = url
  let to_scope = Fetch.Middleware.Url.origin
  let equal a b = Fetch.Middleware.Url.same_origin a b
end

module Uri_template = Httpz_uri.Template

let default_max_body = 64 * 1024 * 1024
let max_session_redirects = 5

(* What bounds one exchange. A timeout without a clock cannot be honoured;
   {!connect} rejects that pairing, so the fallback here never fires in
   practice. *)
type timing = {
  timeout : float option;
  clock : float Eio.Time.clock_ty Eio.Resource.t option;
}

let timed timing fn =
  match (timing.timeout, timing.clock) with
  | Some seconds, Some clock -> (
      try Eio.Time.with_timeout_exn clock seconds fn
      with Eio.Time.Timeout -> Error (Timeout seconds))
  | _ -> fn ()

(* RFC 8620 2 has the session's "urn:ietf:params:jmap:core" capability state
   "maxConcurrentRequests" and "maxConcurrentUpload": the number of requests
   and of uploads the client may have in flight at once. A server is entitled
   to refuse anything beyond them, so they are held here as two semaphores
   rather than left to the caller to remember. A server that states zero,
   which no client could obey, gets a conservative default. A missing value is
   retained defensively for a manually constructed Session; wire decoding
   requires the core capability and every member of its object. *)

let default_concurrency = 4

(* A semaphore is a number of tokens, so an implausible limit is clamped
   rather than trusted: [Int64.to_int] of a large enough value is negative,
   and [Eio.Semaphore.make] of that raises. *)
let max_concurrency = 4096

let concurrency = function
  | Some n when Int64.compare n 0L > 0 ->
      Int64.to_int (Int64.min n (Int64.of_int max_concurrency))
  | _ -> default_concurrency

type admission = {
  mutable maximum : int;
  mutable active : int;
  available : Eio.Condition.t;
}

type limits = { api : admission; upload : admission }

type endpoint_template = {
  source : Uri_template.t;
  absolute : Uri_template.t;
  origin : Origin.t;
}

type endpoints = {
  api_url : string;
  base : Httpz_uri.t;
  upload_template : endpoint_template;
  download_template : endpoint_template;
  event_source_template : endpoint_template;
  origins : Origin.t list;
}

let session_limit session field =
  Option.map field (Jmap.Proto.Session.core_capability session)

let session_concurrency session =
  let field (f : Jmap.Proto.Capability.Core.t -> int64) =
    session_limit session f
  in
  let requests = concurrency (field (fun c -> c.max_concurrent_requests)) in
  let uploads = concurrency (field (fun c -> c.max_concurrent_upload)) in
  (requests, uploads)

let limits_of_session session =
  let requests, uploads = session_concurrency session in
  let create maximum =
    { maximum; active = 0; available = Eio.Condition.create () }
  in
  { api = create requests; upload = create uploads }

let update_limits limits session =
  let requests, uploads = session_concurrency session in
  let update admission maximum =
    admission.maximum <- maximum;
    Eio.Condition.broadcast admission.available
  in
  update limits.api requests;
  update limits.upload uploads

(* Clients are confined to one domain, so testing and taking a slot cannot
   race without a suspension. Refreshes retain the active count. *)
let with_slot admission fn =
  let rec acquire () =
    Eio.Fiber.check ();
    if admission.active < admission.maximum then
      admission.active <- admission.active + 1
    else begin
      Eio.Condition.await_no_mutex admission.available;
      acquire ()
    end
  in
  acquire ();
  Fun.protect
    ~finally:(fun () ->
      admission.active <- admission.active - 1;
      Eio.Condition.broadcast admission.available)
    fn

type t = {
  mutable session : Jmap.Proto.Session.t;
  mutable session_url : string;
  mutable endpoints : endpoints;
  mutable fetch : Fetch.plain;
      (* [base] with the credentials attached, scoped to the session origins. *)
  base : Fetch.plain;
  credentials : Fetch.Credential.t list;
  allow_insecure : bool;
  trust_redirects : bool;
      (* Whether a session redirect off this site may take the credential. *)
  max_body : int;
  timing : timing;
  mono : Eio.Time.Mono.ty Eio.Resource.t option;
  sw : Eio.Switch.t;
      (* The switch the shared session refresh runs under, so that it belongs
         to the client rather than to whichever fiber happened to trigger
         it. *)
  limits : limits;
  session_lock : Eio.Mutex.t;
      (* Guards [refreshing] only, and is never held across a suspension:
         the refetch itself runs outside it, so that a caller's deadline or
         a Ctrl-C can still interrupt a request. *)
  changed : Eio.Condition.t;
  mutable refreshed_for : string option;
      (* The "sessionState" whose mismatch last caused a refetch attempt,
         recorded whether or not the attempt succeeded. *)
  mutable refreshing :
    (Jmap.Proto.Session.t, error) result Eio.Promise.or_exn option;
      (* The refetch in flight, which every other fiber awaits rather than
         starting a second one. *)
  mutable last_refresh_error : error option;
      (* Why the last refetch failed, for a caller that wants to know that
         the session it is holding is stale. *)
  mutable on_change : (Jmap.Proto.Session.t -> unit) list; (* newest first *)
}

let session t = t.session
let fetch t = t.fetch
let switch t = t.sw
let mono_clock t = t.mono
let last_refresh_error t = t.last_refresh_error
let session_changed t = t.changed
let concurrency_limits t = (t.limits.api.maximum, t.limits.upload.maximum)
let on_session_change t f = t.on_change <- f :: t.on_change
let api_url t = t.endpoints.api_url
let upload_template t = t.endpoints.upload_template.absolute
let download_template t = t.endpoints.download_template.absolute
let event_source_template t = t.endpoints.event_source_template.absolute
let upload_url t = Uri_template.to_string (upload_template t)
let download_url t = Uri_template.to_string (download_template t)
let event_source_url t = Uri_template.to_string (event_source_template t)
let session_url t = t.session_url

let scoped ?(extend = false) ~credentials ~allow_insecure ~origins base =
  match credentials with
  | [] -> base
  | credentials ->
      let scope = List.map Origin.to_scope origins in
      Fetch.with_credentials ~scope ~allow_insecure ~extend credentials base

let add origin origins =
  if List.exists (Origin.equal origin) origins then origins
  else origin :: origins

let add_origin url origins =
  match Origin.of_url url with
  | None -> origins
  | Some origin -> add origin origins

module Url = Fetch.Middleware.Url

let session_url_error field reason =
  Error (Session_error (Fmt.str "%s is invalid: %s" field reason))

let http_scheme uri =
  match Httpz_uri.scheme uri with
  | This ("http" | "https") -> Ok ()
  | This scheme ->
      Error
        (Fmt.str
           "uses unsupported scheme %S; JMAP endpoints must use HTTP or HTTPS"
           scheme)
  | Null -> Error "does not resolve to an absolute HTTP or HTTPS URL"

let reference_scheme ~base reference =
  match Httpz_uri.of_string reference with
  | Null -> Ok ()
  | This reference ->
      Httpz_uri.resolve ~base:(Url.to_uri base) reference |> http_scheme

let resolve_api_url ~base reference =
  match reference_scheme ~base reference with
  | Error reason -> session_url_error "apiUrl" reason
  | Ok () -> (
      match Url.resolve ~base reference with
      | Error reason -> session_url_error "apiUrl" reason
      | Ok url -> Ok (Url.to_string url, url))

let missing_variables ~required variables =
  List.filter
    (fun required -> not (List.exists (String.equal required) variables))
    required

let level_name = function
  | `Level_2 -> "Level 2"
  | `Level_3 -> "Level 3"
  | `Level_4 -> "Level 4"

(* These escaped braces produce an absolute display template only. Requests
   expand the original template before resolving it, since a binding such as
   [".."] can change RFC 3986 dot-segment removal. Escaping percent signs
   first distinguishes expressions from literal percent-encoded braces. *)
let absolute_template ~base template =
  let shield text =
    let buffer = Buffer.create (String.length text) in
    String.iter
      (function
        | '%' -> Buffer.add_string buffer "%25"
        | '{' -> Buffer.add_string buffer "%7B"
        | '}' -> Buffer.add_string buffer "%7D"
        | c -> Buffer.add_char buffer c)
      text;
    Buffer.contents buffer
  in
  let restore text =
    let buffer = Buffer.create (String.length text) in
    let rec loop i =
      if i < String.length text then
        if text.[i] = '%' && i + 2 < String.length text then (
          match String.uppercase_ascii (String.sub text (i + 1) 2) with
          | "25" ->
              Buffer.add_char buffer '%';
              loop (i + 3)
          | "7B" ->
              Buffer.add_char buffer '{';
              loop (i + 3)
          | "7D" ->
              Buffer.add_char buffer '}';
              loop (i + 3)
          | _ ->
              Buffer.add_char buffer text.[i];
              loop (i + 1))
        else begin
          Buffer.add_char buffer text.[i];
          loop (i + 1)
        end
    in
    loop 0;
    Buffer.contents buffer
  in
  match
    ( Httpz_uri.of_string (shield (Httpz_uri.to_string base)),
      Httpz_uri.of_string (shield (Uri_template.to_string template)) )
  with
  | This base, This reference ->
      let text = Httpz_uri.(to_string (resolve ~base reference)) |> restore in
      Result.map_error
        (Fmt.str "%a" Uri_template.pp_error)
        (Uri_template.of_string text)
  | _ -> Error "cannot resolve the URI template"

let resolve_template_url ~base ~field ~required source =
  match Uri_template.of_string source with
  | Error error ->
      session_url_error field (Fmt.str "%a" Uri_template.pp_error error)
  | Ok template -> (
      match Uri_template.level template with
      | (`Level_2 | `Level_3 | `Level_4) as level ->
          session_url_error field
            (Fmt.str "uses %s URI-template syntax; JMAP requires Level 1"
               (level_name level))
      | `Level_1 -> (
          let missing =
            missing_variables ~required (Uri_template.variables template)
          in
          if missing <> [] then
            session_url_error field
              (Fmt.str "does not contain required variable%s %s"
                 (if List.length missing = 1 then "" else "s")
                 (String.concat ", " missing))
          else
            let expand marker =
              let lookup name =
                if List.exists (String.equal name) required then
                  Some (`String marker)
                else None
              in
              match
                Uri_template.expand_resolve ~base:(Url.to_uri base) template
                  lookup
              with
              | Error error -> Error (Fmt.str "%a" Uri_template.pp_error error)
              | Ok uri -> (
                  match http_scheme uri with
                  | Error _ as error -> error
                  | Ok () -> Url.of_uri uri)
            in
            match (expand "x", expand "y") with
            | Error reason, _ | _, Error reason ->
                session_url_error field reason
            | Ok first, Ok second -> (
                if not (Url.same_origin first second) then
                  session_url_error field "places a variable in the URL origin"
                else
                  match absolute_template ~base:(Url.to_uri base) template with
                  | Error reason -> session_url_error field reason
                  | Ok absolute ->
                      let origin = Origin.of_fetch_url first in
                      Ok ({ source = template; absolute; origin }, origin))))

(* Parse, validate, and resolve all session endpoints exactly once. Besides
   rejecting malformed or non-Level-1 templates, expanding required variables
   with two values proves that no variable can redirect credentials to another
   origin. *)
let validate_endpoints ~session_url (session : Jmap.Proto.Session.t) =
  let ( let* ) = Result.bind in
  let* base =
    match Url.of_string session_url with
    | Error reason -> session_url_error "session URL" reason
    | Ok base -> Ok base
  in
  let* api_url, api = resolve_api_url ~base session.api_url in
  let* upload_url, upload_origin =
    resolve_template_url ~base ~field:"uploadUrl" ~required:[ "accountId" ]
      session.upload_url
  in
  let* download_url, download_origin =
    resolve_template_url ~base ~field:"downloadUrl"
      ~required:[ "accountId"; "blobId"; "type"; "name" ]
      session.download_url
  in
  let* event_source_url, event_origin =
    resolve_template_url ~base ~field:"eventSourceUrl"
      ~required:[ "types"; "closeafter"; "ping" ]
      session.event_source_url
  in
  let origins =
    []
    |> add (Origin.of_fetch_url base)
    |> add (Origin.of_fetch_url api)
    |> add upload_origin |> add download_origin |> add event_origin
  in
  Ok
    {
      api_url;
      base = Url.to_uri base;
      upload_template = upload_url;
      download_template = download_url;
      event_source_template = event_source_url;
      origins;
    }

let json_media = "application/json"
let problem_media = "application/problem+json"

let json_headers =
  Fetch.Header.
    [ (content_type, media json_media); (accept, [ pref json_media ]) ]

let accept_json = Fetch.Header.[ (accept, [ pref json_media ]) ]
let session_codec = Jmap.Proto.Json.media Jmap.Proto.Session.jsont
let response_codec = Jmap.Proto.Response.media
let upload_codec = Jmap.Proto.Json.media Jmap.Proto.Blob.upload_response_jsont

(* RFC 8620 3.6.1: a request level error arrives as an RFC 7807 problem
   details object. The codec accepts that media type alone, so a decode of an
   error body that is not one fails before the body is read and leaves it for
   {!read_body}. *)
let problem_codec =
  Jmap.Proto.Json.media ~media:problem_media ~accept:[]
    Jmap.Proto.Error.Request_error.jsont

exception Body_too_large of int

let read_body ~max_body response =
  try Eio.Buf_read.(take_all (of_flow ~max_size:max_body (Fetch.body response)))
  with Eio.Buf_read.Buffer_limit_exceeded -> raise (Body_too_large max_body)

(* A sink that collects into a buffer and stops at the client's body limit,
   so that the in-memory blob download of RFC 8620 6.2 is the streaming one
   pointed at memory rather than a second code path. [Eio.Flow.buffer_sink]
   would do the collecting but has no limit, and a blob is whatever size the
   peer says it is. *)
module Bounded_buffer = struct
  type t = { buf : Buffer.t; limit : int; mutable scratch : Bytes.t }

  let single_write t (cs @ local) =
    let cs = Cstruct.globalize_list cs in
    let n = List.fold_left (fun n c -> n + Cstruct.length c) 0 cs in
    if Buffer.length t.buf + n > t.limit then raise (Body_too_large t.limit);
    List.iter
      (fun c ->
        let len = Cstruct.length c in
        if Bytes.length t.scratch < len then t.scratch <- Bytes.create len;
        Cstruct.blit_to_bytes c 0 t.scratch 0 len;
        Buffer.add_subbytes t.buf t.scratch 0 len)
      cs;
    n

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

  let handler =
    Eio.Flow.Pi.sink
      (module struct
        type nonrec t = t

        let single_write = single_write
        let copy = copy
      end)

  let v ~limit buf =
    Eio.Resource.T ({ buf; limit; scratch = Bytes.create 0 }, handler)
end

exception Sink_error of exn
exception Source_error of exn
exception Upload_too_large of int64

(* A blob has no useful total-transfer deadline: a healthy large download can
   take much longer than an ordinary JMAP exchange. It must still not be able
   to stop producing bytes forever. Fetch owns the per-read flow wrapper so
   this policy can be reused without another source implementation. *)
let idle_timed_source timing source =
  match (timing.timeout, timing.clock) with
  | Some seconds, Some clock ->
      Fetch.with_idle_timeout ~clock ~seconds:(Duration.of_f seconds) source
  | _ -> source

(* A caller's sink is not part of the exchange: a full disk is neither a
   transport failure nor a protocol one, and it may raise anything at all.
   Wrapping it tells the two apart, so that a failure of the sink is
   classified as the invalid request it is and never escapes as an
   exception. *)
module Guarded_sink = struct
  type t = Eio.Flow.sink_ty Eio.Resource.t

  let single_write t (cs @ local) =
    let cs = Cstruct.globalize_list cs in
    try Eio.Flow.single_write t cs with
    | Eio.Cancel.Cancelled _ as e -> raise e
    | e -> raise (Sink_error e)

  (* {!idle_timed_source} wraps every read in a fresh timer, so the 4 KiB
     buffer of [Eio.Flow.Pi.simple_copy] arms sixteen times the timers of the
     64 KiB step Fetch itself reads a bounded body in. *)
  let copy_buffer = 64 * 1024

  let copy t ~src =
    let rec write_all buf =
      if not (Cstruct.is_empty buf) then
        write_all (Cstruct.shift buf (single_write t [ buf ]))
    in
    let buf = Cstruct.create copy_buffer in
    try
      while true do
        write_all (Cstruct.sub buf 0 (Eio.Flow.single_read src buf))
      done
    with End_of_file -> ()

  let handler =
    Eio.Flow.Pi.sink
      (module struct
        type nonrec t = t

        let single_write = single_write
        let copy = copy
      end)

  let v sink =
    Eio.Resource.T ((sink :> Eio.Flow.sink_ty Eio.Resource.t), handler)
end

(* A streamed request body belongs to the caller just as a download sink does.
   Hide any optimized read methods so every read crosses this boundary and an
   arbitrary source exception can be returned by the result API. *)
module Guarded_source = struct
  type t = {
    source : Eio.Flow.source_ty Eio.Resource.t;
    maximum : int64 option;
    mutable read : int64;
  }

  let single_read t buf =
    try
      let got = Eio.Flow.single_read t.source buf in
      let read = Int64.add t.read (Int64.of_int got) in
      Option.iter
        (fun maximum -> if read > maximum then raise (Upload_too_large maximum))
        t.maximum;
      t.read <- read;
      got
    with
    | End_of_file -> raise End_of_file
    | Eio.Cancel.Cancelled _ as e -> raise e
    | Upload_too_large _ as e -> raise e
    | e -> raise (Source_error e)

  let handler =
    Eio.Flow.Pi.source
      (module struct
        type nonrec t = t

        let read_methods = []
        let single_read = single_read
      end)

  let v ?maximum source =
    Eio.Resource.T
      ( { source :> Eio.Flow.source_ty Eio.Resource.t; maximum; read = 0L },
        handler )
end

let invalid_request message =
  Error (Transport (Fetch.Invalid_request message, message))

(* Transport failures become {!Transport}; anything else is a programming
   error and propagates. A body over the limit is reported as the protocol
   error it is: the peer sent more than this client agreed to read. *)
let too_large limit =
  let msg = Fmt.str "response body exceeds %d bytes" limit in
  Error (Transport (Fetch.Protocol_error msg, msg))

(* A [Jsont.Error.t] with no source location, for a decoder that reported a
   message alone. *)
let json_error message =
  Jsont.Error.make_msg Jsont.Error.Context.empty Jsont.Meta.none message

let guard ?url ~operation fn =
  let describe exn =
    Error_context.describe ?url ~operation:(Lazy.force operation) exn
  in
  try fn () with
  (* A JSON body the codec rejected keeps its [Jsont.Error.t], which names
     the member at fault and where it is. A body of the wrong media type is
     not a JSON failure at all and stays a transport one. *)
  | Eio.Io (Fetch.E (Fetch.Decode_failure { error; _ } as e), _) as exn -> (
      match error with
      | Fetch.Media.Malformed { detail = Fetch.Json.Error err; _ } ->
          Error (Json_error err)
      | Fetch.Media.Malformed { message; _ } ->
          Error (Json_error (json_error message))
      | Fetch.Media.Too_large limit -> too_large limit
      | Fetch.Media.Unsupported _ -> Error (Transport (e, describe exn)))
  | Eio.Io (Fetch.E e, _) as exn -> Error (Transport (e, describe exn))
  | Body_too_large limit -> too_large limit
  | Fetch.Idle_timeout seconds -> Error (Timeout seconds)
  | Sink_error (Body_too_large limit) -> too_large limit
  | Sink_error exn ->
      invalid_request (Fmt.str "the download sink failed: %s" (describe exn))
  | Source_error exn ->
      invalid_request (Fmt.str "the upload source failed: %s" (describe exn))
  | Upload_too_large maximum ->
      invalid_request
        (Fmt.str "upload exceeds the session's maxSizeUpload of %Ld bytes"
           maximum)
  (* Any other Eio failure: the sink of a {!download_to} refusing the bytes
     (a full disk, a broken pipe), the source of an {!upload_flow} failing to
     yield them, or a backend behind {!Transport.of_fetch} that raises
     something {!Fetch} does not define. None of these should leave a
     function whose type says [(_, error) result] as an exception.
     [Eio.Cancel.Cancelled] is not an [Eio.Io] and still propagates, as Eio
     requires. *)
  | Eio.Io _ as exn ->
      let msg = describe exn in
      Error (Transport (Fetch.Protocol_error msg, msg))
  (* Eio's Unix backend normally wraps operating-system failures in [Eio.Io],
     but a platform or sandbox can reject [socket(2)] before that boundary.
     It is still an operational connection failure, not a programming
     exception that should escape a result-returning client call. *)
  | Unix.Unix_error _ as exn ->
      let msg = describe exn in
      let reason = Eio.Net.Refused (Fetch_httpz.Httpz_error msg) in
      Error (Transport (Fetch.Connection_failure reason, msg))

let is_success response =
  let status = Fetch.status response in
  status >= 200 && status < 300

let is_redirect response =
  match Fetch.status response with
  | 301 | 302 | 303 | 307 | 308 -> true
  | _ -> false

(* RFC 8620 3.6.1: an HTTP error SHOULD carry an RFC 7807 problem details
   object describing a request-level error. Anything else is the status and
   the body as they arrived. *)
let error_of_response ~max_body response =
  let status = Fetch.status response in
  match Fetch.decode ~limit:max_body problem_codec response with
  | err -> Jmap_error err
  | exception Eio.Io (Fetch.E (Fetch.Decode_failure { error; _ }), _) -> (
      match error with
      (* The media type is not problem+json, so nothing has been read. *)
      | Fetch.Media.Unsupported _ ->
          Http_error (status, read_body ~max_body response)
      | Fetch.Media.Too_large limit -> raise (Body_too_large limit)
      | Fetch.Media.Malformed { message; _ } -> Http_error (status, message))

(* RFC 8620 6.1 gives "uploadUrl" as an RFC 6570 level 1 template. *)
let invalid_expansion field error =
  invalid_request
    (Fmt.str "%s expansion failed: %a" field Uri_template.pp_error error)

let expand_endpoint t ~field template bindings =
  match
    Uri_template.expand_resolve_assoc ~base:t.endpoints.base template.source
      bindings
  with
  | Error error -> invalid_expansion field error
  | Ok uri -> (
      let fail reason =
        invalid_request (Fmt.str "%s expansion failed: %s" field reason)
      in
      match http_scheme uri with
      | Error reason -> fail reason
      | Ok () -> (
          match Url.of_uri uri with
          | Error reason -> fail reason
          | Ok url ->
              if Origin.equal template.origin (Origin.of_fetch_url url) then
                Ok (Httpz_uri.to_string uri)
              else fail "places a variable in the URL origin"))

let expand_event_source_url t bindings =
  expand_endpoint t ~field:"eventSourceUrl" t.endpoints.event_source_template
    bindings

let expand_upload_url t ~account_id =
  expand_endpoint t ~field:"uploadUrl" t.endpoints.upload_template
    [ ("accountId", `String (Jmap.Proto.Id.to_string account_id)) ]

let expand_download_url t ~account_id ~blob_id ?name ?accept () =
  expand_endpoint t ~field:"downloadUrl" t.endpoints.download_template
    [
      ("accountId", `String (Jmap.Proto.Id.to_string account_id));
      ("blobId", `String (Jmap.Proto.Id.to_string blob_id));
      ("name", `String (Option.value name ~default:"download"));
      ("type", `String (Option.value accept ~default:"application/octet-stream"));
    ]

(* RFC 8620 2.2: the well-known session resource may redirect, possibly to
   another origin, and the session it eventually serves is what says where
   the credential may travel. So the scope cannot be known before the first
   request and each hop is judged as it is met.

   A same-site hop, meaning both URLs https on the same port with hosts that
   are equal or share a registrable domain under the Public Suffix List, is a
   deployment moving its endpoint and takes the credential with it. So is any
   hop when [trust_redirects] was given, and any hop at all when there is no
   credential to lose. A hop to an origin already in the scope needs no
   extension and is followed as it stands. Anything else stops the walk,
   because a redirect followed blindly hands a bearer token to whoever wrote
   the Location header. *)
let session_policy ~credentials ~trust_redirects ~origins ~stopped =
  let in_scope url =
    let origin = Origin.of_fetch_url url in
    List.exists (Origin.equal origin) origins
  in
  let on_hop ~from ~to_ _ =
    if
      credentials = [] || trust_redirects || Fetch.Redirect.same_site ~from ~to_
    then Fetch.Redirect.Follow_within_scope
    else if in_scope to_ then Fetch.Redirect.Follow
    else begin
      stopped := Some to_;
      Fetch.Redirect.Stop
    end
  in
  Fetch.Redirect.v ~max_hops:max_session_redirects ~on_hop ()

let stopped_error ~url stopped =
  match stopped with
  | None ->
      Session_error
        (Fmt.str "the session at %s redirects without a usable Location header"
           url)
  | Some target ->
      Session_error
        (Fmt.str
           "the session at %s redirects to %s, which is neither the same site \
            nor already in the credential's scope; pass ~trust_redirects:true \
            to send the credential there anyway"
           url
           (Fetch.Middleware.Url.origin target))

(* The walk itself. One deadline covers every hop, since Fetch makes them
   inside one call, and the scope the credential ended up with comes back
   beside the session. *)
let fetch_session ~base ~credentials ~allow_insecure ~trust_redirects ~max_body
    ~origins url =
  let client = scoped ~extend:true ~credentials ~allow_insecure ~origins base in
  let stopped = ref None in
  let redirect =
    session_policy ~credentials ~trust_redirects ~origins ~stopped
  in
  match
    Fetch.with_response ~headers:accept_json ~redirect client `GET url
    @@ fun response ->
    if is_redirect response then
      Error (stopped_error ~url:(Fetch.url response) !stopped)
    else if is_success response then
      let session = Fetch.decode ~limit:max_body session_codec response in
      let session_url = Fetch.url response in
      match validate_endpoints ~session_url session with
      | Error _ as error -> error
      | Ok endpoints ->
          Ok (session, session_url, Fetch.scope response, endpoints)
    else Error (error_of_response ~max_body response)
  with
  | result -> result
  | exception Eio.Io (Fetch.E Fetch.Too_many_redirects, _) ->
      Error
        (Session_error
           (Fmt.str "more than %d redirects fetching the session from %s"
              max_session_redirects url))

(* The origins the credential may travel to once the session is in hand: the
   ones the session names, and the ones the walk was allowed to extend to. *)
let scoped_origins ~extended (endpoints : endpoints) =
  List.fold_left
    (fun origins url -> add_origin url origins)
    endpoints.origins extended

let of_session ~sw ~timing ~mono ~base ~credentials ~allow_insecure
    ~trust_redirects ~max_body ~session_url ~extended ~endpoints session =
  let origins = scoped_origins ~extended endpoints in
  {
    session;
    session_url;
    endpoints;
    fetch = scoped ~credentials ~allow_insecure ~origins base;
    base;
    credentials;
    allow_insecure;
    trust_redirects;
    max_body;
    timing;
    mono;
    sw;
    limits = limits_of_session session;
    session_lock = Eio.Mutex.create ();
    changed = Eio.Condition.create ();
    refreshed_for = None;
    refreshing = None;
    last_refresh_error = None;
    on_change = [];
  }

(* One deadline covers the whole fetch, redirects included: a per-hop
   deadline would let five slow hops take five times as long as the caller
   allowed. *)
let from_url ~sw ~timing ~mono ~credentials ~allow_insecure ~trust_redirects
    ~max_body base url =
  let fetched =
    guard ~operation:(lazy "fetching JMAP session") ~url @@ fun () ->
    timed timing @@ fun () ->
    fetch_session ~base ~credentials ~allow_insecure ~trust_redirects ~max_body
      ~origins:(add_origin url []) url
  in
  match fetched with
  | Error _ as e -> e
  | Ok (session, session_url, extended, endpoints) ->
      Ok
        (of_session ~sw ~timing ~mono ~base ~credentials ~allow_insecure
           ~trust_redirects ~max_body ~session_url ~extended ~endpoints session)

let connect ~sw ?(auth = Auth.none) ?timeout ?(allow_insecure = false)
    ?(trust_redirects = false) ?(max_body = default_max_body) transport url =
  (match timeout with
  | Some seconds when not (Float.is_finite seconds && seconds >= 0.) ->
      invalid_arg "Client.connect: ?timeout must be finite and non-negative"
  | Some seconds -> ignore (Duration.of_f seconds)
  | None -> ());
  if max_body < 1 then invalid_arg "Client.connect: ?max_body must be positive";
  let clock = Transport.clock transport in
  (match (timeout, clock) with
  | Some _, None ->
      invalid_arg
        "Client.connect: ?timeout needs a transport that carries a clock"
  | _ -> ());
  from_url ~sw ~timing:{ timeout; clock }
    ~mono:(Transport.mono_clock transport)
    ~credentials:(Auth.to_credentials auth) ~allow_insecure ~trust_redirects
    ~max_body
    (Transport.fetch transport)
    url

let connect_env ~sw ?auth ?timeout ?allow_insecure ?trust_redirects ?max_body
    env url =
  guard ~operation:(lazy "connecting JMAP client") ~url @@ fun () ->
  connect ~sw ?auth ?timeout ?allow_insecure ?trust_redirects ?max_body
    (Transport.v env) url

(* RFC 8620 2 has the session object carry a "state" string that changes
   whenever anything in it does, such as a new account, a changed limit or a
   moved endpoint, and 3.4 has every response repeat the value the server holds
   in "sessionState". A client that sees the two disagree is looking at a stale
   session, and RFC 8620 2 tells it to fetch the session resource again.
   That is done here rather than left to the caller, because everything this
   module derives from the session (the URLs it posts to, the origins the
   credential may travel to, the concurrency it obeys) would otherwise go on
   being wrong. *)

(* Refetch and install a new session. Exactly one fiber runs this at a time;
   the new session is returned so that the observers run after it. *)
let refresh_now t =
  (* The old endpoint allowlist is deliberately not a seed. The authenticated
     session resource is the authority for a replacement allowlist, and an
     origin it no longer advertises must stop receiving the credential. *)
  let origins = add_origin t.session_url [] in
  let fetched =
    guard ~operation:(lazy "refreshing JMAP session") ~url:t.session_url
    @@ fun () ->
    timed t.timing @@ fun () ->
    fetch_session ~base:t.base ~credentials:t.credentials
      ~allow_insecure:t.allow_insecure ~trust_redirects:t.trust_redirects
      ~max_body:t.max_body ~origins t.session_url
  in
  match fetched with
  | Error _ as e -> e
  | Ok (session, session_url, extended, endpoints) ->
      let origins = scoped_origins ~extended endpoints in
      t.session <- session;
      t.session_url <- session_url;
      t.endpoints <- endpoints;
      t.fetch <-
        scoped ~credentials:t.credentials ~allow_insecure:t.allow_insecure
          ~origins t.base;
      update_limits t.limits session;
      Ok session

(* Observers are run outside the lock, so that one may itself call
   {!refresh_session}, and one that raises does not stop the others: a
   callback is a notification, not part of the refresh. Cancellation is not
   an observer's failure and propagates. *)
let notify t session =
  List.iter
    (fun f ->
      try f session with
      | Eio.Cancel.Cancelled _ as e -> raise e
      | exn ->
          Eio.Private.Trace.log
            ("jmap: session-change observer failed: "
            ^ Httpz_media.sanitize_diagnostic
                (Error_context.describe
                   ~operation:"notifying JMAP session-change observer" exn)))
    (List.rev t.on_change)

(* One refetch for however many fibers want one.

   The exchange is deliberately {e not} run under a mutex. A lock held across
   a network round trip - and [Eio.Mutex.use_rw ~protect:true] additionally
   makes the critical section uncancellable - would put [request] beyond the
   reach of the caller's own [Eio.Time.with_timeout], of an [Eio.Cancel]
   scope and of Ctrl-C for as long as the session endpoint takes to answer.
   Instead the lock guards a promise, which it can do without ever
   suspending: the first fiber to ask forks the exchange under the client's
   switch and the others await that promise, which is itself a cancellation
   point. A fiber that gives up therefore leaves the refresh running for the
   ones that have not, and the failure - including an exception from a
   transport that raises something {!Fetch} does not define - reaches every
   waiter as a value rather than poisoning a lock. *)
let refresh_shared t =
  let promise =
    Eio.Mutex.use_ro t.session_lock @@ fun () ->
    match t.refreshing with
    | Some p -> p
    | None ->
        let p =
          (* Eio schedules the child before [fork_promise] returns and the
             parent holds [session_lock] until it does, so the child must not
             take the lock before its first suspension. It does not:
             [refresh_now] makes a request first. *)
          Eio.Fiber.fork_promise ~sw:t.sw @@ fun () ->
          let result =
            Fun.protect
              ~finally:(fun () ->
                (* The slot is freed even if the fiber is being cancelled,
                   and taking the lock is a cancellation point. *)
                Eio.Cancel.protect @@ fun () ->
                Eio.Mutex.use_ro t.session_lock (fun () -> t.refreshing <- None))
              (fun () ->
                try refresh_now t with
                (* A fatal or programming exception is not a failure of the
                   session to report as a value, on the terms {!guard} sets
                   for the exchange itself. *)
                | ( Eio.Cancel.Cancelled _ | Out_of_memory | Stack_overflow
                  | Invalid_argument _ | Assert_failure _ | Match_failure _
                  | Fun.Finally_raised _ ) as exn ->
                    raise exn
                | exn ->
                    Eio.Fiber.check ();
                    Error
                      (Session_error
                         (Fmt.str "the session refresh failed: %a" Eio.Exn.pp
                            exn)))
          in
          (* Recorded here rather than by a waiter, so that a waiter which
             gives up cannot leave a later success reported as a failure. *)
          t.last_refresh_error <-
            (match result with Ok _ -> None | Error e -> Some e);
          (* After the slot is free, so that an observer may itself ask for a
             refresh, and before the promise resolves, so that a fiber which
             awaited it sees a session the observers have already been told
             about. *)
          (match result with
          | Ok session ->
              notify t session;
              Eio.Condition.broadcast t.changed
          | Error _ -> ());
          result
        in
        t.refreshing <- Some p;
        p
  in
  match Eio.Promise.await promise with
  | Ok result -> result
  (* The exception belongs to the refresh fiber, not to this one, so a
     cancellation of that fiber is a failed refresh here rather than a
     cancellation to re-raise. A waiter whose own context was cancelled is
     stopped by the check. *)
  | Error exn ->
      Eio.Fiber.check ();
      let error =
        Session_error (Fmt.str "the session refresh failed: %a" Eio.Exn.pp exn)
      in
      t.last_refresh_error <- Some error;
      Error error

let refresh_session t =
  match refresh_shared t with Ok _ -> Ok () | Error e -> Error e

(* [state] is the "sessionState" a response just reported. A refetch already
   in flight is joined rather than duplicated, so that three fibers seeing
   the same new state make one request between them and all three return
   with the session it installed. [refreshed_for] records the state that
   caused the attempt {e before} it is made and whether or not it succeeds:
   a server whose session resource lags its API - the refetched session
   still names the old state - and one whose session endpoint is down would
   otherwise both turn every request into two round trips. The failure is
   kept in {!last_refresh_error} rather than dropped. *)
let sync_session t state =
  if not (String.equal state t.session.Jmap.Proto.Session.state) then
    if
      Option.is_some t.refreshing
      || not (Option.equal String.equal t.refreshed_for (Some state))
    then begin
      t.refreshed_for <- Some state;
      ignore (refresh_shared t)
    end

let refresh_session_exn t = ok_exn (refresh_session t)
let core_limit t field = session_limit t.session field

let check_request_limits t req body =
  let calls = List.length req.Jmap.Proto.Request.method_calls in
  match
    core_limit t (fun c -> c.Jmap.Proto.Capability.Core.max_calls_in_request)
  with
  | Some maximum when Int64.of_int calls > maximum ->
      invalid_request
        (Fmt.str
           "request has %d method calls, exceeding the session's \
            maxCallsInRequest of %Ld"
           calls maximum)
  | None | Some _ -> (
      let size = String.length body in
      match
        core_limit t (fun c -> c.Jmap.Proto.Capability.Core.max_size_request)
      with
      | Some maximum when Int64.of_int size > maximum ->
          invalid_request
            (Fmt.str
               "request is %d bytes, exceeding the session's maxSizeRequest of \
                %Ld bytes"
               size maximum)
      | None | Some _ -> Ok ())

let request_operation req =
  let rec names remaining = function
    | [] -> []
    | _ when remaining = 0 -> [ "..." ]
    | (call : Jmap.Proto.Invocation.t) :: rest ->
        first_line ~limit:80 call.name :: names (remaining - 1) rest
  in
  match names 8 req.Jmap.Proto.Request.method_calls with
  | [] -> "performing JMAP request"
  | names -> "calling JMAP methods " ^ String.concat ", " names

(* RFC 8620 3.3 and 3.4: the request and the response are both
   "application/json". *)
let request t req =
  match Jmap.Proto.Json.encode Jmap.Proto.Request.jsont req with
  | Error e -> Error (Json_error e)
  | Ok body ->
      let result =
        with_slot t.limits.api @@ fun () ->
        match check_request_limits t req body with
        | Error _ as error -> error
        | Ok () ->
            timed t.timing @@ fun () ->
            let url = api_url t in
            guard ~operation:(lazy (request_operation req)) ~url @@ fun () ->
            Fetch.with_response ~headers:json_headers ~body:(Fetch.String body)
              t.fetch `POST url
            @@ fun response ->
            if is_success response then
              Ok (Fetch.decode ~limit:t.max_body response_codec response)
            else Error (error_of_response ~max_body:t.max_body response)
      in
      (match result with
      | Ok resp -> sync_session t resp.Jmap.Proto.Response.session_state
      | Error _ -> ());
      result

let request_exn t req = ok_exn (request t req)

(* The capabilities this library has method builders for, in the order RFC
   8621 and then RFC 9610 introduce them. *)
let known_capabilities =
  [
    Jmap.Proto.Capability.core;
    Jmap.Proto.Capability.mail;
    Jmap.Proto.Capability.submission;
    Jmap.Proto.Capability.vacation_response;
    Jmap.Proto.Capability.contacts;
    Jmap.Proto.Capability.calendars;
  ]

let default_capabilities t =
  List.filter
    (fun uri -> Jmap.Proto.Session.has_capability uri t.session)
    known_capabilities

(* RFC 8620 3.2: one request carries a list of method calls, so building a
   chain and sending it is one round trip and belongs in one call. *)
let chain t ?capabilities c =
  let capabilities =
    Option.value capabilities ~default:(default_capabilities t)
  in
  match Jmap.Chain.build ~capabilities c with
  | exception Invalid_argument message -> invalid_request message
  | req, v -> (
      match request t req with Ok resp -> Ok (v, resp) | Error e -> Error e)

let chain_exn t ?capabilities c = ok_exn (chain t ?capabilities c)

(* RFC 8620 Section 3.6.2 answers a failed call with a response named "error",
   which Chain reports apart from a body that did not decode. *)
let of_parse_error = function
  | Jmap.Chain.Method_error e -> Method_error e
  | Jmap.Chain.Json_error e -> Json_error e

let call t ?capabilities c =
  match chain t ?capabilities c with
  | Error e -> Error e
  | Ok (h, response) -> (
      match Jmap.Chain.parse h response with
      | Ok v -> Ok v
      | Error e -> Error (of_parse_error e))

let call_exn t ?capabilities c = ok_exn (call t ?capabilities c)

let run_with_response t ?capabilities c =
  match chain t ?capabilities c with
  | Error e -> Error e
  | Ok (hs, response) -> (
      match Jmap.Chain.parse_all hs response with
      | Ok v -> Ok (hs, v, response)
      | Error e -> Error (of_parse_error e))

let run_with_response_exn t ?capabilities c =
  ok_exn (run_with_response t ?capabilities c)

let run t ?capabilities c =
  Result.map (fun (_, v, _) -> v) (run_with_response t ?capabilities c)

let run_exn t ?capabilities c = ok_exn (run t ?capabilities c)

let get ?headers ?(on_head = ignore) ~operation t url f =
  guard ~operation ~url @@ fun () ->
  Fetch.with_response ?headers t.fetch `GET url @@ fun response ->
  on_head response;
  if is_success response then f response
  else Error (error_of_response ~max_body:t.max_body response)

exception Callback_error of exn * Printexc.raw_backtrace

let with_get ?headers t url f =
  try
    get ?headers
      ~operation:(lazy "fetching JMAP resource")
      t url
      (fun response ->
        try f response
        with exn ->
          raise (Callback_error (exn, Printexc.get_raw_backtrace ())))
  with Callback_error (exn, backtrace) ->
    Printexc.raise_with_backtrace exn backtrace

(* A GET whose total deadline covers the response head. Once it arrives the
   blob copy uses the same duration as an idle-read timeout instead: a large
   healthy transfer may exceed it in total, but a peer that stops producing
   bytes may not hold the caller forever. *)
let head_timed_get t url f =
  let operation = lazy "downloading JMAP blob" in
  match (t.timing.timeout, t.timing.clock) with
  | Some seconds, Some clock -> (
      let arrived = ref false in
      (* Only a successful blob response changes to the per-read idle timeout
         installed by [download_to]. Keep a non-success response under this
         total deadline while [get] consumes its diagnostic body. *)
      let on_head response = if is_success response then arrived := true in
      match
        Eio.Fiber.first
          (fun () -> `Done (get ~on_head ~operation t url f))
          (fun () ->
            Eio.Time.sleep clock seconds;
            if !arrived then Eio.Fiber.await_cancel () else `Timeout)
      with
      | `Done result -> result
      | `Timeout -> Error (Timeout seconds))
  | _ -> get ~operation t url f

let upload_too_large length maximum =
  invalid_request
    (Fmt.str
       "upload is %Ld bytes, exceeding the session's maxSizeUpload of %Ld bytes"
       length maximum)

(* RFC 8620 6.1: a blob is uploaded by POSTing its bytes, with their media
   type, to the expanded "uploadUrl"; the answer is the JSON object naming
   the blob. Both forms of body go through here, and both take a slot of
   "maxConcurrentUpload" (RFC 8620 2). *)
let upload_body t ~account_id ~content_type make_body =
  with_slot t.limits.upload @@ fun () ->
  let ( let* ) = Result.bind in
  let* body = make_body () in
  match expand_upload_url t ~account_id with
  | Error _ as error -> error
  | Ok url ->
      let media_type = content_type in
      let headers =
        Fetch.Header.
          [ (content_type, media media_type); (accept, [ pref json_media ]) ]
      in
      timed t.timing @@ fun () ->
      guard ~operation:(lazy "uploading JMAP blob") ~url @@ fun () ->
      Fetch.with_response ~headers ~body t.fetch `POST url @@ fun response ->
      if is_success response then
        Ok (Fetch.decode ~limit:t.max_body upload_codec response)
      else Error (error_of_response ~max_body:t.max_body response)

let upload t ~account_id ~content_type ~data =
  upload_body t ~account_id ~content_type @@ fun () ->
  let length = Int64.of_int (String.length data) in
  match
    core_limit t (fun c -> c.Jmap.Proto.Capability.Core.max_size_upload)
  with
  | Some maximum when length > maximum -> upload_too_large length maximum
  | None | Some _ -> Ok (Fetch.String data)

let upload_flow t ~account_id ~content_type ?length source =
  upload_body t ~account_id ~content_type @@ fun () ->
  let maximum =
    core_limit t (fun c -> c.Jmap.Proto.Capability.Core.max_size_upload)
  in
  match (length, maximum) with
  | Some length, _ when Int64.compare length 0L < 0 ->
      let message = Fmt.str "upload length %Ld is negative" length in
      invalid_request message
  | Some length, Some maximum when length > maximum ->
      upload_too_large length maximum
  | None, _ | Some _, _ ->
      let source = Guarded_source.v ?maximum source in
      Ok (Fetch.stream ?length source)

let upload_flow_exn t ~account_id ~content_type ?length source =
  ok_exn (upload_flow t ~account_id ~content_type ?length source)

let upload_exn t ~account_id ~content_type ~data =
  ok_exn (upload t ~account_id ~content_type ~data)

(* RFC 8620 6.2: the server serves the blob as the requested type, but it may
   override it; report what actually arrived. *)
let served_type ~accept response =
  match Fetch.header Fetch.Header.content_type response with
  | Some { Fetch.Header.media; _ } -> media
  | None -> Option.value accept ~default:"application/octet-stream"

let download_to t ~account_id ~blob_id ?name ?accept sink =
  match expand_download_url t ~account_id ~blob_id ?name ?accept () with
  | Error _ as error -> error
  | Ok url ->
      let sink = Guarded_sink.v sink in
      head_timed_get t url @@ fun response ->
      let media = served_type ~accept response in
      let source = idle_timed_source t.timing (Fetch.body response) in
      Eio.Flow.copy source sink;
      Ok media

let download_to_exn t ~account_id ~blob_id ?name ?accept sink =
  ok_exn (download_to t ~account_id ~blob_id ?name ?accept sink)

let download_with_type t ~account_id ~blob_id ?name ?accept () =
  let buf = Buffer.create 4096 in
  let sink = Bounded_buffer.v ~limit:t.max_body buf in
  match download_to t ~account_id ~blob_id ?name ?accept sink with
  | Ok media -> Ok (media, Buffer.contents buf)
  | Error _ as e -> e

let download_with_type_exn t ~account_id ~blob_id ?name ?accept () =
  ok_exn (download_with_type t ~account_id ~blob_id ?name ?accept ())

let download t ~account_id ~blob_id ?name ?accept () =
  Result.map snd (download_with_type t ~account_id ~blob_id ?name ?accept ())

let download_exn t ~account_id ~blob_id ?name ?accept () =
  ok_exn (download t ~account_id ~blob_id ?name ?accept ())
