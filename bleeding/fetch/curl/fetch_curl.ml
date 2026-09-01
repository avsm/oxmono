open Fetch

type tag = [ `Generic | `Curl ]
type t = tag Fetch.ty Eio.Resource.t
type Eio.Exn.Backend.t += Curl_error of Curl.curlCode * string

let i16 = Httpz.Buf_read.i16
module I64 = Stdlib_upstream_compatible.Int64_u

let () =
  Eio.Exn.Backend.register_pp (fun f -> function
    | Curl_error (code, "") ->
        Fmt.pf f "Curl_error(%s)" (Curl.strerror code);
        true
    | Curl_error (code, msg) ->
        Fmt.pf f "Curl_error(%s, %S)" (Curl.strerror code) msg;
        true
    | _ -> false)

type config = {
  tls_verify : bool;
  http_version : [ `Auto | `Http1_1 ];
  proxy : string option;
  timeout_ms : int option;
  connect_timeout_ms : int;
  max_response : int;
  max_request : int;
  user_agent : string;
  verbose : bool;
  resolve : (string * int * string) list;
}

let global_init_done = Atomic.make false
let global_init_lock = Mutex.create ()

(* Keep the common path to one atomic read while still serializing libcurl's
   process-wide first initialization. *)
let global_init () =
  if not (Atomic.get global_init_done) then (
    Mutex.lock global_init_lock;
    match
      if not (Atomic.get global_init_done) then (
        Curl.global_init Curl.CURLINIT_GLOBALALL;
        Atomic.set global_init_done true)
    with
    | () -> Mutex.unlock global_init_lock
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        Mutex.unlock global_init_lock;
        Printexc.raise_with_backtrace exn bt)

let invalid_config name reason =
  invalid_arg (Fmt.str "Fetch_curl.v: %s %s" name reason)

let milliseconds name seconds =
  match Float.classify_float seconds with
  | FP_nan | FP_infinite -> invalid_config name "must be finite"
  | FP_normal | FP_subnormal | FP_zero ->
      if seconds < 0. then invalid_config name "must be non-negative";
      (* ocurl passes an OCaml [int] to a C [long]. Keep the value portable to
       platforms where [long] is only 32 bits, and round a positive
       sub-millisecond duration up instead of accidentally disabling it. *)
      let max_ms =
        if Sys.word_size = 32 then max_int else Int32.to_int Int32.max_int
      in
      let ms = seconds *. 1000. in
      if ms > float_of_int max_ms then invalid_config name "is too large";
      if seconds = 0. then 0 else max 1 (int_of_float (Float.ceil ms))

let non_negative name n =
  if n < 0 then invalid_config name "must be non-negative"

let non_negative_c_long name n =
  non_negative name n;
  if Sys.word_size > 32 && n > Int32.to_int Int32.max_int then
    invalid_config name "is too large"

let field_value name value =
  if not (Middleware.is_field_value value) then
    invalid_config name "contains a forbidden control byte"

let canonical_resolve_host host =
  let authority =
    if String.contains host ':' && not (String.starts_with ~prefix:"[" host)
    then "[" ^ host ^ "]"
    else host
  in
  match Middleware.Url.of_string ("http://" ^ authority ^ "/") with
  | Ok url -> Middleware.Url.host url
  | Error reason -> invalid_config "resolve host" reason

let canonical_resolve_address address =
  match Unix.inet_addr_of_string address with
  | address -> Unix.string_of_inet_addr address
  | exception Failure _ ->
    invalid_config "resolve address" "must be a numeric IPv4 or IPv6 address"

let validate_resolve (host, port, address) =
  if port < 1 || port > 65535 then
    invalid_config "resolve port" "must be between 1 and 65535";
  (canonical_resolve_host host, port, canonical_resolve_address address)

let config ~tls_verify ~http_version ~proxy ~timeout ~connect_timeout
    ~max_response ~max_request ~user_agent ~verbose ~resolve =
  non_negative "max_response" max_response;
  non_negative "max_request" max_request;
  Option.iter
    (fun proxy ->
      if String.contains proxy '\x00' then invalid_config "proxy" "contains NUL")
    proxy;
  field_value "user_agent" user_agent;
  let resolve = List.map validate_resolve resolve in
  {
    tls_verify;
    http_version;
    proxy;
    timeout_ms = Option.map (milliseconds "timeout") timeout;
    connect_timeout_ms = milliseconds "connect_timeout" connect_timeout;
    max_response;
    max_request;
    user_agent;
    verbose;
    resolve;
  }

let map_curl_error code msg =
  let pretty () =
    if msg = "" then Curl.strerror code
    else Fmt.str "%s: %s" (Curl.strerror code) msg
  in
  (* [Multi.remove_finished] exposes libcurl's raw [CURLcode] as an OCaml
     constant constructor. A newer libcurl can return a value that predates
     the installed OCaml binding (for example CURLE_TOO_LARGE = 100). Such a
     value is safe to pass back to curl, but a native-code pattern match may
     assume it is inside the binding's declared constructor range and jump
     out of bounds. Compare the stable numeric codes instead. *)
  let number = Curl.errno code in
  let is expected = number = Curl.errno expected in
  if
    List.exists is
      [
        Curl.CURLE_COULDNT_CONNECT;
        Curl.CURLE_COULDNT_RESOLVE_HOST;
        Curl.CURLE_COULDNT_RESOLVE_PROXY;
      ]
  then err (Connection_failure (Refused (Curl_error (code, msg))))
  else if is Curl.CURLE_OPERATION_TIMEOUTED then
    err (Connection_failure Timeout)
  else if
    List.exists is
      [
        Curl.CURLE_SSL_CONNECT_ERROR;
        Curl.CURLE_SSL_PEER_CERTIFICATE;
        Curl.CURLE_SSL_CACERT;
        Curl.CURLE_SSL_CERTPROBLEM;
        Curl.CURLE_SSL_CIPHER;
        Curl.CURLE_SSL_CACERT_BADFILE;
        Curl.CURLE_SSL_ENGINE_NOTFOUND;
        Curl.CURLE_SSL_ENGINE_SETFAILED;
        Curl.CURLE_SSL_ENGINE_INITFAILED;
        Curl.CURLE_SSL_SHUTDOWN_FAILED;
      ]
  then err (Tls_failure (pretty ()))
  else err (Protocol_error (pretty ()))

(* Response header collection. libcurl delivers one line per callback,
   including the status line, interim (1xx) blocks and — after the body
   has started — chunked/h2 *trailer* lines. Reset on each new status
   line so we keep only the final response's headers, and divert
   post-body lines to [trailer_lines]: RFC 9110 s6.5.1 forbids promoting
   a trailer field to a header, and folding e.g. a trailing [Set-Cookie]
   or [Location] into the header block would do exactly that. *)
type collector = {
  request_method : Http.Method.t;
  mutable status_line : string;
  mutable status : int;
  mutable lines : (string * string) list;
  mutable head_complete : bool;
  mutable in_body : bool;
  mutable trailer_lines : (string * string) list;
  mutable chunked : bool;
  mutable connection_close : bool;
  mutable header_bytes : int;
  mutable headers_capped : bool;
  mutable abort_after_head : bool;
  mutable http2 : bool;
  on_error : string -> unit;
  on_close : discard:bool -> unit;
  on_final_head : unit -> unit;
  on_http2_trailer : int -> bool;
}

(* libcurl's own per-line limit still allows a hostile server to stream
   an unbounded *number* of header lines; cap the total ourselves. A
   short return from the callback aborts the transfer — [headers_capped]
   distinguishes that from a generic write failure. Not reset by the 1xx
   reset below: resetting would let a server stream unbounded interim
   blocks, each one under the cap. *)
let max_header_bytes = 256 * 1024

let trim_ows s =
  let ows = function ' ' | '\t' -> true | _ -> false in
  let first = ref 0 and last = ref (String.length s - 1) in
  while !first <= !last && ows s.[!first] do
    incr first
  done;
  while !last >= !first && ows s.[!last] do
    decr last
  done;
  String.sub s !first (!last - !first + 1)

let line_content line =
  let n = String.length line in
  if n >= 2 && line.[n - 2] = '\r' && line.[n - 1] = '\n' then
    Some (String.sub line 0 (n - 2))
  else None

let status_code line =
  let n = String.length line in
  let code_at off =
    if
      n >= off + 3
      && String.for_all
           (function '0' .. '9' -> true | _ -> false)
           (String.sub line off 3)
      && (n = off + 3 || line.[off + 3] = ' ')
      &&
      let reason_off = min n (off + 4) in
      Middleware.is_field_value (String.sub line reason_off (n - reason_off))
    then
      Some
        (((Char.code line.[off] - Char.code '0') * 100)
        + ((Char.code line.[off + 1] - Char.code '0') * 10)
        + Char.code line.[off + 2] - Char.code '0')
    else None
  in
  if
    n >= 9
    && String.starts_with ~prefix:"HTTP/1." line
    && line.[7] >= '0'
    && line.[7] <= '9'
    && line.[8] = ' '
  then code_at 9
  else if n >= 7 && String.starts_with ~prefix:"HTTP/2 " line then code_at 7
  else None

(* RFC 9112 framing ends at the head for these responses. A 205 is different:
   RFC 9110 forbids representation content, but RFC 9112 still requires its
   declared zero-length framing to be consumed. *)
let framing_bodyless meth status =
  meth = `HEAD
  || (meth = `CONNECT && status >= 200 && status < 300)
  || (status >= 100 && status < 200)
  || status = 204 || status = 304

let contentless meth status = status = 205 || framing_bodyless meth status

let add_field lines line =
  if line <> "" && (line.[0] = ' ' || line.[0] = '\t') then
    match lines with
    | [] -> Error "response field continuation has no preceding field"
    | (name, value) :: rest ->
        let continuation = trim_ows line in
        if Middleware.is_field_value continuation then
          Ok ((name, value ^ " " ^ continuation) :: rest)
        else Error "response field value contains a forbidden control byte"
  else
    match String.index_opt line ':' with
    | None -> Error "response field has no colon"
    | Some i ->
        let name = String.sub line 0 i in
        let value = String.sub line (i + 1) (String.length line - i - 1) in
        if not (Middleware.is_token name) then
          Error "response field name is not an HTTP token"
        else if not (Middleware.is_field_value value) then
          Error "response field value contains a forbidden control byte"
        else Ok ((name, trim_ows value) :: lines)

let field_values name lines =
  let name = String.lowercase_ascii name in
  List.filter_map
    (fun (field, value) ->
      if String.equal (String.lowercase_ascii field) name then Some value
      else None)
    (List.rev lines)

let validate_content_length lines =
  let values = field_values "content-length" lines in
  let parse value =
    let len = String.length value in
    if len > Httpz.buffer_size then Error "Content-Length field is too large"
    else
      let buf = Bytes.unsafe_of_string value in
      let span = Httpz.Span.make ~off:(i16 0) ~len:(i16 len) in
      let #(parsed, overflow, conflicting) =
        Httpz.Span.parse_content_length buf span
      in
      if overflow then Error "Content-Length exceeds int64"
      else if conflicting then Error "conflicting Content-Length values"
      else if I64.compare parsed #0L < 0 then Error "invalid Content-Length"
      else Ok (I64.to_int64 parsed)
  in
  (* Httpz owns the field-value grammar and overflow checks. This fold only
     combines the separate lines delivered by libcurl. *)
  let rec fold expected = function
    | [] -> Ok expected
    | value :: rest -> begin
        match parse value with
        | Error _ as error -> error
        | Ok parsed -> begin
            match expected with
            | None -> fold (Some parsed) rest
            | Some expected when Int64.equal parsed expected ->
                fold (Some expected) rest
            | Some _ -> Error "conflicting Content-Length values"
          end
      end
  in
  fold None values

let transfer_codings lines =
  let values = field_values "transfer-encoding" lines in
  if values = [] then Ok None
  else
    let rec fold count chunked_count last_chunked = function
      | [] -> Ok (count, chunked_count, last_chunked)
      | value :: rest ->
          let len = String.length value in
          if len > Httpz.buffer_size then
            Error "Transfer-Encoding field is too large"
          else
            let buf = Bytes.unsafe_of_string value in
            let span = Httpz.Span.make ~off:(i16 0) ~len:(i16 len) in
            let #(n, chunks, last, valid) =
              Httpz.Span.parse_transfer_encoding buf span
            in
            if not valid then Error "invalid Transfer-Encoding coding"
            else
              fold (count + n) (chunked_count + chunks)
                (if n = 0 then last_chunked else last)
                rest
    in
    match fold 0 0 false values with
    | Error _ as error -> error
    | Ok (0, _, _) -> Error "empty Transfer-Encoding"
    | Ok (count, chunked_count, last_chunked) ->
        if chunked_count <> 1 then
          Error "Transfer-Encoding must contain exactly one chunked coding"
        else if not last_chunked then
          Error "chunked is not the final Transfer-Encoding"
        else if count <> 1 then
          Error "unsupported response Transfer-Encoding chain"
        else Ok (Some `Chunked)

let has_token name wanted lines =
  field_values name lines
  |> List.exists (fun value ->
         let len = String.length value in
         (* A Connection field larger than Httpz's bounded parse window is not
            worth trusting for reuse. *)
         len > Httpz.buffer_size
         ||
         let buf = Bytes.unsafe_of_string value in
         let span = Httpz.Span.make ~off:(i16 0) ~len:(i16 len) in
         Httpz.Span.token_list_contains buf span wanted)

let validate_framing c =
  match (validate_content_length c.lines, transfer_codings c.lines) with
  | Error reason, _ | _, Error reason -> Error reason
  | Ok (Some _), Ok (Some `Chunked)
    when not (framing_bodyless c.request_method c.status) ->
      Error "response contains both Transfer-Encoding and Content-Length"
  | Ok content_length, Ok transfer ->
      c.chunked <- transfer = Some `Chunked;
      let connection = field_values "connection" c.lines in
      let simple_close =
        match connection with
        | [ value ] ->
            String.equal
              (String.lowercase_ascii (trim_ows value))
              "close"
        | _ -> false
      in
      (* Libcurl already handles the ordinary single-token form.  Interpose
         only for a close token hidden in a list or repeated field, which is
         the form affected by its reuse bug. *)
      c.connection_close <-
        (not simple_close) && has_token "connection" "close" c.lines;
      let no_body_delivery =
        framing_bodyless c.request_method c.status
        || (transfer = None && content_length = Some 0L)
      in
      if c.connection_close then c.on_close ~discard:false;
      if c.connection_close && no_body_delivery then begin
        (* A zero-body response has no write callback on which to pause. A
           deliberate header-callback abort keeps libcurl from pooling this
           connection; ordinary fiber context applies FORBID_REUSE before
           the completion is collected. *)
        c.abort_after_head <- true
      end;
      Ok ()

let reject_line c reason =
  c.on_error reason;
  0

let header_callback c raw_line =
  let n = String.length raw_line in
  if n > max_header_bytes - c.header_bytes then (
    c.headers_capped <- true;
    reject_line c (Fmt.str "response headers exceed %d bytes" max_header_bytes))
  else begin
    c.header_bytes <- c.header_bytes + n;
    match line_content raw_line with
    | None -> reject_line c "response head uses an invalid line ending"
    | Some line when String.starts_with ~prefix:"HTTP/" line -> begin
        match status_code line with
        | Some status ->
          c.status_line <- line;
          c.status <- status;
          c.lines <- [];
          c.head_complete <- false;
          c.in_body <- false;
          c.trailer_lines <- [];
          c.chunked <- false;
          c.connection_close <- false;
          c.abort_after_head <- false;
          c.http2 <- String.starts_with ~prefix:"HTTP/2 " line;
          n
        | None -> reject_line c "malformed HTTP response status line"
      end
    | Some "" when c.in_body ->
        if c.http2 && not (c.on_http2_trailer n) then
          0
        else n
    | Some "" ->
        (match validate_framing c with
        | Error reason -> reject_line c reason
        | Ok () ->
            let final = c.status < 100 || c.status >= 200 || c.status = 101 in
            c.head_complete <- final;
            if final then begin
              if not (framing_bodyless c.request_method c.status) then
                c.in_body <- true;
              c.on_final_head ()
            end;
            if c.abort_after_head then 0 else n)
    | Some line ->
        let fields = if c.in_body then c.trailer_lines else c.lines in
        if c.in_body && c.http2 && not (c.on_http2_trailer n) then
          0
        else begin match add_field fields line with
        | Error reason -> reject_line c reason
        | Ok fields ->
            if c.in_body then c.trailer_lines <- fields else c.lines <- fields;
            n
        end
  end

(* CURLOPT_HTTP_TRANSFER_DECODING is disabled below so the application sees
   chunk framing before libcurl's permissive decoder can erase it.  Validate
   the size line and trailers with Httpz, but stream chunk data directly into
   the response queue rather than buffering a whole chunk. *)
module Chunked = struct
  type phase =
    | Size
    | Data of int
    | Data_cr
    | Data_lf
    | Trailers
    | Done

  type t = {
    line : Buffer.t;
    trailers_buf : Buffer.t;
    mutable phase : phase;
    mutable trailers : Http.Header.t option;
  }

  let create () =
    {
      line = Buffer.create 80;
      trailers_buf = Buffer.create 256;
      phase = Size;
      trailers = None;
    }

  let max_size_line = 64 * 1024
  let max_trailers = 16 * 1024

  let ends_in_cr buffer =
    let n = Buffer.length buffer in
    n > 0 && Buffer.nth buffer (n - 1) = '\r'

  let add_line_byte ~limit buffer c =
    if Buffer.length buffer >= limit then Error "chunk framing line is too large"
    else if c = '\n' && not (ends_in_cr buffer) then
      Error "chunk framing contains a bare LF"
    else if c <> '\n' && ends_in_cr buffer then
      Error "chunk framing contains a bare CR"
    else begin
      Buffer.add_char buffer c;
      Ok ()
    end

  let parse_size t =
    let line = Buffer.contents t.line in
    let bytes = Bytes.of_string line in
    Buffer.clear t.line;
    match
      Httpz.Chunk.parse_header bytes ~off:(i16 0)
        ~len:(i16 (Bytes.length bytes))
        ~max_chunk_size:max_int
    with
    | #(Httpz.Chunk.Complete, size, _) ->
        t.phase <- Data size;
        Ok ()
    | #(Httpz.Chunk.Done, _, _) ->
        t.phase <- Trailers;
        Ok ()
    | #(Httpz.Chunk.Chunk_too_large, _, _) -> Error "chunk size is too large"
    | #(Httpz.Chunk.Partial, _, _) | #(Httpz.Chunk.Malformed, _, _) ->
        Error "malformed chunk size or extension"

  let trailers_complete buffer =
    let n = Buffer.length buffer in
    (n = 2 && Buffer.nth buffer 0 = '\r' && Buffer.nth buffer 1 = '\n')
    ||
    (n >= 4
    && Buffer.nth buffer (n - 4) = '\r'
    && Buffer.nth buffer (n - 3) = '\n'
    && Buffer.nth buffer (n - 2) = '\r'
    && Buffer.nth buffer (n - 1) = '\n')

  let parse_trailers t =
    let bytes = Bytes.of_string (Buffer.contents t.trailers_buf) in
    match
      Httpz.Chunk.parse_trailers bytes ~off:(i16 0)
        ~len:(i16 (Bytes.length bytes)) ~max_header_count:(i16 100)
    with
    | #(Httpz.Chunk.Trailer_complete, _, fields) ->
        let fields =
          List.rev (Httpz.Header.to_string_pairs_local bytes fields)
        in
        t.trailers <-
          (match fields with [] -> None | _ -> Some (Http.Header.of_list fields));
        t.phase <- Done;
        Ok ()
    | #(Httpz.Chunk.Trailer_partial, _, _)
    | #(Httpz.Chunk.Trailer_malformed, _, _)
    | #(Httpz.Chunk.Trailer_bare_cr, _, _) ->
        Error "malformed chunk trailers"

  let rec feed t input at emit =
    if at = String.length input then `Ok
    else
      match t.phase with
      | Done -> `Trailing
      | Size ->
          let c = String.unsafe_get input at in
          (match add_line_byte ~limit:max_size_line t.line c with
          | Error reason -> `Error reason
          | Ok () ->
              if c <> '\n' then feed t input (at + 1) emit
              else
                match parse_size t with
                | Error reason -> `Error reason
                | Ok () -> feed t input (at + 1) emit)
      | Data 0 ->
          t.phase <- Data_cr;
          feed t input at emit
      | Data left ->
          let n = min left (String.length input - at) in
          emit (String.sub input at n);
          t.phase <- Data (left - n);
          feed t input (at + n) emit
      | Data_cr ->
          if String.unsafe_get input at <> '\r' then
            `Error "chunk data is not followed by CRLF"
          else begin
            t.phase <- Data_lf;
            feed t input (at + 1) emit
          end
      | Data_lf ->
          if String.unsafe_get input at <> '\n' then
            `Error "chunk data is not followed by CRLF"
          else begin
            t.phase <- Size;
            feed t input (at + 1) emit
          end
      | Trailers ->
          let c = String.unsafe_get input at in
          (match add_line_byte ~limit:max_trailers t.trailers_buf c with
          | Error reason -> `Error reason
          | Ok () ->
              if not (trailers_complete t.trailers_buf) then
                feed t input (at + 1) emit
              else
                match parse_trailers t with
                | Error reason -> `Error reason
                | Ok () -> feed t input (at + 1) emit)

  let feed t input emit = feed t input 0 emit

  let finish t =
    match t.phase with
    | Done -> Ok ()
    | Size -> Error "chunked body ended inside a size line"
    | Data _ -> Error "chunked body ended inside chunk data"
    | Data_cr | Data_lf -> Error "chunked body ended before a data CRLF"
    | Trailers -> Error "chunked body ended before its final trailer line"
end

let version_of_status_line sl : version =
  match String.split_on_char ' ' sl with
  | "HTTP/1.0" :: _ -> `HTTP_1_0
  | "HTTP/1.1" :: _ -> `HTTP_1_1
  | ("HTTP/2" | "HTTP/2.0") :: _ -> `HTTP_2
  | v :: _ when v <> "" -> `Other v
  | _ -> `Other "unknown"

type wire_body = No_body | Fixed of string | Streamed of int64 option

(* RFC 9110 §8.6 asks a user agent to send [Content-Length: 0] on a request
   whose method gives enclosed content a defined meaning, even when there is
   none, so a recipient need not guess whether content was omitted or merely
   not yet framed. libcurl does not add it on its own for a body-less
   [CUSTOMREQUEST]. *)
let has_defined_content = function
  | `POST | `PUT | `PATCH -> true
  | _ -> false

(* CURLOPT_VERBOSE's default sink writes complete request fields, URLs and
   transfer data to stderr.  Those routinely contain credentials.  Preserve
   the useful event direction and byte counts, but never the bytes themselves;
   a caller that needs payload-level tracing can install it outside this
   credential-aware client boundary. *)
let redacted_debug _handle kind data =
  let event =
    match kind with
    | Curl.DEBUGTYPE_TEXT -> "diagnostic"
    | Curl.DEBUGTYPE_HEADER_IN -> "response headers"
    | Curl.DEBUGTYPE_HEADER_OUT -> "request headers"
    | Curl.DEBUGTYPE_DATA_IN -> "response data"
    | Curl.DEBUGTYPE_DATA_OUT -> "request data"
    | Curl.DEBUGTYPE_SSL_DATA_IN -> "TLS data received"
    | Curl.DEBUGTYPE_SSL_DATA_OUT -> "TLS data sent"
    | Curl.DEBUGTYPE_END -> "end"
  in
  Fmt.epr "fetch-curl: %s (%d bytes redacted)@." event (String.length data)

let setup_handle cfg h (req : Middleware.request) body ~on_error ~on_close
    ~on_final_head ~on_http2_trailer =
  Curl.set_followlocation h false;
  Curl.set_protocols h [ Curl.CURLPROTO_HTTP; Curl.CURLPROTO_HTTPS ];
  Curl.set_netrc h Curl.CURL_NETRC_IGNORED;
  Curl.set_proxy h (Option.value cfg.proxy ~default:"");
  (* Empty CURLOPT_NOPROXY overrides both NO_PROXY spellings. In particular,
     an explicitly selected proxy cannot be bypassed by ambient exclusions. *)
  Curl.set_noproxy h "";
  Curl.set_sslverifypeer h cfg.tls_verify;
  Curl.set_sslverifyhost h
    (if cfg.tls_verify then Curl.SSLVERIFYHOST_HOSTNAME
     else Curl.SSLVERIFYHOST_NONE);
  Curl.set_useragent h cfg.user_agent;
  Curl.set_connecttimeoutms h cfg.connect_timeout_ms;
  Option.iter (Curl.set_timeoutms h) cfg.timeout_ms;
  Curl.set_httpversion h
    (match cfg.http_version with
    | `Auto -> Curl.HTTP_VERSION_2TLS
    | `Http1_1 -> Curl.HTTP_VERSION_1_1);
  if cfg.verbose then begin
    Curl.set_debugfunction h redacted_debug;
    Curl.set_verbose h true
  end;
  Curl.set_url h (Middleware.Url.to_string req.url);
  if cfg.resolve <> [] then Curl.set_resolve h cfg.resolve [];
  let auto_decode = not (Http.Header.mem req.headers "accept-encoding") in
  (* Negotiate only gzip, then retain both transfer and content coding until
     the strict streaming layers below have validated them. *)
  if auto_decode then Curl.set_encoding h Curl.CURL_ENCODING_GZIP;
  Curl.set_httptransferdecoding h false;
  Curl.set_httpcontentdecoding h false;
  (match req.meth with
  | `HEAD -> Curl.set_nobody h true
  | m -> Curl.set_customrequest h (Http.Method.to_string m));
  (match body with
  | No_body -> ()
  | Fixed s ->
      Curl.set_postfields h s;
      Curl.set_postfieldsizelarge h (Int64.of_int (String.length s))
  | Streamed length ->
      (* CURLOPT_UPLOAD switches the handle to the read callback and picks
        the framing: [Content-Length] from the infile size, chunked when
        it is unknown. The method still comes from [set_customrequest]. *)
      Curl.set_upload h true;
      Option.iter (Curl.set_infilesizelarge h) length);
  let extra_headers =
    (* "Name:" (no value) removes a curl default header. No 100-continue
       stalls; no invented Content-Type for bodies that didn't declare one. *)
    "Expect:"
    ::
    (if body <> No_body && not (Http.Header.mem req.headers "content-type") then
       [ "Content-Type:" ]
     else [])
    @
    (if body = No_body && has_defined_content req.meth then
       [ "Content-Length: 0" ]
     else [])
  in
  let request_headers =
    (* "Name;" is libcurl's syntax for sending a header with an empty
       value — "Name:" would *remove* the header instead. *)
    List.map
      (fun (n, v) -> if v = "" then n ^ ";" else n ^ ": " ^ v)
      (Http.Header.to_list req.headers)
  in
  Curl.set_httpheader h (request_headers @ extra_headers);
  (* libcurl built with the synchronous resolver would otherwise use
     SIGALRM for DNS timeouts — unsafe with multiple domains. *)
  Curl.set_nosignal h true;
  let col =
    {
      request_method = req.meth;
      status_line = "";
      status = 0;
      lines = [];
      head_complete = false;
      in_body = false;
      trailer_lines = [];
      chunked = false;
      connection_close = false;
      header_bytes = 0;
      headers_capped = false;
      abort_after_head = false;
      http2 = false;
      on_error;
      on_close;
      on_final_head;
      on_http2_trailer;
    }
  in
  Curl.set_headerfunction h (header_callback col);
  (col, auto_decode)

let too_large cfg =
  err
    (Invalid_request (Fmt.str "request body exceeds %d bytes" cfg.max_request))

(* Classify a request body for the wire. A declared length over
   [max_request] is rejected here, before the network is touched. An
   undeclared one is counted as the pump reads it. *)
let wire_body_of cfg (req : Middleware.request) =
  match req.body with
  | Empty -> No_body
  | String s -> Fixed s
  | Stream { length; _ } ->
      (match length with
      | Some l when Int64.compare l 0L < 0 ->
          raise
            (err
               (Invalid_request
                  (Fmt.str "request body length %Ld is negative" l)))
      | Some l when Int64.unsigned_compare l (Int64.of_int cfg.max_request) > 0
        ->
          raise (too_large cfg)
      | _ -> ());
      Streamed length

(* One [Curl.Multi.mt] per client gives connection reuse and HTTP/2
   multiplexing across requests. All fibers live in one domain, so calls
   into libcurl never race.

   Effects cannot cross the C frames of a libcurl callback, so the
   socket/timer callbacks must not fork fibers, resolve promises or
   suspend. They only record intents in [pending_sockets] and
   [pending_timer]; [curl_call] applies them (and dispatches completions)
   from ordinary fiber context after every call into libcurl returns. *)

(* A slot for one sleeping fiber. Each waiter on a job needs its own:
   the response reader and the upload pump sleep on the same job. *)
type waiter = { mutable u : unit Eio.Promise.u option }

(* A streaming request body in flight. The pump fiber fills [chunks]
   from the flow and the read callback drains them: at [high_water] the
   pump stops reading, and with nothing buffered the callback pauses the
   send side until the pump enqueues more — the bound on what an upload
   can make the client buffer. *)
type upload = {
  chunks : string Queue.t;
  wake : waiter;
  stop : unit Eio.Promise.t;
  stop_u : waiter;
  expected : int option;
  mutable head_off : int;
  mutable queued : int;
  mutable read : int;
  mutable eof : bool;
  mutable error : exn option;
  mutable paused : bool;
}

(* One transfer, from [Multi.add] until its handle is cleaned up. The
   write callback fills [chunks]; the response body flow drains them.
   [queued] counts the bytes in [chunks]: at [high_water] the write
   callback pauses the transfer, and the reader unpauses it once it has
   drained down to [low_water] — the bound on what a response can make
   the client buffer. *)
type job = {
  id : string;
  h : Curl.t;
  chunks : string Queue.t;
  errbuf : string ref;
  upload : upload option;
  wake : waiter;
  mutable head_off : int;
  mutable queued : int;
  (* Raw bytes accepted by the write callback, including chunk framing and
     trailers. [received] separately counts de-framed representation bytes. *)
  mutable wire_received : int;
  mutable received : int;
  mutable paused : bool;
  mutable over_limit : bool;
  mutable protocol_error : string option;
  mutable chunked : Chunked.t option;
  mutable close_enforced : bool;
  mutable force_close : bool;
  mutable discard_remainder : bool;
  mutable skip_redelivery : bool;
  mutable added : bool;
  mutable body_started : bool;
  mutable body_abandoned : bool;
  mutable finished : Curl.curlCode option;
  mutable cleaned : bool;
  mutable handle_cleaned : bool;
  mutable pending_wake : bool;
  mutable hook : Eio.Switch.hook;
}

type watcher = { stop : unit Eio.Promise.u }

type engine = {
  cfg : config;
  mt : Curl.Multi.mt;
  sw : Eio.Switch.t;
  (* Calls into the shared [Curl.Multi] are only safe from the domain
     whose event loop drives it; using the client elsewhere would be a
     data race in C. *)
  dom : Domain.id;
  (* Keyed by the handle's ocurl-private tag because [Multi.remove_finished]
     returns the same C handle in a fresh OCaml block. Holds the jobs still
     attached to the multi (until [remove_finished]/cleanup). *)
  jobs : (string, job) Hashtbl.t;
  mutable next_id : int;
  (* Once the engine switch has released the multi, no libcurl multi
     call may run again; late job cleanups (from request switches that
     outlive the client's) must not touch it. *)
  mutable shutting_down : bool;
  mutable closed : bool;
  mutable failure : (exn * Printexc.raw_backtrace) option;
  watchers : (Unix.file_descr, watcher) Hashtbl.t;
  (* Recorded newest first by the callbacks, replayed oldest first. *)
  mutable pending_sockets : (Unix.file_descr * Curl.Multi.poll) list;
  mutable pending_timer : int option;
  mutable timer_gen : int;
  mutable timer_stop : unit Eio.Promise.u option;
}

let notify w =
  match w.u with
  | None -> ()
  | Some u ->
      w.u <- None;
      Eio.Promise.resolve u ()

let wake job =
  notify job.wake;
  Option.iter (fun (up : upload) -> notify up.wake) job.upload

let fail_engine eng ex bt =
  if eng.failure = None then begin
    eng.failure <- Some (ex, bt);
    Hashtbl.iter (fun _ job -> try wake job with _ -> ()) eng.jobs
  end

let raise_engine_failure eng =
  match eng.failure with
  | None -> ()
  | Some (ex, bt) -> Printexc.raise_with_backtrace ex bt

(* Open until the engine switch has released the multi, and live while it
   is also unpoisoned. A poisoned engine makes no further multi call, but
   it still owns the watchers a teardown has to stop. *)
let engine_open eng = (not eng.shutting_down) && not eng.closed
let engine_live eng = engine_open eng && eng.failure = None

(* A transfer libcurl may still be told about: one that has neither
   completed nor been cleaned up, on an engine that can carry it. *)
let job_active eng job =
  job.finished = None && (not job.cleaned) && eng.failure = None

let pause_flags job =
  (if job.paused then [ Curl.PAUSE_RECV ] else [])
  @
  match job.upload with
  | Some up when up.paused -> [ Curl.PAUSE_SEND ]
  | _ -> []

(* Callback code can only record this intent. Apply it before harvesting
   completions after every libcurl call: for a zero-byte response this is the
   sole interval in which the handle is still attached and can be kept out of
   the connection pool. Unpausing here also guarantees that trailing garbage
   discovered after [request] returned cannot strand the transfer. *)
let enforce_close_intents eng =
  Hashtbl.iter
    (fun _ job ->
      if job.force_close && not job.close_enforced then begin
        Curl.set_forbidreuse job.h true;
        job.close_enforced <- true;
        if job.paused && job_active eng job then begin
          job.paused <- false;
          Curl.pause job.h (pause_flags job)
        end
      end)
    eng.jobs

let cleanup_handle job =
  if not job.handle_cleaned then begin
    Curl.cleanup job.h;
    job.handle_cleaned <- true
  end

let fresh_job_id eng =
  let id = string_of_int eng.next_id in
  eng.next_id <- eng.next_id + 1;
  id

(* Wait until [cond] holds. Fibers share one domain and nothing yields
   between testing [cond] and registering the waker, so no wake-up can
   be lost. *)
let rec wait_for w cond =
  if not (cond ()) then begin
    let p, u = Eio.Promise.create () in
    w.u <- Some u;
    Eio.Promise.await p;
    wait_for w cond
  end

let socket_function eng fd (poll : Curl.Multi.poll) =
  eng.pending_sockets <- (fd, poll) :: eng.pending_sockets

let timer_function eng ms = eng.pending_timer <- Some ms

let find_job eng id h =
  match Hashtbl.find_opt eng.jobs id with
  | Some job -> Some job
  | None ->
      (* ocurl's custom comparison uses the underlying [Connection *], so it
         is a safe fallback if retrieving the private tag failed. *)
      Hashtbl.fold
        (fun _ job found ->
          match found with
          | Some _ -> found
          | None -> if compare job.h h = 0 then Some job else None)
        eng.jobs None

let check_completions eng =
  let rec go () =
    match Curl.Multi.remove_finished eng.mt with
    | None -> ()
    | Some (h, code) ->
        let id = try Curl.get_private h with _ -> "" in
        (match find_job eng id h with
        | Some job ->
            Hashtbl.remove eng.jobs job.id;
            job.added <- false;
            (match (code, job.chunked) with
            | Curl.CURLE_OK, Some decoder -> begin
                match Chunked.finish decoder with
                | Ok () -> ()
                | Error reason -> job.protocol_error <- Some reason
              end
            | _ -> ());
            job.finished <- Some code;
            if job.cleaned then cleanup_handle job;
            wake job
        | None ->
            (* [remove_finished] has detached the handle. If no job owns it,
            release the native easy handle instead of relying on ocurl's
            leak-reporting finalizer. *)
            Curl.cleanup h);
        go ()
  in
  go ()

let process_wakes eng =
  Hashtbl.iter
    (fun _ job ->
      if job.pending_wake then begin
        job.pending_wake <- false;
        wake job
      end)
    eng.jobs

let stop_watcher eng fd =
  match Hashtbl.find_opt eng.watchers fd with
  | Some watcher ->
      Hashtbl.remove eng.watchers fd;
      Eio.Promise.resolve watcher.stop ()
  | None -> ()

let discard_pending eng =
  eng.pending_sockets <- [];
  eng.pending_timer <- None

(* Fail the engine with [ex] and re-raise it. The recorded intents are
   dropped rather than applied: a failed native multi operation may have
   left ownership uncertain, so nothing may call back into the multi
   while unwinding it. *)
let poison eng ex bt =
  discard_pending eng;
  fail_engine eng ex bt;
  (try process_wakes eng with _ -> ());
  Printexc.raise_with_backtrace ex bt

(* Effects cannot cross libcurl callbacks, so dispatch their recorded work
   only after the C call returns. *)
let rec curl_call : 'a. engine -> (unit -> 'a) -> 'a =
 fun eng f ->
  match f () with
  | exception ex -> poison eng ex (Printexc.get_raw_backtrace ())
  | value -> (
      try
        if eng.failure <> None then begin
          discard_pending eng;
          process_wakes eng
        end
        else begin
          enforce_close_intents eng;
          check_completions eng;
          process_pending eng;
          process_wakes eng
        end;
        value
      with ex -> poison eng ex (Printexc.get_raw_backtrace ()))

and process_pending eng =
  let sockets = List.rev eng.pending_sockets in
  let timer = eng.pending_timer in
  discard_pending eng;
  List.iter (fun (fd, poll) -> apply_socket eng fd poll) sockets;
  Option.iter (apply_timer eng) timer;
  if eng.pending_sockets <> [] || eng.pending_timer <> None then
    process_pending eng

and apply_socket eng fd (poll : Curl.Multi.poll) =
  stop_watcher eng fd;
  match poll with
  | Curl.Multi.POLL_NONE | Curl.Multi.POLL_REMOVE -> ()
  | Curl.Multi.POLL_IN -> start_watcher eng fd `In
  | Curl.Multi.POLL_OUT -> start_watcher eng fd `Out
  | Curl.Multi.POLL_INOUT -> start_watcher eng fd `Both

and start_watcher eng fd dirs =
  let stop_p, stop_u = Eio.Promise.create () in
  let watcher = { stop = stop_u } in
  Hashtbl.replace eng.watchers fd watcher;
  (* Daemon: engine helpers must not keep the switch alive once the
     application's own fibers are done. *)
  Eio.Fiber.fork_daemon ~sw:eng.sw (fun () ->
      (try
         Eio.Fiber.first
           (fun () -> Eio.Promise.await stop_p)
           (fun () ->
             match dirs with
             | `In -> watch_fd eng fd Curl.Multi.EV_IN Eio_unix.await_readable
             | `Out -> watch_fd eng fd Curl.Multi.EV_OUT Eio_unix.await_writable
             | `Both ->
                 Eio.Fiber.both
                   (fun () ->
                     watch_fd eng fd Curl.Multi.EV_IN Eio_unix.await_readable)
                   (fun () ->
                     watch_fd eng fd Curl.Multi.EV_OUT Eio_unix.await_writable))
       with
      | Eio.Cancel.Cancelled _ as ex -> raise ex
      | ex ->
          let bt = Printexc.get_raw_backtrace () in
          (* A superseded watcher can observe its old descriptor closing.
            A current watcher failing would otherwise strand every transfer
            indefinitely, so poison the engine and wake its callers. *)
          let current =
            match Hashtbl.find_opt eng.watchers fd with
            | Some active -> active == watcher
            | None -> false
          in
          if current && engine_open eng then begin
            Hashtbl.remove eng.watchers fd;
            fail_engine eng ex bt;
            Eio.Private.Trace.log
              (Fmt.str "fetch-curl: fd watcher failed: %s"
                 (Printexc.to_string ex))
          end);
      `Stop_daemon)

and watch_fd eng fd status await =
  await fd;
  if engine_live eng then begin
    ignore (curl_call eng (fun () -> Curl.Multi.action eng.mt fd status) : int);
    watch_fd eng fd status await
  end

and apply_timer eng ms =
  (* Superseding a timer also wakes the previous sleeper (via the
     generation check it exits without acting), so a long libcurl
     timeout does not hold a fiber for its full original duration. *)
  eng.timer_gen <- eng.timer_gen + 1;
  let gen = eng.timer_gen in
  Option.iter
    (fun u ->
      eng.timer_stop <- None;
      Eio.Promise.resolve u ())
    eng.timer_stop;
  if ms >= 0 then begin
    let stop_p, stop_u = Eio.Promise.create () in
    eng.timer_stop <- Some stop_u;
    Eio.Fiber.fork_daemon ~sw:eng.sw (fun () ->
        (try
           Eio.Fiber.first
             (fun () -> Eio.Promise.await stop_p)
             (fun () ->
               if ms > 0 then Eio_unix.sleep (float_of_int ms /. 1000.)
               else Eio.Fiber.yield ();
               if gen = eng.timer_gen && engine_live eng then
                 curl_call eng (fun () -> Curl.Multi.action_timeout eng.mt))
         with
        | Eio.Cancel.Cancelled _ as ex -> raise ex
        | ex ->
            let bt = Printexc.get_raw_backtrace () in
            if gen = eng.timer_gen && engine_open eng then begin
              fail_engine eng ex bt;
              Eio.Private.Trace.log
                (Fmt.str "fetch-curl: timer watcher failed: %s"
                   (Printexc.to_string ex))
            end);
        `Stop_daemon)
  end

(* Free [job]'s handle. Idempotent. Called when the body is drained,
   when the request fails or is cancelled, and as a backstop from the
   request switch's release hook if the body was never read.

   The handle must be detached from the multi before [Curl.cleanup], so
   the three states are distinguished: never added (free it directly);
   finished, hence already detached by [remove_finished] (free it); still
   attached (remove first, and if the remove fails retain the handle until
   multi cleanup makes it safe to free). *)
let cleanup_job eng job =
  if not job.cleaned then begin
    job.cleaned <- true;
    Eio.Switch.remove_hook job.hook;
    Option.iter (fun (up : upload) -> notify up.stop_u) job.upload;
    (match (job.added, job.finished) with
    | false, _ | true, Some _ ->
        Hashtbl.remove eng.jobs job.id;
        job.added <- false;
        cleanup_handle job
    | true, None ->
        if eng.closed then
          Eio.Private.Trace.log
            "fetch-curl: leaking a handle still attached at engine shutdown"
        else
          begin match Curl.Multi.remove eng.mt job.h with
          | () -> (
              Hashtbl.remove eng.jobs job.id;
              job.added <- false;
              cleanup_handle job;
              if eng.shutting_down || eng.failure <> None then
                discard_pending eng
              else
                match
                  process_pending eng;
                  process_wakes eng
                with
                | () -> ()
                | exception ex ->
                    let bt = Printexc.get_raw_backtrace () in
                    discard_pending eng;
                    fail_engine eng ex bt)
          | exception ex ->
              let bt = Printexc.get_raw_backtrace () in
              discard_pending eng;
              fail_engine eng ex bt;
              Eio.Private.Trace.log
                (Fmt.str "fetch-curl: could not detach an easy handle: %s"
                   (Printexc.to_string ex))
          (* On failure, keep the cleaned job in [eng.jobs]. The poisoned
            engine performs no more socket actions, and shutdown frees it
            after [Multi.cleanup] detaches everything. *)
          end);
    wake job
  end

let high_water = 256 * 1024
let low_water = high_water / 2

(* [Curl.pause] sets the state of both directions at once, so a resume
   must name the direction that stays paused: unpausing the sender must
   not also resume a receiver whose reader has fallen behind. *)
let sync_pause eng job =
  curl_call eng (fun () -> Curl.pause job.h (pause_flags job))

let maybe_unpause eng job =
  if job.paused && job_active eng job && job.queued <= low_water then begin
    job.paused <- false;
    sync_pause eng job
  end

(* Libcurl requires an unpause to run in the engine's domain. *)
let resume_send eng job (up : upload) =
  if up.paused && job_active eng job then begin
    up.paused <- false;
    sync_pause eng job
  end

let upload_chunk = 64 * 1024

let upload_short declared produced =
  err
    (Invalid_request
       (Fmt.str "request body ended %d bytes short of the declared length of %d"
          (declared - produced) declared))

(* Read the request body flow into [up] until it ends, the limit is
   passed or the transfer is over, resuming the send whenever data
   lands. Stopping leaves [eof] or [error] set, which is what tells the
   read callback to finish or abort. *)
let pump eng job (up : upload) flow =
  let cfg = eng.cfg in
  let buf = Cstruct.create upload_chunk in
  let probe = Cstruct.create 1 in
  let limit = Option.value up.expected ~default:cfg.max_request in
  let stopped () =
    job.cleaned || job.finished <> None || up.error <> None
    || eng.failure <> None
  in
  let check_end () =
    match up.expected with
    | Some _ -> up.eof <- true
    | None -> (
        match Eio.Flow.single_read flow probe with
        | _ -> up.error <- Some (too_large cfg)
        | exception End_of_file -> up.eof <- true)
  in
  let rec loop () =
    if stopped () then ()
    else if up.queued >= high_water then begin
      wait_for up.wake (fun () -> up.queued < high_water || stopped ());
      loop ()
    end
    else begin
      (if up.read = limit then check_end ()
       else
         let room = min (Cstruct.length buf) (limit - up.read) in
         match Eio.Flow.single_read flow (Cstruct.sub_local buf 0 room) with
         | n ->
             up.read <- up.read + n;
             Queue.add (Cstruct.to_string ~len:n buf) up.chunks;
             up.queued <- up.queued + n;
             if up.read = limit then check_end ()
         | exception End_of_file -> (
             match up.expected with
             | Some declared -> up.error <- Some (upload_short declared up.read)
             | None -> up.eof <- true)
         | exception (Eio.Cancel.Cancelled _ as ex) -> raise ex
         | exception ex -> up.error <- Some ex);
      resume_send eng job up;
      if not up.eof then loop ()
    end
  in
  loop ()

let upload_error job =
  match job.upload with Some up -> up.error | None -> None

(* The exception a failed transfer reports. A failed upload is the cause
   of the abort that follows it, so the caller sees that error rather than
   the curl code. [detail] is libcurl's error buffer text, which only the
   body path attaches: a pre-response failure is unambiguous without it,
   and the text embeds timings ("after 3 ms") that would make error
   output nondeterministic. *)
let transfer_failure eng job ?(headers_capped = false) ~detail code =
  match upload_error job with
  | Some ex -> ex
  | None -> (
      match job.protocol_error with
      | Some reason -> err (Protocol_error reason)
      | None when job.over_limit ->
          err
            (Protocol_error
               (Fmt.str "response body exceeds %d bytes" eng.cfg.max_response))
      | None when headers_capped ->
          err
            (Protocol_error
               (Fmt.str "response headers exceed %d bytes" max_header_bytes))
      | None -> map_curl_error code detail)

type stream_body = { eng : engine; job : job; mutable reading : bool }

module Body_stream = struct
  type t = stream_body

  let read_methods = []

  let rec read t (buf @ local) =
    let job = t.job in
    if job.body_abandoned then raise End_of_file;
    if t.eng.failure <> None then begin
      cleanup_job t.eng job;
      raise_engine_failure t.eng
    end;
    if not (Queue.is_empty job.chunks) then begin
      let chunk = Queue.peek job.chunks in
      let avail = String.length chunk - job.head_off in
      let n = min avail (Cstruct.length buf) in
      Cstruct.blit_from_string chunk job.head_off buf 0 n;
      if n = avail then (
        ignore (Queue.pop job.chunks : string);
        job.head_off <- 0)
      else job.head_off <- job.head_off + n;
      job.queued <- job.queued - n;
      maybe_unpause t.eng job;
      n
    end
    else
      match job.finished with
      | Some Curl.CURLE_OK when job.protocol_error = None ->
          cleanup_job t.eng job;
          raise End_of_file
      | Some code ->
          cleanup_job t.eng job;
          raise (transfer_failure t.eng job ~detail:!(job.errbuf) code)
      | None ->
          if job.cleaned then
            raise (err (Protocol_error "response body is no longer available"))
          else begin
            wait_for job.wake (fun () ->
                (not (Queue.is_empty job.chunks))
                || job.finished <> None || job.cleaned || t.eng.failure <> None);
            read t buf
          end

  let single_read t (buf @ local) =
    (* [maybe_unpause] and [cleanup_job] call into the shared multi;
       the single-domain rule for {!Backend.request} applies here too. *)
    if Domain.self () <> t.eng.dom then
      invalid_arg
        "Fetch_curl: response body used from a domain other than the client's";
    if Cstruct.length buf = 0 then
      invalid_arg "Fetch_curl: response read buffer is empty";
    if t.reading then
      invalid_arg "Fetch_curl: concurrent reads from one response body";
    t.reading <- true;
    match read t buf with
    | n -> t.reading <- false; n
    | exception (Eio.Cancel.Cancelled _ as ex) ->
        let bt = Printexc.get_raw_backtrace () in
        t.reading <- false;
        cleanup_job t.eng t.job;
        Printexc.raise_with_backtrace ex bt
    | exception ex ->
        let bt = Printexc.get_raw_backtrace () in
        t.reading <- false;
        Printexc.raise_with_backtrace ex bt
end

let body_stream_handler = Eio.Flow.Pi.source (module Body_stream)

(* Count the representation after content decoding as well as the encoded
   bytes accepted by the native callback.  On a decoder failure, release the
   easy handle immediately instead of retaining it until the request switch
   happens to end. *)
type checked_body = {
  src : Eio.Flow.source_ty Eio.Resource.t;
  eng : engine;
  job : job;
  mutable seen : int;
  mutable complete : bool;
  mutable reading : bool;
}

module Checked_body = struct
  type t = checked_body

  let read_methods = []

  let read t (buf @ local) =
    match Eio.Flow.single_read t.src buf with
    | n ->
        if n > t.eng.cfg.max_response - t.seen then begin
          cleanup_job t.eng t.job;
          raise
            (err
               (Protocol_error
                  (Fmt.str "response body exceeds %d bytes"
                     t.eng.cfg.max_response)))
        end;
        t.seen <- t.seen + n;
        n
    | exception End_of_file ->
        t.complete <- true;
        cleanup_job t.eng t.job;
        raise End_of_file
    | exception ex ->
        let bt = Printexc.get_raw_backtrace () in
        cleanup_job t.eng t.job;
        Printexc.raise_with_backtrace ex bt

  let single_read t (buf @ local) =
    if t.reading then
      invalid_arg "Fetch_curl: concurrent reads from one response body";
    t.reading <- true;
    match read t buf with
    | n -> t.reading <- false; n
    | exception ex ->
        let bt = Printexc.get_raw_backtrace () in
        t.reading <- false;
        Printexc.raise_with_backtrace ex bt
end

let checked_body_handler = Eio.Flow.Pi.source (module Checked_body)

let checked_body eng job src =
  let state =
    { src; eng; job; seen = 0; complete = false; reading = false }
  in
  ( Eio.Resource.T (state, checked_body_handler),
    fun () -> state.complete )

let create_job eng h body =
  let id = fresh_job_id eng in
  Curl.set_private h id;
  let upload =
    match body with
    | No_body | Fixed _ -> None
    | Streamed length ->
        let stop, stop_u = Eio.Promise.create () in
        Some
          {
            chunks = Queue.create ();
            wake = { u = None };
            stop;
            stop_u = { u = Some stop_u };
            expected = Option.map Int64.to_int length;
            head_off = 0;
            queued = 0;
            read = 0;
            eof = false;
            error = None;
            paused = false;
          }
  in
  {
    id;
    h;
    chunks = Queue.create ();
    errbuf = ref "";
    upload;
    wake = { u = None };
    head_off = 0;
    queued = 0;
    wire_received = 0;
    received = 0;
    paused = false;
    over_limit = false;
    protocol_error = None;
    chunked = None;
    close_enforced = false;
    force_close = false;
    discard_remainder = false;
    skip_redelivery = false;
    added = false;
    body_started = false;
    body_abandoned = false;
    finished = None;
    cleaned = false;
    handle_cleaned = false;
    pending_wake = false;
    hook = Eio.Switch.null_hook;
  }

module Backend = struct
  type t = engine
  type tag = [ `Generic | `Curl ]

  let closed_error () =
    err (Protocol_error "Fetch_curl: client switch already finished")

  let request eng ~sw (req : Middleware.request) =
    if Domain.self () <> eng.dom then
      invalid_arg
        "Fetch_curl: client used from a domain other than its creator's";
    if eng.closed then raise (closed_error ());
    raise_engine_failure eng;
    let cfg = eng.cfg in
    (* Nothing suspends between here and [Multi.add] below, so the
       engine cannot be torn down under us in between. *)
    let body = wire_body_of cfg req in
    let h = Curl.init () in
    let job =
      match create_job eng h body with
      | job -> job
      | exception ex ->
          let bt = Printexc.get_raw_backtrace () in
          (try Curl.cleanup h with _ -> ());
          Printexc.raise_with_backtrace ex bt
    in
    try
      let quarantine ~discard =
        job.force_close <- true;
        if discard then begin
          job.discard_remainder <- true;
          (* Returning [Pause] tells libcurl that this complete callback
             buffer was not consumed. The first discard delivery is that same
             buffer, not more wire bytes. *)
          job.skip_redelivery <- true
        end
      in
      let col, auto_decode =
        setup_handle cfg h req body ~on_error:(fun reason ->
            job.protocol_error <- Some reason)
          ~on_close:quarantine
          ~on_final_head:(fun () -> job.pending_wake <- true)
          ~on_http2_trailer:(fun n ->
            if n > cfg.max_response - job.wire_received then begin
              job.over_limit <- true;
              false
            end
            else begin
              job.wire_received <- job.wire_received + n;
              true
            end)
      in
      Curl.set_errorbuffer h job.errbuf;
      let exception Body_limit in
      let enqueue s =
        let n = String.length s in
        if col.status = 205 && n <> 0 then
          (* RFC 9110 gives 205 response content no semantics, but RFC 9112
             still requires us to frame bytes sent by a broken peer. Consume
             and suppress them, and keep the connection out of the pool. *)
          quarantine ~discard:false
        else if n > cfg.max_response - job.received then begin
          job.over_limit <- true;
          raise_notrace Body_limit
        end
        else begin
          Queue.add s job.chunks;
          job.received <- job.received + n;
          job.queued <- job.queued + n
        end
      in
      let count_wire s =
        let n = String.length s in
        if n > cfg.max_response - job.wire_received then begin
          job.over_limit <- true;
          false
        end
        else begin
          job.wire_received <- job.wire_received + n;
          true
        end
      in
      Curl.set_writefunction2 h (fun s ->
          (* C callback frame: record and signal only. Refusing the data
             ([Pause]) makes libcurl keep it and deliver it again on
             unpause, so a paused job queues nothing further. *)
          if s = "" then Curl.proceed
          else begin
            job.body_started <- true;
            col.in_body <- true;
            job.pending_wake <- true;
            if job.cleaned || eng.failure <> None || job.protocol_error <> None
            then Curl.Abort
            else if col.connection_close && not job.close_enforced then begin
              (* Setting CURLOPT_FORBID_REUSE from inside a native callback is
                 not safe. Pause before any body byte can complete the
                 transfer; ordinary fiber context marks it below and resumes. *)
              job.paused <- true;
              Curl.Pause
            end
            else if (not job.discard_remainder) && job.queued >= high_water then (
              job.paused <- true;
              Curl.Pause)
            else if
              job.discard_remainder && job.skip_redelivery
            then begin
              job.skip_redelivery <- false;
              Curl.proceed
            end
            else if not (count_wire s) then Curl.Abort
            else if job.discard_remainder then Curl.proceed
            else begin
              (try
                 if col.chunked then begin
                   let decoder =
                     match job.chunked with
                     | Some decoder -> decoder
                     | None ->
                         let decoder = Chunked.create () in
                         job.chunked <- Some decoder;
                         decoder
                   in
                   match Chunked.feed decoder s enqueue with
                   | `Ok -> Curl.proceed
                   | `Trailing ->
                       (* The response itself is complete. Quarantine the
                          connection in fiber context, then accept the
                          redelivered callback without treating its suffix as
                          another response body. *)
                       quarantine ~discard:true;
                       job.paused <- true;
                       Curl.Pause
                   | `Error reason ->
                       job.protocol_error <- Some reason;
                       Curl.Abort
                 end
                 else begin
                   enqueue s;
                   Curl.proceed
                 end
               with Body_limit -> Curl.Abort)
            end
          end);
      (match (req.body, job.upload) with
      | Stream { flow; _ }, Some up ->
          Curl.set_readfunction2 h (fun n ->
              (* C callback frame: record and signal only. [Pause] leaves
                the send stopped until the pump has more to give. *)
              if
                n <= 0 || job.cleaned || eng.failure <> None || up.error <> None
              then Curl.Abort
              else if up.queued > 0 then begin
                let chunk = Queue.peek up.chunks in
                let avail = String.length chunk - up.head_off in
                let n = min avail n in
                let s = String.sub chunk up.head_off n in
                if n = avail then (
                  ignore (Queue.pop up.chunks : string);
                  up.head_off <- 0)
                else up.head_off <- up.head_off + n;
                up.queued <- up.queued - n;
                job.pending_wake <- true;
                Curl.Proceed s
              end
              else if up.eof then Curl.Proceed ""
              else (
                up.paused <- true;
                Curl.Pause))
      | _ -> ());
      job.hook <-
        Eio.Switch.on_release_cancellable sw (fun () -> cleanup_job eng job);
      Hashtbl.replace eng.jobs job.id job;
      curl_call eng (fun () ->
          Curl.Multi.add eng.mt h;
          job.added <- true);
      (match (req.body, job.upload) with
      | Stream { flow; _ }, Some up ->
          (* A daemon because it must not keep the request switch open. Job
             cleanup resolves [stop], which also cancels a pump blocked in an
             arbitrary source flow. *)
          Eio.Fiber.fork_daemon ~sw (fun () ->
              ignore
                (Eio.Fiber.first
                   (fun () -> pump eng job up flow)
                   (fun () -> Eio.Promise.await up.stop));
              `Stop_daemon)
      | _ -> ());
      curl_call eng (fun () -> Curl.Multi.action_timeout eng.mt);
      (* The response is ready at the final completed head. The callback only
         records a pending wake; [curl_call] resolves it after returning from C.
         [cleaned] also ends the wait — the switch may be released while
         we sit here, and its backstop wake must not be lost. *)
      wait_for job.wake (fun () ->
          col.head_complete || job.finished <> None || job.cleaned
          || eng.failure <> None);
      raise_engine_failure eng;
      if job.cleaned then
        (* The handle is already freed; touching [h] below would be a
           use-after-free in C. *)
        raise (err (Protocol_error "request abandoned: switch released"));
      (match job.finished with
      | Some code
        when col.abort_after_head
             && job.protocol_error = None
             && Curl.errno code = Curl.errno Curl.CURLE_WRITE_ERROR ->
          (* [header_callback] deliberately aborted after the complete head to
             prevent a zero-body close-marked connection entering the pool. *)
          job.finished <- Some Curl.CURLE_OK
      | _ -> ());
      (match job.finished with
      | Some code when code <> Curl.CURLE_OK || job.protocol_error <> None ->
          raise
            (transfer_failure eng job ~headers_capped:col.headers_capped
               ~detail:"" code)
      | _ -> ());
      let status = Curl.get_responsecode h in
      if not col.head_complete then
        raise (err (Protocol_error "response head is not terminated by CRLF"));
      if status = 101 then
        raise
          (err
             (Protocol_error
                "server switched protocols, which this backend did not request"));
      let headers = Http.Header.of_list (List.rev col.lines) in
      let version = version_of_status_line col.status_line in
      if version = `HTTP_1_0 && Http.Header.mem headers "transfer-encoding" then
        raise
          (err
             (Protocol_error
                "HTTP/1.0 response contains Transfer-Encoding"));
      let framing_bodyless = framing_bodyless req.meth status in
      let contentless = contentless req.meth status in
      let headers =
        if framing_bodyless then headers
        else Http.Header.remove headers "transfer-encoding"
      in
      let gzip =
        auto_decode && (not contentless)
        &&
        match Http.Header.get_multi headers "content-encoding" with
        | [ value ] ->
            let value = String.lowercase_ascii (String.trim value) in
            String.equal value "gzip" || String.equal value "x-gzip"
        | _ -> false
      in
      let headers =
        if gzip then
          Http.Header.remove
            (Http.Header.remove headers "content-encoding")
            "content-length"
        else headers
      in
      (* A semantic no-content response never exposes queued representation
         bytes. Otherwise a completed transfer can release its handle now;
         queued chunks outlive it. *)
      if contentless || job.finished <> None then cleanup_job eng job;
      let raw_body =
        if contentless then Eio.Flow.string_source ""
        else Eio.Resource.T ({ eng; job; reading = false }, body_stream_handler)
      in
      let decoded_body =
        if gzip then Gzip_stream.gunzip raw_body else raw_body
      in
      let body, body_complete = checked_body eng job decoded_body in
      let trailers () =
        if Domain.self () <> eng.dom then
          invalid_arg
            "Fetch_curl: response trailers used from a domain other than the \
             client's";
        (* Per the interface, [Some] only once the body has been fully
           consumed: the transfer succeeded *and* the reader has drained
           the queue. (For a small response the transfer can finish
           before the caller reads a byte, so [finished] alone would
           answer too early.) *)
        if not (body_complete ()) then None
        else
          match (job.finished, job.chunked) with
          | Some Curl.CURLE_OK, Some decoder -> decoder.Chunked.trailers
          | Some Curl.CURLE_OK, None -> begin
              let lines =
                List.filter
                  (fun (name, _) ->
                    not (Httpz.Chunk.is_forbidden_trailer_name name))
                  col.trailer_lines
              in
              match lines with
              | [] -> None
              | lines -> Some (Http.Header.of_list (List.rev lines))
            end
          | _ -> None
      in
      let close () =
        if not job.body_abandoned then begin
          job.body_abandoned <- true;
          Queue.clear job.chunks;
          job.queued <- 0;
          cleanup_job eng job
        end
      in
      Fetch.Middleware.Pi.response ~status ~headers ~version ~trailers
        ~close ~body ~url:req.url ()
    with ex ->
      let bt = Printexc.get_raw_backtrace () in
      cleanup_job eng job;
      Printexc.raise_with_backtrace ex bt
end

let handler = Fetch.Middleware.Pi.client (module Backend)

let shutdown eng =
  if Domain.self () <> eng.dom then
    invalid_arg
      "Fetch_curl: client switch released from a domain other than its creator's";
  if not eng.closed then begin
    eng.shutting_down <- true;
    let first_error = ref None in
    let protect f =
      match f () with
      | () -> ()
      | exception ex ->
          if !first_error = None then
            first_error := Some (ex, Printexc.get_raw_backtrace ())
    in
    let outstanding = Hashtbl.fold (fun _ job acc -> job :: acc) eng.jobs [] in
    List.iter (fun job -> protect (fun () -> cleanup_job eng job)) outstanding;
    eng.timer_gen <- eng.timer_gen + 1;
    Option.iter
      (fun stop ->
        eng.timer_stop <- None;
        protect (fun () -> Eio.Promise.resolve stop ()))
      eng.timer_stop;
    let watched = Hashtbl.fold (fun fd _ acc -> fd :: acc) eng.watchers [] in
    List.iter (fun fd -> protect (fun () -> stop_watcher eng fd)) watched;
    (* No callback or late body operation may enter a freed [CURLM]. *)
    eng.closed <- true;
    protect (fun () -> Curl.Multi.cleanup eng.mt);
    (* A failed [Multi.remove] leaves ownership uncertain until the multi is
       gone. It is now detached and safe to release. *)
    let stranded = Hashtbl.fold (fun _ job acc -> job :: acc) eng.jobs [] in
    List.iter (fun job -> protect (fun () -> cleanup_handle job)) stranded;
    Hashtbl.clear eng.jobs;
    discard_pending eng;
    match !first_error with
    | None -> ()
    | Some (ex, bt) -> Printexc.raise_with_backtrace ex bt
  end

let v ~sw ?(tls_verify = true) ?(http_version = `Auto) ?proxy ?timeout
    ?(connect_timeout = 30.) ?(max_response = 256 * 1024 * 1024)
    ?(max_request = 256 * 1024 * 1024) ?(user_agent = "fetch-curl")
    ?(verbose = false) ?(resolve = []) ?max_connections_per_host
    ?max_total_connections ?(multiplex = true) () : t =
  Option.iter
    (non_negative_c_long "max_connections_per_host")
    max_connections_per_host;
  Option.iter
    (non_negative_c_long "max_total_connections")
    max_total_connections;
  let cfg =
    config ~tls_verify ~http_version ~proxy ~timeout ~connect_timeout
      ~max_response ~max_request ~user_agent ~verbose ~resolve
  in
  global_init ();
  let jobs = Hashtbl.create 8 in
  let watchers = Hashtbl.create 8 in
  let mt = Curl.Multi.create () in
  let eng =
    {
      cfg;
      mt;
      sw;
      dom = Domain.self ();
      jobs;
      shutting_down = false;
      closed = false;
      failure = None;
      watchers;
      pending_sockets = [];
      pending_timer = None;
      timer_gen = 0;
      timer_stop = None;
      next_id = 0;
    }
  in
  match
    if multiplex then
      Curl.Multi.setopt mt
        (Curl.Multi.CURLMOPT_PIPELINING [ Curl.Multi.PIPE_MULTIPLEX ]);
    Option.iter
      (fun n ->
        Curl.Multi.setopt mt (Curl.Multi.CURLMOPT_MAX_HOST_CONNECTIONS n))
      max_connections_per_host;
    Option.iter
      (fun n ->
        Curl.Multi.setopt mt (Curl.Multi.CURLMOPT_MAX_TOTAL_CONNECTIONS n))
      max_total_connections;
    Curl.Multi.set_socket_function mt (socket_function eng);
    Curl.Multi.set_timer_function mt (timer_function eng);
    Eio.Switch.on_release sw (fun () -> shutdown eng)
  with
  | () -> Eio.Resource.T (eng, handler)
  | exception ex ->
      let bt = Printexc.get_raw_backtrace () in
      (try shutdown eng with _ -> ());
      Printexc.raise_with_backtrace ex bt

let std ~sw ?cookies ?retry ?max_concurrent ?min_interval ?resolve env =
  Fetch_cookies.std ?cookies ?retry ?max_concurrent ?min_interval env
    (v ~sw ?resolve ())
