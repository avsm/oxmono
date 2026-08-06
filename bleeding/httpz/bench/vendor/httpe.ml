(* Httpe - Efficient HTTP/1.1 parser and serializer for Eio

   Uses Eio Buf_read/Buf_write as the sole mechanism for I/O.
   Designed for efficiency with zero-copy parsing where possible.
*)

(* Efficient case-insensitive string comparison using word-at-a-time comparison.
   Adapted from cohttp's implementation. *)
module Caseless = struct
  external string_unsafe_get64 : string -> int -> int64 = "%caml_string_get64u"

  let equal a b =
    if a == b
    then true
    else (
      let len = String.length a in
      len = String.length b
      &&
      let rec word_loop i =
        if i = len
        then true
        else (
          let i' = i + 8 in
          if i' > len
          then byte_loop i len
          else if string_unsafe_get64 a i = string_unsafe_get64 b i
          then word_loop i'
          else byte_loop i i' && word_loop i')
      and byte_loop i limit =
        if i = limit
        then true
        else (
          let c1 = String.unsafe_get a i in
          let c2 = String.unsafe_get b i in
          Char.lowercase_ascii c1 = Char.lowercase_ascii c2 && byte_loop (i + 1) limit)
      in
      word_loop 0)
  ;;
end

module Version = struct
  type t =
    [ `HTTP_1_0
    | `HTTP_1_1
    | `Other of string
    ]

  let to_string = function
    | `HTTP_1_0 -> "HTTP/1.0"
    | `HTTP_1_1 -> "HTTP/1.1"
    | `Other s -> s
  ;;

  let of_string = function
    | "HTTP/1.0" -> `HTTP_1_0
    | "HTTP/1.1" -> `HTTP_1_1
    | s -> `Other s
  ;;

  let pp fmt t = Format.pp_print_string fmt (to_string t)
end

module Method = struct
  type t =
    [ `GET
    | `POST
    | `HEAD
    | `DELETE
    | `PATCH
    | `PUT
    | `OPTIONS
    | `TRACE
    | `CONNECT
    | `Other of string
    ]

  let to_string : t -> string = function
    | `GET -> "GET"
    | `POST -> "POST"
    | `HEAD -> "HEAD"
    | `DELETE -> "DELETE"
    | `PATCH -> "PATCH"
    | `PUT -> "PUT"
    | `OPTIONS -> "OPTIONS"
    | `TRACE -> "TRACE"
    | `CONNECT -> "CONNECT"
    | `Other s -> s
  ;;

  let of_string : string -> t = function
    | "GET" -> `GET
    | "POST" -> `POST
    | "HEAD" -> `HEAD
    | "DELETE" -> `DELETE
    | "PATCH" -> `PATCH
    | "PUT" -> `PUT
    | "OPTIONS" -> `OPTIONS
    | "TRACE" -> `TRACE
    | "CONNECT" -> `CONNECT
    | s -> `Other s
  ;;

  let body_allowed = function
    | `GET | `HEAD | `CONNECT | `TRACE -> false
    | `DELETE | `POST | `PUT | `PATCH | `OPTIONS | `Other _ -> true
  ;;

  let pp fmt t = Format.pp_print_string fmt (to_string t)
end

module Status = struct
  type informational =
    [ `Continue
    | `Switching_protocols
    | `Processing
    ]

  type success =
    [ `OK
    | `Created
    | `Accepted
    | `Non_authoritative_information
    | `No_content
    | `Reset_content
    | `Partial_content
    | `Multi_status
    ]

  type redirection =
    [ `Multiple_choices
    | `Moved_permanently
    | `Found
    | `See_other
    | `Not_modified
    | `Use_proxy
    | `Temporary_redirect
    | `Permanent_redirect
    ]

  type client_error =
    [ `Bad_request
    | `Unauthorized
    | `Payment_required
    | `Forbidden
    | `Not_found
    | `Method_not_allowed
    | `Not_acceptable
    | `Proxy_authentication_required
    | `Request_timeout
    | `Conflict
    | `Gone
    | `Length_required
    | `Precondition_failed
    | `Request_entity_too_large
    | `Request_uri_too_long
    | `Unsupported_media_type
    | `Requested_range_not_satisfiable
    | `Expectation_failed
    | `I_m_a_teapot
    | `Unprocessable_entity
    | `Too_many_requests
    ]

  type server_error =
    [ `Internal_server_error
    | `Not_implemented
    | `Bad_gateway
    | `Service_unavailable
    | `Gateway_timeout
    | `Http_version_not_supported
    ]

  type standard =
    [ informational
    | success
    | redirection
    | client_error
    | server_error
    ]

  type t =
    [ `Code of int
    | standard
    ]

  let of_int : int -> t = function
    | 100 -> `Continue
    | 101 -> `Switching_protocols
    | 102 -> `Processing
    | 200 -> `OK
    | 201 -> `Created
    | 202 -> `Accepted
    | 203 -> `Non_authoritative_information
    | 204 -> `No_content
    | 205 -> `Reset_content
    | 206 -> `Partial_content
    | 207 -> `Multi_status
    | 300 -> `Multiple_choices
    | 301 -> `Moved_permanently
    | 302 -> `Found
    | 303 -> `See_other
    | 304 -> `Not_modified
    | 305 -> `Use_proxy
    | 307 -> `Temporary_redirect
    | 308 -> `Permanent_redirect
    | 400 -> `Bad_request
    | 401 -> `Unauthorized
    | 402 -> `Payment_required
    | 403 -> `Forbidden
    | 404 -> `Not_found
    | 405 -> `Method_not_allowed
    | 406 -> `Not_acceptable
    | 407 -> `Proxy_authentication_required
    | 408 -> `Request_timeout
    | 409 -> `Conflict
    | 410 -> `Gone
    | 411 -> `Length_required
    | 412 -> `Precondition_failed
    | 413 -> `Request_entity_too_large
    | 414 -> `Request_uri_too_long
    | 415 -> `Unsupported_media_type
    | 416 -> `Requested_range_not_satisfiable
    | 417 -> `Expectation_failed
    | 418 -> `I_m_a_teapot
    | 422 -> `Unprocessable_entity
    | 429 -> `Too_many_requests
    | 500 -> `Internal_server_error
    | 501 -> `Not_implemented
    | 502 -> `Bad_gateway
    | 503 -> `Service_unavailable
    | 504 -> `Gateway_timeout
    | 505 -> `Http_version_not_supported
    | code -> `Code code
  ;;

  let to_int : t -> int = function
    | `Continue -> 100
    | `Switching_protocols -> 101
    | `Processing -> 102
    | `OK -> 200
    | `Created -> 201
    | `Accepted -> 202
    | `Non_authoritative_information -> 203
    | `No_content -> 204
    | `Reset_content -> 205
    | `Partial_content -> 206
    | `Multi_status -> 207
    | `Multiple_choices -> 300
    | `Moved_permanently -> 301
    | `Found -> 302
    | `See_other -> 303
    | `Not_modified -> 304
    | `Use_proxy -> 305
    | `Temporary_redirect -> 307
    | `Permanent_redirect -> 308
    | `Bad_request -> 400
    | `Unauthorized -> 401
    | `Payment_required -> 402
    | `Forbidden -> 403
    | `Not_found -> 404
    | `Method_not_allowed -> 405
    | `Not_acceptable -> 406
    | `Proxy_authentication_required -> 407
    | `Request_timeout -> 408
    | `Conflict -> 409
    | `Gone -> 410
    | `Length_required -> 411
    | `Precondition_failed -> 412
    | `Request_entity_too_large -> 413
    | `Request_uri_too_long -> 414
    | `Unsupported_media_type -> 415
    | `Requested_range_not_satisfiable -> 416
    | `Expectation_failed -> 417
    | `I_m_a_teapot -> 418
    | `Unprocessable_entity -> 422
    | `Too_many_requests -> 429
    | `Internal_server_error -> 500
    | `Not_implemented -> 501
    | `Bad_gateway -> 502
    | `Service_unavailable -> 503
    | `Gateway_timeout -> 504
    | `Http_version_not_supported -> 505
    | `Code code -> code
  ;;

  let reason_phrase : t -> string = function
    | `Continue -> "Continue"
    | `Switching_protocols -> "Switching Protocols"
    | `Processing -> "Processing"
    | `OK -> "OK"
    | `Created -> "Created"
    | `Accepted -> "Accepted"
    | `Non_authoritative_information -> "Non-Authoritative Information"
    | `No_content -> "No Content"
    | `Reset_content -> "Reset Content"
    | `Partial_content -> "Partial Content"
    | `Multi_status -> "Multi-Status"
    | `Multiple_choices -> "Multiple Choices"
    | `Moved_permanently -> "Moved Permanently"
    | `Found -> "Found"
    | `See_other -> "See Other"
    | `Not_modified -> "Not Modified"
    | `Use_proxy -> "Use Proxy"
    | `Temporary_redirect -> "Temporary Redirect"
    | `Permanent_redirect -> "Permanent Redirect"
    | `Bad_request -> "Bad Request"
    | `Unauthorized -> "Unauthorized"
    | `Payment_required -> "Payment Required"
    | `Forbidden -> "Forbidden"
    | `Not_found -> "Not Found"
    | `Method_not_allowed -> "Method Not Allowed"
    | `Not_acceptable -> "Not Acceptable"
    | `Proxy_authentication_required -> "Proxy Authentication Required"
    | `Request_timeout -> "Request Timeout"
    | `Conflict -> "Conflict"
    | `Gone -> "Gone"
    | `Length_required -> "Length Required"
    | `Precondition_failed -> "Precondition Failed"
    | `Request_entity_too_large -> "Request Entity Too Large"
    | `Request_uri_too_long -> "Request-URI Too Long"
    | `Unsupported_media_type -> "Unsupported Media Type"
    | `Requested_range_not_satisfiable -> "Requested Range Not Satisfiable"
    | `Expectation_failed -> "Expectation Failed"
    | `I_m_a_teapot -> "I'm a teapot"
    | `Unprocessable_entity -> "Unprocessable Entity"
    | `Too_many_requests -> "Too Many Requests"
    | `Internal_server_error -> "Internal Server Error"
    | `Not_implemented -> "Not Implemented"
    | `Bad_gateway -> "Bad Gateway"
    | `Service_unavailable -> "Service Unavailable"
    | `Gateway_timeout -> "Gateway Timeout"
    | `Http_version_not_supported -> "HTTP Version Not Supported"
    | `Code code -> string_of_int code
  ;;

  let to_string t =
    let code = to_int t in
    string_of_int code ^ " " ^ reason_phrase t
  ;;

  let is_informational t =
    let c = to_int t in
    c >= 100 && c < 200
  ;;

  let is_success t =
    let c = to_int t in
    c >= 200 && c < 300
  ;;

  let is_redirection t =
    let c = to_int t in
    c >= 300 && c < 400
  ;;

  let is_client_error t =
    let c = to_int t in
    c >= 400 && c < 500
  ;;

  let is_server_error t =
    let c = to_int t in
    c >= 500 && c < 600
  ;;

  let pp fmt t = Format.pp_print_string fmt (to_string t)
end

module Headers = struct
  type t = (string * string) list

  let empty = []

  let is_empty = function
    | [] -> true
    | _ -> false
  ;;

  let mem h k =
    let rec loop = function
      | [] -> false
      | (k', _) :: rest -> Caseless.equal k k' || loop rest
    in
    loop h
  ;;

  let get h k =
    let rec loop = function
      | [] -> None
      | (k', v) :: rest -> if Caseless.equal k k' then Some v else loop rest
    in
    loop h
  ;;

  let get_exn h k =
    match get h k with
    | Some v -> v
    | None -> failwith ("Header not found: " ^ k)
  ;;

  let get_multi h k =
    let rec loop acc = function
      | [] -> List.rev acc
      | (k', v) :: rest ->
        if Caseless.equal k k' then loop (v :: acc) rest else loop acc rest
    in
    loop [] h
  ;;

  let add h k v = (k, v) :: h
  let add_unless_exists h k v = if mem h k then h else add h k v
  let add_list h l = List.fold_left (fun h (k, v) -> add h k v) h l
  let remove h k = List.filter (fun (k', _) -> not (Caseless.equal k k')) h

  let replace h k v =
    let rec loop seen = function
      | [] -> if seen then [] else [ k, v ]
      | (k', _) :: rest when Caseless.equal k k' ->
        if seen then loop true rest else (k, v) :: loop true rest
      | x :: rest -> x :: loop seen rest
    in
    loop false h
  ;;

  let fold f h init = List.fold_left (fun acc (k, v) -> f k v acc) init h
  let iter f h = List.iter (fun (k, v) -> f k v) h
  let of_list l = List.rev l
  let to_list h = List.rev h

  let pp fmt h =
    let pp_pair fmt (k, v) = Format.fprintf fmt "%s: %s" k v in
    Format.fprintf
      fmt
      "@[<v>%a@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_cut pp_pair)
      (to_list h)
  ;;

  (* Common header accessors *)
  let get_content_length h =
    match get h "content-length" with
    | Some s -> Int64.of_string_opt s
    | None -> None
  ;;

  let get_transfer_encoding h = get h "transfer-encoding"

  let is_chunked h =
    match get_transfer_encoding h with
    | Some te -> Caseless.equal te "chunked"
    | None -> false
  ;;

  let get_connection h = get h "connection"

  let is_close h =
    match get_connection h with
    | Some c -> Caseless.equal c "close"
    | None -> false
  ;;

  let is_keep_alive h =
    match get_connection h with
    | Some c -> Caseless.equal c "keep-alive"
    | None -> false
  ;;

  let get_host h = get h "host"
  let get_content_type h = get h "content-type"
  let get_expect h = get h "expect"

  let expects_100_continue h =
    match get_expect h with
    | Some e -> Caseless.equal e "100-continue"
    | None -> false
  ;;

  let supports_trailers h =
    get_multi h "te" |> List.exists (fun v -> Caseless.equal v "trailers")
  ;;
end

module Transfer = struct
  type encoding =
    | Chunked
    | Fixed of int64
    | Unknown

  let encoding_of_headers headers =
    if Headers.is_chunked headers
    then Chunked
    else (
      match Headers.get_content_length headers with
      | Some len -> Fixed len
      | None -> Unknown)
  ;;

  let has_body = function
    | Fixed 0L -> `No
    | Chunked | Fixed _ -> `Yes
    | Unknown -> `Unknown
  ;;
end

(* HTTP parse errors *)
exception Parse_error of string

let parse_error msg = raise (Parse_error msg)

(* Parsing helpers using Buf_read directly *)
module Parse = struct
  open Eio.Buf_read

  let is_tchar = function
    | '0' .. '9'
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '!'
    | '#'
    | '$'
    | '%'
    | '&'
    | '\''
    | '*'
    | '+'
    | '-'
    | '.'
    | '^'
    | '_'
    | '`'
    | '|'
    | '~' -> true
    | _ -> false
  ;;

  let is_space = function
    | ' ' | '\t' -> true
    | _ -> false
  ;;

  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false
  ;;

  let is_hex = function
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
    | _ -> false
  ;;

  let token r =
    let s = take_while1 is_tchar r in
    s
  ;;

  let sp r = char ' ' r
  let crlf r = string "\r\n" r
  let ows r = skip_while is_space r

  (* Parse a single header line *)
  let header r =
    let name = take_while1 is_tchar r in
    char ':' r;
    ows r;
    let value_line = line r in
    (* Trim trailing whitespace from value *)
    let value = String.trim value_line in
    name, value
  ;;

  (* Parse all headers until empty line *)
  let headers r =
    let rec loop acc =
      match peek_char r with
      | Some '\r' ->
        crlf r;
        Headers.of_list (List.rev acc)
      | Some _ ->
        let h = header r in
        loop (h :: acc)
      | None -> parse_error "Unexpected EOF while parsing headers"
    in
    loop []
  ;;

  (* Parse HTTP version *)
  let version r =
    string "HTTP/1." r;
    let minor = any_char r in
    match minor with
    | '0' -> `HTTP_1_0
    | '1' -> `HTTP_1_1
    | _ -> parse_error "Invalid HTTP version"
  ;;

  (* Parse request line: METHOD SP request-target SP HTTP-version CRLF *)
  let request_line r =
    let meth_str = token r in
    sp r;
    let target = take_while1 (fun c -> c <> ' ' && c <> '\r' && c <> '\n') r in
    sp r;
    let ver = version r in
    crlf r;
    Method.of_string meth_str, target, ver
  ;;

  (* Parse status line: HTTP-version SP status-code SP reason-phrase CRLF *)
  let status_line r =
    let ver = version r in
    sp r;
    let code_str = take_while1 is_digit r in
    let code = int_of_string code_str in
    sp r;
    let _reason = line r in
    (* We ignore the reason phrase, use our own *)
    ver, Status.of_int code
  ;;

  (* Parse chunk size line: chunk-size [chunk-ext] CRLF *)
  let chunk_size r =
    let hex_str = take_while1 is_hex r in
    (* Skip optional chunk extensions *)
    skip_while (fun c -> c <> '\r') r;
    crlf r;
    Int64.of_string ("0x" ^ hex_str)
  ;;

  (* Parse trailer headers (same format as regular headers) *)
  let trailers = headers
end

module Request = struct
  type t =
    { meth : Method.t
    ; target : string
    ; version : Version.t
    ; headers : Headers.t
    }

  let meth t = t.meth
  let target t = t.target
  let version t = t.version
  let headers t = t.headers

  let make ?(version = `HTTP_1_1) ?(headers = Headers.empty) meth target =
    { meth; target; version; headers }
  ;;

  let is_keep_alive t =
    match Headers.get_connection t.headers with
    | Some c when Caseless.equal c "close" -> false
    | Some c when Caseless.equal c "keep-alive" -> true
    | _ -> t.version = `HTTP_1_1
  ;;

  let encoding t =
    if Method.body_allowed t.meth
    then Transfer.encoding_of_headers t.headers
    else Transfer.Fixed 0L
  ;;

  let has_body t =
    if Method.body_allowed t.meth then Transfer.has_body (encoding t) else `No
  ;;

  let expects_100_continue t = Headers.expects_100_continue t.headers

  let pp fmt t =
    Format.fprintf
      fmt
      "@[<v>%a %s %a@,%a@]"
      Method.pp
      t.meth
      t.target
      Version.pp
      t.version
      Headers.pp
      t.headers
  ;;
end

module Response = struct
  type t =
    { version : Version.t
    ; status : Status.t
    ; headers : Headers.t
    }

  let version t = t.version
  let status t = t.status
  let headers t = t.headers

  let make ?(version = `HTTP_1_1) ?(headers = Headers.empty) status =
    { version; status; headers }
  ;;

  let is_keep_alive t =
    match Headers.get_connection t.headers with
    | Some c when Caseless.equal c "close" -> false
    | Some c when Caseless.equal c "keep-alive" -> true
    | _ -> t.version = `HTTP_1_1
  ;;

  let encoding t = Transfer.encoding_of_headers t.headers

  let has_body ?request_method t =
    let code = Status.to_int t.status in
    (* 1xx, 204, 304 have no body *)
    if code < 200 || code = 204 || code = 304
    then `No (* 2xx response to CONNECT has no body *)
    else (
      match request_method with
      | Some `CONNECT when code >= 200 && code < 300 -> `No
      | _ -> Transfer.has_body (encoding t))
  ;;

  let pp fmt t =
    Format.fprintf
      fmt
      "@[<v>%a %a@,%a@]"
      Version.pp
      t.version
      Status.pp
      t.status
      Headers.pp
      t.headers
  ;;
end

(* Body handling - streaming bodies as Eio flows *)
module Body = struct
  type read_body =
    { reader : Eio.Buf_read.t
    ; mutable remaining : [ `Chunked | `Fixed of int64 | `Close | `Empty ]
    ; mutable trailers : Headers.t option
    }

  (* Fixed-length body reader as an Eio flow *)
  module Fixed_reader = struct
    type t =
      { reader : Eio.Buf_read.t
      ; mutable remaining : int64
      }

    let single_read t dst =
      if t.remaining = 0L
      then raise End_of_file
      else (
        let available = Eio.Buf_read.buffered_bytes t.reader in
        if available = 0
        then (
          Eio.Buf_read.ensure t.reader 1;
          let available = Eio.Buf_read.buffered_bytes t.reader in
          let to_read =
            Int64.to_int
              (min t.remaining (Int64.of_int (min available (Cstruct.length dst))))
          in
          let data = Eio.Buf_read.peek t.reader in
          Cstruct.blit data 0 dst 0 to_read;
          Eio.Buf_read.consume t.reader to_read;
          t.remaining <- Int64.sub t.remaining (Int64.of_int to_read);
          to_read)
        else (
          let to_read =
            Int64.to_int
              (min t.remaining (Int64.of_int (min available (Cstruct.length dst))))
          in
          let data = Eio.Buf_read.peek t.reader in
          Cstruct.blit data 0 dst 0 to_read;
          Eio.Buf_read.consume t.reader to_read;
          t.remaining <- Int64.sub t.remaining (Int64.of_int to_read);
          to_read))
    ;;

    let read_methods = []
  end

  (* Chunked body reader as an Eio flow *)
  module Chunked_reader = struct
    type t =
      { reader : Eio.Buf_read.t
      ; mutable chunk_remaining : int64
      ; mutable done_ : bool
      ; mutable trailers : Headers.t option
      }

    let read_next_chunk_size t =
      if t.done_
      then 0L
      else (
        let size = Parse.chunk_size t.reader in
        if size = 0L
        then (
          (* Last chunk - read trailers *)
          let trailers = Parse.trailers t.reader in
          t.trailers <- (if Headers.is_empty trailers then None else Some trailers);
          t.done_ <- true);
        size)
    ;;

    let single_read t dst =
      if t.done_ && t.chunk_remaining = 0L
      then raise End_of_file
      else (
        (* Need more chunk data? *)
        if t.chunk_remaining = 0L
        then (
          t.chunk_remaining <- read_next_chunk_size t;
          if t.chunk_remaining = 0L then raise End_of_file);
        let available = Eio.Buf_read.buffered_bytes t.reader in
        if available = 0
        then (
          Eio.Buf_read.ensure t.reader 1;
          let available = Eio.Buf_read.buffered_bytes t.reader in
          let to_read =
            Int64.to_int
              (min t.chunk_remaining (Int64.of_int (min available (Cstruct.length dst))))
          in
          let data = Eio.Buf_read.peek t.reader in
          Cstruct.blit data 0 dst 0 to_read;
          Eio.Buf_read.consume t.reader to_read;
          t.chunk_remaining <- Int64.sub t.chunk_remaining (Int64.of_int to_read);
          (* If chunk is done, consume CRLF *)
          if t.chunk_remaining = 0L && not t.done_ then Parse.crlf t.reader;
          to_read)
        else (
          let to_read =
            Int64.to_int
              (min t.chunk_remaining (Int64.of_int (min available (Cstruct.length dst))))
          in
          let data = Eio.Buf_read.peek t.reader in
          Cstruct.blit data 0 dst 0 to_read;
          Eio.Buf_read.consume t.reader to_read;
          t.chunk_remaining <- Int64.sub t.chunk_remaining (Int64.of_int to_read);
          (* If chunk is done, consume CRLF *)
          if t.chunk_remaining = 0L && not t.done_ then Parse.crlf t.reader;
          to_read))
    ;;

    let read_methods = []
  end

  (* Read until connection close *)
  module Close_reader = struct
    type t = Eio.Buf_read.t

    let single_read t dst =
      let available = Eio.Buf_read.buffered_bytes t in
      if available = 0
      then (
        match Eio.Buf_read.ensure t 1 with
        | () ->
          let available = Eio.Buf_read.buffered_bytes t in
          let to_read = min available (Cstruct.length dst) in
          let data = Eio.Buf_read.peek t in
          Cstruct.blit data 0 dst 0 to_read;
          Eio.Buf_read.consume t to_read;
          to_read
        | exception End_of_file -> raise End_of_file)
      else (
        let to_read = min available (Cstruct.length dst) in
        let data = Eio.Buf_read.peek t in
        Cstruct.blit data 0 dst 0 to_read;
        Eio.Buf_read.consume t to_read;
        to_read)
    ;;

    let read_methods = []
  end

  type t =
    | Empty
    | Fixed of Fixed_reader.t
    | Chunked of Chunked_reader.t
    | Close of Close_reader.t

  let of_encoding reader encoding =
    match encoding with
    | Transfer.Fixed 0L -> Empty
    | Transfer.Fixed len -> Fixed { reader; remaining = len }
    | Transfer.Chunked ->
      Chunked { reader; chunk_remaining = 0L; done_ = false; trailers = None }
    | Transfer.Unknown -> Close reader
  ;;

  let single_read t dst =
    match t with
    | Empty -> raise End_of_file
    | Fixed f -> Fixed_reader.single_read f dst
    | Chunked c -> Chunked_reader.single_read c dst
    | Close r -> Close_reader.single_read r dst
  ;;

  let read_methods = []

  let as_flow t =
    let module F = struct
      type nonrec t = t

      let single_read = single_read
      let read_methods = read_methods
    end
    in
    let ops = Eio.Flow.Pi.source (module F) in
    Eio.Resource.T (t, ops)
  ;;

  (* Convenience: read entire body as string *)
  let to_string t =
    let buf = Buffer.create 4096 in
    let chunk = Cstruct.create 4096 in
    let rec loop () =
      match single_read t chunk with
      | n ->
        Buffer.add_string buf (Cstruct.to_string ~off:0 ~len:n chunk);
        loop ()
      | exception End_of_file -> Buffer.contents buf
    in
    loop ()
  ;;

  (* Drain body without reading *)
  let drain t =
    let chunk = Cstruct.create 4096 in
    let rec loop () =
      match single_read t chunk with
      | _ -> loop ()
      | exception End_of_file -> ()
    in
    loop ()
  ;;

  let trailers = function
    | Chunked c -> c.trailers
    | _ -> None
  ;;
end

(* Serialization using Buf_write *)
module Write = struct
  let crlf w = Eio.Buf_write.string w "\r\n"

  let request_line w req =
    Eio.Buf_write.string w (Method.to_string req.Request.meth);
    Eio.Buf_write.char w ' ';
    Eio.Buf_write.string w req.Request.target;
    Eio.Buf_write.char w ' ';
    Eio.Buf_write.string w (Version.to_string req.Request.version);
    crlf w
  ;;

  let status_line w resp =
    Eio.Buf_write.string w (Version.to_string resp.Response.version);
    Eio.Buf_write.char w ' ';
    Eio.Buf_write.string w (string_of_int (Status.to_int resp.Response.status));
    Eio.Buf_write.char w ' ';
    Eio.Buf_write.string w (Status.reason_phrase resp.Response.status);
    crlf w
  ;;

  let header w name value =
    Eio.Buf_write.string w name;
    Eio.Buf_write.string w ": ";
    Eio.Buf_write.string w value;
    crlf w
  ;;

  let headers w hdrs =
    Headers.iter (fun k v -> header w k v) hdrs;
    crlf w
  ;;

  let request w req =
    request_line w req;
    headers w req.Request.headers
  ;;

  let response w resp =
    status_line w resp;
    headers w resp.Response.headers
  ;;

  (* Write a fixed-length body *)
  let fixed_body w body = Eio.Buf_write.string w body

  (* Write chunked body from a flow *)
  let chunked_body w flow =
    let chunk = Cstruct.create 8192 in
    let rec loop () =
      match Eio.Flow.single_read flow chunk with
      | n ->
        (* Write chunk size in hex *)
        Eio.Buf_write.string w (Printf.sprintf "%x\r\n" n);
        Eio.Buf_write.cstruct w (Cstruct.sub chunk 0 n);
        crlf w;
        loop ()
      | exception End_of_file ->
        (* Final chunk *)
        Eio.Buf_write.string w "0\r\n\r\n"
    in
    loop ()
  ;;

  (* Write chunked body with trailers *)
  let chunked_body_with_trailers w flow trailers =
    let chunk = Cstruct.create 8192 in
    let rec loop () =
      match Eio.Flow.single_read flow chunk with
      | n ->
        Eio.Buf_write.string w (Printf.sprintf "%x\r\n" n);
        Eio.Buf_write.cstruct w (Cstruct.sub chunk 0 n);
        crlf w;
        loop ()
      | exception End_of_file ->
        (* Final chunk *)
        Eio.Buf_write.string w "0\r\n";
        headers w trailers
    in
    loop ()
  ;;

  (* Write a single chunk *)
  let chunk w data =
    let len = String.length data in
    if len > 0
    then (
      Eio.Buf_write.string w (Printf.sprintf "%x\r\n" len);
      Eio.Buf_write.string w data;
      crlf w)
  ;;

  let final_chunk w = Eio.Buf_write.string w "0\r\n\r\n"

  let final_chunk_with_trailers w trailers =
    Eio.Buf_write.string w "0\r\n";
    headers w trailers
  ;;

  (* 100 Continue response *)
  let continue w =
    Eio.Buf_write.string w "HTTP/1.1 100 Continue\r\n\r\n";
    Eio.Buf_write.flush w
  ;;
end

(* High-level read operations *)
module Read = struct
  let request reader =
    let meth, target, version = Parse.request_line reader in
    let headers = Parse.headers reader in
    { Request.meth; target; version; headers }
  ;;

  let response reader =
    let version, status = Parse.status_line reader in
    let headers = Parse.headers reader in
    { Response.version; status; headers }
  ;;

  let body reader encoding = Body.of_encoding reader encoding
end

(* Server-side helpers *)
module Server = struct
  type handler =
    Request.t
    -> Body.t
    -> Response.t
       * [ `String of string | `Flow of Eio.Flow.source_ty Eio.Resource.t | `Empty ]

  let handle_request reader writer handler =
    let req = Read.request reader in
    let body = Body.of_encoding reader (Request.encoding req) in
    (* Handle 100-continue *)
    if Request.expects_100_continue req then Write.continue writer;
    let resp, resp_body = handler req body in
    (* Drain request body if not consumed *)
    Body.drain body;
    (* Write response *)
    Write.response writer resp;
    (* Write response body *)
    (match resp_body with
     | `Empty -> ()
     | `String s -> Write.fixed_body writer s
     | `Flow flow -> Write.chunked_body writer flow);
    Eio.Buf_write.flush writer;
    (* Return whether to keep connection alive *)
    Request.is_keep_alive req && Response.is_keep_alive resp
  ;;

  let run reader writer handler =
    let rec loop () =
      match handle_request reader writer handler with
      | true -> loop ()
      | false -> ()
      | exception End_of_file -> ()
    in
    loop ()
  ;;
end

(* Client-side helpers *)
module Client = struct
  let request writer req body =
    Write.request writer req;
    (match body with
     | `Empty -> ()
     | `String s -> Write.fixed_body writer s
     | `Flow flow -> Write.chunked_body writer flow);
    Eio.Buf_write.flush writer
  ;;

  let response reader =
    let resp = Read.response reader in
    let body = Body.of_encoding reader (Response.encoding resp) in
    resp, body
  ;;
end
