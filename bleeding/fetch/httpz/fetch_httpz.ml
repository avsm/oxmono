open Fetch

type tag = [ `Generic | `Httpz ]

type t = tag Fetch.ty Eio.Resource.t

type conn = Httpz_tls.flow

type connect = sw:Eio.Switch.t -> host:string -> port:int -> conn

type https = Httpz_tls.client

let no_https _uri _connection =
  raise (err (Tls_failure "HTTPS disabled by client configuration"))

type Eio.Exn.Backend.t += Httpz_error of string

let () =
  Eio.Exn.Backend.register_pp (fun f -> function
      | Httpz_error msg -> Fmt.pf f "Httpz_error(%S)" msg; true
      | _ -> false
    )

let i16 = Httpz.Buf_read.i16
let to_int = Httpz.Buf_read.to_int

(* The response head must fit httpz's parse buffer, whose offsets are
   signed [int16#]: the window may hold at most 32767 bytes. The head
   itself is bounded a little lower so the two failure paths (parse
   sees the end of an oversized block; the window fills before the end
   arrives) report the same number. *)
let max_head_bytes = 30000
let window_size = 32767

module I64 = Stdlib_upstream_compatible.Int64_u

(* [max_response] bounds the body as it streams, so the declared
   Content-Length is not held to the parse limit. *)
let limits =
  #{ Httpz.Buf_read.default_limits with
     max_header_size = i16 max_head_bytes
   ; max_content_length = I64.of_int64 Int64.max_int
   }

type config = {
  (* There is no [net] field, because an [Eio.Net.t]'s platform tag
     cannot be narrowed by coercion: closing over the network capability
     is what lets a client of one concrete type hold on to it. *)
  connect : connect;
  https : https option;
  max_response : int;
  user_agent : string;
  decode : bool;
  (* [Eio.Time.Timeout.none] when no clock was supplied, which is also
     what makes [idle] [None]: without a clock nothing is bounded. *)
  connect_timeout : Eio.Time.Timeout.t;
  idle : (Eio.Time.Timeout.t * float) option;
  close_tls : (conn -> unit) option;
}

(* Preserve cancellation while translating Eio transport failures to
   [Fetch.error]. *)
let map_exn ex =
  match ex with
  | Eio.Cancel.Cancelled _ | Eio.Io (E _, _) -> ex
  | Eio.Io (Eio.Net.E (Connection_failure f), _) -> err (Connection_failure f)
  | Eio.Io (Eio.Net.E (Address_lookup_failed _), _) ->
    err (Connection_failure (Refused (Httpz_error "host lookup failed")))
  | Eio.Io (Eio.Net.E (Connection_reset _), _) ->
    err (Protocol_error "connection reset by peer")
  | End_of_file -> err (Protocol_error "connection closed by peer")
  | ex -> ex

let reraise ex =
  let bt = Printexc.get_raw_backtrace () in
  Printexc.raise_with_backtrace (map_exn ex) bt

let close_flow flow =
  Eio.Cancel.protect (fun () ->
      try Eio.Resource.close flow with _ -> ())

(* [deflate] is ambiguous on the wire (RFC 1950 zlib versus
   RFC 1951 raw) and enough servers get it wrong that asking for it buys
   a guessing game. Since we advertise only what we can decode, a
   conformant server sends us either gzip or identity. *)

(* A single [Content-Encoding] of exactly [gzip] is what we asked for.
   Anything else, whether a coding we did not advertise such as [br], a
   list of codings, or a repeated field, is handed on untouched with its
   header intact. *)
let is_gzip headers =
  match Http.Header.get_multi headers "content-encoding" with
  | [ v ] ->
    (match String.trim (String.lowercase_ascii v) with
     | "gzip" | "x-gzip" -> true
     | _ -> false)
  | _ -> false

(* The authority for the [Host] header, which is the backend's to derive
   from the URL policy approved: [Middleware.Url.host] holds an IPv6
   literal without its brackets, and [Host: ::1:80] would be ambiguous.
   The port is elided when it is the scheme's default, as every other
   client does. *)
let host_header url =
  let host = Middleware.Url.host url in
  let host = if String.contains host ':' then "[" ^ host ^ "]" else host in
  let port = Middleware.Url.port url in
  if port = Middleware.Url.default_port (Middleware.Url.scheme url) then host
  else Fmt.str "%s:%d" host port

(* A declared length is what goes out as [Content-Length], so the flow
   must be held to it: a longer one would leave trailing bytes for the
   server to read as the head of another request, and a shorter one
   leaves the request truncated. Bytes past the declared length are
   dropped; falling short is an error, since by then a short body has
   already gone out. *)
module Limited = struct
  type t = {
    src : Eio.Flow.source_ty Eio.Resource.t;
    length : int64;
    mutable left : int64;
  }

  let read_methods = []

  let single_read t (buf @ local) =
    if t.left <= 0L then raise End_of_file;
    let room =
      if Int64.compare t.left (Int64.of_int (Cstruct.length buf)) >= 0 then
        Cstruct.length buf
      else Int64.to_int t.left
    in
    match Eio.Flow.single_read t.src (Cstruct.sub_local buf 0 room) with
    | n -> t.left <- Int64.sub t.left (Int64.of_int n); n
    | exception End_of_file ->
      raise (err (Invalid_request
                    (Fmt.str "request body ended %Ld bytes short of the \
                              declared length of %Ld" t.left t.length)))
end

let limited_handler = Eio.Flow.Pi.source (module Limited)

let limited ~length src =
  Eio.Resource.T ({ Limited.src; length; left = length }, limited_handler)

(* A peer that accepts the connection and then says nothing costs a fiber
   and a socket for as long as the caller lets it. The bound belongs on
   the connection rather than at each call site: wrapping it once covers
   the head write, a streamed or chunked body, the head and body reads,
   the trailers, and the reads the gunzip decoder makes underneath. *)
module Timed = struct
  type t = {
    conn : conn;
    timeout : Eio.Time.Timeout.t;
    seconds : float;
  }

  let read_methods = []

  let bounded t what fn =
    match Eio.Time.Timeout.run_exn t.timeout fn with
    | v -> v
    | exception Eio.Time.Timeout ->
      raise (err (Protocol_error
                    (Fmt.str "idle timeout of %gs elapsed while %s"
                       t.seconds what)))

  let single_read t (buf @ local) =
    (* [bounded] installs competing fiber closures, so the descriptor must
       outlive this stack region until the winner is known. *)
    let buf = Cstruct.globalize buf in
    bounded t "reading from the connection" (fun () ->
        Eio.Flow.single_read t.conn buf)

  let single_write t (bufs @ local) =
    (* The timeout fiber can retain the write closure while the syscall is
       suspended. *)
    let bufs = Cstruct.globalize_list bufs in
    bounded t "writing to the connection" (fun () ->
        Eio.Flow.single_write t.conn bufs)

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdown t cmd = Eio.Flow.shutdown t.conn cmd

  (* Closing is left unbounded: it runs from a switch release hook, where
     a nested cancellation context is the wrong thing to introduce, and
     [close_transport] already tolerates a failure there. *)
  let close t = Eio.Resource.close t.conn
end

let timed_handler :
    (Timed.t, [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ])
    Eio.Resource.handler =
  Eio.Resource.handler
    (Eio.Resource.H (Eio.Resource.Close, Timed.close)
     :: Eio.Resource.bindings (Eio.Flow.Pi.two_way (module Timed)))

let timed idle conn : conn =
  match idle with
  | None -> conn
  | Some (timeout, seconds) ->
    Eio.Resource.T ({ Timed.conn; timeout; seconds }, timed_handler)

type transport = {
  raw : conn;
  finish : unit -> unit;
  mutable closed : bool;
  (* Closes the connection if the request's switch finishes before the
     response body is done with. [Eio.Net.connect] already registers the
     socket, but a TLS wrapper around it is ours to shut down, and this
     hook runs before the socket's own. *)
  mutable hook : Eio.Switch.hook;
}

let close_transport_with t close =
  if not t.closed then begin
    t.closed <- true;
    Eio.Switch.remove_hook t.hook;
    Eio.Cancel.protect (fun () ->
        try close () with ex ->
          (* The peer is gone either way, and raising here would take out
             the caller's switch instead of the request. *)
          Eio.Private.Trace.log
            (Fmt.str "fetch-httpz: closing connection: %s"
               (Printexc.to_string ex)))
  end

let close_transport t = close_transport_with t (fun () -> Eio.Resource.close t.raw)
let finish_transport t = close_transport_with t t.finish

let connect_tcp ~net ~sw ~host ~port : conn =
  let addrs =
    Eio.Net.getaddrinfo_stream ~service:(string_of_int port) net host
  in
  (* Try every resolver result in order and preserve the first failure if
     none connects. *)
  let rec try_addrs first = function
    | [] ->
      (match first with
       | Some ex -> raise ex
       | None ->
         raise (err (Connection_failure
                       (Refused (Httpz_error "no addresses for host")))))
    | addr :: rest ->
      (match Eio.Net.connect ~sw net addr with
       | sock -> (sock :> conn)
       | exception (Eio.Cancel.Cancelled _ as ex) -> raise ex
       | exception ex ->
         try_addrs (Some (Option.value first ~default:ex)) rest)
  in
  try_addrs None addrs

(* The request head is written into one buffer and sent in one
   write. The writers' offsets are unchecked [int16#], so the block is
   bounded before each header is written. *)

let write_head buf ~meth ~target headers =
  (* [write_request_line] deliberately uses unchecked writers.  Account for
     both spaces, "HTTP/1.1", its CRLF, and the empty line that terminates the
     head before handing it the caller-controlled method and target.  Write
     the comparisons as subtractions so enormous strings cannot wrap an
     [int] while their lengths are added. *)
  let fixed = 1 + 1 + String.length "HTTP/1.1" + 2 + 2 in
  let meth_len = String.length meth in
  let target_len = String.length target in
  if meth_len > max_head_bytes - fixed
     || target_len > max_head_bytes - fixed - meth_len
  then
    raise
      (err
         (Invalid_request
            (Fmt.str "request head exceeds %d bytes" max_head_bytes)));
  let off = ref 0 in
  off :=
    to_int
      (Httpz.Req.write_request_line buf ~off:(i16 0) ~meth ~target
         Httpz.Version.Http_1_1);
  List.iter
    (fun (name, value) ->
       let fixed = 4 + 2 (* [": "] + CRLF, then the final empty line. *) in
       let name_len = String.length name in
       let value_len = String.length value in
       if name_len > max_head_bytes - !off - fixed
          || value_len > max_head_bytes - !off - fixed - name_len
       then
         raise (err (Invalid_request
                       (Fmt.str "request head exceeds %d bytes"
                          max_head_bytes)));
       off := to_int (Httpz.Res.write_header buf ~off:(i16 !off) name value))
    headers;
  off := to_int (Httpz.Res.write_crlf buf ~off:(i16 !off));
  !off

(* A [Stream] body with no declared length goes out chunked. Each read
   becomes one chunk, framed around the bytes in place. *)
let send_chunked conn flow =
  let data = Cstruct.create 16384 in
  let head = Bytes.create 32 in
  let head_cs = Cstruct.create 32 in
  let crlf = Cstruct.of_string "\r\n" in
  let rec loop () =
    match Eio.Flow.single_read flow data with
    | n ->
      let hlen =
        to_int (Httpz.Res.write_chunk_header head ~off:(i16 0) ~size:n)
      in
      Cstruct.blit_from_bytes head 0 head_cs 0 hlen;
      Eio.Flow.write conn
        (stack_
          [ Cstruct.sub_local head_cs 0 hlen;
            Cstruct.sub_local data 0 n;
            crlf ]);
      loop ()
    | exception End_of_file ->
      Eio.Flow.copy_string "0\r\n\r\n" conn
  in
  loop ()

type head_info = {
  code : int;
  version : Fetch.version;
  resp_headers : (string * string) list;
  content_length : int64;
  chunked : bool;
  bodyless : bool;
}

(* The parse window over the head, which then carries the leftover
   bytes a read pulled in past it, and later serves as the chunked
   framing window. *)
type window = {
  tr : transport;
  wbuf : bytes;
  wcs : Cstruct.t;
  mutable pos : int;
  mutable len : int;
}

let head_overflow () =
  raise (err (Protocol_error
                (Fmt.str "response headers exceed %d bytes" max_head_bytes)))

(* Discard what a previous phase consumed, so that the free space is
   contiguous and a parse starts at offset zero. *)
let shift w =
  if w.pos > 0 then begin
    Bytes.blit w.wbuf w.pos w.wbuf 0 (w.len - w.pos);
    w.len <- w.len - w.pos;
    w.pos <- 0
  end

(* Read more into the window, shifting consumed bytes out first. [what]
   names the phase for the two distinct failures: a window that is full
   even after the shift, and a peer that stopped mid-phase. *)
let refill w ~what =
  shift w;
  if w.len >= window_size then
    raise (err (Protocol_error (Fmt.str "response %s exceeds the %d byte \
                                         window" what window_size)));
  match
    Eio.Flow.single_read w.tr.raw
      (Cstruct.sub_local w.wcs w.len (window_size - w.len))
  with
  | n -> Cstruct.blit_to_bytes w.wcs w.len w.wbuf w.len n; w.len <- w.len + n
  | exception End_of_file ->
    raise (err (Protocol_error (Fmt.str "connection closed while reading \
                                         response %s" what)))

(* Parse one response head out of the window, materializing everything
   that refers to the parse buffer before returning. The parser starts
   at offset zero, so bytes an earlier head consumed are shifted out
   first. *)
let read_head w request_method =
  shift w;
  let rec loop () =
    let #(status, res, headers) =
      Httpz.Res.parse ?request_method w.wbuf ~len:(i16 w.len) ~limits
    in
    match status with
    | Httpz.Buf_read.Complete ->
      let resp_headers =
        List.rev (Httpz.Header.to_string_pairs_local w.wbuf headers)
      in
      let version : Fetch.version =
        match res.#version with
        | Httpz.Version.Http_1_1 -> `HTTP_1_1
        | Httpz.Version.Http_1_0 -> `HTTP_1_0
      in
      w.pos <- to_int res.#body_off;
      { code = to_int res.#code;
        version;
        resp_headers;
        content_length = I64.to_int64 res.#content_length;
        chunked = res.#is_chunked;
        bodyless = res.#bodyless }
    | Httpz.Buf_read.Partial ->
      if w.len >= window_size then head_overflow ()
      else begin refill w ~what:"head"; loop () end
    | Httpz.Buf_read.Headers_too_large -> head_overflow ()
    | status ->
      raise (err (Protocol_error
                    (Fmt.str "invalid response: %s"
                       (Httpz.Buf_read.status_to_string status))))
  in
  loop ()

(* One source serves the three framings an HTTP/1.1 response may use.
   Leftover bytes that arrived with the head are served out of the
   window first; chunk framing keeps using the window, while chunk data
   beyond it is read straight into the caller's buffer, so a chunk may
   be arbitrarily large. *)

type framing =
  | To_eof
  | Length of int64 ref
  | Chunk_header
  | Chunk_data of int ref

type body = {
  w : window;
  mutable framing : framing;
  mutable trailers : Http.Header.t option;
  mutable body_eof : bool;
}

module Body = struct
  type t = body

  let read_methods = []

  let window_bytes b = b.w.len - b.w.pos

  let from_window b dst limit =
    let n = min (window_bytes b) (min (Cstruct.length dst) limit) in
    Cstruct.blit_from_bytes b.w.wbuf b.w.pos dst 0 n;
    b.w.pos <- b.w.pos + n;
    n

  let direct_read b dst limit =
    let n = min (Cstruct.length dst) limit in
    let got = Eio.Flow.single_read b.w.tr.raw (Cstruct.sub_local dst 0 n) in
    got

  (* The CRLF after a chunk's data, which may itself arrive in pieces. *)
  let eat_chunk_crlf b =
    while window_bytes b < 2 do refill b.w ~what:"chunk framing" done;
    if not (Bytes.get b.w.wbuf b.w.pos = '\r'
            && Bytes.get b.w.wbuf (b.w.pos + 1) = '\n')
    then raise (err (Protocol_error "malformed chunked framing"));
    b.w.pos <- b.w.pos + 2

  let read_trailers b =
    let rec loop () =
      let #(status, end_off, hdrs) =
        Httpz.Chunk.parse_trailers b.w.wbuf ~off:(i16 b.w.pos)
          ~len:(i16 b.w.len) ~max_header_count:(i16 100)
      in
      match status with
      | Httpz.Chunk.Trailer_complete ->
        b.w.pos <- to_int end_off;
        (match List.rev (Httpz.Header.to_string_pairs_local b.w.wbuf hdrs) with
         | [] -> ()
         | l -> b.trailers <- Some (Http.Header.of_list l))
      | Httpz.Chunk.Trailer_partial -> refill b.w ~what:"trailers"; loop ()
      | Httpz.Chunk.Trailer_malformed | Httpz.Chunk.Trailer_bare_cr ->
        raise (err (Protocol_error "malformed chunked trailers"))
    in
    loop ()

  let rec single_read b dst =
    if b.body_eof then raise End_of_file;
    match b.framing with
    | To_eof ->
      if window_bytes b > 0 then from_window b dst max_int
      else begin
        match direct_read b dst max_int with
        | n -> n
        | exception End_of_file -> b.body_eof <- true; raise End_of_file
      end
    | Length left ->
      if Int64.compare !left 0L <= 0 then begin
        b.body_eof <- true;
        raise End_of_file
      end
      else begin
        let limit =
          if Int64.compare !left (Int64.of_int max_int) >= 0 then max_int
          else Int64.to_int !left
        in
        let n =
          if window_bytes b > 0 then from_window b dst limit
          else
            match direct_read b dst limit with
            | n -> n
            | exception End_of_file ->
              raise (err (Protocol_error
                            (Fmt.str "response body ended %Ld bytes short \
                                      of the declared length" !left)))
        in
        left := Int64.sub !left (Int64.of_int n);
        n
      end
    | Chunk_header ->
      let #(status, size, data_off) =
        Httpz.Chunk.parse_header b.w.wbuf ~off:(i16 b.w.pos) ~len:(i16 b.w.len)
          ~max_chunk_size:max_int
      in
      (match status with
       | Httpz.Chunk.Complete ->
         b.w.pos <- to_int data_off;
         b.framing <- Chunk_data (ref size);
         single_read b dst
       | Httpz.Chunk.Done ->
         b.w.pos <- to_int data_off;
         read_trailers b;
         b.body_eof <- true;
         raise End_of_file
       | Httpz.Chunk.Partial ->
         refill b.w ~what:"chunk framing";
         single_read b dst
       | Httpz.Chunk.Malformed | Httpz.Chunk.Chunk_too_large ->
         raise (err (Protocol_error "malformed chunked framing")))
    | Chunk_data left ->
      if !left = 0 then begin
        eat_chunk_crlf b;
        b.framing <- Chunk_header;
        single_read b dst
      end
      else begin
        let n =
          if window_bytes b > 0 then from_window b dst !left
          else
            match direct_read b dst !left with
            | n -> n
            | exception End_of_file ->
              raise (err (Protocol_error "connection closed mid-chunk"))
        in
        left := !left - n;
        n
      end
end

let body_handler = Eio.Flow.Pi.source (module Body)

(* Caps the body and releases the connection as soon as it is done with,
   rather than at the end of the request's switch. The cap counts decoded
   bytes, so it bounds a compression bomb too. *)
type response_body = {
  src : Eio.Flow.source_ty Eio.Resource.t;
  max_response : int;
  description : string;
  mutable seen : int;
  finish : unit -> unit;
  abort : unit -> unit;
}

module Response_body = struct
  type t = response_body

  let read_methods = []

  let single_read t (buf @ local) =
    match Eio.Flow.single_read t.src buf with
    | n ->
      if n > t.max_response - t.seen then begin
        t.abort ();
        raise (err (Protocol_error
                      (Fmt.str "%s exceeds %d bytes" t.description
                         t.max_response)))
      end;
      t.seen <- t.seen + n;
      n
    | exception End_of_file -> t.finish (); raise End_of_file
    | exception ex -> t.abort (); reraise ex
end

let response_body_handler = Eio.Flow.Pi.source (module Response_body)

let capped_body ~description ~max_response ~finish ~abort src =
  Eio.Resource.T
    ({ src; max_response; description; seen = 0; finish; abort },
     response_body_handler)

module Backend = struct
  type t = config
  type tag = [ `Generic | `Httpz ]

  (* RFC 9110 §8.6 asks a user agent to send [Content-Length: 0] on a request
     whose method gives enclosed content a defined meaning, even when there
     is none, so a recipient need not guess whether content was omitted or
     merely not yet framed; a method without defined content semantics gets
     the plain no-framing treatment of RFC 9112 §6.3 instead. *)
  let has_defined_content = function
    | `POST | `PUT | `PATCH -> true
    | _ -> false

  let framing (req : Middleware.request) headers =
    match req.body with
    | Empty when has_defined_content req.meth ->
      (`String "", Http.Header.replace headers "content-length" "0")
    | Empty -> (`None, headers)
    | String s ->
      ( `String s,
        Http.Header.replace headers "content-length"
          (string_of_int (String.length s)) )
    | Stream { length = None; flow } ->
      (`Chunked flow, Http.Header.replace headers "transfer-encoding" "chunked")
    | Stream { length = Some length; flow } ->
      if Int64.compare length 0L < 0 then
        raise (err (Invalid_request
                      (Fmt.str "request body has a negative declared length \
                                of %Ld" length)));
      ( `Flow (limited ~length flow),
        Http.Header.replace headers "content-length" (Int64.to_string length) )

  let request cfg ~sw (req : Middleware.request) =
    let url = req.url in
    let uri = Middleware.Url.to_uri url in
    let host = Middleware.Url.host url in
    let port = Middleware.Url.port url in
    let scheme = Middleware.Url.scheme url in
    let decode =
      cfg.decode && not (Http.Header.mem req.headers "accept-encoding")
    in
    let headers = req.headers in
    let headers =
      if decode then Http.Header.add headers "accept-encoding" "gzip"
      else headers
    in
    let headers =
      Http.Header.add_unless_exists headers "user-agent" cfg.user_agent
    in
    (* Use [replace] as defence in depth so exactly one [Host] reaches the
       wire even if an invalid request bypassed normal validation. *)
    let headers = Http.Header.replace headers "host" (host_header url) in
    (* A statement of fact rather than a preference: this backend opens a
       connection per exchange and drops it afterwards, so saying so lets
       the server release it too (RFC 9112 s9.6). *)
    let headers = Http.Header.replace headers "connection" "close" in
    let body, headers = framing req headers in
    let transport = ref None in
    let release () = Option.iter close_transport !transport in
    let finish () = Option.iter finish_transport !transport in
    (* A missing TLS provider is settled before anything is dialled, so an
       https URL never reaches [connect]. *)
    let wrap_tls =
      match scheme with
      | `Http -> None
      | `Https ->
        (match cfg.https with
         | None ->
           raise (err (Tls_failure
                         "no TLS provider: pass ~https to fetch https URLs"))
         | Some wrap -> Some wrap)
    in
    try
      (* Name resolution, the handshake with the peer, and the TLS
         handshake are one phase to the caller and share one bound. *)
      let raw =
        match
          Eio.Time.Timeout.run_exn cfg.connect_timeout (fun () ->
              let raw = cfg.connect ~sw ~host ~port in
              match wrap_tls with
              | None -> raw
              | Some wrap ->
                (* Whatever the wrapper raises for a rejected certificate is
                   its own affair, so name it for what it is: a handshake
                   failure is not worth retrying, and [Protocol_error] would
                   not say that. *)
                (match wrap uri raw with
                 | conn -> conn
                 | exception (Eio.Cancel.Cancelled _ as ex) ->
                   close_flow raw; raise ex
                 | exception (Eio.Io (E (Tls_failure _), _) as ex) ->
                   close_flow raw; raise ex
                 | exception Httpz_tls.Error message ->
                   close_flow raw;
                   raise (err (Tls_failure message))
                 | exception ex ->
                   close_flow raw;
                   raise (err (Tls_failure (Printexc.to_string ex)))))
        with
        | raw -> timed cfg.idle raw
        | exception Eio.Time.Timeout ->
          raise (err (Connection_failure Timeout))
      in
      let finish_raw =
        match scheme, cfg.close_tls with
        | `Https, Some close -> fun () -> close raw
        | _ -> fun () -> Eio.Resource.close raw
      in
      let tr =
        { raw; finish = finish_raw; closed = false; hook = Eio.Switch.null_hook }
      in
      tr.hook <-
        Eio.Switch.on_release_cancellable sw (fun () -> close_transport tr);
      transport := Some tr;
      let head = Bytes.create Httpz.buffer_size in
      let head_len =
        write_head head ~meth:(Http.Method.to_string req.meth)
          ~target:(Middleware.Url.path_and_query url)
          (Http.Header.to_list headers)
      in
      let wcs = Cstruct.create Httpz.buffer_size in
      Cstruct.blit_from_bytes head 0 wcs 0 head_len;
      let local_ head_cs = Cstruct.sub_local wcs 0 head_len in
      (match body with
       | `None -> Eio.Flow.write raw (stack_ [ head_cs ])
       | `String s ->
         Eio.Flow.write raw (stack_ [ head_cs; Cstruct.of_string_local s ])
       | `Flow flow ->
         Eio.Flow.write raw (stack_ [ head_cs ]); Eio.Flow.copy flow raw
       | `Chunked flow ->
         Eio.Flow.write raw (stack_ [ head_cs ]); send_chunked raw flow);
      (* The head buffer's contents have been sent, so it becomes the
         response's parse window. *)
      let w =
        { tr; wbuf = head; wcs; pos = 0; len = 0 }
      in
      let request_method =
        match req.meth with
        | `HEAD -> Some Httpz.Method.Head
        | `CONNECT -> Some Httpz.Method.Connect
        | _ -> None
      in
      (* An interim response precedes the one it announces (RFC 9110
         s15.2). Nothing here asks for one, but a server may volunteer
         an unsolicited [103]; each is a bare head to skip. The count is
         bounded so a server cannot feed us interim heads forever. *)
      let rec final_head interim_left =
        let info = read_head w request_method in
        if info.code >= 200 || info.code < 100 then info
        else if info.code = 101 then
          raise (err (Protocol_error
                        "server switched protocols, which this backend \
                         did not request"))
        else if interim_left = 0 then
          raise (err (Protocol_error "server sent too many interim responses"))
        else final_head (interim_left - 1)
      in
      let info = final_head 8 in
      let headers = Http.Header.of_list info.resp_headers in
      if
        (not info.bodyless)
        && (not info.chunked)
        && Http.Header.mem headers "transfer-encoding"
      then begin
        release ();
        raise
          (err
             (Protocol_error
                "unsupported non-chunked Transfer-Encoding in response"))
      end;
      let headers =
        if info.chunked then Http.Header.remove headers "transfer-encoding"
        else headers
      in
      (* RFC 9112 still frames a 205, but RFC 9110 forbids it from carrying
         content. This backend never pools transports, so close rather than
         exposing a server's invalid representation through the Fetch API. *)
      let contentless = info.bodyless || info.code = 205 in
      let framing =
        if info.chunked then Chunk_header
        else if Int64.compare info.content_length 0L >= 0 then
          Length (ref info.content_length)
        else To_eof
      in
      let b = { w; framing; trailers = None; body_eof = contentless } in
      let close_response () =
        b.body_eof <- true;
        release ()
      in
      let raw_body = Eio.Resource.T (b, body_handler) in
      let src, headers =
        if contentless then (Eio.Flow.string_source "", headers)
        else if decode && is_gzip headers then
          (* Bound the coded representation as well as the decoded one.  A
             stream of empty gzip members otherwise produces no decoded bytes
             and can run forever without reaching the outer limit. *)
          let encoded =
            capped_body ~description:"encoded response body"
              ~max_response:cfg.max_response ~finish ~abort:release raw_body
          in
          ( Gzip_stream.gunzip encoded,
            Http.Header.remove
              (Http.Header.remove headers "content-encoding")
              "content-length" )
        else (raw_body, headers)
      in
      if contentless then finish ();
      let capped =
        capped_body ~description:"response body"
          ~max_response:cfg.max_response ~finish ~abort:release src
      in
      Fetch.Middleware.Pi.response ~status:info.code ~headers
        ~version:info.version ~body:capped
        ~trailers:(fun () -> b.trailers)
        ~close:close_response ~url ()
    with ex -> release (); reraise ex
  end

let handler = Fetch.Middleware.Pi.client (module Backend)

let v ?clock ?connect ?https ?(max_response = 256 * 1024 * 1024)
    ?(user_agent = "fetch-httpz") ?(decode = true) ?(connect_timeout = 30.)
    ?(idle_timeout = 60.) net () : t =
  if max_response < 0 then
    invalid_arg "Fetch_httpz.v: max_response must be non-negative";
  if not (Middleware.is_field_value user_agent) then
    invalid_arg
      "Fetch_httpz.v: user_agent contains a forbidden control byte";
  let valid_timeout name seconds =
    match Float.classify_float seconds with
    | FP_nan | FP_infinite ->
      invalid_arg (Fmt.str "Fetch_httpz.v: %s must be finite" name)
    | FP_normal | FP_subnormal | FP_zero ->
      if seconds < 0. then
        invalid_arg
          (Fmt.str "Fetch_httpz.v: %s must be non-negative" name)
  in
  valid_timeout "connect_timeout" connect_timeout;
  valid_timeout "idle_timeout" idle_timeout;
  let connect =
    Option.value connect ~default:(fun ~sw ~host ~port ->
        connect_tcp ~net ~sw ~host ~port)
  in
  let connect_timeout, idle, close_tls =
    match clock with
    | None -> (Eio.Time.Timeout.none, None, None)
    | Some clock ->
      ( Eio.Time.Timeout.seconds clock connect_timeout,
        Some (Eio.Time.Timeout.seconds clock idle_timeout, idle_timeout),
        Some (fun flow -> Httpz_tls.close ~clock flow) )
  in
  Eio.Resource.T
    ({ connect; https; max_response; user_agent; decode; connect_timeout;
       idle; close_tls },
     handler)

let std ?connect ?(https = Httpz_tls.system) ?cookies ?retry ?max_concurrent
    ?min_interval
    ?connect_timeout ?idle_timeout env =
  Fetch_cookies.std ?cookies ?retry ?max_concurrent ?min_interval env
    (v ~clock:env#mono_clock ?connect ~https ?connect_timeout ?idle_timeout
       env#net ())
