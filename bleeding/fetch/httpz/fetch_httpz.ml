open Fetch

type tag = [ `Generic | `Httpz ]

type t = tag Fetch.ty Eio.Resource.t

type conn = [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t

type https = Uri.t -> conn -> conn

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
  connect : sw:Eio.Switch.t -> host:string -> port:int -> conn;
  https : https option;
  max_response : int;
  user_agent : string;
  decode : bool;
}

(* {2 Errors}

   Eio reports transport failures as [Eio.Io]. They become
   {!Fetch.error}, as backend conformance requires, and cancellation
   passes through. Protocol violations are raised as [Fetch.error]
   directly by the code below. *)
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

(* {2 Transparent content-coding}

   [gzip] only: [deflate] is ambiguous on the wire (RFC 1950 zlib versus
   RFC 1951 raw) and enough servers get it wrong that asking for it buys
   a guessing game. Since we advertise only what we can decode, a
   conformant server sends us either gzip or identity. *)

module type INF = sig
  type decoder

  val decode :
    decoder ->
    [ `Await of decoder | `Flush of decoder | `End of decoder
    | `Malformed of string ]

  val src : decoder -> De.bigstring -> int -> int -> decoder
  val dst_rem : decoder -> int
  val src_rem : decoder -> int
  val reset : decoder -> decoder
  val flush : decoder -> decoder
end

(* A pull-based inflating source over [decompress]'s non-blocking
   decoders: each read drains what is already decoded, and only refills
   from the transport when the decoder asks for more. Nothing buffers the
   whole body. *)
module Inflate (Inf : INF) = struct
  type phase =
    | Decoding
    | Member_ended
    | Ended

  type t = {
    src : Eio.Flow.source_ty Eio.Resource.t;
    i : De.bigstring;  (* input, handed to the decoder *)
    o : De.bigstring;  (* output, owned by the decoder *)
    o_cs : Cstruct.t;
    i_cs : Cstruct.t;
    mutable d : Inf.decoder;
    mutable ready : (int * int) option;  (* undrained window into [o] *)
    mutable phase : phase;
    mutable input_len : int;
  }

  let v ~src ~i ~o d =
    { src; i; o; o_cs = Cstruct.of_bigarray o; i_cs = Cstruct.of_bigarray i;
      d; ready = None; phase = Decoding; input_len = 0 }

  let read_methods = []

  (* How much of [o] the decoder has filled. *)
  let window t = De.bigstring_length t.o - Inf.dst_rem t.d

  let rec single_read t buf =
    match t.ready with
    | Some (pos, len) ->
      let n = min len (Cstruct.length buf) in
      Cstruct.blit t.o_cs pos buf 0 n;
      if n = len then begin
        t.ready <- None;
        (* [o] is the decoder's own buffer, and may only be reused once
           every byte of the window has been handed on. *)
        if t.phase = Decoding then t.d <- Inf.flush t.d
      end
      else t.ready <- Some (pos + n, len - n);
      n
    | None ->
      (match t.phase with
      | Ended -> raise End_of_file
      | Member_ended -> begin
        (* A gzip representation is a sequence of members. [src_rem] belongs
           to the input range most recently handed to the decoder, so retain
           that suffix when resetting for the next member. If the member ended
           exactly at a read boundary, probe the underlying framed body once:
           EOF ends the representation; any byte starts another member and
           malformed trailing data is rejected by its header parser. *)
        let rem = Inf.src_rem t.d in
        let d = Inf.reset t.d in
        t.phase <- Decoding;
        if rem > 0 then begin
          t.d <- Inf.src d t.i (t.input_len - rem) rem;
          single_read t buf
        end
        else
          match Eio.Flow.single_read t.src t.i_cs with
          | n ->
            t.input_len <- n;
            t.d <- Inf.src d t.i 0 n;
            single_read t buf
          | exception End_of_file ->
            t.phase <- Ended;
            raise End_of_file
        end
      | Decoding ->
        match Inf.decode t.d with
        | `Await d ->
          t.d <- d;
          let n =
            match Eio.Flow.single_read t.src t.i_cs with
            | n -> n
            | exception End_of_file -> 0  (* [l = 0] signals end of input *)
          in
          t.input_len <- n;
          t.d <- Inf.src t.d t.i 0 n;
          single_read t buf
        | `Flush d ->
          t.d <- d;
          (* An empty window would have us return a zero-length read,
             which a source may not do. *)
          (match window t with
           | 0 -> t.d <- Inf.flush t.d
           | len -> t.ready <- Some (0, len));
          single_read t buf
        | `End d ->
          t.d <- d;
          t.phase <- Member_ended;
          (match window t with
           | 0 -> single_read t buf
           | len -> t.ready <- Some (0, len); single_read t buf)
        | `Malformed msg ->
          raise (err (Protocol_error
                        (Fmt.str "malformed gzip response: %s" msg)))
      )
end

module Gunzip = Inflate (Gz.Inf)

let gunzip_handler = Eio.Flow.Pi.source (module Gunzip)

let gunzip src =
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let d = Gz.Inf.decoder `Manual ~o in
  Eio.Resource.T (Gunzip.v ~src ~i ~o d, gunzip_handler)

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

(* {2 Request bodies} *)

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

  let single_read t buf =
    if t.left <= 0L then raise End_of_file;
    let room =
      if Int64.compare t.left (Int64.of_int (Cstruct.length buf)) >= 0 then
        Cstruct.length buf
      else Int64.to_int t.left
    in
    match Eio.Flow.single_read t.src (Cstruct.sub buf 0 room) with
    | n -> t.left <- Int64.sub t.left (Int64.of_int n); n
    | exception End_of_file ->
      raise (err (Invalid_request
                    (Fmt.str "request body ended %Ld bytes short of the \
                              declared length of %Ld" t.left t.length)))
end

let limited_handler = Eio.Flow.Pi.source (module Limited)

let limited ~length src =
  Eio.Resource.T ({ Limited.src; length; left = length }, limited_handler)

(* {2 The transport} *)

type transport = {
  raw : conn;
  mutable closed : bool;
  (* Closes the connection if the request's switch finishes before the
     response body is done with. [Eio.Net.connect] already registers the
     socket, but a TLS wrapper around it is ours to shut down, and this
     hook runs before the socket's own. *)
  mutable hook : Eio.Switch.hook;
}

let close_transport t =
  if not t.closed then begin
    t.closed <- true;
    Eio.Switch.remove_hook t.hook;
    try Eio.Resource.close t.raw with
    | Eio.Cancel.Cancelled _ as ex ->
      (* A TLS wrapper's close writes, so it can be cancelled; that has
         to propagate rather than be logged away. *)
      raise ex
    | ex ->
      (* The peer is gone either way, and raising here would take out
         the caller's switch instead of the request. *)
      Eio.Private.Trace.log
        (Fmt.str "fetch-httpz: closing connection: %s"
           (Printexc.to_string ex))
  end

let connect_tcp ~net ~sw ~host ~port : conn =
  let addrs =
    Eio.Net.getaddrinfo_stream ~service:(string_of_int port) net host
  in
  (* Try each address in turn, as a happy-eyeballs client would, and
     report the first failure if none of them answer. *)
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

(* {2 The request head}

   Written with the httpz writers into one buffer and sent in one
   write. The writers' offsets are unchecked [int16#], so the block is
   bounded before each header is written. *)

let write_head buf ~meth ~target headers =
  let off = ref 0 in
  off :=
    to_int
      (Httpz.Req.write_request_line buf ~off:(i16 0) ~meth ~target
         Httpz.Version.Http_1_1);
  List.iter
    (fun (name, value) ->
       if !off + String.length name + String.length value + 4 > max_head_bytes
       then
         raise (err (Invalid_request
                       (Fmt.str "request headers exceed %d bytes"
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
  let rec loop () =
    match Eio.Flow.single_read flow data with
    | n ->
      let hlen =
        to_int (Httpz.Res.write_chunk_header head ~off:(i16 0) ~size:n)
      in
      Eio.Flow.write conn
        [ Cstruct.of_bytes head ~off:0 ~len:hlen;
          Cstruct.sub data 0 n;
          Cstruct.of_string "\r\n" ];
      loop ()
    | exception End_of_file ->
      Eio.Flow.copy_string "0\r\n\r\n" conn
  in
  loop ()

(* {2 The response head} *)

type head_info = {
  code : int;
  version : Fetch.version;
  resp_headers : (string * string) list;  (* wire order *)
  content_length : int64;  (* -1 when absent *)
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

(* Read more into the window, shifting consumed bytes out first. [what]
   names the phase for the two distinct failures: a window that is full
   even after the shift, and a peer that stopped mid-phase. *)
let refill w ~what =
  if w.pos > 0 then begin
    Bytes.blit w.wbuf w.pos w.wbuf 0 (w.len - w.pos);
    w.len <- w.len - w.pos;
    w.pos <- 0
  end;
  if w.len >= window_size then
    raise (err (Protocol_error (Fmt.str "response %s exceeds the %d byte \
                                         window" what window_size)));
  match
    Eio.Flow.single_read w.tr.raw
      (Cstruct.sub w.wcs w.len (window_size - w.len))
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
  if w.pos > 0 then begin
    Bytes.blit w.wbuf w.pos w.wbuf 0 (w.len - w.pos);
    w.len <- w.len - w.pos;
    w.pos <- 0
  end;
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
      else begin
        (match Eio.Flow.single_read w.tr.raw
                 (Cstruct.sub w.wcs w.len (window_size - w.len)) with
         | n -> Cstruct.blit_to_bytes w.wcs w.len w.wbuf w.len n;
           w.len <- w.len + n
         | exception End_of_file ->
           raise (err (Protocol_error "connection closed by peer")));
        loop ()
      end
    | Httpz.Buf_read.Headers_too_large -> head_overflow ()
    | status ->
      raise (err (Protocol_error
                    (Fmt.str "invalid response: %s"
                       (Httpz.Buf_read.status_to_string status))))
  in
  loop ()

(* {2 Response bodies}

   One source serves the three framings an HTTP/1.1 response may use.
   Leftover bytes that arrived with the head are served out of the
   window first; chunk framing keeps using the window, while chunk data
   beyond it is read straight into the caller's buffer, so a chunk may
   be arbitrarily large. *)

type framing =
  | To_eof
  | Length of int64 ref  (* bytes still owed *)
  | Chunk_header  (* at a chunk-size line *)
  | Chunk_data of int ref  (* bytes of data left, then CRLF *)

type body = {
  w : window;
  mutable framing : framing;
  mutable trailers : Http.Header.t option;
  mutable body_eof : bool;
  release : unit -> unit;
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
    Eio.Flow.single_read b.w.tr.raw (Cstruct.sub dst 0 n)

  (* The CRLF after a chunk's data, which may itself arrive in pieces. *)
  let eat_chunk_crlf b =
    while window_bytes b < 2 do refill b.w ~what:"chunk framing" done;
    if not (Bytes.get b.w.wbuf b.w.pos = '\r'
            && Bytes.get b.w.wbuf (b.w.pos + 1) = '\n')
    then raise (err (Protocol_error "malformed chunked framing"));
    b.w.pos <- b.w.pos + 2

  let read_trailers b =
    let rec loop () =
      let #(status, _end_off, hdrs) =
        Httpz.Chunk.parse_trailers b.w.wbuf ~off:(i16 b.w.pos)
          ~len:(i16 b.w.len) ~max_header_count:(i16 100)
      in
      match status with
      | Httpz.Chunk.Trailer_complete ->
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
         b.release ();
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
  mutable seen : int;
  release : unit -> unit;
}

module Response_body = struct
  type t = response_body

  let read_methods = []

  let single_read t buf =
    match Eio.Flow.single_read t.src buf with
    | n ->
      t.seen <- t.seen + n;
      if t.seen > t.max_response then begin
        t.release ();
        raise (err (Protocol_error
                      (Fmt.str "response body exceeds %d bytes"
                         t.max_response)))
      end;
      n
    | exception End_of_file -> t.release (); raise End_of_file
    | exception ex -> t.release (); reraise ex
end

let response_body_handler = Eio.Flow.Pi.source (module Response_body)

(* {2 One exchange} *)

module Backend = struct
  type t = config
  type tag = [ `Generic | `Httpz ]

  (* The framing headers this backend derives. A request with no
     content carries neither header, which RFC 9110 s8.6 asks of a user
     agent; a declared length goes out as [Content-Length] and an
     undeclared stream as [Transfer-Encoding: chunked]. *)
  let framing (req : Middleware.request) headers =
    match req.body with
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
    (* Every path into a backend refuses a caller's [Host] as reserved,
       but [replace] rather than [add] means that even if one arrived
       there is a single [Host] on the wire. *)
    let headers = Http.Header.replace headers "host" (host_header url) in
    (* A statement of fact rather than a preference: this backend opens a
       connection per exchange and drops it afterwards, so saying so lets
       the server release it too (RFC 9112 s9.6). *)
    let headers = Http.Header.replace headers "connection" "close" in
    let body, headers = framing req headers in
    let transport = ref None in
    let release () = Option.iter close_transport !transport in
    try
      let raw = cfg.connect ~sw ~host ~port in
      let raw =
        match scheme with
        | `Http -> raw
        | `Https ->
          (match cfg.https with
           | None ->
             Eio.Resource.close raw;
             raise (err (Tls_failure
                           "no TLS provider: pass ~https to fetch https URLs"))
           | Some wrap ->
             (* Whatever the wrapper raises for a rejected certificate is
                its own affair, so name it for what it is: a handshake
                failure is not worth retrying, and [Protocol_error] would
                not say that. *)
             (match wrap uri raw with
              | conn -> conn
              | exception (Eio.Cancel.Cancelled _ as ex) ->
                Eio.Resource.close raw; raise ex
              | exception ex ->
                Eio.Resource.close raw;
                raise (err (Tls_failure (Printexc.to_string ex)))))
      in
      let tr = { raw; closed = false; hook = Eio.Switch.null_hook } in
      tr.hook <-
        Eio.Switch.on_release_cancellable sw (fun () -> close_transport tr);
      transport := Some tr;
      let head = Bytes.create Httpz.buffer_size in
      let head_len =
        write_head head ~meth:(Http.Method.to_string req.meth)
          ~target:(Middleware.Url.path_and_query url)
          (Http.Header.to_list headers)
      in
      let head_cs = Cstruct.of_bytes head ~off:0 ~len:head_len in
      (match body with
       | `None -> Eio.Flow.write raw [ head_cs ]
       | `String s -> Eio.Flow.write raw [ head_cs; Cstruct.of_string s ]
       | `Flow flow -> Eio.Flow.write raw [ head_cs ]; Eio.Flow.copy flow raw
       | `Chunked flow ->
         Eio.Flow.write raw [ head_cs ]; send_chunked raw flow);
      (* The head buffer's contents have been sent, so it becomes the
         response's parse window. *)
      let w =
        { tr; wbuf = head; wcs = Cstruct.of_bytes head; pos = 0; len = 0 }
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
      let bodyless = info.bodyless in
      let framing =
        if info.chunked then Chunk_header
        else if Int64.compare info.content_length 0L >= 0 then
          Length (ref info.content_length)
        else To_eof
      in
      let b = { w; framing; trailers = None; body_eof = bodyless; release } in
      let raw_body = Eio.Resource.T (b, body_handler) in
      let src, headers =
        if bodyless then (Eio.Flow.string_source "", headers)
        else if decode && is_gzip headers then
          ( gunzip raw_body,
            (* Present the decoded view, as a backend must. *)
            Http.Header.remove
              (Http.Header.remove headers "content-encoding")
              "content-length" )
        else (raw_body, headers)
      in
      if bodyless then release ();
      let capped =
        Eio.Resource.T
          ({ src; max_response = cfg.max_response; seen = 0; release },
           response_body_handler)
      in
      Fetch.Middleware.Pi.response ~status:info.code ~headers
        ~version:info.version ~body:capped
        ~trailers:(fun () -> b.trailers)
        ~url ()
    with ex -> release (); reraise ex
  end

let handler = Fetch.Middleware.Pi.client (module Backend)

let default_user_agent = "fetch-httpz"

let v ?https ?(max_response = 256 * 1024 * 1024)
    ?(user_agent = default_user_agent) ?(decode = true) net () : t =
  let connect ~sw ~host ~port = connect_tcp ~net ~sw ~host ~port in
  Eio.Resource.T ({ connect; https; max_response; user_agent; decode }, handler)

(* The design.md s11.2 recommended stack over this backend, minted from
   stdenv capabilities: retries re-consult the jar and are paced, and any
   policy the caller stacks on top still gates every attempt. *)
let std ?https ?(cookies = `Memory) ?retry ?(max_concurrent = 6)
    ?min_interval env =
  let clock = env#clock in
  let mono_clock = env#mono_clock in
  let backend = v ?https env#net () in
  let with_cookies =
    match cookies with
    | `Off -> fun t -> Fetch.Middleware.of_handler (Fetch.Middleware.handler t)
    | `Memory -> Fetch_cookies.with_jar (Fetch_cookies.Jar.in_memory ~clock ())
    | `File path ->
      Fetch_cookies.with_jar (Fetch_cookies.Jar.of_file ~clock path)
  in
  backend
  |> with_cookies
  |> Fetch.with_limits ~clock:mono_clock ?min_interval ~max_concurrent
  |> Fetch.with_retry ~clock:mono_clock ~random:env#secure_random ?config:retry
