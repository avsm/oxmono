open Fetch

type tag = [ `Generic | `Cohttp ]

type t = tag Fetch.ty Eio.Resource.t

type conn = [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t

type https = Uri.t -> conn -> conn

type Eio.Exn.Backend.t += Cohttp_error of string

let () =
  Eio.Exn.Backend.register_pp (fun f -> function
      | Cohttp_error msg -> Fmt.pf f "Cohttp_error(%S)" msg; true
      | _ -> false
    )

(* cohttp-eio parses the status line and header block with an unbounded
   [Buf_read], so a server that never stops sending header lines would
   make us allocate without limit. The transport wrapper below bounds
   what may be read before the response is parsed. *)
let max_header_bytes = 256 * 1024

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

   cohttp reports a malformed or truncated exchange as [Failure], and Eio
   reports transport failures as [Eio.Io]. Both become {!Fetch.error}, as
   backend conformance requires, and cancellation passes through. *)
let map_exn ex =
  match ex with
  | Eio.Cancel.Cancelled _ | Eio.Io (E _, _) -> ex
  | Eio.Io (Eio.Net.E (Connection_failure f), _) -> err (Connection_failure f)
  | Eio.Io (Eio.Net.E (Address_lookup_failed _), _) ->
    err (Connection_failure (Refused (Cohttp_error "host lookup failed")))
  | Eio.Io (Eio.Net.E (Connection_reset _), _) ->
    err (Protocol_error "connection reset by peer")
  | End_of_file -> err (Protocol_error "connection closed by peer")
  | Failure msg -> err (Protocol_error msg)
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
  val flush : decoder -> decoder
end

(* A pull-based inflating source over [decompress]'s non-blocking
   decoders: each read drains what is already decoded, and only refills
   from the transport when the decoder asks for more. Nothing buffers the
   whole body. *)
module Inflate (Inf : INF) = struct
  type t = {
    src : Eio.Flow.source_ty Eio.Resource.t;
    i : De.bigstring;  (* input, handed to the decoder *)
    o : De.bigstring;  (* output, owned by the decoder *)
    o_cs : Cstruct.t;
    i_cs : Cstruct.t;
    mutable d : Inf.decoder;
    mutable ready : (int * int) option;  (* undrained window into [o] *)
    mutable ended : bool;
  }

  let v ~src ~i ~o d =
    { src; i; o; o_cs = Cstruct.of_bigarray o; i_cs = Cstruct.of_bigarray i;
      d; ready = None; ended = false }

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
        if not t.ended then t.d <- Inf.flush t.d
      end
      else t.ready <- Some (pos + n, len - n);
      n
    | None ->
      if t.ended then raise End_of_file
      else begin
        match Inf.decode t.d with
        | `Await d ->
          t.d <- d;
          let n =
            match Eio.Flow.single_read t.src t.i_cs with
            | n -> n
            | exception End_of_file -> 0  (* [l = 0] signals end of input *)
          in
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
          t.ended <- true;
          (match window t with
           | 0 -> raise End_of_file
           | len -> t.ready <- Some (0, len); single_read t buf)
        | `Malformed msg ->
          raise (err (Protocol_error (Fmt.str "malformed gzip response: %s" msg)))
      end
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

let default_port = function `Http -> 80 | `Https -> 443

(* Where the request goes, read off the URL [Fetch] validated. That
   validation is why the two failures below are unreachable: a URL that
   reaches a backend has an http or https scheme and a host. *)
let authority uri =
  let broken what = raise (err (Invalid_url (Fmt.str "%a %s" Uri.pp uri what))) in
  let scheme =
    match Uri.scheme uri with
    | Some "http" -> `Http
    | Some "https" -> `Https
    | _ -> broken "has no http or https scheme"
  in
  let host = match Uri.host uri with Some h -> h | None -> broken "has no host" in
  let port = Option.value (Uri.port uri) ~default:(default_port scheme) in
  (scheme, host, port)

(* The authority for the [Host] header, which is the backend's to derive
   from the URL policy approved rather than cohttp's to reconstruct: it
   builds one by appending the port to [Uri.host], and [Uri.host] strips
   the brackets from an IPv6 literal, so [http://[::1]:80/] would go out
   as the ambiguous [Host: ::1:80]. The port is elided when it is the
   scheme's default, as every other client does. *)
let host_header ~scheme ~host ~port =
  let host = if String.contains host ':' then "[" ^ host ^ "]" else host in
  if port = default_port scheme then host else Fmt.str "%s:%d" host port

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

(* {2 The transport}

   The connection is made here rather than by [Cohttp_eio.Client.make]:
   that one reads a global proxy configuration, which is exactly the
   ambient authority a client capability must not have. Making it
   ourselves also gives us the header bound and a handle to close once
   the response body is done with. *)

type transport = {
  raw : conn;
  mutable budget : int;  (* header-phase read allowance; [max_int] after *)
  mutable closed : bool;
  (* Closes the connection if the request's switch finishes before the
     response body is done with. [Eio.Net.connect] already registers the
     socket, but a TLS wrapper around it is ours to shut down, and this
     hook runs before the socket's own. *)
  mutable hook : Eio.Switch.hook;
}

module Transport = struct
  type t = transport

  let read_methods = []

  let single_read t buf =
    let n = Eio.Flow.single_read t.raw buf in
    if t.budget < max_int then begin
      t.budget <- t.budget - n;
      if t.budget < 0 then
        raise (err (Protocol_error
                      (Fmt.str "response headers exceed %d bytes"
                         max_header_bytes)))
    end;
    n

  let single_write t bufs = Eio.Flow.single_write t.raw bufs
  let copy t ~src = Eio.Flow.copy src t.raw
  let shutdown t cmd = Eio.Flow.shutdown t.raw cmd

  let close t =
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
          (Fmt.str "fetch-cohttp: closing connection: %s"
             (Printexc.to_string ex))
    end
end

let transport_handler :
  (transport, [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ])
    Eio.Resource.handler =
  Eio.Resource.handler
    (Eio.Resource.H (Eio.Resource.Close, Transport.close)
     :: Eio.Resource.bindings (Eio.Flow.Pi.two_way (module Transport)))

let connect_tcp ~net ~sw ~host ~port : conn =
  let addrs = Eio.Net.getaddrinfo_stream ~service:(string_of_int port) net host in
  (* Try each address in turn, as a happy-eyeballs client would, and
     report the first failure if none of them answer. *)
  let rec try_addrs first = function
    | [] ->
      (match first with
       | Some ex -> raise ex
       | None ->
         raise (err (Connection_failure
                       (Refused (Cohttp_error "no addresses for host")))))
    | addr :: rest ->
      (match Eio.Net.connect ~sw net addr with
       | sock -> (sock :> conn)
       | exception (Eio.Cancel.Cancelled _ as ex) -> raise ex
       | exception ex ->
         try_addrs (Some (Option.value first ~default:ex)) rest)
  in
  try_addrs None addrs

(* {2 Response bodies} *)

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
    | exception ex ->
      (* A framing error part way through a body is cohttp's [Failure];
         it has to reach the caller as a {!Fetch.error} like any other. *)
      t.release (); reraise ex
end

let response_body_handler = Eio.Flow.Pi.source (module Response_body)

(* {2 One exchange} *)

let meth_is_head = function `HEAD -> true | _ -> false

module Backend = struct
  type t = config
  type tag = [ `Generic | `Cohttp ]

  let framing (req : Middleware.request) headers =
    match req.body with
    | Empty -> (None, headers)
    | String s -> (Some (Cohttp_eio.Body.of_string s), headers)
    | Stream { length = None; flow } -> (Some flow, headers)
    | Stream { length = Some length; flow } ->
      if Int64.compare length 0L < 0 then
        raise (err (Invalid_request
                      (Fmt.str "request body has a negative declared length \
                                of %Ld" length)));
      (* [Http.Header.add_transfer_encoding] leaves a caller's
         [Content-Length] alone and skips chunking, so declaring the
         length here is what keeps a known-length body unchunked. *)
      ( Some (limited ~length flow),
        Http.Header.replace headers "content-length" (Int64.to_string length) )

  let request cfg ~sw (req : Middleware.request) =
    let uri = Middleware.Url.to_uri req.url in
    let scheme, host, port = authority uri in
    let decode = cfg.decode && not (Http.Header.mem req.headers "accept-encoding") in
    let headers = req.headers in
    let headers =
      if decode then Http.Header.add headers "accept-encoding" "gzip"
      else headers
    in
    let headers = Http.Header.add_unless_exists headers "user-agent" cfg.user_agent in
    (* [Cohttp.Request.make] keeps a [Host] it is given. A request cannot
       have set one, since every path into a backend refuses it as
       reserved, but [replace] rather than [add] means that even if one
       arrived there is a single [Host] on the wire. *)
    let headers =
      Http.Header.replace headers "host" (host_header ~scheme ~host ~port)
    in
    (* A statement of fact rather than a preference: this backend opens a
       connection per exchange and drops it afterwards, so saying so lets
       the server release it too (RFC 9112 s9.6). *)
    let headers = Http.Header.replace headers "connection" "close" in
    let body, headers = framing req headers in
    let transport = ref None in
    let connect ~sw:_ _uri =
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
      let tr =
        { raw; budget = max_header_bytes; closed = false;
          hook = Eio.Switch.null_hook }
      in
      tr.hook <- Eio.Switch.on_release_cancellable sw (fun () -> Transport.close tr);
      transport := Some tr;
      (Eio.Resource.T (tr, transport_handler) : conn)
    in
    let release () = Option.iter Transport.close !transport in
    match
      Cohttp_eio.Client.call
        (Cohttp_eio.Client.make_generic connect)
        ~sw ~headers ?body req.meth uri
    with
    | exception ex -> release (); reraise ex
    | resp, cbody ->
      (* The headers are parsed by now, so the rest of the connection is
         the body, bounded by [max_response] instead. *)
      Option.iter (fun tr -> tr.budget <- max_int) !transport;
      let status = Http.Status.to_int (Http.Response.status resp) in
      if status < 200 then begin
        (* cohttp-eio hands back the first response block it reads, so an
           interim one would masquerade as the final response. Nothing
           here asks for [100 Continue]; a [1xx] therefore means the
           server volunteered one (RFC 9110 s15.2) and we cannot get past
           it to the response it precedes. *)
        release ();
        raise (err (Protocol_error
                      (Fmt.str "server sent an interim %d response, which \
                                this backend cannot skip" status)))
      end;
      let headers = Http.Response.headers resp in
      let bodyless =
        (* A HEAD response carries the headers of a body that is not
           there: reading one would block until the peer gave up. *)
        meth_is_head req.meth || status = 204 || status = 304
      in
      let src, headers =
        if bodyless then (Eio.Flow.string_source "", headers)
        else if decode && is_gzip headers then
          ( gunzip cbody,
            (* Present the decoded view, as a backend must. *)
            Http.Header.remove
              (Http.Header.remove headers "content-encoding")
              "content-length" )
        else (cbody, headers)
      in
      if bodyless then release ();
      let body =
        Eio.Resource.T
          ({ src; max_response = cfg.max_response; seen = 0; release },
           response_body_handler)
      in
      Fetch.Middleware.Pi.response ~status ~headers
        ~version:(Http.Response.version resp :> Fetch.version)
        ~body ~url:req.url ()
end

let handler = Fetch.Middleware.Pi.client (module Backend)

let default_user_agent = "fetch-cohttp"

let v ?https ?(max_response = 256 * 1024 * 1024)
    ?(user_agent = default_user_agent) ?(decode = true) net () : t =
  let connect ~sw ~host ~port = connect_tcp ~net ~sw ~host ~port in
  Eio.Resource.T ({ connect; https; max_response; user_agent; decode }, handler)

(* The design.md s11.2 recommended stack over this backend, minted from
   stdenv capabilities: retries re-consult the jar and are paced, and any
   policy the caller stacks on top still gates every attempt. *)
let std ?https ?(cookies = `Memory) ?retry ?(max_concurrent = 6) ?min_interval env =
  let clock = env#clock in
  let mono_clock = env#mono_clock in
  let backend = v ?https env#net () in
  let with_cookies =
    match cookies with
    | `Off -> fun t -> Fetch.Middleware.of_handler (Fetch.Middleware.handler t)
    | `Memory -> Fetch_cookies.with_jar (Fetch_cookies.Jar.in_memory ~clock ())
    | `File path -> Fetch_cookies.with_jar (Fetch_cookies.Jar.of_file ~clock path)
  in
  backend
  |> with_cookies
  |> Fetch.with_limits ~clock:mono_clock ?min_interval ~max_concurrent
  |> Fetch.with_retry ~clock:mono_clock ~random:env#secure_random ?config:retry
