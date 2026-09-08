module I16 = Stdlib_stable.Int16_u
module I64 = Stdlib_upstream_compatible.Int64_u
module F64 = Stdlib_upstream_compatible.Float_u
module H = Httpz.Header_name
module St = Httpz.Res

let[@inline] i16 x = I16.of_int x
let[@inline] to_int x = I16.to_int x

type config = {
  backlog : int;
  max_connections : int;
  first_byte_timeout : Duration.t;
  idle_timeout : Duration.t;
  request_timeout : Duration.t;
  write_timeout : Duration.t;
}

type tls = Httpz_tls.server

let default_config =
  {
    backlog = 64;
    max_connections = 512;
    first_byte_timeout = Duration.of_sec 5;
    idle_timeout = Duration.of_sec 75;
    request_timeout = Duration.of_sec 15;
    write_timeout = Duration.of_sec 30;
  }

type event = {
  remote_addr : string;
  meth : Proffer.Method.t;
  target : string;
  path : string;
  request_headers : (string * string) list;
  status : Proffer.Status.t;
  response_content_type : string option;
  cache_status : string option;
  body_size : int;
  duration_us : int;
}

let globalize_string (s : string @ local) =
  Bytes.unsafe_to_string (Bytes.of_string s)

let rec globalize_event_headers
    (headers : (string * string) list @ local) =
  match headers with
  | [] -> []
  | (name, value) :: tl ->
      (globalize_string name, globalize_string value)
      :: globalize_event_headers tl

let globalize_event_option (value : string option @ local) =
  match value with
  | None -> None
  | Some value -> Some (globalize_string value)

let globalize_event (event : event @ local) : event =
  {
    remote_addr = globalize_string event.remote_addr;
    meth = event.meth;
    target = globalize_string event.target;
    path = globalize_string event.path;
    request_headers = globalize_event_headers event.request_headers;
    status = event.status;
    response_content_type = globalize_event_option event.response_content_type;
    cache_status = globalize_event_option event.cache_status;
    body_size = event.body_size;
    duration_us = event.duration_us;
  }

(* [Httpz.parse] is told the fill level as an [int16#], so a buffer filled to
   all 32768 bytes would present itself as a negative length. One byte is
   held back rather than the parse limit lowered, because the same bound
   governs the body: a request that does not fit here gets 413. *)
let read_capacity = min Httpz.buffer_size 32767

(* [Httpz.Res] offsets are signed 16-bit and its stores are unchecked, so the
   whole response head must stay under 32768 bytes. Handler headers are
   unbounded, hence the separate lower bound below. *)
let write_buffer_size = 32768
let body_chunk_size = 65536
let max_header_block = 30000

exception Headers_too_large
exception Trailers_require_http_1_1

(* Httpz returns fields in reverse arrival order. This pass restores wire order
   so first-value lookups preserve request semantics. *)
(* Request strings are built in the request's region straight out of the
   parse buffer. *)
let[@zero_alloc] own_span buf (span : Httpz.Span.t) = exclave_
  Httpz.Span.to_string_local buf span

let[@zero_alloc] own_body buf off len = exclave_
  own_span buf (Httpz.Span.make ~off:(i16 off) ~len:(i16 len))

let[@zero_alloc] rec block_of_headers_rev buf (acc : Proffer.Headers.t @ local)
    (hs : Httpz.Header.t list @ local) = exclave_
  match hs with
  | [] -> acc
  | h :: tl ->
      let value = own_span buf h.Httpz.Header.value in
      let local_ field =
        match h.Httpz.Header.name with
        | H.Other ->
            Proffer.Headers.other_local
              (own_span buf h.Httpz.Header.name_span)
              value
        | known -> Proffer.Headers.h_local known value
      in
      let local_ acc = field :: acc in
      let local_ result = block_of_headers_rev buf acc tl in
      result

let[@zero_alloc] block_of_headers buf (hs : Httpz.Header.t list @ local) = exclave_
  block_of_headers_rev buf [] hs

let redacted = "<redacted>"

(* An access log is not a place for credentials, and [on_event] is where a
   whole request block leaves the server. The handler still sees the real
   values; only the copy handed to telemetry is scrubbed. *)
let[@zero_alloc] rec same_ascii_caseless_from a b i =
  i = String.length a
  || (Char.equal
        (Char.lowercase_ascii (String.unsafe_get a i))
        (String.unsafe_get b i)
     && same_ascii_caseless_from a b (i + 1))

let[@zero_alloc] same_ascii_caseless a b =
  String.length a = String.length b && same_ascii_caseless_from a b 0

let[@zero_alloc] is_secret_field name spelling =
  match name with
  | H.Authorization | H.Proxy_authorization | H.Cookie -> true
  | _ ->
      same_ascii_caseless spelling "proxy-authorization"
      || same_ascii_caseless spelling "if"
      || same_ascii_caseless spelling "lock-token"

let[@zero_alloc] rec event_headers (block : Proffer.Headers.t @ local) = exclave_
  match block with
  | [] -> []
  | { Proffer.Headers.name; spelling; value } :: tl ->
      let value = if is_secret_field name spelling then redacted else value in
      let local_ tl = event_headers tl in
      (spelling, value) :: tl

let[@zero_alloc] rec field_or_null (block : Proffer.Headers.t @ local) name =
  exclave_
  match block with
  | [] -> Null
  | field :: tl ->
      if Proffer.Headers.same_name field.Proffer.Headers.name name
      then This field.Proffer.Headers.value
      else field_or_null tl name

(* [call_event] runs under a guard, where a raise counts as an ordinary path
   for the checker, so it is assumed clean on every path. *)
let[@inline never][@zero_alloc assume strict] call_event f
    (event : event @ local) =
  f event
let[@inline never][@zero_alloc assume] call_error f exn = f exn

(* The connection record erases the monotonic clock's type. Sampling Eio's
   external clock and converting its Mtime value may allocate, so that effect
   is explicitly outside the server response-path allocation contract at this
   one boundary, just like erased I/O callbacks. The closure returns [float#],
   so no additional boxed timestamp crosses back into the response path. *)
let[@inline never][@zero_alloc assume] monotonic_now now = now ()

let[@zero_alloc] emit_event
    now
    (on_event : (event @ local -> unit) option)
    on_error
    (t0 : float#)
    ~addr_str
    ~meth
    ~(target : string @ local)
    (request_block : Proffer.Headers.t @ local)
    ~(path : string @ local)
    ~(content_type : string or_null @ local)
    ~(cache : string or_null @ local)
    ~status
    ~body_size
  =
  match on_event with
  | None -> ()
  | Some f ->
      let elapsed = F64.sub (monotonic_now now) t0 in
      let elapsed = if F64.compare elapsed #0. < 0 then #0. else elapsed in
      let us = F64.mul elapsed (F64.of_int 1_000_000) in
      let local_ request_headers = event_headers request_block in
      let local_ response_content_type =
        match content_type with
        | Null -> None
        | This value -> stack_ (Some value)
      in
      let local_ cache_status =
        match cache with
        | Null -> None
        | This value -> stack_ (Some value)
      in
      let local_ event =
        stack_
          {
            remote_addr = addr_str;
            meth;
            target;
            path;
            request_headers;
            status;
            response_content_type;
            cache_status;
            body_size;
            duration_us = F64.to_int us;
          }
      in
      (try call_event f event with exn -> call_error on_error exn)

let addr_string (addr : Eio.Net.Sockaddr.stream) =
  match addr with
  | `Tcp (ip, port) -> Format.asprintf "%a:%d" Eio.Net.Ipaddr.pp ip port
  | `Unix path -> Printf.sprintf "unix:%S" path

(* Closures erase the flow and clock type parameters from connection state. *)
type conn = {
  transport : Proffer.Req.transport;
  now : unit -> float#;
  read : float# -> Cstruct.t -> int;
  write : Cstruct.t list -> unit;
  shutdown : unit -> unit;
  read_buf : bytes;
  read_cs : Cstruct.t;
  write_buf : bytes;
  head_cs : Cstruct.t;
  body_cs : Cstruct.t;
  chunk_buf : bytes;
  chunk_cs : Cstruct.t;
  mutable sink : Proffer.Body.Sink.t or_null;
  mutable read_len : int;
  mutable handoff_off : int;
  mutable keep_alive : bool;
  mutable body_written : int64#;
  mutable body_chunked : bool;
  (* The declared Content-Length of the response body being streamed. The sink
     refuses to put the first byte beyond it on the wire. *)
  (* A negative budget means the response declared no length. Keeping it
     unboxed avoids copying a local [int64 option] out of the outcome. *)
  mutable body_budget : int64#;
}

(* A negative read result denotes a deadline. The connection is discarded if
   a concurrent socket read wins that race and consumes bytes. *)
let create_conn flow ~mono_clock ~config ~transport =
  let started = Eio.Time.Mono.now mono_clock in
  let now () =
    F64.mul
      (F64.of_float
         (Mtime.Span.to_float_ns
            (Mtime.span started (Eio.Time.Mono.now mono_clock))))
      #1e-9
  in
  let write_timeout = Eio.Time.Timeout.seconds mono_clock (Duration.to_f config.write_timeout) in
  {
    transport;
    now;
    read =
      (fun deadline cs ->
        let remaining = F64.to_float (F64.max #0. (F64.sub deadline (now ()))) in
        try
          Eio.Time.Timeout.run_exn
            (Eio.Time.Timeout.seconds mono_clock remaining) (fun () ->
              Eio.Flow.single_read flow cs)
        with Eio.Time.Timeout -> -1);
    write =
      (fun bufs ->
        Eio.Time.Timeout.run_exn write_timeout (fun () ->
            Eio.Flow.write flow bufs));
    shutdown = (fun () -> Eio.Flow.shutdown flow `All);
    read_buf = Bytes.create Httpz.buffer_size;
    read_cs = Cstruct.create Httpz.buffer_size;
    write_buf = Bytes.create write_buffer_size;
    head_cs = Cstruct.create write_buffer_size;
    body_cs = Cstruct.create body_chunk_size;
    chunk_buf = Bytes.create 32;
    chunk_cs = Cstruct.create 32;
    sink = Null;
    read_len = 0;
    handoff_off = 0;
    keep_alive = true;
    body_written = I64.of_int 0;
    body_chunked = false;
    body_budget = I64.of_int (-1);
  }

type read_status = Read_ok | Read_eof | Read_timeout | Read_buffer_full

(* Eio reads through Cstruct while Httpz parses bytes. This is the one input
   representation boundary, and it is outside the allocation check. *)
let[@inline never][@zero_alloc assume] read_boundary conn ~deadline =
  let cs =
    Cstruct.sub conn.read_cs conn.read_len (read_capacity - conn.read_len)
  in
  match conn.read deadline cs with
  | -1 -> -1
  | n ->
      Cstruct.blit_to_bytes cs 0 conn.read_buf conn.read_len n;
      n
  | exception End_of_file -> -2

let[@zero_alloc] read_more conn ~deadline : read_status =
  if conn.read_len >= read_capacity then Read_buffer_full
  else begin
    match read_boundary conn ~deadline with
    | -1 -> Read_timeout
    | -2 -> Read_eof
    | n ->
        conn.read_len <- conn.read_len + n;
        Read_ok
  end

let shift_buffer conn consumed =
  if consumed >= conn.read_len then conn.read_len <- 0
  else if consumed > 0 then begin
    Bytes.blit conn.read_buf consumed conn.read_buf 0 (conn.read_len -
      consumed);
    conn.read_len <- conn.read_len - consumed
  end

type length_mode = Known of int | Chunked | Omit

type connection_mode =
  | Normal of bool
  | Tunnel
  | Upgrade
  | Advertise_upgrade of bool

(* Cstruct values and the lists Eio consumes are the output-side
   representation boundary. Keeping them in these assumed functions leaves
   everything around them checked. *)
let[@inline never][@zero_alloc assume] cstruct_of_string s = Cstruct.of_string s
let[@inline never][@zero_alloc assume] cstruct_sub cs off len = Cstruct.sub cs off len
let[@inline never][@zero_alloc assume] write_one conn a = conn.write [ a ]
let[@inline never][@zero_alloc assume] write_two conn a b = conn.write [ a; b ]
let[@inline never][@zero_alloc assume] write_three conn a b c = conn.write [ a; b; c ]

(* Proffer's block arrives at [local], and httpz's writers take their strings
   at [local] too, so nothing is copied to write a response. *)
let[@zero_alloc] rec write_headers buf off
    (headers : Proffer.Headers.t @ local) =
  match headers with
  | [] -> off
  | { Proffer.Headers.name; spelling; value } :: rest ->
      if
        to_int off + String.length spelling + String.length value + 4
        > max_header_block
      then raise Headers_too_large;
      let off =
        match name with
        | H.Other ->
            St.write_header buf ~off spelling value
        | known -> St.write_header_name buf ~off known value
      in
      write_headers buf off rest

(* The head, copied out of [conn.write_buf] into [conn.head_cs] and valid until
   the next head or trailer section is built. That copy is what lets
   [write_final_chunk_with_trailers] reuse [conn.write_buf] after the head has
   reached the socket. Raises [Headers_too_large] before writing anything to
   the socket. *)
let[@zero_alloc] head_cstruct conn ~connection ~version ~status ~headers
    ~last_modified ~mode =
  let buf = conn.write_buf in
  let off = St.write_status_line buf ~off:(i16 0) status version in
  let off =
    if Proffer.Headers.mem headers H.Date then off
    else
      Httpz.Date.write_date_header buf ~off
        (F64.of_float (Unix.gettimeofday ()))
  in
  let off = write_headers buf off headers in
  let off =
    match last_modified with
    | None -> off
    | Some t -> Httpz.Date.write_last_modified buf ~off (F64.of_float t)
  in
  let off =
    match mode with
    | Known n -> St.write_content_length buf ~off n
    | Chunked -> St.write_transfer_encoding_chunked buf ~off
    | Omit -> off
  in
  let off =
    match connection with
    | Normal keep_alive -> St.write_connection buf ~off ~keep_alive
    | Tunnel -> off
    | Upgrade -> St.write_header_name buf ~off H.Connection "Upgrade"
    | Advertise_upgrade keep_alive ->
        St.write_header_name buf ~off H.Connection
          (if keep_alive then "Upgrade" else "Upgrade, close")
  in
  let off = St.write_crlf buf ~off in
  Cstruct.blit_from_bytes buf 0 conn.head_cs 0 (to_int off);
  cstruct_sub conn.head_cs 0 (to_int off)

let text_type = "text/plain; charset=utf-8"

let[@zero_alloc] send_error conn ~version ~status message =
  let local_ headers =
    Proffer.Headers.h_local H.Content_type text_type :: []
  in
  let head =
    head_cstruct conn ~connection:(Normal false) ~version ~status ~headers
      ~last_modified:None ~mode:(Known (String.length message))
  in
  write_two conn head (cstruct_of_string message)

(* Closing a stream socket with unread peer data can turn the close into a RST
   and erase the error response the client is trying to read. After writing a
   rejection, consume a bounded amount for a bounded time. Buffered bytes count
   against the limit even though discarding them needs no socket read. *)
let[@inline never][@zero_alloc assume] drain_rejected_boundary conn ~limit
    ~drained =
  let deadline = F64.add (conn.now ()) #0.25 in
  let mutable drained = drained in
  let mutable reading = true in
  while reading && drained < limit do
    let length = min (Cstruct.length conn.read_cs) (limit - drained) in
    match conn.read deadline (Cstruct.sub conn.read_cs 0 length) with
    | n when n > 0 -> drained <- drained + n
    | _ -> reading <- false
    | exception End_of_file | exception Eio.Io _ -> reading <- false
  done

let[@zero_alloc] drain_rejected conn =
  let limit = 64 * 1024 in
  let drained = if conn.read_len < limit then conn.read_len else limit in
  conn.read_len <- 0;
  drain_rejected_boundary conn ~limit ~drained

let[@zero_alloc] reject conn ~version status message =
  conn.keep_alive <- false;
  send_error conn ~version ~status message;
  drain_rejected conn

let[@zero_alloc] refuse_request conn ~version ~on_event ~on_error (t0 : float#)
    ~addr_str
    ~meth ~(target : string @ local) (request_block : Proffer.Headers.t @ local)
    status message =
  reject conn ~version status message;
  emit_event conn.now on_event on_error t0 ~addr_str ~meth ~target request_block
    ~path:"" ~content_type:Null ~cache:Null ~status
    ~body_size:(String.length message);
  `Close

let[@inline never][@zero_alloc assume] copy_string_to_body conn s off len =
  Cstruct.blit_from_string s off conn.body_cs 0 len

let[@inline never][@zero_alloc assume] copy_bytes_to_body conn b off len =
  Cstruct.blit_from_bytes b off conn.body_cs 0 len

let[@zero_alloc] write_parts conn before body after =
  match before, after with
  | Null, Null -> write_one conn body
  | This before, Null -> write_two conn before body
  | Null, This after -> write_two conn body after
  | This before, This after -> write_three conn before body after

let[@zero_alloc] rec write_string_range conn s ~off ~len ~before ~after =
  let cap = Cstruct.length conn.body_cs in
  let n = min cap len in
  copy_string_to_body conn s off n;
  let body = cstruct_sub conn.body_cs 0 n in
  let last = n = len in
  write_parts conn before body (if last then after else Null);
  if not last then
    write_string_range conn s ~off:(off + n) ~len:(len - n) ~before:Null
      ~after

let[@zero_alloc] write_through conn s ~before ~after =
  write_string_range conn s ~off:0 ~len:(String.length s) ~before ~after

let[@zero_alloc] rec write_bytes_range conn b ~off ~len ~before ~after =
  let cap = Cstruct.length conn.body_cs in
  let n = min cap len in
  copy_bytes_to_body conn b off n;
  let body = cstruct_sub conn.body_cs 0 n in
  let last = n = len in
  write_parts conn before body (if last then after else Null);
  if not last then
    write_bytes_range conn b ~off:(off + n) ~len:(len - n) ~before:Null
      ~after

let[@inline never][@zero_alloc assume] chunk_framing conn n =
  let hoff =
    to_int (St.write_chunk_header conn.chunk_buf ~off:(i16 0) ~size:n)
  in
  let foff = to_int (St.write_chunk_footer conn.chunk_buf ~off:(i16 hoff)) in
  Cstruct.blit_from_bytes conn.chunk_buf 0 conn.chunk_cs 0 foff;
  #( This (Cstruct.sub conn.chunk_cs 0 hoff),
     This (Cstruct.sub conn.chunk_cs hoff (foff - hoff)) )

let[@zero_alloc] framing conn n =
  if not conn.body_chunked then #(Null, Null)
  else chunk_framing conn n

let[@cold][@zero_alloc assume error] stream_overrun budget written =
  invalid_arg
    (Printf.sprintf
       "Proffer_httpz: streamed body declared %s bytes but wrote %s"
       (I64.to_string budget) (I64.to_string written))

(* A declared Content-Length is a promise the client frames the connection by,
   so an overrun must not reach the wire at all: bytes already sent cannot be
   retracted, and the excess would be read as the head of the next response. *)
let[@zero_alloc] charge conn len =
  let budget = conn.body_budget in
  if I64.compare budget (I64.of_int 0) >= 0 then begin
    let written = I64.add conn.body_written (I64.of_int len) in
    if I64.compare written budget > 0 then begin
      conn.keep_alive <- false;
      stream_overrun budget written
    end
  end

(* Ignore empty writes because a zero-length chunk terminates chunked
   framing. *)
let prepare_sink conn =
  let emit s =
    let n = String.length s in
    if n > 0 then begin
      charge conn n;
      let #(before, after) = framing conn n in
      write_through conn s ~before ~after;
      conn.body_written <- I64.add conn.body_written (I64.of_int n)
    end
  in
  let emit_sub b off len =
    if len > 0 then begin
      charge conn len;
      let #(before, after) = framing conn len in
      write_bytes_range conn b ~off ~len ~before ~after;
      conn.body_written <- I64.add conn.body_written (I64.of_int len)
    end
  in
  conn.sink <- This (Proffer.Backend.sink ~emit_sub emit)

let[@cold][@zero_alloc assume error] uninitialized_sink () =
  failwith "Proffer_httpz: uninitialized stream sink"

let[@zero_alloc] sink_for conn =
  match conn.sink with
  | This sink -> sink
  | Null -> uninitialized_sink ()

let[@inline never][@zero_alloc assume] call_stream write sink = write sink

let[@cold][@zero_alloc assume error] stream_underrun expected written =
  invalid_arg
    (Printf.sprintf
       "Proffer_httpz: streamed body declared %s bytes but wrote %s"
       (I64.to_string (I64.of_int64 expected))
       (I64.to_string written))

let[@inline never][@zero_alloc assume] write_final_chunk conn =
  let off = to_int (St.write_final_chunk conn.chunk_buf ~off:(i16 0)) in
  Cstruct.blit_from_bytes conn.chunk_buf 0 conn.chunk_cs 0 off;
  conn.write [ Cstruct.sub conn.chunk_cs 0 off ]

let[@zero_alloc] write_final_chunk_with_trailers conn
    (trailers : Proffer.Headers.t @ local) =
  match trailers with
  | [] -> write_final_chunk conn
  | _ :: _ ->
      let buf = conn.write_buf in
      let off = St.write_chunk_header buf ~off:(i16 0) ~size:0 in
      let off = write_headers buf off trailers in
      let off = St.write_crlf buf ~off in
      Cstruct.blit_from_bytes buf 0 conn.head_cs 0 (to_int off);
      write_one conn (cstruct_sub conn.head_cs 0 (to_int off))

let[@zero_alloc] rec check_trailers_size
    (trailers : Proffer.Headers.t @ local) size =
  match trailers with
  | [] -> ()
  | header :: rest ->
      let spelling = String.length header.Proffer.Headers.spelling in
      let value = String.length header.Proffer.Headers.value in
      if spelling > max_header_block - size - 4
         || value > max_header_block - size - 4 - spelling
      then raise Headers_too_large;
      let field_size = spelling + value + 4 in
      check_trailers_size rest (size + field_size)

let handoff_socket conn ~idle_timeout =
  let read b off len =
    if conn.handoff_off < conn.read_len then begin
      let n = min len (conn.read_len - conn.handoff_off) in
      Bytes.blit conn.read_buf conn.handoff_off b off n;
      conn.handoff_off <- conn.handoff_off + n;
      n
    end
    else
      (* Into the connection's own cstruct and then out to [b].
         [Cstruct.of_bytes] would allocate a fresh Bigarray and read into that,
         leaving the caller a byte count over whatever [b] already held. *)
      let cs = Cstruct.sub conn.read_cs 0 (min len (Cstruct.length conn.read_cs)) in
      match conn.read (F64.add (conn.now ()) (F64.of_float idle_timeout)) cs with
      | -1 -> raise Eio.Time.Timeout
      | n ->
          Cstruct.blit_to_bytes cs 0 b off n;
          n
      | exception End_of_file -> 0
  in
  let write b off len =
    write_bytes_range conn b ~off ~len ~before:Null ~after:Null
  in
  Proffer.Backend.socket ~read ~write ~shutdown:conn.shutdown

let[@inline never][@zero_alloc assume] call_handoff conn ~idle_timeout run =
  run (handoff_socket conn ~idle_timeout)

let[@zero_alloc] write_outcome conn ~keep_alive ~chunked ~version
    (o : Proffer.Backend.outcome @ local) =
  let { Proffer.Backend.status; headers; last_modified; body; content_length }
      = o
  in
  (match body with
   | Proffer.Backend.Stream { trailers = _ :: _ as trailers; _ } ->
       (* Three bytes for the zero chunk and two for the closing CRLF. Check
          before writing the response head so an oversized trailer block
          cannot fail after a partial response has reached the peer. *)
       check_trailers_size trailers 5
   | _ -> ());
  let head connection mode =
    head_cstruct conn ~connection ~version ~status ~headers ~last_modified
      ~mode
  in
  let response_connection =
    if Proffer.Headers.mem headers H.Upgrade
    then Advertise_upgrade keep_alive
    else Normal keep_alive
  in
  match body with
  | Proffer.Backend.Empty ->
      let mode =
        match content_length with
        | Some n -> Known (Int64.to_int n)
        | None -> Omit
      in
      write_one conn (head response_connection mode);
      0
  | Proffer.Backend.String s ->
      let n = String.length s in
      let head = head response_connection (Known n) in
      (* One write for the whole response rather than a slice at a time, so
         [write_timeout] bounds the response instead of each 64 KiB window. *)
      if n = 0 then write_one conn head
      else write_two conn head (cstruct_of_string s);
      n
  | Proffer.Backend.Stream { length; write; trailers } ->
      let mode =
        if chunked then Chunked
        else match length with Some n -> Known (Int64.to_int n) | None -> Omit
      in
      write_one conn (head response_connection mode);
      conn.body_written <- I64.of_int 0;
      conn.body_chunked <- chunked;
      conn.body_budget <-
        (match length with None -> I64.of_int (-1) | Some n -> I64.of_int64 n);
      call_stream write (sink_for conn);
      (* [Int64.equal] takes its arguments at global, and the declared length
         is read out of a local outcome. *)
      (match length with
      | Some expected
        when not
               (I64.equal (I64.of_int64 expected) conn.body_written) ->
          stream_underrun expected conn.body_written
      | _ -> ());
      if chunked then write_final_chunk_with_trailers conn trailers;
      I64.to_int conn.body_written
  | Proffer.Backend.Handoff { kind; run = _ } ->
      conn.keep_alive <- false;
      let connection =
        match kind with
        | Proffer.Body.Tunnel -> Tunnel
        | Proffer.Body.Upgrade _ -> Upgrade
      in
      write_one conn (head connection Omit);
      0

let continue_line = "HTTP/1.1 100 Continue\r\n\r\n"

(* [has_lf b ~from ~stop] and [head_complete b ~from ~stop] answer whether the
   bytes that just arrived can have changed a parser's answer. Both are what
   keep a head or a trailer section that trickles in one byte per packet from
   costing one whole rescan per byte. *)
let[@zero_alloc] rec has_lf_from b i stop =
  i < stop && (Char.equal (Bytes.unsafe_get b i) '\n' || has_lf_from b (i + 1) stop)

let[@zero_alloc] has_lf b ~from ~stop = has_lf_from b (max 0 from) stop

(* [Httpz.parse] reports [Complete] only once the head's terminating CRLF
   follows the CRLF of the line before it, so those four bytes are always
   present together in the buffer. *)
let[@zero_alloc] rec head_complete_from b i stop =
  i + 3 < stop
  && ((Char.equal (Bytes.unsafe_get b i) '\r'
       && Char.equal (Bytes.unsafe_get b (i + 1)) '\n'
       && Char.equal (Bytes.unsafe_get b (i + 2)) '\r'
       && Char.equal (Bytes.unsafe_get b (i + 3)) '\n')
     || head_complete_from b (i + 1) stop)

let[@zero_alloc] head_complete b ~from ~stop = head_complete_from b (max 0 from) stop
let continue_cs = Cstruct.of_string continue_line
let[@inline never][@zero_alloc assume] write_continue conn = conn.write [ continue_cs ]

(* Compact the unread suffix to [dst]. This is also what preserves a pipelined
   request after dechunking: once the handler consumes the decoded body, the
   next request begins exactly at that boundary. *)
let[@zero_alloc] compact_suffix conn ~src ~dst =
  let suffix = conn.read_len - src in
  if suffix > 0 then Bytes.blit conn.read_buf src conn.read_buf dst suffix;
  conn.read_len <- dst + suffix

(* Decode a chunked body in place. At every step the buffer is
   [head][decoded content][unparsed wire bytes], so completed framing is
   discarded as it is consumed and does not reduce the bounded body's useful
   capacity. Trailer fields are validated even though Proffer v1 does not
   expose them to handlers. *)
type body_status =
  | Body_ready
  | Body_expectation_failed
  | Body_too_large
  | Body_malformed
  | Body_timed_out
  | Body_incomplete

let[@inline][@zero_alloc] no_body status = #(status, 0, 0)

let[@zero_alloc] rec request_chunks conn ~deadline ~body_off decoded :
    #(body_status * int * int) =
  let #(status, chunk) =
    Httpz.Chunk.parse_with_limit conn.read_buf ~off:(i16 decoded)
      ~len:(i16 conn.read_len)
      ~max_chunk_size:Httpz.default_limits.#max_chunk_size
  in
  match status with
  | Httpz.Chunk.Complete ->
      let data_off = to_int chunk.#data_off in
      let data_len = to_int chunk.#data_len in
      let next_off = to_int chunk.#next_off in
      Bytes.blit conn.read_buf data_off conn.read_buf decoded data_len;
      let decoded = decoded + data_len in
      compact_suffix conn ~src:next_off ~dst:decoded;
      request_chunks conn ~deadline ~body_off decoded
  | Httpz.Chunk.Done ->
      request_trailers conn ~deadline ~body_off ~decoded
        (to_int chunk.#data_off)
  (* [parse_with_limit] reads the size line and then compares offsets, so
     resuming it once per arriving byte rescans no chunk content. *)
  | Httpz.Chunk.Partial ->
      (match read_more conn ~deadline with
      | Read_ok -> request_chunks conn ~deadline ~body_off decoded
      | Read_timeout -> no_body Body_timed_out
      | Read_eof -> no_body Body_incomplete
      | Read_buffer_full -> no_body Body_too_large)
  | Httpz.Chunk.Malformed -> no_body Body_malformed
  | Httpz.Chunk.Chunk_too_large -> no_body Body_too_large

and[@zero_alloc] request_trailers conn ~deadline ~body_off ~decoded trailer_off :
    #(body_status * int * int) =
  let #(status, end_off, _headers) =
    Httpz.Chunk.parse_trailers conn.read_buf ~off:(i16 trailer_off)
      ~len:(i16 conn.read_len)
      ~max_header_count:Httpz.default_limits.#max_header_count
  in
  match status with
  | Httpz.Chunk.Trailer_complete ->
      compact_suffix conn ~src:(to_int end_off) ~dst:decoded;
      #(Body_ready, body_off, decoded - body_off)
  (* [parse_trailers] rescans the whole section from [trailer_off], and every
     status it can still reach needs a line end, so bytes that close no line
     cannot change its answer and must not pay for a rescan. *)
  | Httpz.Chunk.Trailer_partial ->
      await_trailer_line conn ~deadline ~body_off ~decoded trailer_off
  | Httpz.Chunk.Trailer_malformed | Httpz.Chunk.Trailer_bare_cr ->
      no_body Body_malformed

and[@zero_alloc] await_trailer_line conn ~deadline ~body_off ~decoded trailer_off
    : #(body_status * int * int) =
  let from = conn.read_len in
  match read_more conn ~deadline with
  | Read_ok ->
      if has_lf conn.read_buf ~from ~stop:conn.read_len then
        request_trailers conn ~deadline ~body_off ~decoded trailer_off
      else await_trailer_line conn ~deadline ~body_off ~decoded trailer_off
  | Read_timeout -> no_body Body_timed_out
  | Read_eof -> no_body Body_incomplete
  | Read_buffer_full -> no_body Body_too_large

let[@zero_alloc] request_chunked conn ~deadline (req : Httpz.Req.t) =
  let body_off = to_int req.#body_off in
  request_chunks conn ~deadline ~body_off body_off

let[@zero_alloc] rec fill_body conn ~deadline ~body_off ~body_len ~body_end :
    #(body_status * int * int) =
  if conn.read_len >= body_end then #(Body_ready, body_off, body_len)
  else
    match read_more conn ~deadline with
    | Read_ok -> fill_body conn ~deadline ~body_off ~body_len ~body_end
    | Read_timeout -> no_body Body_timed_out
    | Read_eof | Read_buffer_full -> no_body Body_incomplete

(* An oversized body is refused from the head alone, so telling the client to
   send it first would only invite bytes that get 413 anyway. A chunked body
   has no declared size, so nothing can be checked before it arrives. *)
let[@zero_alloc] request_body conn ~deadline (req : Httpz.Req.t) =
  if req.#unsupported_expectation
  then no_body Body_expectation_failed
  else if req.#is_chunked
  then (
    if req.#expect_continue then write_continue conn;
    request_chunked conn ~deadline req)
  else
    let cl = I64.to_int req.#content_length in
    let cl = if cl < 0 then 0 else cl in
    let body_off = to_int req.#body_off in
    if body_off + cl > read_capacity
    then no_body Body_too_large
    else (
      if req.#expect_continue then write_continue conn;
      let body_end = body_off + cl in
      (* [body_end] is within [read_capacity], so [Read_buffer_full] cannot
         arise here and the arm in [fill_body] is defence in depth. *)
      fill_body conn ~deadline ~body_off ~body_len:cl ~body_end)

let[@zero_alloc] request_of_parsed ~transport buf (req : Httpz.Req.t)
    ~(target : string @ local) (headers : Proffer.Headers.t @ local)
    ~(body : string @ local) = exclave_
  let #(path, query) =
    if Httpz.Span.len req.#path = 0
       && (match req.#meth with
           | Httpz.Method.Connect -> true
           | Httpz.Method.Options -> String.equal target "*"
           | _ -> false)
    then #(target, "")
    else #((if Httpz.Span.len req.#path = 0 then "/" else own_span buf req.#path),
           own_span buf req.#query)
  in
  let local_ request = Proffer.Backend.request ~meth:req.#meth ~version:req.#version
    ~connection_upgrade:req.#connection_upgrade ~target ~path ~query headers ~body in
  Proffer.Backend.with_transport request transport

let write_response conn ~version ~idle_timeout
    ~(routed_path : string @ local) ~on_event ~on_error t0 ~addr_str ~meth
    ~(target : string @ local) (req_headers : Proffer.Headers.t @ local)
    (outcome : Proffer.Backend.outcome @ local) =
  let http_1_1 = version = Httpz.Version.Http_1_1 in
  let #(needs_chunked, has_trailers) =
    match outcome.Proffer.Backend.body with
    | Proffer.Backend.Stream
        { length = None; trailers = []; _ } ->
        #(true, false)
    | Proffer.Backend.Stream { trailers = _ :: _; _ } ->
        #(true, true)
    | _ -> #(false, false)
  in
  (* Without chunked encoding the only frame left for a body of
     unknown length is the end of the connection. *)
  let chunked = needs_chunked && http_1_1 in
  let local_ emit_outcome body_size =
    let content_type =
      field_or_null outcome.Proffer.Backend.headers H.Content_type
    in
    let cache =
      field_or_null outcome.Proffer.Backend.headers H.X_cache
    in
    let () =
      emit_event conn.now on_event on_error t0 ~addr_str ~meth ~target
        req_headers ~path:routed_path ~content_type ~cache
        ~status:outcome.Proffer.Backend.status ~body_size
    in
    ()
  in
  if needs_chunked && not chunked then conn.keep_alive <- false;
  match
    if has_trailers && not http_1_1 then
      raise Trailers_require_http_1_1
    else
      write_outcome conn ~keep_alive:conn.keep_alive ~chunked ~version
        outcome
  with
  | body_size ->
      emit_outcome body_size;
      (match outcome.Proffer.Backend.body with
       | Proffer.Backend.Handoff { run; _ } ->
           call_handoff conn ~idle_timeout run
       | _ -> ())
  | exception ((Headers_too_large | Trailers_require_http_1_1) as exn) ->
      call_error on_error exn;
      let message = "Internal Server Error\n" in
      reject conn ~version St.Internal_server_error message;
      emit_event conn.now on_event on_error t0 ~addr_str ~meth ~target
        req_headers ~path:routed_path ~content_type:Null ~cache:Null
        ~status:St.Internal_server_error
        ~body_size:(String.length message)
  | exception exn ->
      (* The response may be partially written and cannot be
         reused. *)
      conn.keep_alive <- false;
      raise exn


(* Streaming is selected only by an explicitly admitted endpoint. The
   ordinary buffered path retains its existing limits and allocation budget. *)
let[@inline never][@zero_alloc assume] admitted_request conn ~deadline
    ~idle_timeout ~addr_str ~on_event ~on_error ~t0 ~version ~meth
    ~(target : string @ local) (req_headers : Proffer.Headers.t @ local)
    (preq : Proffer.Req.t @ local) (req : Httpz.Req.t) accepted =
  let limit = Proffer.Backend.body_limit accepted in
  let declared = I64.to_int64 req.#content_length in
  let fail status = raise (Proffer.Req.Input.Rejected status) in
  let more () =
    match read_more conn ~deadline with
    | Read_ok -> ()
    | Read_timeout -> fail St.Request_timeout
    | Read_eof -> fail St.Bad_request
    | Read_buffer_full -> fail St.Payload_too_large in
  let finished = ref (not req.#is_chunked && declared <= 0L) in
  let remaining = ref (max 0L declared) in
  let total = ref 0L in
  let phase = ref (if req.#is_chunked then `Header else `Data) in
  let rec trailers () =
    let #(status, stop, _) =
      Httpz.Chunk.parse_trailers conn.read_buf ~off:(i16 0)
        ~len:(i16 conn.read_len)
        ~max_header_count:Httpz.default_limits.#max_header_count in
    match status with
    | Httpz.Chunk.Trailer_complete ->
        shift_buffer conn (to_int stop);
        finished := true
    | Httpz.Chunk.Trailer_partial -> more (); trailers ()
    | _ -> fail St.Bad_request in
  let rec read bytes ~off ~len =
    if !finished then 0
    else match !phase with
    | `Header ->
        let max_chunk_size =
          Int64.to_int (min (Int64.of_int max_int) (Int64.sub limit !total)) in
        let #(status, size, start) =
          Httpz.Chunk.parse_header conn.read_buf ~off:(i16 0)
            ~len:(i16 conn.read_len) ~max_chunk_size in
        (match status with
        | Httpz.Chunk.Complete ->
            shift_buffer conn (to_int start);
            remaining := Int64.of_int size;
            phase := `Data
        | Httpz.Chunk.Done ->
            shift_buffer conn (to_int start);
            trailers ()
        | Httpz.Chunk.Partial -> more ()
        | Httpz.Chunk.Chunk_too_large -> fail St.Payload_too_large
        | Httpz.Chunk.Malformed -> fail St.Bad_request);
        read bytes ~off ~len
    | `Delimiter ->
        if conn.read_len < 2 then (more (); read bytes ~off ~len)
        else if Bytes.get conn.read_buf 0 <> '\r'
             || Bytes.get conn.read_buf 1 <> '\n' then fail St.Bad_request
        else begin
          shift_buffer conn 2;
          phase := `Header;
          read bytes ~off ~len
        end
    | `Data ->
        if !remaining = 0L then
          if req.#is_chunked then begin
            phase := `Delimiter;
            read bytes ~off ~len
          end else (finished := true; 0)
        else if conn.read_len = 0 then (more (); read bytes ~off ~len)
        else
          let n = min len (min conn.read_len
            (Int64.to_int (min !remaining (Int64.of_int max_int)))) in
          Bytes.blit conn.read_buf 0 bytes off n;
          shift_buffer conn n;
          remaining := Int64.sub !remaining (Int64.of_int n);
          total := Int64.add !total (Int64.of_int n);
          if !total > limit then fail St.Payload_too_large;
          if not req.#is_chunked && !remaining = 0L then finished := true;
          n in
  let input = Proffer.Backend.input read in
  let local_ request = Proffer.Backend.with_input preq input in
  let local_ write outcome =
    let () = write_response conn ~version ~idle_timeout
      ~routed_path:(Proffer.Req.path preq) ~on_event ~on_error t0
      ~addr_str ~meth ~target req_headers outcome in () in
  (try
    if req.#unsupported_expectation then fail St.Expectation_failed;
    if declared > limit then fail St.Payload_too_large;
    shift_buffer conn (to_int req.#body_off);
    if req.#expect_continue then write_continue conn;
    Proffer.Backend.handle_accepted ~on_error accepted request write
  with
  | Proffer.Req.Input.Rejected status ->
      ignore (refuse_request conn ~version ~on_event ~on_error t0
        ~addr_str ~meth ~target req_headers status "Request body rejected\n")
  | exn ->
      Proffer.Backend.close_input input;
      conn.keep_alive <- false;
      raise exn);
  Proffer.Backend.close_input input;
  if not !finished then conn.keep_alive <- false;
  if conn.keep_alive then `Continue else `Close

let[@zero_alloc] handle_request conn ~deadline ~idle_timeout ~addr_str ~site
    ~env ~on_event ~on_error =
  let buf = conn.read_buf in
  let #(status, req, headers) =
    Httpz.parse buf ~len:(i16 conn.read_len) ~limits:Httpz.default_limits
  in
  match status with
  | Httpz.Buf_read.Complete ->
      let t0 =
        match on_event with
        | None -> #0.
        | Some _ -> monotonic_now conn.now
      in
      let version = req.#version in
      conn.keep_alive <- req.#keep_alive;
      let meth = req.#meth in
      let target = own_span buf req.#target in
      let req_headers = block_of_headers buf headers in
      let preq = request_of_parsed ~transport:conn.transport buf req ~target req_headers ~body:"" in
      let local_ write_admission outcome =
        conn.keep_alive <- false;
        let () = write_response conn ~version ~idle_timeout
          ~routed_path:(Proffer.Req.path preq) ~on_event ~on_error t0
          ~addr_str ~meth ~target req_headers outcome in () in
      (match Proffer.Backend.admit ~on_error site env preq write_admission with
      | Proffer.Backend.Responded -> `Close
      | Proffer.Backend.Accepted accepted ->
          let result = admitted_request conn ~deadline ~idle_timeout ~addr_str
            ~on_event ~on_error ~t0 ~version ~meth ~target req_headers
            preq req accepted in result
      | Proffer.Backend.Ordinary ->
      (* Rejections before routing have no routed path or response metadata.
         Telemetry must not decide the fate of a response already written, so
         a failing callback is reported and dropped. *)
      (match request_body conn ~deadline req with
      | #(Body_expectation_failed, _, _) ->
          let result =
            refuse_request conn ~version ~on_event ~on_error t0 ~addr_str
              ~meth ~target req_headers St.Expectation_failed
              "Expectation Failed\n"
          in
          result
      | #(Body_too_large, _, _) ->
          let result =
            refuse_request conn ~version ~on_event ~on_error t0 ~addr_str
              ~meth ~target req_headers St.Payload_too_large
              "Payload Too Large\n"
          in
          result
      | #(Body_malformed, _, _) ->
          let result =
            refuse_request conn ~version ~on_event ~on_error t0 ~addr_str
              ~meth ~target req_headers St.Bad_request "Bad Request\n"
          in
          result
      (* The request line parsed, so this timeout has a method and a target
         to report and the client is still in the exchange. *)
      | #(Body_timed_out, _, _) ->
          let result =
            refuse_request conn ~version ~on_event ~on_error t0 ~addr_str
              ~meth ~target req_headers St.Request_timeout "Request Timeout\n"
          in
          result
      | #(Body_incomplete, _, _) ->
          conn.keep_alive <- false;
          `Close
      | #(Body_ready, body_off, body_len) ->
          let body =
            if body_len = 0 then "" else own_body buf body_off body_len
          in
          let preq = request_of_parsed ~transport:conn.transport buf req ~target req_headers ~body in
          let routed_path = Proffer.Req.path preq in
          let consumed = body_off + body_len in
          conn.handoff_off <- consumed;
          let local_ write : Proffer.Backend.writer = fun outcome ->
            write_response conn ~version ~idle_timeout ~routed_path ~on_event
              ~on_error t0 ~addr_str ~meth ~target req_headers outcome
          in
          let () =
            Proffer.Backend.handle_unboxed ~on_error
              ~now:(F64.of_float (Unix.gettimeofday ())) site env preq write
          in
          shift_buffer conn consumed;
          if conn.keep_alive then `Continue else `Close))
  | Httpz.Buf_read.Partial -> `Need_more
  | failure ->
      let status, message =
        match failure with
        | Httpz.Buf_read.Headers_too_large ->
            St.Request_header_fields_too_large, "Request Header Fields Too Large\n"
        | Httpz.Buf_read.Content_length_overflow ->
            St.Payload_too_large, "Payload Too Large\n"
        | Httpz.Buf_read.Uri_too_long -> St.Uri_too_long, "URI Too Long\n"
        (* RFC 9112 section 6.1 requires 501 for unsupported transfer coding. *)
        | Httpz.Buf_read.Unsupported_method
        | Httpz.Buf_read.Unsupported_transfer_encoding ->
            St.Not_implemented, "Not Implemented\n"
        | _ -> St.Bad_request, "Bad Request\n"
      in
      reject conn ~version:Httpz.Version.Http_1_1 status message;
      `Close

(* Three deadlines bound a connection. A newly accepted connection gets the
   short first-byte timeout; a keep-alive connection gets the longer idle
   timeout between requests. From the first byte, the whole request head and
   body must arrive within [request_timeout], since a request that trickles in
   is the shape a slowloris takes. *)
let handle_connection conn ~config ~addr_str ~site ~env ~on_event ~on_error =
  (* Reached only while the head is still incomplete ([Buf_read.Partial]), so
     it is always the head, not a declared body, that did not fit. *)
  let too_large () =
    reject conn ~version:Httpz.Version.Http_1_1
      St.Request_header_fields_too_large "Request Header Fields Too Large\n"
  in
  (* No access event is possible until a request line provides a target. *)
  let timed_out () =
    reject conn ~version:Httpz.Version.Http_1_1 St.Request_timeout
      "Request Timeout\n"
  in
  (* Entered only with an empty buffer, so [Read_buffer_full] is unreachable
     here and its arm is defence in depth. *)
  let rec await_request timeout =
    match read_more conn ~deadline:(F64.add (conn.now ()) (F64.of_float timeout)) with
    | Read_eof | Read_timeout -> ()
    | Read_buffer_full -> too_large ()
    | Read_ok ->
        serve (F64.add (conn.now ()) (F64.of_float (Duration.to_f config.request_timeout)))
  and serve deadline =
    match
      handle_request conn ~deadline ~idle_timeout:(Duration.to_f config.idle_timeout) ~addr_str
        ~site ~env ~on_event ~on_error
    with
    | `Close -> ()
    | `Continue ->
        (* Bytes left over are a pipelined request, whose own clock starts
           here rather than when the request before it began. *)
        if conn.read_len = 0 then await_request (Duration.to_f config.idle_timeout)
        else serve (F64.add (conn.now ()) (F64.of_float (Duration.to_f config.request_timeout)))
    (* [Httpz.parse] rescans the whole head, so a head arriving one byte per
       packet would otherwise cost one rescan per byte. Only the bytes that
       complete the head can turn [Partial] into [Complete]; a malformed head
       is answered once they arrive, or by the request timeout if they never
       do. *)
    | `Need_more -> await_head deadline
  and await_head deadline =
    let from = conn.read_len - 3 in
    match read_more conn ~deadline with
    | Read_eof -> ()
    | Read_timeout -> timed_out ()
    | Read_buffer_full -> too_large ()
    | Read_ok ->
        if head_complete conn.read_buf ~from ~stop:conn.read_len then
          serve deadline
        else await_head deadline
  in
  await_request (Duration.to_f config.first_byte_timeout)

let default_on_listening ~secure : Eio.Net.Sockaddr.stream -> unit = function
  | `Tcp (ip, port) ->
      let host =
        if ip = Eio.Net.Ipaddr.V4.loopback || ip = Eio.Net.Ipaddr.V6.loopback
        then "localhost"
        else Format.asprintf "%a" Eio.Net.Ipaddr.pp ip
      in
      let scheme = if secure then "https" else "http" in
      Printf.printf "Running at %s://%s:%d\n%!" scheme host port
  | `Unix path -> Printf.printf "Running on the Unix socket %S\n%!" path

let default_on_error exn = prerr_endline (Printexc.to_string exn)

(* [on_error] runs in the connection fibre, whose failure would fail the
   server switch and close the listening socket, so a raising callback would
   cost the whole server. Report both exceptions the way the default callback
   reports one, and carry on. *)
let protect on_error exn =
  try on_error exn
  with secondary ->
    prerr_endline (Printexc.to_string exn);
    prerr_endline (Printexc.to_string secondary)

let with_http_flow ~mono_clock ~config tls raw fn =
  match tls with
  | None -> fn raw
  | Some wrap ->
      let flow =
        match Eio.Time.Timeout.run_exn
            (Eio.Time.Timeout.seconds mono_clock (Duration.to_f config.first_byte_timeout))
            (fun () -> wrap raw) with
        | flow -> flow
        | exception ex ->
            Httpz_tls.close ~clock:mono_clock raw;
            raise ex
      in
      Fun.protect ~finally:(fun () -> Httpz_tls.close ~clock:mono_clock flow)
        (fun () -> fn flow)

let serve ~sw ~net ~mono_clock ~addr ~config ~tls ~on_listening ~on_event
    ~on_error ~stop ~env site =
  let sock = Eio.Net.listen net ~sw ~backlog:config.backlog ~reuse_addr:true addr in
  (* The caller's switch may outlive this invocation, so release the listener
     on return and on callback failure. *)
  Fun.protect ~finally:(fun () -> Eio.Net.close sock) @@ fun () ->
  on_listening (Eio.Net.listening_addr sock);
  let on_error = protect on_error in
  let handler flow client_addr =
    with_http_flow ~mono_clock ~config tls (flow :> Httpz_tls.flow) @@ fun flow ->
    let transport = if Option.is_some tls then Proffer.Req.Secure else
      match client_addr with
      | `Tcp (ip, _) when ip = Eio.Net.Ipaddr.V4.loopback ||
          ip = Eio.Net.Ipaddr.V6.loopback -> Proffer.Req.Loopback
      | _ -> Proffer.Req.Insecure in
    let conn = create_conn flow ~mono_clock ~config ~transport in
    prepare_sink conn;
    handle_connection conn ~config ~addr_str:(addr_string client_addr) ~site
      ~env ~on_event ~on_error
  in
  Eio.Net.run_server sock ~max_connections:config.max_connections ~on_error
    ?stop handler |> ignore

let run ?sw ?port ?addr ?(config = default_config) ?tls ?on_listening ?on_event
    ?(on_error = default_on_error) ?stop stdenv ~env site =
  let positive name n =
    if n <= 0 then
      invalid_arg
        (Printf.sprintf "Proffer_httpz.run: config.%s must be positive" name)
  in
  (* Compare the raw int64: a coerced or NaN-derived negative maps to ~292
     years under [Duration.to_f], which would silently disable the bound. *)
  let positive_timeout name (t : Duration.t) =
    if t <= 0L then
      invalid_arg
        (Printf.sprintf "Proffer_httpz.run: config.%s must be positive" name)
  in
  positive "backlog" config.backlog;
  positive "max_connections" config.max_connections;
  positive_timeout "first_byte_timeout" config.first_byte_timeout;
  positive_timeout "idle_timeout" config.idle_timeout;
  positive_timeout "request_timeout" config.request_timeout;
  positive_timeout "write_timeout" config.write_timeout;
  let addr =
    match addr, port with
    | Some addr, None -> addr
    | None, port ->
        `Tcp (Eio.Net.Ipaddr.V4.loopback, Option.value port ~default:8765)
    | Some _, Some _ ->
        invalid_arg "Proffer_httpz.run: pass either port or addr, not both"
  in
  let net = stdenv#net and mono_clock = stdenv#mono_clock in
  let on_listening =
    Option.value on_listening
      ~default:(default_on_listening ~secure:(Option.is_some tls))
  in
  let go sw =
    serve ~sw ~net ~mono_clock ~addr ~config ~tls ~on_listening ~on_event
      ~on_error ~stop ~env site
  in
  match sw with Some sw -> go sw | None -> Eio.Switch.run go
