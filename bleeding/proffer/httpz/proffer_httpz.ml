(* Wire code, and nothing else. Everything a backend could get wrong twice
   lives in Proffer.Backend, so this file parses, frames and writes. *)

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
  idle_timeout : float;
  request_timeout : float;
}

let default_config =
  {
    backlog = 64;
    max_connections = 512;
    idle_timeout = 75.;
    request_timeout = 15.;
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

(* [Httpz.parse] is told the fill level as an [int16#], so a buffer filled to
   all 32768 bytes would present itself as a negative length. One byte is
   held back rather than the parse limit lowered, because the same bound
   governs the body: a request that does not fit here gets 413. *)
let read_capacity = min Httpz.buffer_size 32767

(* [Httpz.Res] offsets are signed 16-bit and its stores are unchecked, so the
   whole response head must stay under 32768 bytes. Handler headers are
   unbounded, hence the separate lower bound below. *)
let write_buffer_size = 32768

(* How much of a body goes out per write. A response that fits stays one
   [writev] of head and body together, which is every page this serves bar the
   feeds and the largest listings. *)
let body_chunk_size = 65536
let max_header_block = 30000

exception Headers_too_large

(** {1 Vocabulary} *)

(* Proffer speaks httpz's methods, statuses and header names, so there is
   nothing to convert here any more. What used to live in this file was a
   thirty-one arm status match, a method match, and a header name mapped in
   both directions through a string. All of it was the cost of proffer having
   its own copy of a vocabulary httpz already owned.

   The parser accumulates fields as it goes, so it yields them in reverse
   arrival order. Accumulating onto a list reverses that again, so one pass
   produces the block in arrival order. Order matters because
   [Proffer.Headers.find] answers with the first match, so a repeated
   Authorization or If-None-Match must resolve to the one sent first.

   A name httpz recognises costs nothing: the constructor is carried over and
   the spelling is [canonical]'s constant. Only a field httpz does not name
   copies its spelling out of the read buffer. *)
let block_of_headers buf (hs : Httpz.Header.t list @ local) :
    Proffer.Headers.t =
  let rec go acc (hs : Httpz.Header.t list @ local) =
    match hs with
    | [] -> acc
    | h :: tl ->
        let value = Httpz.Span.to_string buf h.Httpz.Header.value in
        let field =
          match h.Httpz.Header.name with
          | H.Other ->
              Proffer.Headers.other
                (Httpz.Span.to_string buf h.Httpz.Header.name_span)
                value
          | known -> Proffer.Headers.h known value
        in
        go (field :: acc) tl
  in
  go [] hs

let addr_string (addr : Eio.Net.Sockaddr.stream) =
  match addr with
  | `Tcp (ip, port) -> Format.asprintf "%a:%d" Eio.Net.Ipaddr.pp ip port
  | `Unix path -> path

(** {1 Connections} *)

(* The flow and the clock are held as closures so that this record has no
   type parameter, and with it no [Eio.Net.stream_socket] or [Eio.Time.clock]
   constraint to thread through every function below. *)
type conn = {
  now : unit -> float;
  read : float -> Cstruct.t -> int;
      (** [read deadline cs] fills [cs], and is [-1] when the absolute time
          [deadline] passes first. Eio never reads zero bytes, so the
          sentinel cannot collide with a real result. A read that loses that
          race may have taken bytes off the socket, which costs nothing
          because every caller drops the connection on a timeout. *)
  write : Cstruct.t list -> unit;
  read_buf : bytes;  (** What the httpz parser reads. *)
  read_cs : Cstruct.t;  (** What Eio reads into, blitted to [read_buf]. *)
  write_buf : bytes;  (** The response head under construction. *)
  body_cs : Cstruct.t;
      (** Scratch for the body on its way to the socket, reused for every
          response on this connection. A string body used to go out as one
          [Cstruct.of_string], which mallocs and copies the whole body: the
          largest route this serves answers 3.3MB, so a burst of concurrent
          requests for it held a bigstring each. Writing through a fixed
          scratch bounds that at its length per connection. Eio's write
          completes before it returns, so the scratch is free to reuse on the
          next chunk. *)
  chunk_buf : bytes;
      (** Scratch for a chunk's size line and its trailing CRLF, reused like
          {!body_cs}. Building these per chunk cost two [Bytes.create] and two
          [Cstruct.of_bytes] on every 64KB of a streamed body. *)
  chunk_cs : Cstruct.t;
      (** The same bytes as a cstruct, so a chunk's framing reaches the socket
          through [Cstruct.sub], which is a record, rather than
          [Cstruct.of_bytes], which mallocs a bigstring per call. *)
  mutable sink : Proffer.Body.Sink.t option;
      (** The sink lent to a streamed body, built once for the connection
          rather than per response. Its closures read [body_written] and
          [body_chunked] out of this record instead of capturing a [ref] and
          a [bool] made per response, which is what lets them be built once.
          [None] until the first streamed response on this connection. *)
  mutable read_len : int;
  mutable keep_alive : bool;
  mutable body_written : int;  (** Body bytes sent for the response in hand. *)
  mutable body_chunked : bool;  (** Whether that response is framed chunked. *)
}

let create_conn flow ~clock =
  {
    now = (fun () -> Eio.Time.now clock);
    read =
      (fun deadline cs ->
        Eio.Fiber.first
          (fun () ->
            Eio.Time.sleep_until clock deadline;
            -1)
          (fun () -> Eio.Flow.single_read flow cs));
    write = (fun bufs -> Eio.Flow.write flow bufs);
    read_buf = Bytes.create Httpz.buffer_size;
    read_cs = Cstruct.create Httpz.buffer_size;
    write_buf = Bytes.create write_buffer_size;
    body_cs = Cstruct.create body_chunk_size;
    chunk_buf = Bytes.create 32;
    chunk_cs = Cstruct.create 32;
    sink = None;
    read_len = 0;
    keep_alive = true;
    body_written = 0;
    body_chunked = false;
  }

let read_more conn ~deadline =
  if conn.read_len >= read_capacity then `Buffer_full
  else
    let cs =
      Cstruct.sub conn.read_cs conn.read_len (read_capacity - conn.read_len)
    in
    match conn.read deadline cs with
    | -1 -> `Timeout
    | n ->
        Cstruct.blit_to_bytes cs 0 conn.read_buf conn.read_len n;
        conn.read_len <- conn.read_len + n;
        `Ok n
    | exception End_of_file -> `Eof

let shift_buffer conn consumed =
  if consumed >= conn.read_len then conn.read_len <- 0
  else if consumed > 0 then begin
    Bytes.blit conn.read_buf consumed conn.read_buf 0 (conn.read_len -
      consumed);
    conn.read_len <- conn.read_len - consumed
  end

(** {1 Writing a response} *)

type length_mode =
  | Known of int
  | Chunked
  | Omit  (** No framing field, which is right for 304 and for a HEAD whose
              length the handler did not declare. *)

(* Walks proffer's block where it lies. It arrives at [local], and httpz's
   writers take their strings at [local] too, so nothing is copied to write a
   response: no association list, and no name spelled as a string for a field
   both libraries name. [write_header_name] emits a known name from a
   precomputed byte sequence, which is why the mapping is worth doing here
   rather than falling back to [canonical]. *)
(* Walks proffer's block where it lies. It arrives at [local], and httpz's
   writers take their strings at [local] too, so nothing is copied to write a
   response: no association list, and no name spelled as a string for a field
   httpz names. [write_header_name] emits a known name from a precomputed byte
   sequence. *)
let rec write_headers buf off (headers : Proffer.Headers.t @ local) =
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

(* The head as one cstruct over [conn.write_buf], valid until the next call.
   Raises [Headers_too_large] before writing anything to the socket. *)
let head_cstruct conn ~keep_alive ~version ~status ~headers ~mode =
  let buf = conn.write_buf in
  let off = St.write_status_line buf ~off:(i16 0) status version in
  let off =
    Httpz.Date.write_date_header buf ~off (F64.of_float (Unix.gettimeofday ()))
  in
  let off = write_headers buf off headers in
  let off =
    match mode with
    | Known n -> St.write_content_length buf ~off n
    | Chunked -> St.write_transfer_encoding_chunked buf ~off
    | Omit -> off
  in
  let off = St.write_connection buf ~off ~keep_alive in
  let off = St.write_crlf buf ~off in
  Cstruct.of_bytes buf ~off:0 ~len:(to_int off)

let text_type = "text/plain; charset=utf-8"

let send_error conn ~version ~status message =
  let head =
    head_cstruct conn ~keep_alive:false ~version ~status
      ~headers:[ Proffer.Headers.h H.Content_type text_type ]
      ~mode:(Known (String.length message))
  in
  conn.write [ head; Cstruct.of_string message ]

(* [write_range conn ~len ~blit ~before ~after] sends [before], then [len]
   bytes produced by [blit], then [after]. The body goes through the
   connection's scratch a chunk at a time rather than as one
   [Cstruct.of_string], which used to malloc and copy the whole body: the
   largest route this serves answers 3.3MB, so a burst of concurrent requests
   for it held a bigstring each. A body that fits in the scratch is still a
   single [writev] carrying [before] with it. Eio's write completes before it
   returns, so the scratch is free to reuse.

   [blit src_off dst_off n] copies [n] bytes from offset [src_off] of whatever
   the source is into the scratch at [dst_off]. That is what lets a string
   body and a slice handed over by a streaming encoder share this path. *)
let write_range conn ~len ~blit ~before ~after =
  let cap = Cstruct.length conn.body_cs in
  let first = min cap len in
  blit 0 0 first;
  let tail = if first = len then after else [] in
  conn.write (before @ (Cstruct.sub conn.body_cs 0 first :: tail));
  let rec rest off =
    if off < len then begin
      let k = min cap (len - off) in
      blit off 0 k;
      let tail = if off + k = len then after else [] in
      conn.write (Cstruct.sub conn.body_cs 0 k :: tail);
      rest (off + k)
    end
  in
  rest first

let write_through conn s ~before ~after =
  write_range conn ~len:(String.length s)
    ~blit:(fun src dst n -> Cstruct.blit_from_string s src conn.body_cs dst n)
    ~before ~after

let write_through_bytes conn b ~off ~len ~before ~after =
  write_range conn ~len
    ~blit:(fun src dst n ->
      Cstruct.blit_from_bytes b (off + src) conn.body_cs dst n)
    ~before ~after

(* A chunk's size line and trailing CRLF go through the connection's own
   scratch. Built per chunk, as they were, this cost two [Bytes.create] and
   two [Cstruct.of_bytes] for every 64KB of a streamed body. The header and
   the footer do not overlap in the buffer, so one buffer serves both. *)
let framing conn n =
  if not conn.body_chunked then ([], [])
  else begin
    let hoff = to_int (St.write_chunk_header conn.chunk_buf ~off:(i16 0)
                         ~size:n) in
    let foff = to_int (St.write_chunk_footer conn.chunk_buf ~off:(i16 hoff)) in
    Cstruct.blit_from_bytes conn.chunk_buf 0 conn.chunk_cs 0 foff;
    ([ Cstruct.sub conn.chunk_cs 0 hoff ],
     [ Cstruct.sub conn.chunk_cs hoff (foff - hoff) ])
  end

(* [sink_for conn] is the sink lent to a streamed body, made once for the
   connection. The emitters read the response's framing and byte count out of
   [conn] rather than closing over values made per response, which is what
   lets them, and the sink record holding them, be built once rather than on
   every streamed response.

   A zero-length write is dropped rather than sent, because a zero-length
   chunk is what ends a chunked body. *)
let sink_for conn =
  match conn.sink with
  | Some k -> k
  | None ->
      let emit s =
        let n = String.length s in
        if n > 0 then begin
          let before, after = framing conn n in
          write_through conn s ~before ~after;
          conn.body_written <- conn.body_written + n
        end
      in
      let emit_sub b off len =
        if len > 0 then begin
          let before, after = framing conn len in
          write_through_bytes conn b ~off ~len ~before ~after;
          conn.body_written <- conn.body_written + len
        end
      in
      let k = Proffer.Backend.sink ~emit_sub emit in
      conn.sink <- Some k;
      k

(* [write_outcome conn ~keep_alive ~chunked ~version o] sends [o] and is the
   number of body bytes it wrote. *)
let write_outcome conn ~keep_alive ~chunked ~version
    (o : Proffer.Backend.outcome @ local) =
  let { Proffer.Backend.status; headers; body; content_length } = o in
  let head mode =
    head_cstruct conn ~keep_alive ~version ~status ~headers ~mode
  in
  match body with
  | Proffer.Backend.Empty ->
      let mode =
        match content_length with
        | Some n -> Known (Int64.to_int n)
        | None -> Omit
      in
      conn.write [ head mode ];
      0
  | Proffer.Backend.String s ->
      let n = String.length s in
      let head = head (Known n) in
      if n = 0 then conn.write [ head ]
      else write_through conn s ~before:[ head ] ~after:[];
      n
  | Proffer.Backend.Stream { length; write } ->
      let mode =
        match length with
        | Some n -> Known (Int64.to_int n)
        | None -> if chunked then Chunked else Omit
      in
      conn.write [ head mode ];
      conn.body_written <- 0;
      conn.body_chunked <- chunked;
      write (sink_for conn);
      if chunked then begin
        let off = to_int (St.write_final_chunk conn.chunk_buf ~off:(i16 0)) in
        Cstruct.blit_from_bytes conn.chunk_buf 0 conn.chunk_cs 0 off;
        conn.write [ Cstruct.sub conn.chunk_cs 0 off ]
      end;
      conn.body_written

(** {1 Serving one request} *)

let continue_line = "HTTP/1.1 100 Continue\r\n\r\n"

(* [request_body conn ~deadline req] brings the whole request body into
   [conn.read_buf] and is where it lies, or why it cannot be served.

   A chunked body is refused with 411 rather than dechunked: this backend
   serves forms and small uploads, and the parse buffer is the only place a
   body goes. For the same reason a declared length that cannot fit is 413,
   decided before a byte of it is read. *)
let request_body conn ~deadline (req : Httpz.Req.t) =
  if req.#is_chunked then `Length_required
  else
    let cl = I64.to_int req.#content_length in
    let cl = if cl < 0 then 0 else cl in
    let body_off = to_int req.#body_off in
    if body_off + cl > read_capacity then `Too_large
    else begin
      if req.#expect_continue then conn.write [ Cstruct.of_string
        continue_line ];
      let body_end = body_off + cl in
      let rec fill () =
        if conn.read_len >= body_end then `Body (body_off, cl)
        else
          match read_more conn ~deadline with
          | `Ok _ -> fill ()
          | `Timeout -> `Timed_out
          | `Eof | `Buffer_full -> `Incomplete
      in
      fill ()
    end

(* [handle_request conn ~deadline ...] serves at most one request from the
   buffered bytes, and is `Continue, `Close or `Need_more. [deadline] is the
   absolute time by which the rest of this request must arrive. *)
let handle_request conn ~deadline ~addr_str ~compiled ~env ~on_event ~on_error =
  let buf = conn.read_buf in
  let #(status, req, headers) =
    Httpz.parse buf ~len:(i16 conn.read_len) ~limits:Httpz.default_limits
  in
  match status with
  | Httpz.Buf_read.Complete ->
      let t0 = Unix.gettimeofday () in
      let version = req.#version in
      let http_1_1 =
        match version with
        | Httpz.Version.Http_1_1 -> true
        | Httpz.Version.Http_1_0 -> false
      in
      conn.keep_alive <- req.#keep_alive;
      let meth = req.#meth in
      let target = Httpz.Span.to_string buf req.#target in
      let req_headers = block_of_headers buf headers in
      (* A request refused before it is routed has no path and no response of
         the handler's making, so those fields default to nothing. The
         request fields are known from the parse, so every event carries
         them. *)
      let emit ?(path = "") ?content_type ?cache status body_size =
        match on_event with
        | None -> ()
        | Some f ->
            let us = (Unix.gettimeofday () -. t0) *. 1_000_000. in
            f
              {
                remote_addr = addr_str;
                meth;
                target;
                path;
                (* Only an event carries the fields as an association list,
                   so the copy is paid for by the site that asked for one. *)
                request_headers = Proffer.Headers.to_list req_headers;
                status;
                response_content_type = content_type;
                cache_status = cache;
                body_size;
                duration_us = int_of_float us;
              }
      in
      let refuse status message =
        conn.keep_alive <- false;
        send_error conn ~version ~status message;
        emit status (String.length message);
        `Close
      in
      (match request_body conn ~deadline req with
      | `Length_required ->
          refuse St.Length_required "Length Required\n"
      | `Too_large -> refuse St.Payload_too_large "Payload Too Large\n"
      (* The request line parsed, so this timeout has a method and a target
         to report and the client is still in the exchange. *)
      | `Timed_out -> refuse St.Request_timeout "Request Timeout\n"
      | `Incomplete ->
          (* The client stopped mid-body. Nothing it would read is left to
             send, so drop the connection. *)
          conn.keep_alive <- false;
          `Close
      | `Body (body_off, body_len) ->
          let body =
            if body_len = 0 then "" else Bytes.sub_string buf body_off body_len
          in
          let preq =
            Proffer.Req.v ~meth ~target ~headers:req_headers ~body ()
          in
          let path = Proffer.Req.path preq in
          (* The outcome reaches the writer at [local] and is written from
             inside [handle], so nothing about the response is ever a heap
             value here either. *)
          let local_ write : Proffer.Backend.writer =
           fun outcome ->
            let field name =
              Proffer.Headers.find outcome.Proffer.Backend.headers name
            in
            let unknown_stream =
              match outcome.Proffer.Backend.body with
              | Proffer.Backend.Stream { length = None; _ } -> true
              | _ -> false
            in
            (* Without chunked encoding the only frame left for a body of
               unknown length is the end of the connection. *)
            let chunked = unknown_stream && http_1_1 in
            if unknown_stream && not chunked then conn.keep_alive <- false;
            match
              write_outcome conn ~keep_alive:conn.keep_alive ~chunked ~version
                outcome
            with
            | body_size ->
                emit ~path
                  ?content_type:(field H.Content_type)
                  ?cache:(field H.X_cache)
                  outcome.Proffer.Backend.status body_size
            | exception Headers_too_large ->
                on_error Headers_too_large;
                conn.keep_alive <- false;
                let message = "Internal Server Error\n" in
                send_error conn ~version
                  ~status:St.Internal_server_error message;
                (* The handler's response never reached the wire, so none of it
                   is what was served. *)
                emit ~path St.Internal_server_error (String.length
                  message)
          in
          let () = Proffer.Backend.handle ~on_error compiled env preq write in
          let consumed =
            if body_len > 0 then body_off + body_len else body_off
          in
          shift_buffer conn consumed;
          if conn.keep_alive then `Continue else `Close)
  | Httpz.Buf_read.Partial -> `Need_more
  | Httpz.Buf_read.Headers_too_large | Httpz.Buf_read.Content_length_overflow ->
      conn.keep_alive <- false;
      send_error conn ~version:Httpz.Version.Http_1_1
        ~status:St.Payload_too_large "Payload Too Large\n";
      `Close
  | _ ->
      conn.keep_alive <- false;
      send_error conn ~version:Httpz.Version.Http_1_1
        ~status:St.Bad_request
        "Bad Request\n";
      `Close

(* Two deadlines bound a connection. One with nothing buffered is idle and
   waits [idle_timeout] for the first byte of a request. From that byte the
   whole request, head and body, must arrive within [request_timeout], since
   a request that trickles in is the shape a slowloris takes. *)
let handle_connection conn ~config ~addr_str ~compiled ~env ~on_event ~on_error
    =
  let too_large () =
    conn.keep_alive <- false;
    send_error conn ~version:Httpz.Version.Http_1_1
      ~status:St.Payload_too_large
      "Payload Too Large\n"
  in
  (* No part of a request line parsed, so there is no method or target for an
     event. The 408 still goes out, because a client that has begun sending
     is a client still reading. *)
  let timed_out () =
    conn.keep_alive <- false;
    send_error conn ~version:Httpz.Version.Http_1_1
      ~status:St.Request_timeout
      "Request Timeout\n"
  in
  let rec idle () =
    match read_more conn ~deadline:(conn.now () +. config.idle_timeout) with
    | `Eof | `Timeout -> ()
    | `Buffer_full -> too_large ()
    | `Ok _ -> serve (conn.now () +. config.request_timeout)
  and serve deadline =
    match
      handle_request conn ~deadline ~addr_str ~compiled ~env ~on_event ~on_error
    with
    | `Close -> ()
    | `Continue ->
        (* Bytes left over are a pipelined request, whose own clock starts
           here rather than when the request before it began. *)
        if conn.read_len = 0 then idle ()
        else serve (conn.now () +. config.request_timeout)
    | `Need_more -> (
        match read_more conn ~deadline with
        | `Eof -> ()
        | `Timeout -> timed_out ()
        | `Buffer_full -> too_large ()
        | `Ok _ -> serve deadline)
  in
  idle ()

(** {1 Entry point} *)

let run ~sw ~net ~clock ~addr ?(config = default_config) ?on_listening
    ?on_event ~on_error ~env compiled =
  let sock =
    Eio.Net.listen net ~sw ~backlog:config.backlog ~reuse_addr:true addr
  in
  (match on_listening with
  | None -> ()
  | Some f -> f (Eio.Net.listening_addr sock));
  Eio.Net.run_server sock ~max_connections:config.max_connections ~on_error
    (fun flow client_addr ->
      let conn = create_conn flow ~clock in
      handle_connection conn ~config ~addr_str:(addr_string client_addr)
        ~compiled ~env ~on_event ~on_error)
