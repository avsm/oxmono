(* Wire code, and nothing else. Everything a backend could get wrong twice
   lives in Proffer.Serve, so this file parses, frames and writes. *)

module I16 = Stdlib_stable.Int16_u
module I64 = Stdlib_upstream_compatible.Int64_u
module F64 = Stdlib_upstream_compatible.Float_u

let[@inline] i16 x = I16.of_int x
let[@inline] to_int x = I16.to_int x

type config = { backlog : int }

let default_config = { backlog = 64 }

module Log = struct
  type event = {
    remote_addr : string;
    meth : Proffer.Method.t;
    target : string;
    status : Proffer.Status.t;
    body_size : int;
    duration_us : int;
  }
end

(* [Httpz.parse] is told the fill level as an [int16#], so a buffer filled to
   all 32768 bytes would present itself as a negative length. One byte is
   held back rather than the parse limit lowered, because the same bound
   governs the body: a request that does not fit here gets 413. *)
let read_capacity = min Httpz.buffer_size 32767

(* [Httpz.Res] offsets are signed 16-bit and its stores are unchecked, so the
   whole response head must stay under 32768 bytes. Handler headers are
   unbounded, hence the separate lower bound below. *)
let write_buffer_size = 32768
let max_header_block = 30000

exception Headers_too_large

(** {1 Vocabulary} *)

let status_of (s : Proffer.Status.t) : Httpz.Res.status =
  match s with
  | `OK -> Httpz.Res.Success
  | `Created -> Httpz.Res.Created
  | `No_content -> Httpz.Res.No_content
  | `Moved_permanently -> Httpz.Res.Moved_permanently
  | `Found -> Httpz.Res.Found
  | `See_other -> Httpz.Res.See_other
  | `Not_modified -> Httpz.Res.Not_modified
  | `Bad_request -> Httpz.Res.Bad_request
  | `Forbidden -> Httpz.Res.Forbidden
  | `Not_found -> Httpz.Res.Not_found
  | `Method_not_allowed -> Httpz.Res.Method_not_allowed
  | `Conflict -> Httpz.Res.Conflict
  | `Length_required -> Httpz.Res.Length_required
  | `Payload_too_large -> Httpz.Res.Payload_too_large
  | `Unsupported_media_type -> Httpz.Res.Unsupported_media_type
  | `Internal_server_error -> Httpz.Res.Internal_server_error
  | `Not_implemented -> Httpz.Res.Not_implemented

(* httpz rejects a method it does not know at parse time, so [`Other] here
   only ever carries a method httpz names and proffer does not. *)
let meth_of (m : Httpz.Method.t) : Proffer.Method.t =
  match m with
  | Httpz.Method.Get -> `GET
  | Httpz.Method.Head -> `HEAD
  | Httpz.Method.Post -> `POST
  | Httpz.Method.Put -> `PUT
  | Httpz.Method.Delete -> `DELETE
  | Httpz.Method.Patch -> `PATCH
  | Httpz.Method.Options -> `OPTIONS
  | other -> `Other (Httpz.Method.to_string other)

let addr_string (addr : Eio.Net.Sockaddr.stream) =
  match addr with
  | `Tcp (ip, port) -> Format.asprintf "%a:%d" Eio.Net.Ipaddr.pp ip port
  | `Unix path -> path

(** {1 Connections} *)

(* The flow is held as two closures so that this record has no type
   parameter, and with it no [Eio.Net.stream_socket] constraint to thread
   through every function below. *)
type conn = {
  read : Cstruct.t -> int;
  write : Cstruct.t list -> unit;
  read_buf : bytes;  (** What the httpz parser reads. *)
  read_cs : Cstruct.t;  (** What Eio reads into, blitted to [read_buf]. *)
  write_buf : bytes;  (** The response head under construction. *)
  mutable read_len : int;
  mutable keep_alive : bool;
}

let create_conn flow =
  {
    read = (fun cs -> Eio.Flow.single_read flow cs);
    write = (fun bufs -> Eio.Flow.write flow bufs);
    read_buf = Bytes.create Httpz.buffer_size;
    read_cs = Cstruct.create Httpz.buffer_size;
    write_buf = Bytes.create write_buffer_size;
    read_len = 0;
    keep_alive = true;
  }

let read_more conn =
  if conn.read_len >= read_capacity then `Buffer_full
  else
    let cs =
      Cstruct.sub conn.read_cs conn.read_len (read_capacity - conn.read_len)
    in
    match conn.read cs with
    | n ->
        Cstruct.blit_to_bytes cs 0 conn.read_buf conn.read_len n;
        conn.read_len <- conn.read_len + n;
        `Ok n
    | exception End_of_file -> `Eof

let shift_buffer conn consumed =
  if consumed >= conn.read_len then conn.read_len <- 0
  else if consumed > 0 then begin
    Bytes.blit conn.read_buf consumed conn.read_buf 0 (conn.read_len - consumed);
    conn.read_len <- conn.read_len - consumed
  end

(** {1 Writing a response} *)

type length_mode =
  | Known of int
  | Chunked
  | Omit  (** No framing field, which is right for 304 and for a HEAD whose
              length the handler did not declare. *)

let rec write_headers buf off headers =
  match headers with
  | [] -> off
  | (name, value) :: rest ->
      if
        to_int off + String.length name + String.length value + 4
        > max_header_block
      then raise Headers_too_large;
      let off = Httpz.Res.write_header buf ~off name value in
      write_headers buf off rest

(* The head as one cstruct over [conn.write_buf], valid until the next call.
   Raises [Headers_too_large] before writing anything to the socket. *)
let head_cstruct conn ~keep_alive ~version ~status ~headers ~mode =
  let buf = conn.write_buf in
  let off = Httpz.Res.write_status_line buf ~off:(i16 0) status version in
  let off =
    Httpz.Date.write_date_header buf ~off (F64.of_float (Unix.gettimeofday ()))
  in
  let off = write_headers buf off headers in
  let off =
    match mode with
    | Known n -> Httpz.Res.write_content_length buf ~off n
    | Chunked -> Httpz.Res.write_transfer_encoding_chunked buf ~off
    | Omit -> off
  in
  let off = Httpz.Res.write_connection buf ~off ~keep_alive in
  let off = Httpz.Res.write_crlf buf ~off in
  Cstruct.of_bytes buf ~off:0 ~len:(to_int off)

let text_type = "text/plain; charset=utf-8"

let send_error conn ~version ~status message =
  let head =
    head_cstruct conn ~keep_alive:false ~version ~status:(status_of status)
      ~headers:[ ("Content-Type", text_type) ]
      ~mode:(Known (String.length message))
  in
  conn.write [ head; Cstruct.of_string message ]

(* [write_outcome conn ~keep_alive ~chunked ~version o] sends [o] and is the
   number of body bytes it wrote. *)
let write_outcome conn ~keep_alive ~chunked ~version o =
  let { Proffer.Serve.status; headers; body; content_length } = o in
  let status = status_of status in
  let head mode =
    head_cstruct conn ~keep_alive ~version ~status ~headers ~mode
  in
  match body with
  | `Empty ->
      let mode =
        match content_length with
        | Some n -> Known (Int64.to_int n)
        | None -> Omit
      in
      conn.write [ head mode ];
      0
  | `String s ->
      let n = String.length s in
      let head = head (Known n) in
      if n = 0 then conn.write [ head ]
      else conn.write [ head; Cstruct.of_string s ];
      n
  | `Stream (length, write) ->
      let written = ref 0 in
      let mode =
        match length with
        | Some n -> Known (Int64.to_int n)
        | None -> if chunked then Chunked else Omit
      in
      conn.write [ head mode ];
      let emit =
        if chunked then (
          let scratch = Bytes.create 32 in
          fun s ->
            let n = String.length s in
            (* A zero-length chunk ends the body, so an empty write is
               dropped rather than sent. *)
            if n > 0 then begin
              let off = Httpz.Res.write_chunk_header scratch ~off:(i16 0) ~size:n in
              let hdr = Cstruct.of_bytes scratch ~off:0 ~len:(to_int off) in
              let off = Httpz.Res.write_chunk_footer scratch ~off:(i16 0) in
              let ftr = Cstruct.of_bytes scratch ~off:0 ~len:(to_int off) in
              conn.write [ hdr; Cstruct.of_string s; ftr ];
              written := !written + n
            end)
        else fun s ->
          let n = String.length s in
          if n > 0 then begin
            conn.write [ Cstruct.of_string s ];
            written := !written + n
          end
      in
      write (Proffer.Serve.sink emit);
      if chunked then begin
        let scratch = Bytes.create 8 in
        let off = Httpz.Res.write_final_chunk scratch ~off:(i16 0) in
        conn.write [ Cstruct.of_bytes scratch ~off:0 ~len:(to_int off) ]
      end;
      !written

(** {1 Serving one request} *)

let continue_line = "HTTP/1.1 100 Continue\r\n\r\n"

(* [request_body conn req] brings the whole request body into [conn.read_buf]
   and is where it lies, or why it cannot be served.

   A chunked body is refused with 411 rather than dechunked: this backend
   serves forms and small uploads, and the parse buffer is the only place a
   body goes. For the same reason a declared length that cannot fit is 413,
   decided before a byte of it is read. *)
let request_body conn (req : Httpz.Req.t) =
  if req.#is_chunked then `Length_required
  else
    let cl = I64.to_int req.#content_length in
    let cl = if cl < 0 then 0 else cl in
    let body_off = to_int req.#body_off in
    if body_off + cl > read_capacity then `Too_large
    else begin
      if req.#expect_continue then conn.write [ Cstruct.of_string continue_line ];
      let body_end = body_off + cl in
      let rec fill () =
        if conn.read_len >= body_end then `Body (body_off, cl)
        else
          match read_more conn with
          | `Ok _ -> fill ()
          | `Eof | `Buffer_full -> `Incomplete
      in
      fill ()
    end

(* [handle_request conn ...] serves at most one request from the buffered
   bytes, and is `Continue, `Close or `Need_more. *)
let handle_request conn ~addr_str ~compiled ~env ~on_event ~on_error =
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
      let meth = meth_of req.#meth in
      let target = Httpz.Span.to_string buf req.#target in
      let req_headers = Httpz.Header.to_string_pairs_local buf headers in
      let emit status body_size =
        match on_event with
        | None -> ()
        | Some f ->
            let us = (Unix.gettimeofday () -. t0) *. 1_000_000. in
            f
              {
                Log.remote_addr = addr_str;
                meth;
                target;
                status;
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
      (match request_body conn req with
      | `Length_required -> refuse `Length_required "Length Required\n"
      | `Too_large -> refuse `Payload_too_large "Payload Too Large\n"
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
          let outcome = Proffer.Serve.handle ~on_error compiled env preq in
          let unknown_stream =
            match outcome.Proffer.Serve.body with
            | `Stream (None, _) -> true
            | _ -> false
          in
          (* Without chunked encoding the only frame left for a body of
             unknown length is the end of the connection. *)
          let chunked = unknown_stream && http_1_1 in
          if unknown_stream && not chunked then conn.keep_alive <- false;
          (match
             write_outcome conn ~keep_alive:conn.keep_alive ~chunked ~version
               outcome
           with
          | body_size -> emit outcome.Proffer.Serve.status body_size
          | exception Headers_too_large ->
              on_error Headers_too_large;
              conn.keep_alive <- false;
              let message = "Internal Server Error\n" in
              send_error conn ~version ~status:`Internal_server_error message;
              emit `Internal_server_error (String.length message));
          let consumed =
            if body_len > 0 then body_off + body_len else body_off
          in
          shift_buffer conn consumed;
          if conn.keep_alive then `Continue else `Close)
  | Httpz.Buf_read.Partial -> `Need_more
  | Httpz.Buf_read.Headers_too_large | Httpz.Buf_read.Content_length_overflow ->
      conn.keep_alive <- false;
      send_error conn ~version:Httpz.Version.Http_1_1
        ~status:`Payload_too_large "Payload Too Large\n";
      `Close
  | _ ->
      conn.keep_alive <- false;
      send_error conn ~version:Httpz.Version.Http_1_1 ~status:`Bad_request
        "Bad Request\n";
      `Close

let handle_connection conn ~addr_str ~compiled ~env ~on_event ~on_error =
  let rec after_read = function
    | `Eof -> ()
    | `Buffer_full ->
        conn.keep_alive <- false;
        send_error conn ~version:Httpz.Version.Http_1_1
          ~status:`Payload_too_large "Payload Too Large\n"
    | `Ok _ -> loop ()
  and loop () =
    if conn.read_len = 0 then after_read (read_more conn)
    else
      match
        handle_request conn ~addr_str ~compiled ~env ~on_event ~on_error
      with
      | `Continue -> loop ()
      | `Close -> ()
      | `Need_more -> after_read (read_more conn)
  in
  loop ()

(** {1 Entry point} *)

let run ~sw ~net ~addr ?(config = default_config) ?on_event ~on_error ~env
    compiled =
  let sock =
    Eio.Net.listen net ~sw ~backlog:config.backlog ~reuse_addr:true addr
  in
  Eio.Net.run_server sock ~on_error (fun flow client_addr ->
      let conn = create_conn flow in
      handle_connection conn ~addr_str:(addr_string client_addr) ~compiled ~env
        ~on_event ~on_error)
