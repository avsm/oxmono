(* httpz_server.ml - Async static file server using httpz with zero-copy bigstring I/O *)

open Core
open Async
open Async.Deferred.Let_syntax

module I64 = Stdlib_upstream_compatible.Int64_u
module F64 = Stdlib_upstream_compatible.Float_u

let i16 = Httpz.Buf_write.i16
let to_int = Httpz.Buf_write.to_int

(* Res buffer size - 64KB for headers *)
let response_buffer_size = 65536

(* Connection state *)
type conn_state =
  { reader : Reader.t
  ; writer : Writer.t
  ; read_buf : Httpz.buffer  (* bytes for parsing *)
  ; read_bigbuf : Bigstring.t  (* bigstring for Async I/O *)
  ; write_buf : Httpz.buffer
  ; mutable read_len : int
  ; mutable keep_alive : bool
  (* Reusable arrays for range parsing *)
  ; ranges : Httpz.Range.byte_range array
  ; resolved : Httpz.Range.resolved array
  }

(* Create connection state *)
let create_conn reader writer =
  { reader
  ; writer
  ; read_buf = Bytes.create Httpz.buffer_size
  ; read_bigbuf = Bigstring.create Httpz.buffer_size
  ; write_buf = Bytes.create response_buffer_size
  ; read_len = 0
  ; keep_alive = true
  ; ranges = Array.create ~len:(to_int Httpz.Range.max_ranges) Httpz.Range.empty
  ; resolved = Array.create ~len:(to_int Httpz.Range.max_ranges) Httpz.Range.empty_resolved
  }
;;

(* Basic MIME type detection *)
let mime_type_of_path path =
  match Filename.split_extension path with
  | _, Some "html" | _, Some "htm" -> "text/html"
  | _, Some "css" -> "text/css"
  | _, Some "js" -> "application/javascript"
  | _, Some "json" -> "application/json"
  | _, Some "txt" -> "text/plain"
  | _, Some "md" -> "text/markdown"
  | _, Some "xml" -> "application/xml"
  | _, Some "png" -> "image/png"
  | _, Some "jpg" | _, Some "jpeg" -> "image/jpeg"
  | _, Some "gif" -> "image/gif"
  | _, Some "svg" -> "image/svg+xml"
  | _, Some "ico" -> "image/x-icon"
  | _, Some "pdf" -> "application/pdf"
  | _, Some "woff" -> "font/woff"
  | _, Some "woff2" -> "font/woff2"
  | _, Some "ttf" -> "font/ttf"
  | _, Some "ml" | _, Some "mli" -> "text/x-ocaml"
  | _, Some "c" | _, Some "h" -> "text/x-c"
  | _, Some "py" -> "text/x-python"
  | _, Some "sh" -> "text/x-shellscript"
  | _, Some "yaml" | _, Some "yml" -> "text/yaml"
  | _, Some "toml" -> "text/toml"
  | _ -> "application/octet-stream"
;;

(* Generate weak ETag from file stats: W/"mtime-size" *)
let generate_etag ~(mtime : float) ~(size : int64) : string =
  sprintf "W/\"%x-%Lx\"" (Float.to_int (mtime *. 1000.0)) size
;;

(* Server limits configuration *)
let server_limits = Httpz.default_limits

(* Get current time as unboxed float *)
let now () = F64.of_float (Unix.gettimeofday ())

(* Write common response headers *)
let write_common_headers buf ~off ~keep_alive =
  let off = Httpz.Date.write_date_header buf ~off (now ()) in
  let off = Httpz.Res.write_header_name buf ~off Httpz.Header_name.Server "httpz/0.1" in
  Httpz.Res.write_connection buf ~off ~keep_alive
;;

(* Write response headers for a full file response *)
let write_file_headers conn ~off status content_type file_size etag mtime version =
  let buf = conn.write_buf in
  let off = Httpz.Res.write_status_line buf ~off status version in
  let off = Httpz.Res.write_header_name buf ~off Httpz.Header_name.Content_type content_type in
  let off = Httpz.Res.write_content_length buf ~off (Int64.to_int_exn file_size) in
  let off = Httpz.Range.write_accept_ranges buf ~off in
  let off = Httpz.Res.write_header buf ~off "ETag" etag in
  let off = Httpz.Date.write_last_modified buf ~off (F64.of_float mtime) in
  let off = write_common_headers buf ~off ~keep_alive:conn.keep_alive in
  Httpz.Res.write_crlf buf ~off
;;

(* Write response headers for a partial content (206) response *)
let write_partial_headers conn ~off content_type ~start ~end_ ~total etag mtime version =
  let buf = conn.write_buf in
  let off = Httpz.Res.write_status_line buf ~off Httpz.Res.Partial_content version in
  let off = Httpz.Res.write_header_name buf ~off Httpz.Header_name.Content_type content_type in
  let content_length = Int64.(to_int_exn (end_ - start + 1L)) in
  let off = Httpz.Res.write_content_length buf ~off content_length in
  let off = Httpz.Range.write_content_range buf ~off
      ~start:(I64.of_int64 start) ~end_:(I64.of_int64 end_) ~total:(I64.of_int64 total) in
  let off = Httpz.Res.write_header buf ~off "ETag" etag in
  let off = Httpz.Date.write_last_modified buf ~off (F64.of_float mtime) in
  let off = write_common_headers buf ~off ~keep_alive:conn.keep_alive in
  Httpz.Res.write_crlf buf ~off
;;

(* Write 304 Not Modified response *)
let write_not_modified_headers conn ~off etag mtime version =
  let buf = conn.write_buf in
  let off = Httpz.Res.write_status_line buf ~off Httpz.Res.Not_modified version in
  let off = Httpz.Res.write_header buf ~off "ETag" etag in
  let off = Httpz.Date.write_last_modified buf ~off (F64.of_float mtime) in
  let off = write_common_headers buf ~off ~keep_alive:conn.keep_alive in
  Httpz.Res.write_crlf buf ~off
;;

(* Write 416 Range Not Satisfiable response *)
let write_range_not_satisfiable conn ~off total version =
  let buf = conn.write_buf in
  let off = Httpz.Res.write_status_line buf ~off Httpz.Res.Range_not_satisfiable version in
  let off = Httpz.Range.write_content_range_unsatisfiable buf ~off ~total:(I64.of_int64 total) in
  let off = write_common_headers buf ~off ~keep_alive:conn.keep_alive in
  Httpz.Res.write_crlf buf ~off
;;

(* Send error response *)
let send_error conn status message version =
  let buf = conn.write_buf in
  let off = Httpz.Res.write_status_line buf ~off:(i16 0) status version in
  let off = Httpz.Res.write_header_name buf ~off Httpz.Header_name.Content_type "text/plain" in
  let off = Httpz.Res.write_content_length buf ~off (String.length message) in
  let off = write_common_headers buf ~off ~keep_alive:conn.keep_alive in
  let off = Httpz.Res.write_crlf buf ~off in
  Writer.write_bytes conn.writer buf ~pos:0 ~len:(to_int off);
  Writer.write conn.writer message;
  Writer.flushed conn.writer
;;

(* Normalize path - remove .. and resolve to absolute within root *)
let normalize_path ~root request_path =
  let decoded = request_path in
  let parts = String.split decoded ~on:'/' in
  let rec resolve acc = function
    | [] -> List.rev acc
    | "" :: rest | "." :: rest -> resolve acc rest
    | ".." :: rest ->
      (match acc with
       | [] -> resolve [] rest
       | _ :: acc' -> resolve acc' rest)
    | part :: rest -> resolve (part :: acc) rest
  in
  let normalized = resolve [] parts in
  let relative = String.concat ~sep:"/" normalized in
  Filename.concat root relative
;;

(* File metadata for caching decisions *)
type file_meta =
  { size : int64
  ; mtime : float
  ; etag : string
  ; content_type : string
  }

(* Extracted/parsed header values for conditional requests and ranges.
   We parse while headers are in scope to avoid string round-trips. *)
type request_headers =
  { if_none_match : string option  (* Need string for ETag comparison *)
  ; range_count : int              (* 0 = no range, >0 = parsed count in conn.ranges *)
  }

(* Get file metadata *)
let get_file_meta file_path =
  let%map stats = Unix.stat file_path in
  let size = stats.size in
  let mtime = Time_float_unix.to_span_since_epoch stats.mtime |> Time_float.Span.to_sec in
  let etag = generate_etag ~mtime ~size in
  let content_type = mime_type_of_path file_path in
  { size; mtime; etag; content_type }
;;

(* Check If-None-Match header for conditional GET *)
let check_if_none_match etag if_none_match_str =
  match if_none_match_str with
  | None -> false (* No condition, proceed normally *)
  | Some value ->
    (* Handle "*" case *)
    if String.equal (String.strip value) "*" then true
    else
      (* Simple weak ETag comparison - extract value and compare *)
      let normalize_etag s =
        let s = String.strip s in
        if String.is_prefix s ~prefix:"W/" then String.chop_prefix_exn s ~prefix:"W/" else s
      in
      let our_value = normalize_etag etag in
      (* Check if any comma-separated etag matches *)
      let tags = String.split value ~on:',' in
      List.exists tags ~f:(fun tag ->
        let their_value = normalize_etag tag in
        String.equal our_value their_value
      )
;;


(* Send file with support for range requests and conditional GET *)
let send_file_with_meta conn ~file_path ~meta ~(req_headers : request_headers) ~version =
  let { size; mtime; etag; content_type } = meta in
  (* Check conditional GET: If-None-Match *)
  if check_if_none_match etag req_headers.if_none_match then (
    let off = write_not_modified_headers conn ~off:(i16 0) etag mtime version in
    Writer.write_bytes conn.writer conn.write_buf ~pos:0 ~len:(to_int off);
    Writer.flushed conn.writer
  )
  else if req_headers.range_count = 0 then (
    (* Full content response - no range requested *)
    let off = write_file_headers conn ~off:(i16 0) Httpz.Res.Success content_type size etag mtime version in
    Writer.write_bytes conn.writer conn.write_buf ~pos:0 ~len:(to_int off);
    (* Stream file contents *)
    let%bind fd = Unix.openfile file_path ~mode:[`Rdonly] in
    let%bind () = Writer.transfer conn.writer
      (Reader.pipe (Reader.create fd))
      (fun s -> Writer.write conn.writer s)
    in
    Writer.flushed conn.writer
  )
  else (
    (* Range request - evaluate ranges against file size *)
    let range_count = req_headers.range_count in
      (* Evaluate ranges against file size *)
      let #(result, _resolved_count) =
        Httpz.Range.evaluate conn.ranges ~count:(i16 range_count)
          ~resource_length:(I64.of_int64 size) conn.resolved
      in
      match result with
      | Httpz.Range.Full_content ->
        (* Treat as full content *)
        let off = write_file_headers conn ~off:(i16 0) Httpz.Res.Success content_type size etag mtime version in
        Writer.write_bytes conn.writer conn.write_buf ~pos:0 ~len:(to_int off);
        let%bind fd = Unix.openfile file_path ~mode:[`Rdonly] in
        let%bind () = Writer.transfer conn.writer
          (Reader.pipe (Reader.create fd))
          (fun s -> Writer.write conn.writer s)
        in
        Writer.flushed conn.writer
      | Httpz.Range.Not_satisfiable ->
        (* 416 Range Not Satisfiable *)
        conn.keep_alive <- false;
        let off = write_range_not_satisfiable conn ~off:(i16 0) size version in
        Writer.write_bytes conn.writer conn.write_buf ~pos:0 ~len:(to_int off);
        Writer.flushed conn.writer
      | Httpz.Range.Single_range | Httpz.Range.Multiple_ranges ->
        (* 206 Partial Content - serve first range *)
        let r = Array.get conn.resolved 0 in
        let start = I64.to_int64 r.#start in
        let end_ = I64.to_int64 r.#end_ in
        let range_len = Int64.(end_ - start + 1L) in
        let len = Int64.to_int_exn range_len in
        let off = write_partial_headers conn ~off:(i16 0) content_type ~start ~end_ ~total:size etag mtime version in
        Writer.write_bytes conn.writer conn.write_buf ~pos:0 ~len:(to_int off);
        (* Read the specific byte range *)
        let%bind contents = In_thread.run (fun () ->
          let fd = Core_unix.openfile file_path ~mode:[Core_unix.O_RDONLY] in
          let (_ : int64) = Core_unix.lseek fd start ~mode:Core_unix.SEEK_SET in
          let buf = Bytes.create len in
          let n = Core_unix.read fd ~buf ~pos:0 ~len in
          Core_unix.close fd;
          Bytes.sub buf ~pos:0 ~len:n)
        in
        Writer.write_bytes conn.writer contents;
        Writer.flushed conn.writer
  )
;;

(* Try to serve index.html from a directory *)
let serve_directory conn ~file_path ~req_headers ~version =
  let index_path = Filename.concat file_path "index.html" in
  let%bind index_status = Sys.file_exists index_path in
  match index_status with
  | `Yes ->
    let%bind meta = get_file_meta index_path in
    send_file_with_meta conn ~file_path:index_path ~meta ~req_headers ~version
  | `No | `Unknown ->
    send_error conn Httpz.Res.Not_found "Not Found" version
;;

(* Try to serve a regular file, checking it's within root *)
let serve_regular_file conn ~root_abs ~file_path ~req_headers ~version =
  let%bind result =
    Monitor.try_with (fun () ->
      let file_abs = Filename_unix.realpath file_path in
      if String.is_prefix file_abs ~prefix:root_abs
      then (
        let%map meta = get_file_meta file_path in
        Some (file_path, meta)
      )
      else return None)
  in
  match result with
  | Error _ -> send_error conn Httpz.Res.Not_found "Not Found" version
  | Ok None -> send_error conn Httpz.Res.Forbidden "Forbidden" version
  | Ok (Some (file_path, meta)) ->
    send_file_with_meta conn ~file_path ~meta ~req_headers ~version
;;

(* Serve a file *)
let serve_file conn ~root target_str req_headers version =
  let path = Option.value (Option.map ~f:fst (String.lsplit2 target_str ~on:'?')) ~default:target_str in
  let file_path = normalize_path ~root path in
  let root_abs = Filename_unix.realpath root in
  let%bind file_status = Sys.file_exists file_path in
  match file_status with
  | `No | `Unknown ->
    send_error conn Httpz.Res.Not_found "Not Found" version
  | `Yes ->
    let%bind is_dir = Sys.is_directory file_path in
    match is_dir with
    | `Yes -> serve_directory conn ~file_path ~req_headers ~version
    | `No | `Unknown -> serve_regular_file conn ~root_abs ~file_path ~req_headers ~version
;;

(* Read more data into buffer *)
let read_more conn =
  if conn.read_len >= Httpz.buffer_size
  then return `Buffer_full
  else (
    let available = Httpz.buffer_size - conn.read_len in
    let bss = Bigsubstring.create conn.read_bigbuf ~pos:conn.read_len ~len:available in
    let%map result = Reader.read_bigsubstring conn.reader bss in
    match result with
    | `Eof -> `Eof
    | `Ok n ->
      (* Copy from bigstring to bytes for parsing *)
      Bigstring.To_bytes.blit ~src:conn.read_bigbuf ~src_pos:conn.read_len
        ~dst:conn.read_buf ~dst_pos:conn.read_len ~len:n;
      conn.read_len <- conn.read_len + n;
      `Ok n)
;;

(* Shift buffer contents to remove processed data *)
let shift_buffer conn consumed =
  if consumed > 0 && consumed < conn.read_len
  then (
    let remaining = conn.read_len - consumed in
    Bytes.blit ~src:conn.read_buf ~src_pos:consumed ~dst:conn.read_buf ~dst_pos:0 ~len:remaining;
    Bigstring.blit ~src:conn.read_bigbuf ~src_pos:consumed ~dst:conn.read_bigbuf ~dst_pos:0 ~len:remaining;
    conn.read_len <- remaining)
  else if consumed >= conn.read_len
  then conn.read_len <- 0
;;

(* Handle one request on connection *)
let handle_request conn ~root =
  let buf = conn.read_buf in
  let len = conn.read_len in
  let len16 = i16 len in
  let #(status, req, headers) = Httpz.parse buf ~len:len16 ~limits:server_limits in
  let body_off = to_int req.#body_off in
  let version = req.#version in
  let target = req.#target in
  match status with
  | Httpz.Buf_read.Complete ->
    (* Extract/parse header values while still in scope (headers are local) *)
    let target_str = Httpz.Span.to_string buf target in
    let if_none_match =
      match Httpz.Header.find headers Httpz.Header_name.If_none_match with
      | None -> None
      | Some hdr -> Some (Httpz.Span.to_string buf hdr.value)
    in
    (* Parse Range header directly from buffer - no string round-trip *)
    let range_count =
      match Httpz.Header.find headers Httpz.Header_name.Range with
      | None -> 0
      | Some hdr ->
        let #(status, count) = Httpz.Range.parse buf hdr.value conn.ranges in
        match status with
        | Httpz.Range.Invalid -> 0
        | Httpz.Range.Valid -> to_int count
    in
    let req_headers = { if_none_match; range_count } in
    let body_span = Httpz.Req.body_span ~len:len16 req in
    let body_span_len = Httpz.Span.len body_span in
    let body_span_off = Httpz.Span.off body_span in
    if body_span_len = -1
    then return `Need_more
    else (
      conn.keep_alive <- req.#keep_alive;
      let%map () = serve_file conn ~root target_str req_headers version in
      let consumed =
        if body_span_len > 0 then body_span_off + body_span_len else body_off
      in
      shift_buffer conn consumed;
      if conn.keep_alive then `Continue else `Close)
  | Httpz.Buf_read.Partial -> return `Need_more
  | Httpz.Buf_read.Headers_too_large
  | Httpz.Buf_read.Content_length_overflow ->
    conn.keep_alive <- false;
    let%map () =
      send_error
        conn
        Httpz.Res.Payload_too_large
        "Payload Too Large"
        Httpz.Version.Http_1_1
    in
    `Close
  | Httpz.Buf_read.Bare_cr_detected
  | Httpz.Buf_read.Ambiguous_framing ->
    conn.keep_alive <- false;
    let%map () =
      send_error conn Httpz.Res.Bad_request "Bad Request" Httpz.Version.Http_1_1
    in
    `Close
  | Httpz.Buf_read.Missing_host_header ->
    conn.keep_alive <- false;
    let%map () =
      send_error conn Httpz.Res.Bad_request "Missing Host Header" Httpz.Version.Http_1_1
    in
    `Close
  | _ ->
    conn.keep_alive <- false;
    let%map () =
      send_error conn Httpz.Res.Bad_request "Bad Request" Httpz.Version.Http_1_1
    in
    `Close
;;

(* Send payload too large error and close connection *)
let send_payload_too_large conn =
  conn.keep_alive <- false;
  send_error conn Httpz.Res.Payload_too_large "Payload Too Large" Httpz.Version.Http_1_1
;;

(* Handle connection loop *)
let handle_connection conn ~root =
  let handle_read_result ~continue = function
    | `Eof -> return ()
    | `Buffer_full -> send_payload_too_large conn
    | `Ok _ -> continue ()
  in
  let rec loop () =
    if conn.read_len = 0 then
      read_more conn >>= handle_read_result ~continue:loop
    else
      let%bind req_result = handle_request conn ~root in
      match req_result with
      | `Continue -> loop ()
      | `Close -> return ()
      | `Need_more -> read_more conn >>= handle_read_result ~continue:loop
  in
  loop ()
;;

(* Handle a single client connection *)
let handle_client ~root addr reader writer =
  let conn = create_conn reader writer in
  let%map result = Monitor.try_with (fun () -> handle_connection conn ~root) in
  match result with
  | Ok () -> ()
  | Error exn ->
    let addr_str =
      match addr with
      | `Inet (host, port) -> sprintf "%s:%d" (Unix.Inet_addr.to_string host) port
      | `Unix path -> path
    in
    printf "[%s] Error: %s\n%!" addr_str (Exn.to_string exn)
;;

(* Run server *)
let run ~port ~root () =
  let where_to_listen = Tcp.Where_to_listen.of_port port in
  printf "httpz serving %s on http://localhost:%d/\n%!" root port;
  printf "  Supports: Range requests, ETag, If-None-Match\n%!";
  let%bind _server =
    Tcp.Server.create
      ~on_handler_error:`Raise
      ~backlog:128
      ~max_connections:10000
      where_to_listen
      (fun addr reader writer -> handle_client ~root addr reader writer)
  in
  Deferred.never ()
;;

(* Command-line interface *)
let command =
  Command.async
    ~summary:"Static file server using httpz with Range, ETag, and conditional request support"
    (Command.Param.map2
       (Command.Param.flag
          "-p"
          (Command.Param.optional_with_default 8080 Command.Param.int)
          ~doc:"PORT Port to listen on (default: 8080)")
       (Command.Param.flag
          "-d"
          (Command.Param.optional_with_default "." Command.Param.string)
          ~doc:"DIR Directory to serve (default: .)")
       ~f:(fun port root () -> run ~port ~root ()))
;;

let () = Command_unix.run command
