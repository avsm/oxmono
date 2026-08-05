(* Range tracking *)

type range = { start : int; stop : int }

let merge_range ranges new_range =
  (* Insert the new range and sweep to coalesce overlapping/adjacent intervals *)
  let merged = ref [] in
  let cur_start = ref new_range.start in
  let cur_stop = ref new_range.stop in
  let inserted = ref false in
  List.iter
    (fun r ->
      if !inserted then begin
        (* After insertion point, just append remaining *)
        merged := r :: !merged
      end else if r.stop < !cur_start then begin
        (* r is entirely before current; keep it *)
        merged := r :: !merged
      end else if r.start > !cur_stop then begin
        (* r is entirely after current; emit current, then r *)
        merged := r :: { start = !cur_start; stop = !cur_stop } :: !merged;
        inserted := true
      end else begin
        (* Overlap or adjacent: extend current *)
        cur_start := min !cur_start r.start;
        cur_stop := max !cur_stop r.stop
      end)
    ranges;
  if not !inserted then
    merged := { start = !cur_start; stop = !cur_stop } :: !merged;
  List.rev !merged

let is_range_cached ranges ~start ~len =
  let stop = start + len in
  List.exists (fun r -> r.start <= start && stop <= r.stop) ranges

let is_complete ranges ~total_len =
  match ranges with
  | [ r ] -> r.start = 0 && r.stop >= total_len
  | _ -> false

(* Metadata *)

type meta = {
  content_length : int;
  content_type : string;
  headers : (string * string) list;
  ranges : range list;
  complete : bool;
  status_code : int;
}

(* JSON type descriptors using jsont *)

let range_jsont : range Jsont.t =
  Jsont.Object.map ~kind:"Range" (fun start stop -> { start; stop })
  |> Jsont.Object.mem "start" Jsont.int ~enc:(fun r -> r.start)
  |> Jsont.Object.mem "stop" Jsont.int ~enc:(fun r -> r.stop)
  |> Jsont.Object.finish

let header_pair_jsont : (string * string) Jsont.t =
  Jsont.Object.map ~kind:"HeaderPair" (fun k v -> (k, v))
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun (k, _) -> k)
  |> Jsont.Object.mem "value" Jsont.string ~enc:(fun (_, v) -> v)
  |> Jsont.Object.finish

let meta_jsont : meta Jsont.t =
  Jsont.Object.map ~kind:"Meta"
    (fun content_length content_type headers ranges complete status_code ->
      { content_length; content_type; headers; ranges; complete; status_code })
  |> Jsont.Object.mem "content_length" Jsont.int
       ~enc:(fun m -> m.content_length)
  |> Jsont.Object.mem "content_type" Jsont.string
       ~enc:(fun m -> m.content_type)
  |> Jsont.Object.mem "headers" (Jsont.list header_pair_jsont)
       ~enc:(fun m -> m.headers)
       ~dec_absent:[]
  |> Jsont.Object.mem "ranges" (Jsont.list range_jsont)
       ~enc:(fun m -> m.ranges)
       ~dec_absent:[]
  |> Jsont.Object.mem "complete" Jsont.bool
       ~enc:(fun m -> m.complete)
       ~dec_absent:false
  |> Jsont.Object.mem "status_code" Jsont.int
       ~enc:(fun m -> m.status_code)
       ~dec_absent:200
  |> Jsont.Object.finish

(* Filesystem helpers *)

let ensure_cache_dirs ~(fs : Eio.Fs.dir_ty Eio.Path.t) ~cache_dir ~rel_path =
  let dir = Filename.dirname rel_path in
  if dir <> "." then begin
    let parts = String.split_on_char '/' dir in
    ignore (List.fold_left (fun acc part ->
      let p = acc ^ "/" ^ part in
      (try Eio.Path.mkdir ~perm:0o755 Eio.Path.(fs / p)
       with Eio.Io _ -> ());
      p
    ) cache_dir parts)
  end

let read_meta fs key =
  let path = key ^ ".meta" in
  match Eio.Path.load Eio.Path.(fs / path) with
  | contents ->
    (match Jsont_bytesrw.decode_string meta_jsont contents with
     | Ok meta -> Some meta
     | Error _msg -> None)
  | exception _exn -> None

let write_meta fs key meta =
  let path = key ^ ".meta" in
  match Jsont_bytesrw.encode_string ~format:Jsont.Indent meta_jsont meta with
  | Ok json_str ->
    Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / path) json_str
  | Error msg -> failwith ("write_meta: " ^ msg)

(* Cache file I/O *)

let cache_path ~host ~url_path =
  (* Strip leading slash from url_path, use host as top-level directory *)
  let trimmed =
    if String.length url_path > 0 && url_path.[0] = '/' then
      String.sub url_path 1 (String.length url_path - 1)
    else url_path
  in
  let path = if trimmed = "" then "index" else trimmed in
  host ^ "/" ^ path

let data_path key = key ^ ".data"

let write_data ~sw:_ (fs : Eio.Fs.dir_ty Eio.Path.t) key ~off data =
  let path = data_path key in
  Eio.Switch.run (fun file_sw ->
    let file = Eio.Path.open_out ~sw:file_sw ~create:(`If_missing 0o644)
      Eio.Path.(fs / path) in
    Eio.File.pwrite_all file
      ~file_offset:(Optint.Int63.of_int off)
      [Cstruct.of_string data])

let read_data ~sw:_ (fs : Eio.Fs.dir_ty Eio.Path.t) key ~off ~len =
  let path = data_path key in
  Eio.Switch.run (fun file_sw ->
    let file = Eio.Path.open_in ~sw:file_sw Eio.Path.(fs / path) in
    let buf = Cstruct.create len in
    Eio.File.pread_exact file
      ~file_offset:(Optint.Int63.of_int off)
      [buf];
    Cstruct.to_string buf)

(* Range header parsing *)

let parse_range_header s =
  (* Parse "bytes=START-END", "bytes=START-", or suffix "bytes=-N" *)
  let prefix = "bytes=" in
  let prefix_len = String.length prefix in
  if String.length s < prefix_len then None
  else if String.sub s 0 prefix_len <> prefix then None
  else begin
    let rest = String.sub s prefix_len (String.length s - prefix_len) in
    (* Check for suffix-byte-range: "bytes=-N" (last N bytes, RFC 7233) *)
    if String.length rest > 1 && rest.[0] = '-' then
      let suffix_str = String.sub rest 1 (String.length rest - 1) in
      match int_of_string_opt suffix_str with
      | Some n when n > 0 -> Some (`Suffix n)
      | _ -> None
    else
      match String.index_opt rest '-' with
      | None -> None
      | Some dash_pos ->
        let start_str = String.sub rest 0 dash_pos in
        let end_str =
          String.sub rest (dash_pos + 1)
            (String.length rest - dash_pos - 1)
        in
        (match int_of_string_opt start_str with
         | None -> None
         | Some start_val ->
           if end_str = "" then Some (`Range (start_val, None))
           else
             match int_of_string_opt end_str with
             | None -> None
             | Some end_val -> Some (`Range (start_val, Some end_val)))
  end

(* URL mapping *)

type url_map = {
  prefix : string;
  upstream : string;
  upstream_host : string;
}

let parse_map s =
  match String.index_opt s '=' with
  | None -> None
  | Some eq_pos ->
    let prefix = String.sub s 0 eq_pos in
    let upstream = String.sub s (eq_pos + 1) (String.length s - eq_pos - 1) in
    (* Extract host from upstream URL *)
    let host =
      (* Strip scheme: "https://host/path" -> "host/path" *)
      let after_scheme =
        match String.index_opt upstream ':' with
        | None -> upstream
        | Some colon_pos ->
          let rest = String.sub upstream (colon_pos + 1)
              (String.length upstream - colon_pos - 1) in
          (* Skip "//" after scheme *)
          if String.length rest >= 2
             && rest.[0] = '/' && rest.[1] = '/' then
            String.sub rest 2 (String.length rest - 2)
          else rest
      in
      (* Take up to first '/' *)
      match String.index_opt after_scheme '/' with
      | None -> after_scheme
      | Some slash_pos -> String.sub after_scheme 0 slash_pos
    in
    Some { prefix; upstream; upstream_host = host }

let find_map maps request_path =
  let rec try_maps = function
    | [] -> None
    | map :: rest ->
      let prefix_len = String.length map.prefix in
      if prefix_len <= String.length request_path
         && String.sub request_path 0 prefix_len = map.prefix then
        let suffix = String.sub request_path prefix_len
            (String.length request_path - prefix_len) in
        Some (map, suffix)
      else
        try_maps rest
  in
  try_maps maps

(* Content-Range header parsing *)

let parse_content_range s =
  (* Parse "bytes START-END/TOTAL" -> Some total *)
  let prefix = "bytes " in
  let prefix_len = String.length prefix in
  if String.length s < prefix_len then None
  else if String.sub s 0 prefix_len <> prefix then None
  else begin
    let rest = String.sub s prefix_len (String.length s - prefix_len) in
    match String.index_opt rest '/' with
    | None -> None
    | Some slash_pos ->
      let total_str = String.sub rest (slash_pos + 1)
          (String.length rest - slash_pos - 1) in
      if total_str = "*" then None
      else int_of_string_opt total_str
  end

(* Map header name strings to Httpz.Header_name.t.
   Only headers in this list can be forwarded to clients, since
   Httpz.Header_name.Other has no string payload. *)

let header_name_of_string s =
  match String.lowercase_ascii s with
  | "content-type" -> Some Httpz.Header_name.Content_type
  | "content-length" -> Some Httpz.Header_name.Content_length
  | "content-range" -> Some Httpz.Header_name.Content_range
  | "accept-ranges" -> Some Httpz.Header_name.Accept_ranges
  | "etag" -> Some Httpz.Header_name.Etag
  | "last-modified" -> Some Httpz.Header_name.Last_modified
  | "cache-control" -> Some Httpz.Header_name.Cache_control
  | "content-encoding" -> Some Httpz.Header_name.Content_encoding
  | "content-disposition" -> Some Httpz.Header_name.Content_disposition
  | "vary" -> Some Httpz.Header_name.Vary
  | "access-control-allow-origin" -> Some Httpz.Header_name.Access_control_allow_origin
  | _ -> None

(* Filter upstream response headers to the subset we cache and can forward *)
let filter_cacheable_headers resp_headers =
  List.filter_map (fun (name, value) ->
    let ln = String.lowercase_ascii name in
    match ln with
    | "content-type" | "etag" | "last-modified" | "cache-control"
    | "accept-ranges" | "content-encoding" | "content-disposition"
    | "vary" -> Some (ln, value)
    | _ -> None)
    (Requests.Headers.to_list resp_headers)

(* Build response headers from metadata *)

let cors_headers =
  [ (Httpz.Header_name.Access_control_allow_origin, "*");
    (Httpz.Header_name.Access_control_allow_methods, "GET, HEAD, OPTIONS");
    (Httpz.Header_name.Access_control_allow_headers, "Range") ]

(* Convert cached header pairs to httpz response headers.
   Excludes content-type and content-length since callers add those explicitly. *)
let resp_headers_of_meta meta =
  List.filter_map (fun (name, value) ->
    match header_name_of_string name with
    | None -> None
    | Some hn ->
      (* Skip headers that callers add explicitly *)
      if hn = Httpz.Header_name.Content_type
         || hn = Httpz.Header_name.Content_length then None
      else Some (hn, value))
    meta.headers

(* Upstream URL path portion for cache key computation *)

let url_path_of_upstream upstream =
  (* Extract path from "https://host/path" -> "/path" *)
  match String.index_opt upstream ':' with
  | None -> upstream
  | Some colon_pos ->
    let rest = String.sub upstream (colon_pos + 1)
        (String.length upstream - colon_pos - 1) in
    let after_scheme =
      if String.length rest >= 2 && rest.[0] = '/' && rest.[1] = '/' then
        String.sub rest 2 (String.length rest - 2)
      else rest
    in
    match String.index_opt after_scheme '/' with
    | None -> "/"
    | Some slash_pos ->
      String.sub after_scheme slash_pos
        (String.length after_scheme - slash_pos)

(* Response type returned by handle_request *)

type response = {
  status : Httpz.Res.status;
  resp_headers : Httpz_server.Route.resp_header list;
  body : Httpz_server.Route.body;
}

(* Handle HEAD requests *)

let handle_head ~(fs : Eio.Fs.dir_ty Eio.Path.t) ~session ~cp ~upstream_url =
  (* Try to serve from cached .meta *)
  match read_meta fs cp with
  | Some meta when meta.status_code >= 400 ->
    (* Negative cache: return the cached error status, not 200 *)
    let status = match Httpz.Res.status_of_int meta.status_code with
      | Some s -> s | None -> Httpz.Res.Not_found in
    { status; resp_headers = cors_headers;
      body = Httpz_server.Route.Empty }
  | Some meta ->
    let headers =
      (Httpz.Header_name.Content_type, meta.content_type)
      :: (Httpz.Header_name.Content_length, string_of_int meta.content_length)
      :: (Httpz.Header_name.Accept_ranges, "bytes")
      :: cors_headers
    in
    { status = Httpz.Res.Success; resp_headers = headers;
      body = Httpz_server.Route.Empty }
  | None ->
    (* Fetch upstream HEAD — only cache .meta for 2xx *)
    (try
       let resp = Requests.head session upstream_url in
       let status_code = Requests.Response.status_code resp in
       let status = match Httpz.Res.status_of_int status_code with
         | Some s -> s
         | None -> Httpz.Res.Bad_gateway
       in
       let ct = match Requests.Response.header_string "content-type" resp with
         | Some s -> s | None -> "application/octet-stream" in
       let cl = match Requests.Response.header_string "content-length" resp with
         | Some s -> (match int_of_string_opt s with Some n -> n | None -> 0)
         | None -> 0 in
       let header_pairs = filter_cacheable_headers (Requests.Response.headers resp) in
       (* Only persist .meta for successful responses *)
       if status_code >= 200 && status_code < 300 then begin
         let meta = {
           content_length = cl; content_type = ct;
           headers = header_pairs; ranges = []; complete = false;
           status_code;
         } in
         write_meta fs cp meta
       end;
       let headers =
         (Httpz.Header_name.Content_type, ct)
         :: (Httpz.Header_name.Content_length, string_of_int cl)
         :: (Httpz.Header_name.Accept_ranges, "bytes")
         :: cors_headers
       in
       { status; resp_headers = headers; body = Httpz_server.Route.Empty }
     with exn ->
       let msg = Printexc.to_string exn in
       Printf.eprintf "    upstream HEAD error: %s\n%!" msg;
       { status = Httpz.Res.Bad_gateway; resp_headers = cors_headers;
         body = Httpz_server.Route.String ("Upstream error: " ^ msg) })

(* Upstream connection: keeps a reference to the raw socket for closing *)
type upstream_conn = {
  flow : Eio.Flow.two_way_ty Eio.Resource.t;
  close : unit -> unit;
}

(* Connect to an upstream host, returning a two-way Eio flow and a close function.
   The connection lives on the provided switch but can be closed early via close(). *)

let connect_upstream ~sw ~(net : _ Eio.Net.t) ~host ~port ~is_https =
  let addrs = Eio.Net.getaddrinfo_stream net host
      ~service:(string_of_int port) in
  let socket = match addrs with
    | addr :: _ -> Eio.Net.connect ~sw net addr
    | [] -> failwith ("DNS resolution failed for: " ^ host)
  in
  let close_socket () =
    (try Eio.Resource.close socket with _ -> ())
  in
  if is_https then begin
    let authenticator = match Ca_certs.authenticator () with
      | Ok a -> a
      | Error _ -> fun ?ip:_ ~host:_ _certs -> Ok None
    in
    let tls_config = match Tls.Config.client ~authenticator () with
      | Ok c -> c
      | Error (`Msg msg) -> failwith ("TLS config error: " ^ msg)
    in
    let host' = match Domain_name.of_string host with
      | Ok dn -> (match Domain_name.host dn with Ok h -> Some h | Error _ -> None)
      | Error _ -> None
    in
    let tls_flow = Tls_eio.client_of_flow tls_config ?host:host' socket in
    let close () =
      (try Eio.Resource.close tls_flow with _ -> ())
    in
    { flow = (tls_flow :> Eio.Flow.two_way_ty Eio.Resource.t); close }
  end else
    { flow = (socket :> Eio.Flow.two_way_ty Eio.Resource.t); close = close_socket }

(* Parse a URL into (scheme, host, port, path) *)
let parse_url url =
  let uri = Uri.of_string url in
  let scheme = Option.value (Uri.scheme uri) ~default:"https" in
  let host = match Uri.host uri with Some h -> h | None -> failwith ("No host in URL: " ^ url) in
  let is_https = scheme = "https" in
  let port = match Uri.port uri with
    | Some p -> p
    | None -> if is_https then 443 else 80
  in
  let path = Uri.path_and_query uri in
  (host, port, is_https, path, uri)

(* Fetch full resource from upstream with streaming.
   For 2xx responses, returns a Stream body that tees data to cache.
   For non-2xx, buffers the (small) error body and returns String. *)

(* Fetch full resource from upstream with streaming.
   For 2xx responses, returns a Stream body that tees data to cache.
   For non-2xx, buffers the (small) error body and returns String.

   The upstream connection is opened on the outer sw but closed explicitly
   via conn.close after streaming completes (or immediately for non-2xx). *)

let fetch_and_cache_streaming ~sw ~net ~(fs : Eio.Fs.dir_ty Eio.Path.t) ~cp
    ~upstream_url ~verbose =
  let (host, port, is_https, _path, uri) = parse_url upstream_url in
  let conn = connect_upstream ~sw ~net ~host ~port ~is_https in
  let no_body_limit = Requests.Response_limits.make
      ~max_response_body_size:Int64.max_int () in
  let (status_code, resp_headers, stream_body) =
    Requests.Http_client.make_request_streaming
      ~limits:no_body_limit
      ~sw ~method_:`GET ~uri
      ~headers:Requests.Headers.empty
      ~body:Requests.Body.empty
      conn.flow
  in
  let content_type =
    match Requests.Headers.get `Content_type resp_headers with
    | Some ct -> ct
    | None -> "application/octet-stream"
  in
  let content_length =
    match Requests.Headers.get `Content_length resp_headers with
    | Some cl -> (match int_of_string_opt cl with Some n -> Some n | None -> None)
    | None -> None
  in
  let header_pairs = filter_cacheable_headers resp_headers in
  if verbose then
    Printf.printf "    upstream status %d, content-length: %s\n%!"
      status_code
      (match content_length with Some n -> string_of_int n | None -> "unknown");
  if status_code >= 200 && status_code < 300 then begin
    (* 2xx: stream body, tee to cache file *)
    let body = match stream_body with
      | `Stream source ->
        let total_written = ref 0 in
        let iter write_chunk =
          let buf = Cstruct.create 65536 in
          Eio.Switch.run (fun file_sw ->
            let cache_file = Eio.Path.open_out ~sw:file_sw ~create:(`If_missing 0o644)
              Eio.Path.(fs / data_path cp) in
            (try
               while true do
                 let n = Eio.Flow.single_read source buf in
                 let chunk = Cstruct.to_string (Cstruct.sub buf 0 n) in
                 Eio.File.pwrite_all cache_file
                   ~file_offset:(Optint.Int63.of_int !total_written)
                   [Cstruct.sub buf 0 n];
                 total_written := !total_written + n;
                 write_chunk chunk
               done
             with End_of_file -> ()));
          (* cache_file closed by file_sw, close upstream connection *)
          conn.close ();
          if verbose then
            Printf.printf "    streamed %d bytes\n%!" !total_written;
          let meta = {
            content_length = !total_written;
            content_type;
            headers = header_pairs;
            ranges = [ { start = 0; stop = !total_written } ];
            complete = true;
            status_code;
          } in
          write_meta fs cp meta
        in
        Httpz_server.Route.Stream { length = content_length; iter }
      | `String s ->
        conn.close ();
        let body_len = String.length s in
        write_data ~sw fs cp ~off:0 s;
        let meta = {
          content_length = body_len;
          content_type;
          headers = header_pairs;
          ranges = [ { start = 0; stop = body_len } ];
          complete = true;
          status_code;
        } in
        write_meta fs cp meta;
        Httpz_server.Route.String s
      | `None ->
        conn.close ();
        let meta = {
          content_length = 0;
          content_type;
          headers = header_pairs;
          ranges = [ { start = 0; stop = 0 } ];
          complete = true;
          status_code;
        } in
        write_meta fs cp meta;
        Httpz_server.Route.Empty
    in
    (content_type, body, status_code)
  end else begin
    (* Non-2xx: buffer small body, close connection, cache 4xx *)
    let body_str = match stream_body with
      | `Stream source ->
        let buf = Buffer.create 4096 in
        (try
           let tmp = Cstruct.create 4096 in
           while true do
             let n = Eio.Flow.single_read source tmp in
             Buffer.add_string buf (Cstruct.to_string (Cstruct.sub tmp 0 n))
           done;
           assert false
         with End_of_file -> Buffer.contents buf)
      | `String s -> s
      | `None -> ""
    in
    conn.close ();
    if status_code >= 400 && status_code < 500 then begin
      let meta = {
        content_length = 0;
        content_type;
        headers = header_pairs;
        ranges = [];
        complete = true;
        status_code;
      } in
      write_meta fs cp meta
    end;
    (content_type, Httpz_server.Route.String body_str, status_code)
  end

(* Fetch a range from upstream *)

let fetch_and_cache_range ~sw ~(fs : Eio.Fs.dir_ty Eio.Path.t) ~session ~cp
    ~upstream_url ~range_start ~(range_end : int option) ~verbose =
  let headers =
    Requests.Headers.empty
    |> Requests.Headers.range
         ~start:(Int64.of_int range_start)
         ?end_:(Option.map Int64.of_int range_end)
         ()
  in
  let resp = Requests.get session ~headers upstream_url in
  let status_code = Requests.Response.status_code resp in
  let body = Requests.Response.text resp in
  let body_len = String.length body in
  if verbose then
    Printf.printf "    range response: %d bytes (status %d)\n%!" body_len status_code;
  (* Determine the actual range we got *)
  let actual_start, actual_end, total_size =
    if status_code = 206 then begin
      (* Parse Content-Range header *)
      let total = match Requests.Response.header_string "content-range" resp with
        | Some cr -> parse_content_range cr
        | None -> None
      in
      let total_size = match total with
        | Some t -> t
        | None ->
          (* Fallback: use content-length from upstream or body length *)
          match Requests.Response.header_string "content-length" resp with
          | Some cl -> (match int_of_string_opt cl with Some n -> n | None -> body_len)
          | None -> body_len
      in
      (range_start, range_start + body_len, total_size)
    end else begin
      (* Server returned 200 instead of 206 — full body *)
      (0, body_len, body_len)
    end
  in
  let content_type =
    match Requests.Response.header_string "content-type" resp with
    | Some ct -> ct
    | None -> "application/octet-stream"
  in
  let header_pairs = filter_cacheable_headers (Requests.Response.headers resp) in
  (* Only cache 2xx responses *)
  if status_code >= 200 && status_code < 300 then begin
    write_data ~sw fs cp ~off:actual_start body;
    let existing_meta = read_meta fs cp in
    let old_ranges = match existing_meta with
      | Some m -> m.ranges
      | None -> []
    in
    let new_ranges = merge_range old_ranges
        { start = actual_start; stop = actual_end } in
    let is_complete = status_code = 200 || is_complete new_ranges ~total_len:total_size in
    let meta = {
      content_length = total_size;
      content_type;
      headers = header_pairs;
      ranges = new_ranges;
      complete = is_complete;
      status_code;
    } in
    write_meta fs cp meta
  end;
  (content_type, body, actual_start, actual_end, total_size, status_code)

(* Main handler — returns a response tuple *)

let handle_request ~sw ~net ~(fs : Eio.Fs.dir_ty Eio.Path.t) ~cache_dir ~session ~maps
    ~verbose ~path ~is_head ~range_header =
  let meth = if is_head then "HEAD" else "GET" in
  let range_info = match range_header with
    | None -> ""
    | Some r -> " [" ^ r ^ "]"
  in
  Printf.printf "%s %s%s\n%!" meth path range_info;
  match find_map maps path with
  | None ->
    Printf.printf "  no matching map\n%!";
    { status = Httpz.Res.Not_found; resp_headers = cors_headers;
      body = Httpz_server.Route.String "Not Found" }
  | Some (map, suffix) ->
    let upstream_url = map.upstream ^ suffix in
    let upstream_path = url_path_of_upstream map.upstream in
    let url_path =
      if String.length upstream_path > 1 then
        upstream_path ^ suffix
      else
        suffix
    in
    let rel = cache_path ~host:map.upstream_host ~url_path in
    let cp = cache_dir ^ "/" ^ rel in
    ensure_cache_dirs ~fs ~cache_dir ~rel_path:rel;
    if verbose then
      Printf.printf "  -> upstream: %s  cache: %s\n%!" upstream_url cp;
    if is_head then
      handle_head ~fs ~session ~cp ~upstream_url
    else begin
      match range_header with
      | None ->
        (* Full GET — serve from cache if complete, else fetch *)
        (match read_meta fs cp with
         | Some meta when meta.complete && meta.status_code >= 400 ->
           (* Negative cache hit — serve cached error status *)
           if verbose then
             Printf.printf "    cache HIT (negative, status %d)\n%!" meta.status_code;
           let status = match Httpz.Res.status_of_int meta.status_code with
             | Some s -> s | None -> Httpz.Res.Not_found in
           let headers =
             (Httpz.Header_name.Content_type, meta.content_type)
             :: cors_headers
           in
           { status; resp_headers = headers;
             body = Httpz_server.Route.Empty }
         | Some meta when meta.complete ->
           if verbose then
             Printf.printf "    cache HIT (complete, %d bytes)\n%!" meta.content_length;
           let body = read_data ~sw fs cp ~off:0 ~len:meta.content_length in
           let headers =
             (Httpz.Header_name.Content_type, meta.content_type)
             :: (Httpz.Header_name.Accept_ranges, "bytes")
             :: resp_headers_of_meta meta
             @ cors_headers
           in
           { status = Httpz.Res.Success; resp_headers = headers;
             body = Httpz_server.Route.String body }
         | _ ->
           if verbose then
             Printf.printf "    cache MISS, fetching full (streaming)\n%!";
           (try
              let (content_type, body, status_code) =
                fetch_and_cache_streaming ~sw ~net ~fs ~cp ~upstream_url ~verbose in
              let status = match Httpz.Res.status_of_int status_code with
                | Some s -> s
                | None -> Httpz.Res.Bad_gateway
              in
              let headers =
                (Httpz.Header_name.Content_type, content_type)
                :: (Httpz.Header_name.Accept_ranges, "bytes")
                :: cors_headers
              in
              { status; resp_headers = headers; body }
            with exn ->
              let msg = Printexc.to_string exn in
              Printf.eprintf "    upstream fetch error: %s\n%!" msg;
              { status = Httpz.Res.Bad_gateway; resp_headers = cors_headers;
                body = Httpz_server.Route.String ("Upstream error: " ^ msg) }))
      | Some range_str ->
        (* Range GET *)
        (match parse_range_header range_str with
         | None ->
           { status = Httpz.Res.Bad_request; resp_headers = cors_headers;
             body = Httpz_server.Route.String "Invalid Range header" }
         | Some parsed_range ->
           let cached_meta = read_meta fs cp in
           (* Resolve suffix ranges to absolute offsets using cached size *)
           let (range_start, range_end_opt) = match parsed_range with
             | `Range (s, e) -> (s, e)
             | `Suffix n ->
               match cached_meta with
               | Some meta when meta.content_length > 0 ->
                 let total = meta.content_length in
                 (max 0 (total - n), Some (total - 1))
               | _ ->
                 (* No cached size — request full resource from upstream *)
                 (0, None)
           in
           let range_end = match range_end_opt with
             | Some e -> Some e
             | None ->
               (match cached_meta with
                | Some meta when meta.content_length > 0 ->
                  Some (meta.content_length - 1)
                | _ -> None)
           in
           (* Open-ended range with unknown total size: pass through to upstream *)
           let range_len = match range_end with
             | Some e -> e - range_start + 1
             | None -> 0 (* sentinel: will not match cache *) in
           let can_serve_from_cache =
             match cached_meta, range_end with
             | Some meta, Some _ ->
               range_len > 0
               && is_range_cached meta.ranges ~start:range_start ~len:range_len
             | _ -> false
           in
           if can_serve_from_cache then begin
             let meta = match cached_meta with Some m -> m | None -> assert false in
             let re = match range_end with Some e -> e | None -> assert false in
             if verbose then
               Printf.printf "    cache HIT (range %d-%d)\n%!" range_start re;
             let body = read_data ~sw fs cp ~off:range_start ~len:range_len in
             let content_range =
               Printf.sprintf "bytes %d-%d/%d" range_start re
                 meta.content_length
             in
             let headers =
               (Httpz.Header_name.Content_type, meta.content_type)
               :: (Httpz.Header_name.Content_range, content_range)
               :: (Httpz.Header_name.Accept_ranges, "bytes")
               :: cors_headers
             in
             { status = Httpz.Res.Partial_content; resp_headers = headers;
               body = Httpz_server.Route.String body }
           end else begin
             if verbose then
               Printf.printf "    cache MISS (range %d-%s), fetching\n%!"
                 range_start
                 (match range_end with Some e -> string_of_int e | None -> "");
             try
               let (content_type, body, actual_start, actual_end, total_size,
                    status_code) =
                 fetch_and_cache_range ~sw ~fs ~session ~cp ~upstream_url
                   ~range_start ~range_end ~verbose
               in
               let status = match Httpz.Res.status_of_int status_code with
                 | Some s -> s
                 | None -> Httpz.Res.Bad_gateway
               in
               if status_code = 200 then begin
                 let headers =
                   (Httpz.Header_name.Content_type, content_type)
                   :: (Httpz.Header_name.Accept_ranges, "bytes")
                   :: cors_headers
                 in
                 { status; resp_headers = headers;
                   body = Httpz_server.Route.String body }
               end else if status_code >= 200 && status_code < 300 then begin
                 let content_range =
                   Printf.sprintf "bytes %d-%d/%d" actual_start
                     (actual_end - 1) total_size
                 in
                 let headers =
                   (Httpz.Header_name.Content_type, content_type)
                   :: (Httpz.Header_name.Content_range, content_range)
                   :: (Httpz.Header_name.Accept_ranges, "bytes")
                   :: cors_headers
                 in
                 { status; resp_headers = headers;
                   body = Httpz_server.Route.String body }
               end else begin
                 (* Non-2xx: pass through without caching *)
                 let headers =
                   (Httpz.Header_name.Content_type, content_type)
                   :: cors_headers
                 in
                 { status; resp_headers = headers;
                   body = Httpz_server.Route.String body }
               end
             with exn ->
               let msg = Printexc.to_string exn in
               Printf.eprintf "    upstream range fetch error: %s\n%!" msg;
               { status = Httpz.Res.Bad_gateway; resp_headers = cors_headers;
                 body = Httpz_server.Route.String ("Upstream error: " ^ msg) }
           end)
    end

let cors_preflight_response =
  { status = Httpz.Res.No_content; resp_headers = cors_headers;
    body = Httpz_server.Route.Empty }
