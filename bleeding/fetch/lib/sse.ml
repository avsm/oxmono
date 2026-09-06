open Client

type event = {
  name : string;
  data : string;
  id : string option;
  retry : int option;
}

let media_type = "text/event-stream"

type decoder = {
  response : response;
  flow : Eio.Flow.source_ty Eio.Resource.t;
  input : Cstruct.t;
  line : Buffer.t;
  data : Buffer.t;
  max_event : int;
  mutable input_pos : int;
  mutable input_len : int;
  mutable at_start : bool;
  mutable skip_lf : bool;
  mutable block_bytes : int;
  mutable has_data : bool;
  mutable name : string or_null;
  mutable id : string option;
  mutable retry : int option;
  mutable last_id : string option;
  mutable reconnect_ms : int option;
  mutable on_block : unit -> unit;
}

let invalid_max_event caller max_event =
  if max_event < 1 then invalid_arg (caller ^ ": max_event must be at least 1")

let make_decoder ~max_event response =
  let ct = content_type response in
  if
    not
      (match ct with
      | Some ct -> Media.matches ~range:media_type ct
      | None -> false)
  then raise (decode_failure response media_type (Media.Unsupported ct));
  {
    response;
    flow = body response;
    input = Cstruct.create 65536;
    line = Buffer.create 128;
    data = Buffer.create 256;
    max_event;
    input_pos = 0;
    input_len = 0;
    at_start = true;
    skip_lf = false;
    block_bytes = 0;
    has_data = false;
    name = Null;
    id = None;
    retry = None;
    last_id = None;
    reconnect_ms = None;
    on_block = (fun () -> ());
  }

let ensure_input t =
  if t.input_pos < t.input_len then true
  else
    match Eio.Flow.single_read t.flow t.input with
    | n ->
        t.input_pos <- 0;
        t.input_len <- n;
        true
    | exception End_of_file -> false

let peek t = Cstruct.get_char t.input t.input_pos

let consume t =
  let c = peek t in
  t.input_pos <- t.input_pos + 1;
  c

let bump t n =
  if n > t.max_event - t.block_bytes then
    raise (decode_failure t.response media_type (Media.Too_large t.max_event));
  t.block_bytes <- t.block_bytes + n

let prepare_line t =
  if t.skip_lf then begin
    t.skip_lf <- false;
    if ensure_input t && Char.equal (peek t) '\n' then ignore (consume t)
  end

(* WHATWG "parsing an event stream" removes one leading U+FEFF, and only
   the first. It holds neither CR nor LF, so it cannot straddle a line:
   taking it off the first line covers a BOM split across reads too. *)
let strip_bom t line =
  if not t.at_start then line
  else begin
    t.at_start <- false;
    if String.starts_with ~prefix:"\xef\xbb\xbf" line then
      String.sub line 3 (String.length line - 3)
    else line
  end

let read_line t =
  prepare_line t;
  Buffer.clear t.line;
  let finish newline_bytes =
    let line = strip_bom t (Buffer.contents t.line) in
    (* The empty line is the boundary between blocks, rather than part of
       either block. Count both bytes of CRLF on every field line. *)
    if line <> "" then bump t newline_bytes;
    `Line line
  in
  let rec loop () =
    if not (ensure_input t) then `Eof
    else
      match consume t with
      | '\n' -> finish 1
      | '\r' ->
          let nonempty = Buffer.length t.line > 0 in
          let lf =
            if t.input_pos < t.input_len then
              if Char.equal (peek t) '\n' then begin
                ignore (consume t);
                true
              end
              else false
            else if nonempty then
              if ensure_input t && Char.equal (peek t) '\n' then begin
                ignore (consume t);
                true
              end
              else false
            else begin
              (* A blank lone CR dispatches immediately. If a split LF
                 follows, [prepare_line] discards it before the next line. *)
              t.skip_lf <- true;
              false
            end
          in
          finish (if lf then 2 else 1)
      | c ->
          bump t 1;
          Buffer.add_char t.line c;
          loop ()
  in
  loop ()

let add_field t line =
  let len = String.length line in
  if len > 0 && line.[0] <> ':' then begin
    let colon = match String.index_opt line ':' with None -> len | Some i -> i in
    let first = if colon = len then len
      else if colon + 1 < len && line.[colon + 1] = ' ' then colon + 2 else colon + 1 in
    let named name =
      let rec equal i = i = colon || (name.[i] = line.[i] && equal (i + 1)) in
      String.length name = colon && equal 0
    in
    if named "data" then begin
      t.has_data <- true;
      Buffer.add_substring t.data line first (len - first);
      Buffer.add_char t.data '\n'
    end else if named "event" then t.name <- This (String.sub line first (len - first))
    else if named "id" then begin
      let rec has_nul i = i < len && (line.[i] = '\000' || has_nul (i + 1)) in
      if not (has_nul first) then t.id <- Some (String.sub line first (len - first))
    end else if named "retry" then begin
      let rec decimal i = i = len || (line.[i] >= '0' && line.[i] <= '9' && decimal (i + 1)) in
      if first < len && decimal first then
        (* WHATWG "parsing an event stream" ignores a field it cannot use. A
           value too large for an int is one, so the block keeps whatever a
           previous [retry] field set. *)
        match int_of_string_opt (String.sub line first (len - first)) with
        | Some _ as retry -> t.retry <- retry
        | None -> ()
    end
  end

let reset_block t =
  t.block_bytes <- 0;
  t.has_data <- false;
  t.name <- Null;
  t.id <- None;
  t.retry <- None;
  Buffer.clear t.data

let dispatch t =
  (match t.id with None -> () | Some _ as id -> t.last_id <- id);
  (match t.retry with None -> () | Some _ as retry -> t.reconnect_ms <- retry);
  t.on_block ();
  let event =
    if not t.has_data then None
    else
      let data = Buffer.sub t.data 0 (Buffer.length t.data - 1) in
      let name =
        match t.name with
        | Null | This "" -> "message"
        | This name -> name
      in
      Some { name; data; id = t.last_id; retry = t.retry }
  in
  reset_block t;
  event

let rec next_event t () =
  match read_line t with
  | `Eof -> Seq.Nil
  | `Line "" -> (
      match dispatch t with
      | Some event -> Seq.Cons (event, next_event t)
      | None -> next_event t ())
  | `Line line ->
      add_field t line;
      next_event t ()

let decode ?(max_event = 1024 * 1024) response =
  invalid_max_event "Fetch.Sse.decode" max_event;
  let decoder = make_decoder ~max_event response in
  next_event decoder

let with_last_event_id id headers =
  match id with
  | None -> headers
  | Some id ->
      let headers =
        Header.to_http headers |> fun hs ->
        Http.Header.remove hs "Last-Event-ID"
      in
      let headers =
        if Middleware.is_field_value id then
          Http.Header.replace headers "Last-Event-ID" id
        else headers
      in
      Header.of_http headers

let connect_decoder ~sw ?headers ?last_event_id ~max_event client url =
  let headers = with_accept media_type headers in
  let headers = with_last_event_id last_event_id headers in
  let response = fetch ~sw ~headers client `GET url in
  if is_success response then (
    match make_decoder ~max_event response with
    | decoder -> Ok decoder
    | exception ex ->
        Middleware.close response;
        raise ex)
  else Error response

let connect ~sw ?headers ?last_event_id ?(max_event = 1024 * 1024) client url =
  invalid_max_event "Fetch.Sse.connect" max_event;
  match connect_decoder ~sw ?headers ?last_event_id ~max_event client url with
  | Ok decoder -> Ok (next_event decoder)
  | Error response -> Error response

type subscription = {
  stream : [ `Event of event | `End ] Eio.Stream.t;
  last_id : string option Atomic.t;
  result : (unit, exn) result Eio.Promise.t;
  result_u : (unit, exn) result Eio.Promise.u;
  cancel : Eio.Cancel.t option Atomic.t;
  closed : bool Atomic.t;
  capacity : int;
}

exception Closed

let events t = t.stream
let last_event_id t = Atomic.get t.last_id
let result t = t.result

(* The daemon is the stream's only producer, so an add guarded by the room
   check cannot block. Waiting for room instead would wedge the owning
   switch for good: [Switch.await_idle] waits on daemons through a path
   that cannot be cancelled, so a consumer that stops draining a full
   stream would keep the scope alive forever. *)
let finish t ~protect result =
  ignore (Eio.Promise.try_resolve t.result_u result);
  let add () =
    if Eio.Stream.length t.stream < t.capacity then Eio.Stream.add t.stream `End
  in
  if protect then Eio.Cancel.protect add else add ()

let close t =
  if Atomic.compare_and_set t.closed false true then
    match Atomic.get t.cancel with
    | None -> ()
    | Some cancel -> Eio.Cancel.cancel cancel Closed

let default_retryable = function
  | Eio.Io (E (Connection_failure _ | Protocol_error _ | Too_many_redirects), _)
    ->
      true
  | Rejected response ->
      status response = 429 || (status response >= 500 && status response <= 599)
  | _ -> false

let subscribe ~sw ~clock ?headers ?last_event_id ?(max_event = 1024 * 1024)
    ?(backoff_initial = Duration.of_sec 1) ?(backoff_max = Duration.of_sec 60)
    ?(capacity = 64) ?(retryable = default_retryable) client url =
  invalid_max_event "Fetch.Sse.subscribe" max_event;
  if
    backoff_initial = 0L
    || Int64.unsigned_compare backoff_max backoff_initial < 0
  then
    invalid_arg "Fetch.Sse.subscribe: backoff must satisfy 0 < initial <= max";
  let backoff_initial = Duration.to_f backoff_initial in
  let backoff_max = Duration.to_f backoff_max in
  if capacity < 1 then
    invalid_arg "Fetch.Sse.subscribe: capacity must be at least 1";
  let result, result_u = Eio.Promise.create () in
  let t =
    {
      stream = Eio.Stream.create capacity;
      last_id = Atomic.make last_event_id;
      result;
      result_u;
      cancel = Atomic.make None;
      closed = Atomic.make false;
      capacity;
    }
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Cancel.sub @@ fun cancel ->
      Atomic.set t.cancel (Some cancel);
      if Atomic.get t.closed then Eio.Cancel.cancel cancel Closed;
      let reconnect_ms = ref None in
      let next_backoff delay = Float.min backoff_max (delay *. 2.) in
      (* A server's [retry] field is advice from the other end of an
       untrusted connection: honour it only between a floor that keeps a
       hostile or broken server from turning the subscription into a
       reconnection loop, and the caller's own backoff ceiling. *)
      let sleep delay =
        let delay =
          match !reconnect_ms with
          | Some milliseconds ->
              Float.min backoff_max
                (Float.max 0.1 (float_of_int milliseconds /. 1000.))
          | None -> delay
        in
        if delay > 0. then Eio.Time.Mono.sleep clock delay
      in
    let sync (decoder : decoder) =
      (match decoder.last_id with
       | None -> ()
       | Some _ as id -> Atomic.set t.last_id id);
      (match decoder.reconnect_ms with
       | None -> ()
       | Some _ as retry -> reconnect_ms := retry)
      in
      let rec loop delay =
        if Atomic.get t.closed then raise Closed;
        let delivered = ref false in
        let rec consume seq =
          match seq () with
          | Seq.Nil -> ()
          | Seq.Cons (event, rest) ->
              delivered := true;
              Eio.Stream.add t.stream (`Event event);
              if Atomic.get t.closed then raise Closed;
              consume rest
        in
        let outcome =
          match
            connect_decoder ~sw ?headers ?last_event_id:(Atomic.get t.last_id)
              ~max_event client url
          with
          | exception ex -> Error ex
          | Error response ->
              Middleware.close response;
              Error (Rejected response)
          | Ok connected -> (
              (* Every block synchronizes the cursor, including blocks without events. *)
              connected.on_block <- (fun () -> sync connected);
              match
                Fun.protect
                  ~finally:(fun () -> Middleware.close connected.response)
                  (fun () -> consume (next_event connected))
              with
              | () -> Ok ()
              | exception ex -> Error ex)
        in
        (match outcome with
        | Error ex when not (retryable ex) -> raise ex
        | Ok () | Error _ -> ());
        let delay = if !delivered then backoff_initial else delay in
        sleep delay;
        loop (next_backoff delay)
      in
      (match loop backoff_initial with
      | () | (exception (Eio.Cancel.Cancelled _ | Closed)) ->
          Atomic.set t.cancel None;
          finish t ~protect:true (Ok ())
      | exception ex ->
          Atomic.set t.cancel None;
          finish t ~protect:false (Error ex));
      `Stop_daemon);
  t
