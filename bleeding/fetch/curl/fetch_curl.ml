open Fetch

type tag = [ `Generic | `Curl ]

type t = tag Fetch.ty Eio.Resource.t

type Eio.Exn.Backend.t += Curl_error of Curl.curlCode * string

let () =
  Eio.Exn.Backend.register_pp (fun f -> function
      | Curl_error (code, "") ->
        Fmt.pf f "Curl_error(%s)" (Curl.strerror code);
        true
      | Curl_error (code, msg) ->
        Fmt.pf f "Curl_error(%s, %S)" (Curl.strerror code) msg;
        true
      | _ -> false
    )

type config = {
  tls_verify : bool;
  http_version : [ `Auto | `Http1_1 ];
  proxy : string option;
  timeout : float option;
  connect_timeout : float;
  max_response : int;
  max_request : int;
  user_agent : string;
  verbose : bool;
}

let global_init = lazy (Curl.global_init Curl.CURLINIT_GLOBALALL)

let map_curl_error code msg =
  let pretty () =
    if msg = "" then Curl.strerror code
    else Fmt.str "%s: %s" (Curl.strerror code) msg
  in
  match code with
  (* Resolution failures would be [No_matching_addresses] in eio
     versions that have it; this one only offers [Refused]. *)
  | Curl.CURLE_COULDNT_CONNECT
  | Curl.CURLE_COULDNT_RESOLVE_HOST
  | Curl.CURLE_COULDNT_RESOLVE_PROXY ->
    err (Connection_failure (Refused (Curl_error (code, msg))))
  | Curl.CURLE_OPERATION_TIMEOUTED ->
    err (Connection_failure Timeout)
  | Curl.CURLE_SSL_CONNECT_ERROR
  | Curl.CURLE_SSL_PEER_CERTIFICATE
  | Curl.CURLE_SSL_CACERT
  | Curl.CURLE_SSL_CERTPROBLEM
  | Curl.CURLE_SSL_CIPHER
  | Curl.CURLE_SSL_CACERT_BADFILE
  | Curl.CURLE_SSL_ENGINE_NOTFOUND
  | Curl.CURLE_SSL_ENGINE_SETFAILED
  | Curl.CURLE_SSL_ENGINE_INITFAILED
  | Curl.CURLE_SSL_SHUTDOWN_FAILED ->
    err (Tls_failure (pretty ()))
  | _ ->
    err (Protocol_error (pretty ()))

(* Response header collection. libcurl delivers one line per callback,
   including the status line, interim (1xx) blocks and — after the body
   has started — chunked/h2 *trailer* lines. Reset on each new status
   line so we keep only the final response's headers, and divert
   post-body lines to [trailer_lines]: RFC 9110 s6.5.1 forbids promoting
   a trailer field to a header, and folding e.g. a trailing [Set-Cookie]
   or [Location] into the header block would do exactly that. *)
type collector = {
  mutable status_line : string;
  mutable lines : string list;  (* reversed *)
  mutable in_body : bool;       (* set by the write callback *)
  mutable trailer_lines : string list;  (* reversed *)
  mutable header_bytes : int;
  mutable headers_capped : bool;
}

(* libcurl's own per-line limit still allows a hostile server to stream
   an unbounded *number* of header lines; cap the total ourselves. A
   short return from the callback aborts the transfer — [headers_capped]
   distinguishes that from a generic write failure. Not reset by the 1xx
   reset below: resetting would let a server stream unbounded interim
   blocks, each one under the cap. *)
let max_header_bytes = 256 * 1024

let header_callback c line =
  let n = String.length line in
  c.header_bytes <- c.header_bytes + n;
  if c.header_bytes > max_header_bytes then (c.headers_capped <- true; 0)
  else begin
    (* The status-line reset (for interim 1xx blocks) applies only before
       the body: libcurl passes trailer lines through this callback
       unvalidated, so a trailer spelled "HTTP/1.1 999 x" would otherwise
       clear the header snapshot and forge the version. *)
    if (not c.in_body) && n >= 5 && String.sub line 0 5 = "HTTP/" then (
      c.status_line <- String.trim line;
      c.lines <- []
    ) else (
      let line = String.trim line in
      if line <> "" then
        if c.in_body then c.trailer_lines <- line :: c.trailer_lines
        else c.lines <- line :: c.lines
    );
    n
  end

let parse_header_line l =
  match String.index_opt l ':' with
  | None -> None
  | Some i ->
    let name = String.sub l 0 i in
    let value = String.trim (String.sub l (i + 1) (String.length l - i - 1)) in
    Some (name, value)

let version_of_status_line sl : version =
  match String.split_on_char ' ' sl with
  | "HTTP/1.0" :: _ -> `HTTP_1_0
  | "HTTP/1.1" :: _ -> `HTTP_1_1
  | ("HTTP/2" | "HTTP/2.0") :: _ -> `HTTP_2
  | v :: _ when v <> "" -> `Other v
  | _ -> `Other "unknown"

(* How a request body reaches the wire. [Fixed] is handed to libcurl
   whole before the transfer starts, [Streamed] is fed to the read
   callback as the pump fiber produces it. *)
type wire_body =
  | No_body
  | Fixed of string
  | Streamed of int64 option    (* declared length, if any *)

(* Configure an easy handle for one exchange. Conformance: single exchange,
   no ambient authority. *)
let setup_handle cfg h (req : Middleware.request) body =
  Curl.set_followlocation h false;
  Curl.set_protocols h [ Curl.CURLPROTO_HTTP; Curl.CURLPROTO_HTTPS ];
  Curl.set_netrc h Curl.CURL_NETRC_IGNORED;
  Curl.set_proxy h (Option.value cfg.proxy ~default:"");
  Curl.set_sslverifypeer h cfg.tls_verify;
  Curl.set_sslverifyhost h
    (if cfg.tls_verify then Curl.SSLVERIFYHOST_HOSTNAME else Curl.SSLVERIFYHOST_NONE);
  Curl.set_useragent h cfg.user_agent;
  Curl.set_connecttimeoutms h (int_of_float (cfg.connect_timeout *. 1000.));
  Option.iter (fun s -> Curl.set_timeoutms h (int_of_float (s *. 1000.))) cfg.timeout;
  Curl.set_httpversion h
    (match cfg.http_version with
     | `Auto -> Curl.HTTP_VERSION_2TLS
     | `Http1_1 -> Curl.HTTP_VERSION_1_1);
  if cfg.verbose then Curl.set_verbose h true;
  Curl.set_url h (Middleware.Url.to_string req.url);
  (* Transparent content-coding, unless the caller negotiates its own. *)
  let auto_decode = not (Http.Header.mem req.headers "accept-encoding") in
  if auto_decode then Curl.set_encoding h Curl.CURL_ENCODING_ANY;
  (match req.meth with
   | `HEAD -> Curl.set_nobody h true
   | m -> Curl.set_customrequest h (Http.Method.to_string m));
  (match body with
   | No_body -> ()
   | Fixed s ->
     Curl.set_postfields h s;
     Curl.set_postfieldsize h (String.length s)
   | Streamed length ->
     (* CURLOPT_UPLOAD switches the handle to the read callback and picks
        the framing: [Content-Length] from the infile size, chunked when
        it is unknown. The method still comes from [set_customrequest]. *)
     Curl.set_upload h true;
     Option.iter (Curl.set_infilesizelarge h) length);
  let extra_headers =
    (* "Name:" (no value) removes a curl default header. No 100-continue
       stalls; no invented Content-Type for bodies that didn't declare one. *)
    "Expect:"
    :: (if body <> No_body && not (Http.Header.mem req.headers "content-type")
        then [ "Content-Type:" ] else [])
  in
  let request_headers =
    (* "Name;" is libcurl's syntax for sending a header with an empty
       value — "Name:" would *remove* the header instead. *)
    List.map
      (fun (n, v) -> if v = "" then n ^ ";" else n ^ ": " ^ v)
      (Http.Header.to_list req.headers)
  in
  Curl.set_httpheader h (request_headers @ extra_headers);
  (* libcurl built with the synchronous resolver would otherwise use
     SIGALRM for DNS timeouts — unsafe with multiple domains. *)
  Curl.set_nosignal h true;
  let col = { status_line = ""; lines = []; in_body = false;
              trailer_lines = []; header_bytes = 0; headers_capped = false } in
  Curl.set_headerfunction h (header_callback col);
  (col, auto_decode)

let too_large cfg =
  err (Invalid_request
         (Fmt.str "request body exceeds %d bytes" cfg.max_request))

(* Classify a request body for the wire. A declared length over
   [max_request] is rejected here, before the network is touched. An
   undeclared one is counted as the pump reads it. *)
let wire_body_of cfg (req : Middleware.request) =
  match req.body with
  | Empty -> No_body
  | String s -> Fixed s
  | Stream { length; _ } ->
    (match length with
     | Some l when Int64.unsigned_compare l (Int64.of_int cfg.max_request) > 0 ->
       raise (too_large cfg)
     | _ -> ());
    Streamed length

(* {2 The multi engine}

   One [Curl.Multi.mt] per client: its connection cache gives reuse and h2
   multiplexing across requests. All fibers live in one domain, so calls
   into libcurl never race.

   Effects cannot cross the C frames of a libcurl callback, so the
   socket/timer callbacks must not fork fibers, resolve promises or
   suspend. They only record intents in [pending_sockets]/[pending_timer];
   [curl_call] applies them (and dispatches completions) from ordinary
   fiber context after every call into libcurl returns. *)

(* A slot for one sleeping fiber. Each waiter on a job needs its own:
   the response reader and the upload pump sleep on the same job. *)
type waiter = { mutable u : unit Eio.Promise.u option }

(* A streaming request body in flight. The pump fiber fills [chunks]
   from the flow and the read callback drains them: at [high_water] the
   pump stops reading, and with nothing buffered the callback pauses the
   send side until the pump enqueues more — the bound on what an upload
   can make the client buffer. *)
type upload = {
  chunks : string Queue.t;
  wake : waiter;                (* the pump sleeps here *)
  mutable head_off : int;       (* read offset into the head chunk *)
  mutable queued : int;         (* bytes in [chunks] *)
  mutable read : int;           (* total bytes taken from the flow *)
  mutable eof : bool;           (* the flow is drained *)
  mutable error : exn option;   (* the pump failed: abort the transfer *)
  mutable paused : bool;        (* the send side is paused *)
}

(* One transfer, from [Multi.add] until its handle is cleaned up. The
   write callback fills [chunks]; the response body flow drains them.
   [queued] counts the bytes in [chunks]: at [high_water] the write
   callback pauses the transfer, and the reader unpauses it once it has
   drained down to [low_water] — the bound on what a response can make
   the client buffer. *)
type job = {
  id : string;
  h : Curl.t;
  chunks : string Queue.t;
  errbuf : string ref;          (* libcurl's error detail for [finished] *)
  upload : upload option;       (* [Some] for a streamed request body *)
  wake : waiter;
  mutable head_off : int;       (* read offset into the head chunk *)
  mutable queued : int;         (* bytes in [chunks] *)
  mutable received : int;       (* total body bytes accepted *)
  mutable paused : bool;
  mutable over_limit : bool;
  mutable added : bool;         (* attached to the multi *)
  mutable body_started : bool;  (* implies the headers are complete *)
  mutable finished : Curl.curlCode option;
  mutable cleaned : bool;
  mutable hook : Eio.Switch.hook;
}

type engine = {
  cfg : config;
  mt : Curl.Multi.mt;
  sw : Eio.Switch.t;
  (* Calls into the shared [Curl.Multi] are only safe from the domain
     whose event loop drives it; using the client elsewhere would be a
     data race in C. *)
  dom : Domain.id;
  (* Keyed by the handle's CURLOPT_PRIVATE tag: [Multi.remove_finished]
     returns the same C handle in a fresh OCaml block, so neither physical
     nor structural equality on [Curl.t] can identify the job. Holds the
     jobs still attached to the multi (until [remove_finished]/cleanup). *)
  jobs : (string, job) Hashtbl.t;
  mutable next_id : int;
  (* Once the engine switch has released the multi, no libcurl multi
     call may run again; late job cleanups (from request switches that
     outlive the client's) must not touch it. *)
  mutable closed : bool;
  watchers : (Unix.file_descr, unit Eio.Promise.u) Hashtbl.t;
  mutable pending_sockets : (Unix.file_descr * Curl.Multi.poll) list; (* reversed *)
  mutable pending_timer : int option;
  mutable timer_gen : int;
  mutable timer_stop : unit Eio.Promise.u option;
  (* Jobs whose write callback got data; woken after the libcurl call
     returns, since a C callback frame cannot resolve promises. *)
  mutable pending_wakes : job list;
}

let notify w =
  match w.u with
  | None -> ()
  | Some u -> w.u <- None; Eio.Promise.resolve u ()

(* Wake whoever is waiting on [job]: the fiber wanting headers or body
   data, and the pump wanting room or news of the transfer. *)
let wake job =
  notify job.wake;
  Option.iter (fun (up : upload) -> notify up.wake) job.upload

(* Wait until [cond] holds. Fibers share one domain and nothing yields
   between testing [cond] and registering the waker, so no wake-up can
   be lost. *)
let rec wait_for w cond =
  if not (cond ()) then begin
    let p, u = Eio.Promise.create () in
    w.u <- Some u;
    Eio.Promise.await p;
    wait_for w cond
  end

(* Callbacks: recording only — see above. *)
let socket_function eng fd (poll : Curl.Multi.poll) =
  eng.pending_sockets <- (fd, poll) :: eng.pending_sockets

let timer_function eng ms =
  eng.pending_timer <- Some ms

let check_completions eng =
  let rec go () =
    match Curl.Multi.remove_finished eng.mt with
    | None -> ()
    | Some (h, code) ->
      let id = try Curl.get_private h with _ -> "" in
      (match Hashtbl.find_opt eng.jobs id with
       | Some job ->
         Hashtbl.remove eng.jobs id;
         job.finished <- Some code;
         wake job
       | None -> ());
      go ()
  in
  go ()

let process_wakes eng =
  let rec go () =
    match eng.pending_wakes with
    | [] -> ()
    | jobs -> eng.pending_wakes <- []; List.iter wake jobs; go ()
  in
  go ()

let stop_watcher eng fd =
  match Hashtbl.find_opt eng.watchers fd with
  | Some stop ->
    Hashtbl.remove eng.watchers fd;
    Eio.Promise.resolve stop ()
  | None -> ()

(* [curl_call eng f] runs a libcurl multi call, then dispatches whatever it
   caused: finished transfers, socket-interest changes, timer updates. *)
let rec curl_call : 'a. engine -> (unit -> 'a) -> 'a = fun eng f ->
  let r = f () in
  check_completions eng;
  process_pending eng;
  process_wakes eng;
  r

and process_pending eng =
  let socks = List.rev eng.pending_sockets in
  let timer = eng.pending_timer in
  eng.pending_sockets <- [];
  eng.pending_timer <- None;
  if socks <> [] || timer <> None then (
    List.iter (fun (fd, poll) -> apply_socket eng fd poll) socks;
    Option.iter (apply_timer eng) timer;
    process_pending eng
  )

and apply_socket eng fd (poll : Curl.Multi.poll) =
  stop_watcher eng fd;
  match poll with
  | Curl.Multi.POLL_NONE | Curl.Multi.POLL_REMOVE -> ()
  | Curl.Multi.POLL_IN -> start_watcher eng fd `In
  | Curl.Multi.POLL_OUT -> start_watcher eng fd `Out
  | Curl.Multi.POLL_INOUT -> start_watcher eng fd `Both

and start_watcher eng fd dirs =
  let stop_p, stop_u = Eio.Promise.create () in
  Hashtbl.replace eng.watchers fd stop_u;
  (* Daemon: engine helpers must not keep the switch alive once the
     application's own fibers are done. *)
  Eio.Fiber.fork_daemon ~sw:eng.sw (fun () ->
      (try
         Eio.Fiber.first
           (fun () -> Eio.Promise.await stop_p)
           (fun () ->
              match dirs with
              | `In -> watch_fd eng fd Curl.Multi.EV_IN Eio_unix.await_readable
              | `Out -> watch_fd eng fd Curl.Multi.EV_OUT Eio_unix.await_writable
              | `Both ->
                Eio.Fiber.both
                  (fun () -> watch_fd eng fd Curl.Multi.EV_IN Eio_unix.await_readable)
                  (fun () -> watch_fd eng fd Curl.Multi.EV_OUT Eio_unix.await_writable))
       with
       | Eio.Cancel.Cancelled _ as ex -> raise ex
       | ex ->
         (* Usually the fd was closed under us and this watcher is
            obsolete — but a silent exit on a real failure would stall
            every transfer waiting on this fd, so leave a trace. *)
         Eio.Private.Trace.log
           (Fmt.str "fetch-curl: fd watcher stopped: %s" (Printexc.to_string ex)));
      `Stop_daemon)

and watch_fd eng fd status await =
  await fd;
  ignore (curl_call eng (fun () -> Curl.Multi.action eng.mt fd status) : int);
  watch_fd eng fd status await

and apply_timer eng ms =
  (* Superseding a timer also wakes the previous sleeper (via the
     generation check it exits without acting), so a long libcurl
     timeout does not hold a fiber for its full original duration. *)
  eng.timer_gen <- eng.timer_gen + 1;
  let gen = eng.timer_gen in
  Option.iter (fun u -> eng.timer_stop <- None; Eio.Promise.resolve u ())
    eng.timer_stop;
  if ms >= 0 then begin
    let stop_p, stop_u = Eio.Promise.create () in
    eng.timer_stop <- Some stop_u;
    Eio.Fiber.fork_daemon ~sw:eng.sw (fun () ->
        Eio.Fiber.first
          (fun () -> Eio.Promise.await stop_p)
          (fun () ->
             if ms > 0 then Eio_unix.sleep (float_of_int ms /. 1000.)
             else Eio.Fiber.yield ();
             if gen = eng.timer_gen then
               curl_call eng (fun () -> Curl.Multi.action_timeout eng.mt));
        `Stop_daemon)
  end

(* Free [job]'s handle. Idempotent. Called when the body is drained,
   when the request fails or is cancelled, and as a backstop from the
   request switch's release hook if the body was never read.

   The handle must be detached from the multi before [Curl.cleanup], so
   the three states are distinguished: never added (free it directly);
   finished, hence already detached by [remove_finished] (free it); still
   attached (remove first, and if the remove fails leak the handle rather
   than free one the multi may still own). [Curl.Multi.cleanup] detaches
   attached handles without freeing them, but the engine's release hook
   removes every job before destroying the multi, so an attached handle
   after [closed] would be a bug — leak it and say so. *)
let cleanup_job eng job =
  if not job.cleaned then begin
    job.cleaned <- true;
    Eio.Switch.remove_hook job.hook;
    Hashtbl.remove eng.jobs job.id;
    (match job.added, job.finished with
     | false, _ | true, Some _ -> Curl.cleanup job.h
     | true, None ->
       if eng.closed then
         Eio.Private.Trace.log
           "fetch-curl: leaking a handle still attached at engine shutdown"
       else begin
         match curl_call eng (fun () -> Curl.Multi.remove eng.mt job.h) with
         | () -> Curl.cleanup job.h
         | exception _ -> ()
       end);
    wake job
  end

let high_water = 256 * 1024
let low_water = high_water / 2

(* [Curl.pause] sets the state of both directions at once, so a resume
   must name the direction that stays paused: unpausing the sender must
   not also resume a receiver whose reader has fallen behind. *)
let sync_pause eng job =
  let flags =
    (if job.paused then [ Curl.PAUSE_RECV ] else [])
    @ (match job.upload with
       | Some up when up.paused -> [ Curl.PAUSE_SEND ]
       | _ -> [])
  in
  curl_call eng (fun () -> Curl.pause job.h flags)

let maybe_unpause eng job =
  if job.paused && job.finished = None && not job.cleaned
     && job.queued <= low_water then begin
    job.paused <- false;
    sync_pause eng job
  end

(* Resume a send the read callback paused for want of data. Runs in the
   engine's domain, which is what libcurl requires of an unpause. *)
let resume_send eng job (up : upload) =
  if up.paused && job.finished = None && not job.cleaned then begin
    up.paused <- false;
    sync_pause eng job
  end

let upload_chunk = 64 * 1024

(* Read the request body flow into [up] until it ends, the limit is
   passed or the transfer is over, resuming the send whenever data
   lands. Stopping leaves [eof] or [error] set, which is what tells the
   read callback to finish or abort. *)
let pump eng job (up : upload) flow =
  let cfg = eng.cfg in
  let buf = Cstruct.create upload_chunk in
  let stopped () = job.cleaned || job.finished <> None || up.error <> None in
  let rec loop () =
    if stopped () then ()
    else if up.queued >= high_water then begin
      wait_for up.wake (fun () -> up.queued < high_water || stopped ());
      loop ()
    end
    else begin
      (match Eio.Flow.single_read flow buf with
       | n ->
         up.read <- up.read + n;
         if up.read > cfg.max_request then up.error <- Some (too_large cfg)
         else begin
           Queue.add (Cstruct.to_string ~len:n buf) up.chunks;
           up.queued <- up.queued + n
         end
       | exception End_of_file -> up.eof <- true
       | exception (Eio.Cancel.Cancelled _ as ex) -> raise ex
       | exception ex -> up.error <- Some ex);
      resume_send eng job up;
      if not up.eof then loop ()
    end
  in
  loop ()

let upload_error job =
  match job.upload with Some up -> up.error | None -> None

(* The response body: a flow draining [job.chunks] as curl delivers
   them, transparently pausing and resuming the transfer. *)
type stream_body = { eng : engine; job : job }

module Body_stream = struct
  type t = stream_body

  let read_methods = []

  let rec single_read t buf =
    let job = t.job in
    (* [maybe_unpause] and [cleanup_job] call into the shared multi;
       the single-domain rule for {!Backend.request} applies here too. *)
    if Domain.self () <> t.eng.dom then
      invalid_arg
        "Fetch_curl: response body used from a domain other than the client's";
    if not (Queue.is_empty job.chunks) then begin
      let chunk = Queue.peek job.chunks in
      let avail = String.length chunk - job.head_off in
      let n = min avail (Cstruct.length buf) in
      Cstruct.blit_from_string chunk job.head_off buf 0 n;
      if n = avail then (ignore (Queue.pop job.chunks : string); job.head_off <- 0)
      else job.head_off <- job.head_off + n;
      job.queued <- job.queued - n;
      maybe_unpause t.eng job;
      n
    end
    else match job.finished with
      | Some Curl.CURLE_OK -> cleanup_job t.eng job; raise End_of_file
      | Some code ->
        cleanup_job t.eng job;
        (* A failed upload is the cause of the abort that follows it, so
           the caller sees that error rather than the curl code. *)
        (match upload_error job with
         | Some ex -> raise ex
         | None ->
           if job.over_limit then
             raise (err (Protocol_error
                           (Fmt.str "response body exceeds %d bytes"
                              t.eng.cfg.max_response)))
           else raise (map_curl_error code !(job.errbuf)))
      | None ->
        if job.cleaned then
          raise (err (Protocol_error "response body is no longer available"))
        else begin
          wait_for job.wake (fun () ->
              not (Queue.is_empty job.chunks)
              || job.finished <> None || job.cleaned);
          single_read t buf
        end
end

let body_stream_handler = Eio.Flow.Pi.source (module Body_stream)

module Backend = struct
  type t = engine
  type tag = [ `Generic | `Curl ]

  let closed_error () =
    err (Protocol_error "Fetch_curl: client switch already finished")

  let request eng ~sw (req : Middleware.request) =
    if Domain.self () <> eng.dom then
      invalid_arg
        "Fetch_curl: client used from a domain other than its creator's";
    if eng.closed then raise (closed_error ());
    let cfg = eng.cfg in
    (* Nothing suspends between here and [Multi.add] below, so the
       engine cannot be torn down under us in between. *)
    let body = wire_body_of cfg req in
    let h = Curl.init () in
    let id = string_of_int eng.next_id in
    eng.next_id <- eng.next_id + 1;
    Curl.set_private h id;
    let upload =
      match body with
      | No_body | Fixed _ -> None
      | Streamed _ ->
        Some { chunks = Queue.create (); wake = { u = None }; head_off = 0;
               queued = 0; read = 0; eof = false; error = None;
               paused = false }
    in
    let job = { id; h; chunks = Queue.create (); errbuf = ref "";
                upload; wake = { u = None };
                head_off = 0; queued = 0;
                received = 0; paused = false; over_limit = false;
                added = false;
                body_started = false; finished = None; cleaned = false;
                hook = Eio.Switch.null_hook } in
    try
      let col, auto_decode = setup_handle cfg h req body in
      Curl.set_errorbuffer h job.errbuf;
      Curl.set_writefunction2 h (fun s ->
          (* C callback frame: record and signal only. Refusing the data
             ([Pause]) makes libcurl keep it and deliver it again on
             unpause, so a paused job queues nothing further. *)
          job.body_started <- true;
          col.in_body <- true;  (* later header lines are trailers *)
          eng.pending_wakes <- job :: eng.pending_wakes;
          if job.queued >= high_water then (job.paused <- true; Curl.Pause)
          else if job.received + String.length s > cfg.max_response then
            (job.over_limit <- true; Curl.Abort)
          else begin
            Queue.add s job.chunks;
            job.received <- job.received + String.length s;
            job.queued <- job.queued + String.length s;
            Curl.proceed
          end);
      (match req.body, upload with
       | Stream { flow; _ }, Some up ->
         Curl.set_readfunction2 h (fun n ->
             (* C callback frame: record and signal only. [Pause] leaves
                the send stopped until the pump has more to give. *)
             if up.error <> None then Curl.Abort
             else if up.queued > 0 then begin
               let chunk = Queue.peek up.chunks in
               let avail = String.length chunk - up.head_off in
               let n = min avail n in
               let s = String.sub chunk up.head_off n in
               if n = avail then
                 (ignore (Queue.pop up.chunks : string); up.head_off <- 0)
               else up.head_off <- up.head_off + n;
               up.queued <- up.queued - n;
               eng.pending_wakes <- job :: eng.pending_wakes;
               Curl.Proceed s
             end
             else if up.eof then Curl.Proceed ""  (* end of body *)
             else (up.paused <- true; Curl.Pause));
         (* A daemon: an abandoned or cancelled request must not be held
            open by a pump still waiting on the flow. *)
         Eio.Fiber.fork_daemon ~sw (fun () -> pump eng job up flow; `Stop_daemon)
       | _ -> ());
      job.hook <-
        Eio.Switch.on_release_cancellable sw (fun () -> cleanup_job eng job);
      Hashtbl.replace eng.jobs id job;
      job.added <- true;
      curl_call eng (fun () -> Curl.Multi.add eng.mt h);
      curl_call eng (fun () -> Curl.Multi.action_timeout eng.mt);
      (* The response is ready as soon as the headers are: the first body
         chunk (or completion, for a bodyless response) signals that.
         [cleaned] also ends the wait — the switch may be released while
         we sit here, and its backstop wake must not be lost. *)
      wait_for job.wake (fun () ->
          job.body_started || job.finished <> None || job.cleaned);
      if job.cleaned then
        (* The handle is already freed; touching [h] below would be a
           use-after-free in C. *)
        raise (err (Protocol_error "request abandoned: switch released"));
      (match job.finished with
       | Some code when code <> Curl.CURLE_OK ->
         (* A failed upload is the cause of the abort that follows it, so
            the caller sees that error rather than the curl code. *)
         (match upload_error job with Some ex -> raise ex | None -> ());
         if job.over_limit then
           raise (err (Protocol_error
                         (Fmt.str "response body exceeds %d bytes" cfg.max_response)))
         else if col.headers_capped then
           raise (err (Protocol_error
                         (Fmt.str "response headers exceed %d bytes"
                            max_header_bytes)))
         else
           (* [strerror] alone here: pre-response failures are
              unambiguous, and the error-buffer text embeds timings
              ("after 3 ms") that would make error output
              nondeterministic. Mid-transfer failures (the body path)
              do attach it. *)
           raise (map_curl_error code "")
       | _ -> ());
      let status = Curl.get_responsecode h in
      let headers =
        Http.Header.of_list (List.filter_map parse_header_line (List.rev col.lines))
      in
      let headers =
        (* When we decode the body, present the decoded view. *)
        if auto_decode && Http.Header.mem headers "content-encoding" then
          Http.Header.remove (Http.Header.remove headers "content-encoding") "content-length"
        else headers
      in
      let version = version_of_status_line col.status_line in
      (* Transfer already complete (small or bodyless response): the
         handle can go now; the queued chunks outlive it. *)
      if job.finished <> None then cleanup_job eng job;
      let trailers () =
        (* Per the interface, [Some] only once the body has been fully
           consumed: the transfer succeeded *and* the reader has drained
           the queue. (For a small response the transfer can finish
           before the caller reads a byte, so [finished] alone would
           answer too early.) *)
        match job.finished, col.trailer_lines with
        | Some Curl.CURLE_OK, (_ :: _ as lines)
          when Queue.is_empty job.chunks ->
          Some (Http.Header.of_list
                  (List.filter_map parse_header_line (List.rev lines)))
        | _ -> None
      in
      Fetch.Middleware.Pi.response ~status ~headers ~version ~trailers
        ~body:(Eio.Resource.T ({ eng; job }, body_stream_handler))
        ~url:req.url ()
    with ex ->
      let bt = Printexc.get_raw_backtrace () in
      cleanup_job eng job;
      Printexc.raise_with_backtrace ex bt
end

let handler = Fetch.Middleware.Pi.client (module Backend)

let v ~sw ?(tls_verify = true) ?(http_version = `Auto) ?proxy ?timeout
    ?(connect_timeout = 30.) ?(max_response = 256 * 1024 * 1024)
    ?(max_request = 256 * 1024 * 1024)
    ?(user_agent = "fetch-curl") ?(verbose = false)
    ?max_connections_per_host ?max_total_connections ?(multiplex = true)
    () : t =
  Lazy.force global_init;
  let cfg = {
    tls_verify; http_version; proxy; timeout; connect_timeout;
    max_response; max_request; user_agent; verbose;
  } in
  let mt = Curl.Multi.create () in
  if multiplex then
    Curl.Multi.setopt mt (Curl.Multi.CURLMOPT_PIPELINING [ Curl.Multi.PIPE_MULTIPLEX ]);
  Option.iter (fun n -> Curl.Multi.setopt mt (Curl.Multi.CURLMOPT_MAX_HOST_CONNECTIONS n))
    max_connections_per_host;
  Option.iter (fun n -> Curl.Multi.setopt mt (Curl.Multi.CURLMOPT_MAX_TOTAL_CONNECTIONS n))
    max_total_connections;
  let eng = { cfg; mt; sw; dom = Domain.self (); jobs = Hashtbl.create 8;
              closed = false; watchers = Hashtbl.create 8;
              pending_sockets = []; pending_timer = None; timer_gen = 0;
              timer_stop = None; next_id = 0; pending_wakes = [] } in
  Curl.Multi.set_socket_function mt (socket_function eng);
  Curl.Multi.set_timer_function mt (timer_function eng);
  Eio.Switch.on_release sw (fun () ->
      (* libcurl requires easy handles to be removed before the multi is
         destroyed, and request switches need not be nested inside the
         client's — so detach and free every outstanding job first
         (waking its fibers), then destroy the multi and refuse any
         libcurl call that arrives later. *)
      let outstanding = Hashtbl.fold (fun _ j acc -> j :: acc) eng.jobs [] in
      List.iter (cleanup_job eng) outstanding;
      eng.closed <- true;
      Curl.Multi.cleanup mt);
  Eio.Resource.T (eng, handler)

(* The design.md s11.2 recommended stack over this backend, minted from
   stdenv capabilities: retries re-consult the jar and are paced, and
   any policy the caller stacks on top still gates every attempt. *)
let std ~sw ?(cookies = `Memory) ?retry ?(max_concurrent = 6) ?min_interval env =
  let clock = env#clock in
  let mono_clock = env#mono_clock in
  let backend = v ~sw () in
  let with_cookies =
    match cookies with
    | `Off -> fun t -> Fetch.Middleware.of_handler (Fetch.Middleware.handler t)
    | `Memory ->
      Fetch_cookies.with_jar (Fetch_cookies.Jar.in_memory ~clock ())
    | `File path ->
      Fetch_cookies.with_jar (Fetch_cookies.Jar.of_file ~clock path)
  in
  backend
  |> with_cookies
  |> Fetch.with_limits ~clock:mono_clock ?min_interval ~max_concurrent
  |> Fetch.with_retry ~clock:mono_clock ~random:env#secure_random ?config:retry
