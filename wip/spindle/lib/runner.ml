(* SPDX-License-Identifier: ISC *)
open Json

type t = {
  job : Job.t;
  mutable status : string;
  mutable started : string option;
  mutable finished : string option;
  mutable error : string option;
  mutable events : Jsont.json list;
  mutable log_bytes : int;
  mutable log_seq : int64;
  log_lock : Eio.Mutex.t;
  mutable cancel : (unit -> unit) option;
  mutable cancelled : bool;
}

type env = {
  system : Eio_unix.Stdenv.base;
  directory : Eio.Fs.dir_ty Eio.Path.t;
  slots : Eio.Semaphore.t;
}

type input = {
  id : string;
  repo : string;
  source : string;
  commit : string;
  metadata : Jsont.json;
}

exception Cancelled

let now () = Ptime.to_rfc3339 ~tz_offset_s:0 (Ptime_clock.now ())
let terminal p = p.status <> "pending" && p.status <> "running"

let v job =
  {
    job;
    status = "pending";
    started = None;
    finished = None;
    error = None;
    events = [];
    log_bytes = 0;
    log_seq = 0L;
    log_lock = Eio.Mutex.create ();
    cancel = None;
    cancelled = false;
  }

let optional k f = function None -> [] | Some v -> [ (k, f v) ]

let view p =
  obj
    ([
       ("id", str p.job.name); ("name", str p.job.name); ("status", str p.status);
     ]
    @ optional "startedAt" str p.started
    @ optional "finishedAt" str p.finished
    @ optional "error" str p.error)

let snapshot p =
  obj
    [
      ("workflow", view p);
      ("events", arr (List.rev p.events));
      ( "steps",
        arr
          (List.map
             (function
               | Job.Metadata -> obj [ ("metadata", bool true) ]
               | Job.Command argv -> obj [ ("argv", arr (List.map str argv)) ])
             p.job.steps) );
    ]

let restore raw =
  let workflow = required "workflow" raw in
  let steps =
    list (required "steps" raw)
    |> List.map (fun step ->
        match field "argv" step with
        | Some argv -> Job.Command (List.map string (list argv))
        | None -> Job.Metadata)
  in
  let p = v (Job.v (get "name" workflow) steps) in
  p.status <- get "status" workflow;
  p.started <- Option.map string (field "startedAt" workflow);
  p.finished <- Option.map string (field "finishedAt" workflow);
  p.error <- Option.map string (field "error" workflow);
  p.events <- List.rev (list (required "events" raw));
  p.log_bytes <-
    List.fold_left (fun n e -> n + String.length (encode e)) 0 p.events;
  if p.status = "running" then (
    p.status <- "failed";
    p.error <- Some "spindle restarted during workflow";
    p.finished <- Some (now ()));
  p

let emit record p ~step ~kind fields =
  Lock.protect p.log_lock @@ fun () ->
  let event =
    obj
      ([
         ("type", str kind);
         ("time", str (now ()));
         ("workflow", str p.job.name);
         ("step", int step);
       ]
      @ fields)
  in
  let raw = encode event in
  let size = String.length raw in
  if p.log_bytes + size > 1024 * 1024 then failwith "job log exceeds 1 MiB";
  let seq = record raw in
  p.log_seq <- seq;
  p.log_bytes <- p.log_bytes + size;
  p.events <- event :: p.events

let control record persist p ~step ~status command =
  emit record p ~step ~kind:"control"
    [
      ("content", str command);
      ("command", str command);
      ("status", str status);
      ("kind", str "user");
    ];
  persist ()

let data record p ~step ~stream content =
  emit record p ~step ~kind:"data"
    [ ("content", str content); ("stream", str stream) ]

let environment input =
  Eio.Process.Env.of_bindings
    [
      ("PATH", "/usr/bin:/bin");
      ("LANG", "C");
      ("LC_ALL", "C");
      ("GIT_CONFIG_NOSYSTEM", "1");
      ("GIT_CONFIG_GLOBAL", "/dev/null");
      ("GIT_TERMINAL_PROMPT", "0");
      ("GIT_ALLOW_PROTOCOL", "file:https:http");
      ("TANGLED_REPO", input.repo);
      ("TANGLED_COMMIT_SHA", input.commit);
      ("TANGLED_PIPELINE_ID", input.id);
      ( "SSL_CERT_FILE",
        Option.value ~default:"/etc/ssl/certs/ca-certificates.crt"
          (Sys.getenv_opt "SSL_CERT_FILE") );
      ( "GIT_SSL_CAINFO",
        Option.value ~default:"/etc/ssl/certs/ca-certificates.crt"
          (Sys.getenv_opt "SSL_CERT_FILE") );
      ("SPINDLE_REQUEST", encode input.metadata);
    ]

(* Log frames contain text. Carry incomplete UTF-8 characters between reads,
   replacing malformed bytes while preserving whitespace and partial lines. *)
let utf8 ~eof raw =
  if String.is_valid_utf_8 raw then (raw, "")
  else
    let output = Buffer.create (String.length raw) in
    let rec loop off =
      if off = String.length raw then (Buffer.contents output, "")
      else
        let width =
          match raw.[off] with
          | '\194' .. '\223' -> 2
          | '\224' .. '\239' -> 3
          | '\240' .. '\244' -> 4
          | _ -> 1
        in
        if (not eof) && String.length raw - off < width then
          (Buffer.contents output, String.sub raw off (String.length raw - off))
        else
          let decoded = String.get_utf_8_uchar raw off in
          Buffer.add_utf_8_uchar output (Uchar.utf_decode_uchar decoded);
          loop (off + Uchar.utf_decode_length decoded)
    in
    loop 0

let command state input ~output:emit_output ~text ~cwd argv =
  Eio.Switch.run @@ fun sw ->
  let mgr = state.system#process_mgr in
  let output, output_write = Eio.Process.pipe ~sw mgr in
  let errors, errors_write = Eio.Process.pipe ~sw mgr in
  let env = environment input in
  let stdin = Eio.Path.open_in ~sw Eio.Path.(state.system#fs / "/dev/null") in
  let fd flow = Option.get (Eio_unix.Resource.fd_opt flow) in
  let process =
    Eio_unix.Process.spawn_unix ~sw mgr ~cwd ~env ~pgid:0
      ~fds:
        [
          (0, fd stdin, `Blocking);
          (1, fd output_write, `Blocking);
          (2, fd errors_write, `Blocking);
        ]
      argv
  in
  (* Shell pipelines and background children inherit this group. Killing only
     the immediate child leaves them running after cancellation or timeout. *)
  let kill_group () =
    try Unix.kill (-Eio.Process.pid process) Sys.sigkill
    with Unix.Unix_error (Unix.ESRCH, _, _) -> ()
  in
  Fun.protect ~finally:kill_group @@ fun () ->
  Eio.Flow.close output_write;
  Eio.Flow.close errors_write;
  let read stream flow () =
    let reader = Eio.Buf_read.of_flow ~initial_size:8192 ~max_size:8192 flow in
    let emit content = if content <> "" then emit_output ~stream content in
    let rec loop pending =
      match Eio.Buf_read.ensure reader 1 with
      | () ->
          let raw =
            Eio.Buf_read.take (Eio.Buf_read.buffered_bytes reader) reader
          in
          if text then (
            let raw = if pending = "" then raw else pending ^ raw in
            let content, pending = utf8 ~eof:false raw in
            emit content;
            loop pending)
          else (
            emit raw;
            loop "")
      | exception End_of_file ->
          if text then emit (fst (utf8 ~eof:true pending))
    in
    loop ""
  in
  Eio.Fiber.all
    [
      read "stdout" output;
      read "stderr" errors;
      (fun () ->
        Eio.Process.await_exn process;
        kill_group ());
    ]

let git args = "git" :: "-c" :: "core.hooksPath=/dev/null" :: args

let capture state input argv =
  let output = Buffer.create 4096 in
  let size = ref 0 in
  command state input ~cwd:state.directory ~text:false
    ~output:(fun ~stream content ->
      size := !size + String.length content;
      if !size > 1024 * 1024 then failwith "command output exceeds 1 MiB";
      if stream = "stdout" then Buffer.add_string output content)
    argv;
  Buffer.contents output

let execute state input ~record ~persist p =
  let open Eio.Path in
  let work = state.directory / (input.id ^ "." ^ p.job.name ^ ".work") in
  Eio.Semaphore.acquire state.slots;
  Fun.protect ~finally:(fun () -> Eio.Semaphore.release state.slots)
  @@ fun () ->
  if not p.cancelled then (
    p.status <- "running";
    p.started <- Some (now ());
    persist ();
    let result =
      try
        if p.cancelled then raise Cancelled;
        Eio.Time.with_timeout_exn state.system#clock 60. @@ fun () ->
        Eio.Switch.run @@ fun sw ->
        p.cancel <- Some (fun () -> Eio.Switch.fail sw Cancelled);
        control record persist p ~step:0 ~status:"start" "checkout";
        command state input ~output:(data record p ~step:0) ~text:true
          ~cwd:state.directory
          (git
             [
               "clone";
               "--no-checkout";
               "--no-hardlinks";
               "--";
               input.source;
               native_exn work;
             ]);
        command state input ~output:(data record p ~step:0) ~text:true ~cwd:work
          (git [ "checkout"; "--detach"; input.commit ]);
        control record persist p ~step:0 ~status:"end" "checkout";
        List.iteri
          (fun index action ->
            let step = index + 1 in
            let name =
              match action with
              | Job.Metadata -> "spindle request metadata"
              | Job.Command argv -> String.concat " " argv
            in
            control record persist p ~step ~status:"start" name;
            (match action with
            | Job.Metadata ->
                let text = encode input.metadata in
                data record p ~step ~stream:"stdout" (text ^ "\n");
                Printf.printf "%s\n%!" text
            | Job.Command argv ->
                command state input ~output:(data record p ~step) ~text:true
                  ~cwd:work argv);
            control record persist p ~step ~status:"end" name)
          p.job.steps;
        Ok ()
      with exn -> Error exn
    in
    Eio.Cancel.protect (fun () ->
        p.cancel <- None;
        p.finished <- Some (now ());
        (match result with
        | Ok () -> p.status <- "success"
        | Error exn ->
            p.status <-
              (if p.cancelled then "cancelled"
               else
                 match exn with Eio.Time.Timeout -> "timeout" | _ -> "failed");
            p.error <- Some (Printexc.to_string exn));
        persist ();
        (* Only the generated TID workspace is removed. *)
        Eio.Process.run state.system#process_mgr
          [ "rm"; "-rf"; "--"; native_exn work ]))

let cancel ~persist p =
  if not (terminal p) then (
    p.cancelled <- true;
    match p.cancel with
    | Some cancel -> cancel ()
    | None ->
        p.status <- "cancelled";
        p.finished <- Some (now ());
        persist ())
