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

let emit p ~step ~kind fields =
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
  let size = String.length (encode event) in
  if p.log_bytes + size > 1024 * 1024 then failwith "job log exceeds 1 MiB";
  p.log_bytes <- p.log_bytes + size;
  p.events <- event :: p.events

let control persist p ~step ~status command =
  emit p ~step ~kind:"control"
    [
      ("content", str command);
      ("command", str command);
      ("status", str status);
      ("kind", str "user");
    ];
  persist ()

let data p ~step ~stream content =
  emit p ~step ~kind:"data" [ ("content", str content); ("stream", str stream) ]

let command state input p ~cwd ~step argv =
  Eio.Switch.run @@ fun sw ->
  let mgr = state.system#process_mgr in
  let output, output_write = Eio.Process.pipe ~sw mgr in
  let errors, errors_write = Eio.Process.pipe ~sw mgr in
  let env =
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
  in
  let process =
    Eio.Process.spawn ~sw mgr ~cwd ~env
      ~stdin:(Eio.Flow.string_source "")
      ~stdout:output_write ~stderr:errors_write argv
  in
  Eio.Flow.close output_write;
  Eio.Flow.close errors_write;
  let read stream flow () =
    let reader = Eio.Buf_read.of_flow ~max_size:65536 flow in
    let rec loop () =
      match Eio.Buf_read.line reader with
      | line ->
          data p ~step ~stream (line ^ "\n");
          loop ()
      | exception End_of_file -> ()
    in
    loop ()
  in
  Eio.Fiber.all
    [
      read "stdout" output;
      read "stderr" errors;
      (fun () -> Eio.Process.await_exn process);
    ]

let git args = "git" :: "-c" :: "core.hooksPath=/dev/null" :: args

let execute state input ~persist p =
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
        control persist p ~step:0 ~status:"start" "checkout";
        command state input p ~cwd:state.directory ~step:0
          (git
             [
               "clone";
               "--no-checkout";
               "--no-hardlinks";
               "--";
               input.source;
               native_exn work;
             ]);
        command state input p ~cwd:work ~step:0
          (git [ "checkout"; "--detach"; input.commit ]);
        control persist p ~step:0 ~status:"end" "checkout";
        List.iteri
          (fun index action ->
            let step = index + 1 in
            let name =
              match action with
              | Job.Metadata -> "spindle request metadata"
              | Job.Command argv -> String.concat " " argv
            in
            control persist p ~step ~status:"start" name;
            (match action with
            | Job.Metadata ->
                let text = encode input.metadata in
                data p ~step ~stream:"stdout" (text ^ "\n");
                Printf.printf "%s\n%!" text
            | Job.Command argv -> command state input p ~cwd:work ~step argv);
            control persist p ~step ~status:"end" name)
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
