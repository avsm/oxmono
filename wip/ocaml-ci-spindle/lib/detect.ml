(* SPDX-License-Identifier: ISC *)

module Executor = Spindle.Executor

let default_image = "debian:13-slim"
let source = "/src"
let command = "brief -json " ^ source

let quote s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (function
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let releases = "https://github.com/git-pkgs/brief/releases"

(* The release's checksums name the newest version, and the archive they name
   for the container's architecture is checked against them. *)
let install =
  String.concat " && "
    [
      "apt-get update";
      "apt-get install -y --no-install-recommends ca-certificates curl";
      "rm -rf /var/lib/apt/lists/*";
      "cd /tmp";
      "arch=$(dpkg --print-architecture)";
      Printf.sprintf
        "curl -fsSLo checksums.txt %s/latest/download/checksums.txt" releases;
      "file=$(sed -n \"s/^[0-9a-f]*  \
       *\\\\(brief_[0-9.]*_linux_${arch}\\\\.tar\\\\.gz\\\\)$/\\\\1/p\" \
       checksums.txt)";
      "test -n \"$file\"";
      "version=${file#brief_}";
      "version=${version%_linux_*}";
      Printf.sprintf "curl -fsSLo \"$file\" \"%s/download/v${version}/${file}\""
        releases;
      "grep \" ${file}$\" checksums.txt | sha256sum -c -";
      "tar -xzf \"$file\" brief";
      "install -m 0755 brief /usr/local/bin/brief";
      "rm -f checksums.txt \"$file\" brief";
      "brief -version";
    ]

let spec ?(image = default_image) refreshed =
  let run ?(network = false) cmd =
    Printf.sprintf "(run %s(shell %s))"
      (if network then "(network host) " else "")
      (quote cmd)
  in
  String.concat "\n "
    [
      Printf.sprintf "((from %s)" image;
      "(comment \"brief image\")";
      run ~network:true (Printf.sprintf "%s # refreshed %s" install refreshed);
      run (Printf.sprintf "mkdir -p %s && chown 1000:1000 %s" source source);
      "(user (uid 1000) (gid 1000))";
      Printf.sprintf "(workdir %s)" source;
      "(comment detect)";
      Printf.sprintf "(copy (src .) (dst %s))" source;
      run command ^ ")";
    ]
  ^ "\n"

(* OBuilder heads each operation's output with the operation, after the
   working directory and a colon. *)
let runs_brief line =
  let suffix = "(shell " ^ quote command ^ "))" in
  String.ends_with ~suffix (String.trim line)
  && String.length line > String.length suffix

let report_of_log lines =
  let rec after_run = function
    | [] -> None
    | l :: rest -> if runs_brief l then Some rest else after_run rest
  in
  let rec last_run lines =
    match after_run lines with
    | None -> None
    | Some rest -> (
        match last_run rest with None -> Some rest | some -> some)
  in
  match last_run lines with
  | None -> Error "the job did not run brief"
  | Some rest -> (
      let rec from_brace = function
        | [] -> []
        | "{" :: rest -> "{" :: rest
        | _ :: rest -> from_brace rest
      in
      let rec to_brace acc = function
        | [] -> None
        | "}" :: rest -> (
            match to_brace ("}" :: acc) rest with
            | None -> Some (List.rev ("}" :: acc))
            | some -> some)
        | l :: rest -> to_brace (l :: acc) rest
      in
      match to_brace [] (from_brace rest) with
      | Some json -> Ok (String.concat "\n" json ^ "\n")
      | None -> Error "brief wrote no report")

let today () =
  let (y, m, d), _ = Ptime.to_date_time (Ptime_clock.now ()) in
  Printf.sprintf "%04d-%02d-%02d" y m d

let scan ?image ?(today = today) ~pool ~clock scheduler : Spindle_brief.scan =
 fun (ctx : Executor.context) (r : Executor.request) ->
  let spec = spec ?image (today ()) in
  let submission = Ocluster_client.submission scheduler in
  Fun.protect ~finally:(fun () -> Ocluster_client.release_submission submission)
  @@ fun () ->
  ctx.log `Stdout spec;
  let ticket =
    Ocluster_client.submit submission ~pool ~cache_hint:"brief"
      ~src:(r.source, [ r.commit ]) spec
  in
  let job = Ocluster_client.job ticket in
  Fun.protect ~finally:(fun () ->
      Ocluster_client.release_job job;
      Ocluster_client.release ticket)
  @@ fun () ->
  try
    ctx.queued (fun () ->
        match Ocluster_client.started job with
        | Ok () -> ()
        | Error e -> failwith ("OCluster did not start the job: " ^ e));
    (* The log shows the job up to brief's own output, which the report step
       holds. *)
    let lines = ref [] and partial = Buffer.create 256 and quiet = ref false in
    let line l =
      lines := l :: !lines;
      if not !quiet then ctx.log `Stdout (l ^ "\n");
      if runs_brief l then quiet := true
    in
    Ocluster_client.stream_log job (fun chunk ->
        String.iter
          (fun c ->
            if c = '\n' then (
              line (Buffer.contents partial);
              Buffer.clear partial)
            else Buffer.add_char partial c)
          chunk);
    if Buffer.length partial > 0 then line (Buffer.contents partial);
    (match Ocluster_client.result job with
    | Ok _ -> ()
    | Error message ->
        ctx.log `Stderr (message ^ "\n");
        failwith message);
    match report_of_log (List.rev !lines) with
    | Ok json -> json
    | Error e -> failwith e
  with Eio.Cancel.Cancelled _ as e ->
    Eio.Cancel.protect (fun () ->
        try
          Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 30.)
            (fun () -> Ocluster_client.cancel_ticket ticket)
        with _ -> ());
    raise e
