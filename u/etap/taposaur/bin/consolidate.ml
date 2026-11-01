open Taposaur
module A = Taposaur_archive
module S = Taposaur_review_store
module G = Taposaur_services.Grobid
module P = Taposaur_regeneration
let ( / ) = Eio.Path.( / )
let get = function Ok x -> x | Error e -> failwith e
let load p = let ch = open_in_bin p in
  Fun.protect ~finally:(fun () -> close_in ch) (fun () ->
    really_input_string ch (in_channel_length ch))
let save p s =
  let ch = open_out_bin (p ^ ".tmp") in
  Fun.protect ~finally:(fun () -> close_out ch) (fun () ->
    output_string ch s; flush ch; Unix.fsync (Unix.descr_of_out_channel ch));
  Unix.rename (p ^ ".tmp") p
let run root db reviews snapshot work server jobs limit only =
  try Eio_main.run (fun env -> Eio.Switch.run (fun sw ->
    if jobs < 1 || jobs > 4 then invalid_arg "jobs must be 1..4";
    let audit = Taposaur_audit_cli.open_session ~sw ~fs:env#fs
      ~component:"taposaur.consolidate" in
    Taposaur_audit.with_operation ~session:audit
      ~action:"consolidation.batch" ~subject:("snapshot:" ^ string_of_int snapshot)
      ~details:["work",work;"archive_root",root;"catalogue",db;
        "jobs",string_of_int jobs]
      (fun scoped -> let audit = Option.get scoped in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (env#fs / work);
    let batch_lock = Unix.openfile (Filename.concat work "batch.lock")
      [Unix.O_RDWR;Unix.O_CREAT] 0o600 in
    Eio.Switch.on_release sw (fun () -> Unix.close batch_lock);
    Unix.lockf batch_lock Unix.F_TLOCK 0;
    let reviews = S.open_read ~sw (env#fs / reviews) in
    let dois = S.export reviews snapshot Taposaur_review.Dois
      |> String.split_on_char '\n' |> List.filter ((<>) "") in
    let identity = Printf.sprintf "%s\n%s\n%d\n%s\n%s"
      root db snapshot server (Digest.to_hex (Digest.string
        (String.concat "\n" dois))) in
    let identity_file = Filename.concat work "batch.identity" in
    if Sys.file_exists identity_file then begin
      if load identity_file <> identity then
        failwith "Work directory belongs to a different batch"
    end else save identity_file identity;
    let archive = A.open_root ~sw (env#fs / root) in
    let catalog = Taposaur_catalog.open_write ~sw (env#fs / db) in
    let client = Fetch_main.std ~sw ~max_concurrent:jobs
      ~retry:(Fetch.Retry.v ~max_retries:0 ()) env
      |> G.client ~server in
    let done_ = ref 0 and updated = ref 0 and skipped = ref 0
    and failed = ref 0 and resumed = ref 0 in
    let report uid state message =
      let quote s = get (Jsont_bytesrw.encode_string Jsont.string s) in
      Printf.printf "{\"uid\":%s,\"state\":%s,\"message\":%s}\n%!"
        (quote uid) (quote state) (quote message);
      Taposaur_audit.record audit ~action:"consolidation.paper.result"
        ~subject:("doi:" ^ uid) ~details:["outcome",state] ();
      incr done_;
      save (Filename.concat work "status.json") (Printf.sprintf
        "{\"total\":%d,\"examined\":%d,\"updated\":%d,\"skipped\":%d,\"failed\":%d,\"resumed\":%d,\"time\":%.0f}\n"
        (List.length dois) !done_ !updated !skipped !failed !resumed
        (Unix.gettimeofday ())) in
    let dois = match only with None -> dois | Some doi ->
      if not (List.mem doi dois) then invalid_arg "DOI is outside snapshot";
      [doi] in
    let dois = match limit with None -> dois
      | Some n -> List.filteri (fun i _ -> i < n) dois in
    Eio.Fiber.List.iter ~max_fibers:jobs (fun doi ->
      let uid = get (Uid.of_input doi) in
      let dir = Filename.concat work (Digest.to_hex (Digest.string doi)) in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (env#fs / dir);
      let path n = Filename.concat dir n in
      try
        if Sys.file_exists (path "done") then begin
          incr resumed; report doi "resumed" (load (path "done"))
        end else match A.find archive uid with
        | None -> incr skipped; report doi "skipped" "No archived paper"
        | Some paper ->
          let pdf = List.find_opt (fun (a : Manifest.Artifact.t) ->
            a.format = Pdf) (A.manifest paper).artifacts in
          match pdf with
          | None -> incr skipped; report doi "skipped" "No archived PDF"
          | Some pdf ->
            save (path "doi") doi;
            let xml = if Sys.file_exists (path "new.tei.xml") &&
                         Sys.file_exists (path "intent") then
                load (path "new.tei.xml")
              else if Sys.file_exists (path "validated.xml") then
                load (path "validated.xml")
              else begin
                let input = A.read_artifact ~max_bytes:(128*1024*1024)
                  paper pdf.file in
                save (path "input.sha256")
                  Digestif.SHA256.(to_hex (digest_string input));
                let xml = Taposaur_audit.with_operation ~session:audit
                  ~action:"conversion.grobid" ~subject:(Uid.to_string uid)
                  ~details:["input",pdf.file;"response_file",path "response.xml";
                    "header_consolidation","full";
                    "input_sha256",load (path "input.sha256")]
                  (fun _ -> Eio.Time.with_timeout_exn env#clock 180.
                  (fun () -> Eio.Switch.run (fun sw ->
                    let response = G.process_fulltext ~sw client input
                      ~filename:pdf.file ~consolidate_header:Full
                      ~consolidate_citations:Off ~consolidate_funders:Off in
                    let body = Eio.Buf_read.(take_all
                      (of_flow ~max_size:(16*1024*1024)
                        (Fetch.body response))) in
                    save (path "response.xml") body;
                    save (path "http-status")
                      (string_of_int (Fetch.status response));
                    if Fetch.status response <> 200 then
                      failwith (Printf.sprintf "GROBID HTTP %d"
                        (Fetch.status response));
                    ignore (P.validate uid body); body))) in
                save (path "validated.xml") xml; xml
              end in
            let b = P.validate uid xml in
            P.publish ~audit ~root ~work:dir uid ~input:pdf.file ~xml;
            let result = ref None in
            ignore (Taposaur_indexing.Enrich.run ~audit ~uid ~force:true ~jobs:1
              ~progress:(fun _ r -> result := Some r) catalog archive);
            (match !result with
             | Some (Updated | Partial _) -> ()
             | _ -> failwith "Published TEI but catalogue refresh needs retry");
            let publisher = Option.value ~default:"" b.publisher in
            save (path "done") publisher;
            incr updated; report doi "updated" publisher
      with
      | (Failure _ | Invalid_argument _ | Eio.Io _ | Unix.Unix_error _ |
         Eio.Time.Timeout | A.Parse_error _) as ex ->
          incr failed; report doi "failed" (Printexc.to_string ex)) dois;
    save (Filename.concat work "finished")
      (Printf.sprintf "updated=%d skipped=%d failed=%d resumed=%d\n"
        !updated !skipped !failed !resumed);
    Taposaur_audit.record audit ~action:"consolidation.batch.result"
      ~subject:("snapshot:" ^ string_of_int snapshot)
      ~details:["updated",string_of_int !updated;"failed",string_of_int !failed;
        "skipped",string_of_int !skipped;"resumed",string_of_int !resumed] ())));
    Ok ()
  with ex -> Error (Printexc.to_string ex)
open Cmdliner
let str name default = Arg.(value & opt string default & info [name])
let () = exit (Cmd.eval_result (Cmd.v
  (Cmd.info "taposaur-consolidate" ~doc:"Consolidate archived review PDFs.")
  Term.(const run $
    str "root" "/data/papers/taposaur" $ str "db" "taposaur.db" $
    str "reviews-db" "reviews.db" $
    Arg.(required & opt (some int) None & info ["snapshot"]) $
    Arg.(required & opt (some string) None & info ["work"]) $
    str "server" "http://localhost:8070" $
    Arg.(value & opt int 2 & info ["jobs"]) $
    Arg.(value & opt (some int) None & info ["limit"]) $
    Arg.(value & opt (some string) None & info ["doi"])) ))
