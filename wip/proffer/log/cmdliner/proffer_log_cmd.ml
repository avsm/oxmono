(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Cmdliner

type path = Eio_unix.Stdenv.base -> Eio.Fs.dir_ty Eio.Path.t

let with_log ~path f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let p = path env in
  f env sw p (Proffer_log.open_ ~sw p)

let date_of_unix ts =
  let tm = Unix.gmtime ts in
  Printf.sprintf "%04d-%02d-%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday

let setup_term = function
  | Some t -> t
  | None -> Term.const ()

let rotate ~setup ~path =
  let run () keep batch =
    with_log ~path (fun _env _sw _p log ->
      let before = Unix.gettimeofday () -. (float_of_int keep *. 86400.) in
      let last = ref 0 in
      let progress n =
        if n - !last >= 100_000 then begin
          Printf.printf "moved %d rows\n%!" n;
          last := n
        end
      in
      let r = Proffer_log.rotate log ~before ~batch ~progress () in
      Printf.printf "Moved %d rows before %s into %s\n" r.moved
        (date_of_unix before)
        (match r.written with
         | [] -> "no archive"
         | files -> String.concat ", " files);
      0)
  in
  let keep =
    let doc = "Keep the last $(docv) days in the live file." in
    Arg.(value & opt int 90 & info [ "keep" ] ~docv:"DAYS" ~doc)
  in
  let batch =
    let doc = "Move $(docv) rows per transaction." in
    Arg.(value & opt int 20000 & info [ "batch" ] ~docv:"ROWS" ~doc)
  in
  let doc = "Move old requests into yearly archive files." in
  let man = [
    `S Manpage.s_description;
    `P "Rows older than the retention window move into an archive beside \
        the live file, one per calendar year, named after the live file \
        with the year appended. A running server keeps logging meanwhile. \
        Analytics read the archives through the live file, so nothing is \
        lost from reports.";
  ] in
  Cmd.v (Cmd.info "rotate" ~doc ~man)
    Term.(const run $ setup_term setup $ keep $ batch)

let merge ~setup ~path =
  let run () file =
    with_log ~path (fun env _sw _p log ->
      let from = Eio.Path.(Eio.Stdenv.fs env / file) in
      let n = Proffer_log.merge log ~from in
      Printf.printf "Merged %d rows from %s\n" n file;
      0)
  in
  let file =
    let doc = "Log or archive file to copy rows from." in
    Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)
  in
  let doc = "Copy every row of another log file into the live file." in
  Cmd.v (Cmd.info "merge" ~doc) Term.(const run $ setup_term setup $ file)

let vacuum ~setup ~path =
  let run () =
    with_log ~path (fun _env _sw _p log ->
      Proffer_log.vacuum log;
      print_endline "Vacuumed";
      0)
  in
  let doc = "Rebuild the live file to return freed space and let the server \
             return it nightly from then on. Stop the server first." in
  Cmd.v (Cmd.info "vacuum" ~doc) Term.(const run $ setup_term setup)

let ls ~setup ~path =
  let run () =
    with_log ~path (fun _env _sw p log ->
      Printf.printf "%-28s %10s  %s\n" (Filename.basename (snd p))
        (string_of_int (Proffer_log.live_rows log)) "live";
      List.iter
        (fun (a : Proffer_log.archive) ->
          Printf.printf "%-28s %10d  %s to %s\n" a.file a.rows
            (date_of_unix a.range_start) (date_of_unix a.range_end))
        (Proffer_log.archives log);
      0)
  in
  let doc = "List the live file and its archives with row counts." in
  Cmd.v (Cmd.info "ls" ~doc) Term.(const run $ setup_term setup)

let commands ?setup ~path () =
  [ rotate ~setup ~path; merge ~setup ~path; vacuum ~setup ~path;
    ls ~setup ~path ]

let group ?setup ?(name = "log") ~path () =
  let doc = "Maintain the access log database." in
  Cmd.group (Cmd.info name ~doc) (commands ?setup ~path ())
