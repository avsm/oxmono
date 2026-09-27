(* doc/index.mld copies this program verbatim. It is built by @all and
   never run, so the page fails to build when an interface it shows
   changes. *)

let ( let* ) = Result.bind

let important =
  match Mail_flag.Imap_flag.keyword "$Important" with
  | Ok keyword -> keyword
  | Error message -> invalid_arg message

let deliver maildir message =
  Maildir.with_writer maildir @@ fun writer ->
  let* occurrence =
    Maildir.append writer ~source:(Eio.Flow.string_source message)
      ~length:(Int64.of_int (String.length message))
      ~flags:[ Mail_flag.Imap_flag.system Seen; important ]
      ~mtime:1_700_000_000. ()
  in
  Maildir.set_flags writer occurrence
    (Mail_flag.Imap_flag.system Flagged :: occurrence.flags)

let () =
  Eio_main.run @@ fun env ->
  let path = Eio.Path.(Eio.Stdenv.fs env / "Mail" / "INBOX") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 path;
  let result =
    let* maildir = Maildir.open_dir path in
    let* occurrence = deliver maildir "Subject: hello\r\n\r\nHello.\r\n" in
    print_endline occurrence.filename;
    Maildir.fold maildir ~init:0 ~f:(fun n _ -> n + 1)
  in
  match result with
  | Ok count -> Printf.printf "%d messages\n" count
  | Error e ->
      Format.eprintf "maildir: %a@." Maildir.pp_error e;
      exit 1
