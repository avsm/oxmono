(* doc/sync.mld copies this program verbatim. It is built by @all and
   never run, so the page fails to build when an interface it shows
   changes. *)

let ( let* ) = Result.bind

let fresh random prefix () =
  let bytes = Cstruct.create 16 in
  Eio.Flow.read_exact random bytes;
  prefix ^ Cstruct.to_hex_string bytes

let open_state ~sw fs =
  let state = Eio.Path.(fs / "state") in
  let blob_dir = Eio.Path.(state / "blobs")
  and spool_dir = Eio.Path.(state / "spool") in
  List.iter
    (Eio.Path.mkdirs ~exists_ok:true ~perm:0o700)
    [ blob_dir; spool_dir ];
  let store =
    Imap_store.open_path ~sw ~blob_dir Eio.Path.(state / "sync.sqlite3")
  in
  let* maildir =
    Maildir.open_dir Eio.Path.(fs / "Mail" / "INBOX")
    |> Result.map_error (fun e -> Imap_sync.Error.Maildir e)
  in
  let* () = Imap_sync.Bridge.recover_local ~maildir ~spool_dir () in
  Maildir.with_writer maildir (fun _writer ->
      Imap_store.Blob.reap_orphans_iter store ~removed:ignore);
  Ok (store, maildir, spool_dir)

let cycle ~random ~store ~maildir ~spool_dir client =
  let scope =
    { Imap.Mirror.endpoint = "imaps://imap.example.org"; account = "alice";
      mailbox_key = "inbox"; raw_name = "INBOX";
      encoding = Imap_eio.Client.mailbox_mode client; mailbox_id = None }
  in
  let* ctx =
    Imap_sync.Ctx.v ~client ~store ~scope ~mailbox:"INBOX" ~spool_dir
      ~next_id:(fresh random "op-")
  in
  let* receipt =
    Imap_sync.Bridge.copy_once ~ctx ~maildir
      ~stage_id:(fresh random "stage-" ()) ()
  in
  Format.printf "to Maildir %d, to IMAP %d, flags %d, deletions %d@."
    receipt.remote_to_local receipt.local_to_remote receipt.flags_updated
    receipt.deletions;
  if receipt.flags_held > 0 || receipt.deletions_held > 0 then
    Format.printf "held pairs: %s@." (String.concat " " receipt.held_pair_ids);
  Ok receipt.more

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let random = Eio.Stdenv.secure_random env in
  let transport =
    Imap_eio.Transport.v ~net:(Eio.Stdenv.net env) ~host:"imap.example.org" ()
  in
  let auth =
    Imap_eio.Auth.password ~username:"alice" ~password:"secret" ()
  in
  let result =
    let* store, maildir, spool_dir = open_state ~sw (Eio.Stdenv.fs env) in
    let* client =
      Imap_eio.Client.connect ~sw ~auth transport
      |> Result.map_error (fun e -> Imap_sync.Error.Client e)
    in
    Fun.protect ~finally:(fun () -> Imap_eio.Client.close client) (fun () ->
        cycle ~random ~store ~maildir ~spool_dir client)
  in
  match result with
  | Ok more -> if more then print_endline "more work remains"
  | Error e ->
      Format.eprintf "sync: %a@." Imap_sync.Error.pp e;
      exit 1
