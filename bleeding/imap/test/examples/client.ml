(* doc/client.mld copies this program verbatim. It is built by @all and
   never run, so the page fails to build when an interface it shows
   changes. *)

let ( let* ) = Result.bind

let archive selected ~set =
  let open Imap_eio in
  match Selected.Move.require selected with
  | Ok move ->
      let* _receipt = Selected.Move.uid_move move ~set ~mailbox:"Archive" in
      Ok ()
  | Error (Error.Unsupported _) ->
      let* info = Selected.info selected in
      let* condstore = Selected.Condstore.require selected in
      let* _receipt = Selected.uid_copy selected ~set ~mailbox:"Archive" in
      let* receipt =
        Selected.Condstore.uid_store_flags condstore ~set ~operation:`Add
          ~flags:[ Mail_flag.Imap_flag.system Deleted ]
          ~unchangedsince:(Option.value info.highestmodseq ~default:0L)
      in
      if not (Imap.Uid_set.is_empty receipt.modified) then
        Format.printf "changed since selection: %a@." Imap.Uid_set.pp
          receipt.modified;
      Ok ()
  | Error e -> Error e

let archive_any selected ~set =
  let open Imap_eio in
  let moved =
    Mailbox.move (Mailbox.of_selected selected) ~set ~mailbox:"Archive"
  in
  let strategy =
    match moved.strategy with
    | `Move -> "MOVE"
    | `Copy_then_expunge -> "COPY, STORE and UID EXPUNGE"
    | `Copy_then_flag -> "COPY and STORE, expunge pending"
    | `Copied _ -> "COPY, then a failed STORE"
    | `Copied_and_flagged _ -> "COPY and STORE, then a failed EXPUNGE"
  in
  Format.printf "archive used %s@." strategy;
  Result.map ignore moved.result

let archive_old selected =
  let criteria =
    Imap.Search.(And [ Seen; Before { day = 1; month = 1; year = 2025 } ])
  in
  let* uids = Imap_eio.Selected.uid_search selected ~criteria in
  let uids = List.filteri (fun i _ -> i < 1000) uids in
  let* rows =
    Imap_eio.Selected.fetch selected ~uids
      ~items:[ Imap.Fetch_item.Internal_date; Rfc822_size ]
  in
  List.iter
    (fun (row : Imap_eio.Selected.row) ->
      Format.printf "%a %Ld octets@." Imap.Uid.pp row.uid
        (Option.value row.size ~default:0L))
    rows;
  if uids = [] then Ok ()
  else archive selected ~set:(Imap.Uid_set.of_list uids)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let transport =
    Imap_eio.Transport.v ~net:(Eio.Stdenv.net env) ~host:"imap.example.org" ()
  in
  let auth =
    Imap_eio.Auth.password ~username:"alice" ~password:"secret" ()
  in
  let result =
    let* client = Imap_eio.Client.connect ~sw ~auth transport in
    match
      Imap_eio.Client.with_mailbox client ~mode:`Read_write "INBOX"
        archive_old
    with
    | Ok () -> Imap_eio.Client.logout client
    | Error _ as error ->
        Imap_eio.Client.close client;
        error
  in
  match result with
  | Ok () -> ()
  | Error e ->
      Format.eprintf "imap: %a@." Imap_eio.Client.pp_error e;
      exit 1
