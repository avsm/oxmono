(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api
module Key = Tangled.Api.Lex.Key.SshKey

let collection = "org.tangled.key.sshKey"

(* The key material goes on a line of its own, outside any frame, so that
   it can be copied whole. *)
let show_keys keys =
  Ui.print (Ui.heading "SSH keys" (List.length keys));
  List.iter
    (fun (rkey, (k : Key.main)) ->
      Fmt.pr "@.";
      Ui.print
        (Ui.V.vcat
           [
             Ui.V.concat [ Ui.title k.name; Ui.muted ("  " ^ rkey) ];
             Ui.V.hcat ~gap:0
               [
                 Ui.text "  ";
                 Ui.fields
                   [
                     ( "Usages",
                       Some
                         (Ui.text
                            (String.concat ", "
                               (Option.value ~default:[ "auth"; "sign" ]
                                  k.usages))) );
                     ("Added", Some (Ui.muted k.created_at));
                   ];
               ];
           ]);
      Fmt.pr "  %s@." (Console_text.sanitize k.public_key))
    keys

let list =
  let action user json =
    Common.run (fun env ->
        let records =
          Common.with_api ~busy:"Reading SSH keys" env (fun api ->
              Api.list_records api ~did:(Common.did api user) ~collection ())
        in
        if json || not (Ui.tty ()) then
          Common.print
            (Jsont.list Api.Atproto.Repo.ListRecords.record_jsont)
            records
        else
          show_keys
            (List.map
               (fun (r : Api.Atproto.Repo.ListRecords.record) ->
                 (Api.rkey_of_uri r.uri, Api.decode Key.main_jsont r.value))
               records))
  in
  Common.cmd
    (Cmd.info "list" ~doc:"List a user's SSH public keys.")
    Term.(term_result (const action $ Common.user $ Common.json_flag))

let add =
  let file =
    Common.positional 0 "FILE"
      "The public key file, such as $(i,~/.ssh/id_ed25519.pub)."
  in
  let name =
    Arg.(
      required
      & opt (some string) None
      & info [ "name" ] ~docv:"NAME"
          ~doc:
            "A name to tell the key apart by, such as the computer it is on.")
  in
  let sign =
    Arg.(
      value & flag
      & info [ "sign" ]
          ~doc:
            "Also use the key to verify commits you sign with it, which then \
             show as verified.")
  in
  let action file name sign =
    Common.run (fun env ->
        let key = String.trim (Common.read_file file) in
        let words = String.split_on_char ' ' key |> List.filter (( <> ) "") in
        (match words with
        | algorithm :: _ :: _
          when (String.starts_with ~prefix:"ssh-" algorithm
               || String.starts_with ~prefix:"ecdsa-" algorithm
               || String.starts_with ~prefix:"sk-" algorithm)
               && not (String.contains key '\n' || String.contains key '\r') ->
            ()
        | _ ->
            invalid_arg
              "Expected one OpenSSH public key, not private key material");
        let record : Key.main =
          {
            public_key = key;
            name;
            usages = Some (if sign then [ "auth"; "sign" ] else [ "auth" ]);
            created_at = Api.now ();
          }
        in
        let value = Api.encode Key.main_jsont record in
        Tangled.Schema.validate ~nsid:collection
          (Tangled.Schema.main collection)
          value;
        let created =
          Common.with_api ~busy:"Publishing key" env (fun api ->
              Api.create_record api ~collection value)
        in
        if Ui.tty () then
          Ui.ok "Published key %s as %s" name (Api.rkey_of_uri created.uri)
        else Common.print Api.Atproto.Repo.CreateRecord.output_jsont created)
  in
  Common.cmd
    (Cmd.info "add" ~doc:"Add an SSH public key, so that you can push with it.")
    Term.(term_result (const action $ file $ name $ sign))

let remove =
  let rkey =
    Common.positional 0 "ID" "The key's ID, as $(b,key list) shows it."
  in
  let action rkey =
    Common.run (fun env ->
        Common.with_api ~busy:"Removing key" env (fun api ->
            match
              Api.get_record api ~did:(Api.get_did api) ~collection ~rkey ()
            with
            | None -> failwith "Key not found"
            | Some record ->
                let cid =
                  match record.cid with
                  | Some cid -> cid
                  | None -> failwith "Missing record CID"
                in
                Api.delete_record api ~collection ~rkey ~swap_record:cid ());
        Ui.ok "Removed key %s" rkey)
  in
  Common.cmd
    (Cmd.info "remove" ~doc:"Remove one of your SSH keys.")
    Term.(term_result (const action $ rkey))

let cmd =
  Cmd.group
    (Cmd.info "key" ~doc:"Manage the SSH keys you push with."
       ~man:
         (Common.man
            [
              "Knots let you push over SSH with the keys added here. Your keys \
               are public, so anyone can list them, and only the public half \
               of a key is ever added.";
            ]))
    [ list; add; remove ]
