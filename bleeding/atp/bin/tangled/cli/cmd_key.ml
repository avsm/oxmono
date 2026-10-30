(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api
module Key = Tangled.Api.Lex.Key.SshKey

let collection = "org.tangled.key.sshKey"

let list =
  let action user =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let did = Common.did api user in
            Common.print
              (Jsont.list Api.Atproto.Repo.ListRecords.record_jsont)
              (Api.list_records api ~did ~collection ())))
  in
  Cmd.v
    (Cmd.info "list"
       ~doc:"List SSH keys with their record keys and complete public material.")
    Term.(term_result (const action $ Common.user))

let add =
  let file = Common.positional 0 "FILE" "OpenSSH public key file." in
  let name =
    Arg.(required & opt (some string) None & info [ "name" ] ~docv:"NAME")
  in
  let sign =
    Arg.(
      value & flag
      & info [ "sign" ]
          ~doc:"Also publish the key for commit signature verification.")
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
        Common.with_api env (fun api ->
            Common.print Api.Atproto.Repo.CreateRecord.output_jsont
              (Api.create_record api ~collection value)))
  in
  Cmd.v
    (Cmd.info "add" ~doc:"Publish an SSH public key.")
    Term.(term_result (const action $ file $ name $ sign))

let remove =
  let rkey = Common.positional 0 "RKEY" "SSH key record key." in
  let action rkey =
    Common.run (fun env ->
        Common.with_api env (fun api ->
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
                Api.delete_record api ~collection ~rkey ~swap_record:cid ()))
  in
  Cmd.v
    (Cmd.info "remove" ~doc:"Remove your SSH public key record.")
    Term.(term_result (const action $ rkey))

let cmd =
  Cmd.group
    (Cmd.info "key" ~doc:"Manage SSH public keys.")
    [ list; add; remove ]
