(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api
module S = Tangled.Schema

let collection =
  Common.positional 0 "COLLECTION"
    "The record type, such as $(b,org.tangled.feed.star)."

let rkey = Common.positional 1 "RKEY" "The record's key."

let input =
  Arg.(
    required
    & opt (some string) None
    & info [ "input" ] ~docv:"FILE"
        ~doc:
          "Read the record, as JSON, from FILE, or standard input if FILE is \
           -.")

let require_record collection =
  if S.kind collection <> "record" then invalid_arg "Expected a record lexicon"

let record_input collection path =
  require_record collection;
  let value = Common.read_json path in
  if S.text (S.field "$type" value) <> collection then
    invalid_arg "Record $type differs from collection";
  S.validate ~nsid:collection (S.main collection) value;
  value

let list_cmd =
  let action collection user =
    Common.run (fun env ->
        require_record collection;
        Common.with_api env (fun api ->
            Common.print
              (Jsont.list Api.Atproto.Repo.ListRecords.record_jsont)
              (Api.list_records api ~collection ~did:(Common.did api user) ())))
  in
  Common.cmd
    (Cmd.info "list" ~doc:"List a user's records of one type.")
    Term.(term_result (const action $ collection $ Common.user))

let get_cmd =
  let action collection rkey user =
    Common.run (fun env ->
        require_record collection;
        Common.with_api env (fun api ->
            match
              Api.get_record api ~collection ~rkey ~did:(Common.did api user) ()
            with
            | Some value ->
                Common.print Api.Atproto.Repo.GetRecord.output_jsont value
            | None -> failwith "Record not found"))
  in
  Common.cmd
    (Cmd.info "get" ~doc:"Print one record.")
    Term.(term_result (const action $ collection $ rkey $ Common.user))

let create_cmd =
  let rkey =
    Common.optional [ "rkey" ] "RKEY"
      "Store the record under RKEY. Record types with a fixed key need it."
  in
  let action collection rkey input =
    Common.run (fun env ->
        let value = record_input collection input in
        Common.with_api env (fun api ->
            Common.print Api.Atproto.Repo.CreateRecord.output_jsont
              (Api.create_record api ~collection ?rkey value)))
  in
  Common.cmd
    (Cmd.info "create"
       ~doc:"Add a record to your account, after checking it is well formed.")
    Term.(term_result (const action $ collection $ rkey $ input))

let put_cmd =
  let cid =
    Arg.(
      required
      & opt (some string) None
      & info [ "cid" ] ~docv:"CID"
          ~doc:
            "The CID $(b,record get) printed. The record is replaced only if \
             it has not changed since.")
  in
  let action collection rkey cid input =
    Common.run (fun env ->
        let value = record_input collection input in
        Common.with_api env (fun api ->
            Common.print Api.Atproto.Repo.PutRecord.output_jsont
              (Api.put_record api ~collection ~rkey ~swap_record:(Some cid)
                 value)))
  in
  Common.cmd
    (Cmd.info "put" ~doc:"Replace one of your records.")
    Term.(term_result (const action $ collection $ rkey $ cid $ input))

let delete_cmd =
  let action collection rkey =
    Common.run (fun env ->
        require_record collection;
        Common.with_api env (fun api ->
            match
              Api.get_record api ~collection ~rkey ~did:(Api.get_did api) ()
            with
            | None -> failwith "Record not found"
            | Some record ->
                let cid =
                  match record.cid with
                  | Some cid -> cid
                  | None -> failwith "Missing record CID"
                in
                Api.delete_record api ~collection ~rkey ~swap_record:cid ()))
  in
  Common.cmd
    (Cmd.info "delete" ~doc:"Delete one of your records.")
    Term.(term_result (const action $ collection $ rkey))

let cmd =
  Cmd.group
    (Cmd.info "record" ~doc:"Read and write the Tangled records in an account."
       ~man:
         (Common.man
            [
              "Each account stores what its owner publishes as records, such \
               as stars, SSH keys and follows. These commands read anyone's \
               and change your own.";
            ]))
    [ list_cmd; get_cmd; create_cmd; put_cmd; delete_cmd ]
