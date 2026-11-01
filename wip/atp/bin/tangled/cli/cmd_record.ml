(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api
module S = Tangled.Schema

let collection =
  Common.positional 0 "COLLECTION" "Vendored Tangled record collection."

let rkey = Common.positional 1 "RKEY" "Record key."

let input =
  Arg.(
    required
    & opt (some string) None
    & info [ "input" ] ~docv:"FILE" ~doc:"JSON record file, or - for stdin.")

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
              (Api.list_records api ~collection ~did:(Common.did api user))))
  in
  Cmd.v
    (Cmd.info "list" ~doc:"Read all pages of a Tangled record collection.")
    Term.(term_result (const action $ collection $ Common.user))

let get_cmd =
  let action collection rkey user =
    Common.run (fun env ->
        require_record collection;
        Common.with_api env (fun api ->
            match
              Api.get_record api ~collection ~rkey ~did:(Common.did api user)
            with
            | Some value ->
                Common.print Api.Atproto.Repo.GetRecord.output_jsont value
            | None -> failwith "Record not found"))
  in
  Cmd.v
    (Cmd.info "get" ~doc:"Read a record with its URI and CID.")
    Term.(term_result (const action $ collection $ rkey $ Common.user))

let create_cmd =
  let rkey =
    Common.optional [ "rkey" ] "RKEY"
      "Explicit record key (required for literal-key records)."
  in
  let action collection rkey input =
    Common.run (fun env ->
        let value = record_input collection input in
        Common.with_api env (fun api ->
            Common.print Api.Atproto.Repo.CreateRecord.output_jsont
              (Api.create_record api ~collection ?rkey value)))
  in
  Cmd.v
    (Cmd.info "create" ~doc:"Create a validated Tangled record on your PDS.")
    Term.(term_result (const action $ collection $ rkey $ input))

let put_cmd =
  let cid =
    Arg.(
      required
      & opt (some string) None
      & info [ "cid" ] ~docv:"CID" ~doc:"Expected current CID, from record get.")
  in
  let action collection rkey cid input =
    Common.run (fun env ->
        let value = record_input collection input in
        Common.with_api env (fun api ->
            Common.print Api.Atproto.Repo.PutRecord.output_jsont
              (Api.put_record api ~collection ~rkey ~swap_record:(Some cid)
                 value)))
  in
  Cmd.v
    (Cmd.info "put" ~doc:"Replace a record only if its CID still matches.")
    Term.(term_result (const action $ collection $ rkey $ cid $ input))

let delete_cmd =
  let action collection rkey =
    Common.run (fun env ->
        require_record collection;
        Common.with_api env (fun api ->
            match
              Api.get_record api ~collection ~rkey ~did:(Api.get_did api)
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
  Cmd.v
    (Cmd.info "delete"
       ~doc:"Delete one of your Tangled records with a CID precondition.")
    Term.(term_result (const action $ collection $ rkey))

let cmd =
  Cmd.group
    (Cmd.info "record"
       ~doc:
         "Manage Tangled PDS records, including issues, pulls, comments and \
          follows.")
    [ list_cmd; get_cmd; create_cmd; put_cmd; delete_cmd ]
