(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api
module S = Tangled.Schema

let appview =
  Arg.(
    value
    & opt string "https://tangled.org"
    & info [ "appview" ] ~docv:"URL"
        ~doc:"Appview origin for indexed issue and pull queries.")

let repo = Common.positional 0 "REPO" "Repository DID."
let uri = Common.positional 0 "URI" "Issue or pull record URI."

let list kind =
  let state =
    Common.optional [ "state" ] "STATE"
      "Filter by state (open, closed, or merged for pulls)."
  in
  let author = Common.optional [ "author" ] "DID" "Filter by author DID." in
  let action repo appview state author limit cursor =
    Common.run (fun env ->
        let field = if kind = "Issue" then "state" else "status" in
        let nsid = "sh.tangled.repo.list" ^ kind ^ "s" in
        let params =
          [ ("subject", repo); ("limit", string_of_int limit) ]
          @ List.filter_map Fun.id
              [
                Option.map (fun x -> (field, x)) state;
                Option.map (fun x -> ("author", x)) author;
                Option.map (fun x -> ("cursor", x)) cursor;
              ]
        in
        S.params nsid params;
        Common.with_api ~authenticated:false env (fun api ->
            Common.json
              (Xrpc.Client.query
                 (Api.public_client api ~service:appview)
                 ~nsid ~params ~decoder:Jsont.json)))
  in
  Cmd.v
    (Cmd.info "list" ~doc:"List indexed records with filters and pagination.")
    Term.(
      term_result
        (const action $ repo $ appview $ state $ author $ Common.limit
       $ Common.cursor))

let show kind =
  let action uri appview =
    Common.run (fun env ->
        let nsid = "sh.tangled.repo.get" ^ kind in
        let params = [ (String.lowercase_ascii kind, uri) ] in
        S.params nsid params;
        Common.with_api ~authenticated:false env (fun api ->
            Common.json
              (Xrpc.Client.query
                 (Api.public_client api ~service:appview)
                 ~nsid ~params ~decoder:Jsont.json)))
  in
  Cmd.v
    (Cmd.info "show" ~doc:"Read an indexed record from an appview.")
    Term.(term_result (const action $ uri $ appview))

let create =
  let title =
    Arg.(required & opt (some string) None & info [ "title" ] ~docv:"TITLE")
  in
  let body =
    Common.optional [ "body-file" ] "FILE" "Issue body file, or - for stdin."
  in
  let action repo title body =
    Common.run (fun env ->
        let collection = "sh.tangled.repo.issue" in
        let pairs =
          [
            ("$type", Common.string collection);
            ("repo", Common.string repo);
            ("title", Common.string title);
            ("createdAt", Common.string (Api.now ()));
          ]
          @ Option.to_list
              (Option.map
                 (fun file -> ("body", Common.string (Common.read_file file)))
                 body)
        in
        let value = Common.object_ pairs in
        S.validate ~nsid:collection (S.main collection) value;
        Common.with_api env (fun api ->
            Common.print Api.Atproto.Repo.CreateRecord.output_jsont
              (Api.create_record api ~collection value)))
  in
  Cmd.v
    (Cmd.info "create" ~doc:"Create an issue in your PDS.")
    Term.(term_result (const action $ repo $ title $ body))

let state name value =
  let action uri =
    Common.run (fun env ->
        let collection = "sh.tangled.repo.issue.state" in
        let record =
          Common.object_
            [
              ("$type", Common.string collection);
              ("issue", Common.string uri);
              ("createdAt", Common.string (Api.now ()));
              ("state", Common.string (collection ^ "." ^ value));
            ]
        in
        S.validate ~nsid:collection (S.main collection) record;
        Common.with_api env (fun api ->
            Common.print Api.Atproto.Repo.CreateRecord.output_jsont
              (Api.create_record api ~collection record)))
  in
  Cmd.v
    (Cmd.info name
       ~doc:"Publish an issue state update. The appview checks your authority.")
    Term.(term_result (const action $ uri))

let cmd =
  Cmd.group
    (Cmd.info "issue" ~doc:"Query, create and update issues.")
    [
      list "Issue";
      show "Issue";
      create;
      state "close" "closed";
      state "reopen" "open";
    ]

let pull_cmd =
  Cmd.group
    (Cmd.info "pull"
       ~doc:"Query pull requests. Use record commands to publish revisions.")
    [ list "Pull"; show "Pull" ]
