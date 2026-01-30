(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

let version = "0.1.0"

(* Styled output helpers *)
let header_style = Fmt.(styled `Bold string)
let label_style = Fmt.(styled `Faint string)
let value_style = Fmt.(styled (`Fg `Cyan) string)
let success_style = Fmt.(styled (`Fg `Green) string)
let warning_style = Fmt.(styled (`Fg `Yellow) string)
let error_style = Fmt.(styled (`Fg `Red) string)

(* Common arguments *)
let collection_arg =
  let doc = "Collection name." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"COLLECTION" ~doc)

let id_arg =
  let doc = "Document ID." in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"ID" ~doc)

let file_arg =
  let doc = "Path to file." in
  Arg.(value & opt (some string) None & info ["file"; "f"] ~docv:"FILE" ~doc)

let output_arg =
  let doc = "Output file path." in
  Arg.(value & opt (some string) None & info ["output"; "o"] ~docv:"FILE" ~doc)

(* Collections commands *)

let collections_list_cmd env fs =
  let doc = "List all collections." in
  let info = Cmd.info "list" ~doc in
  let action (style_renderer, level) requests_config profile =
    Typesense_auth.Cmd.setup_logging_with_config style_renderer level requests_config;
    Typesense_auth.Error.wrap (fun () ->
      Typesense_auth.Cmd.with_client ~requests_config ?profile (fun _fs client ->
        let api = Typesense_auth.Client.client client in
        (* Use raw request since get_collections returns wrong type *)
        let session = Typesense.session api in
        let base_url = Typesense.base_url api in
        let response = Requests.get session (base_url ^ "/collections") in
        if not (Requests.Response.ok response) then
          failwith ("Failed to list collections: " ^ string_of_int (Requests.Response.status_code response));
        let json = Requests.Response.json response in
        match json with
        | Jsont.Array (items, _) ->
            if items = [] then
              Fmt.pr "%a No collections found.@." warning_style "Note:"
            else begin
              Fmt.pr "%a@." header_style "Collections:";
              List.iter (fun item ->
                match Jsont_bytesrw.decode_string Typesense.Collection.Response.jsont
                        (match Jsont_bytesrw.encode_string Jsont.json item with Ok s -> s | Error _ -> "{}") with
                | Ok c ->
                    let name = Typesense.Collection.Response.name c in
                    let num_docs = Typesense.Collection.Response.num_documents c in
                    Fmt.pr "  %a (%Ld documents)@." value_style name num_docs
                | Error _ -> ()
              ) items
            end
        | _ -> failwith "Unexpected response format"
      ) env)
  in
  Cmd.v info
    Term.(const action $ Typesense_auth.Cmd.setup_logging $ Typesense_auth.Cmd.requests_config_term fs $ Typesense_auth.Cmd.profile_arg)

let collections_get_cmd env fs =
  let doc = "Get a collection by name." in
  let info = Cmd.info "get" ~doc in
  let name_arg =
    let doc = "Collection name." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)
  in
  let action (style_renderer, level) requests_config profile name =
    Typesense_auth.Cmd.setup_logging_with_config style_renderer level requests_config;
    Typesense_auth.Error.wrap (fun () ->
      Typesense_auth.Cmd.with_client ~requests_config ?profile (fun _fs client ->
        let api = Typesense_auth.Client.client client in
        let collection = Typesense.Collection.get_collection api ~collection_name:name () in
        Fmt.pr "%a %a@." label_style "Name:" value_style (Typesense.Collection.Response.name collection);
        Fmt.pr "%a %Ld@." label_style "Documents:" (Typesense.Collection.Response.num_documents collection);
        Fmt.pr "%a %a@." label_style "Created:" value_style (Int64.to_string (Typesense.Collection.Response.created_at collection));
        let fields = Typesense.Collection.Response.fields collection in
        Fmt.pr "%a@." header_style "Fields:";
        List.iter (fun field ->
          let name = Typesense.Field.T.name field in
          let type_ = Typesense.Field.T.type_ field in
          Fmt.pr "  - %a: %s@." value_style name type_
        ) fields
      ) env)
  in
  Cmd.v info
    Term.(const action $ Typesense_auth.Cmd.setup_logging $ Typesense_auth.Cmd.requests_config_term fs $ Typesense_auth.Cmd.profile_arg $ name_arg)

let collections_delete_cmd env fs =
  let doc = "Delete a collection." in
  let info = Cmd.info "delete" ~doc in
  let name_arg =
    let doc = "Collection name." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)
  in
  let action (style_renderer, level) requests_config profile name =
    Typesense_auth.Cmd.setup_logging_with_config style_renderer level requests_config;
    Typesense_auth.Error.wrap (fun () ->
      Typesense_auth.Cmd.with_client ~requests_config ?profile (fun _fs client ->
        let api = Typesense_auth.Client.client client in
        ignore (Typesense.Collection.delete_collection api ~collection_name:name ());
        Fmt.pr "%a Deleted collection: %a@." success_style "Success:" value_style name
      ) env)
  in
  Cmd.v info
    Term.(const action $ Typesense_auth.Cmd.setup_logging $ Typesense_auth.Cmd.requests_config_term fs $ Typesense_auth.Cmd.profile_arg $ name_arg)

let collections_cmd env fs =
  let doc = "Collection management commands." in
  let info = Cmd.info "collections" ~doc in
  Cmd.group info
    [ collections_list_cmd env fs
    ; collections_get_cmd env fs
    ; collections_delete_cmd env fs
    ]

(* Documents commands *)

let documents_get_cmd env fs =
  let doc = "Get a document by ID." in
  let info = Cmd.info "get" ~doc in
  let action (style_renderer, level) requests_config profile collection id =
    Typesense_auth.Cmd.setup_logging_with_config style_renderer level requests_config;
    Typesense_auth.Error.wrap (fun () ->
      Typesense_auth.Cmd.with_client ~requests_config ?profile (fun _fs client ->
        let api = Typesense_auth.Client.client client in
        let doc = Typesense.Client.get_document api ~collection_name:collection ~document_id:id () in
        let json_str = match Jsont_bytesrw.encode_string ~format:Jsont.Indent Jsont.json doc with
          | Ok s -> s
          | Error e -> failwith ("Failed to encode: " ^ e)
        in
        print_endline json_str
      ) env)
  in
  Cmd.v info
    Term.(const action $ Typesense_auth.Cmd.setup_logging $ Typesense_auth.Cmd.requests_config_term fs $ Typesense_auth.Cmd.profile_arg $ collection_arg $ id_arg)

let documents_delete_cmd env fs =
  let doc = "Delete a document by ID." in
  let info = Cmd.info "delete" ~doc in
  let action (style_renderer, level) requests_config profile collection id =
    Typesense_auth.Cmd.setup_logging_with_config style_renderer level requests_config;
    Typesense_auth.Error.wrap (fun () ->
      Typesense_auth.Cmd.with_client ~requests_config ?profile (fun _fs client ->
        let api = Typesense_auth.Client.client client in
        ignore (Typesense.Client.delete_document api ~collection_name:collection ~document_id:id ());
        Fmt.pr "%a Deleted document %a from %a@." success_style "Success:" value_style id value_style collection
      ) env)
  in
  Cmd.v info
    Term.(const action $ Typesense_auth.Cmd.setup_logging $ Typesense_auth.Cmd.requests_config_term fs $ Typesense_auth.Cmd.profile_arg $ collection_arg $ id_arg)

let documents_import_cmd env fs =
  let doc = "Import documents from JSONL file." in
  let info = Cmd.info "import" ~doc in
  let action_arg =
    let doc = "Import action: create, upsert, update, or emplace." in
    let actions = ["create", Typesense_auth.Client.Create; "upsert", Typesense_auth.Client.Upsert; "update", Typesense_auth.Client.Update; "emplace", Typesense_auth.Client.Emplace] in
    Arg.(value & opt (enum actions) Typesense_auth.Client.Upsert & info ["action"; "a"] ~docv:"ACTION" ~doc)
  in
  let action (style_renderer, level) requests_config profile collection file action_type =
    Typesense_auth.Cmd.setup_logging_with_config style_renderer level requests_config;
    Typesense_auth.Error.wrap (fun () ->
      Typesense_auth.Cmd.with_client ~requests_config ?profile (fun _fs client ->
        let file = match file with
          | Some f -> f
          | None -> failwith "Please specify --file"
        in
        let content = In_channel.with_open_text file In_channel.input_all in
        let docs = String.split_on_char '\n' content
          |> List.filter (fun line -> String.trim line <> "")
          |> List.map (fun line ->
              match Jsont_bytesrw.decode_string Jsont.json line with
              | Ok doc -> doc
              | Error e -> failwith ("Invalid JSON on line: " ^ e))
        in
        let results = Typesense_auth.Client.import client ~collection ~action:action_type docs in
        let success_count = List.length (List.filter (fun r -> r.Typesense_auth.Client.success) results) in
        let error_count = List.length results - success_count in
        Fmt.pr "%a Imported %d documents (%d successful, %d errors)@."
          success_style "Done:" (List.length results) success_count error_count;
        if error_count > 0 then begin
          Fmt.pr "%a@." warning_style "Errors:";
          List.iteri (fun i r ->
            if not r.Typesense_auth.Client.success then
              match r.Typesense_auth.Client.error with
              | Some e -> Fmt.pr "  Line %d: %s@." (i + 1) e
              | None -> Fmt.pr "  Line %d: Unknown error@." (i + 1)
          ) results
        end
      ) env)
  in
  Cmd.v info
    Term.(const action $ Typesense_auth.Cmd.setup_logging $ Typesense_auth.Cmd.requests_config_term fs $ Typesense_auth.Cmd.profile_arg $ collection_arg $ file_arg $ action_arg)

let documents_export_cmd env fs =
  let doc = "Export documents to JSONL." in
  let info = Cmd.info "export" ~doc in
  let filter_arg =
    let doc = "Filter expression." in
    Arg.(value & opt (some string) None & info ["filter"] ~docv:"FILTER" ~doc)
  in
  let action (style_renderer, level) requests_config profile collection output filter =
    Typesense_auth.Cmd.setup_logging_with_config style_renderer level requests_config;
    Typesense_auth.Error.wrap (fun () ->
      Typesense_auth.Cmd.with_client ~requests_config ?profile (fun _fs client ->
        let params = Typesense_auth.Client.export_params ?filter_by:filter () in
        let docs = Typesense_auth.Client.export client ~collection ~params () in
        let output_fn = match output with
          | Some path ->
              let oc = open_out path in
              (fun line -> output_string oc (line ^ "\n")),
              (fun () -> close_out oc)
          | None ->
              (fun line -> print_endline line),
              (fun () -> ())
        in
        let (output_line, close) = output_fn in
        List.iter (fun doc ->
          match Jsont_bytesrw.encode_string Jsont.json doc with
          | Ok s -> output_line s
          | Error _ -> ()
        ) docs;
        close ();
        Fmt.epr "%a Exported %d documents@." success_style "Done:" (List.length docs)
      ) env)
  in
  Cmd.v info
    Term.(const action $ Typesense_auth.Cmd.setup_logging $ Typesense_auth.Cmd.requests_config_term fs $ Typesense_auth.Cmd.profile_arg $ collection_arg $ output_arg $ filter_arg)

let documents_cmd env fs =
  let doc = "Document operations." in
  let info = Cmd.info "documents" ~doc in
  Cmd.group info
    [ documents_get_cmd env fs
    ; documents_delete_cmd env fs
    ; documents_import_cmd env fs
    ; documents_export_cmd env fs
    ]

(* Search command *)

let search_cmd env fs =
  let doc = "Search documents in a collection." in
  let info = Cmd.info "search" ~doc in
  let query_arg =
    let doc = "Search query." in
    Arg.(required & opt (some string) None & info ["query"; "q"] ~docv:"QUERY" ~doc)
  in
  let query_by_arg =
    let doc = "Fields to search in (comma-separated)." in
    Arg.(required & opt (some string) None & info ["query-by"] ~docv:"FIELDS" ~doc)
  in
  let filter_arg =
    let doc = "Filter expression." in
    Arg.(value & opt (some string) None & info ["filter-by"] ~docv:"FILTER" ~doc)
  in
  let limit_arg =
    let doc = "Maximum number of results." in
    Arg.(value & opt int 10 & info ["limit"; "n"] ~docv:"NUM" ~doc)
  in
  let action (style_renderer, level) requests_config profile collection query query_by filter limit =
    Typesense_auth.Cmd.setup_logging_with_config style_renderer level requests_config;
    Typesense_auth.Error.wrap (fun () ->
      Typesense_auth.Cmd.with_client ~requests_config ?profile (fun _fs client ->
        let api = Typesense_auth.Client.client client in
        (* Build search_parameters query string *)
        let params = [
          ("q", query);
          ("query_by", query_by);
          ("per_page", string_of_int limit);
        ] @ (match filter with Some f -> [("filter_by", f)] | None -> []) in
        let search_parameters =
          params
          |> List.map (fun (k, v) -> Uri.pct_encode k ^ "=" ^ Uri.pct_encode v)
          |> String.concat "&"
        in
        let result = Typesense.Search.search_collection api ~collection_name:collection ~search_parameters () in
        let found = Option.value ~default:0 (Typesense.Search.Result.found result) in
        let hits = Typesense.Search.Result.hits result in
        Fmt.pr "%a %d results (showing %d)@." header_style "Found:" found (List.length (Option.value ~default:[] hits));
        match hits with
        | None | Some [] -> ()
        | Some hits ->
            List.iter (fun hit ->
              match Typesense.SearchResultHit.T.document hit with
              | Some doc ->
                  (match Jsont_bytesrw.encode_string Jsont.json doc with
                   | Ok s -> print_endline s
                   | Error _ -> ())
              | None -> ()
            ) hits
      ) env)
  in
  Cmd.v info
    Term.(const action $ Typesense_auth.Cmd.setup_logging $ Typesense_auth.Cmd.requests_config_term fs $ Typesense_auth.Cmd.profile_arg $ collection_arg $ query_arg $ query_by_arg $ filter_arg $ limit_arg)

(* Health command *)

let health_cmd env fs =
  let doc = "Check server health." in
  let info = Cmd.info "health" ~doc in
  let action (style_renderer, level) requests_config profile =
    Typesense_auth.Cmd.setup_logging_with_config style_renderer level requests_config;
    Typesense_auth.Error.wrap (fun () ->
      Typesense_auth.Cmd.with_client ~requests_config ?profile (fun _fs client ->
        let api = Typesense_auth.Client.client client in
        let health = Typesense.Health.health api () in
        if Typesense.Health.Status.ok health then
          Fmt.pr "%a Server is healthy@." success_style "OK:"
        else
          Fmt.pr "%a Server is not healthy@." error_style "Error:"
      ) env)
  in
  Cmd.v info
    Term.(const action $ Typesense_auth.Cmd.setup_logging $ Typesense_auth.Cmd.requests_config_term fs $ Typesense_auth.Cmd.profile_arg)

(* Main *)

let () =
  let exit_code =
    try
      Eio_main.run @@ fun env ->
      let fs = env#fs in
      let doc = "Typesense CLI - A command-line interface for Typesense search server" in
      let man = [
        `S Manpage.s_description;
        `P "A command-line interface for interacting with Typesense search servers.";
        `P "Use $(b,typesense-cli auth login) to authenticate with your server.";
        `S Manpage.s_commands;
        `S Manpage.s_bugs;
        `P "Report bugs at https://tangled.org/@anil.recoil.org/ocaml-typesense/issues";
      ] in
      let info = Cmd.info "typesense-cli" ~version ~doc ~man in
      let cmds =
        [ Typesense_auth.Cmd.auth_cmd env fs
        ; collections_cmd env fs
        ; documents_cmd env fs
        ; search_cmd env fs
        ; health_cmd env fs
        ]
      in
      Cmd.eval (Cmd.group info cmds)
    with
    | Eio.Cancel.Cancelled Stdlib.Exit ->
        (* Eio wraps Exit in Cancelled when a fiber is cancelled *)
        0
    | Typesense_auth.Error.Exit_code code ->
        (* Exit code from Error.wrap - already printed error message *)
        code
    | Openapi.Runtime.Api_error _ as exn ->
        (* Handle Typesense API errors with nice formatting *)
        Typesense_auth.Error.handle_exn exn
    | Failure msg ->
        Fmt.epr "Error: %s@." msg;
        1
    | exn ->
        Fmt.epr "Unexpected error: %s@." (Printexc.to_string exn);
        125
  in
  exit exit_code
