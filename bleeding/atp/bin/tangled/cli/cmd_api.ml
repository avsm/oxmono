(* SPDX-License-Identifier: ISC *)
open Cmdliner
module S = Tangled.Schema
module Api = Tangled.Api

let nsid = Common.positional 0 "NSID" "Vendored lexicon identifier."

let service =
  Arg.(
    required
    & opt (some string) None
    & info [ "service" ] ~docv:"URL"
        ~doc:
          "Service origin. Choose the knot, spindle, appview or PDS serving \
           this method.")

let auth =
  Arg.(
    value
    & opt (enum [ ("none", `None); ("service", `Service); ("pds", `Pds) ]) `None
    & info [ "auth" ]
        ~doc:
          "Authentication: none (public), service (method-bound JWT), pds \
           (saved PDS session).")

let input =
  Common.optional [ "input" ] "FILE" "Request body file, or - for stdin."

let params =
  Common.strings [ "param"; "q" ] "KEY=VALUE"
    "Query parameter (repeat for arrays)."

let call_action env ~nsid ~service ~auth ~audience ~input ~params =
  let params = List.map Common.pair params in
  S.params nsid params;
  let schema = S.main nsid in
  match S.kind nsid with
  | "subscription" ->
      if input <> None || auth <> `None then
        invalid_arg "Subscriptions are public and take no input body";
      Stream.subscribe env ~service ~nsid ~params (fun type_ value ->
          Common.json
            (Common.object_ [ ("type", Common.string type_); ("body", value) ]))
  | ("query" | "procedure") as kind ->
      if kind = "query" && input <> None then
        invalid_arg "Queries take no input body";
      let encoding =
        match S.member "input" schema with
        | None -> None
        | Some input -> Some (S.text (S.field "encoding" input))
      in
      let body =
        match (encoding, input) with
        | None, None -> None
        | None, Some _ -> invalid_arg "Procedure takes no input body"
        | Some _, None -> invalid_arg "Procedure requires --input FILE"
        | Some "application/json", Some path ->
            let json = Common.read_json path in
            S.validate_input nsid json;
            Some
              (match Jsont_bytesrw.encode_string Jsont.json json with
              | Ok value -> value
              | Error message -> invalid_arg message)
        | Some _, Some path -> Some (Common.read_file path)
      in
      Common.with_api ~authenticated:(auth <> `None) env @@ fun api ->
      let client =
        match auth with
        | `None -> Api.public_client api ~service
        | `Service -> Api.service_client api ~service ?audience ~nsid ()
        | `Pds ->
            let client = Api.get_client api in
            if Xrpc.Client.get_service client <> Api.service_url service then
              invalid_arg
                "PDS authentication is restricted to the saved session's PDS";
            ignore (Api.get_did api);
            client
      in
      let output_json =
        match S.member "output" schema with
        | Some output ->
            Option.map S.text (S.member "encoding" output)
            = Some "application/json"
        | None -> false
      in
      if kind = "query" then begin
        if input <> None then invalid_arg "Queries take no input body";
        if output_json then
          Common.json
            (Xrpc.Client.query client ~nsid ~params ~decoder:Jsont.json)
        else
          let bytes, _ = Xrpc.Client.query_bytes client ~nsid ~params in
          output_string stdout bytes
      end
      else begin
        let result =
          Xrpc.Client.procedure_bytes client ~nsid ~params ~body
            ~content_type:(Option.value ~default:"application/json" encoding)
        in
        match result with
        | None -> ()
        | Some ("", _) -> ()
        | Some (body, _) when output_json ->
            Common.json
              (match Jsont_bytesrw.decode_string Jsont.json body with
              | Ok value -> value
              | Error message -> failwith message)
        | Some (body, _) -> output_string stdout body
      end
  | _ ->
      invalid_arg
        "This lexicon is a record or definition. Use 'tangled record'."

let call_cmd =
  let action nsid service auth audience input params =
    Common.run (fun env ->
        call_action env ~nsid ~service ~auth ~audience ~input ~params)
  in
  Cmd.v
    (Cmd.info "call"
       ~doc:"Call a vendored API, including binary responses and subscriptions.")
    Term.(
      term_result
        (const action $ nsid $ service $ auth $ Common.audience $ input $ params))

let schema_cmd =
  Cmd.v
    (Cmd.info "schema" ~doc:"Print a vendored lexicon without network access.")
    Term.(
      term_result
        (const (fun nsid -> Common.run (fun _ -> Common.json (S.document nsid)))
        $ nsid))

let list_cmd =
  let prefix = Arg.(value & pos 0 string "" & info [] ~docv:"PREFIX") in
  Cmd.v
    (Cmd.info "list"
       ~doc:"List all vendored APIs, records and definitions offline.")
    Term.(
      const (fun prefix ->
          List.iter
            (fun (nsid, _) ->
              if String.starts_with ~prefix nsid then
                Fmt.pr "%s\t%s@." (S.kind nsid) nsid)
            S.documents)
      $ prefix)

let cmd =
  Cmd.group
    (Cmd.info "api" ~doc:"Discover and call every vendored Tangled API.")
    [ list_cmd; schema_cmd; call_cmd ]
