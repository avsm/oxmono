(* SPDX-License-Identifier: ISC *)
open Cmdliner
module S = Tangled.Schema
module Api = Tangled.Api

let nsid =
  Common.positional 0 "NAME"
    "The method or record type, such as $(b,org.tangled.git.listBranches)."

let service =
  Arg.(
    required
    & opt (some string) None
    & info [ "service" ] ~docv:"URL"
        ~doc:
          "The server to call, which must be one that serves the method: a \
           knot, a spindle or an account's server.")

let auth =
  Arg.(
    value
    & opt (enum [ ("none", `None); ("service", `Service); ("pds", `Pds) ]) `None
    & info [ "auth" ] ~docv:"MODE"
        ~doc:
          "How to log in: $(b,none) for a public method, $(b,service) for a \
           method of a knot or spindle that needs a login, or $(b,pds) for a \
           method of your own account's server.")

let input =
  Common.optional [ "input" ] "FILE"
    "Send the JSON in FILE as the input, or standard input if FILE is -."

let params =
  Common.strings [ "param"; "q" ] "KEY=VALUE"
    "Pass the parameter KEY with VALUE. Repeat the option for more, or to give \
     a list."

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
  Common.cmd
    (Cmd.info "call" ~doc:"Call a method and print its answer.")
    Term.(
      term_result
        (const action $ nsid $ service $ auth $ Common.audience $ input
       $ params))

let schema_cmd =
  Common.cmd
    (Cmd.info "schema" ~doc:"Print the definition of a method or record type.")
    Term.(
      term_result
        (const (fun nsid -> Common.run (fun _ -> Common.json (S.document nsid)))
        $ nsid))

let kind_view kind =
  let role =
    match kind with
    | "query" -> `Accent
    | "procedure" -> `Warning
    | "subscription" -> `Succeeded
    | _ -> `Muted
  in
  Ui.V.role role (Ui.text kind)

(* The NSIDs as a tree of their segments, with the shared prefix of the
   whole list as its root. *)
let tree nsids =
  let split = List.map (fun n -> (String.split_on_char '.' n, n)) nsids in
  let rec node label entries =
    let leaves, inner =
      List.partition (fun (path, _) -> List.length path = 1) entries
    in
    let groups =
      List.fold_left
        (fun acc (path, nsid) ->
          match path with
          | head :: rest ->
              let previous =
                Option.value ~default:[] (List.assoc_opt head acc)
              in
              (head, (rest, nsid) :: previous) :: List.remove_assoc head acc
          | [] -> acc)
        [] inner
      |> List.rev_map (fun (k, v) -> (k, List.rev v))
      |> List.sort (fun (a, _) (b, _) -> String.compare a b)
    in
    let children =
      List.map (fun (head, entries) -> node (Ui.bold head) entries) groups
      @ List.map
          (fun (path, nsid) ->
            Console.Tree.v
              (Ui.V.concat
                 [
                   Ui.text (List.hd path);
                   Ui.text "  ";
                   kind_view (S.kind nsid);
                 ])
              [])
          (List.sort compare leaves)
    in
    Console.Tree.v label children
  in
  (* Fold single-child chains such as org > tangled into org.tangled. *)
  let rec root prefix entries =
    match
      ( List.sort_uniq String.compare
          (List.filter_map
             (fun (p, _) ->
               match p with [ _ ] | [] -> None | h :: _ -> Some h)
             entries),
        List.exists (fun (p, _) -> List.length p <= 1) entries )
    with
    | [ head ], false ->
        root
          (if prefix = "" then head else prefix ^ "." ^ head)
          (List.map (fun (p, n) -> (List.tl p, n)) entries)
    | _ -> node (Ui.title (if prefix = "" then "APIs" else prefix)) entries
  in
  Ui.V.vcat
    [
      Ui.heading "Lexicons" (List.length nsids);
      Console.Tree.view (root "" split);
    ]

let list_cmd =
  let prefix =
    Arg.(
      value & pos 0 string ""
      & info [] ~docv:"PREFIX" ~doc:"List only names starting with PREFIX.")
  in
  Common.cmd
    (Cmd.info "list"
       ~doc:"List the methods and record types this program knows.")
    Term.(
      const (fun prefix ->
          let nsids =
            List.filter_map
              (fun (nsid, _) ->
                if String.starts_with ~prefix nsid then Some nsid else None)
              S.documents
          in
          if Ui.tty () then Ui.print (tree nsids)
          else
            List.iter (fun nsid -> Fmt.pr "%s\t%s@." (S.kind nsid) nsid) nsids)
      $ prefix)

let cmd =
  Cmd.group
    (Cmd.info "api"
       ~doc:"Call any Tangled method, or read and write records directly."
       ~man:
         (Common.man
            [
              "These commands reach what the others do not cover. Tangled is \
               built from methods, which servers answer, and records, which \
               accounts store. Each has a name such as \
               $(b,org.tangled.git.listBranches). $(b,list) and $(b,schema) \
               show what there is, and $(b,call) and $(b,record) use them.";
            ]))
    [ list_cmd; schema_cmd; call_cmd; Cmd_record.cmd ]
