(* SPDX-License-Identifier: ISC *)
(* Labels: a repository's own tags for its issues and pull requests. *)
open Cmdliner
module Api = Tangled.Api

let view (l : Api.label) = Ui.label ?color:l.definition.color l.definition.name

(* [find labels name] is the label [name] means: its key, its name in any
   case, or a name that makes the same key. *)
let find (labels : Api.label list) name =
  let wanted = String.lowercase_ascii (String.trim name) in
  let key = try Some (Api.label_key name) with Invalid_argument _ -> None in
  match
    List.find_opt
      (fun (l : Api.label) ->
        l.key = name
        || String.lowercase_ascii l.definition.name = wanted
        || Some l.key = key)
      labels
  with
  | Some l -> l
  | None ->
      failwith
        (match labels with
        | [] ->
            Printf.sprintf
              "There is no label %s. This repository has no labels yet. Its \
               owner can add one with: tangled repo label add %S"
              name name
        | _ ->
            Printf.sprintf "There is no label %s. The labels are: %s." name
              (String.concat ", "
                 (List.map (fun (l : Api.label) -> l.definition.name) labels)))

let keys api repository = function
  | [] -> []
  | names ->
      let labels = Api.labels api repository in
      List.map (fun n -> (find labels n).key) names

let name_arg = Common.positional 0 "NAME" "The label's name."

(* An argument, since --color is the option that colours the output. *)
let color =
  Arg.(
    value
    & pos 1 (some string) None
    & info [] ~docv:"COLOUR"
        ~doc:"The colour to show the label in, in hex, such as #e11d48.")

let list_cmd =
  let action repo knot json =
    Common.run (fun env ->
        let labels =
          Common.with_api ~busy:"Reading labels" env (fun api ->
              Api.labels api (Common.resolve api ?knot repo))
        in
        if json then
          Common.json
            (Jsont.Json.list
               (List.map
                  (fun (l : Api.label) ->
                    Common.object_
                      [
                        ("key", Common.string l.key);
                        ( "record",
                          Api.encode Api.Lex.Label.Definition.main_jsont
                            l.definition );
                      ])
                  labels))
        else if Ui.tty () then
          Ui.table
            ~heading:(Ui.heading "Labels" (List.length labels))
            [ Ui.column "Name"; Ui.column "Key"; Ui.column "Colour" ]
            (List.map
               (fun (l : Api.label) ->
                 [
                   view l;
                   Ui.muted l.key;
                   Ui.muted (Option.value ~default:"" l.definition.color);
                 ])
               labels)
        else
          List.iter
            (fun (l : Api.label) -> Fmt.pr "%s\t%s@." l.key l.definition.name)
            labels)
  in
  Common.cmd
    (Cmd.info "list" ~doc:"List the labels defined in a repository.")
    Term.(
      term_result (const action $ Common.repo $ Common.knot $ Common.json_flag))

let add_cmd =
  let action repo knot name color =
    Common.run (fun env ->
        let key =
          Common.with_api ~busy:("Adding " ^ name) env (fun api ->
              Api.create_label api
                (Common.resolve api ?knot repo)
                ~name ?color ())
        in
        if Ui.tty () then Ui.ok "Added the label %s" name
        else print_endline key)
  in
  Common.cmd
    (Cmd.info "add" ~doc:"Define a new label."
       ~man:
         [
           `S Manpage.s_description;
           `P
             "Defines a label that issues and pull requests in the repository \
              can then carry. Only the repository's owner may define labels.";
           `P
             "A label is stored under a key made from its name, in lower case \
              with hyphens for spaces. Once a label is removed, its key cannot \
              be used again, so a label of the same name cannot be added back.";
         ])
    Term.(
      term_result (const action $ Common.repo $ Common.knot $ name_arg $ color))

let edit_cmd =
  let rename =
    Common.optional [ "name"; "n" ] "NAME" "Rename the label to NAME."
  in
  let action repo knot name rename color =
    Common.run (fun env ->
        if rename = None && color = None then
          failwith "Nothing to change. Give a new COLOUR or --name.";
        Common.with_api ~busy:("Editing " ^ name) env (fun api ->
            let r = Common.resolve api ?knot repo in
            let l = find (Api.labels api r) name in
            Api.edit_label api r ~key:l.key ?name:rename ?color ());
        Ui.ok "Edited the label %s" name)
  in
  Common.cmd
    (Cmd.info "edit" ~doc:"Rename or recolour a label."
       ~man:
         (Common.man
            [
              "Issues and pull requests that carry the label keep it under its \
               new name and colour.";
            ]))
    Term.(
      term_result
        (const action $ Common.repo $ Common.knot $ name_arg $ rename $ color))

let remove_cmd =
  let action repo knot name =
    Common.run (fun env ->
        Common.with_api ~busy:("Removing " ^ name) env (fun api ->
            let r = Common.resolve api ?knot repo in
            Api.delete_label api r ~key:(find (Api.labels api r) name).key);
        Ui.ok "Removed the label %s" name)
  in
  Common.cmd
    (Cmd.info "remove" ~doc:"Remove a label from the repository."
       ~man:
         (Common.man
            [
              "Takes the label off the issues and pull requests that carry it, \
               then removes it. A label of the same name cannot be added \
               afterwards.";
            ]))
    Term.(term_result (const action $ Common.repo $ Common.knot $ name_arg))

let cmd =
  Cmd.group
    (Cmd.info "label"
       ~doc:"Define the labels that issues and pull requests can carry."
       ~man:
         [
           `S Manpage.s_description;
           `P
             "A label is a short coloured tag, such as $(i,bug) or $(i,good \
              first issue), that sorts issues and pull requests. Each \
              repository has its own set, which only its owner may change.";
           `P
             "Put a label on an issue or pull request with $(b,--label) when \
              creating it, or later with $(b,tangled issue edit --label). Find \
              those carrying one with $(b,tangled issue list --label).";
         ])
    [ list_cmd; add_cmd; edit_cmd; remove_cmd ]
