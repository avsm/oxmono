(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api
module Ticket = Api.Lex.Track.Ticket
module Patch = Api.Lex.Review.Patch

let kind ~pulls = if pulls then "pull request" else "issue"
let an ~pulls = if pulls then "a pull request" else "an issue"

let id ~pulls =
  Common.positional 0 "ID"
    (Printf.sprintf "The %s's number, as $(b,list) shows it." (kind ~pulls))

let text_of field =
  match Tangled.Schema.member "text" field with
  | Some text -> Tangled.Schema.text text
  | None -> ""

let state_of (t : Ticket.main) = Tangled.Schema.text t.state

(* A ticket with a target branch is a pull request. *)
let is_pull (t : Ticket.main) = Option.is_some t.target_branch
let date s = if String.length s >= 10 then String.sub s 0 10 else s

let source_of (p : Patch.main) =
  Option.map (Api.decode Patch.source_jsont) p.source

(* A ticket with what a reader needs beside it: its author, its comments and,
   for a pull request, its revisions. Handles are looked up only for a
   terminal, which shows them. *)
type detail = {
  rkey : string;
  ticket : Ticket.main;
  author : string option;
  comments : Api.comment list;
  revisions : Patch.main list;
  labels : Api.label list;
  names : (string * string option) list;
}

(* [labels_of defined t] is the labels on [t], in its order. A label deleted
   since is left out. *)
let labels_of (defined : Api.label list) t =
  List.filter_map
    (fun key -> List.find_opt (fun (l : Api.label) -> l.key = key) defined)
    (Api.ticket_labels t)

let label_names labels =
  List.map (fun (l : Api.label) -> l.definition.name) labels

let label_views labels =
  Ui.V.concat ~sep:(Ui.text ", ") (List.map Cmd_label.view labels)

let detail api repository rkey =
  match Api.ticket api repository rkey with
  | None -> failwith ("No ticket #" ^ rkey)
  | Some ticket ->
      let author = Api.ticket_author api repository rkey in
      let comments = Api.comments api repository ~ticket:rkey in
      let revisions =
        if is_pull ticket then Api.patches api repository ~ticket:rkey else []
      in
      let labels =
        if Api.ticket_labels ticket = [] then []
        else labels_of (Api.labels api repository) ticket
      in
      let names =
        if Ui.tty () then
          Common.handles api
            (List.sort_uniq String.compare
               (Option.to_list author
               @ List.filter_map (fun (c : Api.comment) -> c.author) comments
               @ Option.value ~default:[] ticket.assignees))
        else []
      in
      { rkey; ticket; author; comments; revisions; labels; names }

let who d did = Ui.account (did, Option.join (List.assoc_opt did d.names))

let detail_json d =
  let opt = Option.fold ~none:(Jsont.Json.null ()) ~some:Common.string in
  Common.object_
    [
      ("id", Common.string d.rkey);
      ("author", opt d.author);
      ("record", Api.encode Ticket.main_jsont d.ticket);
      ( "labels",
        Jsont.Json.list (List.map Common.string (label_names d.labels)) );
      ( "comments",
        Jsont.Json.list
          (List.map
             (fun (c : Api.comment) ->
               Common.object_
                 [
                   ("id", Common.string c.rkey);
                   ("author", opt c.author);
                   ( "record",
                     Api.encode Api.Lex.Feed.Comment.main_jsont c.comment );
                 ])
             d.comments) );
      ( "revisions",
        Jsont.Json.list (List.map (Api.encode Patch.main_jsont) d.revisions) );
    ]

let show_detail d =
  let t = d.ticket in
  let body = Option.fold ~none:"" ~some:text_of t.body in
  let revisions =
    if d.revisions = [] then []
    else
      [
        Console.Table.view
          [
            Ui.column ~align:`Right "Rev";
            Ui.column "Base";
            Ui.column "Head";
            Ui.column "From";
            Ui.column "Submitted";
          ]
          (List.mapi
             (fun i (p : Patch.main) ->
               [
                 Ui.muted (string_of_int (i + 1));
                 Ui.text (Ui.short_sha (Api.oid_of p.base));
                 Ui.accent (Ui.short_sha (Api.oid_of p.head));
                 Ui.text
                   (Option.fold ~none:""
                      ~some:(fun (s : Patch.source) -> s.branch)
                      (source_of p));
                 Ui.muted (date p.created_at);
               ])
             d.revisions);
      ]
  in
  Ui.panel
    (Ui.V.concat [ Ui.muted ("#" ^ d.rkey ^ " "); Ui.title (text_of t.title) ])
    (Ui.V.vcat ~gap:1
       (Ui.fields
          [
            ("State", Some (Ui.state (state_of t)));
            ("Author", Option.map (who d) d.author);
            ("Target", Option.map Ui.accent t.target_branch);
            ("Opened", Some (Ui.muted t.created_at));
            ( "Labels",
              if d.labels = [] then None else Some (label_views d.labels) );
            ( "Assignees",
              Option.map
                (fun a -> Ui.V.concat ~sep:(Ui.text ", ") (List.map (who d) a))
                t.assignees );
          ]
        :: (if String.trim body = "" then [] else [ Ui.para body ])
       @ revisions));
  List.iter
    (fun (c : Api.comment) ->
      Ui.print
        (Ui.V.vcat
           [
             Ui.V.concat
               [
                 Option.fold ~none:(Ui.muted "unknown") ~some:(who d) c.author;
                 Ui.muted ("  " ^ c.comment.created_at);
               ];
             Ui.para (text_of c.comment.body);
             Ui.text "";
           ]))
    d.comments

let list ~pulls =
  let state =
    Common.optional [ "state" ] "STATE"
      "Show only those in STATE: $(b,open), $(b,closed) or $(b,completed)."
  in
  let label =
    Common.optional [ "label" ] "LABEL" "Show only those carrying LABEL."
  in
  let action repo knot state label json =
    Common.run (fun env ->
        let tickets, defined =
          Common.with_api ~busy:"Reading tickets" env (fun api ->
              let r = Common.resolve api ?knot repo in
              let tickets =
                List.filter
                  (fun (_, t) ->
                    is_pull t = pulls
                    && match state with None -> true | Some s -> state_of t = s)
                  (Api.tickets api r)
              in
              let labelled =
                List.exists (fun (_, t) -> Api.ticket_labels t <> []) tickets
              in
              ( tickets,
                if label <> None || (labelled && Ui.tty ()) then
                  Api.labels api r
                else [] ))
        in
        let tickets =
          match label with
          | None -> tickets
          | Some name ->
              let key = (Cmd_label.find defined name).key in
              List.filter
                (fun (_, t) -> List.mem key (Api.ticket_labels t))
                tickets
        in
        let show_labels = defined <> [] in
        if json then
          Common.json
            (Jsont.Json.list
               (List.map
                  (fun (rkey, t) ->
                    Common.object_
                      [
                        ("id", Common.string rkey);
                        ("record", Api.encode Ticket.main_jsont t);
                      ])
                  tickets))
        else if Ui.tty () then
          Ui.table
            ~heading:
              (Ui.heading
                 (if pulls then "Pull requests" else "Issues")
                 (List.length tickets))
            ([
               Ui.column ~align:`Right "#";
               Ui.column "State";
               Ui.column "Title";
             ]
            @ (if pulls then [ Ui.column "Target" ] else [])
            @ (if show_labels then [ Ui.column "Labels" ] else [])
            @ [ Ui.column "Opened" ])
            (List.map
               (fun (rkey, (t : Ticket.main)) ->
                 [
                   Ui.muted rkey;
                   Ui.state (state_of t);
                   Ui.bold (text_of t.title);
                 ]
                 @ (if pulls then
                      [ Ui.accent (Option.value ~default:"" t.target_branch) ]
                    else [])
                 @ (if show_labels then [ label_views (labels_of defined t) ]
                    else [])
                 @ [ Ui.muted t.created_at ])
               tickets)
        else
          List.iter
            (fun (rkey, (t : Ticket.main)) ->
              Fmt.pr "%-6s %-9s %s%a@." rkey (state_of t) (text_of t.title)
                Fmt.(option (fmt " -> %s"))
                t.target_branch)
            tickets)
  in
  Common.cmd
    (Cmd.info "list"
       ~doc:
         (if pulls then "List a repository's pull requests."
          else "List a repository's issues."))
    Term.(
      term_result
        (const action $ Common.repo $ Common.knot $ state $ label
       $ Common.json_flag))

let show ~pulls =
  let action repo id knot json =
    Common.run (fun env ->
        let d =
          Common.with_api ~busy:("Reading #" ^ id) env (fun api ->
              detail api (Common.resolve api ?knot repo) id)
        in
        if json || not (Ui.tty ()) then Common.json (detail_json d)
        else show_detail d)
  in
  Common.cmd
    (Cmd.info "show"
       ~doc:
         (if pulls then "Show a pull request, its revisions and its comments."
          else "Show an issue and its comments."))
    Term.(
      term_result
        (const action $ Common.repo $ id ~pulls $ Common.knot
       $ Common.json_flag))

let title =
  Arg.(
    required
    & opt (some string) None
    & info [ "title"; "t" ] ~docv:"TITLE" ~doc:"The title, in Markdown.")

let labels_opt =
  Common.strings [ "label"; "l" ] "LABEL"
    "Put LABEL on it. Repeat the option for more."

let opened ~pulls (created : Api.Atproto.Repo.CreateRecord.output) =
  if Ui.tty () then
    Ui.ok "Opened %s #%s" (kind ~pulls) (Api.rkey_of_uri created.uri)
  else Common.print Api.Atproto.Repo.CreateRecord.output_jsont created

let create_issue =
  let action repo knot title body labels =
    Common.run (fun env ->
        opened ~pulls:false
          (Common.with_api ~busy:"Opening issue" env (fun api ->
               let r = Common.resolve api ?knot repo in
               Api.open_ticket api r ~title ?body
                 ~labels:(Cmd_label.keys api r labels)
                 ())))
  in
  Common.cmd
    (Cmd.info "create" ~doc:"Open an issue."
       ~man:
         (Common.man
            [
              "Opens an issue in the repository. Anyone logged in may open \
               one, unless the knot allows fewer. Give the description with \
               $(b,--body), or write it in a file and give $(b,--body-file).";
            ]))
    Term.(
      term_result
        (const action $ Common.repo $ Common.knot $ title $ Common.body
       $ labels_opt))

(* Where a revision comes from. *)
let branch_opt =
  Common.optional [ "branch" ] "BRANCH"
    "Propose BRANCH instead of the current branch."

let source_opt =
  Common.optional [ "source" ] "REPO"
    "Take the branch from REPO, such as your fork, instead of the repository \
     the pull request is for."

let base_opt =
  Common.optional [ "base" ] "SHA"
    "Propose the changes since commit SHA instead of since the branch left the \
     target."

let head_opt =
  Common.optional [ "head" ] "SHA"
    "Propose up to commit SHA instead of the branch's latest pushed commit."

(* [revision api repository ~target ...] is the base, head, source and branch
   of a revision to propose to [target]. The head is the branch's tip on the
   knot, which is what the knot can fetch. The base is where it left
   [target], found in the clone when it has both commits. *)
let revision api (repository : Api.repository) ~target ?branch ?source ?base
    ?head () =
  let source =
    match source with None -> repository | Some s -> Api.resolve_repo api s
  in
  let head, branch =
    match head with
    | Some h -> (h, branch)
    | None -> (
        let name =
          match (branch, Local.branch ()) with
          | Some b, _ | None, Some b -> b
          | None, None -> failwith "Not on a branch. Name one with --branch."
        in
        match Api.branch api source name with
        | None ->
            failwith
              (Printf.sprintf
                 "The knot has no branch %s. Push it first, as in: git push \
                  origin %s"
                 name name)
        | Some b ->
            (match Local.rev_parse name with
            | Some local when local <> b.hash ->
                Logs.warn (fun m ->
                    m
                      "Your %s is at %s, but the knot has %s. Push to propose \
                       your latest commits."
                      name (Ui.short_sha local) (Ui.short_sha b.hash))
            | _ -> ());
            (b.hash, Some name))
  in
  let base =
    match base with
    | Some b -> b
    | None -> (
        match Api.branch api repository target with
        | None -> failwith ("The target has no branch " ^ target)
        | Some t ->
            if Local.has_commit t.hash && Local.has_commit head then
              Option.value ~default:t.hash (Local.merge_base t.hash head)
            else t.hash)
  in
  if base = head then
    failwith
      (Printf.sprintf "Nothing to propose: %s is already in %s"
         (Ui.short_sha head) target);
  (base, head, source.did, branch)

let create_pull =
  let target =
    Common.optional [ "target" ] "BRANCH"
      "Propose merging into BRANCH instead of the default branch."
  in
  let action repo knot title body labels target branch source base head =
    Common.run (fun env ->
        let created, submitted =
          Common.with_api ~busy:"Opening pull request" env (fun api ->
              let repository = Common.resolve api ?knot repo in
              let target =
                match target with
                | Some t -> t
                | None -> (
                    match Api.default_branch api repository with
                    | Some b -> b
                    | None -> failwith "Name the target branch with --target.")
              in
              let base, head, source, branch =
                revision api repository ~target ?branch ?source ?base ?head ()
              in
              let created =
                Api.open_ticket api repository ~title ?body
                  ~target_branch:target
                  ~labels:(Cmd_label.keys api repository labels)
                  ()
              in
              let ticket = Api.rkey_of_uri created.uri in
              (match
                 Api.submit_patch api repository ~ticket ~base ~head ~source
                   ?branch ()
               with
              | _ -> ()
              | exception ex ->
                  Logs.err (fun m ->
                      m
                        "Opened #%s, but submitting its first revision failed. \
                         Retry with: tangled pull update %s"
                        ticket ticket);
                  raise ex);
              (created, (base, head, target)))
        in
        let base, head, target = submitted in
        if Ui.tty () then
          Ui.ok "Opened pull request #%s: %s..%s into %s"
            (Api.rkey_of_uri created.uri)
            (Ui.short_sha base) (Ui.short_sha head) target
        else Common.print Api.Atproto.Repo.CreateRecord.output_jsont created)
  in
  Common.cmd
    (Cmd.info "create" ~doc:"Propose merging a branch."
       ~man:
         (Common.man
            [
              "Opens a pull request proposing the current branch for merging \
               into the repository's default branch. Push the branch first: \
               what is proposed is what the knot has, and you are warned if \
               your branch has commits it lacks.";
              "To propose from a fork, push the branch to your fork and give \
               it with $(b,--source). The other options choose a different \
               branch or target, or exact commits.";
              "After pushing more commits, run $(b,tangled pull update) so \
               reviewers see them.";
            ]))
    Term.(
      term_result
        (const action $ Common.repo $ Common.knot $ title $ Common.body
       $ labels_opt $ target $ branch_opt $ source_opt $ base_opt $ head_opt))

let update_pull =
  let action repo id knot branch source base head =
    Common.run (fun env ->
        let base, head =
          Common.with_api ~busy:("Updating #" ^ id) env (fun api ->
              let repository = Common.resolve api ?knot repo in
              let ticket =
                match Api.ticket api repository id with
                | Some t -> t
                | None -> failwith ("No ticket #" ^ id)
              in
              let target =
                match ticket.target_branch with
                | Some t -> t
                | None -> failwith ("#" ^ id ^ " is an issue")
              in
              (* Keep proposing from where the last revision came from. *)
              let last = List.rev (Api.patches api repository ~ticket:id) in
              let previous = Option.bind (List.nth_opt last 0) source_of in
              let branch =
                match branch with
                | Some _ -> branch
                | None when Local.branch () <> None -> None
                | None ->
                    Option.map (fun (s : Patch.source) -> s.branch) previous
              in
              let source =
                match (source, previous) with
                | Some s, _ -> Some s
                | None, Some p when p.repo <> repository.did -> Some p.repo
                | None, _ -> None
              in
              let base, head, source, branch =
                revision api repository ~target ?branch ?source ?base ?head ()
              in
              ignore
                (Api.submit_patch api repository ~ticket:id ~base ~head ~source
                   ?branch ());
              (base, head))
        in
        Ui.ok "Submitted %s..%s to pull request #%s" (Ui.short_sha base)
          (Ui.short_sha head) id)
  in
  Common.cmd
    (Cmd.info "update"
       ~doc:"Propose the latest commits of a pull request's branch."
       ~man:
         (Common.man
            [
              "Adds a revision to the pull request from the branch it was \
               proposed from, so that reviewers see the commits pushed since. \
               Push them first. Earlier revisions stay, and $(b,pull diff) can \
               show any of them.";
            ]))
    Term.(
      term_result
        (const action $ Common.repo $ id ~pulls:true $ Common.knot $ branch_opt
       $ source_opt $ base_opt $ head_opt))

(* [nth_revision d n] is revision [n], counted from 1, or the latest. *)
let nth_revision api repository id n =
  let revisions = Api.patches api repository ~ticket:id in
  match (revisions, n) with
  | [], _ -> failwith ("#" ^ id ^ " has no revision yet")
  | l, None -> List.nth l (List.length l - 1)
  | l, Some n -> (
      match List.nth_opt l (n - 1) with
      | Some p when n > 0 -> p
      | _ -> failwith (Printf.sprintf "#%s has %d revisions" id (List.length l))
      )

let revision_opt =
  Arg.(
    value
    & opt (some int) None
    & info [ "revision" ] ~docv:"N"
        ~doc:"Use revision N, counting the first as 1, instead of the latest.")

let diff_pull =
  let stat =
    Arg.(
      value & flag
      & info [ "stat" ] ~doc:"List the files changed instead of the changes.")
  in
  let action repo id knot n stat =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let repository, source, p =
              Ui.busy ("Reading #" ^ id) (fun () ->
                  let repository = Common.resolve api ?knot repo in
                  let p = nth_revision api repository id n in
                  let source =
                    match source_of p with
                    | Some s when s.repo <> repository.did ->
                        Some (Api.resolve_repo api s.repo)
                    | _ -> None
                  in
                  (repository, source, p))
            in
            let show r =
              Diff_view.show api r ~base:(Api.oid_of p.base)
                ~head:(Api.oid_of p.head) ~stat
            in
            (* A fork's head is in the fork, which has the base too unless
               it fell behind. Then only the target may have both. *)
            match source with
            | None -> show repository
            | Some fork -> (
                try show fork
                with
                | Eio.Io
                    (Xrpc.Error.E (Xrpc_error { error = "RefNotFound"; _ }), _)
                ->
                  show repository)))
  in
  Common.cmd
    (Cmd.info "diff" ~doc:"Show the changes a pull request proposes.")
    Term.(
      term_result
        (const action $ Common.repo $ id ~pulls:true $ Common.knot
       $ revision_opt $ stat))

let checkout_pull =
  let name =
    Common.optional [ "branch" ] "BRANCH"
      "Put the commits on BRANCH instead of $(i,pull/ID). An existing branch \
       of that name is reset."
  in
  let action repo id knot n name =
    Common.run (fun env ->
        let url, head, branch =
          Common.with_api ~busy:("Reading #" ^ id) env (fun api ->
              let repository = Common.resolve api ?knot repo in
              let p = nth_revision api repository id n in
              let source = source_of p in
              let from =
                match source with
                | Some s when s.repo <> repository.did ->
                    Api.resolve_repo api s.repo
                | _ -> repository
              in
              ( Api.git_url from,
                Api.oid_of p.head,
                Option.map (fun (s : Patch.source) -> s.branch) source ))
        in
        let fetch what = Local.run [ "fetch"; "--quiet"; url; what ] in
        let fetched =
          (match branch with Some b -> fetch b | None -> false)
          && Local.rev_parse "FETCH_HEAD" = Some head
        in
        if not (fetched || fetch head) then
          failwith ("Could not fetch " ^ Ui.short_sha head ^ " from " ^ url);
        let name = Option.value ~default:("pull/" ^ id) name in
        if not (Local.run [ "switch"; "--quiet"; "-C"; name; head ]) then
          failwith ("Could not switch to " ^ name);
        Ui.ok "Switched to %s at %s" name (Ui.short_sha head))
  in
  Common.cmd
    (Cmd.info "checkout" ~doc:"Try out a pull request in your clone."
       ~man:
         (Common.man
            [
              "Fetches the pull request's commits, from a fork if it came from \
               one, onto the branch $(i,pull/ID) and switches to it. Run it in \
               a clone of the repository.";
            ]))
    Term.(
      term_result
        (const action $ Common.repo $ id ~pulls:true $ Common.knot
       $ revision_opt $ name))

let comment ~pulls =
  let action repo id knot body =
    Common.run (fun env ->
        let body =
          match body with
          | Some b when String.trim b <> "" -> b
          | _ -> failwith "Give the comment with --body or --body-file."
        in
        let created =
          Common.with_api ~busy:("Commenting on #" ^ id) env (fun api ->
              Api.add_comment api
                (Common.resolve api ?knot repo)
                ~ticket:id body)
        in
        if Ui.tty () then Ui.ok "Commented on #%s" id
        else Common.print Api.Atproto.Repo.CreateRecord.output_jsont created)
  in
  Common.cmd
    (Cmd.info "comment"
       ~doc:(Printf.sprintf "Comment on %s." (an ~pulls))
       ~man:
         (Common.man
            [
              "Adds a comment, written in Markdown. Give it with $(b,--body), \
               or write it in a file and give $(b,--body-file).";
            ]))
    Term.(
      term_result
        (const action $ Common.repo $ id ~pulls $ Common.knot $ Common.body))

let edit ~pulls =
  let title =
    Common.optional [ "title"; "t" ] "TITLE" "The new title, in Markdown."
  in
  let users names doc = Common.strings names "USER" doc in
  let action repo id knot title body assign unassign label unlabel =
    Common.run (fun env ->
        if
          title = None && body = None
          && List.for_all (( = ) []) [ assign; unassign; label; unlabel ]
        then failwith "Nothing to change. See --help.";
        Common.with_api ~busy:("Editing #" ^ id) env (fun api ->
            let r = Common.resolve api ?knot repo in
            let dids = List.map (Api.resolve_handle api) in
            let keys = Cmd_label.keys api r in
            Api.edit_ticket api r ~rkey:id ?title ?body ~assign:(dids assign)
              ~unassign:(dids unassign) ~label:(keys label)
              ~unlabel:(keys unlabel) ());
        Ui.ok "Edited #%s" id)
  in
  Common.cmd
    (Cmd.info "edit"
       ~doc:"Change the title, body, assignees or labels."
       ~man:
         [
           `S Manpage.s_description;
           `P
             "Changes what the options name and leaves the rest as it is. An \
              empty $(b,--body) removes the description.";
           `P
             (Printf.sprintf
                "The %s's author, the repository's owner and its collaborators \
                 may edit it. Only the owner and collaborators may assign it \
                 to someone."
                (kind ~pulls));
           `P "A label must first be defined with $(b,tangled repo label add).";
         ])
    Term.(
      term_result
        (const action $ Common.repo $ id ~pulls $ Common.knot $ title
       $ Common.body
        $ users [ "assign" ] "Assign USER. Repeat the option for more."
        $ users [ "unassign" ] "Unassign USER. Repeat the option for more."
        $ Common.strings [ "label"; "l" ] "LABEL"
            "Add LABEL. Repeat the option for more."
        $ Common.strings [ "unlabel" ] "LABEL"
            "Take LABEL off. Repeat the option for more."))

let history ~pulls =
  let action repo id knot json =
    Common.run (fun env ->
        let versions, names =
          Common.with_api ~busy:("Reading history of #" ^ id) env (fun api ->
              let repository = Common.resolve api ?knot repo in
              let versions =
                Api.record_versions api repository
                  ~uri:(Api.ticket_uri repository id)
              in
              ( versions,
                if Ui.tty () && not json then
                  Common.handles api
                    (List.sort_uniq String.compare
                       (List.map
                          (fun (v : Api.Lex.Repo.ListRecordVersions.version) ->
                            v.editor)
                          versions))
                else [] ))
        in
        let module V = Api.Lex.Repo.ListRecordVersions in
        if json || not (Ui.tty ()) then
          Common.print (Jsont.list V.version_jsont) versions
        else
          Ui.table
            ~heading:(Ui.heading ("History of #" ^ id) (List.length versions))
            [ Ui.column "When"; Ui.column "Editor"; Ui.column "Change" ]
            (List.map
               (fun (v : V.version) ->
                 [
                   Ui.muted v.created_at;
                   Ui.account
                     (v.editor, Option.join (List.assoc_opt v.editor names));
                   Ui.text
                     (if v.cid = None then "deleted"
                      else Ui.short_sha v.change_id);
                 ])
               versions))
  in
  Common.cmd
    (Cmd.info "history"
       ~doc:
         (Printf.sprintf
            "List each change to %s, oldest first, and who made it."
            (an ~pulls)))
    Term.(
      term_result
        (const action $ Common.repo $ id ~pulls $ Common.knot
       $ Common.json_flag))

let state ~pulls name value ~doc =
  let action repo id knot =
    Common.run (fun env ->
        Common.with_api ~busy:("Updating #" ^ id) env (fun api ->
            Api.set_ticket_state api
              (Common.resolve api ?knot repo)
              ~rkey:id value);
        Ui.ok "#%s is now %s" id value)
  in
  Common.cmd (Cmd.info name ~doc)
    Term.(term_result (const action $ Common.repo $ id ~pulls $ Common.knot))

let issue_states =
  [
    ("close", "closed", "Close an issue without resolving it.");
    ("complete", "completed", "Close an issue as resolved.");
    ("reopen", "open", "Reopen a closed issue.");
  ]

let pull_states =
  [
    ("close", "closed", "Close a pull request without merging it.");
    ("reopen", "open", "Reopen a closed pull request.");
  ]

let states ~pulls =
  List.map
    (fun (name, value, doc) -> state ~pulls name value ~doc)
    (if pulls then pull_states else issue_states)

let state_man ~pulls =
  Printf.sprintf
    "Only the %s's author, the repository's owner and its collaborators may \
     close or reopen it."
    (kind ~pulls)

let cmd =
  Cmd.group
    (Cmd.info "issue" ~doc:"Open, discuss and close a repository's issues."
       ~man:
         (Common.man
            [
              "An issue reports a problem or asks for a change. People discuss \
               it in comments, and it is closed once dealt with, as \
               $(b,completed) if resolved. Each issue has a number within its \
               repository, which the commands take.";
              "Anyone may read issues. Anyone logged in may open one or \
               comment, unless the knot hosting the repository allows fewer.";
              state_man ~pulls:false;
            ]))
    ([
       list ~pulls:false;
       show ~pulls:false;
       create_issue;
       comment ~pulls:false;
       edit ~pulls:false;
       history ~pulls:false;
     ]
    @ states ~pulls:false)

let pull_cmd =
  Cmd.group
    (Cmd.info "pull" ~doc:"Propose, review and revise pull requests."
       ~man:
         (Common.man
            [
              "A pull request proposes merging the commits on a branch into \
               another, usually the default branch. Its author pushes the \
               branch, then opens the pull request. Reviewers read its diff \
               and comment. Each time the author pushes more commits and runs \
               $(b,pull update), the pull request gains a revision, and \
               earlier ones stay for comparison.";
              "The branch may be in the repository itself, for those who may \
               push to it, or in a fork, for everyone else.";
              "Merging a pull request is done with Git: merge the branch and \
               push, then close the pull request.";
              state_man ~pulls:true;
            ]))
    ([
       list ~pulls:true;
       show ~pulls:true;
       create_pull;
       update_pull;
       diff_pull;
       checkout_pull;
       comment ~pulls:true;
       edit ~pulls:true;
       history ~pulls:true;
     ]
    @ states ~pulls:true)
