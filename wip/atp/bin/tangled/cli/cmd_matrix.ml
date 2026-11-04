(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api
module M = Tangled.Matrix

let read env url =
  Eio.Switch.run @@ fun sw ->
  let http = Fetch_httpz.std env in
  let r = Fetch.get ~sw ~redirects:3 http url in
  let body =
    Eio.Buf_read.(parse_exn ~max_size:(64 * 1024) take_all (Fetch.body r))
  in
  (Fetch.status r, body)

let member key json =
  match Tangled.Schema.member key json with
  | Some (Jsont.String (s, _)) -> Some s
  | Some v -> Some (Tangled.Schema.text v)
  | None -> None

let decode body =
  match Jsont_bytesrw.decode_string Jsont.json body with
  | Ok json -> Some json
  | Error _ -> None

(* The client API of [server], from its [.well-known/matrix/client] or else
   the server itself, as Matrix clients discover it. *)
let homeserver env server =
  let fallback = "https://" ^ server in
  match read env (fallback ^ "/.well-known/matrix/client") with
  | 200, body -> (
      match
        Option.bind (decode body) (fun j ->
            Option.bind
              (Tangled.Schema.member "m.homeserver" j)
              (member "base_url"))
      with
      | Some url when String.starts_with ~prefix:"https://" url ->
          if String.ends_with ~suffix:"/" url then
            String.sub url 0 (String.length url - 1)
          else url
      | _ -> fallback)
  | _ -> fallback
  | exception (Eio.Cancel.Cancelled _ as e) -> raise e
  | exception _ -> fallback

(* A room alias names its room through the alias's server directory, which
   answers without a login. *)
let room_of_alias env alias =
  let server =
    match M.server_name alias with
    | Some s -> s
    | None -> failwith ("Not a room alias: " ^ alias)
  in
  let url =
    homeserver env server ^ "/_matrix/client/v3/directory/room/"
    ^ Uriz.pct_encode ~component:`Unreserved alias
  in
  match read env url with
  | 200, body -> (
      match Option.bind (decode body) (member "room_id") with
      | Some room when M.valid_room_id room -> room
      | _ -> failwith ("The directory gave no room for " ^ alias))
  | 404, _ -> failwith ("No room has the alias " ^ alias)
  | status, _ ->
      failwith
        (Printf.sprintf "The directory of %s answered HTTP %d" server status)

(* People write [@bot:server], and sometimes [:bot@server]. *)
let user_id s =
  let s =
    if String.length s > 1 && s.[0] = ':' then
      match String.index_opt s '@' with
      | Some i ->
          "@"
          ^ String.sub s 1 (i - 1)
          ^ ":"
          ^ String.sub s (i + 1) (String.length s - i - 1)
      | None -> s
    else s
  in
  if M.valid_user_id s then s
  else failwith ("Not a Matrix user ID, such as @bot:example.org: " ^ s)

let room =
  Arg.(
    required
    & opt (some string) None
    & info [ "room" ] ~docv:"ROOM"
        ~doc:
          "The room, by alias such as $(i,#ci:example.org) or by ID such as \
           $(i,!abc:example.org). An alias is resolved to the room's ID.")

let bot =
  Arg.(
    required
    & opt (some string) None
    & info [ "bot"; "user" ] ~docv:"USER"
        ~doc:
          "The Matrix user of the $(b,tangled-matrix) poster that should \
           report, such as $(i,@ci:example.org).")

let repository api repo = Common.resolve api repo

let set =
  let action repo bot room json =
    Common.run (fun env ->
        let bot = user_id bot in
        let alias, room_id =
          if M.valid_alias room then (Some room, room_of_alias env room)
          else if M.valid_room_id room then (None, room)
          else failwith ("Not a room alias or ID: " ^ room)
        in
        let binding =
          Common.with_api ~busy:"Saving" env (fun api ->
              let r = repository api repo in
              let binding =
                {
                  M.repo = r.did;
                  bot;
                  room = room_id;
                  alias;
                  created_at = Api.now ();
                }
              in
              let swap =
                Option.bind
                  (Api.get_record api ~did:(Api.get_did api) ~collection:M.nsid
                     ~rkey:r.did ())
                  (fun (o : Api.Atproto.Repo.GetRecord.output) -> o.cid)
              in
              ignore
                (Api.put_record api ~collection:M.nsid ~rkey:r.did
                   ~swap_record:swap
                   (Api.encode M.jsont binding));
              binding)
        in
        if json then Common.print M.jsont binding
        else
          Ui.ok "%s will report pipelines in %s%s" bot room_id
            (Option.fold ~none:"" ~some:(fun a -> " (" ^ a ^ ")") alias))
  in
  Common.cmd
    (Cmd.info "set" ~doc:"Report the repository's pipelines in a Matrix room.")
    Term.(
      term_result (const action $ Common.repo $ bot $ room $ Common.json_flag))

let show =
  let action repo user json =
    Common.run (fun env ->
        let bindings =
          Common.with_api env (fun api ->
              let r = repository api repo in
              let who =
                match user with
                | Some u -> [ Api.resolve_handle api u ]
                | None -> r.owner :: Api.collaborators api r
              in
              List.concat_map
                (fun did ->
                  List.filter_map
                    (fun (b : M.t) ->
                      if b.repo = r.did then Some (did, b) else None)
                    (try M.list api ~did with
                    | Eio.Cancel.Cancelled _ as e -> raise e
                    | _ -> []))
                (List.sort_uniq String.compare who))
        in
        if json then
          Common.json
            (Jsont.Json.list
               (List.map
                  (fun (did, b) ->
                    Common.object_
                      [
                        ("author", Common.string did);
                        ("binding", Api.encode M.jsont b);
                      ])
                  bindings))
        else
          List.iter
            (fun (did, (b : M.t)) ->
              Printf.printf "%s %s %s%s\n" did b.bot b.room
                (Option.fold ~none:"" ~some:(fun a -> " " ^ a) b.alias))
            bindings)
  in
  Common.cmd
    (Cmd.info "show"
       ~doc:
         "List the Matrix rooms that the repository's owner and collaborators \
          have asked for its pipelines to be reported in.")
    Term.(
      term_result (const action $ Common.repo $ Common.user $ Common.json_flag))

let remove =
  let action repo =
    Common.run (fun env ->
        Common.with_api ~busy:"Removing" env (fun api ->
            let r = repository api repo in
            Api.delete_record api ~collection:M.nsid ~rkey:r.did ());
        Ui.ok "Pipelines will no longer be reported to Matrix")
  in
  Common.cmd
    (Cmd.info "remove" ~doc:"Stop reporting the repository's pipelines.")
    Term.(term_result (const action $ Common.repo))

let cmd =
  Cmd.group
    (Cmd.info "matrix" ~doc:"Report a repository's pipelines in a Matrix room."
       ~man:
         (Common.man
            [
              "A $(b,tangled-matrix) poster, logged in as a Matrix bot, keeps \
               one message per pipeline in a room up to date as the pipeline \
               runs. $(b,set) asks it to, by writing an \
               $(i,org.recoil.tangled.matrix) record to your account, keyed by \
               the repository. The poster honours it only if you own the \
               repository or collaborate on it, and only if the bot has joined \
               the room. Invite the bot to the room first.";
              "The record is public. It names the room's ID but none of its \
               messages, which stay end-to-end encrypted.";
            ]))
    [ set; show; remove ]
