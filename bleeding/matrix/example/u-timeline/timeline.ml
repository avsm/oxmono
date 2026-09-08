module M = Matrix_eio
module Ui = Matrix_ui
module Cli = Matrix_cli
module Profile_store = Matrix_client.Profile_store
module Session = Matrix_client.Session

(* A safety net for an unattended run: give up after this many diff batches
   if nothing ever changes, rather than hang forever. Ctrl-C stops it
   sooner. *)
let max_batches = 20

let client_of_session (session : Session.Session_file.t) ~sw ~env =
  let client =
    M.Client.create ~sw ~env ~homeserver:session.server.homeserver ()
  in
  M.Client.with_session client
    {
      user_id = session.server.user_id;
      device_id = session.auth.device_id;
      access_token = session.auth.access_token;
      refresh_token = session.auth.refresh_token;
    }

let login_and_save ~sw ~env ~profile ~homeserver ~username ~password store =
  match (username, password) with
  | Some user, Some password ->
      let client = M.login_password ~sw ~env ~homeserver ~user ~password () in
      let session = Option.get (M.Client.session client) in
      let now = Ptime_clock.now () in
      let file : Session.Session_file.t =
        {
          server = { homeserver; user_id = session.user_id };
          auth =
            {
              access_token = session.access_token;
              device_id = session.device_id;
              refresh_token = session.refresh_token;
              access_token_expires_at = None;
              method_ = Session.Auth.Matrix;
            };
          sync = { next_batch = None; filter_id = None };
          metadata =
            { created_at = now; last_used_at = now; client_name = "u-timeline" };
        }
      in
      (match Profile_store.save_session store file with
      | Ok () -> ()
      | Error e ->
          Logs.err (fun m ->
              m "Cannot save the session: %a" Matrix_client.Error.pp e);
          exit Cli.exit_internal);
      client
  | _ ->
      Logs.err (fun m ->
          m "No session for profile %S; pass --username and set MATRIX_PASSWORD"
            profile);
      exit Cli.exit_usage

let client_of_profile ~sw ~env ~profile ~homeserver ~username ~password =
  let xdg = Xdge.create (Eio.Stdenv.fs env) "matrix" in
  let store = Profile_store.create ~xdg ~profile in
  match Profile_store.load_session store with
  | Ok (Some session) -> client_of_session session ~sw ~env
  | Ok None ->
      login_and_save ~sw ~env ~profile ~homeserver ~username ~password store
  | Error e ->
      Logs.err (fun m ->
          m "The session of profile %S is unreadable: %a" profile
            Matrix_client.Error.pp e);
      exit Cli.exit_internal

let event_store_of_profile ~env ~profile =
  let xdg = Xdge.create (Eio.Stdenv.fs env) "matrix" in
  let dir = Eio.Path.(Xdge.data_dir xdg / "profiles" / profile) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  let path = Eio.Path.native_exn Eio.Path.(dir / "timeline.sqlite3") in
  match Matrix_ui_sqlite.create path with
  | Ok store -> store
  | Error e ->
      Logs.err (fun m ->
          m "Cannot open %s: %s" path (Ui.Event_store.Error.to_string e));
      exit Cli.exit_internal

let string_of_virtual = function
  | Ui.Room_timeline.Gap _ -> "gap"
  | Ui.Room_timeline.Timeline_start -> "start of room"
  | Ui.Room_timeline.Read_marker -> "read marker"
  | Ui.Room_timeline.Date_divider { year; month; day } ->
      Printf.sprintf "-- %04d-%02d-%02d --" year month day

let print_item = function
  | Ui.Room_timeline.Event event_item ->
      let body =
        match Ui.Presentation.preview event_item.event with
        | Some body -> body
        | None -> "(no preview)"
      in
      Printf.printf "  %-24s %s\n%!"
        (Matrix_proto.Id.User_id.to_string event_item.event.sender)
        body
  | Ui.Room_timeline.Virtual { content; _ } ->
      Printf.printf "  [%s]\n%!" (string_of_virtual content)

let print_diff = function
  | Ui.Observable.List.Insert { index; value } ->
      Printf.printf "+ [%d]\n%!" index;
      print_item value
  | Ui.Observable.List.Remove { index } -> Printf.printf "- [%d]\n%!" index
  | Ui.Observable.List.Set { index; value } ->
      Printf.printf "= [%d]\n%!" index;
      print_item value
  | Ui.Observable.List.Move { from; to_ } ->
      Printf.printf "~ [%d -> %d]\n%!" from to_
  | Ui.Observable.List.Truncate { length } ->
      Printf.printf "truncate to %d\n%!" length
  | Ui.Observable.List.Reset values ->
      Printf.printf "reset (%d item(s))\n%!" (Array.length values);
      Array.iter print_item values

let string_of_pagination : Ui.Room_timeline.pagination -> string = function
  | `Reached_start -> "reached_start"
  | `More -> "more"
  | `Nothing_to_do -> "nothing_to_do"

let run () homeserver username password profile room_id =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client =
    client_of_profile ~sw ~env ~profile ~homeserver ~username ~password
  in
  let event_store = event_store_of_profile ~env ~profile in
  let sync = M.Sync_service.of_user ~user_id:(M.Client.user_id client) () in
  let rt =
    Ui.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) ~client ~sync
      ~event_store ()
  in
  Ui.Runtime.start rt;
  let timeline = Ui.Runtime.timeline rt room_id in
  let list = Ui.Room_timeline.items timeline in
  let snapshot, subscription = Ui.Observable.List.subscribe ~sw list in
  Printf.printf "%d item(s) in the cache:\n%!" (Array.length snapshot);
  Array.iter print_item snapshot;
  (match Ui.Room_timeline.paginate_back timeline () with
  | Ok pagination ->
      Printf.printf "paginate_back: %s\n%!" (string_of_pagination pagination)
  | Error error ->
      Format.printf "paginate_back failed: %a@." Matrix_client.Error.pp error);
  let rec loop count =
    if count < max_batches then
      match Ui.Observable.List.next subscription with
      | None -> ()
      | Some diffs ->
          List.iter print_diff diffs;
          loop (count + 1)
  in
  loop 0

let term =
  Cmdliner.Term.(
    const run $ Cli.verbosity_term $ Cli.homeserver_term $ Cli.username_opt_term
    $ Cli.password_opt_term $ Cli.profile_term $ Cli.room_term)

let cmd =
  let doc = "follow matrix-chat.ui's reactive room timeline over a sqlite cache" in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "timeline" ~doc) term

let () = exit (Cmdliner.Cmd.eval cmd)
