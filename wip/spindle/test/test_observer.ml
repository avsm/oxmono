(* SPDX-License-Identifier: ISC *)
module J = Spindle__Json
module Store = Spindle__Store
module Observer = Spindle__Observer
open J

let () =
  Eio_main.run @@ fun env ->
  let name = Filename.temp_file "spindle-observer-" "" in
  Sys.remove name;
  Unix.mkdir name 0o700;
  Fun.protect ~finally:(fun () ->
      Eio.Process.run env#process_mgr [ "rm"; "-rf"; "--"; name ])
  @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let store = Store.open_ ~sw Eio.Path.(env#fs / name) in
  let settings =
    Spindle.Settings.v ~store
      ~now:(fun () -> 0.)
      ~defaults:{ repo = []; user = [] }
  in
  let owner = "did:plc:aaaaaaaaaaaaaaaaaaaaaaaa" in
  let other = "did:plc:bbbbbbbbbbbbbbbbbbbbbbbb" in
  let repo = "did:plc:rrrrrrrrrrrrrrrrrrrrrrrr" in
  Spindle.Settings.note_owner settings ~repo ~owner;
  let commit collection =
    obj
      [
        ("kind", str "commit");
        ("did", str repo);
        ("commit", obj [ ("collection", str collection) ]);
      ]
  in
  let account did active =
    obj
      [
        ("kind", str "account");
        ("did", str did);
        ("account", obj [ ("active", bool active); ("did", str did) ]);
      ]
  in
  (* Only followed collections and an inactive owner with repositories are
     kept. *)
  assert (Observer.wanted settings (commit "org.tangled.review.patch"));
  assert (not (Observer.wanted settings (commit "app.bsky.feed.post")));
  assert (Observer.wanted settings (account owner false));
  assert (not (Observer.wanted settings (account owner true)));
  assert (not (Observer.wanted settings (account other false)));
  assert (
    not
      (Observer.wanted settings
         (obj [ ("kind", str "identity"); ("did", str owner) ])));
  (* A repository's events stay in order, apart from other repositories'. *)
  let item key did = (key, encode (obj [ ("did", str did) ])) in
  let groups =
    Observer.groups
      [ item "1" repo; item "2" other; item "3" repo; ("4", "not json") ]
  in
  assert (List.map (List.map fst) groups = [ [ "1"; "3" ]; [ "2" ]; [ "4" ] ]);
  print_endline "observer: event filtering and per-repository ordering passed"
