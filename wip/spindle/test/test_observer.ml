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
    Spindle__Settings.v ~store
      ~now:(fun () -> 0.)
      ~defaults:{ repo = []; user = [] }
  in
  let owner = "did:plc:aaaaaaaaaaaaaaaaaaaaaaaa" in
  let other = "did:plc:bbbbbbbbbbbbbbbbbbbbbbbb" in
  let repo = "did:plc:rrrrrrrrrrrrrrrrrrrrrrrr" in
  let self = "did:web:spindle.test" in
  Spindle__Settings.note_owner settings ~repo ~owner;
  let commit collection =
    obj
      [
        ("kind", str "commit");
        ("did", str repo);
        ("commit", obj [ ("collection", str collection) ]);
      ]
  in
  let account ?(status = "deactivated") did active =
    obj
      [
        ("kind", str "account");
        ("did", str did);
        ( "account",
          obj
            ([ ("active", bool active); ("did", str did) ]
            @ if active then [] else [ ("status", str status) ]) );
      ]
  in
  (* Only followed collections of known repositories, manifests naming this
     spindle, and an owner with repositories going away for good are kept. *)
  let stranger ?record collection =
    obj
      [
        ("kind", str "commit");
        ("did", str other);
        ( "commit",
          obj
            ([ ("collection", str collection) ]
            @ match record with Some r -> [ ("record", r) ] | None -> []) );
      ]
  in
  assert (
    not (Observer.wanted settings ~self (stranger "org.tangled.review.patch")));
  assert (
    not (Observer.wanted settings ~self (stranger "org.tangled.track.ticket")));
  assert (
    not
      (Observer.wanted settings ~self
         (stranger ~record:(obj [ ("spindle", str "did:web:other.test") ])
            "org.tangled.repo.manifest")));
  assert (
    Observer.wanted settings ~self
      (stranger
         ~record:(obj [ ("spindle", str self) ])
         "org.tangled.repo.manifest"));
  assert (Observer.wanted settings ~self (commit "org.tangled.repo.manifest"));
  assert (
    not
      (Observer.wanted settings ~self
         (account ~status:"deactivated" owner false)));
  assert (Observer.wanted settings ~self (commit "org.tangled.review.patch"));
  assert (not (Observer.wanted settings ~self (commit "app.bsky.feed.post")));
  assert (
    Observer.wanted settings ~self (account ~status:"deleted" owner false));
  assert (not (Observer.wanted settings ~self (account owner true)));
  (* A relay's passing states are not an account going away. *)
  assert (
    not
      (Observer.wanted settings ~self
         (account ~status:"throttled" owner false)));
  assert (not (Observer.wanted settings ~self (account other false)));
  assert (
    not
      (Observer.wanted settings ~self
         (obj [ ("kind", str "identity"); ("did", str owner) ])));
  (* A repository's events stay in order of their keys, apart from other
     repositories'. *)
  let item ts did = (Printf.sprintf "jetstream/%d/%s/c/r/create" ts did, "") in
  let groups =
    Observer.groups [ item 3 repo; item 2 other; item 1 repo; ("bad", "") ]
  in
  assert (
    List.sort compare (List.map (List.map fst) groups)
    = List.sort compare
        [
          [ fst (item 1 repo); fst (item 3 repo) ];
          [ fst (item 2 other) ];
          [ "bad" ];
        ]);
  print_endline "observer: event filtering and per-repository ordering passed"
