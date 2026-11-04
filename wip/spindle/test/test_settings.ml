(* SPDX-License-Identifier: ISC *)
module Store = Spindle__Store
module Settings = Spindle__Settings
module E = Spindle__Xrpc_error
module J = Spindle__Json

let refused code f =
  match f () with
  | _ -> assert false
  | exception E.E (_, c, _) -> assert (c = code)

let () =
  Eio_main.run @@ fun env ->
  let name = Filename.temp_file "spindle-settings-" "" in
  Sys.remove name;
  Unix.mkdir name 0o700;
  Fun.protect ~finally:(fun () ->
      Eio.Process.run env#process_mgr [ "rm"; "-rf"; "--"; name ])
  @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let store = Store.open_ ~sw Eio.Path.(env#fs / name) in
  let clock = ref 1_800_000_000. in
  let t =
    Settings.v ~store
      ~now:(fun () -> !clock)
      ~defaults:{ repo = [ ("workflows", 2) ]; user = [ ("webhooks", 0) ] }
  in
  let repo = "did:plc:repo" and owner = "did:plc:owner" in
  (* Secrets *)
  Settings.add_secret t ~repo ~by:owner "TOKEN" "s3cret";
  refused "SecretAlreadyExists" (fun () ->
      Settings.add_secret t ~repo ~by:owner "TOKEN" "other");
  refused "InvalidRequest" (fun () ->
      Settings.add_secret t ~repo ~by:owner "1BAD" "x");
  refused "InvalidRequest" (fun () ->
      Settings.add_secret t ~repo ~by:owner "EMPTY" "");
  assert (Settings.secrets t repo = [ ("TOKEN", "s3cret") ]);
  assert (Settings.secrets t "did:plc:other" = []);
  let listed = J.list (J.required "secrets" (Settings.list_secrets t repo)) in
  assert (List.map (J.get "key") listed = [ "TOKEN" ]);
  assert (J.field "value" (List.hd listed) = None);
  Settings.remove_secret t ~repo "TOKEN";
  refused "SecretNotFound" (fun () -> Settings.remove_secret t ~repo "TOKEN");
  (* Bans and the repositories they reach *)
  Settings.note_owner t ~repo ~owner;
  assert (Settings.repos_of t owner = [ repo ]);
  Settings.ban t owner;
  assert (Settings.banned t owner);
  Settings.unban t owner;
  refused "BanNotFound" (fun () -> Settings.unban t owner);
  (* Invite codes: uses, addressee and expiry *)
  let code ?for_account uses =
    J.get "code" (Settings.invite t ~by:owner ~uses ~for_account)
  in
  let shared = code 2 in
  Settings.redeem t shared ~actor:"did:plc:a";
  (* An account already allowed spends no use. *)
  Settings.redeem t shared ~actor:"did:plc:a";
  Settings.redeem t shared ~actor:"did:plc:b";
  refused "InviteNotFound" (fun () ->
      Settings.redeem t shared ~actor:"did:plc:c");
  assert (Store.get store "allow" "did:plc:b" = Some "");
  (* Redemptions at once spend a code's uses once each. *)
  let once = code 1 in
  let spent = ref 0 in
  Eio.Fiber.List.iter
    (fun actor ->
      match Settings.redeem t once ~actor with
      | () -> incr spent
      | exception E.E _ -> ())
    [ "did:plc:r1"; "did:plc:r2"; "did:plc:r3" ];
  assert (!spent = 1);
  let addressed = code ~for_account:"did:plc:d" 1 in
  refused "InviteNotFound" (fun () ->
      Settings.redeem t addressed ~actor:"did:plc:e");
  Settings.redeem t addressed ~actor:"did:plc:d";
  let stale = code 1 in
  clock := !clock +. (3. *. 86400.);
  refused "InviteNotFound" (fun () ->
      Settings.redeem t stale ~actor:"did:plc:f");
  Settings.ban t "did:plc:g";
  let banned = code 1 in
  refused "AccessControl" (fun () ->
      Settings.redeem t banned ~actor:"did:plc:g");
  (* Quotas: defaults, overrides and unlimited *)
  assert (Settings.limit t Repo repo "workflows" = Some 2);
  assert (Settings.limit t User owner "webhooks" = None);
  Settings.set_limit t repo "workflows" (Some 5);
  assert (Settings.limit t Repo repo "workflows" = Some 5);
  Settings.set_limit t repo "workflows" None;
  assert (Settings.limit t Repo repo "workflows" = None);
  refused "Generic" (fun () -> Settings.set_limit t repo "workflows" (Some 0));
  let limits = J.list (J.required "limits" (Settings.limits t)) in
  assert (List.map (fun l -> J.number (J.required "limit" l)) limits = [ -1. ]);
  Settings.unset_limit t repo "workflows";
  assert (Settings.limit t Repo repo "workflows" = Some 2);
  (* Monthly minutes, charged once to a repository that owns itself *)
  Settings.add_minutes t [ repo; owner; repo ] 3;
  assert (Settings.minutes t repo = 3 && Settings.minutes t owner = 3);
  (* Charges at once all count. *)
  Eio.Fiber.List.iter
    (fun () -> Settings.add_minutes t [ owner ] 1)
    (List.init 5 (fun _ -> ()));
  assert (Settings.minutes t owner = 8);
  clock := !clock +. (40. *. 86400.);
  assert (Settings.minutes t repo = 0);
  Settings.add_secret t ~repo ~by:owner "KEPT" "x";
  Settings.wipe t repo;
  assert (Settings.secrets t repo = []);
  print_endline
    "settings: secrets, bans, invite codes, quotas and minutes passed"
