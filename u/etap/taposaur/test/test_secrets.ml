module S = Taposaur_secrets
let ( / ) = Eio.Path.( / )
let check = Alcotest.(check bool)
let equal = Alcotest.(check string)
let with_temp f = Eio_main.run (fun env ->
  let name = Filename.temp_file "taposaur-secrets-" "" in
  Unix.unlink name;
  let root = env#fs / name in
  Eio.Path.mkdir ~perm:0o700 root;
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree root)
    (fun () -> Eio.Switch.run (fun sw -> f env sw root)))
let names () =
  List.iter (fun c -> check "name round-trips" true (S.of_name (S.name c) = Some c))
    S.credentials;
  check "unknown name" true (S.of_name "taposaur/other" = None)
let resolution () = with_temp (fun env sw root ->
  let vault = root / "vault" and key_file = root / "master.key" in
  let env_of table name = List.assoc_opt name table in
  let absent = S.open_ ~env:(env_of []) ~sw ~fs:env#fs ~root:vault ~key_file () in
  check "no vault is not an error" false (S.keychain absent);
  check "nothing configured" true (S.find absent Elsevier_api_key = None);
  let fallback = S.open_ ~env:(env_of ["ELSEVIER_API_KEY", "env-key";
    "SPRINGER_API_KEY", "  "]) ~sw ~fs:env#fs ~root:vault ~key_file () in
  check "environment fallback" true (S.origin fallback Elsevier_api_key
    = Some Environment);
  check "blank environment is absent" true
    (S.find fallback Springer_api_key = None);
  Sekrit_eio.Store.init ~root:vault ~key_file;
  Eio.Switch.run (fun sw ->
    let store = Sekrit_eio.Store.open_ ~sw ~root:vault ~key_file in
    Sekrit_eio.Store.put store ~name:(S.name Elsevier_api_key) ~replace:false
      (Sekrit.Secret.of_string "vault-key"));
  let both = S.open_ ~env:(env_of ["ELSEVIER_API_KEY", "env-key";
    "CORE_API_TOKEN", "core-env"]) ~sw ~fs:env#fs ~root:vault ~key_file () in
  check "vault opened" true (S.keychain both);
  check "keychain overrides environment" true
    (S.origin both Elsevier_api_key = Some Keychain);
  equal "keychain value" "vault-key" (Option.get (S.value both Elsevier_api_key) ());
  equal "environment value" "core-env" (Option.get (S.value both Core_api_token) ());
  check "unset credential" true (S.value both Wiley_tokens = None);
  Eio.Switch.run (fun sw ->
    let store = Sekrit_eio.Store.open_ ~sw ~root:vault ~key_file in
    Sekrit_eio.Store.put store ~name:(S.name Elsevier_api_key) ~replace:true
      (Sekrit.Secret.of_string "rotated"));
  equal "rotation seen on next call" "rotated"
    (Option.get (S.value both Elsevier_api_key) ()))
let () = Alcotest.run "Taposaur secrets" ["secrets", [
  Alcotest.test_case "names" `Quick names;
  Alcotest.test_case "resolution" `Quick resolution]]
