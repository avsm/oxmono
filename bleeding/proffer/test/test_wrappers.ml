open Proffer
open Proffer.Route
module H = Httpz.Header_name
module St = Httpz.Res
module M = Httpz.Method

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let site =
  Site.of_routes
    [ moved (s "old.xml" /? nil) "/new.xml"; found (s "wiki" /? nil) "/notes" ]

let compiled = Compiled.compile site

let () =
  let r = Proffer_mock.request compiled () M.Get "/old.xml" in
  check "moved is 301" (Proffer_mock.status r = St.Moved_permanently);
  check "moved sets location"
    (Proffer_mock.header r H.Location = Some "/new.xml");
  let r = Proffer_mock.request compiled () M.Get "/wiki" in
  check "found is 302" (Proffer_mock.status r = St.Found);
  check "found sets location"
    (Proffer_mock.header r H.Location = Some "/notes")

let authed =
  Site.of_routes
    [ get (s "action" /? nil) (fun _env _req respond ->
            Resp.text respond "secret") ]
  |> Site.with_auth ~scope:[ [ "action" ] ] ~realm:"stats" ~check:(fun auth ->
         auth = Some "Basic ok")
  |> Site.with_headers [ ("X-Frame-Options", "DENY") ]

let compiled_authed = Compiled.compile authed

let () =
  let r = Proffer_mock.request compiled_authed () M.Get "/action" in
  check "no auth is 401" (Proffer_mock.status r = St.Unauthorized);
  check "challenge names the realm"
    (Proffer_mock.header r H.Www_authenticate
    = Some "Basic realm=\"stats\"");
  let r =
    Proffer_mock.request compiled_authed ()
      ~headers:[ ("Authorization", "Basic ok") ]
      M.Get "/action"
  in
  check "good auth passes"
    (Proffer_mock.status r = St.Success && Proffer_mock.body r = "secret");
  check "security header is present"
    (Proffer_mock.header_other r "X-Frame-Options" = Some "DENY");
  (* A 405 under the gate must not tell an unauthenticated caller which routes
     exist, so the gate answers before the method check is reported. *)
  let r = Proffer_mock.request compiled_authed () M.Post "/action" in
  check "405 under the gate is 401 instead"
    (Proffer_mock.status r = St.Unauthorized);
  let r = Proffer_mock.request compiled_authed () M.Get "/elsewhere" in
  check "security header reaches the fallback"
    (Proffer_mock.status r = St.Not_found
    && Proffer_mock.header_other r "X-Frame-Options" = Some "DENY")

let mounted =
  Site.mount ~at:[ "api" ]
    (Site.of_routes
       [ get (s "ping" /? nil) (fun _env _req respond ->
                Resp.text respond "pong") ])
    (Site.of_routes
       [ get nil (fun _env _req respond -> Resp.text respond "root") ])

let compiled_mounted = Compiled.compile mounted

let () =
  let r = Proffer_mock.request compiled_mounted () M.Get "/api/ping" in
  check "mounted sub-site answers" (Proffer_mock.body r = "pong");
  let r = Proffer_mock.request compiled_mounted () M.Get "/" in
  check "parent still answers" (Proffer_mock.body r = "root")

let () =
  let gated =
    Site.with_auth ~scope:[ [] ] ~realm:"stats" ~check:(fun _ -> false)
      (Site.of_routes
         [ get nil (fun _env _req respond -> Resp.text respond "sub") ])
  in
  check "mounting a gated sub-site raises"
    (match Site.mount ~at:[ "action" ] gated (Site.of_routes []) with
    | _ -> false
    | exception Invalid_argument _ -> true)

(* [[]] gates everything and [[[]]] is one keystroke away from it, so an empty
   scope has to be refused. Serving the site open behind a wrapper named
   [with_auth] is the one outcome that must not be reachable by accident. *)
let () =
  check "an empty scope raises"
    (match
       Site.with_auth ~scope:[] ~realm:"stats" ~check:(fun _ -> false)
         (Site.of_routes
            [ get nil (fun _env _req respond -> Resp.text respond "open") ])
     with
    | _ -> false
    | exception Invalid_argument _ -> true)

let () = Printf.printf "test_wrappers: %d checks ok\n" !checks
