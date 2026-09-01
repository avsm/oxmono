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
    [ moved (s "old.xml") "/new.xml"; found (s "wiki") "/notes" ]

let compiled = site

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
    [ get (s "action") (fun _env _req respond ->
            Resp.text respond "secret") ]
  |> Site.with_auth ~scope:[ [ "action" ] ] ~realm:"stats" ~check:(fun auth ->
         auth = Some "Basic ok")
  |> Site.with_headers [ ("X-Frame-Options", "DENY") ]

let compiled_authed = authed

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
       [ get (s "ping") (fun _env _req respond ->
                Resp.text respond "pong") ])
    (Site.of_routes
       [ get root (fun _env _req respond -> Resp.text respond "root") ])

let compiled_mounted = mounted

let () =
  let r = Proffer_mock.request compiled_mounted () M.Get "/api/ping" in
  check "mounted sub-site answers" (Proffer_mock.body r = "pong");
  let r = Proffer_mock.request compiled_mounted () M.Get "/" in
  check "parent still answers" (Proffer_mock.body r = "root")

let () =
  let gated =
    Site.with_auth ~scope:[ [] ] ~realm:"stats" ~check:(fun _ -> false)
      (Site.of_routes
         [ get root (fun _env _req respond -> Resp.text respond "sub") ])
  in
  check "mounting a gated sub-site raises"
    (match Site.mount ~at:[ "action" ] gated (Site.of_routes []) with
    | _ -> false
    | exception Invalid_argument _ -> true)

(* A scope names paths in the site it was written against. Mounting that site
   under a prefix would leave every prefix in the scope naming a path that is
   no longer there, so the gate would match nothing and serve the sub-site
   open. The refusal at [mount] is what keeps that unreachable. *)
let () =
  let gated =
    Site.of_routes
      [ get (s "secret") (fun _env _req respond -> Resp.text respond "sub") ]
    |> Site.with_auth ~scope:[ [ "secret" ] ] ~realm:"stats"
         ~check:(fun _ -> false)
  in
  check "mounting a site with a stale scope raises"
    (match Site.mount ~at:[ "api" ] gated (Site.of_routes []) with
    | _ -> false
    | exception Invalid_argument _ -> true);
  (* The same refusal covers [with_headers], whose fields would be dropped. *)
  let decorated =
    Site.of_routes
      [ get (s "page") (fun _env _req respond -> Resp.text respond "sub") ]
    |> Site.with_headers [ ("X-Frame-Options", "DENY") ]
  in
  check "mounting a header-wrapped sub-site raises"
    (match Site.mount ~at:[ "api" ] decorated (Site.of_routes []) with
    | _ -> false
    | exception Invalid_argument _ -> true)

(* Gating after mounting is the supported order: the parent's decoration is
   applied to the request path, so a scope written in mounted terms gates the
   mounted routes and nothing else. *)
let () =
  let site =
    Site.mount ~at:[ "api" ]
      (Site.of_routes
         [ get (s "secret") (fun _env _req respond -> Resp.text respond "sub") ])
      (Site.of_routes
         [ get root (fun _env _req respond -> Resp.text respond "root") ])
    |> Site.with_auth ~scope:[ [ "api" ] ] ~realm:"stats" ~check:(fun auth ->
           auth = Some "Basic ok")
  in
  let r = Proffer_mock.request site () M.Get "/api/secret" in
  check "a mounted path under the scope is gated"
    (Proffer_mock.status r = St.Unauthorized);
  let r =
    Proffer_mock.request site ()
      ~headers:[ ("Authorization", "Basic ok") ]
      M.Get "/api/secret"
  in
  check "a mounted path passes with credentials" (Proffer_mock.body r = "sub");
  let r = Proffer_mock.request site () M.Get "/" in
  check "a path outside the scope is not gated" (Proffer_mock.body r = "root")

(* Repeated credentials are ambiguous across intermediaries, so the gate
   rejects them before application verification. *)
let () =
  let seen : string option Atomic.t = Atomic.make None in
  let site =
    Site.of_routes
      [ get root (fun _env _req respond -> Resp.text respond "in") ]
    |> Site.with_auth ~scope:[ [] ] ~realm:"stats" ~check:(fun auth ->
           Atomic.set seen
             (match auth with None -> None | Some s -> Some (Req.globalize s));
           match auth with Some s -> String.equal s "Basic first" | None -> false)
  in
  let r =
    Proffer_mock.request site ()
      ~headers:
        [ ("Authorization", "Basic first"); ("aUtHoRiZaTiOn", "Basic second") ]
      M.Get "/"
  in
  check "a duplicated Authorization is rejected before check"
    (Atomic.get seen = None && Proffer_mock.status r = St.Unauthorized)

(* [[]] gates everything and [[[]]] is one keystroke away from it, so an empty
   scope has to be refused. Serving the site open behind a wrapper named
   [with_auth] is the one outcome that must not be reachable by accident. *)
let () =
  check "an empty scope raises"
    (match
       Site.with_auth ~scope:[] ~realm:"stats" ~check:(fun _ -> false)
         (Site.of_routes
            [ get root (fun _env _req respond -> Resp.text respond "open") ])
     with
    | _ -> false
    | exception Invalid_argument _ -> true)

let () =
  List.iter
    (fun segment ->
      check "an ambiguous scope segment raises"
        (match
           Site.with_auth ~scope:[ [ segment ] ] ~realm:"stats"
             ~check:(fun _ -> false) (Site.of_routes [])
         with
        | _ -> false
        | exception Invalid_argument _ -> true))
    [ ""; "."; ".."; "a/b"; "a\\b"; "a\000b"; "a\tb"; "a\127b" ]

let () =
  let site =
    Site.of_routes []
    |> Site.with_auth ~scope:[ [ "%2F" ] ] ~realm:"stats"
         ~check:(fun _ -> false)
  in
  let literal = Proffer_mock.request site () M.Get "/%252F" in
  let separator = Proffer_mock.request site () M.Get "/%2F" in
  check "a literal percent escape is a usable decoded scope segment"
    (Proffer_mock.status literal = St.Unauthorized);
  check "an encoded separator does not match its literal percent spelling"
    (Proffer_mock.status separator = St.Not_found)

let () =
  List.iter
    (fun realm ->
      check "an unsafe realm raises"
        (match
           Site.with_auth ~scope:[ [] ] ~realm
             ~check:(fun _ -> false)
             (Site.of_routes [])
         with
        | _ -> false
        | exception Invalid_argument _ -> true))
    [
      "bad\"realm";
      "bad\\realm";
      "bad\rrealm";
      "bad\nrealm";
      "bad\000realm";
      "bad\001realm";
      "bad\127realm";
    ]

let () =
  let site =
    Site.of_routes []
    |> Site.with_auth ~scope:[ [] ] ~realm:"caf\195\169" ~check:(fun _ -> false)

  in
  let r = Proffer_mock.request site () M.Get "/" in
  check "a non-ASCII realm is preserved"
    (Proffer_mock.header r H.Www_authenticate
    = Some "Basic realm=\"caf\195\169\"")

(* A response decorator is applied after [Resp.v], so it has to re-check the
   invariants that depend on the final combined header block. *)
let () =
  let request site = Proffer_mock.request site () M.Get "/" in
  let base f = Site.of_routes [ get root (fun _ _ respond -> f respond) ] in
  let content_type =
    base (fun respond -> Resp.text respond "ok")
    |> Site.with_headers [ ("Content-Type", "text/html") ]
    |> request
  in
  check "site headers cannot duplicate typed Content-Type"
    (Proffer_mock.status content_type = St.Internal_server_error);
  let etag =
    base (fun respond -> Resp.html respond ~etag:(Etag.strong "v1") "ok")
    |> Site.with_headers [ ("ETag", "\"v2\"") ]
    |> request
  in
  check "site headers cannot duplicate a typed ETag"
    (Proffer_mock.status etag = St.Internal_server_error);
  check "site headers cannot set backend-owned Trailer"
    (match
       base (fun respond -> Resp.text respond "ok")
       |> Site.with_headers [ ("Trailer", "X-Other") ]
     with
    | _ -> false
    | exception Invalid_argument _ -> true);
  let upgrade =
    base (fun respond -> Resp.upgrade respond ~protocol:"echo" (fun _ -> ()))
    |> Site.with_headers [ ("Upgrade", "other") ]
    |> request
  in
  check "site headers cannot replace generated Upgrade"
    (Proffer_mock.status upgrade = St.Internal_server_error);
  let multipart_range =
    base (fun respond ->
        Resp.v respond ~status:St.Partial_content ~headers:Headers.empty
          ~content_type:(This "multipart/byteranges; boundary=b") Body.Empty)
    |> Site.with_headers [ ("Content-Range", "bytes 0-0/1") ]
    |> request
  in
  check "site headers cannot add a top-level range to multipart 206"
    (Proffer_mock.status multipart_range = St.Internal_server_error)

let () = Printf.printf "test_wrappers: %d checks ok\n" !checks
