module D = Fetch_dav
module S = D.Session

let check name b = if not b then failwith name

let dav_error =
  "<d:error xmlns:d='DAV:'><d:unsupported-method/></d:error>"

let prop href body =
  "<d:multistatus xmlns:d='DAV:' xmlns:c='urn:ietf:params:xml:ns:caldav'>"
  ^ "<d:response><d:href>" ^ href ^ "</d:href><d:propstat><d:prop>" ^ body
  ^ "</d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat>"
  ^ "</d:response></d:multistatus>"

let () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let run ?(root_status = 404) ?(root_body = "") ?(well_known_status = 301)
      ?(location = "/dav/") ?(url = "https://example.test/") () =
    let seen = ref [] in
    let backend =
      Fetch_mock.client (fun req ->
          let url = Fetch.Middleware.Url.to_string req.url in
          let meth = Http.Method.to_string req.meth in
          check "requests stay on the configured origin"
            (String.starts_with ~prefix:"https://example.test/" url);
          check "credentials accompany discovery"
            (Http.Header.get req.headers "authorization" <> None);
          seen := (meth, url) :: !seen;
          let xml ?(status = 207) body =
            Fetch_mock.respond ~status
              ~headers:
                (Http.Header.of_list [ ("Content-Type", "application/xml") ])
              body req
          in
          match (meth, url) with
          | "GET", "https://example.test/.well-known/caldav" ->
              Fetch_mock.respond ~status:well_known_status
                ~headers:(Http.Header.of_list [ ("Location", location) ])
                "" req
          | "PROPFIND", "https://example.test/dav/" ->
              xml
                (prop "/dav/"
                   "<d:current-user-principal><d:href>/principal/</d:href></d:current-user-principal>")
          | "PROPFIND", "https://example.test/principal/" ->
              xml
                (prop "/principal/"
                   "<c:calendar-home-set><d:href>/calendars/</d:href></c:calendar-home-set>")
          | "PROPFIND", _ -> xml ~status:root_status root_body
          | _ -> failwith "Unexpected discovery request")
    in
    let result =
      S.connect ~sw ~service:`Caldav
        ~credentials:
          [ Fetch.Credential.basic ~user:"owner" ~password:"synthetic" ]
        ~home_set:(Httpz_dav.caldav "calendar-home-set")
        backend url
    in
    (result, List.rev !seen)
  in
  let check_discovery root_body root_status =
    let result, seen = run ~root_status ~root_body () in
    let session =
      match result with
      | Ok s -> s
      | Error e -> failwith ("discovery failed: " ^ S.error_to_string e)
    in
    check "origin root discovers the authenticated home set"
      (S.principal session = "https://example.test/principal/"
      && S.home_sets session = [ "https://example.test/calendars/" ]);
    check "discovery uses one explicit well-known GET"
      (seen
      = [
          ("PROPFIND", "https://example.test/");
          ("GET", "https://example.test/.well-known/caldav");
          ("PROPFIND", "https://example.test/dav/");
          ("PROPFIND", "https://example.test/principal/");
        ])
  in
  List.iter
    (fun root_body ->
      List.iter (check_discovery root_body)
        [ 404; 405; 301; 302; 303; 307; 308 ])
    [ ""; dav_error ];
  let result, seen = run ~url:"https://example.test" () in
  check "empty origin path discovers"
    (Result.is_ok result && List.length seen = 4);
  let result, seen = run ~url:"https://example.test/.well-known/caldav" () in
  check "explicit well-known discovery is unchanged"
    (Result.is_ok result && List.length seen = 3);
  List.iter
    (fun root_status ->
      let result, seen = run ~root_status () in
      check "authentication and server failures are not retried"
        (match result with
        | Error (S.Http (status, _)) ->
            status = root_status && List.length seen = 1
        | _ -> false))
    [ 401; 403; 500 ];
  List.iter
    (fun root_status ->
      let result, seen = run ~root_status ~root_body:dav_error () in
      check "DAV authentication and server failures are not retried"
        (match result with
        | Error (S.Dav (status, _)) ->
            status = root_status && List.length seen = 1
        | _ -> false))
    [ 401; 403; 500 ];
  let result, seen =
    run ~root_status:405 ~root_body:dav_error
      ~url:"https://example.test/missing/" ()
  in
  check "explicit DAV failures are not replaced"
    (match result with
    | Error (S.Dav (405, _)) -> List.length seen = 1
    | _ -> false);
  let result, seen = run ~url:"https://example.test/missing/" () in
  check "explicit missing paths are not replaced"
    (match result with
    | Error (S.Not_found _) -> List.length seen = 1
    | _ -> false);
  List.iter
    (fun (well_known_status, location) ->
      let result, seen = run ~well_known_status ~location () in
      check "missing or root-pointing discovery terminates"
        (match result with
        | Error (S.Not_found _) -> List.length seen = 2
        | _ -> false))
    [ (404, "/dav/"); (301, "/") ];
  let result, seen = run ~well_known_status:401 () in
  check "well-known authentication failure is preserved"
    (match result with
    | Error (S.Http (401, _)) -> List.length seen = 2
    | _ -> false);
  List.iter
    (fun location ->
      let result, seen = run ~location () in
      check "discovery cannot cross origins or downgrade TLS"
        (match result with
        | Error (S.Discovery _) -> List.length seen = 2
        | _ -> false))
    [ "https://other.test/dav/"; "http://example.test/dav/" ];
  print_endline
    "DAV root discovery, failure handling and credential scope passed."
