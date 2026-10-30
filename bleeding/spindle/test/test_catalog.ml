(* SPDX-License-Identifier: ISC *)
module J = Spindle__Json
module Store = Spindle__Store
module Catalog = Spindle__Catalog
module Network = Spindle__Network
open J

(* One loopback server plays the PLC directory, the knot and the owner's PDS.
   Records live in [records], keyed by account, collection and record key. *)
let () =
  Eio_main.run @@ fun env ->
  Eio.Time.with_timeout_exn env#clock 15. @@ fun () ->
  let name = Filename.temp_file "spindle-catalog-" "" in
  Sys.remove name;
  Unix.mkdir name 0o700;
  Fun.protect ~finally:(fun () ->
      Eio.Process.run env#process_mgr [ "rm"; "-rf"; "--"; name ])
  @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let store = Store.open_ ~sw Eio.Path.(env#fs / name) in
  let listener =
    Eio.Net.listen ~sw ~reuse_addr:true ~backlog:8 env#net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr listener with
    | `Tcp (_, port) -> port
    | _ -> assert false
  in
  let origin = Printf.sprintf "http://127.0.0.1:%d" port in
  let network = Network.v ~allow_http:true ~plc:origin env in
  let owner = "did:plc:aaaaaaaaaaaaaaaaaaaaaaaa" in
  let stranger = "did:plc:bbbbbbbbbbbbbbbbbbbbbbbb" in
  let helper = "did:plc:cccccccccccccccccccccccc" in
  let repo = "did:plc:rrrrrrrrrrrrrrrrrrrrrrrr" in
  let clock = ref 0. in
  let catalog =
    Catalog.v ~store ~network ~owner ~hostname:"spindle.test"
      ~admission:Catalog.Closed ~static:None ~ttl:60.
      ~now:(fun () -> !clock)
      ()
  in
  let self = Catalog.self catalog in
  assert (self = "did:web:spindle.test");
  let records = Hashtbl.create 8 in
  let reads = ref 0 in
  let manifest ?(spindle = self) owner =
    obj
      [
        ("$type", str "org.tangled.repo.manifest");
        ("declaration", obj [ ("owner", str owner); ("name", str "Demo") ]);
        ("spindle", str spindle);
        ("createdAt", str "2026-01-01T00:00:00Z");
      ]
  in
  let declaration repo =
    obj
      [
        ("$type", str "org.tangled.repo.declaration");
        ("repo", str repo);
        ("createdAt", str "2026-01-01T00:00:00Z");
      ]
  in
  let set account collection rkey value =
    Hashtbl.replace records (account, collection, rkey) value
  in
  let document did services =
    obj
      [
        ("id", str did);
        ( "service",
          arr
            (List.map
               (fun (id, kind, endpoint) ->
                 obj
                   [
                     ("id", str id);
                     ("type", str kind);
                     ("serviceEndpoint", str endpoint);
                   ])
               services) );
      ]
  in
  let respond target =
    let uri = Uriz.of_string_exn ("http://x" ^ target) in
    let param k =
      match List.assoc_opt k (Uriz.query_params uri) with
      | Some (Some v) -> v
      | _ -> ""
    in
    match Uriz.path uri with
    | "/xrpc/com.atproto.repo.getRecord" -> (
        incr reads;
        let key = (param "repo", param "collection", param "rkey") in
        match Hashtbl.find_opt records key with
        | Some value -> obj [ ("uri", str "at://x"); ("value", value) ]
        | None -> obj [ ("error", str "RecordNotFound") ])
    | path ->
        let did = String.sub path 1 (String.length path - 1) in
        if did = repo then
          document did
            [
              ("#atproto_pds", "AtprotoPersonalDataServer", origin);
              ("#tangled_knot", "TangledKnot", origin ^ "/repo/abc");
            ]
        else
          document did [ ("#atproto_pds", "AtprotoPersonalDataServer", origin) ]
  in
  let serve () =
    while true do
      Eio.Switch.run @@ fun connection ->
      let flow, _ = Eio.Net.accept ~sw:connection listener in
      let reader = Eio.Buf_read.of_flow ~max_size:16384 flow in
      let request = Eio.Buf_read.line reader in
      let rec headers () = if Eio.Buf_read.line reader <> "" then headers () in
      headers ();
      let target = List.nth (String.split_on_char ' ' request) 1 in
      let body = encode (respond target) in
      Eio.Flow.copy_string
        (Printf.sprintf
           "HTTP/1.1 200 OK\r\n\
            Content-Length: %d\r\n\
            Connection: close\r\n\
            \r\n\
            %s"
           (String.length body) body)
        flow
    done
  in
  Eio.Fiber.first serve (fun () ->
      let served () = Option.is_some (Catalog.managed catalog repo) in
      let refresh () =
        Catalog.forget catalog repo;
        served ()
      in
      assert (not (served ()));
      set repo "org.tangled.repo.manifest" "self" (manifest owner);
      assert (not (refresh ()));
      (* The declaration slug is the lowercased name. *)
      set owner "org.tangled.repo.declaration" "demo" (declaration repo);
      assert (refresh ());
      let r = Option.get (Catalog.managed catalog repo) in
      assert (r.owner = owner && r.name = "Demo");
      assert (r.knot = origin && r.source = origin ^ "/" ^ repo);
      assert (
        Catalog.knot_did catalog repo
        = Printf.sprintf "did:web:127.0.0.1%%3A%d" port);
      (* Acceptance is cached until it expires or is dropped. *)
      let before = !reads in
      assert (served ());
      assert (!reads = before);
      set repo "org.tangled.repo.manifest" "self"
        (manifest ~spindle:"did:web:other.test" owner);
      assert (served ());
      clock := 61.;
      assert (not (served ()));
      set repo "org.tangled.repo.manifest" "self" (manifest owner);
      assert (refresh ());
      (* A declaration naming another repository does not claim this one. *)
      set owner "org.tangled.repo.declaration" "demo" (declaration helper);
      assert (not (refresh ()));
      set owner "org.tangled.repo.declaration" "demo" (declaration repo);
      (* A closed spindle serves admins and allow-listed owners only. *)
      set repo "org.tangled.repo.manifest" "self" (manifest stranger);
      set stranger "org.tangled.repo.declaration" "demo" (declaration repo);
      assert (not (refresh ()));
      Catalog.allow catalog stranger;
      assert (served ());
      assert (Catalog.allowed catalog = [ stranger ]);
      Catalog.disallow catalog stranger;
      assert (not (served ()));
      set repo "org.tangled.repo.manifest" "self" (manifest owner);
      assert (refresh ());
      (* Collaboration is read from the knot on every check. *)
      let r = Option.get (Catalog.managed catalog repo) in
      assert (Catalog.authorized catalog r owner);
      assert (not (Catalog.authorized catalog r helper));
      set repo "org.tangled.graph.collaborator" helper
        (obj [ ("createdAt", str "2026-01-01T00:00:00Z") ]);
      assert (Catalog.authorized catalog r helper);
      Hashtbl.remove records (repo, "org.tangled.graph.collaborator", helper);
      assert (not (Catalog.authorized catalog r helper));
      assert (Catalog.default_branch catalog r = None);
      set repo "org.tangled.git.refs" "self"
        (obj [ ("defaultBranch", str "trunk"); ("refs", obj []) ]);
      assert (Catalog.default_branch catalog r = Some "trunk"));
  print_endline
    "catalog: manifest and declaration agreement, admission, caching and live \
     collaborator checks passed"
