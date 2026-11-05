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
  let settings =
    Spindle__Settings.v ~store
      ~now:(fun () -> 0.)
      ~defaults:{ repo = []; user = [] }
  in
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
    Catalog.v ~store ~settings ~network ~owner ~hostname:"spindle.test"
      ~admission:Catalog.Closed ~static:None ~ttl:60.
      ~now:(fun () -> !clock)
      ()
  in
  let self = Catalog.self catalog in
  assert (self = "did:web:spindle.test");
  let records = Hashtbl.create 8 in
  let reads = ref 0 in
  let failing = ref false in
  let gone = ref false in
  (* A pull request's versions, oldest first, as editor and labels, and
     whether the knot corrupts the blocks it serves. *)
  let pull = "at://" ^ repo ^ "/org.tangled.track.ticket/t1" in
  let pull_versions = ref [] and corrupt = ref false in
  let block labels =
    let data =
      Atp.Dagcbor.encode_string
        (`Map
           [
             ( "tangledLabels",
               `List (List.map (fun k -> `Map [ ("key", `String k) ]) labels) );
           ])
    in
    (Atp.Cid.create `Dag_cbor data, data)
  in
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
    | "/xrpc/org.tangled.git.listCommits" ->
        let hashes =
          if param "ref" = "refs/heads/main" then [ "c1"; "c2" ] else [ "c3" ]
        in
        obj
          [
            ("commits", arr (List.map (fun h -> obj [ ("hash", str h) ]) hashes));
          ]
    | "/xrpc/com.atproto.sync.getRepoStatus" ->
        if !gone then
          obj
            [
              ("did", str (param "did"));
              ("active", bool false);
              ("status", str "deleted");
            ]
        else obj [ ("did", str (param "did")); ("active", bool true) ]
    | "/xrpc/org.tangled.repo.listRecordVersions" ->
        assert (param "uri" = pull);
        obj
          [
            ("uri", str pull);
            ( "versions",
              arr
                (List.map
                   (fun (editor, labels) ->
                     obj
                       [
                         ("changeId", str "c");
                         ("editor", str editor);
                         ("createdAt", str "2026-01-01T00:00:00Z");
                         ("cid", str (Atp.Cid.to_string (fst (block labels))));
                       ])
                   !pull_versions) );
          ]
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
      let status, body =
        if !failing then ("503 Service Unavailable", "<html>down</html>")
        else if
          String.starts_with ~prefix:"/xrpc/com.atproto.sync.getBlocks" target
        then
          let blocks =
            List.map
              (fun (_, labels) ->
                let cid, data = block labels in
                (cid, if !corrupt then data ^ "x" else data))
              !pull_versions
          in
          ( "200 OK",
            Atp.Car.to_string { version = 1; roots = [] } (List.to_seq blocks)
          )
        else ("200 OK", encode (respond target))
      in
      Eio.Flow.copy_string
        (Printf.sprintf
           "HTTP/1.1 %s\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
           status (String.length body) body)
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
      (* A host's server error is reported apart from a refusal. *)
      failing := true;
      (match refresh () with
      | _ -> failwith "a server error was taken as an answer"
      | exception Network.Unavailable _ -> ());
      failing := false;
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
      (* The repository's own token acts for it, as Tangled's settings send. *)
      assert (Catalog.permitted catalog r repo);
      assert (not (Catalog.authorized catalog r repo));
      assert (Catalog.permitted catalog r owner);
      assert (not (Catalog.permitted catalog r helper));
      assert (Catalog.default_branch catalog r = None);
      set repo "org.tangled.git.refs" "self"
        (obj [ ("defaultBranch", str "trunk"); ("refs", obj []) ]);
      assert (Catalog.default_branch catalog r = Some "trunk");
      (* During a rename the owner may have no declaration yet. The repository
         stays served under its old name, and the rename is reported once the
         new declaration names it. *)
      let renamed = ref [] in
      Catalog.on_rename catalog (fun r ~old_name ->
          renamed := (old_name, r.name) :: !renamed);
      let rename name =
        obj
          [
            ("$type", str "org.tangled.repo.manifest");
            ("declaration", obj [ ("owner", str owner); ("name", str name) ]);
            ("spindle", str self);
          ]
      in
      Hashtbl.remove records (owner, "org.tangled.repo.declaration", "demo");
      set repo "org.tangled.repo.manifest" "self" (rename "Renamed");
      assert (refresh ());
      assert ((Option.get (Catalog.managed catalog repo)).name = "Demo");
      assert (!renamed = []);
      set owner "org.tangled.repo.declaration" "renamed" (declaration repo);
      assert (refresh ());
      assert ((Option.get (Catalog.managed catalog repo)).name = "Renamed");
      assert (!renamed = [ ("Demo", "Renamed") ]);
      (* An accepted repository is known without asking the network, and a
         deletion is what its knot reports. *)
      assert (Catalog.known catalog repo && not (Catalog.known catalog helper));
      assert (Catalog.names_self catalog (rename "Renamed"));
      assert (not (Catalog.names_self catalog (manifest ~spindle:"x" owner)));
      (* A manual run's ref must hold its commit. *)
      let r = Option.get (Catalog.managed catalog repo) in
      assert (
        Catalog.ref_contains catalog r ~ref_:"refs/heads/main" ~commit:"c2");
      assert (
        not
          (Catalog.ref_contains catalog r ~ref_:"refs/heads/other" ~commit:"c2"));
      (* A label counts when someone with push access last added it, not the
         pull request's author. *)
      let labels () = Catalog.pull_labels catalog r ~pull in
      pull_versions :=
        [
          (stranger, [ "ci-full" ]);
          (owner, [ "ci-full"; "ci-diagnose" ]);
          (stranger, [ "ci-full"; "ci-diagnose"; "other" ]);
        ];
      assert (labels () = [ "ci-diagnose" ]);
      (* Removed and added again by a collaborator, it counts. *)
      set repo "org.tangled.graph.collaborator" helper
        (obj [ ("createdAt", str "2026-01-01T00:00:00Z") ]);
      pull_versions :=
        !pull_versions
        @ [
            (helper, [ "ci-diagnose" ]); (helper, [ "ci-diagnose"; "ci-full" ]);
          ];
      assert (labels () = [ "ci-diagnose"; "ci-full" ]);
      (* A revoked collaborator's labels stop counting. *)
      Hashtbl.remove records (repo, "org.tangled.graph.collaborator", helper);
      assert (labels () = [ "ci-diagnose" ]);
      (* Re-added by the author after a collaborator removed it, it does not. *)
      pull_versions :=
        [ (owner, [ "ci-full" ]); (owner, []); (stranger, [ "ci-full" ]) ];
      assert (labels () = []);
      (* Blocks that do not match their content ids are refused. *)
      pull_versions := [ (owner, [ "ci-diagnose" ]) ];
      assert (labels () = [ "ci-diagnose" ]);
      corrupt := true;
      assert (labels () = []);
      corrupt := false;
      (* The default branch's head comes from the refs record's show-ref
         lines, which atproto writes as unpadded base64. *)
      let r = Option.get (Catalog.managed catalog repo) in
      assert (Catalog.default_head catalog r = None);
      let head = String.make 40 'c' in
      let refs lines =
        obj
          [
            ("defaultBranch", str "trunk");
            ( "refs",
              obj
                [
                  ( "$bytes",
                    str
                      (Base64.encode_string ~pad:false
                         (String.concat "\n" lines)) );
                ] );
          ]
      in
      set repo "org.tangled.git.refs" "self"
        (refs
           [
             String.make 40 'a' ^ " refs/heads/feature";
             head ^ " refs/heads/trunk";
             String.make 40 'b' ^ " refs/tags/v1";
           ]);
      assert (Catalog.default_head catalog r = Some ("trunk", head));
      set repo "org.tangled.git.refs" "self"
        (refs [ head ^ " refs/heads/feature"; "nothex refs/heads/trunk" ]);
      assert (Catalog.default_head catalog r = None);
      assert (not (Catalog.deleted catalog repo));
      gone := true;
      assert (Catalog.deleted catalog repo));
  print_endline
    "catalog: manifest and declaration agreement, admission, caching, renames, \
     live collaborator and repository-token checks and pull request labels \
     passed"
