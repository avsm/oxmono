(* SPDX-License-Identifier: ISC *)
module J = Spindle__Json
module Store = Spindle__Store
module Catalog = Spindle__Catalog
module Network = Spindle__Network
open J

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
  let member = "did:plc:bbbbbbbbbbbbbbbbbbbbbbbb" in
  let catalog =
    Catalog.v ~store ~network ~owner ~hostname:"spindle.test" ~static:None
  in
  let collection = "sh.tangled.spindle.member" in
  let key = owner ^ "/" ^ collection in
  let records = ref [] and on_read = ref (fun () -> ()) in
  let serve () =
    while true do
      Eio.Switch.run @@ fun connection ->
      let flow, _ = Eio.Net.accept ~sw:connection listener in
      let reader = Eio.Buf_read.of_flow ~max_size:16384 flow in
      let request = Eio.Buf_read.line reader in
      let rec headers () = if Eio.Buf_read.line reader <> "" then headers () in
      headers ();
      let target = List.nth (String.split_on_char ' ' request) 1 in
      let body =
        if String.starts_with ~prefix:"/xrpc/" target then (
          let snapshot = obj [ ("records", arr !records) ] in
          !on_read ();
          snapshot)
        else
          obj
            [
              ("id", str (String.sub target 1 (String.length target - 1)));
              ( "service",
                arr
                  [
                    obj
                      [
                        ("id", str "#atproto_pds");
                        ("type", str "AtprotoPersonalDataServer");
                        ("serviceEndpoint", str origin);
                      ];
                  ] );
            ]
      in
      let body = encode body in
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
      let notice publisher =
        Catalog.notice catalog ~owner:publisher ~collection ~rkey:"grant"
      in
      let refresh () =
        Catalog.refresh catalog key
          ~value:(Option.get (Store.get store "reconcile" key))
      in
      let grant =
        obj [ ("subject", str member); ("instance", str "spindle.test") ]
      in
      let record =
        obj
          [
            ("uri", str ("at://" ^ owner ^ "/" ^ collection ^ "/grant"));
            ("value", grant);
          ]
      in
      records := [ record ];
      notice owner;
      refresh ();
      assert (List.mem member (Catalog.members catalog));
      assert (Store.get store "reconcile" (member ^ "/sh.tangled.repo") <> None);
      (* A forged member event from a non-owner schedules no owner PDS read. *)
      notice member;
      assert (Store.get store "reconcile" key = None);
      (* A delayed grant event reads the current, already revoked PDS state. *)
      records := [];
      notice owner;
      refresh ();
      assert (not (List.mem member (Catalog.members catalog)));
      notice owner;
      refresh ();
      assert (not (List.mem member (Catalog.members catalog)));
      (* An event received while a snapshot is in flight invalidates that snapshot. *)
      records := [ record ];
      notice owner;
      (on_read := fun () -> notice owner);
      refresh ();
      assert (not (List.mem member (Catalog.members catalog)));
      assert (Store.get store "reconcile" key <> None);
      (on_read := fun () -> ());
      refresh ();
      assert (List.mem member (Catalog.members catalog));
      Store.delete store "reconcile" (member ^ "/sh.tangled.repo");
      let repo = "did:web:repo.test" in
      Store.put store "sh.tangled.repo" (member ^ "/repo")
        (encode
           (obj
              [
                ("repoDid", str repo);
                ("knot", str origin);
                ("spindle", str "spindle.test");
              ]));
      notice owner;
      (match Catalog.member catalog member with
      | _ -> failwith "pending revocation must block pull recovery"
      | exception Catalog.Pending -> ());
      (match Catalog.managed catalog repo with
      | _ -> failwith "pending revocation must fail closed"
      | exception Catalog.Pending -> ());
      let canonical : Catalog.repo =
        {
          did = repo;
          owner = member;
          rkey = "repo";
          knot = origin;
          source = origin ^ "/" ^ repo;
        }
      in
      (match Catalog.current catalog canonical with
      | _ -> failwith "pending revocation must block an in-flight dispatch"
      | exception Catalog.Pending -> ());
      records := [];
      refresh ();
      assert (not (Catalog.current catalog canonical));
      print_endline
        "catalog: authoritative refresh, stale snapshots, publisher checks and \
         pending grants passed")
