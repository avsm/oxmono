module D = Fetch_dav
let ok = function Ok x -> x | Error (`Msg e) -> failwith e
let check name b = if not b then failwith name
let expect_http status f = match f () with
  | _ -> failwith ("expected HTTP " ^ string_of_int status)
  | exception D.Http_error e -> check
      (Printf.sprintf "expected HTTP %d, received %d" status e.status) (e.status = status)
let read r = Eio.Buf_read.(parse_exn take_all) (Fetch.body r) ~max_size:1_000_000
let property name m =
  let r = match m.Httpz_dav.responses with [r] -> r | _ -> failwith "expected one resource" in
  match Httpz_dav.property name r with Some (Ok e) -> e | _ -> failwith ("missing property " ^ snd name)
let () = Eio_main.run @@ fun env ->
  let ca = In_channel.with_open_bin (Sys.getenv "WEBDAV_CA_FILE") In_channel.input_all in
  let anchors = ok (X509.Certificate.decode_pem_multiple ca) in
  let verification_time = Ptime.of_float_s (Eio.Time.now env#clock) in
  let authenticator = X509.Authenticator.chain_of_trust_no_crl
    ~time:(fun () -> verification_time) anchors in
  let backend = Fetch_httpz.v ~clock:env#mono_clock ~https:(Httpz_tls.client ~authenticator) env#net () in
  List.iter (fun (label, root) ->
    let authenticated user password = Fetch.with_credentials ~scope:[root] ~allow_insecure:true
      [Fetch.Credential.basic ~user ~password] backend in
    let client = D.v ~root (authenticated (Sys.getenv "WEBDAV_USER") (Sys.getenv "WEBDAV_PASSWORD")) in
    let bob = D.v ~root (authenticated (Sys.getenv "WEBDAV_OTHER_USER") (Sys.getenv "WEBDAV_OTHER_PASSWORD")) in
    let anonymous = D.v ~root backend in
    expect_http 401 (fun () -> D.propfind anonymous "" Httpz_dav.Propname);
    let caps = D.options client "" in
    check "DAV classes" (List.mem "1" caps.dav && List.mem "2" caps.dav);
    let collection = "ocaml-" ^ label ^ "/" in
    D.mkcol client collection;
    Fun.protect ~finally:(fun () -> ignore (D.delete client collection)) (fun () ->
      expect_http 405 (fun () -> D.mkcol client collection);
      let file = D.child client ~collection "space % café.txt" in
      check "create file" ((D.put ~condition:D.If_absent client file (Fetch.String "first")).status = 201);
      expect_http 412 (fun () -> D.put ~condition:D.If_absent client file (Fetch.String "collision"));
      check "download" (D.with_download client file read = "first");
      check "range" (D.with_download ~headers:Fetch.Header.[raw "Range" "bytes=1-3"] client file
        (fun r -> Fetch.status r = 206 && read r = "irs"));
      let tag () = D.with_download client file (fun r ->
        match Fetch.header Fetch.Header.etag r with Some t -> t | None -> failwith "ETag absent") in
      let deadline = Eio.Time.now env#clock +. 5. in
      let rec strong () = let t = tag () in if not t.weak then t else begin
        check "strong ETag timeout" (Eio.Time.now env#clock < deadline);
        Eio.Time.sleep env#clock 0.1; strong () end in
      let validator = strong () in
      check "conditional GET" (D.with_download ~headers:Fetch.Header.[if_none_match, `Etags [validator]]
        client file Fetch.status = 304);
      expect_http 412 (fun () -> D.put ~condition:(D.If_match {weak=false; tag="stale"}) client file (Fetch.String "stale"));
      ignore (D.put ~condition:(D.If_match validator) client file (Fetch.String "updated"));
      let colour = "urn:ocaml-dav:test", "colour" in
      let detail = "urn:ocaml-dav:test", "detail" in
      let value = Httpz_dav.element colour [Httpz_dav.Text "blue & green"] in
      let nested = Httpz_dav.element ~attrs:[("", "label"), "  two  spaces  "]
        ("urn:ocaml-dav:test", "note") [Httpz_dav.Text "nested"] in
      let result = D.proppatch client file [Httpz_dav.Set [value; Httpz_dav.element detail [Httpz_dav.Element nested]]] in
      ignore (property colour result);
      let query = Httpz_dav.Prop [Httpz_dav.dav "resourcetype"; Httpz_dav.dav "getetag"; colour; detail; ("urn:ocaml-dav:test", "absent")] in
      let result = D.propfind client file query in
      let p = property colour result in
      check "dead property text" (Httpz_dav.text p = Ok "blue & green");
      let note = List.hd (Httpz_dav.children nested.name (property detail result)) in
      check "dead property nested attribute" (List.assoc ("", "label") note.attrs = "  two  spaces  ");
      check "missing property distinct" (Httpz_dav.property ("urn:ocaml-dav:test", "absent") (List.hd result.responses) = Some (Error 404));
      let repeated updates expected =
        let result = D.proppatch client file updates in
        let reports = Httpz_dav.property_results colour (List.hd result.responses) in
        check "all repeated instruction results" (List.length reports = 2 && List.for_all Result.is_ok reports);
        let result = D.propfind client file query in
        let actual = match Httpz_dav.property colour (List.hd result.responses) with
          | Some (Ok p) -> Httpz_dav.text p
          | Some (Error 404) -> Error "absent"
          | _ -> failwith "unexpected property result" in
        check "repeated instruction final state" (actual = expected)
      in
      let set value = Httpz_dav.Set [Httpz_dav.element colour [Httpz_dav.Text value]] in
      repeated [set "first"; set "second"] (Ok "second");
      repeated [set "temporary"; Httpz_dav.Remove [colour]] (Error "absent");
      repeated [Httpz_dav.Remove [colour]; set "blue & green"] (Ok "blue & green");
      let rollback = D.proppatch client file [Httpz_dav.Set [Httpz_dav.element colour [Httpz_dav.Text "rollback"]];
        Httpz_dav.Set [Httpz_dav.element (Httpz_dav.dav "getetag") [Httpz_dav.Text "protected"]]] in
      check "atomic failure" (Httpz_dav.property colour (List.hd rollback.responses) = Some (Error 424));
      check "rollback retained" (Httpz_dav.text (property colour (D.propfind client file query)) = Ok "blue & green");
      let listing = D.propfind ~depth:`One client collection query in
      check "listing includes root" (List.length listing.responses = 2);
      check "returned encoded href" (List.exists (fun (r : Httpz_dav.response) ->
        List.exists (fun href -> Httpz_dav.resolve_href ~base:root href = Ok file) r.hrefs) listing.responses);
      expect_http 403 (fun () -> D.propfind ~depth:`Infinity client collection query);
      let copied = collection ^ "copy" and moved = collection ^ "moved" in
      ignore (D.copy client ~src:file ~dst:copied ());
      expect_http 412 (fun () -> D.copy client ~src:file ~dst:copied ());
      ignore (D.copy ~overwrite:true client ~src:file ~dst:copied ());
      ignore (D.move client ~src:copied ~dst:moved ());
      expect_http 404 (fun () -> D.with_download client copied read);
      check "move content" (D.with_download client moved read = "updated");
      let destination = collection ^ "locked-destination" in
      ignore (D.put client destination (Fetch.String "old destination"));
      let with_destination_lock action =
        let lease = D.lock client destination in
        Fun.protect ~finally:(fun () -> D.unlock client lease)
          (fun () -> action (D.lock_condition lease))
      in
      with_destination_lock (fun if_ ->
        expect_http 423 (fun () -> D.copy ~overwrite:true client ~src:file ~dst:destination ());
        ignore (D.copy ~overwrite:true ~if_ client ~src:file ~dst:destination ());
        check "locked COPY destination" (D.with_download client destination read = "updated"));
      let move_source = collection ^ "move-source" in
      ignore (D.put client move_source (Fetch.String "moved into locked destination"));
      with_destination_lock (fun if_ ->
        ignore (D.move ~overwrite:true ~if_ client ~src:move_source ~dst:destination ());
        check "locked MOVE destination" (D.with_download client destination read = "moved into locked destination"));
      let locked_collection = collection ^ "locked-collection/" in
      D.mkcol client locked_collection;
      let lease = D.lock ~depth:`Zero client locked_collection in
      Fun.protect ~finally:(fun () -> D.unlock client lease) (fun () ->
        let if_ = D.lock_condition lease in
        (* Apache reports the locked parent in a 207, rather than giving the
           nonexistent request resource a whole-response 423. *)
        (match D.put client (locked_collection ^ "child") (Fetch.String "no token") with
        | _ -> failwith "missing parent lock token accepted"
        | exception D.Http_error e ->
            check "parent lock multistatus" (e.status = 207);
            let m = match Result.bind (Httpz_dav.parse_xml e.body) Httpz_dav.multistatus with
              | Ok m -> m | Error reason -> failwith reason in
            check "parent is locked" (List.exists (fun (r : Httpz_dav.response) ->
              r.outcome = Httpz_dav.Status 423) m.responses));
        ignore (D.put ~if_ client (locked_collection ^ "child") (Fetch.String "new child"));
        D.mkcol ~if_ client (locked_collection ^ "sub/"));
      let lease = D.lock ~timeout:(Httpz_dav.Seconds 60L) client file in
      expect_http 423 (fun () -> D.put client file (Fetch.String "no token"));
      expect_http 423 (fun () -> D.put bob file (Fetch.String "other user"));
      ignore (D.put ~if_:(D.lock_condition lease) client file (Fetch.String "locked write"));
      let lease = D.refresh_lock client lease in
      D.unlock client lease;
      ignore (D.put client file (Fetch.stream (Eio.Flow.string_source "chunked upload")));
      check "unknown-length upload" (D.with_download client file read = "chunked upload");
      ignore (D.put client file (Fetch.stream ~length:12L (Eio.Flow.string_source "known length")));
      check "known-length stream" (D.with_download client file read = "known length");
      ignore (D.proppatch client file [Httpz_dav.Remove [colour]]);
      check "removed property" (Httpz_dav.property colour (List.hd (D.propfind client file query).responses) = Some (Error 404));
      ignore (D.delete client moved);
      expect_http 404 (fun () -> D.delete client moved));
    Printf.printf "fetch.dav Docker %s: file, property, condition and lock workflows passed\n%!" label
  ) ["http", Sys.getenv "WEBDAV_HTTP_URL"; "https", Sys.getenv "WEBDAV_URL"]
