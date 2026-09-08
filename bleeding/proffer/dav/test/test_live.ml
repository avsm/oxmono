(* Opt-in smoke test. Existing resources are read-only. Mutations are scoped
   to a new, unpredictable collection whose successful creation we witnessed. *)
module D = Proffer_dav
exception Step_failed of string * exn
exception Injected_failure
let check label condition = if not condition then failwith label
let log fmt = Printf.printf (fmt ^^ "\n%!")
let read r = Eio.Buf_read.(parse_exn take_all) (Fetch.body r) ~max_size:1_048_577
let result = function Ok x -> x | Error _ -> failwith "invalid protocol value"
let query = Httpz_dav.Prop (List.map Httpz_dav.dav ["resourcetype"; "getetag"; "getcontentlength"])
let prop name response = match Httpz_dav.property (Httpz_dav.dav name) response with
  | Some (Ok p) -> Some p | Some (Error 404) | None -> None
  | Some (Error _) -> failwith "property query failed"
let text_prop name r = Option.map (fun p -> result (Httpz_dav.text p)) (prop name r)
let is_collection r = match prop "resourcetype" r with
  | Some p -> Httpz_dav.children (Httpz_dav.dav "collection") p <> []
  | None -> failwith "resource type absent"
let href client (r : Httpz_dav.response) = match r.hrefs with
  | [href] -> D.resolve client href
  | _ -> failwith "expected a single resource href"
let entries client = (D.propfind ~depth:`One client "" query).responses
  |> List.map (fun r -> href client r, r)
  |> List.filter (fun (url, _) -> url <> D.root client)
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)
let snapshot client =
  let entries = entries client in
  check "smoke test limited to 32 immediate children" (List.length entries <= 32);
  List.map (fun (url, r) ->
    let collection = is_collection r in
    let tag = text_prop "getetag" r and size = text_prop "getcontentlength" r in
    let body = if collection then None else match Option.bind size Int64.of_string_opt with
      | Some n when n >= 0L && n <= 1_048_576L -> Some (D.with_download client url read)
      | _ -> None
    in
    url, collection, (if collection then None else tag),
    (if collection then None else size), body) entries
let complete = function
  | D.Complete _ -> ()
  | D.Multi _ -> failwith "unexpected multistatus for single-resource mutation"
let expect_http status f = match f () with
  | _ -> failwith "request unexpectedly succeeded"
  | exception D.Http_error e when e.status = status -> ()
let password_file path = In_channel.with_open_bin path (fun channel ->
  let value = In_channel.input_all channel in
  check "invalid password file size" (String.length value > 0 && String.length value <= 8192);
  let n = String.length value in
  let n = if value.[n - 1] = '\n' then n - 1 else n in
  let n = if n > 0 && value.[n - 1] = '\r' then n - 1 else n in
  check "empty password" (n > 0);
  String.sub value 0 n)
let nonce random =
  let bytes = Cstruct.create 16 in
  Eio.Flow.read_exact random bytes;
  String.init 32 (fun i ->
    let c = Cstruct.get_uint8 bytes (i / 2) in
    "0123456789abcdef".[if i mod 2 = 0 then c lsr 4 else c land 15])
let scratch_test ?(after_upload = fun () -> ()) ~issue ~step ~random ~http ~caps client =
  let collection = "proffer-dav-test-" ^ nonce random ^ "/" in
  let scratch_root = D.resolve client collection in
  (* Print only our generated path: a lost MKCOL reply may need manual cleanup.
     A rejection or lost reply never grants this program deletion authority. *)
  log "Scratch candidate: %s" collection;
  step "create scratch collection" (fun () -> D.mkcol client collection);
  let scratch = D.v ~root:scratch_root http in
  let file = D.child scratch ~collection:"" "space % café.txt" in
  let copied = D.resolve scratch "copy.txt" and moved = D.resolve scratch "moved.txt" in
  let allowed = [file; copied; moved] in
  let cleanup () = step "scratch cleanup" (fun () ->
    let remaining = entries scratch in
    (* Never recursively delete an unexpected collection or child. *)
    check "unexpected scratch resource; leaving collection for inspection"
      (List.for_all (fun (url, r) -> List.mem url allowed && not (is_collection r)) remaining);
    List.iter (fun (url, _) -> complete (D.delete scratch url)) remaining;
    check "scratch collection is not empty" (entries scratch = []);
    complete (D.delete scratch "");
    expect_http 404 (fun () -> D.propfind scratch "" query);
    log "Scratch collection removed")
  in
  Fun.protect ~finally:cleanup (fun () ->
    step "create and read file" (fun () ->
      check "PUT creation status" (D.put ~condition:D.If_absent scratch file (Fetch.String "dav client test\n") = 201);
      check "uploaded content differs" (D.with_download scratch file read = "dav client test\n");
      let collision = match D.put ~condition:D.If_absent scratch file (Fetch.String "collision") with
        | status -> Some status
        | exception D.Http_error e when e.status = 412 -> None in
      let content = D.with_download scratch file read in
      (match collision with
      | None -> check "rejected creation changed content" (content = "dav client test\n")
      | Some status -> issue (Printf.sprintf
          "If-None-Match: * PUT returned %d; scratch file overwritten=%b"
          status (content = "collision"))));
    after_upload ();
    step "conditional download and update" (fun () ->
      let tag = D.with_download scratch file (Fetch.header Fetch.Header.etag) in
      match tag with
      | None -> log "ETag checks skipped: no validator"
      | Some tag ->
          let status = D.with_download
            ~headers:Fetch.Header.[if_none_match, `Etags [tag]] scratch file Fetch.status in
          if status <> 304 then issue (Printf.sprintf "conditional GET returned %d, expected 304" status);
          if tag.weak then log "Conditional update skipped: weak validator"
          else begin
            let stale = {tag with Fetch.Header.tag = tag.tag ^ "-deliberately-stale"} in
            (match D.put ~condition:(D.If_match stale) scratch file (Fetch.String "stale condition") with
            | status -> issue (Printf.sprintf "stale If-Match PUT returned %d, expected 412" status)
            | exception D.Http_error e when e.status = 412 -> ());
            (* Obtain a fresh validator if the server ignored the stale one. *)
            let tag = D.with_download scratch file (Fetch.header Fetch.Header.etag) |> Option.get in
            ignore (D.put ~condition:(D.If_match tag) scratch file (Fetch.String "conditional update\n"));
            check "conditional update content" (D.with_download scratch file read = "conditional update\n")
          end);
    let patch label target = step label (fun () ->
      let name = "urn:proffer:dav:smoke-test", "value" in
      let set s = Httpz_dav.Set [Httpz_dav.element name [Httpz_dav.Text s]] in
      try
      let m = D.proppatch scratch target [set "first"; set "second"] in
      let reports = List.concat_map (Httpz_dav.property_results name) m.responses in
      if reports <> [] && List.for_all Result.is_ok reports then begin
        let m = D.propfind scratch target (Httpz_dav.Prop [name]) in
        check "last instruction did not win" (match m.responses with
          | [r] -> (match Httpz_dav.property name r with Some (Ok p) -> Httpz_dav.text p = Ok "second" | _ -> false)
          | _ -> false)
      end else if reports <> [] && List.for_all (function Error (403 | 409 | 424) -> true | _ -> false) reports then
        log "Dead properties rejected by server policy"
      else failwith "unexpected PROPPATCH results"
      with
      | D.Protocol_error message -> issue (label ^ ": " ^ message)
      | D.Http_error e when List.mem e.status [403; 405; 501] ->
          issue (Printf.sprintf "PROPPATCH returned HTTP %d" e.status)) in
    patch "PROPPATCH on encoded filename" file;
    step "COPY and MOVE" (fun () ->
      complete (D.copy scratch ~src:file ~dst:copied ());
      (match D.copy scratch ~src:file ~dst:copied () with
      | _ -> issue "COPY ignored Overwrite: F for a scratch destination"
      | exception D.Http_error e when e.status = 412 -> ());
      complete (D.move scratch ~src:copied ~dst:moved ());
      expect_http 404 (fun () -> D.with_download scratch copied read);
      check "COPY/MOVE content differs" (D.with_download scratch moved read = D.with_download scratch file read));
    patch "PROPPATCH on plain filename" moved;
    if List.mem "2" caps.D.dav then step "lock, destination condition, refresh and unlock" (fun () ->
      let lease = try Some (D.lock ~timeout:(Httpz_dav.Seconds 60L) scratch moved)
        with D.Http_error e -> issue (Printf.sprintf "LOCK acquisition returned HTTP %d" e.status); None in
      match lease with
      | None -> ()
      | Some lease ->
          log "Lock acquired";
          let action label f =
            try f (); log "Completed: %s" label with
            | D.Http_error e -> issue (Printf.sprintf "%s returned HTTP %d" label e.status)
            | D.Protocol_error message -> issue (label ^ ": " ^ message)
          in
          Fun.protect ~finally:(fun () ->
            try D.unlock scratch lease; log "Lock released"
            with ex -> raise (Step_failed ("UNLOCK", ex))) (fun () ->
            action "locked PUT" (fun () ->
              ignore (D.put ~if_:(D.lock_condition lease) scratch moved (Fetch.String "locked update")));
            action "lock refresh" (fun () -> ignore (D.refresh_lock ~timeout:(Httpz_dav.Seconds 60L) scratch lease));
            action "COPY to locked destination" (fun () ->
              complete (D.copy ~overwrite:true ~if_:(D.lock_condition lease) scratch ~src:file ~dst:moved ()))))
    else log "Lock checks skipped: DAV class 2 not advertised";
    step "DELETE file" (fun () -> complete (D.delete scratch moved)))
let () =
  let root = ref "" and user = ref "" and password_path = ref "" in
  let scratch = ref false and fixture = ref false in
  Arg.parse [
    "--root", Arg.Set_string root, "HTTPS collection URL";
    "--user", Arg.Set_string user, "WebDAV username";
    "--password-file", Arg.Set_string password_path, "Password file (never printed)";
    "--scratch", Arg.Set scratch, "Also test writes in a new temporary collection";
    "--fixture", Arg.Set fixture, "Use the Docker runner's test credentials and CA";
  ] (fun _ -> raise (Arg.Bad "unexpected argument")) "Explicit WebDAV smoke test (read-only by default)";
  let current = ref "configuration" in
  try Eio_main.run @@ fun env ->
    let password, https = if !fixture then begin
      root := Sys.getenv "WEBDAV_URL"; user := Sys.getenv "WEBDAV_USER";
      let ca = In_channel.with_open_bin (Sys.getenv "WEBDAV_CA_FILE") In_channel.input_all in
      let anchors = match X509.Certificate.decode_pem_multiple ca with
        | Ok anchors -> anchors | Error _ -> failwith "invalid fixture CA" in
      let verification_time = Ptime.of_float_s (Eio.Time.now env#clock) in
      let authenticator = X509.Authenticator.chain_of_trust_no_crl
        ~time:(fun () -> verification_time) anchors in
      Sys.getenv "WEBDAV_PASSWORD", Httpz_tls.client ~authenticator
    end else begin
      check "root, user and password-file are required" (!root <> "" && !user <> "" && !password_path <> "");
      password_file !password_path, Httpz_tls.system
    end in
    check "HTTPS required" (String.starts_with ~prefix:"https://" !root);
    let http = Fetch_httpz.v ~clock:env#mono_clock ~https ~max_response:8_388_608 env#net ()
      |> Fetch.with_credentials ~scope:[!root] [Fetch.Credential.basic ~user:!user ~password] in
    let client = D.v ~root:!root http in
    let issues = ref [] in
    let issue message = issues := message :: !issues; log "Server deviation: %s" message in
    let step label f =
      current := label;
      let value = try Eio.Time.with_timeout_exn env#clock 45. f
        with ex -> raise (Step_failed (label, ex)) in
      log "Completed: %s" label; value
    in
    let caps = step "authenticated OPTIONS" (fun () -> D.options client "") in
    log "Capabilities: DAV class 1=%b, class 2=%b" (List.mem "1" caps.dav) (List.mem "2" caps.dav);
    let before = step "discovery and bounded reads" (fun () -> snapshot client) in
    log "Existing children: %d; small files read: %d" (List.length before)
      (List.length (List.filter (fun (_, _, _, _, body) -> Option.is_some body) before));
    if !scratch then begin
      let verify () = step "existing resources unchanged" (fun () ->
        check "existing listing, validators or contents changed" (snapshot client = before)) in
      if !fixture then begin
        (match Fun.protect ~finally:verify (fun () ->
          scratch_test ~after_upload:(fun () -> raise Injected_failure)
            ~issue ~step ~random:env#secure_random ~http ~caps client) with
        | () -> failwith "cleanup failure injection did not run"
        | exception Injected_failure -> log "Passed: cleanup after injected failure")
      end;
      Fun.protect ~finally:verify (fun () -> scratch_test ~issue ~step ~random:env#secure_random ~http ~caps client)
    end;
    current := "server behavior checks";
    check "server deviations found (see above); cleanup and preservation checks completed" (!issues = []);
    log "Live smoke test passed (%s)" (if !scratch then "isolated writes and cleanup" else "read-only")
  with ex ->
    (* Avoid printing response bodies, authentication headers or file contents. *)
    let rec unwrap = function
      | Step_failed (label, ex) -> current := label; unwrap ex
      | Fun.Finally_raised ex -> unwrap ex
      | ex -> ex in
    let ex = unwrap ex in
    let reason = match ex with
      | D.Http_error e -> Printf.sprintf "HTTP %d" e.status
      | D.Protocol_error message -> "DAV decoder: " ^ message
      | Failure message -> message
      | _ -> "transport, configuration or cleanup failure" in
    Printf.eprintf "Failed during %s: %s\n%!" !current reason;
    exit 1
