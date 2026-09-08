module Client = Matrix_client.Client
module Error = Matrix_client.Error
module Media = Matrix_client.Media
module Fetcher = Matrix_client.Media_fetcher
module Store = Matrix_client.Media_store
module Attachment = Matrix_client.Encrypted_attachment
module Id = Matrix_proto.Id
module Eio_client = Matrix_eio.Client
module Eio_media = Matrix_eio.Media

let mock_env =
  object
    method secure_random = Eio.Flow.string_source (String.make 4096 'r')
  end

let session =
  {
    Client.user_id = Result.get_ok (Id.User_id.of_string "@alice:example.org");
    access_token = "token";
    device_id = Result.get_ok (Id.Device_id.of_string "DEVICE");
    refresh_token = None;
  }

let client fetch =
  let config =
    Client.config ~homeserver:(Uriz.of_string_exn "https://hs.example") ()
  in
  Client.with_session
    (Client.create ~config ~fetch
       ~random:(Matrix_client.Random.of_env mock_env))
    session

let mxc s = Result.get_ok (Media.Mxc.of_string s)
let request uri = { Media.source = Media.Plain (mxc uri); format = Media.File }

let encrypted_request file =
  { Media.source = Media.Encrypted file; format = Media.File }

let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)
let run f = Eio_mock.Backend.run f

let test_cache_hit_and_miss () =
  run @@ fun () ->
  let calls = ref 0 in
  let fetcher =
    Fetcher.create (fun _ _ ->
        incr calls;
        Ok "from fetcher")
  in
  let store = Store.memory () in
  let c = client (Fetch_mock.client (fun _ -> assert false)) in
  let req = request "mxc://hs.example/cache-hit" in
  (match Fetcher.get_content ~store ~fetcher c req with
  | Ok bytes -> check_string "first fetch" "from fetcher" bytes
  | Error _ -> Alcotest.fail "first fetch failed");
  (match Fetcher.get_content ~store ~fetcher c req with
  | Ok bytes -> check_string "cache hit" "from fetcher" bytes
  | Error _ -> Alcotest.fail "cache hit failed");
  check_bool "fetcher called once" true (!calls = 1)

let test_local_does_not_fetch () =
  run @@ fun () ->
  let calls = ref 0 in
  let fetcher =
    Fetcher.create (fun _ _ ->
        incr calls;
        Ok "network")
  in
  let store = Store.memory () in
  let uri = Store.local_uri ~txn_id:"local-fetcher" in
  let key = Store.{ uri; format = File } in
  ignore (Store.add ~protected:true store key ~data:"local bytes");
  let c = client (Fetch_mock.client (fun _ -> assert false)) in
  let req = { Media.source = Media.Plain uri; format = Media.File } in
  (match Fetcher.get_content ~store ~fetcher c req with
  | Ok bytes -> check_string "local bytes" "local bytes" bytes
  | Error _ -> Alcotest.fail "local fetch failed");
  ignore (Store.remove store key);
  (match Fetcher.get_content ~store ~fetcher c req with
  | Error (Media.Media_error (Error.Policy_denied _)) -> ()
  | _ -> Alcotest.fail "missing local media did not fail locally");
  check_bool "local fetcher not called" true (!calls = 0)

let test_cache_can_be_bypassed () =
  run @@ fun () ->
  let calls = ref 0 in
  let fetcher =
    Fetcher.create (fun _ _ ->
        incr calls;
        Ok (Printf.sprintf "fetch-%d" !calls))
  in
  let store = Store.memory () in
  let c = client (Fetch_mock.client (fun _ -> assert false)) in
  let req = request "mxc://hs.example/no-cache" in
  (match Fetcher.get_content ~use_cache:false ~store ~fetcher c req with
  | Ok bytes -> check_string "first uncached fetch" "fetch-1" bytes
  | Error _ -> Alcotest.fail "first uncached fetch failed");
  (match Fetcher.get_content ~use_cache:false ~store ~fetcher c req with
  | Ok bytes -> check_string "second uncached fetch" "fetch-2" bytes
  | Error _ -> Alcotest.fail "second uncached fetch failed");
  check_bool "uncached bytes were not stored" true
    (Store.get ~now:Ptime.epoch store
       Store.{ uri = mxc "mxc://hs.example/no-cache"; format = File }
    = Ok None)

let test_failure_not_cached () =
  run @@ fun () ->
  let calls = ref 0 in
  let fetcher =
    Fetcher.create (fun _ _ ->
        incr calls;
        Error (Media.Media_error (Error.Network_error "no network")))
  in
  let store = Store.memory () in
  let c = client (Fetch_mock.client (fun _ -> assert false)) in
  let req = request "mxc://hs.example/failure" in
  (match Fetcher.get_content ~store ~fetcher c req with
  | Error (Media.Media_error (Error.Network_error _)) -> ()
  | _ -> Alcotest.fail "failure was not returned");
  check_bool "failed fetch not cached" true
    (Store.get ~now:Ptime.epoch store
       Store.{ uri = mxc "mxc://hs.example/failure"; format = File }
    = Ok None);
  check_bool "failure called once" true (!calls = 1)

let test_encrypted_plaintext_and_tamper () =
  run @@ fun () ->
  let encrypted =
    Attachment.encrypt
      ~random:
        (Matrix_client.Random.of_source
           (Eio.Flow.string_source (String.make 4096 'e')))
      "authenticated plaintext"
  in
  let uri = mxc "mxc://hs.example/encrypted" in
  let file =
    Attachment.Metadata.to_event_file ~url:(Media.Mxc.to_string uri)
      encrypted.metadata
  in
  let raw = ref encrypted.ciphertext in
  let fetcher =
    Fetcher.create (fun _ request ->
        match request.Media.source with
        | Media.Encrypted file -> (
            match Attachment.Metadata.of_event_file file with
            | Error error -> Error (Media.Attachment_error error)
            | Ok metadata ->
                Result.map_error
                  (fun error -> Media.Attachment_error error)
                  (Attachment.decrypt_verified metadata !raw))
        | Media.Plain _ -> Ok !raw)
  in
  let store = Store.memory () in
  let c = client (Fetch_mock.client (fun _ -> assert false)) in
  let req = encrypted_request file in
  (match Fetcher.get_content ~store ~fetcher c req with
  | Ok bytes -> check_string "decrypted result" "authenticated plaintext" bytes
  | Error _ -> Alcotest.fail "encrypted fetch failed");
  let key =
    Store.derived_key ~namespace:"encrypted-plaintext"
      ~identity:
        (file.url ^ "\000"
        ^ Attachment.Metadata.to_json_string encrypted.metadata)
      Store.File
  in
  check_bool "cache stores plaintext" true
    (Store.get ~now:Ptime.epoch store key = Ok (Some "authenticated plaintext"));
  raw :=
    String.mapi
      (fun i c -> if i = 0 then Char.chr (Char.code c lxor 1) else c)
      !raw;
  let tamper_store = Store.memory () in
  (match Fetcher.get_content ~store:tamper_store ~fetcher c req with
  | Error (Media.Attachment_error Attachment.Hash_mismatch) -> ()
  | _ -> Alcotest.fail "tampered encrypted fetch accepted");
  check_bool "tampered result not cached" true
    (Store.get ~now:Ptime.epoch tamper_store key = Ok None)

let test_custom_encrypted_plaintext_not_double_decrypted () =
  run @@ fun () ->
  let encrypted =
    Attachment.encrypt
      ~random:
        (Matrix_client.Random.of_source
           (Eio.Flow.string_source (String.make 4096 'p')))
      "custom plaintext"
  in
  let uri = mxc "mxc://hs.example/custom-plaintext" in
  let file =
    Attachment.Metadata.to_event_file ~url:(Media.Mxc.to_string uri)
      encrypted.metadata
  in
  let fetcher = Fetcher.create (fun _ _ -> Ok "custom plaintext") in
  let c = client (Fetch_mock.client (fun _ -> assert false)) in
  match
    Fetcher.get_content ~store:(Store.memory ()) ~fetcher c
      (encrypted_request file)
  with
  | Ok bytes ->
      check_string "custom fetcher final bytes" "custom plaintext" bytes
  | Error _ -> Alcotest.fail "custom plaintext was decrypted twice"

let test_encrypted_cache_uses_moved_queue_ciphertext () =
  run @@ fun () ->
  let encrypted =
    Attachment.encrypt
      ~random:
        (Matrix_client.Random.of_source
           (Eio.Flow.string_source (String.make 4096 'q')))
      "queue plaintext"
  in
  let uri = mxc "mxc://hs.example/collision" in
  let file =
    Attachment.Metadata.to_event_file ~url:(Media.Mxc.to_string uri)
      encrypted.metadata
  in
  let store = Store.memory () in
  let plain_key = Store.{ uri; format = File } in
  ignore (Store.add store plain_key ~data:encrypted.ciphertext);
  let calls = ref 0 in
  let fetcher =
    Fetcher.create (fun _ _ ->
        incr calls;
        Ok encrypted.ciphertext)
  in
  let c = client (Fetch_mock.client (fun _ -> assert false)) in
  let req = encrypted_request file in
  (match Fetcher.get_content ~store ~fetcher c req with
  | Ok bytes -> check_string "moved queue ciphertext" "queue plaintext" bytes
  | _ -> Alcotest.fail "encrypted fetch failed after ordinary ciphertext entry");
  check_bool "encrypted cache key differs" true
    (not
       (Media.Mxc.equal
          (Store.derived_key ~namespace:"encrypted-plaintext"
             ~identity:
               (file.url ^ "\000"
               ^ Attachment.Metadata.to_json_string encrypted.metadata)
             Store.File)
            .uri
          plain_key.uri));
  check_bool "fetcher bypassed by ordinary ciphertext entry" true (!calls = 0)

let test_corrupt_queue_ciphertext_falls_through () =
  run @@ fun () ->
  let encrypted =
    Attachment.encrypt
      ~random:
        (Matrix_client.Random.of_source
           (Eio.Flow.string_source (String.make 4096 'c')))
      "queue plaintext"
  in
  let uri = mxc "mxc://hs.example/corrupt-queue" in
  let file =
    Attachment.Metadata.to_event_file ~url:(Media.Mxc.to_string uri)
      encrypted.metadata
  in
  let store = Store.memory () in
  ignore (Store.add store Store.{ uri; format = File } ~data:"corrupted");
  let calls = ref 0 in
  let fetcher =
    Fetcher.create (fun _ _ ->
        incr calls;
        Ok "network plaintext")
  in
  let c = client (Fetch_mock.client (fun _ -> assert false)) in
  (match Fetcher.get_content ~store ~fetcher c (encrypted_request file) with
  | Ok bytes ->
      check_string "corrupt queue falls through" "network plaintext" bytes
  | Error _ -> Alcotest.fail "corrupt queue did not fall through");
  check_bool "fetcher called after corrupt queue bytes" true (!calls = 1)

let test_sqlite_cache_hit () =
  Eio_main.run @@ fun _env ->
  let path = Filename.temp_file "matrix-media-fetcher" ".sqlite" in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun suffix ->
          let path = path ^ suffix in
          if Sys.file_exists path then Unix.unlink path)
        [ ""; "-shm"; "-wal" ])
    (fun () ->
      let store =
        match Matrix_ui_sqlite.create_media_store path with
        | Ok store -> store
        | Error error ->
            Alcotest.failf "open sqlite media store: %s" (Error.to_string error)
      in
      let calls = ref 0 in
      let fetcher =
        Fetcher.create (fun _ _ ->
            incr calls;
            Ok "sqlite bytes")
      in
      let c = client (Fetch_mock.client (fun _ -> assert false)) in
      let req = request "mxc://hs.example/sqlite" in
      ignore (Fetcher.get_content ~store ~fetcher c req);
      Store.close store;
      let store =
        match Matrix_ui_sqlite.create_media_store path with
        | Ok store -> store
        | Error error ->
            Alcotest.failf "reopen sqlite media store: %s"
              (Error.to_string error)
      in
      (match Fetcher.get_content ~store ~fetcher c req with
      | Ok bytes -> check_string "sqlite cache hit" "sqlite bytes" bytes
      | Error _ -> Alcotest.fail "sqlite cache hit failed");
      check_bool "sqlite fetcher called once" true (!calls = 1);
      Store.close store)

let test_eio_facade_fetcher_cell () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let seen = ref [] in
  let calls = ref 0 in
  let injected =
    Fetcher.create (fun _ request ->
        incr calls;
        seen := request :: !seen;
        Ok "injected")
  in
  let replaced = Fetcher.create (fun _ _ -> Ok "replaced") in
  let transport = Fetch_mock.client (fun _ -> Alcotest.fail "HTTP bypassed") in
  let raw =
    Eio_client.create ~sw ~env
      ~homeserver:(Uriz.of_string_exn "https://hs.example")
      ~fetch:transport ~media_fetcher:injected ()
  in
  let session =
    {
      Client.user_id = Id.User_id.of_string_exn "@alice:example.org";
      access_token = "token";
      device_id = Id.Device_id.of_string_exn "DEVICE";
      refresh_token = None;
    }
  in
  let derived = Eio_client.with_session raw session in
  let token_derived = Eio_client.with_access_token raw "another-token" in
  Alcotest.(check bool)
    "token-derived client shares cell" true
    (Eio_client.media_fetcher token_derived == injected);
  let uri = mxc "mxc://hs.example/facade" in
  let request = Eio_media.{ source = Plain uri; format = File } in
  (match Eio_media.get_content derived request with
  | Ok bytes -> check_string "injected fetcher" "injected" bytes
  | Error _ -> Alcotest.fail "injected fetcher failed");
  Alcotest.(check int) "injected request observed" 1 (List.length !seen);
  (match List.hd !seen with
  | { Media.source = Media.Plain observed; format = Media.File } ->
      check_bool "exact request URI" true (Media.Mxc.equal uri observed)
  | _ -> Alcotest.fail "unexpected request observed");
  Eio_client.set_media_fetcher raw replaced;
  Alcotest.(check bool)
    "derived client shares replacement" true
    (Eio_client.media_fetcher derived == replaced);
  (match Eio_media.get_content derived request with
  | Ok bytes -> check_string "replacement fetcher" "replaced" bytes
  | Error _ -> Alcotest.fail "replacement fetcher failed");
  let cache_store = Store.memory () in
  Eio_client.set_media_fetcher raw injected;
  let cached_request =
    Eio_media.{ source = Plain (mxc "mxc://hs.example/cache"); format = File }
  in
  (match Eio_media.get_content ~store:cache_store derived cached_request with
  | Ok bytes -> check_string "cache miss" "injected" bytes
  | Error _ -> Alcotest.fail "cache miss failed");
  (match Eio_media.get_content ~store:cache_store derived cached_request with
  | Ok bytes -> check_string "cache hit" "injected" bytes
  | Error _ -> Alcotest.fail "cache hit failed");
  check_bool "cache avoids fetcher" true (!calls = 2);
  let local_uri : Media.Mxc.t =
    Matrix_client.Media_store.local_uri ~txn_id:"eio-facade-local"
  in
  ignore
    (Store.add ~protected:true cache_store
       ({ uri = local_uri; format = Store.File } : Store.key)
       ~data:"local");
  let local_request = Eio_media.{ source = Plain local_uri; format = File } in
  (match Eio_media.get_content ~store:cache_store derived local_request with
  | Ok bytes -> check_string "local URI cache" "local" bytes
  | Error _ -> Alcotest.fail "local URI lookup failed");
  check_bool "local URI bypasses fetcher" true (!calls = 2)

let () =
  Alcotest.run "media_fetcher"
    [
      ( "pipeline",
        [
          Alcotest.test_case "cache hit and miss" `Quick test_cache_hit_and_miss;
          Alcotest.test_case "local store only" `Quick test_local_does_not_fetch;
          Alcotest.test_case "cache can be bypassed" `Quick
            test_cache_can_be_bypassed;
          Alcotest.test_case "failure is not cached" `Quick
            test_failure_not_cached;
          Alcotest.test_case "encrypted plaintext and tamper" `Quick
            test_encrypted_plaintext_and_tamper;
          Alcotest.test_case "custom plaintext is not decrypted twice" `Quick
            test_custom_encrypted_plaintext_not_double_decrypted;
          Alcotest.test_case "moved queue ciphertext" `Quick
            test_encrypted_cache_uses_moved_queue_ciphertext;
          Alcotest.test_case "corrupt queue ciphertext falls through" `Quick
            test_corrupt_queue_ciphertext_falls_through;
          Alcotest.test_case "sqlite cache hit" `Quick test_sqlite_cache_hit;
          Alcotest.test_case "Eio facade fetcher cell" `Quick
            test_eio_facade_fetcher_cell;
        ] );
    ]
