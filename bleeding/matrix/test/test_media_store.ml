module M = Matrix_client.Media
module S = Matrix_client.Media_store
module Sqlite = Matrix_ui_sqlite

let check_result name = function
  | Ok value -> value
  | Error error ->
      Alcotest.failf "%s: %s" name (Matrix_client.Error.to_string error)

let at seconds =
  match Ptime.of_float_s seconds with
  | Some value -> value
  | None -> Alcotest.fail "invalid test timestamp"

let uri value =
  match M.Mxc.of_string value with
  | Ok value -> value
  | Error (`Msg error) -> Alcotest.failf "invalid test URI: %s" error

let file uri = { S.uri; format = S.File }

let thumbnail uri =
  {
    S.uri;
    format = S.Thumbnail { width = 32; height = 32; resize = Some `Crop };
  }

let test_key_separation () =
  let uri = uri "mxc://hs.example/media" in
  let store = S.create () in
  check_result "add file" (S.add ~now:(at 0.) store (file uri) ~data:"file");
  check_result "add thumb"
    (S.add ~now:(at 0.) store (thumbnail uri) ~data:"thumb");
  Alcotest.(check (option string))
    "file and thumbnail do not collide" (Some "file")
    (check_result "get file" (S.get ~now:(at 1.) store (file uri)));
  Alcotest.(check (option string))
    "thumbnail retained separately" (Some "thumb")
    (check_result "get thumbnail" (S.get ~now:(at 1.) store (thumbnail uri)))

let test_local_uri () =
  let local = S.local_uri ~txn_id:"mtransaction-1_foo" in
  Alcotest.(check string)
    "local URI"
    "mxc://send-queue.localhost/v2_81a7b549ec3573f3a81727fc5992b47a3e32e0765d788d51748b9fa42daf25ba"
    (M.Mxc.to_string local);
  Alcotest.(check string)
    "arbitrary transaction id"
    "mxc://send-queue.localhost/v2_e2268192dce70b2680fed314e0af7a920b2ecc47dcd8b4f80dd51e15303d709a"
    (M.Mxc.to_string (S.local_uri ~txn_id:"m/valid transaction"));
  Alcotest.(check bool) "local URI detected" true (S.is_local_uri local);
  Alcotest.(check bool)
    "pre-v2 local URI detected" true
    (S.is_local_uri (uri "mxc://send-queue.localhost/mtransaction-1_foo"));
  Alcotest.(check bool)
    "remote URI not local" false
    (S.is_local_uri (uri "mxc://hs.example/mtransaction-1_foo"))

let test_protection_and_expiry () =
  let uri = uri "mxc://hs.example/protected" in
  let policy =
    {
      S.max_file_size = None;
      max_total_size = None;
      expiry = Some (Ptime.Span.of_int_s 5);
      cleanup_frequency = None;
    }
  in
  let store = S.create ~retention:policy () in
  let key = file uri in
  check_result "add protected"
    (S.add ~protected:true ~now:(at 0.) store key ~data:"x");
  check_result "clean protected" (S.clean ~now:(at 5.) store);
  Alcotest.(check (option string))
    "protected survives expiry" (Some "x")
    (check_result "get protected" (S.get ~now:(at 5.) store key));
  check_result "unprotect" (S.unprotect store key);
  check_result "clean expired" (S.clean ~now:(at 10.) store);
  Alcotest.(check (option string))
    "unprotected expires" None
    (check_result "get expired" (S.get ~now:(at 10.) store key))

let test_total_size_lru () =
  let policy =
    {
      S.max_file_size = None;
      max_total_size = Some 5;
      expiry = None;
      cleanup_frequency = None;
    }
  in
  let store = S.create ~retention:policy () in
  let a = file (uri "mxc://hs.example/a")
  and b = file (uri "mxc://hs.example/b")
  and c = file (uri "mxc://hs.example/c") in
  check_result "add a" (S.add ~now:(at 0.) store a ~data:"aa");
  check_result "add b" (S.add ~now:(at 1.) store b ~data:"bb");
  check_result "add c" (S.add ~now:(at 2.) store c ~data:"cc");
  (* The first entry is the oldest; a read moves b to the newest position. *)
  ignore (check_result "touch a" (S.get ~now:(at 3.) store a));
  check_result "clean total" (S.clean ~now:(at 4.) store);
  Alcotest.(check (option string))
    "oldest untouched entry evicted" None
    (check_result "get b" (S.get ~now:(at 4.) store b));
  Alcotest.(check (option string))
    "touched a retained" (Some "aa")
    (check_result "get a" (S.get ~now:(at 4.) store a));
  Alcotest.(check (option string))
    "c retained" (Some "cc")
    (check_result "get c" (S.get ~now:(at 4.) store c))

let test_oversize_and_ignore_retention () =
  let policy =
    {
      S.max_file_size = Some 2;
      max_total_size = Some 2;
      expiry = Some (Ptime.Span.of_int_s 1);
      cleanup_frequency = None;
    }
  in
  let store = S.create ~retention:policy () in
  let key = file (uri "mxc://hs.example/large") in
  check_result "initial add" (S.add ~now:(at 0.) store key ~data:"ok");
  check_result "ordinary oversize add"
    (S.add ~now:(at 0.) store key ~data:"123");
  Alcotest.(check (option string))
    "ordinary oversize preserves replacement" (Some "ok")
    (check_result "get ordinary oversize" (S.get ~now:(at 0.) store key));
  check_result "ignored oversize add"
    (S.add ~ignore_retention:true ~now:(at 0.) store key ~data:"123");
  check_result "clean ignored" (S.clean ~now:(at 10.) store);
  Alcotest.(check (option string))
    "ignored survives limits and expiry" (Some "123")
    (check_result "get ignored" (S.get ~now:(at 10.) store key));
  check_result "clear ignore" (S.set_ignore_retention store key false);
  check_result "clean after clearing ignore" (S.clean ~now:(at 10.) store);
  Alcotest.(check (option string))
    "cleared ignore allows expiry" None
    (check_result "get cleared" (S.get ~now:(at 10.) store key))

let test_changed_size_policy () =
  let key = file (uri "mxc://hs.example/policy-change") in
  let protected_key = file (uri "mxc://hs.example/protected-policy-change") in
  let store = S.create () in
  check_result "add before policy change"
    (S.add ~now:(at 0.) store key ~data:"123");
  check_result "add protected before policy change"
    (S.add ~protected:true ~now:(at 0.) store protected_key ~data:"123");
  check_result "set size policy"
    (S.set_retention store
       {
         S.max_file_size = Some 2;
         max_total_size = None;
         expiry = None;
         cleanup_frequency = None;
       });
  check_result "clean after policy change" (S.clean ~now:(at 1.) store);
  Alcotest.(check (option string))
    "new file limit removes old entry" None
    (check_result "get after policy change" (S.get ~now:(at 1.) store key));
  Alcotest.(check (option string))
    "protected entry survives a smaller file limit" (Some "123")
    (check_result "get protected after policy change"
       (S.get ~now:(at 1.) store protected_key));
  check_result "set total policy"
    (S.set_retention store
       {
         S.max_file_size = None;
         max_total_size = Some 2;
         expiry = None;
         cleanup_frequency = None;
       });
  check_result "total limit is also a per-file limit"
    (S.add ~now:(at 2.) store key ~data:"123");
  Alcotest.(check (option string))
    "oversized entry was not cached" None
    (check_result "get total-limited entry" (S.get ~now:(at 2.) store key))

let test_invalid_policy () =
  Alcotest.check_raises "negative size rejected"
    (Invalid_argument "Media_store: negative max_file_size") (fun () ->
      ignore
        (S.create
           ~retention:
             {
               S.max_file_size = Some (-1);
               max_total_size = None;
               expiry = None;
               cleanup_frequency = None;
             }
           ()))

let test_default_policy () =
  let policy = S.retention (S.create ()) in
  Alcotest.(check (option int))
    "default file limit"
    (Some (20 * 1024 * 1024))
    policy.max_file_size;
  Alcotest.(check (option int))
    "default total limit"
    (Some (400 * 1024 * 1024))
    policy.max_total_size;
  Alcotest.(check bool)
    "default expiry present" true
    (Option.is_some policy.expiry);
  Alcotest.(check bool)
    "default cleanup frequency is daily" true
    (Option.map Ptime.Span.to_float_s policy.cleanup_frequency = Some 86400.);
  let unlimited =
    S.create
      ~retention:
        {
          S.max_file_size = None;
          max_total_size = None;
          expiry = None;
          cleanup_frequency = Some Ptime.Span.zero;
        }
      ()
  in
  check_result "unlimited manual clean" (S.clean ~now:(at 1.) unlimited);
  Alcotest.(check bool)
    "unlimited clean does not advance marker" true
    (S.last_cleanup unlimited = None)

let test_replace_and_remove () =
  let old_uri = uri "mxc://hs.example/old" in
  let new_uri = uri "mxc://hs.example/new" in
  let old_key = thumbnail old_uri and new_key = thumbnail new_uri in
  let store = S.create () in
  check_result "add" (S.add store old_key ~data:"bytes");
  check_result "replace" (S.replace_key store ~from_:old_key ~to_:new_key);
  check_result "idempotent replace"
    (S.replace_key store ~from_:old_key ~to_:new_key);
  Alcotest.(check (option string))
    "replacement moved" (Some "bytes")
    (check_result "get replacement" (S.get ~now:(at 0.) store new_key));
  check_result "remove key" (S.remove store new_key);
  Alcotest.(check (option string))
    "remove clears one key" None
    (check_result "get removed key" (S.get ~now:(at 0.) store new_key));
  check_result "re-add file" (S.add store (file new_uri) ~data:"file");
  check_result "re-add thumbnail" (S.add store new_key ~data:"thumb");
  check_result "remove URI" (S.remove_uri store new_uri);
  Alcotest.(check (option string))
    "remove URI clears all formats" None
    (check_result "get removed thumbnail" (S.get ~now:(at 0.) store new_key));
  Alcotest.(check (option string))
    "remove URI clears file too" None
    (check_result "get removed file" (S.get ~now:(at 0.) store (file new_uri)))

module Fake = struct
  type t = { inner : S.t; mutable adds : int; mutable fail_prune : bool }

  let retention t = S.retention t.inner
  let set_retention t policy = S.set_retention t.inner policy

  let add ?ignore_retention ?protected ?owner ?now t key ~data =
    t.adds <- t.adds + 1;
    S.add ?ignore_retention ?protected ?owner ?now t.inner key ~data

  let get ~now t key = S.get ~now t.inner key
  let protect t key = S.protect t.inner key
  let unprotect t key = S.unprotect t.inner key
  let is_protected t key = S.is_protected t.inner key

  let set_ignore_retention t key value =
    S.set_ignore_retention t.inner key value

  let replace_key t ~from_ ~to_ = S.replace_key t.inner ~from_ ~to_
  let remove t key = S.remove t.inner key
  let remove_uri t uri = S.remove_uri t.inner uri

  let prune_local ~owner ~keep ~older_than t =
    if t.fail_prune then
      Error (Matrix_client.Error.Network_error "prune failed")
    else S.prune_local ~owner ~keep ~older_than t.inner

  let clean ~now t = S.clean ~now t.inner
  let last_cleanup t = S.last_cleanup t.inner
  let set_last_cleanup t value = S.set_last_cleanup t.inner value
  let close t = S.close t.inner
end

let test_backend_wrapper_and_close () =
  let inner = S.memory () in
  let backend = { Fake.inner; adds = 0; fail_prune = false } in
  let store = S.v (module Fake : S.S with type t = Fake.t) backend in
  let key = file (uri "mxc://hs.example/fake") in
  check_result "fake add" (S.add ~now:(at 0.) store key ~data:"fake");
  Alcotest.(check int) "backend dispatched" 1 backend.adds;
  Alcotest.(check (option string))
    "fake get" (Some "fake")
    (check_result "fake get" (S.get ~now:(at 0.) store key));
  S.close store;
  S.close store;
  Alcotest.(check (result (option string) string))
    "closed rejects operations"
    (Error "Request denied by policy: media store is closed")
    (Result.map_error Matrix_client.Error.to_string
       (S.get ~now:(at 0.) store key))

let test_prune_local_error_is_reported () =
  let backend = { Fake.inner = S.memory (); adds = 0; fail_prune = true } in
  let store = S.v (module Fake : S.S with type t = Fake.t) backend in
  match
    S.prune_local ~owner:"@alice:example.org" ~keep:[] ~older_than:(at 10.)
      store
  with
  | Error (Matrix_client.Error.Network_error "prune failed") -> ()
  | Ok () -> Alcotest.fail "failed prune was reported as success"
  | Error error ->
      Alcotest.failf "wrong prune error: %s"
        (Matrix_client.Error.to_string error)

let test_memory_close_idempotence () =
  let store = S.create () in
  S.close store;
  S.close store;
  Alcotest.(check (result unit string))
    "closed add" (Error "Request denied by policy: media store is closed")
    (Result.map_error Matrix_client.Error.to_string
       (S.add store (file (uri "mxc://hs.example/closed")) ~data:"x"))

let test_prune_local_memory () =
  let owner = "@alice:example.org" in
  let local name = file (S.local_uri ~txn_id:name) in
  let stale = local "stale" in
  let protected = local "protected-orphan" in
  let kept = local "kept" in
  let foreign = local "foreign" in
  let remote = file (uri "mxc://hs.example/remote") in
  let store = S.create () in
  check_result "add stale" (S.add ~owner ~now:(at 0.) store stale ~data:"stale");
  check_result "add protected"
    (S.add ~owner ~protected:true ~now:(at 0.) store protected ~data:"safe");
  check_result "add kept"
    (S.add ~owner ~protected:true ~now:(at 0.) store kept ~data:"kept");
  check_result "add foreign"
    (S.add ~owner:"@bob:example.org" ~now:(at 0.) store foreign ~data:"foreign");
  check_result "add remote"
    (S.add ~owner ~now:(at 0.) store remote ~data:"remote");
  check_result "prune local"
    (S.prune_local ~owner ~keep:[ kept ] ~older_than:(at 10.) store);
  Alcotest.(check (option string))
    "stale local is removed" None
    (check_result "stale" (S.get ~now:(at 10.) store stale));
  Alcotest.(check (option string))
    "protected orphan is removed" None
    (check_result "protected" (S.get ~now:(at 10.) store protected));
  Alcotest.(check (option string))
    "active protected keep-set local is retained" (Some "kept")
    (check_result "kept" (S.get ~now:(at 10.) store kept));
  Alcotest.(check (option string))
    "foreign owner is untouched" (Some "foreign")
    (check_result "foreign" (S.get ~now:(at 10.) store foreign));
  Alcotest.(check (option string))
    "remote media is untouched" (Some "remote")
    (check_result "remote" (S.get ~now:(at 10.) store remote))

let with_sqlite f =
  let path = Filename.temp_file "matrix-media-store-" ".sqlite" in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () -> f path)

let open_sqlite path =
  match Sqlite.create_media_store path with
  | Ok store -> store
  | Error error ->
      Alcotest.failf "open media store: %s"
        (Matrix_client.Error.to_string error)

let test_sqlite_open_failure_is_result () =
  let marker = Filename.temp_file "matrix-sqlite-missing-" ".parent" in
  Sys.remove marker;
  let path = Filename.concat marker "store.sqlite" in
  let media_raised =
    try
      match Sqlite.create_media_store path with
      | Ok store ->
          Matrix_client.Media_store.close store;
          false
      | Error _ -> false
    with _ -> true
  in
  Alcotest.(check bool) "media open failure is returned" false media_raised;
  let events_raised =
    try
      match Sqlite.create path with
      | Ok store ->
          Matrix_ui.Event_store.close store;
          false
      | Error _ -> false
    with _ -> true
  in
  Alcotest.(check bool)
    "event-store open failure is returned" false events_raised

let test_sqlite_malformed_schema_is_result () =
  with_sqlite (fun path ->
      let oc = open_out_bin path in
      output_string oc "not a SQLite database";
      close_out oc;
      let media_raised =
        try
          match Sqlite.create_media_store path with
          | Ok store ->
              Matrix_client.Media_store.close store;
              false
          | Error _ -> false
        with _ -> true
      in
      Alcotest.(check bool)
        "malformed media schema is returned" false media_raised;
      let events_raised =
        try
          match Sqlite.create path with
          | Ok store ->
              Matrix_ui.Event_store.close store;
              false
          | Error _ -> false
        with _ -> true
      in
      Alcotest.(check bool)
        "malformed event schema is returned" false events_raised)

let test_sqlite_restart_and_flags () =
  with_sqlite (fun path ->
      let store = open_sqlite path in
      let key = file (uri "mxc://hs.example/restart") in
      check_result "sqlite add"
        (S.add ~protected:true ~ignore_retention:true ~now:(at 10.) store key
           ~data:"bytes");
      S.close store;
      let reopened = open_sqlite path in
      Alcotest.(check (option string))
        "bytes survive restart" (Some "bytes")
        (check_result "get" (S.get ~now:(at 12.) reopened key));
      Alcotest.(check bool)
        "protected survives restart" true
        (S.is_protected reopened key = Ok true);
      S.close reopened)

let test_sqlite_prune_local_restart () =
  with_sqlite (fun path ->
      let owner = "@alice:example.org" in
      let local name = file (S.local_uri ~txn_id:name) in
      let stale = local "sqlite-stale" in
      let protected = local "sqlite-protected-orphan" in
      let kept = local "sqlite-kept" in
      let foreign = local "sqlite-foreign" in
      let remote = file (uri "mxc://hs.example/sqlite-remote") in
      let store = open_sqlite path in
      check_result "sqlite add stale"
        (S.add ~owner ~now:(at 0.) store stale ~data:"stale");
      check_result "sqlite add protected"
        (S.add ~owner ~protected:true ~now:(at 0.) store protected
           ~data:"protected");
      check_result "sqlite add kept"
        (S.add ~owner ~protected:true ~now:(at 0.) store kept ~data:"kept");
      check_result "sqlite add foreign"
        (S.add ~owner:"@bob:example.org" ~now:(at 0.) store foreign
           ~data:"foreign");
      check_result "sqlite add remote"
        (S.add ~owner ~now:(at 0.) store remote ~data:"remote");
      S.close store;
      let reopened = open_sqlite path in
      check_result "sqlite prune local"
        (S.prune_local ~owner ~keep:[ kept ] ~older_than:(at 10.) reopened);
      Alcotest.(check (option string))
        "sqlite stale local is removed" None
        (check_result "sqlite stale" (S.get ~now:(at 10.) reopened stale));
      Alcotest.(check (option string))
        "sqlite protected orphan is removed" None
        (check_result "sqlite protected"
           (S.get ~now:(at 10.) reopened protected));
      Alcotest.(check (option string))
        "sqlite active protected keep-set is retained" (Some "kept")
        (check_result "sqlite kept" (S.get ~now:(at 10.) reopened kept));
      Alcotest.(check (option string))
        "sqlite foreign owner is untouched" (Some "foreign")
        (check_result "sqlite foreign" (S.get ~now:(at 10.) reopened foreign));
      Alcotest.(check (option string))
        "sqlite remote is untouched" (Some "remote")
        (check_result "sqlite remote" (S.get ~now:(at 10.) reopened remote));
      S.close reopened)

let test_sqlite_explicit_none_policy () =
  with_sqlite (fun path ->
      let policy =
        {
          S.max_file_size = None;
          max_total_size = None;
          expiry = None;
          cleanup_frequency = None;
        }
      in
      let store =
        match Sqlite.create_media_store ~retention:policy path with
        | Ok store -> store
        | Error error ->
            Alcotest.failf "open media store: %s"
              (Matrix_client.Error.to_string error)
      in
      let retained = S.retention store in
      Alcotest.(check (option int))
        "unlimited file size before reopen" None retained.max_file_size;
      Alcotest.(check (option int))
        "unlimited total size before reopen" None retained.max_total_size;
      Alcotest.(check bool)
        "unlimited expiry before reopen" true (retained.expiry = None);
      Alcotest.(check bool)
        "unlimited cleanup frequency before reopen" true
        (retained.cleanup_frequency = None);
      S.close store;
      let reopened = open_sqlite path in
      let retained = S.retention reopened in
      Alcotest.(check (option int))
        "unlimited file size survives reopen" None retained.max_file_size;
      Alcotest.(check (option int))
        "unlimited total size survives reopen" None retained.max_total_size;
      Alcotest.(check bool)
        "unlimited expiry survives reopen" true (retained.expiry = None);
      Alcotest.(check bool)
        "unlimited cleanup frequency survives reopen" true
        (retained.cleanup_frequency = None);
      S.close reopened)

let test_sqlite_expiry_only_cleanup_restart () =
  with_sqlite (fun path ->
      let policy =
        {
          S.max_file_size = None;
          max_total_size = None;
          expiry = Some (Ptime.Span.of_int_s 5);
          cleanup_frequency = None;
        }
      in
      let store =
        match Sqlite.create_media_store ~retention:policy path with
        | Ok store -> store
        | Error error ->
            Alcotest.failf "open media store: %s"
              (Matrix_client.Error.to_string error)
      in
      (* Policy setup can run a wall-clock opportunistic cleanup.  Reset the
         marker so this test exercises the deterministic manual transaction. *)
      check_result "reset cleanup marker" (S.set_last_cleanup store None);
      let key = file (uri "mxc://hs.example/expiry-only") in
      check_result "add expiry-only entry"
        (S.add ~now:(at 0.) store key ~data:"bytes");
      check_result "expiry-only clean" (S.clean ~now:(at 5.) store);
      Alcotest.(check (option string))
        "expiry-only entry removed" None
        (check_result "get removed entry" (S.get ~now:(at 5.) store key));
      Alcotest.(check bool)
        "expiry-only marker recorded" true
        (Option.map Ptime.to_float_s (S.last_cleanup store) = Some 5.);
      S.close store;
      let reopened = open_sqlite path in
      Alcotest.(check bool)
        "expiry-only marker survives restart" true
        (Option.map Ptime.to_float_s (S.last_cleanup reopened) = Some 5.);
      Alcotest.(check (option string))
        "expiry-only removal survives restart" None
        (check_result "get removed after restart"
           (S.get ~now:(at 5.) reopened key));
      S.close reopened)

let test_sqlite_policy_cleanup_and_replace () =
  with_sqlite (fun path ->
      let policy =
        {
          S.max_file_size = None;
          max_total_size = Some 3;
          expiry = Some (Ptime.Span.of_int_s 5);
          cleanup_frequency = None;
        }
      in
      let store =
        match Sqlite.create_media_store ~retention:policy path with
        | Ok store -> store
        | Error error ->
            Alcotest.failf "open media store: %s"
              (Matrix_client.Error.to_string error)
      in
      let a = file (uri "mxc://hs.example/a")
      and b = file (uri "mxc://hs.example/b") in
      check_result "add a" (S.add ~now:(at 0.) store a ~data:"aa");
      check_result "add b" (S.add ~now:(at 1.) store b ~data:"bb");
      check_result "oversize replacement"
        (S.add ~now:(at 1.25) store a ~data:"long");
      Alcotest.(check (option string))
        "oversize replacement preserves bytes" (Some "aa")
        (check_result "get preserved a" (S.get ~now:(at 1.25) store a));
      ignore (check_result "touch a" (S.get ~now:(at 1.5) store a));
      check_result "clean lru" (S.clean ~now:(at 2.) store);
      Alcotest.(check bool)
        "last cleanup is recorded" true
        (Option.map Ptime.to_float_s (S.last_cleanup store) = Some 2.);
      Alcotest.(check (option string))
        "oldest evicted" None
        (check_result "get b" (S.get ~now:(at 2.) store b));
      S.close store;
      let reopened = open_sqlite path in
      Alcotest.(check (option int))
        "policy persists" (Some 3) (S.retention reopened).max_total_size;
      Alcotest.(check bool)
        "last cleanup persists" true
        (Option.map Ptime.to_float_s (S.last_cleanup reopened) = Some 2.);
      Alcotest.(check (option string))
        "source survives restart" (Some "aa")
        (check_result "get source" (S.get ~now:(at 2.5) reopened a));
      let remote = file (uri "mxc://hs.example/remote") in
      check_result "replace" (S.replace_key reopened ~from_:a ~to_:remote);
      check_result "replace idempotent"
        (S.replace_key reopened ~from_:a ~to_:remote);
      Alcotest.(check (option string))
        "remote bytes survive replacement" (Some "aa")
        (check_result "get remote" (S.get ~now:(at 3.) reopened remote));
      Alcotest.(check (option string))
        "local key is absent after replacement" None
        (check_result "get local" (S.get ~now:(at 3.) reopened a));
      S.close reopened)

let test_sqlite_auto_cleanup_cadence () =
  with_sqlite (fun path ->
      let policy =
        {
          S.max_file_size = None;
          max_total_size = None;
          expiry = Some (Ptime.Span.of_int_s 5);
          cleanup_frequency = Some (Ptime.Span.of_int_s 10);
        }
      in
      let store =
        match Sqlite.create_media_store ~retention:policy path with
        | Ok store -> store
        | Error error ->
            Alcotest.failf "open media store: %s"
              (Matrix_client.Error.to_string error)
      in
      (* The policy setter may perform an immediate wall-clock cleanup; reset
         the seam so this test can use deterministic timestamps. *)
      check_result "reset cleanup marker" (S.set_last_cleanup store None);
      let old = file (uri "mxc://hs.example/old") in
      check_result "add old" (S.add ~now:(at 0.) store old ~data:"old");
      check_result "rollback operation"
        (S.add ~now:(at (-1.)) store
           (file (uri "mxc://hs.example/rollback"))
           ~data:"rollback");
      Alcotest.(check (option string))
        "rollback does not clean" (Some "old")
        (check_result "get old" (S.get ~now:(at 1.) store old));
      check_result "cadence operation"
        (S.add ~now:(at 10.) store
           (file (uri "mxc://hs.example/new"))
           ~data:"new");
      Alcotest.(check bool)
        "cadence marker advances" true
        (Option.map Ptime.to_float_s (S.last_cleanup store) = Some 10.);
      Alcotest.(check (option string))
        "cadence cleans expiry" None
        (check_result "get expired" (S.get ~now:(at 10.) store old));
      S.close store)

let test_sqlite_coexists_with_event_store () =
  with_sqlite (fun path ->
      let room_id =
        match Matrix_proto.Id.Room_id.of_string "!coexist:example.org" with
        | Ok room_id -> room_id
        | Error (`Msg error) -> Alcotest.failf "room id: %s" error
      in
      let events =
        match Sqlite.create path with
        | Ok store -> store
        | Error error ->
            Alcotest.failf "open event store: %s"
              (Matrix_ui.Event_store.Error.to_string error)
      in
      let room =
        {
          Matrix_ui.Event_store.Internal.chunks = [];
          next_chunk_id = 0;
          external_events = [];
        }
      in
      (match Matrix_ui.Event_store.save_room events room_id room with
      | Ok () -> ()
      | Error error ->
          Alcotest.failf "save event store: %s"
            (Matrix_ui.Event_store.Error.to_string error));
      let media = open_sqlite path in
      let key = file (uri "mxc://hs.example/coexist") in
      check_result "add alongside event store" (S.add media key ~data:"bytes");
      S.close media;
      match Matrix_ui.Event_store.load_room events room_id with
      | Ok (Some _) -> Matrix_ui.Event_store.close events
      | Ok None -> Alcotest.fail "media schema removed event room"
      | Error error ->
          Alcotest.failf "load event store: %s"
            (Matrix_ui.Event_store.Error.to_string error))

let () =
  Eio_main.run @@ fun _ ->
  Alcotest.run "media_store"
    [
      ( "cache",
        [
          Alcotest.test_case "key separation" `Quick test_key_separation;
          Alcotest.test_case "local URI" `Quick test_local_uri;
          Alcotest.test_case "protection and expiry" `Quick
            test_protection_and_expiry;
          Alcotest.test_case "total size LRU" `Quick test_total_size_lru;
          Alcotest.test_case "oversize and ignore" `Quick
            test_oversize_and_ignore_retention;
          Alcotest.test_case "changed size policy" `Quick
            test_changed_size_policy;
          Alcotest.test_case "invalid policy" `Quick test_invalid_policy;
          Alcotest.test_case "default policy" `Quick test_default_policy;
          Alcotest.test_case "replace and remove" `Quick test_replace_and_remove;
          Alcotest.test_case "backend wrapper and close" `Quick
            test_backend_wrapper_and_close;
          Alcotest.test_case "memory close idempotence" `Quick
            test_memory_close_idempotence;
          Alcotest.test_case "prune local memory" `Quick test_prune_local_memory;
          Alcotest.test_case "prune local error" `Quick
            test_prune_local_error_is_reported;
          Alcotest.test_case "sqlite restart and flags" `Quick
            test_sqlite_restart_and_flags;
          Alcotest.test_case "sqlite open failure is a result" `Quick
            test_sqlite_open_failure_is_result;
          Alcotest.test_case "sqlite malformed schema is a result" `Quick
            test_sqlite_malformed_schema_is_result;
          Alcotest.test_case "sqlite prune local restart" `Quick
            test_sqlite_prune_local_restart;
          Alcotest.test_case "sqlite explicit none policy" `Quick
            test_sqlite_explicit_none_policy;
          Alcotest.test_case "sqlite expiry-only cleanup restart" `Quick
            test_sqlite_expiry_only_cleanup_restart;
          Alcotest.test_case "sqlite policy cleanup and replace" `Quick
            test_sqlite_policy_cleanup_and_replace;
          Alcotest.test_case "sqlite automatic cleanup cadence" `Quick
            test_sqlite_auto_cleanup_cadence;
          Alcotest.test_case "sqlite coexists with event store" `Quick
            test_sqlite_coexists_with_event_store;
        ] );
    ]
