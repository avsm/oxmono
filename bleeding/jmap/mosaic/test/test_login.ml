(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Login = Jmap_mosaic.Login
module Io = Jmap_mosaic.Io
module Model = Jmap_mosaic.Model

let permissions path = (Unix.stat path).st_perm land 0o777

let test_profiles () =
  let config = Filename.temp_dir "jmap-mosaic-" "-profiles" in
  Unix.putenv "XDG_CONFIG_HOME" config;
  Eio_main.run @@ fun env ->
  let profiles_dir = Login.profiles_path env in
  let profile_file = Filename.concat profiles_dir "work" in
  let cleanup () =
    Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / config)
  in
  Fun.protect ~finally:cleanup @@ fun () ->
  let login =
    Model.
      {
        blank with
        profile = "work";
        url = "https://example.test/.well-known/jmap";
        secret = "first-token";
      }
  in
  Login.write_profile env login;
  Alcotest.(check int)
    "private profiles directory" 0o700 (permissions profiles_dir);
  Alcotest.(check int) "private profile" 0o600 (permissions profile_file);
  let saved = Login.profiles env in
  Alcotest.(check int) "one profile" 1 (List.length saved);
  let saved = List.hd saved in
  Alcotest.(check string) "name" "work" saved.profile;
  Alcotest.(check string) "url" login.url saved.url;
  Alcotest.(check string) "secret" "first-token" saved.secret;
  Login.write_profile env { login with secret = "replacement-token" };
  let replaced = Option.get (Login.read_profile env "work") in
  Alcotest.(check string) "replacement" "replacement-token" replaced.secret;
  Alcotest.(check (list string))
    "no temporary profile remains" [ "work" ]
    (Array.to_list (Sys.readdir profiles_dir));
  Login.write_profile env { login with secret = String.make (64 * 1024) 'x' };
  let after_oversized = Option.get (Login.read_profile env "work") in
  Alcotest.(check string)
    "oversized replacement leaves the old profile" "replacement-token"
    after_oversized.secret;
  Unix.chmod profile_file 0o644;
  Alcotest.(check bool)
    "public profile is rejected" true
    (Option.is_none (Login.read_profile env "work"));
  Alcotest.(check int)
    "public profile is not listed" 0
    (List.length (Login.profiles env));
  Unix.chmod profile_file 0o600;
  Login.write_profile env { login with profile = "../escape" };
  Alcotest.(check int)
    "unsafe name ignored" 1
    (List.length (Login.profiles env))

let test_public_shared_store_is_ignored () =
  let config = Filename.temp_dir "jmap-mosaic-" "-public-store" in
  let old_config = Sys.getenv_opt "XDG_CONFIG_HOME" in
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "XDG_CONFIG_HOME" (Option.value ~default:"" old_config))
    (fun () ->
      Unix.putenv "XDG_CONFIG_HOME" config;
      Eio_main.run @@ fun env ->
      let directory =
        List.fold_left Filename.concat config [ "jmap"; "profiles" ]
      in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
        Eio.Path.(Eio.Stdenv.fs env / directory);
      Unix.chmod directory 0o755;
      Fun.protect
        ~finally:(fun () ->
          Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / config))
        (fun () ->
          let login =
            Model.
              {
                blank with
                profile = "work";
                url = "https://example.test/.well-known/jmap";
                secret = "TOKEN";
              }
          in
          Login.write_profile env login;
          Alcotest.(check int) "mode unchanged" 0o755 (permissions directory);
          Alcotest.(check bool)
            "profile not written" false
            (Sys.file_exists (Filename.concat directory "work"));
          Alcotest.(check int)
            "public store not listed" 0
            (List.length (Login.profiles env))))

let test_dispatch_exception () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let io = Io.create ~sw env in
  let calls = ref 0 in
  match
    Io.perform io Model.Load_mailboxes (fun _ ->
        incr calls;
        failwith "dispatch failed")
  with
  | exception Failure message when String.equal message "dispatch failed" ->
      Alcotest.(check int) "called once" 1 !calls
  | exception exn ->
      Alcotest.failf "wrong exception: %s" (Printexc.to_string exn)
  | () -> Alcotest.fail "the callback exception did not propagate"

let test_invalid_credential () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let io = Io.create ~sw env in
  let answer = ref None in
  let credentials =
    Model.
      {
        url = "https://example.test/.well-known/jmap";
        scheme = Bearer;
        user = "";
        secret = "two words";
      }
  in
  Io.perform io (Model.Connect credentials) (fun message ->
      answer := Some message);
  match !answer with
  | Some (Model.Login_failed message) ->
      Alcotest.(check bool) "validation reason" true (String.length message > 0)
  | Some _ -> Alcotest.fail "an invalid login produced the wrong message"
  | None -> Alcotest.fail "an invalid login produced no message"

let test_network_timeout () =
  Alcotest.(check (float 0.))
    "shared CLI timeout" Jmap_eio.Cli.default_timeout Io.default_timeout;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let slow req =
    Eio.Time.sleep clock 60.;
    Fetch_mock.respond "too late" req
  in
  let transport = Jmap_eio.Transport.of_fetch ~clock (Fetch_mock.client slow) in
  let io = Io.create ~sw ~timeout:0.01 ~transport env in
  let answer = ref None in
  let credentials =
    Model.
      {
        url = "https://example.test/.well-known/jmap";
        scheme = Bearer;
        user = "";
        secret = "TOKEN";
      }
  in
  Io.perform io (Model.Connect credentials) (fun message ->
      answer := Some message);
  match !answer with
  | Some (Model.Login_failed message) ->
      Alcotest.(check bool)
        ("timeout is reported: " ^ message)
        true
        (String.starts_with ~prefix:"Connection error: no response within"
           message)
  | Some _ -> Alcotest.fail "a timed-out login produced the wrong message"
  | None -> Alcotest.fail "a timed-out login produced no message"

let () =
  Alcotest.run "jmap-mosaic login"
    [
      ( "profiles",
        [
          Alcotest.test_case "round trip" `Quick test_profiles;
          Alcotest.test_case "public shared store is ignored" `Quick
            test_public_shared_store_is_ignored;
        ] );
      ( "io",
        [
          Alcotest.test_case "dispatch exception" `Quick test_dispatch_exception;
          Alcotest.test_case "invalid credential" `Quick test_invalid_credential;
          Alcotest.test_case "network timeout" `Quick test_network_timeout;
        ] );
    ]
