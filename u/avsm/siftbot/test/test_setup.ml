module S = Siftbot_setup
module P = Jmap_eio.Profile

let check label condition = if not condition then failwith label
let ok = function Ok x -> x | Error _ -> failwith "shared store error"
let saved = function S.Saved p -> p | _ -> failwith "save failed"
let rejected = function S.Error _ -> () | _ -> failwith "expected rejection"

let contains text needle =
  let n = String.length needle in
  let rec loop i =
    i + n <= String.length text && (String.sub text i n = needle || loop (i + 1))
  in
  loop 0

(* The environment is isolated before Eio or any worker domain starts. *)
let () =
  Xdge_testenv.with_xdg ~prefix:"siftbot-setup-" @@ fun root ->
  let config = Filename.concat root "config" in
  Xdge_testenv.with_bindings
    [
      ("SEKRIT_STORE_DIR", Some (Filename.concat root "vault"));
      ("SEKRIT_KEY_FILE", Some (Filename.concat root "keys/master.key"));
    ]
    (fun () ->
      Eio_main.run (fun env ->
          let call = Siftbot.Setup.handle env in
          let p : S.profile =
            {
              name = "work";
              jmap_profile = "mail-work";
              url = "https://example.test/session";
              account = "account-1";
              chat_model = "provider/model";
              has_token = false;
            }
          in
          let token = "secret-test-token" in
          check "empty profiles" (call S.List = Profiles []);
          let p = saved (call (Save (p, token, true))) in
          check "token indicator" p.has_token;
          let store = ok (P.xdg_store env) in
          let credential () = P.credential (ok (P.load store "mail-work")) in
          check "stored token" (credential () = Bearer token);
          let path = Filename.concat config "siftbot/work/profile.json" in
          check "private local settings"
            ((Unix.stat path).st_perm land 0o077 = 0);
          let ic = open_in_bin path in
          let raw = really_input_string ic (in_channel_length ic) in
          close_in ic;
          check "local config has no token" (not (contains raw token));
          let local = Siftbot.Profile.load "work" in
          Siftbot.Profile.save
            {
              local with
              body_limit = 1234;
              max_steps = 42;
              chat_base_url = Some "http://sequoia:8000/v1";
              sortal_roots = [ "/example/sortal" ];
            };
          ignore
            (saved (call (Save ({ p with account = "account-2" }, "", false))));
          check "blank retains token" (credential () = Bearer token);
          let local = Siftbot.Profile.load "work" in
          check "advanced settings preserved"
            (local.body_limit = 1234 && local.max_steps = 42
            && local.sortal_roots = [ "/example/sortal" ]
            && local.chat_base_url = Some "http://sequoia:8000/v1"
            && local.account = Some "account-2");
          rejected
            (call
               (Save ({ p with url = "https://other.test/session" }, "", false)));
          check "changed URL cannot reuse token"
            (P.session_url (ok (P.load store "mail-work")) = p.url);
          rejected (call (Save (p, "replacement", true)));
          rejected
            (call
               (Save
                  ({ p with url = "http://example.test/session" }, token, false)));
          rejected (call (Save ({ p with name = "../outside" }, token, true)));
          rejected
            (call (Save ({ p with url = p.url ^ "?key=value" }, token, false)));
          rejected
            (call (Save ({ p with url = p.url ^ "#fragment" }, token, false)));
          let response = call S.List |> S.response in
          check "list is redacted" (not (contains response token));
          ignore (saved (call (Save (p, "rotated-token", false))));
          check "explicit rotation" (credential () = Bearer "rotated-token");
          let second = { p with name = "personal" } in
          ignore (saved (call (Save (second, "", true))));
          check "two selectable profiles"
            (match call S.List with
            | Profiles ps -> List.length ps = 2
            | _ -> false);
          let raw = S.request (Save (p, token, false)) in
          check "setup roundtrip"
            (S.parse_request raw = Ok (Save (p, token, false)));
          check "review rejects setup channel"
            (Result.is_error (Siftbot_protocol.parse_request raw));
          check "invalid parse does not echo secret"
            (S.parse_request ("broken " ^ token) = Error "Invalid setup request");
          check "bounded parse"
            (Result.is_error (S.parse_request (String.make 65537 'x')));
          let reference = P.Bearer_ref "sekrit:sift/mail" in
          ok
            (P.save store
               (ok (P.v ~name:p.jmap_profile ~session_url:p.url reference)));
          let preserved = saved (call (Save (p, "", false))) in
          check "named reference shown without resolving it" preserved.has_token;
          check "blank retains unresolved reference" (credential () = reference);
          rejected
            (call
               (Save ({ p with url = "https://other.test/session" }, "", false)));
          Eio.Switch.run (fun sw ->
              let fs = Eio.Stdenv.fs env in
              let root = Eio.Path.(fs / Sekrit.Paths.root ()) in
              let key_file = Eio.Path.(fs / Sekrit.Paths.key_file ()) in
              Sekrit_eio.Store.init ~root ~key_file;
              let secrets = Sekrit_eio.Store.open_ ~sw ~root ~key_file in
              Sekrit_eio.Store.put secrets ~name:"sift/mail" ~replace:false
                (Sekrit.Secret.of_string "vault-only-token");
              let calls = ref 0 in
              let fetch =
                Fetch_mock.client (fun r ->
                    incr calls;
                    check "reference resolves for session authentication"
                      (Http.Header.get r.headers "authorization"
                      = Some "Bearer vault-only-token");
                    check "discovery only" (r.meth = `GET);
                    let headers =
                      Http.Header.of_list
                        [ ("content-type", "application/json") ]
                    in
                    Fetch_mock.respond ~headers
                      {|{"capabilities":{"urn:ietf:params:jmap:core":{"maxSizeUpload":1000,"maxConcurrentUpload":1,"maxSizeRequest":100000,"maxConcurrentRequests":1,"maxCallsInRequest":4,"maxObjectsInGet":10,"maxObjectsInSet":10,"collationAlgorithms":[]},"urn:ietf:params:jmap:mail":{}},"accounts":{"account-1":{"name":"Mail","isPersonal":true,"isReadOnly":true,"accountCapabilities":{"urn:ietf:params:jmap:mail":{"maxMailboxesPerEmail":null,"maxMailboxDepth":null,"maxSizeMailboxName":256,"maxSizeAttachmentsPerEmail":50000000,"emailQuerySortOptions":["receivedAt"],"mayCreateTopLevelMailbox":false}}}},"primaryAccounts":{"urn:ietf:params:jmap:mail":"account-1"},"username":"owner","apiUrl":"https://example.test/api","downloadUrl":"https://example.test/download/{accountId}/{blobId}/{name}?type={type}","uploadUrl":"https://example.test/upload/{accountId}","eventSourceUrl":"https://example.test/events?types={types}&close={closeafter}&ping={ping}","state":"session1"}|}
                      r)
              in
              let remote =
                Siftbot.Profile.connect ~sw ~fetch env
                  (Siftbot.Profile.load "work")
              in
              check "named secret opens restricted account"
                (remote.account = "account-1" && !calls = 1);
              check "connection preserves reference" (credential () = reference))));
  print_endline
    "Profile setup: storage, preservation, validation and redaction passed"
