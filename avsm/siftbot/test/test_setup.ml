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

let rec remove path =
  if Sys.is_directory path then (
    Array.iter (fun n -> remove (Filename.concat path n)) (Sys.readdir path);
    Unix.rmdir path)
  else Unix.unlink path

let () =
  let root = Filename.temp_file "siftbot-setup-" "" in
  Unix.unlink root;
  Unix.mkdir root 0o700;
  Unix.putenv "XDG_CONFIG_HOME" root;
  Fun.protect
    ~finally:(fun () -> remove root)
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
          let path = Filename.concat root "siftbot/work/profile.json" in
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
              sortal_roots = [ "/example/sortal" ];
            };
          ignore
            (saved (call (Save ({ p with account = "account-2" }, "", false))));
          check "blank retains token" (credential () = Bearer token);
          let local = Siftbot.Profile.load "work" in
          check "advanced settings preserved"
            (local.body_limit = 1234 && local.max_steps = 42
            && local.sortal_roots = [ "/example/sortal" ]
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
            (Result.is_error (S.parse_request (String.make 65537 'x')))));
  print_endline
    "Profile setup: storage, preservation, validation and redaction passed"
