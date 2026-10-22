(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Manifest

let decode_json codec path =
  match Jsont_bytesrw.decode_string codec (read path) with
  | Ok s -> s
  | Error _ -> failwith "Malformed session. Log in again with the owning CLI"

let secret prompt =
  Printf.printf "%s: %!" prompt;
  let old = Unix.tcgetattr Unix.stdin in
  Fun.protect ~finally:(fun () ->
    Unix.tcsetattr Unix.stdin Unix.TCSANOW old; print_newline ())
    (fun () ->
      Unix.tcsetattr Unix.stdin Unix.TCSANOW { old with c_echo = false };
      let value = read_line () |> String.trim in
      if value = "" then failwith "Empty credential";
      value)

let check ~sw ~env s =
  let path = session_path s in
  match s.kind with
  | "standard-site" ->
      let session = decode_json Xrpc_auth.Session.jsont path in
      if session.handle <> s.account || session.pds <> s.endpoint then
        failwith "Saved StandardSite identity does not match manifest";
      let client = Xrpc_auth.Client.create ~sw ~env ~app_name:s.kind
          ~profile:s.profile ~pds:s.endpoint () in
      Xrpc_auth.Client.resume client ~session;
      let handle = Xrpc.Client.query (Xrpc_auth.Client.get_client client)
          ~nsid:"com.atproto.server.getSession" ~params:[]
          ~decoder:(json_string_field "handle") in
      if handle <> s.account then failwith "StandardSite identity mismatch"
  | "apub" ->
      let session = decode_json Apub_auth_session.jsont path in
      (match session.oauth_instance, session.oauth_access_token with
       | Some instance, Some access_token ->
           if "https://" ^ instance <> s.endpoint then
             failwith "Saved ActivityPub instance does not match manifest";
           let fetch = Fetch_curl.std ~sw env in
           (match Apub_mastodon_oauth.verify_credentials fetch
                    ~instance ~access_token with
            | Ok account ->
                if account.username ^ "@" ^ instance <> s.account then
                  failwith "ActivityPub identity mismatch"
            | Error _ -> failwith "ActivityPub verification failed")
       | _ -> failwith
           "Signature identity requires explicit apub auth setup/key import")
  | "karakeep" ->
      let session = match Karakeep_auth.Session.load env#fs
          ~profile:s.profile () with
        | Some s -> s | None -> failwith "Malformed Karakeep credentials" in
      if Karakeep_auth.Session.base_url session <> s.endpoint then
        failwith "Saved Karakeep endpoint does not match manifest";
      let client = Karakeep_auth.Client.resume ~sw ~env
          ~profile:s.profile ~session () in
      ignore (Karakeep.Client.get_users_me
        (Karakeep_auth.Client.client client) ())
  | _ -> assert false

let login ~sw ~env s =
  match s.kind with
  | "standard-site" ->
      let password = secret "StandardSite app password" in
      let client = Xrpc_auth.Client.create ~sw ~env ~app_name:s.kind
          ~profile:s.profile ~pds:s.endpoint () in
      Xrpc_auth.Client.login client ~identifier:s.account ~password
  | "apub" ->
      Process.run ["apub"; "auth"; "login"; "--profile"; s.profile; s.account]
  | "karakeep" ->
      let api_key = secret "Karakeep API key" in
      ignore (Karakeep_auth.Client.login ~sw ~env ~profile:s.profile
        ~base_url:s.endpoint ~api_key ())
  | _ -> assert false

let select ~env s =
  match s.kind with
  | "standard-site" ->
      Xrpc_auth.Session.set_current_profile env#fs ~app_name:s.kind s.profile
  | "apub" ->
      Apub_auth_session.set_current_profile env#fs ~app_name:s.kind s.profile
  | "karakeep" ->
      Karakeep_auth.Session.set_current_profile env#fs s.profile
  | _ -> assert false

let ensure ~sw ~env ~non_interactive s =
  let exists = Sys.file_exists (session_path s) in
  let valid = exists && (try check ~sw ~env s; true with _ -> false) in
  if valid then select ~env s else begin
    if non_interactive || not (Unix.isatty Unix.stdin) then
      failwith "Login or verification required. Rerun interactively";
    Printf.printf "%s (%s at %s): %s. Log in now? [y/N] %!"
      s.kind s.account s.endpoint
      (if exists then "verification failed (credentials or network)"
       else "no saved credentials");
    match String.lowercase_ascii (String.trim (read_line ())) with
    | "y" | "yes" ->
        login ~sw ~env s;
        check ~sw ~env s;
        select ~env s
    | _ -> failwith "Login skipped"
  end
