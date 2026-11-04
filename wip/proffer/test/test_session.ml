(* SPDX-License-Identifier: ISC *)
open Proffer
module S = Proffer_session
module M = Httpz.Method

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let has s sub =
  let n = String.length sub in
  let rec at i =
    i + n <= String.length s && (String.sub s i n = sub || at (i + 1))
  in
  at 0

(* A request is local, so each use builds one and calls [f] on it without a
   tail call. *)
let with_req ?(meth = M.Get) ?(headers = []) ?body target f =
  let local_ r = Req.v ~meth ~target ~headers:(Headers.of_list headers) ?body () in
  f r [@nontail]

(* The cookie value a Set-Cookie field gives. *)
let value_of (f : Headers.field) =
  let v = f.value in
  let start = String.index v '=' + 1 in
  let stop = try String.index v ';' with Not_found -> String.length v in
  String.sub v start (stop - start)

let () =
  Eio_main.run @@ fun env ->
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 1_800_000_000.;
  let keys = ref [] and puts = ref 0 in
  let memory = S.Store.memory () in
  let store =
    {
      memory with
      put =
        (fun k r ->
          incr puts;
          keys := k :: !keys;
          memory.put k r);
    }
  in
  let t =
    S.v ~name:"app" ~origin:"https://ci.example.org" ~random:env#secure_random
      ~clock store
  in
  (* The cookie is __Host-, Secure, HttpOnly, Lax and host-wide. *)
  let s, field = S.create t ~subject:"did:plc:alice" ~data:"{}" () in
  check "a Set-Cookie field" (field.name = Httpz.Header_name.Set_cookie);
  let id = value_of field in
  check "an ID of 256 bits" (String.length id = 43);
  check "the cookie's attributes"
    (String.starts_with ~prefix:("__Host-app=" ^ id ^ "; Expires=") field.value
    && String.ends_with ~suffix:"; Path=/; Secure; HttpOnly; SameSite=Lax"
         field.value
    && not (has field.value "Domain"));
  (* The store is keyed by the ID's hash, never the ID. *)
  check "keyed by hash" (!keys = [ S.id_hash id ] && not (List.mem id !keys));
  check "the hash is SHA-256" (String.length (S.id_hash id) = 64);
  let cookie v = ("Cookie", "theme=dark; __Host-app=" ^ v) in
  let found () = with_req ~headers:[ cookie id ] "/" (S.find t) in
  (match found () with
  | Some s -> check "found" (S.subject s = "did:plc:alice" && S.data s = "{}")
  | None -> check "found" false);
  (* A second cookie of the same name, or a malformed one, finds nothing. *)
  check "duplicate cookie"
    (with_req ~headers:[ ("Cookie", "__Host-app=" ^ id ^ "; __Host-app=" ^ id) ] "/" (S.find t)
    = None);
  check "duplicate field"
    (with_req ~headers:[ cookie id; ("Cookie", "__Host-app=x") ] "/" (S.find t) = None);
  check "malformed" (with_req ~headers:[ cookie "nope" ] "/" (S.find t) = None);
  check "unknown" (with_req ~headers:[ cookie (String.make 43 'A') ] "/" (S.find t) = None);
  check "another name" (with_req ~headers:[ ("Cookie", "app=" ^ id) ] "/" (S.find t) = None);
  (* A login always mints a new ID, and a destroyed session is gone. *)
  let s2, field2 = S.create t ~subject:"did:plc:alice" () in
  check "a new ID each time" (value_of field2 <> id);
  let cleared = S.destroy t s2 in
  check "destroy clears the cookie"
    (has cleared.value "__Host-app=; Expires=Thu, 01 Jan 1970");
  check "destroyed"
    (with_req ~headers:[ cookie (value_of field2) ] "/" (S.find t) = None);
  (* Last seen is written at most once a minute. *)
  let before = !puts in
  ignore (found ());
  check "no write within a minute" (!puts = before);
  Eio_mock.Clock.set_time clock (1_800_000_000. +. 61.);
  ignore (found ());
  check "a write after a minute" (!puts = before + 1);
  (* Idle expiry: two hours without use. *)
  Eio_mock.Clock.set_time clock (1_800_000_000. +. 61. +. 7201.);
  check "idle expiry" (found () = None);
  check "an expired session is deleted" (store.get (S.id_hash id) = None);
  (* Absolute expiry: twelve hours, however often it is used. *)
  let start = 1_800_100_000. in
  Eio_mock.Clock.set_time clock start;
  let _, f = S.create t ~subject:"did:plc:bob" () in
  let bob = value_of f in
  let rec use h =
    if h < 12 then (
      Eio_mock.Clock.set_time clock (start +. (float h *. 3600.));
      check "used within its lifetime"
        (with_req ~headers:[ cookie bob ] "/" (S.find t) <> None);
      use (h + 1))
  in
  use 1;
  Eio_mock.Clock.set_time clock (start +. (12. *. 3600.));
  check "absolute expiry" (with_req ~headers:[ cookie bob ] "/" (S.find t) = None);
  (* CSRF: an unsafe request needs the token and a same-origin browser. *)
  let token = S.csrf_token s in
  check "tokens differ by purpose" (S.csrf_token ~purpose:"delete" s <> token);
  check "tokens do not reveal the ID" (not (has token id));
  let form = ("Content-Type", "application/x-www-form-urlencoded") in
  let post ?(extra = []) body =
    with_req ~meth:M.Post ~headers:(form :: extra) ~body "/act"
      (S.check_unsafe t s)
  in
  let same = [ ("Origin", "https://ci.example.org"); ("Sec-Fetch-Site", "same-origin") ] in
  check "accepted" (post ~extra:same ("_csrf=" ^ token));
  check "missing token" (not (post ~extra:same "x=1"));
  check "wrong token" (not (post ~extra:same ("_csrf=" ^ S.csrf_token ~purpose:"other" s)));
  check "truncated token"
    (not (post ~extra:same ("_csrf=" ^ String.sub token 0 42)));
  check "cross-site"
    (not (post ~extra:[ ("Sec-Fetch-Site", "cross-site") ] ("_csrf=" ^ token)));
  check "same-site is not same-origin"
    (not (post ~extra:[ ("Sec-Fetch-Site", "same-site") ] ("_csrf=" ^ token)));
  check "a foreign origin"
    (not (post ~extra:[ ("Origin", "https://evil.example") ] ("_csrf=" ^ token)));
  check "a null origin" (not (post ~extra:[ ("Origin", "null") ] ("_csrf=" ^ token)));
  check "a repeated origin"
    (not
       (post
          ~extra:[ ("Origin", "https://ci.example.org"); ("Origin", "https://ci.example.org") ]
          ("_csrf=" ^ token)));
  check "no origin fields, with the token" (post ("_csrf=" ^ token));
  check "no origin fields, without the token" (not (post ""));
  check "a header token"
    (with_req ~meth:M.Post ~headers:(("X-CSRF-Token", token) :: same) "/act"
       (S.check_unsafe t s));
  check "a safe method needs nothing" (with_req "/" (S.check_unsafe t s));
  check "request_origin"
    (with_req ~headers:same "/" (S.request_origin t) = `Same
    && with_req "/" (S.request_origin t) = `Unknown
    && with_req ~headers:[ ("Sec-Fetch-Site", "none") ] "/" (S.request_origin t)
       = `Same);
  (* The login cookie binds a login to the browser that began it. *)
  let nonce, lf = S.Login.start t in
  check "login cookie"
    (String.starts_with ~prefix:("__Host-app-login=" ^ nonce ^ "; Expires=") lf.value
    && String.ends_with ~suffix:"; Path=/; Secure; HttpOnly; SameSite=Lax" lf.value);
  let login v = ("Cookie", "__Host-app-login=" ^ v) in
  let hash = S.Login.hash nonce in
  check "login matches" (with_req ~headers:[ login nonce ] "/cb" (fun r -> S.Login.matches t r ~hash));
  check "another browser"
    (not (with_req "/cb" (fun r -> S.Login.matches t r ~hash)));
  check "another login"
    (not
       (with_req ~headers:[ login (String.make 43 'B') ] "/cb" (fun r ->
            S.Login.matches t r ~hash)));
  check "a duplicate login cookie"
    (not
       (with_req
          ~headers:
            [ ("Cookie", "__Host-app-login=" ^ nonce ^ "; __Host-app-login=" ^ nonce) ]
          "/cb"
          (fun r -> S.Login.matches t r ~hash)));
  check "login clear" (has (S.Login.clear t).value "__Host-app-login=; Expires=Thu, 01 Jan 1970");
  (* Configuration is checked. *)
  let refused f = match f () with _ -> false | exception Invalid_argument _ -> true in
  let mk ?(name = "app") origin () =
    S.v ~name ~origin ~random:env#secure_random ~clock (S.Store.memory ())
  in
  check "a bad name" (refused (mk ~name:"a b" "https://x.example"));
  check "a path" (refused (mk "https://x.example/app"));
  check "plain http" (refused (mk "http://x.example"));
  check "credentials" (refused (mk "https://u@x.example"));
  check "loopback http" (not (refused (mk "http://localhost:8080")));
  Printf.printf "session: %d checks passed\n" !checks
