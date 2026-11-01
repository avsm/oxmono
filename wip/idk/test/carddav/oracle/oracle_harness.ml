(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  env : Eio_unix.Stdenv.base;
  sw : Eio.Switch.t;
  client : Carddav_eio.Client.t;
  user : string;
}

let join collection name =
  if String.ends_with ~suffix:"/" collection then collection ^ name
  else collection ^ "/" ^ name

let getenv name default =
  match Sys.getenv_opt name with Some v when v <> "" -> v | _ -> default

let configured () = Sys.getenv_opt "CARDDAV_ORACLE_URL" <> None

let url () =
  match Sys.getenv_opt "CARDDAV_ORACLE_URL" with
  | Some u when u <> "" -> u
  | _ -> Alcotest.fail "CARDDAV_ORACLE_URL is not set"

let user () = getenv "CARDDAV_ORACLE_USER" "alice"
let password () = getenv "CARDDAV_ORACLE_PASSWORD" "x"

let credentials () =
  [ Fetch.Credential.basic ~user:(user ()) ~password:(password ()) ]

let connect_with ~sw ?credentials:c env =
  let credentials = match c with Some c -> c | None -> credentials () in
  Carddav_eio.Client.connect ~sw ~credentials ~allow_insecure:true
    (Fetch_httpz.std env) (url ())

let ok what = function
  | Ok v -> v
  | Error e ->
      Alcotest.failf "%s: %s" what (Carddav_eio.Client.error_to_string e)

(* Every collection a test creates is removed when it ends, whatever its
   outcome, so that a shared account is left as it was found. *)
let created : string list ref = ref []

let test_case name f =
  let run () =
    if not (configured ()) then Alcotest.skip ()
    else
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let client = ok "connect" (connect_with ~sw env) in
      created := [];
      Fun.protect
        ~finally:(fun () ->
          List.iter
            (fun url ->
              ignore (Carddav_eio.Client.delete_addressbook client url))
            !created)
        (fun () -> f { env; sw; client; user = user () })
  in
  Alcotest.test_case name `Quick run

let counter = ref 0

let unique prefix =
  incr counter;
  Printf.sprintf "%s-%d-%d-%d" prefix (Unix.getpid ())
    (int_of_float (Unix.gettimeofday () *. 1000.) mod 1_000_000)
    !counter

let fresh_addressbook t =
  match Carddav_eio.Client.home_sets t.client with
  | [] -> Alcotest.fail "the principal has no address book home"
  | home :: _ ->
      let url = join home (unique "book") ^ "/" in
      ok "create address book"
        (Carddav_eio.Client.create_addressbook t.client ~display_name:"Oracle"
           url);
      created := url :: !created;
      url

let vcard ?uid ?email full =
  let uid = match uid with Some u -> u | None -> unique "uid" in
  let props =
    [ Vcard.Property.of_text "UID" uid; Vcard.Property.of_text "FN" full ]
    @
    match email with
    | Some e -> [ Vcard.Property.of_text "EMAIL" e ]
    | None -> []
  in
  Vcard.v props
