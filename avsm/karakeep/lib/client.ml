(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  client : Karakeep.t;
  session : Session.credentials;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  profile : string option;
}

let api_base_url base_url =
  let base = if String.ends_with ~suffix:"/" base_url
    then String.sub base_url 0 (String.length base_url - 1)
    else base_url
  in
  base ^ "/api/v1"

let create_with_session ~sw ~env ?profile ~session () =
  let fs = env#fs in
  let base_url = api_base_url (Session.base_url session) in
  let api_key = Session.api_key session in
  let requests_session = Requests.create ~sw env in
  let requests_session =
    Requests.set_auth requests_session (Requests.Auth.bearer ~token:api_key)
  in
  let client = Karakeep.create ~session:requests_session ~sw env ~base_url in
  { client; session; fs; profile }

let login ~sw ~env ?profile ~base_url ~api_key () =
  let fs = env#fs in
  let api_url = api_base_url base_url in
  let requests_session = Requests.create ~sw env in
  let requests_session =
    Requests.set_auth requests_session (Requests.Auth.bearer ~token:api_key)
  in
  let client = Karakeep.create ~session:requests_session ~sw env ~base_url:api_url in
  (* Validate by calling whoami *)
  let _user = Karakeep.Client.get_users_me client () in
  let session = Session.create ~base_url ~api_key () in
  Session.save fs ?profile session;
  let profiles = Session.list_profiles fs in
  let profile_name = Option.value ~default:Session.default_profile profile in
  if profiles = [] || Option.is_some profile then
    Session.set_current_profile fs profile_name;
  { client; session; fs; profile }

let resume ~sw ~env ?profile ~session () =
  create_with_session ~sw ~env ?profile ~session ()

let logout t =
  Session.clear t.fs ?profile:t.profile ()

let client t = t.client
let session t = t.session
let profile t = t.profile
let fs t = t.fs
