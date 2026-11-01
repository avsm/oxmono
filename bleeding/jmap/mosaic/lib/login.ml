(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Profile = Jmap_eio.Profile

let profiles_path env =
  match Profile.xdg_store env with
  | Ok store -> Profile.directory store
  | Error _ -> ""

let login_of_profile profile =
  let scheme, user, secret, secret_ref =
    match Profile.credential profile with
    | Profile.Bearer token -> (Model.Bearer, "", token, None)
    | Profile.Basic { user; password } -> (Model.Basic, user, password, None)
    | Profile.Bearer_ref source -> (Model.Bearer, "", "", Some source)
    | Profile.Basic_ref source -> (Model.Basic, "", "", Some source)
  in
  {
    Model.blank with
    profile = Profile.name profile;
    url = Profile.session_url profile;
    scheme;
    user;
    secret;
    secret_ref;
  }

let profile_of_login (login : Model.login) =
  let credential =
    match (login.scheme, login.secret_ref) with
    | Model.Bearer, Some source -> Profile.Bearer_ref source
    | Model.Basic, Some source -> Profile.Basic_ref source
    | Model.Bearer, None -> Profile.Bearer login.secret
    | Model.Basic, None ->
        Profile.Basic { user = login.user; password = login.secret }
  in
  Profile.v ~name:login.profile ~session_url:login.url credential

let shared_store env = Result.to_option (Profile.xdg_store env)

let from_store store name =
  Result.to_option (Profile.load store name) |> Option.map login_of_profile

let read_profile env name =
  Option.bind (shared_store env) (fun store -> from_store store name)

let profiles_from = function
  | None -> []
  | Some store -> (
      match Profile.list store with
      | Ok profiles -> List.map login_of_profile profiles
      | Error _ -> [])

let profiles env = profiles_from (shared_store env)

let write_profile env (login : Model.login) =
  match (shared_store env, profile_of_login login) with
  | Some store, Ok profile -> ignore (Profile.save store profile)
  | None, _ | _, Error _ -> ()
