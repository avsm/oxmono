(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type session = {
  access_jwt : string;
  refresh_jwt : string;
  did : string;
  handle : string;
  pds_uri : string option;
  email : string option;
  email_confirmed : bool option;
  email_auth_factor : bool option;
  active : bool option;
  status : string option;
}

let session_jsont =
  Jsont.Object.map ~kind:"Session"
    (fun
      access_jwt
      refresh_jwt
      did
      handle
      _did_doc
      pds_uri
      email
      email_confirmed
      email_auth_factor
      active
      status
    ->
      {
        access_jwt;
        refresh_jwt;
        did;
        handle;
        pds_uri;
        email;
        email_confirmed;
        email_auth_factor;
        active;
        status;
      })
  |> Jsont.Object.mem "accessJwt" Jsont.string ~enc:(fun s -> s.access_jwt)
  |> Jsont.Object.mem "refreshJwt" Jsont.string ~enc:(fun s -> s.refresh_jwt)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun s -> s.did)
  |> Jsont.Object.mem "handle" Jsont.string ~enc:(fun s -> s.handle)
  |> Jsont.Object.opt_mem "didDoc" Jsont.json ~enc:(fun _ -> None)
    (* didDoc is read-only, we extract pds_uri separately *)
  |> Jsont.Object.opt_mem "pdsUri" Jsont.string ~enc:(fun s -> s.pds_uri)
  |> Jsont.Object.opt_mem "email" Jsont.string ~enc:(fun s -> s.email)
  |> Jsont.Object.opt_mem "emailConfirmed" Jsont.bool ~enc:(fun s ->
      s.email_confirmed)
  |> Jsont.Object.opt_mem "emailAuthFactor" Jsont.bool ~enc:(fun s ->
      s.email_auth_factor)
  |> Jsont.Object.opt_mem "active" Jsont.bool ~enc:(fun s -> s.active)
  |> Jsont.Object.opt_mem "status" Jsont.string ~enc:(fun s -> s.status)
  |> Jsont.Object.finish

type error_payload = { error : string; message : string option }

let error_payload_jsont =
  Jsont.Object.map ~kind:"ErrorPayload" (fun error message ->
      { error; message })
  |> Jsont.Object.mem "error" Jsont.string ~enc:(fun e -> e.error)
  |> Jsont.Object.opt_mem "message" Jsont.string ~enc:(fun e -> e.message)
  |> Jsont.Object.finish

type login_request = {
  identifier : string;
  password : string;
  auth_factor_token : string option;
}

let login_request_jsont =
  Jsont.Object.map ~kind:"LoginRequest"
    (fun identifier password auth_factor_token ->
      { identifier; password; auth_factor_token })
  |> Jsont.Object.mem "identifier" Jsont.string ~enc:(fun r -> r.identifier)
  |> Jsont.Object.mem "password" Jsont.string ~enc:(fun r -> r.password)
  |> Jsont.Object.opt_mem "authFactorToken" Jsont.string ~enc:(fun r ->
      r.auth_factor_token)
  |> Jsont.Object.finish

(* Accepts any JSON and returns unit *)
let empty_jsont = Jsont.ignore
