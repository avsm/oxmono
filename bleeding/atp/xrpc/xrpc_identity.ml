(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

(* An identity document is small, so a long answer is not one. *)
let limit = 1024 * 1024

let get http url =
  Fetch.with_response (Fetch.restrict http) `GET url (fun response ->
      let status = Fetch.status response in
      if status <> 200 then Error (Printf.sprintf "%s: HTTP %d" url status)
      else Ok (Fetch.decode ~limit Fetch.Media.octets response))

let member name = function
  | Jsont.Object (ms, _) -> (
      match Jsont.Json.find_mem name ms with
      | Some (_, v) -> Some v
      | None -> None)
  | _ -> None

let str = function Some (Jsont.String (s, _)) -> Some s | _ -> None

let pds_of_document json =
  match Jsont_bytesrw.decode_string Jsont.json json with
  | Error _ -> None
  | Ok doc -> (
      match member "service" doc with
      | Some (Jsont.Array (services, _)) ->
          List.find_map
            (fun s ->
              if str (member "type" s) = Some "AtprotoPersonalDataServer" then
                str (member "serviceEndpoint" s)
              else None)
            services
      | _ -> None)

let document_url did =
  match Atp.Did.of_string did with
  | Error _ -> Error (Printf.sprintf "%S is not a DID" did)
  | Ok _ -> (
      match String.split_on_char ':' did with
      | [ "did"; "plc"; _ ] -> Ok ("https://plc.directory/" ^ did)
      | [ "did"; "web"; host ] ->
          Ok (Printf.sprintf "https://%s/.well-known/did.json" host)
      | _ -> Error (Printf.sprintf "%s: unsupported DID method" did))

let did_of_handle http handle =
  match Atp.Handle.of_string handle with
  | Error _ -> Error (Printf.sprintf "%S is not a handle" handle)
  | Ok _ -> (
      let* body =
        get http (Printf.sprintf "https://%s/.well-known/atproto-did" handle)
      in
      let did = String.trim body in
      match Atp.Did.of_string did with
      | Ok _ -> Ok did
      | Error _ -> Error (Printf.sprintf "%s does not publish a DID" handle))

let pds_of_did http did =
  let* url = document_url did in
  let* body = get http url in
  match pds_of_document body with
  | Some pds -> Ok pds
  | None -> Error (Printf.sprintf "%s lists no data server" did)

let pds_of_handle http handle =
  let* did = did_of_handle http handle in
  pds_of_did http did
