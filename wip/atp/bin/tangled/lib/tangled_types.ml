(* SPDX-License-Identifier: ISC *)

type at_uri = { did : string; collection : string; rkey : string }

let parse_at_uri text =
  match Atp.At_uri.of_string text with
  | Error _ -> None
  | Ok uri -> (
      match (Atp.At_uri.collection uri, Atp.At_uri.rkey uri) with
      | Some collection, Some rkey ->
          Some { did = Atp.At_uri.authority uri; collection; rkey }
      | _ -> None)

let make_at_uri ~did ~collection ~rkey =
  match Atp.At_uri.make ~authority:did ~collection ~rkey () with
  | Ok uri -> Atp.At_uri.to_string uri
  | Error _ -> invalid_arg "Invalid record URI"

let pp_at_uri ppf uri =
  Fmt.string ppf
    (make_at_uri ~did:uri.did ~collection:uri.collection ~rkey:uri.rkey)
