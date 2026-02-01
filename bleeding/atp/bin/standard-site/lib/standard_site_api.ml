(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Aliases for generated lexicon modules *)
module Atproto = Atp_lexicon_atproto.Com.Atproto
module Standard = Atp_lexicon_standard_site.Site.Standard
module StrongRef = Atp_lexicon_standard_site.Com.Atproto.Repo.StrongRef

type t = Xrpc_auth.Client.t

let create = Xrpc_auth.Client.create
let login = Xrpc_auth.Client.login
let resume = Xrpc_auth.Client.resume
let logout = Xrpc_auth.Client.logout
let get_session = Xrpc_auth.Client.get_session
let is_logged_in = Xrpc_auth.Client.is_logged_in
let get_did = Xrpc_auth.Client.get_did
let resolve_handle t handle = Xrpc_auth.Cmd.resolve_did t (Some handle)

let resolve_bsky_post t url =
  (* Parse Bluesky URL: https://bsky.app/profile/{handle}/post/{rkey}
     Also supports AT URIs: at://{did}/app.bsky.feed.post/{rkey} *)
  let url = String.trim url in
  let handle, rkey =
    if String.starts_with ~prefix:"at://" url then
      (* AT URI format: at://did:plc:xxx/app.bsky.feed.post/rkey *)
      let parts = String.split_on_char '/' url in
      match parts with
      | [ "at:"; ""; did; "app.bsky.feed.post"; rkey ] -> (did, rkey)
      | _ -> failwith ("Invalid AT URI format: " ^ url)
    else
      (* Web URL format: https://bsky.app/profile/handle/post/rkey *)
      let uri = Uri.of_string url in
      let path = Uri.path uri in
      let parts = String.split_on_char '/' path in
      match parts with
      | [ ""; "profile"; handle; "post"; rkey ] -> (handle, rkey)
      | _ -> failwith ("Invalid Bluesky URL format: " ^ url)
  in
  (* Resolve handle to DID if needed *)
  let did =
    if String.starts_with ~prefix:"did:" handle then handle
    else resolve_handle t handle
  in
  (* Fetch the post record to get the CID *)
  let client = Xrpc_auth.Client.get_client t in
  let resp =
    Xrpc.Client.query client ~nsid:"com.atproto.repo.getRecord"
      ~params:
        [
          ("repo", did);
          ("collection", "app.bsky.feed.post");
          ("rkey", rkey);
        ]
      ~decoder:Atproto.Repo.GetRecord.output_jsont
  in
  let cid =
    match resp.cid with
    | Some cid -> cid
    | None -> failwith "No CID returned for Bluesky post"
  in
  let uri = Printf.sprintf "at://%s/app.bsky.feed.post/%s" did rkey in
  ({ uri; cid } : StrongRef.main)

(* Helper to decode listRecords response with filtering *)
let decode_list_records (decoder : 'a Jsont.t)
    (resp : Atproto.Repo.ListRecords.output) : (string * 'a) list =
  List.filter_map
    (fun (r : Atproto.Repo.ListRecords.record) ->
      (* Extract rkey from URI *)
      let rkey =
        match String.split_on_char '/' r.uri |> List.rev with
        | rkey :: _ -> rkey
        | [] -> r.uri
      in
      match Jsont.Json.decode decoder r.value with
      | Ok v -> Some (rkey, v)
      | Error _ -> None)
    resp.records

(* Helper to encode a value to Jsont.json *)
let encode_to_json (encoder : 'a Jsont.t) (value : 'a) : Jsont.json =
  match Jsont.Json.encode encoder value with
  | Ok json -> json
  | Error e -> failwith ("Failed to encode: " ^ e)

(* Publication Operations *)

let list_publications t ?did () =
  let client = Xrpc_auth.Client.get_client t in
  let target_did = match did with Some d -> d | None -> get_did t in
  let resp =
    Xrpc.Client.query client ~nsid:"com.atproto.repo.listRecords"
      ~params:
        [
          ("repo", target_did);
          ("collection", "site.standard.publication");
          ("limit", "100");
        ]
      ~decoder:Atproto.Repo.ListRecords.output_jsont
  in
  decode_list_records Standard.Publication.main_jsont resp

let get_publication t ~did ~rkey =
  let client = Xrpc_auth.Client.get_client t in
  try
    let resp =
      Xrpc.Client.query client ~nsid:"com.atproto.repo.getRecord"
        ~params:
          [
            ("repo", did);
            ("collection", "site.standard.publication");
            ("rkey", rkey);
          ]
        ~decoder:Atproto.Repo.GetRecord.output_jsont
    in
    match Jsont.Json.decode Standard.Publication.main_jsont resp.value with
    | Ok v -> Some v
    | Error _ -> None
  with Eio.Io (Xrpc.Error.E _, _) -> None

let create_publication t ~name ~url ?description ?rkey () =
  let client = Xrpc_auth.Client.get_client t in
  let did = get_did t in
  let record : Standard.Publication.main =
    {
      name;
      url;
      description;
      icon = None;
      basic_theme = None;
      preferences = None;
    }
  in
  let input : Atproto.Repo.CreateRecord.input =
    {
      repo = did;
      collection = "site.standard.publication";
      rkey;
      validate = Some false;
      record = encode_to_json Standard.Publication.main_jsont record;
      swap_commit = None;
    }
  in
  let resp =
    Xrpc.Client.procedure client ~nsid:"com.atproto.repo.createRecord"
      ~params:[] ~input:(Some Atproto.Repo.CreateRecord.input_jsont)
      ~input_data:(Some input) ~decoder:Atproto.Repo.CreateRecord.output_jsont
  in
  (* Extract rkey from AT URI *)
  match String.split_on_char '/' resp.uri |> List.rev with
  | rkey :: _ -> rkey
  | [] -> resp.uri

let update_publication t ~rkey ~name ~url ?description () =
  let client = Xrpc_auth.Client.get_client t in
  let did = get_did t in
  let record : Standard.Publication.main =
    {
      name;
      url;
      description;
      icon = None;
      basic_theme = None;
      preferences = None;
    }
  in
  let input : Atproto.Repo.PutRecord.input =
    {
      repo = did;
      collection = "site.standard.publication";
      rkey;
      validate = Some false;
      record = encode_to_json Standard.Publication.main_jsont record;
      swap_record = None;
      swap_commit = None;
    }
  in
  let _ =
    Xrpc.Client.procedure client ~nsid:"com.atproto.repo.putRecord" ~params:[]
      ~input:(Some Atproto.Repo.PutRecord.input_jsont) ~input_data:(Some input)
      ~decoder:Atproto.Repo.PutRecord.output_jsont
  in
  ()

let delete_publication t ~rkey =
  let client = Xrpc_auth.Client.get_client t in
  let did = get_did t in
  let input : Atproto.Repo.DeleteRecord.input =
    {
      repo = did;
      collection = "site.standard.publication";
      rkey;
      swap_record = None;
      swap_commit = None;
    }
  in
  let _ =
    Xrpc.Client.procedure client ~nsid:"com.atproto.repo.deleteRecord"
      ~params:[] ~input:(Some Atproto.Repo.DeleteRecord.input_jsont)
      ~input_data:(Some input) ~decoder:Atproto.Repo.DeleteRecord.output_jsont
  in
  ()

(* Document Operations *)

let list_documents t ?did () =
  let client = Xrpc_auth.Client.get_client t in
  let target_did = match did with Some d -> d | None -> get_did t in
  let resp =
    Xrpc.Client.query client ~nsid:"com.atproto.repo.listRecords"
      ~params:
        [
          ("repo", target_did);
          ("collection", "site.standard.document");
          ("limit", "100");
        ]
      ~decoder:Atproto.Repo.ListRecords.output_jsont
  in
  decode_list_records Standard.Document.main_jsont resp

let get_document t ~did ~rkey =
  let client = Xrpc_auth.Client.get_client t in
  try
    let resp =
      Xrpc.Client.query client ~nsid:"com.atproto.repo.getRecord"
        ~params:
          [
            ("repo", did);
            ("collection", "site.standard.document");
            ("rkey", rkey);
          ]
        ~decoder:Atproto.Repo.GetRecord.output_jsont
    in
    match Jsont.Json.decode Standard.Document.main_jsont resp.value with
    | Ok v -> Some v
    | Error _ -> None
  with Eio.Io (Xrpc.Error.E _, _) -> None

let create_document t ~site ~title ~published_at ?path ?description
    ?text_content ?tags ?bsky_post_ref ?rkey () =
  let client = Xrpc_auth.Client.get_client t in
  let did = get_did t in
  let record : Standard.Document.main =
    {
      site;
      title;
      published_at;
      path;
      description;
      text_content;
      tags;
      updated_at = None;
      cover_image = None;
      content = None;
      bsky_post_ref;
    }
  in
  let input : Atproto.Repo.CreateRecord.input =
    {
      repo = did;
      collection = "site.standard.document";
      rkey;
      validate = Some false;
      record = encode_to_json Standard.Document.main_jsont record;
      swap_commit = None;
    }
  in
  let resp =
    Xrpc.Client.procedure client ~nsid:"com.atproto.repo.createRecord"
      ~params:[] ~input:(Some Atproto.Repo.CreateRecord.input_jsont)
      ~input_data:(Some input) ~decoder:Atproto.Repo.CreateRecord.output_jsont
  in
  match String.split_on_char '/' resp.uri |> List.rev with
  | rkey :: _ -> rkey
  | [] -> resp.uri

let update_document t ~rkey ~site ~title ~published_at ?path ?description
    ?text_content ?tags ?bsky_post_ref ?updated_at () =
  let client = Xrpc_auth.Client.get_client t in
  let did = get_did t in
  let record : Standard.Document.main =
    {
      site;
      title;
      published_at;
      path;
      description;
      text_content;
      tags;
      updated_at;
      cover_image = None;
      content = None;
      bsky_post_ref;
    }
  in
  let input : Atproto.Repo.PutRecord.input =
    {
      repo = did;
      collection = "site.standard.document";
      rkey;
      validate = Some false;
      record = encode_to_json Standard.Document.main_jsont record;
      swap_record = None;
      swap_commit = None;
    }
  in
  let _ =
    Xrpc.Client.procedure client ~nsid:"com.atproto.repo.putRecord" ~params:[]
      ~input:(Some Atproto.Repo.PutRecord.input_jsont) ~input_data:(Some input)
      ~decoder:Atproto.Repo.PutRecord.output_jsont
  in
  ()

let delete_document t ~rkey =
  let client = Xrpc_auth.Client.get_client t in
  let did = get_did t in
  let input : Atproto.Repo.DeleteRecord.input =
    {
      repo = did;
      collection = "site.standard.document";
      rkey;
      swap_record = None;
      swap_commit = None;
    }
  in
  let _ =
    Xrpc.Client.procedure client ~nsid:"com.atproto.repo.deleteRecord"
      ~params:[] ~input:(Some Atproto.Repo.DeleteRecord.input_jsont)
      ~input_data:(Some input) ~decoder:Atproto.Repo.DeleteRecord.output_jsont
  in
  ()
