(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Aliases for generated lexicon modules *)
module Atproto = Atp_lexicon_atproto.Com.Atproto
module Tangled = Atp_lexicon_tangled.Sh.Tangled

(* Use shared xrpc_auth client *)
type t = Xrpc_auth.Client.t

let create = Xrpc_auth.Client.create
let login = Xrpc_auth.Client.login
let resume = Xrpc_auth.Client.resume
let logout = Xrpc_auth.Client.logout
let get_session = Xrpc_auth.Client.get_session
let is_logged_in = Xrpc_auth.Client.is_logged_in
let get_did = Xrpc_auth.Client.get_did
let get_client = Xrpc_auth.Client.get_client

(* Identity *)

let resolve_handle t handle =
  let client = get_client t in
  let resp =
    Xrpc.Client.query client ~nsid:"com.atproto.identity.resolveHandle"
      ~params:[ ("handle", handle) ]
      ~decoder:Tangled_types.resolve_handle_response_jsont
  in
  resp.did

(* Repository Operations *)

(* Helper to extract rkey from AT URI *)
let rkey_of_uri uri =
  match Tangled_types.parse_at_uri uri with Some u -> u.rkey | None -> uri

(* Helper to decode listRecords response with filtering *)
let decode_list_records (decoder : 'a Jsont.t)
    (resp : Atproto.Repo.ListRecords.output) : (string * 'a) list =
  List.filter_map
    (fun (r : Atproto.Repo.ListRecords.record) ->
      match Jsont.Json.decode decoder r.value with
      | Ok v -> Some (rkey_of_uri r.uri, v)
      | Error _ -> None)
    resp.records

let list_repos t ?did () =
  let client = get_client t in
  let target_did = match did with Some d -> d | None -> get_did t in
  let resp =
    Xrpc.Client.query client ~nsid:"com.atproto.repo.listRecords"
      ~params:
        [
          ("repo", target_did);
          ("collection", "sh.tangled.repo");
          ("limit", "100");
        ]
      ~decoder:Atproto.Repo.ListRecords.output_jsont
  in
  decode_list_records Tangled.Repo.main_jsont resp

let get_repo t ~did ~rkey =
  let client = get_client t in
  try
    let resp =
      Xrpc.Client.query client ~nsid:"com.atproto.repo.getRecord"
        ~params:
          [ ("repo", did); ("collection", "sh.tangled.repo"); ("rkey", rkey) ]
        ~decoder:Atproto.Repo.GetRecord.output_jsont
    in
    (* Decode the value field *)
    match Jsont.Json.decode Tangled.Repo.main_jsont resp.value with
    | Ok v -> Some v
    | Error _ -> None
  with Eio.Io (Xrpc.Error.E _, _) -> None

(* Helper to create a client for a specific knot server *)
let knot_client t ~knot =
  let service = "https://" ^ knot in
  Xrpc_auth.Client.make_client t ~service

(* Create a minimal session for ServiceAuth tokens *)
let make_service_auth_session ~did ~token : Xrpc.Types.session =
  {
    access_jwt = token;
    refresh_jwt = "";
    did;
    handle = "";
    pds_uri = None;
    email = None;
    email_confirmed = None;
    email_auth_factor = None;
    active = None;
    status = None;
  }

(* Get ServiceAuth token for knot operations *)
let get_service_auth t ~knot =
  let client = get_client t in
  (* Audience is did:web:<knot-host> *)
  let aud = "did:web:" ^ knot in
  (* ServiceAuth tokens must expire within 60 seconds *)
  let exp = Int64.to_string (Int64.add (Int64.of_float (Unix.time ())) 60L) in
  let resp =
    Xrpc.Client.query client ~nsid:"com.atproto.server.getServiceAuth"
      ~params:[ ("aud", aud); ("exp", exp) ]
      ~decoder:Tangled_types.service_auth_response_jsont
  in
  resp.token

(* Helper to encode a value to Jsont.json *)
let encode_to_json (encoder : 'a Jsont.t) (value : 'a) : Jsont.json =
  match Jsont.Json.encode encoder value with
  | Ok json -> json
  | Error e -> failwith ("Failed to encode: " ^ e)

let create_repo t ~name ~knot ?description ?default_branch () =
  let client = get_client t in
  let did = get_did t in

  (* 1. Create sh.tangled.repo record on PDS *)
  let now = Ptime.to_rfc3339 (Ptime_clock.now ()) in
  let repo_record : Tangled.Repo.main =
    {
      name;
      knot;
      description;
      created_at = now;
      spindle = None;
      website = None;
      topics = None;
      source = None;
      labels = None;
    }
  in
  let input : Atproto.Repo.CreateRecord.input =
    {
      repo = did;
      collection = "sh.tangled.repo";
      rkey = None;
      validate = Some false;
      record = encode_to_json Tangled.Repo.main_jsont repo_record;
      swap_commit = None;
    }
  in
  let resp =
    Xrpc.Client.procedure client ~nsid:"com.atproto.repo.createRecord"
      ~params:[] ~input:(Some Atproto.Repo.CreateRecord.input_jsont)
      ~input_data:(Some input) ~decoder:Atproto.Repo.CreateRecord.output_jsont
  in

  (* Extract rkey from AT URI *)
  let rkey = rkey_of_uri resp.uri in

  (* 2. Get ServiceAuth for knot *)
  let sa_token = get_service_auth t ~knot in

  (* 3. Create repo on knot *)
  let knot_input : Tangled.Repo.Create.input =
    { rkey; default_branch; source = None }
  in
  let knot_client = knot_client t ~knot in
  Xrpc.Client.set_session knot_client
    (make_service_auth_session ~did ~token:sa_token);
  let _ =
    Xrpc.Client.procedure knot_client ~nsid:"sh.tangled.repo.create" ~params:[]
      ~input:(Some Tangled.Repo.Create.input_jsont)
      ~input_data:(Some knot_input) ~decoder:Xrpc.Types.empty_jsont
  in
  rkey

let delete_repo t ~name ~knot =
  let client = get_client t in
  let did = get_did t in

  (* Find the repo record to get the rkey *)
  let repos = list_repos t ~did () in
  let rkey =
    match
      List.find_opt
        (fun (_, (r : Tangled.Repo.main)) -> r.name = name && r.knot = knot)
        repos
    with
    | Some (rkey, _) -> rkey
    | None -> failwith ("Repository not found: " ^ name)
  in

  (* 1. Delete from knot *)
  let sa_token = get_service_auth t ~knot in
  let knot_client = knot_client t ~knot in
  Xrpc.Client.set_session knot_client
    (make_service_auth_session ~did ~token:sa_token);
  let knot_input : Tangled.Repo.Delete.input = { did; name; rkey } in
  let _ =
    Xrpc.Client.procedure knot_client ~nsid:"sh.tangled.repo.delete" ~params:[]
      ~input:(Some Tangled.Repo.Delete.input_jsont)
      ~input_data:(Some knot_input) ~decoder:Xrpc.Types.empty_jsont
  in

  (* 2. Delete record from PDS *)
  let delete_input : Atproto.Repo.DeleteRecord.input =
    {
      repo = did;
      collection = "sh.tangled.repo";
      rkey;
      swap_record = None;
      swap_commit = None;
    }
  in
  let _ =
    Xrpc.Client.procedure client ~nsid:"com.atproto.repo.deleteRecord"
      ~params:[] ~input:(Some Atproto.Repo.DeleteRecord.input_jsont)
      ~input_data:(Some delete_input)
      ~decoder:Atproto.Repo.DeleteRecord.output_jsont
  in
  ()

(* Convert from generated lexicon type to our language type *)
let language_of_generated (g : Tangled.Repo.Languages.language) :
    Tangled_types.language =
  {
    name = g.name;
    size = g.size;
    percentage = g.percentage;
    file_count = g.file_count;
    color = g.color;
    extensions = g.extensions;
  }

let get_repo_info t ~knot ~did ~name =
  let knot_client = knot_client t ~knot in
  (* Repo identifier in format "did/name" *)
  let repo = did ^ "/" ^ name in
  let params = [ ("repo", repo) ] in

  (* Get default branch using generated lexicon type *)
  let branch_resp =
    Xrpc.Client.query knot_client ~nsid:"sh.tangled.repo.getDefaultBranch"
      ~params ~decoder:Tangled.Repo.GetDefaultBranch.output_jsont
  in

  (* Get languages using generated type, then convert to our type *)
  let lang_resp =
    Xrpc.Client.query knot_client ~nsid:"sh.tangled.repo.languages" ~params
      ~decoder:Tangled.Repo.Languages.output_jsont
  in
  let languages = List.map language_of_generated lang_resp.languages in

  (* Use the branch name from the response *)
  { Tangled_types.default_branch = branch_resp.name; languages }

(* Git Operations *)

let git_url _t ~knot ~did ~name =
  Printf.sprintf "https://%s/%s/%s.git" knot did name

let clone t ~repo ?dir () =
  (* Parse repo identifier: "user/name" or AT URI *)
  let did, name, knot =
    if String.starts_with ~prefix:"at://" repo then
      match Tangled_types.parse_at_uri repo with
      | Some uri -> (
          (* Need to fetch the record to get the knot *)
          let repos = list_repos t ~did:uri.did () in
          match List.find_opt (fun (rkey, _) -> rkey = uri.rkey) repos with
          | Some (_, (r : Tangled.Repo.main)) -> (uri.did, r.name, r.knot)
          | None -> failwith ("Repository not found: " ^ repo))
      | None -> failwith ("Invalid AT URI: " ^ repo)
    else
      (* user/name format *)
      match String.split_on_char '/' repo with
      | [ user; name ] -> (
          let did =
            if String.starts_with ~prefix:"did:" user then user
            else resolve_handle t user
          in
          let repos = list_repos t ~did () in
          match
            List.find_opt
              (fun (_, (r : Tangled.Repo.main)) -> r.name = name)
              repos
          with
          | Some (_, (r : Tangled.Repo.main)) -> (did, name, r.knot)
          | None -> failwith ("Repository not found: " ^ repo))
      | _ -> failwith ("Invalid repo format: " ^ repo ^ " (expected user/name)")
  in

  let url = git_url t ~knot ~did ~name in
  let target = match dir with Some d -> d | None -> name in

  (* Shell out to git *)
  let cmd =
    Printf.sprintf "git clone %s %s" (Filename.quote url)
      (Filename.quote target)
  in
  let ret = Sys.command cmd in
  if ret <> 0 then
    failwith (Printf.sprintf "git clone failed with exit code %d" ret)

(* Pipeline Operations *)

(* Helper to create a client for querying a spindle *)
let spindle_client t ~spindle =
  let service = "https://" ^ spindle in
  Xrpc_auth.Client.make_client t ~service

(* Helper to get the spindle DID from hostname *)
let spindle_did spindle = "did:web:" ^ spindle

(* Get the spindle for a repo, returns (spindle_hostname, spindle_did) *)
let get_spindle_for_repo t ~did ~repo_name =
  let repos = list_repos t ~did () in
  match
    List.find_opt (fun (_, (r : Tangled.Repo.main)) -> r.name = repo_name) repos
  with
  | Some (_, r) -> (
      match r.spindle with Some s -> Some (s, spindle_did s) | None -> None)
  | None -> None

let list_pipelines t ~spindle () =
  let client = spindle_client t ~spindle in
  let spindle_did = spindle_did spindle in
  let resp =
    Xrpc.Client.query client ~nsid:"com.atproto.repo.listRecords"
      ~params:
        [
          ("repo", spindle_did);
          ("collection", "sh.tangled.pipeline");
          ("limit", "100");
        ]
      ~decoder:Atproto.Repo.ListRecords.output_jsont
  in
  decode_list_records Tangled.Pipeline.main_jsont resp

let list_pipelines_for_repo t ~spindle ~did ~repo_name () =
  let all_pipelines = list_pipelines t ~spindle () in
  List.filter
    (fun (_, (p : Tangled.Pipeline.main)) ->
      p.trigger_metadata.repo.repo = repo_name
      && p.trigger_metadata.repo.did = did)
    all_pipelines

let list_pipeline_statuses t ~spindle ~pipeline_rkey () =
  let client = spindle_client t ~spindle in
  let spindle_did = spindle_did spindle in
  (* Construct the AT-URI for the pipeline *)
  let pipeline_uri =
    Tangled_types.make_at_uri ~did:spindle_did ~collection:"sh.tangled.pipeline"
      ~rkey:pipeline_rkey
  in
  (* List all status records *)
  let resp =
    Xrpc.Client.query client ~nsid:"com.atproto.repo.listRecords"
      ~params:
        [
          ("repo", spindle_did);
          ("collection", "sh.tangled.pipeline.status");
          ("limit", "100");
        ]
      ~decoder:Atproto.Repo.ListRecords.output_jsont
  in
  let records = decode_list_records Tangled.Pipeline.Status.main_jsont resp in
  (* Filter by pipeline URI and sort by created_at descending *)
  records
  |> List.filter (fun (_, (s : Tangled.Pipeline.Status.main)) ->
      s.pipeline = pipeline_uri)
  |> List.map snd
  |> List.sort
       (fun
         (a : Tangled.Pipeline.Status.main)
         (b : Tangled.Pipeline.Status.main)
       -> String.compare b.created_at a.created_at)
(* newest first *)

let get_pipeline_summary t ~spindle ~pipeline_rkey () =
  let statuses = list_pipeline_statuses t ~spindle ~pipeline_rkey () in
  (* Group by workflow, take most recent (first since sorted newest first) *)
  List.fold_left
    (fun acc (s : Tangled.Pipeline.Status.main) ->
      if List.mem_assoc s.workflow acc then acc else (s.workflow, s) :: acc)
    [] statuses
  |> List.rev (* Preserve workflow order *)

(* Knot Operations *)

let get_knot_version t ~knot =
  let client = knot_client t ~knot in
  Xrpc.Client.query client ~nsid:"sh.tangled.knot.version" ~params:[]
    ~decoder:Tangled.Knot.Version.output_jsont

let list_knot_keys t ~knot ?limit ?cursor () =
  let client = knot_client t ~knot in
  let params =
    List.filter_map Fun.id
      [
        Option.map (fun l -> ("limit", string_of_int l)) limit;
        Option.map (fun c -> ("cursor", c)) cursor;
      ]
  in
  Xrpc.Client.query client ~nsid:"sh.tangled.knot.listKeys" ~params
    ~decoder:Tangled.Knot.ListKeys.output_jsont

(* Profile Operations *)

let get_profile t ~did =
  let client = get_client t in
  try
    let resp =
      Xrpc.Client.query client ~nsid:"com.atproto.repo.getRecord"
        ~params:
          [
            ("repo", did);
            ("collection", "sh.tangled.actor.profile");
            ("rkey", "self");
          ]
        ~decoder:Atproto.Repo.GetRecord.output_jsont
    in
    match Jsont.Json.decode Tangled.Actor.Profile.main_jsont resp.value with
    | Ok v -> Some v
    | Error _ -> None
  with Eio.Io (Xrpc.Error.E _, _) -> None

(* Public Keys (user's SSH keys stored in their PDS) *)

let list_public_keys t ?did () =
  let client = get_client t in
  let target_did = match did with Some d -> d | None -> get_did t in
  let resp =
    Xrpc.Client.query client ~nsid:"com.atproto.repo.listRecords"
      ~params:
        [
          ("repo", target_did);
          ("collection", "sh.tangled.publicKey");
          ("limit", "100");
        ]
      ~decoder:Atproto.Repo.ListRecords.output_jsont
  in
  decode_list_records Tangled.PublicKey.main_jsont resp

(* Stars *)

let list_stars t ?did () =
  let client = get_client t in
  let target_did = match did with Some d -> d | None -> get_did t in
  let resp =
    Xrpc.Client.query client ~nsid:"com.atproto.repo.listRecords"
      ~params:
        [
          ("repo", target_did);
          ("collection", "sh.tangled.feed.star");
          ("limit", "100");
        ]
      ~decoder:Atproto.Repo.ListRecords.output_jsont
  in
  decode_list_records Tangled.Feed.Star.main_jsont resp
