(* SPDX-License-Identifier: ISC *)

module Atproto = Atp_lexicon_atproto.Com.Atproto
module Lex = Atp_lexicon_tangled.Sh.Tangled

type t = Xrpc_auth.Client.t

let create = Xrpc_auth.Client.create
let login = Xrpc_auth.Client.login
let resume = Xrpc_auth.Client.resume
let logout = Xrpc_auth.Client.logout
let get_session = Xrpc_auth.Client.get_session
let is_logged_in = Xrpc_auth.Client.is_logged_in
let get_did = Xrpc_auth.Client.get_did

let get_client t =
  if is_logged_in t then Xrpc_auth.Client.get_client t
  else Xrpc_auth.Client.make_client t ~service:(Xrpc_auth.Client.get_pds t)

let service_url service =
  let url =
    if
      String.starts_with ~prefix:"http://" service
      || String.starts_with ~prefix:"https://" service
    then service
    else "https://" ^ service
  in
  let url = Xrpc.Client.normalize_service url in
  let uri = Uriz.of_string_exn url in
  if Uriz.path uri <> "" && Uriz.path uri <> "/" then
    invalid_arg "Service must be a hostname or an HTTP(S) origin";
  url

let service_host service =
  let url = service_url service in
  let start = String.index url ':' + 3 in
  String.sub url start (String.length url - start)

let service_did service =
  "did:web:"
  ^ String.concat "%3A" (String.split_on_char ':' (service_host service))

let public_client t ~service =
  Xrpc_auth.Client.make_client t ~service:(service_url service)

let service_client t ~service ?audience ~nsid () =
  let did = get_did t in
  let service = service_url service in
  let aud = Option.value ~default:(service_did service) audience in
  let exp = Int64.to_string (Int64.add (Int64.of_float (Unix.time ())) 60L) in
  let response =
    Xrpc.Client.query (get_client t) ~nsid:"com.atproto.server.getServiceAuth"
      ~params:[ ("aud", aud); ("exp", exp); ("lxm", nsid) ]
      ~decoder:
        (Jsont.Object.map Fun.id
        |> Jsont.Object.mem "token" Jsont.string ~enc:Fun.id
        |> Jsont.Object.finish)
  in
  let client = public_client t ~service in
  let session : Xrpc.Types.session =
    {
      access_jwt = response;
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
  in
  Xrpc.Client.set_session client session;
  client

let resolve_handle t handle =
  if String.starts_with ~prefix:"did:" handle then handle
  else
    let response =
      Xrpc.Client.query (get_client t)
        ~nsid:"com.atproto.identity.resolveHandle"
        ~params:[ ("handle", handle) ]
        ~decoder:
          (Jsont.Object.map Fun.id
          |> Jsont.Object.mem "did" Jsont.string ~enc:Fun.id
          |> Jsont.Object.finish)
    in
    response

let encode codec value =
  match Jsont.Json.encode codec value with
  | Ok json -> json
  | Error message -> invalid_arg message

let decode codec json =
  match Jsont.Json.decode codec json with
  | Ok value -> value
  | Error message -> failwith message

let rkey_of_uri uri =
  match Tangled_types.parse_at_uri uri with
  | Some uri -> uri.rkey
  | None -> invalid_arg ("Invalid record URI: " ^ uri)

let list_records t ~did ~collection =
  let rec pages seen cursor acc =
    let params =
      [ ("repo", did); ("collection", collection); ("limit", "100") ]
      @ Option.to_list (Option.map (fun c -> ("cursor", c)) cursor)
    in
    let page =
      Xrpc.Client.query (get_client t) ~nsid:"com.atproto.repo.listRecords"
        ~params ~decoder:Atproto.Repo.ListRecords.output_jsont
    in
    let acc = List.rev_append page.records acc in
    match page.cursor with
    | None | Some "" -> List.rev acc
    | Some next ->
        if List.mem next seen then
          failwith "Server repeated a pagination cursor";
        pages (next :: seen) (Some next) acc
  in
  pages [] None []

let get_record t ~did ~collection ~rkey =
  try
    Some
      (Xrpc.Client.query (get_client t) ~nsid:"com.atproto.repo.getRecord"
         ~params:[ ("repo", did); ("collection", collection); ("rkey", rkey) ]
         ~decoder:Atproto.Repo.GetRecord.output_jsont)
  with
  | Eio.Io (Xrpc.Error.E (Xrpc_error { error = "RecordNotFound"; _ }), _) ->
    None

let create_record t ~collection ?rkey record =
  let input : Atproto.Repo.CreateRecord.input =
    {
      repo = get_did t;
      collection;
      rkey;
      record;
      validate = None;
      swap_commit = None;
    }
  in
  Xrpc.Client.procedure (get_client t) ~nsid:"com.atproto.repo.createRecord"
    ~params:[] ~input:(Some Atproto.Repo.CreateRecord.input_jsont)
    ~input_data:(Some input) ~decoder:Atproto.Repo.CreateRecord.output_jsont

let put_record t ~collection ~rkey ~swap_record record =
  let input : Atproto.Repo.PutRecord.input =
    {
      repo = get_did t;
      collection;
      rkey;
      record;
      validate = None;
      swap_record = Option.map Option.some swap_record;
      swap_commit = None;
    }
  in
  Xrpc.Client.procedure (get_client t) ~nsid:"com.atproto.repo.putRecord"
    ~params:[] ~input:(Some Atproto.Repo.PutRecord.input_jsont)
    ~input_data:(Some input) ~decoder:Atproto.Repo.PutRecord.output_jsont

let delete_record t ~collection ~rkey ?swap_record () =
  let input : Atproto.Repo.DeleteRecord.input =
    { repo = get_did t; collection; rkey; swap_record; swap_commit = None }
  in
  ignore
    (Xrpc.Client.procedure (get_client t) ~nsid:"com.atproto.repo.deleteRecord"
       ~params:[] ~input:(Some Atproto.Repo.DeleteRecord.input_jsont)
       ~input_data:(Some input) ~decoder:Atproto.Repo.DeleteRecord.output_jsont)

let records t ?did ~collection codec =
  let did = match did with Some did -> did | None -> get_did t in
  list_records t ~did ~collection
  |> List.map (fun (r : Atproto.Repo.ListRecords.record) ->
      (rkey_of_uri r.uri, decode codec r.value))

let list_repos t ?did () =
  records t ?did ~collection:"sh.tangled.repo" Lex.Repo.main_jsont

let list_public_keys t ?did () =
  records t ?did ~collection:"sh.tangled.publicKey" Lex.PublicKey.main_jsont

let list_stars t ?did () =
  records t ?did ~collection:"sh.tangled.feed.star" Lex.Feed.Star.main_jsont

let get_repo t ~did ~rkey =
  Option.map
    (fun (r : Atproto.Repo.GetRecord.output) ->
      decode Lex.Repo.main_jsont r.value)
    (get_record t ~did ~collection:"sh.tangled.repo" ~rkey)

let get_profile t ~did =
  Option.map
    (fun (r : Atproto.Repo.GetRecord.output) ->
      decode Lex.Actor.Profile.main_jsont r.value)
    (get_record t ~did ~collection:"sh.tangled.actor.profile" ~rkey:"self")

type repository = {
  owner : string;
  rkey : string;
  record : Lex.Repo.main;
  service : string;
}

let repo_did repository =
  match repository.record.repo_did with
  | Some did -> did
  | None -> failwith "Repository has no repo DID. Upgrade it on its knot first."

let resolve_repo t ?knot repo =
  let resolved owner rkey (record : Lex.Repo.main) =
    let service = Option.value ~default:record.knot knot in
    if service_host service <> service_host record.knot then
      failwith "Selected knot does not match the repository record";
    { owner; rkey; record; service = service_url service }
  in
  let by_record owner rkey =
    let owner = resolve_handle t owner in
    match get_repo t ~did:owner ~rkey with
    | Some record -> resolved owner rkey record
    | None -> failwith ("Repository record not found: " ^ repo)
  in
  if String.starts_with ~prefix:"at://" repo then
    match Tangled_types.parse_at_uri repo with
    | Some uri when uri.collection = "sh.tangled.repo" ->
        by_record uri.did uri.rkey
    | _ -> invalid_arg "Expected a sh.tangled.repo record URI"
  else if
    String.starts_with ~prefix:"did:" repo && not (String.contains repo '/')
  then (
    let knot =
      match knot with
      | Some knot -> knot
      | None ->
          invalid_arg "A repo DID requires --knot for authoritative lookup"
    in
    let description =
      Xrpc.Client.query
        (public_client t ~service:knot)
        ~nsid:"sh.tangled.repo.describeRepo"
        ~params:[ ("repoDid", repo) ]
        ~decoder:Lex.Repo.DescribeRepo.output_jsont
    in
    let result = by_record description.owner_did description.rkey in
    if
      result.record.repo_did <> Some repo
      || service_host result.record.knot <> service_host knot
    then failwith "Knot metadata does not match the repository record";
    result)
  else
    let owner, name =
      match String.split_on_char '/' repo with
      | [ owner; name ] -> (resolve_handle t owner, name)
      | [ name ] -> (get_did t, name)
      | _ -> invalid_arg "Expected owner/name, a record URI, or a repo DID"
    in
    let matches =
      list_repos t ~did:owner ()
      |> List.filter (fun (rkey, r) ->
          (r.Lex.Repo.name = Some name || rkey = name)
          &&
          match knot with
          | None -> true
          | Some k -> service_host r.knot = service_host k)
    in
    match matches with
    | [ (rkey, record) ] -> resolved owner rkey record
    | [] -> failwith ("Repository not found: " ^ repo)
    | _ -> failwith "Repository name is ambiguous. Use its record URI."

let now () = Ptime.to_rfc3339 (Ptime_clock.now ())

let create_repo t ~name ~knot ?audience ?description ?default_branch ?source ()
    =
  let rkey = String.lowercase_ascii name in
  if not (Atp.Record_key.is_valid rkey) then
    invalid_arg "Invalid repository name";
  if Option.is_some (get_repo t ~did:(get_did t) ~rkey) then
    failwith "A repository record already exists with this name";
  let nsid = "sh.tangled.repo.create" in
  let client = service_client t ~service:knot ?audience ~nsid () in
  let source_url = Option.map (fun r -> r.service ^ "/" ^ repo_did r) source in
  let input : Lex.Repo.Create.input =
    { rkey; name; default_branch; source = source_url; repo_did = None }
  in
  let created =
    Xrpc.Client.procedure client ~nsid ~params:[]
      ~input:(Some Lex.Repo.Create.input_jsont) ~input_data:(Some input)
      ~decoder:Lex.Repo.Create.output_jsont
  in
  let minted_did =
    match created.repo_did with
    | Some did -> did
    | None -> failwith "Knot did not return a repo DID"
  in
  let record : Lex.Repo.main =
    {
      name = Some name;
      knot = service_host knot;
      description;
      created_at = now ();
      repo_did = Some minted_did;
      spindle = None;
      website = None;
      topics = None;
      source = Option.map (fun r -> repo_did r) source;
      labels = None;
    }
  in
  (* Preserve the minted identity on an uncertain PDS write. Deleting the knot
     here could destroy a repository after a successful but timed-out write. *)
  (try
     ignore
       (create_record t ~collection:"sh.tangled.repo" ~rkey
          (encode Lex.Repo.main_jsont record))
   with Eio.Io _ as ex ->
     failwith
       (Printf.sprintf
          "Knot created %s, but PDS publication failed: %s. Retry publication \
           with this DID and rkey %s."
          minted_did (Printexc.to_string ex) rkey));
  { owner = get_did t; rkey; record; service = service_url knot }

let delete_repo t ?audience repository =
  if repository.owner <> get_did t then
    failwith "Only the owner can delete this repository";
  let repo = repo_did repository in
  (* The knot verifies that the owner's PDS record has gone before teardown. *)
  let existing =
    get_record t ~did:repository.owner ~collection:"sh.tangled.repo"
      ~rkey:repository.rkey
  in
  Option.iter
    (fun (r : Atproto.Repo.GetRecord.output) ->
      if decode Lex.Repo.main_jsont r.value <> repository.record then
        failwith "Repository changed. Resolve it again before deleting.";
      delete_record t ~collection:"sh.tangled.repo" ~rkey:repository.rkey
        ~swap_record:
          (match r.cid with
          | Some cid -> cid
          | None -> failwith "PDS did not return a record CID")
        ())
    existing;
  let nsid = "sh.tangled.repo.delete" in
  let client =
    service_client t ~service:repository.service ?audience ~nsid ()
  in
  let input : Lex.Repo.Delete.input =
    {
      repo;
      did = Some repository.owner;
      name = repository.record.name;
      rkey = Some repository.rkey;
      force = None;
    }
  in
  Xrpc.Client.procedure_unit client ~nsid ~params:[]
    ~input:(Some Lex.Repo.Delete.input_jsont) ~input_data:(Some input)

let git_url repository = repository.service ^ "/" ^ repo_did repository

let clone t ~repo ?knot ?dir () =
  let repository = resolve_repo t ?knot repo in
  let target =
    Option.value
      ~default:(Option.value ~default:repository.rkey repository.record.name)
      dir
  in
  let command =
    String.concat " "
      (List.map Filename.quote
         [ "git"; "clone"; "--"; git_url repository; target ])
  in
  let code = Sys.command command in
  if code <> 0 then
    failwith (Printf.sprintf "git clone exited with status %d" code)

let query_pipelines t ~spindle ~repo ?(commits = []) ?(kinds = []) ?(limit = 50)
    ?cursor () =
  if limit < 1 || limit > 250 then invalid_arg "Pipeline limit must be 1..250";
  let params =
    [ ("repo", repo); ("limit", string_of_int limit) ]
    @ List.map (fun x -> ("commits", x)) commits
    @ List.map (fun x -> ("kinds", x)) kinds
    @ Option.to_list (Option.map (fun x -> ("cursor", x)) cursor)
  in
  Xrpc.Client.query
    (public_client t ~service:spindle)
    ~nsid:"sh.tangled.ci.queryPipelines" ~params
    ~decoder:Lex.Ci.QueryPipelines.output_jsont

let get_pipeline t ~spindle ~pipeline =
  Xrpc.Client.query
    (public_client t ~service:spindle)
    ~nsid:"sh.tangled.ci.getPipeline"
    ~params:[ ("pipeline", pipeline) ]
    ~decoder:Lex.Ci.GetPipeline.output_jsont

let trigger_pipeline t ~spindle ?audience ~repo ~trigger ?workflows () =
  let nsid = "sh.tangled.ci.triggerPipeline" in
  let input : Lex.Ci.TriggerPipeline.input = { repo; trigger; workflows } in
  Xrpc.Client.procedure
    (service_client t ~service:spindle ?audience ~nsid ())
    ~nsid ~params:[] ~input:(Some Lex.Ci.TriggerPipeline.input_jsont)
    ~input_data:(Some input) ~decoder:Lex.Ci.TriggerPipeline.output_jsont

let cancel_pipeline t ~spindle ?audience ~repo ~pipeline ?workflows () =
  let nsid = "sh.tangled.ci.cancelPipeline" in
  let input : Lex.Ci.CancelPipeline.input = { repo; pipeline; workflows } in
  Xrpc.Client.procedure_unit
    (service_client t ~service:spindle ?audience ~nsid ())
    ~nsid ~params:[] ~input:(Some Lex.Ci.CancelPipeline.input_jsont)
    ~input_data:(Some input)
