(* SPDX-License-Identifier: ISC *)

module Atproto = Atp_lexicon_atproto.Com.Atproto
module Lex = Atp_lexicon_tangled.Org.Tangled
module Legacy = Atp_lexicon_tangled.Sh.Tangled

type t = { client : Xrpc_auth.Client.t; plc : string }

let default_plc = "https://plc.directory"

let create ~sw ~env ~app_name ?profile ?(plc = default_plc) ~pds ?http () =
  let client =
    Xrpc_auth.Client.create ~sw ~env ~app_name ?profile ~pds ?http ()
  in
  { client; plc = Xrpc.Client.normalize_service plc }

let login t = Xrpc_auth.Client.login t.client

let login_oauth t ~net ?identifier ?redirect ?scope ~open_url () =
  Xrpc_auth.Client.login_oauth t.client ~net ~plc:t.plc ?identifier ?redirect
    ?scope ~open_url ()

let service_methods =
  [
    "com.atproto.repo.createRecord";
    "com.atproto.repo.putRecord";
    "com.atproto.repo.deleteRecord";
    "org.tangled.repo.createRepo";
    "org.tangled.repo.deleteRepo";
    "org.tangled.review.submitPatch";
    "org.tangled.event.notifyEvent";
    "org.tangled.ci.cancelPipeline";
    "org.tangled.ci.approvePipeline";
    "org.tangled.ci.retryPipeline";
    "org.tangled.temp.spindle.allowList.list";
    "org.tangled.temp.spindle.allowList.add";
    "org.tangled.temp.spindle.allowList.remove";
    "com.atproto.repo.uploadBlob";
    "org.tangled.git.deleteBranch";
    "org.tangled.git.setDefaultBranch";
    "org.tangled.secret.listSecrets";
    "org.tangled.secret.addSecret";
    "org.tangled.secret.removeSecret";
    "org.tangled.webhook.listWebhooks";
    "org.tangled.webhook.createWebhook";
    "org.tangled.webhook.updateWebhook";
    "org.tangled.webhook.deleteWebhook";
    "org.tangled.webhook.listWebhookDeliveries";
    "org.tangled.webhook.retryWebhookDelivery";
    "org.tangled.temp.server.createInviteCode";
    "org.tangled.temp.server.redeemInviteCode";
    "org.tangled.temp.spindle.moderation.listBans";
    "org.tangled.temp.spindle.moderation.ban";
    "org.tangled.temp.spindle.moderation.unban";
    "org.tangled.temp.spindle.quota.list";
    "org.tangled.temp.spindle.quota.usage";
    "org.tangled.temp.spindle.quota.set";
    "org.tangled.temp.spindle.quota.unset";
  ]
let resume t = Xrpc_auth.Client.resume t.client
let logout t = Xrpc_auth.Client.logout t.client
let get_session t = Xrpc_auth.Client.get_session t.client
let is_logged_in t = Xrpc_auth.Client.is_logged_in t.client
let get_did t = Xrpc_auth.Client.get_did t.client
let plc t = t.plc

let get_client t =
  if is_logged_in t then Xrpc_auth.Client.get_client t.client
  else
    Xrpc_auth.Client.make_client t.client
      ~service:(Xrpc_auth.Client.get_pds t.client)

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

let origin url =
  let uri = Uriz.of_string_exn url in
  match (Uriz.scheme uri, Uriz.host uri) with
  | This (("http" | "https") as scheme), This host ->
      let port =
        match Uriz.port uri with This p -> ":" ^ string_of_int p | Null -> ""
      in
      scheme ^ "://" ^ host ^ port
  | _ -> invalid_arg ("Not an HTTP(S) URL: " ^ url)

let service_host service =
  let url = service_url service in
  let start = String.index url ':' + 3 in
  String.sub url start (String.length url - start)

let service_did service =
  "did:web:"
  ^ String.concat "%3A" (String.split_on_char ':' (service_host service))

let public_client t ~service =
  Xrpc_auth.Client.make_client t.client ~service:(service_url service)

let ok = function Ok v -> v | Error message -> failwith message

let service_endpoint t did ~id =
  ok
    (Xrpc.Identity.service_of_did ~plc:t.plc
       (Xrpc_auth.Client.http t.client)
       did ~id)

let pds_of t did =
  ok (Xrpc.Identity.pds_of_did ~plc:t.plc (Xrpc_auth.Client.http t.client) did)

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
    Xrpc.Client.query (get_client t) ~nsid:"com.atproto.identity.resolveHandle"
      ~params:[ ("handle", handle) ]
      ~decoder:
        (Jsont.Object.map Fun.id
        |> Jsont.Object.mem "did" Jsont.string ~enc:Fun.id
        |> Jsont.Object.finish)

let handle_of_did t did =
  let claimed =
    match
      Xrpc.Identity.document ~plc:t.plc (Xrpc_auth.Client.http t.client) did
    with
    | Error _ -> None
    | Ok text -> (
        let aka =
          Jsont.Object.map Fun.id
          |> Jsont.Object.mem "alsoKnownAs" (Jsont.list Jsont.string)
               ~dec_absent:(fun () -> []) ~enc:Fun.id
          |> Jsont.Object.skip_unknown |> Jsont.Object.finish
        in
        match Jsont_bytesrw.decode_string aka text with
        | Error _ -> None
        | Ok names ->
            List.find_map
              (fun s ->
                if String.starts_with ~prefix:"at://" s then
                  Some (String.sub s 5 (String.length s - 5))
                else None)
              names)
  in
  (* A document may claim any handle. Show it only if it names [did] back. *)
  Option.bind claimed (fun handle ->
      match resolve_handle t handle with
      | resolved when resolved = did -> Some handle
      | _ -> None
      | exception Eio.Io _ -> None)

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

(* AT Protocol datetimes must name their offset. Without [tz_offset_s] Ptime
   writes -00:00, the RFC 3339 form for an unknown offset. *)
let now () = Ptime.to_rfc3339 ~tz_offset_s:0 ~frac_s:3 (Ptime_clock.now ())

let reader t ?service _did =
  match service with
  | Some service -> public_client t ~service
  | None -> get_client t

(* The session's own records are read through its PDS. Another account's are
   read from the PDS its DID document names. *)
let home t did =
  if is_logged_in t && did = get_did t then None else Some (pds_of t did)

(* knot2 writes an absent cursor as null, which the lexicon does not allow. *)
let without_nulls = function
  | Jsont.Object (fields, meta) ->
      Jsont.Object
        ( List.filter (function _, Jsont.Null _ -> false | _ -> true) fields,
          meta )
  | json -> json

let list_records t ?service ~did ~collection () =
  let client = reader t ?service did in
  let rec pages seen cursor acc =
    let params =
      [ ("repo", did); ("collection", collection); ("limit", "100") ]
      @ Option.to_list (Option.map (fun c -> ("cursor", c)) cursor)
    in
    let page =
      Xrpc.Client.query client ~nsid:"com.atproto.repo.listRecords" ~params
        ~decoder:Jsont.json
      |> without_nulls
      |> decode Atproto.Repo.ListRecords.output_jsont
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

let get_record t ?service ~did ~collection ~rkey () =
  try
    Some
      (Xrpc.Client.query (reader t ?service did)
         ~nsid:"com.atproto.repo.getRecord"
         ~params:[ ("repo", did); ("collection", collection); ("rkey", rkey) ]
         ~decoder:Atproto.Repo.GetRecord.output_jsont)
  with
  | Eio.Io (Xrpc.Error.E (Xrpc_error { error = "RecordNotFound"; _ }), _) ->
    None

let typed collection record =
  match record with
  | Jsont.Object (fields, meta) ->
      let fields =
        List.filter (fun ((name, _), _) -> name <> "$type") fields
      in
      Jsont.Object
        ( Jsont.Json.mem ("$type", Jsont.Meta.none)
            (Jsont.Json.string collection)
          :: fields,
          meta )
  | _ -> invalid_arg "A record must be a JSON object"

let create_in ~decoder client ~repo ~collection ?rkey record =
  let input : Atproto.Repo.CreateRecord.input =
    {
      repo;
      collection;
      rkey;
      record = typed collection record;
      validate = None;
      swap_commit = None;
    }
  in
  Xrpc.Client.procedure client ~nsid:"com.atproto.repo.createRecord"
    ~params:[] ~input:(Some Atproto.Repo.CreateRecord.input_jsont)
    ~input_data:(Some input) ~decoder

let put_in client ~repo ~collection ~rkey ~swap_record record =
  let input : Atproto.Repo.PutRecord.input =
    {
      repo;
      collection;
      rkey;
      record = typed collection record;
      validate = None;
      swap_record = Option.map Option.some swap_record;
      swap_commit = None;
    }
  in
  Xrpc.Client.procedure client ~nsid:"com.atproto.repo.putRecord" ~params:[]
    ~input:(Some Atproto.Repo.PutRecord.input_jsont) ~input_data:(Some input)
    ~decoder:Atproto.Repo.PutRecord.output_jsont

let delete_in client ~repo ~collection ~rkey ~swap_record =
  let input : Atproto.Repo.DeleteRecord.input =
    { repo; collection; rkey; swap_record; swap_commit = None }
  in
  ignore
    (Xrpc.Client.procedure client ~nsid:"com.atproto.repo.deleteRecord"
       ~params:[] ~input:(Some Atproto.Repo.DeleteRecord.input_jsont)
       ~input_data:(Some input) ~decoder:Atproto.Repo.DeleteRecord.output_jsont)

let create_record t ~collection ?rkey record =
  create_in ~decoder:Atproto.Repo.CreateRecord.output_jsont (get_client t)
    ~repo:(get_did t) ~collection ?rkey record

let put_record t ~collection ~rkey ~swap_record record =
  put_in (get_client t) ~repo:(get_did t) ~collection ~rkey ~swap_record record

let delete_record t ~collection ~rkey ?swap_record () =
  delete_in (get_client t) ~repo:(get_did t) ~collection ~rkey ~swap_record

let records t ?did ~collection codec =
  let did = match did with Some did -> did | None -> get_did t in
  list_records t ?service:(home t did) ~did ~collection ()
  |> List.map (fun (r : Atproto.Repo.ListRecords.record) ->
      (rkey_of_uri r.uri, decode codec r.value))

let declaration_collection = "org.tangled.repo.declaration"
let manifest_collection = "org.tangled.repo.manifest"
let collaborator_collection = "org.tangled.graph.collaborator"
let member_collection = "org.tangled.knot.member"
let ticket_collection = "org.tangled.track.ticket"
let artifact_collection = "org.tangled.repo.artifact"

let list_declarations t ?did () =
  records t ?did ~collection:declaration_collection
    Lex.Repo.Declaration.main_jsont

let list_ssh_keys t ?did () =
  records t ?did ~collection:"org.tangled.key.sshKey" Lex.Key.SshKey.main_jsont

let list_stars t ?did () =
  records t ?did ~collection:"org.tangled.feed.star" Lex.Feed.Star.main_jsont

let get_profile t ~did =
  Option.map
    (fun (r : Atproto.Repo.GetRecord.output) ->
      decode Lex.Actor.Profile.main_jsont r.value)
    (get_record t ?service:(home t did) ~did
       ~collection:"org.tangled.actor.profile" ~rkey:"self" ())

type repository = {
  did : string;
  owner : string;
  name : string;
  knot : string;
  manifest : Lex.Repo.Manifest.main;
  manifest_value : Jsont.json;
  manifest_cid : string option;
}

let slug name = String.lowercase_ascii name

let knot_of_repo t did =
  let endpoint =
    match service_endpoint t did ~id:"#tangled_knot" with
    | endpoint -> endpoint
    | exception Failure _ -> service_endpoint t did ~id:"#atproto_pds"
  in
  origin endpoint

let declared_repo t ~owner ~slug =
  Option.map
    (fun (r : Atproto.Repo.GetRecord.output) ->
      (decode Lex.Repo.Declaration.main_jsont r.value).repo)
    (get_record t ?service:(home t owner) ~did:owner
       ~collection:declaration_collection ~rkey:slug ())

let by_did t ?knot did =
  let knot =
    match knot with Some k -> service_url k | None -> knot_of_repo t did
  in
  let record =
    match
      get_record t ~service:knot ~did ~collection:manifest_collection
        ~rkey:"self" ()
    with
    | Some record -> record
    | None -> failwith ("Knot has no manifest for " ^ did)
  in
  let manifest = decode Lex.Repo.Manifest.main_jsont record.value in
  let declaration =
    decode Lex.Repo.Manifest.declaration_jsont manifest.declaration
  in
  (* A repository is listed under owner/name only while the owner's
     declaration names it back. *)
  if declared_repo t ~owner:declaration.owner ~slug:(slug declaration.name)
     <> Some did
  then failwith "The owner does not declare this repository";
  {
    did;
    owner = declaration.owner;
    name = declaration.name;
    knot;
    manifest;
    manifest_value = record.value;
    manifest_cid = record.cid;
  }

let resolve_repo t ?knot spec =
  let by_name owner name =
    let owner = resolve_handle t owner in
    match declared_repo t ~owner ~slug:(slug name) with
    | None -> failwith ("Repository not found: " ^ spec)
    | Some did ->
        let r = by_did t ?knot did in
        if r.owner <> owner || slug r.name <> slug name then
          failwith "Repository manifest names another owner or name";
        r
  in
  if String.starts_with ~prefix:"at://" spec then
    match Tangled_types.parse_at_uri spec with
    | Some uri when uri.collection = declaration_collection ->
        by_name uri.did uri.rkey
    | _ -> invalid_arg ("Expected a " ^ declaration_collection ^ " URI")
  else if
    String.starts_with ~prefix:"did:" spec && not (String.contains spec '/')
  then by_did t ?knot spec
  else
    match String.split_on_char '/' spec with
    | [ owner; name ] -> by_name owner name
    | [ name ] -> by_name (get_did t) name
    | _ -> invalid_arg "Expected owner/name, a declaration URI or a repo DID"

let valid_name name =
  name <> ""
  && String.length name <= 100
  && name <> "self"
  && (not (String.starts_with ~prefix:"." name))
  && (not (String.ends_with ~suffix:"." name))
  && (not
        (let rec dots i =
           i + 1 < String.length name
           && ((name.[i] = '.' && name.[i + 1] = '.') || dots (i + 1))
         in
         dots 0))
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '_' | '-' -> true
         | _ -> false)
       name

let source_tangled did =
  Jsont.Json.object'
    [
      Jsont.Json.mem ("$type", Jsont.Meta.none)
        (Jsont.Json.string "org.tangled.repo.defs#sourceTangled");
      Jsont.Json.mem ("did", Jsont.Meta.none) (Jsont.Json.string did);
    ]

let source_external uri =
  Jsont.Json.object'
    [
      Jsont.Json.mem ("$type", Jsont.Meta.none)
        (Jsont.Json.string "org.tangled.repo.defs#sourceExternal");
      Jsont.Json.mem ("uri", Jsont.Meta.none) (Jsont.Json.string uri);
    ]

let create_repo t ~name ~knot ?audience ?description ?default_branch ?spindle
    ?source ?import () =
  if not (valid_name name) then invalid_arg "Invalid repository name";
  let owner = get_did t in
  if declared_repo t ~owner ~slug:(slug name) <> None then
    failwith "A repository is already declared with this name";
  let nsid = "org.tangled.repo.createRepo" in
  let input : Lex.Repo.CreateRepo.input =
    {
      name;
      created_at = Some (now ());
      default_branch;
      description;
      did = None;
      source =
        (match (source, import) with
        | Some _, Some _ -> invalid_arg "A repository is a fork or an import"
        | Some r, None -> Some (source_tangled r.did)
        | None, Some uri -> Some (source_external uri)
        | None, None -> None);
      spindle = Option.map service_did spindle;
      topics = None;
      website = None;
    }
  in
  let created =
    Xrpc.Client.procedure
      (service_client t ~service:knot ?audience ~nsid ())
      ~nsid ~params:[] ~input:(Some Lex.Repo.CreateRepo.input_jsont)
      ~input_data:(Some input) ~decoder:Lex.Repo.CreateRepo.output_jsont
  in
  let did =
    match created.did with
    | Some did -> did
    | None -> failwith "Knot did not return a repo DID"
  in
  let declaration : Lex.Repo.Declaration.main =
    { repo = did; created_at = now () }
  in
  (* Keep the minted repository on an uncertain PDS write. Deleting it here
     could destroy a repository after a successful but timed-out write. *)
  (try
     ignore
       (create_record t ~collection:declaration_collection ~rkey:(slug name)
          (encode Lex.Repo.Declaration.main_jsont declaration))
   with Eio.Io _ as ex ->
     failwith
       (Printf.sprintf
          "Knot created %s, but declaring it on the PDS failed: %s. Write an \
           %s record with rkey %s naming this DID."
          did (Printexc.to_string ex) declaration_collection (slug name)));
  by_did t ~knot did

let require_owner t repository =
  if repository.owner <> get_did t then
    failwith "Only the repository owner can do this"

let delete_repo t ?audience repository =
  require_owner t repository;
  (* The knot refuses to delete while the owner still declares the repo. *)
  (match
     get_record t ~did:repository.owner ~collection:declaration_collection
       ~rkey:(slug repository.name) ()
   with
  | None -> ()
  | Some r ->
      if (decode Lex.Repo.Declaration.main_jsont r.value).repo <> repository.did
      then failwith "Declaration changed. Resolve the repository again.";
      delete_record t ~collection:declaration_collection
        ~rkey:(slug repository.name) ?swap_record:r.cid ());
  let nsid = "org.tangled.repo.deleteRepo" in
  let input : Lex.Repo.DeleteRepo.input =
    { repo = repository.did; force = None }
  in
  Xrpc.Client.procedure_unit
    (service_client t ~service:repository.knot ?audience ~nsid ())
    ~nsid ~params:[] ~input:(Some Lex.Repo.DeleteRepo.input_jsont)
    ~input_data:(Some input)

let git_url repository = repository.knot ^ "/" ^ repository.did

let clone t ~repo ?knot ?dir () =
  let repository = resolve_repo t ?knot repo in
  let target = Option.value ~default:repository.name dir in
  let command =
    String.concat " "
      (List.map Filename.quote
         [ "git"; "clone"; "--"; git_url repository; target ])
  in
  let code = Sys.command command in
  if code <> 0 then
    failwith (Printf.sprintf "git clone exited with status %d" code)

(* Records in a repository account are written through its knot, with a token
   whose audience is the repository DID rather than the knot. *)
let repo_client t repository nsid =
  service_client t ~service:repository.knot ~audience:repository.did ~nsid ()

let repo_create ~decoder t repository ~collection ?rkey record =
  create_in ~decoder
    (repo_client t repository "com.atproto.repo.createRecord")
    ~repo:repository.did ~collection ?rkey record

(* A knot answers a write with the record's reference and the change that
   made it. *)
type write = { uri : string; cid : string; change : string option }

let write_jsont =
  Jsont.Object.map (fun uri cid change -> { uri; cid; change })
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun w -> w.uri)
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun w -> w.cid)
  |> Jsont.Object.opt_mem "x-tngl-change" Jsont.string ~enc:(fun w -> w.change)
  |> Jsont.Object.skip_unknown |> Jsont.Object.finish

let attestation_collection = "org.tangled.graph.attestation"

(* The knot says who wrote a record in a repository account. The record is
   tied to its author only once the author says so too, with an attestation
   in their own account. The write has landed by then, so a failure to
   attest is reported rather than raised. *)
let attest t (w : write) =
  let mem k v = Jsont.Json.mem (k, Jsont.Meta.none) v in
  let record =
    Jsont.Json.object'
      ([
         mem "subject"
           (Jsont.Json.object'
              [
                mem "uri" (Jsont.Json.string w.uri);
                mem "cid" (Jsont.Json.string w.cid);
              ]);
         mem "createdAt" (Jsont.Json.string (now ()));
       ]
      @ Option.to_list
          (Option.map (fun c -> mem "change" (Jsont.Json.string c)) w.change))
  in
  match create_record t ~collection:attestation_collection record with
  | _ -> ()
  | exception ((Eio.Io _ | Failure _) as ex) ->
      Logs.warn (fun m ->
          m "The knot wrote %s, but attesting it failed: %s" w.uri
            (Printexc.to_string ex))

let repo_create_record t repository ~collection ?rkey record =
  let json =
    repo_create ~decoder:Jsont.json t repository ~collection ?rkey record
  in
  attest t (decode write_jsont json);
  decode Atproto.Repo.CreateRecord.output_jsont json

let repo_put_record t repository ~collection ~rkey ~swap_record record =
  let output =
    put_in
      (repo_client t repository "com.atproto.repo.putRecord")
      ~repo:repository.did ~collection ~rkey ~swap_record:(Some swap_record)
      record
  in
  attest t { uri = output.uri; cid = output.cid; change = None };
  output

let repo_delete_record t repository ~collection ~rkey ~swap_record =
  delete_in
    (repo_client t repository "com.atproto.repo.deleteRecord")
    ~repo:repository.did ~collection ~rkey ~swap_record:(Some swap_record)

let repo_records t repository ~collection =
  list_records t ~service:repository.knot ~did:repository.did ~collection ()

let repo_record t repository ~collection ~rkey =
  get_record t ~service:repository.knot ~did:repository.did ~collection ~rkey
    ()

let set_field name value = function
  | Jsont.Object (fields, meta) ->
      let fields = List.filter (fun ((n, _), _) -> n <> name) fields in
      let fields =
        match value with
        | None -> fields
        | Some v -> Jsont.Json.mem (name, Jsont.Meta.none) v :: fields
      in
      Jsont.Object (fields, meta)
  | _ -> invalid_arg "A record must be a JSON object"

(* The knot stamps [x-tngl-editor] on records it serves. It is not part of
   the record and must not be written back. *)
let strip_stamps = function
  | Jsont.Object (fields, meta) ->
      Jsont.Object
        ( List.filter
            (fun ((n, _), _) -> not (String.starts_with ~prefix:"x-tngl-" n))
            fields,
          meta )
  | v -> v

let set_spindle t repository spindle =
  require_owner t repository;
  let cid =
    match repository.manifest_cid with
    | Some cid -> cid
    | None -> failwith "Knot did not return a manifest CID"
  in
  let value =
    strip_stamps repository.manifest_value
    |> set_field "spindle"
         (Option.map (fun s -> Jsont.Json.string (service_did s)) spindle)
  in
  ignore
    (repo_put_record t repository ~collection:manifest_collection ~rkey:"self"
       ~swap_record:cid value)

let collaborators t repository =
  repo_records t repository ~collection:collaborator_collection
  |> List.map (fun (r : Atproto.Repo.ListRecords.record) -> rkey_of_uri r.uri)

let add_collaborator t repository subject =
  require_owner t repository;
  let record : Lex.Graph.Collaborator.main = { created_at = now () } in
  (* The knot answers a grant with an empty object, not a record reference. *)
  ignore
    (repo_create ~decoder:Jsont.json t repository
       ~collection:collaborator_collection ~rkey:subject
       (encode Lex.Graph.Collaborator.main_jsont record))

let cid_of (r : Atproto.Repo.GetRecord.output) =
  match r.cid with
  | Some cid -> cid
  | None -> failwith "Server did not return a record CID"

let remove_collaborator t repository subject =
  require_owner t repository;
  match
    repo_record t repository ~collection:collaborator_collection ~rkey:subject
  with
  | None -> failwith "Not a collaborator"
  | Some r ->
      repo_delete_record t repository ~collection:collaborator_collection
        ~rkey:subject ~swap_record:(cid_of r)

let knot_members t ~knot =
  (* The knot's own repository exists only once it holds a record. *)
  match
    list_records t ~service:(service_url knot) ~did:(service_did knot)
      ~collection:member_collection ()
  with
  | records ->
      List.map
        (fun (r : Atproto.Repo.ListRecords.record) -> rkey_of_uri r.uri)
        records
  | exception
      Eio.Io (Xrpc.Error.E (Xrpc_error { error = "RepoNotFound"; _ }), _) ->
      []

let knot_client t ~knot ?audience nsid =
  service_client t ~service:knot ?audience ~nsid ()

let add_knot_member t ~knot ?audience subject =
  let record : Lex.Knot.Member.main = { created_at = now () } in
  ignore
    (create_in ~decoder:Jsont.json
       (knot_client t ~knot ?audience "com.atproto.repo.createRecord")
       ~repo:(service_did knot) ~collection:member_collection ~rkey:subject
       (encode Lex.Knot.Member.main_jsont record))

let remove_knot_member t ~knot ?audience subject =
  let did = service_did knot in
  match
    get_record t ~service:(service_url knot) ~did
      ~collection:member_collection ~rkey:subject ()
  with
  | None -> failwith "Not a member"
  | Some r ->
      delete_in
        (knot_client t ~knot ?audience "com.atproto.repo.deleteRecord")
        ~repo:did ~collection:member_collection ~rkey:subject
        ~swap_record:(Some (cid_of r))

let markdown ?(inline = false) text =
  Jsont.Json.object'
    [
      Jsont.Json.mem ("$type", Jsont.Meta.none)
        (Jsont.Json.string
           (if inline then "org.tangled.markup.markdown#inline"
            else "org.tangled.markup.markdown"));
      Jsont.Json.mem ("text", Jsont.Meta.none) (Jsont.Json.string text);
    ]

let tickets t repository =
  repo_records t repository ~collection:ticket_collection
  |> List.map (fun (r : Atproto.Repo.ListRecords.record) ->
      (rkey_of_uri r.uri, decode Lex.Track.Ticket.main_jsont r.value))

let ticket t repository rkey =
  Option.map
    (fun (r : Atproto.Repo.GetRecord.output) ->
      decode Lex.Track.Ticket.main_jsont r.value)
    (repo_record t repository ~collection:ticket_collection ~rkey)

let label_tokens keys =
  List.map
    (fun key ->
      Jsont.Json.object'
        [ Jsont.Json.mem ("key", Jsont.Meta.none) (Jsont.Json.string key) ])
    keys

let open_ticket t repository ~title ?body ?target_branch ?(labels = []) () =
  let ticket : Lex.Track.Ticket.main =
    {
      title = markdown ~inline:true title;
      body = Option.map markdown body;
      state = Jsont.Json.string "open";
      created_at = now ();
      target_branch;
      assignees = None;
      labels = None;
      tangled_labels =
        (match labels with [] -> None | keys -> Some (label_tokens keys));
    }
  in
  repo_create_record t repository ~collection:ticket_collection
    (encode Lex.Track.Ticket.main_jsont ticket)

(* Labels *)

let label_collection = "org.tangled.label.definition"

type label = { key : string; definition : Lex.Label.Definition.main }

let labels t repository =
  repo_records t repository ~collection:label_collection
  |> List.filter_map (fun (r : Atproto.Repo.ListRecords.record) ->
      match decode Lex.Label.Definition.main_jsont r.value with
      | definition -> Some { key = rkey_of_uri r.uri; definition }
      | exception Failure _ -> None)

let ticket_labels (ticket : Lex.Track.Ticket.main) =
  List.filter_map
    (fun json ->
      match decode Lex.Label.Defs.label_token_jsont json with
      | token -> Some token.key
      | exception Failure _ -> None)
    (Option.value ~default:[] ticket.tangled_labels)

let set_labels keys value =
  set_field "tangledLabels"
    (match keys with
    | [] -> None
    | l -> Some (Jsont.Json.list (label_tokens l)))
    value

(* The knot refuses to store a ticket that carries a deleted label, so every
   write of a ticket keeps only the labels still defined. *)
let live_labels t repository ticket =
  match ticket_labels ticket with
  | [] -> []
  | keys ->
      let defined = List.map (fun l -> l.key) (labels t repository) in
      List.filter (fun k -> List.mem k defined) keys

let drop_deleted t repository ticket value =
  let live = live_labels t repository ticket in
  if live = ticket_labels ticket then value else set_labels live value

let set_ticket_state t repository ~rkey state =
  if not (List.mem state [ "open"; "closed"; "completed" ]) then
    invalid_arg "Ticket state must be open, closed or completed";
  match repo_record t repository ~collection:ticket_collection ~rkey with
  | None -> failwith "Ticket not found"
  | Some r ->
      let ticket = decode Lex.Track.Ticket.main_jsont r.value in
      let value =
        strip_stamps r.value
        |> set_field "state" (Some (Jsont.Json.string state))
        |> drop_deleted t repository ticket
      in
      ignore
        (repo_put_record t repository ~collection:ticket_collection ~rkey
           ~swap_record:(cid_of r) value)

let submit_patch t repository ~ticket ~base ~head ?source ?branch () =
  let nsid = "org.tangled.review.submitPatch" in
  let oid value =
    Jsont.Json.object'
      [ Jsont.Json.mem ("oid", Jsont.Meta.none) (Jsont.Json.string value) ]
  in
  let mem k v = Jsont.Json.mem (k, Jsont.Meta.none) v in
  let patch =
    Jsont.Json.object'
      ([
         mem "ticket" (Jsont.Json.string ticket);
         mem "base" (oid base);
         mem "head" (oid head);
         mem "sourceRepo"
           (Jsont.Json.string (Option.value ~default:repository.did source));
       ]
      @ Option.to_list
          (Option.map
             (fun b -> mem "sourceBranch" (Jsont.Json.string b))
             branch)
      )
  in
  Xrpc.Client.procedure
    (repo_client t repository nsid)
    ~nsid ~params:[] ~input:(Some Jsont.json)
    ~input_data:
      (Some
         (Jsont.Json.object'
            [
              mem "repo" (Jsont.Json.string repository.did);
              mem "patch" patch;
            ]))
    ~decoder:Lex.Review.SubmitPatch.output_jsont

let list_artifacts t repository =
  repo_records t repository ~collection:artifact_collection
  |> List.filter_map (fun (r : Atproto.Repo.ListRecords.record) ->
      match decode Lex.Repo.Artifact.main_jsont r.value with
      | exception Failure _ -> None
      | artifact -> Some (rkey_of_uri r.uri, artifact))

let list_legacy_artifacts t ~did ~repo =
  let names = Hashtbl.create 8 in
  let repo_name rkey =
    match Hashtbl.find_opt names rkey with
    | Some name -> name
    | None ->
        let name =
          match
            get_record t ~did ~collection:"sh.tangled.repo" ~rkey ()
          with
          | exception Failure _ -> None
          | None -> None
          | Some r -> (
              match decode Legacy.Repo.main_jsont r.value with
              | exception Failure _ -> None
              | { name = Some name; _ } -> Some name
              | { name = None; _ } -> Some rkey)
        in
        Hashtbl.add names rkey name;
        name
  in
  List.filter_map
    (fun (r : Atproto.Repo.ListRecords.record) ->
      match decode Legacy.Repo.Artifact.main_jsont r.value with
      | exception Failure _ -> None
      | artifact -> (
          match Option.bind artifact.repo Tangled_types.parse_at_uri with
          | Some { did = owner; rkey; _ }
            when owner = did && repo_name rkey = Some repo ->
              Some (rkey_of_uri r.uri, artifact)
          | _ -> None))
    (list_records t ~did ~collection:"sh.tangled.repo.artifact" ())

let archive_suffixes =
  [ ".tbz"; ".tar.bz2"; ".tar.gz"; ".tgz"; ".tar.xz"; ".zip" ]

let artifact_version name =
  let base =
    List.find_map
      (fun suffix ->
        if String.ends_with ~suffix name then
          Some (String.sub name 0 (String.length name - String.length suffix))
        else None)
      archive_suffixes
  in
  match base with
  | None -> None
  | Some base ->
      let n = String.length base in
      let rec first_dash_digit i =
        if i + 1 >= n then None
        else if base.[i] = '-' && base.[i + 1] >= '0' && base.[i + 1] <= '9'
        then Some (String.sub base (i + 1) (n - i - 1))
        else first_dash_digit (i + 1)
      in
      first_dash_digit 0

(* Spindles *)

let spindle_url t spindle =
  if String.starts_with ~prefix:"did:" spindle then
    origin (service_endpoint t spindle ~id:"#tangled_spindle")
  else service_url spindle

let spindle_of t ?spindle repository =
  match (spindle, repository.manifest.spindle) with
  | Some s, _ -> spindle_url t s
  | None, Some did -> spindle_url t did
  | None, None -> failwith "Repository has no spindle. Supply --spindle."

let query_pipelines t ~spindle ~repo ?(commits = []) ?(triggers = [])
    ?(limit = 50) ?cursor () =
  if limit < 1 || limit > 250 then invalid_arg "Pipeline limit must be 1..250";
  let params =
    [ ("repo", repo); ("limit", string_of_int limit) ]
    @ List.map (fun x -> ("commits", x)) commits
    @ List.map (fun x -> ("triggers", x)) triggers
    @ Option.to_list (Option.map (fun x -> ("cursor", x)) cursor)
  in
  Xrpc.Client.query
    (public_client t ~service:spindle)
    ~nsid:"org.tangled.ci.queryPipelines" ~params
    ~decoder:Lex.Ci.QueryPipelines.output_jsont

let get_pipeline t ~spindle ~repo ~pipeline =
  Xrpc.Client.query
    (public_client t ~service:spindle)
    ~nsid:"org.tangled.ci.getPipeline"
    ~params:[ ("repo", repo); ("pipeline", pipeline) ]
    ~decoder:Lex.Ci.Defs.pipeline_jsont

let workflow_runs (p : Lex.Ci.Defs.pipeline) =
  List.map (decode Lex.Ci.Defs.workflow_run_jsont) p.workflows

let ci_output (output : Lex.Event.NotifyEvent.output) =
  match output.ci with
  | None -> { Lex.Event.NotifyEvent.pipelines = None; message = None }
  | Some ci -> decode Lex.Event.NotifyEvent.ci_output_jsont ci

let trigger_pipeline t ~spindle ?audience ~repo ~commit ?ref_ ?workflows
    ?(inputs = []) () =
  let nsid = "org.tangled.event.notifyEvent" in
  let manual : Lex.Ci.Trigger.Manual.main =
    {
      repo;
      commit;
      ref_;
      workflows;
      inputs =
        (match inputs with
        | [] -> None
        | inputs ->
            Some
              (List.map
                 (fun (key, value) ->
                   Jsont.Json.object'
                     [
                       Jsont.Json.mem ("key", Jsont.Meta.none)
                         (Jsont.Json.string key);
                       Jsont.Json.mem ("value", Jsont.Meta.none)
                         (Jsont.Json.string value);
                     ])
                 inputs));
    }
  in
  let input : Lex.Event.NotifyEvent.input =
    {
      repo;
      event =
        typed "org.tangled.ci.trigger.manual"
          (encode Lex.Ci.Trigger.Manual.main_jsont manual);
    }
  in
  ci_output
    (Xrpc.Client.procedure
       (service_client t ~service:spindle ?audience ~nsid ())
       ~nsid ~params:[] ~input:(Some Lex.Event.NotifyEvent.input_jsont)
       ~input_data:(Some input) ~decoder:Lex.Event.NotifyEvent.output_jsont)

let spindle_procedure t ~spindle ?audience ~nsid input =
  Xrpc.Client.procedure
    (service_client t ~service:spindle ?audience ~nsid ())
    ~nsid ~params:[] ~input:(Some Jsont.json) ~input_data:(Some input)
    ~decoder:Jsont.json

let pipeline_input ~repo ~pipeline ?workflows () =
  Jsont.Json.object'
    ([
       Jsont.Json.mem ("repo", Jsont.Meta.none) (Jsont.Json.string repo);
       Jsont.Json.mem ("pipeline", Jsont.Meta.none)
         (Jsont.Json.string pipeline);
     ]
    @ Option.to_list
        (Option.map
           (fun ws ->
             Jsont.Json.mem ("workflows", Jsont.Meta.none)
               (Jsont.Json.list (List.map Jsont.Json.string ws)))
           workflows))

let cancel_pipeline t ~spindle ?audience ~repo ~pipeline ?workflows () =
  ignore
    (spindle_procedure t ~spindle ?audience
       ~nsid:"org.tangled.ci.cancelPipeline"
       (pipeline_input ~repo ~pipeline ?workflows ()))

let approve_pipeline t ~spindle ?audience ~repo ~pipeline () =
  ignore
    (spindle_procedure t ~spindle ?audience
       ~nsid:"org.tangled.ci.approvePipeline"
       (pipeline_input ~repo ~pipeline ()))

let retry_pipeline t ~spindle ?audience ~repo ~pipeline ?workflows () =
  let output =
    spindle_procedure t ~spindle ?audience ~nsid:"org.tangled.ci.retryPipeline"
      (pipeline_input ~repo ~pipeline ?workflows ())
  in
  (decode Lex.Ci.RetryPipeline.output_jsont output).pipeline

let workflow_definitions t ~spindle ~repo ?ref_ () =
  Xrpc.Client.query
    (public_client t ~service:spindle)
    ~nsid:"org.tangled.ci.listWorkflowDefinitions"
    ~params:
      ([ ("repo", repo) ]
      @ Option.to_list (Option.map (fun r -> ("ref", r)) ref_))
    ~decoder:Lex.Ci.ListWorkflowDefinitions.output_jsont

let allow_list t ~spindle ?audience () =
  let nsid = "org.tangled.temp.spindle.allowList.list" in
  Xrpc.Client.query
    (service_client t ~service:spindle ?audience ~nsid ())
    ~nsid ~params:[] ~decoder:Jsont.json

let allow_list_change t ~spindle ?audience ~add did =
  let nsid =
    "org.tangled.temp.spindle.allowList." ^ if add then "add" else "remove"
  in
  ignore
    (spindle_procedure t ~spindle ?audience ~nsid
       (Jsont.Json.object'
          [ Jsont.Json.mem ("did", Jsont.Meta.none) (Jsont.Json.string did) ]))

(* Git *)

let param name = Option.map (fun v -> (name, v))
let int_param name = Option.map (fun v -> (name, string_of_int v))

let knot_query t repository ~nsid params decoder =
  Xrpc.Client.query
    (public_client t ~service:repository.knot)
    ~nsid
    ~params:(("repo", repository.did) :: List.filter_map Fun.id params)
    ~decoder

let not_found names = function
  | Eio.Io (Xrpc.Error.E (Xrpc_error { error; _ }), _) -> List.mem error names
  | _ -> false

let branches t repository ?limit ?cursor () =
  knot_query t repository ~nsid:"org.tangled.git.listBranches"
    [ int_param "limit" limit; param "cursor" cursor ]
    Lex.Git.ListBranches.output_jsont

let branch t repository name =
  match
    knot_query t repository ~nsid:"org.tangled.git.getBranch"
      [ Some ("name", name) ]
      Lex.Git.GetBranch.output_jsont
  with
  | b -> Some b
  | exception ex when not_found [ "BranchNotFound"; "RefNotFound" ] ex -> None

let default_branch t repository =
  (branches t repository ~limit:1 ()).default_branch

let tags t repository ?limit ?cursor () =
  knot_query t repository ~nsid:"org.tangled.git.listTags"
    [ int_param "limit" limit; param "cursor" cursor ]
    Lex.Git.ListTags.output_jsont

let commits t repository ~ref_ ?path ?limit ?cursor () =
  knot_query t repository ~nsid:"org.tangled.git.listCommits"
    [
      Some ("ref", ref_);
      param "path" path;
      int_param "limit" limit;
      param "cursor" cursor;
    ]
    Lex.Git.ListCommits.output_jsont

let tree t repository ?ref_ ?path () =
  knot_query t repository ~nsid:"org.tangled.git.getTree"
    [ param "ref" ref_; param "path" path ]
    Lex.Git.GetTree.output_jsont

let blob t repository ?ref_ ~path () =
  fst
    (Xrpc.Client.query_bytes
       (public_client t ~service:repository.knot)
       ~nsid:"org.tangled.git.getBlob"
       ~params:
         ([ ("repo", repository.did); ("path", path) ]
         @ Option.to_list (param "ref" ref_)))

let diff t repository ~base ~head ?paths () =
  knot_query t repository ~nsid:"org.tangled.git.getDiff"
    [
      Some ("base", base);
      Some ("head", head);
      Option.map (fun p -> ("paths", String.concat "," p)) paths;
    ]
    Lex.Git.GetDiff.output_jsont

(* Branch changes are addressed to the repository, as its records are. *)
let branch_procedure t repository ~nsid branch =
  Xrpc.Client.procedure
    (repo_client t repository nsid)
    ~nsid ~params:[] ~input:(Some Jsont.json)
    ~input_data:
      (Some
         (Jsont.Json.object'
            [
              Jsont.Json.mem ("repo", Jsont.Meta.none)
                (Jsont.Json.string repository.did);
              Jsont.Json.mem
                ("branch", Jsont.Meta.none)
                (Jsont.Json.string branch);
            ]))
    ~decoder:Jsont.json

let delete_branch t repository branch =
  ignore
    (branch_procedure t repository ~nsid:"org.tangled.git.deleteBranch" branch)

let set_default_branch t repository branch =
  ignore
    (branch_procedure t repository ~nsid:"org.tangled.git.setDefaultBranch"
       branch)

let import_status t repository =
  knot_query t repository ~nsid:"org.tangled.repo.getImportStatus" []
    Lex.Repo.GetImportStatus.output_jsont

(* Record history *)

let record_versions t repository ~uri =
  let client = public_client t ~service:repository.knot in
  let rec pages cursor acc =
    let page =
      Xrpc.Client.query client ~nsid:"org.tangled.repo.listRecordVersions"
        ~params:
          ([ ("uri", uri); ("limit", "100") ]
          @ Option.to_list (param "cursor" cursor))
        ~decoder:Jsont.json
      |> without_nulls
      |> decode Lex.Repo.ListRecordVersions.output_jsont
    in
    let acc =
      List.rev_append
        (List.map
           (decode Lex.Repo.ListRecordVersions.version_jsont)
           page.versions)
        acc
    in
    match page.cursor with
    | Some next when next <> "" && Some next <> cursor -> pages (Some next) acc
    | _ -> List.rev acc
  in
  pages None []

(* Comments *)

let comment_collection = "org.tangled.feed.comment"

type comment = {
  rkey : string;
  author : string option;
  comment : Lex.Feed.Comment.main;
}

let editor = function
  | Jsont.Object (fields, _) -> (
      match List.find_opt (fun ((n, _), _) -> n = "x-tngl-editor") fields with
      | Some (_, Jsont.String (did, _)) -> Some did
      | _ -> None)
  | _ -> None

let uri_of strong_ref =
  match strong_ref with
  | Jsont.Object (fields, _) -> (
      match List.find_opt (fun ((n, _), _) -> n = "uri") fields with
      | Some (_, Jsont.String (uri, _)) -> Some uri
      | _ -> None)
  | _ -> None

let ticket_uri repository rkey =
  Printf.sprintf "at://%s/%s/%s" repository.did ticket_collection rkey

let comments t repository ~ticket =
  let subject = ticket_uri repository ticket in
  repo_records t repository ~collection:comment_collection
  |> List.filter_map (fun (r : Atproto.Repo.ListRecords.record) ->
      match decode Lex.Feed.Comment.main_jsont r.value with
      | exception Failure _ -> None
      | c when uri_of c.subject = Some subject ->
          Some
            { rkey = rkey_of_uri r.uri; author = editor r.value; comment = c }
      | _ -> None)
  |> List.stable_sort (fun a b ->
      String.compare a.comment.created_at b.comment.created_at)

let strong_ref ~uri ~cid =
  Jsont.Json.object'
    [
      Jsont.Json.mem ("$type", Jsont.Meta.none)
        (Jsont.Json.string "com.atproto.repo.strongRef");
      Jsont.Json.mem ("uri", Jsont.Meta.none) (Jsont.Json.string uri);
      Jsont.Json.mem ("cid", Jsont.Meta.none) (Jsont.Json.string cid);
    ]

let add_comment t repository ~ticket body =
  match repo_record t repository ~collection:ticket_collection ~rkey:ticket with
  | None -> failwith "Ticket not found"
  | Some r ->
      let comment : Lex.Feed.Comment.main =
        {
          subject = strong_ref ~uri:r.uri ~cid:(cid_of r);
          reply_to = None;
          body = markdown body;
          embed = None;
          created_at = now ();
        }
      in
      repo_create_record t repository ~collection:comment_collection
        (encode Lex.Feed.Comment.main_jsont comment)

(* Tickets *)

let ticket_author t repository rkey =
  Option.bind (repo_record t repository ~collection:ticket_collection ~rkey)
    (fun (r : Atproto.Repo.GetRecord.output) -> editor r.value)

(* [revise before ~add ~remove] is [before] without [remove], then with those
   of [add] it lacked, in order. *)
let revise before ~add ~remove =
  let kept = List.filter (fun x -> not (List.mem x remove)) before in
  kept @ List.filter (fun x -> not (List.mem x kept)) add

let edit_ticket t repository ~rkey ?title ?body ?(assign = []) ?(unassign = [])
    ?(label = []) ?(unlabel = []) () =
  match repo_record t repository ~collection:ticket_collection ~rkey with
  | None -> failwith "Ticket not found"
  | Some r ->
      let current = decode Lex.Track.Ticket.main_jsont r.value in
      let live = live_labels t repository current in
      let value = strip_stamps r.value in
      let value =
        match title with
        | None -> value
        | Some s -> set_field "title" (Some (markdown ~inline:true s)) value
      in
      let value =
        match body with
        | None -> value
        | Some "" -> set_field "body" None value
        | Some s -> set_field "body" (Some (markdown s)) value
      in
      let value =
        if assign = [] && unassign = [] then value
        else
          set_field "assignees"
            (match
               revise
                 (Option.value ~default:[] current.assignees)
                 ~add:assign ~remove:unassign
             with
            | [] -> None
            | l -> Some (Jsont.Json.list (List.map Jsont.Json.string l)))
            value
      in
      let value =
        let keys = revise live ~add:label ~remove:unlabel in
        if keys = ticket_labels current then value else set_labels keys value
      in
      ignore
        (repo_put_record t repository ~collection:ticket_collection ~rkey
           ~swap_record:(cid_of r) value)

(* Labels *)

(* The web client's slug, so that both name a label alike. *)
let label_key name =
  let b = Buffer.create (String.length name) in
  let gap = ref false in
  String.iter
    (fun c ->
      match Char.lowercase_ascii c with
      | ('a' .. 'z' | '0' .. '9' | '.' | '_' | '~' | '-') as c ->
          if !gap then Buffer.add_char b '-';
          gap := false;
          Buffer.add_char b c
      | _ -> gap := true)
    (String.trim name);
  if !gap then Buffer.add_char b '-';
  let key = Buffer.contents b in
  let first = ref 0 and last = ref (String.length key) in
  while !first < !last && key.[!first] = '-' do incr first done;
  while !last > !first && key.[!last - 1] = '-' do decr last done;
  let key = String.sub key !first (min 64 (!last - !first)) in
  if key = "" then invalid_arg ("A label name needs a letter or digit: " ^ name)
  else key

let valid_color = function
  | None -> ()
  | Some c ->
      let hex = String.sub c 1 (max 0 (String.length c - 1)) in
      if
        not
          (List.mem (String.length c) [ 4; 5; 7; 9 ]
          && c.[0] = '#'
          && String.for_all
               (function
                 | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false)
               hex)
      then invalid_arg ("A colour is written in hex, as #e11d48, not " ^ c)

let create_label t repository ~name ?color () =
  valid_color color;
  let definition : Lex.Label.Definition.main =
    { name; color; created_at = now () }
  in
  let key = label_key name in
  ignore
    (repo_create_record t repository ~collection:label_collection ~rkey:key
       (encode Lex.Label.Definition.main_jsont definition));
  key

let edit_label t repository ~key ?name ?color () =
  valid_color color;
  match repo_record t repository ~collection:label_collection ~rkey:key with
  | None -> failwith ("No label " ^ key)
  | Some r ->
      let value = strip_stamps r.value in
      let value =
        match name with
        | None -> value
        | Some n -> set_field "name" (Some (Jsont.Json.string n)) value
      in
      let value =
        match color with
        | None -> value
        | Some c -> set_field "color" (Some (Jsont.Json.string c)) value
      in
      ignore
        (repo_put_record t repository ~collection:label_collection ~rkey:key
           ~swap_record:(cid_of r) value)

let delete_label t repository ~key =
  match repo_record t repository ~collection:label_collection ~rkey:key with
  | None -> failwith ("No label " ^ key)
  | Some r ->
      (* Taken off first, so that the tickets carrying it stay editable in
         other clients too. *)
      List.iter
        (fun (rkey, ticket) ->
          if List.mem key (ticket_labels ticket) then
            edit_ticket t repository ~rkey ~unlabel:[ key ] ())
        (tickets t repository);
      repo_delete_record t repository ~collection:label_collection ~rkey:key
        ~swap_record:(cid_of r)

let patch_collection = "org.tangled.review.patch"

let patches t repository ~ticket =
  repo_records t repository ~collection:patch_collection
  |> List.filter_map (fun (r : Atproto.Repo.ListRecords.record) ->
      match decode Lex.Review.Patch.main_jsont r.value with
      | exception Failure _ -> None
      | p when p.ticket = ticket -> Some p
      | _ -> None)
  |> List.stable_sort (fun (a : Lex.Review.Patch.main) b ->
      String.compare a.created_at b.created_at)

let oid_of json =
  match json with
  | Jsont.Object (fields, _) -> (
      match List.find_opt (fun ((n, _), _) -> n = "oid") fields with
      | Some (_, Jsont.String (oid, _)) -> oid
      | _ -> failwith "Expected a Git object ID")
  | _ -> failwith "Expected a Git object ID"

(* Releases *)

let git_oid oid =
  Jsont.Json.object'
    [
      Jsont.Json.mem ("$type", Jsont.Meta.none)
        (Jsont.Json.string "org.tangled.git.oid");
      Jsont.Json.mem ("oid", Jsont.Meta.none) (Jsont.Json.string oid);
    ]

let max_artifact_bytes = 52_428_800

let upload_artifact t repository ~oid ~name
    ?(content_type = "application/octet-stream") data =
  if String.length data > max_artifact_bytes then
    invalid_arg "An artifact can be at most 50 MiB";
  let nsid = "com.atproto.repo.uploadBlob" in
  let blob =
    Xrpc.Client.procedure_blob
      (repo_client t repository nsid)
      ~nsid ~params:[] ~blob:data ~content_type
      ~decoder:
        (Jsont.Object.map Fun.id
        |> Jsont.Object.mem "blob" Atp.Blob_ref.jsont ~enc:Fun.id
        |> Jsont.Object.skip_unknown |> Jsont.Object.finish)
  in
  let artifact : Lex.Repo.Artifact.main =
    { subject = git_oid oid; name; created_at = now (); artifact = blob }
  in
  repo_create_record t repository ~collection:artifact_collection
    (encode Lex.Repo.Artifact.main_jsont artifact)

let download_artifact t repository (artifact : Lex.Repo.Artifact.main) =
  fst
    (Xrpc.Client.query_bytes
       (public_client t ~service:repository.knot)
       ~nsid:"com.atproto.sync.getBlob"
       ~params:
         [
           ("did", repository.did);
           ("cid", Atp.Cid.to_string artifact.artifact.cid);
         ])

let delete_artifact t repository ~rkey =
  match repo_record t repository ~collection:artifact_collection ~rkey with
  | None -> failwith "Artifact not found"
  | Some r ->
      repo_delete_record t repository ~collection:artifact_collection ~rkey
        ~swap_record:(cid_of r)

(* Knot administration *)

let invite_code t ~service ?audience ?(uses = 1) ?for_account () =
  let nsid = "org.tangled.temp.server.createInviteCode" in
  let input : Lex.Temp.Server.CreateInviteCode.input =
    { use_count = uses; for_account }
  in
  (Xrpc.Client.procedure
     (service_client t ~service ?audience ~nsid ())
     ~nsid ~params:[] ~input:(Some Lex.Temp.Server.CreateInviteCode.input_jsont)
     ~input_data:(Some input)
     ~decoder:Lex.Temp.Server.CreateInviteCode.output_jsont)
    .code

let redeem_invite t ~service ?audience code =
  let nsid = "org.tangled.temp.server.redeemInviteCode" in
  ignore
    (Xrpc.Client.procedure
       (service_client t ~service ?audience ~nsid ())
       ~nsid ~params:[] ~input:(Some Jsont.json)
       ~input_data:
         (Some
            (Jsont.Json.object'
               [
                 Jsont.Json.mem ("code", Jsont.Meta.none)
                   (Jsont.Json.string code);
               ]))
       ~decoder:Jsont.json)

let block_collection = "org.tangled.knot.block"

let knot_bans t ~knot =
  match
    list_records t ~service:(service_url knot) ~did:(service_did knot)
      ~collection:block_collection ()
  with
  | records ->
      List.map
        (fun (r : Atproto.Repo.ListRecords.record) -> rkey_of_uri r.uri)
        records
  | exception ex when not_found [ "RepoNotFound" ] ex -> []

let ban_from_knot t ~knot ?audience subject =
  let record : Lex.Knot.Block.main = { created_at = now () } in
  ignore
    (create_in ~decoder:Jsont.json
       (knot_client t ~knot ?audience "com.atproto.repo.createRecord")
       ~repo:(service_did knot) ~collection:block_collection ~rkey:subject
       (encode Lex.Knot.Block.main_jsont record))

let unban_from_knot t ~knot ?audience subject =
  let did = service_did knot in
  match
    get_record t ~service:(service_url knot) ~did ~collection:block_collection
      ~rkey:subject ()
  with
  | None -> failwith "Not banned"
  | Some r ->
      delete_in
        (knot_client t ~knot ?audience "com.atproto.repo.deleteRecord")
        ~repo:did ~collection:block_collection ~rkey:subject
        ~swap_record:(Some (cid_of r))

(* Spindle administration *)

let spindle_query t ~spindle ?audience ~nsid params decoder =
  Xrpc.Client.query
    (service_client t ~service:spindle ?audience ~nsid ())
    ~nsid ~params ~decoder

let object_ pairs =
  Jsont.Json.object'
    (List.filter_map
       (fun (k, v) ->
         Option.map (fun v -> Jsont.Json.mem (k, Jsont.Meta.none) v) v)
       pairs)

let str s = Some (Jsont.Json.string s)

let secrets t ~spindle ?audience ~repo () =
  (spindle_query t ~spindle ?audience ~nsid:"org.tangled.secret.listSecrets"
     [ ("repo", repo) ]
     Lex.Secret.ListSecrets.output_jsont)
    .secrets
  |> List.map (decode Lex.Secret.ListSecrets.secret_jsont)

let add_secret t ~spindle ?audience ~repo ~key value =
  ignore
    (spindle_procedure t ~spindle ?audience ~nsid:"org.tangled.secret.addSecret"
       (object_ [ ("repo", str repo); ("key", str key); ("value", str value) ]))

let remove_secret t ~spindle ?audience ~repo key =
  ignore
    (spindle_procedure t ~spindle ?audience
       ~nsid:"org.tangled.secret.removeSecret"
       (object_ [ ("repo", str repo); ("key", str key) ]))

let webhooks t ~spindle ?audience ~repo () =
  (spindle_query t ~spindle ?audience ~nsid:"org.tangled.webhook.listWebhooks"
     [ ("repo", repo) ]
     Lex.Webhook.ListWebhooks.output_jsont)
    .webhooks
  |> List.map (decode Lex.Webhook.Defs.webhook_jsont)

let strings l = Some (Jsont.Json.list (List.map Jsont.Json.string l))

let create_webhook t ~spindle ?audience ~repo ~url ~events ?secret () =
  let output =
    spindle_procedure t ~spindle ?audience
      ~nsid:"org.tangled.webhook.createWebhook"
      (object_
         [
           ("repo", str repo);
           ("url", str url);
           ("events", strings events);
           ("secret", Option.map Jsont.Json.string secret);
           ("active", Some (Jsont.Json.bool true));
         ])
  in
  (decode Lex.Webhook.CreateWebhook.output_jsont output).id

let update_webhook t ~spindle ?audience ~repo ~webhook ?url ?events ?secret
    ?active () =
  ignore
    (spindle_procedure t ~spindle ?audience
       ~nsid:"org.tangled.webhook.updateWebhook"
       (object_
          [
            ("repo", str repo);
            ("webhook", str webhook);
            ("url", Option.map Jsont.Json.string url);
            ("events", Option.bind events strings);
            ("secret", Option.map Jsont.Json.string secret);
            ("active", Option.map Jsont.Json.bool active);
          ]))

let delete_webhook t ~spindle ?audience ~repo webhook =
  ignore
    (spindle_procedure t ~spindle ?audience
       ~nsid:"org.tangled.webhook.deleteWebhook"
       (object_ [ ("repo", str repo); ("webhook", str webhook) ]))

let webhook_deliveries t ~spindle ?audience ~repo ~webhook ?limit () =
  (spindle_query t ~spindle ?audience
     ~nsid:"org.tangled.webhook.listWebhookDeliveries"
     ([ ("repo", repo); ("webhooks", webhook) ]
     @ Option.to_list (int_param "limit" limit))
     Lex.Webhook.ListWebhookDeliveries.output_jsont)
    .deliveries
  |> List.map (decode Lex.Webhook.Defs.delivery_jsont)

let redeliver_webhook t ~spindle ?audience ~repo ~webhook delivery =
  ignore
    (spindle_procedure t ~spindle ?audience
       ~nsid:"org.tangled.webhook.retryWebhookDelivery"
       (object_
          [
            ("repo", str repo);
            ("webhook", str webhook);
            ("delivery", str delivery);
          ]))

let workflow_definition t ~spindle ~repo workflow =
  decode Lex.Ci.Defs.workflow_jsont
    (Xrpc.Client.query
       (public_client t ~service:spindle)
       ~nsid:"org.tangled.ci.getWorkflowDefinition"
       ~params:[ ("repo", repo); ("workflow", workflow) ]
       ~decoder:Lex.Ci.GetWorkflowDefinition.output_jsont)
      .workflow

let moderation = "org.tangled.temp.spindle.moderation."

let spindle_bans t ~spindle ?audience () =
  spindle_query t ~spindle ?audience ~nsid:(moderation ^ "listBans") []
    Jsont.json

let spindle_ban t ~spindle ?audience ~ban did =
  ignore
    (spindle_procedure t ~spindle ?audience
       ~nsid:(moderation ^ if ban then "ban" else "unban")
       (object_ [ ("did", str did) ]))

let quota = "org.tangled.temp.spindle.quota."

let quotas t ~spindle ?audience () =
  spindle_query t ~spindle ?audience ~nsid:(quota ^ "list") [] Jsont.json

let quota_usage t ~spindle ?audience ?did () =
  spindle_query t ~spindle ?audience ~nsid:(quota ^ "usage")
    (Option.to_list (param "did" did))
    Jsont.json

let set_quota t ~spindle ?audience ~did ~resource limit =
  ignore
    (spindle_procedure t ~spindle ?audience ~nsid:(quota ^ "set")
       (object_
          [
            ("did", str did);
            ("resource", str resource);
            ("limit", Option.map (fun n -> Jsont.Json.int n) limit);
            ( "unlimited",
              if limit = None then Some (Jsont.Json.bool true) else None );
          ]))

let unset_quota t ~spindle ?audience ~did ~resource () =
  ignore
    (spindle_procedure t ~spindle ?audience ~nsid:(quota ^ "unset")
       (object_ [ ("did", str did); ("resource", str resource) ]))
