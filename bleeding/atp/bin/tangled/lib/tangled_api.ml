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

let create_repo t ~name ~knot ?audience ?description ?default_branch ?spindle
    ?source () =
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
      source = Option.map (fun r -> source_tangled r.did) source;
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

let repo_create_record =
  repo_create ~decoder:Atproto.Repo.CreateRecord.output_jsont

let repo_put_record t repository ~collection ~rkey ~swap_record record =
  put_in
    (repo_client t repository "com.atproto.repo.putRecord")
    ~repo:repository.did ~collection ~rkey ~swap_record:(Some swap_record)
    record

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
  list_records t ~service:(service_url knot) ~did:(service_did knot)
    ~collection:member_collection ()
  |> List.map (fun (r : Atproto.Repo.ListRecords.record) -> rkey_of_uri r.uri)

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

let open_ticket t repository ~title ?body ?target_branch () =
  let ticket : Lex.Track.Ticket.main =
    {
      title = markdown ~inline:true title;
      body = Option.map markdown body;
      state = Jsont.Json.string "open";
      created_at = now ();
      target_branch;
      assignees = None;
      labels = None;
      tangled_labels = None;
    }
  in
  repo_create_record t repository ~collection:ticket_collection
    (encode Lex.Track.Ticket.main_jsont ticket)

let set_ticket_state t repository ~rkey state =
  if not (List.mem state [ "open"; "closed"; "completed" ]) then
    invalid_arg "Ticket state must be open, closed or completed";
  match repo_record t repository ~collection:ticket_collection ~rkey with
  | None -> failwith "Ticket not found"
  | Some r ->
      let value =
        strip_stamps r.value
        |> set_field "state" (Some (Jsont.Json.string state))
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
