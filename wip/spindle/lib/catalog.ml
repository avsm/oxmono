(* SPDX-License-Identifier: ISC *)
open Json

type repo = {
  did : string;
  owner : string;
  name : string;
  knot : string;
  source : string;
  created : string;
}

type admission = Open | Closed

type t = {
  store : Store.t;
  settings : Settings.t;
  mutable on_rename : repo -> old_name:string -> unit;
  network : Network.t;
  owner : string;
  admins : string list;
  hostname : string;
  admission : admission;
  static : repo option;
  ttl : float;
  now : unit -> float;
  cache : (string, float * repo option) Hashtbl.t;
}

let v ~store ~settings ~network ~owner ?(admins = []) ~hostname ~admission
    ~static ~ttl ~now () =
  List.iter (fun d -> ignore (did d)) admins;
  {
    store;
    settings;
    on_rename = (fun _ ~old_name:_ -> ());
    network;
    owner;
    admins;
    hostname;
    admission;
    static;
    ttl;
    now;
    cache = Hashtbl.create 32;
  }

let web_did host =
  "did:web:" ^ String.concat "%3A" (String.split_on_char ':' host)

let self t = web_did t.hostname
let admin t actor = actor = t.owner || List.mem actor t.admins
let allowed t = List.map fst (Store.list t.store "allow")

let allow t subject =
  ignore (did subject);
  Store.put t.store "allow" subject "";
  Hashtbl.reset t.cache

let disallow t subject =
  ignore (did subject);
  Store.delete t.store "allow" subject;
  Hashtbl.reset t.cache

(* A ban beats every other admission. *)
let admitted t owner =
  (not (Settings.banned t.settings owner))
  && (admin t owner || t.admission = Open
     || Store.get t.store "allow" owner <> None)

let on_rename t f = t.on_rename <- f

let knot_of t repo =
  let doc = decode (Network.resolve t.network repo) in
  match Network.service doc "tangled_knot" "TangledKnot" with
  | Some endpoint -> Network.knot t.network endpoint
  | None -> (
      match Network.service doc "atproto_pds" "AtprotoPersonalDataServer" with
      | Some endpoint -> Network.knot t.network endpoint
      | None -> invalid "repository identity names no knot")

let knot_origin = knot_of

let knot_did t repo =
  let origin = knot_of t repo in
  let start = String.index origin ':' + 3 in
  web_did (String.sub origin start (String.length origin - start))

let locate t repo =
  match t.static with
  | Some r when r.did = repo -> r.source
  | _ -> knot_of t repo ^ "/" ^ did repo

let slug name = String.lowercase_ascii name

(* Repository acceptance follows the reference spindle. The knot's manifest
   must name this spindle, the owner's declaration must name the repository
   back, and the owner must be admitted. Records are read from their hosts on
   each check, never trusted from an event. *)
let lookup t id =
  if Settings.banned t.settings id then None
  else
    let knot = knot_of t id in
    match
      Network.record t.network ~service:knot ~did:id
        ~collection:"org.tangled.repo.manifest" ~rkey:"self"
    with
    | None -> None
    | Some manifest ->
        let value = required "value" manifest in
        if Option.map string (field "spindle" value) <> Some (self t) then None
        else
          let declaration = required "declaration" value in
          let owner = did (get "owner" declaration) in
          let name = get "name" declaration in
          if not (admitted t owner) then None
          else
            let declared =
              Network.record t.network
                ~service:(Network.pds t.network owner)
                ~did:owner ~collection:"org.tangled.repo.declaration"
                ~rkey:(slug name)
            in
            let names_repo record =
              Option.map string (field "repo" (required "value" record))
              = Some id
            in
            if Option.fold ~none:false ~some:names_repo declared then (
              Settings.note_owner t.settings ~repo:id ~owner;
              Some
                {
                  did = id;
                  owner;
                  name;
                  knot;
                  source = knot ^ "/" ^ id;
                  created =
                    Option.fold ~none:"" ~some:string (field "createdAt" value);
                })
            else None

let managed t id =
  ignore (did id);
  match t.static with
  | Some repo when repo.did = id -> Some repo
  | _ -> (
      let now = t.now () in
      match Hashtbl.find_opt t.cache id with
      | Some (expires, repo) when expires > now -> repo
      | _ ->
          let repo = lookup t id in
          Hashtbl.replace t.cache id (now +. t.ttl, repo);
          Option.iter
            (fun (r : repo) ->
              match Store.get t.store "repo-name" id with
              | Some old when old = r.name -> ()
              | old ->
                  Store.put t.store "repo-name" id r.name;
                  Option.iter
                    (fun old_name ->
                      if old_name <> "" then t.on_rename r ~old_name)
                    old)
            repo;
          repo)

let forget t id = Hashtbl.remove t.cache id

let current t (repo : repo) =
  match managed t repo.did with
  | Some r -> r.owner = repo.owner && r.knot = repo.knot
  | None -> false

(* Ask the knot on each mutation. A revoked collaborator must not keep write
   authority through a cache. *)
let authorized t (repo : repo) actor =
  if repo.owner = actor then true
  else if Option.is_some t.static && repo.knot = "" then false
  else
    Network.record t.network ~service:repo.knot ~did:repo.did
      ~collection:"org.tangled.graph.collaborator" ~rkey:actor
    <> None

let permitted t (repo : repo) actor =
  actor = repo.did || authorized t repo actor

let default_branch t (repo : repo) =
  if repo.knot = "" then None
  else
    match
      Network.record t.network ~service:repo.knot ~did:repo.did
        ~collection:"org.tangled.git.refs" ~rkey:"self"
    with
    | None -> None
    | Some record ->
        Option.map string (field "defaultBranch" (required "value" record))
