(* SPDX-License-Identifier: ISC *)
open Json

type repo = {
  did : string;
  owner : string;
  rkey : string;
  knot : string;
  source : string;
}

type t = {
  store : Store.t;
  network : Network.t;
  owner : string;
  hostname : string;
  static : repo option;
  mutable subscription_cache : (float * string list) option;
}

let v ~store ~network ~owner ~hostname ~static =
  { store; network; owner; hostname; static; subscription_cache = None }

let members t =
  Store.list t.store "sh.tangled.spindle.member"
  |> List.filter_map (fun (key, raw) ->
      let value = decode raw in
      if
        String.starts_with ~prefix:(t.owner ^ "/") key
        && get "instance" value = t.hostname
      then Some (did (get "subject" value))
      else None)
  |> fun xs -> List.sort_uniq String.compare (t.owner :: xs)

let allowed t owner = List.mem owner (members t)

let split_key key =
  match String.split_on_char '/' key with
  | [ owner; rkey ] -> (owner, rkey)
  | _ -> invalid "invalid record key"

let assignments t =
  Store.list t.store "sh.tangled.repo"
  |> List.filter_map (fun (key, raw) ->
      let owner, rkey = split_key key in
      let value = decode raw in
      if
        allowed t owner
        && Option.map string (field "spindle" value) = Some t.hostname
      then
        match field "repoDid" value with
        | Some id -> Some (owner, rkey, did (string id), get "knot" value)
        | None -> None
      else None)

let verified t id =
  ignore (did id);
  match t.static with
  | Some repo when repo.did = id -> repo
  | _ ->
      let doc = decode (Network.resolve t.network id) in
      let endpoint =
        match Network.service doc "tangled_knot" "TangledKnot" with
        | Some endpoint -> endpoint
        | None -> (
            match
              Network.service doc "atproto_pds" "AtprotoPersonalDataServer"
            with
            | Some endpoint -> endpoint
            | None -> invalid "no knot service")
      in
      let knot = Network.knot t.network endpoint in
      let result =
        Network.json t.network
          (Network.query
             (knot ^ "/xrpc/sh.tangled.repo.describeRepo")
             [ ("repoDid", id) ])
      in
      if get "repoDid" result <> id then invalid "knot repository mismatch";
      let owner = did (get "ownerDid" result) in
      let rkey = get "rkey" result in
      if rkey = "" || String.contains rkey '/' then invalid "invalid repo rkey";
      { did = id; owner; rkey; knot; source = knot ^ "/" ^ id }

let managed t id =
  match t.static with
  | Some repo when repo.did = id -> Some repo
  | _ ->
      let candidates =
        assignments t |> List.filter (fun (_, _, repo, _) -> repo = id)
      in
      if candidates = [] then None
      else
        let repo = verified t id in
        if
          List.exists
            (fun (owner, rkey, _, knot) ->
              owner = repo.owner && rkey = repo.rkey
              && Network.knot t.network knot = repo.knot)
            candidates
        then Some repo
        else None

let knots t =
  let now = Unix.gettimeofday () in
  match t.subscription_cache with
  | Some (checked, knots) when now -. checked < 30. -> knots
  | _ ->
      let knots =
        assignments t
        |> List.filter_map (fun (_, _, id, _) ->
            try
              match managed t id with
              | None -> None
              | Some repo -> Some repo.knot
            with
            | Eio.Cancel.Cancelled _ as exn -> raise exn
            | exn ->
                Printf.eprintf "spindle repository %s: %s\n%!" id
                  (Printexc.to_string exn);
                None)
        |> List.sort_uniq String.compare
      in
      t.subscription_cache <- Some (now, knots);
      knots

let authorized t (repo : repo) actor =
  if repo.owner = actor then true
  else if
    (* Ask the canonical knot on each mutation. A deleted or replaced PDS
     collaborator record must never leave stale write authority behind. *)
    Option.is_some t.static && repo.knot = ""
  then false
  else
    let rec pages cursor count =
      if count = 10 then false
      else
        let result =
          Network.json t.network
            (Network.query
               (repo.knot ^ "/xrpc/sh.tangled.repo.listCollaborators")
               ([ ("subject", repo.did); ("limit", "100") ] @ cursor))
        in
        if
          List.exists
            (fun item -> get "subject" item = actor)
            (list (required "items" result))
        then true
        else
          match field "cursor" result with
          | Some value when string value <> "" ->
              let next = [ ("cursor", string value) ] in
              if next = cursor then false else pages next (count + 1)
          | _ -> false
    in
    pages [] 0

let validate t collection value =
  match collection with
  | "sh.tangled.spindle.member" ->
      ignore (did (get "subject" value));
      ignore (get "instance" value)
  | "sh.tangled.repo" ->
      ignore (Network.knot t.network (get "knot" value));
      Option.iter (fun v -> ignore (did (string v))) (field "repoDid" value);
      Option.iter (fun v -> ignore (string v)) (field "spindle" value)
  | _ -> invalid "unsupported catalog collection"

let replace t owner collection =
  t.subscription_cache <- None;
  let records = Network.records t.network owner collection in
  let prefix = owner ^ "/" in
  let deletes =
    Store.list t.store collection
    |> List.filter_map (fun (k, _) ->
        if String.starts_with ~prefix k then Some (collection, k) else None)
  in
  let puts =
    List.filter_map
      (fun record ->
        let uri = get "uri" record in
        let expected = "at://" ^ owner ^ "/" ^ collection ^ "/" in
        if not (String.starts_with ~prefix:expected uri) then
          invalid "PDS returned a record outside the requested collection";
        let rkey =
          String.sub uri (String.length expected)
            (String.length uri - String.length expected)
        in
        if rkey = "" || String.contains rkey '/' then
          invalid "invalid record URI";
        let value = required "value" record in
        try
          validate t collection value;
          Some (collection, prefix ^ rkey, encode value)
        with Invalid message ->
          Printf.eprintf "spindle record %s: %s\n%!" uri message;
          None)
      records
  in
  Store.batch t.store ~puts ~deletes

let bootstrap t =
  replace t t.owner "sh.tangled.spindle.member";
  List.iter (fun owner -> Store.put t.store "reconcile" owner "") (members t)

let apply t ~owner ~collection ~rkey record =
  t.subscription_cache <- None;
  if
    (not
       (List.mem collection [ "sh.tangled.spindle.member"; "sh.tangled.repo" ]))
    || (collection = "sh.tangled.spindle.member" && owner <> t.owner)
    || (collection <> "sh.tangled.spindle.member" && not (allowed t owner))
  then ()
  else (
    ignore (did owner);
    if rkey = "" || String.contains rkey '/' then invalid "invalid record key";
    let key = owner ^ "/" ^ rkey in
    match record with
    | None -> Store.delete t.store collection key
    | Some value ->
        validate t collection value;
        Store.put t.store collection key (encode value))
