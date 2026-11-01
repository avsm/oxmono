(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Common
module D = Httpz_dav

type t = { dav : Fetch_dav.t; readonly : bool; identity : string }
type file = { raw : string; etag : string option }

let make ~fetch ~(config : Config.webdav) ~readonly =
  let methods =
    if readonly then
      [ `GET; `HEAD; `OPTIONS; `Other "PROPFIND"; `Other "REPORT" ]
    else
      [
        `GET;
        `HEAD;
        `OPTIONS;
        `Other "PROPFIND";
        `Other "REPORT";
        `PUT;
        `Other "MKCOL";
      ]
  in
  let fetch =
    fetch
    |> Fetch.restrict ~under:[ config.collection ] ~methods
    |> Fetch.with_credentials ~scope:[ config.collection ]
         [
           Fetch.Credential.basic ~user:config.username
             ~password:(Config.password config);
         ]
  in
  let dav =
    Fetch_dav.v
      ~limits:
        { D.max_bytes = 16 * 1024 * 1024; max_depth = 64; max_nodes = 100_000 }
      ~root:config.collection fetch
  in
  {
    dav;
    readonly;
    identity = json_string (arr [ str config.collection; str config.username ]);
  }

let get t path =
  try
    let raw, etag = Fetch_dav.get t.dav path in
    Some { raw; etag }
  with Fetch_dav.Http_error { status = 404; _ } -> None

let etag s =
  match
    Fetch.Header.get Fetch.Header.etag (Http.Header.of_list [ ("ETag", s) ])
  with
  | Some e when not e.weak -> e
  | _ -> fail "remote replacement requires a strong ETag"

let put t ~path ~previous raw =
  if t.readonly then fail "dry-run transport forbids PUT";
  let condition =
    match previous with
    | None -> Fetch_dav.If_absent
    | Some { etag = Some e; _ } -> Fetch_dav.If_match (etag e)
    | Some _ -> fail "remote replacement requires an ETag"
  in
  ignore
    (Fetch_dav.put ~condition
       ~content_type:
         (if String.ends_with ~suffix:".md" path then
            "text/markdown; charset=utf-8"
          else "application/json")
       t.dav path (Fetch.String raw));
  match get t path with
  | Some f when f.raw = raw -> f
  | _ -> fail "upload readback differs or is missing; candidate retained"

let mkdir t path =
  if t.readonly then fail "dry-run transport forbids MKCOL";
  Fetch_dav.mkcol t.dav path

let listing t path =
  let m =
    Fetch_dav.propfind ~depth:`One t.dav path
      (D.Prop [ D.dav "resourcetype"; D.dav "getetag" ])
  in
  let parent = Fetch_dav.resolve t.dav path in
  let seen = Hashtbl.create 32 and self = ref false in
  let entries =
    List.filter_map
      (fun (r : D.response) ->
        if r.errors <> [] then fail "DAV listing contains an error";
        (match r.outcome with
        | D.Status _ -> fail "DAV listing omitted resource properties"
        | D.Properties ps ->
            List.iter
              (fun (p : D.propstat) ->
                if
                  p.errors <> []
                  || p.status <> 200
                     && not
                          (p.status = 404
                          && List.for_all
                               (fun (e : D.element) -> e.name = D.dav "getetag")
                               p.properties)
                then fail "incomplete DAV property listing")
              ps);
        let href =
          match r.hrefs with
          | [ h ] -> Fetch_dav.resolve t.dav h
          | _ -> fail "ambiguous DAV href"
        in
        if Hashtbl.mem seen href then fail "duplicate DAV href";
        Hashtbl.add seen href ();
        let collection =
          match D.property (D.dav "resourcetype") r with
          | Some (Ok p) -> D.find (D.dav "collection") p <> None
          | _ -> fail "missing DAV resource type"
        in
        ignore (D.property (D.dav "getetag") r);
        if href = parent then (
          if not collection then
            fail "configured destination is not a collection";
          self := true;
          None)
        else (
          if not (String.starts_with ~prefix:parent href) then
            fail "DAV href is outside the requested collection";
          let name =
            String.sub href (String.length parent)
              (String.length href - String.length parent)
          in
          let leaf =
            if collection && String.ends_with ~suffix:"/" name then
              String.sub name 0 (String.length name - 1)
            else name
          in
          if String.contains leaf '/' then fail "unexpected nested DAV response";
          Some (name, collection)))
      m.responses
  in
  if not !self then fail "incomplete DAV listing: collection response missing";
  entries

let exists_collection t path =
  try
    ignore (listing t path);
    true
  with Fetch_dav.Http_error { status = 404; _ } -> false

let notes t =
  let entries = listing t "notes/" in
  List.map
    (fun (name, collection) ->
      if collection || not (String.ends_with ~suffix:".md" name) then
        fail "unrecognized remote resource: %s" name;
      let id = String.sub name 0 (String.length name - 3) in
      check_uuid id;
      match get t ("notes/" ^ name) with
      | None -> fail "remote resource disappeared during listing"
      | Some f -> (id, f))
    entries
