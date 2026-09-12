(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Common
module Url = Fetch.Middleware.Url
module D = Httpz_dav

let default_server = "https://carddav.fastmail.com/"
let limit = 64 * 1024 * 1024
let limits = { D.max_bytes = limit; max_depth = 64; max_nodes = 2_000_000 }

type t = {
  root : string;
  origin : string;
  fetch : Fetch.plain;
  readonly : bool;
}

type card = {
  uid : string;
  href : string;
  etag : string option;
  data : string;
  props : Mapping.property list;
}

let is_readonly t = t.readonly

let url s =
  if String.contains s '#' then fail "CardDAV URLs must not contain fragments";
  let u = get_ok (Url.of_string s) in
  if Url.scheme u <> `Https then fail "CardDAV URLs must use HTTPS";
  u

let make ~fetch ~root ~username ~password ~readonly =
  let u = url root in
  if String.contains root '?' then fail "server URL must not contain a query";
  let origin = Url.origin u in
  let methods =
    if readonly then
      Some [ `GET; `HEAD; `OPTIONS; `Other "PROPFIND"; `Other "REPORT" ]
    else None
  in
  let fetch =
    Fetch.restrict ~under:[ origin ] ?methods fetch
    |> Fetch.with_credentials ~scope:[ origin ]
         [ Fetch.Credential.basic ~user:username ~password ]
  in
  { root; origin; fetch; readonly }

let password path =
  let p = String.trim (read path) in
  if p = "" || String.contains p '\n' || String.contains p '\r' then
    fail "password file must contain one nonempty password";
  p

let validate t s =
  if Url.origin (url s) <> t.origin then
    fail "refusing credentials outside the configured CardDAV origin"

let resolve base reference =
  Url.to_string (get_ok (Url.resolve ~base:(url base) reference))

let request t ?(body = "") ?(headers = []) meth target =
  let method_name = String.uppercase_ascii meth in
  if
    t.readonly
    && not
         (List.mem method_name
            [ "GET"; "HEAD"; "OPTIONS"; "PROPFIND"; "REPORT" ])
  then fail "dry-run transport forbids %s" method_name;
  let meth = Http.Method.of_string method_name in
  let rec loop hops target =
    if hops = 0 then fail "too many CardDAV redirects";
    validate t target;
    let headers =
      List.fold_right
        (fun (k, value) acc ->
          let key, value = Fetch.Header.raw k value in
          Fetch.Header.((key, value) :: acc))
        headers []
    in
    let status, headers, data =
      Fetch.with_response ~headers ~body:(Fetch.String body) ~redirects:0
        t.fetch meth target (fun r ->
          ( Fetch.status r,
            Fetch.headers r,
            Eio.Buf_read.(parse_exn take_all (Fetch.body r) ~max_size:limit) ))
    in
    if List.mem status [ 301; 302; 307; 308 ] then
      match Http.Header.get headers "location" with
      | None -> fail "redirect without Location"
      | Some location -> loop (hops - 1) (resolve target location)
    else (status, headers, data)
  in
  loop 5 target

let responses data base =
  let root = get_ok (D.parse_xml ~limits data) in
  if root.name <> D.dav "multistatus" || D.find (D.dav "error") root <> None
  then fail "invalid or incomplete DAV multistatus";
  let m = get_ok (D.multistatus root) in
  List.map
    (fun (r : D.response) ->
      if r.errors <> [] then fail "DAV listing contains an error";
      let props =
        match r.outcome with
        | D.Status s when s <> 200 ->
            fail "DAV listing contains a failed resource response"
        | D.Status _ -> []
        | D.Properties xs ->
            List.concat_map
              (fun (ps : D.propstat) ->
                if ps.status = 200 then ps.properties else [])
              xs
      in
      let href =
        match r.hrefs with
        | [ s ] -> resolve base s
        | _ -> fail "DAV response needs exactly one href"
      in
      let names = List.map (fun (p : D.element) -> p.name) props in
      if List.length names <> List.length (List.sort_uniq compare names) then
        fail "ambiguous DAV properties";
      (href, props))
    m.responses

let xml t meth target body depth =
  let status, _, data =
    request t ~body
      ~headers:
        [
          ("Content-Type", "application/xml; charset=utf-8");
          ("Depth", string_of_int depth);
        ]
      meth target
  in
  if status <> 207 then fail "%s failed with HTTP %d" meth status;
  let rows = responses data target in
  List.iter (fun (href, _) -> validate t href) rows;
  rows

let prop name props = List.find_opt (fun (p : D.element) -> p.name = name) props

let prop_text name props =
  Option.map (fun e -> get_ok (D.text e)) (prop name props)

let find_href rows name =
  let hrefs =
    List.concat_map
      (fun (base, props) ->
        match prop name props with
        | None -> []
        | Some p ->
            D.children (D.dav "href") p
            |> List.map (fun e -> resolve base (get_ok (D.text e))))
      rows
  in
  match hrefs with
  | [ s ] -> s
  | _ -> fail "expected one %s during discovery" (snd name)

let trim_slash s =
  let rec loop i =
    if i > 0 && s.[i - 1] = '/' then loop (i - 1) else String.sub s 0 i
  in
  loop (String.length s)

let discover ?collection t =
  let query names = D.propfind (D.Prop names) in
  let entry =
    if Url.path_and_query (url t.root) = "/" then
      resolve t.root "/.well-known/carddav"
    else t.root
  in
  let principal =
    find_href
      (xml t "PROPFIND" entry (query [ D.dav "current-user-principal" ]) 0)
      (D.dav "current-user-principal")
  in
  let home =
    find_href
      (xml t "PROPFIND" principal
         (query [ D.carddav "addressbook-home-set" ])
         0)
      (D.carddav "addressbook-home-set")
  in
  let rows =
    xml t "PROPFIND" home
      (query
         [
           D.dav "resourcetype";
           D.dav "displayname";
           D.carddav "max-resource-size";
           D.carddav "supported-address-data";
         ])
      1
  in
  let books =
    List.filter_map
      (fun (href, props) ->
        match prop (D.dav "resourcetype") props with
        | Some rt when D.find (D.carddav "addressbook") rt <> None ->
            let size =
              match prop_text (D.carddav "max-resource-size") props with
              | Some s when s <> "" -> int (int_of_string s)
              | _ -> `Null
            in
            let supported =
              match prop (D.carddav "supported-address-data") props with
              | None -> `Null
              | Some p ->
                  arr
                    (List.map
                       (fun (e : D.element) ->
                         obj
                           (List.map
                              (fun ((_, k), v) -> (k, str v))
                              (List.filter
                                 (fun ((ns, _), _) -> ns = "")
                                 e.attrs)))
                       (D.elements p))
            in
            Some
              (obj
                 [
                   ("href", str href);
                   ( "name",
                     str
                       (Option.value ~default:""
                          (prop_text (D.dav "displayname") props)) );
                   ("max_resource_size", size);
                   ("supported_address_data", supported);
                 ])
        | _ -> None)
      rows
  in
  let selected =
    match collection with
    | Some s ->
        validate t s;
        List.filter (fun b -> trim_slash (field "href" b) = trim_slash s) books
    | None ->
        let defaults =
          List.filter
            (fun b ->
              String.ends_with ~suffix:"/Default" (trim_slash (field "href" b)))
            books
        in
        if List.length defaults = 1 then defaults else books
  in
  match selected with
  | [ b ] -> b
  | _ ->
      fail
        "address-book selection is ambiguous; use --collection with its full \
         URL"

let fetch_all t book =
  let body =
    "<?xml version=\"1.0\"?><c:addressbook-query xmlns:d=\"DAV:\" \
     xmlns:c=\"urn:ietf:params:xml:ns:carddav\"><d:prop><d:getetag/><c:address-data/></d:prop></c:addressbook-query>"
  in
  let seen = Hashtbl.create 512 in
  xml t "REPORT" (field "href" book) body 1
  |> List.map (fun (href, props) ->
      let data =
        match prop_text (D.carddav "address-data") props with
        | Some s when s <> "" -> s
        | _ ->
            fail "incomplete address-book listing; refusing to plan creations"
      in
      let props' = Mapping.parse data in
      let uid = Mapping.untext (Mapping.only props' "UID").value in
      if Hashtbl.mem seen uid then
        fail "duplicate destination UID; refusing to plan creations";
      Hashtbl.add seen uid ();
      {
        uid;
        href;
        data;
        props = props';
        etag = prop_text (D.dav "getetag") props;
      })

let normalize_name s =
  let b = Buffer.create (String.length s) and spaced = ref true in
  let add u =
    if Uucp.White.is_white_space u then (
      if not !spaced then Buffer.add_char b ' ';
      spaced := true)
    else (
      Buffer.add_utf_8_uchar b u;
      spaced := false)
  in
  let s = Uunf_string.normalize_utf_8 `NFKC s in
  let rec loop i =
    if i < String.length s then (
      let d = String.get_utf_8_uchar s i in
      if not (Uchar.utf_decode_is_valid d) then
        fail "invalid UTF-8 in contact name";
      let u = Uchar.utf_decode_uchar d in
      (match Uucp.Case.Fold.fold u with
      | `Self -> add u
      | `Uchars us -> List.iter add us);
      loop (i + Uchar.utf_decode_length d))
  in
  loop 0;
  String.trim (Buffer.contents b)

type identifiers = {
  names : string list;
  emails : string list;
  urls : string list;
}

let identifiers props =
  let names = ref [] and emails = ref [] and urls = ref [] in
  List.iter
    (fun (p : Mapping.property) ->
      let value = Mapping.untext p.value in
      match p.name with
      | "FN" | "X-SORTAL-ALT-NAME" -> names := normalize_name value :: !names
      | "NICKNAME" ->
          names :=
            List.map normalize_name (Mapping.split_value ',' p.value) @ !names
      | "N" -> (
          let parts = Mapping.components p.value in
          names := normalize_name (String.concat " " parts) :: !names;
          match parts with
          | a :: b :: rest ->
              names :=
                normalize_name (String.concat " " (b :: a :: rest)) :: !names
          | _ -> ())
      | "EMAIL" ->
          let value = String.trim value in
          let value =
            match String.rindex_opt value '@' with
            | None -> String.lowercase_ascii value
            | Some i ->
                String.sub value 0 (i + 1)
                ^ String.lowercase_ascii
                    (String.sub value (i + 1) (String.length value - i - 1))
          in
          emails := value :: !emails
      | "URL" | "SOCIALPROFILE" | "X-SOCIALPROFILE" | "IMPP" | "X-ATPROTO" ->
          urls := String.trim value :: !urls
      | _ -> ())
    props;
  let clean xs = List.sort_uniq String.compare (List.filter (( <> ) "") xs) in
  { names = clean !names; emails = clean !emails; urls = clean !urls }

let intersects a b = List.exists (fun s -> List.mem s b) a

let assert_contact data bundle entry =
  let manifest = load_json (Filename.concat bundle "manifest.json") in
  let props = Mapping.parse data in
  if Mapping.untext (Mapping.only props "UID").value <> field "uid" entry then
    fail "server changed UID";
  if
    Mapping.untext (Mapping.only props "X-SORTAL-STORE").value
    <> field "store_id" manifest
  then fail "server changed Sortal store identity";
  let originals = Filename.concat bundle "originals" in
  let c = Bundle.contact (read (safe_path originals (field "source" entry))) in
  let decoded, photos = Mapping.decode data in
  if not (equal c decoded) then
    fail "server fields do not reconstruct the source contact";
  List.iter
    (fun (name, raw) ->
      if read (safe_path originals name) <> raw then
        fail "server changed photo bytes")
    photos;
  let signatures props =
    Mapping.signatures
      (List.filter
         (fun (p : Mapping.property) -> p.name <> "X-SORTAL-MAPPING")
         props)
  in
  let expected =
    signatures (Mapping.parse (read (safe_path bundle (field "card" entry))))
  in
  let rec subset expected actual =
    match (expected, actual) with
    | [], _ -> true
    | _, [] -> false
    | x :: xs, y :: ys ->
        if x = y then subset xs ys
        else if compare x y > 0 then subset expected ys
        else false
  in
  if not (subset expected (signatures props)) then
    fail
      "server removed or transformed an emitted property; review the readback"

let plan bundle manifest book remote =
  let source =
    List.map
      (fun entry ->
        let data = read (safe_path bundle (field "card" entry)) in
        (entry, data, identifiers (Mapping.parse data)))
      (items "contacts" manifest)
  in
  let index = List.map (fun r -> (r, identifiers r.props)) remote in
  List.map
    (fun (entry, data, ids) ->
      let uid = field "uid" entry and handle = field "handle" entry in
      let values r key =
        List.filter_map
          (fun (p : Mapping.property) ->
            if p.name = key then Some (Mapping.untext p.value) else None)
          r.props
      in
      let strong =
        List.filter
          (fun r ->
            r.uid = uid
            || values r "X-SORTAL-ID" = [ handle ]
               && values r "X-SORTAL-STORE" = [ field "store_id" manifest ])
          remote
      in
      let row =
        obj
          [
            ("handle", str handle);
            ("uid", str uid);
            ("card", get "card" entry);
            ("source", get "source" entry);
            ("action", str "create");
            ( "href",
              str
                (resolve
                   (trim_slash (field "href" book) ^ "/")
                   (percent uid ^ ".vcf")) );
          ]
      in
      if strong <> [] then
        let row =
          row
          |> set "action" (str "review")
          |> set "reason" (str "existing identity needs reconciliation")
          |> set "candidates" (arr (List.map (fun r -> str r.href) strong))
        in
        match strong with
        | [ r ] -> (
            try
              assert_contact r.data bundle entry;
              row
              |> set "action" (str "unchanged")
              |> set "href" (str r.href)
              |> set "etag"
                   (match r.etag with Some s -> str s | None -> `Null)
              |> remove "reason"
            with Error _ -> row)
        | _ -> row
      else
        let candidates =
          List.filter_map
            (fun (r, other) ->
              if
                intersects ids.names other.names
                || intersects ids.emails other.emails
                || intersects ids.urls other.urls
              then Some (str r.href)
              else None)
            index
        in
        let local =
          List.filter_map
            (fun (e, _, other) ->
              if field "uid" e <> uid && intersects ids.names other.names then
                Some (get "handle" e)
              else None)
            source
        in
        if candidates <> [] || local <> [] then
          row
          |> set "action" (str "review")
          |> set "reason" (str "possible duplicate name/email/account")
          |> set "candidates" (arr candidates)
          |> set "source_candidates" (arr local)
        else
          match get "max_resource_size" book with
          | `Float size
            when size > 0. && float_of_int (String.length data) > size ->
              row
              |> set "action" (str "review")
              |> set "reason"
                   (str "card exceeds the destination resource size limit")
          | _ -> row)
    source

let strong_etag headers =
  match Http.Header.get headers "etag" with
  | Some s
    when String.length s >= 2 && s.[0] = '"' && s.[String.length s - 1] = '"' ->
      s
  | _ -> fail "server did not return a strong ETag"

let seed_one t bundle row directory =
  let data = read (safe_path bundle (field "card" row)) in
  let status, _, _ =
    request t ~body:data
      ~headers:
        [
          ("Content-Type", "text/vcard; charset=utf-8"); ("If-None-Match", "*");
        ]
      "PUT" (field "href" row)
  in
  if not (List.mem status [ 201; 204 ]) then fail "PUT returned HTTP %d" status;
  let status, headers, stored = request t "GET" (field "href" row) in
  if status <> 200 then fail "readback returned HTTP %d" status;
  write (safe_path directory (field "uid" row ^ ".vcf")) stored;
  assert_contact stored bundle row;
  obj
    [
      ("uid", get "uid" row);
      ("handle", get "handle" row);
      ("href", get "href" row);
      ("etag", str (strong_etag headers));
      ("status", str "verified");
      ("sha256", str (digest stored));
    ]

let inspect ?collection ~apply ~dav ~bundle ~username ~report () =
  fresh report;
  let manifest = Bundle.verify bundle in
  separate report
    [
      field "source" manifest;
      Filename.concat bundle "originals";
      Filename.concat bundle "cards";
    ];
  let book = discover ?collection dav in
  (match get "supported_address_data" book with
  | `Null -> ()
  | v ->
      let version =
        match find "vcard_version" manifest with
        | Some v -> v
        | None -> str "3.0"
      in
      if
        not
          (List.exists
             (fun p ->
               find "content-type" p = Some (str "text/vcard")
               && find "version" p = Some version)
             (list v))
      then fail "destination does not advertise the exported vCard version");
  let remote = fetch_all dav book in
  let rows = plan bundle manifest book remote in
  Unix.mkdir report 0o700;
  let before = Filename.concat report "before"
  and after = Filename.concat report "after" in
  Unix.mkdir before 0o700;
  Unix.mkdir after 0o700;
  List.iter
    (fun r -> write (Filename.concat before (digest r.uid ^ ".vcf")) r.data)
    remote;
  let result =
    ref
      (obj
         [
           ("account", str username);
           ("book", book);
           ("existing_contacts", int (List.length remote));
           ("plan", arr rows);
           ("results", arr []);
           ("applied", `Bool apply);
           ("dry_run", `Bool (not apply));
         ])
  in
  let save () = save_json (Filename.concat report "report.json") !result in
  save ();
  (if apply then
     let creates =
       List.filter (fun r -> field "action" r = "create") rows
       |> List.sort (fun a b ->
           compare
             (field "handle" a <> "avsm", field "handle" a)
             (field "handle" b <> "avsm", field "handle" b))
     in
     (* Serialize writes: after any uncertain outcome, no further creation starts. *)
     List.iter
       (fun row ->
         let recorded, failed =
           try (seed_one dav bundle row after, false)
           with exn ->
             ( obj
                 [
                   ("uid", get "uid" row);
                   ("handle", get "handle" row);
                   ("status", str "failed");
                   ("error", str (Printexc.to_string exn));
                 ],
               true )
         in
         result :=
           set "results" (arr (items "results" !result @ [ recorded ])) !result;
         save ();
         if failed then
           fail
             "contact did not verify; further uploads stopped; see \
              %s/report.json"
             report)
       creates);
  !result
