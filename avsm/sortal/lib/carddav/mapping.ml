(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Common

type property = {
  group : string;
  name : string;
  params : (string * string) list;
  value : string;
}

let text value =
  let b = Buffer.create (String.length value) in
  String.iteri
    (fun i c ->
      if c = '\r' then (
        if i + 1 = String.length value || value.[i + 1] <> '\n' then
          Buffer.add_char b '\n')
      else Buffer.add_char b c)
    value;
  Vcard.Text.escape_component (Buffer.contents b)

let untext = Vcard.Text.unescape

let property line =
  let p = get_ok (Vcard.Property.of_string line) in
  let params =
    List.map
      (fun p -> (Vcard.Param.name p, String.concat "," (Vcard.Param.values p)))
      (Vcard.Property.params p)
  in
  if
    List.length (List.sort_uniq String.compare (List.map fst params))
    <> List.length params
  then fail "duplicate vCard parameter";
  {
    group =
      String.lowercase_ascii (Option.value ~default:"" (Vcard.Property.group p));
    name = Vcard.Property.name p;
    params;
    value = Vcard.Property.value p;
  }

let parse data =
  ignore (get_ok (Vcard.one_of_string data));
  Vcard.unfold data |> List.filter (fun s -> s <> "") |> List.map property

let only props name =
  match List.filter (fun p -> p.name = name) props with
  | [ p ] -> p
  | xs -> fail "expected exactly one %s, found %d" name (List.length xs)

let param p k = List.assoc_opt k p.params

let required p k =
  match param p k with Some s -> s | None -> fail "missing parameter %s" k

let bad_param s = String.exists (fun c -> String.contains "\r\n\"^" c) s

let token c =
  match c with 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> true | _ -> false

let header p =
  let parameters =
    List.map
      (fun (k, v) ->
        if bad_param v then fail "parameter %s requires unsupported quoting" k;
        let token_list =
          k = "TYPE"
          && List.for_all
               (fun s -> s <> "" && String.for_all token s)
               (String.split_on_char ',' v)
        in
        let quoted =
          (not token_list)
          && String.exists (fun c -> String.contains ":;, " c) v
        in
        ";" ^ k ^ "=" ^ if quoted then "\"" ^ v ^ "\"" else v)
      p.params
  in
  (if p.group = "" then "" else p.group ^ ".")
  ^ p.name
  ^ String.concat "" parameters

let render props =
  String.concat ""
    (List.map (fun p -> Vcard.fold (header p ^ ":" ^ p.value) ^ "\r\n") props)

let split_value sep value =
  let rec loop i start acc =
    if i >= String.length value then
      List.rev
        (untext (String.sub value start (String.length value - start)) :: acc)
    else if value.[i] = '\\' then loop (i + 2) start acc
    else if value.[i] = sep then
      loop (i + 1) (i + 1) (untext (String.sub value start (i - start)) :: acc)
    else loop (i + 1) start acc
  in
  loop 0 0 []

let components = split_value ';'

let profile_url platform handle =
  let simple =
    [
      ("github", "https://github.com/");
      ("gitlab", "https://gitlab.com/");
      ("codeberg", "https://codeberg.org/");
      ("orcid", "https://orcid.org/");
      ("scholar", "https://scholar.google.com/citations?user=");
      ("twitter", "https://twitter.com/");
      ("linkedin", "https://www.linkedin.com/in/");
      ("threads", "https://www.threads.com/@");
      ("instagram", "https://www.instagram.com/");
      ("flickr", "https://www.flickr.com/photos/");
    ]
  in
  match List.assoc_opt platform simple with
  | Some prefix -> prefix ^ percent handle
  | None -> (
      let user, host =
        match String.rindex_opt handle '@' with
        | Some i ->
            ( percent (String.sub handle 0 i),
              String.sub handle (i + 1) (String.length handle - i - 1) )
        | None -> fail "invalid federated account: %s" platform
      in
      match platform with
      | "mastodon" | "pixelfed" -> "https://" ^ host ^ "/@" ^ user
      | "peertube" -> "https://" ^ host ^ "/c/" ^ user ^ "/videos"
      | "matrix" -> "https://matrix.to/#/@" ^ user ^ ":" ^ host
      | "discourse" -> "https://" ^ host ^ "/u/" ^ user
      | "zulip" -> "https://" ^ host
      | _ -> fail "unmapped account platform: %s" platform)

let app_url app handle =
  (match app with
    | "bluesky" -> "https://bsky.app/profile/"
    | "tangled" -> "https://tangled.org/@"
    | "standard-site" -> "https://"
    | _ -> fail "unmapped AT Protocol app: %s" app)
  ^ percent handle

let media_types =
  [
    ("atom", "application/atom+xml");
    ("rss", "application/rss+xml");
    ("json", "application/feed+json");
  ]

let normalized_params ?(client = false) ?(annotations = true) p =
  p.params
  |> List.filter_map (fun (k, v) ->
      if
        client
        && (k = "PROP-ID" || (k = "X-SORTAL-PATH" && p.name <> "X-SORTAL-FIELD"))
        || ((not annotations) && String.starts_with ~prefix:"X-SORTAL-" k)
      then None
      else
        Some
          ( k,
            if List.mem k [ "TYPE"; "VALUE"; "ENCODING" ] then
              String.concat ","
                (List.sort String.compare
                   (String.split_on_char ',' (String.lowercase_ascii v)))
            else v ))
  |> List.sort compare

let signatures ?(client = false) props =
  List.map
    (fun p ->
      ( (if client then "" else p.group),
        p.name,
        normalized_params ~client p,
        p.value ))
    props
  |> List.sort compare

let pointer_key key =
  String.concat "~1"
    (String.split_on_char '/'
       (String.concat "~0" (String.split_on_char '~' key)))

let unpercent s =
  let b = Buffer.create (String.length s) in
  let rec loop i =
    if i < String.length s then
      if s.[i] = '%' then (
        if i + 2 >= String.length s then fail "invalid encoded field path";
        let n =
          try int_of_string ("0x" ^ String.sub s (i + 1) 2)
          with Failure _ -> fail "invalid encoded field path"
        in
        Buffer.add_char b (Char.chr n);
        loop (i + 3))
      else (
        Buffer.add_char b s.[i];
        loop (i + 1))
  in
  loop 0;
  Buffer.contents b

let contact_keys =
  [
    "version";
    "kind";
    "handle";
    "names";
    "emails";
    "accounts";
    "links";
    "affiliations";
    "photo";
    "feeds";
    "vcard";
  ]

let affiliation_keys =
  [ "org"; "department"; "title"; "url"; "address"; "from"; "until" ]

let feed_keys = [ "type"; "url"; "name"; "hint"; "paused" ]
let known_platform key = Option.is_some (Sortal_schema.Platform.of_key key)
let known_app s = List.mem s [ "bluesky"; "tangled"; "standard-site" ]

let known_feed v =
  let kind = field "type" v in
  kind = "manual" || List.mem_assoc kind media_types

(* Only this projection is passed through the closed native schema. Encoding
   and verification always use the complete original tree. *)
let known_contact contact =
  let project keys v =
    obj (List.filter (fun (k, _) -> List.mem k keys) (assoc v))
  in
  let map_field k f v =
    match find k v with None -> v | Some x -> set k (f x) v
  in
  let atproto = function
    | `O _ as v ->
        project [ "handle"; "did"; "apps" ] v
        |> map_field "apps" (fun apps ->
            arr (List.filter (fun v -> known_app (string v)) (list apps)))
    | v -> v
  in
  project contact_keys contact
  |> map_field "links" (fun v ->
      arr
        (List.map
           (function `O _ as x -> project [ "url"; "label" ] x | x -> x)
           (list v)))
  |> map_field "affiliations" (fun v ->
      arr (List.map (project affiliation_keys) (list v)))
  |> map_field "feeds" (fun v ->
      arr (List.map (project feed_keys) (List.filter known_feed (list v))))
  |> map_field "accounts" (fun v ->
      obj
        (List.filter_map
           (fun (k, x) ->
             if not (known_platform k) then None
             else
               Some
                 ( k,
                   if k <> "atproto" then x
                   else
                     match x with
                     | `A xs -> arr (List.map atproto xs)
                     | x -> atproto x ))
           (assoc v)))

let encode ?(version = "3.0") ~uid ~store_id ~originals contact =
  if not (List.mem version [ "3.0"; "4.0" ]) then
    fail "unsupported vCard version";
  let v4 = version = "4.0" in
  if number (get "version" contact) <> 2 then
    fail "this mapping requires Sortal V2";
  let props = ref [] and warnings = ref [] and next_group = ref 0 in
  let group () =
    incr next_group;
    "item" ^ string_of_int !next_group
  in
  let add ?path ?(group = "") ?(params = []) ?(raw = false) name value =
    let params =
      params @ match path with None -> [] | Some s -> [ ("X-SORTAL-PATH", s) ]
    in
    let p =
      {
        group;
        name = String.uppercase_ascii name;
        params;
        value = (if raw then value else text value);
      }
    in
    ignore (header p);
    props := p :: !props
  in
  let url ?path ?group:g ?(params = []) name value =
    let group = match g with Some s -> s | None -> group () in
    add ?path ~group ~params ~raw:true name (uri value);
    if uri value <> value && Option.is_some path then (
      add ~group "X-SORTAL-ORIGINAL-URL" value;
      warnings :=
        "URL normalized; original retained in grouped X-SORTAL-ORIGINAL-URL"
        :: !warnings)
  in
  let extra path value =
    add ~path:(percent path) "X-SORTAL-FIELD" (json_string value)
  in
  let extras value allowed path =
    List.iter
      (fun (key, value) ->
        if not (List.mem key allowed) then
          extra (path ^ "/" ^ pointer_key key) value)
      (assoc value)
  in
  let empty path value =
    match value with
    | `A [] ->
        add ~path "X-SORTAL-EMPTY" "array";
        true
    | `O [] ->
        add ~path "X-SORTAL-EMPTY" "object";
        true
    | _ -> false
  in
  add "BEGIN" "VCARD";
  add "VERSION" version;
  add "X-SORTAL-MAPPING" "4";
  add ~params:(if v4 then [ ("VALUE", "text") ] else []) "UID" uid;
  add ~path:"/handle" "X-SORTAL-ID" (field "handle" contact);
  add "X-SORTAL-STORE" store_id;
  add ~path:"/version" "X-SORTAL-SCHEMA" "2";
  extras contact contact_keys "";
  let names = List.map string (items "names" contact) in
  if names = [] || List.exists (( = ) "") names then
    fail "names must be nonempty strings";
  add ~path:"/names/0"
    ~params:(if v4 then [ ("PREF", "1") ] else [])
    "FN" (List.hd names);
  add ~raw:true "N" (text (List.hd names) ^ ";;;;");
  List.iteri
    (fun i n ->
      add
        ~path:("/names/" ^ string_of_int (i + 1))
        (if v4 then "FN" else "X-SORTAL-ALT-NAME")
        n)
    (List.tl names);
  Option.iter
    (fun k ->
      let kind =
        match string k with
        | "person" -> "individual"
        | "organization" -> "org"
        | _ -> fail "unsupported Sortal kind"
      in
      add ~path:"/kind" (if v4 then "KIND" else "X-ADDRESSBOOKSERVER-KIND") kind;
      if kind = "org" then (
        add "X-ABShowAs" "COMPANY";
        add "ORG" (List.hd names)))
    (find "kind" contact);
  List.iter
    (fun k ->
      Option.iter (fun v -> ignore (empty ("/" ^ k) v)) (find k contact))
    [ "emails"; "links"; "accounts"; "affiliations"; "feeds"; "vcard" ];
  List.iteri
    (fun i e ->
      let params =
        if v4 then if i = 0 then [ ("PREF", "1") ] else []
        else [ ("TYPE", if i = 0 then "INTERNET,PREF" else "INTERNET") ]
      in
      add ~path:("/emails/" ^ string_of_int i) ~params "EMAIL" (string e))
    (items "emails" contact);
  List.iteri
    (fun i link ->
      let path = "/links/" ^ string_of_int i and group = group () in
      match link with
      | `O _ ->
          extras link [ "url"; "label" ] path;
          url ~path:(path ^ "/url") ~group "URL" (field "url" link);
          Option.iter
            (fun s -> add ~path:(path ^ "/label") ~group "X-ABLabel" (string s))
            (find "label" link)
      | _ -> url ~path ~group "URL" (string link))
    (items "links" contact);
  let atproto path g account =
    let is_obj = match account with `O _ -> true | _ -> false in
    if is_obj then extras account [ "handle"; "did"; "apps" ] path;
    let handle = if is_obj then field "handle" account else string account in
    let did = if is_obj then Option.map string (find "did" account) else None in
    let params =
      [ ("X-ATPROTO-HANDLE", handle) ]
      @ (if is_obj then [ ("X-SORTAL-SHAPE", "object") ] else [])
      @ match did with Some d -> [ ("X-ATPROTO-DID", d) ] | None -> []
    in
    add ~path ~group:g ~params ~raw:true "X-ATPROTO"
      ("at://" ^ Option.value ~default:handle did);
    let apps = if is_obj then items "apps" account else [] in
    if is_obj then
      Option.iter
        (fun a -> ignore (empty (path ^ "/apps") a))
        (find "apps" account);
    List.iteri
      (fun i app ->
        let app = string app and group = group () in
        add ~path:(path ^ "/apps/" ^ string_of_int i) ~group "X-ATPROTO-APP" app;
        if known_app app then url ~group "URL" (app_url app handle);
        add ~group "X-ABLabel" app)
      apps;
    if apps = [] then (
      url ~group:g
        ~params:[ ("X-SORTAL-DERIVED", "atproto-default") ]
        "URL" (app_url "bluesky" handle);
      add ~group:g "X-ABLabel" "Bluesky")
  in
  Option.iter
    (fun accounts ->
      List.iter
        (fun (platform, values) ->
          let path = "/accounts/" ^ pointer_key platform in
          if not (known_platform platform) then extra path values
          else
            let entries =
              match values with
              | `A xs ->
                  ignore (empty path values);
                  List.mapi (fun i v -> (path ^ "/" ^ string_of_int i, v)) xs
              | x -> [ (path, x) ]
            in
            List.iter
              (fun (path, value) ->
                let g = group () in
                if platform = "atproto" then atproto path g value
                else
                  let handle = string value in
                  let params =
                    [ ((if v4 then "SERVICE-TYPE" else "TYPE"), platform) ]
                  in
                  let params =
                    if bad_param handle then (
                      add ~group:g "X-SORTAL-USERNAME" handle;
                      params)
                    else
                      params
                      @ [ ((if v4 then "USERNAME" else "X-USER"), handle) ]
                  in
                  url ~path ~group:g ~params
                    (if v4 then "SOCIALPROFILE" else "X-SOCIALPROFILE")
                    (profile_url platform handle);
                  url ~group:g
                    ~params:[ ("X-SORTAL-DERIVED", "profile") ]
                    "URL"
                    (profile_url platform handle);
                  add ~group:g "X-ABLabel" platform)
              entries)
        (assoc accounts))
    (find "accounts" contact);
  List.iteri
    (fun i a ->
      let path = "/affiliations/" ^ string_of_int i and group = group () in
      extras a affiliation_keys path;
      let dates =
        List.filter_map
          (fun k ->
            Option.map
              (fun d ->
                ( "X-VALID-" ^ String.uppercase_ascii k,
                  String.concat "" (String.split_on_char '-' (string d)) ))
              (find k a))
          [ "from"; "until" ]
      in
      let cs =
        [ field "org" a ]
        @ match find "department" a with Some s -> [ string s ] | None -> []
      in
      add ~path ~group ~params:dates ~raw:true "ORG"
        (String.concat ";" (List.map text cs));
      List.iter
        (fun (k, p) ->
          Option.iter
            (fun v ->
              let value = string v and path = path ^ "/" ^ k in
              match k with
              | "url" -> url ~path ~group ~params:dates p value
              | "address" ->
                  add ~path ~group
                    ~params:(dates @ [ ("TYPE", "WORK") ])
                    ~raw:true p
                    (";;" ^ text value ^ ";;;;")
              | _ -> add ~path ~group ~params:dates p value)
            (find k a))
        [ ("title", "TITLE"); ("address", "ADR"); ("url", "URL") ])
    (items "affiliations" contact);
  List.iteri
    (fun i feed ->
      let path = "/feeds/" ^ string_of_int i and group = group () in
      extras feed feed_keys path;
      let kind = field "type" feed in
      let params =
        [ ("X-FEED-TYPE", kind) ]
        @
        match (v4, List.assoc_opt kind media_types) with
        | true, Some s -> [ ("MEDIATYPE", s) ]
        | _ -> []
      in
      url ~path ~group ~params "URL" (field "url" feed);
      if find "name" feed = None then
        add ~group "X-ABLabel" (String.uppercase_ascii kind ^ " feed");
      List.iter
        (fun (k, p) ->
          Option.iter
            (fun v ->
              let value =
                if k = "paused" then if bool v then "TRUE" else "FALSE"
                else string v
              in
              add ~path:(path ^ "/" ^ k) ~group p value)
            (find k feed))
        [
          ("name", "X-ABLabel");
          ("hint", "X-FEED-HINT");
          ("paused", "X-FEED-PAUSED");
        ])
    (items "feeds" contact);
  Option.iter
    (fun value ->
      let photo = string value and group = group () in
      if photo = "" then fail "empty photo path";
      if
        String.starts_with ~prefix:"https://" photo
        || String.starts_with ~prefix:"http://" photo
      then url ~path:"/photo" ~group ~params:[ ("VALUE", "uri") ] "PHOTO" photo
      else
        let data = read (safe_path originals photo) in
        let media =
          if String.starts_with ~prefix:"\255\216\255" data then "jpeg"
          else if String.starts_with ~prefix:"\137PNG\r\n\026\n" data then "png"
          else fail "photo is neither JPEG nor PNG"
        in
        add ~group "X-SORTAL-PHOTO-PATH" photo;
        let encoded = Base64.encode_exn data in
        if v4 then
          add ~path:"/photo" ~group ~raw:true "PHOTO"
            ("data:image/" ^ media ^ ";base64," ^ encoded)
        else
          add ~path:"/photo" ~group
            ~params:
              [ ("ENCODING", "b"); ("TYPE", String.uppercase_ascii media) ]
            ~raw:true "PHOTO" encoded)
    (find "photo" contact);
  let reserved =
    [
      "BEGIN";
      "END";
      "VERSION";
      "UID";
      "FN";
      "N";
      "KIND";
      "URL";
      "PHOTO";
      "ORG";
      "TITLE";
      "ADR";
      "SOCIALPROFILE";
      "X-SOCIALPROFILE";
      "X-ATPROTO";
      "X-ATPROTO-APP";
      "X-ADDRESSBOOKSERVER-KIND";
    ]
  in
  Option.iter
    (fun fields ->
      let groups = Hashtbl.create 8 and overlaid = Hashtbl.create 8 in
      List.iter
        (fun (key, v) ->
          let value = string v in
          if String.exists (fun c -> c = '\n' || c = '\r') (key ^ value) then
            fail "passthrough requires single-line wire strings";
          let p = property (key ^ ":" ^ value) in
          if p.value <> value then fail "invalid vcard passthrough property";
          if
            List.mem p.name reserved
            || String.starts_with ~prefix:"X-SORTAL-" p.name
            || List.exists
                 (fun (k, _) -> String.starts_with ~prefix:"X-SORTAL-" k)
                 p.params
          then
            fail
              "vcard passthrough cannot override mapped identities or \
               annotations";
          let g =
            if p.group = "" then group ()
            else
              match Hashtbl.find_opt groups p.group with
              | Some g -> g
              | None ->
                  let g = group () in
                  Hashtbl.add groups p.group g;
                  g
          in
          if p.name = "EMAIL" then
            let matches =
              List.filter
                (fun q -> q.name = "EMAIL" && untext q.value = untext value)
                !props
            in
            match matches with
            | [ original ] when not (Hashtbl.mem overlaid original.value) ->
                Hashtbl.add overlaid original.value ();
                let replacement =
                  {
                    p with
                    group = g;
                    params =
                      p.params
                      @ [ ("X-SORTAL-PATH", required original "X-SORTAL-PATH") ];
                  }
                in
                props :=
                  List.map
                    (fun q -> if q == original then replacement else q)
                    !props
            | _ -> fail "EMAIL passthrough requires one matching native email"
          else add ~group:g ~params:p.params ~raw:true p.name value;
          add ~path:("/vcard/" ^ percent key) ~group:g "X-SORTAL-VCARD-KEY" key)
        (assoc fields))
    (find "vcard" contact);
  add "END" "VCARD";
  (render (List.rev !props), List.rev !warnings)

let decode data =
  let props = parse data in
  let mapping = (only props "X-SORTAL-MAPPING").value in
  if not (List.mem mapping [ "1"; "2"; "3"; "4" ]) then
    fail "unsupported Sortal mapping";
  let siblings p name =
    List.filter (fun q -> q.group = p.group && q.name = name) props
  in
  let sibling p name =
    match siblings p name with
    | [] -> None
    | [ q ] -> Some (untext q.value)
    | _ -> fail "ambiguous grouped %s" name
  in
  let original_uri p =
    match sibling p "X-SORTAL-ORIGINAL-URL" with
    | Some v when uri v = p.value -> v
    | _ -> p.value
  in
  let date value =
    let len = String.length value in
    if
      (not (List.mem len [ 4; 6; 8 ]))
      || not (String.for_all (function '0' .. '9' -> true | _ -> false) value)
    then fail "invalid temporal date";
    String.sub value 0 4
    ^ (if len >= 6 then "-" ^ String.sub value 4 2 else "")
    ^ if len = 8 then "-" ^ String.sub value 6 2 else ""
  in
  let photos = ref [] in
  let assignments =
    List.filter_map
      (fun p ->
        Option.map
          (fun path ->
            let path, value =
              match p.name with
              | "X-SORTAL-FIELD" ->
                  if mapping <> "4" then
                    fail "field extension requires mapping 4";
                  (unpercent path, json (untext p.value))
              | "X-SORTAL-VCARD-KEY" ->
                  let key = untext p.value in
                  let original = property (key ^ ":") in
                  let matches =
                    siblings p original.name
                    |> List.filter (fun q ->
                        normalized_params ~annotations:false q
                        = normalized_params ~annotations:false original)
                  in
                  let value =
                    match matches with
                    | [ q ] -> q.value
                    | _ -> fail "vcard passthrough lost its grouped property"
                  in
                  (* A native header is a single object key, even when it contains '/'. *)
                  let escape s =
                    String.concat "~1"
                      (String.split_on_char '/'
                         (String.concat "~0" (String.split_on_char '~' s)))
                  in
                  ("/vcard/" ^ escape key, str value)
              | "X-SORTAL-SCHEMA" -> (path, int (int_of_string p.value))
              | "KIND" | "X-ADDRESSBOOKSERVER-KIND" ->
                  ( path,
                    str
                      (match String.lowercase_ascii p.value with
                      | "individual" -> "person"
                      | "org" -> "organization"
                      | _ -> fail "unknown kind") )
              | "SOCIALPROFILE" | "X-SOCIALPROFILE" ->
                  let handle =
                    match
                      ( param p "USERNAME",
                        param p "X-USER",
                        sibling p "X-SORTAL-USERNAME" )
                    with
                    | Some s, _, _ | _, Some s, _ | _, _, Some s -> s
                    | _ -> fail "profile has no recoverable username"
                  in
                  let platform =
                    match param p "SERVICE-TYPE" with
                    | Some s -> s
                    | None -> required p "TYPE"
                  in
                  if uri (profile_url platform handle) <> p.value then
                    fail "social URL/username conflict requires reconciliation";
                  Option.iter
                    (fun mirror ->
                      if mirror <> p.value then
                        fail
                          "social visible URL conflict requires reconciliation")
                    (sibling p "URL");
                  (path, str handle)
              | "X-ATPROTO" ->
                  let handle = required p "X-ATPROTO-HANDLE" in
                  let did = param p "X-ATPROTO-DID" in
                  if p.value <> "at://" ^ Option.value ~default:handle did then
                    fail "AT Protocol identity conflict";
                  Option.iter
                    (fun mirror ->
                      if mirror <> app_url "bluesky" handle then
                        fail "AT Protocol visible URL conflict")
                    (sibling p "URL");
                  ( path,
                    if param p "X-SORTAL-SHAPE" = Some "object" then
                      obj
                        ([ ("handle", str handle) ]
                        @
                        match did with
                        | Some d -> [ ("did", str d) ]
                        | None -> [])
                    else str handle )
              | "ORG" ->
                  let cs =
                    match components p.value with
                    | [ org ] -> [ ("org", str org) ]
                    | [ org; department ] ->
                        [ ("org", str org); ("department", str department) ]
                    | _ ->
                        fail
                          "additional organization components require \
                           passthrough"
                  in
                  ( path,
                    obj
                      (cs
                      @ List.filter_map
                          (fun k ->
                            Option.map
                              (fun d -> (k, str (date d)))
                              (param p ("X-VALID-" ^ String.uppercase_ascii k)))
                          [ "from"; "until" ]) )
              | "ADR" -> (
                  match components p.value with
                  | [ ""; ""; address; ""; ""; ""; "" ] -> (path, str address)
                  | _ -> fail "structured remote address requires passthrough")
              | ("URL" | "X-FEED")
                when p.name = "X-FEED" || param p "X-FEED-TYPE" <> None ->
                  let kind = required p "X-FEED-TYPE" in
                  Option.iter
                    (fun m ->
                      if
                        List.assoc_opt kind media_types
                        <> Some (String.lowercase_ascii m)
                      then fail "feed type/media type conflict")
                    (param p "MEDIATYPE");
                  ( path,
                    obj [ ("type", str kind); ("url", str (original_uri p)) ] )
              | "X-FEED-PAUSED" ->
                  ( path,
                    `Bool
                      (match String.uppercase_ascii p.value with
                      | "TRUE" -> true
                      | "FALSE" -> false
                      | _ -> fail "invalid paused value") )
              | "URL" -> (path, str (original_uri p))
              | "PHOTO" -> (
                  match sibling p "X-SORTAL-PHOTO-PATH" with
                  | None -> (path, str (original_uri p))
                  | Some local ->
                      let encoded =
                        if String.starts_with ~prefix:"data:image/" p.value then
                          let i = String.index p.value ',' in
                          String.sub p.value (i + 1)
                            (String.length p.value - i - 1)
                        else p.value
                      in
                      let raw =
                        match Base64.decode encoded with
                        | Ok s -> s
                        | Error (`Msg s) -> fail "%s" s
                      in
                      if List.mem_assoc local !photos then
                        fail "duplicate photo path";
                      photos := (local, raw) :: !photos;
                      (path, str local))
              | "X-SORTAL-EMPTY" ->
                  ( path,
                    match p.value with
                    | "array" -> arr []
                    | "object" -> obj []
                    | _ -> fail "invalid empty marker" )
              | _ -> (path, str (untext p.value))
            in
            (path, value))
          (param p "X-SORTAL-PATH"))
      props
  in
  List.iter
    (fun p ->
      if p.name = "X-SORTAL-FIELD" then (
        let path = unpercent (required p "X-SORTAL-PATH") in
        if path = "" then fail "field extension cannot replace the contact";
        List.iter
          (fun (other, _) ->
            if String.starts_with ~prefix:(path ^ "/") other then
              fail "overlapping extension field paths")
          assignments))
    props;
  let unpointer key =
    let b = Buffer.create (String.length key) in
    let rec loop i =
      if i < String.length key then
        if key.[i] = '~' && i + 1 < String.length key then (
          Buffer.add_char b
            (match key.[i + 1] with
            | '0' -> '~'
            | '1' -> '/'
            | _ -> fail "invalid path escape");
          loop (i + 2))
        else (
          Buffer.add_char b key.[i];
          loop (i + 1))
    in
    loop 0;
    Buffer.contents b
  in
  let is_index s =
    s <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) s
  in
  let rec assign node keys value =
    match keys with
    | [] -> value
    | k :: rest -> (
        let initial () =
          match rest with h :: _ when is_index h -> arr [] | _ -> obj []
        in
        match node with
        | `O xs ->
            if rest = [] && List.mem_assoc k xs then
              fail "overlapping Sortal field paths";
            set k
              (assign
                 (Option.value ~default:(initial ()) (List.assoc_opt k xs))
                 rest value)
              node
        | `A xs ->
            let i =
              if is_index k then int_of_string k
              else fail "invalid collection index"
            in
            if i > 10000 then fail "invalid collection index";
            let xs =
              xs @ List.init (max 0 (i + 1 - List.length xs)) (fun _ -> `Null)
            in
            arr
              (List.mapi
                 (fun j x ->
                   if i = j then
                     assign (if x = `Null then initial () else x) rest value
                   else x)
                 xs)
        | _ -> fail "overlapping Sortal field paths")
  in
  let seen = Hashtbl.create 32 in
  let depth s = List.length (String.split_on_char '/' s) in
  let result =
    List.stable_sort
      (fun (a, _) (b, _) -> compare (depth a) (depth b))
      assignments
    |> List.fold_left
         (fun result (path, value) ->
           if
             (not (String.starts_with ~prefix:"/" path))
             || Hashtbl.mem seen path
           then fail "invalid or duplicate Sortal field path";
           Hashtbl.add seen path ();
           assign result
             (List.map unpointer (List.tl (String.split_on_char '/' path)))
             value)
         (obj [])
  in
  List.iter
    (fun p ->
      if p.name = "X-ATPROTO-APP" then
        Option.iter
          (fun path ->
            let keys = List.tl (String.split_on_char '/' path) in
            let keys =
              List.filteri (fun i _ -> i < List.length keys - 2) keys
            in
            let parent =
              List.fold_left
                (fun v k ->
                  match v with
                  | `A xs -> List.nth xs (int_of_string k)
                  | _ -> get k v)
                result keys
            in
            Option.iter
              (fun mirror ->
                if mirror <> app_url (untext p.value) (field "handle" parent)
                then fail "AT Protocol app visible URL conflict")
              (sibling p "URL"))
          (param p "X-SORTAL-PATH"))
    props;
  (result, List.rev !photos)
