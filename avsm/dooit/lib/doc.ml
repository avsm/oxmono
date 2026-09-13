(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Common

type t = {
  raw : string;
  header : string;
  body : string;
  meta : value;
  prefix : string;
  separator : string;
}

let required k m =
  let s = field k m in
  if String.trim s = "" then fail "%s must not be empty" k;
  s

let statuses = [ "open"; "active"; "blocked"; "done"; "cancelled" ]

let validate m =
  if field "schema" m <> "dooit/v1" then fail "unsupported note schema";
  check_uuid (field "id" m);
  ignore (required "title" m);
  instant (field "created_at" m);
  if not (List.mem (field "status" m) statuses) then
    fail "unsupported task status";
  Option.iter (fun v -> instant (string v)) (find "deleted_at" m);
  Option.iter
    (fun v ->
      let s = string v in
      if String.length s <> 10 then fail "due must be YYYY-MM-DD";
      try
        let y = int_of_string (String.sub s 0 4)
        and mo = int_of_string (String.sub s 5 2)
        and d = int_of_string (String.sub s 8 2) in
        if s.[4] <> '-' || s.[7] <> '-' || Ptime.of_date (y, mo, d) = None then
          fail "invalid due date"
      with Failure _ -> fail "invalid due date")
    (find "due" m);
  let tags = List.map string (items "tags" m) in
  if List.exists (fun s -> String.trim s = "") tags then fail "empty tag";
  if List.length tags <> List.length (List.sort_uniq compare tags) then
    fail "duplicate tags";
  let link_ids =
    List.map
      (fun link ->
        let id = required "id" link in
        ignore (required "rel" link);
        let typ = required "type" link and target = get "target" link in
        ignore (assoc target);
        (match typ with
        | "jmap-email" ->
            List.iter
              (fun k -> ignore (required k target))
              [ "service"; "account_id"; "email_id" ];
            ignore
              (get_ok (Fetch.Middleware.Url.of_string (field "service" target)))
        | "url" ->
            let uri = required "uri" target in
            if not (String.contains uri ':') then
              fail "link URL must be absolute"
        | "dooit" ->
            check_uuid (field "store_id" target);
            check_uuid (field "id" target)
        | _ -> ());
        id)
      (items "links" m)
  in
  if List.length link_ids <> List.length (List.sort_uniq compare link_ids) then
    fail "duplicate link IDs"

let parse raw =
  if String.length raw > 1024 * 1024 then fail "note exceeds 1 MiB";
  ignore (normalize raw);
  let line i =
    let z =
      match String.index_from_opt raw i '\n' with
      | None -> String.length raw
      | Some n -> n + 1
    in
    (String.sub raw i (z - i), z)
  in
  let prefix, start = line 0 in
  let delimiter s = s = "---\n" || s = "---\r\n" || s = "---" in
  if not (delimiter prefix) then fail "note must start with YAML frontmatter";
  let rec close i =
    if i >= String.length raw then fail "unclosed YAML frontmatter";
    let s, z = line i in
    if delimiter s then (i, z, s) else close z
  in
  let stop, body_start, separator = close start in
  let header = String.sub raw start (stop - start) in
  let meta = yaml header in
  validate meta;
  let body = String.sub raw body_start (String.length raw - body_start) in
  { raw; header; body; meta; prefix; separator }

let id t = field "id" t.meta
let title t = field "title" t.meta
let status t = field "status" t.meta
let tags t = List.map string (items "tags" t.meta)
let deleted t = Option.is_some (find "deleted_at" t.meta)
let revision t = digest t.raw

let update t ~meta ~body =
  validate meta;
  let header = Yaml_edit.update t.header meta in
  let result = parse (t.prefix ^ header ^ t.separator ^ body) in
  if not (equal result.meta meta && result.body = body) then
    fail "note patch verification failed";
  result

let create ?id:given ?(tags = []) ?(links = []) ?(body = "") ?due ~title () =
  let id = Option.value given ~default:(new_uuid ()) in
  let meta =
    obj
      ([
         ("schema", str "dooit/v1");
         ("id", str id);
         ("title", str title);
         ("status", str "open");
         ("created_at", str (now ()));
         ("tags", arr (List.map str tags));
         ("links", arr links);
       ]
      @ match due with None -> [] | Some s -> [ ("due", str s) ])
  in
  parse
    ("---\n"
    ^ String.concat ""
        (List.map (fun (k, v) -> k ^ ": " ^ json_string v ^ "\n") (assoc meta))
    ^ "---\n" ^ body)

let change k value t =
  update t
    ~meta:
      (match value with None -> remove k t.meta | Some v -> set k v t.meta)
    ~body:t.body

let public t =
  obj
    [
      ("schema", str "dooit.result/v1");
      ("id", str (id t));
      ("revision", str (revision t));
      ("metadata", t.meta);
      ("body", str t.body);
    ]

let immutable a b =
  List.iter
    (fun k ->
      if find k a.meta <> find k b.meta then
        fail "immutable field changed: %s" k)
    [ "schema"; "id"; "created_at" ]

let patch t patch =
  if field "schema" patch <> "dooit.patch/v1" then
    fail "unsupported patch schema";
  let result =
    List.fold_left
      (fun t op ->
        let k = field "op" op in
        match k with
        | "set_title" -> change "title" (Some (get "value" op)) t
        | "set_status" -> change "status" (Some (get "value" op)) t
        | "set_due" ->
            change "due"
              (match get "value" op with `Null -> None | x -> Some x)
              t
        | "replace_body" -> update t ~meta:t.meta ~body:(field "value" op)
        | "add_tag" ->
            let tag = field "value" op in
            let tags = tags t in
            change "tags"
              (Some
                 (arr
                    (List.map str
                       (if List.mem tag tags then tags else tags @ [ tag ]))))
              t
        | "remove_tag" ->
            change "tags"
              (Some
                 (arr
                    (List.filter
                       (fun v -> string v <> field "value" op)
                       (items "tags" t.meta))))
              t
        | "add_link" ->
            let link = get "value" op in
            let links = items "links" t.meta in
            if List.exists (fun v -> field "id" v = field "id" link) links then
              fail "link ID already exists";
            change "links" (Some (arr (links @ [ link ]))) t
        | "remove_link" ->
            change "links"
              (Some
                 (arr
                    (List.filter
                       (fun v -> field "id" v <> field "value" op)
                       (items "links" t.meta))))
              t
        | _ -> fail "unknown patch operation: %s" k)
      t
      (list (get "operations" patch))
  in
  immutable t result;
  result
