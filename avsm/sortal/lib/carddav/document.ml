(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Common
module Contact = Sortal_schema.Contact

let value raw = fst (Mapping.decode raw)

let project fields =
  let contact =
    get_ok
      (Jsont_bytesrw.decode_string Contact.json_t
         (json_string (Mapping.known_contact fields)))
  in
  get_ok (Contact.check contact);
  contact

let contact raw = Contact.with_source (project (value raw)) (Some raw)

let of_contact contact =
  get_ok (Contact.check contact);
  json (get_ok (Jsont_bytesrw.encode_string Contact.json_t contact))

let same a b =
  match (a, b) with
  | None, None -> true
  | Some a, Some b -> equal a b
  | _ -> false

(* Patch the projection, never serialize it over the complete field tree. *)
let rec patch full before after =
  if equal before after then full
  else
    match (full, before, after) with
    | `O _, `O old, `O newer ->
        List.fold_left
          (fun result key ->
            let old = List.assoc_opt key old
            and newer = List.assoc_opt key newer in
            if same old newer then result
            else
              match (find key result, old, newer) with
              | Some full, Some (`A _ as before), None ->
                  let retained = patch full before (arr []) in
                  if retained = arr [] then remove key result
                  else set key retained result
              | Some full, Some (`O _ as before), None ->
                  let retained = patch full before (obj []) in
                  if retained = obj [] then remove key result
                  else set key retained result
              | _, _, None -> remove key result
              | Some full, Some before, Some after ->
                  set key (patch full before after) result
              | Some full, None, Some (`A _ as after) ->
                  set key (patch full (arr []) after) result
              | Some full, None, Some (`O _ as after) ->
                  set key (patch full (obj []) after) result
              | _, _, Some after -> set key after result)
          full
          (List.sort_uniq String.compare (List.map fst (old @ newer)))
    | `A full, `A old, `A newer ->
        let identity = function
          | `O fields ->
              List.find_map
                (fun k -> List.assoc_opt k fields)
                [ "url"; "org"; "handle" ]
          | `String _ as v -> Some v
          | _ -> None
        in
        let choose next candidates =
          let exact = List.filter (fun (_, v) -> equal v next) candidates in
          let matches =
            if exact <> [] then exact
            else
              List.filter
                (fun (_, v) -> identity v <> None && identity v = identity next)
                candidates
          in
          match matches with
          | [] -> None
          | [ x ] -> Some x
          | _ -> fail "ambiguous repeated values in contact edit"
        in
        let unmatched = ref (List.mapi (fun i v -> (i, v)) full) in
        let positions =
          List.mapi
            (fun i previous ->
              let found =
                match choose previous !unmatched with
                | Some x -> x
                | None when List.length full = List.length old -> (
                    match List.assoc_opt i !unmatched with
                    | Some v -> (i, v)
                    | None -> fail "ambiguous contact projection")
                | None -> fail "cannot align the complete contact projection"
              in
              unmatched := List.remove_assoc (fst found) !unmatched;
              (fst found, previous))
            old
        in
        let remaining = ref positions in
        let replacements =
          List.mapi
            (fun i next ->
              let previous =
                match choose next !remaining with
                | Some _ as found -> found
                | None when List.length old = List.length newer ->
                    let j, _ = List.nth positions i in
                    Option.map (fun v -> (j, v)) (List.assoc_opt j !remaining)
                | None -> None
              in
              match previous with
              | None -> next
              | Some (j, old) ->
                  remaining := List.remove_assoc j !remaining;
                  patch (List.nth full j) old next)
            newer
        in
        let replacements = ref replacements in
        let merged =
          List.mapi (fun i v -> (i, v)) full
          |> List.filter_map (fun (i, v) ->
              if not (List.mem_assoc i positions) then Some v
              else
                match !replacements with
                | [] -> None
                | next :: rest ->
                    replacements := rest;
                    Some next)
        in
        arr (merged @ !replacements)
    | `A [ _ ], (`String _ | `O _), (`String _ | `O _) ->
        patch full (arr [ before ]) (arr [ after ])
    | `A [ _ ], (`String _ | `O _), `A _ -> patch full (arr [ before ]) after
    | `A _, `A _, (`String _ | `O _) -> patch full before (arr [ after ])
    | `O fields, (`String _ | `O _), (`String _ | `O _)
      when List.mem_assoc "url" fields || List.mem_assoc "handle" fields ->
        let key = if List.mem_assoc "url" fields then "url" else "handle" in
        let object_value = function
          | `String _ as value -> obj [ (key, value) ]
          | value -> value
        in
        patch full (object_value before) (object_value after)
    | _ -> after

let path p = Mapping.param p "X-SORTAL-PATH"

let key props (p : Mapping.property) =
  match path p with
  | Some path -> "path:" ^ path ^ ":" ^ p.name
  | None when p.group <> "" -> (
      let anchor =
        List.find_map
          (fun (q : Mapping.property) ->
            if q.group = p.group then path q else None)
          props
      in
      match anchor with
      | Some s -> "group:" ^ s ^ ":" ^ p.name
      | None -> "raw:" ^ Mapping.header p)
  | None -> "plain:" ^ p.name

let update ~originals raw fields =
  let before = value raw in
  ignore (project fields);
  if equal before fields then raw
  else
    let props = Mapping.parse raw in
    let one name = Mapping.untext (Mapping.only props name).value in
    let encode fields =
      Mapping.parse
        (fst
           (Mapping.encode ~version:(one "VERSION") ~uid:(one "UID")
              ~store_id:(one "X-SORTAL-STORE") ~originals fields))
    in
    let old = encode before and newer = encode fields in
    let keyed ps = List.map (fun p -> (key ps p, p)) ps in
    let old_keys = keyed old and new_keys = keyed newer in
    let raw_keys = keyed props in
    let groups = Hashtbl.create 16 in
    let used = ref (List.map (fun p -> p.Mapping.group) props) in
    List.iter
      (fun (k, (p : Mapping.property)) ->
        if p.group <> "" && not (Hashtbl.mem groups p.group) then
          match List.assoc_opt k raw_keys with
          | Some q when q.group <> "" -> Hashtbl.add groups p.group q.group
          | _ -> ())
      new_keys;
    let regroup (p : Mapping.property) =
      if p.group = "" then p
      else
        let group =
          match Hashtbl.find_opt groups p.group with
          | Some g -> g
          | None ->
              let rec fresh i =
                let g = "sortal" ^ string_of_int i in
                if List.mem g !used then fresh (i + 1) else g
              in
              let g = fresh 1 in
              used := g :: !used;
              Hashtbl.add groups p.group g;
              g
        in
        { p with group }
    in
    let consumed = Hashtbl.create 32 in
    let updated =
      List.filter_map
        (fun (k, (p : Mapping.property)) ->
          match List.assoc_opt k old_keys with
          | None -> Some p
          | Some old -> (
              if Hashtbl.mem consumed k then
                fail "ambiguous mapped vCard property: %s" k;
              Hashtbl.add consumed k ();
              match List.assoc_opt k new_keys with
              | None -> None
              | Some next ->
                  if old.value = next.value && old.params = next.params then
                    Some p
                  else if path p = None && p.value <> old.value then Some p
                  else
                    let unmodelled_group_member =
                      p.group <> ""
                      && List.exists
                           (fun (k, (q : Mapping.property)) ->
                             q.group = p.group
                             && not (List.mem_assoc k old_keys))
                           raw_keys
                    in
                    if
                      unmodelled_group_member && old.value <> next.value
                      && List.mem p.name
                           [
                             "URL";
                             "EMAIL";
                             "ORG";
                             "X-SOCIALPROFILE";
                             "SOCIALPROFILE";
                           ]
                    then fail "edit would detach unmodelled grouped metadata";
                    let params =
                      List.filter
                        (fun (k, _) ->
                          (not (List.mem_assoc k old.params))
                          && not (List.mem_assoc k next.params))
                        p.params
                      @ List.map
                          (fun (k, v) ->
                            if List.assoc_opt k old.params = Some v then
                              ( k,
                                Option.value ~default:v
                                  (List.assoc_opt k p.params) )
                            else (k, v))
                          next.params
                    in
                    Some { (regroup next) with params }))
        raw_keys
    in
    let added =
      List.filter_map
        (fun (k, p) ->
          if Hashtbl.mem consumed k || List.mem_assoc k raw_keys then None
          else Some (regroup p))
        new_keys
    in
    let result =
      Mapping.render
        (List.concat_map
           (fun p -> if p.Mapping.name = "END" then added @ [ p ] else [ p ])
           updated)
    in
    if not (equal (value result) fields) then
      fail "vCard edit did not retain the complete contact";
    result

let edit ~originals raw contact =
  let full = value raw in
  let before = of_contact (project full) and after = of_contact contact in
  update ~originals raw (patch full before after)
