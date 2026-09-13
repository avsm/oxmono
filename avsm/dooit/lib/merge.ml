(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Common

exception Conflict of string

let conflict s = raise (Conflict s)

let choose name b l r =
  if l = r then l else if l = b then r else if r = b then l else conflict name

let ordered_merge b l r =
  if l = r then l
  else if l = b then r
  else if r = b then l
  else
    let all = List.sort_uniq String.compare (b @ l @ r) in
    let present =
      List.filter
        (fun s ->
          choose ("tag " ^ s) (List.mem s b) (List.mem s l) (List.mem s r))
        all
    in
    let constraints xs =
      let xs = List.filter (fun x -> List.mem x present) xs in
      let rec pairs = function
        | [] -> []
        | x :: xs -> List.map (fun y -> (x, y)) xs @ pairs xs
      in
      pairs xs
    in
    let edges = constraints l @ constraints r in
    let rec sort done_ = function
      | [] -> List.rev done_
      | remaining -> (
          let free =
            List.filter
              (fun x ->
                not
                  (List.exists
                     (fun (a, z) -> z = x && List.mem a remaining)
                     edges))
              remaining
          in
          match List.sort String.compare free with
          | [] -> conflict "tag ordering"
          | x :: _ -> sort (x :: done_) (List.filter (( <> ) x) remaining))
    in
    sort [] present

let links b l r =
  let bindings xs = List.map (fun v -> (field "id" v, v)) xs in
  let bs = bindings b and ls = bindings l and rs = bindings r in
  let ids = List.sort_uniq compare (List.map fst (bs @ ls @ rs)) in
  let values =
    List.filter_map
      (fun id ->
        Option.map
          (fun v -> (id, v))
          (choose ("link " ^ id) (List.assoc_opt id bs) (List.assoc_opt id ls)
             (List.assoc_opt id rs)))
      ids
  in
  let order =
    ordered_merge (List.map fst bs) (List.map fst ls) (List.map fst rs)
  in
  List.filter_map (fun id -> List.assoc_opt id values) order

let run ~base ~local ~remote =
  if local.Doc.raw = remote.Doc.raw then Ok local
  else if local.raw = base.Doc.raw then Ok remote
  else if remote.raw = base.raw then Ok local
  else
    try
      Doc.immutable base local;
      Doc.immutable base remote;
      let dl = find "deleted_at" local.meta <> find "deleted_at" base.meta
      and dr = find "deleted_at" remote.meta <> find "deleted_at" base.meta in
      if dl || dr then conflict "deletion or restore concurrent with an edit";
      let body = choose "body" base.body local.body remote.body in
      (* Preserve one side's exact presentation. If both independently change
       formatting, retaining a conflict is safer than canonical re-emission. *)
      let local_style =
        (Doc.update base ~meta:local.meta ~body:local.body).raw <> local.raw
      and remote_style =
        (Doc.update base ~meta:remote.meta ~body:remote.body).raw <> remote.raw
      in
      if local_style && remote_style then conflict "concurrent formatting edits";
      let source = if remote_style then remote else local in
      let names =
        List.sort_uniq compare
          (keys base.meta @ keys local.meta @ keys remote.meta)
      in
      let meta =
        List.fold_left
          (fun acc k ->
            let b = find k base.meta
            and l = find k local.meta
            and r = find k remote.meta in
            let v =
              if l = r || l = b || r = b then choose k b l r
              else if k = "tags" then
                Some
                  (arr
                     (List.map str
                        (ordered_merge (Doc.tags base) (Doc.tags local)
                           (Doc.tags remote))))
              else if k = "links" then
                Some
                  (arr
                     (links (items k base.meta) (items k local.meta)
                        (items k remote.meta)))
              else conflict k
            in
            match v with None -> remove k acc | Some v -> set k v acc)
          source.meta names
      in
      Ok (Doc.update source ~meta ~body)
    with Conflict s | Error s -> Error s
