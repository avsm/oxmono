(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Termanil_model
module C = Sortal_carddav.Common
module V = Sortal_carddav.Mapping

let card (r : Sortal_carddav.Remote.card) =
  let values name =
    List.filter_map
      (fun (p : V.property) ->
        if p.name = name then Some (V.untext p.value) else None)
      r.props
  in
  {
    key = "carddav:" ^ r.href;
    name = (match values "FN" with n :: _ -> n | [] -> r.uid);
    emails = values "EMAIL";
    sources = [ r.href ];
    metadata = Sortal_carddav.Metadata.fields r.props;
  }

let local root =
  let root = Unix.realpath root in
  let directory = Sortal_carddav.Store.cards_dir root in
  let warnings = ref [] and ids = Hashtbl.create 256 in
  let contacts =
    Sys.readdir directory |> Array.to_list |> List.sort String.compare
    |> List.filter (fun s ->
        String.lowercase_ascii (Filename.extension s) = ".vcf")
    |> List.filter_map (fun name ->
        let path = Filename.concat directory name in
        try
          if (Unix.lstat path).Unix.st_kind <> Unix.S_REG then
            C.fail "vCard must be a regular file";
          let data = Dooit.Common.read ~limit:(8 * 1024 * 1024) path in
          let props = V.parse data in
          let uid = V.untext (V.only props "UID").value in
          if uid = "" then C.fail "empty vCard UID";
          if Hashtbl.mem ids uid then C.fail "duplicate vCard UID";
          let contact = card { uid; href = path; etag = None; data; props } in
          Hashtbl.add ids uid ();
          Some { contact with key = "vcard:" ^ root ^ ":" ^ uid }
        with
        | C.Error s | Dooit.Common.Error s | Failure s | Sys_error s ->
            warnings := (name ^ ": " ^ s) :: !warnings;
            None
        | Unix.Unix_error (e, _, _) ->
            warnings := (name ^ ": " ^ Unix.error_message e) :: !warnings;
            None)
  in
  if
    contacts = []
    && Array.exists
         (fun s -> List.mem (Filename.extension s) [ ".yaml"; ".yml" ])
         (Sys.readdir root)
  then
    warnings :=
      "Select the native vCard store using contacts.vcard_root" :: !warnings;
  (contacts, List.rev !warnings)

let remote ?collection dav =
  let book = Sortal_carddav.Remote.discover ?collection dav in
  List.map card (Sortal_carddav.Remote.fetch_all dav book)

let combine local remote =
  List.sort (fun a b -> compare (a.name, a.key) (b.name, b.key)) (local @ remote)

let for_addresses addresses contacts =
  let normal = String.lowercase_ascii in
  List.filter
    (fun c ->
      List.exists
        (fun email -> List.exists (fun a -> normal a = normal email) addresses)
        c.emails)
    contacts
