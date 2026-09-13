(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Contact = Sortal_schema.Contact

type t = { data_dir : Eio.Fs.dir_ty Eio.Path.t }

let create fs app_name =
  let xdg = Xdge.create ~create_dirs:false fs app_name in
  let data_dir = Xdge.data_dir xdg in
  { data_dir }

let create_from_xdg xdg =
  let data_dir = Xdge.data_dir xdg in
  { data_dir }

let create_at fs root = { data_dir = Eio.Path.(fs / root) }
let data_dir t = t.data_dir

module Carddav = Sortal_carddav
module Document = Carddav.Document
module Common = Carddav.Common

let root t = Eio.Path.native_exn t.data_dir
let cards t = Filename.concat (root t) "cards"
let marker t = Filename.concat (root t) "store.json"

let identity t =
  let m = Common.load_json (marker t) in
  if Common.number (Common.get "version" m) <> 1 then
    Common.fail "unsupported Sortal store version";
  let id = Common.field "store_id" m in
  ignore (Common.uuid_bytes id);
  id

let regular path =
  if (Unix.lstat path).Unix.st_kind <> Unix.S_REG then
    Common.fail "contact must be a regular file: %s" path

let documents t =
  if not (Common.exists (marker t)) then (
    if Common.exists (root t) then
      Array.iter
        (fun name ->
          if List.mem (Filename.extension name) [ ".yaml"; ".yml" ] then
            Common.fail "legacy contact store requires the one-off migration";
          if name = "cards" then Common.fail "vCard store is missing store.json")
        (Sys.readdir (root t));
    [])
  else
    let store_id = identity t in
    let handles = Hashtbl.create 512 and uids = Hashtbl.create 512 in
    Sys.readdir (cards t)
    |> Array.to_list |> List.sort String.compare
    |> List.filter_map (fun name ->
        if Filename.extension name <> ".vcf" then None
        else
          let path = Common.safe_path (cards t) name in
          regular path;
          let raw = Common.read path in
          let props = Carddav.Mapping.parse raw in
          let one n =
            Carddav.Mapping.untext (Carddav.Mapping.only props n).value
          in
          let uid = one "UID" in
          if name <> uid ^ ".vcf" || one "X-SORTAL-STORE" <> store_id then
            Common.fail "vCard filename or store identity mismatch: %s" name;
          let contact = Document.contact raw in
          let handle = Contact.handle contact in
          if handle = "" || Hashtbl.mem handles handle || Hashtbl.mem uids uid
          then Common.fail "duplicate or empty contact identity: %s" handle;
          Hashtbl.add handles handle ();
          Hashtbl.add uids uid ();
          Some (name, contact))

let list t = List.map snd (documents t) |> List.sort Contact.compare

let lookup t handle =
  List.find_map
    (fun (_, c) -> if Contact.handle c = handle then Some c else None)
    (documents t)

let filename t handle =
  match
    List.find_opt (fun (_, c) -> Contact.handle c = handle) (documents t)
  with
  | Some (name, _) -> "cards/" ^ name
  | None -> raise Not_found

let with_lock t f =
  Common.mkdir (root t);
  let lock = Filename.concat (root t) ".sortal.lock" in
  let fd = Unix.openfile lock [ Unix.O_RDWR; Unix.O_CREAT ] 0o600 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      Unix.lockf fd Unix.F_LOCK 0;
      Fun.protect ~finally:(fun () -> Unix.lockf fd Unix.F_ULOCK 0) f)

let initialize t =
  if Common.exists (marker t) then identity t
  else (
    ignore (documents t);
    let id = Common.new_uuid () in
    Common.mkdir (cards t);
    Common.save_json (marker t)
      (Common.obj [ ("version", Common.int 1); ("store_id", Common.str id) ]);
    id)

let save t contact =
  with_lock t (fun () ->
      let entries = documents t in
      let original = Contact.source contact in
      let existing =
        match original with
        | None ->
            List.find_opt
              (fun (_, c) -> Contact.handle c = Contact.handle contact)
              entries
        | Some raw -> (
            let props = Carddav.Mapping.parse raw in
            let uid =
              Carddav.Mapping.untext (Carddav.Mapping.only props "UID").value
            in
            match
              List.find_opt (fun (name, _) -> name = uid ^ ".vcf") entries
            with
            | None -> Common.fail "contact was deleted since it was read"
            | Some (_, c) as found ->
                if Contact.source c <> original then
                  Common.fail "contact changed since it was read";
                found)
      in
      List.iter
        (fun (name, c) ->
          if
            Contact.handle c = Contact.handle contact
            && Option.map fst existing <> Some name
          then
            Common.fail "contact handle already exists: %s" (Contact.handle c))
        entries;
      let store_id = initialize t in
      let name, before, after =
        match existing with
        | Some (name, c) ->
            let raw = Option.get (Contact.source c) in
            (name, Some raw, Document.edit ~originals:(root t) raw contact)
        | None ->
            let uid = Common.new_uuid () in
            let data, _ =
              Carddav.Mapping.encode ~uid ~store_id ~originals:(root t)
                (Document.of_contact contact)
            in
            (uid ^ ".vcf", None, data)
      in
      if before <> Some after then
        let path = Common.safe_path (cards t) name in
        let check () =
          match before with
          | None -> if Common.exists path then Common.fail "contact appeared"
          | Some raw ->
              regular path;
              if Common.read path <> raw then Common.fail "contact changed"
        in
        Common.atomic_write ~check path after)

let delete t handle =
  with_lock t (fun () ->
      match
        List.find_opt (fun (_, c) -> Contact.handle c = handle) (documents t)
      with
      | None -> ()
      | Some (name, c) ->
          let path = Common.safe_path (cards t) name in
          regular path;
          if Some (Common.read path) <> Contact.source c then
            Common.fail "contact changed";
          Unix.unlink path)

(* Contact modification helpers *)
let update_contact t handle f =
  match lookup t handle with
  | None -> Error (Printf.sprintf "Contact not found: %s" handle)
  | Some contact ->
      let updated = Contact.with_source (f contact) (Contact.source contact) in
      save t updated;
      Ok ()

let with_accounts contact accounts =
  Contact.make ~handle:(Contact.handle contact) ~names:(Contact.names contact)
    ~kind:(Contact.kind contact) ~emails:(Contact.emails contact) ~accounts
    ~links:(Contact.links contact)
    ~affiliations:(Contact.affiliations contact)
    ?photo:(Contact.photo contact) ~feeds:(Contact.feeds contact)
    ~vcard:(Contact.vcard contact) ()

let set_account t handle account =
  match Contact.Account.check account with
  | Error _ as e -> e
  | Ok () ->
      let platform = Contact.Account.platform account in
      update_contact t handle (fun contact ->
          let others =
            List.filter
              (fun a -> Contact.Account.platform a <> platform)
              (Contact.accounts contact)
          in
          with_accounts contact (others @ [ account ]))

let unset_account t handle platform =
  update_contact t handle (fun contact ->
      let accounts =
        List.filter
          (fun a -> Contact.Account.platform a <> platform)
          (Contact.accounts contact)
      in
      with_accounts contact accounts)

let with_feeds contact feeds =
  Contact.make ~handle:(Contact.handle contact) ~names:(Contact.names contact)
    ~kind:(Contact.kind contact) ~emails:(Contact.emails contact)
    ~accounts:(Contact.accounts contact) ~links:(Contact.links contact)
    ~affiliations:(Contact.affiliations contact)
    ?photo:(Contact.photo contact) ~feeds ~vcard:(Contact.vcard contact) ()

let set_feed_paused t handle url paused =
  match lookup t handle with
  | None -> Error (Printf.sprintf "Contact not found: %s" handle)
  | Some contact ->
      let feeds = Contact.feeds contact in
      if not (List.exists (fun f -> Contact.Feed.url f = url) feeds) then
        Error (Printf.sprintf "No feed with URL %s for @%s" url handle)
      else
        let feeds =
          List.map
            (fun f ->
              if Contact.Feed.url f = url then Contact.Feed.set_paused f paused
              else f)
            feeds
        in
        save t
          (Contact.with_source (with_feeds contact feeds)
             (Contact.source contact));
        Ok ()

let thumbnail_path t contact =
  Contact.photo contact
  |> Option.map (fun relative_path -> Eio.Path.(t.data_dir / relative_path))

let png_thumbnail_path t contact =
  match Contact.photo contact with
  | None -> None
  | Some relative_path -> (
      let base = Filename.remove_extension relative_path in
      let png_path = base ^ ".png" in
      let full_path = Eio.Path.(t.data_dir / png_path) in
      try
        ignore (Eio.Path.load full_path);
        Some full_path
      with _ -> None)

let handle_of_name name =
  let name = String.lowercase_ascii name in
  let words = String.split_on_char ' ' name in
  let initials =
    String.concat "" (List.map (fun w -> String.sub w 0 1) words)
  in
  initials ^ List.hd (List.rev words)

let find_by_name t name =
  let name_lower = String.lowercase_ascii name in
  let all_contacts = list t in
  let matches =
    List.filter
      (fun c ->
        List.exists
          (fun n -> String.lowercase_ascii n = name_lower)
          (Contact.names c))
      all_contacts
  in
  match matches with
  | [ contact ] -> contact
  | [] -> raise Not_found
  | _ -> raise (Invalid_argument ("Multiple contacts match: " ^ name))

let find_by_name_opt t name =
  try Some (find_by_name t name) with Not_found | Invalid_argument _ -> None

let contains_substring ~needle haystack =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  if needle_len = 0 then true
  else if needle_len > haystack_len then false
  else
    let rec check i =
      if i > haystack_len - needle_len then false
      else if String.sub haystack i needle_len = needle then true
      else check (i + 1)
    in
    check 0

let search_all t query =
  let query_lower = String.lowercase_ascii query in
  let all = list t in
  let matches =
    List.filter
      (fun c ->
        List.exists
          (fun name ->
            let name_lower = String.lowercase_ascii name in
            String.equal name_lower query_lower
            || String.starts_with ~prefix:query_lower name_lower
            || contains_substring ~needle:query_lower name_lower
            || String.contains name_lower ' '
               && String.split_on_char ' ' name_lower
                  |> List.exists (fun word ->
                      String.starts_with ~prefix:query_lower word))
          (Contact.names c))
      all
  in
  List.sort Contact.compare matches

let find_by_handle t handle = lookup t handle

let lookup_by_name t name =
  let name_lower = String.lowercase_ascii name in
  let all_contacts = list t in
  let matches =
    List.filter
      (fun c ->
        List.exists
          (fun n -> String.lowercase_ascii n = name_lower)
          (Contact.names c))
      all_contacts
  in
  match matches with
  | [ contact ] -> contact
  | [] -> failwith ("Contact not found: " ^ name)
  | _ -> failwith ("Ambiguous contact: " ^ name)

let find_by_org t ~org =
  let org_lower = String.lowercase_ascii org in
  let all = list t in
  let matches =
    List.filter
      (fun c ->
        List.exists
          (fun (a : Contact.affiliation) ->
            contains_substring ~needle:org_lower (String.lowercase_ascii a.org))
          (Contact.affiliations c))
      all
  in
  List.sort Contact.compare matches

let pp ppf t =
  let all = list t in
  Fmt.pf ppf "@[<v>%a: %d contacts stored in XDG data directory@]"
    (Fmt.styled `Bold Fmt.string)
    "Sortal Store" (List.length all)
