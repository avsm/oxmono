(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Contact = Sortal_schema.Contact

type t = {
  xdg : Xdge.t; [@warning "-69"]
  data_dir : Eio.Fs.dir_ty Eio.Path.t;
}

let create fs app_name =
  let xdg = Xdge.create fs app_name in
  let data_dir = Xdge.data_dir xdg in
  { xdg; data_dir }

let create_from_xdg xdg =
  let data_dir = Xdge.data_dir xdg in
  { xdg; data_dir }

let data_dir t = t.data_dir

let contact_file t handle =
  Eio.Path.(t.data_dir / (handle ^ ".yaml"))

let save t contact =
  let path = contact_file t (Contact.handle contact) in
  let buf = Buffer.create 4096 in
  let writer = Bytesrw.Bytes.Writer.of_buffer buf in
  match Yamlt.encode Contact.json_t contact ~eod:true writer with
  | Ok () -> Eio.Path.save ~create:(`Or_truncate 0o644) path (Buffer.contents buf)
  | Error err -> failwith ("Failed to encode contact: " ^ err)

let lookup t handle =
  let path = contact_file t handle in
  try
    let yaml_str = Eio.Path.load path in
    let reader = Bytesrw.Bytes.Reader.of_string yaml_str in
    match Yamlt.decode Contact.json_t reader with
    | Ok contact -> Some contact
    | Error msg ->
        Logs.warn (fun m -> m "Failed to decode contact %s: %s" handle msg);
        None
  with exn ->
    Logs.warn (fun m -> m "Failed to load contact %s: %s" handle (Printexc.to_string exn));
    None

let delete t handle =
  let path = contact_file t handle in
  try
    Eio.Path.unlink path
  with
  | _ -> ()

(* Contact modification helpers *)
let update_contact t handle f =
  match lookup t handle with
  | None -> Error (Printf.sprintf "Contact not found: %s" handle)
  | Some contact ->
      let updated = f contact in
      save t updated;
      Ok ()

let with_accounts contact accounts =
  Contact.make
    ~handle:(Contact.handle contact)
    ~names:(Contact.names contact)
    ~kind:(Contact.kind contact)
    ~emails:(Contact.emails contact)
    ~accounts
    ~links:(Contact.links contact)
    ~affiliations:(Contact.affiliations contact)
    ?photo:(Contact.photo contact)
    ~feeds:(Contact.feeds contact)
    ~vcard:(Contact.vcard contact)
    ()

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

let list t =
  try
    let entries = Eio.Path.read_dir t.data_dir in
    List.filter_map (fun entry ->
      if Filename.check_suffix entry ".yaml" then
        let handle = Filename.chop_suffix entry ".yaml" in
        lookup t handle
      else
        None
    ) entries
  with
  | _ -> []

let thumbnail_path t contact =
  Contact.photo contact
  |> Option.map (fun relative_path -> Eio.Path.(t.data_dir / relative_path))

let png_thumbnail_path t contact =
  match Contact.photo contact with
  | None -> None
  | Some relative_path ->
    let base = Filename.remove_extension relative_path in
    let png_path = base ^ ".png" in
    let full_path = Eio.Path.(t.data_dir / png_path) in
    try
      ignore (Eio.Path.load full_path);
      Some full_path
    with _ -> None

let handle_of_name name =
  let name = String.lowercase_ascii name in
  let words = String.split_on_char ' ' name in
  let initials = String.concat "" (List.map (fun w -> String.sub w 0 1) words) in
  initials ^ List.hd (List.rev words)

let find_by_name t name =
  let name_lower = String.lowercase_ascii name in
  let all_contacts = list t in
  let matches = List.filter (fun c ->
    List.exists (fun n -> String.lowercase_ascii n = name_lower)
      (Contact.names c)
  ) all_contacts in
  match matches with
  | [contact] -> contact
  | [] -> raise Not_found
  | _ -> raise (Invalid_argument ("Multiple contacts match: " ^ name))

let find_by_name_opt t name =
  try
    Some (find_by_name t name)
  with
  | Not_found | Invalid_argument _ -> None

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
  let matches = List.filter (fun c ->
    List.exists (fun name ->
      let name_lower = String.lowercase_ascii name in
      String.equal name_lower query_lower ||
      String.starts_with ~prefix:query_lower name_lower ||
      contains_substring ~needle:query_lower name_lower ||
      (String.contains name_lower ' ' &&
       String.split_on_char ' ' name_lower |> List.exists (fun word ->
         String.starts_with ~prefix:query_lower word
       ))
    ) (Contact.names c)
  ) all in
  List.sort Contact.compare matches

let find_by_handle t handle =
  lookup t handle

let lookup_by_name t name =
  let name_lower = String.lowercase_ascii name in
  let all_contacts = list t in
  let matches = List.filter (fun c ->
    List.exists (fun n -> String.lowercase_ascii n = name_lower)
      (Contact.names c)
  ) all_contacts in
  match matches with
  | [contact] -> contact
  | [] -> failwith ("Contact not found: " ^ name)
  | _ -> failwith ("Ambiguous contact: " ^ name)

let migrate t ~dry_run =
  let migrated = ref 0 and skipped = ref 0 and failures = ref [] in
  let entries = Eio.Path.read_dir t.data_dir in
  List.iter
    (fun entry ->
      if Filename.check_suffix entry ".yaml" then begin
        let handle = Filename.chop_suffix entry ".yaml" in
        let path = Eio.Path.(t.data_dir / entry) in
        let yaml = Eio.Path.load path in
        let reader () = Bytesrw.Bytes.Reader.of_string yaml in
        match Yamlt.decode Sortal_schema.V2.Contact.json_t (reader ()) with
        | Ok _ -> incr skipped
        | Error _ -> (
            match Yamlt.decode Sortal_schema.V1.Contact.json_t (reader ()) with
            | Error e -> failures := (handle, "V1 decode: " ^ e) :: !failures
            | Ok v1 -> (
                match Sortal_schema.Migrate.v1_to_v2 v1 with
                | Error e -> failures := (handle, e) :: !failures
                | Ok v2 ->
                    let buf = Buffer.create 4096 in
                    let writer = Bytesrw.Bytes.Writer.of_buffer buf in
                    (match
                       Yamlt.encode Sortal_schema.V2.Contact.json_t v2
                         ~eod:true writer
                     with
                    | Error e ->
                        failures := (handle, "V2 encode: " ^ e) :: !failures
                    | Ok () ->
                        if not dry_run then
                          Eio.Path.save ~create:(`Or_truncate 0o644) path
                            (Buffer.contents buf);
                        incr migrated)))
      end)
    entries;
  (!migrated, !skipped, List.rev !failures)

let find_by_org t ~org =
  let org_lower = String.lowercase_ascii org in
  let all = list t in
  let matches =
    List.filter
      (fun c ->
        List.exists
          (fun (a : Contact.affiliation) ->
            contains_substring ~needle:org_lower
              (String.lowercase_ascii a.org))
          (Contact.affiliations c))
      all
  in
  List.sort Contact.compare matches

let pp ppf t =
  let all = list t in
  Fmt.pf ppf "@[<v>%a: %d contacts stored in XDG data directory@]"
    (Fmt.styled `Bold Fmt.string) "Sortal Store"
    (List.length all)
