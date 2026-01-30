(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Contact = Sortal_schema.Contact
module Temporal = Sortal_schema.Temporal

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

let add_email t handle (email : Contact.email) =
  match lookup t handle with
  | None -> Error (Printf.sprintf "Contact not found: %s" handle)
  | Some contact ->
      let emails = Contact.emails contact in
      (* Check for duplicate email address *)
      if List.exists (fun (e : Contact.email) -> e.address = email.address) emails then
        Error (Printf.sprintf "Email %s already exists for contact @%s" email.address handle)
      else
        update_contact t handle (fun contact ->
          let emails = Contact.emails contact in
          Contact.make
            ~handle:(Contact.handle contact)
            ~names:(Contact.names contact)
            ~kind:(Contact.kind contact)
            ~emails:(emails @ [email])
            ~organizations:(Contact.organizations contact)
            ~urls:(Contact.urls contact)
            ~services:(Contact.services contact)
            ?icon:(Contact.icon contact)
            ?thumbnail:(Contact.thumbnail contact)
            ?orcid:(Contact.orcid contact)
            ?feeds:(Contact.feeds contact)
            ()
        )

let remove_email t handle address =
  update_contact t handle (fun contact ->
    let emails = Contact.emails contact
                 |> List.filter (fun (e : Contact.email) -> e.address <> address) in
    Contact.make
      ~handle:(Contact.handle contact)
      ~names:(Contact.names contact)
      ~kind:(Contact.kind contact)
      ~emails
      ~organizations:(Contact.organizations contact)
      ~urls:(Contact.urls contact)
      ~services:(Contact.services contact)
      ?icon:(Contact.icon contact)
      ?thumbnail:(Contact.thumbnail contact)
      ?orcid:(Contact.orcid contact)
      ?feeds:(Contact.feeds contact)
      ()
  )

let add_service t handle (service : Contact.service) =
  match lookup t handle with
  | None -> Error (Printf.sprintf "Contact not found: %s" handle)
  | Some contact ->
      let services = Contact.services contact in
      (* Check for duplicate service URL *)
      if List.exists (fun (s : Contact.service) -> s.url = service.url) services then
        Error (Printf.sprintf "Service URL %s already exists for contact @%s" service.url handle)
      else
        update_contact t handle (fun contact ->
          let services = Contact.services contact in
          Contact.make
            ~handle:(Contact.handle contact)
            ~names:(Contact.names contact)
            ~kind:(Contact.kind contact)
            ~emails:(Contact.emails contact)
            ~organizations:(Contact.organizations contact)
            ~urls:(Contact.urls contact)
            ~services:(services @ [service])
            ?icon:(Contact.icon contact)
            ?thumbnail:(Contact.thumbnail contact)
            ?orcid:(Contact.orcid contact)
            ?feeds:(Contact.feeds contact)
            ()
        )

let remove_service t handle url =
  update_contact t handle (fun contact ->
    let services = Contact.services contact
                   |> List.filter (fun (s : Contact.service) -> s.url <> url) in
    Contact.make
      ~handle:(Contact.handle contact)
      ~names:(Contact.names contact)
      ~kind:(Contact.kind contact)
      ~emails:(Contact.emails contact)
      ~organizations:(Contact.organizations contact)
      ~urls:(Contact.urls contact)
      ~services
      ?icon:(Contact.icon contact)
      ?thumbnail:(Contact.thumbnail contact)
      ?orcid:(Contact.orcid contact)
      ?feeds:(Contact.feeds contact)
      ()
  )

let add_organization t handle (org : Contact.organization) =
  match lookup t handle with
  | None -> Error (Printf.sprintf "Contact not found: %s" handle)
  | Some contact ->
      let orgs = Contact.organizations contact in
      (* Check for exact duplicate organization (same name, title, and department) *)
      let is_duplicate = List.exists (fun (o : Contact.organization) ->
        o.name = org.name &&
        o.title = org.title &&
        o.department = org.department
      ) orgs in
      if is_duplicate then
        Error (Printf.sprintf "Organization %s with the same title/department already exists for contact @%s" org.name handle)
      else
        update_contact t handle (fun contact ->
          let orgs = Contact.organizations contact in
          Contact.make
            ~handle:(Contact.handle contact)
            ~names:(Contact.names contact)
            ~kind:(Contact.kind contact)
            ~emails:(Contact.emails contact)
            ~organizations:(orgs @ [org])
            ~urls:(Contact.urls contact)
            ~services:(Contact.services contact)
            ?icon:(Contact.icon contact)
            ?thumbnail:(Contact.thumbnail contact)
            ?orcid:(Contact.orcid contact)
            ?feeds:(Contact.feeds contact)
            ()
        )

let remove_organization t handle name =
  update_contact t handle (fun contact ->
    let orgs = Contact.organizations contact
               |> List.filter (fun (o : Contact.organization) -> o.name <> name) in
    Contact.make
      ~handle:(Contact.handle contact)
      ~names:(Contact.names contact)
      ~kind:(Contact.kind contact)
      ~emails:(Contact.emails contact)
      ~organizations:orgs
      ~urls:(Contact.urls contact)
      ~services:(Contact.services contact)
      ?icon:(Contact.icon contact)
      ?thumbnail:(Contact.thumbnail contact)
      ?orcid:(Contact.orcid contact)
      ?feeds:(Contact.feeds contact)
      ()
  )

let add_url t handle (url_entry : Contact.url_entry) =
  match lookup t handle with
  | None -> Error (Printf.sprintf "Contact not found: %s" handle)
  | Some contact ->
      let urls = Contact.urls contact in
      (* Check for duplicate URL *)
      if List.exists (fun (u : Contact.url_entry) -> u.url = url_entry.url) urls then
        Error (Printf.sprintf "URL %s already exists for contact @%s" url_entry.url handle)
      else
        update_contact t handle (fun contact ->
          let urls = Contact.urls contact in
          Contact.make
            ~handle:(Contact.handle contact)
            ~names:(Contact.names contact)
            ~kind:(Contact.kind contact)
            ~emails:(Contact.emails contact)
            ~organizations:(Contact.organizations contact)
            ~urls:(urls @ [url_entry])
            ~services:(Contact.services contact)
            ?icon:(Contact.icon contact)
            ?thumbnail:(Contact.thumbnail contact)
            ?orcid:(Contact.orcid contact)
            ?feeds:(Contact.feeds contact)
            ()
        )

let remove_url t handle url =
  update_contact t handle (fun contact ->
    let urls = Contact.urls contact
               |> List.filter (fun (u : Contact.url_entry) -> u.url <> url) in
    Contact.make
      ~handle:(Contact.handle contact)
      ~names:(Contact.names contact)
      ~kind:(Contact.kind contact)
      ~emails:(Contact.emails contact)
      ~organizations:(Contact.organizations contact)
      ~urls
      ~services:(Contact.services contact)
      ?icon:(Contact.icon contact)
      ?thumbnail:(Contact.thumbnail contact)
      ?orcid:(Contact.orcid contact)
      ?feeds:(Contact.feeds contact)
      ()
  )

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
  Contact.thumbnail contact
  |> Option.map (fun relative_path -> Eio.Path.(t.data_dir / relative_path))

let png_thumbnail_path t contact =
  match Contact.thumbnail contact with
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

let find_by_email_at t ~email ~date =
  let all = list t in
  List.find_opt (fun c ->
    let emails_at_date = Contact.emails_at c ~date in
    List.exists (fun e -> e.Contact.address = email) emails_at_date
  ) all

let find_by_org t ~org ?from ?until () =
  let org_lower = String.lowercase_ascii org in
  let all = list t in
  let matches = List.filter (fun c ->
    let orgs : Contact.organization list = Contact.organizations c in
    let filtered_orgs = match from, until with
      | None, None -> orgs
      | _, _ -> Temporal.filter ~get:(fun (o : Contact.organization) -> o.range)
                  ~from ~until orgs
    in
    List.exists (fun (o : Contact.organization) ->
      contains_substring ~needle:org_lower
        (String.lowercase_ascii o.name)
    ) filtered_orgs
  ) all in
  List.sort Contact.compare matches

let list_at t ~date =
  let all = list t in
  List.filter (fun c ->
    (* Contact is active if it has any email, org, or URL valid at date *)
    let has_email = Contact.emails_at c ~date <> [] in
    let has_org = Contact.organization_at c ~date <> None in
    let has_url = Contact.url_at c ~date <> None in
    has_email || has_org || has_url
  ) all

let pp ppf t =
  let all = list t in
  Fmt.pf ppf "@[<v>%a: %d contacts stored in XDG data directory@]"
    (Fmt.styled `Bold Fmt.string) "Sortal Store"
    (List.length all)
