module Contact = Sortal_schema.Contact
module Temporal = Sortal_schema.Temporal

let add = Buffer.add_string
let esc = Html.add_escaped

(* The schema's own printers and accessors live in an unannotated library, so
   a portable page cannot call them. Everything below re-derives what it needs
   from the record fields directly. *)

let kind_str (k : Contact.contact_kind) =
  match k with Contact.Person -> "Person" | Contact.Organization -> "Organization"

let email_type_str (t : Contact.email_type) =
  match t with
  | Contact.Work -> "work"
  | Contact.Personal -> "personal"
  | Contact.Other -> "other"

let service_kind_str (k : Contact.service_kind) =
  match k with
  | Contact.ActivityPub v -> (
      match v with
      | Contact.Mastodon -> "Mastodon"
      | Contact.Pixelfed -> "Pixelfed"
      | Contact.PeerTube -> "PeerTube"
      | Contact.Other_activitypub s -> s)
  | Contact.Github -> "GitHub"
  | Contact.Git -> "Git"
  | Contact.Twitter -> "Twitter"
  | Contact.LinkedIn -> "LinkedIn"
  | Contact.Photo -> "Photo"
  | Contact.Custom s -> s

let atproto_type_str (t : Contact.atproto_service_type) =
  match t with
  | Contact.ATBluesky -> "Bluesky"
  | Contact.ATTangled -> "Tangled"
  | Contact.ATCustom s -> s

let date_str (y, m, d) = Printf.sprintf "%04d-%02d-%02d" y m d

let range_str (r : Temporal.range option) =
  match r with
  | None -> ""
  | Some r -> (
      match (r.Temporal.from, r.Temporal.until) with
      | None, None -> ""
      | Some f, None -> date_str f ^ " to present"
      | None, Some u -> "until " ^ date_str u
      | Some f, Some u -> date_str f ^ " to " ^ date_str u)

(* The first entry with no end date, falling back to the first entry at all.
   It is the same intent as the schema's temporal queries, without the clock. *)
let current get l =
  let rec go = function
    | [] -> ( match l with [] -> None | x :: _ -> Some x)
    | x :: tl -> (
        match (get x : Temporal.range option) with
        | None -> Some x
        | Some r -> (
            match r.Temporal.until with None -> Some x | Some _ -> go tl))
  in
  go l

let primary_name (c : Contact.t) =
  match c.Contact.names with [] -> c.Contact.handle | n :: _ -> n

let contact_path h = "/contact/" ^ Html.pct_segment h

(* Small building blocks. Each appends to [b] and escapes what it is given. *)

let add_meta b s =
  if String.length s > 0 then (
    add b "<span class=\"range\">";
    esc b s;
    add b "</span>")

let add_note b n =
  match n with
  | None -> ()
  | Some n ->
      add b "<span class=\"note\">";
      esc b n;
      add b "</span>"

let add_link ?cls b ~href ~text =
  add b "<a";
  (match cls with
  | None -> ()
  | Some c ->
      add b " class=\"";
      add b c;
      add b "\"");
  add b " href=\"";
  esc b href;
  add b "\">";
  esc b text;
  add b "</a>"

let add_field b ~kind ~name ~value =
  add b "<input type=\"";
  add b kind;
  add b "\" name=\"";
  add b name;
  add b "\" value=\"";
  esc b value;
  add b "\">"

let add_hidden b ~name ~value = add_field b ~kind:"hidden" ~name ~value

let add_text_input b ~name ~value ~placeholder =
  add b "<input type=\"text\" name=\"";
  add b name;
  add b "\" placeholder=\"";
  esc b placeholder;
  add b "\" value=\"";
  esc b value;
  add b "\">"

let add_labelled b ~label ~name ~value =
  add b "<label><span class=\"name\">";
  esc b label;
  add b "</span>";
  add_field b ~kind:"text" ~name ~value;
  add b "</label>\n"

let add_kind_select b ~selected =
  add b "<label><span class=\"name\">Kind</span><select name=\"kind\">";
  add b "<option value=\"person\"";
  if String.equal selected "person" then add b " selected";
  add b ">Person</option>";
  add b "<option value=\"org\"";
  if String.equal selected "org" then add b " selected";
  add b ">Organization</option>";
  add b "</select></label>\n"

(* A remove button is a form of its own, so the row it belongs to carries the
   key the handler matches on. *)
let add_remove b ~handle ~collection ~key_name ~key =
  add b "<form method=\"post\" action=\"";
  esc b (contact_path handle);
  add b "/";
  add b collection;
  add b "/remove\">";
  add_hidden b ~name:key_name ~value:key;
  add b "<button class=\"danger\" type=\"submit\">Remove</button></form>"

let add_section b ~title contents =
  add b "<section class=\"card\">\n<h2>";
  esc b title;
  add b "</h2>\n";
  contents ();
  add b "</section>\n"

let add_rows b ~empty items render =
  match items with
  | [] ->
      add b "<p class=\"empty\">";
      esc b empty;
      add b "</p>\n"
  | items ->
      add b "<ul class=\"rows\">\n";
      List.iter
        (fun item ->
          add b "<li>";
          render item;
          add b "</li>\n")
        items;
      add b "</ul>\n"

(* Index *)

let index ~query ~has_thumb contacts =
  let b = Buffer.create 4096 in
  add b "<h1>Contacts</h1>\n<p class=\"count\">";
  let n = List.length contacts in
  add b (string_of_int n);
  add b (if n = 1 then " contact" else " contacts");
  if String.length query > 0 then (
    add b " matching ";
    esc b query);
  add b "</p>\n";
  if contacts = [] then
    add b "<p class=\"empty\">No contacts to show.</p>\n"
  else (
    add b
      "<table>\n\
       <tr><th></th><th>Handle</th><th>Name</th><th>Email</th>\
       <th>Organization</th></tr>\n";
    List.iter
      (fun (c : Contact.t) ->
        let h = c.Contact.handle in
        add b "<tr><td class=\"thumb\">";
        if has_thumb h then (
          add b "<img class=\"avatar\" src=\"/thumbnail/";
          esc b (Html.pct_segment h);
          add b "\" alt=\"\">");
        add b "</td><td>";
        add_link b ~href:(contact_path h) ~text:h;
        add b "</td><td>";
        esc b (primary_name c);
        add b "</td><td>";
        (match
           current (fun (e : Contact.email) -> e.Contact.range) c.Contact.emails
         with
        | None -> ()
        | Some e -> esc b e.Contact.address);
        add b "</td><td>";
        (match
           current
             (fun (o : Contact.organization) -> o.Contact.range)
             c.Contact.organizations
         with
        | None -> ()
        | Some o -> esc b o.Contact.name);
        add b "</td></tr>\n")
      contacts;
    add b "</table>\n");
  Html.page ~title:"Contacts" ~query (Buffer.contents b)

(* Create *)

let new_form ?error ~handle ~name ~kind ~email () =
  let b = Buffer.create 2048 in
  add b "<h1>New contact</h1>\n";
  (match error with
  | None -> ()
  | Some msg ->
      add b "<p class=\"error\">";
      esc b msg;
      add b "</p>\n");
  add b "<form class=\"card\" method=\"post\" action=\"/new\">\n";
  add_labelled b ~label:"Handle" ~name:"handle" ~value:handle;
  add_labelled b ~label:"Name" ~name:"name" ~value:name;
  add_kind_select b ~selected:(if String.equal kind "org" then "org" else "person");
  add_labelled b ~label:"Email" ~name:"email" ~value:email;
  add b
    "<div class=\"actions\"><button type=\"submit\">Create</button>\
     <a class=\"btn plain\" href=\"/\">Cancel</a></div>\n</form>\n";
  Html.page ~title:"New contact" ~query:"" (Buffer.contents b)

(* Detail *)

let detail ~has_thumb (c : Contact.t) =
  let b = Buffer.create 8192 in
  let h = c.Contact.handle in
  add b "<h1>";
  esc b (primary_name c);
  add b "</h1>\n<p class=\"count\">";
  esc b h;
  add b " &middot; ";
  esc b (kind_str c.Contact.kind);
  add b "</p>\n";
  add b "<div class=\"actions\">";
  add_link ~cls:"btn plain" b ~href:(contact_path h ^ "/edit") ~text:"Edit";
  add b "<form method=\"post\" action=\"";
  esc b (contact_path h);
  add b "/delete\">";
  add b
    "<button class=\"danger\" type=\"submit\">Delete this contact \
     permanently</button></form></div>\n";
  add b "<section class=\"card\">\n<h2>Details</h2>\n<dl class=\"facts\">\n";
  add b "<dt>Names</dt><dd>";
  (match c.Contact.names with
  | [] -> add b "<span class=\"empty\">none</span>"
  | names ->
      List.iteri
        (fun i n ->
          if i > 0 then add b ", ";
          esc b n)
        names);
  add b "</dd>\n";
  add b "<dt>Kind</dt><dd>";
  esc b (kind_str c.Contact.kind);
  add b "</dd>\n";
  (match c.Contact.orcid with
  | None -> ()
  | Some o ->
      add b "<dt>ORCID</dt><dd>";
      esc b o;
      add b "</dd>\n");
  (match c.Contact.icon with
  | None -> ()
  | Some i ->
      add b "<dt>Icon</dt><dd>";
      add_link b ~href:i ~text:i;
      add b "</dd>\n");
  if has_thumb then (
    add b "<dt>Thumbnail</dt><dd><img class=\"avatar-lg\" src=\"/thumbnail/";
    esc b (Html.pct_segment h);
    add b "\" alt=\"\"></dd>\n");
  add b "</dl>\n</section>\n";
  add_section b ~title:"Emails" (fun () ->
      add_rows b ~empty:"No emails." c.Contact.emails
        (fun (e : Contact.email) ->
          add b "<span>";
          esc b e.Contact.address;
          add b "</span>";
          (match e.Contact.type_ with
          | None -> ()
          | Some t -> add_meta b (email_type_str t));
          add_meta b (range_str e.Contact.range);
          add_note b e.Contact.note));
  add_section b ~title:"Organizations" (fun () ->
      add_rows b ~empty:"No organizations." c.Contact.organizations
        (fun (o : Contact.organization) ->
          add b "<span>";
          esc b o.Contact.name;
          add b "</span>";
          (match o.Contact.title with None -> () | Some t -> add_meta b t);
          (match o.Contact.department with
          | None -> ()
          | Some d -> add_meta b d);
          add_meta b (range_str o.Contact.range)));
  add_section b ~title:"URLs" (fun () ->
      add_rows b ~empty:"No URLs." c.Contact.urls (fun (u : Contact.url_entry) ->
          add_link b ~href:u.Contact.url ~text:u.Contact.url;
          (match u.Contact.label with None -> () | Some l -> add_meta b l);
          add_meta b (range_str u.Contact.range)));
  add_section b ~title:"Services" (fun () ->
      add_rows b ~empty:"No services." c.Contact.services
        (fun (s : Contact.service) ->
          add_link b ~href:s.Contact.url ~text:s.Contact.url;
          (match s.Contact.kind with
          | None -> ()
          | Some k -> add_meta b (service_kind_str k));
          (match s.Contact.handle with None -> () | Some h -> add_meta b h);
          (match s.Contact.label with None -> () | Some l -> add_meta b l);
          add_meta b (range_str s.Contact.range)));
  (match c.Contact.atproto with
  | None -> ()
  | Some a ->
      add_section b ~title:"AT Protocol" (fun () ->
          add b "<dl class=\"facts\">\n<dt>Handle</dt><dd>";
          esc b a.Contact.atp_handle;
          add b "</dd>\n";
          (match a.Contact.atp_did with
          | None -> ()
          | Some d ->
              add b "<dt>DID</dt><dd>";
              esc b d;
              add b "</dd>\n");
          add b "</dl>\n";
          add_rows b ~empty:"No services." a.Contact.atp_services
            (fun (s : Contact.atproto_service) ->
              add_link b ~href:s.Contact.atp_url ~text:s.Contact.atp_url;
              add_meta b (atproto_type_str s.Contact.atp_type))));
  Html.page ~title:(primary_name c) ~query:"" (Buffer.contents b)

(* Edit *)

let edit (c : Contact.t) =
  let b = Buffer.create 8192 in
  let h = c.Contact.handle in
  let action suffix = contact_path h ^ suffix in
  add b "<h1>Edit ";
  esc b h;
  add b "</h1>\n";
  add b "<form class=\"card\" method=\"post\" action=\"";
  esc b (action "/edit");
  add b "\">\n";
  add b
    "<label><span class=\"name\">Names, one per line, first is \
     primary</span><textarea name=\"names\">";
  List.iteri
    (fun i n ->
      if i > 0 then add b "\n";
      esc b n)
    c.Contact.names;
  add b "</textarea></label>\n";
  add_kind_select b
    ~selected:
      (match c.Contact.kind with
      | Contact.Person -> "person"
      | Contact.Organization -> "org");
  add_labelled b ~label:"ORCID" ~name:"orcid"
    ~value:(match c.Contact.orcid with None -> "" | Some o -> o);
  add_labelled b ~label:"Icon URL" ~name:"icon"
    ~value:(match c.Contact.icon with None -> "" | Some i -> i);
  add b "<div class=\"actions\"><button type=\"submit\">Save</button>";
  add_link ~cls:"btn plain" b ~href:(contact_path h) ~text:"Cancel";
  add b "</div>\n</form>\n";
  add_section b ~title:"Emails" (fun () ->
      add_rows b ~empty:"No emails." c.Contact.emails
        (fun (e : Contact.email) ->
          add b "<span>";
          esc b e.Contact.address;
          add b "</span>";
          (match e.Contact.type_ with
          | None -> ()
          | Some t -> add_meta b (email_type_str t));
          add_meta b (range_str e.Contact.range);
          add_remove b ~handle:h ~collection:"email" ~key_name:"address"
            ~key:e.Contact.address);
      add b "<form class=\"inline\" method=\"post\" action=\"";
      esc b (action "/email/add");
      add b "\">";
      add_text_input b ~name:"address" ~value:"" ~placeholder:"name@example.com";
      add b
        "<select name=\"type\"><option value=\"\">No \
         type</option><option value=\"work\">Work</option><option \
         value=\"personal\">Personal</option><option \
         value=\"other\">Other</option></select>";
      add b "<button type=\"submit\">Add email</button></form>\n");
  add_section b ~title:"Organizations" (fun () ->
      add_rows b ~empty:"No organizations." c.Contact.organizations
        (fun (o : Contact.organization) ->
          add b "<span>";
          esc b o.Contact.name;
          add b "</span>";
          (match o.Contact.title with None -> () | Some t -> add_meta b t);
          add_meta b (range_str o.Contact.range);
          add_remove b ~handle:h ~collection:"org" ~key_name:"name"
            ~key:o.Contact.name);
      add b "<form class=\"inline\" method=\"post\" action=\"";
      esc b (action "/org/add");
      add b "\">";
      add_text_input b ~name:"name" ~value:"" ~placeholder:"Organization";
      add_text_input b ~name:"title" ~value:"" ~placeholder:"Title";
      add b "<button type=\"submit\">Add organization</button></form>\n");
  add_section b ~title:"URLs" (fun () ->
      add_rows b ~empty:"No URLs." c.Contact.urls (fun (u : Contact.url_entry) ->
          add_link b ~href:u.Contact.url ~text:u.Contact.url;
          (match u.Contact.label with None -> () | Some l -> add_meta b l);
          add_meta b (range_str u.Contact.range);
          add_remove b ~handle:h ~collection:"url" ~key_name:"url"
            ~key:u.Contact.url);
      add b "<form class=\"inline\" method=\"post\" action=\"";
      esc b (action "/url/add");
      add b "\">";
      add_text_input b ~name:"url" ~value:"" ~placeholder:"https://example.com";
      add_text_input b ~name:"label" ~value:"" ~placeholder:"Label";
      add b "<button type=\"submit\">Add URL</button></form>\n");
  add_section b ~title:"Services" (fun () ->
      add_rows b ~empty:"No services." c.Contact.services
        (fun (s : Contact.service) ->
          add_link b ~href:s.Contact.url ~text:s.Contact.url;
          (match s.Contact.handle with None -> () | Some sh -> add_meta b sh);
          (match s.Contact.label with None -> () | Some l -> add_meta b l);
          add_meta b (range_str s.Contact.range);
          add_remove b ~handle:h ~collection:"service" ~key_name:"url"
            ~key:s.Contact.url);
      add b "<form class=\"inline\" method=\"post\" action=\"";
      esc b (action "/service/add");
      add b "\">";
      add_text_input b ~name:"url" ~value:""
        ~placeholder:"https://example.com/user";
      add_text_input b ~name:"handle" ~value:"" ~placeholder:"Handle";
      add_text_input b ~name:"label" ~value:"" ~placeholder:"Label";
      add b "<button type=\"submit\">Add service</button></form>\n");
  Html.page ~title:("Edit " ^ h) ~query:"" (Buffer.contents b)

(* Failures *)

let not_found () =
  Html.page ~title:"Not found" ~query:""
    "<h1>Not found</h1>\n\
     <p class=\"empty\">There is nothing at this address.</p>\n\
     <p><a href=\"/\">Back to contacts</a></p>\n"

let error msg =
  let b = Buffer.create 512 in
  add b "<h1>Something went wrong</h1>\n<p class=\"error\">";
  esc b msg;
  add b "</p>\n<p><a href=\"/\">Back to contacts</a></p>\n";
  Html.page ~title:"Error" ~query:"" (Buffer.contents b)
