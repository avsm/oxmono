module Contact = Sortal_schema.Contact
module Account = Sortal_schema.Contact.Account
module Platform = Sortal_schema.Contact.Platform
module Date = Sortal_schema.Contact.Date

let add = Buffer.add_string
let esc = Html.add_escaped

let kind_str = function
  | Contact.Person -> "Person"
  | Contact.Organization -> "Organization"

let contact_path h = "/contact/" ^ Html.pct_segment h

(* Small building blocks. Each appends to [b] and escapes what it is given. *)

let add_meta b s =
  if String.length s > 0 then (
    add b "<span class=\"range\">";
    esc b s;
    add b "</span>")

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

let add_platform_select b =
  add b "<select name=\"platform\">";
  List.iter
    (fun p ->
      add b "<option value=\"";
      add b (Platform.key p);
      add b "\">";
      esc b (Platform.key p);
      add b "</option>")
    Platform.all;
  add b "</select>"

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

(* Every account except ORCID and AT Protocol, which the caller shows
   separately. *)
let other_accounts c =
  List.filter
    (fun a ->
      match Account.platform a with
      | Platform.Simple Platform.Orcid | Platform.Atproto -> false
      | _ -> true)
    (Contact.accounts c)

let affiliation_range (a : Contact.affiliation) =
  match (a.from, a.until) with
  | None, None -> ""
  | Some f, None -> Date.to_string f ^ " to present"
  | None, Some u -> "until " ^ Date.to_string u
  | Some f, Some u -> Date.to_string f ^ " to " ^ Date.to_string u

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
        let h = Contact.handle c in
        add b "<tr><td class=\"thumb\">";
        if has_thumb h then (
          add b "<img class=\"avatar\" src=\"/thumbnail/";
          esc b (Html.pct_segment h);
          add b "\" alt=\"\">");
        add b "</td><td>";
        add_link b ~href:(contact_path h) ~text:h;
        add b "</td><td>";
        esc b (Contact.name c);
        add b "</td><td>";
        (match List.nth_opt (Contact.emails c) 0 with
        | None -> ()
        | Some e -> esc b e);
        add b "</td><td>";
        (match Contact.current_affiliation c with
        | None -> ()
        | Some a -> esc b a.org);
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
  let h = Contact.handle c in
  add b "<h1>";
  esc b (Contact.name c);
  add b "</h1>\n<p class=\"count\">";
  esc b h;
  add b " &middot; ";
  esc b (kind_str (Contact.kind c));
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
  (match Contact.names c with
  | [] -> add b "<span class=\"empty\">none</span>"
  | names ->
      List.iteri
        (fun i n ->
          if i > 0 then add b ", ";
          esc b n)
        names);
  add b "</dd>\n";
  add b "<dt>Kind</dt><dd>";
  esc b (kind_str (Contact.kind c));
  add b "</dd>\n";
  (match Contact.handle_on c (Simple Orcid) with
  | None -> ()
  | Some o ->
      add b "<dt>ORCID</dt><dd>";
      esc b o;
      add b "</dd>\n");
  (match Contact.photo c with
  | None -> ()
  | Some p ->
      add b "<dt>Photo</dt><dd>";
      esc b p;
      add b "</dd>\n");
  if has_thumb then (
    add b "<dt>Thumbnail</dt><dd><img class=\"avatar-lg\" src=\"/thumbnail/";
    esc b (Html.pct_segment h);
    add b "\" alt=\"\"></dd>\n");
  add b "</dl>\n</section>\n";
  add_section b ~title:"Emails" (fun () ->
      add_rows b ~empty:"No emails." (Contact.emails c) (fun e ->
          add b "<span>";
          esc b e;
          add b "</span>"));
  add_section b ~title:"Organizations" (fun () ->
      add_rows b ~empty:"No organizations." (Contact.affiliations c)
        (fun (a : Contact.affiliation) ->
          add b "<span>";
          esc b a.org;
          add b "</span>";
          (match a.title with None -> () | Some t -> add_meta b t);
          (match a.department with None -> () | Some d -> add_meta b d);
          add_meta b (affiliation_range a)));
  add_section b ~title:"URLs" (fun () ->
      add_rows b ~empty:"No URLs." (Contact.links c) (fun (u : Contact.link) ->
          add_link b ~href:u.url ~text:u.url;
          match u.label with None -> () | Some l -> add_meta b l));
  add_section b ~title:"Services" (fun () ->
      add_rows b ~empty:"No services." (other_accounts c) (fun a ->
          add_link b ~href:(Account.url a) ~text:(Account.url a);
          add_meta b (Platform.key (Account.platform a));
          add_meta b (Account.handle a)));
  (match Contact.atproto c with
  | None -> ()
  | Some a ->
      add_section b ~title:"AT Protocol" (fun () ->
          add b "<dl class=\"facts\">\n<dt>Handle</dt><dd>";
          esc b a.handle;
          add b "</dd>\n";
          (match a.did with
          | None -> ()
          | Some d ->
              add b "<dt>DID</dt><dd>";
              esc b d;
              add b "</dd>\n");
          add b "</dl>\n";
          add_rows b ~empty:"No apps." a.apps (fun app ->
              let url = Account.app_url a app in
              add_link b ~href:url ~text:url;
              add_meta b (Account.app_to_string app))));
  Html.page ~title:(Contact.name c) ~query:"" (Buffer.contents b)

(* Edit *)

let edit (c : Contact.t) =
  let b = Buffer.create 8192 in
  let h = Contact.handle c in
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
    (Contact.names c);
  add b "</textarea></label>\n";
  add_kind_select b
    ~selected:(match Contact.kind c with Person -> "person" | Organization -> "org");
  add_labelled b ~label:"ORCID" ~name:"orcid"
    ~value:(Option.value ~default:"" (Contact.handle_on c (Simple Orcid)));
  add_labelled b ~label:"Photo" ~name:"photo"
    ~value:(Option.value ~default:"" (Contact.photo c));
  add b "<div class=\"actions\"><button type=\"submit\">Save</button>";
  add_link ~cls:"btn plain" b ~href:(contact_path h) ~text:"Cancel";
  add b "</div>\n</form>\n";
  add_section b ~title:"Emails" (fun () ->
      add_rows b ~empty:"No emails." (Contact.emails c) (fun e ->
          add b "<span>";
          esc b e;
          add b "</span>";
          add_remove b ~handle:h ~collection:"email" ~key_name:"address" ~key:e);
      add b "<form class=\"inline\" method=\"post\" action=\"";
      esc b (action "/email/add");
      add b "\">";
      add_text_input b ~name:"address" ~value:"" ~placeholder:"name@example.com";
      add b "<button type=\"submit\">Add email</button></form>\n");
  add_section b ~title:"Organizations" (fun () ->
      add_rows b ~empty:"No organizations." (Contact.affiliations c)
        (fun (a : Contact.affiliation) ->
          add b "<span>";
          esc b a.org;
          add b "</span>";
          (match a.title with None -> () | Some t -> add_meta b t);
          add_meta b (affiliation_range a);
          add_remove b ~handle:h ~collection:"org" ~key_name:"name" ~key:a.org);
      add b "<form class=\"inline\" method=\"post\" action=\"";
      esc b (action "/org/add");
      add b "\">";
      add_text_input b ~name:"name" ~value:"" ~placeholder:"Organization";
      add_text_input b ~name:"title" ~value:"" ~placeholder:"Title";
      add b "<button type=\"submit\">Add organization</button></form>\n");
  add_section b ~title:"URLs" (fun () ->
      add_rows b ~empty:"No URLs." (Contact.links c) (fun (u : Contact.link) ->
          add_link b ~href:u.url ~text:u.url;
          (match u.label with None -> () | Some l -> add_meta b l);
          add_remove b ~handle:h ~collection:"url" ~key_name:"url" ~key:u.url);
      add b "<form class=\"inline\" method=\"post\" action=\"";
      esc b (action "/url/add");
      add b "\">";
      add_text_input b ~name:"url" ~value:"" ~placeholder:"https://example.com";
      add_text_input b ~name:"label" ~value:"" ~placeholder:"Label";
      add b "<button type=\"submit\">Add URL</button></form>\n");
  add_section b ~title:"Services" (fun () ->
      add_rows b ~empty:"No services." (other_accounts c) (fun a ->
          add_link b ~href:(Account.url a) ~text:(Account.url a);
          add_meta b (Platform.key (Account.platform a));
          add_meta b (Account.handle a);
          add_remove b ~handle:h ~collection:"service" ~key_name:"platform"
            ~key:(Platform.key (Account.platform a)));
      add b "<form class=\"inline\" method=\"post\" action=\"";
      esc b (action "/service/add");
      add b "\">";
      add_platform_select b;
      add_text_input b ~name:"handle" ~value:""
        ~placeholder:"handle, or user@host";
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
