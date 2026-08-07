open Proffer
open Proffer.Route
module Contact = Sortal_schema.Contact

type env = {
  list_contacts : unit -> Contact.t list;
  lookup : string -> Contact.t option;
  search : string -> Contact.t list;
  save : Contact.t -> (unit, string) result;
  delete : string -> (unit, string) result;
  thumbnail : string -> string option;
}

(* A policy is immutable data, so it may be described once here and still be
   read from a portable handler. *)
let thumb_cache = Cache_control.private' ~max_age:(`Hours 1) ()
let css_cache = Cache_control.public ~max_age:(`Days 365) ~immutable:true ()
let css_etag : Etag.t = `Strong (Digest.to_hex (Digest.string Html.css))

(* Form and query reading. A missing field and an empty one are the same
   thing to every handler here. *)

let raw req name = match Req.form_param req name with Some v -> v | None -> ""
let field req name = String.trim (raw req name)
let opt s = if String.equal s "" then None else Some s

let lines s =
  List.filter
    (fun l -> not (String.equal l ""))
    (List.map String.trim (String.split_on_char '\n' s))

let kind_of s =
  if String.equal s "org" then Contact.Organization else Contact.Person

let email_type_of s =
  match s with
  | "work" -> Some Contact.Work
  | "personal" -> Some Contact.Personal
  | "other" -> Some Contact.Other
  | _ -> None

let contact_url handle = "/contact/" ^ Html.pct_segment handle
let not_found () = Resp.html ~status:`Not_found (Pages.not_found ())
let failed msg = Resp.html ~status:`Internal_server_error (Pages.error msg)

let has_thumb env handle =
  match env.thumbnail handle with Some _ -> true | None -> false

(* Every mutation is the same shape: find the contact, rewrite the record,
   save it, then send the browser away with a 303 so a reload does not repost.
   A transform returning [None] asks for no write, which is what an empty
   required field means. *)
let update env handle ~dest f =
  match env.lookup handle with
  | None -> not_found ()
  | Some c -> (
      match f c with
      | None -> Resp.see_other (contact_url handle ^ dest)
      | Some c' -> (
          match env.save c' with
          | Ok () -> Resp.see_other (contact_url handle ^ dest)
          | Error msg -> failed msg))

let routes =
  [
    get nil (fun env req ->
        let q =
          match Req.query_param req "q" with
          | Some q -> String.trim q
          | None -> ""
        in
        let found =
          if String.equal q "" then env.list_contacts () else env.search q
        in
        let sorted =
          List.sort
            (fun (a : Contact.t) (b : Contact.t) ->
              String.compare a.Contact.handle b.Contact.handle)
            found
        in
        Resp.html
          (Pages.index ~query:q ~has_thumb:(fun h -> has_thumb env h) sorted));
    get (s "new" /? nil) (fun _env _req ->
        Resp.html
          (Pages.new_form ~handle:"" ~name:"" ~kind:"person" ~email:"" ()));
    post (s "new" /? nil) (fun env req ->
        let handle = field req "handle" in
        let name = field req "name" in
        let kind = field req "kind" in
        let email = field req "email" in
        let reject (status : Status.t) msg =
          Resp.html ~status
            (Pages.new_form ~error:msg ~handle ~name ~kind ~email ())
        in
        if String.equal handle "" then reject `Bad_request "A handle is required."
        else if String.contains handle '/' then
          reject `Bad_request "A handle may not contain a slash."
        else
          match env.lookup handle with
          | Some _ ->
              reject `Conflict ("The handle " ^ handle ^ " is already taken.")
          | None -> (
              let c =
                {
                  Contact.version = 1;
                  kind = kind_of kind;
                  handle;
                  names = (if String.equal name "" then [ handle ] else [ name ]);
                  emails =
                    (match opt email with
                    | None -> []
                    | Some address ->
                        [
                          {
                            Contact.address;
                            type_ = None;
                            range = None;
                            note = None;
                          };
                        ]);
                  organizations = [];
                  urls = [];
                  services = [];
                  icon = None;
                  thumbnail = None;
                  orcid = None;
                  feeds = None;
                  atproto = None;
                }
              in
              match env.save c with
              | Ok () -> Resp.see_other (contact_url handle)
              | Error msg -> failed msg));
    get (s "contact" / str /? nil) (fun handle env _req ->
        match env.lookup handle with
        | None -> not_found ()
        | Some c -> Resp.html (Pages.detail ~has_thumb:(has_thumb env handle) c));
    get (s "contact" / str / s "edit" /? nil) (fun handle env _req ->
        match env.lookup handle with
        | None -> not_found ()
        | Some c -> Resp.html (Pages.edit c));
    post (s "contact" / str / s "edit" /? nil) (fun handle env req ->
        update env handle ~dest:"" (fun c ->
            let names = lines (raw req "names") in
            Some
              {
                c with
                Contact.names =
                  (match names with [] -> c.Contact.names | names -> names);
                kind = kind_of (field req "kind");
                orcid = opt (field req "orcid");
                icon = opt (field req "icon");
              }));
    post (s "contact" / str / s "delete" /? nil) (fun handle env _req ->
        match env.lookup handle with
        | None -> not_found ()
        | Some _ -> (
            match env.delete handle with
            | Ok () -> Resp.see_other "/"
            | Error msg -> failed msg));
    post (s "contact" / str / s "email" / s "add" /? nil) (fun handle env req ->
        update env handle ~dest:"/edit" (fun c ->
            match opt (field req "address") with
            | None -> None
            | Some address ->
                let e =
                  {
                    Contact.address;
                    type_ = email_type_of (field req "type");
                    range = None;
                    note = None;
                  }
                in
                Some { c with Contact.emails = c.Contact.emails @ [ e ] }));
    post (s "contact" / str / s "email" / s "remove" /? nil)
      (fun handle env req ->
        update env handle ~dest:"/edit" (fun c ->
            match opt (field req "address") with
            | None -> None
            | Some address ->
                Some
                  {
                    c with
                    Contact.emails =
                      List.filter
                        (fun (e : Contact.email) ->
                          not (String.equal e.Contact.address address))
                        c.Contact.emails;
                  }));
    post (s "contact" / str / s "url" / s "add" /? nil) (fun handle env req ->
        update env handle ~dest:"/edit" (fun c ->
            match opt (field req "url") with
            | None -> None
            | Some url ->
                let u =
                  {
                    Contact.url;
                    label = opt (field req "label");
                    range = None;
                  }
                in
                Some { c with Contact.urls = c.Contact.urls @ [ u ] }));
    post (s "contact" / str / s "url" / s "remove" /? nil) (fun handle env req ->
        update env handle ~dest:"/edit" (fun c ->
            match opt (field req "url") with
            | None -> None
            | Some url ->
                Some
                  {
                    c with
                    Contact.urls =
                      List.filter
                        (fun (u : Contact.url_entry) ->
                          not (String.equal u.Contact.url url))
                        c.Contact.urls;
                  }));
    post (s "contact" / str / s "service" / s "add" /? nil)
      (fun handle env req ->
        update env handle ~dest:"/edit" (fun c ->
            match opt (field req "url") with
            | None -> None
            | Some url ->
                let sv =
                  {
                    Contact.url;
                    kind = None;
                    handle = opt (field req "handle");
                    label = opt (field req "label");
                    range = None;
                    primary = false;
                  }
                in
                Some { c with Contact.services = c.Contact.services @ [ sv ] }));
    post (s "contact" / str / s "service" / s "remove" /? nil)
      (fun handle env req ->
        update env handle ~dest:"/edit" (fun c ->
            match opt (field req "url") with
            | None -> None
            | Some url ->
                Some
                  {
                    c with
                    Contact.services =
                      List.filter
                        (fun (sv : Contact.service) ->
                          not (String.equal sv.Contact.url url))
                        c.Contact.services;
                  }));
    post (s "contact" / str / s "org" / s "add" /? nil) (fun handle env req ->
        update env handle ~dest:"/edit" (fun c ->
            match opt (field req "name") with
            | None -> None
            | Some name ->
                let o =
                  {
                    Contact.name;
                    title = opt (field req "title");
                    department = None;
                    range = None;
                    email = None;
                    url = None;
                    address = None;
                  }
                in
                Some
                  {
                    c with
                    Contact.organizations = c.Contact.organizations @ [ o ];
                  }));
    post (s "contact" / str / s "org" / s "remove" /? nil) (fun handle env req ->
        update env handle ~dest:"/edit" (fun c ->
            match opt (field req "name") with
            | None -> None
            | Some name ->
                Some
                  {
                    c with
                    Contact.organizations =
                      List.filter
                        (fun (o : Contact.organization) ->
                          not (String.equal o.Contact.name name))
                        c.Contact.organizations;
                  }));
    get (s "thumbnail" / str /? nil) (fun handle env _req ->
        match env.thumbnail handle with
        | None -> not_found ()
        | Some png ->
            Resp.media
              ~etag:(`Strong (Digest.to_hex (Digest.string png)))
              ~cache:thumb_cache "image/png" png);
    get (s "static" / s "style.css" /? nil) (fun _env _req ->
        Resp.media ~etag:css_etag ~cache:css_cache "text/css; charset=utf-8"
          Html.css);
  ]

let site =
  Site.with_fallback (fun _env _req -> not_found ()) (Site.of_routes routes)

let compiled = Compiled.compile site
