open Proffer
open Proffer.Route
module Contact = Sortal_schema.Contact
module Account = Sortal_schema.Contact.Account
module Platform = Sortal_schema.Contact.Platform
module St = Httpz.Res

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
let css_etag : Etag.t = Etag.strong (Digest.to_hex (Digest.string Html.css))

(* Form and query reading. A missing field and an empty one are the same
   thing to every handler here. *)

(* The helpers below take the request at [local], since that is how a handler
   receives it. *)
let raw (req : Req.t @ local) name =
  match Req.form_param req name with Some v -> v | None -> ""
let field (req : Req.t @ local) name = String.trim (raw req name)
let opt s = if String.equal s "" then None else Some s

let lines s =
  List.filter
    (fun l -> not (String.equal l ""))
    (List.map String.trim (String.split_on_char '\n' s))

let kind_of s =
  if String.equal s "org" then Contact.Organization else Contact.Person

let contact_url handle = "/contact/" ^ Html.pct_segment handle
let not_found (respond : Resp.respond @ local) =
  Resp.html respond ~status:St.Not_found (Pages.not_found ())

let failed (respond : Resp.respond @ local) msg =
  Resp.html respond ~status:St.Internal_server_error (Pages.error msg)

let has_thumb env handle =
  match env.thumbnail handle with Some _ -> true | None -> false

(* A contact is rebuilt rather than mutated in place: {!Contact.t} is
   abstract, so every change to one of its collections goes through
   {!Contact.make} with the rest of the fields carried over unchanged. *)
let rebuild (c : Contact.t) ~names ~kind ~emails ~accounts ~links
    ~affiliations ~photo =
  Contact.make ~handle:(Contact.handle c) ~names ~kind ~emails ~accounts
    ~links ~affiliations ?photo ~feeds:(Contact.feeds c)
    ~vcard:(Contact.vcard c) ()

let with_emails c emails =
  rebuild c ~names:(Contact.names c) ~kind:(Contact.kind c) ~emails
    ~accounts:(Contact.accounts c) ~links:(Contact.links c)
    ~affiliations:(Contact.affiliations c) ~photo:(Contact.photo c)

let with_links c links =
  rebuild c ~names:(Contact.names c) ~kind:(Contact.kind c)
    ~emails:(Contact.emails c) ~accounts:(Contact.accounts c) ~links
    ~affiliations:(Contact.affiliations c) ~photo:(Contact.photo c)

let with_accounts c accounts =
  rebuild c ~names:(Contact.names c) ~kind:(Contact.kind c)
    ~emails:(Contact.emails c) ~accounts ~links:(Contact.links c)
    ~affiliations:(Contact.affiliations c) ~photo:(Contact.photo c)

let with_affiliations c affiliations =
  rebuild c ~names:(Contact.names c) ~kind:(Contact.kind c)
    ~emails:(Contact.emails c) ~accounts:(Contact.accounts c)
    ~links:(Contact.links c) ~affiliations ~photo:(Contact.photo c)

(* [value] as an account on [platform]. A federated platform expects
   [user@host]; anything else does not parse and is rejected. *)
let account_of_value platform value =
  match (platform : Platform.id) with
  | Simple p -> Some (Account.Simple (p, value))
  | Atproto -> Some (Account.Atproto { handle = value; did = None; apps = [] })
  | Federated p -> (
      match String.index_opt value '@' with
      | Some i ->
          let user = String.sub value 0 i in
          let host =
            String.sub value (i + 1) (String.length value - i - 1)
          in
          Some (Account.Federated (p, user, host))
      | None -> None)

(* Every mutation is the same shape: find the contact, rewrite the record,
   save it, then send the browser away with a 303 so a reload does not repost.
   A transform returning [None] asks for no write, which is what an empty
   required field means. *)
(* [f] is taken at [local] because it captures the request, which a handler
   receives at [local]. Each call site binds it with [local_] and is written
   [let () = ... in ()], since a local argument cannot be passed in a tail
   call. *)
let update (respond : Resp.respond @ local) env handle ~dest
    (f : (Contact.t -> Contact.t option) @ local) =
  match env.lookup handle with
  | None -> not_found respond
  | Some c -> (
      match f c with
      | None -> Resp.see_other respond (contact_url handle ^ dest)
      | Some c' -> (
          match env.save c' with
          | Ok () -> Resp.see_other respond (contact_url handle ^ dest)
          | Error msg -> failed respond msg))

let routes =
  [
    get nil (fun env req respond ->
        let q =
          match Req.query_param req "q" with
          | Some q -> String.trim q
          | None -> ""
        in
        let found =
          if String.equal q "" then env.list_contacts () else env.search q
        in
        let sorted = List.sort Contact.compare found in
        Resp.html respond
          (Pages.index ~query:q ~has_thumb:(fun h -> has_thumb env h) sorted));
    get (s "new" /? nil) (fun _env _req respond ->
        Resp.html respond
          (Pages.new_form ~handle:"" ~name:"" ~kind:"person" ~email:"" ()));
    post (s "new" /? nil) (fun env req respond ->
        let handle = field req "handle" in
        let name = field req "name" in
        let kind = field req "kind" in
        let email = field req "email" in
        (* [reject] closes over the responder, so it is local and cannot be
           called in tail position. *)
        let local_ reject (status : Status.t) msg =
          Resp.html respond ~status
            (Pages.new_form ~error:msg ~handle ~name ~kind ~email ())
        in
        if String.equal handle "" then
          let () = reject St.Bad_request "A handle is required." in
          ()
        else if String.contains handle '/' then
          let () = reject St.Bad_request "A handle may not contain a slash." in
          ()
        else
          match env.lookup handle with
          | Some _ ->
              let () =
                reject St.Conflict
                  ("The handle " ^ handle ^ " is already taken.")
              in
              ()
          | None -> (
              let names = if String.equal name "" then [ handle ] else [ name
                ] in
              let emails = match opt email with None -> [] | Some e -> [ e ] in
              let c = Contact.make ~handle ~names ~kind:(kind_of kind) ~emails
                () in
              match env.save c with
              | Ok () -> Resp.see_other respond (contact_url handle)
              | Error msg -> failed respond msg));
    get (s "contact" / str /? nil) (fun handle env _req respond ->
        match env.lookup handle with
        | None -> not_found respond
        | Some c ->
            Resp.html respond
              (Pages.detail ~has_thumb:(has_thumb env handle) c));
    get (s "contact" / str / s "edit" /? nil) (fun handle env _req respond ->
        match env.lookup handle with
        | None -> not_found respond
        | Some c -> Resp.html respond (Pages.edit c));
    post (s "contact" / str / s "edit" /? nil) (fun handle env req respond ->
        let local_ f c =
            let names = lines (raw req "names") in
            let names = match names with [] -> Contact.names c | ns -> ns in
            let kind = kind_of (field req "kind") in
            let photo = opt (field req "photo") in
            let orcid = opt (field req "orcid") in
            let accounts =
              List.filter
                (fun a -> Account.platform a <> Platform.Simple Platform.Orcid)
                (Contact.accounts c)
              @
              match orcid with
              | None -> []
              | Some o -> [ Account.Simple (Platform.Orcid, o) ]
            in
            Some
              (rebuild c ~names ~kind ~emails:(Contact.emails c) ~accounts
                 ~links:(Contact.links c) ~affiliations:(Contact.affiliations c)
                 ~photo)
          in
        let () = update respond env handle ~dest:"" f in
        ());
    post (s "contact" / str / s "delete" /? nil) (fun handle env _req respond ->
        match env.lookup handle with
        | None -> not_found respond
        | Some _ -> (
            match env.delete handle with
            | Ok () -> Resp.see_other respond "/"
            | Error msg -> failed respond msg));
    post (s "contact" / str / s "email" / s "add" /? nil)
      (fun handle env req respond ->
        let local_ f c =
            match opt (field req "address") with
            | None -> None
            | Some address -> Some (with_emails c (Contact.emails c @ [
              address ]))
          in
        let () = update respond env handle ~dest:"/edit" f in
        ());
    post (s "contact" / str / s "email" / s "remove" /? nil)
      (fun handle env req respond ->
        let local_ f c =
            match opt (field req "address") with
            | None -> None
            | Some address ->
                Some
                  (with_emails c
                     (List.filter
                        (fun e -> not (String.equal e address))
                        (Contact.emails c)))
          in
        let () = update respond env handle ~dest:"/edit" f in
        ());
    post (s "contact" / str / s "url" / s "add" /? nil)
      (fun handle env req respond ->
        let local_ f c =
            match opt (field req "url") with
            | None -> None
            | Some url ->
                let l : Contact.link = { url; label = opt (field req "label")
                  } in
                Some (with_links c (Contact.links c @ [ l ]))
          in
        let () = update respond env handle ~dest:"/edit" f in
        ());
    post (s "contact" / str / s "url" / s "remove" /? nil)
      (fun handle env req respond ->
        let local_ f c =
            match opt (field req "url") with
            | None -> None
            | Some url ->
                Some
                  (with_links c
                     (List.filter
                        (fun (u : Contact.link) -> not (String.equal u.url url))
                        (Contact.links c)))
          in
        let () = update respond env handle ~dest:"/edit" f in
        ());
    post (s "contact" / str / s "service" / s "add" /? nil)
      (fun handle env req respond ->
        let local_ f c =
            match (opt (field req "platform"), opt (field req "handle")) with
            | Some platform_key, Some value -> (
                match Platform.of_key platform_key with
                | None -> None
                | Some platform -> (
                    match account_of_value platform value with
                    | None -> None
                    | Some account ->
                        let others =
                          List.filter
                            (fun a -> Account.platform a <> platform)
                            (Contact.accounts c)
                        in
                        Some (with_accounts c (others @ [ account ]))))
            | _ -> None
          in
        let () = update respond env handle ~dest:"/edit" f in
        ());
    post (s "contact" / str / s "service" / s "remove" /? nil)
      (fun handle env req respond ->
        let local_ f c =
            match opt (field req "platform") with
            | None -> None
            | Some platform_key -> (
                match Platform.of_key platform_key with
                | None -> None
                | Some platform ->
                    Some
                      (with_accounts c
                         (List.filter
                            (fun a -> Account.platform a <> platform)
                            (Contact.accounts c))))
          in
        let () = update respond env handle ~dest:"/edit" f in
        ());
    post (s "contact" / str / s "org" / s "add" /? nil)
      (fun handle env req respond ->
        let local_ f c =
            match opt (field req "name") with
            | None -> None
            | Some org ->
                let a : Contact.affiliation =
                  {
                    org;
                    title = opt (field req "title");
                    department = None;
                    url = None;
                    address = None;
                    from = None;
                    until = None;
                  }
                in
                Some (with_affiliations c (Contact.affiliations c @ [ a ]))
          in
        let () = update respond env handle ~dest:"/edit" f in
        ());
    post (s "contact" / str / s "org" / s "remove" /? nil)
      (fun handle env req respond ->
        let local_ f c =
            match opt (field req "name") with
            | None -> None
            | Some org ->
                Some
                  (with_affiliations c
                     (List.filter
                        (fun (a : Contact.affiliation) ->
                          not (String.equal a.org org))
                        (Contact.affiliations c)))
          in
        let () = update respond env handle ~dest:"/edit" f in
        ());
    get (s "thumbnail" / str /? nil) (fun handle env _req respond ->
        match env.thumbnail handle with
        | None -> not_found respond
        | Some png ->
            Resp.media respond
              ~etag:(Etag.strong (Digest.to_hex (Digest.string png)))
              ~cache:thumb_cache "image/png" png);
    get (s "static" / s "style.css" /? nil) (fun _env _req respond ->
        Resp.media respond ~etag:css_etag ~cache:css_cache
          "text/css; charset=utf-8" Html.css);
  ]

let site =
  Site.with_fallback
    (fun _env _req respond -> not_found respond)
    (Site.of_routes routes)

let compiled = Compiled.compile site
