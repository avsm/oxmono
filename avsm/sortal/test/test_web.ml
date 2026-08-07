(* The web UI driven through proffer.mock: no sockets, no filesystem. The
   store is a hashtable, so a test sees exactly what a handler asked of it. *)

open Proffer
module Contact = Sortal_schema.Contact

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

(* Store *)

let contacts : (string, Contact.t) Hashtbl.t = Hashtbl.create 8
let thumbs : (string, string) Hashtbl.t = Hashtbl.create 8

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i =
    i + n <= h && (String.equal (String.sub hay i n) needle || go (i + 1))
  in
  n = 0 || go 0

let matches q (c : Contact.t) =
  let q = String.lowercase_ascii q in
  let any l = List.exists (fun s -> contains (String.lowercase_ascii s) q) l in
  any (c.Contact.handle :: c.Contact.names)
  || any (List.map (fun (e : Contact.email) -> e.Contact.address) c.Contact.emails)

let all () = Hashtbl.fold (fun _ c acc -> c :: acc) contacts []

let env : Sortal_web.env =
  {
    Sortal_web.list_contacts = all;
    lookup = (fun h -> Hashtbl.find_opt contacts h);
    search = (fun q -> List.filter (matches q) (all ()));
    save =
      (fun c ->
        Hashtbl.replace contacts c.Contact.handle c;
        Ok ());
    delete =
      (fun h ->
        if Hashtbl.mem contacts h then (
          Hashtbl.remove contacts h;
          Ok ())
        else Error "no such contact");
    thumbnail = (fun h -> Hashtbl.find_opt thumbs h);
  }

let stored h =
  match Hashtbl.find_opt contacts h with
  | Some c -> c
  | None ->
      prerr_endline ("FAIL: no stored contact " ^ h);
      exit 1

(* Requests *)

let urlenc s =
  let b = Buffer.create (String.length s + 8) in
  String.iter
    (fun c ->
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' ->
          Buffer.add_char b c
      | ' ' -> Buffer.add_char b '+'
      | c -> Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents b

let encode fields =
  String.concat "&"
    (List.map (fun (k, v) -> urlenc k ^ "=" ^ urlenc v) fields)

let get ?headers target =
  Proffer_mock.request ?headers Sortal_web.compiled env `GET target

let post ?(fields = []) target =
  Proffer_mock.request
    ~headers:[ ("content-type", "application/x-www-form-urlencoded") ]
    ~body:(encode fields) Sortal_web.compiled env `POST target

let code (o : Serve.outcome) = Status.code o.Serve.status

let body (o : Serve.outcome) =
  match o.Serve.body with `String s -> s | `Empty -> "" | `Stream _ -> ""

let header (o : Serve.outcome) name =
  let name = String.lowercase_ascii name in
  let rec go = function
    | [] -> None
    | (k, v) :: tl ->
        if String.equal (String.lowercase_ascii k) name then Some v else go tl
  in
  go o.Serve.headers

let header_exn o name =
  match header o name with
  | Some v -> v
  | None ->
      prerr_endline ("FAIL: no header " ^ name);
      exit 1

(* The empty index *)

let () =
  let o = get "/" in
  check "empty index is 200" (code o = 200);
  check "empty index counts" (contains (body o) "0 contacts");
  check "empty index says so" (contains (body o) "No contacts to show.");
  check "index links the stylesheet"
    (contains (body o) "href=\"/static/style.css\"");
  check "index has no script" (not (contains (body o) "<script"))

(* Creating *)

let () =
  let o = post "/new" ~fields:[ ("handle", "") ] in
  check "empty handle is rejected" (code o = 400);
  let o = post "/new" ~fields:[ ("handle", "a/b") ] in
  check "handle with a slash is rejected" (code o = 400)

let () =
  let o =
    post "/new"
      ~fields:
        [
          ("handle", "avsm");
          ("name", "Anil Madhavapeddy");
          ("kind", "person");
          ("email", "anil@recoil.org");
        ]
  in
  check "create redirects" (code o = 303);
  check "create points at the contact"
    (String.equal (header_exn o "location") "/contact/avsm");
  let c = stored "avsm" in
  check "create keeps the name" (c.Contact.names = [ "Anil Madhavapeddy" ]);
  check "create keeps the email"
    (match c.Contact.emails with
    | [ e ] -> String.equal e.Contact.address "anil@recoil.org"
    | _ -> false);
  check "create is version 1" (c.Contact.version = 1)

let () =
  let o = post "/new" ~fields:[ ("handle", "avsm"); ("name", "Someone") ] in
  check "duplicate handle is a conflict" (code o = 409);
  check "conflict explains itself" (contains (body o) "already taken");
  check "conflict keeps the form" (contains (body o) "value=\"avsm\"")

(* The index with contacts in it *)

let () =
  let o =
    post "/new"
      ~fields:
        [
          ("handle", "jon");
          ("name", "Jon Ludlam");
          ("kind", "person");
          ("email", "jon@example.com");
        ]
  in
  check "second create redirects" (code o = 303);
  let o = get "/" in
  check "index counts both" (contains (body o) "2 contacts");
  check "index links a contact" (contains (body o) "href=\"/contact/avsm\"");
  check "index shows the email" (contains (body o) "anil@recoil.org");
  (* Sorted by handle, so avsm comes before jon. *)
  let s = body o in
  let idx sub =
    let rec go i =
      if i + String.length sub > String.length s then -1
      else if String.equal (String.sub s i (String.length sub)) sub then i
      else go (i + 1)
    in
    go 0
  in
  check "index sorts by handle" (idx "/contact/avsm" < idx "/contact/jon")

let () =
  let o = get "/?q=anil" in
  check "search is 200" (code o = 200);
  check "search counts one" (contains (body o) "1 contact");
  check "search keeps the match" (contains (body o) "/contact/avsm");
  check "search drops the rest" (not (contains (body o) "/contact/jon"));
  check "search echoes the query" (contains (body o) "value=\"anil\"")

(* Detail *)

let () =
  check "unknown contact is 404" (code (get "/contact/nobody") = 404);
  let o = get "/contact/avsm" in
  check "detail is 200" (code o = 200);
  check "detail names the contact" (contains (body o) "Anil Madhavapeddy");
  check "detail lists the email" (contains (body o) "anil@recoil.org");
  check "detail offers delete"
    (contains (body o) "action=\"/contact/avsm/delete\"")

(* Scalar edit *)

let () =
  check "unknown edit form is 404" (code (get "/contact/nobody/edit") = 404);
  let o = get "/contact/avsm/edit" in
  check "edit form is 200" (code o = 200);
  check "edit form posts to itself"
    (contains (body o) "action=\"/contact/avsm/edit\"");
  check "edit form has an email adder"
    (contains (body o) "action=\"/contact/avsm/email/add\"")

let () =
  let o =
    post "/contact/avsm/edit"
      ~fields:
        [
          ("names", "Anil Madhavapeddy\r\nAVSM\r\n");
          ("kind", "org");
          ("orcid", "0000-0001-2345-6789");
          ("icon", "https://example.com/a.png");
        ]
  in
  check "edit redirects to the detail" (code o = 303);
  check "edit redirect target"
    (String.equal (header_exn o "location") "/contact/avsm");
  let c = stored "avsm" in
  check "edit splits the names" (c.Contact.names = [ "Anil Madhavapeddy"; "AVSM" ]);
  check "edit sets the kind" (c.Contact.kind = Contact.Organization);
  check "edit sets the orcid" (c.Contact.orcid = Some "0000-0001-2345-6789");
  check "edit sets the icon" (c.Contact.icon = Some "https://example.com/a.png");
  (* Put it back so the rest of the run reads naturally. *)
  let o =
    post "/contact/avsm/edit"
      ~fields:[ ("names", "Anil Madhavapeddy"); ("kind", "person") ]
  in
  check "edit clears the scalars" (code o = 303);
  let c = stored "avsm" in
  check "edit unsets the orcid" (c.Contact.orcid = None);
  check "edit unsets the icon" (c.Contact.icon = None)

let () =
  check "edit of an unknown contact is 404"
    (code (post "/contact/nobody/edit" ~fields:[ ("kind", "person") ]) = 404)

(* Collections *)

let () =
  let o =
    post "/contact/avsm/email/add"
      ~fields:[ ("address", "avsm@cl.cam.ac.uk"); ("type", "work") ]
  in
  check "email add redirects" (code o = 303);
  check "email add returns to the editor"
    (String.equal (header_exn o "location") "/contact/avsm/edit");
  let c = stored "avsm" in
  check "email add appends" (List.length c.Contact.emails = 2);
  check "email add keeps the type"
    (match List.rev c.Contact.emails with
    | e :: _ -> e.Contact.type_ = Some Contact.Work
    | [] -> false);
  let o =
    post "/contact/avsm/email/remove" ~fields:[ ("address", "avsm@cl.cam.ac.uk") ]
  in
  check "email remove redirects" (code o = 303);
  let c = stored "avsm" in
  check "email remove drops one by address"
    (match c.Contact.emails with
    | [ e ] -> String.equal e.Contact.address "anil@recoil.org"
    | _ -> false)

let () =
  let o =
    post "/contact/avsm/url/add"
      ~fields:[ ("url", "https://anil.recoil.org"); ("label", "Homepage") ]
  in
  check "url add redirects" (code o = 303);
  let c = stored "avsm" in
  check "url add appends"
    (match c.Contact.urls with
    | [ u ] ->
        String.equal u.Contact.url "https://anil.recoil.org"
        && u.Contact.label = Some "Homepage"
        && u.Contact.range = None
    | _ -> false);
  let o =
    post "/contact/avsm/url/remove"
      ~fields:[ ("url", "https://anil.recoil.org") ]
  in
  check "url remove redirects" (code o = 303);
  check "url remove drops it by url" ((stored "avsm").Contact.urls = [])

let () =
  let o =
    post "/contact/avsm/service/add"
      ~fields:
        [
          ("url", "https://github.com/avsm");
          ("handle", "avsm");
          ("label", "Code");
        ]
  in
  check "service add redirects" (code o = 303);
  let c = stored "avsm" in
  check "service add appends"
    (match c.Contact.services with
    | [ sv ] ->
        String.equal sv.Contact.url "https://github.com/avsm"
        && sv.Contact.handle = Some "avsm"
        && sv.Contact.range = None
    | _ -> false);
  let o =
    post "/contact/avsm/service/remove"
      ~fields:[ ("url", "https://github.com/avsm") ]
  in
  check "service remove redirects" (code o = 303);
  check "service remove drops it by url" ((stored "avsm").Contact.services = [])

let () =
  let o =
    post "/contact/avsm/org/add"
      ~fields:[ ("name", "University of Cambridge"); ("title", "Professor") ]
  in
  check "org add redirects" (code o = 303);
  let c = stored "avsm" in
  check "org add appends"
    (match c.Contact.organizations with
    | [ o ] ->
        String.equal o.Contact.name "University of Cambridge"
        && o.Contact.title = Some "Professor"
    | _ -> false);
  check "detail shows the org"
    (contains (body (get "/contact/avsm")) "University of Cambridge");
  let o =
    post "/contact/avsm/org/remove"
      ~fields:[ ("name", "University of Cambridge") ]
  in
  check "org remove redirects" (code o = 303);
  check "org remove drops it by name"
    ((stored "avsm").Contact.organizations = [])

let () =
  check "collection add on an unknown contact is 404"
    (code (post "/contact/nobody/email/add" ~fields:[ ("address", "x@y") ]) = 404);
  check "collection remove on an unknown contact is 404"
    (code (post "/contact/nobody/url/remove" ~fields:[ ("url", "x") ]) = 404)

(* Handles that need encoding *)

let () =
  let o =
    post "/new" ~fields:[ ("handle", "o'brien x"); ("name", "O'Brien") ]
  in
  check "awkward handle is created" (code o = 303);
  check "awkward handle is percent-encoded"
    (String.equal (header_exn o "location") "/contact/o%27brien%20x");
  let o = get "/contact/o%27brien%20x" in
  check "awkward handle resolves" (code o = 200);
  check "awkward handle is HTML-escaped" (contains (body o) "O&#39;Brien");
  check "awkward handle is not raw in a link"
    (not (contains (body o) "/contact/o'brien"));
  check "awkward handle deletes"
    (code (post "/contact/o%27brien%20x/delete") = 303)

(* Thumbnails *)

let () =
  check "missing thumbnail is 404" (code (get "/thumbnail/avsm") = 404);
  Hashtbl.replace thumbs "avsm" "\137PNG\r\n\026\nnot really a png";
  let o = get "/thumbnail/avsm" in
  check "thumbnail is 200" (code o = 200);
  check "thumbnail is a png"
    (String.equal (header_exn o "content-type") "image/png");
  check "thumbnail is privately cached"
    (contains (header_exn o "cache-control") "private");
  let etag = header_exn o "etag" in
  check "thumbnail etag is strong" (String.length etag > 2 && etag.[0] = '"');
  let o = get ~headers:[ ("if-none-match", etag) ] "/thumbnail/avsm" in
  check "thumbnail revalidates to 304" (code o = 304);
  check "304 keeps the etag" (header o "etag" = Some etag);
  check "304 has no body" (String.equal (body o) "");
  let o = get "/" in
  check "index shows the thumbnail"
    (contains (body o) "src=\"/thumbnail/avsm\"")

(* The stylesheet *)

let () =
  let o = get "/static/style.css" in
  check "css is 200" (code o = 200);
  check "css content type"
    (String.equal (header_exn o "content-type") "text/css; charset=utf-8");
  check "css is immutable for a year"
    (contains (header_exn o "cache-control") "immutable"
    && contains (header_exn o "cache-control") "max-age=31536000");
  check "css has content" (contains (body o) "--accent");
  let etag = header_exn o "etag" in
  let o = get ~headers:[ ("if-none-match", etag) ] "/static/style.css" in
  check "css revalidates to 304" (code o = 304)

(* Deleting *)

let () =
  check "delete of an unknown contact is 404"
    (code (post "/contact/nobody/delete") = 404);
  let o = post "/contact/avsm/delete" in
  check "delete redirects" (code o = 303);
  check "delete returns to the index"
    (String.equal (header_exn o "location") "/");
  check "deleted contact is gone" (code (get "/contact/avsm") = 404);
  check "index drops it" (not (contains (body (get "/")) "/contact/avsm"))

(* The fallback *)

let () =
  let o = get "/nowhere/at/all" in
  check "unknown path is 404" (code o = 404);
  check "fallback is a styled page"
    (contains (body o) "/static/style.css" && contains (body o) "Not found")

let () = Printf.printf "test_web: %d checks ok\n" !checks
