(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Common

let service url =
  if String.contains url '#' || String.contains url '?' then
    fail "JMAP identity service cannot contain a query or fragment";
  let u = get_ok (Fetch.Middleware.Url.of_string url) in
  if Fetch.Middleware.Url.scheme u <> `Https then
    fail "JMAP service requires HTTPS";
  Fetch.Middleware.Url.to_string u

let email ~service:server ~account ~email =
  let service = service server in
  if account = "" || email = "" then
    fail "email capture requires account and email IDs";
  obj
    [
      ("service", str service);
      ("account_id", str account);
      ("email_id", str email);
    ]

let key target =
  let quote s =
    let b = Buffer.create (String.length s + 2) in
    Buffer.add_char b '"';
    String.iter
      (fun c ->
        match c with
        | '"' -> Buffer.add_string b "\\\""
        | '\\' -> Buffer.add_string b "\\\\"
        | c when Char.code c < 32 ->
            Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
        | c -> Buffer.add_char b c)
      s;
    Buffer.add_char b '"';
    Buffer.contents b
  in
  "["
  ^ String.concat ","
      (List.map quote
         [
           "jmap-email";
           field "service" target;
           field "account_id" target;
           field "email_id" target;
         ])
  ^ "]"

let source ?(hints = obj []) target =
  obj
    [
      ("id", str "source-email");
      ("rel", str "source");
      ("type", str "jmap-email");
      ("target", target);
      ("hints", hints);
    ]

let matches target note =
  List.exists
    (fun l ->
      field "type" l = "jmap-email" && key (get "target" l) = key target)
    (items "links" note.Doc.meta)

let capture ?(new_task = false) ?(tags = []) ?(body = "") ?(hints = obj [])
    ~title store target =
  let notes, errors = Store.scan store in
  if errors <> [] then
    fail "repair invalid notes before email capture can check for duplicates";
  let found = List.filter (matches target) notes in
  if found <> [] && not new_task then found
  else
    let id =
      if new_task then new_uuid () else uuid5 store.Store.id (key target)
    in
    let doc =
      Doc.create ~id ~title ~tags ~body ~links:[ source ~hints target ] ()
    in
    try [ Store.add store doc ]
    with Error _ as exn ->
      (* A concurrent local capture may have won conditional creation. *)
      if (not new_task) && exists (Store.note_path store id) then
        let current = Store.get store id in
        if matches target current then [ current ] else raise exn
      else raise exn
