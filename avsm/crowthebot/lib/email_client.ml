module P = Jmap.Proto
module Chain = Jmap.Chain
module Client = Jmap_eio.Client
module Auth = Jmap_eio.Auth
module Transport = Jmap_eio.Transport
module Http = Email_http

let invalid () = invalid_arg "Invalid JMAP mail response."

let received s =
  match P.Id.of_string_received s with
  | Ok id -> id
  | Error _ -> invalid_arg "Invalid JMAP ID."

type identity = { account : string; username : string }

type connection = {
  client : Client.t;
  pinned : identity;
  writable : bool;
  page_size : int;
}

type reader = Reader of connection
type writer = Writer of connection

exception Set_error of P.Error.Set_error.t

let error = function
  | Client.Jmap_client_error (Http_error (n, _)) ->
      Printf.sprintf "JMAP mail HTTP %d." n
  | Client.Jmap_client_error (Timeout _) | Eio.Time.Timeout ->
      "JMAP mail request timed out."
  | Client.Jmap_client_error (Transport (Fetch.Denied _, _)) ->
      "JMAP mail capability denied the request."
  | Client.Jmap_client_error (Method_error e) -> (
      match e.P.Error.Method_error.type_ with
      | `State_mismatch ->
          "Email changed concurrently. Read it again before retrying labels."
      | `Other "notFound" -> "Email or thread not found."
      | `Forbidden -> "JMAP mail access denied. Check the token permissions."
      | `Invalid_arguments | `Other "invalidPatch" ->
          "JMAP mail rejected the query or label arguments."
      | _ -> "JMAP mail method failed.")
  | Client.Jmap_client_error _ -> "JMAP mail connection or protocol error."
  | Set_error _ -> "JMAP mail label update failed."
  | _ -> "JMAP mail operation failed."

let checked = function
  | Ok x -> x
  | Error e -> raise (Client.Jmap_client_error e)

let validate c =
  let s = Client.session c.client in
  let account =
    List.find_opt
      (fun (id, _) -> P.Id.to_string id = c.pinned.account)
      s.accounts
  in
  if
    s.username <> c.pinned.username
    || (not (List.mem_assoc P.Capability.mail s.capabilities))
    || not
         (match account with
         | Some (_, a) ->
             List.mem_assoc P.Capability.mail
               a.P.Session.Account.account_capabilities
             && ((not c.writable) || not a.is_read_only)
         | None -> false)
  then invalid_arg "The selected JMAP mail account or permissions changed.";
  c.pinned

let connect ~writable ~sw ~fetch ~clock ~token ?account_id
    ?(max_body = 33554432) url =
  let fetch, bind = Http.create ~url ~writable fetch in
  let client =
    Client.connect ~sw
      ~auth:(Auth.bearer (String.trim token))
      ~timeout:20. ~max_body
      (Transport.of_fetch ~clock fetch)
      url
    |> checked
  in
  let s = Client.session client in
  let account =
    match account_id with
    | Some id ->
        ignore (received id);
        id
    | None -> (
        match P.Session.primary_account_for P.Capability.mail s with
        | Some id -> P.Id.to_string id
        | None -> invalid_arg "No primary JMAP mail account.")
  in
  let page_size =
    match Jmap_eio.Sync.max_objects_in_get client with
    | None -> 10
    | Some n -> Int64.to_int (min 10L n)
  in
  if page_size < 1 then invalid ();
  let c =
    { client; pinned = { account; username = s.username }; writable; page_size }
  in
  ignore (validate c);
  bind ~account ~api_url:(Client.api_url client);
  c

let connect_read_only ~sw ~fetch ~clock ~token ?account_id ?max_body url =
  Reader
    (connect ~writable:false ~sw ~fetch ~clock ~token ?account_id ?max_body url)

let connect_read_write ~sw ~fetch ~clock ~token ?account_id ?max_body url =
  Writer
    (connect ~writable:true ~sw ~fetch ~clock ~token ?account_id ?max_body url)

let identity (Reader c) = validate c
let writer_identity (Writer c) = validate c

type query_page = {
  query : P.Method.query_response;
  next_position : int64 option;
}

type thread_page = {
  thread_id : P.Id.t;
  position : int;
  total : int;
  email_ids : P.Id.t list;
  emails : P.Email.t P.Method.get_response option;
  next_position : int option;
}

let call c build =
  let account_id = received (validate c).account in
  let result =
    Client.call c.client
      ~capabilities:[ P.Capability.core; P.Capability.mail ]
      (build ~account_id)
    |> checked
  in
  ignore (validate c);
  (account_id, result)

let check_account expected actual =
  if not (P.Id.equal expected actual) then invalid ()

let check_get account id wanted (r : _ P.Method.get_response) =
  check_account account r.account_id;
  let actual =
    List.map
      (fun x -> match id x with Some i -> i | None -> invalid ())
      r.list
    @ r.not_found
  in
  if List.sort P.Id.compare actual <> List.sort P.Id.compare wanted then
    invalid ();
  r

let properties : P.Email.property list =
  [
    `Id;
    `Blob_id;
    `Size;
    `Thread_id;
    `Mailbox_ids;
    `Keywords;
    `Received_at;
    `Sent_at;
    `From;
    `To;
    `Cc;
    `Bcc;
    `Reply_to;
    `Subject;
    `Message_id;
    `In_reply_to;
    `References;
    `Sender;
    `Headers;
    `Body_structure;
    `Text_body;
    `Html_body;
    `Body_values;
    `Attachments;
    `Has_attachment;
  ]

let emails c ids =
  let account, r =
    call c (fun ~account_id ->
        Chain.email_get ~account_id ~ids:(Chain.ids ids) ~properties
          ~fetch_text_body_values:true ~fetch_html_body_values:true ())
  in
  check_get account P.Email.id ids r

let read (Reader c) ~id = emails c [ id ]

let query (Reader c) ?filter ?sort ~position ~limit ~collapse_threads () =
  if position < 0 || limit < 1 || limit > 50 then
    invalid_arg "Query position or limit is out of range.";
  let account, query =
    call c (fun ~account_id ->
        Chain.email_query ~account_id ?filter ?sort
          ~position:(Int64.of_int position) ~limit:(Int64.of_int limit)
          ~calculate_total:true ~collapse_threads ())
  in
  check_account account query.account_id;
  let got = List.length query.ids in
  if
    got > limit
    || query.position <> Int64.of_int position
    || got <> List.length (List.sort_uniq P.Id.compare query.ids)
  then invalid ();
  let next = Int64.add query.position (Int64.of_int got) in
  let applied = Option.value ~default:(Int64.of_int limit) query.limit in
  if Int64.of_int got > applied then invalid ();
  let more =
    match query.total with
    | Some total ->
        if (got > 0 && total < next) || (got = 0 && total > next) then
          invalid ();
        got > 0 && next < total
    | None -> got > 0 && Int64.of_int got >= applied
  in
  { query; next_position = (if more then Some next else None) }

let thread (Reader c) ~id ~position ~limit =
  if position < 0 || limit < 1 || limit > 10 then
    invalid_arg "Thread position or limit is out of range.";
  let account, result =
    call c (fun ~account_id ->
        Chain.thread_get ~account_id ~ids:(Chain.id id) ())
  in
  let result = check_get account P.Thread.id [ id ] result in
  match result.list with
  | [] -> invalid_arg "Email thread not found."
  | [ thread ] ->
      let ids =
        match thread.P.Thread.email_ids with
        | Some ids -> ids
        | None -> invalid ()
      in
      if List.length ids <> List.length (List.sort_uniq P.Id.compare ids) then
        invalid ();
      let limit = min limit c.page_size in
      let page =
        List.mapi (fun i id -> (i, id)) ids
        |> List.filter_map (fun (i, id) ->
            if i >= position && i - position < limit then Some id else None)
      in
      let next = position + List.length page in
      {
        thread_id = id;
        position;
        total = List.length ids;
        email_ids = page;
        emails = (if page = [] then None else Some (emails c page));
        next_position = (if next < List.length ids then Some next else None);
      }
  | _ -> invalid ()

let mailboxes (Reader c) =
  let account, r =
    call c (fun ~account_id ->
        Chain.mailbox_get ~account_id
          ~properties:[ `Id; `Name; `Parent_id; `Role; `My_rights ]
          ())
  in
  check_account account r.account_id;
  r

let update_labels (Writer c) ~id ~add ~remove =
  let all = add @ remove in
  if
    all = []
    || List.length all > 100
    || List.length all <> List.length (List.sort_uniq P.Id.compare all)
  then invalid_arg "Supply distinct mailbox IDs to add or remove, up to 100.";
  let account, before =
    call c (fun ~account_id ->
        Chain.email_get ~account_id ~ids:(Chain.id id)
          ~properties:[ `Id; `Mailbox_ids ] ())
  in
  let before = check_get account P.Email.id [ id ] before in
  let email =
    match before.list with [ e ] -> e | _ -> invalid_arg "Email not found."
  in
  let current = P.Email.mailbox_list email in
  if List.filter (fun s -> not (List.mem s remove)) current @ add = [] then
    invalid_arg "An email must retain at least one mailbox label.";
  let patch =
    P.Patch.v
      (List.map P.Email.Patch.add_to_mailbox add
      @ List.map P.Email.Patch.remove_from_mailbox remove)
  in
  let account, r =
    call c (fun ~account_id ->
        Chain.email_set ~account_id ~if_in_state:before.state
          ~update:[ (id, patch) ]
          ())
  in
  check_account account r.account_id;
  let failed = Option.value ~default:[] r.not_updated in
  (match List.assoc_opt id failed with
  | Some e -> raise (Set_error e)
  | None -> ());
  if List.map fst (Option.value ~default:[] r.updated) <> [ id ] || failed <> []
  then invalid ();
  r
