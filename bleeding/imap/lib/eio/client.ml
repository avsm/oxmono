type t = {
  session : Session.t;
  mutable objectid_pins : (string * (string * string)) list
}
type error = Error.t

let pp_error ppf = function
| Session.Closed -> Format.pp_print_string ppf "IMAP connection closed"
| Protocol s -> Format.fprintf ppf "IMAP protocol error: %s" s
| Transport s -> Format.fprintf ppf "IMAP transport error: %s" s
| Rejected {tag; status; code; text} ->
    let label=Option.bind code Imap.Response.response_code_name in
    Format.fprintf ppf "IMAP %s %s%s: %s" tag
      (match status with `No -> "NO" | `Bad -> "BAD")
      (match label with None -> "" | Some name -> " [" ^ name ^ "]") text
| State s -> Format.fprintf ppf "IMAP state error: %s" s
| Missing_uid uid -> Format.fprintf ppf "IMAP UID %Ld vanished" uid
| Limit s -> Format.fprintf ppf "IMAP limit error: %s" s
| Uncertain s -> Format.fprintf ppf "IMAP uncertain outcome: %s" s

let error_to_string e = Format.asprintf "%a" pp_error e

let upper = String.uppercase_ascii
let same_mailbox expected received =
  expected=received || (upper expected="INBOX" && upper received="INBOX")
let begins s p =
  String.length s >= String.length p && upper (String.sub s 0 (String.length p)) = p

let greeting session =
  let response = Session.parse (Session.read_response session) in
  match response with
  | Imap.Response.Untagged (Imap.Response.Ok _) -> false
  | Imap.Response.Untagged (Imap.Response.Bye (_, text)) ->
      raise (Session.Failure (Session.Protocol ("server BYE greeting: " ^ text)))
  | Imap.Response.Untagged (Imap.Response.Preauth _) -> true
  | _ -> raise (Session.Failure (Session.Protocol "invalid IMAP greeting"))

let capability session =
  let responses = Session.command session Imap.Command.capability in
  let caps = List.concat_map (function
    | Imap.Response.Untagged (Imap.Response.Capability caps) -> List.map upper caps
    | _ -> []) responses in
  session.Session.capabilities <- caps

let has session name = List.mem name session.Session.capabilities

let enable_revision session =
  if has session "IMAP4REV2" && has session "IMAP4REV1" then (
    let responses = try Session.command session "ENABLE IMAP4rev2" with
      | Session.Failure (Session.Rejected _) -> [] in
    let enabled = List.concat_map (function
      | Imap.Response.Untagged (Imap.Response.Enabled names) ->
          List.map upper names
      | _ -> []) responses in
    session.Session.enabled <- enabled)

let revision_two session =
  has session "IMAP4REV2" &&
  (not (has session "IMAP4REV1") ||
   List.mem "IMAP4REV2" session.Session.enabled)

let enable_utf8 session =
  if not (revision_two session) && has session "UTF8=ACCEPT" then (
    let responses = try Session.command session "ENABLE UTF8=ACCEPT" with
      | Session.Failure (Session.Rejected _) -> [] in
    let enabled = List.concat_map (function
      | Imap.Response.Untagged (Imap.Response.Enabled names) ->
          List.map upper names
      | _ -> []) responses in
    session.Session.enabled <- enabled @ session.Session.enabled)

let enable_qresync session =
  if has session "QRESYNC" then (
    let responses = try Session.command session "ENABLE QRESYNC" with
      | Session.Failure (Session.Rejected _) -> [] in
    let enabled = List.concat_map (function
      | Imap.Response.Untagged (Imap.Response.Enabled names) ->
          List.map upper names
      | _ -> []) responses in
    session.Session.enabled <- enabled @ session.Session.enabled)

let mailbox_mode session =
  if revision_two session || List.mem "UTF8=ACCEPT" session.Session.enabled
  then Imap.Mailbox_name.Utf8 else Imap.Mailbox_name.Rev1

let mailbox_wire session utf8 =
  match Imap.Mailbox_name.encode ~mode:(mailbox_mode session) utf8 with
  | Ok raw -> raw
  | Error e -> raise (Session.Failure (Session.State
      ("invalid mailbox name: " ^ e)))

let login session auth =
  if has session "LOGINDISABLED" then
    raise (Session.Failure (Session.State "LOGIN disabled by server"));
  let password = Auth.resolve_password auth in
  let syntax = match Imap.Command.login ~username:(Auth.username auth) ~password with
  | Ok x -> x | Error e -> raise (Session.Failure (Session.State e)) in
  ignore (Session.command session syntax)

let authenticate session auth ~secure =
  let mechanism = match Auth.mechanism auth with
  | `Auto ->
      if secure && has session "AUTH=PLAIN" then `Plain
      else if has session "AUTH=CRAM-MD5" then `Cram_md5
      else `Login
  | (`Login | `Cram_md5 | `Plain | `Oauthbearer) as mechanism -> mechanism in
  (match mechanism with
   | `Login ->
       if not secure && not (Auth.allow_insecure_transport auth) then
         raise (Session.Failure (Session.State "LOGIN requires TLS"));
       if has session "LOGINDISABLED" then
         raise (Session.Failure (Session.State "LOGIN disabled by server"))
   | `Cram_md5 ->
       if not (has session "AUTH=CRAM-MD5") then
         raise (Session.Failure (Session.State "server does not advertise AUTH=CRAM-MD5"))
   | (`Plain | `Oauthbearer) as mechanism ->
       let name=if mechanism=`Plain then "PLAIN" else "OAUTHBEARER" in
       if not (has session ("AUTH=" ^ name)) then
         raise (Session.Failure (Session.State ("server does not advertise AUTH=" ^ name)));
       if not secure && not (Auth.allow_insecure_transport auth) then
         raise (Session.Failure (Session.State ("AUTH=" ^ name ^ " requires TLS"))));
  try match mechanism with
  | `Login -> login session auth
  | `Cram_md5 -> Session.authenticate_cram_md5 session auth
  | (`Plain | `Oauthbearer) as mechanism ->
      let name=if mechanism=`Plain then "PLAIN" else "OAUTHBEARER" in
      let encoded=if mechanism=`Plain then Auth.plain_response auth
        else Auth.oauthbearer_response auth in
      Session.authenticate_initial session ~mechanism:name ~encoded
        ~sasl_ir:(has session "SASL-IR") ~oauthbearer:(mechanism=`Oauthbearer)
  with
  | Eio.Cancel.Cancelled _ as ex -> raise ex
  | Session.Failure (Session.Rejected {tag; status; code; _}) ->
      raise (Session.authentication_rejected ~tag ~status ~code)
  | Session.Failure Session.Closed -> raise (Session.Failure Session.Closed)
  | Session.Failure (Session.Limit _) ->
      raise (Session.Failure (Session.Limit "authentication exchange exceeded limits"))
  | Session.Failure (Session.Protocol _) ->
      raise (Session.Failure (Session.Protocol "invalid authentication exchange"))
  | Session.Failure (Session.State _) ->
      raise (Session.Failure (Session.State "invalid authentication credentials"))
  | _ -> raise (Session.Failure (Session.Transport "authentication exchange failed"))

let start ~sw ?auth ?endpoint flow =
  let session = Session.create flow in
  let fail ex = Session.close session; raise ex in
  try
    let preauth = greeting session in
    capability session;
    let secure = match endpoint with
    | None -> false
    | Some transport ->
        (match Transport.tls transport with
         | `Implicit -> true
         | `Plain -> false
         | `Required_starttls ->
             if preauth then raise (Session.Failure
               (Session.State "PREAUTH before required STARTTLS"));
             if not (has session "STARTTLS") then raise (Session.Failure
               (Session.State "server does not offer STARTTLS"));
             ignore (Session.command session "STARTTLS");
             if session.Session.queued <> [] then raise (Session.Failure
               (Session.Protocol "plaintext bytes after STARTTLS completion"));
             (match Imap.Wire.finish session.Session.wire with
              | Error _ -> raise (Session.Failure
                  (Session.Protocol "incomplete plaintext after STARTTLS completion"))
              | Ok () -> ());
             Transport.upgrade transport session.Session.flow;
             session.Session.wire <- Imap.Wire.create ();
             capability session;
             true)
    in
    if not preauth then (
      match auth with
      | None -> raise (Session.Failure (Session.State "authentication required"))
      | Some auth -> authenticate session auth ~secure);
    capability session;
    enable_revision session;
    enable_utf8 session;
    enable_qresync session;
    Eio.Switch.on_release sw (fun () -> Session.close session);
    {session; objectid_pins=[]}
  with ex -> fail ex

let connect ~sw ?auth transport =
  try
    let flow = Transport.connect ~sw transport in
    Ok (start ~sw ?auth ~endpoint:transport flow)
  with
  | Session.Failure e -> Error e
  | Eio.Cancel.Cancelled _ as ex -> raise ex
  | ex -> Error (Session.Transport (Printexc.to_string ex))

let of_flow ~sw ?auth flow =
  let flow = Transport.of_flow flow in
  try Ok (start ~sw ?auth flow) with
  | Session.Failure e -> Error e
  | Eio.Cancel.Cancelled _ as ex -> raise ex
  | ex -> Error (Session.Transport (Printexc.to_string ex))

let capabilities t = t.session.Session.capabilities
let enabled t = t.session.Session.enabled
let mailbox_mode t = mailbox_mode t.session
let noop t = Session.locked t.session (fun () ->
  Session.command t.session Imap.Command.noop)
let logout t = Session.locked t.session (fun () -> Session.logout t.session)

let close t = Session.close t.session
let is_open t = not t.session.Session.closed

let compress_deflate t =
  Session.locked t.session (fun () -> Session.compress_deflate t.session)

let enable_uidonly t =
  Session.locked t.session (fun () ->
    if not (has t.session "UIDONLY" && has t.session "ENABLE") then
      raise (Session.Failure (Session.State
        "UIDONLY and ENABLE must both be advertised"));
    if Option.is_some t.session.Session.selected then
      raise (Session.Failure (Session.State
        "UIDONLY must be enabled before selecting a mailbox"));
    if not (List.mem "UIDONLY" t.session.Session.enabled) then (
      let responses = Session.command t.session "ENABLE UIDONLY" in
      let accepted = List.concat_map (function
        | Imap.Response.Untagged (Imap.Response.Enabled names) ->
            List.map upper names
        | _ -> []) responses in
      if not (List.mem "UIDONLY" accepted) then
        raise (Session.Failure (Session.Protocol
          "UIDONLY ENABLE completed without ENABLED UIDONLY"));
      t.session.Session.enabled <-
        List.sort_uniq String.compare (accepted @ t.session.Session.enabled)))

let enable_objectid_plus t =
  Session.locked t.session (fun () ->
    if not (has t.session "OBJECTID+" && has t.session "ENABLE") then
      raise (Session.Failure (Session.State
        "OBJECTID+ and ENABLE must both be advertised"));
    if Option.is_some t.session.Session.selected then
      raise (Session.Failure (Session.State
        "OBJECTID+ must be enabled outside a selected lease"));
    if not (List.mem "OBJECTID+" t.session.Session.enabled) then (
      let responses=Session.command t.session "ENABLE OBJECTID+" in
      let accepted=List.concat_map (function
        | Imap.Response.Untagged (Imap.Response.Enabled names) ->
            List.map upper names
        | _ -> []) responses in
      if not (List.mem "OBJECTID+" accepted) then
        raise (Session.Failure (Session.Protocol
          "OBJECTID+ ENABLE completed without ENABLED OBJECTID+"));
      t.session.Session.enabled <-
        List.sort_uniq String.compare
          (accepted @ t.session.Session.enabled)))

let pin_mailbox_objectid t ~mailbox ~account_id ~mailbox_id =
  Session.locked t.session (fun () ->
    if not (List.mem "OBJECTID+" t.session.Session.enabled) then
      raise (Session.Failure (Session.State "OBJECTID+ not enabled"));
    if Option.is_some t.session.Session.selected then
      raise (Session.Failure (Session.State
        "cannot pin OBJECTID+ during a selected lease"));
    let identity=(account_id,mailbox_id) in
    (match Imap.Command.select ~objectid:identity mailbox with
     | Ok _ -> ()
     | Error message -> raise (Session.Failure (Session.State message)));
    match List.find_opt (fun (name,_) -> same_mailbox name mailbox)
      t.objectid_pins with
    | None -> t.objectid_pins <- (mailbox,identity)::t.objectid_pins
    | Some (_,existing) when existing=identity -> ()
    | Some _ -> raise (Session.Failure (Session.State
        "OBJECTID+ mailbox pin changed on one connection")))

let list t ?(reference="") ~pattern () =
  Session.locked t.session (fun () ->
    let reference = mailbox_wire t.session reference in
    let pattern = mailbox_wire t.session pattern in
    let syntax = match Imap.Command.list ~reference ~pattern with
    | Ok s -> s | Error e -> raise (Session.Failure (Session.State e)) in
    Session.command t.session syntax
    |> List.filter_map (function
        | Imap.Response.Untagged (Imap.Response.List item) -> Some item
        | _ -> None))

let lsub t ?(reference="") ~pattern () =
  Session.locked t.session (fun () ->
    let reference=mailbox_wire t.session reference in
    let pattern=mailbox_wire t.session pattern in
    let syntax=match Imap.Command.lsub ~reference ~pattern with
      | Ok s -> s | Error e -> raise (Session.Failure (Session.State e)) in
    Session.command t.session syntax
    |> List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.List item)
          when item.subscribed -> Some item
      | _ -> None))

let namespace t =
  Session.locked t.session (fun () ->
    if not (has t.session "NAMESPACE" || revision_two t.session) then
      raise (Session.Failure (Session.State "NAMESPACE unavailable"));
    let responses=Session.command t.session Imap.Command.namespace in
    match List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Namespace item) -> Some item
      | _ -> None) responses with
    | [item] -> item
    | [] -> raise (Session.Failure (Session.Protocol
        "missing NAMESPACE response"))
    | _ -> raise (Session.Failure (Session.Protocol
        "duplicate NAMESPACE response")))

type discovery = {
  mailboxes:(Imap.Response.list_result * Imap.Response.mailbox_status option) list;
  unpaired_status:Imap.Response.mailbox_status list
}

let list_extended t ?(reference="") ~patterns ?(selection=[])
    ?(returns=[]) ?status () =
  Session.locked t.session (fun () ->
    let extended=selection<>[] || returns<>[] || status<>None ||
      List.length patterns<>1 in
    if extended && not (has t.session "LIST-EXTENDED" ||
                        revision_two t.session) then
      raise (Session.Failure (Session.State "LIST-EXTENDED unavailable"));
    if List.length patterns>1 && not (has t.session "LIST-EXTENDED") then
      raise (Session.Failure (Session.State
        "multiple LIST patterns require LIST-EXTENDED"));
    if (List.mem Imap.Command.Special_use selection ||
        List.mem Imap.Command.Return_special_use returns) &&
        not (has t.session "SPECIAL-USE") then
      raise (Session.Failure (Session.State "SPECIAL-USE unavailable"));
    if status<>None && not (has t.session "LIST-STATUS" ||
                            revision_two t.session) then
      raise (Session.Failure (Session.State "LIST-STATUS unavailable"));
    if (match status with Some items -> List.mem Imap.Command.Objectid items
        | None -> false) &&
       not (List.mem "OBJECTID+" t.session.Session.enabled) then
      raise (Session.Failure (Session.State
        "LIST-STATUS OBJECTID requires enabled OBJECTID+"));
    let reference=mailbox_wire t.session reference in
    let patterns=List.map (mailbox_wire t.session) patterns in
    let syntax=match Imap.Command.list_extended ~reference ~patterns
        ~selection ~returns ?status () with
      | Ok syntax -> syntax
      | Error e -> raise (Session.Failure (Session.State e)) in
    let responses=Session.command t.session syntax in
    let canonical name=if upper name="INBOX" then "INBOX" else name in
    let seen=Hashtbl.create 32 in
    let rec collect mailboxes unpaired_status = function
      | Imap.Response.Untagged (Imap.Response.List listing)::rest ->
          let key=canonical listing.Imap.Response.mailbox in
          if Hashtbl.mem seen key then
            raise (Session.Failure (Session.Protocol
              "duplicate LIST mailbox response"));
          Hashtbl.add seen key ();
          (match rest with
           | Imap.Response.Untagged (Imap.Response.Status item)::rest
             when status<>None && same_mailbox listing.mailbox item.mailbox ->
               collect ((listing,Some item)::mailboxes) unpaired_status rest
           | _ -> collect ((listing,None)::mailboxes) unpaired_status rest)
      | Imap.Response.Untagged (Imap.Response.Status item)::rest ->
          collect mailboxes (item::unpaired_status) rest
      | _::rest -> collect mailboxes unpaired_status rest
      | [] -> {mailboxes=List.rev mailboxes;
               unpaired_status=List.rev unpaired_status} in
    collect [] [] responses)

let status_locked t ~mailbox ~items =
    if List.mem Imap.Command.Objectid items &&
       not (List.mem "OBJECTID+" t.session.Session.enabled) then
      raise (Session.Failure (Session.State
        "STATUS OBJECTID requires enabled OBJECTID+"));
    let mailbox = mailbox_wire t.session mailbox in
    let syntax = match Imap.Command.status ~mailbox ~items with
      | Ok s -> s
      | Error message -> raise (Session.Failure (Session.State message)) in
    let responses = Session.command t.session syntax in
    match List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Status status)
        when same_mailbox mailbox status.mailbox -> Some status
      | _ -> None) responses with
    | [status] -> status
    | [] -> raise (Session.Failure (Session.Protocol "missing STATUS response"))
    | _ -> raise (Session.Failure (Session.Protocol "duplicate STATUS response"))

let status t ~mailbox ~items =
  Session.locked t.session (fun () -> status_locked t ~mailbox ~items)

let get_jmap_access t =
  Session.locked t.session (fun () ->
    if not (has t.session "JMAPACCESS") then
      raise (Session.Failure (Session.State "JMAPACCESS unavailable"));
    let responses = Session.command t.session Imap.Command.get_jmap_access in
    match List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Jmapaccess value) -> Some value
      | _ -> None) responses with
    | [value] -> value
    | [] -> raise (Session.Failure (Session.Protocol
        "missing JMAPACCESS response"))
    | _ -> raise (Session.Failure (Session.Protocol
        "duplicate JMAPACCESS response")))

let require_capability session name =
  if not (has session name) then
    raise (Session.Failure (Session.State (name ^ " unavailable")))

let command_syntax = function
  | Ok syntax -> syntax
  | Error message -> raise (Session.Failure (Session.State message))

let one_response name items = match items with
  | [item] -> item
  | [] -> raise (Session.Failure (Session.Protocol
      ("missing " ^ name ^ " response")))
  | _ -> raise (Session.Failure (Session.Protocol
      ("duplicate " ^ name ^ " response")))

let get_acl t ~mailbox =
  Session.locked t.session (fun () ->
    require_capability t.session "ACL";
    let mailbox = mailbox_wire t.session mailbox in
    let syntax=command_syntax (Imap.Command.getacl ~mailbox) in
    Session.command t.session syntax
    |> List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Acl x)
        when same_mailbox mailbox x.mailbox -> Some x
      | _ -> None)
    |> one_response "ACL")

let list_rights t ~mailbox ~identifier =
  Session.locked t.session (fun () ->
    require_capability t.session "ACL";
    let mailbox = mailbox_wire t.session mailbox in
    let syntax=command_syntax (Imap.Command.listrights ~mailbox ~identifier) in
    Session.command t.session syntax
    |> List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.List_rights x)
        when same_mailbox mailbox x.mailbox && x.identifier=identifier -> Some x
      | _ -> None)
    |> one_response "LISTRIGHTS")

let my_rights t ~mailbox =
  Session.locked t.session (fun () ->
    require_capability t.session "ACL";
    let mailbox = mailbox_wire t.session mailbox in
    let syntax=command_syntax (Imap.Command.myrights ~mailbox) in
    Session.command t.session syntax
    |> List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.My_rights x)
        when same_mailbox mailbox x.mailbox -> Some x
      | _ -> None)
    |> one_response "MYRIGHTS")

let set_acl t ~mailbox ~identifier ~operation ~rights =
  Session.locked t.session (fun () ->
    require_capability t.session "ACL";
    let mailbox=mailbox_wire t.session mailbox in
    let syntax=command_syntax (Imap.Command.setacl ~mailbox ~identifier
      ~operation ~rights) in
    ignore (Session.command ~mutation:true t.session syntax))

let delete_acl t ~mailbox ~identifier =
  Session.locked t.session (fun () ->
    require_capability t.session "ACL";
    let mailbox=mailbox_wire t.session mailbox in
    let syntax=command_syntax (Imap.Command.deleteacl ~mailbox ~identifier) in
    ignore (Session.command ~mutation:true t.session syntax))

let has_quota session = has session "QUOTA" ||
  List.exists (fun capability -> begins capability "QUOTA=RES-")
    session.Session.capabilities

let get_quota t ~root =
  Session.locked t.session (fun () ->
    if not (has_quota t.session) then
      raise (Session.Failure (Session.State "QUOTA unavailable"));
    let syntax=command_syntax (Imap.Command.getquota ~root) in
    Session.command t.session syntax
    |> List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Quota x)
        when x.root=root -> Some x
      | _ -> None)
    |> one_response "QUOTA")

let get_quota_root t ~mailbox =
  Session.locked t.session (fun () ->
    if not (has_quota t.session) then
      raise (Session.Failure (Session.State "QUOTA unavailable"));
    let mailbox=mailbox_wire t.session mailbox in
    let syntax=command_syntax (Imap.Command.getquotaroot ~mailbox) in
    let responses=Session.command t.session syntax in
    let mapping=responses |> List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Quota_root x)
        when same_mailbox mailbox x.mailbox -> Some x
      | _ -> None) |> one_response "QUOTAROOT" in
    let quotas=responses |> List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Quota x)
        when List.mem x.root mapping.roots -> Some x
      | _ -> None) in
    mapping,quotas)

let set_quota t ~root ~limits =
  Session.locked t.session (fun () ->
    require_capability t.session "QUOTASET";
    let syntax=command_syntax (Imap.Command.setquota ~root ~limits) in
    let responses=Session.command ~mutation:true t.session syntax in
    match List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Quota x)
        when x.root=root -> Some x
      | _ -> None) responses with
    | [] -> None
    | [quota] -> Some quota
    | _ -> raise (Session.Failure (Session.Protocol
        "duplicate SETQUOTA response")))

let metadata_capability session mailbox =
  if mailbox="" then (
    if not (has session "METADATA" || has session "METADATA-SERVER") then
      raise (Session.Failure (Session.State "server METADATA unavailable")))
  else require_capability session "METADATA"

type metadata_result = {
  responses : Imap.Response.metadata list;
  longentries : int64 option;
}

let get_metadata t ~mailbox ~entries ?maxsize ?depth () =
  Session.locked t.session (fun () ->
    metadata_capability t.session mailbox;
    let mailbox=mailbox_wire t.session mailbox in
    let syntax=command_syntax (Imap.Command.getmetadata ~mailbox ~entries
      ?maxsize ?depth ()) in
    let result=Session.command_result t.session syntax in
    let responses=result.untagged |> List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Metadata x)
        when same_mailbox mailbox x.mailbox -> Some x
      | _ -> None) in
    let longentries=match result.completion with
      | Imap.Response.Tagged {
          code=Some (Imap.Response.Metadata_longentries n);_} -> Some n
      | _ -> None in
    {responses;longentries})

let set_metadata t ~mailbox ~values =
  Session.locked t.session (fun () ->
    metadata_capability t.session mailbox;
    let mailbox=mailbox_wire t.session mailbox in
    let syntax=command_syntax (Imap.Command.setmetadata ~mailbox ~values) in
    ignore (Session.command ~mutation:true t.session syntax))

let notify_set t ?(status=false) ~groups () =
  Session.locked t.session (fun () ->
    require_capability t.session "NOTIFY";
    if List.exists (function
      | (Imap.Command.Selected|Imap.Command.Selected_delayed),_ -> true
      | _ -> false) groups then
      raise (Session.Failure (Session.State
        "selected NOTIFY filters require a selected-session API"));
    let syntax=command_syntax (Imap.Command.notify_set ~status ~groups ()) in
    let responses=Session.command ~mutation:true t.session syntax in
    if List.exists (function
      | Imap.Response.Untagged (Imap.Response.Ok
          (Some Imap.Response.Notificationoverflow,_)) -> true
      | _ -> false) responses then
      raise (Session.Failure (Session.Limit
        "server cancelled NOTIFY after notification overflow"));
    responses |> List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Status x) -> Some x
      | _ -> None))

let notify_none t =
  Session.locked t.session (fun () ->
    require_capability t.session "NOTIFY";
    ignore (Session.command ~mutation:true t.session Imap.Command.notify_none))

let create_mailbox t mailbox =
  Session.locked t.session (fun () ->
    let mailbox = mailbox_wire t.session mailbox in
    let syntax = match Imap.Command.create mailbox with
    | Ok s -> s | Error e -> raise (Session.Failure (Session.State e)) in
    ignore (Session.command ~mutation:true t.session syntax))

let objectid_mutation_receipt t syntax =
  if not (List.mem "OBJECTID+" t.session.Session.enabled) then
    raise (Session.Failure (Session.State "OBJECTID+ not enabled"));
  let result=Session.command_result ~mutation:true t.session syntax in
  match result.completion with
  | Imap.Response.Tagged {status=`Ok;
      code=Some (Imap.Response.Objectid ids);_}
    when ids.account_id<>None && ids.mailbox_id<>None -> ids
  | _ ->
      Session.close t.session;
      raise (Session.Failure (Session.Uncertain
        "OBJECTID+ mutation completed without account/mailbox identity"))

let create_mailbox_objectid t mailbox =
  Session.locked t.session (fun () ->
    let mailbox=mailbox_wire t.session mailbox in
    let syntax=command_syntax (Imap.Command.create mailbox) in
    objectid_mutation_receipt t syntax)

let delete_mailbox t mailbox =
  Session.locked t.session (fun () ->
    let mailbox = mailbox_wire t.session mailbox in
    let syntax = match Imap.Command.delete mailbox with
    | Ok s -> s | Error e -> raise (Session.Failure (Session.State e)) in
    ignore (Session.command ~mutation:true t.session syntax))

let rename_mailbox t ~old_name ~new_name =
  Session.locked t.session (fun () ->
    let old_name=mailbox_wire t.session old_name in
    let new_name=mailbox_wire t.session new_name in
    let syntax=command_syntax (Imap.Command.rename ~old_name ~new_name) in
    ignore (Session.command ~mutation:true t.session syntax))

let rename_mailbox_objectid t ~old_name ~new_name =
  Session.locked t.session (fun () ->
    let old_name=mailbox_wire t.session old_name in
    let new_name=mailbox_wire t.session new_name in
    let syntax=command_syntax (Imap.Command.rename ~old_name ~new_name) in
    objectid_mutation_receipt t syntax)

let subscribe_mailbox t mailbox =
  Session.locked t.session (fun () ->
    let mailbox=mailbox_wire t.session mailbox in
    let syntax=command_syntax (Imap.Command.subscribe mailbox) in
    ignore (Session.command ~mutation:true t.session syntax))

let unsubscribe_mailbox t mailbox =
  Session.locked t.session (fun () ->
    let mailbox=mailbox_wire t.session mailbox in
    let syntax=command_syntax (Imap.Command.unsubscribe mailbox) in
    ignore (Session.command ~mutation:true t.session syntax))

let with_mailbox t ?qresync ?objectid ~mode mailbox callback =
  Eio.Mutex.use_ro t.session.Session.mutex (fun () ->
    let result = Session.protect t.session (fun () ->
      let mailbox_wire = mailbox_wire t.session mailbox in
      let pinned=List.find_opt (fun (name,_) -> same_mailbox name mailbox)
        t.objectid_pins |> Option.map snd in
      let objectid=match objectid,pinned with
        | None,pinned -> pinned
        | Some requested,Some pinned when requested<>pinned ->
            raise (Session.Failure (Session.State
              "OBJECTID+ selection differs from pinned mailbox identity"))
        | Some requested,_ -> Some requested in
      if Option.is_some qresync &&
         not (List.mem "QRESYNC" t.session.Session.enabled) then
        raise (Session.Failure (Session.State "QRESYNC not enabled"));
      if Option.is_some objectid &&
         not (List.mem "OBJECTID+" t.session.Session.enabled) then
        raise (Session.Failure (Session.State "OBJECTID+ not enabled"));
      let condstore = Option.is_none qresync &&
        (has t.session "CONDSTORE" || has t.session "QRESYNC") in
      let syntax = match Imap.Command.select
        ~readonly:(mode = `Read_only) ~condstore ?qresync ?objectid
        mailbox_wire with
      | Ok s -> s | Error e -> raise (Session.Failure (Session.State e)) in
      t.session.Session.generation <- t.session.Session.generation + 1;
      t.session.Session.selected <- None;
      let selected_result = Session.command_result t.session syntax in
      let info = match Imap.Response.select_metadata
        (selected_result.untagged @ [selected_result.completion]) with
      | Ok info -> info
      | Error message ->
          Session.close t.session;
          raise (Session.Failure (Session.Protocol
            ("invalid SELECT metadata: " ^ message))) in
      if info.uidnotsticky then (
        Session.close t.session;
        raise (Session.Failure (Session.State
          "UIDNOTSTICKY mailbox cannot be mirrored with persistent UIDs")));
      (match objectid with
       | None -> ()
       | Some (account_id,mailbox_id) ->
           (match info.objectid with
            | Some ids when ids.account_id=Some account_id &&
                ids.mailbox_id=Some mailbox_id -> ()
            | _ ->
                Session.close t.session;
                raise (Session.Failure (Session.State
                  "OBJECTID+ SELECT fell back to another mailbox"))));
      t.session.Session.selected <- Some mailbox;
      t.session.Session.readonly <- mode = `Read_only || info.readonly = Some true;
      let selected = Selected.create t.session t.session.Session.generation
        info selected_result.untagged in
      let outcome =
        try callback selected with ex ->
          Selected.invalidate selected;
          Session.close t.session;
          raise ex in
      Selected.invalidate selected;
      if not t.session.Session.closed then (
        if has t.session "UNSELECT" ||
           (has t.session "IMAP4REV2" && not (has t.session "IMAP4REV1")) ||
           List.mem "IMAP4REV2" t.session.Session.enabled then
          (try ignore (Session.command t.session "UNSELECT") with ex ->
             Session.close t.session;
             raise ex)
        else Session.close t.session;
        t.session.Session.selected <- None;
        t.session.Session.generation <- t.session.Session.generation + 1);
      outcome)
    in match result with Ok result -> result | Error e -> Error e)

type append_receipt = {
  uidvalidity : Imap.Proto.Uidvalidity.t;
  uid : Imap.Proto.Uid.t;
}

let check_append_destination t ~mailbox =
  (match List.find_opt (fun (name,_) -> same_mailbox name mailbox)
       t.objectid_pins with
     | None -> ()
     | Some (_, (account_id,mailbox_id)) ->
         let status=status_locked t ~mailbox
           ~items:[Imap.Command.Objectid] in
         (match status.objectid with
          | Some ids when ids.account_id=Some account_id &&
              ids.mailbox_id=Some mailbox_id -> ()
          | _ -> raise (Session.Failure (Session.State
              "APPEND destination differs from pinned OBJECTID+ identity"))))

let non_sync_literal session length =
  length<=4096L && (has session "LITERAL-" || has session "LITERAL+" ||
    revision_two session)

let append_receipt ~binary t ~mailbox ?flags ?internal_date ~length source =
  Session.locked t.session (fun () ->
    if binary && not (has t.session "BINARY") then
      raise (Session.Failure (Session.State "binary APPEND requires BINARY capability"));
    check_append_destination t ~mailbox;
    let mailbox = mailbox_wire t.session mailbox in
    let command=if binary then Imap.Command.append_binary_prefix
      else Imap.Command.append_prefix in
    let non_sync=non_sync_literal t.session length in
    let prefix = match command ~mailbox ~non_sync ?flags
      ?internal_date ~size:length () with
    | Ok s -> s | Error e -> raise (Session.Failure (Session.State e)) in
    let completion = Session.append ~synchronizing:(not non_sync) t.session ~prefix ~length source in
    match completion with
    | Imap.Response.Tagged {
        code=Some (Imap.Response.Appenduid (v,u)); _} ->
        let require result = match result with
          | Ok value -> value
          | Error message ->
              raise (Session.Failure (Session.Protocol message)) in
        Some {
          uidvalidity=require (Imap.Proto.Uidvalidity.of_int64 v);
          uid=require (Imap.Proto.Uid.of_int64 u)
        }
    | Imap.Response.Tagged {code=Some (Imap.Response.Appenduid_set _);_} ->
        Session.close t.session;
        raise (Session.Failure (Session.Uncertain
          "single-message APPEND returned multiple destination UIDs"))
    | _ -> None)

let append_flow_receipt t ~mailbox ?flags ?internal_date ~length source =
  append_receipt ~binary:false t ~mailbox ?flags ?internal_date ~length source

let append_binary_flow_receipt t ~mailbox ?flags ?internal_date ~length source =
  append_receipt ~binary:true t ~mailbox ?flags ?internal_date ~length source

let append_flow t ~mailbox ?flags ?internal_date ~length source =
  match append_flow_receipt t ~mailbox ?flags ?internal_date
    ~length source with
  | Ok _ -> Ok ()
  | Error _ as e -> e

let append_binary_flow t ~mailbox ?flags ?internal_date ~length source =
  Result.map (fun _ -> ())
    (append_binary_flow_receipt t ~mailbox ?flags ?internal_date ~length source)


type append_message = {
  flags : string list;
  internal_date : Imap.Internal_date.t option;
  length : int64;
  read : Cstruct.t -> int;
}

let append_message ?(flags=[]) ?internal_date ~length source =
  {flags;internal_date;length;read=Eio.Flow.single_read source}

type multiappend_receipt = {
  uidvalidity : Imap.Proto.Uidvalidity.t;
  uids : Imap.Proto.Uid.t list;
}

let append_messages t ~mailbox messages =
  Session.locked t.session (fun () ->
    let count=List.length messages in
    let state message=raise (Session.Failure (Session.State message)) in
    if count=0 || count>1000 then state "MULTIAPPEND requires 1..1000 messages";
    if count>1 && not (has t.session "MULTIAPPEND") then
      state "MULTIAPPEND capability unavailable";
    List.iter (fun capability ->
      let capability=String.uppercase_ascii capability in
      List.iter (fun prefix ->
        if String.starts_with ~prefix capability then (
          let raw=String.sub capability (String.length prefix)
            (String.length capability-String.length prefix) in
          match Int64.of_string_opt raw with
          | Some limit when limit>0L && limit<=4_294_967_295L &&
              String.for_all (function '0'..'9' -> true | _ -> false) raw ->
              if Int64.of_int count>limit then
                raise (Session.Failure (Session.Limit
                  "APPEND batch exceeds advertised message limit"))
          | _ -> raise (Session.Failure (Session.Protocol
              "invalid advertised APPEND message limit"))))
        ["MESSAGELIMIT=";"SAVELIMIT="]) t.session.Session.capabilities;
    let destination=mailbox in
    let mailbox=mailbox_wire t.session mailbox in
    let bytes=ref 0 in
    let parts=List.mapi (fun index message ->
      if message.length<=0L then state "MULTIAPPEND message must be nonempty";
      let non_sync=non_sync_literal t.session message.length in
      let result=if index=0 then Imap.Command.append_prefix ~mailbox ~non_sync
          ~flags:message.flags ?internal_date:message.internal_date ~size:message.length ()
        else Imap.Command.append_part_prefix ~non_sync ~flags:message.flags
          ?internal_date:message.internal_date ~size:message.length () in
      let prefix=match result with Ok prefix -> prefix | Error message -> state message in
      if String.length prefix>65000 then state "MULTIAPPEND argument exceeds syntax limit";
      bytes:= !bytes+String.length prefix;
      if !bytes>1_048_576 then state "MULTIAPPEND syntax exceeds 1 MiB";
      {Session.prefix;length=message.length;read=message.read;synchronizing=not non_sync}) messages in
    check_append_destination t ~mailbox:destination;
    let completion=Session.append_many t.session parts in
    let invalid () =
      Session.close t.session;
      raise (Session.Failure (Session.Uncertain "MULTIAPPEND returned invalid UID correspondence")) in
    let receipt epoch wire =
      let epoch=match Imap.Proto.Uidvalidity.of_int64 epoch with
        | Ok epoch -> epoch | Error _ -> invalid () in
      let seen=Hashtbl.create count and result=ref [] and total=ref 0 in
      let number raw=match Int64.of_string_opt raw with
        | Some n when n>0L && n<=4_294_967_295L -> n | _ -> invalid () in
      List.iter (fun item ->
        let first,last=match String.split_on_char ':' item with
          | [n] -> let n=number n in n,n
          | [a;b] -> let a=number a and b=number b in min a b,max a b
          | _ -> invalid () in
        let length=Int64.succ (Int64.sub last first) in
        if length>Int64.of_int (count- !total) then invalid ();
        let rec add n =
          if Hashtbl.mem seen n then invalid ();
          Hashtbl.add seen n ();
          let uid=match Imap.Proto.Uid.of_int64 n with Ok uid -> uid | Error _ -> invalid () in
          result:=uid :: !result; incr total;
          if n<last then add (Int64.succ n) in
        add first) (String.split_on_char ',' wire);
      if !total<>count then invalid ();
      Some {uidvalidity=epoch;uids=List.rev !result} in
    match completion with
    | Imap.Response.Tagged {code=Some (Imap.Response.Appenduid (epoch,uid));_} ->
        receipt epoch (Int64.to_string uid)
    | Imap.Response.Tagged {code=Some (Imap.Response.Appenduid_set (epoch,wire));_} ->
        if count=1 then invalid ();
        receipt epoch wire
    | _ -> None)
