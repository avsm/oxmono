type t = {
  session : Session.t;
  mutable objectid_pins : (string * (string * string)) list;
  release : Eio.Switch.hook;
}
type error = Error.t

let pp_error = Error.pp
let error_to_string = Error.to_string

module Cap = Imap.Capability

let upper = String.uppercase_ascii
let canonical name = if upper name = "INBOX" then "INBOX" else name
let same_mailbox expected received = canonical expected = canonical received
let mailbox_wire = Session.mailbox_wire
let fail error = raise (Session.Failure error)

let syntax = function
  | Ok syntax -> syntax
  | Error e ->
      raise (Session.Failure (Session.State (Imap.Command.to_string e)))

let one_response name items = match items with
  | [item] -> item
  | [] -> raise (Session.Failure (Session.Protocol
      ("missing " ^ name ^ " response")))
  | _ -> raise (Session.Failure (Session.Protocol
      ("duplicate " ^ name ^ " response")))

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
  session.Session.capabilities <- Cap.Set.of_list (List.concat_map (function
    | Imap.Response.Untagged (Imap.Response.Capability caps) -> caps
    | _ -> []) responses)

(* RFC 9051 makes ENABLE a base command, so advertising IMAP4rev2 suffices
   before IMAP4rev2 itself is enabled. *)
let can_enable session =
  Session.has session Cap.Enable || Session.has session Cap.Imap4rev2

let enable_session session = function
  | [] -> []
  | capabilities ->
      if not (can_enable session) then fail (Session.Unsupported Cap.Enable);
      List.iter (Session.require session) capabilities;
      if Option.is_some session.Session.selected then
        fail (Session.State "ENABLE is invalid while a mailbox is selected");
      match Cap.Set.(to_list (of_list (List.filter
          (fun c -> not (Session.is_enabled session c)) capabilities))) with
      | [] -> []
      | pending ->
          let accepted = Session.command session
              (syntax (Imap.Command.enable pending))
            |> List.concat_map (function
              | Imap.Response.Untagged (Imap.Response.Enabled caps) -> caps
              | _ -> []) in
          session.Session.enabled <-
            Cap.Set.union (Cap.Set.of_list accepted) session.Session.enabled;
          accepted

let enable_optional session capability =
  if can_enable session then
    try ignore (enable_session session [capability]) with
    | Session.Failure (Session.Rejected _) -> ()

let login session auth =
  let password = Auth.resolve_password auth in
  ignore (Session.command session
    (syntax (Imap.Command.login ~username:(Auth.username auth) ~password)))

let authentication_rejected ~tag ~status ~code =
  let code=match code with
    | Some (Imap.Response.Unavailable | Authenticationfailed
        | Authorizationfailed | Expired | Privacyrequired | Contactadmin
        | Noperm | Inuse | Serverbug | Clientbug | Cannot | Limit as code) ->
        Some code
    | _ -> None in
  Session.Failure (Session.Rejected {tag;status;code;
    text="authentication rejected"})

let authenticate session auth ~secure =
  let mechanism = match Auth.mechanism auth with
  | `Auto ->
      if secure && Session.has session (Cap.Auth "PLAIN") then `Plain
      else if Session.has session (Cap.Auth "CRAM-MD5") then `Cram_md5
      else `Login
  | (`Login | `Cram_md5 | `Plain | `Oauthbearer) as mechanism -> mechanism in
  (match mechanism with
   | `Login ->
       if not secure && not (Auth.allow_insecure_transport auth) then
         raise (Session.Failure (Session.State "LOGIN requires TLS"));
       if Session.has session Cap.Login_disabled then
         raise (Session.Failure (Session.State "LOGIN disabled by server"))
   | `Cram_md5 -> Session.require session (Cap.Auth "CRAM-MD5")
   | `Plain | `Oauthbearer ->
       let name=if mechanism=`Plain then "PLAIN" else "OAUTHBEARER" in
       Session.require session (Cap.Auth name);
       if not secure && not (Auth.allow_insecure_transport auth) then
         fail (Session.State
           (Cap.to_wire (Cap.Auth name) ^ " requires TLS")));
  try match mechanism with
  | `Login -> login session auth
  | `Cram_md5 -> Session.authenticate_cram_md5 session auth
  | `Plain ->
      Session.authenticate_initial session ~mechanism:"PLAIN"
        ~encoded:(Auth.plain_response auth)
        ~sasl_ir:(Session.has session Cap.Sasl_ir) ~oauthbearer:false
  | `Oauthbearer ->
      Session.authenticate_initial session ~mechanism:"OAUTHBEARER"
        ~encoded:(Auth.oauthbearer_response auth)
        ~sasl_ir:(Session.has session Cap.Sasl_ir) ~oauthbearer:true
  with
  | Eio.Cancel.Cancelled _ as ex -> raise ex
  | Session.Failure (Session.Rejected {tag; status; code; _}) ->
      raise (authentication_rejected ~tag ~status ~code)
  | Session.Failure Session.Closed -> raise (Session.Failure Session.Closed)
  | Session.Failure (Session.Limit _) ->
      raise (Session.Failure (Session.Limit "authentication exchange exceeded limits"))
  | Session.Failure (Session.Protocol _) ->
      raise (Session.Failure (Session.Protocol "invalid authentication exchange"))
  | Session.Failure (Session.State _) | Auth.Invalid_credentials ->
      raise (Session.Failure (Session.State "invalid credentials"))
  | ex when Session.io_failure ex ->
      raise (Session.Failure (Session.Transport
        "authentication exchange failed"))

let start ~sw ?auth ?endpoint flow =
  let session = Session.create flow in
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
             Session.require session Cap.Starttls;
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
      | Some auth -> authenticate session auth ~secure; capability session);
    if Session.has session Cap.Imap4rev2 && Session.has session Cap.Imap4rev1
    then enable_optional session Cap.Imap4rev2;
    if not (Session.revision_two session) &&
       Session.has session (Cap.Utf8 `Accept) then
      enable_optional session (Cap.Utf8 `Accept);
    if Session.has session Cap.Qresync then enable_optional session Cap.Qresync;
    let release = Eio.Switch.on_release_cancellable sw (fun () ->
      Session.close session) in
    Ok {session; objectid_pins=[]; release}
  with
  | Session.Failure e -> Session.close session; Error e
  | ex ->
      let bt = Printexc.get_raw_backtrace () in
      Session.close session;
      if Session.io_failure ex then
        Error (Session.Transport (Printexc.to_string ex))
      else Printexc.raise_with_backtrace ex bt

let connect ~sw ?auth transport =
  match Transport.connect ~sw transport with
  | flow -> start ~sw ?auth ~endpoint:transport flow
  | exception (Eio.Cancel.Cancelled _ as ex) -> raise ex
  | exception ex -> Error (Session.Transport (Printexc.to_string ex))

let of_flow ~sw ?auth flow = start ~sw ?auth (Transport.of_flow flow)

let capabilities t = t.session.Session.capabilities
let enabled t = t.session.Session.enabled
let has t capability = Session.has t.session capability
let is_enabled t capability = Session.is_enabled t.session capability
let mailbox_mode t = Session.mailbox_mode t.session
let noop t = Session.locked t.session (fun () ->
  Session.command t.session Imap.Command.noop)
let logout t = Session.locked t.session (fun () -> Session.logout t.session)

let close t =
  Eio.Switch.remove_hook t.release;
  Session.close t.session
let is_open t = not t.session.Session.closed

let compress_deflate t =
  Session.locked t.session (fun () -> Session.compress_deflate t.session)

let enable t capabilities =
  Session.locked t.session (fun () -> enable_session t.session capabilities)

let enable_mode t capability =
  Session.locked t.session (fun () ->
    ignore (enable_session t.session [capability]);
    if not (Session.is_enabled t.session capability) then
      let name = Cap.to_wire capability in
      fail (Session.Protocol
        (name ^ " ENABLE completed without ENABLED " ^ name)))

let enable_uidonly t = enable_mode t Cap.Uidonly
let enable_objectid_plus t = enable_mode t Cap.Objectid_plus

let pinned t mailbox =
  List.find_opt (fun (name,_) -> same_mailbox name mailbox) t.objectid_pins
  |> Option.map snd

let pin_mailbox_objectid t ~mailbox ~account_id ~mailbox_id =
  Session.locked t.session (fun () ->
    Session.require_enabled t.session Cap.Objectid_plus;
    if Option.is_some t.session.Session.selected then
      raise (Session.Failure (Session.State
        "cannot pin OBJECTID+ during a selected lease"));
    let identity=(account_id,mailbox_id) in
    ignore (syntax (Imap.Command.select ~objectid:identity mailbox));
    match pinned t mailbox with
    | None -> t.objectid_pins <- (mailbox,identity)::t.objectid_pins
    | Some existing when existing=identity -> ()
    | Some _ -> raise (Session.Failure (Session.State
        "OBJECTID+ mailbox pin changed on one connection")))

let list t ?(reference="") ~pattern () =
  Session.locked t.session (fun () ->
    let reference = mailbox_wire t.session reference in
    let pattern = mailbox_wire t.session pattern in
    Session.command t.session (syntax (Imap.Command.list ~reference ~pattern))
    |> List.filter_map (function
        | Imap.Response.Untagged (Imap.Response.List item) -> Some item
        | _ -> None))

let lsub t ?(reference="") ~pattern () =
  Session.locked t.session (fun () ->
    let reference=mailbox_wire t.session reference in
    let pattern=mailbox_wire t.session pattern in
    Session.command t.session (syntax (Imap.Command.lsub ~reference ~pattern))
    |> List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.List item)
          when item.subscribed -> Some item
      | _ -> None))

let namespace t =
  Session.locked t.session (fun () ->
    Session.require t.session Cap.Namespace;
    Session.command t.session Imap.Command.namespace
    |> List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Namespace item) -> Some item
      | _ -> None)
    |> one_response "NAMESPACE")

type discovery = {
  mailboxes:(Imap.Response.list_result * Imap.Response.mailbox_status option) list;
  unpaired_status:Imap.Response.mailbox_status list
}

let has_quota session = Session.has session Cap.Quota ||
  Cap.quota_resources session.Session.capabilities <> []

let require_quota session =
  if not (has_quota session) then fail (Session.Unsupported Cap.Quota)

(* RFC 7162 ties HIGHESTMODSEQ to CONDSTORE, RFC 8474 ties MAILBOXID to
   OBJECTID, RFC 8438 and RFC 9051 define SIZE, RFC 9051 defines DELETED and
   RFC 9208 defines both deleted items. *)
let check_status_items session items =
  let require item available capability =
    if List.exists (Imap.Status_item.equal item) items && not available then
      fail (Session.Unsupported capability) in
  let has = Session.has session in
  require Highestmodseq (has Cap.Condstore || has Cap.Qresync)
    Cap.Condstore;
  require Mailboxid (has Cap.Objectid) Cap.Objectid;
  require Size (has Cap.Status_size) Cap.Status_size;
  require Deleted
    (has_quota session || Session.revision_two session) Cap.Quota;
  require Deleted_storage (has_quota session) Cap.Quota

let requests_objectid items =
  List.exists (Imap.Status_item.equal Objectid) items

let list_extended t ?(reference="") ~patterns ?(selection=[])
    ?(returns=[]) ?status () =
  Session.locked t.session (fun () ->
    let extended=selection<>[] || returns<>[] || status<>None ||
      List.length patterns<>1 in
    if extended then Session.require t.session Cap.List_extended;
    if List.exists (Imap.Mailbox_list.equal_selection Special_use)
         selection ||
       List.exists (Imap.Mailbox_list.equal_return Special_use) returns then
      Session.require t.session Cap.Special_use;
    if status<>None then Session.require t.session Cap.List_status;
    Option.iter (check_status_items t.session) status;
    if Option.fold ~none:false ~some:requests_objectid status then
      Session.require_enabled t.session Cap.Objectid_plus;
    let reference=mailbox_wire t.session reference in
    let patterns=List.map (mailbox_wire t.session) patterns in
    let responses=Session.command t.session
      (syntax (Imap.Command.list_extended ~reference ~patterns
        ~selection ~returns ?status ())) in
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
    check_status_items t.session items;
    if requests_objectid items then
      Session.require_enabled t.session Cap.Objectid_plus;
    let mailbox = mailbox_wire t.session mailbox in
    Session.command t.session (syntax (Imap.Command.status ~mailbox ~items))
    |> List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Status status)
        when same_mailbox mailbox status.mailbox -> Some status
      | _ -> None)
    |> one_response "STATUS"

let status t ~mailbox ~items =
  Session.locked t.session (fun () -> status_locked t ~mailbox ~items)

let get_jmap_access t =
  Session.locked t.session (fun () ->
    Session.require t.session Cap.Jmapaccess;
    Session.command t.session Imap.Command.get_jmap_access
    |> List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Jmapaccess value) -> Some value
      | _ -> None)
    |> one_response "JMAPACCESS")

let get_acl t ~mailbox =
  Session.locked t.session (fun () ->
    Session.require t.session Cap.Acl;
    let mailbox = mailbox_wire t.session mailbox in
    Session.command t.session (syntax (Imap.Command.getacl ~mailbox))
    |> List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Acl x)
        when same_mailbox mailbox x.mailbox -> Some x
      | _ -> None)
    |> one_response "ACL")

let list_rights t ~mailbox ~identifier =
  Session.locked t.session (fun () ->
    Session.require t.session Cap.Acl;
    let mailbox = mailbox_wire t.session mailbox in
    Session.command t.session
      (syntax (Imap.Command.listrights ~mailbox ~identifier))
    |> List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.List_rights x)
        when same_mailbox mailbox x.mailbox && x.identifier=identifier -> Some x
      | _ -> None)
    |> one_response "LISTRIGHTS")

let my_rights t ~mailbox =
  Session.locked t.session (fun () ->
    Session.require t.session Cap.Acl;
    let mailbox = mailbox_wire t.session mailbox in
    Session.command t.session (syntax (Imap.Command.myrights ~mailbox))
    |> List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.My_rights x)
        when same_mailbox mailbox x.mailbox -> Some x
      | _ -> None)
    |> one_response "MYRIGHTS")

let set_acl t ~mailbox ~identifier ~operation ~rights =
  Session.locked t.session (fun () ->
    Session.require t.session Cap.Acl;
    let mailbox=mailbox_wire t.session mailbox in
    ignore (Session.command ~mutation:true t.session
      (syntax (Imap.Command.setacl ~mailbox ~identifier ~operation ~rights))))

let delete_acl t ~mailbox ~identifier =
  Session.locked t.session (fun () ->
    Session.require t.session Cap.Acl;
    let mailbox=mailbox_wire t.session mailbox in
    ignore (Session.command ~mutation:true t.session
      (syntax (Imap.Command.deleteacl ~mailbox ~identifier))))

let get_quota t ~root =
  Session.locked t.session (fun () ->
    require_quota t.session;
    Session.command t.session (syntax (Imap.Command.getquota ~root))
    |> List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Quota x)
        when x.root=root -> Some x
      | _ -> None)
    |> one_response "QUOTA")

let get_quota_root t ~mailbox =
  Session.locked t.session (fun () ->
    require_quota t.session;
    let mailbox=mailbox_wire t.session mailbox in
    let responses=Session.command t.session
      (syntax (Imap.Command.getquotaroot ~mailbox)) in
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
    Session.require t.session Cap.Quotaset;
    let responses=Session.command ~mutation:true t.session
      (syntax (Imap.Command.setquota ~root ~limits)) in
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
    if not (Session.has session Cap.Metadata ||
            Session.has session Cap.Metadata_server) then
      fail (Session.Unsupported Cap.Metadata_server))
  else Session.require session Cap.Metadata

type metadata_result = {
  responses : Imap.Response.metadata list;
  longentries : int64 option;
}

let get_metadata t ~mailbox ~entries ?maxsize ?depth () =
  Session.locked t.session (fun () ->
    metadata_capability t.session mailbox;
    let mailbox=mailbox_wire t.session mailbox in
    let result=Session.command_result t.session
      (syntax (Imap.Command.getmetadata ~mailbox ~entries ?maxsize ?depth
        ())) in
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
    ignore (Session.command ~mutation:true t.session
      (syntax (Imap.Command.setmetadata ~mailbox ~values))))

let notify_set t ?(status=false) ~groups () =
  Session.locked t.session (fun () ->
    Session.require t.session Cap.Notify;
    if List.exists (fun (filter,_) -> Imap.Notify.is_selected filter) groups
    then
      raise (Session.Failure (Session.State
        "selected NOTIFY filters require a selected-session API"));
    let responses=Session.command ~mutation:true t.session
      (syntax (Imap.Command.notify_set ~status ~groups ())) in
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
    Session.require t.session Cap.Notify;
    ignore (Session.command ~mutation:true t.session Imap.Command.notify_none))

let mailbox_mutation t command =
  Session.locked t.session (fun () ->
    ignore (Session.command ~mutation:true t.session (syntax (command ()))))

let create_mailbox t ~mailbox =
  mailbox_mutation t (fun () ->
    Imap.Command.create ~mailbox:(mailbox_wire t.session mailbox))

let objectid_mutation_receipt t syntax =
  Session.require_enabled t.session Cap.Objectid_plus;
  let result=Session.command_result ~mutation:true t.session syntax in
  match result.completion with
  | Imap.Response.Tagged {status=`Ok;
      code=Some (Imap.Response.Objectid ids);_}
    when ids.account_id<>None && ids.mailbox_id<>None -> ids
  | _ ->
      Session.close t.session;
      raise (Session.Failure (Session.Uncertain
        "OBJECTID+ mutation completed without account/mailbox identity"))

let create_mailbox_objectid t ~mailbox =
  Session.locked t.session (fun () ->
    let mailbox=mailbox_wire t.session mailbox in
    objectid_mutation_receipt t (syntax (Imap.Command.create ~mailbox)))

let delete_mailbox t ~mailbox =
  mailbox_mutation t (fun () ->
    Imap.Command.delete ~mailbox:(mailbox_wire t.session mailbox))

let rename_mailbox t ~old_name ~new_name =
  mailbox_mutation t (fun () ->
    let old_name=mailbox_wire t.session old_name in
    let new_name=mailbox_wire t.session new_name in
    Imap.Command.rename ~old_name ~new_name)

let rename_mailbox_objectid t ~old_name ~new_name =
  Session.locked t.session (fun () ->
    let old_name=mailbox_wire t.session old_name in
    let new_name=mailbox_wire t.session new_name in
    objectid_mutation_receipt t
      (syntax (Imap.Command.rename ~old_name ~new_name)))

let subscribe_mailbox t ~mailbox =
  mailbox_mutation t (fun () ->
    Imap.Command.subscribe ~mailbox:(mailbox_wire t.session mailbox))

let unsubscribe_mailbox t ~mailbox =
  mailbox_mutation t (fun () ->
    Imap.Command.unsubscribe ~mailbox:(mailbox_wire t.session mailbox))

(* A failed UNSELECT leaves the mailbox selected on the server. Closing the
   connection is the only safe release, and the callback's outcome stands
   because every command it issued has completed. *)
let release_selection session =
  if Session.has session Cap.Unselect then
    match Session.command session "UNSELECT" with
    | _ -> ()
    | exception (Eio.Cancel.Cancelled _ as ex) -> raise ex
    | exception (Session.Failure _) -> Session.close session
    | exception ex when Session.io_failure ex -> Session.close session
  else Session.close session

let with_mailbox t ?qresync ?objectid ~mode mailbox callback =
  let session = t.session in
  Eio.Mutex.use_ro session.Session.mutex (fun () ->
    Session.protect session (fun () ->
      Fun.protect ~finally:(fun () -> session.Session.selected <- None)
      (fun () ->
      let mailbox_wire = mailbox_wire session mailbox in
      let objectid=match objectid,pinned t mailbox with
        | None,pinned -> pinned
        | Some requested,Some pinned when requested<>pinned ->
            raise (Session.Failure (Session.State
              "OBJECTID+ selection differs from pinned mailbox identity"))
        | Some requested,_ -> Some requested in
      if Option.is_some qresync then
        Session.require_enabled session Cap.Qresync;
      if Option.is_some objectid then
        Session.require_enabled session Cap.Objectid_plus;
      let condstore = Option.is_none qresync &&
        (Session.has session Cap.Condstore ||
         Session.has session Cap.Qresync) in
      let qresync = Option.map (fun (validity, modseq) ->
        Imap.Uidvalidity.to_int64 validity, Imap.Modseq.to_int64 modseq)
        qresync in
      let syntax = syntax (Imap.Command.select
        ~readonly:(mode = `Read_only) ~condstore ?qresync ?objectid
        mailbox_wire) in
      session.Session.generation <- session.Session.generation + 1;
      session.Session.selected <- None;
      let selected_result = Session.command_result session syntax in
      let info = match Imap.Response.select_metadata
        (selected_result.untagged @ [selected_result.completion]) with
      | Ok info -> info
      | Error message ->
          Session.close session;
          raise (Session.Failure (Session.Protocol
            ("invalid SELECT metadata: " ^ message))) in
      if info.uidnotsticky then (
        Session.close session;
        raise (Session.Failure (Session.State
          "UIDNOTSTICKY mailbox cannot be mirrored with persistent UIDs")));
      (match objectid with
       | None -> ()
       | Some (account_id,mailbox_id) ->
           (match info.objectid with
            | Some ids when ids.account_id=Some account_id &&
                ids.mailbox_id=Some mailbox_id -> ()
            | _ ->
                Session.close session;
                raise (Session.Failure (Session.State
                  "OBJECTID+ SELECT fell back to another mailbox"))));
      session.Session.selected <- Some mailbox;
      session.Session.readonly <-
        mode = `Read_only || info.readonly = Some true;
      let selected = Selected.create session session.Session.generation
        info selected_result.untagged in
      let outcome =
        try callback selected with ex ->
          let bt = Printexc.get_raw_backtrace () in
          Selected.invalidate selected;
          Session.close session;
          Printexc.raise_with_backtrace ex bt in
      Selected.invalidate selected;
      if not session.Session.closed then (
        release_selection session;
        session.Session.generation <- session.Session.generation + 1);
      outcome))
    |> Result.join)

type append_receipt = {
  uidvalidity : Imap.Uidvalidity.t;
  uid : Imap.Uid.t;
}

let check_append_destination t ~mailbox =
  match pinned t mailbox with
  | None -> ()
  | Some (account_id,mailbox_id) ->
      let status=status_locked t ~mailbox ~items:[Imap.Status_item.Objectid] in
      (match status.objectid with
       | Some ids when ids.account_id=Some account_id &&
           ids.mailbox_id=Some mailbox_id -> ()
       | _ -> raise (Session.Failure (Session.State
           "APPEND destination differs from pinned OBJECTID+ identity")))

let non_sync_literal session length =
  length<=4096L && (Session.has session Cap.Literal_minus ||
    Session.has session Cap.Literal_plus)

(* [Imap.Response] bounds APPENDUID values, so a failed conversion means the
   response contract changed. *)
let proto_value = function
  | Ok value -> value
  | Error message -> raise (Session.Failure (Session.Protocol message))

let append_receipt ~binary t ~mailbox ?flags ?internal_date ~length source =
  Session.locked t.session (fun () ->
    if binary then Session.require t.session Cap.Binary;
    let destination = mailbox in
    let mailbox = mailbox_wire t.session mailbox in
    let command=if binary then Imap.Command.append_binary_prefix
      else Imap.Command.append_prefix in
    let non_sync=non_sync_literal t.session length in
    let prefix = syntax (command ~mailbox ~non_sync ?flags
      ?internal_date ~size:length ()) in
    check_append_destination t ~mailbox:destination;
    let completion = Session.append ~synchronizing:(not non_sync) t.session ~prefix ~length source in
    match completion with
    | Imap.Response.Tagged {
        code=Some (Imap.Response.Appenduid (v,u)); _} ->
        Some {
          uidvalidity=proto_value (Imap.Uidvalidity.of_int64 v);
          uid=proto_value (Imap.Uid.of_int64 u)
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
  Result.map ignore
    (append_flow_receipt t ~mailbox ?flags ?internal_date ~length source)

let append_binary_flow t ~mailbox ?flags ?internal_date ~length source =
  Result.map ignore
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
  uidvalidity : Imap.Uidvalidity.t;
  uids : Imap.Uid.t list;
}

let append_messages t ~mailbox messages =
  Session.locked t.session (fun () ->
    let count=List.length messages in
    let state message=raise (Session.Failure (Session.State message)) in
    if count=0 || count>1000 then state "MULTIAPPEND requires 1..1000 messages";
    if count>1 then Session.require t.session Cap.Multiappend;
    let capabilities=t.session.Session.capabilities in
    if List.exists Cap.malformed_limit (Cap.Set.to_list capabilities) then
      fail (Session.Protocol "invalid advertised APPEND message limit");
    List.iter (function
      | Some limit when Int64.of_int count>limit ->
          fail (Session.Limit "APPEND batch exceeds advertised message limit")
      | _ -> ())
      [Cap.messagelimit capabilities; Cap.savelimit capabilities];
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
      let prefix=syntax result in
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
      let epoch=match Imap.Uidvalidity.of_int64 epoch with
        | Ok epoch -> epoch | Error _ -> invalid () in
      let seen=Hashtbl.create count and result=ref [] and total=ref 0 in
      let number raw=match Int64.of_string_opt raw with
        | Some n -> n | None -> invalid () in
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
          let uid=match Imap.Uid.of_int64 n with Ok uid -> uid | Error _ -> invalid () in
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
