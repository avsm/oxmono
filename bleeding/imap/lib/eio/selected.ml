module Cap = Imap.Capability

type t = {
  session : Session.t;
  generation : int;
  info : Imap.Response.select_metadata;
  select_updates : Imap.Response.t list;
  mutable active : bool;
  commands : Eio.Mutex.t;
  mutable running : bool;
}

let create session generation info select_updates =
  {session; generation; info; select_updates; active=true;
   commands=Eio.Mutex.create (); running=false}
let invalidate t =
  t.active <- false;
  (* An escaped fiber must not race lease cleanup or another selection. *)
  if t.running then Session.close t.session

let check t =
  if not t.active || t.session.Session.generation <> t.generation then
    raise (Session.Failure (Session.State "stale selected mailbox lease"));
  Session.check_open t.session

let run t f =
  Eio.Mutex.use_ro t.commands (fun () ->
    Session.protect t.session (fun () ->
      check t;
      t.running <- true;
      Fun.protect ~finally:(fun () -> t.running <- false) (fun () ->
        let result=f () in
        check t;
        result)))
let noop t = run t (fun () -> Session.command t.session Imap.Command.noop)

let info t = run t (fun () -> t.info)
let select_updates t = run t (fun () -> t.select_updates)

let has t capability = Session.has t.session capability
let require t capability = Session.require t.session capability

(* RFC 9051 Appendix E item 2 folds only the FETCH side of BINARY into
   IMAP4rev2, so BINARY APPEND still needs the capability. *)
let require_binary_fetch t =
  if not (has t Cap.Binary || Session.revision_two t.session) then
    raise (Session.Failure (Session.Unsupported Cap.Binary))

let syntax = function
  | Ok syntax -> syntax
  | Error e ->
      raise (Session.Failure (Session.State (Imap.Command.to_string e)))

let protocol message = raise (Session.Failure (Session.Protocol message))

let received_uid n =
  match Imap.Uid.of_int64 n with
  | Ok uid -> uid
  | Error message -> protocol message

let mem_uid uid uids = List.exists (Imap.Uid.equal uid) uids
let wire_uids uids = String.concat "," (List.map Imap.Uid.to_string uids)

let valid_window ~first ~last =
  let first = Imap.Uid.to_int64 first and last = Imap.Uid.to_int64 last in
  first <= last && Int64.sub last first <= 999L

let nonempty_set set =
  if Imap.Uid_set.is_empty set then
    raise (Session.Failure (Session.State "empty UID set"));
  Imap.Uid_set.to_wire set

let fetch_rows responses = List.filter_map (function
  | Imap.Response.Untagged (Imap.Response.Fetch row)
  | Imap.Response.Untagged (Imap.Response.Uidfetch row) -> Some row
  | _ -> None) responses

let completion_tag (result : Session.command_result) =
  match result.completion with
  | Imap.Response.Tagged {tag;_} -> tag
  | _ -> assert false

let correlated_esearch (result : Session.command_result) =
  let tag = completion_tag result in
  List.filter_map (function
    | Imap.Response.Untagged (Imap.Response.Esearch e) when e.tag=Some tag ->
        Some e
    | _ -> None) result.untagged

let supports_messagelimit t =
  Option.is_some (Cap.messagelimit t.session.Session.capabilities)

(* ESORT orders comma-separated elements but treats either range spelling
   as ascending (RFC 5267 section 3.2). *)
let esearch_uids ?(sort=false) all =
  let max_results = 100_000 in
  let parse_uid s =
    match Option.map Imap.Uid.of_int64 (Int64.of_string_opt s) with
    | Some (Ok uid) -> uid
    | _ -> protocol "invalid ESEARCH UID" in
  let count = ref 0 in
  let seen = Hashtbl.create 32 in
  let add acc uid =
    if sort && Hashtbl.mem seen uid then protocol "duplicate ESORT UID";
    if sort then Hashtbl.add seen uid ();
    incr count;
    if !count > max_results then
      raise (Session.Failure (Session.Limit "ESEARCH result exceeds 100000 UIDs"));
    uid :: acc in
  List.fold_left (fun acc span ->
    match String.split_on_char ':' span with
    | [one] -> add acc (parse_uid one)
    | [start; finish] ->
        let start = parse_uid start and finish = parse_uid finish in
        let start,finish=
          if sort && Imap.Uid.compare finish start < 0 then finish,start
          else start,finish in
        let step = if Imap.Uid.compare start finish <= 0 then Imap.Uid.succ
          else Imap.Uid.pred in
        let rec expand acc uid =
          let acc = add acc uid in
          if Imap.Uid.equal uid finish then acc
          else match step uid with
            | Some next -> expand acc next
            | None -> acc in
        expand acc start
    | _ -> protocol "invalid ESEARCH UID range")
    [] (String.split_on_char ',' all)
  |> List.rev

let check_uidonly_search t criterion =
    if Session.is_enabled t.session Cap.Uidonly then (
      let first = match String.split_on_char ' ' (String.trim criterion) with
        | x::_ -> x | [] -> "" in
      if first<>"" && String.for_all (function
        | '0'..'9' | ',' | ':' | '*' -> true | _ -> false) first then
        raise (Session.Failure (Session.State
          "sequence-set SEARCH criterion forbidden in UIDONLY mode")))

type saved_search = { owner : t; nonce : unit ref; saved_count : int64 }
let saved_search_count saved = saved.saved_count
let check_saved saved =
  if saved.nonce != saved.owner.session.Session.saved_search_nonce then
    raise (Session.Failure (Session.State "saved SEARCH result is stale"))

let uid_search_save t ~criterion =
  run t (fun () ->
    check_uidonly_search t criterion;
    require t Cap.Searchres;
    let result=Session.command_result t.session
      (syntax (Imap.Command.uid_search_save ~criterion)) in
    let count=match correlated_esearch result with
      | [{uid=true;count=Some count;partial=None;_}] -> count
      | _ -> protocol "missing or invalid correlated SEARCH SAVE COUNT" in
    {owner=t;nonce=t.session.Session.saved_search_nonce;saved_count=count})

let uid_search_saved saved ~criterion =
  let t=saved.owner in
  run t (fun () ->
    check_saved saved;
    check_uidonly_search t criterion;
    let result=Session.command_result ~saved_search_criterion:criterion
      t.session (syntax (Imap.Command.uid_search_saved ~criterion)) in
    let response=match correlated_esearch result with
      | [{uid=true;count=Some _;_} as response] -> response
      | _ -> protocol "missing or invalid correlated saved SEARCH result" in
    let count=Option.get response.count in
    if count>saved.saved_count || response.partial<>None then
      protocol "saved SEARCH result contradicts captured set";
    let uids=match response.all with
      | None when count=0L -> []
      | None -> protocol "saved SEARCH omitted requested ALL"
      | Some all -> esearch_uids all in
    if Int64.of_int (List.length uids)<>count ||
       List.length (List.sort_uniq Imap.Uid.compare uids)<>List.length uids
    then
      protocol "saved SEARCH ALL contradicts COUNT or repeats a UID";
    uids)

let search_uids (result:Session.command_result) =
  let tag=completion_tag result in
  let candidates=List.filter_map (function
    | Imap.Response.Untagged (Imap.Response.Search uids) -> Some (`Search uids)
    | Imap.Response.Untagged (Imap.Response.Esearch e) when e.tag=Some tag ->
        Some (`Esearch e)
    | _ -> None) result.untagged in
  match candidates with
  | [`Search uids] ->
      if List.length uids>100_000 then
        raise (Session.Failure (Session.Limit "SEARCH exceeds 100000 UIDs"));
      List.map received_uid uids |> List.sort_uniq Imap.Uid.compare
  | [`Esearch e] ->
      if not e.uid then protocol "ESEARCH result lacks UID marker";
      if e.partial<>None then
        protocol "SEARCH result is a positional partial result";
      let uids=match e.all with
        | Some all -> esearch_uids all |> List.sort_uniq Imap.Uid.compare
        | None when (e.count=None || e.count=Some 0L) && e.min=None && e.max=None -> []
        | None -> protocol "SEARCH omitted ALL for a nonempty result" in
      (match e.count with
       | Some count when count<>Int64.of_int (List.length uids) ->
           protocol "SEARCH ALL contradicts COUNT"
       | _ -> ());
      uids
  | _ -> protocol "missing or repeated SEARCH result"

let uid_search t criterion =
  run t (fun () ->
    check_uidonly_search t criterion;
    search_uids (Session.command_result t.session
      (syntax (Imap.Command.uid_search ~criterion))))

let uid_sort t ~keys ~charset ~criterion =
  run t (fun () ->
    check_uidonly_search t criterion;
    if not (has t Cap.Sort || has t Cap.Sort_display) then
      raise (Session.Failure (Session.Unsupported Cap.Sort));
    let responses=Session.command t.session
      (syntax (Imap.Command.uid_sort ~keys ~charset ~criterion)) in
    match List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Sort uids) -> Some uids
      | _ -> None) responses with
    | [uids] -> List.map received_uid uids
    | _ -> protocol "missing or repeated SORT result")

type sort_result = {
  count : int64;
  first : Imap.Uid.t option;
  last : Imap.Uid.t option;
  uids : Imap.Uid.t list option;
  range : (int64 * int64) option;
}

let uid_sort_extended t ~returns ~keys ~charset ~criterion =
  run t (fun () ->
    check_uidonly_search t criterion;
    require t Cap.Esort;
    let returns=if returns=[] then [Imap.Sort.All] else returns in
    let range=List.find_map (function
      | Imap.Sort.Partial range -> Some range | _ -> None) returns in
    if range<>None then require t (Cap.Context `Sort);
    let requested field=List.exists (Imap.Sort.equal_return field) returns in
    let returns=if requested Count then returns
      else returns @ [Imap.Sort.Count] in
    let result=Session.command_result t.session
      (syntax (Imap.Command.uid_sort_extended ~returns ~keys ~charset
        ~criterion)) in
    let response=match correlated_esearch result with
      | [e] when e.uid -> e
      | _ -> protocol "missing or repeated correlated UID ESORT result" in
    let count=match response.count with
      | Some count -> count
      | None -> protocol "ESORT omitted requested COUNT" in
    let requested field=List.exists (Imap.Sort.equal_return field) returns in
    let min_uid=Option.map received_uid response.min
    and max_uid=Option.map received_uid response.max in
    let endpoint requested value =
      if count=0L && value<>None then protocol "empty ESORT has a MIN or MAX";
      if count>0L && requested && value=None then
        protocol "ESORT omitted requested MIN or MAX" in
    endpoint (requested Min) min_uid;
    endpoint (requested Max) max_uid;
    (match min_uid,max_uid with
     | Some first,Some last when (count=1L)<>Imap.Uid.equal first last ->
         protocol "ESORT MIN/MAX contradict COUNT"
     | _ -> ());
    let uids=match range with
      | None ->
          if response.partial<>None then
            protocol "unexpected ESORT PARTIAL result";
          (match response.all with
           | None when requested All && count=0L -> Some []
           | None when requested All ->
               protocol "ESORT omitted requested ALL"
           | None -> None
           | Some all ->
               let uids=esearch_uids ~sort:true all in
               if Int64.of_int (List.length uids)<>count then
                 protocol "ESORT ALL contradicts COUNT";
               Some uids)
      | Some (first,last) ->
          if response.all<>None then protocol "ESORT returned ALL for PARTIAL";
          let raw=match response.partial with
            | Some (range,raw) when range=Printf.sprintf "%Ld:%Ld" first last -> raw
            | _ -> protocol "missing or mismatched ESORT PARTIAL range" in
          let uids=match raw with None -> [] | Some raw -> esearch_uids ~sort:true raw in
          let lower=Int64.min first last and upper=Int64.max first last in
          let expected=if count<lower then 0L
            else Int64.succ (Int64.sub (Int64.min count upper) lower) in
          if Int64.of_int (List.length uids)<>expected then
            protocol "ESORT PARTIAL length contradicts range or COUNT";
          Some uids in
    (match uids with
     | Some (head::_ as uids) ->
         let starts_at_first,ends_at_last=match range with
           | None -> true,true
           | Some (a,b) -> Int64.min a b=1L,Int64.max a b>=count in
         let differs other uid = not (Imap.Uid.equal other uid) in
         if starts_at_first && Option.fold ~none:false
             ~some:(differs head) min_uid then
           protocol "ESORT MIN contradicts first sorted UID";
         if ends_at_last && Option.fold ~none:false
             ~some:(differs (List.hd (List.rev uids))) max_uid then
           protocol "ESORT MAX contradicts last sorted UID"
     | _ -> ());
    {count;first=min_uid;last=max_uid;uids;range})

type thread = { uid : Imap.Uid.t option; children : thread list }

let rec typed_thread (node : Imap.Response.thread) =
  {uid=Option.map received_uid node.number;
   children=List.map typed_thread node.children}

let uid_thread t ~algorithm ~charset ~criterion =
  run t (fun () ->
    check_uidonly_search t criterion;
    require t (Cap.Thread algorithm);
    let responses=Session.command t.session
      (syntax (Imap.Command.uid_thread ~algorithm ~charset ~criterion)) in
    match List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Thread threads) -> Some threads
      | _ -> None) responses with
    | [threads] -> List.map typed_thread threads
    | _ -> protocol "missing or repeated THREAD result")

let uid_search_partial t ~range ~criterion =
  run t (fun () ->
    check_uidonly_search t criterion;
    require t Cap.Partial;
    let result = Session.command_result t.session
      (syntax (Imap.Command.uid_search_partial ~range ~criterion)) in
    let expected = Printf.sprintf "%Ld:%Ld" (fst range) (snd range) in
    match correlated_esearch result with
    | [e] when e.uid && Option.map fst e.partial=Some expected -> e
    | _ -> protocol "missing or invalid correlated PARTIAL ESEARCH result")

type search_page = {
  uids : Imap.Uid.t list;
  complete : bool;
  limit : int64 option;
  resume_before : Imap.Uid.t option;
}

let uid_search_page t ?before criterion =
  run t (fun () ->
    check_uidonly_search t criterion;
    if not (supports_messagelimit t) then
      raise (Session.Failure
        (Session.Unsupported (Cap.Other "MESSAGELIMIT")));
    let criterion = criterion ^ (match before with
      | None -> "" | Some uid -> " UIDBEFORE " ^ Imap.Uid.to_string uid) in
    let result = Session.command_result ~accept_partial:true t.session
      (syntax (Imap.Command.uid_search ~criterion)) in
    let uids=search_uids result in
    match result.partial with
    | None -> {uids;complete=true;limit=None;resume_before=None}
    | Some (limit,Some last_uid) ->
        let last_uid=received_uid last_uid in
        let below a b = Imap.Uid.compare a b < 0 in
        let not_below_before uid = match before with
          | Some bound -> not (below uid bound)
          | None -> false in
        if Int64.of_int (List.length uids)>limit ||
           List.exists (fun uid -> below uid last_uid ||
             not_below_before uid) uids ||
           not_below_before last_uid
        then
          protocol "MESSAGELIMIT response contradicts processed UID boundary";
        (* A page processed down to UID 1 leaves nothing below it. *)
        if Option.is_some (Imap.Uid.pred last_uid) then
          {uids;complete=false;limit=Some limit;resume_before=Some last_uid}
        else {uids;complete=true;limit=Some limit;resume_before=None}
    | Some (_,None) -> raise (Session.Failure (Session.Limit
        "MESSAGELIMIT response omitted UID continuation boundary")))

let uid_search_range t ~first ~last =
  if not (valid_window ~first ~last) then
    Error (Session.State "invalid SEARCH UID window")
  else
    let criterion=Printf.sprintf "UID %s:%s" (Imap.Uid.to_string first)
      (Imap.Uid.to_string last) in
    let outside uid =
      Imap.Uid.compare uid first < 0 || Imap.Uid.compare uid last > 0 in
    if not (supports_messagelimit t) then
      (match uid_search t criterion with
       | Error _ as error -> error
       | Ok uids when List.exists outside uids ->
           Error (Session.Protocol "SEARCH returned UID outside requested range")
       | Ok uids -> Ok uids)
    else
      let module Uids=Set.Make(Imap.Uid) in
      let rec pages before count found =
        if count>1000 then Error (Session.Limit
          "SEARCH exceeded continuation budget")
        else match uid_search_page t ?before criterion with
        | Error _ as error -> error
        | Ok page ->
            if List.exists outside page.uids then
              Error (Session.Protocol
                "MESSAGELIMIT SEARCH returned UID outside requested range")
            else
              let found=List.fold_left (fun found uid ->
                Uids.add uid found) found page.uids in
              match page.resume_before with
              | Some boundary
                when not page.complete && Imap.Uid.compare boundary first > 0 ->
                  pages (Some boundary) (count+1) found
              | _ -> Ok (Uids.elements found) in
      pages None 1 Uids.empty

(* Body items stream literals that [uid_fetch] would buffer and discard, and
   their non-PEEK forms set \Seen. *)
let reject_body_items items =
  if List.exists (fun item ->
    let item = String.uppercase_ascii item in
    List.exists (fun prefix -> String.starts_with ~prefix item)
      ["BODY["; "BODY.PEEK["; "BINARY["; "BINARY.PEEK["]) items then
    raise (Session.Failure (Session.State
      "UID FETCH body items require fetch_to or fetch_binary_to"))

let uid_fetch_partial t ~set ~items ~range =
  run t (fun () ->
    require t Cap.Partial;
    reject_body_items items;
    let set=nonempty_set set in
    Session.command t.session
      (syntax (Imap.Command.uid_fetch_mod ~partial:range ~set ~items ()))
    |> fetch_rows)

let uid_fetch t ~set ~items =
  run t (fun () ->
    reject_body_items items;
    let set=nonempty_set set in
    Session.command t.session (syntax (Imap.Command.uid_fetch ~set ~items))
    |> fetch_rows |> List.map (fun (row : Imap.Response.fetch) -> row.raw))

let condstore t =
  if not (has t Cap.Condstore || has t Cap.Qresync) then
    raise (Session.Failure (Session.Unsupported Cap.Condstore))

let uid_fetch_saved saved ?partial ~items () =
  let t=saved.owner in
  run t (fun () ->
    check_saved saved;
    if partial<>None then require t Cap.Partial;
    let items=List.map String.uppercase_ascii items in
    let permitted=["UID";"FLAGS";"INTERNALDATE";"RFC822.SIZE";"ENVELOPE";
      "BODYSTRUCTURE";"MODSEQ"] in
    if items=[] || not (List.for_all (fun item -> List.mem item permitted) items) then
      raise (Session.Failure (Session.State "saved FETCH requires metadata attributes"));
    if List.mem "MODSEQ" items then condstore t;
    let items=if List.mem "UID" items then items else "UID"::items in
    Session.command t.session
      (syntax (Imap.Command.uid_fetch_saved ?partial ~items ()))
    |> fetch_rows
    |> List.filter (fun (row : Imap.Response.fetch) -> row.uid<>None))

type preview_row = { uid : Imap.Uid.t; preview : string option }

type envelope_row = {
  uid : Imap.Uid.t;
  envelope : Imap.Response.envelope;
}

let fetch_attribute t ~uids ~item ~decode =
  let seen=Hashtbl.create 50 in
  let requested=List.filter (fun uid ->
    if Hashtbl.mem seen uid then false else (
      Hashtbl.add seen uid ();
      if Hashtbl.length seen>50 then
        raise (Session.Failure (Session.State (item ^ " requires at most 50 UIDs")));
      true)) uids in
  let uids=List.sort_uniq Imap.Uid.compare requested in
  if uids=[] then
    raise (Session.Failure (Session.State
      (item ^ " requires 1..50 valid UIDs")));
  let set=wire_uids uids in
  let module Uids=Map.Make(Imap.Uid) in
  let by_uid=Session.command t.session
      (syntax (Imap.Command.uid_fetch ~set ~items:["UID";item]))
    |> fetch_rows
    |> List.fold_left (fun by_uid (row : Imap.Response.fetch) ->
      let value=match decode row with
        | Ok value -> value
        | Error message -> protocol message in
      match Option.map received_uid row.uid,value with
      | Some uid,Some value when mem_uid uid uids ->
          (match Uids.find_opt uid by_uid with
           | Some previous when item="ENVELOPE" || previous<>value ->
               protocol (item ^ " changed within one FETCH command")
           | _ -> Uids.add uid value by_uid)
      | Some _,Some _ -> protocol (item ^ " response has an unrequested UID")
      | None,Some _ -> protocol (item ^ " response lacks UID")
      | _ -> by_uid) Uids.empty in
  List.filter_map (fun uid ->
    Option.map (fun value -> uid,value) (Uids.find_opt uid by_uid)) requested

let uid_fetch_envelopes t ~uids () =
  run t (fun () ->
    fetch_attribute t ~uids ~item:"ENVELOPE" ~decode:Imap.Response.fetch_envelope
    |> List.map (fun (uid,envelope) -> {uid;envelope}))

type bodystructure_row = {
  uid : Imap.Uid.t;
  bodystructure : Imap.Response.bodystructure;
}

let uid_fetch_bodystructures t ~uids () =
  run t (fun () ->
    fetch_attribute t ~uids ~item:"BODYSTRUCTURE"
      ~decode:Imap.Response.fetch_bodystructure
    |> List.map (fun (uid,bodystructure) -> {uid;bodystructure}))

let uid_fetch_previews t ?(lazy_=false) ~uids () =
  run t (fun () ->
    require t Cap.Preview;
    let uids=List.sort_uniq Imap.Uid.compare uids in
    if uids=[] || List.length uids>50 then
      raise (Session.Failure (Session.State "PREVIEW requires 1..50 valid UIDs"));
    let set=wire_uids uids in
    let module Uids=Map.Make(Imap.Uid) in
    Session.command t.session
      (syntax (Imap.Command.uid_fetch_preview ~set ~lazy_))
    |> fetch_rows
    |> List.fold_left (fun by_uid (row : Imap.Response.fetch) ->
      match row.preview,row.uid with
      | None,_ -> by_uid
      | Some _,None -> protocol "PREVIEW response lacks UID"
      | Some preview,Some uid ->
          let uid=received_uid uid in
          if not (mem_uid uid uids) then by_uid
          else (
            if preview=None && not lazy_ then
              protocol "non-LAZY PREVIEW response was NIL";
            Uids.add uid {uid;preview} by_uid)) Uids.empty
    |> Uids.bindings |> List.map snd)

type object_id_row = {
  uid : Imap.Uid.t;
  email_id : string;
  thread_id : string option;
}

let uid_fetch_object_ids t ~uids () =
  run t (fun () ->
    require t Cap.Objectid;
    if t.info.mailbox_id=None then
      protocol "OBJECTID selection omitted MAILBOXID";
    if List.length uids>50 || List.length uids<>
        List.length (List.sort_uniq Imap.Uid.compare uids) then
      raise (Session.Failure (Session.State
        "OBJECTID requires at most 50 distinct valid UIDs"));
    if uids=[] then [] else
    let set=wire_uids uids in
    let module Uids=Map.Make(Imap.Uid) in
    let by_uid=Session.command t.session
        (syntax (Imap.Command.uid_fetch ~set
          ~items:["UID";"EMAILID";"THREADID"]))
      |> fetch_rows
      |> List.fold_left (fun by_uid (row : Imap.Response.fetch) ->
        match Option.map received_uid row.uid,row.email_id,row.thread_id with
        | Some uid,Some email_id,Some thread_id when mem_uid uid uids ->
            let object_id={uid;email_id;thread_id} in
            (match Uids.find_opt uid by_uid with
             | Some previous when previous<>object_id ->
                 protocol "OBJECTID changed within one FETCH command"
             | _ -> Uids.add uid object_id by_uid)
        | Some uid,_,_
            when mem_uid uid uids &&
                 (row.email_id<>None || row.thread_id<>None) ->
            protocol "incomplete OBJECTID FETCH row"
        | _ -> by_uid) Uids.empty in
    List.filter_map (fun uid -> Uids.find_opt uid by_uid) uids)

type object_id_plus_row = {
  uid : Imap.Uid.t;
  ids : Imap.Response.compound_object_id;
}

let uid_fetch_object_ids_plus t ~uids () =
  run t (fun () ->
    Session.require_enabled t.session Cap.Objectid_plus;
    (match t.info.objectid with
     | Some {account_id=Some _;mailbox_id=Some _;_} -> ()
     | _ -> protocol "OBJECTID+ selection omitted ACCOUNTID or MAILBOXID");
    if List.length uids>50 || List.length uids<>
        List.length (List.sort_uniq Imap.Uid.compare uids) then
      raise (Session.Failure (Session.State
        "OBJECTID+ requires at most 50 distinct valid UIDs"));
    if uids=[] then [] else
    let set=wire_uids uids in
    let module Uids=Map.Make(Imap.Uid) in
    let by_uid=Session.command t.session
        (syntax (Imap.Command.uid_fetch ~set ~items:["UID";"OBJECTID"]))
      |> fetch_rows
      |> List.fold_left (fun by_uid (row : Imap.Response.fetch) ->
        match Option.map received_uid row.uid with
        | Some uid when mem_uid uid uids ->
            (match Imap.Response.fetch_objectid row with
             | Error message -> protocol message
             | Ok None -> by_uid
             | Ok (Some ids) ->
                 if ids.account_id<>None || ids.mailbox_id<>None then
                   protocol ("message OBJECTID unexpectedly contains " ^
                     "account or mailbox ID");
                 let item={uid;ids} in
                 (match Uids.find_opt uid by_uid with
                  | Some previous when previous<>item ->
                      protocol "OBJECTID+ changed within one FETCH command"
                  | _ -> Uids.add uid item by_uid))
        | _ -> by_uid) Uids.empty in
    List.filter_map (fun uid -> Uids.find_opt uid by_uid) uids)

(* [fetch_window t ~first ~last ~what ~command ~keep] fetches the UID range
   [first:last] with [command ~set]. After an RFC 9738 partial success it
   fetches again below the processed UID. Rows accepted by [keep] are kept
   per UID, the last in wire order winning, and returned in UID order. *)
let fetch_window t ~first ~last ~what ~command ~keep =
  let supports_limit=supports_messagelimit t in
  let module Uids = Map.Make(Imap.Uid) in
  let within uid upper =
    Imap.Uid.compare uid first >= 0 && Imap.Uid.compare uid upper <= 0 in
  let rec fetch upper pages by_uid =
    if pages>1000 then raise (Session.Failure (Session.Limit
      (what ^ " FETCH exceeded continuation budget")));
    let set=Imap.Uid.to_string first ^ ":" ^ Imap.Uid.to_string upper in
    let result=Session.command_result ~accept_partial:supports_limit
      t.session (syntax (command ~set)) in
    let by_uid=List.fold_left (fun by_uid (row : Imap.Response.fetch) ->
      match Option.map received_uid row.uid with
      | Some uid when within uid upper && keep row ->
          Uids.add uid row by_uid
      | _ -> by_uid) by_uid (fetch_rows result.untagged) in
    match result.partial with
    | None -> by_uid
    | Some (_,Some boundary) ->
        let boundary=received_uid boundary in
        if not (within boundary upper && Uids.for_all (fun uid _ ->
            Imap.Uid.compare uid boundary >= 0) by_uid) then
          protocol ("invalid MESSAGELIMIT " ^ what ^ " continuation");
        (match Imap.Uid.pred boundary with
         | Some below when not (Imap.Uid.equal boundary first) ->
             fetch below (pages+1) by_uid
         | _ -> by_uid)
    | Some (_,None) ->
        raise (Session.Failure (Session.Limit
          ("MESSAGELIMIT " ^ what ^ " omitted UID continuation boundary"))) in
  Uids.bindings (fetch last 1 Uids.empty) |> List.map snd

let has_flags (row : Imap.Response.fetch) = row.flags<>None

let fetch_metadata_range ?(size=false) ?(internal_date=false)
    t ~first ~last ~modseq =
  run t (fun () ->
    if not (valid_window ~first ~last) then
      raise (Session.Failure (Session.State "invalid metadata UID window"));
    if modseq then condstore t;
    let items = ["UID"; "FLAGS"] @
      (if modseq then ["MODSEQ"] else []) @
      (if size then ["RFC822.SIZE"] else []) @
      (if internal_date then ["INTERNALDATE"] else []) in
    fetch_window t ~first ~last ~what:"metadata" ~keep:has_flags
      ~command:(fun ~set -> Imap.Command.uid_fetch ~set ~items))

type store_receipt = {
  modified : Imap.Uid_set.t;
  updates : Imap.Response.fetch list;
}

let writable t =
  if t.session.Session.readonly then
    raise (Session.Failure (Session.State "mailbox is read-only"))

let store_receipt (result:Session.command_result) =
    let modified = match result.completion with
      | Imap.Response.Tagged {code=Some (Imap.Response.Modified set); _} ->
          (match Imap.Uid_set.of_wire set with
           | Ok set -> set
           | Error message -> protocol message)
      | _ -> Imap.Uid_set.empty in
    {modified;updates=fetch_rows result.untagged}

let mutation_receipt t decode result =
  try decode result with
  | Session.Failure (Session.Protocol message) ->
      Session.close t.session;
      raise (Session.Failure (Session.Uncertain
        ("invalid mutation receipt after successful completion: " ^ message)))

let store_flags_unlocked t ~command ~operation ~flags ?unchangedsince () =
  writable t;
  if Option.is_some unchangedsince then condstore t;
  let flags=List.map Mail_flag.Imap_flag.to_wire flags in
  let syntax=
    syntax (command ?unchangedsince ~operation ~silent:false ~flags ()) in
  mutation_receipt t store_receipt
    (Session.command_result ~mutation:true t.session syntax)

let uid_store_flags t ~set ~operation ~flags ?unchangedsince () =
  run t (fun () ->
    let set=nonempty_set set in
    store_flags_unlocked t ~command:(Imap.Command.uid_store_mod ~set)
      ~operation ~flags ?unchangedsince ())

let uid_store_saved saved ~operation ~flags ?unchangedsince () =
  let t=saved.owner in
  run t (fun () ->
    check_saved saved;
    store_flags_unlocked t ~command:Imap.Command.uid_store_saved
      ~operation ~flags ?unchangedsince ())

type copy_mapping = {
  source_first : Imap.Uid.t;
  destination_first : Imap.Uid.t;
  length : int64;
}

type copy_receipt = {
  uidvalidity : Imap.Uidvalidity.t;
  source : Imap.Uid_set.t;
  destination : Imap.Uid_set.t;
  mapping : copy_mapping list;
}

let copy_code = function
  | Imap.Response.Tagged {code=Some (Imap.Response.Copyuid (v,src,dst)); _}
  | Imap.Response.Untagged
      (Imap.Response.Ok (Some (Imap.Response.Copyuid (v,src,dst)), _)) ->
      Some (v,src,dst)
  | _ -> None

let subset small large = Imap.Uid_set.(is_empty (diff small large))

let copy_receipt ?requested result =
  let codes=List.filter_map copy_code
    (result.Session.untagged @ [result.completion]) in
  let require = function
    | Ok value -> value
    | Error message -> protocol message in
  let ordered wire =
    let set=require (Imap.Uid_set.of_wire wire) in
    let ranges=String.split_on_char ',' wire |> List.map (fun span ->
      match Imap.Uid_set.intervals (require (Imap.Uid_set.of_wire span)) with
      | [first,last] -> Imap.Uid.to_int64 first,Imap.Uid.to_int64 last
      | _ -> assert false) in
    let count=List.fold_left (fun count (first,last) ->
      Int64.add count (Int64.succ (Int64.sub last first))) 0L ranges in
    if count<>Imap.Uid_set.cardinality set then
      protocol "COPYUID repeats a UID";
    set,ranges,count in
  match codes with
  | [] -> None
  | [validity,source,destination] ->
      let source,sources,source_count=ordered source in
      (match requested with
       | Some requested when not (subset source requested) ->
           protocol "COPYUID names a source UID that was not requested"
       | _ -> ());
      let destination,destinations,destination_count=ordered destination in
      if source_count<>destination_count then
        protocol "COPYUID cardinality mismatch";
      let rec pair acc sources destinations = match sources,destinations with
        | [],[] -> List.rev acc
        | (sf,sl)::ss,(df,dl)::ds ->
            let length=Int64.succ (Int64.min (Int64.sub sl sf) (Int64.sub dl df)) in
            let next_source=Int64.add sf length and next_destination=Int64.add df length in
            let range={source_first=require (Imap.Uid.of_int64 sf);
              destination_first=require (Imap.Uid.of_int64 df);length} in
            pair (range::acc)
              (if next_source>sl then ss else (next_source,sl)::ss)
              (if next_destination>dl then ds else (next_destination,dl)::ds)
        | _ -> assert false in
      Some {uidvalidity=require (Imap.Uidvalidity.of_int64 validity);
        source;destination;mapping=pair [] sources destinations}
  | _ -> protocol "duplicate COPYUID receipts"

let copy_or_move_unlocked ?requested t ~move ~command ~mailbox =
  if move then (
    writable t;
    require t Cap.Move);
  let mailbox=Session.mailbox_wire t.session mailbox in
  mutation_receipt t (copy_receipt ?requested)
    (Session.command_result ~mutation:true t.session
      (syntax (command ~mailbox)))

let uid_copy t ~set ~mailbox =
  run t (fun () ->
    let wire=nonempty_set set in
    copy_or_move_unlocked ~requested:set t ~move:false
      ~command:(Imap.Command.uid_copy ~set:wire) ~mailbox)

let uid_move t ~set ~mailbox =
  run t (fun () ->
    let wire=nonempty_set set in
    copy_or_move_unlocked ~requested:set t ~move:true
      ~command:(Imap.Command.uid_move ~set:wire) ~mailbox)

let uid_copy_saved saved ~mailbox =
  let t=saved.owner in
  run t (fun () ->
    check_saved saved;
    copy_or_move_unlocked t ~move:false ~command:Imap.Command.uid_copy_saved ~mailbox)

let uid_move_saved saved ~mailbox =
  let t=saved.owner in
  run t (fun () ->
    check_saved saved;
    copy_or_move_unlocked t ~move:true ~command:Imap.Command.uid_move_saved ~mailbox)

let expunge_unlocked t syntax =
  writable t;
  require t Cap.Uidplus;
  ignore (Session.command_result ~mutation:true t.session syntax)

let uid_expunge t ~set =
  run t (fun () ->
    let set=nonempty_set set in
    expunge_unlocked t (syntax (Imap.Command.uid_expunge ~set)))

let uid_expunge_saved saved =
  let t=saved.owner in
  run t (fun () ->
    check_saved saved;
    expunge_unlocked t Imap.Command.uid_expunge_saved)

let wait_for_change t =
  run t (fun () ->
    require t Cap.Idle;
    Session.idle_once t.session)

let fetch_changes t ~set ~since ~vanished =
  run t (fun () ->
    condstore t;
    if vanished then Session.require_enabled t.session Cap.Qresync;
    let set = nonempty_set set in
    let changedsince = Imap.Modseq.to_int64 since in
    Session.command t.session
      (syntax (Imap.Command.uid_fetch_mod ~changedsince ~vanished
        ~set ~items:["UID"; "FLAGS"; "MODSEQ"] ()))
    |> List.filter (function
      | Imap.Response.Untagged (Imap.Response.Fetch _)
      | Imap.Response.Untagged (Imap.Response.Uidfetch _)
      | Imap.Response.Untagged (Imap.Response.Vanished _)
      | Imap.Response.Untagged (Imap.Response.Ok
          (Some (Imap.Response.Highestmodseq _), _)) -> true
      | _ -> false))

let fetch_changes_range t ~first ~last ~since =
  run t (fun () ->
    condstore t;
    if not (valid_window ~first ~last) then
      raise (Session.Failure (Session.State
        "invalid CHANGEDSINCE UID window"));
    let changedsince=Imap.Modseq.to_int64 since in
    fetch_window t ~first ~last ~what:"CHANGEDSINCE" ~keep:has_flags
      ~command:(fun ~set -> Imap.Command.uid_fetch_mod ~changedsince
        ~vanished:false ~set ~items:["UID";"FLAGS";"MODSEQ"] ()))

let uid_batches t ?range ~size () =
  run t (fun () ->
    require t Cap.Uidbatches;
    if t.session.Session.uidbatches_last_mailbox = t.session.Session.selected then
      raise (Session.Failure (Session.State
        "UIDBATCHES already issued for this mailbox on this connection"));
    let syntax = syntax (Imap.Command.uid_batches ?range ~size ()) in
    t.session.Session.uidbatches_last_mailbox <- t.session.Session.selected;
    let result = Session.command_result t.session syntax in
    let tag = completion_tag result in
    match List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Uidbatches batch)
        when batch.tag = tag -> Some batch
      | _ -> None) result.untagged with
    | [batch] -> batch
    | [] -> protocol "missing UIDBATCHES response"
    | _ -> protocol "duplicate UIDBATCHES response")

let notify_set t ?(status=false) ~groups () =
  run t (fun () ->
    require t Cap.Notify;
    let responses=Session.command ~mutation:true t.session
      (syntax (Imap.Command.notify_set ~status ~groups ())) in
    if List.exists (function
      | Imap.Response.Untagged (Imap.Response.Ok
          (Some Imap.Response.Notificationoverflow,_)) -> true
      | _ -> false) responses then
      raise (Session.Failure (Session.Limit
        "server cancelled NOTIFY after notification overflow"));
    List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Status status) -> Some status
      | _ -> None) responses)

let notify_none t =
  run t (fun () ->
    require t Cap.Notify;
    ignore (Session.command ~mutation:true t.session Imap.Command.notify_none))

(* A failed local sink leaves the command mid-literal, so the session
   closes, but the failure is not the server's. *)
let write_sink sink data =
  try Eio.Flow.write sink [Cstruct.of_string data] with
  | Eio.Cancel.Cancelled _ as ex -> raise ex
  | ex -> raise (Session.Failure (Session.State
      ("local FETCH sink failed: " ^ Printexc.to_string ex)))

let stream_fetch t ~syntax ~max_bytes sink =
  let bytes=ref 0L and declared=ref 0L and literals=ref 0 in
  let exceeds amount used = amount>Int64.sub max_bytes used in
  let on_literal_start length =
    if exceeds length !declared then
      raise (Session.Failure (Session.Limit "FETCH body exceeds byte limit"));
    declared:=Int64.add !declared length;
    incr literals in
  let on_literal chunk =
    let length=Int64.of_int (String.length chunk) in
    if exceeds length !bytes then
      raise (Session.Failure (Session.Limit "FETCH body exceeds byte limit"));
    bytes:=Int64.add !bytes length;
    write_sink sink chunk in
  let responses=Session.command ~on_literal_start ~on_literal t.session syntax in
  responses,!bytes,!literals

let fetch_binary_to t ?(max_bytes=1_073_741_824L) ?partial ~uid ~section sink =
  run t (fun () ->
    require_binary_fetch t;
    if max_bytes<0L then
      raise (Session.Failure (Session.State "invalid BINARY UID or byte limit"));
    let raw_uid=Some (Imap.Uid.to_int64 uid) in
    let syntax=syntax (Imap.Command.uid_fetch_binary
      ~set:(Imap.Uid.to_string uid) ~section ?partial ()) in
    let max_bytes=match partial with
      | None -> max_bytes | Some (_,count) -> Int64.min max_bytes count in
    let responses,bytes,literals=stream_fetch t ~syntax ~max_bytes sink in
    let invalid message =
      Session.close t.session;
      protocol message in
    let rows=fetch_rows responses in
    let payloads=List.filter_map (fun (row : Imap.Response.fetch) ->
      match Imap.Response.fetch_binary row ~section ~offset:(Option.map fst partial) with
      | Ok None -> None
      | Ok (Some binary) -> Some (row,binary)
      | Error message -> invalid message) rows in
    let row,binary=match payloads with
      | [] when literals=0 ->
          if List.exists (fun (row:Imap.Response.fetch) ->
              row.uid=raw_uid) rows then
            invalid "BINARY FETCH omitted the requested section/origin"
          else raise (Session.Failure (Session.Missing_uid uid))
      | [row,binary] when row.uid=raw_uid -> row,binary
      | [row,_] when row.uid=None -> invalid "BINARY FETCH payload lacks UID"
      | _ -> invalid "BINARY FETCH did not match the requested UID" in
    match binary with
    | Imap.Response.Literal length ->
        if literals<>1 || List.length row.literals<>1 || bytes<>length then
          invalid "BINARY FETCH literal metadata did not match streamed bytes";
        Some bytes
    | Imap.Response.Nil ->
        if literals<>0 || row.literals<>[] then
          invalid "BINARY NIL response contained unexpected literals";
        None
    | Imap.Response.Inline value ->
        if literals<>0 || row.literals<>[] then
          invalid "inline BINARY response contained unexpected literals";
        let length=Int64.of_int (String.length value) in
        if length>max_bytes then
          raise (Session.Failure (Session.Limit "BINARY body exceeds byte limit"));
        write_sink sink value;
        Some length)

type binary_size_row = { uid : Imap.Uid.t; size : int64 }

let uid_fetch_binary_sizes t ~uids ~section () =
  run t (fun () ->
    require_binary_fetch t;
    let uids=List.sort_uniq Imap.Uid.compare uids in
    if uids=[] || List.length uids>50 then
      raise (Session.Failure (Session.State "BINARY.SIZE requires 1..50 valid UIDs"));
    let set=wire_uids uids in
    let module Uids=Map.Make(Imap.Uid) in
    let by_uid=Session.command t.session
        (syntax (Imap.Command.uid_fetch_binary_size ~set ~section))
      |> fetch_rows
      |> List.fold_left (fun by_uid (row : Imap.Response.fetch) ->
        let value=match Imap.Response.fetch_binary_size row ~section with
          | Ok value -> value
          | Error message -> protocol message in
        match value,row.uid with
        | None,_ -> by_uid
        | Some _,None -> protocol "BINARY.SIZE response omitted UID"
        | Some size,Some uid ->
            let uid=received_uid uid in
            if not (mem_uid uid uids) then
              protocol "BINARY.SIZE returned an unrequested UID";
            if Uids.mem uid by_uid then protocol "duplicate BINARY.SIZE UID";
            Uids.add uid {uid;size} by_uid)
        Uids.empty in
    List.filter_map (fun uid -> Uids.find_opt uid by_uid) uids)

(* [quoted_bodies raw] lists the decoded quoted-string values of the BODY[]
   items at the top level of the FETCH row text [raw]. *)
let quoted_bodies raw =
  let n=String.length raw in
  let rec after_quoted i =
    if i>=n then n
    else match raw.[i] with
      | '\\' -> after_quoted (i+2)
      | '"' -> i+1
      | _ -> after_quoted (i+1) in
  let unescape s =
    let b=Buffer.create (String.length s) in
    let rec go i =
      if i<String.length s then
        if s.[i]='\\' && i+1<String.length s then
          (Buffer.add_char b s.[i+1]; go (i+2))
        else (Buffer.add_char b s.[i]; go (i+1)) in
    go 0; Buffer.contents b in
  let marker="BODY[] \"" in
  let m=String.length marker in
  let rec scan i depth acc =
    if i>=n then List.rev acc
    else match raw.[i] with
      | '"' -> scan (after_quoted (i+1)) depth acc
      | '(' -> scan (i+1) (depth+1) acc
      | ')' -> scan (i+1) (depth-1) acc
      | _ when depth=1 && (i=0 || raw.[i-1]=' ' || raw.[i-1]='(') &&
          i+m<=n && String.uppercase_ascii (String.sub raw i m)=marker ->
          let stop=after_quoted (i+m) in
          let value=String.sub raw (i+m) (max 0 (stop-1-(i+m))) in
          scan stop depth (unescape value :: acc)
      | _ -> scan (i+1) depth acc in
  scan 0 0 []

let fetch_to t ?(max_bytes=1_073_741_824L) ~uid sink =
  run t (fun () ->
    if max_bytes < 0L then
      raise (Session.Failure (Session.State "negative FETCH byte limit"));
    let raw_uid = Some (Imap.Uid.to_int64 uid) in
    let syntax = syntax (Imap.Command.uid_fetch
      ~set:(Imap.Uid.to_string uid) ~items:["UID"; "BODY.PEEK[]"]) in
    let responses,bytes,literals=stream_fetch t ~syntax ~max_bytes sink in
    let bodies=List.filter_map (fun (row : Imap.Response.fetch) ->
      match row.literals,quoted_bodies row.raw with
      | [],[] -> None
      | streamed,quoted -> Some (row,streamed,quoted)) (fetch_rows responses) in
    let invalid () =
      Session.close t.session;
      protocol ("UID FETCH body metadata did not match requested UID " ^
        "and streamed bytes") in
    match bodies with
    | [] when literals=0 -> raise (Session.Failure (Session.Missing_uid uid))
    | [row,[name,length],[]] when row.uid=raw_uid && literals=1 ->
        let name=String.uppercase_ascii name in
        if not ((name="BODY[]" || name="BODY.PEEK[]") && length=bytes) then
          invalid ()
    | [row,[],[value]] when row.uid=raw_uid && literals=0 ->
        if Int64.of_int (String.length value)>max_bytes then
          raise (Session.Failure (Session.Limit
            "FETCH body exceeds byte limit"));
        write_sink sink value
    | _ -> invalid ())
