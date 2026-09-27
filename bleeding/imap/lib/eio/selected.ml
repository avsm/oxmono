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

(* ESORT orders comma-separated elements but treats either range spelling
   as ascending (RFC 5267 section 3.2). SEARCH retains its existing expansion. *)
let esearch_uids ?(sort=false) all =
  let max_results = 100_000 in
  let parse_uid s = match Int64.of_string_opt s with
  | Some uid when uid >= 1L && uid <= 4_294_967_295L -> uid
  | _ -> raise (Session.Failure (Session.Protocol "invalid ESEARCH UID")) in
  let count = ref 0 in
  let seen = Hashtbl.create 32 in
  let add acc uid =
    if sort && Hashtbl.mem seen uid then
      raise (Session.Failure (Session.Protocol "duplicate ESORT UID"));
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
        let start,finish=if sort then Int64.min start finish,Int64.max start finish
          else start,finish in
        let step = if start <= finish then 1L else -1L in
        let rec expand acc uid =
          let acc = add acc uid in
          if uid = finish then acc else expand acc (Int64.add uid step) in
        expand acc start
    | _ -> raise (Session.Failure (Session.Protocol "invalid ESEARCH UID range")))
    [] (String.split_on_char ',' all)
  |> List.rev

let check_uidonly_search t criterion =
    if List.mem "UIDONLY" t.session.Session.enabled then (
      let first = match String.split_on_char ' ' (String.trim criterion) with
        | x::_ -> x | [] -> "" in
      if first<>"" && String.for_all (function
        | '0'..'9' | ',' | ':' | '*' -> true | _ -> false) first then
        raise (Session.Failure (Session.State
          "sequence-set SEARCH criterion forbidden in UIDONLY mode")))

type saved_search = { owner : t; nonce : unit ref; saved_count : int64 }
let saved_search_count saved = saved.saved_count
let check_saved saved =
  if not (List.mem "SEARCHRES" saved.owner.session.Session.capabilities) then
    raise (Session.Failure (Session.State "SEARCHRES unavailable"));
  if saved.nonce != saved.owner.session.Session.saved_search_nonce then
    raise (Session.Failure (Session.State "saved SEARCH result is stale"))

let uid_search_save t ~criterion =
  run t (fun () ->
    check_uidonly_search t criterion;
    if not (List.mem "SEARCHRES" t.session.Session.capabilities) then
      raise (Session.Failure (Session.State "SEARCHRES unavailable"));
    let syntax=match Imap.Command.uid_search_save ~criterion with
      | Ok syntax -> syntax
      | Error message -> raise (Session.Failure (Session.State message)) in
    let result=Session.command_result t.session syntax in
    let tag=match result.completion with
      | Imap.Response.Tagged {tag;_} -> tag | _ -> assert false in
    let count=match List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Esearch e) when e.tag=Some tag -> Some e
      | _ -> None) result.untagged with
      | [{uid=true;count=Some count;partial=None;_}] -> count
      | _ -> raise (Session.Failure (Session.Protocol
          "missing or invalid correlated SEARCH SAVE COUNT")) in
    {owner=t;nonce=t.session.Session.saved_search_nonce;saved_count=count})

let uid_search_saved saved ~criterion =
  let t=saved.owner in
  run t (fun () ->
    check_saved saved;
    check_uidonly_search t criterion;
    let reject message=raise (Session.Failure (Session.Protocol message)) in
    let syntax=match Imap.Command.uid_search_saved ~criterion with
      | Ok syntax -> syntax
      | Error message -> raise (Session.Failure (Session.State message)) in
    let result=Session.command_result ~saved_search_criterion:criterion t.session syntax in
    let tag=match result.completion with
      | Imap.Response.Tagged {tag;_} -> tag | _ -> assert false in
    let response=match List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Esearch e) when e.tag=Some tag -> Some e
      | _ -> None) result.untagged with
      | [{uid=true;count=Some _;_} as response] -> response
      | _ -> reject "missing or invalid correlated saved SEARCH result" in
    let count=Option.get response.count in
    if count>saved.saved_count || response.partial<>None then
      reject "saved SEARCH result contradicts captured set";
    let uids=match response.all with
      | None when count=0L -> []
      | None -> reject "saved SEARCH omitted requested ALL"
      | Some all -> esearch_uids all in
    if Int64.of_int (List.length uids)<>count ||
       List.length (List.sort_uniq Int64.compare uids)<>List.length uids then
      reject "saved SEARCH ALL contradicts COUNT or repeats a UID";
    uids)

let search_uids (result:Session.command_result) =
  let reject message=raise (Session.Failure (Session.Protocol message)) in
  let tag=match result.completion with
    | Imap.Response.Tagged {tag;_} -> tag | _ -> assert false in
  let candidates=List.filter_map (function
    | Imap.Response.Untagged (Imap.Response.Search uids) -> Some (`Search uids)
    | Imap.Response.Untagged (Imap.Response.Esearch e)
        when e.tag=None || e.tag=Some tag -> Some (`Esearch e)
    | _ -> None) result.untagged in
  match candidates with
  | [`Search uids] ->
      if List.length uids>100_000 then
        raise (Session.Failure (Session.Limit "SEARCH exceeds 100000 UIDs"));
      uids
  | [`Esearch e] ->
      if not e.uid then reject "ESEARCH result lacks UID marker";
      if e.partial<>None then reject "SEARCH result is a positional partial result";
      let uids=match e.all with
        | Some all -> esearch_uids all |> List.sort_uniq Int64.compare
        | None when (e.count=None || e.count=Some 0L) && e.min=None && e.max=None -> []
        | None -> reject "SEARCH omitted ALL for a nonempty result" in
      (match e.count with
       | Some count when count<>Int64.of_int (List.length uids) ->
           reject "SEARCH ALL contradicts COUNT"
       | _ -> ());
      uids
  | _ -> reject "missing or repeated SEARCH result"

let uid_search t criterion =
  run t (fun () ->
    check_uidonly_search t criterion;
    let syntax = match Imap.Command.uid_search ~criterion with
    | Ok s -> s | Error e -> raise (Session.Failure (Session.State e)) in
    search_uids (Session.command_result t.session syntax))

let uid_sort t ~keys ~charset ~criterion =
  run t (fun () ->
    check_uidonly_search t criterion;
    if not (List.exists (String.starts_with ~prefix:"SORT")
        t.session.Session.capabilities) then
      raise (Session.Failure (Session.State "SORT unavailable"));
    let syntax=match Imap.Command.uid_sort ~keys ~charset ~criterion with
      | Ok syntax -> syntax
      | Error message -> raise (Session.Failure (Session.State message)) in
    let responses=Session.command t.session syntax in
    match List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Sort uids) -> Some uids
      | _ -> None) responses with
    | [uids] -> uids
    | _ -> raise (Session.Failure (Session.Protocol
        "missing or repeated SORT result")))

type sort_result = {
  count : int64;
  first : int64 option;
  last : int64 option;
  uids : int64 list option;
  range : (int64 * int64) option;
}

let uid_sort_extended t ~returns ~keys ~charset ~criterion =
  run t (fun () ->
    check_uidonly_search t criterion;
    let reject message=raise (Session.Failure (Session.Protocol message)) in
    if not (List.mem "ESORT" t.session.Session.capabilities) then
      raise (Session.Failure (Session.State "ESORT unavailable"));
    let returns=if returns=[] then [Imap.Command.All] else returns in
    let range=List.find_map (function
      | Imap.Command.Partial range -> Some range | _ -> None) returns in
    if range<>None && not (List.mem "CONTEXT=SORT" t.session.Session.capabilities) then
      raise (Session.Failure (Session.State "CONTEXT=SORT unavailable"));
    let returns=if List.mem Imap.Command.Count returns then returns
      else returns @ [Imap.Command.Count] in
    let syntax=match Imap.Command.uid_sort_extended ~returns ~keys ~charset ~criterion with
      | Ok syntax -> syntax
      | Error message -> raise (Session.Failure (Session.State message)) in
    let result=Session.command_result t.session syntax in
    let tag=match result.completion with
      | Imap.Response.Tagged {tag;_} -> tag | _ -> assert false in
    let response=match List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Esearch e) when e.tag=Some tag -> Some e
      | _ -> None) result.untagged with
      | [e] when e.uid -> e
      | _ -> reject "missing or repeated correlated UID ESORT result" in
    let count=match response.count with
      | Some count -> count
      | None -> reject "ESORT omitted requested COUNT" in
    let requested field=List.mem field returns in
    let endpoint requested value =
      if count=0L && value<>None then reject "empty ESORT has a MIN or MAX";
      if count>0L && requested && value=None then
        reject "ESORT omitted requested MIN or MAX" in
    endpoint (requested Imap.Command.Min) response.min;
    endpoint (requested Imap.Command.Max) response.max;
    (match response.min,response.max with
     | Some first,Some last when (count=1L)<>(first=last) ->
         reject "ESORT MIN/MAX contradict COUNT"
     | _ -> ());
    let uids=match range with
      | None ->
          if response.partial<>None then reject "unexpected ESORT PARTIAL result";
          (match response.all with
           | None when requested Imap.Command.All && count=0L -> Some []
           | None when requested Imap.Command.All -> reject "ESORT omitted requested ALL"
           | None -> None
           | Some all ->
               let uids=esearch_uids ~sort:true all in
               if Int64.of_int (List.length uids)<>count then
                 reject "ESORT ALL contradicts COUNT";
               Some uids)
      | Some (first,last) ->
          if response.all<>None then reject "ESORT returned ALL for PARTIAL";
          let raw=match response.partial with
            | Some (range,raw) when range=Printf.sprintf "%Ld:%Ld" first last -> raw
            | _ -> reject "missing or mismatched ESORT PARTIAL range" in
          let uids=match raw with None -> [] | Some raw -> esearch_uids ~sort:true raw in
          let lower=Int64.min first last and upper=Int64.max first last in
          let expected=if count<lower then 0L
            else Int64.succ (Int64.sub (Int64.min count upper) lower) in
          if Int64.of_int (List.length uids)<>expected then
            reject "ESORT PARTIAL length contradicts range or COUNT";
          Some uids in
    (match uids with
     | Some (head::_ as uids) ->
         let starts_at_first,ends_at_last=match range with
           | None -> true,true
           | Some (a,b) -> Int64.min a b=1L,Int64.max a b>=count in
         if starts_at_first && Option.fold ~none:false
             ~some:(fun first -> first<>head) response.min then
           reject "ESORT MIN contradicts first sorted UID";
         if ends_at_last && Option.fold ~none:false
             ~some:(fun last -> last<>List.hd (List.rev uids)) response.max then
           reject "ESORT MAX contradicts last sorted UID"
     | _ -> ());
    {count;first=response.min;last=response.max;uids;range})

let uid_thread t ~algorithm ~charset ~criterion =
  run t (fun () ->
    check_uidonly_search t criterion;
    let capability=match algorithm with
      | Imap.Command.Orderedsubject -> "THREAD=ORDEREDSUBJECT"
      | Imap.Command.References -> "THREAD=REFERENCES" in
    if not (List.mem capability t.session.Session.capabilities) then
      raise (Session.Failure (Session.State (capability ^ " unavailable")));
    let syntax=match Imap.Command.uid_thread ~algorithm ~charset ~criterion with
      | Ok syntax -> syntax
      | Error message -> raise (Session.Failure (Session.State message)) in
    let responses=Session.command t.session syntax in
    match List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Thread threads) -> Some threads
      | _ -> None) responses with
    | [threads] -> threads
    | _ -> raise (Session.Failure (Session.Protocol
        "missing or repeated THREAD result")))

let uid_search_partial t ~range ~criterion =
  run t (fun () ->
    check_uidonly_search t criterion;
    if not (List.mem "PARTIAL" t.session.Session.capabilities) then
      raise (Session.Failure (Session.State "PARTIAL unavailable"));
    let syntax = match Imap.Command.uid_search_partial ~range ~criterion with
      | Ok syntax -> syntax
      | Error message -> raise (Session.Failure (Session.State message)) in
    let result = Session.command_result t.session syntax in
    let tag = match result.completion with
      | Imap.Response.Tagged {tag;_} -> tag | _ -> assert false in
    let expected = Printf.sprintf "%Ld:%Ld" (fst range) (snd range) in
    match List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Esearch e) when e.tag=Some tag ->
          Some e
      | _ -> None) result.untagged with
    | [e] when e.uid && Option.map fst e.partial=Some expected -> e
    | _ -> raise (Session.Failure (Session.Protocol
        "missing or invalid correlated PARTIAL ESEARCH result")))

type search_page = {
  uids : int64 list;
  complete : bool;
  limit : int64 option;
  resume_before : int64 option;
}

let uid_search_page t ?before criterion =
  run t (fun () ->
    check_uidonly_search t criterion;
    if not (List.exists (fun capability ->
      String.length capability >= 13 &&
      String.sub capability 0 13 = "MESSAGELIMIT=")
        t.session.Session.capabilities) then
      raise (Session.Failure (Session.State "MESSAGELIMIT unavailable"));
    (match before with
     | Some uid when uid < 1L || uid > 4_294_967_295L ->
         raise (Session.Failure (Session.State "invalid UIDBEFORE boundary"))
     | _ -> ());
    let criterion = criterion ^ (match before with
      | None -> "" | Some uid -> Printf.sprintf " UIDBEFORE %Ld" uid) in
    let syntax = match Imap.Command.uid_search ~criterion with
      | Ok syntax -> syntax
      | Error message -> raise (Session.Failure (Session.State message)) in
    let result = Session.command_result ~accept_partial:true t.session syntax in
    let uids=search_uids result in
    match result.partial with
    | None -> {uids;complete=true;limit=None;resume_before=None}
    | Some (limit,Some last_uid) ->
        if Int64.of_int (List.length uids)>limit ||
           List.exists (fun uid -> uid<last_uid ||
             (match before with Some bound -> uid>=bound | None -> false)) uids ||
           (match before with Some bound -> last_uid>=bound | None -> false)
        then raise (Session.Failure (Session.Protocol
          "MESSAGELIMIT response contradicts processed UID boundary"));
        {uids;complete=false;limit=Some limit;
         resume_before=(if last_uid>1L then Some last_uid else None)}
    | Some (_,None) -> raise (Session.Failure (Session.Limit
        "MESSAGELIMIT response omitted UID continuation boundary")))

let uid_search_range t ~first ~last =
  if first<1L || last<first || last>4_294_967_295L ||
     Int64.sub last first>999L then
    Error (Session.State "invalid SEARCH UID window")
  else
    let criterion=Printf.sprintf "UID %Ld:%Ld" first last in
    let supports_limit=List.exists (String.starts_with
      ~prefix:"MESSAGELIMIT=") t.session.Session.capabilities in
    if not supports_limit then
      (match uid_search t criterion with
       | Error _ as error -> error
       | Ok uids when List.exists (fun uid -> uid<first || uid>last) uids ->
           Error (Session.Protocol "SEARCH returned UID outside requested range")
       | Ok uids -> Ok (List.sort_uniq Int64.compare uids))
    else
      let module Uids=Set.Make(Int64) in
      let rec pages before count found =
        if count>1000 then Error (Session.Limit
          "SEARCH exceeded continuation budget")
        else match uid_search_page t ?before criterion with
        | Error _ as error -> error
        | Ok page ->
            if List.exists (fun uid -> uid<first || uid>last) page.uids then
              Error (Session.Protocol
                "MESSAGELIMIT SEARCH returned UID outside requested range")
            else
              let found=List.fold_left (fun found uid ->
                Uids.add uid found) found page.uids in
              if page.complete then Ok (Uids.elements found)
              else match page.resume_before with
              | None -> Ok (Uids.elements found)
              | Some boundary when boundary<=first ->
                  Ok (Uids.elements found)
              | Some boundary -> pages (Some boundary) (count+1) found in
      pages None 1 Uids.empty

let uid_fetch_partial t ~set ~items ~range =
  run t (fun () ->
    if not (List.mem "PARTIAL" t.session.Session.capabilities) then
      raise (Session.Failure (Session.State "PARTIAL unavailable"));
    let syntax = match Imap.Command.uid_fetch_mod ~partial:range
      ~set ~items () with
      | Ok syntax -> syntax
      | Error message -> raise (Session.Failure (Session.State message)) in
    Session.command t.session syntax |> List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Fetch row)
      | Imap.Response.Untagged (Imap.Response.Uidfetch row) -> Some row
      | _ -> None))

let uid_fetch t ~set ~items =
  run t (fun () ->
    let syntax = match Imap.Command.uid_fetch ~set ~items with
    | Ok s -> s | Error e -> raise (Session.Failure (Session.State e)) in
    Session.command t.session syntax
    |> List.filter_map (function
         | Imap.Response.Untagged (Imap.Response.Fetch f)
         | Imap.Response.Untagged (Imap.Response.Uidfetch f) -> Some f.raw
         | _ -> None))

let uid_fetch_saved saved ?partial ~items () =
  let t=saved.owner in
  run t (fun () ->
    check_saved saved;
    if partial<>None && not (List.mem "PARTIAL" t.session.Session.capabilities) then
      raise (Session.Failure (Session.State "PARTIAL unavailable"));
    let items=List.map String.uppercase_ascii items in
    let permitted=["UID";"FLAGS";"INTERNALDATE";"RFC822.SIZE";"ENVELOPE";
      "BODYSTRUCTURE";"MODSEQ"] in
    if items=[] || not (List.for_all (fun item -> List.mem item permitted) items) then
      raise (Session.Failure (Session.State "saved FETCH requires metadata attributes"));
    if List.mem "MODSEQ" items &&
       not (List.mem "CONDSTORE" t.session.Session.capabilities ||
            List.mem "QRESYNC" t.session.Session.capabilities) then
      raise (Session.Failure (Session.State "CONDSTORE unavailable"));
    let items=if List.mem "UID" items then items else "UID"::items in
    let syntax=match Imap.Command.uid_fetch_saved ?partial ~items () with
      | Ok syntax -> syntax
      | Error message -> raise (Session.Failure (Session.State message)) in
    let responses=Session.command t.session syntax in
    List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Fetch row)
      | Imap.Response.Untagged (Imap.Response.Uidfetch row) when row.uid<>None -> Some row
      | _ -> None) responses)

type preview_row = { uid : int64; preview : string option }

type envelope_row = {
  uid : int64;
  envelope : Imap.Response.envelope;
}

(* The bounded structured attributes share correlation and duplicate checks. *)
let fetch_attribute t ~uids ~item ~decode =
  let seen=Hashtbl.create 50 in
  let requested=List.filter (fun uid ->
    if Hashtbl.mem seen uid then false else (
      Hashtbl.add seen uid ();
      if Hashtbl.length seen>50 then
        raise (Session.Failure (Session.State (item ^ " requires at most 50 UIDs")));
      true)) uids in
  let uids=List.sort_uniq Int64.compare requested in
  if uids=[] || List.length uids>50 || List.exists (fun uid ->
    uid<1L || uid>4_294_967_295L) uids then
    raise (Session.Failure (Session.State
      (item ^ " requires 1..50 valid UIDs")));
  let set=String.concat "," (List.map Int64.to_string uids) in
  let syntax=match Imap.Command.uid_fetch ~set ~items:["UID";item] with
    | Ok syntax -> syntax
    | Error message -> raise (Session.Failure (Session.State message)) in
  let module Uids=Map.Make(Int64) in
  let by_uid=Session.command t.session syntax
    |> List.fold_left (fun by_uid -> function
      | Imap.Response.Untagged (Imap.Response.Fetch row)
      | Imap.Response.Untagged (Imap.Response.Uidfetch row) ->
          let value=match decode row with
            | Ok value -> value
            | Error message -> raise (Session.Failure
                (Session.Protocol message)) in
          (match row.uid,value with
           | Some uid,Some value when List.mem uid uids ->
               (match Uids.find_opt uid by_uid with
                | Some previous when item="ENVELOPE" || previous<>value ->
                    raise (Session.Failure (Session.Protocol
                      (item ^ " changed within one FETCH command")))
                | _ -> Uids.add uid value by_uid)
           | Some _,Some _ -> raise (Session.Failure (Session.Protocol
               (item ^ " response has an unrequested UID")))
           | None,Some _ -> raise (Session.Failure (Session.Protocol
               (item ^ " response lacks UID")))
           | _ -> by_uid)
      | _ -> by_uid) Uids.empty in
  List.filter_map (fun uid ->
    Option.map (fun value -> uid,value) (Uids.find_opt uid by_uid)) requested

let uid_fetch_envelopes t ~uids () =
  run t (fun () ->
    fetch_attribute t ~uids ~item:"ENVELOPE" ~decode:Imap.Response.fetch_envelope
    |> List.map (fun (uid,envelope) -> {uid;envelope}))

type bodystructure_row = {
  uid : int64;
  bodystructure : Imap.Response.bodystructure;
}

let uid_fetch_bodystructures t ~uids () =
  run t (fun () ->
    fetch_attribute t ~uids ~item:"BODYSTRUCTURE"
      ~decode:Imap.Response.fetch_bodystructure
    |> List.map (fun (uid,bodystructure) -> {uid;bodystructure}))

let uid_fetch_previews t ?(lazy_=false) ~uids () =
  run t (fun () ->
    if not (List.mem "PREVIEW" t.session.Session.capabilities) then
      raise (Session.Failure (Session.State "PREVIEW unavailable"));
    let uids=List.sort_uniq Int64.compare uids in
    if uids=[] || List.length uids>50 || List.exists (fun uid ->
      uid<1L || uid>4_294_967_295L) uids then
      raise (Session.Failure (Session.State "PREVIEW requires 1..50 valid UIDs"));
    let set=String.concat "," (List.map Int64.to_string uids) in
    let syntax=match Imap.Command.uid_fetch_preview ~set ~lazy_ with
      | Ok syntax -> syntax
      | Error message -> raise (Session.Failure (Session.State message)) in
    let rows=Session.command t.session syntax in
    let module Uids=Map.Make(Int64) in
    let by_uid=List.fold_left (fun by_uid -> function
      | Imap.Response.Untagged (Imap.Response.Fetch row)
      | Imap.Response.Untagged (Imap.Response.Uidfetch row) ->
          (match row.preview,row.uid with
           | None,_ -> by_uid
           | Some _,None -> raise (Session.Failure (Session.Protocol
               "PREVIEW response lacks UID"))
           | Some preview,Some uid when List.mem uid uids ->
               if preview=None && not lazy_ then
                 raise (Session.Failure (Session.Protocol
                   "non-LAZY PREVIEW response was NIL"));
               Uids.add uid {uid;preview} by_uid
           | Some _,Some _ -> by_uid)
      | _ -> by_uid) Uids.empty rows in
    Uids.bindings by_uid |> List.map snd)

type object_id_row = {
  uid : int64;
  email_id : string;
  thread_id : string option;
}

let uid_fetch_object_ids t ~uids () =
  run t (fun () ->
    if not (List.mem "OBJECTID" t.session.Session.capabilities) then
      raise (Session.Failure (Session.State "OBJECTID unavailable"));
    if t.info.mailbox_id=None then
      raise (Session.Failure (Session.Protocol
        "OBJECTID selection omitted MAILBOXID"));
    if List.length uids>50 || List.length uids<>
        List.length (List.sort_uniq Int64.compare uids) ||
       List.exists (fun uid -> uid<1L || uid>4_294_967_295L) uids then
      raise (Session.Failure (Session.State
        "OBJECTID requires at most 50 distinct valid UIDs"));
    if uids=[] then [] else
    let set=String.concat "," (List.map Int64.to_string uids) in
    let syntax=match Imap.Command.uid_fetch ~set
      ~items:["UID";"EMAILID";"THREADID"] with
      | Ok syntax -> syntax
      | Error message -> raise (Session.Failure (Session.State message)) in
    let module Uids=Map.Make(Int64) in
    let by_uid=Session.command t.session syntax
      |> List.fold_left (fun by_uid -> function
        | Imap.Response.Untagged (Imap.Response.Fetch row)
        | Imap.Response.Untagged (Imap.Response.Uidfetch row) ->
            (match row.uid,row.email_id,row.thread_id with
             | Some uid,Some email_id,Some thread_id
                 when List.mem uid uids ->
                   let object_id={uid;email_id;thread_id} in
                   (match Uids.find_opt uid by_uid with
                    | Some previous when previous<>object_id ->
                        raise (Session.Failure (Session.Protocol
                          "OBJECTID changed within one FETCH command"))
                    | _ -> Uids.add uid object_id by_uid)
             | Some uid,_,_
                 when List.mem uid uids &&
                      (row.email_id<>None || row.thread_id<>None) ->
                   raise (Session.Failure (Session.Protocol
                     "incomplete OBJECTID FETCH row"))
             | _ -> by_uid)
        | _ -> by_uid) Uids.empty in
    List.filter_map (fun uid -> Uids.find_opt uid by_uid) uids)

type object_id_plus_row = {
  uid : int64;
  ids : Imap.Response.compound_object_id;
}

let uid_fetch_object_ids_plus t ~uids () =
  run t (fun () ->
    if not (List.mem "OBJECTID+" t.session.Session.enabled) then
      raise (Session.Failure (Session.State
        "OBJECTID+ has not been enabled"));
    (match t.info.objectid with
     | Some {account_id=Some _;mailbox_id=Some _;_} -> ()
     | _ -> raise (Session.Failure (Session.Protocol
         "OBJECTID+ selection omitted ACCOUNTID or MAILBOXID")));
    if List.length uids>50 || List.length uids<>
        List.length (List.sort_uniq Int64.compare uids) ||
       List.exists (fun uid -> uid<1L || uid>4_294_967_295L) uids then
      raise (Session.Failure (Session.State
        "OBJECTID+ requires at most 50 distinct valid UIDs"));
    if uids=[] then [] else
    let set=String.concat "," (List.map Int64.to_string uids) in
    let syntax=match Imap.Command.uid_fetch ~set
      ~items:["UID";"OBJECTID"] with
      | Ok syntax -> syntax
      | Error message -> raise (Session.Failure (Session.State message)) in
    let module Uids=Map.Make(Int64) in
    let by_uid=Session.command t.session syntax
      |> List.fold_left (fun by_uid -> function
        | Imap.Response.Untagged (Imap.Response.Fetch row)
        | Imap.Response.Untagged (Imap.Response.Uidfetch row) ->
            (match row.uid with
             | Some uid when List.mem uid uids ->
                 (match Imap.Response.fetch_objectid row with
                  | Error message ->
                      raise (Session.Failure (Session.Protocol message))
                  | Ok None -> by_uid
                  | Ok (Some ids) ->
                      if ids.account_id<>None || ids.mailbox_id<>None then
                        raise (Session.Failure (Session.Protocol
                          "message OBJECTID unexpectedly contains account or mailbox ID"));
                      let item={uid;ids} in
                      (match Uids.find_opt uid by_uid with
                       | Some previous when previous<>item ->
                           raise (Session.Failure (Session.Protocol
                             "OBJECTID+ changed within one FETCH command"))
                       | _ -> Uids.add uid item by_uid))
             | _ -> by_uid)
        | _ -> by_uid) Uids.empty in
    List.filter_map (fun uid -> Uids.find_opt uid by_uid) uids)

let fetch_metadata_range ?(size=false) ?(internal_date=false)
    t ~first ~last ~modseq =
  run t (fun () ->
    if first < 1L || last < first || last > 4_294_967_295L ||
       Int64.sub last first > 999L then
      raise (Session.Failure (Session.State "invalid metadata UID window"));
    let items = ["UID"; "FLAGS"] @
      (if modseq then ["MODSEQ"] else []) @
      (if size then ["RFC822.SIZE"] else []) @
      (if internal_date then ["INTERNALDATE"] else []) in
    let module Uids = Map.Make(Int64) in
    let supports_limit=List.exists (fun capability ->
      String.starts_with ~prefix:"MESSAGELIMIT=" capability)
      t.session.Session.capabilities in
    let rec fetch upper pages by_uid =
      if pages>1000 then raise (Session.Failure (Session.Limit
        "metadata FETCH exceeded continuation budget"));
      let set=Printf.sprintf "%Ld:%Ld" first upper in
      let syntax=match Imap.Command.uid_fetch ~set ~items with
        | Ok syntax -> syntax
        | Error message -> raise (Session.Failure (Session.State message)) in
      let result=Session.command_result ~accept_partial:supports_limit
        t.session syntax in
      let by_uid=List.fold_left (fun by_uid -> function
        | Imap.Response.Untagged (Imap.Response.Fetch row)
        | Imap.Response.Untagged (Imap.Response.Uidfetch row) ->
            (match row.uid,row.flags with
             | Some uid,Some _ when uid>=first && uid<=upper ->
                 Uids.add uid row by_uid
             | _ -> by_uid)
        | _ -> by_uid) by_uid result.untagged in
      match result.partial with
      | None -> by_uid
      | Some (_,Some boundary) when supports_limit &&
          boundary>=first && boundary<=upper &&
          Uids.for_all (fun uid _ -> uid>=boundary) by_uid ->
          if boundary=first then by_uid
          else fetch (Int64.pred boundary) (pages+1) by_uid
      | Some (_,None) when supports_limit ->
          raise (Session.Failure (Session.Limit
            "MESSAGELIMIT FETCH omitted UID continuation boundary"))
      | Some _ -> raise (Session.Failure (Session.Protocol
          "invalid MESSAGELIMIT FETCH continuation")) in
    let by_uid=fetch last 1 Uids.empty in
    Uids.bindings by_uid |> List.map snd)

type store_receipt = {
  modified : Imap.Proto.Uid_set.t;
  updates : Imap.Response.fetch list;
}

let has t capability = List.mem capability t.session.Session.capabilities
let writable t =
  if t.session.Session.readonly then
    raise (Session.Failure (Session.State "mailbox is read-only"))
let nonempty_set set =
  let wire = Imap.Proto.Uid_set.to_wire set in
  if wire = "" then raise (Session.Failure (Session.State "empty UID set"));
  wire

let store_receipt (result:Session.command_result) =
    let modified = match result.completion with
      | Imap.Response.Tagged {code=Some (Imap.Response.Modified set); _} ->
          (match Imap.Proto.Uid_set.of_wire set with
           | Ok set -> set
           | Error message ->
               raise (Session.Failure (Session.Protocol message)))
      | _ -> Imap.Proto.Uid_set.empty in
    let updates = List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Fetch row)
      | Imap.Response.Untagged (Imap.Response.Uidfetch row) -> Some row
      | _ -> None) result.untagged in
    {modified;updates}

let mutation_receipt t decode result =
  try decode result with
  | Session.Failure (Session.Protocol message) ->
      Session.close t.session;
      raise (Session.Failure (Session.Uncertain
        ("invalid mutation receipt after successful completion: " ^ message)))

let store_flags_unlocked t ~command ~operation ~flags ?unchangedsince () =
  writable t;
  if Option.is_some unchangedsince &&
     not (has t "CONDSTORE" || has t "QRESYNC") then
    raise (Session.Failure (Session.State "CONDSTORE unavailable"));
  let flags=List.map Mail_flag.Imap_flag.to_wire flags in
  let syntax=match command ?unchangedsince ~operation ~silent:false ~flags () with
    | Ok syntax -> syntax
    | Error message -> raise (Session.Failure (Session.State message)) in
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
  source_first : Imap.Proto.Uid.t;
  destination_first : Imap.Proto.Uid.t;
  length : int64;
}

type copy_receipt = {
  uidvalidity : Imap.Proto.Uidvalidity.t;
  source : Imap.Proto.Uid_set.t;
  destination : Imap.Proto.Uid_set.t;
  mapping : copy_mapping list;
}

let mailbox_wire t mailbox =
  let mode = if List.mem "IMAP4REV2" t.session.Session.enabled ||
    (has t "IMAP4REV2" && not (has t "IMAP4REV1")) ||
    List.mem "UTF8=ACCEPT" t.session.Session.enabled
    then Imap.Mailbox_name.Utf8 else Imap.Mailbox_name.Rev1 in
  match Imap.Mailbox_name.encode ~mode mailbox with
  | Ok wire -> wire
  | Error message -> raise (Session.Failure (Session.State message))

let copy_code = function
  | Imap.Response.Tagged {code=Some (Imap.Response.Copyuid (v,src,dst)); _}
  | Imap.Response.Untagged
      (Imap.Response.Ok (Some (Imap.Response.Copyuid (v,src,dst)), _)) ->
      Some (v,src,dst)
  | _ -> None

let copy_receipt result =
  let codes=List.filter_map copy_code
    (result.Session.untagged @ [result.completion]) in
  let require = function
    | Ok value -> value
    | Error message -> raise (Session.Failure (Session.Protocol message)) in
  let ordered wire =
    let set=require (Imap.Proto.Uid_set.of_wire wire) in
    let ranges=String.split_on_char ',' wire |> List.map (fun span ->
      match Imap.Proto.Uid_set.intervals (require (Imap.Proto.Uid_set.of_wire span)) with
      | [first,last] -> Imap.Proto.Uid.to_int64 first,Imap.Proto.Uid.to_int64 last
      | _ -> assert false) in
    let count=List.fold_left (fun count (first,last) ->
      Int64.add count (Int64.succ (Int64.sub last first))) 0L ranges in
    if count<>Imap.Proto.Uid_set.cardinality set then
      raise (Session.Failure (Session.Protocol "COPYUID repeats a UID"));
    set,ranges,count in
  match codes with
  | [] -> None
  | [validity,source,destination] ->
      let source,sources,source_count=ordered source in
      let destination,destinations,destination_count=ordered destination in
      if source_count<>destination_count then
        raise (Session.Failure (Session.Protocol "COPYUID cardinality mismatch"));
      let rec pair acc sources destinations = match sources,destinations with
        | [],[] -> List.rev acc
        | (sf,sl)::ss,(df,dl)::ds ->
            let length=Int64.succ (Int64.min (Int64.sub sl sf) (Int64.sub dl df)) in
            let next_source=Int64.add sf length and next_destination=Int64.add df length in
            let range={source_first=require (Imap.Proto.Uid.of_int64 sf);
              destination_first=require (Imap.Proto.Uid.of_int64 df);length} in
            pair (range::acc)
              (if next_source>sl then ss else (next_source,sl)::ss)
              (if next_destination>dl then ds else (next_destination,dl)::ds)
        | _ -> assert false in
      Some {uidvalidity=require (Imap.Proto.Uidvalidity.of_int64 validity);
        source;destination;mapping=pair [] sources destinations}
  | _ -> raise (Session.Failure (Session.Protocol "duplicate COPYUID receipts"))

let copy_or_move_unlocked t ~move ~command ~mailbox =
  if move then (
    writable t;
    if not (has t "MOVE") then
      raise (Session.Failure (Session.State "MOVE unavailable")));
  let mailbox=mailbox_wire t mailbox in
  let syntax=match command ~mailbox with
    | Ok syntax -> syntax
    | Error message -> raise (Session.Failure (Session.State message)) in
  mutation_receipt t copy_receipt
    (Session.command_result ~mutation:true t.session syntax)

let uid_copy t ~set ~mailbox =
  run t (fun () ->
    let set=nonempty_set set in
    copy_or_move_unlocked t ~move:false ~command:(Imap.Command.uid_copy ~set) ~mailbox)

let uid_move t ~set ~mailbox =
  run t (fun () ->
    let set=nonempty_set set in
    copy_or_move_unlocked t ~move:true ~command:(Imap.Command.uid_move ~set) ~mailbox)

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
  if not (has t "UIDPLUS") then
    raise (Session.Failure (Session.State "UIDPLUS unavailable"));
  ignore (Session.command_result ~mutation:true t.session syntax)

let uid_expunge t ~set =
  run t (fun () ->
    let set=nonempty_set set in
    let syntax=match Imap.Command.uid_expunge ~set with
      | Ok syntax -> syntax
      | Error message -> raise (Session.Failure (Session.State message)) in
    expunge_unlocked t syntax)

let uid_expunge_saved saved =
  let t=saved.owner in
  run t (fun () ->
    check_saved saved;
    expunge_unlocked t Imap.Command.uid_expunge_saved)

let wait_for_change t =
  run t (fun () ->
    if not (has t "IDLE") then
      raise (Session.Failure (Session.State "IDLE unavailable"));
    Session.idle_once t.session)

let fetch_changes t ~set ~since ~vanished =
  run t (fun () ->
    if not (has t "CONDSTORE" || has t "QRESYNC") then
      raise (Session.Failure (Session.State "CONDSTORE unavailable"));
    if vanished &&
       not (List.mem "QRESYNC" t.session.Session.enabled) then
      raise (Session.Failure (Session.State "QRESYNC not enabled"));
    let set = nonempty_set set in
    let changedsince = Imap.Proto.Modseq.to_int64 since in
    let syntax = match Imap.Command.uid_fetch_mod ~changedsince ~vanished
      ~set ~items:["UID"; "FLAGS"; "MODSEQ"] () with
    | Ok s -> s
    | Error message -> raise (Session.Failure (Session.State message)) in
    Session.command t.session syntax
    |> List.filter (function
      | Imap.Response.Untagged (Imap.Response.Fetch _)
      | Imap.Response.Untagged (Imap.Response.Uidfetch _)
      | Imap.Response.Untagged (Imap.Response.Vanished _)
      | Imap.Response.Untagged (Imap.Response.Ok
          (Some (Imap.Response.Highestmodseq _), _)) -> true
      | _ -> false))

let fetch_changes_range t ~first ~last ~since =
  run t (fun () ->
    if not (has t "CONDSTORE" || has t "QRESYNC") then
      raise (Session.Failure (Session.State "CONDSTORE unavailable"));
    if first<1L || last<first || last>4_294_967_295L ||
       Int64.sub last first>999L then
      raise (Session.Failure (Session.State
        "invalid CHANGEDSINCE UID window"));
    let changedsince=Imap.Proto.Modseq.to_int64 since in
    let supports_limit=List.exists
      (String.starts_with ~prefix:"MESSAGELIMIT=")
      t.session.Session.capabilities in
    let module Uids=Map.Make(Int64) in
    let rec fetch upper pages by_uid =
      if pages>1000 then raise (Session.Failure (Session.Limit
        "CHANGEDSINCE FETCH exceeded continuation budget"));
      let set=Printf.sprintf "%Ld:%Ld" first upper in
      let syntax=match Imap.Command.uid_fetch_mod ~changedsince
        ~vanished:false ~set ~items:["UID";"FLAGS";"MODSEQ"] () with
        | Ok syntax -> syntax
        | Error message -> raise (Session.Failure (Session.State message)) in
      let result=Session.command_result ~accept_partial:supports_limit
        t.session syntax in
      let by_uid=List.fold_left (fun by_uid -> function
        | Imap.Response.Untagged (Imap.Response.Fetch row)
        | Imap.Response.Untagged (Imap.Response.Uidfetch row) ->
            (match row.uid with
             | Some uid when uid>=first && uid<=upper ->
                 Uids.add uid row by_uid
             | _ -> by_uid)
        | _ -> by_uid) by_uid result.untagged in
      match result.partial with
      | None -> by_uid
      | Some (_,Some boundary) when supports_limit &&
          boundary>=first && boundary<=upper &&
          Uids.for_all (fun uid _ -> uid>=boundary) by_uid ->
          if boundary=first then by_uid
          else fetch (Int64.pred boundary) (pages+1) by_uid
      | Some (_,None) when supports_limit ->
          raise (Session.Failure (Session.Limit
            "MESSAGELIMIT CHANGEDSINCE omitted UID continuation boundary"))
      | Some _ -> raise (Session.Failure (Session.Protocol
          "invalid MESSAGELIMIT CHANGEDSINCE continuation")) in
    Uids.bindings (fetch last 1 Uids.empty) |> List.map snd)

let uid_batches t ?range ~size () =
  run t (fun () ->
    if not (has t "UIDBATCHES") then
      raise (Session.Failure (Session.State "UIDBATCHES unavailable"));
    if t.session.Session.uidbatches_last_mailbox = t.session.Session.selected then
      raise (Session.Failure (Session.State
        "UIDBATCHES already issued for this mailbox on this connection"));
    let syntax = match Imap.Command.uid_batches ?range ~size () with
    | Ok s -> s
    | Error message -> raise (Session.Failure (Session.State message)) in
    t.session.Session.uidbatches_last_mailbox <- t.session.Session.selected;
    let result = Session.command_result t.session syntax in
    let tag = match result.completion with
      | Imap.Response.Tagged {tag; _} -> tag
      | _ -> assert false in
    match List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Uidbatches batch)
        when batch.tag = tag -> Some batch
      | _ -> None) result.untagged with
    | [batch] -> batch
    | [] -> raise (Session.Failure (Session.Protocol
        "missing UIDBATCHES response"))
    | _ -> raise (Session.Failure (Session.Protocol
        "duplicate UIDBATCHES response")))

let notify_set t ?(status=false) ~groups () =
  run t (fun () ->
    if not (has t "NOTIFY") then
      raise (Session.Failure (Session.State "NOTIFY unavailable"));
    let syntax = match Imap.Command.notify_set ~status ~groups () with
      | Ok syntax -> syntax
      | Error message -> raise (Session.Failure (Session.State message)) in
    let responses=Session.command ~mutation:true t.session syntax in
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
    if not (has t "NOTIFY") then
      raise (Session.Failure (Session.State "NOTIFY unavailable"));
    ignore (Session.command ~mutation:true t.session Imap.Command.notify_none))

let require_binary t =
  let rev2=List.mem "IMAP4REV2" t.session.Session.enabled ||
    (has t "IMAP4REV2" && not (has t "IMAP4REV1")) in
  if not (has t "BINARY" || rev2) then
    raise (Session.Failure (Session.State "BINARY unavailable"))

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
    Eio.Flow.write sink [Cstruct.of_string chunk] in
  let responses=Session.command ~on_literal_start ~on_literal t.session syntax in
  responses,!bytes,!literals

let fetch_binary_to t ?(max_bytes=1_073_741_824L) ?partial ~uid ~section sink =
  run t (fun () ->
    require_binary t;
    if uid<1L || uid>4_294_967_295L || max_bytes<0L then
      raise (Session.Failure (Session.State "invalid BINARY UID or byte limit"));
    let syntax=match Imap.Command.uid_fetch_binary ~set:(Int64.to_string uid)
        ~section ?partial () with
      | Ok syntax -> syntax
      | Error message -> raise (Session.Failure (Session.State message)) in
    let max_bytes=match partial with
      | None -> max_bytes | Some (_,count) -> Int64.min max_bytes count in
    let responses,bytes,literals=stream_fetch t ~syntax ~max_bytes sink in
    let invalid message =
      Session.close t.session;
      raise (Session.Failure (Session.Protocol message)) in
    let rows=List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Fetch row)
      | Imap.Response.Untagged (Imap.Response.Uidfetch row) -> Some row
      | _ -> None) responses in
    let payloads=List.filter_map (fun row ->
      match Imap.Response.fetch_binary row ~section ~offset:(Option.map fst partial) with
      | Ok None -> None
      | Ok (Some binary) -> Some (row,binary)
      | Error message -> invalid message) rows in
    let row,binary=match payloads with
      | [] when literals=0 ->
          if List.exists (fun (row:Imap.Response.fetch) -> row.uid=Some uid) rows then
            invalid "BINARY FETCH omitted the requested section/origin"
          else raise (Session.Failure (Session.Missing_uid uid))
      | [row,binary] when row.uid=Some uid -> row,binary
      | [row,_] when row.uid=None ->
          Session.close t.session;
          raise (Session.Failure (Session.Missing_uid uid))
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
        Eio.Flow.copy_string value sink;
        Some length)

type binary_size_row = { uid : int64; size : int64 }

let uid_fetch_binary_sizes t ~uids ~section () =
  run t (fun () ->
    require_binary t;
    let uids=List.sort_uniq Int64.compare uids in
    if uids=[] || List.length uids>50 || List.exists (fun uid ->
      uid<1L || uid>4_294_967_295L) uids then
      raise (Session.Failure (Session.State "BINARY.SIZE requires 1..50 valid UIDs"));
    let set=String.concat "," (List.map Int64.to_string uids) in
    let syntax=match Imap.Command.uid_fetch_binary_size ~set ~section with
      | Ok syntax -> syntax
      | Error message -> raise (Session.Failure (Session.State message)) in
    let module Uids=Map.Make(Int64) in
    let rows=Session.command t.session syntax in
    let by_uid=List.fold_left (fun by_uid -> function
      | Imap.Response.Untagged (Imap.Response.Fetch row)
      | Imap.Response.Untagged (Imap.Response.Uidfetch row) ->
          let value=match Imap.Response.fetch_binary_size row ~section with
            | Ok value -> value
            | Error message -> raise (Session.Failure (Session.Protocol message)) in
          (match value,row.uid with
           | None,_ -> by_uid
           | Some _,None -> raise (Session.Failure (Session.Protocol
               "BINARY.SIZE response omitted UID"))
           | Some size,Some uid when List.mem uid uids ->
               if Uids.mem uid by_uid then
                 raise (Session.Failure (Session.Protocol "duplicate BINARY.SIZE UID"));
               Uids.add uid {uid;size} by_uid
           | Some _,Some _ -> raise (Session.Failure (Session.Protocol
               "BINARY.SIZE returned an unrequested UID")))
      | _ -> by_uid) Uids.empty rows in
    List.filter_map (fun uid -> Uids.find_opt uid by_uid) uids)

let fetch_to t ?(max_bytes=1_073_741_824L) ~uid sink =
  run t (fun () ->
    if uid < 1L || uid > 4294967295L then
      raise (Session.Failure (Session.State "invalid UID"));
    if max_bytes < 0L then
      raise (Session.Failure (Session.State "negative FETCH byte limit"));
    let syntax = match Imap.Command.uid_fetch
      ~set:(Int64.to_string uid) ~items:["UID"; "BODY.PEEK[]"] with
    | Ok s -> s | Error e -> raise (Session.Failure (Session.State e)) in
    let responses,bytes,literals=stream_fetch t ~syntax ~max_bytes sink in
    let rows = List.filter_map (function
      | Imap.Response.Untagged (Imap.Response.Fetch row)
      | Imap.Response.Untagged (Imap.Response.Uidfetch row) when row.literals<>[] -> Some row
      | _ -> None) responses in
    let valid = match rows with
    | [] when bytes=0L && literals=0 ->
        raise (Session.Failure (Session.Missing_uid uid))
    | [row] when row.uid = Some uid ->
        (match row.literals with
         | [name, length] when literals=1 ->
             let name = String.uppercase_ascii name in
             (name = "BODY[]" || name = "BODY.PEEK[]") &&
             length = bytes
         | _ -> false)
    | _ -> false in
    if not valid then (
      Session.close t.session;
      raise (Session.Failure (Session.Protocol
        "UID FETCH body metadata did not match requested UID and streamed bytes"))))
