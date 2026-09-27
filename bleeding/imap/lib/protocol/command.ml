type error = string
let capability = "CAPABILITY"
let noop = "NOOP"
let logout = "LOGOUT"
let idle = "IDLE"
let done_idle = "DONE"
let get_jmap_access = "GETJMAPACCESS"
let notify_none = "NOTIFY NONE"

let contains_control s =
  String.exists (fun c -> Char.code c < 0x20 || Char.code c = 0x7f) s

let atom s =
  let n = String.length s in
  n > 0 &&
  let ok = ref true in
  String.iter (fun c ->
    let code = Char.code c in
    if code <= 0x20 || code >= 0x7f ||
       String.contains "(){%*\\\"]" c then ok := false) s;
  !ok

let quote s =
  if contains_control s then Error "invalid quoted string"
  else
    let b = Buffer.create (String.length s+2) in
    Buffer.add_char b '"';
    String.iter (fun c ->
      if c='\\' || c='"' then Buffer.add_char b '\\';
      Buffer.add_char b c) s;
    Buffer.add_char b '"'; Ok (Buffer.contents b)

let astring s = if atom s then Ok s else quote s
let bind x f = match x with Ok v -> f v | Error _ as e -> e

let nonempty_astring what s =
  if s="" then Error ("empty " ^ what) else astring s
let rights s =
  String.for_all (function 'a'..'z' | '0'..'9' -> true | _ -> false) s
let getacl ~mailbox =
  bind (astring mailbox) @@ fun mailbox -> Ok ("GETACL " ^ mailbox)
let myrights ~mailbox =
  bind (astring mailbox) @@ fun mailbox -> Ok ("MYRIGHTS " ^ mailbox)
let listrights ~mailbox ~identifier =
  bind (astring mailbox) @@ fun mailbox ->
  bind (nonempty_astring "ACL identifier" identifier) @@ fun identifier ->
  Ok ("LISTRIGHTS " ^ mailbox ^ " " ^ identifier)
let setacl ~mailbox ~identifier ~operation ~rights:grant =
  if not (rights grant) then Error "invalid ACL rights"
  else bind (astring mailbox) @@ fun mailbox ->
  bind (nonempty_astring "ACL identifier" identifier) @@ fun identifier ->
  let prefix=match operation with `Add -> "+" | `Remove -> "-" |
    `Replace -> "" in
  bind (astring (prefix ^ grant)) @@ fun grant ->
  Ok ("SETACL " ^ mailbox ^ " " ^ identifier ^ " " ^ grant)
let deleteacl ~mailbox ~identifier =
  bind (astring mailbox) @@ fun mailbox ->
  bind (nonempty_astring "ACL identifier" identifier) @@ fun identifier ->
  Ok ("DELETEACL " ^ mailbox ^ " " ^ identifier)

let getquota ~root =
  bind (astring root) @@ fun root -> Ok ("GETQUOTA " ^ root)
let getquotaroot ~mailbox =
  bind (astring mailbox) @@ fun mailbox -> Ok ("GETQUOTAROOT " ^ mailbox)
let quota_resource = atom
let setquota ~root ~limits =
  if not (List.for_all (fun (name,limit) ->
    quota_resource name && limit >= 0L) limits) then
    Error "invalid quota limit"
  else if List.length limits <>
          List.length (List.sort_uniq String.compare
            (List.map (fun (n,_) -> String.uppercase_ascii n) limits)) then
    Error "duplicate quota resource"
  else bind (astring root) @@ fun root ->
  let pairs=List.map (fun (n,v) ->
    String.uppercase_ascii n ^ " " ^ Int64.to_string v) limits in
  Ok ("SETQUOTA " ^ root ^ " (" ^ String.concat " " pairs ^ ")")

type metadata_depth = Zero | One | Infinity
let metadata_entry s =
  let n=String.length s in
  n>1 && s.[0]='/' && not (String.contains s '*') &&
  not (String.contains s '%') && not (contains_control s) &&
  not (String.contains s '\000')
let getmetadata ~mailbox ~entries ?maxsize ?depth () =
  if entries=[] || not (List.for_all metadata_entry entries) then
    Error "invalid METADATA entries"
  else if (match maxsize with Some n -> n<0L | None -> false) then
    Error "invalid METADATA MAXSIZE"
  else bind (astring mailbox) @@ fun mailbox ->
  let options=(match maxsize with None -> [] | Some n ->
    ["MAXSIZE " ^ Int64.to_string n]) @
    (match depth with None -> [] | Some Zero -> ["DEPTH 0"] |
      Some One -> ["DEPTH 1"] | Some Infinity -> ["DEPTH infinity"]) in
  let options=if options=[] then "" else
    " (" ^ String.concat " " options ^ ")" in
  let rec entry_list acc = function
    | [] -> Ok (List.rev acc)
    | x::xs -> bind (astring x) @@ fun x -> entry_list (x::acc) xs in
  bind (entry_list [] entries) @@ fun entries ->
  let entries=match entries with [one] -> one | _ ->
    "(" ^ String.concat " " entries ^ ")" in
  Ok ("GETMETADATA" ^ options ^ " " ^ mailbox ^ " " ^ entries)
let setmetadata ~mailbox ~values =
  if values=[] || not (List.for_all (fun (key,_) -> metadata_entry key) values)
  then Error "invalid METADATA values"
  else if List.length values <>
          List.length (List.sort_uniq String.compare
            (List.map fst values)) then Error "duplicate METADATA entry"
  else bind (astring mailbox) @@ fun mailbox ->
  let rec pairs acc = function
    | [] -> Ok (List.rev acc)
    | (key,value)::rest ->
        bind (astring key) @@ fun key ->
        bind (match value with None -> Ok "NIL" | Some v -> quote v) @@ fun value ->
        pairs ((key ^ " " ^ value)::acc) rest in
  bind (pairs [] values) @@ fun values ->
  Ok ("SETMETADATA " ^ mailbox ^ " (" ^ String.concat " " values ^ ")")

type notify_filter = Selected | Selected_delayed | Inboxes | Personal |
  Subscribed | Subtree of string list | Mailboxes of string list
type notify_event = Message_new | Message_expunge | Flag_change |
  Annotation_change | Mailbox_name | Subscription_change |
  Mailbox_metadata_change | Server_metadata_change
type notify_group = notify_filter * notify_event list
let notify_event = function
  | Message_new -> "MessageNew" | Message_expunge -> "MessageExpunge"
  | Flag_change -> "FlagChange" | Annotation_change -> "AnnotationChange"
  | Mailbox_name -> "MailboxName" | Subscription_change -> "SubscriptionChange"
  | Mailbox_metadata_change -> "MailboxMetadataChange"
  | Server_metadata_change -> "ServerMetadataChange"
let notify_filter = function
  | Selected -> Ok "selected" | Selected_delayed -> Ok "selected-delayed"
  | Inboxes -> Ok "inboxes" | Personal -> Ok "personal"
  | Subscribed -> Ok "subscribed"
  | Subtree names | Mailboxes names as filter ->
      if names=[] then Error "empty NOTIFY mailbox selector"
      else
        let rec encode acc = function
          | [] -> Ok (List.rev acc)
          | name::rest -> bind (astring name) @@ fun name ->
              encode (name::acc) rest in
        bind (encode [] names) @@ fun names ->
        let names=match names with [one] -> one | _ ->
          "(" ^ String.concat " " names ^ ")" in
        Ok ((match filter with Subtree _ -> "subtree " | _ -> "mailboxes ") ^ names)
let notify_set ?(status=false) ~groups () =
  if groups=[] then Error "NOTIFY needs an event group"
  else if List.length (List.filter (function
    | (Selected|Selected_delayed),_ -> true | _ -> false) groups)>1 then
    Error "multiple selected NOTIFY groups"
  else
    let rec encode acc = function
      | [] -> Ok (List.rev acc)
      | (filter,events)::rest ->
          let has event=List.mem event events in
          if (has Message_new)<>(has Message_expunge) ||
             (has Flag_change && not (has Message_new)) then
            Error "NOTIFY message events must be paired"
          else if List.length events<>
                  List.length (List.sort_uniq compare events) then
            Error "duplicate NOTIFY event"
          else if (match filter with Selected|Selected_delayed ->
              List.exists (function Message_new|Message_expunge|
                Flag_change|Annotation_change -> false | _ -> true) events
              | _ -> false) then Error "invalid selected NOTIFY event"
          else bind (notify_filter filter) @@ fun filter ->
          let events=if events=[] then "NONE" else
            "(" ^ String.concat " " (List.map notify_event events) ^ ")" in
          encode (("(" ^ filter ^ " " ^ events ^ ")")::acc) rest in
    bind (encode [] groups) @@ fun groups ->
    Ok ("NOTIFY SET" ^ (if status then " STATUS" else "") ^ " " ^
        String.concat " " groups)

let login ~username ~password =
  bind (quote username) @@ fun user ->
  bind (quote password) @@ fun pass ->
  Ok ("LOGIN " ^ user ^ " " ^ pass)

let list ~reference ~pattern =
  bind (astring reference) @@ fun reference ->
  bind (astring pattern) @@ fun pattern ->
  Ok ("LIST " ^ reference ^ " " ^ pattern)

let lsub ~reference ~pattern =
  bind (astring reference) @@ fun reference ->
  bind (astring pattern) @@ fun pattern ->
  Ok ("LSUB " ^ reference ^ " " ^ pattern)

let namespace = "NAMESPACE"

type status_item = Messages | Unseen | Uidnext | Uidvalidity |
  Highestmodseq | Mailboxid | Objectid | Size | Deleted | Deleted_storage
let status_item = function
  | Messages -> "MESSAGES" | Unseen -> "UNSEEN" | Uidnext -> "UIDNEXT"
  | Uidvalidity -> "UIDVALIDITY" | Highestmodseq -> "HIGHESTMODSEQ"
  | Mailboxid -> "MAILBOXID" | Objectid -> "OBJECTID" | Size -> "SIZE"
  | Deleted -> "DELETED" | Deleted_storage -> "DELETED-STORAGE"
let status_items items =
  if items=[] then Error "STATUS needs at least one item"
  else Ok ("(" ^ String.concat " " (List.map status_item items) ^ ")")
type list_selection = Subscribed | Remote | Recursive_match | Special_use
type list_return = Return_subscribed | Children | Return_special_use
let list_selection = function
  | Subscribed -> "SUBSCRIBED" | Remote -> "REMOTE"
  | Recursive_match -> "RECURSIVEMATCH" | Special_use -> "SPECIAL-USE"
let list_return = function
  | Return_subscribed -> "SUBSCRIBED" | Children -> "CHILDREN"
  | Return_special_use -> "SPECIAL-USE"
let list_extended ~reference ~patterns ?(selection=[]) ?(returns=[]) ?status () =
  if patterns=[] then Error "LIST needs at least one pattern"
  else if List.length selection <> List.length (List.sort_uniq compare selection)
       || List.length returns <> List.length (List.sort_uniq compare returns) then
    Error "duplicate LIST option"
  else if List.mem Recursive_match selection &&
          not (List.mem Subscribed selection || List.mem Special_use selection)
  then Error "RECURSIVEMATCH requires a base selection"
  else bind (astring reference) @@ fun reference ->
  let rec encode acc = function
    | [] -> Ok (List.rev acc)
    | name::rest -> bind (astring name) @@ fun name ->
        encode (name::acc) rest in
  bind (encode [] patterns) @@ fun patterns ->
  let patterns=match patterns with [one] -> one | _ ->
    "(" ^ String.concat " " patterns ^ ")" in
  let selection=if selection=[] then "" else " (" ^
    String.concat " " (List.map list_selection selection) ^ ")" in
  let returns=List.map list_return returns in
  let return_options=match status with
    | None -> Ok returns
    | Some items -> bind (status_items items) @@ fun items ->
        Ok (returns @ ["STATUS " ^ items]) in
  bind return_options @@ fun return_options ->
  let returns=if return_options=[] then "" else
    " RETURN (" ^ String.concat " " return_options ^ ")" in
  Ok ("LIST" ^ selection ^ " " ^ reference ^ " " ^ patterns ^ returns)
let status ~mailbox ~items =
  bind (astring mailbox) @@ fun mailbox ->
  bind (status_items items) @@ fun items ->
  Ok ("STATUS " ^ mailbox ^ " " ^ items)
let list_status ~reference ~pattern ~items =
  bind (list ~reference ~pattern) @@ fun listing ->
  bind (status_items items) @@ fun items ->
  Ok (listing ^ " RETURN (STATUS " ^ items ^ ")")

let create mailbox =
  bind (astring mailbox) @@ fun mailbox -> Ok ("CREATE " ^ mailbox)

let delete mailbox =
  bind (astring mailbox) @@ fun mailbox -> Ok ("DELETE " ^ mailbox)

let rename ~old_name ~new_name =
  bind (astring old_name) @@ fun old_name ->
  bind (astring new_name) @@ fun new_name ->
  Ok ("RENAME " ^ old_name ^ " " ^ new_name)

let subscribe mailbox =
  bind (astring mailbox) @@ fun mailbox -> Ok ("SUBSCRIBE " ^ mailbox)

let unsubscribe mailbox =
  bind (astring mailbox) @@ fun mailbox -> Ok ("UNSUBSCRIBE " ^ mailbox)

let finite_set s = match Proto.Uid_set.of_wire s with Ok _ -> true | Error _ -> false

let object_id s =
  let n=String.length s in
  n>0 && n<=255 && String.for_all (function
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '-' -> true
    | _ -> false) s

let select ?(readonly=false) ?(condstore=false) ?qresync ?known_uids
    ?sequence_match ?objectid mailbox =
  bind (astring mailbox) @@ fun mailbox ->
  let q = match qresync with
    | None when known_uids<>None || sequence_match<>None ->
        Error "QRESYNC UID parameters without checkpoint"
    | None -> Ok (if condstore then ["CONDSTORE"] else [])
    | Some (validity, modseq) when validity >= 1L && validity <= 4_294_967_295L
                                  && modseq >= 1L ->
        let known = match known_uids with
          | None -> Ok ""
          | Some s when finite_set s -> Ok (" " ^ s)
          | Some _ -> Error "invalid QRESYNC known UID set" in
        bind known @@ fun known ->
        let matches = match sequence_match with
          | None -> Ok ""
          | Some _ when known_uids=None ->
              Error "QRESYNC sequence match requires known UIDs"
          | Some (seqs,uids) ->
              (match Proto.Uid_set.of_wire seqs,Proto.Uid_set.of_wire uids with
               | Ok seqs_set,Ok uids_set when
                   Proto.Uid_set.cardinality seqs_set =
                   Proto.Uid_set.cardinality uids_set ->
                   Ok (" (" ^ seqs ^ " " ^ uids ^ ")")
               | _ -> Error "invalid QRESYNC sequence match") in
        bind matches @@ fun matches ->
        Ok [Printf.sprintf "QRESYNC (%Ld %Ld%s%s)" validity modseq
              known matches]
    | Some _ -> Error "invalid QRESYNC checkpoint" in
  bind q @@ fun q ->
  let identity=match objectid with
    | None -> Ok []
    | Some (account_id,mailbox_id) when object_id account_id &&
        object_id mailbox_id ->
        Ok ["OBJECTID (MAILBOXID " ^ mailbox_id ^ " ACCOUNTID " ^
          account_id ^ ")"]
    | Some _ -> Error "invalid OBJECTID+ mailbox identity" in
  bind identity @@ fun identity ->
  let params=q@identity in
  let params=if params=[] then "" else
    " (" ^ String.concat " " params ^ ")" in
  Ok ((if readonly then "EXAMINE " else "SELECT ") ^ mailbox ^ params)

let valid_set s =
  let endpoint x =
    if x="*" then true
    else match Int64.of_string_opt x with
      | Some n -> n >= 1L && n <= 4_294_967_295L
      | None -> false in
  s<>"" && List.for_all (fun part ->
    match String.split_on_char ':' part with
    | [x] -> endpoint x
    | [a;b] -> endpoint a && endpoint b
    | _ -> false) (String.split_on_char ',' s)

let valid_items items =
  let known = ["UID";"FLAGS";"MODSEQ";"RFC822.SIZE";"INTERNALDATE";
               "ENVELOPE";"BODYSTRUCTURE";"EMAILID";"THREADID";
               "OBJECTID";
               "BODY[]";"BODY.PEEK[]";"BODY[HEADER]";"BODY.PEEK[HEADER]";
               "BODY[TEXT]";"BODY.PEEK[TEXT]";
               "BINARY[]";"BINARY.PEEK[]";"BINARY.SIZE[]"] in
  items <> [] && List.for_all (fun s -> List.mem (String.uppercase_ascii s) known) items

let partial_range (first,last) =
  let bound n = n<>0L && n<>Int64.min_int &&
    Int64.abs n <= 4_294_967_295L in
  if not (bound first && bound last) ||
     (first<0L)<>(last<0L) then Error "invalid PARTIAL range"
  else Ok (Printf.sprintf "%Ld:%Ld" first last)

let fetch_command ?changedsince ?(vanished=false) ?partial ~set ~items () =
  if not (valid_items items) then Error "invalid FETCH item"
  else if vanished && changedsince=None then
    Error "VANISHED requires CHANGEDSINCE"
  else
    let partial=match partial with
      | None -> Ok []
      | Some range -> bind (partial_range range) @@ fun range ->
          Ok ["PARTIAL " ^ range] in
    bind partial @@ fun partial ->
    let changed=match changedsince with
      | None -> Ok []
      | Some n when n >= 0L -> Ok [Printf.sprintf "CHANGEDSINCE %Ld" n]
      | Some _ -> Error "invalid CHANGEDSINCE" in
    bind changed @@ fun changed ->
    let mods=partial @ changed @ (if vanished then ["VANISHED"] else []) in
    let modifier=if mods=[] then "" else " (" ^ String.concat " " mods ^ ")" in
    Ok ("UID FETCH " ^ set ^ " (" ^ String.concat " " items ^ ")" ^ modifier)

let uid_fetch_mod ?changedsince ?vanished ?partial ~set ~items () =
  if not (valid_set set) then Error "invalid UID set"
  else fetch_command ?changedsince ?vanished ?partial ~set ~items ()

let uid_fetch_saved ?changedsince ?vanished ?partial ~items () =
  fetch_command ?changedsince ?vanished ?partial ~set:"$" ~items ()

let uid_fetch ~set ~items = uid_fetch_mod ~set ~items ()

let binary_section section =
  if List.length section>100 || List.exists (fun n ->
    n<1 || Int64.of_int n>4_294_967_295L) section then
    Error "invalid BINARY section"
  else Ok ("[" ^ String.concat "." (List.map string_of_int section) ^ "]")

let uid_fetch_binary ~set ~section ?partial () =
  if not (valid_set set) then Error "invalid UID set"
  else bind (binary_section section) @@ fun section ->
  let partial=match partial with
    | None -> Ok ""
    | Some (offset,count) when offset>=0L && count>=1L ->
        Ok (Printf.sprintf "<%Ld.%Ld>" offset count)
    | Some _ -> Error "invalid BINARY partial range" in
  bind partial @@ fun partial ->
  Ok ("UID FETCH " ^ set ^ " (UID BINARY.PEEK" ^ section ^ partial ^ ")")

let uid_fetch_binary_size ~set ~section =
  if not (valid_set set) then Error "invalid UID set"
  else bind (binary_section section) @@ fun section ->
  Ok ("UID FETCH " ^ set ^ " (UID BINARY.SIZE" ^ section ^ ")")

let uid_fetch_preview ~set ~lazy_ =
  if not (valid_set set) then Error "invalid UID set"
  else Ok ("UID FETCH " ^ set ^
    (if lazy_ then " (UID PREVIEW (LAZY))" else " (UID PREVIEW)"))

let uid_search ~criterion =
  if contains_control criterion || String.trim criterion="" then Error "invalid SEARCH criterion"
  else Ok ("UID SEARCH " ^ criterion)

let uid_search_save ~criterion =
  bind (uid_search ~criterion) @@ fun _ ->
  Ok ("UID SEARCH RETURN (SAVE COUNT) " ^ criterion)

(* Keep a caller-supplied search inside its group. Full search-key semantics
   remain server-validated, but parentheses cannot escape the fixed command. *)
let grouped_search criterion =
  let n=String.length criterion in
  let rec scan i depth quoted =
    if i=n then depth=0 && not quoted
    else match criterion.[i] with
      | '\\' when quoted ->
          i+1<n && (criterion.[i+1]='\\' || criterion.[i+1]='"') &&
          scan (i+2) depth quoted
      | '"' -> scan (i+1) depth (not quoted)
      | '(' when not quoted -> depth<100 && scan (i+1) (depth+1) quoted
      | ')' when not quoted -> depth>0 && scan (i+1) (depth-1) quoted
      | _ -> scan (i+1) depth quoted in
  bind (uid_search ~criterion) @@ fun _ ->
  if not (scan 0 0 false) then Error "unbalanced saved SEARCH criterion"
  else Ok ("(" ^ criterion ^ ")")

let uid_search_saved ~criterion =
  bind (grouped_search criterion) @@ fun criterion ->
  Ok ("UID SEARCH RETURN (ALL COUNT) UID $ " ^ criterion)

type sort_key = Arrival | Cc | Date | From | Size | Subject | To
type sort_order = Ascending | Descending
type thread_algorithm = Orderedsubject | References

let sort_key = function
  | Arrival -> "ARRIVAL" | Cc -> "CC" | Date -> "DATE" | From -> "FROM"
  | Size -> "SIZE" | Subject -> "SUBJECT" | To -> "TO"

let sort_search ~charset ~criterion =
  if charset="" || String.exists (fun c -> Char.code c>=127) charset then
    Error "invalid SORT/THREAD charset"
  else if String.trim criterion="" then Error "empty SORT/THREAD criterion"
  else bind (astring charset) @@ fun charset ->
  bind (uid_search ~criterion) @@ fun _ -> Ok (charset ^ " " ^ criterion)

let sort_arguments ~keys ~charset ~criterion =
  if keys=[] || List.length keys>100 then Error "SORT requires 1..100 keys"
  else bind (sort_search ~charset ~criterion) @@ fun search ->
  let keys=List.map (fun (key,order) ->
    (match order with Ascending -> "" | Descending -> "REVERSE ") ^
    sort_key key) keys in
  Ok ("(" ^ String.concat " " keys ^ ") " ^ search)

let uid_sort ~keys ~charset ~criterion =
  bind (sort_arguments ~keys ~charset ~criterion) @@ fun arguments ->
  Ok ("UID SORT " ^ arguments)

type sort_return = Min | Max | Count | All | Partial of (int64 * int64)

let uid_sort_extended ~returns ~keys ~charset ~criterion =
  let name = function Min -> "MIN" | Max -> "MAX" | Count -> "COUNT" |
    All -> "ALL" | Partial _ -> "PARTIAL" in
  let names=List.map name returns in
  if List.length names<>List.length (List.sort_uniq String.compare names) then
    Error "duplicate SORT return option"
  else if List.mem "ALL" names && List.mem "PARTIAL" names then
    Error "SORT ALL and PARTIAL are mutually exclusive"
  else if List.exists (function Partial (first,last) ->
    first<1L || last<1L || first>4_294_967_295L || last>4_294_967_295L
    | _ -> false) returns then Error "invalid SORT PARTIAL range"
  else bind (sort_arguments ~keys ~charset ~criterion) @@ fun arguments ->
  let options=List.map (function
    | Partial (first,last) -> Printf.sprintf "PARTIAL %Ld:%Ld" first last
    | option -> name option) returns in
  Ok ("UID SORT RETURN (" ^ String.concat " " options ^ ") " ^ arguments)

let uid_thread ~algorithm ~charset ~criterion =
  bind (sort_search ~charset ~criterion) @@ fun search ->
  let algorithm=match algorithm with
    | Orderedsubject -> "ORDEREDSUBJECT" | References -> "REFERENCES" in
  Ok ("UID THREAD " ^ algorithm ^ " " ^ search)

let uid_search_partial ~range ~criterion =
  bind (partial_range range) @@ fun range ->
  bind (uid_search ~criterion) @@ fun _ ->
  Ok ("UID SEARCH RETURN (PARTIAL " ^ range ^ ") " ^ criterion)

let uid_batches ?range ~size () =
  if size < 500L || size > 4_294_967_295L then
    Error "UIDBATCHES size outside 500..4294967295"
  else match range with
    | None -> Ok (Printf.sprintf "UIDBATCHES %Ld" size)
    | Some (first,last) when first >= 1L && first <= last &&
         last <= 4_294_967_295L &&
         (Int64.succ (Int64.sub last first)) <= Int64.div 100_000L size ->
        Ok (Printf.sprintf "UIDBATCHES %Ld %Ld:%Ld" size first last)
    | Some _ -> Error "invalid UIDBATCHES range or over 100000 messages"

let store_command ?unchangedsince ~set ~operation ~silent ~flags () =
  if (match unchangedsince with Some n -> n < 0L | None -> false) then
    Error "invalid UNCHANGEDSINCE"
  else if not (List.for_all (fun flag ->
    match Mail_flag.Imap_flag.of_wire flag with Ok _ -> true | Error _ -> false) flags)
    then Error "invalid STORE flag"
  else
    let op = match operation with `Add -> "+" | `Remove -> "-" | `Replace -> "" in
    let modifier=match unchangedsince with None -> "" |
      Some n -> Printf.sprintf " (UNCHANGEDSINCE %Ld)" n in
    Ok ("UID STORE " ^ set ^ modifier ^ " " ^ op ^ "FLAGS" ^
        (if silent then ".SILENT" else "") ^ " (" ^
        String.concat " " flags ^ ")")

let uid_store_mod ?unchangedsince ~set ~operation ~silent ~flags () =
  if not (valid_set set) then Error "invalid UID set"
  else store_command ?unchangedsince ~set ~operation ~silent ~flags ()

let uid_store_saved ?unchangedsince ~operation ~silent ~flags () =
  store_command ?unchangedsince ~set:"$" ~operation ~silent ~flags ()

let uid_store ~set ~operation ~silent ~flags =
  uid_store_mod ~set ~operation ~silent ~flags ()

let transfer_command verb ~set ~mailbox =
  bind (astring mailbox) @@ fun mailbox ->
  Ok ("UID " ^ verb ^ " " ^ set ^ " " ^ mailbox)

let uid_copy ~set ~mailbox =
  if not (valid_set set) then Error "invalid UID set"
  else transfer_command "COPY" ~set ~mailbox

let uid_move ~set ~mailbox =
  if not (valid_set set) then Error "invalid UID set"
  else transfer_command "MOVE" ~set ~mailbox

let uid_copy_saved ~mailbox = transfer_command "COPY" ~set:"$" ~mailbox
let uid_move_saved ~mailbox = transfer_command "MOVE" ~set:"$" ~mailbox

let expunge_command set = "UID EXPUNGE " ^ set
let uid_expunge ~set =
  if not (finite_set set) then Error "invalid finite UID set"
  else Ok (expunge_command set)
let uid_expunge_saved = expunge_command "$"

let append_literal_part ~binary ?(non_sync=false) ?(flags=[]) ?internal_date ~size () =
  if size < 0L then Error "negative APPEND size"
  else if not (List.for_all (fun flag ->
    match Mail_flag.Imap_flag.of_wire flag with Ok _ -> true | Error _ -> false) flags)
    then Error "invalid APPEND flag"
  else
    let flags = if flags=[] then "" else " (" ^ String.concat " " flags ^ ")" in
    let date=match internal_date with
      | None -> ""
      | Some date -> " " ^ Internal_date.to_wire date in
    Ok (Printf.sprintf "%s%s %s{%Ld%s}\r\n"
      flags date (if binary then "~" else "") size (if non_sync then "+" else ""))

let append_literal_prefix ~binary ~mailbox ?non_sync ?flags ?internal_date ~size () =
  bind (astring mailbox) @@ fun mailbox ->
  bind (append_literal_part ~binary ?non_sync ?flags ?internal_date ~size ()) @@ fun part ->
  Ok ("APPEND " ^ mailbox ^ part)

let append_part_prefix ?non_sync ?flags ?internal_date ~size () =
  append_literal_part ~binary:false ?non_sync ?flags ?internal_date ~size ()

let append_prefix ~mailbox ?non_sync ?flags ?internal_date ~size () =
  append_literal_prefix ~binary:false ~mailbox ?non_sync ?flags ?internal_date ~size ()

let append_binary_prefix ~mailbox ?non_sync ?flags ?internal_date ~size () =
  append_literal_prefix ~binary:true ~mailbox ?non_sync ?flags ?internal_date ~size ()
