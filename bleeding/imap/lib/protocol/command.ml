type error = string
let capability = "CAPABILITY"
let noop = "NOOP"
let logout = "LOGOUT"
let idle = "IDLE"
let done_idle = "DONE"
let get_jmap_access = "GETJMAPACCESS"
let notify_none = "NOTIFY NONE"

let ( let* ) = Result.bind

let is_digit c = c >= '0' && c <= '9'

let contains_control s =
  String.exists (fun c -> Char.code c < 0x20 || Char.code c = 0x7f) s

let has_duplicates compare items =
  List.length items <> List.length (List.sort_uniq compare items)

let atom s =
  s <> "" && String.for_all (fun c ->
    let code = Char.code c in
    code > 0x20 && code < 0x7f && not (String.contains "(){%*\\\"]" c)) s

(* RFC 9051 quoted strings carry UTF-8 text and no CR, LF or NUL. *)
let quote s =
  if contains_control s || not (String.is_valid_utf_8 s) then
    Error "invalid quoted string"
  else
    let b = Buffer.create (String.length s+2) in
    Buffer.add_char b '"';
    String.iter (fun c ->
      if c='\\' || c='"' then Buffer.add_char b '\\';
      Buffer.add_char b c) s;
    Buffer.add_char b '"'; Ok (Buffer.contents b)

let astring s = if atom s then Ok s else quote s

let astrings names =
  let rec encode acc = function
    | [] -> Ok (List.rev acc)
    | name::rest -> let* name = astring name in encode (name::acc) rest in
  encode [] names

let one_or_list = function
  | [one] -> one
  | items -> "(" ^ String.concat " " items ^ ")"

let nonempty_astring what s =
  if s="" then Error ("empty " ^ what) else astring s

let operation_prefix = function `Add -> "+" | `Remove -> "-" | `Replace -> ""

let rights s =
  String.for_all (function 'a'..'z' | '0'..'9' -> true | _ -> false) s
let getacl ~mailbox =
  let* mailbox = astring mailbox in Ok ("GETACL " ^ mailbox)
let myrights ~mailbox =
  let* mailbox = astring mailbox in Ok ("MYRIGHTS " ^ mailbox)
let listrights ~mailbox ~identifier =
  let* mailbox = astring mailbox in
  let* identifier = nonempty_astring "ACL identifier" identifier in
  Ok ("LISTRIGHTS " ^ mailbox ^ " " ^ identifier)
let setacl ~mailbox ~identifier ~operation ~rights:grant =
  if not (rights grant) || (grant="" && operation<>`Replace) then
    Error "invalid ACL rights"
  else
    let* mailbox = astring mailbox in
    let* identifier = nonempty_astring "ACL identifier" identifier in
    let* grant = astring (operation_prefix operation ^ grant) in
    Ok ("SETACL " ^ mailbox ^ " " ^ identifier ^ " " ^ grant)
let deleteacl ~mailbox ~identifier =
  let* mailbox = astring mailbox in
  let* identifier = nonempty_astring "ACL identifier" identifier in
  Ok ("DELETEACL " ^ mailbox ^ " " ^ identifier)

let getquota ~root =
  let* root = astring root in Ok ("GETQUOTA " ^ root)
let getquotaroot ~mailbox =
  let* mailbox = astring mailbox in Ok ("GETQUOTAROOT " ^ mailbox)
let setquota ~root ~limits =
  if not (List.for_all (fun (name,limit) -> atom name && limit >= 0L) limits)
  then Error "invalid quota limit"
  else if has_duplicates String.compare
      (List.map (fun (n,_) -> String.uppercase_ascii n) limits) then
    Error "duplicate quota resource"
  else
    let* root = astring root in
    let pairs=List.map (fun (n,v) ->
      String.uppercase_ascii n ^ " " ^ Int64.to_string v) limits in
    Ok ("SETQUOTA " ^ root ^ " (" ^ String.concat " " pairs ^ ")")

type metadata_depth = Zero | One | Infinity

(* RFC 5464 section 3.2 entry names. *)
let metadata_entry s =
  let n=String.length s in
  let rec no_double_slash i =
    i+1 >= n || ((s.[i] <> '/' || s.[i+1] <> '/') && no_double_slash (i+1)) in
  n>1 && s.[0]='/' && s.[n-1]<>'/' &&
  String.for_all (fun c ->
    c >= ' ' && c < '\127' && c <> '*' && c <> '%') s &&
  no_double_slash 0

let getmetadata ~mailbox ~entries ?maxsize ?depth () =
  if entries=[] || not (List.for_all metadata_entry entries) then
    Error "invalid METADATA entries"
  else if (match maxsize with
      | Some n -> n<0L || n>4_294_967_295L | None -> false) then
    Error "invalid METADATA MAXSIZE"
  else
    let* mailbox = astring mailbox in
    let options=(match maxsize with None -> [] | Some n ->
      ["MAXSIZE " ^ Int64.to_string n]) @
      (match depth with None -> [] | Some Zero -> ["DEPTH 0"] |
        Some One -> ["DEPTH 1"] | Some Infinity -> ["DEPTH infinity"]) in
    let options=if options=[] then "" else
      " (" ^ String.concat " " options ^ ")" in
    let* entries = astrings entries in
    Ok ("GETMETADATA" ^ options ^ " " ^ mailbox ^ " " ^ one_or_list entries)
let setmetadata ~mailbox ~values =
  if values=[] || not (List.for_all (fun (key,_) -> metadata_entry key) values)
  then Error "invalid METADATA values"
  else if has_duplicates String.compare
      (List.map (fun (key,_) -> String.lowercase_ascii key) values) then
    Error "duplicate METADATA entry"
  else
    let* mailbox = astring mailbox in
    let rec pairs acc = function
      | [] -> Ok (List.rev acc)
      | (key,value)::rest ->
          let* key = astring key in
          let* value = match value with None -> Ok "NIL" | Some v -> quote v in
          pairs ((key ^ " " ^ value)::acc) rest in
    let* values = pairs [] values in
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
        let* names = astrings names in
        Ok ((match filter with Subtree _ -> "subtree " | _ -> "mailboxes ") ^
            one_or_list names)
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
          else if has_duplicates compare events then
            Error "duplicate NOTIFY event"
          else if (match filter with Selected|Selected_delayed ->
              List.exists (function Message_new|Message_expunge|
                Flag_change|Annotation_change -> false | _ -> true) events
              | _ -> false) then Error "invalid selected NOTIFY event"
          else
            let* filter = notify_filter filter in
            let events=if events=[] then "NONE" else
              "(" ^ String.concat " " (List.map notify_event events) ^ ")" in
            encode (("(" ^ filter ^ " " ^ events ^ ")")::acc) rest in
    let* groups = encode [] groups in
    Ok ("NOTIFY SET" ^ (if status then " STATUS" else "") ^ " " ^
        String.concat " " groups)

let login ~username ~password =
  let* user = quote username in
  let* pass = quote password in
  Ok ("LOGIN " ^ user ^ " " ^ pass)

let list ~reference ~pattern =
  let* reference = astring reference in
  let* pattern = astring pattern in
  Ok ("LIST " ^ reference ^ " " ^ pattern)

let lsub ~reference ~pattern =
  let* reference = astring reference in
  let* pattern = astring pattern in
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
  else if has_duplicates compare selection || has_duplicates compare returns
  then Error "duplicate LIST option"
  else if List.mem Recursive_match selection &&
          not (List.mem Subscribed selection || List.mem Special_use selection)
  then Error "RECURSIVEMATCH requires a base selection"
  else
    let* reference = astring reference in
    let* patterns = astrings patterns in
    let selection=if selection=[] then "" else " (" ^
      String.concat " " (List.map list_selection selection) ^ ")" in
    let returns=List.map list_return returns in
    let* return_options = match status with
      | None -> Ok returns
      | Some items ->
          let* items = status_items items in
          Ok (returns @ ["STATUS " ^ items]) in
    let returns=if return_options=[] then "" else
      " RETURN (" ^ String.concat " " return_options ^ ")" in
    Ok ("LIST" ^ selection ^ " " ^ reference ^ " " ^ one_or_list patterns ^
        returns)
let status ~mailbox ~items =
  let* mailbox = astring mailbox in
  let* items = status_items items in
  Ok ("STATUS " ^ mailbox ^ " " ^ items)
let list_status ~reference ~pattern ~items =
  list_extended ~reference ~patterns:[pattern] ~status:items ()

let create mailbox =
  let* mailbox = astring mailbox in Ok ("CREATE " ^ mailbox)

let delete mailbox =
  let* mailbox = astring mailbox in Ok ("DELETE " ^ mailbox)

let rename ~old_name ~new_name =
  let* old_name = astring old_name in
  let* new_name = astring new_name in
  Ok ("RENAME " ^ old_name ^ " " ^ new_name)

let subscribe mailbox =
  let* mailbox = astring mailbox in Ok ("SUBSCRIBE " ^ mailbox)

let unsubscribe mailbox =
  let* mailbox = astring mailbox in Ok ("UNSUBSCRIBE " ^ mailbox)

let uid_set ?(allow_star=true) what s =
  match Proto.Uid_set.of_wire ~allow_star s with
  | Ok set -> Ok set
  | Error e -> Error ("invalid " ^ what ^ ": " ^ e)

let object_id s =
  let n=String.length s in
  n>0 && n<=255 && String.for_all (function
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '-' -> true
    | _ -> false) s

let select ?(readonly=false) ?(condstore=false) ?qresync ?known_uids
    ?sequence_match ?objectid mailbox =
  let* mailbox = astring mailbox in
  let condstore=if condstore then ["CONDSTORE"] else [] in
  let* q = match qresync with
    | None when known_uids<>None || sequence_match<>None ->
        Error "QRESYNC UID parameters without checkpoint"
    | None -> Ok condstore
    | Some (validity, modseq) when
        Result.is_ok (Proto.Uidvalidity.of_int64 validity) &&
        Result.is_ok (Proto.Modseq.of_int64 modseq) ->
        let* known = match known_uids with
          | None -> Ok ""
          | Some s ->
              let* _ = uid_set ~allow_star:false "QRESYNC known UID set" s in
              Ok (" " ^ s) in
        let* matches = match sequence_match with
          | None -> Ok ""
          | Some _ when known_uids=None ->
              Error "QRESYNC sequence match requires known UIDs"
          | Some (seqs,uids) ->
              let what="QRESYNC sequence match" in
              let* seqs_set = uid_set ~allow_star:false what seqs in
              let* uids_set = uid_set ~allow_star:false what uids in
              if Proto.Uid_set.cardinality seqs_set <>
                 Proto.Uid_set.cardinality uids_set
              then Error "QRESYNC sequence match sets differ in size"
              else Ok (" (" ^ seqs ^ " " ^ uids ^ ")") in
        Ok (condstore @ [Printf.sprintf "QRESYNC (%Ld %Ld%s%s)" validity modseq
                           known matches])
    | Some _ -> Error "invalid QRESYNC checkpoint" in
  let* identity = match objectid with
    | None -> Ok []
    | Some (account_id,mailbox_id) when object_id account_id &&
        object_id mailbox_id ->
        Ok ["OBJECTID (MAILBOXID " ^ mailbox_id ^ " ACCOUNTID " ^
          account_id ^ ")"]
    | Some _ -> Error "invalid OBJECTID+ mailbox identity" in
  let params=q@identity in
  let params=if params=[] then "" else
    " (" ^ String.concat " " params ^ ")" in
  Ok ((if readonly then "EXAMINE " else "SELECT ") ^ mailbox ^ params)

let valid_set s =
  let* _ = uid_set "UID set" s in Ok s

let valid_items items =
  let known = ["UID";"FLAGS";"MODSEQ";"RFC822.SIZE";"INTERNALDATE";
               "ENVELOPE";"BODYSTRUCTURE";"EMAILID";"THREADID";
               "OBJECTID";
               "BODY[]";"BODY.PEEK[]";"BODY[HEADER]";"BODY.PEEK[HEADER]";
               "BODY[TEXT]";"BODY.PEEK[TEXT]";
               "BINARY[]";"BINARY.PEEK[]";"BINARY.SIZE[]"] in
  items <> [] &&
  List.for_all (fun s -> List.mem (String.uppercase_ascii s) known) items

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
    let* partial = match partial with
      | None -> Ok []
      | Some range ->
          let* range = partial_range range in Ok ["PARTIAL " ^ range] in
    let* changed = match changedsince with
      | None -> Ok []
      | Some n when n >= 0L -> Ok [Printf.sprintf "CHANGEDSINCE %Ld" n]
      | Some _ -> Error "invalid CHANGEDSINCE" in
    let mods=partial @ changed @ (if vanished then ["VANISHED"] else []) in
    let modifier=if mods=[] then "" else " (" ^ String.concat " " mods ^ ")" in
    Ok ("UID FETCH " ^ set ^ " (" ^ String.concat " " items ^ ")" ^ modifier)

let uid_fetch_mod ?changedsince ?vanished ?partial ~set ~items () =
  let* set = valid_set set in
  fetch_command ?changedsince ?vanished ?partial ~set ~items ()

let uid_fetch_saved ?changedsince ?vanished ?partial ~items () =
  fetch_command ?changedsince ?vanished ?partial ~set:"$" ~items ()

let uid_fetch ~set ~items = uid_fetch_mod ~set ~items ()

let binary_section section =
  if List.length section>100 || List.exists (fun n ->
    n<1 || Int64.of_int n>4_294_967_295L) section then
    Error "invalid BINARY section"
  else Ok ("[" ^ String.concat "." (List.map string_of_int section) ^ "]")

let uid_fetch_binary ~set ~section ?partial () =
  let* set = valid_set set in
  let* section = binary_section section in
  let* partial = match partial with
    | None -> Ok ""
    | Some (offset,count) when offset>=0L && count>=1L ->
        Ok (Printf.sprintf "<%Ld.%Ld>" offset count)
    | Some _ -> Error "invalid BINARY partial range" in
  Ok ("UID FETCH " ^ set ^ " (UID BINARY.PEEK" ^ section ^ partial ^ ")")

let uid_fetch_binary_size ~set ~section =
  let* set = valid_set set in
  let* section = binary_section section in
  Ok ("UID FETCH " ^ set ^ " (UID BINARY.SIZE" ^ section ^ ")")

let uid_fetch_preview ~set ~lazy_ =
  let* set = valid_set set in
  Ok ("UID FETCH " ^ set ^
    (if lazy_ then " (UID PREVIEW (LAZY))" else " (UID PREVIEW)"))

(* A trailing "{n}" or "{n+}" would make the server read the next line as
   literal data and desynchronise the session. *)
let ends_with_literal_marker s =
  let s=String.trim s in
  let n=String.length s in
  n >= 3 && s.[n-1] = '}' &&
  let last=if s.[n-2] = '+' then n-3 else n-2 in
  let rec back i = if i >= 0 && is_digit s.[i] then back (i-1) else i in
  let k=back last in
  k < last && k >= 0 && s.[k] = '{'

let criterion s =
  if contains_control s || String.trim s="" then
    Error "invalid SEARCH criterion"
  else if ends_with_literal_marker s then
    Error "SEARCH criterion ends in a literal marker"
  else Ok s

let uid_search ~criterion:c =
  let* c = criterion c in Ok ("UID SEARCH " ^ c)

let uid_search_save ~criterion:c =
  let* c = criterion c in Ok ("UID SEARCH RETURN (SAVE COUNT) " ^ c)

(* Keep a caller-supplied search inside its group. Full search-key semantics
   remain server-validated, but parentheses cannot escape the fixed command. *)
let grouped_search c =
  let n=String.length c in
  let rec scan i depth quoted =
    if i=n then depth=0 && not quoted
    else match c.[i] with
      | '\\' when quoted ->
          i+1<n && (c.[i+1]='\\' || c.[i+1]='"') && scan (i+2) depth quoted
      | '"' -> scan (i+1) depth (not quoted)
      | '(' when not quoted -> depth<100 && scan (i+1) (depth+1) quoted
      | ')' when not quoted -> depth>0 && scan (i+1) (depth-1) quoted
      | _ -> scan (i+1) depth quoted in
  let* c = criterion c in
  if not (scan 0 0 false) then Error "unbalanced saved SEARCH criterion"
  else Ok ("(" ^ c ^ ")")

let uid_search_saved ~criterion =
  let* criterion = grouped_search criterion in
  Ok ("UID SEARCH RETURN (ALL COUNT) UID $ " ^ criterion)

type sort_key = Arrival | Cc | Date | From | Size | Subject | To
type sort_order = Ascending | Descending
type thread_algorithm = Orderedsubject | References

let sort_key = function
  | Arrival -> "ARRIVAL" | Cc -> "CC" | Date -> "DATE" | From -> "FROM"
  | Size -> "SIZE" | Subject -> "SUBJECT" | To -> "TO"

let sort_search ~charset ~criterion:c =
  if charset="" || String.exists (fun c -> Char.code c>=127) charset then
    Error "invalid SORT/THREAD charset"
  else
    let* charset = astring charset in
    let* c = criterion c in
    Ok (charset ^ " " ^ c)

let sort_arguments ~keys ~charset ~criterion =
  if keys=[] || List.length keys>100 then Error "SORT requires 1..100 keys"
  else
    let* search = sort_search ~charset ~criterion in
    let keys=List.map (fun (key,order) ->
      (match order with Ascending -> "" | Descending -> "REVERSE ") ^
      sort_key key) keys in
    Ok ("(" ^ String.concat " " keys ^ ") " ^ search)

let uid_sort ~keys ~charset ~criterion =
  let* arguments = sort_arguments ~keys ~charset ~criterion in
  Ok ("UID SORT " ^ arguments)

type sort_return = Min | Max | Count | All | Partial of (int64 * int64)

let uid_sort_extended ~returns ~keys ~charset ~criterion =
  let name = function Min -> "MIN" | Max -> "MAX" | Count -> "COUNT" |
    All -> "ALL" | Partial _ -> "PARTIAL" in
  let names=List.map name returns in
  if has_duplicates String.compare names then
    Error "duplicate SORT return option"
  else if List.mem "ALL" names && List.mem "PARTIAL" names then
    Error "SORT ALL and PARTIAL are mutually exclusive"
  else if List.exists (function Partial (first,last) ->
    first<1L || last<1L || first>4_294_967_295L || last>4_294_967_295L
    | _ -> false) returns then Error "invalid SORT PARTIAL range"
  else
    let* arguments = sort_arguments ~keys ~charset ~criterion in
    let options=List.map (function
      | Partial (first,last) -> Printf.sprintf "PARTIAL %Ld:%Ld" first last
      | option -> name option) returns in
    Ok ("UID SORT RETURN (" ^ String.concat " " options ^ ") " ^ arguments)

let uid_thread ~algorithm ~charset ~criterion =
  let* search = sort_search ~charset ~criterion in
  let algorithm=match algorithm with
    | Orderedsubject -> "ORDEREDSUBJECT" | References -> "REFERENCES" in
  Ok ("UID THREAD " ^ algorithm ^ " " ^ search)

let uid_search_partial ~range ~criterion:c =
  let* range = partial_range range in
  let* c = criterion c in
  Ok ("UID SEARCH RETURN (PARTIAL " ^ range ^ ") " ^ c)

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

let flag_list what flags =
  let rec check = function
    | [] -> Ok ()
    | flag::rest ->
        (match Mail_flag.Imap_flag.of_wire flag with
         | Ok _ -> check rest
         | Error e -> Error ("invalid " ^ what ^ " flag: " ^ e)) in
  check flags

let store_command ?unchangedsince ~set ~operation ~silent ~flags () =
  if (match unchangedsince with Some n -> n < 0L | None -> false) then
    Error "invalid UNCHANGEDSINCE"
  else
    let* () = flag_list "STORE" flags in
    let modifier=match unchangedsince with None -> "" |
      Some n -> Printf.sprintf " (UNCHANGEDSINCE %Ld)" n in
    Ok ("UID STORE " ^ set ^ modifier ^ " " ^ operation_prefix operation ^
        "FLAGS" ^ (if silent then ".SILENT" else "") ^ " (" ^
        String.concat " " flags ^ ")")

let uid_store_mod ?unchangedsince ~set ~operation ~silent ~flags () =
  let* set = valid_set set in
  store_command ?unchangedsince ~set ~operation ~silent ~flags ()

let uid_store_saved ?unchangedsince ~operation ~silent ~flags () =
  store_command ?unchangedsince ~set:"$" ~operation ~silent ~flags ()

let uid_store ~set ~operation ~silent ~flags =
  uid_store_mod ~set ~operation ~silent ~flags ()

let transfer_command verb ~set ~mailbox =
  let* mailbox = astring mailbox in
  Ok ("UID " ^ verb ^ " " ^ set ^ " " ^ mailbox)

let uid_copy ~set ~mailbox =
  let* set = valid_set set in transfer_command "COPY" ~set ~mailbox

let uid_move ~set ~mailbox =
  let* set = valid_set set in transfer_command "MOVE" ~set ~mailbox

let uid_copy_saved ~mailbox = transfer_command "COPY" ~set:"$" ~mailbox
let uid_move_saved ~mailbox = transfer_command "MOVE" ~set:"$" ~mailbox

let expunge_command set = "UID EXPUNGE " ^ set
let uid_expunge ~set =
  let* set = valid_set set in Ok (expunge_command set)
let uid_expunge_saved = expunge_command "$"

let append_literal_part ~binary ?(non_sync=false) ?(flags=[]) ?internal_date
    ~size () =
  if size < 0L then Error "negative APPEND size"
  else
    let* () = flag_list "APPEND" flags in
    let flags = if flags=[] then "" else " (" ^ String.concat " " flags ^ ")" in
    let date=match internal_date with
      | None -> ""
      | Some date -> " " ^ Internal_date.to_wire date in
    Ok (Printf.sprintf "%s%s %s{%Ld%s}\r\n"
      flags date (if binary then "~" else "") size (if non_sync then "+" else ""))

let append_literal_prefix ~binary ~mailbox ?non_sync ?flags ?internal_date
    ~size () =
  let* mailbox = astring mailbox in
  let* part =
    append_literal_part ~binary ?non_sync ?flags ?internal_date ~size () in
  Ok ("APPEND " ^ mailbox ^ part)

let append_part_prefix ?non_sync ?flags ?internal_date ~size () =
  append_literal_part ~binary:false ?non_sync ?flags ?internal_date ~size ()

let append_prefix ~mailbox ?non_sync ?flags ?internal_date ~size () =
  append_literal_prefix ~binary:false ~mailbox ?non_sync ?flags ?internal_date
    ~size ()

let append_binary_prefix ~mailbox ?non_sync ?flags ?internal_date ~size () =
  append_literal_prefix ~binary:true ~mailbox ?non_sync ?flags ?internal_date
    ~size ()
