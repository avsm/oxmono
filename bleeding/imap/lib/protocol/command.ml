type error = { command : string; argument : string option; reason : string }

let to_string e = match e.argument with
  | None -> e.command ^ ": " ^ e.reason
  | Some argument -> e.command ^ " " ^ argument ^ ": " ^ e.reason
let pp ppf e = Format.pp_print_string ppf (to_string e)

let capability = "CAPABILITY"
let noop = "NOOP"
let logout = "LOGOUT"
let idle = "IDLE"
let done_idle = "DONE"
let get_jmap_access = "GETJMAPACCESS"
let notify_none = "NOTIFY NONE"

let ( let* ) = Result.bind

let fail ?argument command reason = Error {command; argument; reason}

(* Lifts a helper's reason into an error naming [command] and [argument]. *)
let arg command argument = function
  | Ok v -> Ok v
  | Error reason -> Error {command; argument=Some argument; reason}

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
    Error "control character or invalid UTF-8"
  else
    let b = Buffer.create (String.length s+2) in
    Buffer.add_char b '"';
    String.iter (fun c ->
      if c='\\' || c='"' then Buffer.add_char b '\\';
      Buffer.add_char b c) s;
    Buffer.add_char b '"'; Ok (Buffer.contents b)

let astring s = if atom s then Ok s else quote s

let enable capabilities =
  let tokens = List.map Capability.to_wire capabilities in
  if tokens = [] then fail "ENABLE" "requires a capability"
  else if not (List.for_all atom tokens) then
    fail "ENABLE" "capability is not an atom"
  else Ok ("ENABLE " ^ String.concat " " tokens)

let astrings names =
  let rec encode acc = function
    | [] -> Ok (List.rev acc)
    | name::rest -> let* name = astring name in encode (name::acc) rest in
  encode [] names

let one_or_list = function
  | [one] -> one
  | items -> "(" ^ String.concat " " items ^ ")"

let nonempty_astring s = if s="" then Error "empty" else astring s

let operation_prefix = function `Add -> "+" | `Remove -> "-" | `Replace -> ""

let rights s =
  String.for_all (function 'a'..'z' | '0'..'9' -> true | _ -> false) s
let mailbox_command verb mailbox =
  let* mailbox = arg verb "mailbox" (astring mailbox) in
  Ok (verb ^ " " ^ mailbox)

let getacl ~mailbox = mailbox_command "GETACL" mailbox
let myrights ~mailbox = mailbox_command "MYRIGHTS" mailbox
let listrights ~mailbox ~identifier =
  let* command = mailbox_command "LISTRIGHTS" mailbox in
  let* identifier =
    arg "LISTRIGHTS" "identifier" (nonempty_astring identifier) in
  Ok (command ^ " " ^ identifier)
let setacl ~mailbox ~identifier ~operation ~rights:grant =
  if not (rights grant) || (grant="" && operation<>`Replace) then
    fail ~argument:"rights" "SETACL" "invalid ACL rights"
  else
    let* command = mailbox_command "SETACL" mailbox in
    let* identifier = arg "SETACL" "identifier" (nonempty_astring identifier) in
    let* grant =
      arg "SETACL" "rights" (astring (operation_prefix operation ^ grant)) in
    Ok (command ^ " " ^ identifier ^ " " ^ grant)
let deleteacl ~mailbox ~identifier =
  let* command = mailbox_command "DELETEACL" mailbox in
  let* identifier =
    arg "DELETEACL" "identifier" (nonempty_astring identifier) in
  Ok (command ^ " " ^ identifier)

let getquota ~root =
  let* root = arg "GETQUOTA" "root" (astring root) in Ok ("GETQUOTA " ^ root)
let getquotaroot ~mailbox = mailbox_command "GETQUOTAROOT" mailbox
let setquota ~root ~limits =
  if not (List.for_all (fun (name,limit) -> atom name && limit >= 0L) limits)
  then fail ~argument:"limits" "SETQUOTA" "invalid quota limit"
  else if has_duplicates String.compare
      (List.map (fun (n,_) -> String.uppercase_ascii n) limits) then
    fail ~argument:"limits" "SETQUOTA" "duplicate quota resource"
  else
    let* root = arg "SETQUOTA" "root" (astring root) in
    let pairs=List.map (fun (n,v) ->
      String.uppercase_ascii n ^ " " ^ Int64.to_string v) limits in
    Ok ("SETQUOTA " ^ root ^ " (" ^ String.concat " " pairs ^ ")")

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
  let command="GETMETADATA" in
  if entries=[] || not (List.for_all metadata_entry entries) then
    fail ~argument:"entries" command "invalid METADATA entries"
  else if (match maxsize with
      | Some n -> n<0L || n>4_294_967_295L | None -> false) then
    fail ~argument:"maxsize" command "MAXSIZE outside 0..4294967295"
  else
    let* mailbox = arg command "mailbox" (astring mailbox) in
    let options=(match maxsize with None -> [] | Some n ->
      ["MAXSIZE " ^ Int64.to_string n]) @
      (match depth with None -> [] | Some depth ->
        ["DEPTH " ^ Metadata.depth_to_wire depth]) in
    let options=if options=[] then "" else
      " (" ^ String.concat " " options ^ ")" in
    let* entries = arg command "entries" (astrings entries) in
    Ok ("GETMETADATA" ^ options ^ " " ^ mailbox ^ " " ^ one_or_list entries)
let setmetadata ~mailbox ~values =
  let command="SETMETADATA" in
  if values=[] || not (List.for_all (fun (key,_) -> metadata_entry key) values)
  then fail ~argument:"values" command "invalid METADATA entries"
  else if has_duplicates String.compare
      (List.map (fun (key,_) -> String.lowercase_ascii key) values) then
    fail ~argument:"values" command "duplicate METADATA entry"
  else
    let* mailbox = arg command "mailbox" (astring mailbox) in
    let rec pairs acc = function
      | [] -> Ok (List.rev acc)
      | (key,value)::rest ->
          let* key = astring key in
          let* value = match value with None -> Ok "NIL" | Some v -> quote v in
          pairs ((key ^ " " ^ value)::acc) rest in
    let* values = arg command "values" (pairs [] values) in
    Ok ("SETMETADATA " ^ mailbox ^ " (" ^ String.concat " " values ^ ")")

let notify_filter : Notify.filter -> _ = function
  | Selected -> Ok "selected" | Selected_delayed -> Ok "selected-delayed"
  | Inboxes -> Ok "inboxes" | Personal -> Ok "personal"
  | Subscribed -> Ok "subscribed"
  | Subtree names | Mailboxes names as filter ->
      if names=[] then Error "empty NOTIFY mailbox selector"
      else
        let* names = astrings names in
        Ok ((match filter with Subtree _ -> "subtree " | _ -> "mailboxes ") ^
            one_or_list names)
let notify_set ?(status=false) ~(groups : Notify.group list) () =
  let invalid = fail ~argument:"groups" "NOTIFY SET" in
  if groups=[] then invalid "needs an event group"
  else if List.length (List.filter (fun (filter,_) ->
    Notify.is_selected filter) groups)>1 then
    invalid "multiple selected NOTIFY groups"
  else
    let rec encode acc = function
      | [] -> Ok (List.rev acc)
      | (filter,events)::rest ->
          let has event=List.exists (Notify.equal_event event) events in
          if (has Message_new)<>(has Message_expunge) ||
             (has Flag_change && not (has Message_new)) then
            Error "NOTIFY message events must be paired"
          else if has_duplicates compare events then
            Error "duplicate NOTIFY event"
          else if Notify.is_selected filter && List.exists (function
              | Notify.Message_new | Message_expunge | Flag_change
              | Annotation_change -> false
              | _ -> true) events then Error "invalid selected NOTIFY event"
          else
            let* filter = notify_filter filter in
            let events=if events=[] then "NONE" else "(" ^
              String.concat " " (List.map Notify.event_to_wire events) ^
              ")" in
            encode (("(" ^ filter ^ " " ^ events ^ ")")::acc) rest in
    let* groups = arg "NOTIFY SET" "groups" (encode [] groups) in
    Ok ("NOTIFY SET" ^ (if status then " STATUS" else "") ^ " " ^
        String.concat " " groups)

let login ~username ~password =
  let* user = arg "LOGIN" "username" (quote username) in
  let* pass = arg "LOGIN" "password" (quote password) in
  Ok ("LOGIN " ^ user ^ " " ^ pass)

let list_command verb ~reference ~pattern =
  let* reference = arg verb "reference" (astring reference) in
  let* pattern = arg verb "pattern" (astring pattern) in
  Ok (verb ^ " " ^ reference ^ " " ^ pattern)

let list ~reference ~pattern = list_command "LIST" ~reference ~pattern
let lsub ~reference ~pattern = list_command "LSUB" ~reference ~pattern

let namespace = "NAMESPACE"

let status_items items =
  if items=[] then Error "needs at least one item"
  else Ok ("(" ^ String.concat " " (List.map Status_item.to_wire items) ^ ")")
let list_extended ~reference ~patterns
    ?(selection : Mailbox_list.selection list = [])
    ?(returns : Mailbox_list.return list = []) ?status () =
  let command="LIST" in
  let has option = List.exists (Mailbox_list.equal_selection option) in
  if patterns=[] then
    fail ~argument:"patterns" command "needs at least one pattern"
  else if has_duplicates compare selection then
    fail ~argument:"selection" command "duplicate LIST option"
  else if has_duplicates compare returns then
    fail ~argument:"returns" command "duplicate LIST option"
  else if has Recursive_match selection &&
          not (has Subscribed selection || has Special_use selection)
  then fail ~argument:"selection" command
      "RECURSIVEMATCH requires a base selection"
  else
    let* reference = arg command "reference" (astring reference) in
    let* patterns = arg command "patterns" (astrings patterns) in
    let selection=if selection=[] then "" else " (" ^
      String.concat " " (List.map Mailbox_list.selection_to_wire selection) ^
      ")" in
    let returns=List.map Mailbox_list.return_to_wire returns in
    let* return_options = match status with
      | None -> Ok returns
      | Some items ->
          let* items = arg command "status" (status_items items) in
          Ok (returns @ ["STATUS " ^ items]) in
    let returns=if return_options=[] then "" else
      " RETURN (" ^ String.concat " " return_options ^ ")" in
    Ok ("LIST" ^ selection ^ " " ^ reference ^ " " ^ one_or_list patterns ^
        returns)
let status ~mailbox ~items =
  let* command = mailbox_command "STATUS" mailbox in
  let* items = arg "STATUS" "items" (status_items items) in
  Ok (command ^ " " ^ items)
let list_status ~reference ~pattern ~items =
  list_extended ~reference ~patterns:[pattern] ~status:items ()

let create ~mailbox = mailbox_command "CREATE" mailbox
let delete ~mailbox = mailbox_command "DELETE" mailbox

let rename ~old_name ~new_name =
  let* old_name = arg "RENAME" "old_name" (astring old_name) in
  let* new_name = arg "RENAME" "new_name" (astring new_name) in
  Ok ("RENAME " ^ old_name ^ " " ^ new_name)

let subscribe ~mailbox = mailbox_command "SUBSCRIBE" mailbox
let unsubscribe ~mailbox = mailbox_command "UNSUBSCRIBE" mailbox

let uid_set ?(allow_star=true) s =
  match Uid_set.of_wire ~allow_star s with
  | Ok set -> Ok set
  | Error e -> Error ("invalid UID set: " ^ e)

let object_id s =
  let n=String.length s in
  n>0 && n<=255 && String.for_all (function
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '-' -> true
    | _ -> false) s

let select ?(readonly=false) ?(condstore=false) ?qresync ?known_uids
    ?sequence_match ?objectid mailbox =
  let command=if readonly then "EXAMINE" else "SELECT" in
  let* mailbox = arg command "mailbox" (astring mailbox) in
  let condstore=if condstore then ["CONDSTORE"] else [] in
  let* q = match qresync with
    | None when known_uids<>None || sequence_match<>None ->
        fail ~argument:"qresync" command
          "QRESYNC UID parameters without checkpoint"
    | None -> Ok condstore
    | Some (validity, modseq) when
        Result.is_ok (Uidvalidity.of_int64 validity) &&
        Result.is_ok (Modseq.of_int64 modseq) ->
        let* known = match known_uids with
          | None -> Ok ""
          | Some s ->
              let* _ =
                arg command "known_uids" (uid_set ~allow_star:false s) in
              Ok (" " ^ s) in
        let* matches = match sequence_match with
          | None -> Ok ""
          | Some _ when known_uids=None ->
              fail ~argument:"sequence_match" command
                "QRESYNC sequence match requires known UIDs"
          | Some (seqs,uids) ->
              let set s =
                arg command "sequence_match" (uid_set ~allow_star:false s) in
              let* seqs_set = set seqs in
              let* uids_set = set uids in
              if Uid_set.cardinality seqs_set <>
                 Uid_set.cardinality uids_set
              then fail ~argument:"sequence_match" command
                  "QRESYNC sequence match sets differ in size"
              else Ok (" (" ^ seqs ^ " " ^ uids ^ ")") in
        Ok (condstore @ [Printf.sprintf "QRESYNC (%Ld %Ld%s%s)" validity modseq
                           known matches])
    | Some _ ->
        fail ~argument:"qresync" command "invalid QRESYNC checkpoint" in
  let* identity = match objectid with
    | None -> Ok []
    | Some (account_id,mailbox_id) when object_id account_id &&
        object_id mailbox_id ->
        Ok ["OBJECTID (MAILBOXID " ^ mailbox_id ^ " ACCOUNTID " ^
          account_id ^ ")"]
    | Some _ ->
        fail ~argument:"objectid" command
          "invalid OBJECTID+ mailbox identity" in
  let params=q@identity in
  let params=if params=[] then "" else
    " (" ^ String.concat " " params ^ ")" in
  Ok (command ^ " " ^ mailbox ^ params)

let valid_set command s =
  let* _ = arg command "set" (uid_set s) in Ok s

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
  let command="UID FETCH" in
  if not (valid_items items) then
    fail ~argument:"items" command "invalid FETCH item"
  else if vanished && changedsince=None then
    fail ~argument:"vanished" command "VANISHED requires CHANGEDSINCE"
  else
    let* partial = match partial with
      | None -> Ok []
      | Some range ->
          let* range = arg command "partial" (partial_range range) in
          Ok ["PARTIAL " ^ range] in
    let* changed = match changedsince with
      | None -> Ok []
      | Some n when n >= 0L -> Ok [Printf.sprintf "CHANGEDSINCE %Ld" n]
      | Some _ ->
          fail ~argument:"changedsince" command "negative CHANGEDSINCE" in
    let mods=partial @ changed @ (if vanished then ["VANISHED"] else []) in
    let modifier=if mods=[] then "" else " (" ^ String.concat " " mods ^ ")" in
    Ok ("UID FETCH " ^ set ^ " (" ^ String.concat " " items ^ ")" ^ modifier)

let uid_fetch_mod ?changedsince ?vanished ?partial ~set ~items () =
  let* set = valid_set "UID FETCH" set in
  fetch_command ?changedsince ?vanished ?partial ~set ~items ()

let uid_fetch_saved ?changedsince ?vanished ?partial ~items () =
  fetch_command ?changedsince ?vanished ?partial ~set:"$" ~items ()

let uid_fetch ~set ~items = uid_fetch_mod ~set ~items ()

let binary_section section =
  if List.length section>100 || List.exists (fun n ->
    n<1 || Int64.of_int n>4_294_967_295L) section then
    fail ~argument:"section" "UID FETCH" "invalid BINARY section"
  else Ok ("[" ^ String.concat "." (List.map string_of_int section) ^ "]")

let uid_fetch_binary ~set ~section ?partial () =
  let* set = valid_set "UID FETCH" set in
  let* section = binary_section section in
  let* partial = match partial with
    | None -> Ok ""
    | Some (offset,count) when offset>=0L && count>=1L ->
        Ok (Printf.sprintf "<%Ld.%Ld>" offset count)
    | Some _ ->
        fail ~argument:"partial" "UID FETCH" "invalid BINARY partial range" in
  Ok ("UID FETCH " ^ set ^ " (UID BINARY.PEEK" ^ section ^ partial ^ ")")

let uid_fetch_binary_size ~set ~section =
  let* set = valid_set "UID FETCH" set in
  let* section = binary_section section in
  Ok ("UID FETCH " ^ set ^ " (UID BINARY.SIZE" ^ section ^ ")")

let uid_fetch_preview ~set ~lazy_ =
  let* set = valid_set "UID FETCH" set in
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

let criterion command s =
  let invalid = fail ~argument:"criterion" command in
  if contains_control s || String.trim s="" then
    invalid "invalid SEARCH criterion"
  else if ends_with_literal_marker s then
    invalid "SEARCH criterion ends in a literal marker"
  else Ok s

let uid_search ~criterion:c =
  let* c = criterion "UID SEARCH" c in Ok ("UID SEARCH " ^ c)

let uid_search_save ~criterion:c =
  let* c = criterion "UID SEARCH" c in
  Ok ("UID SEARCH RETURN (SAVE COUNT) " ^ c)

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
  let* c = criterion "UID SEARCH" c in
  if not (scan 0 0 false) then
    fail ~argument:"criterion" "UID SEARCH" "unbalanced saved SEARCH criterion"
  else Ok ("(" ^ c ^ ")")

let uid_search_saved ~criterion =
  let* criterion = grouped_search criterion in
  Ok ("UID SEARCH RETURN (ALL COUNT) UID $ " ^ criterion)

let sort_search command ~charset ~criterion:c =
  if charset="" || String.exists (fun c -> Char.code c>=127) charset then
    fail ~argument:"charset" command "invalid SORT/THREAD charset"
  else
    let* charset = arg command "charset" (astring charset) in
    let* c = criterion command c in
    Ok (charset ^ " " ^ c)

let sort_arguments ~keys ~charset ~criterion =
  if keys=[] || List.length keys>100 then
    fail ~argument:"keys" "UID SORT" "SORT requires 1..100 keys"
  else
    let* search = sort_search "UID SORT" ~charset ~criterion in
    let keys=List.map Sort.criterion_to_wire keys in
    Ok ("(" ^ String.concat " " keys ^ ") " ^ search)

let uid_sort ~keys ~charset ~criterion =
  let* arguments = sort_arguments ~keys ~charset ~criterion in
  Ok ("UID SORT " ^ arguments)

let uid_sort_extended ~(returns : Sort.return list) ~keys ~charset ~criterion =
  let invalid = fail ~argument:"returns" "UID SORT" in
  let name : Sort.return -> string = function
    | Partial _ -> "PARTIAL" | option -> Sort.return_to_wire option in
  let names=List.map name returns in
  if has_duplicates String.compare names then
    invalid "duplicate SORT return option"
  else if List.mem "ALL" names && List.mem "PARTIAL" names then
    invalid "SORT ALL and PARTIAL are mutually exclusive"
  else if List.exists (function
    | Sort.Partial (first,last) ->
        first<1L || last<1L || first>4_294_967_295L || last>4_294_967_295L
    | _ -> false) returns then invalid "invalid SORT PARTIAL range"
  else
    let* arguments = sort_arguments ~keys ~charset ~criterion in
    let options=List.map Sort.return_to_wire returns in
    Ok ("UID SORT RETURN (" ^ String.concat " " options ^ ") " ^ arguments)

let uid_thread ~algorithm ~charset ~criterion =
  let name=Thread.to_wire algorithm in
  if not (atom name) then
    fail ~argument:"algorithm" "UID THREAD" "algorithm is not an atom"
  else
    let* search = sort_search "UID THREAD" ~charset ~criterion in
    Ok ("UID THREAD " ^ name ^ " " ^ search)

let uid_search_partial ~range ~criterion:c =
  let* range = arg "UID SEARCH" "range" (partial_range range) in
  let* c = criterion "UID SEARCH" c in
  Ok ("UID SEARCH RETURN (PARTIAL " ^ range ^ ") " ^ c)

let uid_batches ?range ~size () =
  if size < 500L || size > 4_294_967_295L then
    fail ~argument:"size" "UIDBATCHES" "size outside 500..4294967295"
  else match range with
    | None -> Ok (Printf.sprintf "UIDBATCHES %Ld" size)
    | Some (first,last) when first >= 1L && first <= last &&
         last <= 4_294_967_295L &&
         (Int64.succ (Int64.sub last first)) <= Int64.div 100_000L size ->
        Ok (Printf.sprintf "UIDBATCHES %Ld %Ld:%Ld" size first last)
    | Some _ ->
        fail ~argument:"range" "UIDBATCHES"
          "range is invalid or spans over 100000 messages"

let flag_list command flags =
  let rec check = function
    | [] -> Ok ()
    | flag::rest ->
        (match Mail_flag.Imap_flag.of_wire flag with
         | Ok _ -> check rest
         | Error e -> fail ~argument:"flags" command ("invalid flag: " ^ e)) in
  check flags

let store_command ?unchangedsince ~set ~operation ~silent ~flags () =
  if (match unchangedsince with Some n -> n < 0L | None -> false) then
    fail ~argument:"unchangedsince" "UID STORE" "negative UNCHANGEDSINCE"
  else
    let* () = flag_list "UID STORE" flags in
    let modifier=match unchangedsince with None -> "" |
      Some n -> Printf.sprintf " (UNCHANGEDSINCE %Ld)" n in
    Ok ("UID STORE " ^ set ^ modifier ^ " " ^ operation_prefix operation ^
        "FLAGS" ^ (if silent then ".SILENT" else "") ^ " (" ^
        String.concat " " flags ^ ")")

let uid_store_mod ?unchangedsince ~set ~operation ~silent ~flags () =
  let* set = valid_set "UID STORE" set in
  store_command ?unchangedsince ~set ~operation ~silent ~flags ()

let uid_store_saved ?unchangedsince ~operation ~silent ~flags () =
  store_command ?unchangedsince ~set:"$" ~operation ~silent ~flags ()

let uid_store ~set ~operation ~silent ~flags =
  uid_store_mod ~set ~operation ~silent ~flags ()

let transfer_command verb ~set ~mailbox =
  let* mailbox = arg ("UID " ^ verb) "mailbox" (astring mailbox) in
  Ok ("UID " ^ verb ^ " " ^ set ^ " " ^ mailbox)

let uid_copy ~set ~mailbox =
  let* set = valid_set "UID COPY" set in
  transfer_command "COPY" ~set ~mailbox

let uid_move ~set ~mailbox =
  let* set = valid_set "UID MOVE" set in
  transfer_command "MOVE" ~set ~mailbox

let uid_copy_saved ~mailbox = transfer_command "COPY" ~set:"$" ~mailbox
let uid_move_saved ~mailbox = transfer_command "MOVE" ~set:"$" ~mailbox

let expunge_command set = "UID EXPUNGE " ^ set
let uid_expunge ~set =
  let* set = valid_set "UID EXPUNGE" set in Ok (expunge_command set)
let uid_expunge_saved = expunge_command "$"

let append_literal_part ~binary ?(non_sync=false) ?(flags=[]) ?internal_date
    ~size () =
  if size < 0L then fail ~argument:"size" "APPEND" "negative APPEND size"
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
  let* mailbox = arg "APPEND" "mailbox" (astring mailbox) in
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
