type compound_object_id = {
  account_id:string option; mailbox_id:string option;
  email_id:string option; thread_id:string option;
  unknown:(string*string) list;
}

type code =
  | Uidvalidity of int64 | Uidnext of int64 | Highestmodseq of int64
  | Appenduid of int64 * int64 | Appenduid_set of int64 * string
  | Copyuid of int64 * string * string
  | Modified of string | Permanentflags of string list
  | Mailboxid of string | Objectid of compound_object_id
  | Messagelimit of int64 * int64 option | Uidrequired | Uidnotsticky
  | Expungeissued
  | Overquota | Metadata_longentries of int64 | Metadata_maxsize of int64
  | Metadata_toomany | Metadata_noprivate | Notificationoverflow
  | Badevent of string list
  | Unseen of int64 | Read_only | Read_write | Nomodseq | Closed | Alert
  | Unavailable
  | Authenticationfailed
  | Authorizationfailed
  | Expired
  | Privacyrequired
  | Contactadmin
  | Noperm
  | Inuse
  | Corruption
  | Serverbug
  | Clientbug
  | Cannot
  | Limit
  | Alreadyexists
  | Nonexistent
  | Unknown_cte
  | Trycreate
  | Compressionactive
  | Other_code of string

let no_argument_codes = [
  "READ-ONLY", Read_only;
  "READ-WRITE", Read_write;
  "NOMODSEQ", Nomodseq;
  "CLOSED", Closed;
  "ALERT", Alert;
  "EXPUNGEISSUED", Expungeissued;
  "OVERQUOTA", Overquota;
  "UIDREQUIRED", Uidrequired;
  "UIDNOTSTICKY", Uidnotsticky;
  "NOTIFICATIONOVERFLOW", Notificationoverflow;
  "UNAVAILABLE", Unavailable;
  "AUTHENTICATIONFAILED", Authenticationfailed;
  "AUTHORIZATIONFAILED", Authorizationfailed;
  "EXPIRED", Expired;
  "PRIVACYREQUIRED", Privacyrequired;
  "CONTACTADMIN", Contactadmin;
  "NOPERM", Noperm;
  "INUSE", Inuse;
  "CORRUPTION", Corruption;
  "SERVERBUG", Serverbug;
  "CLIENTBUG", Clientbug;
  "CANNOT", Cannot;
  "LIMIT", Limit;
  "ALREADYEXISTS", Alreadyexists;
  "NONEXISTENT", Nonexistent;
  "UNKNOWN-CTE", Unknown_cte;
  "TRYCREATE", Trycreate;
  "COMPRESSIONACTIVE", Compressionactive;
]

let parameter_code_names =
  ["UIDVALIDITY";"UIDNEXT";"HIGHESTMODSEQ";"APPENDUID";"COPYUID";
   "UNSEEN";"MODIFIED";"PERMANENTFLAGS";"MAILBOXID";"OBJECTID";
   "MESSAGELIMIT";"METADATA";"BADEVENT"]

let response_code_name = function
  | Read_only -> Some "READ-ONLY"
  | Read_write -> Some "READ-WRITE"
  | Nomodseq -> Some "NOMODSEQ"
  | Closed -> Some "CLOSED"
  | Alert -> Some "ALERT"
  | Expungeissued -> Some "EXPUNGEISSUED"
  | Overquota -> Some "OVERQUOTA"
  | Uidrequired -> Some "UIDREQUIRED"
  | Uidnotsticky -> Some "UIDNOTSTICKY"
  | Notificationoverflow -> Some "NOTIFICATIONOVERFLOW"
  | Unavailable -> Some "UNAVAILABLE"
  | Authenticationfailed -> Some "AUTHENTICATIONFAILED"
  | Authorizationfailed -> Some "AUTHORIZATIONFAILED"
  | Expired -> Some "EXPIRED"
  | Privacyrequired -> Some "PRIVACYREQUIRED"
  | Contactadmin -> Some "CONTACTADMIN"
  | Noperm -> Some "NOPERM"
  | Inuse -> Some "INUSE"
  | Corruption -> Some "CORRUPTION"
  | Serverbug -> Some "SERVERBUG"
  | Clientbug -> Some "CLIENTBUG"
  | Cannot -> Some "CANNOT"
  | Limit -> Some "LIMIT"
  | Alreadyexists -> Some "ALREADYEXISTS"
  | Nonexistent -> Some "NONEXISTENT"
  | Unknown_cte -> Some "UNKNOWN-CTE"
  | Trycreate -> Some "TRYCREATE"
  | Compressionactive -> Some "COMPRESSIONACTIVE"
  | Uidvalidity _ -> Some "UIDVALIDITY"
  | Uidnext _ -> Some "UIDNEXT"
  | Highestmodseq _ -> Some "HIGHESTMODSEQ"
  | Appenduid _ | Appenduid_set _ -> Some "APPENDUID"
  | Copyuid _ -> Some "COPYUID"
  | Modified _ -> Some "MODIFIED"
  | Permanentflags _ -> Some "PERMANENTFLAGS"
  | Mailboxid _ -> Some "MAILBOXID"
  | Objectid _ -> Some "OBJECTID"
  | Messagelimit _ -> Some "MESSAGELIMIT"
  | Metadata_longentries _ | Metadata_maxsize _ | Metadata_toomany |
    Metadata_noprivate -> Some "METADATA"
  | Badevent _ -> Some "BADEVENT"
  | Unseen _ -> Some "UNSEEN"
  | Other_code _ -> None

type fetch = {
  seq:int64; uid:int64 option; flags:string list option; modseq:int64 option;
  size:int64 option; internal_date:Internal_date.t option;
  email_id:string option; thread_id:string option option;
  preview:string option option;
  literals:(string*int64) list; raw:string
}
type envelope_address = {
  name:string option; route:string option;
  mailbox:string option; host:string option
}
type envelope = {
  date:string option; subject:string option;
  from:envelope_address list option;
  sender:envelope_address list option;
  reply_to:envelope_address list option;
  to_:envelope_address list option;
  cc:envelope_address list option;
  bcc:envelope_address list option;
  in_reply_to:string option; message_id:string option
}
type body_extension =
  | Ext_nil | Ext_string of string | Ext_number of int64
  | Ext_list of body_extension list
type bodystructure =
  | Single_part of {
      media_type:string; subtype:string;
      parameters:(string*string) list option;
      content_id:string option; description:string option;
      encoding:string; octets:int64; lines:int64 option;
      enclosed:(envelope*bodystructure*int64) option;
      extensions:body_extension list
    }
  | Multipart of {
      parts:bodystructure list; subtype:string;
      extensions:body_extension list
    }
type list_result = {
  subscribed:bool; attributes:string list; delimiter:string option;
  mailbox:string;old_name:string option;childinfo:string list option;
  children:[`Has_children|`Has_no_children|`Unknown];selectable:bool;
  special_use:string list;raw:string
}
type namespace_entry = {
  prefix:string; delimiter:string option;
  extensions:(string*string list) list
}
type namespace = {
  personal:namespace_entry list option;
  other_users:namespace_entry list option;
  shared:namespace_entry list option;raw:string
}
type esearch = {
  tag:string option; uid:bool; min:int64 option; max:int64 option;
  count:int64 option; all:string option; modseq:int64 option;
  partial:(string*string option) option; raw:string
}
type uidbatches = {tag:string;ranges:(int64*int64) list;raw:string}
type acl = {mailbox:string;entries:(string*string) list;raw:string}
type list_rights = {mailbox:string;identifier:string;required:string;
  optional:string list;raw:string}
type my_rights = {mailbox:string;rights:string;raw:string}
type quota = {root:string;resources:(string*int64*int64) list;raw:string}
type quota_root = {mailbox:string;roots:string list;raw:string}
type metadata_payload =
  | Metadata_values of (string*string option) list
  | Metadata_changed of string list
type metadata = {mailbox:string;payload:metadata_payload;raw:string}
type mailbox_status = {
  mailbox:string; messages:int64 option; unseen:int64 option;
  uidnext:int64 option; uidvalidity:int64 option;
  highestmodseq:int64 option;mailbox_id:string option;
  objectid:compound_object_id option;size:int64 option;
  deleted:int64 option;deleted_storage:int64 option;raw:string
}
type thread = { uid : int64 option; children : thread list }
(** A THREAD node. [None] preserves a dummy parent. A sequence of message
    numbers becomes a chain of single-child nodes. Response numbers are UIDs
    only when the command was UID THREAD. *)
type untagged =
  | Ok of code option * string | No of code option * string
  | Bad of code option * string | Bye of code option * string
  | Preauth of code option * string
  | Capability of string list | Enabled of string list
  | Flags of string list
  | Exists of int64 | Recent of int64 | Expunge of int64
  | Fetch of fetch | Uidfetch of fetch | List of list_result | Namespace of namespace
  | Status of mailbox_status
  | Jmapaccess of string | Uidbatches of uidbatches
  | Acl of acl | List_rights of list_rights | My_rights of my_rights
  | Quota of quota | Quota_root of quota_root | Metadata of metadata
  | Vanished of {earlier:bool; uids:string}
  | Search of int64 list | Sort of int64 list | Thread of thread list
  | Esearch of esearch | Other of string
type t =
  | Tagged of {tag:string; status:[`Ok|`No|`Bad]; code:code option; text:string}
  | Untagged of untagged | Continuation of string

let trim = String.trim
let split_words s = String.split_on_char ' ' (trim s) |> List.filter ((<>) "")
let parse_i64 s =
  if s="" || not (String.for_all (fun c -> c >= '0' && c <= '9') s)
  then None else Int64.of_string_opt s
let up = String.uppercase_ascii
let prefix s p = String.length s >= String.length p && String.sub s 0 (String.length p) = p
let after s n = String.sub s n (String.length s-n)
let object_id s =
  let n=String.length s in
  n>0 && n<=255 && String.for_all (function
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '-' -> true
    | _ -> false) s

let compound_of_pairs pairs =
  let rec collect seen value = function
    | [] -> Result.Ok {value with unknown=List.rev value.unknown}
    | (key,id)::rest ->
        let key=up key in
        if not (object_id id) || List.mem key seen then
          Result.Error "invalid compound OBJECTID"
        else let value=match key with
          | "ACCOUNTID" -> {value with account_id=Some id}
          | "MAILBOXID" -> {value with mailbox_id=Some id}
          | "EMAILID" -> {value with email_id=Some id}
          | "THREADID" -> {value with thread_id=Some id}
          | _ -> {value with unknown=(key,id)::value.unknown} in
        collect (key::seen) value rest in
  collect [] {account_id=None;mailbox_id=None;email_id=None;
    thread_id=None;unknown=[]} pairs

let compound_words words =
  let rec pairs acc = function
    | [] -> compound_of_pairs (List.rev acc)
    | key::id::rest -> pairs ((key,id)::acc) rest
    | _ -> Result.Error "invalid compound OBJECTID" in
  pairs [] words

let response_code text =
  if String.length text < 2 || text.[0] <> '[' then None, text else
  match String.index_opt text ']' with
  | None -> None, text
  | Some k ->
      let inner = String.sub text 1 (k-1) in
      let rest = trim (after text (k+1)) in
      let words=split_words inner in
      let canonical=match words with [] -> [] | key::args -> up key::args in
      let code = match canonical with
        | "OBJECTID"::parts ->
            (match parts with
             | first::rest when String.length first>=1 && first.[0]='(' ->
                 let joined=String.concat " " (first::rest) in
                 let n=String.length joined in
                 if n<2 || joined.[0]<>'(' || joined.[n-1]<>')'
                 then None
                 else let inside=String.sub joined 1 (n-2) in
                 (match compound_words (split_words inside) with
                  | Result.Ok value -> Some (Objectid value)
                  | Result.Error _ -> None)
             | _ -> None)
        | ["UIDVALIDITY"; n] ->
            (match parse_i64 n with Some n when n >= 1L && n <= 4_294_967_295L ->
              Some (Uidvalidity n) | _ -> None)
        | ["UIDNEXT"; n] ->
            (match parse_i64 n with Some n when n >= 1L && n <= 4_294_967_296L ->
              Some (Uidnext n) | _ -> None)
        | ["HIGHESTMODSEQ"; n] ->
            (match parse_i64 n with Some n when n >= 1L ->
              Some (Highestmodseq n) | _ -> None)
        | ["APPENDUID"; v; u] ->
            (match parse_i64 v, parse_i64 u with
             | Some v, Some u when v >= 1L && v <= 4_294_967_295L &&
                                   u >= 1L && u <= 4_294_967_295L ->
                 Some (Appenduid(v,u))
             | Some v, _ when v >= 1L && v <= 4_294_967_295L ->
                 (match Proto.Uid_set.of_wire u with
                  | Result.Ok set when Proto.Uid_set.cardinality set > 1L ->
                      Some (Appenduid_set (v,u))
                  | _ -> None)
             | _ -> None)
        | ["COPYUID"; v; src; dst] ->
            (match parse_i64 v, Proto.Uid_set.of_wire src,
                   Proto.Uid_set.of_wire dst with
             | Some v, Result.Ok source, Result.Ok target
               when v >= 1L && v <= 4_294_967_295L &&
                    Proto.Uid_set.cardinality source =
                    Proto.Uid_set.cardinality target ->
                 Some (Copyuid(v,src,dst))
             | _ -> None)
        | ["MODIFIED"; uids] ->
            (match Proto.Uid_set.of_wire uids with
             | Result.Ok _ -> Some (Modified uids) | Result.Error _ -> None)
        | ["MAILBOXID"; wrapped] ->
            let n=String.length wrapped in
            if n>=3 && wrapped.[0]='(' && wrapped.[n-1]=')' then
              let id=String.sub wrapped 1 (n-2) in
              if object_id id then Some (Mailboxid id) else None
            else None
        | ["MESSAGELIMIT"; count] ->
            (match parse_i64 count with
             | Some n when n>=1L && n<=4_294_967_295L ->
                 Some (Messagelimit (n,None))
             | _ -> None)
        | ["MESSAGELIMIT"; count; last] ->
            (match parse_i64 count,parse_i64 last with
             | Some n,Some uid when n>=1L && n<=4_294_967_295L &&
                                  uid>=1L && uid<=4_294_967_295L ->
                 Some (Messagelimit (n,Some uid))
             | _ -> None)
        | ["METADATA"; kind; n] when up kind="LONGENTRIES" ->
            (match parse_i64 n with Some n when n>=0L ->
              Some (Metadata_longentries n) | _ -> None)
        | ["METADATA"; kind; n] when up kind="MAXSIZE" ->
            (match parse_i64 n with Some n when n>=0L ->
              Some (Metadata_maxsize n) | _ -> None)
        | ["METADATA"; kind] when up kind="TOOMANY" -> Some Metadata_toomany
        | ["METADATA"; kind] when up kind="NOPRIVATE" -> Some Metadata_noprivate
        | "BADEVENT"::items ->
            let joined=String.concat " " items in
            let n=String.length joined in
            if n>=3 && joined.[0]='(' && joined.[n-1]=')' then
              let events=String.sub joined 1 (n-2) |> split_words in
              if events<>[] && List.for_all (fun event ->
                event<>"" && String.for_all (function
                  | 'A'..'Z'|'a'..'z'|'0'..'9'|'-'|'_' -> true
                  | _ -> false) event) events
              then Some (Badevent events) else None
            else None
        | "PERMANENTFLAGS"::flags ->
            let atoms=String.concat " " flags in
            let len=String.length atoms in
            if len>=2 && atoms.[0]='(' && atoms.[len-1]=')' then
              let inner=String.sub atoms 1 (len-2) in
              let values=split_words inner in
              if List.for_all (fun flag ->
                flag="\\*" || match Mail_flag.Imap_flag.of_wire flag with
                | Result.Ok _ -> true | Result.Error _ -> false) values
              then Some (Permanentflags values) else None
            else None
        | ["UNSEEN"; n] ->
            (match parse_i64 n with Some n when n >= 1L ->
              Some (Unseen n) | _ -> None)
        | name::args when List.mem_assoc name no_argument_codes ->
            if args=[] then List.assoc_opt name no_argument_codes else None
        | name::_ when List.mem name parameter_code_names -> None
        | _ -> Some (Other_code inner) in
      code, rest

let malformed_known_code text =
  if not (prefix text "[") then false else
  match String.index_opt text ']' with
  | None -> true
  | Some k ->
      let inner=String.sub text 1 (k-1) in
      (match split_words inner with
       | name::_ when List.mem (up name) parameter_code_names ||
           List.mem_assoc (up name) no_argument_codes ->
           (match fst (response_code text) with None -> true | Some _ -> false)
       | _ -> false)

type tok = A of string | Q of string | L | R | Lit of int64

let tokenize s =
  let n = String.length s in
  let rec scan i acc =
    if i >= n then List.rev acc else
    match s.[i] with
    | ' ' | '\r' | '\n' | '\t' -> scan (i+1) acc
    | '(' -> scan (i+1) (L::acc)
    | ')' -> scan (i+1) (R::acc)
    | '"' ->
        let b=Buffer.create 16 in
        let rec quoted j =
          if j >= n then j else
          match s.[j] with
          | '"' -> j+1
          | '\\' when j+1 < n -> Buffer.add_char b s.[j+1]; quoted (j+2)
          | c -> Buffer.add_char b c; quoted (j+1) in
        let j=quoted (i+1) in scan j (Q(Buffer.contents b)::acc)
    | '{' ->
        (match String.index_from_opt s (i+1) '}' with
         | Some j when j+2 < n && s.[j+1]='\r' && s.[j+2]='\n' ->
             (match parse_i64 (String.sub s (i+1) (j-i-1)) with
              | Some k -> scan (j+3) (Lit k::acc)
              | None -> atom i acc)
         | _ -> atom i acc)
    | '~' when i+1 < n && s.[i+1]='{' ->
        (match String.index_from_opt s (i+2) '}' with
         | Some j when j+2 < n && s.[j+1]='\r' && s.[j+2]='\n' ->
             (match parse_i64 (String.sub s (i+2) (j-i-2)) with
              | Some k -> scan (j+3) (Lit k::acc)
              | None -> atom i acc)
         | _ -> atom i acc)
    | _ -> atom i acc
  and atom i acc =
    let rec boundary j brackets =
      if j >= n then j else
      match s.[j] with
      | '[' -> boundary (j+1) (brackets+1)
      | ']' when brackets>0 -> boundary (j+1) (brackets-1)
      | (' '| '\r' | '\n' | '\t' | '(' | ')') when brackets=0 -> j
      | _ -> boundary (j+1) brackets in
    let j=boundary i 0 in
    if j=i then scan (i+1) acc
    else scan j (A (String.sub s i (j-i))::acc)
  in scan 0 []

let token_string = function A s | Q s -> s | Lit n -> Printf.sprintf "{%Ld}" n
  | L -> "(" | R -> ")"
let value_string = function A s | Q s -> Some s | _ -> None
let balanced_quotes s =
  let rec scan i quoted =
    if i=String.length s then not quoted
    else match s.[i] with
      | '"' -> scan (i+1) (not quoted)
      | '\\' when quoted && i+1<String.length s -> scan (i+2) quoted
      | _ -> scan (i+1) quoted in
  scan 0 false
let num = function A s -> parse_i64 s | _ -> None
let rec skip_value = function
  | [] -> []
  | L::rest ->
      let rec list depth = function
        | [] -> []
        | L::xs -> list (depth+1) xs
        | R::xs when depth=1 -> xs
        | R::xs -> list (depth-1) xs
        | _::xs -> list depth xs in
      list 1 rest
  | _::rest -> rest
let list_atoms = function
  | L::rest ->
      let rec gather acc = function
        | R::tail -> Some (List.rev acc),tail
        | (A s|Q s)::tail -> gather (s::acc) tail
        | _ -> None,[] in gather [] rest
  | _ -> None,[]

let valid_flag flag =
  match Mail_flag.Imap_flag.of_wire flag with
  | Result.Ok _ -> true | Result.Error _ -> false

let valid_preview s =
  let n=String.length s in
  if n>1024 then false else
  let continuation i = i<n && Char.code s.[i] land 0xc0 = 0x80 in
  let rec scan i count =
    if count>256 then false else if i=n then true else
    let a=Char.code s.[i] in
    if a<0x80 then scan (i+1) (count+1)
    else if a>=0xc2 && a<=0xdf && continuation (i+1) then
      scan (i+2) (count+1)
    else if a>=0xe0 && a<=0xef && continuation (i+1) &&
      continuation (i+2) &&
      (a<>0xe0 || Char.code s.[i+1]>=0xa0) &&
      (a<>0xed || Char.code s.[i+1]<0xa0) then
      scan (i+3) (count+1)
    else if a>=0xf0 && a<=0xf4 && continuation (i+1) &&
      continuation (i+2) && continuation (i+3) &&
      (a<>0xf0 || Char.code s.[i+1]>=0x90) &&
      (a<>0xf4 || Char.code s.[i+1]<0x90) then
      scan (i+4) (count+1)
    else false in
  scan 0 0

let parse_flags raw =
  let rec find = function
    | A name::rest when up name="FLAGS" ->
        (match list_atoms rest with
         | Some flags,_ when List.for_all valid_flag flags -> Result.Ok flags
         | _ -> Result.Error "invalid FLAGS response")
    | _::rest -> find rest
    | [] -> Result.Error "missing FLAGS response" in
  find (tokenize raw)

let fetch ?(uid_only=false) seq raw =
  let toks=tokenize raw in
  let rec seek = function
    | A a::A b::L::rest when
        (up a="FETCH" || up a="UIDFETCH") && b="" -> rest
    | A a::L::rest when up a="FETCH" || up a="UIDFETCH" -> rest
    | _::rest -> seek rest | [] -> [] in
  let field_tokens=seek toks in
  let rec fields uid flags modseq size internal_date email_id thread_id
      preview literals = function
    | [] | R::_ ->
        if uid_only && (match uid with Some value -> value <> seq | None -> false) then
          Result.Error "UIDFETCH leading UID disagrees with UID data item"
        else Result.Ok {seq;uid=(if uid_only then Some seq else uid);
                        flags;modseq;size;internal_date;email_id;thread_id;
                        preview;
                        literals=List.rev literals;raw}
    | A name::value ->
        let key=up name in
        (match key,value with
         | "UID", _ when Option.is_some uid -> Result.Error "duplicate FETCH UID"
         | "UID", v::rest ->
             (match num v with
              | Some n when n >= 1L && n <= 4_294_967_295L ->
                  fields (Some n) flags modseq size internal_date email_id
                    thread_id preview literals rest
              | _ -> Result.Error "invalid FETCH UID")
         | "RFC822.SIZE", v::rest ->
             (match num v with
              | Some n when n >= 0L ->
                  fields uid flags modseq (Some n) internal_date email_id
                    thread_id preview literals rest
              | _ -> Result.Error "invalid FETCH RFC822.SIZE")
         | "INTERNALDATE", Q value::rest ->
             (match Internal_date.of_string value with
              | Result.Ok value ->
                  fields uid flags modseq size (Some value) email_id
                    thread_id preview literals rest
              | Result.Error _ -> Result.Error "invalid FETCH INTERNALDATE")
         | "INTERNALDATE", _ -> Result.Error "invalid FETCH INTERNALDATE"
         | "FLAGS", _ ->
             let f,rest=list_atoms value in
             (match f with
              | None -> Result.Error "invalid FETCH FLAGS"
              | Some flags ->
                  if List.for_all (fun flag ->
                    match Mail_flag.Imap_flag.of_wire flag with
                    | Result.Ok _ -> true | Result.Error _ -> false) flags
                  then fields uid (Some flags) modseq size internal_date
                    email_id thread_id preview literals rest
                  else Result.Error "invalid FETCH flag atom")
         | "MODSEQ", L::v::R::rest ->
             (match num v with
              | Some n when n >= 1L ->
                  fields uid flags (Some n) size internal_date email_id
                    thread_id preview literals rest
              | _ -> Result.Error "invalid FETCH MODSEQ")
         | "EMAILID", L::A id::R::rest when object_id id ->
             fields uid flags modseq size internal_date (Some id) thread_id
               preview literals rest
         | "EMAILID", _ -> Result.Error "invalid FETCH EMAILID"
         | "THREADID", A nil::rest when up nil="NIL" ->
             fields uid flags modseq size internal_date email_id (Some None)
               preview literals rest
         | "THREADID", L::A id::R::rest when object_id id ->
             fields uid flags modseq size internal_date email_id
               (Some (Some id)) preview literals rest
         | "THREADID", _ -> Result.Error "invalid FETCH THREADID"
         | "PREVIEW", (A nil)::rest when up nil="NIL" && preview=None ->
             fields uid flags modseq size internal_date email_id thread_id
               (Some None) literals rest
         | "PREVIEW", (Q s)::rest when preview=None && valid_preview s ->
             fields uid flags modseq size internal_date email_id thread_id
               (Some (Some s)) literals rest
         | "PREVIEW", _ -> Result.Error "invalid FETCH PREVIEW"
         | _, Lit n::rest ->
             fields uid flags modseq size internal_date email_id thread_id preview
               ((name,n)::literals) rest
         | _ -> fields uid flags modseq size internal_date email_id thread_id
                    preview literals
                  (skip_value value))
    | _::rest -> fields uid flags modseq size internal_date email_id thread_id
                   preview literals rest in
  fields None None None None None None None None [] field_tokens

let decode_envelope_tokens =
  let invalid = Result.Error "invalid FETCH ENVELOPE" in
  let nstring = function
    | A s::rest when up s="NIL" -> Result.Ok (None,rest)
    | Q s::rest when String.length s<=65_536 -> Result.Ok (Some s,rest)
    | _ -> invalid in
  let address = function
    | L::rest ->
        (match nstring rest with
         | Result.Error e -> Result.Error e
         | Result.Ok (name,rest) ->
             match nstring rest with
             | Result.Error e -> Result.Error e
             | Result.Ok (route,rest) ->
                 match nstring rest with
                 | Result.Error e -> Result.Error e
                 | Result.Ok (mailbox,rest) ->
                     match nstring rest with
                     | Result.Ok (host,R::rest) ->
                         Result.Ok ({name;route;mailbox;host},rest)
                     | _ -> invalid)
    | _ -> invalid in
  let addresses = function
    | A s::rest when up s="NIL" -> Result.Ok (None,rest)
    | L::rest ->
        let rec gather count acc = function
          | R::rest when count>0 -> Result.Ok (Some (List.rev acc),rest)
          | _ when count>=1024 -> invalid
          | tokens ->
              (match address tokens with
               | Result.Ok (value,rest) -> gather (count+1) (value::acc) rest
               | Result.Error e -> Result.Error e) in
        gather 0 [] rest
    | _ -> invalid in
  let decode = function
    | L::rest ->
        (match nstring rest with
         | Result.Error e -> Result.Error e
         | Result.Ok (date,rest) ->
           match nstring rest with
           | Result.Error e -> Result.Error e
           | Result.Ok (subject,rest) ->
             match addresses rest with
             | Result.Error e -> Result.Error e
             | Result.Ok (from,rest) ->
               match addresses rest with
               | Result.Error e -> Result.Error e
               | Result.Ok (sender,rest) ->
                 match addresses rest with
                 | Result.Error e -> Result.Error e
                 | Result.Ok (reply_to,rest) ->
                   match addresses rest with
                   | Result.Error e -> Result.Error e
                   | Result.Ok (to_,rest) ->
                     match addresses rest with
                     | Result.Error e -> Result.Error e
                     | Result.Ok (cc,rest) ->
                       match addresses rest with
                       | Result.Error e -> Result.Error e
                       | Result.Ok (bcc,rest) ->
                         match nstring rest with
                         | Result.Error e -> Result.Error e
                         | Result.Ok (in_reply_to,rest) ->
                           match nstring rest with
                           | Result.Ok (message_id,R::rest) ->
                               Result.Ok ({date;subject;from;sender;reply_to;
                                 to_;cc;bcc;in_reply_to;message_id},rest)
                           | _ -> invalid)
    | _ -> invalid in
  decode

type binary = Nil | Inline of string | Literal of int64

let binary_attribute_name name =
  let name=up name in
  let size,prefix_length =
    if prefix name "BINARY.SIZE" then true,11 else false,6 in
  let invalid=Result.Error "invalid FETCH BINARY section or offset" in
  let n=String.length name in
  if n<=prefix_length || name.[prefix_length]<>'[' then invalid
  else match String.index_from_opt name (prefix_length+1) ']' with
  | None -> invalid
  | Some close ->
      let path=String.sub name (prefix_length+1) (close-prefix_length-1) in
      let parts=if path="" then [] else String.split_on_char '.' path in
      let component s = match parse_i64 s with
        | Some value when value>=1L && value<=4_294_967_295L && s.[0]<>'0' ->
            Some (Int64.to_int value)
        | _ -> None in
      let section=List.filter_map component parts in
      if List.length parts>100 || List.length parts<>List.length section then invalid
      else if close=n-1 then Result.Ok (size,section,None)
      else if size || name.[close+1]<>'<' || name.[n-1]<>'>' then invalid
      else match parse_i64 (String.sub name (close+2) (n-close-3)) with
        | Some offset -> Result.Ok (size,section,Some offset)
        | _ -> invalid

let fetch_binary_attribute (row:fetch) ~size ~section ~offset decode =
  let invalid=Result.Error "invalid FETCH BINARY attribute" in
  if String.length row.raw>1_048_576 || not (balanced_quotes row.raw) ||
     List.length section>100 || List.exists (fun n ->
       n<1 || Int64.of_int n>4_294_967_295L) section ||
     (match offset with Some n -> n<0L | None -> false)
  then invalid else
  let rec fields found = function
    | [R] -> Result.Ok found
    | A key::value when prefix (up key) "BINARY" ->
        (match binary_attribute_name key with
         | Result.Error _ as error -> error
         | Result.Ok (got_size,got_section,got_offset) ->
             if not size && not got_size &&
                (got_section<>section || got_offset<>offset) then
               Result.Error "unexpected FETCH BINARY section or offset"
             else if got_size<>size || got_section<>section || got_offset<>offset then
               fields found (skip_value value)
             else match found,value with
               | Some _,_ -> Result.Error "duplicate FETCH BINARY attribute"
               | None,value::rest ->
                   (match decode value with
                    | Result.Ok value -> fields (Some value) rest
                    | Result.Error _ as error -> error)
               | _ -> invalid)
    | A _::value -> fields found (skip_value value)
    | _ -> invalid in
  let rec seek = function
    | A name::L::rest when up name="FETCH" || up name="UIDFETCH" -> fields None rest
    | _::rest -> seek rest
    | [] -> invalid in
  seek (tokenize row.raw)

let fetch_binary row ~section ~offset =
  fetch_binary_attribute row ~size:false ~section ~offset (function
    | A nil when up nil="NIL" -> Result.Ok Nil
    | Q value when not (String.exists (fun c ->
        c='\000' || c='\r' || c='\n') value) -> Result.Ok (Inline value)
    | Lit length when length>=0L -> Result.Ok (Literal length)
    | _ -> Result.Error "invalid FETCH BINARY data")

(* RFC 9051's BINARY.SIZE ABNF retains [number], but Appendix D requires
   clients to handle 63-bit body-part sizes. Accept those sizes here. *)
let fetch_binary_size row ~section =
  fetch_binary_attribute row ~size:true ~section ~offset:None (fun value ->
    match num value with
    | Some size when size>=0L -> Result.Ok size
    | _ -> Result.Error "invalid FETCH BINARY.SIZE")

let fetch_envelope (row:fetch) =
  let invalid = Result.Error "invalid FETCH ENVELOPE" in
  if String.length row.raw > 1_048_576 || not (balanced_quotes row.raw)
  then invalid else
  let rec fields found = function
    | [R] -> Result.Ok found
    | R::_ -> Result.Error "trailing FETCH fields"
    | A key::value when up key="ENVELOPE" ->
        (match found with
         | Some _ -> Result.Error "duplicate FETCH ENVELOPE"
         | None ->
             (match decode_envelope_tokens value with
              | Result.Ok (value,rest) -> fields (Some value) rest
              | Result.Error e -> Result.Error e))
    | A _::value -> fields found (skip_value value)
    | _ -> Result.Error "invalid FETCH fields" in
  let rec seek = function
    | A name::L::rest when up name="FETCH" || up name="UIDFETCH" ->
        fields None rest
    | _::rest -> seek rest
    | [] -> Result.Error "missing FETCH fields" in
  seek (tokenize row.raw)

let fetch_bodystructure (row:fetch) =
  let error = "invalid FETCH BODYSTRUCTURE" in
  let fail () = Result.Error error in
  if String.length row.raw > 1_048_576 || not (balanced_quotes row.raw)
  then fail () else
  let ( let* ) x f = match x with
    | Result.Ok value -> f value | Result.Error e -> Result.Error e in
  let string = function
    | Q s::rest when String.length s<=65_536 ->
        Result.Ok (s,rest)
    | _ -> fail () in
  let nstring = function
    | A s::rest when up s="NIL" -> Result.Ok (None,rest)
    | toks -> let* value,rest=string toks in Result.Ok (Some value,rest) in
  let number ?(limit=Int64.max_int) = function
    | A s::rest ->
        (match parse_i64 s with
         | Some n when n<=limit -> Result.Ok (n,rest)
         | _ -> fail ())
    | _ -> fail () in
  let params = function
    | A s::rest when up s="NIL" -> Result.Ok (None,rest)
    | L::rest ->
        let rec pairs count acc = function
          | R::rest when count>0 -> Result.Ok (Some (List.rev acc),rest)
          | _ when count>=256 -> fail ()
          | toks ->
              let* name,rest=string toks in
              let* value,rest=string rest in
              pairs (count+1) ((name,value)::acc) rest in
        pairs 0 [] rest
    | _ -> fail () in
  let extension_count=ref 0 in
  let rec extension depth toks =
    incr extension_count;
    if depth>16 || !extension_count>4096 then fail () else
    match toks with
    | A s::rest when up s="NIL" -> Result.Ok (Ext_nil,rest)
    | A s::rest ->
        (match parse_i64 s with
         | Some n -> Result.Ok (Ext_number n,rest)
         | None -> fail ())
    | Q s::rest when String.length s<=65_536 ->
        Result.Ok (Ext_string s,rest)
    | L::rest ->
        let rec values count acc = function
          | R::rest when count>0 ->
              Result.Ok (Ext_list (List.rev acc),rest)
          | _ when count>=4096 -> fail ()
          | toks ->
              let* value,rest=extension (depth+1) toks in
              values (count+1) (value::acc) rest in
        values 0 [] rest
    | _ -> fail () in
  let rec extensions acc = function
    | R::rest -> Result.Ok (List.rev acc,rest)
    | toks ->
        let* value,rest=extension 0 toks in
        extensions (value::acc) rest in
  let valid_params = function
    | Ext_nil -> true
    | Ext_list values ->
        let rec pairs = function
          | [Ext_string _;Ext_string _] -> true
          | Ext_string _::Ext_string _::rest -> pairs rest
          | _ -> false in
        pairs values
    | _ -> false in
  let valid_disposition = function
    | Ext_nil -> true
    | Ext_list [Ext_string _;attrs] -> valid_params attrs
    | _ -> false in
  let valid_language = function
    | Ext_nil | Ext_string _ -> true
    | Ext_list values -> values<>[] && List.for_all (function
        | Ext_string _ -> true | _ -> false) values
    | _ -> false in
  let valid_nstring = function Ext_nil | Ext_string _ -> true | _ -> false in
  let valid_extensions multipart = function
    | [] -> true
    | first::rest ->
        (if multipart then valid_params first else valid_nstring first) &&
        (match rest with [] -> true | x::xs -> valid_disposition x &&
          (match xs with [] -> true | x::xs -> valid_language x &&
            (match xs with [] -> true | x::_ -> valid_nstring x))) in
  let nodes=ref 0 in
  let rec body depth toks =
    incr nodes;
    if depth>16 || !nodes>256 then fail () else
    match toks with
    | L::L::_ ->
        let rec children count acc = function
          | L::_ as toks when count<256 ->
              let* child,rest=body (depth+1) toks in
              children (count+1) (child::acc) rest
          | toks when count>0 -> Result.Ok (List.rev acc,toks)
          | _ -> fail () in
        let* parts,rest=children 0 [] (List.tl toks) in
        let* subtype,rest=string rest in
        let* extensions,rest=extensions [] rest in
        if not (valid_extensions true extensions) then fail ()
        else Result.Ok (Multipart {parts;subtype;extensions},rest)
    | L::rest ->
        let* media_type,rest=string rest in
        let* subtype,rest=string rest in
        let* parameters,rest=params rest in
        let* content_id,rest=nstring rest in
        let* description,rest=nstring rest in
        let* encoding,rest=string rest in
        let* octets,rest=number ~limit:4_294_967_295L rest in
        let* lines,enclosed,rest =
          if up media_type="MESSAGE" &&
             List.mem (up subtype) ["RFC822";"GLOBAL"] then
            let* envelope,rest=decode_envelope_tokens rest in
            let* child,rest=body (depth+1) rest in
            let* lines,rest=number rest in
            Result.Ok (None,Some (envelope,child,lines),rest)
          else if up media_type="TEXT" then
            let* lines,rest=number rest in
            Result.Ok (Some lines,None,rest)
          else Result.Ok (None,None,rest) in
        let* extensions,rest=extensions [] rest in
        if not (valid_extensions false extensions) then fail ()
        else Result.Ok (Single_part {media_type;subtype;parameters;
          content_id;description;encoding;octets;lines;enclosed;
          extensions},rest)
    | _ -> fail () in
  let rec fields found = function
    | [R] -> Result.Ok found
    | R::_ -> Result.Error "trailing FETCH fields"
    | A key::value when up key="BODYSTRUCTURE" ->
        (match found with
         | Some _ -> Result.Error "duplicate FETCH BODYSTRUCTURE"
         | None ->
             let* value,rest=body 0 value in
             fields (Some value) rest)
    | A _::value -> fields found (skip_value value)
    | _ -> Result.Error "invalid FETCH fields" in
  let rec seek = function
    | A name::L::rest when up name="FETCH" || up name="UIDFETCH" ->
        fields None rest
    | _::rest -> seek rest
    | [] -> Result.Error "missing FETCH fields" in
  seek (tokenize row.raw)

let fetch_objectid (row:fetch) =
  let rec pairs acc = function
    | R::rest ->
        (match compound_of_pairs (List.rev acc) with
         | Result.Ok value -> Result.Ok (value,rest)
         | Result.Error message -> Result.Error message)
    | A key::A id::rest -> pairs ((key,id)::acc) rest
    | _ -> Result.Error "invalid FETCH OBJECTID" in
  let rec fields found = function
    | R::_ -> Result.Ok found
    | A key::L::rest when up key="OBJECTID" ->
        (match found with
         | Some _ -> Result.Error "duplicate FETCH OBJECTID"
         | None ->
             (match pairs [] rest with
              | Result.Ok (value,rest) -> fields (Some value) rest
              | Result.Error message -> Result.Error message))
    | A key::_ when up key="OBJECTID" ->
        Result.Error "invalid FETCH OBJECTID"
    | A _::value -> fields found (skip_value value)
    | _ -> Result.Error "invalid FETCH fields" in
  let rec seek = function
    | A name::L::rest when up name="FETCH" || up name="UIDFETCH" ->
        fields None rest
    | _::rest -> seek rest
    | [] -> Result.Error "missing FETCH fields" in
  seek (tokenize row.raw)

let delimiter = function
  | A nil when up nil="NIL" -> Result.Ok None
  | Q s when s<>"" -> Result.Ok (Some s)
  | _ -> Result.Error "invalid hierarchy delimiter"

let parse_list subscribed raw =
  if not (balanced_quotes raw) then Result.Error "unterminated LIST quote" else
  let rec extended old_name childinfo = function
    | [R] -> Result.Ok (old_name,childinfo)
    | (A key|Q key)::L::name::R::rest when up key="OLDNAME" ->
        (match value_string name with
         | Some name when old_name=None -> extended (Some name) childinfo rest
         | _ -> Result.Error "invalid LIST OLDNAME")
    | (A key|Q key)::rest when up key="CHILDINFO" ->
        let names,rest=list_atoms rest in
        (match names with
         | Some names when names<>[] && childinfo=None ->
             extended old_name (Some names) rest
         | _ -> Result.Error "invalid LIST CHILDINFO")
    | (A _|Q _)::value -> extended old_name childinfo (skip_value value)
    | _ -> Result.Error "invalid LIST extended data" in
  match tokenize raw with
  | A star::A cmd::rest when star="*" &&
      (up cmd="LIST" || up cmd="LSUB") ->
      let attributes,rest=list_atoms rest in
      (match attributes,rest with
       | Some attributes,delim::mailbox_token::tail ->
           (match delimiter delim,value_string mailbox_token with
            | Result.Ok delimiter,Some mailbox when
                (match mailbox_token with
                 | A nil when up nil="NIL" -> false
                 | _ -> true) ->
                let tail=match tail with [] -> Result.Ok (None,None)
                  | L::more -> extended None None more
                  | _ -> Result.Error "invalid LIST trailing data" in
                (match tail with
                 | Result.Error _ as e -> e
                 | Result.Ok (old_name,childinfo) ->
                     let attrs=List.map up attributes in
                     let contains flag=List.mem flag attrs in
                     let children=if contains "\\HASCHILDREN" then
                       `Has_children else if contains "\\HASNOCHILDREN" ||
                         contains "\\NOINFERIORS" then `Has_no_children
                       else `Unknown in
                     if contains "\\HASCHILDREN" &&
                        (contains "\\HASNOCHILDREN" || contains "\\NOINFERIORS")
                     then Result.Error "conflicting LIST children attributes"
                     else
                       let special_use=List.filter (fun attr ->
                         List.mem (up attr)
                           ["\\ALL";"\\ARCHIVE";"\\DRAFTS";"\\FLAGGED";
                            "\\JUNK";"\\SENT";"\\TRASH";"\\IMPORTANT";
                            "\\SNOOZED"]) attributes in
                       Result.Ok {subscribed;attributes;delimiter;mailbox;
                         old_name;childinfo;children;
                         selectable=not (contains "\\NOSELECT" ||
                                         contains "\\NONEXISTENT");
                         special_use;raw})
            | _ -> Result.Error "invalid LIST mailbox or delimiter")
       | _ -> Result.Error "invalid LIST response")
  | _ -> Result.Error "invalid LIST response"

let parse_namespace raw =
  if not (balanced_quotes raw) then
    Result.Error "unterminated NAMESPACE quote" else
  let rec entries acc = function
    | R::rest when acc<>[] -> Result.Ok (List.rev acc,rest)
    | L::prefix_token::delim::rest ->
        (match value_string prefix_token,delimiter delim with
         | Some prefix,Result.Ok delimiter when
             (match prefix_token with
              | A nil when up nil="NIL" -> false
              | _ -> true) ->
             let rec extensions acc = function
               | R::rest -> Result.Ok (List.rev acc,rest)
               | (A name|Q name)::rest ->
                   let values,rest=list_atoms rest in
                   (match values with
                    | Some values when values<>[] ->
                        extensions ((name,values)::acc) rest
                    | _ -> Result.Error "invalid NAMESPACE extension")
               | _ -> Result.Error "invalid NAMESPACE entry" in
             (match extensions [] rest with
              | Result.Ok (extensions,rest) ->
                  entries ({prefix;delimiter;extensions}::acc) rest
              | Result.Error _ as e -> e)
         | _ -> Result.Error "invalid NAMESPACE prefix or delimiter")
    | _ -> Result.Error "invalid NAMESPACE group" in
  let group = function
    | A nil::rest when up nil="NIL" -> Result.Ok (None,rest)
    | L::rest ->
        (match entries [] rest with
         | Result.Ok (entries,rest) -> Result.Ok (Some entries,rest)
         | Result.Error _ as e -> e)
    | _ -> Result.Error "invalid NAMESPACE group" in
  match tokenize raw with
  | [A star;A cmd] when star="*" && up cmd="NAMESPACE" ->
      Result.Error "missing NAMESPACE groups"
  | A star::A cmd::rest when star="*" && up cmd="NAMESPACE" ->
      (match group rest with
       | Result.Error _ as e -> e
       | Result.Ok (personal,rest) ->
           (match group rest with
            | Result.Error _ as e -> e
            | Result.Ok (other_users,rest) ->
                (match group rest with
                 | Result.Ok (shared,[]) ->
                     Result.Ok {personal;other_users;shared;raw}
                 | _ -> Result.Error "invalid NAMESPACE response")))
  | _ -> Result.Error "invalid NAMESPACE response"

let parse_esearch raw =
  let valid_partial_range s =
    let endpoint x =
      let negative=String.length x>0 && x.[0]='-' in
      let digits=if negative then after x 1 else x in
      match parse_i64 digits with
      | Some n when n>=1L && n<=4_294_967_295L -> Some negative
      | _ -> None in
    match String.split_on_char ':' s with
    | [first;last] ->
        (match endpoint first,endpoint last with
         | Some a,Some b -> a=b | _ -> false)
    | _ -> false in
  let rec start = function
    | A s::xs when up s="ESEARCH" -> xs
    | _::xs -> start xs
    | [] -> [] in
  let tokens=start (tokenize raw) in
  let tag,tokens=match tokens with
    | L::A key::(Q value|A value)::R::xs when up key="TAG" -> Some value,xs
    | _ -> None,tokens in
  let uid,tokens=match tokens with
    | A s::xs when up s="UID" -> true,xs
    | _ -> false,tokens in
  let rec loop min max count all modseq partial = function
    | A key::_ when (match up key with
        | "MIN" -> Option.is_some min | "MAX" -> Option.is_some max
        | "COUNT" -> Option.is_some count | "ALL" -> Option.is_some all
        | "MODSEQ" -> Option.is_some modseq | "PARTIAL" -> Option.is_some partial
        | _ -> false) -> Result.Error ("duplicate ESEARCH " ^ up key)
    | [] -> Result.Ok {tag;uid;min;max;count;all;modseq;partial;raw}
    | A key::L::A range::A results::R::rest when up key="PARTIAL" ->
        if not (valid_partial_range range) then
          Result.Error "invalid ESEARCH PARTIAL range"
        else if up results="NIL" then
          loop min max count all modseq (Some (range,None)) rest
        else (match Proto.Uid_set.of_wire results with
          | Result.Ok _ ->
              loop min max count all modseq (Some (range,Some results)) rest
          | Result.Error _ -> Result.Error "invalid ESEARCH PARTIAL results")
    | A key::v::rest ->
        (match up key with
         | "MIN" | "MAX" | "COUNT" | "MODSEQ" ->
             (match num v with
              | Some n when (match up key with
                  | "MIN" | "MAX" -> n>=1L && n<=4_294_967_295L
                  | "COUNT" -> n>=0L && n<=4_294_967_295L
                  | _ -> n>=0L) ->
                  (match up key with
                   | "MIN" -> loop (Some n) max count all modseq partial rest
                   | "MAX" -> loop min (Some n) count all modseq partial rest
                   | "COUNT" -> loop min max (Some n) all modseq partial rest
                   | _ -> loop min max count all (Some n) partial rest)
              | _ -> Result.Error ("invalid ESEARCH " ^ key))
         | "PARTIAL" -> Result.Error "invalid ESEARCH PARTIAL"
         | "ALL" ->
             (match value_string v with
              | Some x when (match Proto.Uid_set.of_wire x with
                              | Result.Ok _ -> true | Result.Error _ -> false) ->
                  loop min max count (Some x) modseq partial rest
              | _ -> Result.Error "invalid ESEARCH ALL")
         | _ -> loop min max count all modseq partial rest)
    | _ -> Result.Error "invalid ESEARCH result" in
  loop None None None None None None tokens

let parse_status raw =
  let rec start = function
    | A s::mailbox::L::rest when up s="STATUS" ->
        let mailbox=token_string mailbox in
        let rec compound acc = function
          | R::rest ->
              (match compound_of_pairs (List.rev acc) with
               | Result.Ok value -> Result.Ok (value,rest)
               | Result.Error message -> Result.Error message)
          | A key::A id::rest -> compound ((key,id)::acc) rest
          | _ -> Result.Error "invalid STATUS OBJECTID" in
        let rec fields messages unseen uidnext uidvalidity highestmodseq
            mailbox_id objectid size deleted deleted_storage = function
          | R::_ ->
              Result.Ok {mailbox;messages;unseen;uidnext;uidvalidity;
                         highestmodseq;mailbox_id;objectid;size;deleted;
                         deleted_storage;raw}
          | A name::L::rest when up name="OBJECTID" ->
              if objectid<>None then Result.Error "duplicate STATUS OBJECTID"
              else (match compound [] rest with
                | Result.Error message -> Result.Error message
                | Result.Ok (ids,rest) ->
                    fields messages unseen uidnext uidvalidity highestmodseq
                      mailbox_id (Some ids) size deleted deleted_storage rest)
          | A name::L::A id::R::rest when up name="MAILBOXID" ->
              if object_id id then
                fields messages unseen uidnext uidvalidity highestmodseq
                  (Some id) objectid size deleted deleted_storage rest
              else Result.Error "invalid STATUS MAILBOXID"
          | A name::v::rest ->
              let name=up name in
              if name="MAILBOXID" || name="OBJECTID" then
                Result.Error ("invalid STATUS " ^ name)
              else if not (List.mem name
                ["MESSAGES";"UNSEEN";"UIDNEXT";"UIDVALIDITY";
                 "HIGHESTMODSEQ";"SIZE";"DELETED";
                 "DELETED-STORAGE"])
              then fields messages unseen uidnext uidvalidity highestmodseq
                     mailbox_id objectid size deleted deleted_storage
                     (skip_value (v::rest))
              else (match num v with
               | None -> Result.Error ("invalid STATUS " ^ name)
               | Some n ->
                   if n < 0L || ((name="UIDVALIDITY" || name="UIDNEXT") &&
                                  (n < 1L ||
                                   n > (if name="UIDNEXT" then
                                     4_294_967_296L else 4_294_967_295L)))
                   then Result.Error ("invalid STATUS " ^ name)
                   else
                     fields
                       (if name="MESSAGES" then Some n else messages)
                       (if name="UNSEEN" then Some n else unseen)
                       (if name="UIDNEXT" then Some n else uidnext)
                       (if name="UIDVALIDITY" then Some n else uidvalidity)
                       (if name="HIGHESTMODSEQ" then Some n else highestmodseq)
                       mailbox_id objectid
                       (if name="SIZE" then Some n else size)
                       (if name="DELETED" then Some n else deleted)
                       (if name="DELETED-STORAGE" then Some n
                        else deleted_storage)
                       rest)
          | _ -> Result.Error "invalid STATUS fields" in
        fields None None None None None None None None None None rest
    | _::rest -> start rest
    | [] -> Result.Error "invalid STATUS response" in
  start (tokenize raw)

let parse_jmapaccess raw =
  let prefix_text="* JMAPACCESS " in
  let n=String.length raw and k=String.length prefix_text in
  if n<k+2 || up (String.sub raw 0 k)<>prefix_text ||
     raw.[k]<>'"' then Result.Error "invalid JMAPACCESS response"
  else
    let b=Buffer.create (n-k-2) in
    let rec quoted i =
      if i>=n then Result.Error "unterminated JMAPACCESS URL"
      else match raw.[i] with
      | '"' when i=n-1 ->
          if Buffer.length b=0 then Result.Error "empty JMAPACCESS URL"
          else Result.Ok (Buffer.contents b)
      | '"' -> Result.Error "trailing JMAPACCESS data"
      | '\\' when i+1<n && (raw.[i+1]='"' || raw.[i+1]='\\') ->
          Buffer.add_char b raw.[i+1]; quoted (i+2)
      | '\\' -> Result.Error "invalid JMAPACCESS quoted escape"
      | c when Char.code c<0x20 || Char.code c=0x7f ->
          Result.Error "control byte in JMAPACCESS URL"
      | c -> Buffer.add_char b c; quoted (i+1) in
    quoted (k+1)

let parse_uidbatches raw =
  match tokenize raw with
  | A star::A name::L::A key::(Q tag|A tag)::R::tail
    when star="*" && up name="UIDBATCHES" && up key="TAG" && tag<>"" ->
      let data=match tail with [] -> Some [] | [A s] ->
        let range item = match String.split_on_char ':' item with
          | [first;last] ->
              (match parse_i64 first,parse_i64 last with
               | Some hi,Some lo when hi>=lo && hi<=4_294_967_295L &&
                    lo>=1L -> Some (hi,lo)
               | _ -> None)
          | _ -> None in
        let rec collect previous acc = function
          | [] -> Some (List.rev acc)
          | item::rest ->
              (match range item with
               | Some (hi,lo) when (match previous with
                   | None -> true | Some prev_lo -> hi<prev_lo) ->
                     collect (Some lo) ((hi,lo)::acc) rest
               | _ -> None) in
        collect None [] (String.split_on_char ',' s)
        | _ -> None in
      (match data with
       | Some ranges -> Result.Ok {tag;ranges;raw}
       | None -> Result.Error "invalid UIDBATCHES range ordering")
  | _ -> Result.Error "invalid UIDBATCHES response"

let acl_rights s =
  String.for_all (function 'a'..'z'|'0'..'9' -> true | _ -> false) s

let parse_acl raw =
  match tokenize raw with
  | A star::A name::mailbox::fields when star="*" && up name="ACL" ->
      (match value_string mailbox with
       | None -> Result.Error "invalid ACL mailbox"
       | Some mailbox ->
           let rec pairs acc = function
             | [] -> Result.Ok {mailbox;entries=List.rev acc;raw}
             | identifier::rights::rest ->
                 (match value_string identifier,value_string rights with
                  | Some identifier,Some rights when identifier<>"" &&
                      acl_rights rights ->
                      pairs ((identifier,rights)::acc) rest
                  | _ -> Result.Error "invalid ACL entry")
             | _ -> Result.Error "unpaired ACL identifier" in
           pairs [] fields)
  | _ -> Result.Error "invalid ACL response"

let parse_list_rights raw =
  match tokenize raw with
  | A star::A name::mailbox::identifier::required::optional
    when star="*" && up name="LISTRIGHTS" ->
      (match value_string mailbox,value_string identifier,
             value_string required with
       | Some mailbox,Some identifier,Some required
         when identifier<>"" && acl_rights required ->
           let optional=List.map value_string optional in
           if List.for_all (function Some s -> acl_rights s | None -> false)
              optional then
             let optional=List.filter_map Fun.id optional in
             let all=required ^ String.concat "" optional in
             let chars=List.init (String.length all) (String.get all) in
             if List.length chars =
                List.length (List.sort_uniq Char.compare chars) then
               Result.Ok {mailbox;identifier;required;optional;raw}
             else Result.Error "duplicate LISTRIGHTS right"
           else Result.Error "invalid LISTRIGHTS optional rights"
       | _ -> Result.Error "invalid LISTRIGHTS response")
  | _ -> Result.Error "invalid LISTRIGHTS response"

let parse_my_rights raw =
  match tokenize raw with
  | [A star;A name;mailbox;rights] when star="*" && up name="MYRIGHTS" ->
      (match value_string mailbox,value_string rights with
       | Some mailbox,Some rights when acl_rights rights ->
           Result.Ok {mailbox;rights;raw}
       | _ -> Result.Error "invalid MYRIGHTS response")
  | _ -> Result.Error "invalid MYRIGHTS response"

let parse_quota raw =
  match tokenize raw with
  | A star::A name::root::L::fields when star="*" && up name="QUOTA" ->
      (match value_string root with
       | None -> Result.Error "invalid QUOTA root"
       | Some root ->
           let rec resources acc = function
             | [R] -> Result.Ok {root;resources=List.rev acc;raw}
             | A name::A usage::A limit::rest ->
                 (match parse_i64 usage,parse_i64 limit with
                  | Some usage,Some limit when usage>=0L && limit>=0L ->
                      resources ((name,usage,limit)::acc) rest
                  | _ -> Result.Error "invalid QUOTA resource values")
             | _ -> Result.Error "invalid QUOTA resource list" in
           resources [] fields)
  | _ -> Result.Error "invalid QUOTA response"

let parse_quota_root raw =
  match tokenize raw with
  | A star::A name::mailbox::roots when star="*" && up name="QUOTAROOT" ->
      (match value_string mailbox with
       | None -> Result.Error "invalid QUOTAROOT mailbox"
       | Some mailbox ->
           let roots=List.map value_string roots in
           if List.for_all Option.is_some roots then
             Result.Ok {mailbox;roots=List.filter_map Fun.id roots;raw}
           else Result.Error "invalid QUOTAROOT names")
  | _ -> Result.Error "invalid QUOTAROOT response"

let metadata_entry s =
  let n=String.length s in
  n>1 && s.[0]='/' && not (String.contains s '*') &&
  not (String.contains s '%') &&
  not (String.exists (fun c -> Char.code c<0x20 || Char.code c=0x7f) s)

let parse_metadata raw =
  match tokenize raw with
  | A star::A name::mailbox::items when star="*" && up name="METADATA" ->
      (match value_string mailbox with
       | None -> Result.Error "invalid METADATA mailbox"
       | Some mailbox ->
           let payload=match items with
             | L::rest ->
                 let rec values acc = function
                   | [R] when acc<>[] ->
                       Result.Ok (Metadata_values (List.rev acc))
                   | entry::value::rest ->
                       (match value_string entry,value with
                        | Some entry,A nil when metadata_entry entry &&
                            up nil="NIL" ->
                            values ((entry,None)::acc) rest
                        | Some entry,Q v when metadata_entry entry ->
                            values ((entry,Some v)::acc) rest
                        | _ -> Result.Error "invalid METADATA entry/value")
                   | _ -> Result.Error "invalid METADATA values" in
                 values [] rest
             | rest ->
                 let entries=List.map value_string rest in
                 if entries<>[] && List.for_all (function
                   | Some name -> metadata_entry name | None -> false) entries
                 then Result.Ok (Metadata_changed (List.filter_map Fun.id entries))
                 else Result.Error "invalid unsolicited METADATA names" in
           (match payload with
            | Result.Ok payload -> Result.Ok {mailbox;payload;raw}
            | Result.Error _ as e -> e))
  | _ -> Result.Error "invalid METADATA response"

(* SORT and THREAD have strict ordered grammars. Scan directly rather than
   tokenizing away whitespace, so malformed trees never become plausible ones.
   Bound both wire nesting and expanded chains for safe downstream traversal. *)
let parse_ordered_result ~threaded raw =
  let exception Invalid of string in
  let invalid message = raise (Invalid message) in
  let limit=100_000 in
  let nodes=ref 0 in
  let seen=Hashtbl.create 64 in
  let length=String.length raw in
  let pos=ref (if threaded then 8 else 6) in
  let peek () = if !pos<length then Some raw.[!pos] else None in
  let take c =
    if peek ()<>Some c then invalid "invalid SORT/THREAD syntax";
    incr pos in
  let node depth =
    if depth>100 then invalid "THREAD depth exceeds 100";
    incr nodes;
    if !nodes>limit then invalid "SORT/THREAD exceeds 100000 nodes" in
  let number () =
    let start= !pos in
    (match peek () with
     | Some ('1'..'9') -> incr pos
     | _ -> invalid "invalid SORT/THREAD message number");
    while (match peek () with Some ('0'..'9') -> true | _ -> false) do
      incr pos
    done;
    if !pos-start>10 then invalid "SORT/THREAD message number outside range";
    let uid=Int64.of_string (String.sub raw start (!pos-start)) in
    if uid>4_294_967_295L then invalid "SORT/THREAD message number outside range";
    if Hashtbl.mem seen uid then invalid "duplicate SORT/THREAD message number";
    Hashtbl.add seen uid ();
    uid in
  let rec tree depth =
    take '(';
    let result=match peek () with
      | Some '(' ->
          node depth;
          {uid=None;children=nested (depth+1)}
      | _ -> members depth in
    take ')';
    result
  and members depth =
    node depth;
    let uid=number () in
    let children=match peek () with
      | Some ')' -> []
      | Some ' ' ->
          incr pos;
          (match peek () with
           | Some '(' -> nested (depth+1)
           | _ -> [members (depth+1)])
      | _ -> invalid "invalid THREAD member separator" in
    {uid=Some uid;children}
  and nested depth =
    let rec collect count acc =
      match peek () with
      | Some '(' -> collect (count+1) (tree depth::acc)
      | _ ->
          if count<2 then invalid "THREAD branch requires at least two children";
          List.rev acc in
    collect 0 [] in
  try
    if length>2_097_152 then invalid "SORT/THREAD response exceeds 2 MiB";
    let expected=if threaded then "* THREAD" else "* SORT" in
    if length < !pos || up (String.sub raw 0 !pos)<>expected then
      invalid "invalid SORT/THREAD prefix";
    if !pos=length then
      Result.Ok (if threaded then Thread [] else Sort [])
    else (
      take ' ';
      if threaded then (
        let rec roots acc =
          if !pos=length then List.rev acc
          else roots (tree 1::acc) in
        (* Stalwart serializes an empty THREAD with its usual separator. *)
        Result.Ok (Thread (roots [])))
      else (
        let rec numbers acc =
          node 1;
          let uid=number () in
          if !pos=length then List.rev (uid::acc)
          else (take ' '; numbers (uid::acc)) in
        Result.Ok (Sort (numbers []))))
  with Invalid message -> Result.Error message

let parse raw =
  let raw = if String.length raw >= 2 &&
    String.sub raw (String.length raw-2) 2 = "\r\n"
    then String.sub raw 0 (String.length raw-2) else raw in
  if raw="" then Error "empty IMAP response"
  else if prefix raw "+ " then Ok (Continuation (after raw 2))
  else if raw="+" then Ok (Continuation "")
  else
  let words=split_words raw in
  match words with
  | "*"::kind::rest ->
      let u=up kind in
      let body = if String.length raw >= 2+String.length kind
                 then trim (after raw (2+String.length kind)) else "" in
      let status ctor = let code,text=response_code body in Untagged (ctor (code,text)) in
      let value = match u with
        | "OK" -> status (fun (c,t) -> Ok (c,t))
        | "NO" -> status (fun (c,t) -> No (c,t))
        | "BAD" -> status (fun (c,t) -> Bad (c,t))
        | "BYE" -> status (fun (c,t) -> Bye (c,t))
        | "PREAUTH" -> status (fun (c,t) -> Preauth (c,t))
        | "CAPABILITY" -> Untagged (Capability rest)
        | "ENABLED" -> Untagged (Enabled rest)
        | "FLAGS" -> Untagged (Other raw)
        | "LIST" | "LSUB" | "NAMESPACE" -> Untagged (Other raw)
        | "STATUS" -> Untagged (Other raw)
        | "JMAPACCESS" | "UIDBATCHES" | "ACL" | "LISTRIGHTS" |
          "MYRIGHTS" | "QUOTA" | "QUOTAROOT" | "METADATA" ->
            Untagged (Other raw)
        | "VANISHED" ->
            let earlier,uids=match rest with
              | "(EARLIER)"::xs -> true,String.concat " " xs
              | _ -> false,String.concat " " rest in
            Untagged (Vanished {earlier;uids})
        | "SEARCH" ->
            Untagged (Search (List.filter_map parse_i64 rest))
        | "ESEARCH" -> Untagged (Other raw)
        | _ ->
            (match parse_i64 kind,rest with
             | Some n, typ::_ ->
                 (match up typ with
                  | "EXISTS" -> Untagged (Exists n)
                  | "RECENT" -> Untagged (Recent n)
                  | "EXPUNGE" -> Untagged (Expunge n)
                  | "FETCH" | "UIDFETCH" -> Untagged (Other raw)
                  | _ -> Untagged (Other raw))
             | _ -> Untagged (Other raw)) in
      (if List.mem u ["OK";"NO";"BAD";"BYE";"PREAUTH"] &&
          malformed_known_code body then Result.Error "invalid IMAP response code"
       else match parse_i64 kind,rest with
       | Some n, typ::_ when up typ="FETCH" || up typ="UIDFETCH" ->
           if n < 1L || n > 4_294_967_295L then Result.Error "invalid FETCH sequence"
           else (match fetch ~uid_only:(up typ="UIDFETCH") n
                    (String.concat " " rest) with
            | Result.Ok f -> Result.Ok (Untagged
                (if up typ="UIDFETCH" then Uidfetch f else Fetch f))
            | Result.Error e -> Result.Error e)
       | _ ->
           if u="FLAGS" then
             (match parse_flags raw with
              | Result.Ok x -> Result.Ok (Untagged (Flags x))
              | Result.Error e -> Result.Error e)
           else if u="VANISHED" then
             (match value with
              | Untagged (Vanished {uids;_}) ->
                  (match Proto.Uid_set.of_wire uids with
                   | Result.Ok _ -> Result.Ok value
                   | Result.Error _ -> Result.Error "invalid VANISHED UID set")
              | _ -> assert false)
           else if u="SORT" || u="THREAD" then
             (match parse_ordered_result ~threaded:(u="THREAD") raw with
              | Result.Ok result -> Result.Ok (Untagged result)
              | Result.Error _ as error -> error)
           else if u="SEARCH" then
             if List.length rest <> List.length (List.filter_map parse_i64 rest) ||
                List.exists (fun s -> match parse_i64 s with
                  | Some n -> n < 1L || n > 4_294_967_295L | None -> true) rest
             then Result.Error "invalid SEARCH result" else Result.Ok value
           else if u="LIST" || u="LSUB" then
             (match parse_list (u="LSUB") raw with
              | Result.Ok item -> Result.Ok (Untagged (List item))
              | Result.Error e -> Result.Error e)
           else if u="NAMESPACE" then
             (match parse_namespace raw with
              | Result.Ok item -> Result.Ok (Untagged (Namespace item))
              | Result.Error e -> Result.Error e)
           else if u="JMAPACCESS" then
             (match parse_jmapaccess raw with
              | Result.Ok x -> Result.Ok (Untagged (Jmapaccess x))
              | Result.Error e -> Result.Error e)
           else if u="UIDBATCHES" then
             (match parse_uidbatches raw with
              | Result.Ok x -> Result.Ok (Untagged (Uidbatches x))
              | Result.Error e -> Result.Error e)
           else if u="ACL" then
             (match parse_acl raw with
              | Result.Ok x -> Result.Ok (Untagged (Acl x))
              | Result.Error e -> Result.Error e)
           else if u="LISTRIGHTS" then
             (match parse_list_rights raw with
              | Result.Ok x -> Result.Ok (Untagged (List_rights x))
              | Result.Error e -> Result.Error e)
           else if u="MYRIGHTS" then
             (match parse_my_rights raw with
              | Result.Ok x -> Result.Ok (Untagged (My_rights x))
              | Result.Error e -> Result.Error e)
           else if u="QUOTA" then
             (match parse_quota raw with
              | Result.Ok x -> Result.Ok (Untagged (Quota x))
              | Result.Error e -> Result.Error e)
           else if u="QUOTAROOT" then
             (match parse_quota_root raw with
              | Result.Ok x -> Result.Ok (Untagged (Quota_root x))
              | Result.Error e -> Result.Error e)
           else if u="METADATA" then
             (match parse_metadata raw with
              | Result.Ok x -> Result.Ok (Untagged (Metadata x))
              | Result.Error e -> Result.Error e)
           else if u="STATUS" then
             (match parse_status raw with
              | Result.Ok x -> Result.Ok (Untagged (Status x))
              | Result.Error e -> Result.Error e)
           else if u="ESEARCH" then
             (match parse_esearch raw with
              | Result.Ok x -> Result.Ok (Untagged (Esearch x))
              | Result.Error e -> Result.Error e)
           else (match parse_i64 kind,rest with
             | Some n,typ::_ when List.mem (up typ)
                 ["EXISTS";"RECENT";"EXPUNGE"] &&
                 (n < 0L || n > 4_294_967_295L ||
                  (up typ="EXPUNGE" && n=0L)) ->
                 Result.Error "invalid message sequence or count"
             | _ -> Result.Ok value))
  | tag::status::rest when up status="OK" || up status="NO" || up status="BAD" ->
      let status=match up status with "OK" -> `Ok | "NO" -> `No | _ -> `Bad in
      let body=String.concat " " rest in
      if malformed_known_code body then Result.Error "invalid IMAP response code"
      else
        let code,text=response_code body in
        Ok (Tagged {tag;status;code;text})
  | _ -> Error "invalid IMAP response prefix"

let parse_parts ?(max_control_literal=16_777_216) parts =
  if max_control_literal < 0 then invalid_arg "Imap.Response.parse_parts";
  let b=Buffer.create 128 in
  let complete=ref false in
  let control=ref false in
  let fetch_response=ref false in
  let collecting=ref false in
  let current_limit=ref max_control_literal in
  let envelope_literal_bytes=ref 0 in
  let bodystructure_literal_bytes=ref 0 in
  let literal_len=ref 0 in
  let failure=ref None in
  let append_quoted s =
    String.iter (fun c ->
      if c='\\' || c='"' then Buffer.add_char b '\\';
      Buffer.add_char b c) s in
  let inside_fetch_item item s =
    let rec seek = function
      | A name::L::rest when up name="FETCH" || up name="UIDFETCH" ->
          scan 1 false rest
      | _::rest -> seek rest
      | [] -> false
    and scan depth inside = function
      | [] -> inside
      | A name::L::rest when depth=1 && up name=item ->
          scan 2 true rest
      | L::rest -> scan (depth+1) inside rest
      | R::rest -> scan (depth-1) (inside && depth<>2) rest
      | _::rest -> scan depth inside rest in
    seek (tokenize s) in
  List.iter (function
    | Wire.Text s ->
        if Buffer.length b=0 then (
          let u=up s in
          control := List.exists (prefix u)
            ["* LIST ";"* LSUB ";"* STATUS ";"* NAMESPACE ";
             "* ACL ";"* LISTRIGHTS ";"* MYRIGHTS ";"* QUOTA ";
             "* QUOTAROOT ";"* METADATA "];
          fetch_response := match String.split_on_char ' ' s with
            | "*"::seq::kind::_ when parse_i64 seq<>None ->
                up kind="FETCH" || up kind="UIDFETCH"
            | _ -> false);
        Buffer.add_string b s
    | Wire.End_of_response -> complete := true
    | Wire.Literal_start n ->
        let marker=Printf.sprintf "{%Ld}\r\n" n in
        let have=Buffer.contents b in
        let h=String.length have and m=String.length marker in
        let start=h-m in
        let preview= !fetch_response && start>=8 &&
          up (String.sub have (start-8) 8)="PREVIEW " in
        let envelope= !fetch_response && inside_fetch_item "ENVELOPE" have in
        let bodystructure= !fetch_response &&
          inside_fetch_item "BODYSTRUCTURE" have in
        if !control || preview || envelope || bodystructure then (
          let limit=if preview then 1024 else if envelope || bodystructure then
            min 65_536 max_control_literal else max_control_literal in
          if envelope then envelope_literal_bytes :=
            !envelope_literal_bytes + (if n > 262_144L then 262_145
              else Int64.to_int n);
          if bodystructure then bodystructure_literal_bytes :=
            !bodystructure_literal_bytes + (if n > 262_144L then 262_145
              else Int64.to_int n);
          if n > Int64.of_int limit then
            failure := Some (if preview then "PREVIEW literal exceeds limit"
              else if envelope then "ENVELOPE literal exceeds limit"
              else if bodystructure then "BODYSTRUCTURE literal exceeds limit"
              else "control literal exceeds limit")
          else if envelope && !envelope_literal_bytes > 262_144 then
            failure := Some "ENVELOPE literals exceed aggregate limit"
          else if bodystructure && !bodystructure_literal_bytes > 262_144 then
            failure := Some "BODYSTRUCTURE literals exceed aggregate limit"
          else (
            if start<0 || String.sub have start m <> marker ||
               (preview && start>0 && have.[start-1]='~')
              then failure := Some "literal marker mismatch"
            else (
              Buffer.truncate b (if start>0 && have.[start-1]='~'
                                 then start-1 else start);
              Buffer.add_char b '"';
              collecting := true;
              current_limit := limit;
              literal_len := 0)))
    | Wire.Literal_chunk s ->
        if !collecting then (
          literal_len := !literal_len + String.length s;
          if !literal_len > !current_limit then
            failure := Some "retained literal exceeds limit"
          else append_quoted s)
    | Wire.Literal_end ->
        if !collecting then (
          Buffer.add_char b '"'; collecting := false)) parts;
  match !failure with
  | Some e -> Error e
  | None when not !complete -> Error "incomplete IMAP response"
  | None when !collecting -> Error "incomplete control literal"
  | None -> parse (Buffer.contents b)

type select_metadata = {
  exists:int64; recent:int64 option; uidvalidity:int64; uidnext:int64;
  highestmodseq:int64 option; nomodseq:bool; flags:string list option;
  permanentflags:string list option; mailbox_id:string option;
  objectid:compound_object_id option;
  readonly:bool option; uidnotsticky:bool
}

let select_metadata responses =
  let exists=ref None and recent=ref None and validity=ref None and
      next=ref None and highest=ref None and nomodseq=ref false and
      flags=ref None and permanentflags=ref None and readonly=ref None and
      mailbox_id=ref None and objectid=ref None and completed=ref false and
      uidnotsticky=ref false in
  let code = function
    | Uidvalidity n -> validity:=Some n
    | Uidnext n -> next:=Some n
    | Highestmodseq n -> highest:=Some n
    | Nomodseq -> nomodseq:=true
    | Permanentflags f -> permanentflags:=Some f
    | Mailboxid id -> mailbox_id:=Some id
    | Objectid id -> objectid:=Some id
    | Read_only -> readonly:=Some true
    | Read_write -> readonly:=Some false
    | _ -> () in
  List.iter (function
    | Untagged (Exists n) -> exists:=Some n
    | Untagged (Recent n) -> recent:=Some n
    | Untagged (Flags f) -> flags:=Some f
    | Untagged (Ok (Some c,_)) -> code c
    | Untagged (No (Some Uidnotsticky,_)) -> uidnotsticky:=true
    | Tagged {status=`Ok;code=c;_} ->
        completed:=true; Option.iter code c
    | _ -> ()) responses;
  if not !completed then Error "SELECT lacks tagged OK completion"
  else if !nomodseq && !highest<>None then
    Error "SELECT advertised both NOMODSEQ and HIGHESTMODSEQ"
  else match !exists,!validity,!next with
    | Some exists,Some uidvalidity,Some uidnext ->
        Ok {exists;recent= !recent;uidvalidity;uidnext;
            highestmodseq= !highest;nomodseq= !nomodseq;
            flags= !flags;permanentflags= !permanentflags;
            mailbox_id= !mailbox_id;objectid= !objectid;
            readonly= !readonly;uidnotsticky= !uidnotsticky}
    | _ -> Error "SELECT lacks mandatory EXISTS, UIDVALIDITY or UIDNEXT"
