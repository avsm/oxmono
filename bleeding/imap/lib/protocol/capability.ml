type thread_algorithm = Thread.algorithm

type t =
  | Imap4rev1
  | Imap4rev2
  | Auth of string
  | Login_disabled
  | Starttls
  | Sasl_ir
  | Enable
  | Condstore
  | Qresync
  | Uidplus
  | Move
  | Binary
  | Idle
  | Namespace
  | Unselect
  | Literal_plus
  | Literal_minus
  | Multiappend
  | Searchres
  | Esearch
  | Sort
  | Sort_display
  | Esort
  | Context of [ `Search | `Sort ]
  | Thread of Thread.algorithm
  | Partial
  | Preview
  | Objectid
  | Objectid_plus
  | Uidonly
  | Uidbatches
  | Messagelimit of int64
  | Savelimit of int64
  | Utf8 of [ `Accept | `Only ]
  | Compress of [ `Deflate ]
  | Acl
  | Quota
  | Quota_res of string
  | Quotaset
  | Metadata
  | Metadata_server
  | Notify
  | List_extended
  | List_status
  | Special_use
  | Status_size
  | Jmapaccess
  | Id
  | Children
  | Language
  | Other of string

let upper = String.uppercase_ascii

let plain = [
  "IMAP4REV1", Imap4rev1; "IMAP4REV2", Imap4rev2;
  "LOGINDISABLED", Login_disabled; "STARTTLS", Starttls; "SASL-IR", Sasl_ir;
  "ENABLE", Enable; "CONDSTORE", Condstore; "QRESYNC", Qresync;
  "UIDPLUS", Uidplus; "MOVE", Move; "BINARY", Binary; "IDLE", Idle;
  "NAMESPACE", Namespace; "UNSELECT", Unselect; "LITERAL+", Literal_plus;
  "LITERAL-", Literal_minus; "MULTIAPPEND", Multiappend;
  "SEARCHRES", Searchres; "ESEARCH", Esearch; "SORT", Sort;
  "SORT=DISPLAY", Sort_display; "ESORT", Esort;
  "CONTEXT=SEARCH", Context `Search; "CONTEXT=SORT", Context `Sort;
  "PARTIAL", Partial;
  "PREVIEW", Preview; "OBJECTID", Objectid; "OBJECTID+", Objectid_plus;
  "UIDONLY", Uidonly; "UIDBATCHES", Uidbatches;
  "UTF8=ACCEPT", Utf8 `Accept; "UTF8=ONLY", Utf8 `Only;
  "COMPRESS=DEFLATE", Compress `Deflate; "ACL", Acl; "QUOTA", Quota;
  "QUOTASET", Quotaset; "METADATA", Metadata;
  "METADATA-SERVER", Metadata_server; "NOTIFY", Notify;
  "LIST-EXTENDED", List_extended; "LIST-STATUS", List_status;
  "SPECIAL-USE", Special_use; "STATUS=SIZE", Status_size;
  "JMAPACCESS", Jmapaccess; "ID", Id; "CHILDREN", Children;
  "LANGUAGE", Language;
]

let limit value =
  let n = String.length value in
  if n = 0 || n > 10 || value.[0] = '0' ||
     not (String.for_all (function '0'..'9' -> true | _ -> false) value)
  then None
  else match Int64.of_string_opt value with
    | Some v when v <= 4_294_967_295L -> Some v
    | _ -> None

let split token =
  match String.index_opt token '=' with
  | None -> None
  | Some i ->
      Some (String.sub token 0 i,
            String.sub token (i + 1) (String.length token - i - 1))

let of_wire raw =
  let token = upper raw in
  match List.assoc_opt token plain with
  | Some c -> c
  | None ->
      match split token with
      | Some ("AUTH", m) when m <> "" -> Auth m
      | Some ("THREAD", a) when a <> "" -> Thread (Thread.of_wire a)
      | Some ("MESSAGELIMIT", v) ->
          (match limit v with Some n -> Messagelimit n | None -> Other raw)
      | Some ("SAVELIMIT", v) ->
          (match limit v with Some n -> Savelimit n | None -> Other raw)
      | Some ("QUOTA", r)
        when String.length r > 4 && String.starts_with ~prefix:"RES-" r ->
          Quota_res (String.sub r 4 (String.length r - 4))
      | _ -> Other raw

let to_wire = function
  | Auth m -> "AUTH=" ^ upper m
  | Thread a -> "THREAD=" ^ Thread.to_wire a
  | Messagelimit n -> "MESSAGELIMIT=" ^ Int64.to_string n
  | Savelimit n -> "SAVELIMIT=" ^ Int64.to_string n
  | Quota_res r -> "QUOTA=RES-" ^ upper r
  | Other s -> s
  | c ->
      match List.find_opt (fun (_, known) -> known = c) plain with
      | Some (token, _) -> token
      | None -> assert false

let key c = upper (to_wire c)
let compare a b = String.compare (key a) (key b)
let equal a b = compare a b = 0
let pp ppf c = Format.pp_print_string ppf (to_wire c)

let canonical = function Other s -> of_wire s | c -> c

let implied_by_rev2 c =
  match canonical c with
  | Enable | Idle | Namespace | Uidplus | Move | Searchres | Esearch
  | List_extended | List_status | Unselect | Sasl_ir | Literal_minus
  | Status_size -> true
  | _ -> false

let malformed_limit c =
  match canonical c with
  | Other s ->
      (match split (upper s) with
       | Some (("MESSAGELIMIT" | "SAVELIMIT"), _) -> true
       | _ -> false)
  | _ -> false

module Set = struct
  type elt = t
  module S = Stdlib.Set.Make (struct
    type nonrec t = t
    let compare = compare
  end)
  type t = S.t
  let empty = S.empty
  let is_empty = S.is_empty
  let add c s = S.add (canonical c) s
  let of_list l = List.fold_left (fun s c -> add c s) empty l
  let to_list = S.elements
  let mem = S.mem
  let union = S.union
  let equal = S.equal
  let pp ppf s =
    Format.pp_print_list ~pp_sep:Format.pp_print_space pp ppf (to_list s)
end

let smallest f s =
  List.fold_left (fun acc c ->
    match f c, acc with
    | Some n, Some m -> Some (Int64.min n m)
    | Some n, None -> Some n
    | None, acc -> acc) None (Set.to_list s)

let messagelimit =
  smallest (function Messagelimit n -> Some n | _ -> None)
let savelimit = smallest (function Savelimit n -> Some n | _ -> None)
let auth_mechanisms s =
  List.filter_map (function Auth m -> Some (upper m) | _ -> None)
    (Set.to_list s)
let thread_algorithms s =
  List.filter_map (function Thread a -> Some a | _ -> None) (Set.to_list s)
let quota_resources s =
  List.filter_map (function Quota_res r -> Some (upper r) | _ -> None)
    (Set.to_list s)
