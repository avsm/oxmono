type date = { day : int; month : int; year : int }

type t =
  | All | Answered | Deleted | Draft | Flagged | Seen
  | Unanswered | Undeleted | Undraft | Unflagged | Unseen
  | Keyword of Mail_flag.Imap_flag.t
  | Unkeyword of Mail_flag.Imap_flag.t
  | New | Old | Recent
  | Bcc of string | Cc of string | From of string | To of string
  | Subject of string | Body of string | Text of string
  | Header of string * string
  | Before of date | On of date | Since of date
  | Sentbefore of date | Senton of date | Sentsince of date
  | Larger of int64 | Smaller of int64
  | Uid of Uid_set.t
  | Modseq of Modseq.t
  | Emailid of string | Threadid of string
  | Saved
  | Not of t
  | Or of t * t
  | And of t list
  | Raw of string

type error =
  | Needs_utf8 of string
  | Unquotable of string
  | Invalid_key of { key : string; reason : string }
  | Invalid_raw of { raw : string; reason : string }

let error_to_string = function
  | Needs_utf8 s -> Printf.sprintf "SEARCH string %S needs UTF-8" s
  | Unquotable s -> Printf.sprintf "SEARCH string %S needs a literal" s
  | Invalid_key {key; reason} -> Printf.sprintf "SEARCH %s: %s" key reason
  | Invalid_raw {raw; reason} ->
      Printf.sprintf "raw SEARCH criterion %S: %s" raw reason

let pp_error ppf e = Format.pp_print_string ppf (error_to_string e)

let ( let* ) = Result.bind

let months = [:
  "Jan";"Feb";"Mar";"Apr";"May";"Jun";
  "Jul";"Aug";"Sep";"Oct";"Nov";"Dec"
:]

let month_number name =
  let rec find i =
    if String.equal (Stdlib_stable.Iarray.get months i) name then i + 1
    else find (i + 1) in
  find 0

let date_of_internal_date d =
  let s = Internal_date.to_string d in
  let month = String.sub s 3 3 in
  {day = int_of_string (String.trim (String.sub s 0 2));
   month = month_number month;
   year = int_of_string (String.sub s 7 4)}

let leap year = year mod 4 = 0 && (year mod 100 <> 0 || year mod 400 = 0)

let days_in_month year = function
  | 2 -> if leap year then 29 else 28
  | 4 | 6 | 9 | 11 -> 30
  | _ -> 31

let date_text {day; month; year} =
  Printf.sprintf "%d-%s-%04d" day (Stdlib_stable.Iarray.get months (month - 1))
    year

let control c = Char.code c < 0x20 || Char.code c = 0x7f

let quote s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter (fun c ->
    if c = '\\' || c = '"' then Buffer.add_char b '\\';
    Buffer.add_char b c) s;
  Buffer.add_char b '"';
  Buffer.contents b

(* A trailing "{n}" or "{n+}" would make the server read the next line as
   literal data. *)
let ends_with_literal_marker s =
  let s = String.trim s in
  let n = String.length s in
  n >= 3 && s.[n-1] = '}' &&
  let last = if s.[n-2] = '+' then n - 3 else n - 2 in
  let rec back i =
    if i >= 0 && s.[i] >= '0' && s.[i] <= '9' then back (i - 1) else i in
  let k = back last in
  k < last && k >= 0 && s.[k] = '{'

let balanced s =
  let n = String.length s in
  let rec scan i depth quoted =
    if i = n then depth = 0 && not quoted
    else match s.[i] with
      | '\\' when quoted ->
          i + 1 < n && (s.[i+1] = '\\' || s.[i+1] = '"') &&
          scan (i + 2) depth quoted
      | '"' -> scan (i + 1) depth (not quoted)
      | '(' when not quoted -> depth < 100 && scan (i + 1) (depth + 1) quoted
      | ')' when not quoted -> depth > 0 && scan (i + 1) (depth - 1) quoted
      | _ -> scan (i + 1) depth quoted in
  scan 0 0 false

let raw s =
  let invalid reason = Error (Invalid_raw {raw = s; reason}) in
  if String.trim s = "" then invalid "empty"
  else if String.exists control s then invalid "control character"
  else if ends_with_literal_marker s then invalid "ends in a literal marker"
  else if not (balanced s) then invalid "unbalanced parentheses or quotes"
  else Ok s

(* With [strict] false every check passes, which {!pp} relies on. *)
let quoted ~strict ~utf8 s =
  if not strict then Ok (quote s)
  else if String.exists control s || not (String.is_valid_utf_8 s) then
    Error (Unquotable s)
  else if not utf8 && String.exists (fun c -> Char.code c >= 0x80) s then
    Error (Needs_utf8 s)
  else Ok (quote s)

let invalid ~strict key reason ok =
  if strict then Error (Invalid_key {key; reason}) else Ok ok

(* RFC 8474 objectid. *)
let objectid ~strict key s =
  let wire = key ^ " " ^ s in
  if s <> "" && String.length s <= 255 && String.for_all (function
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '-' -> true
    | _ -> false) s
  then Ok wire
  else invalid ~strict key "invalid object identifier" wire

let keyword ~strict key flag =
  let wire = key ^ " " ^ Mail_flag.Imap_flag.to_wire flag in
  match flag with
  | Mail_flag.Imap_flag.Keyword _ -> Ok wire
  | _ -> invalid ~strict key "not a keyword" wire

let size ~strict key n =
  let wire = Printf.sprintf "%s %Ld" key n in
  if n < 0L then invalid ~strict key "negative size" wire else Ok wire

let date ~strict key ({day; month; year} as d) =
  if year < 1 || year > 9999 || month < 1 || month > 12 || day < 1 ||
     day > days_in_month year month
  then invalid ~strict key "invalid date"
    (Printf.sprintf "%s %d-%d-%d" key day month year)
  else Ok (key ^ " " ^ date_text d)

let rec encode ~strict ~utf8 c =
  let string key s =
    let* s = quoted ~strict ~utf8 s in Ok (key ^ " " ^ s) in
  let date = date ~strict and keyword = keyword ~strict in
  let size = size ~strict and objectid = objectid ~strict in
  match c with
  | All -> Ok "ALL" | Answered -> Ok "ANSWERED" | Deleted -> Ok "DELETED"
  | Draft -> Ok "DRAFT" | Flagged -> Ok "FLAGGED" | Seen -> Ok "SEEN"
  | Unanswered -> Ok "UNANSWERED" | Undeleted -> Ok "UNDELETED"
  | Undraft -> Ok "UNDRAFT" | Unflagged -> Ok "UNFLAGGED"
  | Unseen -> Ok "UNSEEN" | New -> Ok "NEW" | Old -> Ok "OLD"
  | Recent -> Ok "RECENT" | Saved -> Ok "$"
  | Keyword flag -> keyword "KEYWORD" flag
  | Unkeyword flag -> keyword "UNKEYWORD" flag
  | Bcc s -> string "BCC" s | Cc s -> string "CC" s
  | From s -> string "FROM" s | To s -> string "TO" s
  | Subject s -> string "SUBJECT" s | Body s -> string "BODY" s
  | Text s -> string "TEXT" s
  | Header (name, value) ->
      let* name = if name = "" then
          invalid ~strict "HEADER" "empty field name" (quote name)
        else quoted ~strict ~utf8 name in
      let* value = quoted ~strict ~utf8 value in
      Ok ("HEADER " ^ name ^ " " ^ value)
  | Before d -> date "BEFORE" d | On d -> date "ON" d
  | Since d -> date "SINCE" d | Sentbefore d -> date "SENTBEFORE" d
  | Senton d -> date "SENTON" d | Sentsince d -> date "SENTSINCE" d
  | Larger n -> size "LARGER" n
  | Smaller n -> size "SMALLER" n
  | Uid set when Uid_set.is_empty set ->
      invalid ~strict "UID" "empty UID set" "UID (empty)"
  | Uid set -> Ok ("UID " ^ Uid_set.to_wire set)
  | Modseq m -> Ok ("MODSEQ " ^ Modseq.to_string m)
  | Emailid s -> objectid "EMAILID" s
  | Threadid s -> objectid "THREADID" s
  | Not c -> let* c = operand ~strict ~utf8 c in Ok ("NOT " ^ c)
  | Or (a, b) ->
      let* a = operand ~strict ~utf8 a in
      let* b = operand ~strict ~utf8 b in
      Ok ("OR " ^ a ^ " " ^ b)
  | And [] -> Ok "ALL"
  | And cs ->
      let* keys = List.fold_right (fun c acc ->
        let* rest = acc in
        let* c = encode ~strict ~utf8 c in
        Ok (c :: rest)) cs (Ok []) in
      Ok (String.concat " " keys)
  | Raw s when strict -> raw s
  | Raw s -> Ok s

(* An operand of NOT or OR is one search key, so a conjunction or a raw
   fragment is grouped. *)
and operand ~strict ~utf8 = function
  | And [c] -> operand ~strict ~utf8 c
  | And (_ :: _ :: _) | Raw _ as c ->
      let* c = encode ~strict ~utf8 c in Ok ("(" ^ c ^ ")")
  | c -> encode ~strict ~utf8 c

let to_wire ~utf8 c = encode ~strict:true ~utf8 c

let pp ppf c =
  match encode ~strict:false ~utf8:true c with
  | Ok wire -> Format.pp_print_string ppf wire
  | Error e -> pp_error ppf e

let rec needs acc = function
  | Modseq _ -> Capability.Condstore :: acc
  | Saved -> Capability.Searchres :: acc
  | Emailid _ | Threadid _ -> Capability.Objectid :: acc
  | Not c -> needs acc c
  | Or (a, b) -> needs (needs acc a) b
  | And cs -> List.fold_left needs acc cs
  | _ -> acc

let capabilities c =
  List.fold_left (fun acc cap ->
    if List.exists (Capability.equal cap) acc then acc else cap :: acc)
    [] (List.rev (needs [] c))
  |> List.rev

let sequence_set token =
  token <> "" && String.for_all (function
    | '0'..'9' | ',' | ':' | '*' -> true
    | _ -> false) token

let rec uidonly_safe = function
  | Raw s ->
      let first = match String.split_on_char ' ' (String.trim s) with
        | first :: _ -> first | [] -> "" in
      not (sequence_set first)
  | Not c -> uidonly_safe c
  | Or (a, b) -> uidonly_safe a && uidonly_safe b
  | And cs -> List.for_all uidonly_safe cs
  | _ -> true

let rec equal a b =
  match a, b with
  | Keyword x, Keyword y | Unkeyword x, Unkeyword y ->
      Mail_flag.Imap_flag.equal x y
  | Uid x, Uid y -> Uid_set.equal x y
  | Modseq x, Modseq y -> Modseq.equal x y
  | Not x, Not y -> equal x y
  | Or (a, b), Or (c, d) -> equal a c && equal b d
  | And xs, And ys ->
      List.length xs = List.length ys && List.for_all2 equal xs ys
  | (Keyword _ | Unkeyword _ | Uid _ | Modseq _ | Not _ | Or _ | And _), _
  | _, (Keyword _ | Unkeyword _ | Uid _ | Modseq _ | Not _ | Or _ | And _) ->
      false
  | a, b -> a = b
