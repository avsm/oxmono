type event =
  | Text of string
  | Literal_start of int64
  | Literal_chunk of string
  | Literal_end
  | End_of_response

type error = { offset : int64; message : string }
type state = Line | Literal of int64
type t = {
  max_control : int;
  max_literal : int64;
  line : Buffer.t;
  mutable state : state;
  mutable offset : int64;
  mutable after_literal : bool;
  mutable response_data : bool;
  mutable failed : error option;
}

let create ?(max_control=1_048_576) ?(max_literal=1_073_741_824L) () =
  if max_control < 16 || max_literal < 0L then invalid_arg "Imap.Wire.create";
  {max_control; max_literal; line=Buffer.create 128; state=Line;
   offset=0L; after_literal=false; response_data=false; failed=None}

let is_digit c = c >= '0' && c <= '9'

let prefix_ci s ~at prefix =
  let n = String.length prefix in
  at + n <= String.length s &&
  let rec same i =
    i = n || (Char.uppercase_ascii s.[at+i] = prefix.[i] && same (i+1)) in
  same 0

(* Only data response grammars may contain literals. A response-text suffix
   such as "* OK explanation {123}" is plain text. *)
let data_response s =
  String.starts_with ~prefix:"* " s &&
  (List.exists (prefix_ci s ~at:2)
     ["LIST ";"LSUB ";"XLIST ";"STATUS ";"NAMESPACE ";"ID ";"METADATA ";
      "QUOTA ";"QUOTAROOT ";"ACL ";"LISTRIGHTS ";"MYRIGHTS ";"ESEARCH ";
      "LANGUAGE "] ||
   let len = String.length s in
   let rec digits i = if i < len && is_digit s.[i] then digits (i+1) else i in
   let j = digits 2 in
   j > 2 && j < len && s.[j] = ' ' &&
   (prefix_ci s ~at:(j+1) "FETCH " || prefix_ci s ~at:(j+1) "UIDFETCH "))

type marker = No_literal | Literal_size of int64 | Oversized | Non_sync

(* RFC 3516 BINARY uses the same framing with a leading tilde. *)
let literal_marker s =
  let n = String.length s in
  if n < 5 || s.[n-2] <> '\r' || s.[n-1] <> '\n' || s.[n-3] <> '}' then
    No_literal
  else
  let non_sync = s.[n-4] = '+' in
  let last = if non_sync then n-5 else n-4 in
  let rec back i = if i >= 0 && is_digit s.[i] then back (i-1) else i in
  let k = back last in
  if k = last || k < 0 || s.[k] <> '{' then No_literal
  else if non_sync then Non_sync
  else
  (* RFC 9051 literals are tokens. A quoted "{n}" is not a literal. *)
  let rec outside i quoted escaped =
    if i >= k then not quoted else
    let c = s.[i] in
    if escaped then outside (i+1) quoted false
    else if quoted && c = '\\' then outside (i+1) quoted true
    else if c = '"' then outside (i+1) (not quoted) false
    else outside (i+1) quoted false in
  if not (outside 0 false false) then No_literal
  else match Int64.of_string_opt (String.sub s (k+1) (last-k)) with
    | Some v -> Literal_size v
    | None -> Oversized

let feed t input =
  match t.failed with Some e -> Error e | None ->
  let len = String.length input in
  let events = ref [] in
  let emit e = events := e :: !events in
  let err message =
    let e = {offset=t.offset; message} in
    t.failed <- Some e;
    if !events = [] then Error e else Ok (List.rev !events) in
  let rec loop i =
    if i = len then Ok (List.rev !events) else
    match t.state with
    | Literal remaining ->
        let take = Int64.to_int (Int64.min remaining (Int64.of_int (len-i))) in
        emit (Literal_chunk (String.sub input i take));
        t.offset <- Int64.add t.offset (Int64.of_int take);
        let left = Int64.sub remaining (Int64.of_int take) in
        if left = 0L then (
          emit Literal_end; t.state <- Line; t.after_literal <- true
        ) else t.state <- Literal left;
        loop (i+take)
    | Line ->
        let c = input.[i] in
        let current = Buffer.length t.line in
        if current >= t.max_control then err "control response exceeds limit"
        else if current > 0 && Buffer.nth t.line (current-1) = '\r' && c <> '\n'
          then err "CR not followed by LF"
        else if c = '\n' && (current = 0 || Buffer.nth t.line (current-1) <> '\r')
          then err "LF without CR"
        else (
          Buffer.add_char t.line c;
          t.offset <- Int64.succ t.offset;
          if c <> '\n' then loop (i+1) else
          let line = Buffer.contents t.line in
          Buffer.clear t.line;
          if not t.after_literal then t.response_data <- data_response line;
          match (if t.response_data then literal_marker line else No_literal)
          with
          | Non_sync ->
              err "server literal may not use non-synchronizing marker"
          | Oversized -> err "literal exceeds limit"
          | Literal_size size when size > t.max_literal ->
              err "literal exceeds limit"
          | Literal_size size ->
              emit (Text line); emit (Literal_start size);
              if size = 0L then (
                emit Literal_end; t.after_literal <- true
              ) else t.state <- Literal size;
              loop (i+1)
          | No_literal ->
              emit (Text line); emit End_of_response;
              t.after_literal <- false; t.response_data <- false;
              loop (i+1))
  in loop 0

let finish t =
  match t.failed with
  | Some e -> Error e
  | None ->
      match t.state with
      | Literal _ -> Error {offset=t.offset; message="truncated literal"}
      | Line when Buffer.length t.line <> 0 ->
          Error {offset=t.offset; message="truncated response line"}
      | Line when t.after_literal ->
          Error {offset=t.offset; message="truncated response after literal"}
      | Line -> Ok ()
