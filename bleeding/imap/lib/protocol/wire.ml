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
  mutable offset : int;
  mutable after_literal : bool;
  mutable response_data : bool;
  mutable failed : error option;
}

let create ?(max_control=1_048_576) ?(max_literal=1_073_741_824L) () =
  if max_control < 16 || max_literal < 0L then invalid_arg "Imap.Wire.create";
  {max_control; max_literal; line=Buffer.create 128; state=Line;
   offset=0; after_literal=false; response_data=false; failed=None}

let is_digit c = c >= '0' && c <= '9'

let prefix_ci s ~at prefix =
  let n = String.length prefix in
  at + n <= String.length s &&
  let mutable i = 0 in
  while i < n && Char.uppercase_ascii s.[at+i] = prefix.[i] do i <- i + 1 done;
  i = n

let data_keywords =
  ["LIST ";"LSUB ";"XLIST ";"STATUS ";"NAMESPACE ";"ID ";"METADATA ";
   "QUOTA ";"QUOTAROOT ";"ACL ";"LISTRIGHTS ";"MYRIGHTS ";"ESEARCH ";
   "LANGUAGE "]

let rec keyword_at s = function
  | [] -> false
  | keyword :: rest -> prefix_ci s ~at:2 keyword || keyword_at s rest

(* Only data response grammars may contain literals. A response-text suffix
   such as "* OK explanation {123}" is plain text. *)
let data_response s =
  String.starts_with ~prefix:"* " s &&
  (keyword_at s data_keywords ||
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

let fail t message =
  t.failed <- Some {offset=Int64.of_int t.offset; message}

let feed t input =
  match t.failed with Some e -> Error e | None ->
  let len = String.length input in
  let mutable events = [] in
  let mutable i = 0 in
  while Option.is_none t.failed && i < len do
    match t.state with
    | Literal remaining ->
        let take = Int64.to_int (Int64.min remaining (Int64.of_int (len-i))) in
        events <- Literal_chunk (String.sub input i take) :: events;
        t.offset <- t.offset + take;
        let left = Int64.sub remaining (Int64.of_int take) in
        if left = 0L then (
          events <- Literal_end :: events;
          t.state <- Line; t.after_literal <- true
        ) else t.state <- Literal left;
        i <- i + take
    | Line ->
        let c = input.[i] in
        let current = Buffer.length t.line in
        let after_cr = current > 0 && Buffer.nth t.line (current-1) = '\r' in
        if current >= t.max_control then fail t "control response exceeds limit"
        else if after_cr && c <> '\n' then fail t "CR not followed by LF"
        else if c = '\n' && not after_cr then fail t "LF without CR"
        else (
          Buffer.add_char t.line c;
          t.offset <- t.offset + 1;
          i <- i + 1;
          if c = '\n' then begin
            let line = Buffer.contents t.line in
            Buffer.clear t.line;
            if not t.after_literal then t.response_data <- data_response line;
            match (if t.response_data then literal_marker line else No_literal)
            with
            | Non_sync ->
                fail t "server literal may not use non-synchronizing marker"
            | Oversized -> fail t "literal exceeds limit"
            | Literal_size size when size > t.max_literal ->
                fail t "literal exceeds limit"
            | Literal_size size ->
                events <- Literal_start size :: Text line :: events;
                if size = 0L then (
                  events <- Literal_end :: events; t.after_literal <- true
                ) else t.state <- Literal size
            | No_literal ->
                events <- End_of_response :: Text line :: events;
                t.after_literal <- false; t.response_data <- false
          end)
  done;
  match t.failed with
  | Some e when events = [] -> Error e
  | _ -> Ok (List.rev events)

let finish t =
  match t.failed with
  | Some e -> Error e
  | None ->
      match t.state with
      | Literal _ ->
          Error {offset=Int64.of_int t.offset; message="truncated literal"}
      | Line when Buffer.length t.line <> 0 ->
          Error {offset=Int64.of_int t.offset;
                 message="truncated response line"}
      | Line when t.after_literal ->
          Error {offset=Int64.of_int t.offset;
                 message="truncated response after literal"}
      | Line -> Ok ()
