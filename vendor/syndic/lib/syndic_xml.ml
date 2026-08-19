type dtd = string option

module Error = Syndic_error

type pos = Xmlm.pos
type tag = Xmlm.tag
type t = Node of pos * tag * t list | Data of pos * string

let resolve ~xmlbase uri =
  match xmlbase with None -> uri | Some b -> Uriz.resolve ~base:b uri

(* A byte that RFC 3986 admits in some component of a URI reference.  The set
   is the union over components, so it keeps every character that carries
   structure and rejects only what no component allows. *)
let is_uri_byte = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> true
  | '-' | '.' | '_' | '~' -> true
  | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' -> true
  | ':' | '@' | '/' | '?' | '#' | '[' | ']' | '%' -> true
  | _ -> false

let is_hex_digit = function
  | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true
  | _ -> false

(* [authority_span s] is the half-open range of [s] holding the authority, or
   [(-1, -1)] when there is none.  It reads the text rather than parsing it,
   because [s] is by assumption not a valid reference.  Brackets are legal
   only inside an authority, so the repair pass has to know where it is. *)
let authority_span s =
  let n = String.length s in
  let rec scheme_end i =
    if i >= n then -1
    else
      match s.[i] with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '+' | '-' | '.' ->
          scheme_end (i + 1)
      | ':' when i > 0 -> i + 1
      | _ -> -1
  in
  let start =
    match if n = 0 then ' ' else s.[0] with
    | 'A' .. 'Z' | 'a' .. 'z' -> ( match scheme_end 0 with -1 -> 0 | i -> i )
    | _ -> 0
  in
  if start + 1 < n && s.[start] = '/' && s.[start + 1] = '/' then (
    let i = ref (start + 2) in
    while
      !i < n && match s.[!i] with '/' | '?' | '#' -> false | _ -> true
    do
      incr i
    done ;
    (start + 2, !i) )
  else (-1, -1)

(* [repair s] percent-encodes the bytes of [s] that no component of a URI
   reference admits, and leaves everything that carries structure alone.  A
   space, a raw UTF-8 byte and a bracket outside the authority are encoded, a
   ['%'] that does not start a triplet becomes ["%25"], and a second ['#'] is
   encoded because only the first opens the fragment.  Nothing else moves, so
   the scheme, the authority and the segment boundaries survive. *)
let repair s =
  let n = String.length s in
  let a_start, a_stop = authority_span s in
  let b = Buffer.create (n + 8) in
  let hex = "0123456789ABCDEF" in
  let seen_hash = ref false in
  let encode c =
    Buffer.add_char b '%' ;
    Buffer.add_char b hex.[Char.code c lsr 4] ;
    Buffer.add_char b hex.[Char.code c land 15]
  in
  for i = 0 to n - 1 do
    let c = s.[i] in
    match c with
    | '%' when i + 2 < n && is_hex_digit s.[i + 1] && is_hex_digit s.[i + 2] ->
        Buffer.add_char b '%'
    | '%' -> encode '%'
    | '[' | ']' when not (i >= a_start && i < a_stop) -> encode c
    | '#' when !seen_hash -> encode c
    | '#' ->
        seen_hash := true ;
        Buffer.add_char b '#'
    | c when is_uri_byte c -> Buffer.add_char b c
    | c -> encode c
  done ;
  Buffer.contents b

let uri_of_string s =
  match Uriz.of_string s with
  | This u -> u
  | Null -> (
    match Uriz.of_string (repair s) with
    | This u -> u
    | Null ->
        (* Every byte is now unreserved or part of a triplet, and no ['/']
           and no [':'] are left, so the result is a single [segment-nz-nc]
           and parses.  The empty string parses as [path-empty]. *)
        Uriz.of_string_exn (Uriz.pct_encode ~component:`Unreserved s) )

(* Specialized version of the Xmlm.make_input one. *)
let input_of_channel fh =
  (* Xmlm.make_input does not raise any exception. *)
  Xmlm.make_input (`Channel fh)

let of_xmlm input =
  let el tag datas = Node (Xmlm.pos input, tag, datas) in
  let data data = Data (Xmlm.pos input, data) in
  try Xmlm.input_doc_tree ~el ~data input with Xmlm.Error (pos, e) ->
    raise (Error.Error (pos, Xmlm.error_message e))

let get_position = function Node (pos, _, _) -> pos | Data (pos, _) -> pos

let rec t_to_xmlm t output =
  match t with
  | Data (_pos, d) -> (
    try Xmlm.output output (`Data d) with Xmlm.Error (pos, e) ->
      raise (Error.Error (pos, Xmlm.error_message e)) )
  | Node (_pos, tag, t_sub) -> (
      Xmlm.output output (`El_start tag) ;
      List.iter (fun t -> t_to_xmlm t output) t_sub ;
      try Xmlm.output output `El_end with Xmlm.Error (pos, e) ->
        raise (Error.Error (pos, Xmlm.error_message e)) )

(* Specialized version of the Xmlm one. *)
let make_output ?ns_prefix dest =
  (* Xmlm.make_output does not raise any exception. *)
  Xmlm.make_output dest ~decl:true ?ns_prefix

let to_xmlm ?dtd t output =
  ( try Xmlm.output output (`Dtd dtd) with Xmlm.Error (pos, e) ->
      raise (Error.Error (pos, Xmlm.error_message e)) ) ;
  t_to_xmlm t output

let to_buffer ?ns_prefix t b =
  let output = Xmlm.make_output ~decl:false (`Buffer b) ?ns_prefix in
  to_xmlm t output

let to_string ?ns_prefix t =
  let b = Buffer.create 4096 in
  to_buffer ?ns_prefix t b ; Buffer.contents b
