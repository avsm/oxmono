(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire <thomas@gazagnaire.org>. All rights
  reserved. SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Zipper over Value.t. Frames carry enough context to rebuild the parent
   container exactly: the surrounding key/index siblings in original order and
   the parent container's Meta.t. This lets [set] / [modify] replace a leaf
   while leaving every parent's source range untouched. *)

(* A frame describes the step taken from the parent to the focus, plus the
   siblings on either side and the parent's metadata. [before] is stored in
   reverse order (head = closest to focus) so descending is O(1); [up]
   re-reverses when rebuilding. *)
type frame =
  | Table_frame of {
      key : Value.name;
      before : (Value.name * Value.t) list;
      after : (Value.name * Value.t) list;
      meta : Loc.Meta.t;
    }
  | Array_frame of {
      index : int;
      before : Value.t list;
      after : Value.t list;
      meta : Loc.Meta.t;
    }

type t = { focus : Value.t; context : frame list }

let root v = { focus = v; context = [] }
let focus c = c.focus

let rebuild_frame focus = function
  | Table_frame { key; before; after; meta } ->
      let pairs = List.rev_append before ((key, focus) :: after) in
      Value.Table (pairs, meta)
  | Array_frame { before; after; meta; _ } ->
      let items = List.rev_append before (focus :: after) in
      Value.Array (items, meta)

let up c =
  match c.context with
  | [] -> None
  | frame :: rest ->
      Some { focus = rebuild_frame c.focus frame; context = rest }

let rec top c =
  match c.context with
  | [] -> c.focus
  | _ -> ( match up c with None -> assert false | Some c' -> top c')

(* Descend into a table entry named [name]. Splits the pair list into
   (reversed-before, focus, after) and preserves the key's original Meta (from
   its source position) in the frame. *)
let down_field name c =
  match c.focus with
  | Value.Table (pairs, meta) ->
      let rec loop before = function
        | [] -> None
        | (((k, _) as key_node), v) :: rest when k = name ->
            let frame =
              Table_frame { key = key_node; before; after = rest; meta }
            in
            Some { focus = v; context = frame :: c.context }
        | pair :: rest -> loop (pair :: before) rest
      in
      loop [] pairs
  | _ -> None

let down_index n c =
  match c.focus with
  | Value.Array (items, meta) when n >= 0 ->
      let rec loop i before = function
        | [] -> None
        | v :: rest when i = n ->
            let frame = Array_frame { index = n; before; after = rest; meta } in
            Some { focus = v; context = frame :: c.context }
        | v :: rest -> loop (i + 1) (v :: before) rest
      in
      loop 0 [] items
  | _ -> None

(* [v] with metadata [m]. Local because the only reason to rewrite a node's
   metadata is to say where it now stands, which is what [set] below does. *)
let with_meta v m =
  match v with
  | Value.String (x, _) -> Value.String (x, m)
  | Value.Int (x, _) -> Value.Int (x, m)
  | Value.Float (x, _) -> Value.Float (x, m)
  | Value.Bool (x, _) -> Value.Bool (x, m)
  | Value.Datetime (x, _) -> Value.Datetime (x, m)
  | Value.Datetime_local (x, _) -> Value.Datetime_local (x, m)
  | Value.Date_local (x, _) -> Value.Date_local (x, m)
  | Value.Time_local (x, _) -> Value.Time_local (x, m)
  | Value.Array (x, _) -> Value.Array (x, m)
  | Value.Table (x, _) -> Value.Table (x, m)

(* A replacement stands where what it replaced stood. It takes that node's
   location, so a layout-preserving write knows which bytes of the document it
   goes over, and drops the source text, so those bytes are written afresh
   rather than copied back in its place. Everything around it -- the key, the
   spacing, the comment at the end of the line -- is untouched and is copied.

   A value that brought its own location from elsewhere loses it: where a node
   came from says nothing about where it now is. *)
let in_place_of ~replaced v =
  let loc = Loc.Meta.loc (Value.meta replaced) in
  if Loc.is_none loc then v
  else with_meta v (Loc.Meta.clear_text (Loc.Meta.with_loc (Value.meta v) loc))

let set v c = { c with focus = in_place_of ~replaced:c.focus v }
let modify f c = set (f c.focus) c

let frame_step = function
  | Table_frame { key; _ } -> Loc.Path.Mem key
  | Array_frame { index; meta; _ } -> Loc.Path.Nth (index, meta)

let path c =
  List.fold_left
    (fun p f -> Loc.Path.push (frame_step f) p)
    Loc.Path.root (List.rev c.context)

(* Pointer syntax.

   Grammar (EBNF, double-quote shown as DQ):

   pointer ::= slash | empty | segment (dot segment)* segment ::= bare_key
   index* | quoted_key index* bare_key ::= [A-Za-z0-9_-]+ (TOML bare-key
   alphabet) quoted_key::= DQ (escape | non-DQ-non-backslash)* DQ index ::= [
   digits ]

   For example, [a.b[2].c] parses as: bare_key a, bare_key b with index 2,
   bare_key c.

   Quoted keys accept TOML 1.1 escapes: backslash-DQ, backslash- backslash, \b
   \t \n \f \r \uXXXX \UXXXXXXXX. *)

(* UTF-8 encode a scalar value. Mirrors Buffer.add_utf_8_uchar semantics but we
   implement it by hand because the parser inputs a plain string and we want
   failure (None) rather than exceptions on out-of-range. *)
let add_uchar buf u =
  if u < 0 || u > 0x10FFFF || (u >= 0xD800 && u <= 0xDFFF) then false
  else begin
    Buffer.add_utf_8_uchar buf (Uchar.of_int u);
    true
  end

let rec hex_loop s pos n i acc =
  if i = n then Some acc
  else
    match Ascii.hex_value_int s.[pos + i] with
    | -1 -> None
    | d -> hex_loop s pos n (i + 1) ((acc lsl 4) lor d)

let parse_hex s pos n =
  if pos + n > String.length s then None else hex_loop s pos n 0 0

let add_hex_escape buf s pos width next =
  match parse_hex s pos width with
  | None -> None
  | Some u -> if add_uchar buf u then Some next else None

(* Process the byte at s.[i + 1] following a backslash inside a quoted key. On
   success returns the index to continue parsing from (one past the escape
   sequence). On malformed input returns None. *)
let consume_quoted_escape buf s len i =
  if i + 1 >= len then None
  else
    match s.[i + 1] with
    | '"' ->
        Buffer.add_char buf '"';
        Some (i + 2)
    | '\\' ->
        Buffer.add_char buf '\\';
        Some (i + 2)
    | 'b' ->
        Buffer.add_char buf '\b';
        Some (i + 2)
    | 't' ->
        Buffer.add_char buf '\t';
        Some (i + 2)
    | 'n' ->
        Buffer.add_char buf '\n';
        Some (i + 2)
    | 'f' ->
        Buffer.add_char buf '\012';
        Some (i + 2)
    | 'r' ->
        Buffer.add_char buf '\r';
        Some (i + 2)
    | 'u' -> add_hex_escape buf s (i + 2) 4 (i + 6)
    | 'U' -> add_hex_escape buf s (i + 2) 8 (i + 10)
    | _ -> None

(* Parse a quoted key starting at s.(pos); pos must point at the opening
   double-quote. Returns Some (unescaped, next_pos) on success (next_pos is past
   the closing quote), or None on malformed input. *)
let rec parse_quoted_key_loop buf s len i =
  if i >= len then None
  else
    match s.[i] with
    | '"' -> Some (Buffer.contents buf, i + 1)
    | '\\' -> (
        match consume_quoted_escape buf s len i with
        | None -> None
        | Some next -> parse_quoted_key_loop buf s len next)
    | c ->
        Buffer.add_char buf c;
        parse_quoted_key_loop buf s len (i + 1)

let parse_quoted_key s pos =
  let len = String.length s in
  if pos >= len || s.[pos] <> '"' then None
  else parse_quoted_key_loop (Buffer.create 16) s len (pos + 1)

(* Bare-key alphabet per TOML 1.1: A-Z a-z 0-9 - _ *)
let is_bare_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' -> true
  | _ -> false

let parse_bare_key s pos =
  let len = String.length s in
  let rec loop i = if i < len && is_bare_char s.[i] then loop (i + 1) else i in
  let stop = loop pos in
  if stop = pos then None else Some (String.sub s pos (stop - pos), stop)

(* Continue scanning index digits/closing bracket inside [N]. [seen] is true
   once at least one digit has been consumed. *)
let rec scan_index_digits s len i acc seen =
  if i >= len then None
  else
    match s.[i] with
    | ']' when seen -> Some (acc, i + 1)
    | '0' .. '9' as c ->
        scan_index_digits s len (i + 1)
          ((acc * 10) + (Char.code c - Char.code '0'))
          true
    | _ -> None

(* Parse one [N] index suffix at s.[pos]. *)
let parse_index s pos =
  let len = String.length s in
  if pos >= len || s.[pos] <> '[' then None
  else scan_index_digits s len (pos + 1) 0 false

(* A parsed step: a key plus zero or more array indices. *)
type step = { name : string; indices : int list }

let rec collect_indices s pos acc =
  if pos < String.length s && s.[pos] = '[' then
    match parse_index s pos with
    | None -> None
    | Some (n, pos') -> collect_indices s pos' (n :: acc)
  else Some (List.rev acc, pos)

(* Parse a single segment starting at pos. Returns step and new pos, or
   [None]. *)
let parse_segment s pos =
  let base =
    if pos < String.length s && s.[pos] = '"' then parse_quoted_key s pos
    else parse_bare_key s pos
  in
  match base with
  | None -> None
  | Some (name, pos) -> (
      match collect_indices s pos [] with
      | None -> None
      | Some (indices, pos) -> Some ({ name; indices }, pos))

(* Continue parsing remaining segments after one was consumed at [pos']. *)
let rec parse_pointer_loop s len pos acc =
  match parse_segment s pos with
  | None -> None
  | Some (step, pos') ->
      if pos' = len then Some (List.rev (step :: acc))
      else if s.[pos'] = '.' then
        if pos' + 1 = len then None (* trailing dot *)
        else parse_pointer_loop s len (pos' + 1) (step :: acc)
      else None

(* Parse a whole pointer string into a list of steps. *)
let parse_pointer s =
  let len = String.length s in
  if len = 0 || s = "/" then Some [] else parse_pointer_loop s len 0 []

let rec descend_indices indices c =
  match indices with
  | [] -> Some c
  | n :: rest -> (
      match down_index n c with
      | None -> None
      | Some c' -> descend_indices rest c')

let rec walk_steps c = function
  | [] -> Some c
  | { name; indices } :: rest -> (
      match down_field name c with
      | None -> None
      | Some c' -> (
          match descend_indices indices c' with
          | None -> None
          | Some c'' -> walk_steps c'' rest))

let of_pointer p c =
  match parse_pointer p with None -> None | Some steps -> walk_steps c steps

(* Serialise a key: bare if possible, else quoted with TOML escapes. *)
let is_bare_key_string s =
  let len = String.length s in
  if len = 0 then false
  else
    let rec loop i =
      if i >= len then true
      else if is_bare_char s.[i] then loop (i + 1)
      else false
    in
    loop 0

let escape_quoted buf s =
  let ppf = Fmt.with_buffer buf in
  Fmt.pf ppf "\"";
  let n = String.length s in
  let rec loop i =
    if i >= n then ()
    else
      let d = String.get_utf_8_uchar s i in
      if not (Uchar.utf_decode_is_valid d) then
        (* Fall back to byte-level quoting for malformed input; the encoder
           elsewhere validates UTF-8, so this is defensive. *)
        Fmt.pf ppf "%s" (String.escaped s)
      else
        let next = i + Uchar.utf_decode_length d in
        let c = Uchar.to_int (Uchar.utf_decode_uchar d) in
        (match c with
        | 0x22 -> Fmt.pf ppf "\\\""
        | 0x5C -> Fmt.pf ppf "\\\\"
        | 0x08 -> Fmt.pf ppf "\\b"
        | 0x09 -> Fmt.pf ppf "\\t"
        | 0x0A -> Fmt.pf ppf "\\n"
        | 0x0C -> Fmt.pf ppf "\\f"
        | 0x0D -> Fmt.pf ppf "\\r"
        | u when u < 0x20 || u = 0x7F -> Fmt.pf ppf "\\u%04X" u
        | _ ->
            Fmt.pf ppf "%!";
            Buffer.add_utf_8_uchar buf (Uchar.utf_decode_uchar d));
        loop next
  in
  loop 0;
  Fmt.pf ppf "\"%!"

let add_segment_name buf s =
  if is_bare_key_string s then Buffer.add_string buf s else escape_quoted buf s

let add_pointer_frame buf ppf first = function
  | Table_frame { key = name, _; _ } ->
      if !first then first := false else Fmt.pf ppf ".";
      Fmt.pf ppf "%!";
      add_segment_name buf name
  | Array_frame { index; _ } ->
      if !first then first := false;
      Fmt.pf ppf "[%d]" index

(* Split the context into dotted-key segments. Consecutive frames describing a
   key followed by N array steps coalesce into one segment of the form
   key[N1][N2].... A leading Array_frame (cursor descended into an array without
   a preceding table key) is not representable in canonical dotted-key syntax;
   we still emit it as a bare [N] index so the result round-trips for positions
   reachable from a Table root. *)
let to_pointer c =
  if c.context = [] then "/"
  else
    let frames = List.rev c.context in
    let buf = Buffer.create 32 in
    let ppf = Fmt.with_buffer buf in
    let first = ref true in
    List.iter (add_pointer_frame buf ppf first) frames;
    Fmt.pf ppf "%!";
    Buffer.contents buf

let pp ppf c = Fmt.string ppf (to_pointer c)
