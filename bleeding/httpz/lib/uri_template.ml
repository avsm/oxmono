(*
 * Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type value =
  [ `String of string
  | `List of string list
  | `Assoc of (string * string) list ]

type error = {
  offset : int;
  message : string;
}

type operator =
  | Simple
  | Reserved
  | Fragment
  | Label
  | Path
  | Path_parameter
  | Query
  | Query_continuation

type modifier = Whole | Prefix of int | Explode

type varspec = {
  name : string;
  modifier : modifier;
  offset : int;
}

type part = Literal of string | Expression of operator * varspec list

type t = {
  source : string;
  parts : part list;
  variables : (string * int) list;
}

type level = [ `Level_1 | `Level_2 | `Level_3 | `Level_4 ]

let pp_error ppf (error : error) =
  Format.fprintf ppf "byte %d: %s" error.offset error.message

let error offset message = Error { offset; message }

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let is_hex = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let hex_value = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' as c -> Char.code c - Char.code 'A' + 10
  | _ -> -1

let is_unreserved = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '.' | '_' | '~' -> true
  | _ -> false

let is_reserved = function
  | ':' | '/' | '?' | '#' | '[' | ']' | '@'
  | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' ->
      true
  | _ -> false

let continuation c =
  let n = Char.code c in
  n >= 0x80 && n <= 0xbf

(* Strict UTF-8 decoding. The returned index is the first byte after the scalar
   value. Overlong encodings, surrogates, and values past U+10FFFF fail. *)
let utf8_next s i =
  let n = String.length s in
  let byte k = Char.code (String.unsafe_get s k) in
  if i >= n then None
  else
    let b0 = byte i in
    if b0 < 0x80 then Some (b0, i + 1)
    else if b0 >= 0xc2 && b0 <= 0xdf && i + 1 < n
            && continuation (String.unsafe_get s (i + 1))
    then
      Some (((b0 land 0x1f) lsl 6) lor (byte (i + 1) land 0x3f), i + 2)
    else if b0 = 0xe0 && i + 2 < n
            && byte (i + 1) >= 0xa0 && byte (i + 1) <= 0xbf
            && continuation (String.unsafe_get s (i + 2))
    then
      Some
        ( ((b0 land 0x0f) lsl 12)
          lor ((byte (i + 1) land 0x3f) lsl 6)
          lor (byte (i + 2) land 0x3f),
          i + 3 )
    else if ((b0 >= 0xe1 && b0 <= 0xec) || (b0 >= 0xee && b0 <= 0xef))
            && i + 2 < n
            && continuation (String.unsafe_get s (i + 1))
            && continuation (String.unsafe_get s (i + 2))
    then
      Some
        ( ((b0 land 0x0f) lsl 12)
          lor ((byte (i + 1) land 0x3f) lsl 6)
          lor (byte (i + 2) land 0x3f),
          i + 3 )
    else if b0 = 0xed && i + 2 < n
            && byte (i + 1) >= 0x80 && byte (i + 1) <= 0x9f
            && continuation (String.unsafe_get s (i + 2))
    then
      Some
        ( ((b0 land 0x0f) lsl 12)
          lor ((byte (i + 1) land 0x3f) lsl 6)
          lor (byte (i + 2) land 0x3f),
          i + 3 )
    else if b0 = 0xf0 && i + 3 < n
            && byte (i + 1) >= 0x90 && byte (i + 1) <= 0xbf
            && continuation (String.unsafe_get s (i + 2))
            && continuation (String.unsafe_get s (i + 3))
    then
      Some
        ( ((b0 land 0x07) lsl 18)
          lor ((byte (i + 1) land 0x3f) lsl 12)
          lor ((byte (i + 2) land 0x3f) lsl 6)
          lor (byte (i + 3) land 0x3f),
          i + 4 )
    else if b0 >= 0xf1 && b0 <= 0xf3 && i + 3 < n
            && continuation (String.unsafe_get s (i + 1))
            && continuation (String.unsafe_get s (i + 2))
            && continuation (String.unsafe_get s (i + 3))
    then
      Some
        ( ((b0 land 0x07) lsl 18)
          lor ((byte (i + 1) land 0x3f) lsl 12)
          lor ((byte (i + 2) land 0x3f) lsl 6)
          lor (byte (i + 3) land 0x3f),
          i + 4 )
    else if b0 = 0xf4 && i + 3 < n
            && byte (i + 1) >= 0x80 && byte (i + 1) <= 0x8f
            && continuation (String.unsafe_get s (i + 2))
            && continuation (String.unsafe_get s (i + 3))
    then
      Some
        ( ((b0 land 0x07) lsl 18)
          lor ((byte (i + 1) land 0x3f) lsl 12)
          lor ((byte (i + 2) land 0x3f) lsl 6)
          lor (byte (i + 3) land 0x3f),
          i + 4 )
    else None

let is_ucschar cp =
  (cp >= 0x00a0 && cp <= 0xd7ff)
  || (cp >= 0xf900 && cp <= 0xfdcf)
  || (cp >= 0xfdf0 && cp <= 0xffef)
  || (cp >= 0x10000 && cp <= 0xdfffd && cp land 0xffff <= 0xfffd)
  || (cp >= 0xe1000 && cp <= 0xefffd)

let is_iprivate cp =
  (cp >= 0xe000 && cp <= 0xf8ff)
  || (cp >= 0xf0000 && cp <= 0xffffd)
  || (cp >= 0x100000 && cp <= 0x10fffd)

let hex_upper = "0123456789ABCDEF"

let add_pct_byte b byte =
  Buffer.add_char b '%';
  Buffer.add_char b (String.unsafe_get hex_upper (byte lsr 4));
  Buffer.add_char b (String.unsafe_get hex_upper (byte land 0x0f))

let add_pct_run b s first last =
  for i = first to last - 1 do
    add_pct_byte b (Char.code (String.unsafe_get s i))
  done

let encode_literal s offset =
  let n = String.length s in
  let b = Buffer.create n in
  let rec loop i =
    if i = n then Ok (Buffer.contents b)
    else
      let c = String.unsafe_get s i in
      let code = Char.code c in
      if code < 0x80 then
        if c = '%' then
          if i + 2 < n && is_hex (String.unsafe_get s (i + 1))
             && is_hex (String.unsafe_get s (i + 2))
          then begin
            Buffer.add_substring b s i 3;
            loop (i + 3)
          end else error (offset + i) "malformed percent triplet in literal"
        else
          let allowed =
            code = 0x21 || code = 0x23 || code = 0x24
            (* Verified RFC 6570 erratum 6937 restores apostrophe here. *)
            || (code >= 0x26 && code <= 0x3b) || code = 0x3d
            || (code >= 0x3f && code <= 0x5b) || code = 0x5d
            || code = 0x5f || (code >= 0x61 && code <= 0x7a)
            || code = 0x7e
          in
          if allowed then begin
            Buffer.add_char b c;
            loop (i + 1)
          end else error (offset + i) "character is not allowed in a template literal"
      else
        match utf8_next s i with
        | Some (cp, next) when is_ucschar cp || is_iprivate cp ->
            add_pct_run b s i next;
            loop next
        | Some _ -> error (offset + i) "Unicode scalar is not allowed in a template literal"
        | None -> error (offset + i) "template is not valid UTF-8"
  in
  loop 0

let varchar_at s i stop =
  if i >= stop then 0
  else
    match String.unsafe_get s i with
    | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') -> 1
    | '%' when i + 2 < stop && is_hex (String.unsafe_get s (i + 1))
               && is_hex (String.unsafe_get s (i + 2)) ->
        3
    | _ -> 0

let operator = function
  | '+' -> Some Reserved
  | '#' -> Some Fragment
  | '.' -> Some Label
  | '/' -> Some Path
  | ';' -> Some Path_parameter
  | '?' -> Some Query
  | '&' -> Some Query_continuation
  | _ -> None

let reserved_operator = function
  | '=' | ',' | '!' | '@' | '|' -> true
  | _ -> false

let parse_expression source open_at close_at =
  let first = open_at + 1 in
  if first = close_at then error open_at "empty template expression"
  else
    let c = String.unsafe_get source first in
    if reserved_operator c then
      error first "reserved extension operator has no RFC 6570 semantics"
    else
      let op, start =
        match operator c with Some op -> op, first + 1 | None -> Simple, first
      in
      if start = close_at then error start "expression has no variable"
      else
        let rec parse_specs pos acc =
          if pos >= close_at then error pos "expression has an empty variable"
          else
            let name_start = pos in
            let first_width = varchar_at source pos close_at in
            if first_width = 0 then error pos "invalid variable name"
            else
              let rec name i =
                let width = varchar_at source i close_at in
                if width > 0 then name (i + width)
                else if i < close_at && String.unsafe_get source i = '.' then
                  let width = varchar_at source (i + 1) close_at in
                  if width = 0 then error i "a dot in a variable name must separate characters"
                  else name (i + 1 + width)
                else Ok i
              in
              match name (pos + first_width) with
              | Error _ as e -> e
              | Ok name_end ->
                  let name = String.sub source name_start (name_end - name_start) in
                  let parse_modifier () =
                    if name_end = close_at then Ok (Whole, name_end)
                    else
                      match String.unsafe_get source name_end with
                      | '*' -> Ok (Explode, name_end + 1)
                      | ':' ->
                          let digit_at = name_end + 1 in
                          if digit_at >= close_at
                             || String.unsafe_get source digit_at < '1'
                             || String.unsafe_get source digit_at > '9'
                          then error digit_at "a prefix length must start with 1 through 9"
                          else
                            let rec digits i count value =
                              if i < close_at && is_digit (String.unsafe_get source i) then
                                if count = 4 then
                                  error i "a prefix length has at most four digits"
                                else
                                  digits (i + 1) (count + 1)
                                    ((value * 10)
                                     + Char.code (String.unsafe_get source i)
                                     - Char.code '0')
                              else Ok (Prefix value, i)
                            in
                            digits digit_at 0 0
                      | _ -> Ok (Whole, name_end)
                  in
                  match parse_modifier () with
                  | Error _ as e -> e
                  | Ok (modifier, next) ->
                      let spec = { name; modifier; offset = name_start } in
                      if next = close_at then Ok (List.rev (spec :: acc))
                      else if String.unsafe_get source next <> ',' then
                        error next "expected a comma or the end of the expression"
                      else parse_specs (next + 1) (spec :: acc)
        in
        match parse_specs start [] with
        | Error _ as e -> e
        | Ok specs -> Ok (Expression (op, specs))

let of_string source =
  let n = String.length source in
  let seen_variables = Hashtbl.create 16 in
  let add_variable variables spec =
    if Hashtbl.mem seen_variables spec.name then variables
    else begin
      Hashtbl.add seen_variables spec.name ();
      (spec.name, spec.offset) :: variables
    end
  in
  let rec scan literal_start pos parts variables =
    if pos = n then
      match encode_literal (String.sub source literal_start (n - literal_start)) literal_start with
      | Error _ as e -> e
      | Ok literal ->
          let parts = if String.equal literal "" then parts else Literal literal :: parts in
          Ok { source; parts = List.rev parts; variables = List.rev variables }
    else
      match String.unsafe_get source pos with
      | '{' ->
          (match encode_literal (String.sub source literal_start (pos - literal_start)) literal_start with
           | Error _ as e -> e
           | Ok literal ->
               let parts = if String.equal literal "" then parts else Literal literal :: parts in
               (match String.index_from_opt source (pos + 1) '}' with
                | None -> error pos "unclosed template expression"
                | Some close_at ->
                    (match parse_expression source pos close_at with
                     | Error _ as e -> e
                     | Ok (Expression (_, specs) as expression) ->
                         let variables = List.fold_left add_variable variables specs in
                         scan (close_at + 1) (close_at + 1)
                           (expression :: parts) variables
                     | Ok (Literal _) -> assert false)))
      | _ -> scan literal_start (pos + 1) parts variables
  in
  scan 0 0 [] []

let of_string_exn source =
  match of_string source with
  | Ok template -> template
  | Error e -> invalid_arg (Format.asprintf "Uri_template.of_string: %a" pp_error e)

let to_string template = template.source
let pp ppf template = Format.pp_print_string ppf template.source
let variables template = List.map fst template.variables

let expression_level op specs =
  if List.exists (fun spec -> spec.modifier <> Whole) specs then `Level_4
  else
    match op, specs with
    | Simple, [ _ ] -> `Level_1
    | (Reserved | Fragment), [ _ ] -> `Level_2
    | _ -> `Level_3

let max_level left right =
  match left, right with
  | `Level_4, _ | _, `Level_4 -> `Level_4
  | `Level_3, _ | _, `Level_3 -> `Level_3
  | `Level_2, _ | _, `Level_2 -> `Level_2
  | `Level_1, `Level_1 -> `Level_1

let level template =
  List.fold_left
    (fun level -> function
      | Literal _ -> level
      | Expression (op, specs) -> max_level level (expression_level op specs))
    `Level_1 template.parts

let validate_utf8 s =
  let rec loop i =
    if i = String.length s then true
    else match utf8_next s i with Some (_, next) -> loop next | None -> false
  in
  loop 0

let value_valid = function
  | `String s -> validate_utf8 s
  | `List values -> List.for_all validate_utf8 values
  | `Assoc pairs ->
      List.for_all (fun (name, value) -> validate_utf8 name && validate_utf8 value) pairs

let value_defined = function
  | `String _ -> true
  | `List (_ :: _) | `Assoc (_ :: _) -> true
  | `List [] | `Assoc [] -> false

let pct_byte_at s i =
  if i + 2 < String.length s && String.unsafe_get s i = '%' then
    let hi = hex_value (String.unsafe_get s (i + 1)) in
    let lo = hex_value (String.unsafe_get s (i + 2)) in
    if hi >= 0 && lo >= 0 then Some ((hi lsl 4) lor lo) else None
  else None

(* A percent-encoded UTF-8 scalar is one character for a prefix modifier. If
   the triplets do not form a scalar, treating the first triplet as one
   character still avoids ever splitting that triplet. *)
let pct_scalar_end s i =
  let continuation_at j =
    match pct_byte_at s j with Some b when b >= 0x80 && b <= 0xbf -> true | _ -> false
  in
  match pct_byte_at s i with
  | None -> i
  | Some b0 when b0 < 0x80 -> i + 3
  | Some b0 when b0 >= 0xc2 && b0 <= 0xdf && continuation_at (i + 3) ->
      i + 6
  | Some 0xe0 ->
      (match pct_byte_at s (i + 3) with
       | Some b1 when b1 >= 0xa0 && b1 <= 0xbf && continuation_at (i + 6) -> i + 9
       | _ -> i + 3)
  | Some b0 when (b0 >= 0xe1 && b0 <= 0xec) || (b0 >= 0xee && b0 <= 0xef) ->
      if continuation_at (i + 3) && continuation_at (i + 6) then i + 9 else i + 3
  | Some 0xed ->
      (match pct_byte_at s (i + 3) with
       | Some b1 when b1 >= 0x80 && b1 <= 0x9f && continuation_at (i + 6) -> i + 9
       | _ -> i + 3)
  | Some 0xf0 ->
      (match pct_byte_at s (i + 3) with
       | Some b1 when b1 >= 0x90 && b1 <= 0xbf
                      && continuation_at (i + 6) && continuation_at (i + 9) ->
           i + 12
       | _ -> i + 3)
  | Some b0 when b0 >= 0xf1 && b0 <= 0xf3 ->
      if continuation_at (i + 3) && continuation_at (i + 6)
         && continuation_at (i + 9)
      then i + 12 else i + 3
  | Some 0xf4 ->
      (match pct_byte_at s (i + 3) with
       | Some b1 when b1 >= 0x80 && b1 <= 0x8f
                      && continuation_at (i + 6) && continuation_at (i + 9) ->
           i + 12
       | _ -> i + 3)
  | Some _ -> i + 3

let prefix_end s count =
  let n = String.length s in
  let rec loop i left =
    if i = n || left = 0 then i
    else if pct_byte_at s i <> None then loop (pct_scalar_end s i) (left - 1)
    else
      match utf8_next s i with
      | Some (_, next) -> loop next (left - 1)
      | None -> assert false
  in
  loop 0 count

let add_encoded b ~allow_reserved s =
  let n = String.length s in
  let rec loop i =
    if i < n then
      let c = String.unsafe_get s i in
      let code = Char.code c in
      if code < 0x80 then
        if is_unreserved c || (allow_reserved && is_reserved c) then begin
          Buffer.add_char b c;
          loop (i + 1)
        end else if allow_reserved && c = '%' && i + 2 < n
                    && is_hex (String.unsafe_get s (i + 1))
                    && is_hex (String.unsafe_get s (i + 2))
        then begin
          Buffer.add_substring b s i 3;
          loop (i + 3)
        end else begin
          add_pct_byte b code;
          loop (i + 1)
        end
      else
        match utf8_next s i with
        | Some (_, next) ->
            add_pct_run b s i next;
            loop next
        | None -> assert false
  in
  loop 0

type behavior = {
  first : char option;
  separator : char;
  named : bool;
  if_empty : char option;
  allow_reserved : bool;
}

let behavior = function
  | Simple ->
      { first = None; separator = ','; named = false; if_empty = None;
        allow_reserved = false }
  | Reserved ->
      { first = None; separator = ','; named = false; if_empty = None;
        allow_reserved = true }
  | Fragment ->
      { first = Some '#'; separator = ','; named = false; if_empty = None;
        allow_reserved = true }
  | Label ->
      { first = Some '.'; separator = '.'; named = false; if_empty = None;
        allow_reserved = false }
  | Path ->
      { first = Some '/'; separator = '/'; named = false; if_empty = None;
        allow_reserved = false }
  | Path_parameter ->
      { first = Some ';'; separator = ';'; named = true; if_empty = None;
        allow_reserved = false }
  | Query ->
      { first = Some '?'; separator = '&'; named = true; if_empty = Some '=';
        allow_reserved = false }
  | Query_continuation ->
      { first = Some '&'; separator = '&'; named = true; if_empty = Some '=';
        allow_reserved = false }

let add_name b name = Buffer.add_string b name

let add_named_value b config name value =
  add_name b name;
  if String.equal value "" then Option.iter (Buffer.add_char b) config.if_empty
  else begin
    Buffer.add_char b '=';
    add_encoded b ~allow_reserved:config.allow_reserved value
  end

let add_joined b separator add values =
  let rec loop = function
    | [] -> ()
    | [ value ] -> add value
    | value :: rest ->
        add value;
        Buffer.add_char b separator;
        loop rest
  in
  loop values

let render_value config spec value =
  let b = Buffer.create 32 in
  match value, spec.modifier with
  | `String value, modifier ->
      let value =
        match modifier with
        | Prefix count -> String.sub value 0 (prefix_end value count)
        | Whole | Explode -> value
      in
      if config.named then add_named_value b config spec.name value
      else add_encoded b ~allow_reserved:config.allow_reserved value;
      Ok (Buffer.contents b)
  | (`List _ | `Assoc _), Prefix _ ->
      error spec.offset "a prefix modifier cannot be applied to a composite value"
  | `List values, Whole ->
      if config.named then begin
        add_name b spec.name;
        Buffer.add_char b '='
      end;
      add_joined b ',' (add_encoded b ~allow_reserved:config.allow_reserved) values;
      Ok (Buffer.contents b)
  | `Assoc pairs, Whole ->
      if config.named then begin
        add_name b spec.name;
        Buffer.add_char b '='
      end;
      let add_pair (name, value) =
        add_encoded b ~allow_reserved:config.allow_reserved name;
        Buffer.add_char b ',';
        add_encoded b ~allow_reserved:config.allow_reserved value
      in
      add_joined b ',' add_pair pairs;
      Ok (Buffer.contents b)
  | `List values, Explode ->
      let add_value value =
        if config.named then add_named_value b config spec.name value
        else add_encoded b ~allow_reserved:config.allow_reserved value
      in
      add_joined b config.separator add_value values;
      Ok (Buffer.contents b)
  | `Assoc pairs, Explode ->
      let add_pair (name, value) =
        add_encoded b ~allow_reserved:config.allow_reserved name;
        if String.equal value "" then begin
          if config.named then Option.iter (Buffer.add_char b) config.if_empty
        end
        else begin
          Buffer.add_char b '=';
          add_encoded b ~allow_reserved:config.allow_reserved value
        end
      in
      add_joined b config.separator add_pair pairs;
      Ok (Buffer.contents b)

let expand_expression output lookup op specs =
  let config = behavior op in
  let emitted = ref false in
  let rec loop = function
    | [] -> Ok ()
    | spec :: rest ->
        (match lookup spec.name with
         | None -> loop rest
         | Some value when not (value_defined value) -> loop rest
         | Some value when not (value_valid value) ->
             error spec.offset (Printf.sprintf "value for %S is not valid UTF-8" spec.name)
         | Some value ->
             match render_value config spec value with
             | Error _ as e -> e
             | Ok rendered ->
                 if !emitted then Buffer.add_char output config.separator
                 else begin
                   Option.iter (Buffer.add_char output) config.first;
                   emitted := true
                 end;
                 Buffer.add_string output rendered;
                 loop rest)
  in
  loop specs

let expand template lookup =
  (* Resolve once so a stateful lookup cannot violate RFC 6570's requirement
     that repeated uses of a variable remain static during one expansion. *)
  let values = Hashtbl.create (List.length template.variables) in
  List.iter
    (fun (name, _) -> Hashtbl.add values name (lookup name))
    template.variables;
  let lookup name = Hashtbl.find_opt values name |> Option.join in
  let output = Buffer.create (String.length template.source + 32) in
  let rec loop = function
    | [] -> Ok (Buffer.contents output)
    | Literal literal :: rest ->
        Buffer.add_string output literal;
        loop rest
    | Expression (op, specs) :: rest ->
        (match expand_expression output lookup op specs with
         | Ok () -> loop rest
         | Error _ as e -> e)
  in
  loop template.parts

let assoc_lookup bindings =
  let values = Hashtbl.create (List.length bindings) in
  List.iter
    (fun (name, value) ->
       if not (Hashtbl.mem values name) then Hashtbl.add values name value)
    bindings;
  fun name -> Hashtbl.find_opt values name

let expand_assoc template bindings = expand template (assoc_lookup bindings)

let expand_uri template lookup =
  match expand template lookup with
  | Error _ as e -> e
  | Ok expanded ->
      (match Httpz_uri.of_string expanded with
       | This uri -> Ok uri
       | Null -> error 0 "expanded text is not a valid RFC 3986 URI reference")

let expand_uri_assoc template bindings =
  expand_uri template (assoc_lookup bindings)

let expand_resolve ~(base : Httpz_uri.t @ local) template lookup =
  match expand_uri template lookup with
  | Error _ as error -> error
  | Ok reference -> Ok (Httpz_uri.resolve ~base reference)

let expand_resolve_assoc ~(base : Httpz_uri.t @ local) template bindings =
  expand_resolve ~base template (assoc_lookup bindings)
