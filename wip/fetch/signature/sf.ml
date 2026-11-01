(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** RFC 8941 Structured Fields (minimal subset for HTTP Signatures) *)

(* Result monad syntax for cleaner parsing *)
let ( let* ) = Result.bind

type item =
  | String of string
  | Token of string
  | Integer of int64
  | Decimal of float
  | Boolean of bool
  | Byte_seq of string

type parameters = (string * item) list

type inner_list = (item * parameters) list * parameters

type list_member =
  | Item of item * parameters
  | Inner_list of inner_list

type dictionary = (string * list_member) list

(* Convenience constructors *)
let string_item s = String s
let token_item s = Token s
let integer_item i = Integer i
let byte_seq_item s = Byte_seq s
let bool_item b = Boolean b

(* Serialization *)

let item_to_string = function
  | String s ->
      (* Escape backslashes and quotes *)
      let escaped = String.to_seq s
        |> Seq.flat_map (fun c ->
            if c = '\\' || c = '"' then List.to_seq ['\\'; c]
            else Seq.return c)
        |> String.of_seq
      in
      "\"" ^ escaped ^ "\""
  | Token s -> s
  | Integer i -> Int64.to_string i
  | Decimal f -> Printf.sprintf "%g" f
  | Boolean true -> "?1"
  | Boolean false -> "?0"
  | Byte_seq s ->
      ":" ^ Base64.encode_string s ^ ":"

let parameters_to_string params =
  String.concat "" (List.map (fun (k, v) ->
    match v with
    | Boolean true -> ";" ^ k
    | _ -> ";" ^ k ^ "=" ^ item_to_string v
  ) params)

let inner_list_to_string (items, params) =
  let item_strs = List.map (fun (item, item_params) ->
    item_to_string item ^ parameters_to_string item_params
  ) items in
  "(" ^ String.concat " " item_strs ^ ")" ^ parameters_to_string params

let dictionary_to_string dict =
  String.concat ", " (List.map (fun (k, member) ->
    match member with
    | Item (Boolean true, params) ->
        k ^ parameters_to_string params
    | Item (item, params) ->
        k ^ "=" ^ item_to_string item ^ parameters_to_string params
    | Inner_list il ->
        k ^ "=" ^ inner_list_to_string il
  ) dict)

(* Parsing helpers *)

let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_digit c = c >= '0' && c <= '9'
let is_lcalpha c = c >= 'a' && c <= 'z'
let is_tchar c =
  is_alpha c || is_digit c ||
  c = '!' || c = '#' || c = '$' || c = '%' || c = '&' || c = '\'' ||
  c = '*' || c = '+' || c = '-' || c = '.' || c = '^' || c = '_' ||
  c = '`' || c = '|' || c = '~'

type parser_state = {
  input : string;
  mutable pos : int;
}

let peek s = if s.pos < String.length s.input then Some s.input.[s.pos] else None
let advance s = s.pos <- s.pos + 1
let _consume s = let c = s.input.[s.pos] in advance s; c
let skip_sp s = while peek s = Some ' ' do advance s done
let skip_ows s = while peek s = Some ' ' || peek s = Some '\t' do advance s done

let parse_token s =
  let start = s.pos in
  match peek s with
  | Some c when is_alpha c || c = '*' ->
      advance s;
      while match peek s with
        | Some c -> is_tchar c || c = ':' || c = '/'
        | None -> false
      do advance s done;
      Ok (String.sub s.input start (s.pos - start))
  | _ -> Error "Expected token"

let parse_key s =
  let start = s.pos in
  match peek s with
  | Some c when is_lcalpha c || c = '*' ->
      advance s;
      while match peek s with
        | Some c -> is_lcalpha c || is_digit c || c = '_' || c = '-' || c = '.' || c = '*'
        | None -> false
      do advance s done;
      Ok (String.sub s.input start (s.pos - start))
  | _ -> Error "Expected key"

let parse_integer s =
  let start = s.pos in
  let neg = if peek s = Some '-' then (advance s; true) else false in
  if not (match peek s with Some c -> is_digit c | None -> false) then
    Error "Expected integer"
  else begin
    while match peek s with Some c -> is_digit c | None -> false do advance s done;
    let str = String.sub s.input start (s.pos - start) in
    match Int64.of_string_opt str with
    | Some i -> Ok (if neg then Int64.neg i else i)
    | None -> Error "Integer overflow"
  end

let parse_decimal s =
  let start = s.pos in
  let _ = if peek s = Some '-' then advance s in
  while match peek s with Some c -> is_digit c | None -> false do advance s done;
  if peek s = Some '.' then begin
    advance s;
    while match peek s with Some c -> is_digit c | None -> false do advance s done
  end;
  let str = String.sub s.input start (s.pos - start) in
  match float_of_string_opt str with
  | Some f -> Ok f
  | None -> Error "Invalid decimal"

let parse_string s =
  if peek s <> Some '"' then Error "Expected string"
  else begin
    advance s;
    let buf = Buffer.create 32 in
    let rec loop () =
      match peek s with
      | None -> Error "Unterminated string"
      | Some '"' -> advance s; Ok (Buffer.contents buf)
      | Some '\\' ->
          advance s;
          (match peek s with
          | Some c when c = '\\' || c = '"' ->
              Buffer.add_char buf c; advance s; loop ()
          | _ -> Error "Invalid escape")
      | Some c ->
          if Char.code c >= 0x20 && Char.code c <= 0x7e then begin
            Buffer.add_char buf c; advance s; loop ()
          end else Error "Invalid character in string"
    in
    loop ()
  end

let parse_byte_seq s =
  if peek s <> Some ':' then Error "Expected byte sequence"
  else begin
    advance s;
    let start = s.pos in
    while match peek s with
      | Some c -> c <> ':'
      | None -> false
    do advance s done;
    if peek s <> Some ':' then Error "Unterminated byte sequence"
    else begin
      let b64 = String.sub s.input start (s.pos - start) in
      advance s;
      match Base64.decode b64 with
      | Ok decoded -> Ok decoded
      | Error (`Msg msg) -> Error msg
    end
  end

let parse_boolean s =
  if peek s <> Some '?' then Error "Expected boolean"
  else begin
    advance s;
    match peek s with
    | Some '1' -> advance s; Ok true
    | Some '0' -> advance s; Ok false
    | _ -> Error "Expected ?0 or ?1"
  end

let rec parse_bare_item s =
  match peek s with
  | Some '"' -> Result.map (fun x -> String x) (parse_string s)
  | Some ':' -> Result.map (fun x -> Byte_seq x) (parse_byte_seq s)
  | Some '?' -> Result.map (fun x -> Boolean x) (parse_boolean s)
  | Some c when c = '-' || is_digit c ->
      (* Could be integer or decimal, peek ahead *)
      let start = s.pos in
      let _ = if peek s = Some '-' then advance s in
      while match peek s with Some c -> is_digit c | None -> false do advance s done;
      if peek s = Some '.' then begin
        s.pos <- start;
        Result.map (fun x -> Decimal x) (parse_decimal s)
      end else begin
        s.pos <- start;
        Result.map (fun x -> Integer x) (parse_integer s)
      end
  | Some c when is_alpha c || c = '*' ->
      Result.map (fun x -> Token x) (parse_token s)
  | _ -> Error "Expected bare item"

and parse_parameters s =
  let rec loop acc =
    if peek s = Some ';' then begin
      advance s;
      skip_sp s;
      match parse_key s with
      | Error _ -> Ok (List.rev acc)
      | Ok key ->
          if peek s = Some '=' then begin
            advance s;
            match parse_bare_item s with
            | Ok v -> loop ((key, v) :: acc)
            | Error e -> Error e
          end else
            loop ((key, Boolean true) :: acc)
    end else Ok (List.rev acc)
  in
  loop []

let parse_item s =
  let* item = parse_bare_item s in
  let* params = parse_parameters s in
  Ok (item, params)

let parse_inner_list_items s =
  let rec loop acc =
    skip_sp s;
    match peek s with
    | Some ')' -> advance s; Ok (List.rev acc)
    | Some _ ->
        let* item = parse_item s in
        loop (item :: acc)
    | None -> Error "Unterminated inner list"
  in
  loop []

let parse_inner_list str =
  let s = { input = str; pos = 0 } in
  skip_sp s;
  if peek s <> Some '(' then Error "Expected '('"
  else begin
    advance s;
    let* items = parse_inner_list_items s in
    let* params = parse_parameters s in
    Ok (items, params)
  end

let parse_list_member s =
  if peek s = Some '(' then begin
    advance s;
    let* items = parse_inner_list_items s in
    let* params = parse_parameters s in
    Ok (Inner_list (items, params))
  end else
    let* (item, params) = parse_item s in
    Ok (Item (item, params))

let parse_dictionary str =
  let s = { input = str; pos = 0 } in
  let rec loop acc =
    skip_ows s;
    if s.pos >= String.length s.input then Ok (List.rev acc)
    else
      let* key = parse_key s in
      let* member =
        if peek s = Some '=' then (advance s; parse_list_member s)
        else
          let* params = parse_parameters s in
          Ok (Item (Boolean true, params))
      in
      skip_ows s;
      if peek s = Some ',' then (advance s; skip_ows s);
      loop ((key, member) :: acc)
  in
  loop []
