(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module String_set = Set.MakePortable (String)

type entry = Remove | Set of Jsont.json
type t = (string * entry) list

let empty = []
let to_list p = p
let is_empty p = p = []
let find p key = List.assoc_opt key p

let equal_entry a b =
  match (a, b) with
  | Remove, Remove -> true
  | Set a, Set b -> Jsont.Json.equal a b
  | _ -> false

let equal a b =
  List.equal
    (fun (ka, va) (kb, vb) -> String.equal ka kb && equal_entry va vb)
    a b

let pp ppf p =
  Format.fprintf ppf "@[<1>{%a}@]"
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
       (fun ppf (k, v) ->
         Format.fprintf ppf "%S: %s" k
           (match v with Remove -> "null" | Set _ -> "...")))
    p

(* A key is a JSON Pointer with the leading "/" left out. Section 1.4.3 makes
   the pointer syntax the RFC 6901 one, where "~" escapes as "~0" and "/" as
   "~1". *)
let check_key key =
  if key = "" then Error "a patch key names no property"
  else
    let len = String.length key in
    let rec check i =
      if i >= len then Ok key
      else if key.[i] <> '~' then check (i + 1)
      else if i + 1 >= len || (key.[i + 1] <> '0' && key.[i + 1] <> '1') then
        Error
          (Printf.sprintf
             "patch key %S is not JSON Pointer syntax: a \"~\" must be \
              followed by \"0\" or \"1\""
             key)
      else check (i + 2)
    in
    match check 0 with
    | Error _ as e -> e
    | Ok key -> (
        (* Json_pointer rejects a reference token that is not valid UTF-8,
           which the escape check above does not rule out. Doing it here keeps
           [validate] a complete answer for a caller that screens a patch
           without applying it. *)
        match Json_pointer.of_string ("/" ^ key) with
        | (_ : Json_pointer.t) -> Ok key
        | exception Jsont.Error e ->
            Error
              (Printf.sprintf "patch key %S is not JSON Pointer syntax: %s" key
                 (Jsont.Error.to_string e)))

let sort p = List.sort (fun (a, _) (b, _) -> String.compare a b) p

let of_list entries =
  let rec go seen = function
    | [] -> Ok (sort entries)
    | (key, _) :: rest -> (
        match check_key key with
        | Error _ as e -> e
        | Ok _ ->
            if String_set.mem key seen then
              Error (Printf.sprintf "duplicate patch key %S" key)
            else go (String_set.add key seen) rest)
  in
  go (String_set.of_list []) entries

let v entries =
  match of_list entries with
  | Ok p -> p
  | Error msg -> invalid_arg ("Jscontact_patch.v: " ^ msg)

(* Section 1.4.3 condition 3: no key may be a prefix of another, at a token
   boundary. Sorting by the token lists rather than by the key text puts a key
   immediately before every key it prefixes, so only neighbours need
   comparing. Sorting the text would not, since every character below "/"
   sorts between a key and its extensions: "name", "name-vendor" and
   "name/full" would then pass. *)
let check_prefixes p =
  let keyed = List.map (fun (k, _) -> (String.split_on_char '/' k, k)) p in
  let sorted =
    List.sort (fun (a, _) (b, _) -> List.compare String.compare a b) keyed
  in
  let rec prefix a b =
    match (a, b) with
    | [], _ :: _ -> true
    | x :: a, y :: b when String.equal x y -> prefix a b
    | _ -> false
  in
  let rec go = function
    | (ta, ka) :: ((tb, kb) :: _ as rest) ->
        if prefix ta tb then
          Error
            (Printf.sprintf "patch key %S is a prefix of patch key %S" ka kb)
        else go rest
    | _ -> Ok p
  in
  go sorted

let validate p =
  Jscontact_valid.(
    let rec keys = function
      | [] -> ok p
      | (key, _) :: rest -> (
          match check_key key with Ok _ -> keys rest | Error msg -> Error msg)
    in
    let* _ = keys p in
    let* _ = check_prefixes p in
    ok p)

let last_token key =
  match String.rindex_opt key '/' with
  | None -> key
  | Some i -> String.sub key (i + 1) (String.length key - i - 1)

(* [apply] validates [p] first, so the key is known well formed here. *)
let apply_entry json (key, value) =
  let pointer = Json_pointer.of_string ("/" ^ key) in
  let parent =
    Option.value (Json_pointer.parent pointer) ~default:Json_pointer.root
  in
  let token = last_token key in
  let parent_json = Json_pointer.find parent json in
  match parent_json with
  | None ->
      Error
        (Printf.sprintf
           "patch key %S: the value it patches within does not exist" key)
  | Some (Jsont.Array _) -> (
      (* Section 1.4.3 condition 1: an array member may be replaced but not
             removed, and "-" is never a valid index. *)
      match value with
      | Remove ->
          Error
            (Printf.sprintf
               "patch key %S: an array member cannot be removed; replace the \
                whole array instead"
               key)
      | Set _ when String.equal token "-" ->
          Error
            (Printf.sprintf
               "patch key %S: \"-\" is not a valid array index in a PatchObject"
               key)
      | Set v -> (
          try Ok (Json_pointer.replace pointer json ~value:v)
          with Jsont.Error e ->
            Error
              (Printf.sprintf "patch key %S: %s" key (Jsont.Error.to_string e)))
      )
  | Some (Jsont.Object _) -> (
      match value with
      | Remove -> (
          if Json_pointer.find pointer json = None then Ok json
          else
            try Ok (Json_pointer.remove pointer json)
            with Jsont.Error e ->
              Error
                (Printf.sprintf "patch key %S: %s" key (Jsont.Error.to_string e))
          )
      | Set v -> (
          try Ok (Json_pointer.add pointer json ~value:v)
          with Jsont.Error e ->
            Error
              (Printf.sprintf "patch key %S: %s" key (Jsont.Error.to_string e)))
      )
  | Some _ ->
      Error
        (Printf.sprintf
           "patch key %S: the value it patches within is neither an object nor \
            an array"
           key)

let apply p json =
  match validate p with
  | Error msg -> Error msg
  | Ok _ ->
      let rec go json = function
        | [] -> Ok json
        | ((key, _) as entry) :: rest -> (
            (* Json_pointer raises on a reference token that is not valid
               UTF-8, which check_key does not rule out. This function returns
               a result, so the raise is caught here rather than escaping into
               a caller such as Jscontact_card.localize. *)
            match apply_entry json entry with
            | Ok json -> go json rest
            | Error _ as e -> e
            | exception Jsont.Error e ->
                Error
                  (Printf.sprintf "patch key %S: %s" key
                     (Jsont.Error.to_string e)))
      in
      go json p

let entry_jsont =
  Jsont.map ~kind:"patch value"
    ~dec:(function Jsont.Null _ -> Remove | j -> Set j)
    ~enc:(function Remove -> Jsont.Json.null () | Set j -> j)
    Jsont.json

let jsont = Jscontact_json.Map.of_string entry_jsont
