(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  raw : string;
  authority : string;
  collection : string option;
  rkey : string option;
}

type error = [ `Invalid_at_uri of string ]

let pp_error ppf = function
  | `Invalid_at_uri s -> Fmt.pf ppf "invalid AT-URI: %S" s

(* Check if s contains substring sub *)
let contains_substring ~sub s =
  let len_sub = String.length sub in
  let len_s = String.length s in
  len_sub <= len_s
  &&
  let rec check i =
    i <= len_s - len_sub
    && (String.sub s i len_sub = sub || check (i + 1))
  in
  check 0

(* Check if authority is a valid DID or handle *)
let is_valid_authority s = Did.is_valid s || Handle.is_valid s

(* Parse and validate an AT-URI *)
let parse s =
  let len = String.length s in
  (* Must start with "at://" *)
  if len < 5 || String.sub s 0 5 <> "at://" then None
  else
    let rest = String.sub s 5 (len - 5) in
    let rest_len = String.length rest in
    (* Cannot be empty after at:// *)
    if rest_len = 0 then None (* Cannot have trailing slash *)
    else if rest.[rest_len - 1] = '/' then None (* Cannot have double slashes *)
    else if
      String.starts_with ~prefix:"//" rest || contains_substring ~sub:"//" rest
    then None (* Cannot have leading space *)
    else if rest.[0] = ' ' then None (* Cannot have trailing space *)
    else if rest.[rest_len - 1] = ' ' then None (* Cannot have fragments *)
    else if String.contains rest '#' then None
    else
      let parts = String.split_on_char '/' rest in
      match parts with
      | [] -> None
      | [ "" ] -> None
      | authority :: rest_parts -> (
          if not (is_valid_authority authority) then None
          else
            match rest_parts with
            | [] -> Some { raw = s; authority; collection = None; rkey = None }
            | [ "" ] -> None (* trailing slash *)
            | [ collection ] ->
                if Nsid.is_valid collection then
                  Some
                    {
                      raw = s;
                      authority;
                      collection = Some collection;
                      rkey = None;
                    }
                else None
            | [ collection; rkey ] ->
                if Nsid.is_valid collection && Record_key.is_valid rkey then
                  Some
                    {
                      raw = s;
                      authority;
                      collection = Some collection;
                      rkey = Some rkey;
                    }
                else None
            | _ -> None (* Too many path segments *))

let is_valid s = Option.is_some (parse s)

let of_string s =
  match parse s with Some t -> Ok t | None -> Error (`Invalid_at_uri s)

let of_string_exn s =
  match of_string s with
  | Ok t -> t
  | Error e -> invalid_arg (Format.asprintf "%a" pp_error e)

let to_string t = t.raw

let make ~authority ?collection ?rkey () =
  (* Validate authority *)
  if not (is_valid_authority authority) then
    Error (`Invalid_at_uri ("invalid authority: " ^ authority))
  (* Validate collection if present *)
    else
    match collection with
    | Some c when not (Nsid.is_valid c) ->
        Error (`Invalid_at_uri ("invalid collection: " ^ c))
    | _ -> (
        (* Validate rkey if present *)
        match rkey with
        | Some _ when collection = None ->
            Error (`Invalid_at_uri "rkey requires collection")
        | Some r when not (Record_key.is_valid r) ->
            Error (`Invalid_at_uri ("invalid rkey: " ^ r))
        | _ ->
            let raw =
              match (collection, rkey) with
              | None, _ -> "at://" ^ authority
              | Some c, None -> "at://" ^ authority ^ "/" ^ c
              | Some c, Some r -> "at://" ^ authority ^ "/" ^ c ^ "/" ^ r
            in
            Ok { raw; authority; collection; rkey })

let authority t = t.authority
let collection t = t.collection
let rkey t = t.rkey
let equal a b = String.equal a.raw b.raw
let compare a b = String.compare a.raw b.raw
let pp ppf t = Fmt.string ppf t.raw
