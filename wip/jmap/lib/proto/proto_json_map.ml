(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module String_map = Map.MakePortable (String)
module String_set = Set.MakePortable (String)

let map_jsont ~(kind : string) ~dec_key ~enc_key value_jsont =
  let dec_add meta name value m =
    if String_map.mem name m then
      Jsont.Error.msgf meta "%s: duplicate key %S" kind name
    else
      match dec_key name with
      | Ok key -> String_map.add name (key, value) m
      | Error msg ->
          Jsont.Error.msgf meta "%s: invalid key %S: %s" kind name msg
  in
  let dec_finish _meta m = List.map snd (List.of_seq (String_map.to_seq m)) in
  let enc =
    {
      Jsont.Object.Mems.enc =
        (fun add pairs acc ->
          let mem (seen, acc) (key, value) =
            let name = enc_key key in
            if String_set.mem name seen then
              Jsont.Error.msgf Jsont.Meta.none "%s: duplicate key %S" kind name
            else (String_set.add name seen, add Jsont.Meta.none name value acc)
          in
          snd (List.fold_left mem ((String_set.of_list []), acc) pairs));
    }
  in
  let mems =
    Jsont.Object.Mems.map ~kind
      ~dec_empty:(fun () -> (String_map.of_list []))
      ~dec_add ~dec_finish ~enc value_jsont
  in
  Jsont.Object.map ~kind Fun.id
  |> Jsont.Object.keep_unknown mems ~enc:Fun.id
  |> Jsont.Object.finish

let of_string value_jsont =
  map_jsont ~kind:"String map" ~dec_key:Result.ok ~enc_key:Fun.id value_jsont

let of_id value_jsont =
  map_jsont ~kind:"Id map" ~dec_key:Proto_id.of_string_received
    ~enc_key:Proto_id.to_string value_jsont

let of_creation value_jsont =
  let dec_key s =
    Result.map
      (fun id -> Proto_id.creation (Proto_id.to_string id))
      (Proto_id.of_string s)
  in
  map_jsont ~kind:"Creation id map" ~dec_key
    ~enc_key:(fun c -> Proto_id.to_string (Proto_id.creation_id c))
    value_jsont

let of_id_or_creation value_jsont =
  map_jsont ~kind:"Id or creation reference map"
    ~dec_key:Proto_id.of_string_or_creation_received ~enc_key:Proto_id.to_string
    value_jsont

let id_to_bool = of_id_or_creation Jsont.bool
let string_to_bool = of_string Jsont.bool

let nullable_mem name t ~enc map =
  Jsont.Object.mem name (Jsont.option t) ~dec_absent:(fun () -> None)
    ~enc_omit:Option.is_none ~enc map

let nullable_mem_null name t ~enc map =
  Jsont.Object.mem name (Jsont.option t) ~dec_absent:(fun () -> None) ~enc map
