(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let type_codec name =
  let dec s =
    if String.equal s name then ()
    else
      Jsont.Error.msgf Jsont.Meta.none "@type: expected %S but found %S" name s
  in
  Jsont.map ~kind:"@type" ~dec ~enc:(fun () -> name) Jsont.string

let type_mem name map =
  Jsont.Object.mem "@type" (type_codec name) ~dec_absent:(fun () -> ())
    ~enc:(fun _ -> ())
    ~enc_omit:(fun () -> true)
    map

let type_mem_required name map =
  Jsont.Object.mem "@type" (type_codec name) ~enc:(fun _ -> ()) map

let type_mem_partial name map =
  Jsont.Object.mem "@type" (type_codec name) ~dec_absent:(fun () -> ())
    ~enc:(fun _ -> ())
    map

(* RFC 9553 Section 1.4.2: an UnsignedInt is an integer in the range 0 to
   2^53-1, represented as a JSON Number. Jsont.int is too lenient for that on
   both sides: it also decodes a JSON string, it truncates a fractional number,
   and it encodes a value outside the safe range as a string. So decoding goes
   through a float, which sees whether the number was an integer, and Jsont.any
   restricts it to the number sort; encoding keeps Jsont.int, which writes a
   plain integer, behind a range check. *)
let max_safe_int = 9007199254740991

let unsigned ~(kind : string) =
  let dec f =
    if Float.is_integer f && f >= 0. && f <= Float.of_int max_safe_int then
      int_of_float f
    else
      Jsont.Error.msgf Jsont.Meta.none
        "%s: %.17g is not an UnsignedInt, which is an integer in the range 0 \
         to 2^53-1"
        kind f
  in
  let enc i =
    if i >= 0 && i <= max_safe_int then i
    else
      Jsont.Error.msgf Jsont.Meta.none
        "%s: %d is not an UnsignedInt, which is an integer in the range 0 to \
         2^53-1"
        kind i
  in
  let dec_number = Jsont.map ~kind ~dec ~enc:Float.of_int Jsont.number in
  let enc_number = Jsont.map ~kind ~dec:Fun.id ~enc Jsont.int in
  Jsont.any ~kind ~dec_number ~enc:(fun _ -> enc_number) ()

module Map = struct
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
                Jsont.Error.msgf Jsont.Meta.none "%s: duplicate key %S" kind
                  name
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
    map_jsont ~kind:"Id map" ~dec_key:Jscontact_id.of_string
      ~enc_key:Jscontact_id.to_string value_jsont

  let of_key ~kind ~to_string ~of_string value_jsont =
    map_jsont ~kind
      ~dec_key:(fun s -> Ok (of_string s))
      ~enc_key:to_string value_jsont

  let bool_set ~(kind : string) ~to_string ~of_string
      ?(show : ('a -> string) @ portable = to_string) () =
    (* [to_string] may reject a value it cannot write, so the decoder names a
       key with [show], which never fails. *)
    let base =
      map_jsont ~kind
        ~dec_key:(fun s -> Ok (of_string s))
        ~enc_key:to_string Jsont.bool
    in
    let dec pairs =
      let key (k, set) =
        if set then k
        else
          Jsont.Error.msgf Jsont.Meta.none "%s: key %S is mapped to false" kind
            (show k)
      in
      List.map key pairs
    in
    let enc keys = List.map (fun k -> (k, true)) keys in
    Jsont.map ~kind ~dec ~enc base

  let string_set ~kind = bool_set ~kind ~to_string:Fun.id ~of_string:Fun.id ()

  let equal ~key eq a b =
    let by_key (k, _) (k', _) = key k k' in
    List.equal
      (fun (k, v) (k', v') -> key k k' = 0 && eq v v')
      (List.sort by_key a) (List.sort by_key b)
end
