(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = {
  id : Id.Message.t;
  automatic_new_visibility_policy : Topic_visibility.t option;
  raw : Jsont.json;
}

let id t = t.id
let automatic_new_visibility_policy t = t.automatic_new_visibility_policy
let raw t = t.raw
let pp fmt t = Format.fprintf fmt "MessageResponse{id=%a}" Id.Message.pp t.id

let jsont =
  let make id automatic_new_visibility_policy unknown =
    let mem name value = ((name, Jsont.Meta.none), value) in
    let known =
      [ mem "id" (Jsont.Json.int (Id.Message.to_int id)) ]
      @ Option.fold ~none:[]
          ~some:(fun policy ->
            [
              mem "automatic_new_visibility_policy"
                (Jsont.Json.int (Topic_visibility.to_int policy));
            ])
          automatic_new_visibility_policy
    in
    let unknown =
      match unknown with Jsont.Object (members, _) -> members | _ -> []
    in
    {
      id;
      automatic_new_visibility_policy;
      raw = Jsont.Object (known @ unknown, Jsont.Meta.none);
    }
  in
  Jsont.Object.map ~kind:"Zulip send-message response" make
  |> Jsont.Object.mem "id" Id.Message.jsont ~enc:id
  |> Jsont.Object.opt_mem "automatic_new_visibility_policy"
       Topic_visibility.jsont ~enc:automatic_new_visibility_policy
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun t ->
      let known = [ "id"; "automatic_new_visibility_policy" ] in
      match t.raw with
      | Jsont.Object (members, meta) ->
          Jsont.Object
            ( List.filter
                (fun ((name, _), _) -> not (List.mem name known))
                members,
              meta )
      | _ -> Jsont.Json.object' [])
  |> Jsont.Object.finish
