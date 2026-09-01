(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  name : string;
  config : Jsont.json option;
  must_understand : bool;
}

let v ?config ?(must_understand = true) name = { name; config; must_understand }

let config_mems t =
  match t.config with Some (Jsont.Object (mems, _)) -> mems | _ -> []

let config_mem t n =
  match Jsont.Json.find_mem n (config_mems t) with
  | Some (_, j) -> Some j
  | None -> None

let equal a b =
  String.equal a.name b.name
  && a.must_understand = b.must_understand
  &&
  match (a.config, b.config) with
  | None, None -> true
  | Some x, Some y -> Jsont.Json.equal x y
  | Some _, None | None, Some _ -> false

let pp ppf t =
  match t.config with
  | None -> Format.pp_print_string ppf t.name
  | Some c -> Format.fprintf ppf "%s %a" t.name Jsont.Json.pp c

(* The name must conform to the extension name rules, which admit a
   registered name or, for backwards compatibility, a URI. Neither can
   be empty, and that is all the two forms have in common. *)
let check_name name =
  if String.equal name "" then
    Jsont.Error.msg Jsont.Meta.none "extension name is empty"
  else name

(* The three encoded shapes. [name_jsont] doubles as the decoder for the
   bare string form. *)

let name_jsont =
  Jsont.map ~kind:"Ext"
    ~dec:(fun name ->
      { name = check_name name; config = None; must_understand = true })
    ~enc:(fun t -> t.name) Jsont.string

let name_only_jsont =
  Jsont.Object.enc_only ~kind:"Ext" ()
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun t -> t.name)
  |> Jsont.Object.finish

let object_jsont =
  Jsont.Object.map ~kind:"Ext" (fun name config must_understand ->
      { name = check_name name; config; must_understand })
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun t -> t.name)
  |> Jsont.Object.mem "configuration"
       (Jsont.option Jsont.json_object)
       ~dec_absent:(fun () -> None)
       ~enc:(fun t -> t.config)
       ~enc_omit:Option.is_none
  |> Jsont.Object.mem "must_understand" Jsont.bool ~dec_absent:(fun () -> true)
       ~enc:(fun t -> t.must_understand)
       ~enc_omit:Fun.id
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

(* The bare string and the name only object both mean must_understand
   true, so an extension that carries [false] takes the full object form
   whatever its configuration is. Writing it as a short form would
   promote it to true. *)
let jsont =
  Jsont.any ~kind:"Ext" ~dec_string:name_jsont ~dec_object:object_jsont
    ~enc:(fun t ->
      if not t.must_understand then object_jsont
      else
        match t.config with
        | None -> name_jsont
        | Some (Jsont.Object ([], _)) -> name_only_jsont
        | Some _ -> object_jsont)
    ()
