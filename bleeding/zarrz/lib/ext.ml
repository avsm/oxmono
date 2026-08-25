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

(* The three encoded shapes. [name_jsont] doubles as the decoder for the
   bare string form. *)

let name_jsont =
  Jsont.map ~kind:"Ext"
    ~dec:(fun name -> { name; config = None; must_understand = true })
    ~enc:(fun t -> t.name) Jsont.string

let name_only_jsont =
  Jsont.Object.enc_only ~kind:"Ext" ()
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun t -> t.name)
  |> Jsont.Object.finish

let object_jsont =
  Jsont.Object.map ~kind:"Ext" (fun name config must_understand ->
      { name; config; must_understand })
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun t -> t.name)
  |> Jsont.Object.mem "configuration"
       (Jsont.option Jsont.json_object)
       ~dec_absent:None
       ~enc:(fun t -> t.config)
       ~enc_omit:Option.is_none
  |> Jsont.Object.mem "must_understand" Jsont.bool ~dec_absent:true
       ~enc:(fun t -> t.must_understand)
       ~enc_omit:Fun.id
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let jsont =
  Jsont.any ~kind:"Ext" ~dec_string:name_jsont ~dec_object:object_jsont
    ~enc:(fun t ->
      match t.config with
      | None -> name_jsont
      | Some (Jsont.Object ([], _)) -> name_only_jsont
      | Some _ -> object_jsont)
    ()
