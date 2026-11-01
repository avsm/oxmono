(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open! Core
open Bonsai_term
open Bonsai.Let_syntax

let rows ~fields ~width =
  List.concat_map fields ~f:(fun (label, value) ->
      let label = Text_layout.wrap ~width:(max 1 (width - 2)) label in
      let value = Text_layout.wrap ~width:(max 1 (width - 4)) value in
      List.map label ~f:(fun s ->
          Text_layout.text ~attrs:[ Attr.bold ] (" " ^ s ^ ":"))
      @ List.map value ~f:(fun s -> Text_layout.text ("   " ^ s)))

let content_height ~fields ~width = List.length (rows ~fields ~width)

let render ~fields ~width ~height ~scroll =
  let rows = rows ~fields ~width in
  let offset = max 0 (min scroll (List.length rows - height)) in
  List.take (List.drop rows offset) (max 0 height)
  |> View.vcat
  |> Text_layout.fit ~width ~height

let component ~fields ~width ~height ~scroll (local_ _graph) =
  let%arr fields and width and height and scroll in
  render ~fields ~width ~height ~scroll
