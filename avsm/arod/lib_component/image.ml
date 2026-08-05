(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Responsive image component using htmlit.

    Renders images with srcset for responsive loading, with support for
    various layout modes: right-captioned, centered, left-float, right-float. *)

open Htmlit

module Img = Srcsetter

let responsive ~ctx:_ ?alt ?(title="") img_ent =
  let origin_url = Printf.sprintf "/images/%s" (Img.name img_ent) in
  let srcsets = String.concat ","
    (List.map (fun (f,(w,_h)) -> Printf.sprintf "/images/%s %dw" f w)
      (Img.MS.bindings img_ent.Img.variants)) in
  let base_attrs = [
    At.v "loading" "lazy"; At.src origin_url;
    At.v "srcset" srcsets; At.v "sizes" "(max-width: 768px) 100vw, 33vw"
  ] in
  match alt with
  | Some "%r" ->
    El.figure ~at:[At.class' "my-8"] [
      El.img ~at:(At.alt title :: At.title title :: base_attrs @ [At.class' "rounded-lg"]) ();
      El.figcaption ~at:[At.class' "text-sm text-secondary mt-2 text-center"] [El.txt title]]
  | Some "%c" ->
    El.figure ~at:[At.class' "my-8"] [
      El.img ~at:(At.alt title :: At.title title :: base_attrs @ [At.class' "rounded-lg mx-auto"]) ();
      El.figcaption ~at:[At.class' "text-sm text-secondary mt-2 text-center"] [El.txt title]]
  | Some "%lc" ->
    El.figure ~at:[At.class' "float-img float-left mr-3 mb-1 mt-0.5 relative"] [
      El.img ~at:(At.alt title :: At.title title :: base_attrs @ [At.class' "rounded-full w-28 h-28 object-cover"]) ()]
  | Some "%rc" ->
    El.figure ~at:[At.class' "float-img float-right ml-3 mb-1 mt-0.5 relative"] [
      El.img ~at:(At.alt title :: At.title title :: base_attrs @ [At.class' "rounded-full w-28 h-28 object-cover"]) ()]
  | _ ->
    let alt_text = match alt with Some a -> a | None -> "" in
    El.img ~at:(At.alt alt_text :: At.title title :: base_attrs @ [At.class' "rounded-lg"]) ()
