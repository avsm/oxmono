(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type 'a node = Node of 'a * 'a node list

(* Internal record; exposed as abstract [t] in the interface *)
type view = { guide : Guide.t; tree : Span.t node }
type t = view

(* The guide a render uses: an explicit [guide] wins, else the [theme]'s guide,
   else the unicode default. *)
let resolve_guide ?theme ?guide () =
  match guide with
  | Some g -> g
  | None -> (
      match theme with Some t -> Theme.guide t | None -> Guide.unicode)

let of_tree ?theme ?guide tree =
  { guide = resolve_guide ?theme ?guide (); tree }

let v ?theme ?guide f root =
  let rec render_fn node = f render_fn node in
  { guide = resolve_guide ?theme ?guide (); tree = render_fn root }

(* The guide prefix for a node, given the is-last-among-siblings flag at each
   level from a top-level node down to and including this one. Each strict
   ancestor contributes a vertical continuation ([pipe]) or blank ([space])
   depending on whether it was the last of its siblings; the node itself
   contributes the [branch] or [last] connector. The empty list is the root,
   which has no prefix. *)
let prefix ?(guide = Guide.unicode) lasts =
  match List.rev lasts with
  | [] -> ""
  | own :: rev_ancestors ->
      let buf = Buffer.create 16 in
      List.iter
        (fun ancestor_is_last ->
          Buffer.add_string buf
            (if ancestor_is_last then Guide.space guide else Guide.pipe guide))
        (List.rev rev_ancestors);
      Buffer.add_string buf
        (if own then Guide.last guide else Guide.branch guide);
      Buffer.contents buf

(* The guide beside the continuation of a node's label: each ancestor's, then
   the node's own vertical line if a sibling follows it. *)
let continuation ~guide lasts =
  String.concat ""
    (List.map
       (fun last -> if last then Guide.space guide else Guide.pipe guide)
       lasts)

let pp ppf t =
  let margin = Format.pp_get_margin ppf () in
  let p = Paint.v ppf in
  let style = Guide.style t.guide in
  (* [lasts] is the is-last flag at each level from the top down to this node;
     the root carries the empty list and prints with no prefix. A literal
     newline separates nodes: a [Fmt.cut] break hint collapses with no
     enclosing box. *)
  let rec go lasts (Node (label, children)) =
    let prefix = prefix ~guide:t.guide lasts in
    if lasts <> [] then (
      Paint.newline p;
      Paint.ink p style prefix);
    let label = Span.sanitize ~keep_newlines:false label in
    let room = max 1 (margin - Width.string_width prefix) in
    (match
       if Span.width label <= room then [ label ]
       else Span.wrap ~hang:(Span.hanging label) room label
     with
    | [] -> ()
    | first :: rest ->
        Paint.close p;
        Span.pp ppf first;
        let beside = continuation ~guide:t.guide lasts in
        List.iter
          (fun line ->
            Paint.newline p;
            Paint.ink p style beside;
            Paint.close p;
            Span.pp ppf line)
          rest);
    let n = List.length children in
    List.iteri (fun i child -> go (lasts @ [ i = n - 1 ]) child) children
  in
  go [] t.tree;
  Paint.close p

let to_string = Render.to_string pp
let to_ansi_string = Render.to_string ~style_renderer:`Ansi_tty pp
let anim t = Anim.const (to_ansi_string t)
