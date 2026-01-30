(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** YAML document with directives and content *)

type t = {
  version : (int * int) option;
  tags : (string * string) list;
  root : Yaml.t option;
  implicit_start : bool;
  implicit_end : bool;
}

let make ?(version : (int * int) option) ?(tags : (string * string) list = [])
    ?(implicit_start = true) ?(implicit_end = true) root =
  { version; tags; root; implicit_start; implicit_end }

let version t = t.version
let tags t = t.tags
let root t = t.root
let implicit_start t = t.implicit_start
let implicit_end t = t.implicit_end
let with_version version t = { t with version = Some version }
let with_tags tags t = { t with tags }
let with_root root t = { t with root = Some root }

let pp fmt t =
  Format.fprintf fmt "@[<v 2>document(@,";
  (match t.version with
  | Some (maj, min) -> Format.fprintf fmt "version=%d.%d,@ " maj min
  | None -> ());
  if t.tags <> [] then begin
    Format.fprintf fmt "tags=[";
    List.iteri
      (fun i (h, p) ->
        if i > 0 then Format.fprintf fmt ", ";
        Format.fprintf fmt "%s -> %s" h p)
      t.tags;
    Format.fprintf fmt "],@ "
  end;
  Format.fprintf fmt "implicit_start=%b,@ " t.implicit_start;
  Format.fprintf fmt "implicit_end=%b,@ " t.implicit_end;
  (match t.root with
  | Some root -> Format.fprintf fmt "root=%a" Yaml.pp root
  | None -> Format.fprintf fmt "root=<empty>");
  Format.fprintf fmt "@]@,)"

let equal a b =
  Option.equal ( = ) a.version b.version
  && List.equal ( = ) a.tags b.tags
  && Option.equal Yaml.equal a.root b.root
  && a.implicit_start = b.implicit_start
  && a.implicit_end = b.implicit_end
