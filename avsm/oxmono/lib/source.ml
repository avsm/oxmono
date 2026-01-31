(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Package source types for oxmono *)

type git_source = {
  url : string;
  commit : string;
}

type archive_source = {
  url : string;
  checksum : string;
}

type source =
  | Git of git_source
  | Archive of archive_source

type t = {
  packages : (string * source) list;
}

let empty = { packages = [] }

let add_package name source t =
  { packages = (name, source) :: List.filter (fun (n, _) -> n <> name) t.packages }

let find_package name t =
  List.assoc_opt name t.packages

let packages t = t.packages

let git ~url ~commit = Git { url; commit }
let archive ~url ~checksum = Archive { url; checksum }

let source_url = function
  | Git { url; _ } -> url
  | Archive { url; _ } -> url

let is_git_url url =
  String.length url > 4 && String.sub url (String.length url - 4) 4 = ".git"
  || String.starts_with ~prefix:"git+" url
  || String.starts_with ~prefix:"git://" url
