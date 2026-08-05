(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Temporal = Sortal_schema.Temporal
module Feed = Sortal_schema.Feed
module Contact = Sortal_schema.Contact
module Store = Sortal_store
module Git_store = Sortal_git_store
module Cmd = Sortal_cmd

type t = Store.t

let create = Store.create
let create_from_xdg = Store.create_from_xdg
let save = Store.save
let lookup = Store.lookup
let delete = Store.delete
let list = Store.list
let thumbnail_path = Store.thumbnail_path
let png_thumbnail_path = Store.png_thumbnail_path
let find_by_name = Store.find_by_name
let find_by_name_opt = Store.find_by_name_opt
let search_all = Store.search_all
let handle_of_name = Store.handle_of_name
let pp = Store.pp
