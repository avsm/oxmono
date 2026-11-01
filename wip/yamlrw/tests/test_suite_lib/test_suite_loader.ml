(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Load yaml-test-suite test cases using standard OCaml I/O *)

(** Synchronous file I/O implementation *)
module Sync_io : Test_suite_loader_generic.FILE_IO with type ctx = unit = struct
  type ctx = unit

  let read_file () path =
    try
      let ic = open_in path in
      let n = in_channel_length ic in
      let s = really_input_string ic n in
      close_in ic;
      Some s
    with _ -> None

  let file_exists () path = Sys.file_exists path
  let is_directory () path = Sys.file_exists path && Sys.is_directory path
  let read_dir () path = Array.to_list (Sys.readdir path)
end

module Loader = Test_suite_loader_generic.Make (Sync_io)
(** Internal loader module *)

type test_case = Loader.test_case = {
  id : string;
  name : string;
  yaml : string;
  tree : string option;
  json : string option;
  fail : bool;
}
(** Re-export test_case type from loader *)

(** Load tests without needing to pass a context *)
let load_directory path : test_case list = Loader.load_directory () path
