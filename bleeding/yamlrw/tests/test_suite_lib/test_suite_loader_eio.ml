(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Load yaml-test-suite test cases using Eio for file I/O *)

module Generic = Test_suite_lib.Test_suite_loader_generic

(** Eio file I/O implementation *)
module Eio_io : Generic.FILE_IO with type ctx = Eio.Fs.dir_ty Eio.Path.t =
struct
  type ctx = Eio.Fs.dir_ty Eio.Path.t

  let read_file fs path =
    try Some (Eio.Path.load Eio.Path.(fs / path)) with _ -> None

  let file_exists fs path =
    match Eio.Path.kind ~follow:true Eio.Path.(fs / path) with
    | `Regular_file -> true
    | _ -> false
    | exception _ -> false

  let is_directory fs path =
    match Eio.Path.kind ~follow:true Eio.Path.(fs / path) with
    | `Directory -> true
    | _ -> false
    | exception _ -> false

  let read_dir fs path = Eio.Path.read_dir Eio.Path.(fs / path)
end

module Loader = Generic.Make (Eio_io)
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

(** Load tests with Eio filesystem context *)
let load_directory ~fs path : test_case list = Loader.load_directory fs path

(** Parallel loading of test directories - load all test IDs concurrently *)
let load_directory_parallel ~fs test_suite_path : test_case list =
  if not (Eio_io.is_directory fs test_suite_path) then []
  else
    let entries = Eio_io.read_dir fs test_suite_path in
    let test_ids =
      entries
      |> List.filter (fun e ->
          Eio_io.is_directory fs (Filename.concat test_suite_path e)
          && String.length e >= 4
          && e.[0] >= '0'
          && e.[0] <= 'Z')
      |> List.sort String.compare
    in
    (* Load each test ID in parallel using fibers with bounded concurrency *)
    Eio.Fiber.List.map ~max_fibers:50
      (fun test_id -> Loader.load_test_id fs test_suite_path test_id)
      test_ids
    |> List.concat
