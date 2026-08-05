(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "requests.timeout" ~doc:"HTTP Request Timeouts"
module Log = (val Logs.src_log src : Logs.LOG)

type t = {
  connect : float option;
  read : float option;
  total : float option;
  expect_100_continue : float option;  (** Timeout for 100-continue response *)
}

let none = {
  connect = None;
  read = None;
  total = None;
  expect_100_continue = None;
}

let create ?connect ?read ?total ?expect_100_continue () = {
  connect;
  read;
  total;
  expect_100_continue;
}

let default = {
  connect = Some 10.0;
  read = Some 30.0;
  total = None;
  expect_100_continue = Some 1.0;  (* 1 second default for 100-continue *)
}

let connect t = t.connect
let read t = t.read
let total t = t.total
let expect_100_continue t = t.expect_100_continue

let pp ppf t =
  let fmt_opt name = Option.map (Printf.sprintf "%s:%.1fs" name) in
  let items = List.filter_map Fun.id [
    fmt_opt "connect" t.connect;
    fmt_opt "read" t.read;
    fmt_opt "total" t.total;
    fmt_opt "expect" t.expect_100_continue;
  ] in
  match items with
  | [] -> Format.fprintf ppf "no timeouts"
  | _ -> Format.fprintf ppf "%s" (String.concat ", " items)