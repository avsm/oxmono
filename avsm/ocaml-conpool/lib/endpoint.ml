(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Network endpoint representation *)

let src =
  Logs.Src.create "conpool.endpoint" ~doc:"Connection pool endpoint operations"

module Log = (val Logs.src_log src : Logs.LOG)

type t = { host : string; port : int }

let make ~host ~port =
  (* Validate port range *)
  if port < 1 || port > 65535 then
    invalid_arg
      (Printf.sprintf "Invalid port number: %d (must be 1-65535)" port);

  (* Validate hostname is not empty *)
  if String.trim host = "" then invalid_arg "Hostname cannot be empty";

  { host; port }

let host t = t.host
let port t = t.port
let equal t1 t2 = String.equal t1.host t2.host && t1.port = t2.port
let hash t = Hashtbl.hash (t.host, t.port)
let pp = Fmt.of_to_string (fun t -> Printf.sprintf "%s:%d" t.host t.port)
