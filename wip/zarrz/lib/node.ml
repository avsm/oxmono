(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = [ `Array of Arr.t | `Group of Group.t ]

let node_type ~key j =
  let err fmt =
    Printf.ksprintf
      (fun m -> Error.raise_ (Error.Metadata (key ^ ": " ^ m)))
      fmt
  in
  match j with
  | Jsont.Object (mems, _) -> (
      match Jsont.Json.find_mem "node_type" mems with
      | Some (_, Jsont.String (s, _)) -> s
      | Some _ -> err "node_type is not a string"
      | None -> err "no node_type member")
  | _ -> err "not a JSON object"

let open_ ?codecs store ~path =
  let key = Chunk_key.meta_key ~path in
  let j = Store.get_json store ~key in
  match node_type ~key j with
  | "array" -> `Array (Arr.of_json ?codecs store ~path j)
  | "group" -> `Group (Group.of_json store ~path j)
  | s ->
      Error.raise_
        (Error.Metadata (Printf.sprintf "%s: unknown node_type %S" key s))
