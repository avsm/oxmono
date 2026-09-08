(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  using : string list;
  method_calls : Proto_invocation.t list;
  created_ids : (Proto_id.t * Proto_id.t) list option;
}

let create ~using ~method_calls ?created_ids () =
  { using; method_calls; created_ids }

let make using method_calls created_ids = { using; method_calls; created_ids }

let jsont =
  let kind = "Request" in
  Jsont.Object.map ~kind make
  |> Jsont.Object.mem "using" (Jsont.list Jsont.string) ~enc:(fun r -> r.using)
  |> Jsont.Object.mem "methodCalls" (Jsont.list Proto_invocation.jsont)
       ~enc:(fun r -> r.method_calls)
  |> Jsont.Object.opt_mem "createdIds" (Proto_json_map.of_id Proto_id.jsont)
       ~enc:(fun r -> r.created_ids)
  |> Jsont.Object.finish

let pp ppf r = Proto_json.pp jsont ppf r
