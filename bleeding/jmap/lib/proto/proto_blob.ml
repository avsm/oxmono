(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type upload_response = {
  account_id : Proto_id.t;
  blob_id : Proto_id.t;
  type_ : string;
  size : int64;
}

let upload_response_make account_id blob_id type_ size =
  { account_id; blob_id; type_; size }

let upload_response_jsont =
  let kind = "Upload response" in
  Jsont.Object.map ~kind upload_response_make
  |> Jsont.Object.mem "accountId" Proto_id.jsont ~enc:(fun r -> r.account_id)
  |> Jsont.Object.mem "blobId" Proto_id.jsont ~enc:(fun r -> r.blob_id)
  |> Jsont.Object.mem "type" Jsont.string ~enc:(fun r -> r.type_)
  |> Jsont.Object.mem "size" Proto_int53.Unsigned.jsont ~enc:(fun r -> r.size)
  |> Jsont.Object.finish

type download_vars = {
  account_id : Proto_id.t;
  blob_id : Proto_id.t;
  type_ : string;
  name : string;
}

let download_bindings vars =
  [
    ("accountId", Proto_id.to_string vars.account_id);
    ("blobId", Proto_id.to_string vars.blob_id);
    ("type", vars.type_);
    ("name", vars.name);
  ]

let expand_download_template ~template vars =
  Proto_template.expand_template ~vars:(download_bindings vars) template

let expand_download_url ~template vars =
  Proto_template.expand ~vars:(download_bindings vars) template

type copy_args = {
  from_account_id : Proto_id.t;
  account_id : Proto_id.t;
  blob_ids : Proto_id.t list;
}

let copy_args_make from_account_id account_id blob_ids =
  { from_account_id; account_id; blob_ids }

let copy_args_jsont =
  let kind = "Blob/copy args" in
  Jsont.Object.map ~kind copy_args_make
  |> Jsont.Object.mem "fromAccountId" Proto_id.jsont ~enc:(fun a ->
      a.from_account_id)
  |> Jsont.Object.mem "accountId" Proto_id.jsont ~enc:(fun a -> a.account_id)
  |> Jsont.Object.mem "blobIds" (Jsont.list Proto_id.jsont) ~enc:(fun a ->
      a.blob_ids)
  |> Jsont.Object.finish

type copy_response = {
  from_account_id : Proto_id.t;
  account_id : Proto_id.t;
  copied : (Proto_id.t * Proto_id.t) list option;
  not_copied : (Proto_id.t * Proto_error.Set_error.t) list option;
}

let copy_response_make from_account_id account_id copied not_copied =
  { from_account_id; account_id; copied; not_copied }

let copy_response_jsont =
  let kind = "Blob/copy response" in
  Jsont.Object.map ~kind copy_response_make
  |> Jsont.Object.mem "fromAccountId" Proto_id.jsont ~enc:(fun r ->
      r.from_account_id)
  |> Jsont.Object.mem "accountId" Proto_id.jsont ~enc:(fun r -> r.account_id)
  |> Proto_json_map.nullable_mem "copied" (Proto_json_map.of_id Proto_id.jsont)
       ~enc:(fun r -> r.copied)
  |> Proto_json_map.nullable_mem "notCopied"
       (Proto_json_map.of_id Proto_error.Set_error.jsont) ~enc:(fun r ->
         r.not_copied)
  |> Jsont.Object.finish
