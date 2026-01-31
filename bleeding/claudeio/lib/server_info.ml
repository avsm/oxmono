(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Server capabilities and metadata. *)

type t = {
  version : string;
  capabilities : string list;
  commands : string list;
  output_styles : string list;
}

let version t = t.version
let capabilities t = t.capabilities
let commands t = t.commands
let output_styles t = t.output_styles
let has_capability t cap = List.mem cap t.capabilities
let supports_hooks t = has_capability t "hooks"
let supports_structured_output t = has_capability t "structured-output"

let of_proto (proto : Proto.Control.Server_info.t) : t =
  {
    version = Proto.Control.Server_info.version proto;
    capabilities = Proto.Control.Server_info.capabilities proto;
    commands = Proto.Control.Server_info.commands proto;
    output_styles = Proto.Control.Server_info.output_styles proto;
  }

let of_sdk_control (sdk : Sdk_control.Server_info.t) : t =
  {
    version = Sdk_control.Server_info.version sdk;
    capabilities = Sdk_control.Server_info.capabilities sdk;
    commands = Sdk_control.Server_info.commands sdk;
    output_styles = Sdk_control.Server_info.output_styles sdk;
  }
