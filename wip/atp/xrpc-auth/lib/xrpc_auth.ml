(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** XRPC Authentication library for AT Protocol CLIs.

    This library provides common authentication functionality that can be shared
    across multiple AT Protocol CLI applications. *)

module Session = Xrpc_auth_session
module Client = Xrpc_auth_client
module Cmd = Xrpc_auth_cmd
