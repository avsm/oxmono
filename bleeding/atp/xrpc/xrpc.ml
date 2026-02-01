(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Error = Xrpc_error
module Jwt = Xrpc_jwt
module Types = Xrpc_types
module Client = Xrpc_client
module Credential = Xrpc_cred

type client = Xrpc_client.t
type session = Xrpc_types.session
type error = Xrpc_error.error
