(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Immich authentication library.

    This library provides authentication support for the Immich API client,
    supporting both API keys and JWT tokens with profile-based session management. *)

module Session = Session
module Client = Client
module Cmd = Cmd
module Error = Error
