(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Unified Mail Flags for IMAP and JMAP

    This library provides a unified representation of message flags and mailbox
    attributes that works across both IMAP (RFC 9051) and JMAP (RFC 8621)
    protocols.

    The core types use polymorphic variants for type safety and extensibility.
*)

module Keyword = Keyword
module Mailbox_attr = Mailbox_attr
module Flag_color = Flag_color
