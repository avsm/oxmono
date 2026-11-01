(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
val filter : mailbox:Jmap.Proto.Id.t option -> string -> Jmap.Proto.Email.filter
(** [filter ~mailbox query] combines the mailbox, free text and supported field
    filters with AND. Double quotes retain spaces in a field value. Invalid
    dates and reserved filters fail before any network request. *)
