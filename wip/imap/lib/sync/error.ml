type t =
  | Client of Imap_eio.Error.t
  | Maildir of Maildir.error
  | Store_stale_revision
  | Mirror of Imap.Mirror.error
  | Invalid_scope of string
  | Incomplete of string
  | Limit of string
  | Uidvalidity_changed
  | Writer_busy
  | Missing_pair
  | Stale_pair
  | Missing_occurrence
  | Stale_inventory
  | Identity_changed
  | Modified
  | Conditional_store_unavailable
  | Permanent_flag_unavailable of Mail_flag.Imap_flag.t
  | Unsupported of string
  | Pending_operations of string list
  | No_pending_operation
  | Bootstrap_requires_pairing
  | Source_vanished of Imap.Uid.t
  | Local_source_changed of string
  | Content_mismatch of string
  | Content_diverged of string
  | Flags_diverged of string
  | Date_diverged of string
  | Diverged of string
  | Invalid_operation of string
  | Invalid_configuration of string

let pp ppf = function
  | Client e -> Imap_eio.Client.pp_error ppf e
  | Maildir e -> Maildir.pp_error ppf e
  | Store_stale_revision ->
      Format.pp_print_string ppf "sync state changed concurrently"
  | Mirror (Imap.Mirror.Invalid why) ->
      Format.fprintf ppf "IMAP scan cannot be planned: %s" why
  | Invalid_scope why -> Format.fprintf ppf "invalid IMAP scope: %s" why
  | Incomplete why -> Format.fprintf ppf "incomplete IMAP response: %s" why
  | Limit why -> Format.fprintf ppf "IMAP sync limit: %s" why
  | Uidvalidity_changed ->
      Format.pp_print_string ppf "mailbox UIDVALIDITY changed"
  | Writer_busy -> Format.pp_print_string ppf "Maildir writer lease is busy"
  | Missing_pair -> Format.pp_print_string ppf "sync pair is missing"
  | Stale_pair -> Format.pp_print_string ppf "sync pair revision changed"
  | Missing_occurrence ->
      Format.pp_print_string ppf "paired occurrence is absent"
  | Stale_inventory -> Format.pp_print_string ppf "complete inventory changed"
  | Identity_changed ->
      Format.pp_print_string ppf "paired occurrence identity changed"
  | Modified -> Format.pp_print_string ppf "endpoint changed concurrently"
  | Conditional_store_unavailable ->
      Format.pp_print_string ppf "conditional UID STORE is unavailable"
  | Permanent_flag_unavailable flag ->
      Format.fprintf ppf "remote flag %a is not permanently writable"
        Mail_flag.Imap_flag.pp flag
  | Unsupported why -> Format.fprintf ppf "deletion unavailable: %s" why
  | Pending_operations ids ->
      Format.fprintf ppf "pending sync operations: %s"
        (String.concat ", " ids)
  | No_pending_operation ->
      Format.pp_print_string ppf "no matching pending operation in this scope"
  | Bootstrap_requires_pairing ->
      Format.pp_print_string ppf "both endpoints contain unpaired messages"
  | Source_vanished uid ->
      Format.fprintf ppf "remote UID %Ld vanished before archival"
        (Imap.Uid.to_int64 uid)
  | Local_source_changed id ->
      Format.fprintf ppf "local occurrence %s changed before archival" id
  | Content_mismatch id ->
      Format.fprintf ppf "paired local content differs for %s" id
  | Content_diverged id ->
      Format.fprintf ppf "copied content diverged for %s" id
  | Flags_diverged id -> Format.fprintf ppf "copied flags diverged for %s" id
  | Date_diverged id ->
      Format.fprintf ppf "copied INTERNALDATE diverged for %s" id
  | Diverged why | Invalid_operation why | Invalid_configuration why ->
      Format.pp_print_string ppf why

let to_string e = Format.asprintf "%a" pp e
