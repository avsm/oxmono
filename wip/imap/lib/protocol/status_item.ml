type t =
  | Messages | Unseen | Uidnext | Uidvalidity | Highestmodseq | Mailboxid
  | Objectid | Size | Deleted | Deleted_storage

let to_wire = function
  | Messages -> "MESSAGES" | Unseen -> "UNSEEN" | Uidnext -> "UIDNEXT"
  | Uidvalidity -> "UIDVALIDITY" | Highestmodseq -> "HIGHESTMODSEQ"
  | Mailboxid -> "MAILBOXID" | Objectid -> "OBJECTID" | Size -> "SIZE"
  | Deleted -> "DELETED" | Deleted_storage -> "DELETED-STORAGE"

let equal (a : t) b = a = b
let pp ppf i = Format.pp_print_string ppf (to_wire i)
