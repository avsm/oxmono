open Database
module M = Imap.Mirror
module P = Imap.Proto

let of_checked name f x =
  match f x with Ok v -> v | Error e -> fail (name ^ ": " ^ e)
let uid x = of_checked "UID" P.Uid.of_int64 x
let validity x = of_checked "UIDVALIDITY" P.Uidvalidity.of_int64 x
let modseq x = of_checked "MODSEQ" P.Modseq.of_int64 x
let enc = function Imap.Mailbox_name.Utf8 -> "utf8" | Rev1 -> "mutf7"
let dec_enc = function
  | "utf8" -> Imap.Mailbox_name.Utf8
  | "mutf7" -> Imap.Mailbox_name.Rev1
  | _ -> fail "unknown mailbox encoding"
let phase = function M.New -> 0L | M.Live -> 1L
let dec_phase = function 0L -> M.New | 1L -> M.Live | _ -> fail "unknown phase"
let mode = function M.Baseline -> 0L | M.Condstore -> 1L
let dec_mode = function 0L -> M.Baseline | 1L -> M.Condstore | _ -> fail "unknown mode"
let scope_key (x:M.scope) = [s x.endpoint; s x.account; s x.mailbox_key]

let decode_cursor scope r =
  let stored_scope = { M.endpoint = text r.(0); account = text r.(1);
    mailbox_key = text r.(2); raw_name = text r.(3);
    encoding = dec_enc (text r.(4)); mailbox_id = nullable_text r.(5) } in
  if stored_scope <> scope then fail "stored mailbox scope differs from requested scope";
  let uidvalidity = Option.map validity (nullable_int r.(7)) in
  let anchor = Option.map modseq (nullable_int r.(10)) in
  match M.restore ~schema_version:1 ~scope ~phase:(dec_phase (int r.(6)))
    ~uidvalidity ~generation:(int r.(8)) ~revision:(int r.(9))
    ~anchor ~frontier:(int r.(11)) ~inventory_ref:(nullable_text r.(12))
    ~mode:(dec_mode (int r.(13))) with
  | Ok c -> c | Error _ -> fail "invalid persisted cursor"

