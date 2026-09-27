open Database
module M = Imap.Mirror

exception Scope_mismatch

let of_checked name f x =
  match f x with Ok v -> v | Error e -> fail (name ^ ": " ^ e)
let uid x = of_checked "UID" Imap.Uid.of_int64 x
let validity x = of_checked "UIDVALIDITY" Imap.Uidvalidity.of_int64 x
let modseq x = of_checked "MODSEQ" Imap.Modseq.of_int64 x
let enc = function Imap.Mailbox_name.Utf8 -> "utf8" | Rev1 -> "mutf7"
let dec_enc = function
  | "utf8" -> Imap.Mailbox_name.Utf8
  | "mutf7" -> Imap.Mailbox_name.Rev1
  | other -> fail (Printf.sprintf "unknown mailbox encoding %S" other)
let phase = function M.New -> 0L | M.Live -> 1L
let dec_phase = function
  | 0L -> M.New | 1L -> M.Live
  | other -> fail (Printf.sprintf "unknown phase %Ld" other)
let mode = function M.Baseline -> 0L | M.Condstore -> 1L
let dec_mode = function
  | 0L -> M.Baseline | 1L -> M.Condstore
  | other -> fail (Printf.sprintf "unknown mode %Ld" other)
let scope_key (x:M.scope) = [s x.endpoint; s x.account; s x.mailbox_key]

let mirror_error (M.Invalid why) = why

let is_sha256_hex x =
  String.length x = 64 && String.for_all (function
    | '0'..'9' | 'a'..'f' -> true | _ -> false) x

let decode_cursor (scope:M.scope) r =
  if text r.(3) <> scope.raw_name || dec_enc (text r.(4)) <> scope.encoding
     || nullable_text r.(5) <> scope.mailbox_id then None
  else
    let uidvalidity = Option.map validity (nullable_int r.(7)) in
    let anchor = Option.map modseq (nullable_int r.(10)) in
    match M.restore ~schema_version:1 ~scope ~phase:(dec_phase (int r.(6)))
      ~uidvalidity ~generation:(int r.(8)) ~revision:(int r.(9))
      ~anchor ~frontier:(int r.(11)) ~inventory_ref:(nullable_text r.(12))
      ~mode:(dec_mode (int r.(13))) with
    | Ok c -> Some c
    | Error e -> fail (mirror_error e)

let current_cursor t scope =
  match rows t "SELECT endpoint,account,mailbox_key,raw_name,encoding,\
    mailbox_id,phase,uidvalidity,generation,revision,anchor,frontier,\
    inventory_ref,mode FROM mailboxes \
    WHERE endpoint=? AND account=? AND mailbox_key=?" (scope_key scope) with
  | [] -> Some (M.initial scope)
  | r :: _ -> decode_cursor scope r

let cursor_exn t scope =
  match current_cursor t scope with
  | Some cursor -> cursor
  | None -> raise Scope_mismatch

let stale t (cursor:M.cursor) =
  match current_cursor t cursor.scope with
  | None -> true
  | Some current ->
    current.revision <> cursor.revision ||
    current.uidvalidity <> cursor.uidvalidity

let stale_revision t scope ~revision =
  match current_cursor t scope with
  | None -> true
  | Some current -> current.revision <> revision

let check_page_args who (scope:M.scope) (cursor:M.cursor) limit =
  if limit < 1 || limit > 10_000 then
    invalid_arg (who ^ ": limit must be 1..10000");
  if cursor.scope <> scope then invalid_arg (who ^ ": scope/cursor mismatch")

let group_flags what ~flag rows =
  let close acc = function
    | None -> acc
    | Some (first, flags) -> (first, List.rev flags) :: acc in
  let rec go acc current = function
    | [] -> List.rev (close acc current)
    | r :: rest ->
      let flags = match r.(flag) with
        | Sqlite3.Data.NULL -> []
        | x -> [of_checked what Mail_flag.Imap_flag.of_wire (text x)] in
      match current with
      | Some (first, seen) when first.(0) = r.(0) ->
        go acc (Some (first, flags @ seen)) rest
      | _ -> go (close acc current) (Some (r, flags)) rest in
  go [] None rows
