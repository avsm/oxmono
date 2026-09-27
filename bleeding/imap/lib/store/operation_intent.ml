open Database
open Record_codec
module M = Imap.Mirror
module P = Imap.Proto
module S = Sqlite3

type t = Database.t
type intent_kind =
  | Append of {
      message_id : string; content_digest : string; spool_ref : string;
      pre_send_uid_frontier : int64 option;
      expected_length : int64 option;
      expected_flags : Mail_flag.Imap_flag.t list option;
      expected_internal_date : string option
    }
  | Other of string
type intent_state = Prepared | Sent | Ambiguous | Confirmed | Rejected
type intent = {
  id : string; scope : M.scope; kind : intent_kind; state : intent_state;
  uidvalidity : P.Uidvalidity.t option; uid : P.Uid.t option
}

let state = function
  | Prepared -> "prepared" | Sent -> "sent" | Ambiguous -> "ambiguous"
  | Confirmed -> "confirmed" | Rejected -> "rejected"
let dec_state = function
  | "prepared" -> Prepared | "sent" -> Sent | "ambiguous" -> Ambiguous
  | "confirmed" -> Confirmed | "rejected" -> Rejected
  | _ -> fail "unknown intent state"
let kind = function Append _ -> "append" | Other _ -> "other"

let prepare_intent t x =
  if x.id = "" then invalid_arg "Imap_store.prepare_intent: empty ID";
  if x.state <> Prepared then invalid_arg "Imap_store.prepare_intent: state must be Prepared";
  transaction t (fun () ->
    let message_id,digest,spool_ref,frontier,length,flags,date = match x.kind with
      | Append {message_id;content_digest;spool_ref;
                pre_send_uid_frontier;expected_length;expected_flags;
                expected_internal_date} ->
        if message_id="" || content_digest="" || spool_ref="" then
          invalid_arg "Imap_store.prepare_intent: incomplete APPEND recovery data";
        if not (is_sha256_hex content_digest) then
          invalid_arg "Imap_store.prepare_intent: expected lowercase SHA-256 hex";
        Option.iter (fun n ->
          if n < 0L || n > 4_294_967_295L then
            invalid_arg "Imap_store.prepare_intent: invalid UID frontier")
          pre_send_uid_frontier;
        Option.iter (fun n ->
          if n < 0L then
            invalid_arg "Imap_store.prepare_intent: negative expected length")
          expected_length;
        Option.iter (fun date ->
          match Imap.Internal_date.of_string date with
          | Ok _ -> ()
          | Error _ -> invalid_arg "Imap_store.prepare_intent: invalid date metadata")
          expected_internal_date;
        Some message_id,Some content_digest,Some spool_ref,
        pre_send_uid_frontier,expected_length,expected_flags,
        expected_internal_date
      | Other _ -> None,None,None,None,None,None,None in
    run t "INSERT INTO intents VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
      ([s x.id] @ scope_key x.scope @
       [s x.scope.raw_name; s (enc x.scope.encoding); ns x.scope.mailbox_id;
        s (kind x.kind); ns message_id; ns digest;
        ns (match x.kind with Other payload -> Some payload | Append _ -> spool_ref);
        s (state x.state);
        ni (Option.map P.Uidvalidity.to_int64 x.uidvalidity);
        ni (Option.map P.Uid.to_int64 x.uid);
        ni frontier; ni length; ni (Option.map (fun _ -> 1L) flags); ns date]);
    Option.iter (List.iteri (fun ord flag ->
      run t "INSERT INTO intent_flags VALUES (?,?,?)"
        [s x.id; i (Int64.of_int ord); s (Mail_flag.Imap_flag.to_wire flag)]))
      flags)

let decode_append t ~id ~message_id ~digest ~spool_ref ~frontier ~length
    ~flags_known ~date =
  let expected_flags = match flags_known with
    | S.Data.NULL -> None
    | S.Data.INT 1L ->
      Some (rows t "SELECT flag FROM intent_flags WHERE intent_id=? ORDER BY ord"
        [s id] |> List.map (fun r ->
          of_checked "intent flag" Mail_flag.Imap_flag.of_wire (text r.(0))))
    | _ -> fail "invalid expected-flags marker" in
  let frontier=nullable_int frontier and length=nullable_int length in
  Option.iter (fun n -> if n<0L || n>4_294_967_295L then
    fail "invalid stored UID frontier") frontier;
  Option.iter (fun n -> if n<0L then fail "negative stored expected length") length;
  Append {message_id=text message_id;content_digest=text digest;
    spool_ref=text spool_ref;pre_send_uid_frontier=frontier;
    expected_length=length;expected_flags;
    expected_internal_date=nullable_text date}

let legal before after = match before,after with
  | Prepared,(Sent|Ambiguous|Rejected)
  | Sent,(Ambiguous|Confirmed|Rejected)
  | Ambiguous,(Confirmed|Rejected) -> true
  | _ -> false

let set_intent_state t ~id next =
  transaction t (fun () ->
    match rows t "SELECT state FROM intents WHERE id=?" [s id] with
    | [r] ->
      let before = dec_state (text r.(0)) in
      if not (legal before next) then invalid_arg "Imap_store.set_intent_state: illegal transition";
      run t "UPDATE intents SET state=? WHERE id=?" [s (state next); s id]
    | [] -> invalid_arg "Imap_store.set_intent_state: unknown ID"
    | _ -> fail "duplicate intent ID")

let confirm_intent t ~id ~uidvalidity ~uid =
  if uid <> None && uidvalidity = None then
    invalid_arg "Imap_store.confirm_intent: UID without UIDVALIDITY";
  transaction t (fun () ->
    match rows t "SELECT state FROM intents WHERE id=?" [s id] with
    | [r] when legal (dec_state (text r.(0))) Confirmed ->
      run t "UPDATE intents SET state='confirmed', uidvalidity=?, uid=? WHERE id=?"
        [ni (Option.map P.Uidvalidity.to_int64 uidvalidity);
         ni (Option.map P.Uid.to_int64 uid); s id]
    | [r] ->
      ignore (dec_state (text r.(0)) : intent_state);
      invalid_arg "Imap_store.confirm_intent: illegal transition"
    | [] -> invalid_arg "Imap_store.confirm_intent: unknown ID"
    | _ -> fail "duplicate intent ID")

let pending_intents t ~scope =
  locked t (fun () ->
    rows t "SELECT id,raw_name,encoding,mailbox_id,kind,message_id,digest, \
      spool_ref,state,uidvalidity,uid,pre_send_frontier,expected_length, \
      expected_flags_known,expected_internal_date FROM intents WHERE \
      endpoint=? AND account=? AND mailbox_key=? AND \
      state IN ('prepared','sent','ambiguous') ORDER BY rowid" (scope_key scope)
    |> List.map (fun r ->
      let stored_scope : M.scope = { scope with raw_name=text r.(1);
        encoding=dec_enc (text r.(2)); mailbox_id=nullable_text r.(3) } in
      if stored_scope <> scope then fail "stored intent scope differs from requested scope";
      let kind = match text r.(4) with
        | "append" -> decode_append t ~id:(text r.(0))
            ~message_id:r.(5) ~digest:r.(6) ~spool_ref:r.(7)
            ~frontier:r.(11) ~length:r.(12) ~flags_known:r.(13)
            ~date:r.(14)
        | "other" -> Other (text r.(7))
        | _ -> fail "unknown intent kind" in
      {id=text r.(0);scope;kind;state=dec_state (text r.(8));
       uidvalidity=Option.map validity (nullable_int r.(9));
       uid=Option.map uid (nullable_int r.(10))}))

let find_intent t ~id =
  locked t (fun () ->
    match rows t "SELECT endpoint,account,mailbox_key,raw_name,encoding, \
      mailbox_id,kind,message_id,digest,spool_ref,state,uidvalidity,uid, \
      pre_send_frontier,expected_length,expected_flags_known, \
      expected_internal_date \
      FROM intents WHERE id=?" [s id] with
    | [] -> None
    | [r] ->
      let scope : M.scope = {endpoint=text r.(0);account=text r.(1);
        mailbox_key=text r.(2);raw_name=text r.(3);
        encoding=dec_enc (text r.(4));mailbox_id=nullable_text r.(5)} in
      let kind=match text r.(6) with
        | "append" -> decode_append t ~id
            ~message_id:r.(7) ~digest:r.(8) ~spool_ref:r.(9)
            ~frontier:r.(13) ~length:r.(14) ~flags_known:r.(15)
            ~date:r.(16)
        | "other" -> Other (text r.(9))
        | _ -> fail "unknown intent kind" in
      Some {id;scope;kind;state=dec_state (text r.(10));
        uidvalidity=Option.map validity (nullable_int r.(11));
        uid=Option.map uid (nullable_int r.(12))}
    | _ -> fail "duplicate intent ID")

