open Database
open Record_codec
module M = Imap.Mirror
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
  uidvalidity : Imap.Uidvalidity.t option; uid : Imap.Uid.t option
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
  if x.state <> Prepared then
    invalid_arg "Imap_store.prepare_intent: state must be Prepared";
  if x.uid <> None && x.uidvalidity = None then
    invalid_arg "Imap_store.prepare_intent: UID without UIDVALIDITY";
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
        ni (Option.map Imap.Uidvalidity.to_int64 x.uidvalidity);
        ni (Option.map Imap.Uid.to_int64 x.uid);
        ni frontier; ni length; ni (Option.map (fun _ -> 1L) flags); ns date]);
    Option.iter (List.iteri (fun ord flag ->
      run t "INSERT INTO intent_flags VALUES (?,?,?)"
        [s x.id; i (Int64.of_int ord); s (Mail_flag.Imap_flag.to_wire flag)]))
      flags)

let decode_intent t r =
  let id=text r.(0) in
  let scope : M.scope = {endpoint=text r.(1);account=text r.(2);
    mailbox_key=text r.(3);raw_name=text r.(4);
    encoding=dec_enc (text r.(5));mailbox_id=nullable_text r.(6)} in
  let legacy x = Option.value ~default:"" (nullable_text x) in
  let kind=match text r.(7) with
    | "append" ->
      let expected_flags=match r.(16) with
        | S.Data.NULL -> None
        | S.Data.INT 1L ->
          Some (rows t "SELECT flag FROM intent_flags WHERE intent_id=? \
            ORDER BY ord" [s id] |> List.map (fun f ->
              of_checked "intent flag" Mail_flag.Imap_flag.of_wire
                (text f.(0))))
        | _ -> fail "invalid expected-flags marker" in
      let frontier=nullable_int r.(14) and length=nullable_int r.(15) in
      Option.iter (fun n -> if n<0L || n>4_294_967_295L then
        fail "invalid stored UID frontier") frontier;
      Option.iter (fun n -> if n<0L then
        fail "negative stored expected length") length;
      Append {message_id=legacy r.(8);content_digest=legacy r.(9);
        spool_ref=legacy r.(10);pre_send_uid_frontier=frontier;
        expected_length=length;expected_flags;
        expected_internal_date=nullable_text r.(17)}
    | "other" -> Other (legacy r.(10))
    | other -> fail (Printf.sprintf "unknown intent kind %S" other) in
  {id;scope;kind;state=dec_state (text r.(11));
   uidvalidity=Option.map validity (nullable_int r.(12));
   uid=Option.map uid (nullable_int r.(13))}

let select_intents = "SELECT id,endpoint,account,mailbox_key,raw_name,\
  encoding,mailbox_id,kind,message_id,digest,spool_ref,state,uidvalidity,\
  uid,pre_send_frontier,expected_length,expected_flags_known,\
  expected_internal_date FROM intents WHERE "

let legal before after = match before,after with
  | Prepared,(Sent|Ambiguous|Rejected)
  | Sent,(Ambiguous|Confirmed|Rejected)
  | Ambiguous,(Confirmed|Rejected) -> true
  | _ -> false

let set_intent_state t ~id next =
  transaction t (fun () ->
    match rows t "SELECT state FROM intents WHERE id=?" [s id] with
    | r :: _ ->
      if not (legal (dec_state (text r.(0))) next) then
        invalid_arg "Imap_store.set_intent_state: illegal transition";
      run t "UPDATE intents SET state=? WHERE id=?" [s (state next); s id]
    | [] -> invalid_arg "Imap_store.set_intent_state: unknown ID")

let confirm_intent t ~id ~uidvalidity ~uid =
  if uid <> None && uidvalidity = None then
    invalid_arg "Imap_store.confirm_intent: UID without UIDVALIDITY";
  transaction t (fun () ->
    match rows t "SELECT state FROM intents WHERE id=?" [s id] with
    | r :: _ when legal (dec_state (text r.(0))) Confirmed ->
      run t "UPDATE intents SET state='confirmed',\
        uidvalidity=COALESCE(?,uidvalidity),uid=COALESCE(?,uid) WHERE id=?"
        [ni (Option.map Imap.Uidvalidity.to_int64 uidvalidity);
         ni (Option.map Imap.Uid.to_int64 uid); s id]
    | _ :: _ -> invalid_arg "Imap_store.confirm_intent: illegal transition"
    | [] -> invalid_arg "Imap_store.confirm_intent: unknown ID")

let pending_intents t ~scope =
  locked t (fun () ->
    rows t (select_intents ^ "endpoint=? AND account=? AND mailbox_key=? \
      AND state IN ('prepared','sent','ambiguous') ORDER BY rowid")
      (scope_key scope)
    |> List.map (fun r ->
      let x=decode_intent t r in
      if x.scope <> scope then
        fail "stored intent scope differs from requested scope";
      x))

let find_intent t ~id =
  locked t (fun () ->
    match rows t (select_intents ^ "id=?") [s id] with
    | [] -> None
    | r :: _ -> Some (decode_intent t r))
