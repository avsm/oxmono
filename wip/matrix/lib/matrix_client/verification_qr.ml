module Ev = Matrix_proto.Event
module Cancel_code = Verification_base.Cancel_code
module Transaction = Verification_base.Transaction
module Message = Verification_base.Message

type error = [ `Msg of string ]

let err fmt = Format.kasprintf (fun m -> Error (`Msg m)) fmt
let b64e s = Matrix_proto.Base64.encode s

type mode =
  | Verifying_another_user
  | Self_verifying_master_key_trusted
  | Self_verifying_master_key_untrusted

let mode_to_int = function
  | Verifying_another_user -> 0
  | Self_verifying_master_key_trusted -> 1
  | Self_verifying_master_key_untrusted -> 2

let mode_of_int = function
  | 0 -> Some Verifying_another_user
  | 1 -> Some Self_verifying_master_key_trusted
  | 2 -> Some Self_verifying_master_key_untrusted
  | _ -> None

let pp_mode ppf m = Format.pp_print_int ppf (mode_to_int m)

type t = {
  mode : mode;
  flow_id : string;
  first_key : string;
  second_key : string;
  shared_secret : string;
}

let header = "MATRIX"
let version = 2
let min_secret_len = 8
let secret_len = 8
let max_flow_id_len = 0xffff
let key_len = 32
let mode t = t.mode
let flow_id t = t.flow_id
let first_key t = b64e t.first_key
let second_key t = b64e t.second_key
let shared_secret t = b64e t.shared_secret
let shared_secret_raw t = t.shared_secret
let pp ppf t = Format.fprintf ppf "qr mode=%a flow=%s" pp_mode t.mode t.flow_id

let equal a b =
  a.mode = b.mode
  && String.equal a.flow_id b.flow_id
  && String.equal a.first_key b.first_key
  && String.equal a.second_key b.second_key
  && String.equal a.shared_secret b.shared_secret

let key_of_base64 what s =
  match Matrix_proto.Base64.decode_opt s with
  | Some raw when String.length raw = key_len -> Ok raw
  | Some raw ->
      err "%s: expected a %d byte key, got %d" what key_len (String.length raw)
  | None -> err "%s: not valid base64" what

let make ~mode ~flow_id ~first_key ~second_key ~shared_secret =
  let ( let* ) = Result.bind in
  let* first_key = key_of_base64 "first key" first_key in
  let* second_key = key_of_base64 "second key" second_key in
  let* shared_secret =
    match Matrix_proto.Base64.decode_opt shared_secret with
    | None -> err "shared secret: not valid base64"
    | Some s when String.length s < min_secret_len ->
        err "shared secret: shorter than %d bytes" min_secret_len
    | Some s -> Ok s
  in
  if String.length flow_id > max_flow_id_len then err "flow id is too long"
  else Ok { mode; flow_id; first_key; second_key; shared_secret }

let create ~random ~mode ~flow_id ~first_key ~second_key =
  make ~mode ~flow_id ~first_key ~second_key
    ~shared_secret:(b64e (Random.generate random secret_len))

let for_other_user ~random ~flow_id ~our_master_key ~their_master_key =
  create ~random ~mode:Verifying_another_user ~flow_id ~first_key:our_master_key
    ~second_key:their_master_key

let for_self_trusted ~random ~flow_id ~master_key ~their_device_key =
  create ~random ~mode:Self_verifying_master_key_trusted ~flow_id
    ~first_key:master_key ~second_key:their_device_key

let for_self_untrusted ~random ~flow_id ~our_device_key ~master_key =
  create ~random ~mode:Self_verifying_master_key_untrusted ~flow_id
    ~first_key:our_device_key ~second_key:master_key

let encode t =
  if String.length t.flow_id > max_flow_id_len then err "flow id is too long"
  else if
    String.length t.first_key <> key_len
    || String.length t.second_key <> key_len
  then err "keys must be %d bytes" key_len
  else begin
    let buf = Buffer.create 128 in
    Buffer.add_string buf header;
    Buffer.add_char buf (Char.chr version);
    Buffer.add_char buf (Char.chr (mode_to_int t.mode));
    let len = String.length t.flow_id in
    Buffer.add_char buf (Char.chr ((len lsr 8) land 0xff));
    Buffer.add_char buf (Char.chr (len land 0xff));
    Buffer.add_string buf t.flow_id;
    Buffer.add_string buf t.first_key;
    Buffer.add_string buf t.second_key;
    Buffer.add_string buf t.shared_secret;
    Ok (Buffer.contents buf)
  end

let decode data =
  let n = String.length data in
  if n < 10 then err "QR payload is too short"
  else if String.sub data 0 6 <> header then err "bad QR header"
  else
    let v = Char.code data.[6] in
    let m = Char.code data.[7] in
    if v <> version then err "unsupported QR code version %d" v
    else
      match mode_of_int m with
      | None -> err "unknown QR verification mode %d" m
      | Some mode ->
          let len = (Char.code data.[8] lsl 8) lor Char.code data.[9] in
          let after_flow = 10 + len in
          if n < after_flow + (2 * key_len) + min_secret_len then
            err "QR payload is truncated"
          else
            let flow_id = String.sub data 10 len in
            let first_key = String.sub data after_flow key_len in
            let second_key = String.sub data (after_flow + key_len) key_len in
            let secret_off = after_flow + (2 * key_len) in
            let shared_secret = String.sub data secret_off (n - secret_off) in
            Ok { mode; flow_id; first_key; second_key; shared_secret }

let check t ~flow_id ~our_master_key ~our_device_key ~their_master_key
    ~their_device_key =
  let want expected got =
    match expected with
    | Some e when String.equal e got -> Ok ()
    | _ -> Error Cancel_code.Key_mismatch
  in
  let ( let* ) = Result.bind in
  if not (String.equal t.flow_id flow_id) then
    Error Cancel_code.Unknown_transaction
  else
    let first = first_key t and second = second_key t in
    match t.mode with
    | Verifying_another_user ->
        let* () = want their_master_key first in
        want our_master_key second
    | Self_verifying_master_key_trusted ->
        let* () = want their_master_key first in
        want our_device_key second
    | Self_verifying_master_key_untrusted ->
        let* () = want their_device_key first in
        want our_master_key second

let reciprocate_start ~transaction ~from_device t =
  Message.v transaction
    (Message.Start
       (Ev.Key_verification_start_content.make
          ~from_device:(Matrix_proto.Id.Device_id.to_string from_device)
          ~method_:"m.reciprocate.v1"
          ?transaction_id:(Transaction.transaction_id transaction)
          ~secret:(shared_secret t)
          ?relates_to:(Transaction.relates_to transaction)
          ()))

let check_reciprocate t ~secret =
  match Matrix_proto.Base64.decode_opt secret with
  | Some raw when Olm_primitives.ct_equal raw t.shared_secret -> Ok ()
  | _ -> Error Cancel_code.Key_mismatch
