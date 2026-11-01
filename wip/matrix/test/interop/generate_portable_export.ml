module Export = Matrix_client.Room_key_export
module Olm = Matrix_client.Olm
module Ck = Matrix_client.Crypto_key
module Rnd = Matrix_client.Random
module Id = Matrix_proto.Id

let room_id = Id.Room_id.of_string_exn "!interop:example.org"
let passphrase = "ocaml-rust-portable-export"

let export_random () =
  Rnd.of_source
    (Eio.Flow.string_source
       (String.make 16 '\x11' ^ String.make 16 '\x22' ^ String.make 4096 '\000'))

let session_random () =
  Rnd.of_source (Eio.Flow.string_source (String.make 4096 '\x33'))

let curve byte =
  match Ck.Curve25519.Public.of_bytes (String.make 32 byte) with
  | Ok key -> Ck.Curve25519.Public.to_base64 key
  | Error (`Msg message) -> failwith message

let encoded_or_fail codec value =
  match Jsont_bytesrw.encode_string codec value with
  | Ok value -> value
  | Error message -> failwith message

let () =
  (* Generate a real deterministic Megolm v1 session.  In particular, the
     portable file carries the exported key (wire version 1), not the ordinary
     room-key form (wire version 2). *)
  let outbound =
    Olm.Megolm.Outbound.create ~random:(session_random ()) ~room_id ()
  in
  let key =
    {
      Export.algorithm = "m.megolm.v1.aes-sha2";
      room_id;
      sender_key = curve '\007';
      session_id = Olm.Megolm.Outbound.session_id outbound;
      session_key = Olm.Megolm.Outbound.exported_session_key outbound;
      sender_claimed_keys =
        [
          ( "ed25519",
            Ck.Ed25519.Public.to_base64
              (Olm.Megolm.Outbound.signing_key outbound) );
        ];
      forwarding_curve25519_key_chain = [ curve '\009'; curve '\010' ];
      shared_history = true;
    }
  in
  let plaintext = encoded_or_fail Export.room_keys_jsont [ key ] in
  let armored =
    match
      Export.encrypt ~random:(export_random ()) ~passphrase ~rounds:10_000
        [ key ]
    with
    | Ok value -> value
    | Error error -> failwith (Format.asprintf "%a" Export.pp_error error)
  in
  if Array.length Sys.argv > 1 && String.equal Sys.argv.(1) "--plaintext" then
    print_endline plaintext
  else print_endline armored
