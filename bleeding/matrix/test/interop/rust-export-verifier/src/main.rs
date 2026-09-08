use std::{env, fs, io::Cursor, path::PathBuf};

use matrix_sdk_crypto::decrypt_room_key_export;
use serde_json::Value;

fn main() {
    let root = env::args_os()
        .nth(1)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("../../.."));
    let fixture = root.join("test/fixtures/portable-room-keys.ocaml.megolm");
    let expected = root.join("test/fixtures/portable-room-keys.expected.json");
    let passphrase = "ocaml-rust-portable-export";

    let armored = fs::read_to_string(&fixture).expect("read OCaml export fixture");
    let expected: Value =
        serde_json::from_str(&fs::read_to_string(&expected).expect("read expected plaintext"))
            .expect("parse expected plaintext");
    let keys = decrypt_room_key_export(Cursor::new(armored), passphrase)
        .expect("Rust should decrypt the OCaml-generated export");
    assert_eq!(keys.len(), 1, "fixture should contain one room key");
    let actual = serde_json::to_value(&keys[0]).expect("serialize Rust room key");
    let expected = expected
        .as_array()
        .and_then(|keys| keys.first())
        .expect("expected plaintext should contain one room key");
    assert_eq!(&actual, expected, "all decrypted room-key fields must match");
    assert_eq!(actual["m.shared_history"], true);
    assert_eq!(actual["forwarding_curve25519_key_chain"].as_array().unwrap().len(), 2);
    println!("verified Rust matrix-sdk-crypto room-key export interop");
}
