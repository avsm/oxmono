//! A line-oriented JSON oracle around vodozemac.
//!
//! Reads one JSON command per line on stdin and writes one JSON reply per
//! line on stdout. Handles (accounts and sessions) live in memory only; pickle
//! commands return strings and no command touches the disk. See README.md for
//! the command set.

use std::io::{self, BufRead, Write};

use hkdf::Hkdf;
use serde_json::{json, Map, Value};
use sha2::Sha256;
use vodozemac::megolm::{
    ExportedSessionKey, GroupSession, InboundGroupSession, MegolmMessage, SessionConfig as MegolmSessionConfig,
    SessionKey,
};
use vodozemac::olm::{Account, OlmMessage, Session, SessionConfig};
use vodozemac::sas::{EstablishedSas, Sas};
use vodozemac::{base64_decode, base64_encode, Curve25519PublicKey, Ed25519PublicKey};

#[derive(Default)]
struct State {
    accounts: Vec<Account>,
    sessions: Vec<Session>,
    group_sessions: Vec<GroupSession>,
    inbound_group_sessions: Vec<InboundGroupSession>,
    sas: Vec<Option<Sas>>,
    established_sas: Vec<Option<EstablishedSas>>,
}

type Res = Result<Value, String>;

fn str_arg(v: &Map<String, Value>, name: &str) -> Result<String, String> {
    v.get(name)
        .and_then(|x| x.as_str())
        .map(|s| s.to_owned())
        .ok_or_else(|| format!("missing string argument {name:?}"))
}

fn usize_arg(v: &Map<String, Value>, name: &str) -> Result<usize, String> {
    v.get(name)
        .and_then(|x| x.as_u64())
        .map(|n| n as usize)
        .ok_or_else(|| format!("missing integer argument {name:?}"))
}

fn u32_arg(v: &Map<String, Value>, name: &str) -> Result<u32, String> {
    v.get(name)
        .and_then(|x| x.as_u64())
        .map(|n| n as u32)
        .ok_or_else(|| format!("missing integer argument {name:?}"))
}

fn curve_key(s: &str) -> Result<Curve25519PublicKey, String> {
    Curve25519PublicKey::from_base64(s).map_err(|e| e.to_string())
}

fn pickle_key(v: &Map<String, Value>) -> Result<Vec<u8>, String> {
    let mut key =
        base64_decode(&str_arg(v, "pickle_key")?).map_err(|e| e.to_string())?;
    if let Some(device_id) = v.get("device_id").and_then(|x| x.as_str()) {
        let hk = Hkdf::<Sha256>::new(Some(device_id.as_bytes()), &key);
        let mut derived = [0u8; 32];
        hk.expand(b"dehydrated-device-pickle-key", &mut derived)
            .map_err(|e| e.to_string())?;
        key = derived.to_vec();
    }
    Ok(key)
}

fn account_mut<'a>(st: &'a mut State, i: usize) -> Result<&'a mut Account, String> {
    st.accounts.get_mut(i).ok_or_else(|| format!("no account {i}"))
}

fn session_mut<'a>(st: &'a mut State, i: usize) -> Result<&'a mut Session, String> {
    st.sessions.get_mut(i).ok_or_else(|| format!("no session {i}"))
}

fn one_time_keys_json(account: &Account) -> Value {
    let mut m = Map::new();
    for (id, key) in account.one_time_keys() {
        m.insert(id.to_base64(), Value::String(key.to_base64()));
    }
    Value::Object(m)
}

fn dispatch(st: &mut State, cmd: &str, a: &Map<String, Value>) -> Res {
    match cmd {
        // ---- accounts ------------------------------------------------
        "create_account" => {
            let mut account = Account::new();
            let count = a.get("one_time_keys").and_then(|x| x.as_u64()).unwrap_or(0) as usize;
            if count > 0 {
                account.generate_one_time_keys(count);
            }
            let keys = account.identity_keys();
            let otks = one_time_keys_json(&account);
            st.accounts.push(account);
            Ok(json!({
                "account": st.accounts.len() - 1,
                "curve25519": keys.curve25519.to_base64(),
                "ed25519": keys.ed25519.to_base64(),
                "one_time_keys": otks,
            }))
        }
        "generate_one_time_keys" => {
            let i = usize_arg(a, "account")?;
            let count = usize_arg(a, "count")?;
            let account = account_mut(st, i)?;
            account.generate_one_time_keys(count);
            Ok(json!({ "one_time_keys": one_time_keys_json(account) }))
        }
        "generate_fallback_key" => {
            let i = usize_arg(a, "account")?;
            let account = account_mut(st, i)?;
            account.generate_fallback_key();
            Ok(json!({
                "fallback_keys": account
                    .fallback_key()
                    .keys()
                    .map(|k| k.to_base64())
                    .collect::<Vec<_>>()
            }))
        }
        "mark_keys_as_published" => {
            let i = usize_arg(a, "account")?;
            account_mut(st, i)?.mark_keys_as_published();
            Ok(json!({ "marked": true }))
        }
        "identity_keys" => {
            let i = usize_arg(a, "account")?;
            let account = account_mut(st, i)?;
            let keys = account.identity_keys();
            Ok(json!({
                "curve25519": keys.curve25519.to_base64(),
                "ed25519": keys.ed25519.to_base64(),
            }))
        }
        "account_pickle" => {
            let i = usize_arg(a, "account")?;
            let key = pickle_key(a)?;
            let account = st
                .accounts
                .get(i)
                .ok_or_else(|| format!("no account {i}"))?;
            let pickle = account.to_libolm_pickle(&key).map_err(|e| e.to_string())?;
            Ok(json!({ "pickle": pickle }))
        }
        "account_from_pickle" => {
            let pickle = str_arg(a, "pickle")?;
            let key = pickle_key(a)?;
            let account =
                Account::from_libolm_pickle(&pickle, &key).map_err(|e| e.to_string())?;
            let keys = account.identity_keys();
            Ok(json!({
                "ed25519": keys.ed25519.to_base64(),
                "curve25519": keys.curve25519.to_base64(),
                "one_time_keys": one_time_keys_json(&account),
                "fallback_keys": account
                    .fallback_key()
                    .keys()
                    .map(|k| k.to_base64())
                    .collect::<Vec<_>>(),
            }))
        }
        "account_sign" => {
            let i = usize_arg(a, "account")?;
            let message = str_arg(a, "message")?;
            let account = st
                .accounts
                .get(i)
                .ok_or_else(|| format!("no account {i}"))?;
            Ok(json!({
                "signature": base64_encode(account.sign(message.as_bytes()).to_bytes())
            }))
        }
        "account_sign_from_pickle" => {
            let pickle = str_arg(a, "pickle")?;
            let key = pickle_key(a)?;
            let message = str_arg(a, "message")?;
            let account =
                Account::from_libolm_pickle(&pickle, &key).map_err(|e| e.to_string())?;
            Ok(json!({
                "signature": base64_encode(account.sign(message.as_bytes()).to_bytes())
            }))
        }

        // ---- olm sessions -------------------------------------------
        "create_outbound_session" => {
            let i = usize_arg(a, "account")?;
            let identity_key = curve_key(&str_arg(a, "identity_key")?)?;
            let one_time_key = curve_key(&str_arg(a, "one_time_key")?)?;
            let account = account_mut(st, i)?;
            let session =
                account.create_outbound_session(SessionConfig::version_1(), identity_key, one_time_key);
            let id = session.session_id();
            st.sessions.push(session);
            Ok(json!({ "session": st.sessions.len() - 1, "session_id": id }))
        }
        "create_inbound_session" => {
            let i = usize_arg(a, "account")?;
            let identity_key = curve_key(&str_arg(a, "identity_key")?)?;
            let message = str_arg(a, "ciphertext")?;
            let raw = base64_decode(&message).map_err(|e| e.to_string())?;
            let message = match OlmMessage::from_parts(0, &raw) {
                Ok(OlmMessage::PreKey(m)) => m,
                Ok(_) => return Err("expected a pre-key message".to_owned()),
                Err(e) => return Err(e.to_string()),
            };
            let account = account_mut(st, i)?;
            let result = account
                .create_inbound_session(identity_key, &message)
                .map_err(|e| e.to_string())?;
            let id = result.session.session_id();
            let plaintext = String::from_utf8(result.plaintext).map_err(|e| e.to_string())?;
            st.sessions.push(result.session);
            Ok(json!({
                "session": st.sessions.len() - 1,
                "session_id": id,
                "plaintext": plaintext,
            }))
        }
        "session_id" => {
            let i = usize_arg(a, "session")?;
            Ok(json!({ "session_id": session_mut(st, i)?.session_id() }))
        }
        "session_encrypt" => {
            let i = usize_arg(a, "session")?;
            let plaintext = str_arg(a, "plaintext")?;
            let session = session_mut(st, i)?;
            let message = session.encrypt(plaintext.as_bytes());
            let (message_type, ciphertext) = message.to_parts();
            Ok(json!({
                "message_type": message_type,
                "ciphertext": base64_encode(ciphertext),
            }))
        }
        "session_decrypt" => {
            let i = usize_arg(a, "session")?;
            let message_type = usize_arg(a, "message_type")?;
            let ciphertext = str_arg(a, "ciphertext")?;
            let raw = base64_decode(&ciphertext).map_err(|e| e.to_string())?;
            let message = OlmMessage::from_parts(message_type, &raw).map_err(|e| e.to_string())?;
            let session = session_mut(st, i)?;
            let plaintext = session.decrypt(&message).map_err(|e| e.to_string())?;
            Ok(json!({ "plaintext": String::from_utf8(plaintext).map_err(|e| e.to_string())? }))
        }

        // ---- SAS ----------------------------------------------------
        "sas_create" => {
            let sas = Sas::new();
            let public_key = sas.public_key().to_base64();
            st.sas.push(Some(sas));
            Ok(json!({
                "sas": st.sas.len() - 1,
                "public_key": public_key,
            }))
        }
        "sas_establish" => {
            let i = usize_arg(a, "sas")?;
            let other_public_key = str_arg(a, "public_key")?;
            let sas = st
                .sas
                .get_mut(i)
                .ok_or_else(|| format!("no SAS {i}"))?
                .take()
                .ok_or_else(|| format!("SAS {i} was already established"))?;
            let established = sas
                .diffie_hellman_with_raw(&other_public_key)
                .map_err(|e| e.to_string())?;
            st.established_sas.push(Some(established));
            Ok(json!({ "established": st.established_sas.len() - 1 }))
        }
        "sas_mac" => {
            let i = usize_arg(a, "established")?;
            let input = str_arg(a, "input")?;
            let info = str_arg(a, "info")?;
            let legacy = a.get("legacy").and_then(|x| x.as_bool()).unwrap_or(false);
            let sas = st
                .established_sas
                .get(i)
                .and_then(|sas| sas.as_ref())
                .ok_or_else(|| format!("no established SAS {i}"))?;
            let mac = if legacy {
                sas.calculate_mac_invalid_base64(&input, &info)
            } else {
                sas.calculate_mac(&input, &info).to_base64()
            };
            Ok(json!({ "mac": mac }))
        }

        // ---- megolm --------------------------------------------------
        "megolm_create" => {
            let session = GroupSession::new(MegolmSessionConfig::version_1());
            let id = session.session_id();
            let key = session.session_key().to_base64();
            st.group_sessions.push(session);
            Ok(json!({
                "session": st.group_sessions.len() - 1,
                "session_id": id,
                "session_key": key,
            }))
        }
        "megolm_session_key" => {
            let i = usize_arg(a, "session")?;
            let session = st
                .group_sessions
                .get(i)
                .ok_or_else(|| format!("no group session {i}"))?;
            Ok(json!({
                "session_key": session.session_key().to_base64(),
                "message_index": session.message_index(),
            }))
        }
        "megolm_encrypt" => {
            let i = usize_arg(a, "session")?;
            let plaintext = str_arg(a, "plaintext")?;
            let session = st
                .group_sessions
                .get_mut(i)
                .ok_or_else(|| format!("no group session {i}"))?;
            let index = session.message_index();
            let message = session.encrypt(plaintext.as_bytes());
            Ok(json!({ "message_index": index, "ciphertext": message.to_base64() }))
        }
        "megolm_inbound_import" => {
            let key = str_arg(a, "session_key")?;
            let exported = a.get("exported").and_then(|x| x.as_bool()).unwrap_or(false);
            let session = if exported {
                let key = ExportedSessionKey::from_base64(&key).map_err(|e| e.to_string())?;
                InboundGroupSession::import(&key, MegolmSessionConfig::version_1())
            } else {
                let key = SessionKey::from_base64(&key).map_err(|e| e.to_string())?;
                InboundGroupSession::new(&key, MegolmSessionConfig::version_1())
            };
            let id = session.session_id();
            let first = session.first_known_index();
            st.inbound_group_sessions.push(session);
            Ok(json!({
                "session": st.inbound_group_sessions.len() - 1,
                "session_id": id,
                "first_known_index": first,
            }))
        }
        "megolm_decrypt" => {
            let i = usize_arg(a, "session")?;
            let ciphertext = str_arg(a, "ciphertext")?;
            let message = MegolmMessage::from_base64(&ciphertext).map_err(|e| e.to_string())?;
            let session = st
                .inbound_group_sessions
                .get_mut(i)
                .ok_or_else(|| format!("no inbound group session {i}"))?;
            let decrypted = session.decrypt(&message).map_err(|e| e.to_string())?;
            Ok(json!({
                "plaintext": String::from_utf8(decrypted.plaintext).map_err(|e| e.to_string())?,
                "message_index": decrypted.message_index,
            }))
        }
        "megolm_export_at" => {
            let i = usize_arg(a, "session")?;
            let index = u32_arg(a, "index")?;
            let session = st
                .inbound_group_sessions
                .get_mut(i)
                .ok_or_else(|| format!("no inbound group session {i}"))?;
            match session.export_at(index) {
                Some(key) => Ok(json!({ "session_key": key.to_base64() })),
                None => Err(format!("cannot export at index {index}")),
            }
        }
        "megolm_first_known_index" => {
            let i = usize_arg(a, "session")?;
            let session = st
                .inbound_group_sessions
                .get(i)
                .ok_or_else(|| format!("no inbound group session {i}"))?;
            Ok(json!({ "first_known_index": session.first_known_index() }))
        }
        "ed25519_verify" => {
            // Handy for checking that OCaml-produced signatures are valid.
            let key = Ed25519PublicKey::from_base64(&str_arg(a, "key")?)
                .map_err(|e| e.to_string())?;
            let message = str_arg(a, "message")?;
            let signature = vodozemac::Ed25519Signature::from_base64(&str_arg(a, "signature")?)
                .map_err(|e| e.to_string())?;
            key.verify(message.as_bytes(), &signature)
                .map_err(|e| e.to_string())?;
            Ok(json!({ "verified": true }))
        }
        "ping" => Ok(json!({ "pong": true })),
        other => Err(format!("unknown command {other:?}")),
    }
}

fn main() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut state = State::default();

    for line in stdin.lock().lines() {
        let line = match line {
            Ok(l) => l,
            Err(_) => break,
        };
        if line.trim().is_empty() {
            continue;
        }
        let reply = match serde_json::from_str::<Value>(&line) {
            Err(e) => json!({ "ok": false, "error": format!("bad JSON: {e}") }),
            Ok(Value::Object(args)) => {
                let cmd = args.get("cmd").and_then(|x| x.as_str()).unwrap_or("").to_owned();
                match dispatch(&mut state, &cmd, &args) {
                    Ok(Value::Object(mut m)) => {
                        m.insert("ok".to_owned(), Value::Bool(true));
                        Value::Object(m)
                    }
                    Ok(v) => json!({ "ok": true, "result": v }),
                    Err(e) => json!({ "ok": false, "error": e }),
                }
            }
            Ok(_) => json!({ "ok": false, "error": "expected a JSON object" }),
        };
        if writeln!(stdout, "{reply}").is_err() {
            break;
        }
        if stdout.flush().is_err() {
            break;
        }
    }
}
