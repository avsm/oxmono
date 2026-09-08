module Ed25519 = Crypto_key.Ed25519
module Curve25519 = Crypto_key.Curve25519

type stored_key = { key_id : string; secret : Curve25519.Secret.t }

type t = {
  ed25519 : Ed25519.Private.t;
  curve25519 : Curve25519.Secret.t;
  mutable one_time_keys : stored_key list;
  mutable fallback : stored_key option;
  mutable previous_fallback : stored_key option;
  mutable next_key_id : int;
  max_one_time_keys : int;
}

(* vodozemac deliberately exposes a small public pool while retaining a much
   larger private reserve.  The reserve lets a device generate replacements
   as the homeserver consumes published keys without losing the secrets needed
   to finish handshakes which race with the count update. *)
let private_one_time_key_limit t =
  if t.max_one_time_keys > max_int / 100 then max_int
  else max 0 (100 * t.max_one_time_keys)

let one_time_key_algorithm = "signed_curve25519"

let create ~random () =
  let ed25519, _ = Ed25519.generate ~random () in
  let curve25519, _ = Curve25519.generate ~random () in
  {
    ed25519;
    curve25519;
    one_time_keys = [];
    fallback = None;
    previous_fallback = None;
    next_key_id = 0;
    max_one_time_keys = 50;
  }

let ed25519_key t = Ed25519.Private.public t.ed25519
let curve25519_key t = Curve25519.Secret.public t.curve25519
let identity_keys t = (ed25519_key t, curve25519_key t)
let sign t data = Ed25519.Private.sign t.ed25519 data

(* Key ids are the base64 of a big-endian 64-bit counter, as in vodozemac. *)
let generate_key_id t =
  let n = t.next_key_id in
  t.next_key_id <- n + 1;
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 (Int64.of_int n);
  Olm_primitives.base64_encode (Bytes.unsafe_to_string b)

let key_id k =
  Crypto_key.Key_id.v ~algorithm:one_time_key_algorithm ~id:k.key_id

let public k = Curve25519.Secret.public k.secret

let generate_one_time_keys ~random t count =
  let generated = ref [] in
  for _ = 1 to count do
    let secret, _ = Curve25519.generate ~random () in
    let key_id = generate_key_id t in
    generated := { key_id; secret } :: !generated
  done;
  let all = !generated @ t.one_time_keys in
  let limit = private_one_time_key_limit t in
  let rec take n = function
    | _ when n = 0 -> []
    | [] -> []
    | x :: xs -> x :: take (n - 1) xs
  in
  t.one_time_keys <- take limit all

let one_time_keys t = List.map (fun k -> (key_id k, public k)) t.one_time_keys
let one_time_key_ids t = List.map key_id t.one_time_keys

let signed_one_time_keys ?(exclude = fun _ -> false) t =
  List.map
    (fun k ->
      let pub = public k in
      (* The canonical JSON lives in {!Keys} so that this and a key uploaded
         from a stored pool sign over the same bytes. *)
      let json =
        Keys.one_time_key_signing_json (Curve25519.Public.to_base64 pub)
      in
      (key_id k, pub, sign t json))
    (List.filter (fun k -> not (exclude (key_id k))) t.one_time_keys)

let generate_fallback_key ~random t =
  let secret, _ = Curve25519.generate ~random () in
  let key_id = generate_key_id t in
  (* vodozemac retains exactly one previous fallback key.  This lets a
     pre-key message sent just before rotation complete its handshake, while
     ensuring that only [fallback] is ever published. *)
  t.previous_fallback <- t.fallback;
  t.fallback <- Some { key_id; secret }

let fallback_key t = Option.map (fun k -> (key_id k, public k)) t.fallback

let forget_previous_fallback_key t =
  match t.previous_fallback with
  | None -> false
  | Some _ ->
      t.previous_fallback <- None;
      true

let one_time_keys_count t = List.length t.one_time_keys
let max_one_time_keys t = t.max_one_time_keys

let exchange secret public =
  match Curve25519.key_exchange ~secret ~public with
  | Ok s -> Ok s
  | Error (`Msg m) -> Error (Olm_error.Key_exchange_failed m)

let identity_exchange t public = exchange t.curve25519 public

let find_one_time_key t key =
  let same k = Curve25519.Public.equal (public k) key in
  match List.find_opt same t.one_time_keys with
  | Some k -> Some k.secret
  | None -> (
      match t.fallback with
      | Some k when same k -> Some k.secret
      | _ -> (
          match t.previous_fallback with
          | Some k when same k -> Some k.secret
          | _ -> None))

let one_time_key_exchange t ~key ~peer =
  match find_one_time_key t key with
  | None -> Error Olm_error.Unknown_one_time_key
  | Some secret -> exchange secret peer

let consume_one_time_key t key =
  t.one_time_keys <-
    List.filter
      (fun k -> not (Curve25519.Public.equal (public k) key))
      t.one_time_keys

type pickle = {
  ed25519 : Ed25519.Private.t;
  curve25519 : Curve25519.Secret.t;
  stored_one_time_keys : stored_key list;
  stored_fallback_key : stored_key option;
  stored_previous_fallback_key : stored_key option;
  next_key_id : int;
  max_one_time_keys : int;
}

let to_pickle (t : t) =
  {
    ed25519 = t.ed25519;
    curve25519 = t.curve25519;
    stored_one_time_keys = t.one_time_keys;
    stored_fallback_key = t.fallback;
    stored_previous_fallback_key = t.previous_fallback;
    next_key_id = t.next_key_id;
    max_one_time_keys = t.max_one_time_keys;
  }

let of_pickle (p : pickle) : t =
  {
    ed25519 = p.ed25519;
    curve25519 = p.curve25519;
    one_time_keys = p.stored_one_time_keys;
    fallback = p.stored_fallback_key;
    previous_fallback = p.stored_previous_fallback_key;
    next_key_id = p.next_key_id;
    max_one_time_keys = p.max_one_time_keys;
  }
