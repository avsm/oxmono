module S = Matrix_client.Secret_storage

type key = S.key
type key_check = S.key_check = Correct | Incorrect | Unchecked

let of_result = function
  | Ok v -> v
  | Error (`Msg msg) -> raise (Error.err (Error.Json msg))

let algorithm = S.algorithm
let pbkdf2_algorithm = S.pbkdf2_algorithm
let secret_cross_signing_master = S.secret_cross_signing_master
let secret_cross_signing_self_signing = S.secret_cross_signing_self_signing
let secret_cross_signing_user_signing = S.secret_cross_signing_user_signing
let secret_megolm_backup_v1 = S.secret_megolm_backup_v1
let generate_key = S.generate_key
let key_of_bytes b = of_result (S.key_of_bytes b)
let to_bytes = S.to_bytes

module Recovery_key = struct
  let encode = S.Recovery_key.encode
  let decode s = of_result (S.Recovery_key.decode s)
end

module Passphrase_info = struct
  type t = S.Passphrase_info.t = {
    algorithm : string;
    salt : string;
    iterations : int;
    bits : int;
  }

  let default_bits = S.Passphrase_info.default_bits
  let v = S.Passphrase_info.v
  let jsont = S.Passphrase_info.jsont
end

module Key_description = struct
  type t = S.Key_description.t = {
    name : string option;
    algorithm : string;
    passphrase : Passphrase_info.t option;
    iv : string option;
    mac : string option;
  }

  let v = S.Key_description.v
  let jsont = S.Key_description.jsont
end

let key_of_passphrase ~passphrase info =
  of_result (S.key_of_passphrase ~passphrase info)

let check_key = S.check_key

module Encrypted = struct
  type t = S.Encrypted.t = { iv : string; ciphertext : string; mac : string }

  let jsont = S.Encrypted.jsont
end

let encrypt = S.encrypt
let decrypt key ~name data = of_result (S.decrypt key ~name data)
