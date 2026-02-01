(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type payload = {
  exp : Ptime.t option;
  iat : Ptime.t option;
  sub : string option;
  aud : string option;
  iss : string option;
}

(* Build payload from jsonwt Claims *)
let payload_of_claims (claims : Jsonwt.Claims.t) : payload =
  let aud =
    match Jsonwt.Claims.aud claims with [] -> None | hd :: _ -> Some hd
  in
  {
    exp = Jsonwt.Claims.exp claims;
    iat = Jsonwt.Claims.iat claims;
    sub = Jsonwt.Claims.sub claims;
    aud;
    iss = Jsonwt.Claims.iss claims;
  }

let decode_payload jwt =
  match Jsonwt.parse ~strict:false jwt with
  | Ok token -> Ok (payload_of_claims (Jsonwt.claims token))
  | Error e -> Error (Jsonwt.error_to_string e)

let is_expired ?(leeway = Ptime.Span.of_int_s 60) jwt =
  match Jsonwt.parse ~strict:false jwt with
  | Ok token ->
      let now = Ptime_clock.now () in
      (* Negate leeway to check if token expires within that time *)
      let neg_leeway = Ptime.Span.neg leeway in
      Jsonwt.is_expired ~now ~leeway:neg_leeway token
  | Error _ ->
      (* Decode failed - fail safe by assuming expired *)
      true

let get_expiration jwt =
  match Jsonwt.parse ~strict:false jwt with
  | Ok token -> Jsonwt.Claims.exp (Jsonwt.claims token)
  | Error _ -> None

let time_to_expiry jwt =
  match Jsonwt.parse ~strict:false jwt with
  | Ok token -> Jsonwt.time_to_expiry ~now:(Ptime_clock.now ()) token
  | Error _ -> None
