(* SPDX-License-Identifier: ISC *)
let decode s = Result.get_ok (Jsont_bytesrw.decode_string Jsont.json s)

let member name = function
  | Jsont.Object (members, _) ->
      snd (List.find (fun ((key, _), _) -> key = name) members)
  | _ -> assert false

let string = function Jsont.String (s, _) -> s | _ -> assert false
let array = function Jsont.Array (a, _) -> a | _ -> assert false

let () =
  let fixtures =
    In_channel.with_open_bin "auth-fixtures.json" (fun channel ->
        decode (In_channel.input_all channel))
  in
  let document = string (member "document" fixtures) in
  let tests = array (member "tests" fixtures) in
  List.iter
    (fun test ->
      let name = string (member "name" test) in
      let expected = member "valid" test = Jsont.Json.bool true in
      let resolve _ =
        try string (member "document" test) with Not_found -> document
      in
      let consumed = ref 0 in
      let actual =
        try
          ignore
            (Spindle.Service_auth.authenticate ~resolve
               ~consume:(fun ~issuer:_ ~jti:_ ~expires:_ ->
                 incr consumed;
                 true)
               ~audience:"did:web:spindle.test"
               ~meth:"sh.tangled.ci.triggerPipeline" ~now:1000.
               (string (member "token" test)));
          true
        with Spindle.Service_auth.Rejected -> false
      in
      if actual <> expected || !consumed <> if expected then 1 else 0 then
        failwith name)
    tests;
  (* [controller] reads the claim without verifying the signature. *)
  let unsigned claims =
    let b64 s =
      Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet s
    in
    b64 {|{"alg":"ES256K","typ":"JWT"}|} ^ "." ^ b64 claims ^ "." ^ b64 "sig"
  in
  let controller claims = Spindle.Service_auth.controller (unsigned claims) in
  let user = "did:plc:nhyitepp3u4u6fcfboegzcjw" in
  assert (controller ({|{"act":{"sub":"|} ^ user ^ {|"}}|}) = Some user);
  assert (controller {|{"iss":"did:plc:rrrrrrrrrrrrrrrrrrrrrrrr"}|} = None);
  assert (controller {|{"act":{"sub":"not a did"}}|} = None);
  assert (controller {|{"act":"did:plc:nhyitepp3u4u6fcfboegzcjw"}|} = None);
  assert (Spindle.Service_auth.controller "garbage" = None);
  Printf.printf "service auth: %d signed-token cases passed\n"
    (List.length tests)
