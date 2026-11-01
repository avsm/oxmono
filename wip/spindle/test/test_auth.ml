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
  Printf.printf "service auth: %d signed-token cases passed\n"
    (List.length tests)
