let get = function Ok value -> value | Error message -> failwith message
let check message condition = if not condition then failwith message

let () =
  let module Actor = Apubt_proto.Actor in
  let actor = get (Jsont_bytesrw.decode_string Actor.jsont
    {|{"id":"https://EXAMPLE.org/users/a","type":"Person",
       "inbox":"https://example.org/inbox","outbox":"https://example.org/outbox",
       "alsoKnownAs":"https://example.net/a%2Fb"}|}) in
  check "actor URI is canonical"
    (Uriz.to_string (Actor.id actor) = "https://example.org/users/a");
  check "URI list accepts a single value"
    (Option.map (List.map Uriz.to_string) (Actor.also_known_as actor)
      = Some ["https://example.net/a%2Fb"]);
  let encoded = get (Jsont_bytesrw.encode_string Actor.jsont actor) in
  let decoded = get (Jsont_bytesrw.decode_string Actor.jsont encoded) in
  check "actor URI round trip" (Uriz.equal (Actor.id actor) (Actor.id decoded));
  (match Jsont_bytesrw.decode_string Apubt_proto.uri_jsont {|"https://bad host/a"|} with
   | Error _ -> ()
   | Ok _ -> failwith "invalid URI accepted")
