module L = Hermest.Lexicon_types
let get = function Ok value -> value | Error message -> failwith message

let () =
  let doc = get (Jsont_bytesrw.decode_string L.lexicon_doc_jsont
    {|{"lexicon":1,"id":"test.record","defs":{
      "nested":{"type":"object","properties":{}},
      "main":{"type":"record","key":"literal:test","record":{
        "type":"object","properties":{
          "z":{"type":"string"},
          "a":{"type":"array","items":{"type":"ref","ref":"#nested"}}
        }}}
    }}|}) in
  (match doc.defs with
   | [{ L.name = "main"; type_def = L.Record record }; { L.name = "nested"; _ }] ->
       if List.map fst record.record.properties <> ["a"; "z"] then
         failwith "property ordering changed"
   | _ -> failwith "recursive record cases or definition ordering changed");
  let encoded = get (Jsont_bytesrw.encode_string L.lexicon_doc_jsont doc) in
  let decoded = get (Jsont_bytesrw.decode_string L.lexicon_doc_jsont encoded) in
  if decoded <> doc then failwith "recursive lexicon codec round trip changed"
