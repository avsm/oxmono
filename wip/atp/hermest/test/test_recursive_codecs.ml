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

let () =
  let doc = get (Jsont_bytesrw.decode_string L.lexicon_doc_jsont
    {|{"lexicon":1,"id":"com.example.bytes","defs":{"main":{"type":"object","required":["data"],"properties":{"data":{"type":"bytes"}}}}}|}) in
  let contains needle text =
    let rec loop i = i + String.length needle <= String.length text &&
      (String.sub text i (String.length needle) = needle || loop (i + 1)) in
    loop 0 in
  List.iter (fun code ->
    if not (contains "Atp.Lex.bytes_jsont" code) || contains "Jsont.binary_string" code then
      failwith "generator emitted non-AT Protocol bytes codec")
    [Hermest.Codegen_jsont.gen_lexicon_module doc;
     Hermest.Codegen_jsont.gen_unified_module ~module_name:"Fixture" [doc]]
