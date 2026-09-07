(* The shipped specification must accept what hoard.recoil.org sends. A
   bookmark whose tagging or summarisation has not run yet carries null in
   both status fields, which an enum lacking null rejects even under
   [nullable: true]. *)

let read path =
  In_channel.with_open_bin path In_channel.input_all

let member name = function
  | Jsont.Object (ms, _) -> (
    match Jsont.Json.find_mem name ms with
    | Some (_, v) -> v
    | None -> failwith ("no member " ^ name))
  | _ -> failwith ("not an object at " ^ name)

let () =
  let decode = Openapi.Runtime.Json.decode in
  let encode = Openapi.Runtime.Json.encode in
  let spec =
    Result.get_ok (decode Jsont.json (read "karakeep-openapi-spec.json"))
  in
  let schemas = member "schemas" (member "components" spec) in
  let definitions = Result.get_ok (encode Jsont.json schemas) in
  let context = Openapi.Schema.of_string ~version:"3.0.0" definitions in
  let guard =
    Openapi.Schema.guard_string context
      {|{"$ref":"#/components/schemas/Bookmark"}|} Jsont.json
  in
  let bookmark status =
    String.concat ""
      [ {|{"id":"b1","createdAt":"2026-08-18T08:44:27.000Z",|};
        {|"firstCreatedAt":"2026-08-18T08:44:27.000Z","modifiedAt":null,|};
        {|"archived":false,"favourited":false,"taggingStatus":|}; status;
        {|,"summarizationStatus":|}; status;
        {|,"embeddingStatus":"pending","source":"api","userId":"u1",|};
        {|"tags":[],"content":{"type":"link","url":"https://example.com"},|};
        {|"assets":[]}|} ]
  in
  let accepts status =
    match decode guard (bookmark status) with
    | Ok _ -> ()
    | Error e ->
      prerr_endline ("FAIL: status " ^ status ^ ": " ^ e);
      exit 1
  in
  accepts {|"success"|};
  accepts "null";
  print_endline "test_spec: bookmark with null statuses accepted"
