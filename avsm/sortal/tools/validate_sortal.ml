(* Validate imported YAML using the actual Sortal V2 schema. *)
let () =
  if Array.length Sys.argv <> 2 then failwith "usage: validate_sortal CONTACT.yaml";
  let data = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  let module C = Sortal_schema.Contact in
  match Yamlt.decode C.json_t (Bytesrw.Bytes.Reader.of_string data) with
  | Error error -> failwith error
  | Ok contact ->
      Printf.printf "Native Sortal schema: %s, %d email(s), %d vCard passthrough field(s).\n"
        (C.handle contact) (List.length (C.emails contact))
        (List.length (C.vcard contact))
