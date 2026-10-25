let reject f =
  match f () with
  | _ -> failwith "expected invalid field"
  | exception Tomlt.Fields.Invalid message ->
      if String.contains message '\n' then failwith "multiline diagnostic"

let () =
  let open Tomlt in
  let value =
    Toml.table [ ("name", Toml.string " value "); ("number", Toml.int 1L) ]
  in
  Fields.known [ "name"; "number" ] value;
  assert (Fields.string_opt "absent" value = None);
  assert (Fields.required_string "name" value = " value ");
  reject (fun () -> Fields.known [ "name" ] value);
  reject (fun () -> Fields.known [] (Toml.string "private input"));
  reject (fun () -> Fields.string_opt "number" value);
  reject (fun () -> Fields.required_string "absent" value);
  reject (fun () -> Fields.nonempty "name" " \n");
  print_endline "Strict TOML fields reject unknown keys and wrong types"
