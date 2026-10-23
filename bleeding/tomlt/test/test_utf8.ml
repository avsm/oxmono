let check_valid body =
  List.iter
    (fun source ->
      match Tomlt_bytesrw.of_string source with
      | Ok _ -> ()
      | Error e -> failwith (Tomlt.Toml.Error.to_string e))
    ["#" ^ body ^ "\n"; "x = \"" ^ body ^ "\"\n"]

let check_invalid body =
  List.iter
    (fun source ->
      match Tomlt_bytesrw.of_string source with
      | Error _ -> ()
      | Ok _ -> failwith (Printf.sprintf "accepted malformed UTF-8 %S" source))
    ["#" ^ body; "x = \"" ^ body ^ "\""]

let words f =
  ignore (Sys.opaque_identity (f ()));
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  for _ = 1 to 10 do ignore (Sys.opaque_identity (f ())) done;
  (Gc.allocated_bytes () -. before) /. 80.

let () =
  List.iter check_valid
    ["ASCII"; "\194\128"; "\223\191"; "\224\160\128";
     "\237\159\191"; "\238\128\128"; "\239\191\191";
     "\240\144\128\128"; "\244\143\191\191"; "é日本語😀"];
  List.iter check_invalid
    ["\192\000xx"; "\192x"; "\128"; "\191"; "\192\128";
     "\193\191"; "\194";
     "\194x"; "\224\128\128"; "\224\160"; "\237\160\128";
     "\240\128\128\128"; "\244\144\128\128"; "\245\128\128\128";
     "\255"; "\226x\128"; "\240\144x\128"];
  List.iter (fun (source, expected) ->
    match Tomlt_bytesrw.of_string source with
    | Error e when Tomlt.Toml.Error.to_string e = expected -> ()
    | Error e -> failwith (Tomlt.Toml.Error.to_string e)
    | Ok _ -> failwith "accepted malformed UTF-8")
    ["#\128", "-:1:2: invalid UTF-8 sequence";
     "#\194", "-:1:2: incomplete UTF-8 sequence";
     "#ok\n#\240\144", "-:2:2: incomplete UTF-8 sequence";
     "#\192\000xx", "-:1:2: invalid UTF-8 sequence"];
  let ascii = String.make 10000 'x' in
  let unicode = String.concat "" (List.init 5000 (fun _ -> "é")) in
  List.iter
    (fun wrap ->
      let a = wrap ascii and u = wrap unicode in
      let aw = words (fun () -> Tomlt_bytesrw.parse a) in
      let uw = words (fun () -> Tomlt_bytesrw.parse u) in
      if uw > aw +. 128. then
        failwith (Printf.sprintf "UTF-8 allocates %.0f extra words" (uw -. aw)))
    [(fun s -> "#" ^ s ^ "\n"); (fun s -> "x = \"" ^ s ^ "\"\n")]
