open Bytesrw

let reference_string =
  Jsont.Base.string
    (Jsont.Base.map ~kind:"ignore"
       ~dec:(fun _ value ->
         ignore (Sys.opaque_identity (String.length value))) ())

let reference_ignore =
  Jsont.any ~kind:"ignore" ~dec_string:reference_string
    ~enc:(fun () -> reference_string) ()

let decode ~layout ~locs ~chunk codec source =
  Jsont_bytesrw.decode ~layout ~locs ~file:"skip.json" codec
    (Bytes.Reader.of_string ~slice_length:chunk source)

let compare codec reference source =
  List.iter (fun layout -> List.iter (fun locs ->
    List.iter (fun chunk ->
      let actual = decode ~layout ~locs ~chunk codec source in
      let expected = decode ~layout ~locs ~chunk reference source in
      if actual <> expected then
        failwith (Printf.sprintf "skip differs for %S (chunk %d)" source chunk))
      [1; 2; 7; 4096]) [false; true]) [false; true]

let allocated source =
  let run () =
    match Jsont_bytesrw.decode_string Jsont.ignore source with
    | Ok () -> () | Error e -> failwith e
  in
  run ();
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  for _ = 1 to 10 do run () done;
  (Gc.allocated_bytes () -. before) /. 80.

let () =
  let cases =
    ["\"\""; " \n\t\"abcé日本語😀\" \r\n";
     {|"\"\\\/\b\f\n\r\t\u0041\u0000\ud834\udd1e"|};
     {|"\ud800"|}; {|"\udc00"|}; {|"\ud800\u0041"|};
     {|"\u0x00"|}; {|"\q"|}; "\"unterminated";
     "\"line\nbreak\""; "\"\194\128\""; "\"\194x\"";
     "\"\237\160\128\""; "\"\244\144\128\128\"";
     "\"\240\144"; "\"ok\" trailing"]
  in
  List.iter (fun source ->
    compare Jsont.ignore reference_ignore source;
    compare (Jsont.list Jsont.ignore) (Jsont.list reference_ignore)
      ("[\"before\", " ^ source ^ ", \"after\"]")) cases;
  let projection =
    Jsont.Object.map Fun.id
    |> Jsont.Object.mem "keep" Jsont.string ~enc:Fun.id
    |> Jsont.Object.finish
  in
  let source =
    {|{"discard":{"nested":["é",null,1e9999,true,"\ud834\udd1e"]},
       "keep":"after"}|}
  in
  List.iter (fun layout -> List.iter (fun locs -> List.iter (fun chunk ->
    match decode ~layout ~locs ~chunk projection source with
    | Ok "after" -> ()
    | Ok _ -> failwith "lost retained member after skipped value"
    | Error e -> failwith e) [1; 2; 7; 4096]) [false; true]) [false; true];
  let preserves_value =
    Jsont.Base.string
      (Jsont.Base.map ~kind:"ignore" ~dec:(fun meta s -> meta, s) ())
  in
  (match Jsont_bytesrw.decode_string ~layout:true ~locs:true
           preserves_value " \"content\"\n" with
  | Ok (meta, "content") ->
      if Jsont.Meta.ws_before meta <> " " || Jsont.Meta.ws_after meta <> "\n"
      then failwith "custom decoder lost layout"
  | _ -> failwith "custom decoder lost value");
  let small = allocated "\"\"" in
  let large = allocated ("\"" ^ String.make 100000 'x' ^ "\"") in
  if large > small +. 256. then
    failwith (Printf.sprintf "discarded string allocates %.0f extra words"
                (large -. small))
