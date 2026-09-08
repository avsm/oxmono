let () = List.iter (fun n ->
  let b = Buffer.create (n*24) in
  Buffer.add_string b "<x";
  for i = 1 to n do Printf.bprintf b " xmlns:n%d='urn:n%d'" i i done;
  Buffer.add_string b "/>";
  let source = Buffer.contents b in
  let start = Sys.time () in
  for _ = 1 to 3 do
    match Davz.parse_xml source with Ok _ -> () | Error e -> failwith e
  done;
  Printf.printf "%6d namespaces %8d bytes %.6fs CPU/parse\n%!"
    n (String.length source) ((Sys.time () -. start) /. 3.)
) [2000; 4000; 8000; 16000; 32000]
