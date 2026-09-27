let with_spool path f =
  let created = ref false in
  Fun.protect ~finally:(fun () ->
    if !created then Eio.Cancel.protect (fun () ->
      Eio.Path.unlink ~missing_ok:true path)) (fun () ->
    Eio.Path.with_open_out ~create:(`Exclusive 0o600) path (fun output ->
      created := true;
      f output))

let hash_file path =
  Eio.Path.with_open_in path (fun input ->
    let buffer = Cstruct.create 65536 in
    let rec loop length hash =
      match Eio.Flow.single_read input buffer with
      | count ->
          let next = Int64.add length (Int64.of_int count) in
          if next < length then invalid_arg "Spool.hash_file: length overflow";
          loop next (Digestif.SHA256.feed_string hash
            (Cstruct.to_string (Cstruct.sub buffer 0 count)))
      | exception End_of_file ->
          length, Digestif.SHA256.to_hex (Digestif.SHA256.get hash) in
    loop 0L Digestif.SHA256.empty)
