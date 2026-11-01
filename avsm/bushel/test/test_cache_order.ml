module D = Bushel.Doi_entry
module V = Bushel_sync.Peertube.VideoIndex

let check name b =
  if not b then failwith name

let with_file f =
  let path = Filename.temp_file "bushel-cache-order" ".yml" in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () -> f path)

let read path = In_channel.with_open_bin path In_channel.input_all

let () =
  let entry doi status = D.{
    doi; title = "A title"; authors = ["Second"; "First"]; year = 2026;
    bibtype = "article"; publisher = "Publisher"; resolved_at = "2026-10-22";
    source_urls = ["https://example.org/source"]; status; ignore = false;
  } in
  let a = entry "10.1234/a" D.Resolved in
  let b = { (entry "10.1234/b" (D.Failed "unavailable")) with
    title = ""; authors = []; year = 0; bibtype = ""; publisher = "";
    ignore = true } in
  let c = entry "10.1234/c" D.Resolved in
  let expected = D.to_yaml_string [a; b; c] in
  List.iter (fun entries ->
    check "DOI encoding is independent of input order"
      (D.to_yaml_string entries = expected);
    with_file (fun path ->
      D.save_file path (D.merge_entries [] entries);
      check "merged DOI cache is saved in DOI order" (read path = expected);
      check "DOI metadata survives" (D.load_file path = [a; b; c]))
  ) [[c; a; b]; [b; c; a]; [a; b; c]];
  let pairs = List.init 100 (fun i ->
    (Printf.sprintf "uuid-%03d" i, Printf.sprintf "server-%d" (i mod 3))) in
  let expected = "# UUID -> PeerTube server name mapping\n" ^
    String.concat "" (List.map (fun (uuid, server) -> uuid ^ ": " ^ server ^ "\n") pairs) in
  List.iter (fun random ->
    List.iter (fun entries ->
      let index = Hashtbl.create ~random 1 in
      List.iter (fun (uuid, server) -> V.add index ~uuid ~server) entries;
      with_file (fun path ->
        V.save_file path index;
        check "video cache is saved in UUID order" (read path = expected);
        let loaded = V.load_file path in
        List.iter (fun (uuid, server) ->
          check "video mapping survives" (V.find loaded uuid = Some server)) pairs;
        V.save_file path loaded;
        check "video rewrite is byte-identical" (read path = expected))
    ) [pairs; List.rev pairs]
  ) [false; true]
