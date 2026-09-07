let feed = {|<feed xmlns="http://www.w3.org/2005/Atom">
<id>urn:test:feed</id><title>Test feed</title>
<entry><id>urn:test:entry</id><title>Test entry</title>
<updated>2026-09-07T00:00:00Z</updated></entry>
</feed>|}

let input () = Xmlm.make_input (`String (0, feed))

let () =
  (match Syndic.Atom.parse (input ()) with
   | _ -> failwith "missing feed updated date should require relaxed parsing"
   | exception Syndic.Atom.Error.Error _ -> ());
  let fallback = Syndic.Date.of_rfc3339 "2026-09-06T00:00:00Z" in
  let parsed = Syndic.Atom.parse
      ~relaxed:[Syndic.Atom.MissingFeedUpdatedTag fallback] (input ()) in
  assert (Ptime.equal parsed.updated fallback);
  assert (List.length parsed.entries = 1);
  (* The monorepo continues to tolerate entries without an author. *)
  let entry = List.hd parsed.entries in
  assert (Ptime.equal entry.updated
            (Syndic.Date.of_rfc3339 "2026-09-07T00:00:00Z"));
  print_endline "Syndic relaxed parsing and local author fallback passed"
