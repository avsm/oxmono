(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* The link predicate differential.

   This is a tool, not a test. It is an [executable] rather than a [test]
   stanza because it needs a corpus of real URLs that is not in this
   repository, so [dune runtest] must not reach it. Run it by hand:

     grep "^- url: " ~/bushel/data/links.yml \
       | sed "s/^- url: //; s/^'//; s/'$//" > /tmp/urls.txt
     dune exec avsm/arod/test/link_predicate_diff.exe /tmp/urls.txt

   {!Bushel.Link.is_academic_url} and {!Bushel.Link.is_doi_url} were moved off
   the opam [Uri] and off [Astring] so that {!Bushel.Link.is_paper_url}, which
   runs at render time on the links listing, could be portable. [Uriz] refuses
   a string that is not an RFC 3986 URI-reference where [Uri] coerced one, so
   the move could have reclassified a URL rather than merely reimplemented the
   test. This holds both spellings side by side and reports every URL they
   disagree on.

   What it proved, over the 4659 URLs in [links.yml] as of August 2026:

     urls=4659 academic_flips=0 doi_flips=0 paper_flips=0
     old: academic=196 doi=98 paper=294
     new: academic=196 doi=98 paper=294

   Zero flips. Feeding it a list of deliberately malformed URLs does produce
   flips, always in one direction and always in one class: a URL carrying a
   byte that a URI-reference may not carry, such as a space, a non-ASCII byte
   or one of the brace, bar, caret, backslash, quote and angle bracket set,
   was coerced to [true] by the old spelling and is [false] under the new one.
   That is the intended answer for a predicate asking whether a stored URL
   names a paper, and no URL in the corpus takes the branch.

   The old side is copied here rather than referenced, because the code it
   describes no longer exists. Only its Uri and Astring calls are the point;
   [academic_patterns] below has to be a verbatim copy of the one in
   [bushel_link.ml], because the new side calls the real function and reads
   the real table. Check that first: a transcribed-from-memory table produced 23
   flips on the corpus, every one of them a host the two tables disagreed
   about and none of them a difference in what this tool exists to measure. So
   if the flip count is not zero, check that the two tables still match before
   concluding anything about Uriz. *)

let academic_patterns =
  [
    ("arxiv.org", ["/abs/"; "/pdf/"]);
    ("dl.acm.org", ["/doi/10."]);
    ("linkinghub.elsevier.com", []);
    ("sciencedirect.com", ["/science/article"]);
    ("ieeexplore.ieee.org", []);
    ("academic.oup.com", []);
    ("nature.com", ["/articles/"]);
    ("journals.sagepub.com", []);
    ("garfield.library.upenn.edu", []);
    ("link.springer.com", []);
    ("tandfonline.com", ["/doi/"]);
    ("cambridge.org", ["/core/journals/"]);
    ("science.org", ["/doi/"]);
    ("royalsocietypublishing.org", []);
    ("pnas.org", ["/doi/"]);
    ("onlinelibrary.wiley.com", ["/doi/"]);
    ("zenodo.org", ["/record"; "/records"]);
    ("frontiersin.org", ["/articles/"]);
    ("biorxiv.org", ["/content/"]);
    ("medrxiv.org", ["/content/"]);
    ("journals.plos.org", ["/plosone/article"]);
    ("cell.com", []);
    ("elifesciences.org", ["/articles/"]);
    ("peerj.com", ["/articles/"]);
    ("mdpi.com", []);
  ]

(* The spelling that shipped before this change, on opam [Uri] and [Astring]. *)

let old_is_academic_url url =
  let uri = Uri.of_string url in
  match Uri.host uri with
  | None -> false
  | Some host ->
    let host =
      match Astring.String.cut ~sep:"www." host with
      | Some ("", rest) -> rest
      | _ -> host
    in
    let path = Uri.path uri in
    List.exists
      (fun (domain, prefixes) ->
        let domain_match =
          host = domain || Astring.String.is_suffix ~affix:("." ^ domain) host
        in
        domain_match
        && (prefixes = []
           || List.exists
                (fun prefix -> Astring.String.is_prefix ~affix:prefix path)
                prefixes))
      academic_patterns

let old_is_doi_url url = Astring.String.is_infix ~affix:"doi.org/" url
let old_is_paper_url url = old_is_doi_url url || old_is_academic_url url

(* The spelling that ships now. These call the real functions rather than a
   copy of them, so the tool cannot drift away from what it is checking. *)

let new_is_academic_url = Bushel.Link.is_academic_url
let new_is_doi_url = Bushel.Link.is_doi_url
let new_is_paper_url = Bushel.Link.is_paper_url

let () =
  if Array.length Sys.argv <> 2 then (
    prerr_endline "usage: link_predicate_diff.exe URL-FILE";
    exit 2);
  let urls =
    let ic = open_in Sys.argv.(1) in
    let acc = ref [] in
    (try
       while true do
         match String.trim (input_line ic) with
         | "" -> ()
         | u -> acc := u :: !acc
       done
     with End_of_file -> ());
    close_in ic;
    List.rev !acc
  in
  let n = ref 0 and fa = ref 0 and fd = ref 0 and fp = ref 0 in
  List.iter
    (fun u ->
      incr n;
      let oa = old_is_academic_url u and na = new_is_academic_url u in
      let od = old_is_doi_url u and nd = new_is_doi_url u in
      let op = old_is_paper_url u and np = new_is_paper_url u in
      if oa <> na then (
        incr fa;
        Printf.printf "ACADEMIC old=%b new=%b uriz=%s %s\n" oa na
          (match Uriz.of_string u with
          | Null -> "Null"
          | This x ->
            Printf.sprintf "host=%s path=%s"
              (match Uriz.host x with Null -> "<null>" | This h -> h)
              (Uriz.path x))
          u);
      if od <> nd then (
        incr fd;
        Printf.printf "DOI old=%b new=%b %s\n" od nd u);
      if op <> np then (
        incr fp;
        Printf.printf "PAPER old=%b new=%b %s\n" op np u))
    urls;
  Printf.printf "urls=%d academic_flips=%d doi_flips=%d paper_flips=%d\n" !n !fa
    !fd !fp;
  let count f = List.length (List.filter f urls) in
  Printf.printf "old: academic=%d doi=%d paper=%d\n" (count old_is_academic_url)
    (count old_is_doi_url) (count old_is_paper_url);
  Printf.printf "new: academic=%d doi=%d paper=%d\n" (count new_is_academic_url)
    (count new_is_doi_url) (count new_is_paper_url);
  if !fa + !fd + !fp > 0 then exit 1
