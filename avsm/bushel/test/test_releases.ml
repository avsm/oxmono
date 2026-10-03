module R = Bushel_sync.Releases

let check name b =
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let cand =
  {
    Bushel_sync.Forge.repo = "realworldocaml/mdx";
    forge = Bushel.Release.Github;
    tag = "2.6.0";
    version = "2.6.0";
    date = (2026, 7, 22);
    title = Some "2.6.0";
    url = "https://github.com/realworldocaml/mdx/releases/tag/2.6.0";
    author = Some "avsm";
    prerelease = false;
  }

let read p = In_channel.with_open_bin ("fixtures/" ^ p) In_channel.input_all

let contains ~sub s =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

let ok = function
  | Ok v -> v
  | Error e ->
    prerr_endline e;
    exit 1

(* A GitHub whose answers come from [respond], recording each URL asked for. *)
let github respond =
  let seen = ref [] in
  let http =
    Fetch_mock.client (fun req ->
        let url = Fetch.Middleware.Url.to_string req.Fetch.Middleware.url in
        seen := url :: !seen;
        Fetch_mock.respond (respond url) req)
  in
  (http, seen)

let () =
  Eio_mock.Backend.run @@ fun () ->
  (* A tag is one path segment, so its separators are encoded. *)
  let http, seen = github (fun _ -> read "github_release_tag.json") in
  ignore
    (ok (R.github_release ~http ~token:None ~repo:"o/r" ~tag:"feature/x#1"));
  check "the tag is encoded in the url"
    (List.exists (contains ~sub:"releases/tags/feature%2Fx%231") !seen);

  (* Every page of releases is read, and no more than there are. *)
  let one_more = "[" ^ read "github_release_tag.json" ^ "]" in
  let pages url =
    if contains ~sub:"&page=1" url then read "github_releases.json"
    else if contains ~sub:"&page=2" url then one_more
    else "[]"
  in
  let http, seen = github pages in
  let all = ok (R.github_releases ~http ~token:None ~repo:"o/r") in
  check "releases from every page" (List.length all = 5);
  check "it stops at the first empty page" (List.length !seen = 3);
  check "pages are numbered from one"
    (List.exists (contains ~sub:"&page=3") !seen);

  (* A server that ignores the page number does not loop. *)
  let http, seen = github (fun _ -> read "github_releases.json") in
  let all = ok (R.github_releases ~http ~token:None ~repo:"o/r") in
  check "a repeated page adds nothing" (List.length all = 4);
  check "a repeated page ends the walk" (List.length !seen = 2);
  check "an error is returned, not raised"
    (let http, _ = github (fun _ -> "{") in
     match R.github_releases ~http ~token:None ~repo:"o/r" with
     | Error _ -> true
     | Ok _ -> false)

let () =
  let reg =
    {
      Bushel.Release.name = "opam.ocaml.org";
      package = "mdx";
      url = "https://opam.ocaml.org/packages/mdx/mdx.2.6.0/";
    }
  in
  let r =
    R.build cand ~registries:[ reg ]
      ~description:(Some "Executable code blocks inside markdown files. Extra.")
      ~summary:None
  in
  check "summary from the description"
    (r.Bushel.Release.summary
    = "Executable code blocks inside markdown files.");
  check "date from the forge" (r.Bushel.Release.date = (2026, 7, 22));
  check "url from the forge"
    (r.Bushel.Release.url = cand.Bushel_sync.Forge.url);
  check "no tag when it equals the version" (r.Bushel.Release.tag = None);
  check "registries attached" (r.Bushel.Release.registries = [ reg ]);
  let r =
    R.build cand ~registries:[] ~description:(Some "d.") ~summary:(Some "Mine")
  in
  check "an explicit summary wins" (r.Bushel.Release.summary = "Mine");
  let r = R.build cand ~registries:[] ~description:None ~summary:None in
  check "a title that is only the version is not a summary"
    (r.Bushel.Release.summary = "mdx 2.6.0");
  let r =
    R.build
      { cand with Bushel_sync.Forge.title = Some "Faster parsing" }
      ~registries:[] ~description:None ~summary:None
  in
  check "a real title is the next fallback"
    (r.Bushel.Release.summary = "Faster parsing");
  let r =
    R.build
      { cand with Bushel_sync.Forge.title = None }
      ~registries:[] ~description:None ~summary:None
  in
  check "the package and version are the last fallback"
    (r.Bushel.Release.summary = "mdx 2.6.0");
  let r =
    R.build
      { cand with Bushel_sync.Forge.tag = "v2.6.0" }
      ~registries:[] ~description:None ~summary:None
  in
  check "a tag that differs from the version is kept"
    (r.Bushel.Release.tag = Some "v2.6.0");
  let r =
    R.build cand ~registries:[] ~description:(Some "  ") ~summary:(Some "")
  in
  check "blank text falls through"
    (r.Bushel.Release.summary = "mdx 2.6.0");
  check "no token when the variable is unset" (R.token_of_env None = None);
  check "no token when the variable is empty" (R.token_of_env (Some "") = None);
  check "a token is read from the variable"
    (R.token_of_env (Some "abc") = Some "abc");
  (* Registering a release again keeps what the author already had. *)
  let stored =
    {
      (R.build cand ~registries:[ reg ] ~description:None
         ~summary:(Some "Mine"))
      with
      Bushel.Release.date = (2026, 7, 21);
    }
  in
  let fresh = R.build cand ~registries:[] ~description:None ~summary:None in
  let again = R.reconcile ~existing:(Some stored) ~summary_given:false fresh in
  check "a hand-written summary survives registering again"
    (again.Bushel.Release.summary = "Mine");
  check "registries survive a lookup that found none"
    (again.Bushel.Release.registries = [ reg ]);
  check "the forge's date and url are refreshed"
    (again.Bushel.Release.date = (2026, 7, 22));
  let given =
    R.build cand ~registries:[] ~description:None ~summary:(Some "New")
  in
  check "an explicit summary replaces the old one"
    ((R.reconcile ~existing:(Some stored) ~summary_given:true given)
       .Bushel.Release.summary
    = "New");
  let more =
    {
      Bushel.Release.name = "pypi.org";
      package = "mdx";
      url = "https://pypi.org/p";
    }
  in
  let widened =
    R.reconcile ~existing:(Some stored) ~summary_given:false
      { fresh with Bushel.Release.registries = [ more ] }
  in
  check "a new registry is added and the old one kept"
    (List.map (fun r -> r.Bushel.Release.name) widened.Bushel.Release.registries
    = [ "opam.ocaml.org"; "pypi.org" ]);
  check "a first registration is unchanged"
    (R.reconcile ~existing:None ~summary_given:false fresh = fresh);

  (* Only your own releases are registered without --force. *)
  let by author = { cand with Bushel_sync.Forge.author } in
  let refusal ?(user = Some "avsm") ?(force = false) author =
    R.refusal ~github_user:user ~force (by author)
  in
  check "your own release is accepted" (refusal (Some "avsm") = None);
  check "the comparison ignores case"
    (refusal ~user:(Some "AVSM") (Some "avsm") = None);
  (match refusal (Some "Julow") with
  | Some msg -> check "the refusal names the author" (String.contains msg 'J')
  | None -> check "someone else's release is refused" false);
  check "--force accepts it" (refusal ~force:true (Some "Julow") = None);
  (match R.refusal ~github_user:None ~force:false (by (Some "avsm")) with
  | Some msg ->
    check "an unset github_user is reported"
      (String.length msg > 0 && String.contains msg '_')
  | None -> check "an unset github_user is refused" false);
  check "a release with no author is accepted"
    (R.refusal ~github_user:None ~force:false (by None) = None);

  (* refresh only adds registries, inside the window. *)
  let release_of version date registries =
    {
      Bushel.Release.version;
      tag = None;
      date;
      summary = "Kept";
      url = "u";
      registries;
    }
  in
  let repo =
    { Bushel.Release.repo = "realworldocaml/mdx"; forge = Bushel.Release.Github;
      project = None;
      releases =
        [ release_of "2.7.0" (2026, 10, 2) [];
          release_of "2.6.0" (2026, 7, 22) [ reg ];
          release_of "2.0.0" (2025, 1, 1) [] ] }
  in
  let calls = ref [] in
  let lookup _ (r : Bushel.Release.release) =
    calls := r.Bushel.Release.version :: !calls;
    if r.Bushel.Release.version = "2.7.0" then Ok [ reg ] else Ok []
  in
  let updated, attached, failed =
    R.refresh ~cutoff:(2026, 6, 1) ~lookup [ repo ]
  in
  check "no lookup outside the window" (not (List.mem "2.0.0" !calls));
  check "a registry that appeared is attached"
    (attached = [ ("realworldocaml/mdx", "2.7.0", "opam.ocaml.org") ]);
  check "nothing failed" (failed = []);
  let rs = (List.hd updated).Bushel.Release.releases in
  let find v = List.find (fun r -> r.Bushel.Release.version = v) rs in
  check "the new registry is stored"
    ((find "2.7.0").Bushel.Release.registries = [ reg ]);
  check "a registry is never removed"
    ((find "2.6.0").Bushel.Release.registries = [ reg ]);
  check "summaries and dates are untouched"
    (List.for_all (fun r -> r.Bushel.Release.summary = "Kept") rs
    && (find "2.7.0").Bushel.Release.date = (2026, 10, 2));
  let again, attached, _ = R.refresh ~cutoff:(2026, 6, 1) ~lookup updated in
  check "refreshing again attaches nothing" (attached = [] && again = updated);
  let down _ _ = Error "ecosyste.ms is down" in
  let kept, attached, failed =
    R.refresh ~cutoff:(2026, 6, 1) ~lookup:down [ repo ]
  in
  check "a failed lookup changes nothing" (kept = [ repo ] && attached = []);
  check "a failed lookup is reported"
    (List.map (fun (r, v, _) -> (r, v)) failed
    = [ ("realworldocaml/mdx", "2.7.0"); ("realworldocaml/mdx", "2.6.0") ]);
  print_endline "ok"
