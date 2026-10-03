(* Release artifacts are records in the author's repository that point at a
   repository record. The responses are recorded from a real data server and
   the mock backend serves them, so nothing here touches the network. *)

module L = Atp_lexicon_tangled.Sh.Tangled

let check name p = if not p then failwith name
let read p = In_channel.with_open_bin ("fixtures/" ^ p) In_channel.input_all
let did = "did:plc:nhyitepp3u4u6fcfboegzcjw"

let contains ~sub s =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

let after ~sub s =
  let n = String.length sub in
  let rec go i =
    if i + n > String.length s then None
    else if String.sub s i n = sub then
      Some (String.sub s (i + n) (String.length s - i - n))
    else go (i + 1)
  in
  go 0

let json body req =
  Fetch_mock.respond
    ~headers:(Http.Header.of_list [ ("Content-Type", "application/json") ])
    body req

(* Answers listRecords with the recorded artifacts and an empty second page,
   getRecord with the recorded repository record, and counts the latter. *)
let data_server gets =
  Fetch_mock.client (fun req ->
      let url = Fetch.Middleware.Url.to_string req.Fetch.Middleware.url in
      if contains ~sub:"com.atproto.repo.listRecords" url then
        json
          (if contains ~sub:"cursor=" url then {|{"records":[]}|}
           else read "artifacts.json")
          req
      else
        match after ~sub:"rkey=" url with
        | Some rkey ->
            incr gets;
            json (read ("repo_" ^ rkey ^ ".json")) req
        | None -> Fetch_mock.respond ~status:404 "" req)

let names artifacts =
  List.map (fun (_, (a : L.Repo.Artifact.main)) -> a.name) artifacts

let () =
  let v = Tangled.Api.artifact_version in
  check "package and version" (v "dune-rpc-eio-0.1.0.tbz" = Some "0.1.0");
  check "a short version" (v "json-pointer-1.0.tbz" = Some "1.0");
  check "a package named differently from its repository"
    (v "mlgpx-1.0.0.tbz" = Some "1.0.0");
  check "a version with a hash"
    (v "oxcaml-5.2.0minus31-3416edee6.tar.gz" = Some "5.2.0minus31-3416edee6");
  check "no version" (v "readme.tbz" = None);
  check "not an archive" (v "x-1.0.exe" = None);
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let gets = ref 0 in
  let api =
    Tangled.Api.create ~sw ~env ~app_name:"tangled-test"
      ~pds:"https://pds.example" ~http:(data_server gets) ()
  in
  let artifacts repo = Tangled.Api.list_artifacts api ~did ~repo in
  check "a repository named by its record"
    (names (artifacts "ocaml-json-pointer") = [ "json-pointer-1.0.tbz" ]);
  check "a repository named by its rkey"
    (names (artifacts "dune-rpc-eio") = [ "dune-rpc-eio-0.1.0.tbz" ]);
  check "a repository with an opaque rkey"
    (names (artifacts "xdge") = [ "xdge-1.1.0.tbz"; "xdge-1.0.0.tbz" ]);
  check "a repository with several artifacts"
    (names (artifacts "ocaml-jsonfeed")
    = [ "jsonfeed-1.1.0.tbz"; "jsonfeed-1.0.0.tbz" ]);
  check "an unknown repository has none" (artifacts "nothing" = []);
  check "each repository record is read once per call" (!gets = 5 * 9);
  print_endline "ok"
