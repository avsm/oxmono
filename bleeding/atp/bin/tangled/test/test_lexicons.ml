module L = Atp_lexicon_tangled.Sh.Tangled

let get = function Ok v -> v | Error e -> failwith e
let decode codec s = get (Jsont_bytesrw.decode_string codec s)
let check name condition = if not condition then failwith name

let rejects f =
  match f () with
  | _ -> failwith "Invalid input accepted"
  | exception Invalid_argument _ -> ()
  | exception Failure _ -> ()

let () =
  check "complete catalogue" (List.length Tangled.Schema.documents = 231);
  List.iter
    (fun (nsid, _) -> ignore (Tangled.Schema.kind nsid))
    Tangled.Schema.documents;
  let languages =
    decode L.Repo.Languages.output_jsont {|{"ref":"HEAD","languages":null}|}
  in
  check "required nullable decoded" (languages.languages = None);
  let encoded =
    get (Jsont_bytesrw.encode_string L.Repo.Languages.output_jsont languages)
  in
  check "required null re-encoded" (String.contains encoded 'n');
  rejects (fun () -> decode L.Repo.Languages.output_jsont {|{"ref":"HEAD"}|});
  ignore
    (decode L.Ci.SubscribePipelineLogs.data_jsont
       {|{"time":"2026-01-01T00:00:00Z","workflow":"inspect","step":1,"content":"partial","stream":"stdout"}|});
  ignore (decode L.Knot.SubscribeRepos.params_jsont "{}");
  let put = Atp_lexicon_atproto.Com.Atproto.Repo.PutRecord.input_jsont in
  let absent =
    decode put
      {|{"repo":"did:plc:alice","collection":"sh.tangled.repo","rkey":"demo","record":{}}|}
  in
  let null =
    decode put
      {|{"repo":"did:plc:alice","collection":"sh.tangled.repo","rkey":"demo","record":{},"swapRecord":null}|}
  in
  check "absent and explicit null remain distinct"
    (absent.swap_record = None && null.swap_record = Some None);
  Tangled.Schema.params "sh.tangled.ci.queryPipelines"
    [
      ("repo", "did:plc:repo");
      ("limit", "250");
      ("kinds", "push");
      ("kinds", "manual");
    ];
  rejects (fun () ->
      Tangled.Schema.params "sh.tangled.ci.queryPipelines"
        [ ("repo", "did:plc:repo"); ("limit", "251") ]);
  rejects (fun () ->
      Tangled.Schema.params "sh.tangled.ci.queryPipelines"
        [ ("repo", "not-a-did") ]);
  rejects (fun () ->
      Tangled.Schema.params "sh.tangled.ci.queryPipelines"
        [ ("repo", "did:plc:repo"); ("kinds", "typo") ]);
  rejects (fun () ->
      Tangled.Schema.params "sh.tangled.ci.queryPipelines"
        [ ("repo", "did:plc:repo"); ("limit", "1"); ("limit", "2") ]);
  rejects (fun () -> Tangled.Schema.params "sh.tangled.ci.getPipeline" []);
  check "service audience with port"
    (Tangled.Api.service_did "http://localhost:9000/"
    = "did:web:localhost%3A9000");
  List.iter
    (fun origin -> rejects (fun () -> Tangled.Api.service_url origin))
    [
      "https://user:password@example.com";
      "https://example.com/path";
      "https://example.com?query";
    ];
  check "record URI validates"
    (Tangled.Types.parse_at_uri "at://did:plc:alice/sh.tangled.repo/name"
    <> None);
  check "partial URI rejected"
    (Tangled.Types.parse_at_uri "at://did:plc:alice" = None);
  print_endline
    "PASS: complete lexicon catalogue, subscriptions, nullability and input \
     validation"
