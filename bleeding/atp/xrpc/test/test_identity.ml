(* Identity resolution reads DID documents and handle records. The documents
   are recorded from plc.directory and the requests are answered by the mock
   backend, so nothing here touches the network. *)

let check name p = if not p then failwith name
let read p = In_channel.with_open_bin ("fixtures/" ^ p) In_channel.input_all
let pds = "https://amanita.us-east.host.bsky.network"
let did = "did:plc:nhyitepp3u4u6fcfboegzcjw"
let well_known h = Printf.sprintf "https://%s/.well-known/atproto-did" h

(* A client that answers each URL of [routes] with its body and everything else
   with a 404, recording every URL asked for. *)
let serving routes seen =
  Fetch_mock.client (fun req ->
      let url = Fetch.Middleware.Url.to_string req.Fetch.Middleware.url in
      seen := url :: !seen;
      match List.assoc_opt url routes with
      | Some body -> Fetch_mock.respond body req
      | None -> Fetch_mock.respond ~status:404 "" req)

let is_error = function Error _ -> true | Ok _ -> false

let () =
  Eio_mock.Backend.run @@ fun () ->
  let module I = Xrpc.Identity in
  check "pds of a recorded document"
    (I.pds_of_document (read "plc_did.json") = Some pds);
  check "no service, no pds" (I.pds_of_document {|{"id":"did:plc:a"}|} = None);
  check "another service type is not a pds"
    (I.pds_of_document
       {|{"service":[{"type":"Other","serviceEndpoint":"https://x.example"}]}|}
    = None);
  check "a malformed document has no pds" (I.pds_of_document "{" = None);

  check "plc document url"
    (I.document_url "did:plc:abc" = Ok "https://plc.directory/did:plc:abc");
  check "web document url"
    (I.document_url "did:web:example.com"
    = Ok "https://example.com/.well-known/did.json");
  check "an unsupported method" (is_error (I.document_url "did:key:zQ3"));
  check "not a did" (is_error (I.document_url "alice"));

  let seen = ref [] in
  let http =
    serving [ (well_known "alice.example", "did:plc:abc123\n") ] seen
  in
  check "handle to did"
    (I.did_of_handle http "alice.example" = Ok "did:plc:abc123");
  check "the well-known url was asked for"
    (!seen = [ well_known "alice.example" ]);
  seen := [];
  check "an invalid handle makes no request"
    (is_error (I.did_of_handle http "not a handle") && !seen = []);
  check "an unknown handle is an error"
    (is_error (I.did_of_handle http "bob.example"));
  let bad = serving [ (well_known "alice.example", "garbage") ] (ref []) in
  check "a body that is not a did is an error"
    (is_error (I.did_of_handle bad "alice.example"));

  let http =
    serving
      [
        ("https://plc.directory/" ^ did, read "plc_did.json");
        (well_known "alice.example", did);
      ]
      (ref [])
  in
  check "pds of a did" (I.pds_of_did http did = Ok pds);
  check "pds of an unknown did"
    (is_error (I.pds_of_did http "did:plc:unknown"));
  check "pds of a handle" (I.pds_of_handle http "alice.example" = Ok pds);
  print_endline "ok"
