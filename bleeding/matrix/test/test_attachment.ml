(** Byte-level interoperability and streaming tests for encrypted attachments.
*)

module A = Matrix_client.Encrypted_attachment
module C = Matrix_client.Client
module M = Matrix_client.Media
module R = Matrix_client.Random

let fixture_json =
  {|{"v":"v2","key":{"kty":"oct","alg":"A256CTR","ext":true,"k":"Voq2nkPme_x8no5-Tjq_laDAdxE6iDbxnlQXxwFPgE4","key_ops":["decrypt","encrypt"]},"iv":"i0DovxYdJEcAAAAAAAAAAA","hashes":{"sha256":"ANdt819a8bZl4jKy3Z+jcqtiNICa2y0AW4BBJ/iQRAU"}}|}

let fixture_ciphertext =
  String.init 26 (function
    | 0 -> Char.chr 179
    | 1 -> Char.chr 154
    | 2 -> Char.chr 118
    | 3 -> Char.chr 127
    | 4 -> Char.chr 186
    | 5 -> Char.chr 127
    | 6 -> Char.chr 110
    | 7 -> Char.chr 33
    | 8 -> Char.chr 203
    | 9 -> Char.chr 33
    | 10 -> Char.chr 33
    | 11 -> Char.chr 134
    | 12 -> Char.chr 67
    | 13 -> Char.chr 100
    | 14 -> Char.chr 173
    | 15 -> Char.chr 46
    | 16 -> Char.chr 235
    | 17 -> Char.chr 27
    | 18 -> Char.chr 215
    | 19 -> Char.chr 172
    | 20 -> Char.chr 36
    | 21 -> Char.chr 26
    | 22 -> Char.chr 75
    | 23 -> Char.chr 47
    | 24 -> Char.chr 33
    | 25 -> Char.chr 160
    | _ -> assert false)

let metadata () =
  match A.Metadata.of_json_string fixture_json with
  | Ok x -> x
  | Error e -> Alcotest.failf "fixture metadata: %a" A.pp_error e

let tampered_ciphertext () =
  let tampered = Bytes.of_string fixture_ciphertext in
  Bytes.set tampered 4 (Char.chr (Char.code (Bytes.get tampered 4) lxor 1));
  Bytes.unsafe_to_string tampered

let chunk_source chunks =
  let chunks = ref chunks in
  fun () ->
    match !chunks with
    | [] -> None
    | chunk :: rest ->
        chunks := rest;
        Some chunk

module Eof_sink = struct
  type t = unit

  let single_write () _ = raise End_of_file
  let copy () ~src:_ = raise End_of_file
end

let eof_sink () = Eio.Resource.T ((), Eio.Flow.Pi.sink (module Eof_sink))

let test_fixture () =
  match A.decrypt (metadata ()) fixture_ciphertext with
  | Ok p -> Alcotest.(check string) "plaintext" "It's a secret to everybody" p
  | Error e -> Alcotest.failf "fixture decrypt: %a" A.pp_error e

let test_chunks () =
  let random = R.of_source (Eio.Flow.string_source (String.make 64 '\x42')) in
  let input = String.init 257 (fun i -> Char.chr (i land 255)) in
  let enc = A.Encryptor.create ~random () in
  let ciphertext =
    let sizes = [ 1; 15; 16; 17; 31; 64; 113 ] in
    let pos = ref 0 in
    List.map
      (fun n ->
        let chunk = String.sub input !pos n in
        pos := !pos + n;
        A.Encryptor.feed enc chunk)
      sizes
    |> fun chunks ->
    let tail =
      A.Encryptor.feed enc (String.sub input !pos (String.length input - !pos))
    in
    String.concat "" (chunks @ [ tail ])
  in
  (* Feed all input, including the prefix consumed above, using a second
     encryptor.  This checks that a stream crossing every block boundary is
     equivalent to one large read. *)
  let random' = R.of_source (Eio.Flow.string_source (String.make 64 '\x42')) in
  let enc' = A.Encryptor.create ~random:random' () in
  let expected = A.Encryptor.feed enc' input in
  let metadata' = A.Encryptor.finish enc' in
  Alcotest.(check string) "chunked encryption" expected ciphertext;
  let dec =
    match A.Decryptor.create metadata' with
    | Ok d -> d
    | Error e -> Alcotest.failf "decryptor: %a" A.pp_error e
  in
  let out = Buffer.create (String.length input) in
  let pos = ref 0 in
  List.iter
    (fun n ->
      let n = min n (String.length ciphertext - !pos) in
      Buffer.add_string out
        (A.Decryptor.feed dec (String.sub ciphertext !pos n));
      pos := !pos + n)
    [ 2; 14; 19; 33; 64; 125 ];
  (match A.Decryptor.finish dec with
  | Ok () -> ()
  | Error e -> Alcotest.failf "digest: %a" A.pp_error e);
  Alcotest.(check string) "chunked decryption" input (Buffer.contents out)

let test_tamper () =
  let m = metadata () in
  match A.decrypt m (tampered_ciphertext ()) with
  | Error A.Hash_mismatch -> ()
  | Error e -> Alcotest.failf "wrong tamper error: %a" A.pp_error e
  | Ok _ -> Alcotest.fail "tampered ciphertext was accepted"

let test_unsupported () =
  let replace old by s =
    let i =
      match String.index_opt s old.[0] with
      | None -> Alcotest.fail "fixture replacement failed"
      | Some i ->
          let rec find i =
            if
              i + String.length old <= String.length s
              && String.sub s i (String.length old) = old
            then i
            else find (i + 1)
          in
          find i
    in
    String.sub s 0 i ^ by
    ^ String.sub s
        (i + String.length old)
        (String.length s - i - String.length old)
  in
  let bad_version = replace "v2" "v1" fixture_json in
  (match A.Metadata.of_json_string bad_version with
  | Error (A.Unsupported_version "v1") -> ()
  | Error e -> Alcotest.failf "wrong version error: %a" A.pp_error e
  | Ok _ -> Alcotest.fail "unsupported version accepted");
  let bad_alg = replace "A256CTR" "BAD" fixture_json in
  match A.Metadata.of_json_string bad_alg with
  | Error (A.Unsupported_algorithm "BAD") -> ()
  | Error e -> Alcotest.failf "wrong algorithm error: %a" A.pp_error e
  | Ok _ -> Alcotest.fail "unsupported algorithm accepted"

let test_jsont_validates () =
  let bad =
    {|{"v":"v2","key":{"kty":"oct","alg":"A256CTR","ext":true,"k":"short","key_ops":["decrypt","encrypt"]},"iv":"i0DovxYdJEcAAAAAAAAAAA","hashes":{"sha256":"ANdt819a8bZl4jKy3Z+jcqtiNICa2y0AW4BBJ/iQRAU"}}|}
  in
  (match Jsont_bytesrw.decode_string A.Metadata.jsont bad with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "Metadata.jsont constructed invalid metadata");
  let bad_hash =
    {|{"v":"v2","key":{"kty":"oct","alg":"A256CTR","ext":true,"k":"Voq2nkPme_x8no5-Tjq_laDAdxE6iDbxnlQXxwFPgE4","key_ops":["decrypt","encrypt"]},"iv":"i0DovxYdJEcAAAAAAAAAAA","hashes":{"sha256":"short"}}|}
  in
  match Jsont_bytesrw.decode_string A.Metadata.jsont bad_hash with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "Metadata.jsont accepted a short SHA-256 hash"

let test_verified_chunks () =
  let delivered = ref [] in
  let on_chunk chunk = delivered := chunk :: !delivered in
  let chunks =
    chunk_source
      [ String.sub fixture_ciphertext 0 7; String.sub fixture_ciphertext 7 19 ]
  in
  (match A.decrypt_chunks (metadata ()) ~chunks ~on_chunk with
  | Ok () -> ()
  | Error e -> Alcotest.failf "verified chunks: %a" A.pp_error e);
  Alcotest.(check string)
    "verified plaintext" "It's a secret to everybody"
    (String.concat "" (List.rev !delivered));
  delivered := [];
  let chunks = chunk_source [ tampered_ciphertext () ] in
  (match A.decrypt_chunks (metadata ()) ~chunks ~on_chunk with
  | Error A.Hash_mismatch -> ()
  | Error e -> Alcotest.failf "wrong chunk tamper error: %a" A.pp_error e
  | Ok () -> Alcotest.fail "tampered chunks were accepted");
  Alcotest.(check int) "no unauthenticated callback" 0 (List.length !delivered)

let with_spooled_attachment env ~name ~metadata ~ciphertext f =
  let spool_path =
    Eio.Path.(Eio.Stdenv.fs env / Filename.get_temp_dir_name () / name)
  in
  Fun.protect
    ~finally:(fun () -> Eio.Path.unlink ~missing_ok:true spool_path)
    (fun () ->
      Eio.Path.with_open_out ~create:(`Or_truncate 0o600) spool_path
        (fun spool ->
          Eio.File.pwrite_all spool ~file_offset:Optint.Int63.zero
            [ Cstruct.of_string ciphertext ];
          f spool))

let test_spooled_decrypt env () =
  let plaintext = String.init 131071 (fun i -> Char.chr (i land 255)) in
  let encrypted =
    A.encrypt
      ~random:(R.of_source (Eio.Flow.string_source (String.make 4096 '\x46')))
      plaintext
  in
  with_spooled_attachment env ~name:"matrix-test-attachment-helper"
    ~metadata:encrypted.metadata ~ciphertext:encrypted.ciphertext (fun spool ->
      ignore (Eio.File.seek spool (Optint.Int63.of_int 17) `Set);
      let output = Buffer.create (String.length plaintext) in
      (match
         A.decrypt_spooled encrypted.metadata ~spool
           ~output:(Eio.Flow.buffer_sink output)
       with
      | Ok () ->
          Alcotest.(check string)
            "spooled plaintext" plaintext (Buffer.contents output)
      | Error e -> Alcotest.failf "spooled decrypt: %a" A.pp_error e);
      Alcotest.(check int)
        "spooled cursor reset" 0
        (Optint.Int63.to_int (Eio.File.seek spool Optint.Int63.zero `Cur)))

let test_spooled_tamper env () =
  let plaintext = "spooled tamper test" in
  let encrypted =
    A.encrypt
      ~random:(R.of_source (Eio.Flow.string_source (String.make 4096 '\x47')))
      plaintext
  in
  let tampered = Bytes.of_string encrypted.ciphertext in
  Bytes.set tampered 0 (Char.chr (Char.code (Bytes.get tampered 0) lxor 1));
  with_spooled_attachment env ~name:"matrix-test-attachment-helper-tamper"
    ~metadata:encrypted.metadata ~ciphertext:(Bytes.unsafe_to_string tampered)
    (fun spool ->
      let output = Buffer.create 32 in
      (match
         A.decrypt_spooled encrypted.metadata ~spool
           ~output:(Eio.Flow.buffer_sink output)
       with
      | Error A.Hash_mismatch -> ()
      | Error e -> Alcotest.failf "wrong spooled tamper error: %a" A.pp_error e
      | Ok () -> Alcotest.fail "tampered spool was accepted");
      Alcotest.(check string)
        "tampered spool emits no plaintext" "" (Buffer.contents output);
      Alcotest.(check int)
        "tampered spool cursor reset" 0
        (Optint.Int63.to_int (Eio.File.seek spool Optint.Int63.zero `Cur)))

let test_spooled_output_eof env () =
  let plaintext = "spooled output eof" in
  let encrypted =
    A.encrypt
      ~random:(R.of_source (Eio.Flow.string_source (String.make 4096 '\x48')))
      plaintext
  in
  with_spooled_attachment env ~name:"matrix-test-attachment-helper-output-eof"
    ~metadata:encrypted.metadata ~ciphertext:encrypted.ciphertext (fun spool ->
      Alcotest.check_raises "output End_of_file is not input EOF" End_of_file
        (fun () ->
          ignore
            (A.decrypt_spooled encrypted.metadata ~spool ~output:(eof_sink ()))))

let run_media_stream ?(versions = "v1.10") ?access_token ?output_sink env
    ~plaintext ~file ~ciphertext =
  let requests = ref [] in
  let fetch =
    Fetch_mock.client (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        requests :=
          (url, Http.Header.get req.headers "authorization") :: !requests;
        if String.ends_with ~suffix:"/_matrix/client/versions" url then
          Fetch_mock.respond
            (Printf.sprintf {|{"versions":["%s"]}|} versions)
            req
        else
          Fetch_mock.respond
            ~headers:
              (Http.Header.of_list
                 [ ("content-type", "application/octet-stream") ])
            ciphertext req)
  in
  let config =
    C.config ~homeserver:(Uriz.of_string_exn "https://hs.example") ()
  in
  let client =
    C.create ~config ~fetch
      ~random:(R.of_source (Eio.Flow.string_source (String.make 4096 '\x43')))
  in
  let client =
    Option.fold ~none:client ~some:(C.with_access_token client) access_token
  in
  let spool_path =
    Eio.Path.(
      Eio.Stdenv.fs env
      / Filename.get_temp_dir_name ()
      / "matrix-test-attachment-spool")
  in
  Eio.Path.with_open_out ~create:(`Or_truncate 0o600) spool_path (fun spool ->
      let output = Buffer.create (String.length plaintext) in
      let result =
        M.download_encrypted_stream client file ~spool
          ~output:
            (Option.value output_sink ~default:(Eio.Flow.buffer_sink output))
      in
      Alcotest.(check int)
        "spool cursor is rewound" 0
        (Optint.Int63.to_int (Eio.File.seek spool Optint.Int63.zero `Cur));
      (result, Buffer.contents output, List.rev !requests))
  |> fun result ->
  Eio.Path.unlink ~missing_ok:true spool_path;
  result

let test_media_stream_output_eof env () =
  let plaintext = "media output eof" in
  let encrypted =
    A.encrypt
      ~random:(R.of_source (Eio.Flow.string_source (String.make 4096 '\x49')))
      plaintext
  in
  let file =
    A.Metadata.to_event_file ~url:"mxc://hs.example/output-eof"
      encrypted.metadata
  in
  Alcotest.check_raises "media output End_of_file is not input EOF" End_of_file
    (fun () ->
      ignore
        (run_media_stream ~output_sink:(eof_sink ()) env ~plaintext ~file
           ~ciphertext:encrypted.ciphertext))

let test_media_stream env () =
  (* Larger than one response read, so the download loop exercises several
     independently sized chunks even with the canned fetch backend. *)
  let plaintext =
    String.init ((128 * 1024) + 123) (fun i -> Char.chr (i land 255))
  in
  let encrypted =
    A.encrypt
      ~random:(R.of_source (Eio.Flow.string_source (String.make 4096 '\x42')))
      plaintext
  in
  let file =
    A.Metadata.to_event_file ~url:"mxc://hs.example/streamed" encrypted.metadata
  in
  match
    run_media_stream env ~plaintext ~file ~ciphertext:encrypted.ciphertext
  with
  | Ok (), output, [ _versions; (url, authorization) ] ->
      Alcotest.(check string) "streamed plaintext" plaintext output;
      Alcotest.(check string)
        "legacy streaming path"
        "https://hs.example/_matrix/media/v3/download/hs.example/streamed" url;
      Alcotest.(check (option string))
        "legacy stream has no bearer" None authorization
  | Ok (), _, requests ->
      Alcotest.failf "unexpected streaming requests: %d" (List.length requests)
  | Error e, _, _ ->
      Alcotest.failf "streamed media download: %a" M.pp_encrypted_error e

let test_media_stream_tamper env () =
  let plaintext = "authenticated streaming plaintext" in
  let encrypted =
    A.encrypt
      ~random:(R.of_source (Eio.Flow.string_source (String.make 4096 '\x44')))
      plaintext
  in
  let tampered = Bytes.of_string encrypted.ciphertext in
  Bytes.set tampered 0 (Char.chr (Char.code (Bytes.get tampered 0) lxor 1));
  let file =
    A.Metadata.to_event_file ~url:"mxc://hs.example/tampered" encrypted.metadata
  in
  match
    run_media_stream env ~plaintext ~file
      ~ciphertext:(Bytes.unsafe_to_string tampered)
  with
  | Error (M.Attachment_error A.Hash_mismatch), output, _ ->
      Alcotest.(check string) "no unauthenticated plaintext" "" output
  | Error e, _, _ ->
      Alcotest.failf "wrong tamper error: %a" M.pp_encrypted_error e
  | Ok (), _, _ -> Alcotest.fail "tampered stream was accepted"

let test_media_stream_authenticated env () =
  let plaintext = "authenticated streaming plaintext" in
  let encrypted =
    A.encrypt
      ~random:(R.of_source (Eio.Flow.string_source (String.make 4096 '\x45')))
      plaintext
  in
  let file =
    A.Metadata.to_event_file ~url:"mxc://hs.example/auth-streamed"
      encrypted.metadata
  in
  match
    run_media_stream ~versions:"v1.12" ~access_token:"syt_secret_token" env
      ~plaintext ~file ~ciphertext:encrypted.ciphertext
  with
  | Ok (), output, [ _versions; (url, authorization) ] ->
      Alcotest.(check string) "streamed plaintext" plaintext output;
      Alcotest.(check string)
        "authenticated streaming path"
        "https://hs.example/_matrix/client/v1/media/download/hs.example/auth-streamed"
        url;
      Alcotest.(check (option string))
        "authenticated stream bearer" (Some "Bearer syt_secret_token")
        authorization
  | Ok (), _, requests ->
      Alcotest.failf "unexpected streaming requests: %d" (List.length requests)
  | Error e, _, _ ->
      Alcotest.failf "authenticated streamed media download: %a"
        M.pp_encrypted_error e

let () =
  Eio_main.run (fun env ->
      Alcotest.run "encrypted attachment"
        [
          ( "attachment",
            [
              Alcotest.test_case "Rust fixture" `Quick test_fixture;
              Alcotest.test_case "chunk boundaries" `Quick test_chunks;
              Alcotest.test_case "tampering" `Quick test_tamper;
              Alcotest.test_case "unsupported metadata" `Quick test_unsupported;
              Alcotest.test_case "jsont validates" `Quick test_jsont_validates;
              Alcotest.test_case "verified chunk release" `Quick
                test_verified_chunks;
              Alcotest.test_case "spooled decrypt" `Quick
                (test_spooled_decrypt env);
              Alcotest.test_case "tampered spool" `Quick
                (test_spooled_tamper env);
              Alcotest.test_case "spooled output EOF" `Quick
                (test_spooled_output_eof env);
              Alcotest.test_case "media streaming roundtrip" `Quick
                (test_media_stream env);
              Alcotest.test_case "media streaming tamper" `Quick
                (test_media_stream_tamper env);
              Alcotest.test_case "authenticated media streaming" `Quick
                (test_media_stream_authenticated env);
              Alcotest.test_case "media output EOF" `Quick
                (test_media_stream_output_eof env);
            ] );
        ])
