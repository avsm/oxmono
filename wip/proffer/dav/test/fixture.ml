(* A loopback-only interoperability fixture. Never use these test credentials
   or [allow_insecure_loopback] for a public listener. DAV is disabled unless
   --enable-dav is explicitly supplied. *)
module D = Proffer_dav
module S = Proffer_dav_eio
let () =
  let enabled = ref false and create = ref false and port = ref 18765 in
  let folder = ref "" and store = ref "" and cert = ref ""
  and key = ref "" and require_tls = ref false in
  Arg.parse [
    "--enable-dav", Arg.Set enabled, " Enable the test exports";
    "--create", Arg.Set create, " Initialize an empty private store";
    "--port", Arg.Set_int port, " Loopback port";
    "--folder", Arg.Set_string folder, " Read-only directory";
    "--store", Arg.Set_string store, " Private managed store";
    "--cert", Arg.Set_string cert, " TLS certificate PEM";
    "--key", Arg.Set_string key, " TLS private key PEM";
    "--require-tls", Arg.Set require_tls, " Require actual TLS provenance"
  ] (fun _ -> raise (Arg.Bad "unexpected argument")) "DAV test fixture";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let tls = if !cert = "" then None else
    let read path = In_channel.with_open_bin path In_channel.input_all in
    let ok = function Ok x -> x | Error (`Msg e) -> failwith e in
    let certs = ok (X509.Certificate.decode_pem_multiple (read !cert)) in
    let key = ok (X509.Private_key.decode_pem (read !key)) in
    Some (Httpz_tls.server (ok (Tls.Config.server
      ~certificates:(`Single (certs, key)) ~alpn_protocols:["http/1.1"] ()))) in
  let site = Proffer.Site.of_routes [] in
  let exports, site = if not !enabled then [], site else begin
    let scheme = if tls <> None || !require_tls then "https" else "http" in
    let origin = Printf.sprintf "%s://127.0.0.1:%d" scheme !port in
    let security = D.Security.authenticated ~realm:"DAV test fixture"
      ~authenticate:(fun field ->
        (* Constant-length digest comparisons avoid credential timing leaks.
           The identities and passwords here are public test data. *)
        let hash s = Digestif.SHA256.digest_string s in
        let candidates = ["alice", "alice:test-alice";
          "bob", "bob:test-bob"; "reader", "reader:test-reader"] in
        List.find_map (fun (principal, credential) ->
          if Digestif.SHA256.equal (hash field)
            (hash ("Basic " ^ Base64.encode_string credential))
          then Some principal else None) candidates)
      ~authorize:(function "alice" | "bob" -> Some D.Read_write
        | "reader" -> Some D.Read_only | _ -> None) in
    let ro = S.reader ~sw Eio.Path.(env#fs / !folder) in
    let quota = {S.max_file_bytes=4_194_304L; max_storage_bytes=16_777_216L;
      max_staging_bytes=8_388_608L; max_entries=1000;
      max_metadata_bytes=4_194_304} in
    let rw = S.writer ~sw ~create:!create ~quota ~clock:env#clock
      ~mono_clock:env#mono_clock ~random:env#secure_random
      Eio.Path.(env#fs / !store) in
    let limits = {D.default_limits with max_file_bytes=quota.max_file_bytes;
      max_xml_bytes=65536; max_resources=1000} in
    let ro = D.read_only ~limits ~allow_insecure_loopback:true ~origin
      ~at:["ro"] ~security ro in
    let rw = D.read_write ~limits ~allow_insecure_loopback:true ~origin
      ~at:["rw"] ~security rw in
    let exports = ["ro", ro; "rw", rw] in
    let site = site
      |> D.mount ~at:["ro"] (fun exports -> List.assoc "ro" exports)
      |> D.mount ~at:["rw"] (fun exports -> List.assoc "rw" exports) in
    exports, site
  end in
  let site = site |> Proffer.Site.with_headers ["X-DAV-Fixture", "yes"] in
  let config = {Proffer_httpz.default_config with
    request_timeout=Duration.of_sec 5; max_connections=32} in
  Proffer_httpz.run ~sw ~port:!port ~config ?tls env ~env:exports site
    ~on_error:(fun exn -> prerr_endline (Printexc.to_string exn))
    ~on_event:(fun event ->
      (* Keep a regression oracle for token redaction without logging values. *)
      let event = Proffer_httpz.globalize_event event in
      List.iter (fun (name, value) ->
        if List.mem (String.lowercase_ascii name)
          ["authorization"; "if"; "lock-token"] && value <> "<redacted>"
        then (prerr_endline "DAV fixture: unredacted credential"; exit 2))
        event.Proffer_httpz.request_headers)
