(* Exercise Fetch.dav over the real streaming Proffer backend. All paths and
   credentials belong to the disposable test fixture. *)
module D = Fetch_dav
let checks = ref 0
let check name ok = incr checks; if not ok then failwith name
let () = Eio_main.run @@ fun env ->
  let root = Sys.argv.(1) in
  let backend = Fetch_httpz.v ~clock:env#mono_clock env#net () in
  let scoped = Fetch.with_credentials ~scope:[root] ~allow_insecure:true
    [Fetch.Credential.basic ~user:"alice" ~password:"test-alice"] backend in
  let client = D.v ~root scoped in
  let collection = "fetch/" in
  let denied status fn = match fn () with
    | _ -> failwith "expected DAV rejection"
    | exception D.Http_error e -> check "rejection status" (e.status = status) in
  D.mkcol client collection;
  Fun.protect ~finally:(fun () -> ignore (D.delete client collection)) (fun () ->
    let path = D.child client ~collection "é % .txt" in
    let bytes = String.init 350000 (fun i -> Char.chr (i mod 256)) in
    let r = D.put ~condition:D.If_absent client path
      (Fetch.stream (Eio.Flow.string_source bytes)) in
    check "chunked PUT returns validator" (r.status = 201 && r.etag <> None);
    let received, tag = D.get client path in
    check "Fetch streams exact bytes" (received = bytes && tag = r.etag);
    denied 412 (fun () -> D.put ~condition:D.If_absent client path Fetch.Empty);
    let lease = D.lock client path in
    denied 423 (fun () -> D.put client path Fetch.Empty);
    ignore (D.put ~if_:(D.lock_condition lease) client path (Fetch.String "locked"));
    let lease = D.refresh_lock client lease in
    D.unlock client lease;
    let property = "urn:fetch:test", "value" in
    ignore (D.proppatch client path [Httpz_dav.Set
      [Httpz_dav.element property [Httpz_dav.Text "kept"]]]);
    let props = D.propfind client path (Httpz_dav.Prop [property]) in
    check "Fetch property decoding" (match props.responses with
      | [r] -> (match Httpz_dav.property property r with
        | Some (Ok e) -> Httpz_dav.text e = Ok "kept" | _ -> false)
      | _ -> false);
    ignore (D.copy client ~src:path ~dst:(collection ^ "copied") ());
    ignore (D.move client ~src:(collection ^ "copied")
      ~dst:(collection ^ "moved") ());
    check "Fetch COPY/MOVE" (fst (D.get client (collection ^ "moved")) = "locked");
    check "DAV discovery" ((D.options client "").dav = ["1"; "2"]));
  Printf.printf "%d Fetch-to-Proffer DAV checks passed\n%!" !checks
