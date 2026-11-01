open Proffer
open Proffer.Route

let page =
  {|<!doctype html>
<form method="post" action="/greet">
  <label>Your name: <input name="name"></label>
  <button>Greet me</button>
</form>

<form method="post" action="/upload" enctype="multipart/form-data">
  <label>Your name: <input name="name"></label>
  <label>A file: <input type="file" name="file"></label>
  <button>Upload it</button>
</form>
|}

let site =
  Site.of_routes
    [ get root (fun () _request respond -> Resp.html respond page);

      post (s "greet") (fun () request respond ->
        match Req.form_param request "name" with
        | Some name -> Resp.see_other respond ("/hello/" ^ name)
        | None -> Resp.bad_request respond ());

      post (s "upload") (fun () request respond ->
        match Multipart.of_req request with
        | Error e -> Resp.text respond ~status:Bad_request
            (Media.error_to_string e ^ "\n")
        | Ok parts ->
          match Multipart.file parts "file" with
          | None -> Resp.text respond ~status:Bad_request "No file was sent.\n"
          | Some file ->
            let name =
              Option.value (Multipart.field request parts "name")
                ~default:"stranger"
            in
            Resp.text respond
              (Printf.sprintf "Thank you %s, I got %s (%d bytes).\n" name
                 (Option.value file.filename ~default:"a file") file.len));

      get (s "hello" / str) (fun name () _request respond ->
        Resp.text respond ("Good morning, " ^ Req.globalize name ^ "!\n"));

      get (s "search") (fun () request respond ->
        match Req.query_param request "q" with
        | Some q -> Resp.text respond ("You searched for: " ^ q ^ "\n")
        | None -> Resp.text respond "Add ?q=something to the URL.\n") ]

let () =
  Eio_main.run @@ fun stdenv ->
  Proffer_httpz.run stdenv ~env:() site
