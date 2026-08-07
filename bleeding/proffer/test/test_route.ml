(* Dispatch: every converter form, the tail capture, 405 with Allow, and the
   fallback. *)

open Proffer
open Proffer.Route

type env = { prefix : string }

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let routes =
  [
    get nil (fun _env _req -> Resp.text "index");
    get (s "contact" / str /? nil) (fun handle env _req ->
        Resp.text (env.prefix ^ handle));
    get (s "a" / str / s "b" / int' /? nil) (fun x n _env _req ->
        Resp.text (Printf.sprintf "%s:%d" x n));
    get
      (s "kind"
      / conv ~name:"kind" (fun v ->
            match v with "person" | "org" -> Some v | _ -> None)
      /? nil)
      (fun k _env _req -> Resp.text ("kind " ^ k));
    get (s "static" /* rest) (fun segs _env _req ->
        Resp.text (String.concat "|" segs));
    post (s "new" /? nil) (fun _env req ->
        match Req.form_param req "handle" with
        | Some h -> Resp.see_other ("/contact/" ^ h)
        | None -> Resp.bad_request ());
    route `DELETE (s "contact" / str /? nil) (fun handle _env _req ->
        Resp.text ("deleted " ^ handle));
  ]

let compiled = Compiled.compile (Site.of_routes routes)
let env = { prefix = "hello " }

let body (o : Serve.outcome) =
  match o.Serve.body with
  | `String s -> s
  | `Empty -> ""
  | `Stream _ -> "<stream>"

let get_ ?headers ?body:b target =
  Proffer_mock.request ?headers ?body:b compiled env `GET target

let () =
  let o = get_ "/" in
  check "index status" (Status.code o.Serve.status = 200);
  check "index body" (body o = "index");
  check "index length" (o.Serve.content_length = Some 5L)

let () =
  let o = get_ "/contact/avsm" in
  check "capture body" (body o = "hello avsm")

let () =
  let o = get_ "/a/x/b/42" in
  check "two captures" (body o = "x:42");
  let o = get_ "/a/x/b/nope" in
  check "int' rejects" (Status.code o.Serve.status = 404)

let () =
  check "conv accepts" (body (get_ "/kind/person") = "kind person");
  check "conv rejects" (Status.code (get_ "/kind/plant").Serve.status = 404)

let () =
  check "rest captures" (body (get_ "/static/css/site.css") = "css|site.css");
  check "rest empty" (body (get_ "/static") = "")

let () =
  let o = get_ "/nowhere" in
  check "default fallback status" (Status.code o.Serve.status = 404);
  check "default fallback body" (body o = "Not Found\n")

let () =
  let site =
    Site.with_fallback
      (fun _env req -> Resp.text ~status:`Not_found ("no " ^ Req.path req))
      (Site.of_routes routes)
  in
  let o = Proffer_mock.request (Compiled.compile site) env `GET "/nowhere" in
  check "custom fallback" (body o = "no /nowhere")

let () =
  let o =
    Proffer_mock.request
      ~headers:[ ("Content-Type", "application/x-www-form-urlencoded") ]
      ~body:"handle=avsm" compiled env `POST "/new"
  in
  check "post status" (Status.code o.Serve.status = 303);
  check "post location"
    (List.assoc_opt "Location" o.Serve.headers = Some "/contact/avsm")

let () =
  let o = Proffer_mock.request compiled env `PUT "/contact/avsm" in
  check "405 status" (Status.code o.Serve.status = 405);
  check "405 allow"
    (List.assoc_opt "Allow" o.Serve.headers = Some "GET, DELETE, HEAD");
  let o = Proffer_mock.request compiled env `DELETE "/contact/avsm" in
  check "delete matches" (body o = "deleted avsm")

let () =
  (* A path with no route at all is the fallback, not a 405. *)
  let o = Proffer_mock.request compiled env `PUT "/nowhere" in
  check "405 needs a path match" (Status.code o.Serve.status = 404)

let () =
  let site =
    Site.of_routes [ get nil (fun _env _req -> failwith "handler blew up") ]
  in
  let seen = ref None in
  let o =
    Serve.handle
      ~on_error:(fun e -> seen := Some e)
      (Compiled.compile site) env
      (Req.v ~meth:`GET ~target:"/" ())
  in
  check "handler exception is 500" (Status.code o.Serve.status = 500);
  check "on_error is told"
    (match !seen with Some (Failure _) -> true | _ -> false)

let () = Printf.printf "test_route: %d checks ok\n" !checks
