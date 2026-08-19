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
    get (s "a" / str / s "b" / int /? nil) (fun x n _env _req ->
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

let body = Proffer_mock.body
let code o = Status.code (Proffer_mock.status o)
let header = Proffer_mock.header

let get_ ?headers ?body:b target =
  Proffer_mock.request ?headers ?body:b compiled env `GET target

let () =
  let o = get_ "/" in
  check "index status" (code o = 200);
  check "index body" (body o = "index");
  check "index length" (Proffer_mock.content_length o = Some 5L)

let () =
  let o = get_ "/contact/avsm" in
  check "capture body" (body o = "hello avsm")

let () =
  let o = get_ "/a/x/b/42" in
  check "two captures" (body o = "x:42");
  let o = get_ "/a/x/b/nope" in
  check "int rejects" (code o = 404)

let () =
  check "conv accepts" (body (get_ "/kind/person") = "kind person");
  check "conv rejects" (code (get_ "/kind/plant") = 404)

let () =
  check "rest captures" (body (get_ "/static/css/site.css") = "css|site.css");
  check "rest empty" (body (get_ "/static") = "")

let () =
  let o = get_ "/nowhere" in
  check "default fallback status" (code o = 404);
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
  check "post status" (code o = 303);
  check "post location" (header o "Location" = Some "/contact/avsm")

let () =
  let o = Proffer_mock.request compiled env `PUT "/contact/avsm" in
  check "405 status" (code o = 405);
  check "405 allow" (header o "Allow" = Some "GET, DELETE, HEAD");
  let o = Proffer_mock.request compiled env `DELETE "/contact/avsm" in
  check "delete matches" (body o = "deleted avsm")

let () =
  (* A path with no route at all is the fallback, not a 405. *)
  let o = Proffer_mock.request compiled env `PUT "/nowhere" in
  check "405 needs a path match" (code o = 404)

let () =
  let site =
    Site.of_routes [ get nil (fun _env _req -> failwith "handler blew up") ]
  in
  let seen = ref None in
  let o =
    Proffer_mock.request
      ~on_error:(fun e -> seen := Some e)
      (Compiled.compile site) env `GET "/"
  in
  check "handler exception is 500" (code o = 500);
  check "on_error is told"
    (match !seen with Some (Failure _) -> true | _ -> false)

(* A response that would split the wire is refused where it is built, and the
   handler guard turns that into a 500 reported to [on_error]. *)
let () =
  let site =
    Site.of_routes
      [ get nil (fun _env _req -> Resp.see_other "/next\r\nSet-Cookie: x") ]
  in
  let seen = ref None in
  let o =
    Proffer_mock.request
      ~on_error:(fun e -> seen := Some e)
      (Compiled.compile site) env `GET "/"
  in
  check "an illegal header is 500" (code o = 500);
  check "on_error names the field"
    (match !seen with Some (Invalid_argument _) -> true | _ -> false)

let () =
  let o = get_ "/static/caf%C3%A9/a%20b/x%2Fy" in
  check "a tail segment is decoded" (body o = "caf\xc3\xa9|a b|x/y");
  check "plus stays literal in a tail" (body (get_ "/static/a+b") = "a+b")

(* Allow lists the methods the path has routes for, in route order, and HEAD
   whenever GET is among them. *)
let () =
  let site meths =
    Compiled.compile
      (Site.of_routes
         (List.map
            (fun m -> route m (s "r" /? nil) (fun _env _req -> Resp.text "r"))
            meths))
  in
  let allow c = header (Proffer_mock.request c env `PUT "/r") "Allow" in
  check "allow follows route order"
    (allow (site [ `DELETE; `GET; `POST ]) = Some "DELETE, GET, POST, HEAD");
  check "another order lists another way"
    (allow (site [ `POST; `GET; `DELETE ]) = Some "POST, GET, DELETE, HEAD");
  check "no GET means no HEAD" (allow (site [ `POST ]) = Some "POST");
  check "a method with two routes is listed once"
    (allow (site [ `GET; `GET ]) = Some "GET, HEAD");
  let o = Proffer_mock.request (site [ `POST ]) env `HEAD "/r" in
  check "HEAD does not reach a POST route" (code o = 405);
  check "the 405 for HEAD lists POST" (header o "Allow" = Some "POST")

(* Methods compare by wire spelling, so a route declared with an [`Other]
   token answers the request a parser turned into [`GET]. *)
let () =
  let c =
    Compiled.compile
      (Site.of_routes
         [
           route (`Other "GET") (s "odd" /? nil) (fun _env _req ->
               Resp.text "odd");
         ])
  in
  check "an Other spelling of GET is GET"
    (body (Proffer_mock.request c env `GET "/odd") = "odd");
  check "and it answers HEAD as well"
    (code (Proffer_mock.request c env `HEAD "/odd") = 200);
  check "and it is spelled GET in Allow"
    (header (Proffer_mock.request c env `PUT "/odd") "Allow"
    = Some "GET, HEAD")

let () = Printf.printf "test_route: %d checks ok\n" !checks
