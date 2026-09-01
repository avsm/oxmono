open Proffer
open Proffer.Route
module H = Httpz.Header_name
module St = Httpz.Res
module M = Httpz.Method

type env = { prefix : string }

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

(* Captured segments are local to the request, so they are copied out. *)
let rec join (segs : string list @ local) =
  match segs with
  | [] -> ""
  | [ x ] -> Req.globalize x
  | x :: tl -> Req.globalize x ^ "|" ^ join tl

let routes =
  [
    get root (fun _env _req respond -> Resp.text respond "index");
    get (s "contact" / str) (fun handle env _req respond ->
        Resp.text respond (env.prefix ^ Req.globalize handle));
    get (s "a" / str / s "b" / int) (fun x n _env _req respond ->
        Resp.text respond (Printf.sprintf "%s:%d" (Req.globalize x) n));
    get
      (s "kind"
      / conv ~name:"kind" (fun v ->
            match v with "person" | "org" -> Some v | _ -> None)
     )
      (fun k _env _req respond -> Resp.text respond ("kind " ^ k));
    get (s "static" / rest) (fun segs _env _req respond ->
        Resp.text respond (join segs));
    post (s "new") (fun _env req respond ->
        match Req.form_param req "handle" with
        | Some h -> Resp.see_other respond ("/contact/" ^ h)
        | None -> Resp.bad_request respond ());
    route M.Delete (s "contact" / str) (fun handle _env _req respond ->
        Resp.text respond ("deleted " ^ Req.globalize handle));
  ]

let compiled = (Site.of_routes routes)
let env = { prefix = "hello " }

let body = Proffer_mock.body
let code o = Status.code (Proffer_mock.status o)
let header = Proffer_mock.header

let get_ ?headers ?body:b target =
  Proffer_mock.request ?headers ?body:b compiled env M.Get target

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

(* One resource, one path: [int] takes decimal and nothing else, so the OCaml
   literal spellings [int_of_string] would accept cannot name it too. *)
let () =
  check "a negative integer captures" (body (get_ "/a/x/b/-42") = "x:-42");
  check "zero captures" (body (get_ "/a/x/b/0") = "x:0");
  List.iter
    (fun seg ->
      check ("int rejects " ^ seg) (code (get_ ("/a/x/b/" ^ seg)) = 404))
    [
      "007"; "00"; "-0"; "-01"; "0x1f"; "0X1F"; "0o17"; "0b101"; "1_000"; "+3"; "-"; ""; " 4"; "4 ";
      "4.0"; "1e3"; "--1";
      (* Two past [max_int] on 64-bit, and its 63-bit negative twin. *)
      "9223372036854775808"; "-9223372036854775809";
    ]

(* Matching skips empty segments, which is a normalization a front proxy
   authorizing by path prefix has to make too, or its rules miss these. *)
let () =
  check "a doubled separator collapses" (body (get_ "/a//x/b/1") = "x:1");
  check "a trailing separator is ignored" (body (get_ "/a/x/b/1/") = "x:1");
  check "the root tolerates repeats" (body (get_ "///") = "index")

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
      (fun _env req respond ->
        Resp.text respond ~status:St.Not_found ("no " ^ Req.globalize (Req.path req)))
      (Site.of_routes routes)
  in
  let o = Proffer_mock.request (site) env M.Get "/nowhere" in
  check "custom fallback" (body o = "no /nowhere")

let () =
  let o =
    Proffer_mock.request
      ~headers:[ ("Content-Type", "application/x-www-form-urlencoded") ]
      ~body:"handle=avsm" compiled env M.Post "/new"
  in
  check "post status" (code o = 303);
  check "post location" (header o H.Location = Some "/contact/avsm")

let () =
  let o = Proffer_mock.request compiled env M.Put "/contact/avsm" in
  check "405 status" (code o = 405);
  check "405 allow" (header o H.Allow = Some "GET, DELETE, HEAD");
  let o = Proffer_mock.request compiled env M.Delete "/contact/avsm" in
  check "delete matches" (body o = "deleted avsm")

let () =
  let o = Proffer_mock.request compiled env M.Put "/nowhere" in
  check "405 needs a path match" (code o = 404)

let () =
  let site =
    Site.of_routes
      [ get root (fun _env _req _respond -> failwith "handler blew up") ]
  in
  let seen = ref None in
  let o =
    Proffer_mock.request
      ~on_error:(fun e -> seen := Some e)
      (site) env M.Get "/"
  in
  check "handler exception is 500" (code o = 500);
  check "on_error is told"
    (match !seen with Some (Failure _) -> true | _ -> false)

(* Response-splitting attempts become a reported 500. *)
let () =
  let site =
    Site.of_routes
      [ get root (fun _env _req respond ->
            Resp.see_other respond "/next\r\nSet-Cookie: x") ]
  in
  let seen = ref None in
  let o =
    Proffer_mock.request
      ~on_error:(fun e -> seen := Some e)
      (site) env M.Get "/"
  in
  check "an illegal header is 500" (code o = 500);
  check "on_error names the field"
    (match !seen with Some (Invalid_argument _) -> true | _ -> false)

let () =
  let site =
    Site.of_routes
      [ get
          (s "explode" / conv ~name:"explode" (fun _ -> failwith "converter"))
          (fun _ _env _req respond -> Resp.text respond "unreachable") ]
  in
  let seen = ref false in
  let response =
    Proffer_mock.request ~on_error:(fun _ -> seen := true) site env M.Get "/explode/x"
  in
  check "a raising route converter becomes 500"
    (code response = 500 && !seen)

let () =
  let ran = Atomic.make false in
  let site =
    Site.of_routes
      [ get root (fun _env _req respond ->
          Atomic.set ran true;
          Resp.text respond "unreachable") ]
  in
  let response =
    Proffer_mock.request
      ~headers:[ "Content-Type", "text/plain"; "Content-Type", "text/html" ]
      site env M.Get "/"
  in
  check "duplicate request Content-Type is rejected before dispatch"
    (code response = 400 && not (Atomic.get ran))

let () =
  let o = get_ "/static/caf%C3%A9/a%20b/x%2Fy" in
  check "a tail segment is decoded" (body o = "caf\xc3\xa9|a b|x/y");
  check "plus stays literal in a tail" (body (get_ "/static/a+b") = "a+b")

(* Allow lists the methods the path has routes for, in route order, and HEAD
   whenever GET is among them. *)
let () =
  let site meths =
    (Site.of_routes
         (List.map
            (fun m ->
              route m (s "r") (fun _env _req respond ->
                  Resp.text respond "r"))
            meths))
  in
  let allow c = header (Proffer_mock.request c env M.Put "/r") H.Allow in
  check "allow follows route order"
    (allow (site [ M.Delete; M.Get; M.Post ]) = Some "DELETE, GET, POST, HEAD");
  check "another order lists another way"
    (allow (site [ M.Post; M.Get; M.Delete ]) = Some "POST, GET, DELETE, HEAD");
  check "no GET means no HEAD" (allow (site [ M.Post ]) = Some "POST");
  check "a method with two routes is listed once"
    (allow (site [ M.Get; M.Get ]) = Some "GET, HEAD");
  check "an explicit HEAD is not duplicated"
    (allow (site [ M.Get; M.Head ]) = Some "GET, HEAD");
  let o = Proffer_mock.request (site [ M.Post ]) env M.Head "/r" in
  check "HEAD does not reach a POST route" (code o = 405);
  check "the 405 for HEAD lists POST" (header o H.Allow = Some "POST")

let () = Printf.printf "test_route: %d checks ok\n" !checks
