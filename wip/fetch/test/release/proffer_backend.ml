(* Manual benchmark: setup stays outside the measured request loops. *)
open Proffer

let iterations = 500_000
let observed = ref 0
let write (outcome : Backend.outcome @ local) =
  observed := !observed + Status.code outcome.status

let measure name (run : (unit -> unit) @ local) =
  Gc.full_major ();
  let allocated = Gc.allocated_bytes () in
  let started = Unix.gettimeofday () in
  for _ = 1 to iterations do run () done;
  let elapsed = Unix.gettimeofday () -. started in
  let bytes = Gc.allocated_bytes () -. allocated in
  Printf.printf "%s: %.0f ns/request; %.0f bytes/request\n%!" name
    (elapsed *. 1e9 /. float iterations) (bytes /. float iterations)

let request ?(headers = Headers.empty) meth = exclave_
  Req.v ~meth ~target:"/page" ~headers ()

let () =
  let tag = Etag.strong "v1" in
  let describe (respond : Resp.respond @ local) =
    Resp.v respond ~etag:tag ~headers:Headers.empty
      ~content_type:(This "text/plain") (Body.String "hello")
  in
  let local_ req = request Httpz.Method.Get in
  measure "GET" (fun () -> Backend.run req describe write);
  let local_ req = request Httpz.Method.Head in
  measure "HEAD" (fun () -> Backend.run req describe write);
  let local_ req = request
      ~headers:[Headers.h Httpz.Header_name.If_none_match "\"v1\""] Httpz.Method.Get in
  measure "304" (fun () -> Backend.run req describe write);
  let local_ req = request
      ~headers:[Headers.h Httpz.Header_name.If_match "\"other\""] Httpz.Method.Get in
  measure "412" (fun () -> Backend.run req describe write);
  let site = Site.of_routes Route.[get (s "page") (fun () _req respond -> describe respond)] in
  let local_ req = request Httpz.Method.Get in
  measure "dispatch GET" (fun () -> Backend.handle site () req write);
  let local_ req = request Httpz.Method.Post in
  measure "dispatch 405" (fun () -> Backend.handle site () req write);
  ignore (Sys.opaque_identity !observed)
