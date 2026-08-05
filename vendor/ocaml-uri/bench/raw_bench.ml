(* Cost of RFC 3986 validation per call and per byte.

   [Raw.parse_sub] is what an HTTP server runs over a request-target sitting in
   its connection buffer, so each target is timed in place inside a larger
   buffer rather than as a bare string.

   [@@@ai: written by Claude (claude-opus-5) under human direction.] *)

let sink = ref 0

(* A target is scanned out of the middle of a buffer, the way a server sees it:
   request line before, headers after. *)
let embed s =
  let pre = "GET " in
  let post = " HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  (pre ^ s ^ post, String.length pre, String.length s)

let targets =
  [ ("origin-form", "/api/v1/users/12345?q=1&r=2");
    ( "long path",
      "/v2/organisations/acme-corp/projects/website-redesign/issues/4821/comments?page=3&per_page=50&sort=created_at"
    );
    ( "authority",
      "https://user:pass@www.example.com:8080/a/b/c?foo=bar&baz=qux#frag" );
    ("encoded", "/files/My%20Documents/report%2Ddraft%5Ffinal.pdf?dl=1");
    ("ipv6 authority", "https://[2001:db8::1]:443/api/v1/items?filter=a,b,c") ]

(* Median of [runs] timings, each of [iters] calls.  The median discards the
   scheduler noise that a mean would keep. *)
let median xs =
  let a = Array.of_list xs in
  Array.sort compare a;
  a.(Array.length a / 2)

let time_one buf pos len iters =
  let t0 = Unix.gettimeofday () in
  for _ = 1 to iters do
    sink := !sink + Uriz.Raw.err (Uriz.Raw.parse_sub buf ~pos ~len)
  done;
  let t1 = Unix.gettimeofday () in
  (t1 -. t0) *. 1e9 /. float iters

let () =
  let iters = 2_000_000 in
  let runs = 5 in
  Printf.printf "%-16s %5s  %10s  %10s\n" "target" "bytes" "ns/call" "ns/byte";
  List.iter
    (fun (name, s) ->
      let buf, pos, len = embed s in
      (match Uriz.Raw.err (Uriz.Raw.parse_sub buf ~pos ~len) with
      | 0 -> ()
      | e -> failwith (Printf.sprintf "%s: invalid at %d" name (e - 1)));
      (* warm up *)
      ignore (time_one buf pos len (iters / 10));
      let ns =
        median (List.init runs (fun _ -> time_one buf pos len iters))
      in
      Printf.printf "%-16s %5d  %10.1f  %10.2f\n" name len ns
        (ns /. float len))
    targets;
  (* A whole-string [parse] for reference: the same scan, entry point aside. *)
  let s = "/api/v1/users/12345?q=1&r=2" in
  let n = String.length s in
  ignore (time_one s 0 n (iters / 10));
  let ns = median (List.init runs (fun _ -> time_one s 0 n iters)) in
  Printf.printf "%-16s %5d  %10.1f  %10.2f\n" "bare parse_sub" n ns
    (ns /. float n);
  ignore !sink
