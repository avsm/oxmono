(* Head-to-head core_bench comparison of upstream uri (4.4.0, angstrom-based)
   against uriz on parsing, printing, query lookup and reference resolution.

   [@@@ai: The comparison harness in this file was written by Claude
   (claude-fable-5) under human direction; see uriz.opam x-ai-* fields.] *)

open Core
open Core_bench

let uris =
  [| "https://user:pass@www.example.com:8080/a/b/c?foo=bar&baz=qux,quux#frag";
     "http://example.com/";
     "/relative/path?q=1";
     "https://[2001:db8::1]:443/api/v1/items?filter=a,b,c";
     "urn:isbn:0451450523" |]

(* Pre-parsed values for the non-parsing benchmarks. *)
let uri_ts = Array.map uris ~f:Uri.of_string

let uriz_ts =
  Array.map uris ~f:(fun s ->
      match Uriz.of_string s with
      | This u -> u
      | Null -> failwith "uriz rejected benchmark uri")

let query_uri = "https://example.com/search?alpha=1&beta=two&gamma=3,4&delta"
let uri_q = Uri.of_string query_uri

let uriz_q =
  match Uriz.of_string query_uri with
  | This u -> u
  | Null -> assert false

(* RFC 3986 §5.4 base and a representative relative reference. *)
let base_str = "http://a/b/c/d;p?q"
let rel_str = "../../g"
let uri_base = Uri.of_string base_str
let uri_rel = Uri.of_string rel_str

let uriz_base =
  match Uriz.of_string base_str with This u -> u | Null -> assert false

let uriz_rel =
  match Uriz.of_string rel_str with This u -> u | Null -> assert false

let sink = ref 0

let tests =
  [ Bench.Test.create_group ~name:"of_string"
      [ Bench.Test.create ~name:"uri" (fun () ->
            Array.iter uris ~f:(fun s -> ignore (Uri.of_string s : Uri.t)));
        Bench.Test.create ~name:"uriz" (fun () ->
            Array.iter uris ~f:(fun s ->
                match Uriz.of_string s with
                | This u -> sink := !sink + Uriz.port_int u
                | Null -> ()));
        Bench.Test.create ~name:"uriz_canonical" (fun () ->
            Array.iter uris ~f:(fun s ->
                match Uriz.of_string_canonical s with
                | This u -> sink := !sink + Uriz.port_int u
                | Null -> ()));
        Bench.Test.create ~name:"uriz_raw_parse" (fun () ->
            Array.iter uris ~f:(fun s ->
                sink := !sink + Uriz.Raw.err (Uriz.Raw.parse s)))
      ];
    Bench.Test.create_group ~name:"to_string"
      [ Bench.Test.create ~name:"uri" (fun () ->
            Array.iter uri_ts ~f:(fun t ->
                sink := !sink + String.length (Uri.to_string t)));
        Bench.Test.create ~name:"uriz" (fun () ->
            Array.iter uriz_ts ~f:(fun t ->
                sink := !sink + String.length (Uriz.to_string t)))
      ];
    Bench.Test.create_group ~name:"query_param"
      [ Bench.Test.create ~name:"uri" (fun () ->
            ignore (Uri.get_query_param uri_q "gamma" : string option));
        Bench.Test.create ~name:"uriz" (fun () ->
            match Uriz.find_query uriz_q "gamma" with
            | This v -> sink := !sink + String.length v
            | Null -> ())
      ];
    Bench.Test.create_group ~name:"resolve"
      [ Bench.Test.create ~name:"uri" (fun () ->
            ignore (Uri.resolve "" uri_base uri_rel : Uri.t));
        Bench.Test.create ~name:"uriz" (fun () ->
            sink := !sink + Uriz.port_int (Uriz.resolve ~base:uriz_base uriz_rel));
        (* composes the result text in the caller's region: no heap at all *)
        Bench.Test.create ~name:"uriz_local" (fun () ->
            sink
            := !sink
               + Uriz.port_int (Uriz.resolve__local ~base:uriz_base uriz_rel))
      ]
  ]

let () = Command_unix.run (Bench.make_command tests)
