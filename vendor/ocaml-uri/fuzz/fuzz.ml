(* Crowbar properties for the RFC 3986 parser.
 *
 * There is no oracle to compare against any more, so the properties are the
 * library's own invariants: parsing never crashes, the canonical form is a
 * fixpoint of parsing, normalization and the percent codecs are idempotent,
 * and reference resolution stays inside the language it claims to produce. *)

open Crowbar

let bases =
  List.filter_map
    (fun s -> match Uriz.of_string s with This u -> Some u | Null -> None)
    [ "http://a/b/c/d;p?q";
      "https://user@example.com:8080/x/y?k=v#f";
      "file:///etc/passwd";
      "urn:isbn:0451450523";
      "//host/path" ]

let () =
  (* Parsing arbitrary bytes must terminate and either reject, or produce a
     canonical string that re-parses to itself. *)
  add_test ~name:"parse fixpoint" [ bytes ] (fun s ->
      match Uriz.of_string s with
      | Null -> ()
      | This u -> (
        let text = Uriz.to_string u in
        match Uriz.of_string text with
        | Null -> failf "canonical form %S does not re-parse" text
        | This u' ->
          check_eq ~pp:pp_string text (Uriz.to_string u');
          (* the already-canonical fast path shares the string *)
          check (Uriz.to_string u' == text);
          check (Uriz.equal u u')));
  (* The heap-free entry point must accept exactly the canonical strings, and
     agree with [of_string] on them. *)
  add_test ~name:"of_string_canonical agrees" [ bytes ] (fun s ->
      let canonical =
        match Uriz.of_string_canonical s with
        | Null -> false
        | This u -> check (Uriz.to_string__local u == s); true
      in
      match Uriz.of_string s with
      | Null -> if canonical then fail "canonical accepted an invalid string"
      | This u ->
        (* it accepts precisely the inputs [of_string] returns unchanged *)
        check_eq ~pp:(fun f b -> Format.fprintf f "%b" b) canonical
          (Uriz.to_string u == s));
  add_test ~name:"normalize idempotent" [ bytes ] (fun s ->
      match Uriz.of_string s with
      | Null -> ()
      | This u ->
        let n = Uriz.normalize u in
        let n2 = Uriz.normalize n in
        check_eq ~pp:pp_string (Uriz.to_string n) (Uriz.to_string n2));
  add_test ~name:"components are substrings" [ bytes ] (fun s ->
      match Uriz.of_string s with
      | Null -> ()
      | This u ->
        let raw = Uriz.to_string u in
        let n = String.length raw in
        let ck (#(off, len) : #(int * int)) =
          check (off = -1 || (off >= 0 && len >= 0 && off + len <= n))
        in
        ck (Uriz.scheme_span u);
        ck (Uriz.userinfo_span u);
        ck (Uriz.host_span u);
        ck (Uriz.port_span u);
        ck (Uriz.path_span u);
        ck (Uriz.query_span u);
        ck (Uriz.fragment_span u));
  add_test ~name:"resolve terminates and re-parses" [ bytes ] (fun s ->
      match Uriz.of_string s with
      | Null -> ()
      | This r ->
        List.iter
          (fun base ->
            let t = Uriz.resolve ~base r in
            (* §5.2: with an absolute base the result carries a scheme. *)
            if Uriz.is_absolute base then check (Uriz.is_absolute t);
            let text = Uriz.to_string t in
            match Uriz.of_string text with
            | Null -> failf "resolve produced unparseable %S" text
            | This t' -> check_eq ~pp:pp_string text (Uriz.to_string t'))
          bases);
  (* the region-allocating variants must agree with the heap ones, and
     globalize must survive the region ending *)
  add_test ~name:"region producers agree" [ bytes ] (fun s ->
      match Uriz.of_string s with
      | Null -> ()
      | This r ->
        let n = Uriz.normalize r in
        let g = Uriz.globalize (Uriz.normalize__local r) in
        check_eq ~pp:pp_string (Uriz.to_string n) (Uriz.to_string g);
        List.iter
          (fun base ->
            let heap = Uriz.resolve ~base r in
            let region = Uriz.globalize (Uriz.resolve__local ~base r) in
            check_eq ~pp:pp_string (Uriz.to_string heap) (Uriz.to_string region))
          bases);
  add_test ~name:"globalize preserves text" [ bytes ] (fun s ->
      match Uriz.of_string s with
      | Null -> ()
      | This u ->
        let g = Uriz.globalize u in
        check_eq ~pp:pp_string (Uriz.to_string u) (Uriz.to_string g);
        check (Uriz.equal u g));
  add_test ~name:"query iteration terminates" [ bytes ] (fun s ->
      match Uriz.of_string s with
      | Null -> ()
      | This u ->
        let n = ref 0 in
        Uriz.query_iter u (fun ~key:_ ~value:_ -> incr n);
        check (!n >= 0));
  add_test ~name:"pct codecs round-trip" [ bytes ] (fun s ->
      let e = Uriz.pct_encode ~component:`Segment s in
      match Uriz.pct_decode e with
      | Null -> failf "encoded %S did not decode" e
      | This d -> check_eq ~pp:pp_string s d);
  add_test ~name:"pct_decode is total" [ bytes ] (fun s ->
      match Uriz.pct_decode s with Null -> () | This _ -> ())

(* {2 Windowed entry points}
 *
 * What an in-buffer caller relies on: what is read out of a window does not
 * depend on the bytes around it, and every offset that comes back is an
 * absolute index into the buffer. *)

let pp_int f x = Format.fprintf f "%d" x
let pp_bool f b = Format.fprintf f "%b" b

let () =
  add_test ~name:"parse_sub ignores its surroundings" [ bytes; bytes; bytes ]
    (fun pre s suf ->
      let pos = String.length pre in
      let v = Uriz.Raw.parse_sub (pre ^ s ^ suf) ~pos ~len:(String.length s) in
      let w = Uriz.Raw.parse s in
      let shift x = if x < 0 then x else x + pos in
      let eq a b = check_eq ~pp:pp_int a b in
      let e = Uriz.Raw.err w in
      eq (if e = 0 then 0 else e + pos) (Uriz.Raw.err v);
      eq (Uriz.Raw.shrink w) (Uriz.Raw.shrink v);
      (* a length, not an offset, so it does not shift *)
      eq (Uriz.Raw.scheme_len w) (Uriz.Raw.scheme_len v);
      eq (shift (Uriz.Raw.userinfo_off w)) (Uriz.Raw.userinfo_off v);
      eq (Uriz.Raw.userinfo_len w) (Uriz.Raw.userinfo_len v);
      eq (shift (Uriz.Raw.host_off w)) (Uriz.Raw.host_off v);
      eq (Uriz.Raw.host_len w) (Uriz.Raw.host_len v);
      eq (Uriz.Raw.host_kind w) (Uriz.Raw.host_kind v);
      eq (shift (Uriz.Raw.port_off w)) (Uriz.Raw.port_off v);
      eq (Uriz.Raw.port_len w) (Uriz.Raw.port_len v);
      eq (Uriz.Raw.port_val w) (Uriz.Raw.port_val v);
      eq (shift (Uriz.Raw.path_off w)) (Uriz.Raw.path_off v);
      eq (Uriz.Raw.path_len w) (Uriz.Raw.path_len v);
      eq (shift (Uriz.Raw.query_off w)) (Uriz.Raw.query_off v);
      eq (Uriz.Raw.query_len w) (Uriz.Raw.query_len v);
      eq (shift (Uriz.Raw.frag_off w)) (Uriz.Raw.frag_off v);
      eq (Uriz.Raw.frag_len w) (Uriz.Raw.frag_len v);
      check_eq ~pp:pp_bool
        (Uriz.Raw.needs_normalization w)
        (Uriz.Raw.needs_normalization v));
  add_test ~name:"pct_decode_into agrees with pct_decode"
    [ bytes; bytes; bytes; bool ] (fun pre s suf plus_as_space ->
      let pos = String.length pre in
      let len = String.length s in
      let buf = pre ^ s ^ suf in
      let dst = Bytes.create (len + 1) in
      let r =
        Uriz.Raw.pct_decode_into buf ~pos ~len ~dst ~dst_pos:0 ~plus_as_space
      in
      let needs = Uriz.Raw.needs_decode buf ~pos ~len ~plus_as_space in
      match Uriz.pct_decode ~plus_as_space s with
      | Null ->
        check (r = -1);
        check needs
      | This d ->
        check_eq ~pp:pp_int (String.length d) r;
        check_eq ~pp:pp_string d (Bytes.sub_string dst 0 r);
        check_eq ~pp:pp_bool (not (String.equal d s)) needs)
