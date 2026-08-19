(*
 * Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2012-2014 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-5"]
[@@@ai_provider "Anthropic"]

open OUnit2
open Printf

(* {2 Helpers} *)

let str s = s
let opt = function Null -> "<absent>" | This s -> s
let opt_int = function Null -> "<absent>" | This i -> string_of_int i

let parse s =
  match Uriz.of_string s with
  | This u -> u
  | Null -> assert_failure (sprintf "expected %S to parse" s)

let kind_str = function
  | `None -> "none"
  | `Reg_name -> "reg-name"
  | `Ipv4 -> "ipv4"
  | `Ipv6 -> "ipv6"
  | `Ipvfuture -> "ipvfuture"

let case name f = name >:: fun _ -> f ()

(* {2 Acceptance and rejection} *)

let valid_uris =
  [ "";
    "/";
    "//";
    "///";
    "?";
    "#";
    "?#";
    "a";
    "a/b";
    "/a/b";
    "./a:b";
    "a:b";
    "http:";
    "http://";
    "http://a";
    "http://a/";
    "http://a:80/";
    "http://a:/";
    "http://user:pw@a:80/p?q#f";
    "urn:isbn:0451450523";
    "mailto:a@b.c";
    "//[::1]";
    "//[::]";
    "//[v7.aBc:1]";
    "//1.2.3.4";
    "//999.1.1.1";
    "//01.2.3.4";
    "//a/b%20c";
    "?a=1&b=2";
    "#frag";
    "foo//bar";
    "//a?q";
    "s://a?q#f" ]

let invalid_uris =
  [ ":";
    ":a";
    "1a:b";
    "%";
    "%A";
    "%GZ";
    "a%2";
    "a b";
    "http://a/b c";
    "http://a\n";
    "http://a/\000";
    "http://[::1";
    "http://[1:2:3:4:5:6:7:8:9]/";
    "http://[12345::1]/";
    "http://[1::2::3]/";
    "http://[]/";
    "http://[zz]/";
    "http://a@b@c/";
    "http://a/b?q#f#g";
    "http://a:99999999999999999999999999/";
    "http://a/b|c";
    "<>";
    "a\\b" ]

let test_accept =
  List.map
    (fun s -> case (sprintf "accept:%S" s) (fun () ->
         match Uriz.of_string s with
         | This _ -> ()
         | Null -> assert_failure (sprintf "%S should be a valid URI reference" s)))
    valid_uris

let test_reject =
  List.map
    (fun s -> case (sprintf "reject:%S" s) (fun () ->
         match Uriz.of_string s with
         | Null -> ()
         | This u ->
           assert_failure
             (sprintf "%S should be rejected, got %S" s (Uriz.to_string u))))
    invalid_uris

(* {2 Component decomposition} *)

let decompositions =
  (* input, scheme, userinfo, host, host_kind, port, path, query, fragment *)
  [ ("http://user:pw@example.com:8080/a/b?q=1#f", "http", "user:pw",
     "example.com", "reg-name", "8080", "/a/b", "q=1", "f");
    ("http://a", "http", "<absent>", "a", "reg-name", "<absent>", "", "<absent>",
     "<absent>");
    ("http://a/", "http", "<absent>", "a", "reg-name", "<absent>", "/",
     "<absent>", "<absent>");
    ("http://a?", "http", "<absent>", "a", "reg-name", "<absent>", "", "",
     "<absent>");
    ("http://a#", "http", "<absent>", "a", "reg-name", "<absent>", "",
     "<absent>", "");
    ("http://a:/", "http", "<absent>", "a", "reg-name", "<absent>", "/",
     "<absent>", "<absent>");
    ("http://a:0/", "http", "<absent>", "a", "reg-name", "0", "/", "<absent>",
     "<absent>");
    ("//", "<absent>", "<absent>", "", "reg-name", "<absent>", "", "<absent>",
     "<absent>");
    ("http:", "http", "<absent>", "<absent>", "none", "<absent>", "",
     "<absent>", "<absent>");
    ("urn:isbn:0451450523", "urn", "<absent>", "<absent>", "none", "<absent>",
     "isbn:0451450523", "<absent>", "<absent>");
    ("//[2001:db8::1]:443/x", "<absent>", "<absent>", "2001:db8::1", "ipv6",
     "443", "/x", "<absent>", "<absent>");
    ("//192.0.2.1/x", "<absent>", "<absent>", "192.0.2.1", "ipv4", "<absent>",
     "/x", "<absent>", "<absent>");
    ("//[v7.a:b]/", "<absent>", "<absent>", "v7.a:b", "ipvfuture", "<absent>",
     "/", "<absent>", "<absent>");
    ("?q", "<absent>", "<absent>", "<absent>", "none", "<absent>", "", "q",
     "<absent>");
    ("#f", "<absent>", "<absent>", "<absent>", "none", "<absent>", "",
     "<absent>", "f") ]

let test_decompose =
  List.map
    (fun (input, sc, ui, ho, hk, po, pa, qu, fr) ->
      case (sprintf "decompose:%S" input) (fun () ->
          let u = parse input in
          assert_equal ~printer:str ~msg:"scheme" sc (opt (Uriz.scheme u));
          assert_equal ~printer:str ~msg:"userinfo" ui (opt (Uriz.userinfo u));
          assert_equal ~printer:str ~msg:"host" ho (opt (Uriz.host u));
          assert_equal ~printer:str ~msg:"host_kind" hk
            (kind_str (Uriz.host_kind u));
          assert_equal ~printer:str ~msg:"port" po (opt_int (Uriz.port u));
          assert_equal ~printer:str ~msg:"path" pa (Uriz.path u);
          assert_equal ~printer:str ~msg:"query" qu (opt (Uriz.query u));
          assert_equal ~printer:str ~msg:"fragment" fr (opt (Uriz.fragment u))))
    decompositions

(* {2 Empty vs absent} *)

let test_empty_vs_absent =
  [ case "empty-vs-absent:query" (fun () ->
        assert_equal ~printer:str "<absent>" (opt (Uriz.query (parse "http://a")));
        assert_equal ~printer:str "" (opt (Uriz.query (parse "http://a?"))));
    case "empty-vs-absent:fragment" (fun () ->
        assert_equal ~printer:str "<absent>"
          (opt (Uriz.fragment (parse "http://a")));
        assert_equal ~printer:str "" (opt (Uriz.fragment (parse "http://a#"))));
    case "empty-vs-absent:authority" (fun () ->
        assert_bool "http:// has authority" (Uriz.has_authority (parse "http://"));
        assert_bool "http: has none"
          (not (Uriz.has_authority (parse "http:"))));
    case "empty-vs-absent:path" (fun () ->
        assert_equal ~printer:str "" (Uriz.path (parse "http://a"));
        assert_equal ~printer:str "/" (Uriz.path (parse "http://a/")));
    case "empty-vs-absent:roundtrip" (fun () ->
        List.iter
          (fun s ->
            assert_equal ~printer:str ~msg:s s (Uriz.to_string (parse s)))
          [ "http://a"; "http://a/"; "http://a?"; "http://a#"; "http://a?#";
            "http://a/?#"; "http://a:"; "//"; "http:"; "" ]) ]

(* {2 Normalization, RFC 3986 §6.2.2} *)

let normalizations =
  [ ("HTTP://EXAMPLE.COM/", "http://example.com/");
    ("http://EXAMPLE.com/Path", "http://example.com/Path");
    ("hTtP://a/", "http://a/");
    (* §6.2.2.2 uppercase the hex digits *)
    ("http://a/%2f", "http://a/%2F");
    ("http://a/%3a%3B", "http://a/%3A%3B");
    (* §6.2.2.2 decode unreserved characters *)
    ("http://a/%7Euser", "http://a/~user");
    ("http://a/%41%42", "http://a/AB");
    ("http://a/%2D%2E%5F", "http://a/-._");
    (* the reserved ones stay encoded *)
    ("http://a/%2F", "http://a/%2F");
    (* host case folding also applies to decoded octets *)
    ("http://%45xample.com/", "http://example.com/");
    (* IPv6 hex is folded, brackets are preserved *)
    ("http://[2001:DB8::1]/", "http://[2001:db8::1]/");
    (* userinfo, query and fragment keep their case *)
    ("http://User@a/?Q=A#F", "http://User@a/?Q=A#F");
    (* percent normalization reaches every component *)
    ("http://%75ser@a/%61?%62#%63", "http://user@a/a?b#c") ]

let test_normalize_syntax =
  List.map
    (fun (input, expected) ->
      case (sprintf "normalize:%S" input) (fun () ->
          assert_equal ~printer:str expected (Uriz.to_string (parse input))))
    normalizations

let test_normalize_idempotent =
  List.map
    (fun (input, _) ->
      case (sprintf "idempotent:%S" input) (fun () ->
          let once = Uriz.to_string (parse input) in
          let twice = Uriz.to_string (parse once) in
          assert_equal ~printer:str once twice))
    normalizations

let test_dot_segments =
  [ case "of_string keeps dot segments" (fun () ->
        assert_equal ~printer:str "http://a/b/../c"
          (Uriz.to_string (parse "http://a/b/../c")));
    case "normalize removes dot segments" (fun () ->
        List.iter
          (fun (input, expected) ->
            assert_equal ~printer:str ~msg:input expected
              (Uriz.to_string (Uriz.normalize (parse input))))
          [ ("http://a/b/../c", "http://a/c");
            ("http://a/b/./c", "http://a/b/c");
            ("http://a/./b/../b/c/", "http://a/b/c/");
            ("http://a/../../g", "http://a/g");
            ("http://a/b/c/.", "http://a/b/c/");
            ("http://a/b/c/..", "http://a/b/");
            ("http://a/", "http://a/");
            ("urn:a/b/../c", "urn:a/c") ]);
    case "normalize leaves relative references alone" (fun () ->
        List.iter
          (fun s ->
            assert_equal ~printer:str ~msg:s s
              (Uriz.to_string (Uriz.normalize (parse s))))
          [ "../a"; "./a"; "a/../b" ]);
    case "normalize is idempotent" (fun () ->
        List.iter
          (fun s ->
            let n = Uriz.to_string (Uriz.normalize (parse s)) in
            let n2 = Uriz.to_string (Uriz.normalize (parse n)) in
            assert_equal ~printer:str ~msg:s n n2)
          [ "http://a/b/../c"; "http://a/../../g"; "http://a/b/c/..";
            "urn:a/b/../c"; "http://a/b/c/." ]);
    case "normalize guards // paths" (fun () ->
        (* "s:/a/..//b" reduces to a path starting "//", which RFC 3986 §3.3
           forbids without an authority, so the "/." escape of §4.2 is
           inserted rather than emitting something that would re-parse as an
           authority. *)
        let n = Uriz.normalize (parse "s:/a/..//b") in
        assert_equal ~printer:str "s:/.//b" (Uriz.to_string n);
        assert_bool "no authority" (not (Uriz.has_authority n));
        assert_equal ~printer:str "/.//b" (Uriz.path n);
        assert_equal ~printer:str "s:/.//b"
          (Uriz.to_string (Uriz.normalize n))) ]

(* {2 IPv4 and IPv6 literals} *)

let ipv4_accept =
  [ "0.0.0.0"; "1.2.3.4"; "255.255.255.255"; "192.0.2.1"; "9.9.9.9";
    "10.0.0.255"; "127.0.0.1" ]

let ipv4_reject =
  [ ""; "1.2.3"; "1.2.3.4.5"; "999.1.1.1"; "256.1.1.1"; "01.2.3.4";
    "1.2.3.04"; "00.0.0.0"; "1.2.3."; ".1.2.3"; "1.2.3.4."; "1234.1.1.1";
    "-1.2.3.4"; "1.2.3.a"; "1.2.3.4:5" ]

let ipv6_accept =
  [ "::"; "::1"; "1::"; "fe80::1"; "2001:db8::1";
    "2001:db8:1234:5678:90ab:cdef:0123:4567";
    "2001:db8:1234:5678:90ab:cdef:0123::";
    "2001:db8:1234:5678:90ab:cdef::123";
    "2001:db8:1234:5678:90ab:cdef:192.0.2.1";
    "::ffff:192.0.2.1"; "::192.0.2.1"; "1:2:3:4:5:6:7:8"; "1::8";
    "1:2:3:4:5:6:7::"; "::2:3:4:5:6:7:8"; "0:0:0:0:0:0:0:0"; "ABCD::1" ]

let ipv6_reject =
  [ ""; ":"; ":::"; ":1"; "1:"; "1:2:3:4:5:6:7"; "1:2:3:4:5:6:7:8:9";
    "1::2::3"; "12345::1"; "::1:2:3:4:5:6:7:8"; "1:2:3:4:5:6:7:8::";
    "g::1"; "1.2.3.4"; "1:2:3:4:5:6:7:1.2.3.4"; "::1.2.3"; "1::2:3:4:5:6:7:8";
    "fe80::1%eth0" ]

let test_ipv4 =
  List.map
    (fun s -> case (sprintf "ipv4-accept:%S" s) (fun () ->
         assert_bool s (Uriz.Raw.is_ipv4 s);
         assert_equal ~printer:str ~msg:"host_kind" "ipv4"
           (kind_str (Uriz.host_kind (parse ("//" ^ s))))))
    ipv4_accept
  @ List.map
      (fun s -> case (sprintf "ipv4-reject:%S" s) (fun () ->
           assert_bool s (not (Uriz.Raw.is_ipv4 s))))
      ipv4_reject

let test_ipv6 =
  List.map
    (fun s -> case (sprintf "ipv6-accept:%S" s) (fun () ->
         assert_bool s (Uriz.Raw.is_ipv6 s);
         let u = parse (sprintf "//[%s]" s) in
         assert_equal ~printer:str ~msg:"host_kind" "ipv6"
           (kind_str (Uriz.host_kind u));
         assert_equal ~printer:str ~msg:"host" (String.lowercase_ascii s)
           (opt (Uriz.host u))))
    ipv6_accept
  @ List.map
      (fun s -> case (sprintf "ipv6-reject:%S" s) (fun () ->
           assert_bool s (not (Uriz.Raw.is_ipv6 s));
           match Uriz.of_string (sprintf "//[%s]" s) with
           | Null -> ()
           | This u ->
             assert_failure
               (sprintf "//[%s] should not parse, got %S" s (Uriz.to_string u))))
      ipv6_reject

let test_host_kinds =
  [ case "reg-name fallbacks" (fun () ->
        List.iter
          (fun (input, kind) ->
            assert_equal ~printer:str ~msg:input kind
              (kind_str (Uriz.host_kind (parse ("//" ^ input)))))
          [ ("1.2.3.4", "ipv4"); ("1.2.3.4.5", "reg-name");
            ("999.1.1.1", "reg-name"); ("01.2.3.4", "reg-name");
            ("example.com", "reg-name"); ("", "reg-name") ]);
    case "brackets are not part of the host" (fun () ->
        let u = parse "//[::1]:8080/" in
        assert_equal ~printer:str "::1" (opt (Uriz.host u));
        assert_equal ~printer:str "//[::1]:8080/" (Uriz.to_string u)) ]

(* {2 Percent codecs} *)

let test_pct =
  [ case "pct_decode basics" (fun () ->
        assert_equal ~printer:str "abc" (opt (Uriz.pct_decode "abc"));
        assert_equal ~printer:str "a/b" (opt (Uriz.pct_decode "a%2Fb"));
        assert_equal ~printer:str "a/b" (opt (Uriz.pct_decode "a%2fb"));
        assert_equal ~printer:str "~" (opt (Uriz.pct_decode "%7E"));
        assert_equal ~printer:str " " (opt (Uriz.pct_decode "%20")));
    case "pct_decode rejects malformed" (fun () ->
        List.iter
          (fun s ->
            match Uriz.pct_decode s with
            | Null -> ()
            | This d -> assert_failure (sprintf "%S decoded to %S" s d))
          [ "%"; "%A"; "%GZ"; "%2"; "a%"; "a%1"; "%%41"; "%zz" ]);
    case "pct_decode shares when there is nothing to do" (fun () ->
        let s = "no-escapes-here" in
        match Uriz.pct_decode s with
        | This d -> assert_bool "physically equal" (d == s)
        | Null -> assert_failure "unexpected Null");
    case "pct_encode component sets" (fun () ->
        assert_equal ~printer:str "a%20b" (Uriz.pct_encode "a b");
        assert_equal ~printer:str "a/b" (Uriz.pct_encode ~component:`Path "a/b");
        assert_equal ~printer:str "a%2Fb"
          (Uriz.pct_encode ~component:`Segment "a/b");
        assert_equal ~printer:str "a%26b"
          (Uriz.pct_encode ~component:`Query_value "a&b");
        assert_equal ~printer:str "a&b" (Uriz.pct_encode ~component:`Query "a&b");
        assert_equal ~printer:str "a-b_c.d~e9"
          (Uriz.pct_encode ~component:`Unreserved "a-b_c.d~e9");
        assert_equal ~printer:str "o%27brien%20x"
          (Uriz.pct_encode ~component:`Unreserved "o'brien x");
        assert_equal ~printer:str "%C3%A9"
          (Uriz.pct_encode ~component:`Unreserved "\xc3\xa9");
        assert_equal ~printer:str "%25" (Uriz.pct_encode "%");
        assert_equal ~printer:str "%2541" (Uriz.pct_encode "%41"));
    case "pct_encode shares when there is nothing to do" (fun () ->
        let s = "already-safe" in
        assert_bool "physically equal" (Uriz.pct_encode s == s));
    case "pct codecs round-trip" (fun () ->
        let sample = String.init 256 Char.chr in
        let e = Uriz.pct_encode ~component:`Segment sample in
        assert_equal ~printer:str sample (opt (Uriz.pct_decode e)));
    case "%2F is not a separator" (fun () ->
        let u = parse "http://a/x%2Fy/z" in
        assert_equal ~printer:str "/x%2Fy/z" (Uriz.path u);
        assert_equal ~printer:str "/x/y/z" (Uriz.path_decoded u)) ]

(* {2 Query} *)

let test_query =
  [ case "query_iter" (fun () ->
        let u = parse "http://a/?x=1&y=2&z&=v&w=" in
        let acc = ref [] in
        Uriz.query_iter u (fun ~key ~value ->
            acc := (key, match value with Null -> "<none>" | This v -> v) :: !acc);
        assert_equal
          ~printer:(fun l ->
            String.concat "," (List.map (fun (k, v) -> k ^ "=" ^ v) l))
          [ ("x", "1"); ("y", "2"); ("z", "<none>"); ("", "v"); ("w", "") ]
          (List.rev !acc));
    case "query_iter decodes" (fun () ->
        let u = parse "?a%20b=c%2Fd" in
        let acc = ref [] in
        Uriz.query_iter u (fun ~key ~value -> acc := (key, opt value) :: !acc);
        assert_equal ~printer:(fun l ->
            String.concat "," (List.map (fun (k, v) -> k ^ "=" ^ v) l))
          [ ("a b", "c/d") ] !acc);
    case "query_iter on empty and absent" (fun () ->
        let count s =
          let n = ref 0 in
          Uriz.query_iter (parse s) (fun ~key:_ ~value:_ -> incr n);
          !n
        in
        assert_equal ~printer:string_of_int 0 (count "http://a");
        assert_equal ~printer:string_of_int 0 (count "http://a?");
        assert_equal ~printer:string_of_int 1 (count "http://a?x");
        assert_equal ~printer:string_of_int 2 (count "http://a?x&"));
    case "find_query" (fun () ->
        let u = parse "?a=1&b=2&b=3&c" in
        assert_equal ~printer:str "1" (opt (Uriz.find_query u "a"));
        assert_equal ~printer:str "2" (opt (Uriz.find_query u "b"));
        assert_equal ~printer:str "" (opt (Uriz.find_query u "c"));
        assert_equal ~printer:str "<absent>" (opt (Uriz.find_query u "d")));
    case "find_query matches decoded keys" (fun () ->
        let u = parse "?a%20b=1" in
        assert_equal ~printer:str "1" (opt (Uriz.find_query u "a b"));
        assert_equal ~printer:str "<absent>" (opt (Uriz.find_query u "a%20b")));
    case "query_step spans" (fun () ->
        let u = parse "?ab=cd&e" in
        let raw = Uriz.to_string u in
        let pos = Uriz.query_cursor u in
        let #(ko, kl, vo, vl, next) = Uriz.query_step u pos in
        assert_equal ~printer:str "ab" (String.sub raw ko kl);
        assert_equal ~printer:str "cd" (String.sub raw vo vl);
        let #(ko, kl, vo, _, next) = Uriz.query_step u next in
        assert_equal ~printer:str "e" (String.sub raw ko kl);
        assert_equal ~printer:string_of_int (-1) vo;
        assert_equal ~printer:string_of_int (-1) next) ]

(* {2 RFC 3986 §5.4 reference resolution} *)

let resolution_base = "http://a/b/c/d;p?q"

let resolution_normal =
  [ ("g:h", "g:h");
    ("g", "http://a/b/c/g");
    ("./g", "http://a/b/c/g");
    ("g/", "http://a/b/c/g/");
    ("/g", "http://a/g");
    ("//g", "http://g");
    ("?y", "http://a/b/c/d;p?y");
    ("g?y", "http://a/b/c/g?y");
    ("#s", "http://a/b/c/d;p?q#s");
    ("g#s", "http://a/b/c/g#s");
    ("g?y#s", "http://a/b/c/g?y#s");
    (";x", "http://a/b/c/;x");
    ("g;x", "http://a/b/c/g;x");
    ("g;x?y#s", "http://a/b/c/g;x?y#s");
    ("", "http://a/b/c/d;p?q");
    (".", "http://a/b/c/");
    ("./", "http://a/b/c/");
    ("..", "http://a/b/");
    ("../", "http://a/b/");
    ("../g", "http://a/b/g");
    ("../..", "http://a/");
    ("../../", "http://a/");
    ("../../g", "http://a/g") ]

let resolution_abnormal =
  [ ("../../../g", "http://a/g");
    ("../../../../g", "http://a/g");
    ("/./g", "http://a/g");
    ("/../g", "http://a/g");
    ("g.", "http://a/b/c/g.");
    (".g", "http://a/b/c/.g");
    ("g..", "http://a/b/c/g..");
    ("..g", "http://a/b/c/..g");
    ("./../g", "http://a/b/g");
    ("./g/.", "http://a/b/c/g/");
    ("g/./h", "http://a/b/c/g/h");
    ("g/../h", "http://a/b/c/h");
    ("g;x=1/./y", "http://a/b/c/g;x=1/y");
    ("g;x=1/../y", "http://a/b/c/y");
    ("g?y/./x", "http://a/b/c/g?y/./x");
    ("g?y/../x", "http://a/b/c/g?y/../x");
    ("g#s/./x", "http://a/b/c/g#s/./x");
    ("g#s/../x", "http://a/b/c/g#s/../x");
    (* strict parser: "http:g" keeps its own (empty-authority-less) form *)
    ("http:g", "http:g") ]

let test_resolution =
  let base = lazy (parse resolution_base) in
  List.map
    (fun (r, expected) ->
      case (sprintf "resolve:%S" r) (fun () ->
          let got =
            Uriz.to_string (Uriz.resolve ~base:(Lazy.force base) (parse r))
          in
          assert_equal ~printer:str expected got))
    (resolution_normal @ resolution_abnormal)

let test_resolution_misc =
  [ case "resolve with authority-only reference" (fun () ->
        assert_equal ~printer:str "http://other/x"
          (Uriz.to_string
             (Uriz.resolve ~base:(parse "http://a/b") (parse "//other/x"))));
    case "resolve empty against base with fragment" (fun () ->
        assert_equal ~printer:str "http://a/b?q"
          (Uriz.to_string
             (Uriz.resolve ~base:(parse "http://a/b?q") (parse ""))));
    case "resolve keeps reference fragment" (fun () ->
        assert_equal ~printer:str "http://a/b?q#f"
          (Uriz.to_string
             (Uriz.resolve ~base:(parse "http://a/b?q") (parse "#f"))));
    case "resolve against rootless base" (fun () ->
        (* §5.2.3 merge: all but the last segment of the base path *)
        assert_equal ~printer:str "urn:a/c"
          (Uriz.to_string (Uriz.resolve ~base:(parse "urn:a/b") (parse "c")));
        assert_equal ~printer:str "urn:c"
          (Uriz.to_string (Uriz.resolve ~base:(parse "urn:b") (parse "c"))));
    case "resolve against base with empty path" (fun () ->
        assert_equal ~printer:str "http://a/g"
          (Uriz.to_string (Uriz.resolve ~base:(parse "http://a") (parse "g"))));
    case "resolve results re-parse" (fun () ->
        List.iter
          (fun (r, _) ->
            let t = Uriz.resolve ~base:(parse resolution_base) (parse r) in
            let s = Uriz.to_string t in
            assert_equal ~printer:str ~msg:r s (Uriz.to_string (parse s)))
          (resolution_normal @ resolution_abnormal)) ]

(* {2 Identity} *)

let test_identity =
  [ case "equal after normalization" (fun () ->
        assert_bool "case-insensitive scheme/host"
          (Uriz.equal (parse "HTTP://EXAMPLE.com/a") (parse "http://example.COM/a"));
        assert_bool "hex case"
          (Uriz.equal (parse "http://a/%2f") (parse "http://a/%2F"));
        assert_bool "unreserved decoding"
          (Uriz.equal (parse "http://a/%7E") (parse "http://a/~")));
    case "distinctions kept" (fun () ->
        assert_bool "empty vs absent path"
          (not (Uriz.equal (parse "http://a") (parse "http://a/")));
        assert_bool "default port is not dropped"
          (not (Uriz.equal (parse "http://a:80/") (parse "http://a/")));
        assert_bool "%2F is not /"
          (not (Uriz.equal (parse "http://a/%2F") (parse "http://a//"))));
    case "compare is a total order on canonical text" (fun () ->
        assert_equal ~printer:string_of_int 0
          (Uriz.compare (parse "http://a/") (parse "HTTP://A/"));
        assert_bool "ordering"
          (Uriz.compare (parse "http://a/") (parse "http://b/") < 0));
    case "hash agrees with equal" (fun () ->
        assert_equal ~printer:string_of_int
          (Uriz.hash (parse "HTTP://A/x"))
          (Uriz.hash (parse "http://a/x"))) ]

(* {2 make and with_*} *)

let test_make =
  [ case "make full" (fun () ->
        assert_equal ~printer:str "http://u:p@example.com:8080/a/b?q=1#f"
          (Uriz.to_string
             (Uriz.make ~scheme:"http" ~userinfo:"u:p" ~host:"example.com"
                ~port:8080 ~path:"/a/b" ~query:"q=1" ~fragment:"f" ())));
    case "make encodes what it must" (fun () ->
        assert_equal ~printer:str "http://a/a%20b"
          (Uriz.to_string (Uriz.make ~scheme:"http" ~host:"a" ~path:"/a b" ()));
        assert_equal ~printer:str "http://a/%25"
          (Uriz.to_string (Uriz.make ~scheme:"http" ~host:"a" ~path:"/%" ())));
    case "make preserves valid triplets" (fun () ->
        assert_equal ~printer:str "http://a/a%2Fb"
          (Uriz.to_string (Uriz.make ~scheme:"http" ~host:"a" ~path:"/a%2Fb" ())));
    case "make round-trips a parsed path" (fun () ->
        let u = parse "http://a/x%2Fy/z%20w" in
        assert_equal ~printer:str (Uriz.to_string u)
          (Uriz.to_string
             (Uriz.make ~scheme:"http" ~host:"a" ~path:(Uriz.path u) ())));
    case "make brackets IPv6" (fun () ->
        assert_equal ~printer:str "http://[::1]/"
          (Uriz.to_string (Uriz.make ~scheme:"http" ~host:"::1" ~path:"/" ()));
        assert_equal ~printer:str "http://[::1]/"
          (Uriz.to_string (Uriz.make ~scheme:"http" ~host:"[::1]" ~path:"/" ())));
    case "make adds the missing slash" (fun () ->
        assert_equal ~printer:str "http://a/b"
          (Uriz.to_string (Uriz.make ~scheme:"http" ~host:"a" ~path:"b" ())));
    case "make guards a colon in a relative first segment" (fun () ->
        assert_equal ~printer:str "./a:b"
          (Uriz.to_string (Uriz.make ~path:"a:b" ())));
    case "make lowercases" (fun () ->
        assert_equal ~printer:str "http://a/"
          (Uriz.to_string (Uriz.make ~scheme:"HTTP" ~host:"A" ~path:"/" ())));
    case "make rejects bad schemes" (fun () ->
        assert_raises (Invalid_argument "Uriz: invalid scheme: 1a") (fun () ->
            Uriz.make ~scheme:"1a" ~host:"a" ());
        assert_raises (Invalid_argument "Uriz: invalid scheme: ") (fun () ->
            Uriz.make ~scheme:"" ~host:"a" ()));
    case "make rejects negative ports" (fun () ->
        assert_raises (Invalid_argument "Uriz: negative port") (fun () ->
            Uriz.make ~scheme:"http" ~host:"a" ~port:(-1) ())) ]

let test_with =
  [ case "with_scheme" (fun () ->
        let u = parse "http://a/b?q#f" in
        assert_equal ~printer:str "https://a/b?q#f"
          (Uriz.to_string (Uriz.with_scheme u (This "https")));
        assert_equal ~printer:str "//a/b?q#f"
          (Uriz.to_string (Uriz.with_scheme u Null)));
    case "with_host" (fun () ->
        let u = parse "http://a:8080/b" in
        assert_equal ~printer:str "http://z:8080/b"
          (Uriz.to_string (Uriz.with_host u (This "z")));
        assert_equal ~printer:str "http:/b"
          (Uriz.to_string (Uriz.with_host u Null)));
    case "with_port" (fun () ->
        let u = parse "http://a/b" in
        assert_equal ~printer:str "http://a:99/b"
          (Uriz.to_string (Uriz.with_port u (This 99)));
        assert_equal ~printer:str "http://a/b"
          (Uriz.to_string (Uriz.with_port (parse "http://a:99/b") Null)));
    case "with_userinfo" (fun () ->
        let u = parse "http://a/b" in
        assert_equal ~printer:str "http://u@a/b"
          (Uriz.to_string (Uriz.with_userinfo u (This "u"))));
    case "with_path" (fun () ->
        let u = parse "http://a/b?q" in
        assert_equal ~printer:str "http://a/z?q"
          (Uriz.to_string (Uriz.with_path u "/z"));
        assert_equal ~printer:str "http://a?q"
          (Uriz.to_string (Uriz.with_path u "")));
    case "with_query" (fun () ->
        let u = parse "http://a/b?q#f" in
        assert_equal ~printer:str "http://a/b?x=1#f"
          (Uriz.to_string (Uriz.with_query u (This "x=1")));
        assert_equal ~printer:str "http://a/b#f"
          (Uriz.to_string (Uriz.with_query u Null)));
    case "with_fragment" (fun () ->
        let u = parse "http://a/b#f" in
        assert_equal ~printer:str "http://a/b#g"
          (Uriz.to_string (Uriz.with_fragment u (This "g")));
        assert_equal ~printer:str "http://a/b"
          (Uriz.to_string (Uriz.with_fragment u Null)));
    case "with_ round-trips" (fun () ->
        let u = parse "http://u:p@a:8080/x/y?k=v#f" in
        assert_equal ~printer:str (Uriz.to_string u)
          (Uriz.to_string (Uriz.with_fragment u (Uriz.fragment u)))) ]

(* {2 Decoded accessors} *)

let test_decoded =
  [ case "path_decoded" (fun () ->
        assert_equal ~printer:str "/a b"
          (Uriz.path_decoded (parse "http://h/a%20b"));
        assert_equal ~printer:str "" (Uriz.path_decoded (parse "http://h")));
    case "userinfo_decoded" (fun () ->
        assert_equal ~printer:str "u:p@x"
          (opt (Uriz.userinfo_decoded (parse "http://u:p%40x@h/"))));
    case "fragment_decoded" (fun () ->
        assert_equal ~printer:str "a b"
          (opt (Uriz.fragment_decoded (parse "http://h/#a%20b")));
        assert_equal ~printer:str "<absent>"
          (opt (Uriz.fragment_decoded (parse "http://h/")))) ]

(* {2 Sharing and allocation}

   [@zero_alloc] in the mlis is the static proof; these are the runtime
   cross-check the spec asks for. *)

let sink = ref 0
let ssink = ref ""

let alloc_delta n f =
  let before = Gc.allocated_bytes () in
  for _ = 1 to n do
    f ()
  done;
  Gc.allocated_bytes () -. before

(* [Gc.allocated_bytes] itself allocates, so a single delta carries a fixed
   overhead.  Comparing two run lengths cancels it and leaves the true
   per-call cost. *)
let assert_no_alloc name f =
  f ();
  f ();
  let d1 = alloc_delta 1000 f in
  let d2 = alloc_delta 3000 f in
  assert_equal
    ~printer:(fun x -> sprintf "%.3f bytes/call" x)
    ~msg:name 0.
    ((d2 -. d1) /. 2000.)

let alloc_sample = "https://user:pass@www.example.com:8080/a/b/c?foo=bar#frag"

let test_allocation =
  [ case "Raw.parse allocates nothing" (fun () ->
        assert_no_alloc "Raw.parse" (fun () ->
            let v = Uriz.Raw.parse alloc_sample in
            sink := !sink + Uriz.Raw.err v + Uriz.Raw.host_off v
                    + Uriz.Raw.path_len v + Uriz.Raw.port_val v));
    case "Raw.parse on an invalid string allocates nothing" (fun () ->
        assert_no_alloc "Raw.parse invalid" (fun () ->
            let v = Uriz.Raw.parse "http://a/b c\000%zz" in
            sink := !sink + Uriz.Raw.err v));
    case "to_string allocates nothing" (fun () ->
        let u = parse alloc_sample in
        assert_no_alloc "to_string" (fun () -> ssink := Uriz.to_string u));
    case "span accessors allocate nothing" (fun () ->
        let u = parse alloc_sample in
        assert_no_alloc "spans" (fun () ->
            let #(a, b) = Uriz.host_span u in
            let #(c, d) = Uriz.path_span u in
            let #(e, f) = Uriz.query_span u in
            sink := !sink + a + b + c + d + e + f + Uriz.port_int u));
    case "query_step allocates nothing" (fun () ->
        let u = parse "http://a/?x=1&y=2&z=3" in
        assert_no_alloc "query_step" (fun () ->
            let mutable pos = Uriz.query_cursor u in
            let mutable go = pos >= 0 in
            while go do
              let #(ko, kl, vo, vl, next) = Uriz.query_step u pos in
              sink := !sink + ko + kl + vo + vl;
              if next < 0 then go <- false else pos <- next
            done));
    case "ipv4/ipv6 validators allocate nothing" (fun () ->
        assert_no_alloc "is_ipv6" (fun () ->
            if Uriz.Raw.is_ipv6 "2001:db8::1" then incr sink;
            if Uriz.Raw.is_ipv4 "192.0.2.1" then incr sink)) ]

let test_sharing =
  [ case "canonical input is shared, not copied" (fun () ->
        let s = "http://example.com/a/b?c=d#e" in
        match Uriz.of_string s with
        | Null -> assert_failure "should parse"
        | This u -> assert_bool "physically equal" (Uriz.to_string u == s));
    case "non-canonical input is rebuilt" (fun () ->
        let s = "HTTP://example.com/" in
        match Uriz.of_string s with
        | Null -> assert_failure "should parse"
        | This u ->
          assert_bool "not physically equal" (not (Uriz.to_string u == s));
          assert_equal ~printer:str "http://example.com/" (Uriz.to_string u)) ]

(* {2 or_null behaviour} *)

let test_or_null =
  [ case "of_string returns Null on garbage" (fun () ->
        let garbage = [ "\000"; "\255\254"; "a b"; "%"; "<>"; "\n" ] in
        List.iter
          (fun s ->
            match Uriz.of_string s with
            | Null -> ()
            | This u ->
              assert_failure (sprintf "%S parsed as %S" s (Uriz.to_string u)))
          garbage);
    case "of_string_exn raises" (fun () ->
        assert_raises
          (Invalid_argument "Uriz.of_string_exn: not a URI reference: %")
          (fun () -> Uriz.of_string_exn "%"));
    case "error offsets are reported" (fun () ->
        let v = Uriz.Raw.parse "http://a/b c" in
        assert_bool "invalid" (not (Uriz.Raw.is_valid v));
        assert_equal ~printer:string_of_int 10 (Uriz.Raw.error_offset v));
    case "valid spans report no error" (fun () ->
        let v = Uriz.Raw.parse "http://a/b" in
        assert_bool "valid" (Uriz.Raw.is_valid v);
        assert_equal ~printer:string_of_int (-1) (Uriz.Raw.error_offset v)) ]

(* {2 Locality}

   Positive compilation coverage of the [__local] template variants and of
   {!Uriz.of_string_canonical}.  The negative case cannot be written as a test,
   because it is a compile error by construction; kept here as documentation:

   {[
     (* rejected: "This value escapes its region" *)
     let escape () =
       let local_ u = Uriz.of_string_canonical "http://a/" in
       match u with This t -> t | Null -> assert false
   ]}

   Likewise [Uriz.to_string__local] on a local URI yields a local string, so
   returning it from the enclosing region is refused. *)

let local_sink = ref 0

let test_locality =
  [ case "of_string_canonical accepts canonical input" (fun () ->
        List.iter
          (fun s ->
            match Uriz.of_string_canonical s with
            | This u ->
              (* a canonical parse shares the caller's string outright, so
                 physical equality is the strongest check available - and the
                 only one, since a local URI yields a local string *)
              assert_bool ("shares the input: " ^ s)
                (Uriz.to_string__local u == s)
            | Null -> assert_failure (sprintf "%S should parse canonically" s))
          [ "http://example.com/a/b?c=d#e"; "urn:isbn:0451450523"; "";
            "//[2001:db8::1]:443/x"; "/rel?q" ]);
    case "of_string_canonical refuses non-canonical input" (fun () ->
        List.iter
          (fun s ->
            (* of_string still accepts and normalizes these *)
            assert_bool ("of_string accepts " ^ s)
              (match Uriz.of_string s with This _ -> true | Null -> false);
            match Uriz.of_string_canonical s with
            | Null -> ()
            | This _ ->
              assert_failure
                (sprintf "%S is not canonical but was accepted" s))
          [ "HTTP://a/"; "http://A/"; "http://a/%2f"; "http://a/%7E";
            "http://[2001:DB8::1]/" ]);
    case "of_string_canonical refuses invalid input" (fun () ->
        List.iter
          (fun s ->
            match Uriz.of_string_canonical s with
            | Null -> ()
            | This _ -> assert_failure (sprintf "%S should be rejected" s))
          [ "%"; "a b"; "http://[zz]/"; "\000" ]);
    case "of_string_canonical agrees with of_string" (fun () ->
        List.iter
          (fun s ->
            match Uriz.of_string_canonical s with
            | Null -> ()
            | This a -> (
              match Uriz.of_string s with
              | Null -> assert_failure (sprintf "%S: canonical-only" s)
              | This b ->
                assert_bool (sprintf "%S: canonical parse differs" s)
                  (Uriz.to_string__local a == Uriz.to_string b)))
          (valid_uris @ [ resolution_base; "http://a/%2F" ]));
    case "local parse round trip through accessors" (fun () ->
        (* everything below runs on a stack-allocated URI *)
        let n =
          match Uriz.of_string_canonical "http://u@example.com:8080/a/b?k=v#f" with
          | Null -> assert_failure "should parse"
          | This u ->
            assert_equal ~printer:str "http" (opt (Uriz.scheme u));
            assert_equal ~printer:str "example.com" (opt (Uriz.host u));
            assert_equal ~printer:str "u" (opt (Uriz.userinfo u));
            assert_equal ~printer:string_of_int 8080 (Uriz.port_int u);
            assert_equal ~printer:str "/a/b" (Uriz.path u);
            assert_equal ~printer:str "v" (opt (Uriz.find_query u "k"));
            assert_equal ~printer:str "f" (opt (Uriz.fragment u));
            assert_bool "absolute" (Uriz.is_absolute u);
            assert_bool "authority" (Uriz.has_authority u);
            assert_equal ~printer:str "reg-name" (kind_str (Uriz.host_kind u));
            let #(o, l) = Uriz.host_span u in
            assert_equal ~printer:string_of_int 11 l;
            assert_equal ~printer:string_of_int 9 o;
            (* to_string at local mode yields a local string; measuring its
               length keeps it inside the region *)
            String.length (Uriz.to_string__local u)
        in
        assert_equal ~printer:string_of_int 35 n);
    case "local equal/compare/hash" (fun () ->
        match
          Uriz.of_string_canonical "http://a/x", Uriz.of_string_canonical "http://a/x"
        with
        | This a, This b ->
          assert_bool "equal" (Uriz.equal a b);
          assert_equal ~printer:string_of_int 0 (Uriz.compare a b);
          assert_equal ~printer:string_of_int (Uriz.hash a) (Uriz.hash b)
        | _ -> assert_failure "should parse");
    case "of_string__local matches of_string" (fun () ->
        List.iter
          (fun s ->
            let g = match Uriz.of_string s with This u -> Uriz.to_string u | Null -> "!" in
            let l =
              match Uriz.of_string__local s with
              | This u -> Uriz.to_string__local u |> String.length |> string_of_int
              | Null -> "!"
            in
            let expect = if g = "!" then "!" else string_of_int (String.length g) in
            assert_equal ~printer:str ~msg:s expect l)
          (valid_uris @ List.map fst normalizations @ invalid_uris));
    case "local query iteration" (fun () ->
        match Uriz.of_string_canonical "http://a/?x=1&y=2" with
        | Null -> assert_failure "should parse"
        | This u ->
          let n = ref 0 in
          (* the callback is passed at mode local *)
          Uriz.query_iter u (fun ~key:_ ~value:_ -> incr n);
          assert_equal ~printer:string_of_int 2 !n);
    case "of_string_canonical allocates nothing" (fun () ->
        (* the flagship claim: parse, inspect, drop, with no heap traffic *)
        assert_no_alloc "of_string_canonical" (fun () ->
            match Uriz.of_string_canonical alloc_sample with
            | Null -> ()
            | This u ->
              let #(ho, hl) = Uriz.host_span u in
              let #(po, pl) = Uriz.path_span u in
              local_sink :=
                !local_sink + ho + hl + po + pl + Uriz.port_int u
                + String.length (Uriz.to_string__local u)
                + Uriz.query_cursor u
                + (if Uriz.is_absolute u then 1 else 0)));
    case "Raw.parse accepts a local string" (fun () ->
        assert_no_alloc "Raw.parse local" (fun () ->
            let v = Uriz.Raw.parse alloc_sample in
            local_sink := !local_sink + Uriz.Raw.err v + Uriz.Raw.host_len v)) ]

(* {2 Region-allocated producers} *)

let test_region =
  let base = lazy (parse resolution_base) in
  [ case "resolve__local agrees with resolve" (fun () ->
        List.iter
          (fun (r, expected) ->
            let base = Lazy.force base in
            let rel = parse r in
            let got = Uriz.resolve__local ~base rel in
            (* [equal] takes both sides local, so no copy is needed to compare *)
            assert_bool
              (sprintf "resolve__local %S gave %S" r
                 (Uriz.to_string (Uriz.globalize got)))
              (Uriz.equal got (Uriz.resolve ~base rel));
            assert_equal ~printer:str ~msg:r expected
              (Uriz.to_string (Uriz.globalize got)))
          (resolution_normal @ resolution_abnormal));
    case "resolve__local allocates no heap" (fun () ->
        let base = Lazy.force base in
        let rel = parse "../../g" in
        assert_no_alloc "resolve__local" (fun () ->
            let u = Uriz.resolve__local ~base rel in
            local_sink :=
              !local_sink
              + String.length (Uriz.to_string__local u)
              + Uriz.port_int u
              + (if Uriz.is_absolute u then 1 else 0)));
    case "resolve does allocate, for contrast" (fun () ->
        let base = Lazy.force base in
        let rel = parse "../../g" in
        let d1 = alloc_delta 1000 (fun () -> ssink := Uriz.to_string (Uriz.resolve ~base rel)) in
        let d2 = alloc_delta 3000 (fun () -> ssink := Uriz.to_string (Uriz.resolve ~base rel)) in
        assert_bool "global resolve allocates" ((d2 -. d1) /. 2000. > 0.));
    case "normalize__local agrees with normalize" (fun () ->
        List.iter
          (fun (s, expected) ->
            let u = parse s in
            let got = Uriz.normalize__local u in
            assert_bool s (Uriz.equal got (Uriz.normalize u));
            assert_equal ~printer:str ~msg:s expected
              (Uriz.to_string (Uriz.globalize got)))
          [ ("http://a/b/../c", "http://a/c");
            ("http://a/b/./c", "http://a/b/c");
            ("http://a/../../g", "http://a/g");
            ("s:/a/..//b", "s:/.//b");
            (* unchanged: handed straight back, no region use at all *)
            ("http://a/b/c", "http://a/b/c");
            ("../a", "../a") ]);
    case "normalize__local allocates no heap" (fun () ->
        let u = parse "http://a/b/./c/../d" in
        assert_no_alloc "normalize__local" (fun () ->
            let n = Uriz.normalize__local u in
            local_sink :=
              !local_sink + String.length (Uriz.to_string__local n)));
    case "normalize allocates nothing when there is nothing to do" (fun () ->
        let u = parse "http://a/b/c" in
        assert_no_alloc "normalize identity" (fun () ->
            ssink := Uriz.to_string (Uriz.normalize u)));
    case "globalize copies out of the region" (fun () ->
        let base = Lazy.force base in
        let rel = parse "g;x=1/../y" in
        let g = Uriz.globalize (Uriz.resolve__local ~base rel) in
        assert_equal ~printer:str "http://a/b/c/y" (Uriz.to_string g);
        assert_bool "equals the global resolve"
          (Uriz.equal g (Uriz.resolve ~base rel));
        (* the copy is a fresh heap string, not the region one *)
        assert_bool "text is usable after the region ends"
          (String.length (Uriz.to_string g) = 14));
    case "globalize preserves every component" (fun () ->
        let u = parse "http://u@h:8080/a/b?k=v#f" in
        let g = Uriz.globalize u in
        assert_equal ~printer:str (Uriz.to_string u) (Uriz.to_string g);
        assert_equal ~printer:str (opt (Uriz.userinfo u)) (opt (Uriz.userinfo g));
        assert_equal ~printer:str (opt (Uriz.host u)) (opt (Uriz.host g));
        assert_equal ~printer:string_of_int (Uriz.port_int u) (Uriz.port_int g);
        assert_equal ~printer:str (Uriz.path u) (Uriz.path g);
        assert_equal ~printer:str (opt (Uriz.query u)) (opt (Uriz.query g));
        assert_equal ~printer:str (opt (Uriz.fragment u)) (opt (Uriz.fragment g));
        assert_equal ~printer:str "reg-name" (kind_str (Uriz.host_kind g)));
    case "with_ and make have local variants" (fun () ->
        let u = parse "http://a/b?q#f" in
        let n =
          String.length (Uriz.to_string__local (Uriz.with_scheme__local u (This "https")))
          + String.length (Uriz.to_string__local (Uriz.with_port__local u (This 99)))
          + String.length (Uriz.to_string__local (Uriz.with_path__local u "/z"))
          + String.length (Uriz.to_string__local (Uriz.with_query__local u Null))
          + String.length
              (Uriz.to_string__local
                 (Uriz.make__local ~scheme:"http" ~host:"a" ~path:"/b" ()))
        in
        assert_equal ~printer:string_of_int 68 n;
        assert_bool "with_scheme__local agrees"
          (Uriz.equal
             (Uriz.with_scheme__local u (This "https"))
             (Uriz.with_scheme u (This "https"))));
    case "of_string_exn has a local variant" (fun () ->
        let n =
          String.length (Uriz.to_string__local (Uriz.of_string_exn__local "http://a/x"))
        in
        assert_equal ~printer:string_of_int 10 n;
        assert_raises
          (Invalid_argument "Uriz.of_string_exn: not a URI reference: %")
          (fun () -> Uriz.of_string_exn "%")) ]

(* {2 Windowed parsing}

   [parse_sub] is what lets a server scan a request-target in place inside its
   connection buffer, so the two properties tested here are the ones it rests
   on: the result is [parse] of the window's contents with every offset shifted
   by [pos], and no byte outside the window is ever read.  The padding is made
   of the bytes most likely to be picked up by an over-read - ['%'], ['['],
   ['?'], ['#'] and friends. *)

let junk n = String.init n (fun i -> "%:@?#[]./aF0+".[i mod 13])

let spans_agree ~msg ~pos (w : Uriz.Raw.spans) (v : Uriz.Raw.spans) =
  let shift x = if x < 0 then x else x + pos in
  let eq name a b =
    assert_equal ~printer:string_of_int ~msg:(msg ^ ": " ^ name) a b
  in
  let eqb name a b = assert_equal ~printer:string_of_bool ~msg:(msg ^ ": " ^ name) a b in
  let e = Uriz.Raw.err w in
  eq "err" (if e = 0 then 0 else e + pos) (Uriz.Raw.err v);
  eqb "needs_normalization" (Uriz.Raw.needs_normalization w)
    (Uriz.Raw.needs_normalization v);
  eq "shrink" (Uriz.Raw.shrink w) (Uriz.Raw.shrink v);
  (* the one relative field: a length, so it does not shift *)
  eq "scheme_len" (Uriz.Raw.scheme_len w) (Uriz.Raw.scheme_len v);
  eq "userinfo_off" (shift (Uriz.Raw.userinfo_off w)) (Uriz.Raw.userinfo_off v);
  eq "userinfo_len" (Uriz.Raw.userinfo_len w) (Uriz.Raw.userinfo_len v);
  eq "host_off" (shift (Uriz.Raw.host_off w)) (Uriz.Raw.host_off v);
  eq "host_len" (Uriz.Raw.host_len w) (Uriz.Raw.host_len v);
  eq "host_kind" (Uriz.Raw.host_kind w) (Uriz.Raw.host_kind v);
  eq "port_off" (shift (Uriz.Raw.port_off w)) (Uriz.Raw.port_off v);
  eq "port_len" (Uriz.Raw.port_len w) (Uriz.Raw.port_len v);
  eq "port_val" (Uriz.Raw.port_val w) (Uriz.Raw.port_val v);
  eq "path_off" (shift (Uriz.Raw.path_off w)) (Uriz.Raw.path_off v);
  eq "path_len" (Uriz.Raw.path_len w) (Uriz.Raw.path_len v);
  eq "query_off" (shift (Uriz.Raw.query_off w)) (Uriz.Raw.query_off v);
  eq "query_len" (Uriz.Raw.query_len w) (Uriz.Raw.query_len v);
  eq "frag_off" (shift (Uriz.Raw.frag_off w)) (Uriz.Raw.frag_off v);
  eq "frag_len" (Uriz.Raw.frag_len w) (Uriz.Raw.frag_len v)

let window_corpus =
  valid_uris @ invalid_uris
  @ List.map fst normalizations
  @ [ resolution_base;
      "http://a/b/../c";
      "../../g";
      "./a";
      "//[2001:db8::1]:443/x";
      "//[::ffff:192.0.2.1]";
      "//[v7.a:b]/";
      "http://u:p@a:80/p?q#f";
      "http://a/?";
      "http://a/?#";
      "a%20b";
      "http://a/%2f%7E";
      "http://a:99999999999999999999999999/";
      "http://[::1";
      "urn:isbn:0451450523" ]

let test_parse_sub =
  [ case "parse_sub agrees with parse at every offset" (fun () ->
        List.iter
          (fun s ->
            let n = String.length s in
            let w = Uriz.Raw.parse s in
            List.iter
              (fun pos ->
                List.iter
                  (fun tail ->
                    let buf = junk pos ^ s ^ junk tail in
                    let v = Uriz.Raw.parse_sub buf ~pos ~len:n in
                    spans_agree
                      ~msg:(sprintf "%S at %d, %d trailing" s pos tail)
                      ~pos w v)
                  [ 0; 1; 3; 16 ])
              [ 0; 1; 2; 3; 5; 8; 17 ])
          window_corpus);
    case "parse_sub at the very end of a buffer" (fun () ->
        List.iter
          (fun s ->
            let pos = 11 in
            let buf = junk pos ^ s in
            let v = Uriz.Raw.parse_sub buf ~pos ~len:(String.length s) in
            spans_agree ~msg:(sprintf "%S at the end" s) ~pos (Uriz.Raw.parse s) v)
          window_corpus);
    case "parse_sub of a truncated window" (fun () ->
        (* every prefix of a full URI, read out of the middle of a buffer whose
           remaining bytes would change the answer if they were touched *)
        List.iter
          (fun s ->
            let n = String.length s in
            let pos = 7 in
            let buf = junk pos ^ s ^ junk 9 in
            for len = 0 to n do
              let v = Uriz.Raw.parse_sub buf ~pos ~len in
              spans_agree
                ~msg:(sprintf "%S truncated to %d" s len)
                ~pos
                (Uriz.Raw.parse (String.sub s 0 len))
                v
            done)
          [ "http://user:pw@example.com:8080/a/b?q=1#f";
            "//[2001:db8::1]:443/x?y#z"; "urn:isbn:0451450523"; "a%20b%2F";
            "http://a/%2f" ]);
    case "parse_sub rejects an out-of-range window" (fun () ->
        let s = "http://a/b" in
        let bad =
          [ (-1, 3); (0, -1); (0, 11); (5, 6); (11, 0); (max_int, 1);
            (1, max_int) ]
        in
        List.iter
          (fun (pos, len) ->
            let v = Uriz.Raw.parse_sub s ~pos ~len in
            assert_bool
              (sprintf "pos=%d len=%d should be rejected" pos len)
              (not (Uriz.Raw.is_valid v));
            assert_equal ~printer:string_of_int
              ~msg:(sprintf "pos=%d len=%d err" pos len)
              1 (Uriz.Raw.err v))
          bad;
        (* the empty window at the very end is in range, and is the empty URI *)
        assert_bool "empty window at the end"
          (Uriz.Raw.is_valid (Uriz.Raw.parse_sub s ~pos:10 ~len:0)));
    case "parse_sub allocates nothing" (fun () ->
        let buf = junk 5 ^ alloc_sample ^ junk 5 in
        let n = String.length alloc_sample in
        assert_no_alloc "Raw.parse_sub" (fun () ->
            let v = Uriz.Raw.parse_sub buf ~pos:5 ~len:n in
            sink :=
              !sink + Uriz.Raw.err v + Uriz.Raw.host_off v + Uriz.Raw.path_len v)) ]

(* {2 In-place percent decoding} *)

let decode_corpus =
  [ ""; "abc"; "a%20b"; "%7E"; "%2F"; "%2f"; "a+b"; "+"; "++"; "a%2Bb";
    "%20+%20"; "%41%42%43"; "x=1&y=2"; "%00"; "%ff"; "%FF"; "100%"; "%"; "%4";
    "%zz"; "%4z"; "a%"; "a%1"; "%%41"; "%GZ"; "%2"; "+%2B+" ]

let test_pct_decode_into =
  List.concat_map
    (fun plus_as_space ->
      [ case (sprintf "pct_decode_into agrees with pct_decode (plus=%b)" plus_as_space)
          (fun () ->
            List.iter
              (fun s ->
                let n = String.length s in
                let buf = junk 3 ^ s ^ junk 4 in
                let dst = Bytes.make (n + 8) '\xee' in
                let r =
                  Uriz.Raw.pct_decode_into buf ~pos:3 ~len:n ~dst ~dst_pos:2
                    ~plus_as_space
                in
                match Uriz.pct_decode ~plus_as_space s with
                | Null ->
                  assert_equal ~printer:string_of_int
                    ~msg:(sprintf "%S is malformed" s) (-1) r
                | This d ->
                  assert_equal ~printer:string_of_int
                    ~msg:(sprintf "%S written" s) (String.length d) r;
                  assert_equal ~printer:str ~msg:(sprintf "%S bytes" s) d
                    (Bytes.sub_string dst 2 r);
                  assert_equal ~printer:str ~msg:(sprintf "%S before dst_pos" s)
                    "\xee\xee" (Bytes.sub_string dst 0 2))
              decode_corpus);
        case (sprintf "needs_decode agrees with pct_decode (plus=%b)" plus_as_space)
          (fun () ->
            List.iter
              (fun s ->
                let buf = junk 3 ^ s ^ junk 4 in
                let got =
                  Uriz.Raw.needs_decode buf ~pos:3 ~len:(String.length s)
                    ~plus_as_space
                in
                let want =
                  match Uriz.pct_decode ~plus_as_space s with
                  | Null -> true
                  | This d -> not (String.equal d s)
                in
                assert_equal ~printer:string_of_bool
                  ~msg:(sprintf "%S plus=%b" s plus_as_space)
                  want got)
              decode_corpus) ])
    [ false; true ]
  @ [ case "pct_decode_into stops at the window edge" (fun () ->
          (* the '4' and '1' are in the buffer but outside the window, so the
             truncated triplet must fail rather than reach for them *)
          let buf = "xxa%41yy" in
          assert_equal ~printer:string_of_int (-1)
            (Uriz.Raw.pct_decode_into buf ~pos:2 ~len:2
               ~dst:(Bytes.create 8) ~dst_pos:0 ~plus_as_space:false);
          assert_equal ~printer:string_of_int (-1)
            (Uriz.Raw.pct_decode_into buf ~pos:2 ~len:3
               ~dst:(Bytes.create 8) ~dst_pos:0 ~plus_as_space:false);
          let dst = Bytes.create 8 in
          assert_equal ~printer:string_of_int 2
            (Uriz.Raw.pct_decode_into buf ~pos:2 ~len:4 ~dst ~dst_pos:0
               ~plus_as_space:false);
          assert_equal ~printer:str "aA" (Bytes.sub_string dst 0 2);
          assert_bool "needs_decode stops too"
            (not (Uriz.Raw.needs_decode buf ~pos:2 ~len:1 ~plus_as_space:true)));
      case "pct_decode_into refuses a short destination" (fun () ->
          assert_equal ~printer:string_of_int (-1)
            (Uriz.Raw.pct_decode_into "abcd" ~pos:0 ~len:4
               ~dst:(Bytes.create 3) ~dst_pos:0 ~plus_as_space:false);
          assert_equal ~printer:string_of_int (-1)
            (Uriz.Raw.pct_decode_into "abcd" ~pos:0 ~len:4
               ~dst:(Bytes.create 4) ~dst_pos:1 ~plus_as_space:false);
          assert_equal ~printer:string_of_int (-1)
            (Uriz.Raw.pct_decode_into "abcd" ~pos:0 ~len:4
               ~dst:(Bytes.create 8) ~dst_pos:(-1) ~plus_as_space:false);
          (* an exact fit is enough, since decoding never lengthens *)
          assert_equal ~printer:string_of_int 4
            (Uriz.Raw.pct_decode_into "abcd" ~pos:0 ~len:4
               ~dst:(Bytes.create 4) ~dst_pos:0 ~plus_as_space:false));
      case "pct_decode_into refuses an out-of-range window" (fun () ->
          List.iter
            (fun (pos, len) ->
              assert_equal ~printer:string_of_int
                ~msg:(sprintf "pos=%d len=%d" pos len)
                (-1)
                (Uriz.Raw.pct_decode_into "abcd" ~pos ~len
                   ~dst:(Bytes.create 64) ~dst_pos:0 ~plus_as_space:false))
            [ (-1, 2); (0, -1); (0, 5); (3, 2); (5, 0) ];
          assert_bool "needs_decode is false out of range"
            (not (Uriz.Raw.needs_decode "a%b" ~pos:0 ~len:9 ~plus_as_space:false)));
      case "pct_decode_into allocates nothing" (fun () ->
          let src = "/a%20b/c%2Fd+e" in
          let n = String.length src in
          let dst = Bytes.create 32 in
          assert_no_alloc "Raw.pct_decode_into" (fun () ->
              sink :=
                !sink
                + Uriz.Raw.pct_decode_into src ~pos:0 ~len:n ~dst ~dst_pos:0
                    ~plus_as_space:true
                + (if Uriz.Raw.needs_decode src ~pos:0 ~len:n ~plus_as_space:false
                   then 1
                   else 0))) ]

(* {2 The form-urlencoded '+' rule} *)

let plus_query = "?q=a+b&x%2By=1&a+b=2"

let test_plus_as_space =
  [ case "pct_decode plus_as_space" (fun () ->
        assert_equal ~printer:str "a+b" (opt (Uriz.pct_decode "a+b"));
        assert_equal ~printer:str "a b"
          (opt (Uriz.pct_decode ~plus_as_space:true "a+b"));
        (* an encoded '+' is a literal '+' under either rule *)
        assert_equal ~printer:str "a+b"
          (opt (Uriz.pct_decode ~plus_as_space:true "a%2Bb"));
        assert_equal ~printer:str "a+b" (opt (Uriz.pct_decode "a%2Bb"));
        assert_equal ~printer:str "<absent>"
          (opt (Uriz.pct_decode ~plus_as_space:true "%zz")));
    case "pct_decode still shares when there is nothing to do" (fun () ->
        let s = "no-escapes-here" in
        match Uriz.pct_decode ~plus_as_space:true s with
        | This d -> assert_bool "physically equal" (d == s)
        | Null -> assert_failure "unexpected Null");
    case "find_query plus_as_space" (fun () ->
        let u = parse plus_query in
        assert_equal ~printer:str "a+b" (opt (Uriz.find_query u "q"));
        assert_equal ~printer:str "a b"
          (opt (Uriz.find_query ~plus_as_space:true u "q"));
        (* %2B is a literal '+' in a key either way *)
        assert_equal ~printer:str "1" (opt (Uriz.find_query u "x+y"));
        assert_equal ~printer:str "1"
          (opt (Uriz.find_query ~plus_as_space:true u "x+y"));
        (* a bare '+' in a key is a space only under the form rule *)
        assert_equal ~printer:str "2" (opt (Uriz.find_query u "a+b"));
        assert_equal ~printer:str "<absent>" (opt (Uriz.find_query u "a b"));
        assert_equal ~printer:str "<absent>"
          (opt (Uriz.find_query ~plus_as_space:true u "a+b"));
        assert_equal ~printer:str "2"
          (opt (Uriz.find_query ~plus_as_space:true u "a b")));
    case "query_iter plus_as_space" (fun () ->
        let u = parse plus_query in
        let collect ?plus_as_space () =
          let acc = ref [] in
          Uriz.query_iter ?plus_as_space u (fun ~key ~value ->
              acc := (key, opt value) :: !acc);
          List.rev !acc
        in
        let pr l = String.concat "," (List.map (fun (k, v) -> k ^ "=" ^ v) l) in
        assert_equal ~printer:pr
          [ ("q", "a+b"); ("x+y", "1"); ("a+b", "2") ]
          (collect ());
        assert_equal ~printer:pr
          [ ("q", "a b"); ("x+y", "1"); ("a b", "2") ]
          (collect ~plus_as_space:true ()));
    case "query_step is unaffected by '+'" (fun () ->
        let u = parse plus_query in
        let raw = Uriz.to_string u in
        let #(ko, kl, vo, vl, _) = Uriz.query_step u (Uriz.query_cursor u) in
        assert_equal ~printer:str "q" (String.sub raw ko kl);
        assert_equal ~printer:str "a+b" (String.sub raw vo vl));
    case "'+' is never a space outside the query" (fun () ->
        assert_equal ~printer:str "/a+b"
          (Uriz.path_decoded (parse "http://h/a+b"));
        assert_equal ~printer:str "a+b"
          (opt (Uriz.fragment_decoded (parse "http://h/#a+b")));
        assert_equal ~printer:str "a+b"
          (opt (Uriz.userinfo_decoded (parse "http://a+b@h/")))) ]

(* {2 Exhaustive character classes}

   The scanners decide set membership by indexing a byte table, so a single
   wrong bit would quietly change the accepted language for one code point.
   These pin all 256 of them against the RFC 3986 ABNF transcribed
   independently here, each in a context where one class alone decides the
   outcome. *)

let r_alpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
let r_digit c = c >= '0' && c <= '9'

let r_unreserved c =
  r_alpha c || r_digit c || c = '-' || c = '.' || c = '_' || c = '~'

let r_sub_delim c = String.contains "!$&'()*+,;=" c
let r_base c = r_unreserved c || r_sub_delim c
let one c = String.make 1 c

(* [make c] places [c] where class membership alone decides whether the
   reference parses, and [component] is the text that component must then
   hold.  Demanding the exact text rather than mere acceptance stops a
   delimiter that happens to leave the reference valid - ['/'] in a userinfo
   position, say - from passing itself off as a member. *)
let class_probe ~name ~make ~component ~get ~in_class =
  case name (fun () ->
      for i = 0 to 255 do
        let c = Char.chr i in
        let got =
          match Uriz.of_string (make c) with
          | Null -> false
          | This u -> (
            match get u with
            | Null -> false
            | This v -> String.equal v (component c))
        in
        assert_equal ~printer:string_of_bool
          ~msg:(sprintf "%s: byte %d (%C) in %S" name i c (make c))
          (in_class c) got
      done)

let test_char_classes =
  [ class_probe ~name:"class:userinfo"
      ~make:(fun c -> "//" ^ one c ^ "@h")
      ~component:one ~get:Uriz.userinfo
      ~in_class:(fun c -> r_base c || c = ':');
    class_probe ~name:"class:reg-name"
      ~make:(fun c -> "//" ^ one c)
      ~component:(fun c -> one (Char.lowercase_ascii c))
      ~get:Uriz.host ~in_class:r_base;
    (* [path-abempty] is pchar with ['/'] between segments, and no context can
       tell a ['/'] that is a segment separator from one that is not. *)
    class_probe ~name:"class:pchar"
      ~make:(fun c -> "http://h/" ^ one c)
      ~component:(fun c -> "/" ^ one c)
      ~get:(fun u -> This (Uriz.path u))
      ~in_class:(fun c -> r_base c || c = ':' || c = '@' || c = '/');
    class_probe ~name:"class:query"
      ~make:(fun c -> "http://h/?" ^ one c)
      ~component:one ~get:Uriz.query
      ~in_class:(fun c -> r_base c || c = ':' || c = '@' || c = '/' || c = '?');
    class_probe ~name:"class:fragment"
      ~make:(fun c -> "http://h/#" ^ one c)
      ~component:one ~get:Uriz.fragment
      ~in_class:(fun c -> r_base c || c = ':' || c = '@' || c = '/' || c = '?');
    class_probe ~name:"class:segment-nz-nc"
      ~make:(fun c -> one c ^ "/x")
      ~component:(fun c -> one c ^ "/x")
      ~get:(fun u -> This (Uriz.path u))
      ~in_class:(fun c -> r_base c || c = '@');
    class_probe ~name:"class:ipvfuture"
      ~make:(fun c -> "//[v7." ^ one c ^ "]")
      ~component:(fun c -> "v7." ^ one c)
      ~get:Uriz.host
      ~in_class:(fun c -> r_base c || c = ':');
    (* The [segment-nz-nc] charset covers the first segment only, so a ':'
       that is illegal at the start of a path-noscheme is legal one segment
       later. *)
    case "class:segment-nz-nc ends at the first '/'" (fun () ->
        assert_equal ~printer:str "a/:b" (Uriz.path (parse "a/:b"));
        assert_equal ~printer:str "./a:b" (Uriz.to_string (parse "./a:b"));
        List.iter
          (fun s ->
            match Uriz.of_string s with
            | Null -> ()
            | This u ->
              assert_failure (sprintf "%S should be rejected, got %S" s
                                (Uriz.to_string u)))
          [ ":/x"; ":a"; ":" ]);
    case "hex digits agree with the ABNF" (fun () ->
        for i = 0 to 255 do
          let c = Char.chr i in
          let want =
            if r_digit c then Char.code c - 48
            else if c >= 'A' && c <= 'F' then Char.code c - 55
            else if c >= 'a' && c <= 'f' then Char.code c - 87
            else -1
          in
          assert_equal ~printer:string_of_int
            ~msg:(sprintf "hex_val %d (%C)" i c)
            want (Uriz.Raw.hex_val c);
          assert_equal ~printer:string_of_bool
            ~msg:(sprintf "is_hexdig %d (%C)" i c)
            (want >= 0) (Uriz.Raw.is_hexdig c)
        done);
    case "byte predicates agree with the ABNF" (fun () ->
        for i = 0 to 255 do
          let c = Char.chr i in
          let eq name a b =
            assert_equal ~printer:string_of_bool
              ~msg:(sprintf "%s %d (%C)" name i c)
              a b
          in
          eq "is_alpha" (r_alpha c) (Uriz.Raw.is_alpha c);
          eq "is_digit" (r_digit c) (Uriz.Raw.is_digit c);
          eq "is_unreserved" (r_unreserved c) (Uriz.Raw.is_unreserved c);
          eq "is_sub_delim" (r_sub_delim c) (Uriz.Raw.is_sub_delim c)
        done) ]

(* {2 Round-trip} *)

let test_roundtrip =
  [ case "parse is a fixpoint on its own output" (fun () ->
        List.iter
          (fun s ->
            let once = Uriz.to_string (parse s) in
            let twice = Uriz.to_string (parse once) in
            assert_equal ~printer:str ~msg:s once twice)
          (valid_uris
          @ List.map fst normalizations
          @ [ resolution_base; "http://a/%2F"; "//[2001:db8::1]:443/x" ])) ]

let suite =
  "uri"
  >::: test_accept @ test_reject @ test_decompose @ test_empty_vs_absent
       @ test_normalize_syntax @ test_normalize_idempotent @ test_dot_segments
       @ test_ipv4 @ test_ipv6 @ test_host_kinds @ test_pct @ test_query
       @ test_resolution @ test_resolution_misc @ test_identity @ test_make
       @ test_with @ test_decoded @ test_allocation @ test_sharing
       @ test_or_null @ test_locality @ test_region @ test_parse_sub
       @ test_pct_decode_into @ test_plus_as_space @ test_char_classes
       @ test_roundtrip

let () = run_test_tt_main suite
