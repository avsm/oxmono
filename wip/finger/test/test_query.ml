(* RFC 1288 query-line parsing and its wire-format printer. *)

module Q = Finger.Query

let failures = ref 0

let query ?(verbose = false) ?host user : Q.t = { verbose; user; host }

let show (q : Q.t) =
  Printf.sprintf "{verbose=%b; user=%S; host=%s}" q.verbose q.user
    (match q.host with None -> "None" | Some h -> Printf.sprintf "Some %S" h)

let parses line expected =
  let got = Q.parse line in
  if got <> expected then (
    incr failures;
    Printf.eprintf "FAIL: parse %S = %s, expected %s\n" line (show got)
      (show expected))

let prints q expected =
  let got = Format.asprintf "%a" Q.pp q in
  if got <> expected then (
    incr failures;
    Printf.eprintf "FAIL: pp %s = %S, expected %S\n" (show q) got expected)

let () =
  (* Null queries list users. *)
  parses "" (query "");
  parses "\r\n" (query "");
  parses "/W\r\n" (query ~verbose:true "");
  parses "/w \r\n" (query ~verbose:true "");
  (* Local user queries, with any line ending. *)
  parses "anil\r\n" (query "anil");
  parses "anil\n" (query "anil");
  parses "anil\r" (query "anil");
  parses "anil" (query "anil");
  parses "/W anil\r\n" (query ~verbose:true "anil");
  parses "  anil  \r\n" (query "anil");
  (* Forwarding queries go to the rightmost host, which receives the rest. *)
  parses "anil@host\r\n" (query ~host:"host" "anil");
  parses "@host\r\n" (query ~host:"host" "");
  parses "/W anil@host\r\n" (query ~verbose:true ~host:"host" "anil");
  parses "anil@a@b\r\n" (query ~host:"b" "anil@a");
  parses "anil@\r\n" (query "anil");
  (* Only a leading /W is the verbose flag. *)
  parses "W anil\r\n" (query "W anil");
  prints (query "") "";
  prints (query ~verbose:true "") "/W ";
  prints (query "anil") "anil";
  prints (query ~verbose:true ~host:"host" "anil") "/W anil@host";
  prints (query ~host:"host" "") "@host";
  (* Printing then parsing is the identity on parsed queries. *)
  List.iter
    (fun line ->
      let q = Q.parse line in
      parses (Format.asprintf "%a\r\n" Q.pp q) q)
    [ ""; "/W"; "anil"; "/W anil@host"; "@host"; "anil@a@b" ];
  if !failures > 0 then exit 1;
  print_endline "test_query: finger query parsing passed"
