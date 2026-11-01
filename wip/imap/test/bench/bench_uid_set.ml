(* Parse, union, probe and print UID sets of 100,000 UIDs held in 10,000
   intervals of ten. *)

let intervals = 10_000
let probes = 10_000

let wire offset =
  String.concat "," (List.init intervals (fun i ->
    let first = 20 * i + 1 + offset in
    Printf.sprintf "%d:%d" first (first + 9)))

let left = wire 0
let right = wire 5

let parse s =
  match Imap.Uid_set.of_wire s with Ok set -> set | Error e -> failwith e

let uid n =
  match Imap.Uid.of_int64 (Int64.of_int n) with
  | Ok u -> u
  | Error e -> failwith e

let probe_uids = Array.init probes (fun k -> uid (20 * k + k mod 20 + 1))

let workload () =
  let a = parse left and b = parse right in
  let u = Imap.Uid_set.union a b in
  let hits = ref 0 in
  Array.iter (fun p -> if Imap.Uid_set.mem p u then incr hits) probe_uids;
  let wa = Imap.Uid_set.to_wire a and wu = Imap.Uid_set.to_wire u in
  Imap.Uid_set.cardinality a, Imap.Uid_set.cardinality u, !hits,
  String.length wa, String.length wu

let () =
  let ca, cu, hits, la, _ =
    Bench_measure.run "uid_set 100000 UIDs in 10000 intervals" workload in
  if ca <> 100_000L || cu <> 150_000L || hits <> 7_500
     || la <> String.length left
  then failwith "uid_set result"
