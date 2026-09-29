(* Encode 100,000 search criteria and 100,000 FETCH items. *)

let count = 100_000

let flag s =
  match Mail_flag.Imap_flag.of_wire s with Ok f -> f | Error e -> failwith e

let uid_set s =
  match Imap.Uid_set.of_wire s with Ok set -> set | Error e -> failwith e

let modseq n =
  match Imap.Modseq.of_int64 n with Ok m -> m | Error e -> failwith e

let criteria =
  let open Imap.Search in
  Array.init 64 (fun i ->
    And [
      Uid (uid_set (Printf.sprintf "%d:%d,%d" (i + 1) (i + 100) (i + 500)));
      Since { day = 1 + i mod 28; month = 1 + i mod 12; year = 2020 };
      Not Seen;
      Subject (Printf.sprintf "report %d" i);
      Or (From "alice@example.org", To "bob@example.org");
      Header ("X-Mailer", "bench");
      Larger (Int64.of_int (1000 * i));
      Keyword (flag "$Important");
      Modseq (modseq (Int64.of_int (i + 1)));
    ])

let items =
  let open Imap.Fetch_item in
  [| Uid; Flags; Internal_date; Rfc822_size; Envelope; Bodystructure;
     Modseq; Emailid; Threadid; Objectid; Preview { lazy_ = true };
     Binary_size [1; 2] |]

let search () =
  let bytes = ref 0 in
  for i = 0 to count - 1 do
    match Imap.Search.to_wire ~utf8:false criteria.(i land 63) with
    | Ok s -> bytes := !bytes + String.length s
    | Error e -> failwith (Imap.Search.error_to_string e)
  done;
  !bytes

let fetch_items () =
  let bytes = ref 0 in
  for i = 0 to count - 1 do
    bytes := !bytes + String.length
      (Imap.Fetch_item.to_wire items.(i mod Array.length items))
  done;
  !bytes

let () =
  let s = Bench_measure.run "Search.to_wire 100000 criteria" search in
  let f = Bench_measure.run "Fetch_item.to_wire 100000 items" fetch_items in
  if s = 0 || f = 0 then failwith "empty encoding"
