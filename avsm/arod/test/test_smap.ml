(* [Bushel.Smap] is a hand-written search tree, and every slug and image
   lookup in bushel and every feed and link lookup in arod now goes through
   it. It exists because no stdlib container declares a kind on its type, so
   neither a [Hashtbl.t] nor a [Map.S.t] can be read from a portable closure
   that captured it. The golden fixtures reach it three calls deep and could
   not tell a lookup bug from a rendering one, so it is guarded here directly.

   [Map.Make (String)] is the oracle. The two agree on everything except
   duplicate keys, and that difference is the load-bearing case: [of_list]
   must keep the LAST binding for a repeated key, because that is what
   [Hashtbl.add] followed by [Hashtbl.find] did in [Bushel_entry.v]. A slug
   claimed by both a note and a paper has to keep resolving to the paper.

   The seed is fixed so that a failure here is reproducible. *)

module M = Map.Make (String)
module S = Bushel.Smap

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

(* The shallowest depth any binary tree over [n] nodes can have. *)
let ceil_log2 n =
  let rec go acc p = if p >= n then acc else go (acc + 1) (p * 2) in
  go 0 1

let oracle l = List.fold_left (fun m (k, v) -> M.add k v m) M.empty l

(* [agree name l probes] requires the tree built from [l] to answer every probe
   as the oracle does, to hold its bindings in key order, and to be as shallow
   as its size allows. Building the oracle by folding [M.add] over [l] in order
   is what makes it check the last-binding-wins rule too. *)
let agree name l probes =
  let t = S.of_list l and o = oracle l in
  let n = M.cardinal o in
  check (name ^ ": bindings match the oracle, in key order")
    (S.bindings t = M.bindings o);
  check (name ^ ": depth is the minimum for the number of keys")
    (S.depth t = ceil_log2 (n + 1));
  List.iter
    (fun k ->
      check
        (Printf.sprintf "%s: find_opt %S" name k)
        (S.find_opt k t = M.find_opt k o);
      check
        (Printf.sprintf "%s: find %S" name k)
        ((try Some (S.find k t) with Not_found -> None) = M.find_opt k o))
    probes

let asc n = List.init n (fun i -> (Printf.sprintf "k%06d" i, i))

let () =
  Random.init 20260819;

  (* Structural edge cases. The gap probes matter because a lookup that walked
     the wrong way at a node would still find the keys that are present. *)
  agree "empty" [] [ ""; "a"; "zzz" ];
  check "empty is the empty tree" (S.of_list [] = S.empty);
  check "empty has no bindings" (S.bindings S.empty = []);
  check "find on empty raises"
    (match S.find "a" S.empty with
     | exception Not_found -> true
     | _ -> false);
  agree "singleton" [ ("m", 1) ] [ ""; "a"; "m"; "mm"; "z" ];
  agree "keys with gaps between them"
    [ ("a", 1); ("c", 2); ("e", 3); ("g", 4) ]
    [ ""; "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "zz" ];

  (* Duplicates. [of_list] sorts stably and then collapses each run of equal
     keys to its last element, so the input order has to survive the sort. *)
  agree "a key repeated three times" [ ("k", 1); ("k", 2); ("k", 3) ] [ "k"; "j" ];
  check "the last binding wins"
    (S.find_opt "k" (S.of_list [ ("k", 1); ("k", 2); ("k", 3) ]) = Some 3);
  check "the last binding wins when the repeats are interleaved"
    (S.find_opt "b"
       (S.of_list [ ("b", 1); ("a", 9); ("b", 2); ("c", 8); ("b", 3) ])
     = Some 3);
  let all_one_key = List.init 500 (fun i -> ("same", i)) in
  agree "five hundred bindings for one key" all_one_key [ "sam"; "same"; "samee" ];
  check "five hundred bindings collapse to the last"
    (S.find_opt "same" (S.of_list all_one_key) = Some 499);

  (* The slug case [Bushel_entry.v] depends on, spelled out. The five kinds are
     added note, project, idea, video, paper, so a paper shadows a note. *)
  check "a paper shadows a note that claimed the same slug"
    (S.find_opt "a" (S.of_list [ ("a", `Note); ("a", `Paper) ]) = Some `Paper);

  (* Ascending and descending input are what would degenerate a tree built by
     repeated insertion into a list. [agree] asserts the exact floor. *)
  List.iter
    (fun n ->
      let l = asc n in
      let probes = List.map fst l @ [ "a"; "k000000x"; "zzz" ] in
      agree (Printf.sprintf "%d keys in ascending order" n) l probes;
      agree (Printf.sprintf "%d keys in descending order" n) (List.rev l) probes)
    [ 1; 2; 3; 7; 8; 9; 15; 16; 1000 ];
  check "a thousand ascending keys are ten deep, not a thousand"
    (S.depth (S.of_list (asc 1000)) = 10);

  (* Randomised differential. The keyspace is small relative to the number of
     bindings so that duplicates and misses are both frequent, and the probes
     run past both ends of it. *)
  for trial = 1 to 300 do
    let n = 1 + Random.int 200 in
    let keyspace = 1 + Random.int 40 in
    let l = List.init n (fun i -> (Printf.sprintf "k%d" (Random.int keyspace), i)) in
    let probes =
      List.init (keyspace + 10) (fun i -> Printf.sprintf "k%d" (i - 5))
      @ [ ""; "k"; "zzzz" ]
    in
    agree (Printf.sprintf "random trial %d" trial) l probes
  done;

  (* Keys that are arbitrary bytes, since slugs and URLs are not checked. *)
  let bytes =
    List.init 2000 (fun i ->
        let len = 1 + Random.int 6 in
        (String.init len (fun _ -> Char.chr (Random.int 256)), i))
  in
  agree "two thousand arbitrary byte keys" bytes
    (List.map fst bytes @ [ ""; "\000"; "\255\255" ]);

  Printf.printf "test_smap: %d checks ok\n" !checks
