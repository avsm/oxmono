(* Compile-time probes of the kind and mode claims in the protocol
   interfaces. Each claim is a module-level value captured by a
   [@ portable] closure, which compiles only when its type crosses
   portability and contention, or a local value returned at global mode,
   which compiles only when its type crosses locality. Running the probes
   checks that the captured values are intact. *)

let get = function Ok x -> x | Error e -> failwith e

let uid = get (Imap.Uid.of_int64 4_294_967_295L)
let uidvalidity = get (Imap.Uidvalidity.of_int64 7L)
let seq = get (Imap.Seq.of_int64 42L)

let global_uid (u : Imap.Uid.t @ local) : Imap.Uid.t = u
let global_uidvalidity (v : Imap.Uidvalidity.t @ local) : Imap.Uidvalidity.t
    = v
let global_seq (n : Imap.Seq.t @ local) : Imap.Seq.t = n

let immediates = fun () @ portable -> uid, uidvalidity, seq

let test_immediates () =
  let u, v, n = immediates () in
  let u = global_uid u and v = global_uidvalidity v and n = global_seq n in
  Alcotest.(check int64) "uid" 4_294_967_295L (Imap.Uid.to_int64 u);
  Alcotest.(check int64) "uidvalidity" 7L (Imap.Uidvalidity.to_int64 v);
  Alcotest.(check int64) "seq" 42L (Imap.Seq.to_int64 n)

let () =
  Alcotest.run "IMAP kinds and modes"
    [ "immediates", [ Alcotest.test_case "cross every mode" `Quick
                        test_immediates ] ]
