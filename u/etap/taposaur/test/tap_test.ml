(* Helpers shared by the Taposaur tests. *)

(* [get r] is the value of [r]. It fails the test with the message of an
   [Error]. *)
let get = function Ok x -> x | Error e -> Alcotest.fail e

(* [with_temp prefix f] runs [f env sw root name] under Eio with a fresh
   private directory [root] at the native path [name], whose basename starts
   with [prefix]. The directory is removed when [f] returns or raises. *)
let with_temp prefix f =
  Eio_main.run @@ fun env ->
  let name = Filename.temp_file prefix "" in
  Unix.unlink name;
  let root = Eio.Path.(env#fs / name) in
  Eio.Path.mkdir ~perm:0o700 root;
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree root)
    (fun () -> Eio.Switch.run (fun sw -> f env sw root name))
