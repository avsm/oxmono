(* Compile-time probes of the kind and mode claims in the maildir
   interfaces. Each abbreviation in [Kinds] compiles only when its kind
   holds. Each probe is a closure bound at portable mode, as in
   [let (f @ portable) = fun () -> ...], that captures module-level values
   and reads them through the library, so it compiles only when their types
   cross portability and contention and the functions called are
   portable. *)

module K = Maildir.Keywords
module Flag = Mail_flag.Imap_flag

module Kinds = struct
  type keywords : immutable_data = K.t
  type error : immutable_data = Maildir.error
  type location : immutable_data = Maildir.location
  type occurrence : immutable_data = Maildir.occurrence
end

let get = function
  | Ok x -> x
  | Error e -> failwith (Format.asprintf "%a" Maildir.pp_error e)

let flag s = match Flag.of_wire s with Ok f -> f | Error e -> failwith e
let mapping = get (K.parse "0 $Important\n3 $Junk\n")
let flags = [ flag "\\Seen"; flag "$Important"; flag "$Junk" ]
let error = Maildir.Unknown_letter { file = "x"; letter = 'q' }

let (keywords @ portable) = fun () ->
  let encoded = get (K.encode mapping) in
  let letters = K.letters ~passed:true mapping flags in
  let parsed = get (K.flags mapping ~file:"probe" letters) in
  encoded, letters, List.map Flag.to_wire parsed,
  K.equal mapping (get (K.add mapping [ flag "$Junk" ]))

let (printer @ portable) = fun () ->
  Format.asprintf "%a" Maildir.pp_error error

let test_keywords () =
  let encoded, letters, parsed, unchanged = keywords () in
  Alcotest.(check string) "encoded" "0 $Important\n3 $Junk\n" encoded;
  Alcotest.(check string) "letters" "PSad" letters;
  Alcotest.(check (list string)) "flags"
    (List.map Flag.to_wire (Flag.durable flags)) parsed;
  Alcotest.(check bool) "add of a mapped keyword" true unchanged

let test_printer () =
  Alcotest.(check bool) "printed" true (String.length (printer ()) > 0)

let () =
  Alcotest.run "Maildir kinds and modes" [
    "portable", [
      Alcotest.test_case "keywords" `Quick test_keywords;
      Alcotest.test_case "pp_error" `Quick test_printer ] ]
