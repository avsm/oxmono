(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Independent syntax and property check for the field-level export. Optional
   output reserializes the cards through the native parser so the Python
   inverse can verify the resulting contact values against the source. *)

let read path = In_channel.with_open_bin path In_channel.input_all
let fail s = failwith s
let get = function Ok x -> x | Error e -> fail e

let one card key =
  match Vcard.find_all card key with
  | [ p ] -> Vcard.Property.text p
  | ps -> fail (Printf.sprintf "%s: expected one property, got %d" key (List.length ps))

let () =
  if Array.length Sys.argv < 2 || Array.length Sys.argv > 3 then
    fail "usage: validate_carddav BUNDLE [NEW-ROUNDTRIP.vcf]";
  let bundle = Sys.argv.(1) in
  let cards = get (Vcard.of_string (read (Filename.concat bundle "contacts.vcf"))) in
  let seen = Hashtbl.create (List.length cards) in
  let photos = ref 0 and fields = ref 0 in
  List.iter
    (fun card ->
      (match Vcard.version card with
       | "3.0" -> ignore (one card "FN")
       | "4.0" -> ignore (get (Vcard.validate card))
       | _ -> fail "unsupported vCard version");
      List.iter (fun key -> ignore (one card key)) [ "N"; "X-SORTAL-ID"; "X-SORTAL-STORE" ];
      if not (List.mem (one card "X-SORTAL-MAPPING") [ "1"; "2"; "3" ]) then
        fail "unsupported field mapping";
      if Vcard.find card "X-SORTAL-META" <> None then fail "unexpected serialized contact payload";
      let uid = one card "UID" in
      if Hashtbl.mem seen uid then fail "duplicate UID";
      Hashtbl.add seen uid ();
      let paths = Hashtbl.create 16 in
      List.iter (fun p ->
        match Vcard.Property.find_first p "X-SORTAL-PATH" with
        | None -> ()
        | Some path ->
            if Hashtbl.mem paths path then fail "duplicate field path";
            Hashtbl.add paths path ();
            incr fields) (Vcard.properties card);
      List.iter
        (fun p ->
          let value = Vcard.Property.value p in
          let encoded =
            if Option.map String.lowercase_ascii (Vcard.Property.find_first p "ENCODING") = Some "b" then Some value
            else if String.starts_with ~prefix:"data:image/" value then
              (match String.index_opt value ',' with
               | Some i -> Some (String.sub value (i + 1) (String.length value - i - 1))
               | None -> fail "malformed data URI")
            else None
          in
          match encoded with
          | Some encoded ->
              let data = get (Result.map_error (fun (`Msg e) -> e) (Base64.decode encoded)) in
              if data = "" then fail "empty embedded photo";
              incr photos
          | None -> ())
        (Vcard.find_all card "PHOTO"))
    cards;
  if Array.length Sys.argv = 3 then (
    let oc = open_out_gen [ Open_wronly; Open_creat; Open_excl; Open_binary ] 0o600 Sys.argv.(2) in
    Fun.protect ~finally:(fun () -> close_out oc)
      (fun () -> List.iter (fun card -> output_string oc (Vcard.to_string card)) cards));
  Printf.printf "Native parser: %d vCards, %d mapped properties, %d decoded photos.\n"
    (List.length cards) !fields !photos
