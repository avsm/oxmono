let src = Logs.Src.create "x509.decoding" ~doc:"X509 decoding"
module Log = (val Logs.src_log src : Logs.LOG)

let ( let* ) = Result.bind

let decode codec cs =
  let* a, cs = Asn.decode codec cs in
  if String.length cs = 0 then Ok a else Error (`Parse "Leftover")

let decoder_of encoding asn : _ @ portable =
  let decoder = Asn.decoder encoding asn in
  (fun cs ->
     match decoder cs with
     | Ok (value, "") -> Ok value
     | Ok _ -> Error (`Parse "Leftover")
     | Error _ as error -> error : _ @ portable)

let projections_of encoding asn =
  let c = Asn.codec encoding asn in (decode c, Asn.encode c)

let case_of_oid ~(default @ portable) (cases @ portable) : _ @ portable =
  let branch expected (make @ portable) (next @ portable) : _ @ portable =
    fun actual ->
      if Asn.OID.equal expected actual then make () else next actual
  in
  let rec compile = function
    | [] -> default
    | (expected, make) :: rest ->
      let next = compile rest in
      branch expected make next
  in
  compile cases

let case_of_oid_f ~(default @ portable) (cases @ portable) : _ @ portable =
  let rec compile = function
    | [] -> fun (oid, value) -> default oid value
    | (expected, apply) :: rest ->
      let next = compile rest in
      (fun (actual, value) ->
         if Asn.OID.equal expected actual
         then apply value
         else next (actual, value)
       : _ @ portable)
  in
  compile cases

(*
 * A way to parse by propagating (and contributing to) exceptions, so those can
 * be handles up in a single place. Meant for parsing embedded structures.
 *
 * XXX Would be nicer if combinators could handle embedded structures.
 *)
let project_exn asn =
  let c = Asn.(codec der) asn in
  let dec cs = match decode c cs with
    | Ok a      -> a
    | Error err -> Asn.S.error err in
  (dec, Asn.encode c)

let project_exn_decoder asn : _ @ portable =
  let decode = decoder_of Asn.der asn in
  (fun cs ->
     match decode cs with
     | Ok value -> value
     | Error err -> Asn.S.error err : _ @ portable)

let (err_to_msg @ portable) f =
  Result.map_error (fun (`Parse msg) -> `Msg msg) f

(* specified in RFC 5280 4.1.2.5.2 - "MUST NOT include fractional seconds" *)
let generalized_time_no_frac_s =
  Asn.S.(map
           (fun x ->
              if Ptime.Span.(equal zero (Ptime.frac_s x)) then
                x
              else
                parse_error "generalized time has fractional seconds")
           (fun y -> Ptime.truncate ~frac_s:0 y)
           generalized_time)

(* serial number, as defined in RFC 5280 4.1.2.2: must be > 0 and not be longer
   than 20 octets. we accept 0.
   we also accept < 0, but when encoding mandate >= 0!
*)
let serial =
  Asn.S.(map
           (fun x ->
              if String.length x > 20 then parse_error "serial exceeds 20 octets";
              x)
           (fun y ->
              if String.length y > 20 then failwith "serial exceeds 20 octets";
              if String.length y > 0 && String.get_uint8 y 0 > 0x7F then
                "\x00" ^ y
              else
                y)
           integer)
