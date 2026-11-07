(* Spawning domains is what this test is for, so the OxCaml alerts that steer
   ordinary code away from [Domain.spawn] are off here. *)
[@@@alert "-do_not_spawn_domains"]

open Fetch

(* Regression for R34/R40: [Form.multipart]'s boundary generator used to be a
   shared [Random.State.t] behind a plain [lazy], so two Eio domains drawing
   a boundary at once could race on the same mutable state. It now draws
   from the flow the caller passes or from the calling domain's default
   [Random] generator, and shares nothing, so several domains drawing at
   once neither raise nor corrupt each other's draw. A short-lived,
   black-box test cannot force the specific unsynchronized-mutation
   interleaving that made the old code unsound, so this is a concurrency
   smoke test rather than a proof: it exercises many domains drawing many
   boundaries at once and checks every result is well-formed and distinct. *)

let is_boundary_hex s =
  String.length s = String.length "form" + 32 + 2
  && String.sub s 0 4 = "form"
  && String.for_all
       (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
       (String.sub s 4 32)

let boundary_of parts =
  let headers, _ = Form.multipart parts in
  match Header.to_list headers with
  | (_, content_type) :: _ ->
      Option.get (Httpz_media.Multipart.boundary_of_content_type content_type)
  | [] -> Alcotest.fail "multipart body carries no Content-Type"

let draws_per_domain = 200
let domain_count = 8

let test_boundary_generator_is_domain_safe () =
  let parts = [ Form.field "a" "1" ] in
  (* [Domain.Safe.spawn] needs a portable body, and [Form.multipart] is not
     marked portable, so the unchecked spawn stays. The body only reads
     immutable data and each domain's own default generator. *)
  let domains =
    List.init domain_count (fun _ ->
        (Stdlib.Domain.spawn [@alert "-unsafe_multidomain"]) (fun () ->
            List.init draws_per_domain (fun _ -> boundary_of parts)))
  in
  let results = List.concat_map Stdlib.Domain.join domains in
  Alcotest.(check int) "every domain completed every draw"
    (domain_count * draws_per_domain)
    (List.length results);
  Alcotest.(check bool) "every boundary is well-formed" true
    (List.for_all is_boundary_hex results);
  Alcotest.(check int) "no two draws collided" (List.length results)
    (List.length (List.sort_uniq compare results))

let test_boundary_comes_from_random () =
  let parts = [ Form.field "a" "1" ] in
  let boundary_of_bytes bytes =
    let random = Eio.Flow.string_source bytes in
    let headers, _ = Form.multipart ~random parts in
    match Header.to_list headers with
    | (_, content_type) :: _ ->
        Option.get (Httpz_media.Multipart.boundary_of_content_type content_type)
    | [] -> Alcotest.fail "multipart body carries no Content-Type"
  in
  Alcotest.(check string)
    "the boundary is the hex of the 16 bytes read from random"
    ("form" ^ String.make 32 '0' ^ "x0")
    (boundary_of_bytes (String.make 16 '\000'));
  Alcotest.(check string)
    "only 16 bytes are read"
    ("form" ^ String.make 32 'f' ^ "x0")
    (boundary_of_bytes (String.make 20 '\255'))

let () =
  Alcotest.run "fetch-form-boundary"
    [
      ( "multipart",
        [
          Alcotest.test_case "domain-safe generator" `Quick
            test_boundary_generator_is_domain_safe;
          Alcotest.test_case "boundary drawn from random" `Quick
            test_boundary_comes_from_random;
        ] );
    ]
