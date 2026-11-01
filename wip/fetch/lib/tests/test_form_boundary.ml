(* Spawning domains is what this test is for, so the OxCaml alerts that steer
   ordinary code away from [Domain.spawn] are off here. *)
[@@@alert "-do_not_spawn_domains"]
[@@@alert "-unsafe_multidomain"]

open Fetch

(* Regression for R34/R40: [Form.multipart]'s boundary generator used to be a
   shared [Random.State.t] behind a plain [lazy], so two Eio domains drawing
   a boundary at once could race on the same mutable state. It now keeps one
   generator per domain via [Stdlib.Domain.DLS], so several domains drawing
   at once neither raise nor corrupt each other's draw. A short-lived,
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
  let domains =
    List.init domain_count (fun _ ->
        Stdlib.Domain.spawn (fun () ->
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

let () =
  Alcotest.run "fetch-form-boundary"
    [
      ( "multipart",
        [
          Alcotest.test_case "domain-safe generator" `Quick
            test_boundary_generator_is_domain_safe;
        ] );
    ]
