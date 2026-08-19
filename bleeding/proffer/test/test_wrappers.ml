open Proffer
open Proffer.Route

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let site =
  Site.of_routes
    [ moved (s "old.xml" /? nil) "/new.xml"; found (s "wiki" /? nil) "/notes" ]

let compiled = Compiled.compile site

let () =
  let r = Proffer_mock.request compiled () `GET "/old.xml" in
  check "moved is 301" (Proffer_mock.status r = `Moved_permanently);
  check "moved sets location"
    (Proffer_mock.header r "location" = Some "/new.xml");
  let r = Proffer_mock.request compiled () `GET "/wiki" in
  check "found is 302" (Proffer_mock.status r = `Found);
  check "found sets location" (Proffer_mock.header r "location" = Some "/notes")

let () = Printf.printf "test_wrappers: %d checks ok\n" !checks
