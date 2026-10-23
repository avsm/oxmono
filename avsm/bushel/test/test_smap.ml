module Smap = Bushel.Smap
module Reference = Map.Make (String)

let table = Smap.of_list [ ("present", "value"); ("empty", "") ]

(* The captured map must remain usable by portable request handlers. *)
let portable_lookup : (string -> string or_null) @ portable =
 fun key -> Smap.find_or_null key table

let[@zero_alloc] lookup_length key =
  match portable_lookup key with
  | Null -> -1
  | This value -> String.length value

let () =
  assert (lookup_length "present" = 5);
  assert (lookup_length "empty" = 0);
  assert (lookup_length "missing" = -1);
  let state = Random.State.make [| 419 |] in
  for n = 0 to 200 do
    let bindings =
      List.init n (fun i -> (string_of_int (Random.State.int state 100), i))
    in
    let reference =
      List.fold_left
        (fun map (key, value) -> Reference.add key value map)
        Reference.empty bindings
    in
    let map = Smap.of_list bindings in
    assert (Smap.bindings map = Reference.bindings reference);
    for i = -1 to 101 do
      let key = string_of_int i in
      let expected = Reference.find_opt key reference in
      assert (Smap.find_opt key map = expected);
      match (Smap.find_or_null key map, expected) with
      | Null, None -> ()
      | This actual, Some value -> assert (actual = value)
      | _ -> assert false
    done
  done;
  Printf.printf "Smap nullable lookup and duplicate-key tests passed\n"
