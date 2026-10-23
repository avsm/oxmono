module V = Retrieval.Vector
module D = Sqlite3.Data

let check name b = if not b then failwith name

let () =
  Sqlite_vec.register ();
  let db = Sqlite3.db_open ":memory:" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let parse value =
        let s = Sqlite3.prepare db "SELECT vec_f32(?)" in
        Fun.protect
          ~finally:(fun () -> ignore (Sqlite3.finalize s))
          (fun () ->
            Sqlite3.Rc.check (Sqlite3.bind s 1 value);
            match Sqlite3.step s with
            | ROW -> Ok (Sqlite3.column s 0)
            | rc -> Error (rc, Sqlite3.errmsg db))
      in
      let compare vector =
        check "binary vector preserves SQLite float32 bytes and errors"
          (parse (D.TEXT (V.json vector)) = parse (V.binding vector))
      in
      List.iter compare
        [
          [];
          [ 0.; -0. ];
          [ 1.; -1.; 0.1 ];
          [ Float.max_float; -.Float.max_float ];
          [ Float.min_float; -.Float.min_float ];
          [ Float.next_after 0. 1.; Float.next_after 0. (-1.) ];
          [ 0x1p-149; -0x1p-149; 0x1p-150 ];
          [ 0x1.fffffep127; 0x1p128 ];
          [ 1e30; -1e30; 1e-30; -1e-30 ];
          [ 1. +. 0x1p-24; Float.next_after (1. +. 0x1p-24) 2. ];
        ];
      for exponent = -99 to 99 do
        let base = Float.ldexp 1. exponent in
        let midpoint = base +. Float.ldexp 1. (exponent - 24) in
        List.iter
          (fun value -> compare [ value; -.value ])
          [
            midpoint;
            Float.next_after midpoint infinity;
            Float.next_after midpoint neg_infinity;
          ]
      done;
      let random = Random.State.make [| 0x123456 |] in
      for _ = 1 to 2000 do
        let bits = Random.State.int32 random Int32.max_int in
        let lower = Int32.float_of_bits bits in
        let upper = Int32.float_of_bits (Int32.succ bits) in
        if Float.is_finite lower && Float.is_finite upper then
          let midpoint = (lower +. upper) *. 0.5 in
          List.iter
            (fun value -> compare [ value; -.value ])
            [
              midpoint;
              Float.next_after midpoint infinity;
              Float.next_after midpoint neg_infinity;
            ]
      done;
      List.iter
        (fun n ->
          let vector = List.init n (fun i -> sin (float i) *. 0.123) in
          compare vector;
          check "ordinary vectors use compact BLOBs"
            (match V.binding vector with
            | D.BLOB s -> String.length s = 4 * n
            | _ -> false))
        [ 1; 256; 768; 1536; 4096 ];
      List.iter
        (fun value ->
          check "nonfinite vector rejected before SQL"
            (try
               ignore (V.binding [ 1.; value ]);
               false
             with Invalid_argument _ -> true))
        [ nan; infinity; neg_infinity ];
      V.init db;
      let index =
        V.index db ~model:"test" ~dimensions:2 ~normalization:"test"
      in
      V.put db ~index ~source:"one" ~revision:"1" ~input_hash:"old" [ 1.; 0. ];
      check "invalid input does not delete existing vector"
        (try
           V.put db ~index ~source:"one" ~revision:"1" ~input_hash:"new"
             [ nan; 0. ];
           false
         with Invalid_argument _ ->
           V.current db ~index ~source:"one" ~revision:"1" ~input_hash:"old"));
  print_endline "vector encoding tests passed"
