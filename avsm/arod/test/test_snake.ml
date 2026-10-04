(* The timeline is one SVG spine that snakes down the page, with an exit curve
   from it to each entry's node. The page is laid out in fixed rows so that the
   spine can be drawn without measuring anything, which makes the geometry the
   thing to pin: the spine must be continuous and stay in its lane, every exit
   must start on the spine, and no node may sit on the spine. *)

module S = Arod_component.Snake

let checks = ref 0

let check name cond =
  incr checks;
  if not cond then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let near ?(eps = 1e-6) a b = Float.abs (a -. b) < eps

let rec range a b step = if a > b then [] else a :: range (a +. step) b step

let () =
  check "the spine starts at the left of its lane" (near (S.spine_x 0.) S.xl);
  check "the spine reaches the right of its lane after a segment"
    (near (S.spine_x S.seg) S.xr);
  check "and swings back after the next" (near (S.spine_x (2. *. S.seg)) S.xl);
  let ys = range 0. 400. 0.37 in
  check "the spine stays in its lane"
    (List.for_all
       (fun y ->
         let x = S.spine_x y in
         x >= S.xl -. 1e-9 && x <= S.xr +. 1e-9)
       ys);
  check "the spine is continuous, with no jump between neighbouring heights"
    (List.for_all
       (fun y -> Float.abs (S.spine_x (y +. 0.01) -. S.spine_x y) < 0.01)
       ys);
  check "the spine never runs sideways: it moves less than it falls"
    (List.for_all
       (fun y ->
         Float.abs (S.spine_x (y +. 0.1) -. S.spine_x y) < 0.1)
       ys);
  check "the spine turns smoothly at the ends of a swing"
    (Float.abs (S.spine_x (S.seg -. 0.2) -. S.spine_x (S.seg +. 0.2)) < 0.05);

  (* The exit of an entry leaves the spine and arrives at its node. *)
  List.iter
    (fun kind ->
      List.iter
        (fun y_abs ->
          let e = S.exit_ ~plain:(kind = S.Release) ~kind ~y_abs in
          check "an exit starts on the spine"
            (near ~eps:1e-4 e.S.start_x (S.spine_x (y_abs +. e.S.start_y)));
          check "an exit ends at its node's edge, level with its centre"
            (near e.S.end_x (S.arrive ~plain:(kind = S.Release) kind)
            && near e.S.end_y (S.center kind));
          check "an exit comes with the stretch of spine it merges from"
            (String.length e.S.lane > 0 && e.S.lane.[0] = 'M'
            && String.contains e.S.lane 'L');
          check "an exit leaves the spine above its node"
            (e.S.start_y < S.center kind))
        [ 0.; 3.7; 12.5; 27.1; 101.9 ])
    [ S.Note; S.Week; S.Release ];

  check "an exit with no thumbnail runs on to the text"
    (let e = S.exit_ ~plain:true ~kind:S.Note ~y_abs:10. in
     e.S.end_x > S.node_left S.Note && e.S.end_x < S.text_left S.Note);
  check "no node touches the spine"
    (List.for_all
       (fun k -> S.node_left k > S.xr)
       [ S.Note; S.Week; S.Release ]);
  check "an entry's size says how much it matters"
    (S.height S.Note >= S.height S.Week && S.height S.Week > S.height S.Release
    && S.node_height S.Week > S.node_height S.Release);
  check "a node fits in its row"
    (List.for_all
       (fun k ->
         S.center k -. (S.node_height k /. 2.) >= 0.
         && S.center k +. (S.node_height k /. 2.) <= S.height k)
       [ S.Note; S.Week; S.Release ]);
  check "text sits clear of its node"
    (List.for_all
       (fun k -> S.text_left k > S.node_left k +. S.node_width k)
       [ S.Note; S.Week; S.Release ]);

  let d = S.spine_path ~height:100. in
  check "the spine path is one path from the top"
    (String.length d > 0 && d.[0] = 'M');
  check "the spine path reaches below the page"
    (let n = ref 0 in
     String.iter (fun c -> if c = 'C' then incr n) d;
     float_of_int !n *. S.seg >= 100.);
  check "a path is made of numbers that print the same way every time"
    (S.spine_path ~height:50. = S.spine_path ~height:50.);
  Printf.printf "ok: %d checks\n" !checks
