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
  (* The seasons. *)
  check "the months make the four seasons"
    (List.map S.season_of_month [ 12; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11 ]
    = [ S.Winter; S.Winter; S.Winter; S.Spring; S.Spring; S.Spring; S.Summer;
        S.Summer; S.Summer; S.Autumn; S.Autumn; S.Autumn ]);
  List.iter
    (fun month ->
      List.iter
        (fun (seed, y0, height) ->
          let ms = S.motifs ~month ~seed ~y0 ~height in
          check "a month has motifs" (List.length ms > 0);
          check "motifs lie inside the strip"
            (List.for_all
               (fun m ->
                 m.S.cx -. m.S.r >= 0.
                 && m.S.cx +. m.S.r <= S.season_width
                 && m.S.cy -. m.S.r >= 0.
                 && m.S.cy +. m.S.r <= height)
               ms);
          check "motifs keep clear of the spine"
            (List.for_all
               (fun m ->
                 Float.abs (m.S.cx -. S.spine_x (y0 +. m.S.cy))
                 >= S.season_clear -. 1e-9)
               ms);
          check "a month's motifs are the same each time"
            (ms = S.motifs ~month ~seed ~y0 ~height);
          check "another month's motifs differ"
            (ms <> S.motifs ~month ~seed:(seed + 1) ~y0 ~height))
        [ (24313, 0., 30.); (24320, 41.7, 22.); (24325, 130.2, 55.) ];
      let paths = S.season_paths ~month ~seed:24313 ~y0:0. ~height:30. in
      check "a season draws paths"
        (paths <> []
        && List.for_all
             (fun (_, _, d) -> String.length d > 0 && d.[0] = 'M')
             paths))
    [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12 ];
  check "a month too short for motifs has none"
    (S.motifs ~month:7 ~seed:1 ~y0:0. ~height:S.month_height = []);
  (* Over many months the motifs of one season come in all four shapes. *)
  List.iter
    (fun month ->
      let seen = Hashtbl.create 8 in
      for seed = 1 to 40 do
        List.iter
          (fun m ->
            if m.S.season = S.season_of_month month then
              Hashtbl.replace seen m.S.variant ())
          (S.motifs ~month ~seed ~y0:0. ~height:40.)
      done;
      check "each season has four motifs" (Hashtbl.length seen = 4))
    [ 1; 4; 7; 10 ];
  (* The first and last month of a season take on its neighbour near the edge
     they share, and only there. The page runs newest first, so a month's top
     meets the month after it. *)
  let seasons_in ~month ~lo ~hi =
    let found = ref [] in
    for seed = 1 to 60 do
      List.iter
        (fun m ->
          let u = m.S.cy /. 40. in
          if u >= lo && u < hi && not (List.mem m.S.season !found) then
            found := m.S.season :: !found)
        (S.motifs ~month ~seed ~y0:0. ~height:40.)
    done;
    !found
  in
  check "the last month of winter opens into spring at its top"
    (List.mem S.Spring (seasons_in ~month:2 ~lo:0. ~hi:0.25)
    && not (List.mem S.Spring (seasons_in ~month:2 ~lo:0.65 ~hi:1.)));
  check "the first month of spring runs back into winter at its foot"
    (List.mem S.Winter (seasons_in ~month:3 ~lo:0.75 ~hi:1.)
    && not (List.mem S.Winter (seasons_in ~month:3 ~lo:0. ~hi:0.35)));
  check "the middle month of a season is its own"
    (seasons_in ~month:1 ~lo:0. ~hi:1. = [ S.Winter ]);
  let stops =
    S.season_stops [ (9, 0., 20.); (8, 20., 40.); (7, 60., 40.) ] ~total:100.
  in
  check "the spine has a colour stop in the middle of each month"
    (List.map fst stops = [ 0.1; 0.4; 0.8 ]
    && List.map snd stops = [ S.Autumn; S.Summer; S.Summer ]);
  Printf.printf "ok: %d checks\n" !checks
