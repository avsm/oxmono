open! Core
open! Bonsai_test

let computation graph =
  let open Bonsai.Let_syntax in
  let queries, set_queries = Bonsai.state [] graph in
  let scoper, set_scoper = Bonsai.state 0 graph in
  let memo, reset =
    Bonsai.with_model_resetter
      ~f:
        (Bonsai.scope_model
           (module Int)
           ~on:scoper
           ~for_:
             (Bonsai.Memo.create (module Int) ~f:(fun input _ ->
                let%arr input in
                input * -1)))
      graph
  in
  let queries =
    let%arr queries in
    String.Map.of_alist_exn queries
  in
  let results =
    Bonsai.assoc
      (module String)
      queries
      ~f:(fun _key data graph ->
        let r = Bonsai.Memo.lookup memo data graph in
        Bonsai.both data r)
      graph
  in
  let inject =
    let%arr set_queries and reset and set_scoper in
    function
    | `Set_queries x -> set_queries x
    | `Reset -> reset
    | `Scope_model x -> set_scoper x
  in
  let result_view =
    let%arr results and memo in
    let results =
      Map.to_alist results
      |> List.map ~f:(fun (name, (input, output)) ->
        let output = Option.value_map output ~f:Int.to_string ~default:"?" in
        [%string "%{name}: %{input#Int} -> %{output}"])
      |> String.concat_lines
    in
    let query_states =
      match Bonsai.Debug.memo_subscribers memo with
      | x when Map.is_empty x -> "No polled queries"
      | subscribers ->
        let per_query =
          Map.fold
            subscribers
            ~init:(Map.empty (module Int))
            ~f:(fun ~key:path ~data:query acc -> Map.add_multi acc ~key:query ~data:path)
          |> Map.to_alist
        in
        let s =
          List.map per_query ~f:(fun (key, paths) ->
            let paths_str =
              paths
              |> List.map ~f:Bonsai.Path.to_unique_identifier_string
              |> List.map ~f:(String.chop_prefix_if_exists ~prefix:"bonsai_path_")
              |> String.concat ~sep:", "
            in
            [%string "%{key#Int}: (%{paths_str})"])
          |> String.concat ~sep:"; "
        in
        "Polled Queries: " ^ s
    in
    [%string "%{results}\n%{query_states}"]
  in
  Bonsai.both result_view inject
;;

module Result_spec = struct
  type t =
    string
    * ([ `Set_queries of (string * int) list | `Reset | `Scope_model of int ]
       -> unit Effect.t)

  type incoming =
    [ `Set_queries of (string * int) list
    | `Reset
    | `Scope_model of int
    ]

  let view (x, _) = x
  let incoming (_, inject) t = inject t
end

let%expect_test "If Memo reset, but not [lookup]s [lookup]s recover after a frame" =
  let handle = Handle.create (module Result_spec) computation in
  Handle.do_actions handle [ `Set_queries [ "one", 1; "two", 2; "three", 3 ] ];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> ?
    three: 3 -> ?
    two: 2 -> ?

    No polled queries
    |}];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    three: 3 -> -3
    two: 2 -> -2

    Polled Queries: 1: (y_gpgogf_x); 2: (y_hehhgp_x); 3: (y_hegihcgfgf_x)
    |}];
  Handle.do_actions handle [ `Set_queries [ "one", 1; "two", 2; "one_dup", 1 ] ];
  Handle.show handle;
  (* We don't need to do a round-trip, because we have a result for "1" cached. *)
  [%expect
    {|
    one: 1 -> -1
    one_dup: 1 -> -1
    two: 2 -> -2

    Polled Queries: 1: (y_gpgogf_x); 2: (y_hehhgp_x); 3: (y_hegihcgfgf_x)
    |}];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    one_dup: 1 -> -1
    two: 2 -> -2

    Polled Queries: 1: (y_gpgogffpgehfha_x, y_gpgogf_x); 2: (y_hehhgp_x)
    |}];
  (* 3 is cleaned up after 2 frames. *)
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    one_dup: 1 -> -1
    two: 2 -> -2

    Polled Queries: 1: (y_gpgogffpgehfha_x, y_gpgogf_x); 2: (y_hehhgp_x)
    |}];
  Handle.do_actions handle [ `Set_queries [ "one", 1; "two", 2; "three", 3 ] ];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    three: 3 -> ?
    two: 2 -> -2

    Polled Queries: 1: (y_gpgogffpgehfha_x, y_gpgogf_x); 2: (y_hehhgp_x)
    |}];
  (* 1 is scheduled for removal, because "one_dup" went away... *)
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    three: 3 -> -3
    two: 2 -> -2

    Polled Queries: 1: (y_gpgogf_x); 2: (y_hehhgp_x); 3: (y_hegihcgfgf_x)
    |}];
  (* ... but "one" reaffirmed its presence, so it remains. *)
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    three: 3 -> -3
    two: 2 -> -2

    Polled Queries: 1: (y_gpgogf_x); 2: (y_hehhgp_x); 3: (y_hegihcgfgf_x)
    |}];
  Handle.do_actions handle [ `Reset ];
  (* It takes us a frame to start comuting state again... *)
  Handle.show handle;
  [%expect
    {|
    one: 1 -> ?
    three: 3 -> ?
    two: 2 -> ?

    No polled queries
    |}];
  (* ... But eventually, we're good again! *)
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    three: 3 -> -3
    two: 2 -> -2

    Polled Queries: 1: (y_gpgogf_x); 2: (y_hehhgp_x); 3: (y_hegihcgfgf_x)
    |}]
;;

let%expect_test "If Memo changed via scope model, but not [lookup]s, [lookup]s recover \
                 after a frame"
  =
  let handle = Handle.create (module Result_spec) computation in
  Handle.do_actions handle [ `Set_queries [ "one", 1; "two", 2; "three", 3 ] ];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> ?
    three: 3 -> ?
    two: 2 -> ?

    No polled queries
    |}];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    three: 3 -> -3
    two: 2 -> -2

    Polled Queries: 1: (y_gpgogf_x); 2: (y_hehhgp_x); 3: (y_hegihcgfgf_x)
    |}];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    three: 3 -> -3
    two: 2 -> -2

    Polled Queries: 1: (y_gpgogf_x); 2: (y_hehhgp_x); 3: (y_hegihcgfgf_x)
    |}];
  Handle.do_actions handle [ `Set_queries [ "one", 1; "two", 2; "one_dup", 1 ] ];
  Handle.show handle;
  (* We don't need to do a round-trip, because we have a result for "1" cached. Our poller
     will stop polling for it one frame later. *)
  [%expect
    {|
    one: 1 -> -1
    one_dup: 1 -> -1
    two: 2 -> -2

    Polled Queries: 1: (y_gpgogf_x); 2: (y_hehhgp_x); 3: (y_hegihcgfgf_x)
    |}];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    one_dup: 1 -> -1
    two: 2 -> -2

    Polled Queries: 1: (y_gpgogffpgehfha_x, y_gpgogf_x); 2: (y_hehhgp_x)
    |}];
  (* 3 is cleaned up after 2 frames. *)
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    one_dup: 1 -> -1
    two: 2 -> -2

    Polled Queries: 1: (y_gpgogffpgehfha_x, y_gpgogf_x); 2: (y_hehhgp_x)
    |}];
  Handle.do_actions handle [ `Set_queries [ "one", 1; "two", 2; "three", 3 ] ];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    three: 3 -> ?
    two: 2 -> -2

    Polled Queries: 1: (y_gpgogffpgehfha_x, y_gpgogf_x); 2: (y_hehhgp_x)
    |}];
  (* 1 is scheduled for removal, because "one_dup" went away... *)
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    three: 3 -> -3
    two: 2 -> -2

    Polled Queries: 1: (y_gpgogf_x); 2: (y_hehhgp_x); 3: (y_hegihcgfgf_x)
    |}];
  (* ... but "one" reaffirmed its presence, so it remains. *)
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    three: 3 -> -3
    two: 2 -> -2

    Polled Queries: 1: (y_gpgogf_x); 2: (y_hehhgp_x); 3: (y_hegihcgfgf_x)
    |}];
  Handle.do_actions handle [ `Scope_model 1 ];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> ?
    three: 3 -> ?
    two: 2 -> ?

    No polled queries
    |}];
  (* We start polling again after a frame. *)
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    three: 3 -> -3
    two: 2 -> -2

    Polled Queries: 1: (y_gpgogf_x); 2: (y_hehhgp_x); 3: (y_hegihcgfgf_x)
    |}];
  Handle.do_actions handle [ `Set_queries [ "two_dup", 2; "two", 2; "three", 3 ] ];
  (* Old state is still there! *)
  Handle.do_actions handle [ `Scope_model 0 ];
  Handle.show handle;
  [%expect
    {|
    three: 3 -> ?
    two: 2 -> ?
    two_dup: 2 -> ?

    No polled queries
    |}];
  Handle.show handle;
  ();
  [%expect
    {|
    three: 3 -> -3
    two: 2 -> -2
    two_dup: 2 -> -2

    Polled Queries: 2: (y_hehhgpfpgehfha_x, y_hehhgp_x); 3: (y_hegihcgfgf_x)
    |}];
  ()
;;
