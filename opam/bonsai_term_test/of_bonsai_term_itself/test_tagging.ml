open! Core
open Bonsai_test
open Bonsai_term

let display ?(dimensions = { Dimensions.width = 50; height = 10 }) view =
  let handle =
    Bonsai_term_test.create_handle_without_handler (fun ~dimensions:_ _ ->
      Bonsai.return view)
  in
  Bonsai_term_test.set_dimensions handle dimensions;
  Handle.show handle
;;

let rect ~w ~h char = View.rectangle ~fill:char ~width:w ~height:h ()

let print_location view id key =
  match View.Tag.find view ~id key with
  | None -> print_endline "tag not found!"
  | Some location -> print_s [%message "" ~_:(location : Region.t)]
;;

module%test Data_setting_and_lookup = struct
  let print_tags view ~id ~sexp_of =
    List.map (View.Tag.keys view ~id) ~f:(fun key ->
      match View.Tag.find view ~id key with
      | None -> assert false
      | Some data ->
        [%sexp (View.Tag.sexp_of_key id key : Sexp.t), (sexp_of data : Sexp.t)])
    |> Sexp.List
    |> print_s
  ;;

  let concat_id =
    View.Tag.create (module String) ~transform_regions:(fun data _ -> data) ~reduce:( @ )
  ;;

  let%expect_test "mark and lookup" =
    let view = View.none in
    let view = View.Tag.mark view ~id:concat_id ~key:"hello" ~f:(Fn.const [ "world" ]) in
    print_tags view ~id:concat_id ~sexp_of:[%sexp_of: string list];
    [%expect {| ((hello (world))) |}];
    let view = View.Tag.mark view ~id:concat_id ~key:"hi" ~f:(Fn.const [ "there" ]) in
    print_tags view ~id:concat_id ~sexp_of:[%sexp_of: string list];
    [%expect
      {|
      ((hello (world))
       (hi    (there)))
      |}]
  ;;

  let%expect_test "removal" =
    let view = View.none in
    let view = View.Tag.mark view ~id:concat_id ~key:"hello" ~f:(Fn.const [ "world" ]) in
    print_tags view ~id:concat_id ~sexp_of:[%sexp_of: string list];
    [%expect {| ((hello (world))) |}];
    let view = View.Tag.remove view ~id:concat_id ~key:"hello" in
    print_tags view ~id:concat_id ~sexp_of:[%sexp_of: string list];
    [%expect {| () |}]
  ;;

  let%expect_test "keys" =
    let view = View.none in
    let view = View.Tag.mark view ~id:concat_id ~key:"hello" ~f:(Fn.const [ "world" ]) in
    print_s [%sexp (View.Tag.keys view ~id:concat_id : string list)];
    [%expect {| (hello) |}];
    let view = View.Tag.mark view ~id:concat_id ~key:"hi" ~f:(Fn.const [ "there" ]) in
    print_s [%sexp (View.Tag.keys view ~id:concat_id : string list)];
    [%expect {| (hello hi) |}]
  ;;

  let%expect_test "data is reduced when using the same key" =
    let view = View.none in
    let view = View.Tag.mark view ~id:concat_id ~key:"hello" ~f:(Fn.const [ "world" ]) in
    print_tags view ~id:concat_id ~sexp_of:[%sexp_of: string list];
    [%expect {| ((hello (world))) |}];
    let view = View.Tag.mark view ~id:concat_id ~key:"hello" ~f:(Fn.const [ "there" ]) in
    print_tags view ~id:concat_id ~sexp_of:[%sexp_of: string list];
    [%expect {| ((hello (world there))) |}];
    (* When we remove ["hello"] do we go back to the "previous" "hello" or do we go back
       to the emptiness? *)
    let view = View.Tag.remove view ~id:concat_id ~key:"hello" in
    print_tags view ~id:concat_id ~sexp_of:[%sexp_of: string list];
    (* We go back to emptiness. *)
    [%expect {| () |}]
  ;;

  let%expect_test "data is reduced when using the same key in a concatenation" =
    let view = View.none in
    let view =
      View.vcat
        [ View.Tag.mark view ~id:concat_id ~key:"hello" ~f:(Fn.const [ "world" ])
        ; View.Tag.mark view ~id:concat_id ~key:"hello" ~f:(Fn.const [ "there" ])
        ]
    in
    print_tags view ~id:concat_id ~sexp_of:[%sexp_of: string list];
    [%expect {| ((hello (world there))) |}]
  ;;

  let%expect_test "Tag.mem" =
    let view = View.none in
    let view = View.Tag.mark view ~id:concat_id ~key:"hello" ~f:(Fn.const [ "world" ]) in
    assert (View.Tag.mem view ~id:concat_id "hello");
    let view = View.Tag.remove view ~id:concat_id ~key:"hello" in
    assert (not (View.Tag.mem view ~id:concat_id "hello"))
  ;;

  let%expect_test "remove_all" =
    let id_a =
      View.Tag.create (module String) ~reduce:( @ ) ~transform_regions:(fun data _ ->
        data)
    in
    let id_b =
      View.Tag.create (module Int) ~reduce:( @ ) ~transform_regions:(fun data _ -> data)
    in
    let view = View.none in
    (* Mark view with two ids *)
    let view = View.Tag.mark view ~id:id_a ~key:"hello" ~f:(Fn.const [ "world" ]) in
    let view = View.Tag.mark view ~id:id_b ~key:0 ~f:(Fn.const [ 1 ]) in
    print_tags view ~id:id_a ~sexp_of:[%sexp_of: string list];
    print_tags view ~id:id_b ~sexp_of:[%sexp_of: int list];
    [%expect
      {|
      ((hello (world)))
      ((0 (1)))
      |}];
    (* Remove all tags for [id_a] *)
    let view = View.Tag.remove_all view ~id:id_a in
    print_tags view ~id:id_a ~sexp_of:[%sexp_of: string list];
    print_tags view ~id:id_b ~sexp_of:[%sexp_of: int list];
    [%expect
      {|
      ()
      ((0 (1)))
      |}];
    (* Remove all tags for [id_b] *)
    let view = View.Tag.remove_all view ~id:id_b in
    print_tags view ~id:id_a ~sexp_of:[%sexp_of: string list];
    print_tags view ~id:id_b ~sexp_of:[%sexp_of: int list];
    [%expect
      {|
      ()
      ()
      |}]
  ;;
end

module%test Location_tracking = struct
  let tag_id =
    View.Tag.create
      (module Unit)
      ~reduce:(fun _ t -> t)
      ~transform_regions:(fun region f -> f region)
  ;;

  let%expect_test "mark" =
    let marked_view = View.Tag.mark (rect ~w:3 ~h:2 'x') ~id:tag_id ~key:() ~f:Fn.id in
    display marked_view;
    [%expect
      {|
      ┌──────────────────────────────────────────────────┐
      │xxx                                               │
      │xxx                                               │
      │                                                  │
      │                                                  │
      │                                                  │
      │                                                  │
      │                                                  │
      │                                                  │
      │                                                  │
      └──────────────────────────────────────────────────┘
      |}];
    print_location marked_view tag_id ();
    [%expect
      {|
      ((x      0)
       (y      0)
       (width  3)
       (height 2))
      |}]
  ;;

  let%expect_test "lookup nonexistent mark" =
    let unmarked_view = rect ~w:3 ~h:2 'x' in
    display unmarked_view;
    [%expect
      {|
      ┌──────────────────────────────────────────────────┐
      │xxx                                               │
      │xxx                                               │
      │                                                  │
      │                                                  │
      │                                                  │
      │                                                  │
      │                                                  │
      │                                                  │
      │                                                  │
      └──────────────────────────────────────────────────┘
      |}];
    print_location unmarked_view tag_id ();
    [%expect {| tag not found! |}]
  ;;

  let%expect_test "vcat" =
    let marked_view = View.Tag.mark (rect ~w:3 ~h:2 'x') ~id:tag_id ~key:() ~f:Fn.id in
    let view = View.vcat [ rect ~w:5 ~h:4 'a'; marked_view ] in
    display view;
    [%expect
      {|
      ┌──────────────────────────────────────────────────┐
      │aaaaa                                             │
      │aaaaa                                             │
      │aaaaa                                             │
      │aaaaa                                             │
      │xxx                                               │
      │xxx                                               │
      │                                                  │
      │                                                  │
      │                                                  │
      └──────────────────────────────────────────────────┘
      |}];
    print_location view tag_id ();
    [%expect
      {|
      ((x      0)
       (y      4)
       (width  3)
       (height 2))
      |}]
  ;;

  let%expect_test "hcat" =
    let marked_view = View.Tag.mark (rect ~w:3 ~h:2 'x') ~id:tag_id ~key:() ~f:Fn.id in
    let view = View.hcat [ rect ~w:5 ~h:4 'a'; marked_view ] in
    display view;
    [%expect
      {|
      ┌──────────────────────────────────────────────────┐
      │aaaaaxxx                                          │
      │aaaaaxxx                                          │
      │aaaaa                                             │
      │aaaaa                                             │
      │                                                  │
      │                                                  │
      │                                                  │
      │                                                  │
      │                                                  │
      └──────────────────────────────────────────────────┘
      |}];
    print_location view tag_id ();
    [%expect
      {|
      ((x      5)
       (y      0)
       (width  3)
       (height 2))
      |}]
  ;;

  let%expect_test "zcat" =
    let marked_view = View.Tag.mark (rect ~w:3 ~h:2 'x') ~id:tag_id ~key:() ~f:Fn.id in
    let view = View.zcat [ marked_view; rect ~w:5 ~h:4 'a' ] in
    display view;
    [%expect
      {|
      ┌──────────────────────────────────────────────────┐
      │xxxaa                                             │
      │xxxaa                                             │
      │aaaaa                                             │
      │aaaaa                                             │
      │                                                  │
      │                                                  │
      │                                                  │
      │                                                  │
      │                                                  │
      └──────────────────────────────────────────────────┘
      |}];
    print_location view tag_id ();
    [%expect
      {|
      ((x      0)
       (y      0)
       (width  3)
       (height 2))
      |}]
  ;;

  module%test Overriding_behavior = struct
    let%expect_test "vcat" =
      let top_view = rect ~w:5 ~h:4 't' |> View.Tag.mark ~id:tag_id ~key:() ~f:Fn.id in
      let bottom_view = rect ~w:3 ~h:2 'b' |> View.Tag.mark ~id:tag_id ~key:() ~f:Fn.id in
      let view = View.vcat [ top_view; bottom_view ] in
      display view;
      [%expect
        {|
        ┌──────────────────────────────────────────────────┐
        │ttttt                                             │
        │ttttt                                             │
        │ttttt                                             │
        │ttttt                                             │
        │bbb                                               │
        │bbb                                               │
        │                                                  │
        │                                                  │
        │                                                  │
        └──────────────────────────────────────────────────┘
        |}];
      print_location view tag_id ();
      (* bottom view overrides *)
      [%expect
        {|
        ((x      0)
         (y      4)
         (width  3)
         (height 2))
        |}]
    ;;

    let%expect_test "hcat" =
      let left_view = rect ~w:5 ~h:4 'l' |> View.Tag.mark ~id:tag_id ~key:() ~f:Fn.id in
      let right_view = rect ~w:3 ~h:2 'r' |> View.Tag.mark ~id:tag_id ~key:() ~f:Fn.id in
      let view = View.hcat [ left_view; right_view ] in
      display view;
      [%expect
        {|
        ┌──────────────────────────────────────────────────┐
        │lllllrrr                                          │
        │lllllrrr                                          │
        │lllll                                             │
        │lllll                                             │
        │                                                  │
        │                                                  │
        │                                                  │
        │                                                  │
        │                                                  │
        └──────────────────────────────────────────────────┘
        |}];
      print_location view tag_id ();
      (* right view overrides *)
      [%expect
        {|
        ((x      5)
         (y      0)
         (width  3)
         (height 2))
        |}]
    ;;

    let%expect_test "zcat" =
      let above_view = rect ~w:3 ~h:2 'a' |> View.Tag.mark ~id:tag_id ~key:() ~f:Fn.id in
      let below_view = rect ~w:5 ~h:4 'b' |> View.Tag.mark ~id:tag_id ~key:() ~f:Fn.id in
      let view = View.zcat [ above_view; below_view ] in
      display view;
      [%expect
        {|
        ┌──────────────────────────────────────────────────┐
        │aaabb                                             │
        │aaabb                                             │
        │bbbbb                                             │
        │bbbbb                                             │
        │                                                  │
        │                                                  │
        │                                                  │
        │                                                  │
        │                                                  │
        └──────────────────────────────────────────────────┘
        |}];
      print_location view tag_id ();
      (* above view overrides *)
      [%expect
        {|
        ((x      0)
         (y      0)
         (width  3)
         (height 2))
        |}]
    ;;
  end

  let%expect_test "pad right" =
    let marked_view = View.Tag.mark (rect ~w:3 ~h:2 'x') ~id:tag_id ~key:() ~f:Fn.id in
    let view = View.pad ~l:2 marked_view in
    display view;
    [%expect
      {|
      ┌──────────────────────────────────────────────────┐
      │  xxx                                             │
      │  xxx                                             │
      │                                                  │
      │                                                  │
      │                                                  │
      │                                                  │
      │                                                  │
      │                                                  │
      │                                                  │
      └──────────────────────────────────────────────────┘
      |}];
    print_location view tag_id ();
    [%expect
      {|
      ((x      2)
       (y      0)
       (width  3)
       (height 2))
      |}]
  ;;

  let%expect_test "pad top" =
    let marked_view = View.Tag.mark (rect ~w:3 ~h:2 'x') ~id:tag_id ~key:() ~f:Fn.id in
    let view = View.pad ~t:2 marked_view in
    display view;
    [%expect
      {|
      ┌──────────────────────────────────────────────────┐
      │                                                  │
      │                                                  │
      │xxx                                               │
      │xxx                                               │
      │                                                  │
      │                                                  │
      │                                                  │
      │                                                  │
      │                                                  │
      └──────────────────────────────────────────────────┘
      |}];
    print_location view tag_id ();
    [%expect
      {|
      ((x      0)
       (y      2)
       (width  3)
       (height 2))
      |}]
  ;;

  let%expect_test "Same view multiple locations" =
    let marked_view = View.Tag.mark (rect ~w:3 ~h:2 'x') ~id:tag_id ~key:() ~f:Fn.id in
    print_location marked_view tag_id ();
    [%expect
      {|
      ((x      0)
       (y      0)
       (width  3)
       (height 2))
      |}];
    print_location (View.vcat [ marked_view; marked_view ]) tag_id ();
    [%expect
      {|
      ((x      0)
       (y      2)
       (width  3)
       (height 2))
      |}];
    print_location
      (View.vcat [ marked_view; View.hcat [ marked_view; marked_view ] ])
      tag_id
      ();
    [%expect
      {|
      ((x      3)
       (y      2)
       (width  3)
       (height 2))
      |}]
  ;;

  let%expect_test "Remember all locations" =
    let tag_id =
      View.Tag.create
        (module Unit)
        ~transform_regions:(fun data f -> Set.map (module Region) data ~f)
        ~reduce:Set.union
    in
    let marked_view =
      View.Tag.mark
        (rect ~w:3 ~h:2 'x')
        ~id:tag_id
        ~key:()
        ~f:(Set.singleton (module Region))
    in
    let print_locations view id key =
      match View.Tag.find view ~id key with
      | None -> print_endline "tag not found!"
      | Some locations -> print_s [%sexp (locations : Set.M(Region).t)]
    in
    print_locations marked_view tag_id ();
    [%expect
      {|
      ((
        (x      0)
        (y      0)
        (width  3)
        (height 2)))
      |}];
    print_locations (View.vcat [ marked_view; marked_view ]) tag_id ();
    [%expect
      {|
      (((x      0)
        (y      0)
        (width  3)
        (height 2))
       ((x      0)
        (y      2)
        (width  3)
        (height 2)))
      |}];
    print_locations
      (View.vcat [ marked_view; View.hcat [ marked_view; marked_view ] ])
      tag_id
      ();
    [%expect
      {|
      (((x      0)
        (y      0)
        (width  3)
        (height 2))
       ((x      0)
        (y      2)
        (width  3)
        (height 2))
       ((x      3)
        (y      2)
        (width  3)
        (height 2)))
      |}]
  ;;
end
