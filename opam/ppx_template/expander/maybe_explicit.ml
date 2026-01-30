open Stdppx

type explicitness =
  | Explicit
  | Drop_axis_if_all_defaults

type 'a t = explicitness * 'a

let map t ~f = Import.map_snd t ~f

let ok = function
  | _, (Error _ as err) -> err
  | e, Ok ok -> Ok (e, ok)
;;

let map_result t ~f = map t ~f |> ok

module Both = struct
  type 'a maybe_explicit = 'a t

  type 'a t =
    { explicit : 'a
    ; drop_axis_if_all_defaults : 'a
    }

  let extract_list { explicit; drop_axis_if_all_defaults } =
    [ explicit; drop_axis_if_all_defaults ]
  ;;

  let all ts =
    let explicit, drop_axis_if_all_defaults =
      List.map ts ~f:(fun { explicit; drop_axis_if_all_defaults } ->
        explicit, drop_axis_if_all_defaults)
      |> List.split
    in
    { explicit; drop_axis_if_all_defaults }
  ;;

  type 'a opt_map_result =
    | Neither
    | One of 'a maybe_explicit
    | Both of 'a t

  let create f =
    { explicit = f Explicit; drop_axis_if_all_defaults = f Drop_axis_if_all_defaults }
  ;;

  let opt_fold_map { explicit; drop_axis_if_all_defaults } ~init ~f =
    let init, explicit = f init explicit in
    let init, drop_axis_if_all_defaults = f init drop_axis_if_all_defaults in
    let map_result =
      match explicit, drop_axis_if_all_defaults with
      | None, None -> Neither
      | Some res, None -> One (Explicit, res)
      | None, Some res -> One (Drop_axis_if_all_defaults, res)
      | Some explicit, Some drop_axis_if_all_defaults ->
        Both { explicit; drop_axis_if_all_defaults }
    in
    init, map_result
  ;;

  let opt_map t ~f = opt_fold_map t ~init:() ~f:(fun () x -> (), f x) |> snd
end
