open Core

exception Not_enough_rows

(* exception Invalid_range of string list *)

type row_processor =
  Csv_common.Or_file.t
  -> skip_header:bool
  -> sep:char
  -> f:(string array -> unit)
  -> [ `Limit_to of string list | `All_but of string list ]
  -> unit

let load_rows ?on_invalid_row file ~sep ~f =
  let process ic =
    protectx
      ic
      ~f:(fun ic ->
        Delimited_kernel.Read.fold_lines
          ?on_invalid_row
          ~sep
          ~header:`No
          Delimited_kernel.Read.Row.builder
          ic
          ~init:()
          ~f:(fun () row -> f (Delimited_kernel.Read.Row.to_array row)))
      ~finally:In_channel.close
  in
  match (file : Csv_common.Or_file.t) with
  | Csv { header; lines } ->
    List.iter (header :: lines) ~f:(fun row -> Array.of_list row |> f)
  | Stdin | File "-" -> process In_channel.stdin
  | File x -> process (In_channel.create x)
;;

let get_positions header_row headers_wanted =
  match headers_wanted with
  | `Limit_to headers ->
    let hmap =
      Map.of_iteri_exn (module String) ~iteri:(fun ~f ->
        Array.iteri header_row ~f:(fun i h -> f ~key:h ~data:i) [@nontail])
    in
    Array.of_list headers
    |> Array.map ~f:(fun header ->
      match Map.find hmap header with
      | Some position -> position
      | None ->
        (try Int.of_string header with
         | _ -> raise_s [%message "Unknown header" ~_:(header : string)]))
  | `All_but headers ->
    let headers = String.Set.of_list headers in
    Array.filter_mapi header_row ~f:(fun pos header ->
      if Set.mem headers header then None else Some pos)
;;

let index_positions header_row headers_wanted =
  match headers_wanted with
  | `Limit_to headers -> Array.of_list_map headers ~f:Int.of_string
  | `All_but headers ->
    let headers = List.map headers ~f:Int.of_string |> Int.Set.of_list in
    Array.filter_mapi header_row ~f:(fun pos _ ->
      if Set.mem headers pos then None else Some pos)
;;

let cut_by_fields file ~consume_header_names ~skip_header ~sep ~f headers_wanted =
  let handle_row = ref (fun _ -> assert false) in
  (handle_row
   := fun row ->
        let positions =
          match consume_header_names with
          | true -> get_positions row headers_wanted
          | false -> index_positions row headers_wanted
        in
        let grab row =
          Array.map positions ~f:(fun i -> if Array.length row < i then "" else row.(i))
        in
        if not skip_header then f (grab row);
        handle_row := fun row -> f (grab row));
  load_rows file ~sep ~f:(fun row -> !handle_row row)
;;

let cut_by_field_names = cut_by_fields ~consume_header_names:true
let cut_by_field_indices = cut_by_fields ~consume_header_names:false

exception First_row of string list

let field_names ~sep file =
  let throw_row row = raise (First_row (Array.to_list row)) in
  try
    load_rows
      ~on_invalid_row:
        (Delimited_kernel.Read.On_invalid_row.create (fun ~line_number:_ _ _ exn ->
           match exn with
           | First_row _ -> `Raise exn
           | _ -> `Fallback Delimited_kernel.Read.On_invalid_row.raise))
      file
      ~sep
      ~f:throw_row;
    (* should throw First_row before this! *)
    raise Not_enough_rows
  with
  | First_row row -> row
;;

let regex_match file ~sep ~f ~regexp =
  let handle_row rows =
    let grab rows =
      Array.map rows ~f:(fun row -> if Pcre.pmatch ~rex:regexp row then row else "")
    in
    f (grab rows)
  in
  load_rows file ~sep ~f:handle_row
;;

(* apply [f_populated] to rows that have all fields corresponding to [headers] populated
   apply [f_unpopulated] to all other rows
*)
let split_populated_rows file ~skip_header ~sep ~f_populated ~f_unpopulated headers_wanted
  =
  let handle_row = ref (fun _ -> assert false) in
  (handle_row
   := fun row ->
        let positions = get_positions row headers_wanted in
        let populated row =
          let n = Array.length row in
          Array.for_all positions ~f:(fun i -> i < n && String.length row.(i) > 0)
        in
        if not skip_header
        then (
          f_populated row;
          f_unpopulated row);
        handle_row
        := fun row -> if populated row then f_populated row else f_unpopulated row);
  load_rows file ~sep ~f:(fun row -> !handle_row row)
;;

let fully_populated_rows file ~skip_header ~sep ~f headers_wanted =
  split_populated_rows
    file
    ~skip_header
    ~sep
    ~f_populated:f
    ~f_unpopulated:ignore
    headers_wanted
;;

let not_fully_populated_rows file ~skip_header ~sep ~f headers_wanted =
  split_populated_rows
    file
    ~skip_header
    ~sep
    ~f_populated:ignore
    ~f_unpopulated:f
    headers_wanted
;;
