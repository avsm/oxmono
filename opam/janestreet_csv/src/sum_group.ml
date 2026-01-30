open Core
open Async

module Agg : sig
  type t [@@deriving enumerate]

  val name : t -> string
  val desc : t -> string
  val update : t -> string -> t
  val get_val : t -> string
end = struct
  type 'a u =
    { name : string
    ; desc : string
    ; value : 'a
    ; update : 'a -> string -> 'a
    ; get_val : 'a -> string
    }

  type t = T : 'a u -> t

  let name (T t) = t.name
  let desc (T t) = t.desc
  let update (T t) s = T { t with value = t.update t.value (String.strip s) }
  let get_val (T t) = t.get_val t.value

  let all =
    let unless_empty f init s =
      if String.is_empty s then init else f init (Bignum.of_string s)
    in
    [ T
        { name = "sum"
        ; desc = "Sum of column"
        ; value = Bignum.zero
        ; update = unless_empty Bignum.( + )
        ; get_val = Bignum.to_string_hum
        }
    ; T
        { name = "count"
        ; desc = "Count how many unique strings are in this column"
        ; value = String.Set.empty
        ; update = Set.add
        ; get_val = (fun acc -> Int.to_string (Set.length acc))
        }
    ; T
        { name = "list"
        ; desc = "List of all unique values in this column, separated by semicolons"
        ; value = String.Set.empty
        ; update = Set.add
        ; get_val = (fun acc -> String.concat ~sep:";" (Set.to_list acc))
        }
    ; T
        { name = "sum-pos"
        ; desc = "Sum of all positive values in this column, ignoring negatives"
        ; value = Bignum.zero
        ; update =
            unless_empty (fun acc n ->
              Bignum.( + ) acc Bignum.(if n > zero then n else zero))
        ; get_val = Bignum.to_string_hum
        }
    ; T
        { name = "sum-neg"
        ; desc = "Sum of all negative values in this column, ignoring positives"
        ; value = Bignum.zero
        ; update =
            unless_empty (fun acc n ->
              Bignum.( + ) acc Bignum.(if n < zero then n else zero))
        ; get_val = Bignum.to_string_hum
        }
    ]
  ;;
end

let process_input_file ~sep ~keys ~aggregations ~init reader =
  let pipe =
    Delimited.Read.pipe_of_reader Delimited.Read.Row.builder ~sep ~header:`Yes reader
  in
  Pipe.fold_without_pushback pipe ~init ~f:(fun init row ->
    let key = List.map keys ~f:(Delimited.Read.Row.get_exn row) in
    Map.update init key ~f:(fun prev ->
      let prev = Option.value ~default:aggregations prev in
      List.map prev ~f:(fun (col, agg) ->
        col, Agg.update agg (Delimited.Read.Row.get_exn row col))))
;;

let write_output ~keys ~aggregations ~sep data =
  let w =
    Delimited.Write.Expert.By_row.of_writer_and_close ~sep (Lazy.force Writer.stdout)
  in
  Pipe.write_if_open
    w
    (keys @ List.map aggregations ~f:(fun (col, agg) -> col ^ "_" ^ Agg.name agg))
  >>= fun () ->
  Deferred.Map.iteri ~how:`Sequential data ~f:(fun ~key ~data ->
    if not (Pipe.is_closed w)
    then Pipe.write_if_open w (key @ List.map data ~f:(fun (_, agg) -> Agg.get_val agg))
    else Deferred.unit)
  >>| fun () -> Pipe.close w
;;

module Key = struct
  module T = struct
    type t = string list [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let readme () =
  {|
Fold over a csv file, creating a new csv file containing
key, and value fields, where the input is grouped by key
fields, then values are aggregated in one of a few
different ways.  E.g.,

$ csv sum-group          \
  -key     apple         \
  -key     orange        \
  -sum-pos price         \
  -sum-neg price         \
  -sum     price         \
  -count   supplier      \
  - <<EOF | csv pretty
apple,orange,supplier,price
fuji,navel,dole,1.5
red delicious,navel,dole,-1.5
fuji,navel,sysco,0.1
EOF

               orange
               |      price_sum-neg
               |        |  price_sum-pos
               |        |     |  supplier_count
apple          |        |     |  |  price_sum
|              |        |     |  |     |
fuji           navel  1.6     0  2   1.6
red delicious  navel    0  -1.5  1  -1.5
                  |}
;;

(* We want to offer the user a separate flag for each aggregation, but internally see one
   big list of aggregations. *)
let aggregation_flags =
  List.map Agg.all ~f:(fun agg ->
    let open Command.Param in
    flag
      ("-" ^ Agg.name agg)
      ~doc:(" " ^ Agg.desc agg)
      (listed (Arg_type.create (fun column_to_aggregate -> column_to_aggregate, agg))))
  |> Command.Param.all
  |> Command.Param.map ~f:List.concat
;;

let command =
  let summary =
    "sum a csv file, grouping by specified fields, producing a new csv file"
  in
  Command.async
    ~summary
    ~readme
    (let%map_open.Csv_param sep
     and keys = flag "-key" (listed string) ~doc:" group by these fields"
     and aggregations = aggregation_flags
     and csv, csvs =
       anon (non_empty_sequence_as_pair ("input-csv" %: Filename_unix.arg_type))
     in
     let open Async in
     fun () ->
       Deferred.List.fold (csv :: csvs) ~init:Key.Map.empty ~f:(fun init csv ->
         match csv with
         | "-" ->
           process_input_file ~sep ~keys ~aggregations ~init (Lazy.force Reader.stdin)
         | csv ->
           Reader.with_file csv ~f:(process_input_file ~sep ~keys ~aggregations ~init))
       >>= write_output ~sep ~keys ~aggregations)
    ~behave_nicely_in_pipeline:false
;;
