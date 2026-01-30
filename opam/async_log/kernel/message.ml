(** Log messages are stored, starting with V2, as an explicit version followed by the
    message itself. This makes it easier to move the message format forward while still
    allowing older logs to be read by the new code.

    If you make a new version you must add a version to the Version module below and
    should follow the Make_versioned_serializable pattern. *)
module Stable = struct
  open Core.Core_stable

  module T1 = struct
    module V2 = struct
      module T = struct
        type 'time t =
          { time : 'time
          ; level : Level.Stable.V1.t option
          ; message : Sexp_or_string.Stable.V1.t
          ; tags : (string * string) list
          }
        [@@deriving bin_io, sexp]
      end

      include Versioned.Stable.Make (struct
          type 'time t = 'time T.t [@@deriving bin_io, sexp]

          let%expect_test "bin_digest Message.V2" =
            print_endline [%bin_digest: unit t];
            [%expect {| 26b02919ac3971aaace97169310e9d15 |}]
          ;;

          let version = Versioned.Stable.Version.V2
        end)

      (* this allows for automagical reading of any versioned sexp, so long as we can
         always lift to a Message.t *)
      let t_of_sexp time_of_sexp (sexp : Core.Sexp.t) =
        match sexp with
        | List [ (Atom _ as version); _ ] ->
          (match Versioned.Stable.Version.t_of_sexp version with
           | V2 -> t_of_sexp time_of_sexp sexp)
        | _ ->
          Core.failwithf !"Log.Message.t_of_sexp: malformed sexp: %{Core.Sexp}" sexp ()
      ;;
    end
  end
end

open! Core
open! Async_kernel
open! Import

module T1 = struct
  type 'time t = 'time Stable.T1.V2.t [@@deriving sexp_of]
end

type t = Time_float.t T1.t

let create_raw ?level ~time ?(tags = []) message : t = { time; level; message; tags }

let create ?level ?time ?tags message =
  let time = Option.value_or_thunk time ~default:Time_float.now in
  create_raw ?level ~time ?tags message
;;

let time (t : t) = t.time
let level (t : t) = t.level
let set_level (t : t) level = { t with level }
let raw_message (t : t) = t.message
let message (t : t) = Sexp_or_string.Stable.V1.to_string (raw_message t)
let tags (t : t) = t.tags
let add_tags (t : t) tags = { t with tags = List.rev_append tags t.tags }

let level_string (t : t) =
  match t.level with
  | None -> ""
  | Some l -> Level.to_string l ^ " "
;;

let format_tags (t : t) =
  match t.tags with
  | [] -> []
  | _ :: _ -> " --" :: List.concat_map t.tags ~f:(fun (t, v) -> [ " ["; t; ": "; v; "]" ])
;;

let to_write_only_text (t : t) zone =
  let prefix = level_string t in
  let formatted_tags = format_tags t in
  let time_string = Time_float.to_string_abs ~zone t.time in
  String.concat ~sep:"" (time_string :: " " :: prefix :: message t :: formatted_tags)
;;

module For_testing = struct
  let to_string
    (t : t)
    (zone : Core_private.Time_zone.t)
    ~(time : [ `Keep | `Omit ])
    ~(tags : [ `Keep | `Omit ])
    ~(level : [ `Keep | `Omit ])
    =
    let prefix =
      match level with
      | `Keep -> level_string t
      | `Omit -> ""
    in
    let formatted_tags =
      match tags with
      | `Keep -> format_tags t
      | `Omit -> []
    in
    let time_string =
      match time with
      | `Keep -> Time_float.to_string_abs ~zone t.time ^ " "
      | `Omit -> ""
    in
    let list_to_print = time_string :: prefix :: message t :: formatted_tags in
    let filtered_list =
      List.filter list_to_print ~f:(fun str -> not (String.is_empty str))
    in
    String.concat ~sep:"" filtered_list
  ;;
end
