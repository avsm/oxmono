module Stable = struct
  open Core.Core_stable
  open Import_stable

  module Naming_scheme = struct
    module V1 = struct
      type t =
        [ `Numbered
        | `Timestamped
        | `Dated
        | `User_defined of (module Rotation_id.S)
        ]

      let sexp_of_t : t -> Core.Sexp.t = function
        | `Numbered -> Atom "Numbered"
        | `Timestamped -> Atom "Timestamped"
        | `Dated -> Atom "Dated"
        | `User_defined (_ : (module Rotation_id.S)) -> Atom "User_defined"
      ;;
    end
  end

  module V3 = struct
    type t =
      { messages : int option [@sexp.option]
      ; size : Byte_units.V1.t option [@sexp.option]
      ; time : Time_float.Ofday.V1.t option [@sexp.option]
      ; keep : [ `All | `Newer_than of Time_float.Span.V3.t | `At_least of int ]
      ; naming_scheme : Naming_scheme.V1.t
      ; zone : Time_float_unix.Zone.V1.t
      }
    [@@deriving fields ~getters ~iterators:fold, sexp_of]
  end
end

open! Core
open! Import

(* description of boundaries for file rotation. If all fields are None the file will never
   be rotated. Any field set to Some _ will cause rotation to happen when that boundary is
   crossed. Multiple boundaries may be set. Log rotation always causes incrementing
   rotation conditions (e.g. size) to reset, though this is the responsibililty of the
   caller to should_rotate.
*)

include Stable.V3

let create ?messages ?size ?time ?zone ~keep ~naming_scheme () =
  { messages
  ; size
  ; time
  ; zone = Option.value zone ~default:(force Time_float_unix.Zone.local)
  ; keep
  ; naming_scheme
  }
;;

let first_occurrence_after time ~ofday ~zone =
  let first_at_or_after time =
    Time_float.occurrence `First_after_or_at time ~ofday ~zone
  in
  let candidate = first_at_or_after time in
  (* we take care not to return the same time we were given *)
  if Time_float.equal time candidate
  then first_at_or_after (Time_float.add time Time_float.Span.robust_comparison_tolerance)
  else candidate
;;

let should_rotate t ~last_messages ~last_size ~last_time ~current_time =
  Fields.fold
    ~init:false
    ~messages:(fun acc field ->
      match Field.get field t with
      | None -> acc
      | Some rotate_messages -> acc || rotate_messages <= last_messages)
    ~size:(fun acc field ->
      match Field.get field t with
      | None -> acc
      | Some rotate_size -> acc || Byte_units.( <= ) rotate_size last_size)
    ~time:(fun acc field ->
      match Field.get field t with
      | None -> acc
      | Some rotation_ofday ->
        let rotation_time =
          first_occurrence_after last_time ~ofday:rotation_ofday ~zone:t.zone
        in
        acc || Time_float.( >= ) current_time rotation_time)
    ~zone:(fun acc _ -> acc)
    ~keep:(fun acc _ -> acc)
    ~naming_scheme:(fun acc _ -> acc)
;;

let default ?(zone = force Time_float_unix.Zone.local) () =
  { messages = None
  ; size = None
  ; time = Some Time_float.Ofday.start_of_day
  ; keep = `All
  ; naming_scheme = `Dated
  ; zone
  }
;;

module Naming_scheme = struct
  type t =
    [ `Numbered
    | `Timestamped
    | `Dated
    | `User_defined of (module Rotation_id.S)
    ]
end
