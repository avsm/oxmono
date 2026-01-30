open! Core
open! Import

type t =
  { time : Time_float.t
  ; level : Level.t option
  ; raw_message : Message_data.t
      (* [`Sexp] comes from uses of [%log.t.sexp] or from upstream logs, and [`String]
         comes from uses of [%log.t.string]; [`Sexp | `String] can also come from message
         events reconstructed out of serialized [Message]s.

         [`Structured] comes from uses of [%log.t]. *)
  ; source : Message_source.t
  ; legacy_tags : (string * string) list
  ; user_scope : string option
  ; function_name : string option
  ; async_trace_span : Univ.t option
  }
[@@deriving fields ~getters ~iterators:create]

let create
  ?time
  ?(source = Message_source.Manually_constructed "")
  ?(legacy_tags = [])
  ?level
  raw_message
  =
  let time = Option.value_or_thunk time ~default:Time_float.now in
  { raw_message
  ; source
  ; level
  ; time
  ; legacy_tags
  ; user_scope = None
  ; function_name = None
  ; async_trace_span = None
  }
;;

let set_level t ~level = { t with level }

let message t =
  match t.raw_message with
  | #Sexp_or_string.t as s -> Sexp_or_string.Stable.V1.to_string s
  | `Structured m -> Message_sexp.render m |> Sexp.to_string
;;

let downgrade_to_unstructured_and_map t ~f =
  { t with
    raw_message =
      ((match t.raw_message with
        | #Sexp_or_string.t as s -> f s
        | `Structured m -> f (`Sexp (Message_sexp.render m)))
        :> [ Sexp_or_string.t | `Structured of Message_sexp.t ])
  }
;;

let stringify_message_and_map t ~f = { t with raw_message = `String (f (message t)) }
let add_tags t ~tags = { t with legacy_tags = List.rev_append tags t.legacy_tags }
let map_legacy_tags t ~f = { t with legacy_tags = List.map t.legacy_tags ~f }
let legacy_tags t = t.legacy_tags
let map_time t ~f = { t with time = f t.time }

let to_serialized_message_lossy
  { raw_message
  ; source = _
  ; level
  ; time
  ; legacy_tags
  ; user_scope = _
  ; function_name = _
  ; async_trace_span = _
  }
  =
  (match raw_message with
   | #Sexp_or_string.t as s -> s
   | `Structured data -> `Sexp (Message_sexp.render data))
  |> Message.create ?level ~time ~tags:legacy_tags
;;

let of_serialized_message msg =
  { raw_message =
      (Message.raw_message msg :> [ Sexp_or_string.t | `Structured of Message_sexp.t ])
  ; source = Manually_constructed "from serialized message"
  ; level = Message.level msg
  ; time = Message.time msg
  ; legacy_tags = Message.tags msg
  ; user_scope = None
  ; function_name = None
  ; async_trace_span = None
  }
;;

module Unstable = struct
  module Time_float = struct
    type t = Time_float.t

    let sexp_of_t t =
      Time_ns.of_time_float_round_nearest t |> Time_ns.Alternate_sexp.sexp_of_t
    ;;
  end

  type nonrec t = t =
    { time : Time_float.t
    ; level : Level.t option [@sexp.option]
    ; raw_message : Message_data.Unstable.t
    ; source : Message_source.t Sexp_hidden_in_test.t
    ; legacy_tags : (string * string) list [@sexp.list]
    ; user_scope : string option [@sexp.option]
    ; function_name : string option [@sexp.option]
    ; async_trace_span : Univ.t option [@sexp.option]
    }
  [@@deriving sexp_of]
end

module Private = struct
  let create raw_message source = Fields.create ~raw_message ~source
  let async_trace_span = async_trace_span
  let user_scope = user_scope
end
