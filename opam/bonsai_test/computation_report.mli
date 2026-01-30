open! Core
open Bonsai_bench_scenario

module Incr_report : sig
  type t =
    { node_count : int
    ; nodes_created : int
    ; nodes_recomputed : int
    ; nodes_invalidated : int
    ; max_height : int
    ; max_node_id : int
    ; annotated_counts_diff : Bonsai.Private.Annotate_incr.Counts.t
    }
  [@@deriving sexp_of]

  (** [measure f] will compute the "incr" count report of the incremental nodes created
      during the execution of [f]. If you would like to time an async function/you can use
      a combination of [start_measure] and [finish_measure]. *)
  val measure : (unit -> 'a) -> 'a * t

  module Start_measurement : sig
    type t [@@deriving sexp_of]
  end

  (** [start_measure] will record the "current" state of incremental node counts. *)
  val start_measure : unit -> Start_measurement.t

  (** [finish_measure start_measurement] will compare the "current" state of incremental
      node counts with the counts when [start_measurement] was take and create a report. *)
  val finish_measure : Start_measurement.t -> t
end

module Startup : sig
  val run
    :  (local_ Bonsai.graph -> 'a Bonsai.t)
    -> Bonsai.Private.Skeleton.Counts.t * Incr_report.t

  val print_many
    :  (string, Bonsai.Private.Skeleton.Counts.t * Incr_report.t) Base.List.Assoc.t
    -> unit

  val run_and_print_compare
    :  computations:(string * ('input, _) compare_computation) list
    -> (string * 'input) list
    -> unit

  val diff_pairs_incr_summary_only
    :  ?title:string
    -> computation_pairs:
         (string * ('input, _) compare_computation * ('input, _) compare_computation) list
    -> (string * 'input) list
    -> unit
end

module Interaction : sig
  val run
    :  get_inject:('r -> ('action -> unit Ui_effect.t))
    -> (local_ Bonsai.graph -> 'r Bonsai.t)
    -> 'action Interaction.Finalized.t list
    -> Incr_report.t

  (** [print_max_height] defaults to [false] b/c it's the same for all interactions
      [print_node_count] defaults to [true] [print_max_node_id] defaults to [false] b/c
      it's almost the same as [print_num_created]. [print_num_created] defaults to [true]
      [print_num_recomputed] defaults to [true] [print_num_invalidated] defaults to [true] *)
  val run_and_print_compare
    :  ?print_max_height:bool
    -> ?print_node_count:bool
    -> ?print_max_node_id:bool
    -> ?print_num_created:bool
    -> ?print_num_recomputed:bool
    -> ?print_num_invalidated:bool
    -> ?title:string
    -> get_inject:('output -> ('action -> unit Bonsai.Effect.t))
    -> computations:(string * ('input, 'output) compare_computation) list
    -> ('input, 'action) Scenario.t list
    -> unit

  val diff_pairs
    :  ?print_max_height:bool
    -> ?print_node_count:bool
    -> ?print_max_node_id:bool
    -> ?print_num_created:bool
    -> ?print_num_recomputed:bool
    -> ?print_num_invalidated:bool
    -> ?title:string
    -> get_inject:('output -> ('action -> unit Bonsai.Effect.t))
    -> computation_pairs:
         (string
         * ('input, 'output) compare_computation
         * ('input, 'output) compare_computation)
           list
    -> ('input, 'action) Scenario.t list
    -> unit
end
