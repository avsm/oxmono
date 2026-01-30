open! Core
open! Hardcaml

include struct
  open Build_register_pipeline_intf
  module Pipeline_stage_descr = Pipeline_stage_descr
  module M = M

  module type Reg = Reg
end

module Make
    (Master : Interface.S)
    (Slave : Interface.S)
    (Reg : Reg with module Master := Master and module Slave := Slave) =
struct
  open Reg

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; slave_dn : 'a Slave.t [@rtlprefix "slave_dn$"]
      ; master_up : 'a Master.t [@rtlprefix "master_up$"]
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  module O = struct
    type 'a t =
      { slave_up : 'a Slave.t [@rtlprefix "slave_up$"]
      ; master_dn : 'a Master.t [@rtlprefix "master_dn$"]
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  module Pipeline_stage = struct
    type t =
      { master_dn : Signal.t Master.t
      ; slave_up : Signal.t Slave.t
      ; slave_dn : Signal.t Slave.t
      }
  end

  let build_pipeline ~config ~scope ~clock =
    let rec loop ~master_up ~pipeline_stages =
      match (pipeline_stages : Pipeline_stage_descr.t list) with
      | [] -> []
      | hd :: tl ->
        let this_stage =
          let slave_dn = Slave.Of_signal.wires () in
          let slave_up, master_dn =
            hierarchical
              ?instance:hd.instance_name
              ~config
              scope
              ~clock
              ~clear:hd.clear
              ~slave_dn
              ~master_up
          in
          { Pipeline_stage.master_dn; slave_up; slave_dn }
        in
        let remaining_stages = loop ~master_up:this_stage.master_dn ~pipeline_stages:tl in
        this_stage :: remaining_stages
    in
    fun ~master_up ~pipeline_stages -> loop ~master_up ~pipeline_stages
  ;;

  let pipeline_expert
    ~config
    ~(pipeline_stages : Pipeline_stage_descr.t list)
    scope
    ({ clock; clear = _; master_up; slave_dn } : _ I.t)
    =
    let pipeline = build_pipeline ~config ~scope ~clock ~master_up ~pipeline_stages in
    ignore
      (List.fold_right pipeline ~init:slave_dn ~f:(fun pipeline_stage slave_dn ->
         Slave.Of_signal.( <-- ) pipeline_stage.slave_dn slave_dn;
         pipeline_stage.slave_up)
       : Signal.t Slave.t);
    match pipeline_stages with
    | [] -> { O.master_dn = master_up; slave_up = slave_dn }
    | _ ->
      { O.master_dn = (List.last_exn pipeline).master_dn
      ; slave_up = (List.hd_exn pipeline).slave_up
      }
  ;;

  let pipeline_simple ?instance ~config ~n scope (i : _ I.t) =
    let pipeline_stages =
      List.init
        n
        ~f:(Fn.const { Pipeline_stage_descr.instance_name = instance; clear = i.clear })
    in
    pipeline_expert ~config ~pipeline_stages scope i
  ;;
end
