[@@@ocaml.warning "-27-32-37-60"]

type ro = Capnp.Message.ro
type rw = Capnp.Message.rw

module type S = sig
  module MessageWrapper : Capnp.RPC.S
  type 'cap message_t = 'cap MessageWrapper.Message.t
  type 'a reader_t = 'a MessageWrapper.StructStorage.reader_t
  type 'a builder_t = 'a MessageWrapper.StructStorage.builder_t

  module MetricsSource_15417368036688066650 : sig
    type t =
      | Agent
      | Host
      | Undefined of int
  end

  module Reader : sig
    type array_t
    type builder_array_t
    type pointer_t = ro MessageWrapper.Slice.t option
    val of_pointer : pointer_t -> 'a reader_t
    module DockerBuild : sig
      type struct_t = [`DockerBuild_b7cdf36259df55e5]
      type t = struct_t reader_t
      module Dockerfile : sig
        type struct_t = [`Dockerfile_82458b994ecd8ced]
        type t = struct_t reader_t
        type unnamed_union_t =
          | Contents of string
          | Path of string
          | Undefined of int
        val get : t -> unnamed_union_t
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      val dockerfile_get : t -> Dockerfile.t
      val has_push_target : t -> bool
      val push_target_get : t -> string
      val has_push_user : t -> bool
      val push_user_get : t -> string
      val has_push_password : t -> bool
      val push_password_get : t -> string
      val has_build_args : t -> bool
      val build_args_get : t -> (ro, string, array_t) Capnp.Array.t
      val build_args_get_list : t -> string list
      val build_args_get_array : t -> string array
      val squash_get : t -> bool
      val buildkit_get : t -> bool
      val include_git_get : t -> bool
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module OBuilder : sig
      type struct_t = [`OBuilder_8b282bf65fdee6f3]
      type t = struct_t reader_t
      val has_spec : t -> bool
      val spec_get : t -> string
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Custom : sig
      type struct_t = [`Custom_a5a1f28b5d271349]
      type t = struct_t reader_t
      val has_kind : t -> bool
      val kind_get : t -> string
      val payload_get : t -> pointer_t
      val payload_get_interface : t -> 'a MessageWrapper.Capability.t option
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Day10 : sig
      type struct_t = [`Day10_a3fa907d15d846a2]
      type t = struct_t reader_t
      val has_verb : t -> bool
      val verb_get : t -> string
      val has_opam_repository : t -> bool
      val opam_repository_get : t -> string
      val has_opam_repository_commit : t -> bool
      val opam_repository_commit_get : t -> string
      val has_opam_repository_base : t -> bool
      val opam_repository_base_get : t -> string
      val has_ocaml_version : t -> bool
      val ocaml_version_get : t -> string
      val has_package : t -> bool
      val package_get : t -> string
      val with_test_get : t -> bool
      val with_doc_get : t -> bool
      val lower_bound_get : t -> bool
      val has_dune_args : t -> bool
      val dune_args_get : t -> (ro, string, array_t) Capnp.Array.t
      val dune_args_get_list : t -> string list
      val dune_args_get_array : t -> string array
      val has_arch : t -> bool
      val arch_get : t -> string
      val has_os : t -> bool
      val os_get : t -> string
      val has_os_family : t -> bool
      val os_family_get : t -> string
      val has_os_distribution : t -> bool
      val os_distribution_get : t -> string
      val has_os_version : t -> bool
      val os_version_get : t -> string
      val has_only_packages : t -> bool
      val only_packages_get : t -> (ro, string, array_t) Capnp.Array.t
      val only_packages_get_list : t -> string list
      val only_packages_get_array : t -> string array
      val update_invariant_get : t -> bool
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Secret : sig
      type struct_t = [`Secret_d3d0bc06b7e0ea84]
      type t = struct_t reader_t
      val has_id : t -> bool
      val id_get : t -> string
      val has_value : t -> bool
      val value_get : t -> string
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module JobDescr : sig
      type struct_t = [`JobDescr_e8a11e4cd6914f3e]
      type t = struct_t reader_t
      module Action : sig
        type struct_t = [`Action_fd64455b116ae773]
        type t = struct_t reader_t
        type unnamed_union_t =
          | DockerBuild of DockerBuild.t
          | Obuilder of OBuilder.t
          | Custom of Custom.t
          | Undefined of int
        val get : t -> unnamed_union_t
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      val action_get : t -> Action.t
      val has_cache_hint : t -> bool
      val cache_hint_get : t -> string
      val has_repository : t -> bool
      val repository_get : t -> string
      val has_commits : t -> bool
      val commits_get : t -> (ro, string, array_t) Capnp.Array.t
      val commits_get_list : t -> string list
      val commits_get_array : t -> string array
      val has_secrets : t -> bool
      val secrets_get : t -> (ro, Secret.t, array_t) Capnp.Array.t
      val secrets_get_list : t -> Secret.t list
      val secrets_get_array : t -> Secret.t array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Job : sig
      type t = [`Job_ca66fd1f972efccc]
      module Log : sig
        module Params : sig
          type struct_t = [`Log_ee48e526c3cf1ac6]
          type t = struct_t reader_t
          val start_get : t -> int64
          val start_get_int_exn : t -> int
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`Log_87ce774bfb720195]
          type t = struct_t reader_t
          val has_log : t -> bool
          val log_get : t -> string
          val next_get : t -> int64
          val next_get_int_exn : t -> int
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
      module Result : sig
        module Params : sig
          type struct_t = [`Result_edc3bd08cf4a825e]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`Result_dda39f9d43376c0e]
          type t = struct_t reader_t
          val has_output : t -> bool
          val output_get : t -> string
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
      module Cancel : sig
        module Params : sig
          type struct_t = [`Cancel_ec46c649d15aa0b9]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`Cancel_d3176f1e80316652]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
    end
    module Queue : sig
      type t = [`Queue_817f986f80263aaf]
      module Pop : sig
        module Params : sig
          type struct_t = [`Pop_ec17b04ccaad2552]
          type t = struct_t reader_t
          val job_get : t -> Job.t MessageWrapper.Capability.t option
          val job_get_pipelined : struct_t MessageWrapper.StructRef.t -> Job.t MessageWrapper.Capability.t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`Pop_f8aeb978990dd018]
          type t = struct_t reader_t
          val has_descr : t -> bool
          val descr_get : t -> JobDescr.t
          val descr_get_pipelined : struct_t MessageWrapper.StructRef.t -> JobDescr.struct_t MessageWrapper.StructRef.t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
      module SetActive : sig
        module Params : sig
          type struct_t = [`SetActive_8941f84c1670d4c6]
          type t = struct_t reader_t
          val active_get : t -> bool
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`SetActive_ecfd397f1b4ef2d7]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
    end
    module Metric : sig
      type struct_t = [`Metric_a6d34960e6d0feab]
      type t = struct_t reader_t
      val has_content_type : t -> bool
      val content_type_get : t -> string
      val has_data : t -> bool
      val data_get : t -> string
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module AdditionalMetric : sig
      type struct_t = [`AdditionalMetric_cf9916b2e02f841b]
      type t = struct_t reader_t
      type unnamed_union_t =
        | Metric of Metric.t
        | NotReported
        | Undefined of int
      val get : t -> unnamed_union_t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Worker : sig
      type t = [`Worker_e06eedcf95bf7905]
      module MetricsSource : sig
        type t = MetricsSource_15417368036688066650.t =
          | Agent
          | Host
          | Undefined of int
      end
      module Metrics : sig
        module Params : sig
          type struct_t = [`Metrics_80b259789cf381c5]
          type t = struct_t reader_t
          val source_get : t -> MetricsSource_15417368036688066650.t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`Metrics_c9a605598c875b61]
          type t = struct_t reader_t
          val has_content_type : t -> bool
          val content_type_get : t -> string
          val has_data : t -> bool
          val data_get : t -> string
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
      module SelfUpdate : sig
        module Params : sig
          type struct_t = [`SelfUpdate_9e269f0be2d31020]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`SelfUpdate_a8cb29d2302f0044]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
      module AdditionalMetric : sig
        module Params : sig
          type struct_t = [`AdditionalMetric_c8efdd741d957fd2]
          type t = struct_t reader_t
          val has_source : t -> bool
          val source_get : t -> string
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`AdditionalMetric_a1250062bda8585a]
          type t = struct_t reader_t
          val has_metric : t -> bool
          val metric_get : t -> AdditionalMetric.t
          val metric_get_pipelined : struct_t MessageWrapper.StructRef.t -> AdditionalMetric.struct_t MessageWrapper.StructRef.t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
    end
    module Registration : sig
      type t = [`Registration_e098a12b91e35740]
      module Register : sig
        module Params : sig
          type struct_t = [`Register_cd3903a89e7d4463]
          type t = struct_t reader_t
          val has_name : t -> bool
          val name_get : t -> string
          val worker_get : t -> Worker.t MessageWrapper.Capability.t option
          val worker_get_pipelined : struct_t MessageWrapper.StructRef.t -> Worker.t MessageWrapper.Capability.t
          val capacity_get : t -> int32
          val capacity_get_int_exn : t -> int
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`Register_ed39dc99da78b704]
          type t = struct_t reader_t
          val queue_get : t -> Queue.t MessageWrapper.Capability.t option
          val queue_get_pipelined : struct_t MessageWrapper.StructRef.t -> Queue.t MessageWrapper.Capability.t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
    end
    module Ticket : sig
      type t = [`Ticket_9497c03b7ca5b27b]
      module Job : sig
        module Params : sig
          type struct_t = [`Job_fe1358984e6402b9]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`Job_aec939d3264640ca]
          type t = struct_t reader_t
          val job_get : t -> Job.t MessageWrapper.Capability.t option
          val job_get_pipelined : struct_t MessageWrapper.StructRef.t -> Job.t MessageWrapper.Capability.t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
      module Cancel : sig
        module Params : sig
          type struct_t = [`Cancel_cb46490a81e5e4f2]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`Cancel_82c6338a750c4a8b]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
    end
    module Submission : sig
      type t = [`Submission_f4e8a768b32a7c42]
      module Submit : sig
        module Params : sig
          type struct_t = [`Submit_97c091c41928fcd7]
          type t = struct_t reader_t
          val has_pool : t -> bool
          val pool_get : t -> string
          val has_descr : t -> bool
          val descr_get : t -> JobDescr.t
          val descr_get_pipelined : struct_t MessageWrapper.StructRef.t -> JobDescr.struct_t MessageWrapper.StructRef.t
          val urgent_get : t -> bool
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`Submit_d77a09dd23a9c879]
          type t = struct_t reader_t
          val ticket_get : t -> Ticket.t MessageWrapper.Capability.t option
          val ticket_get_pipelined : struct_t MessageWrapper.StructRef.t -> Ticket.t MessageWrapper.Capability.t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
    end
    module WorkerInfo : sig
      type struct_t = [`WorkerInfo_89d251d58984b1eb]
      type t = struct_t reader_t
      val has_name : t -> bool
      val name_get : t -> string
      val active_get : t -> bool
      val connected_get : t -> bool
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Progress : sig
      type t = [`Progress_d7529e2702cd958a]
      module Report : sig
        module Params : sig
          type struct_t = [`Report_9b1977ad3bcf968f]
          type t = struct_t reader_t
          val has_status : t -> bool
          val status_get : t -> string
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`Report_c679d2ad6191ba6b]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
    end
    module PoolAdmin : sig
      type t = [`PoolAdmin_d906b771b4082451]
      module Show : sig
        module Params : sig
          type struct_t = [`Show_8986d669d533884f]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`Show_d6c449f873bf3b5d]
          type t = struct_t reader_t
          val has_state : t -> bool
          val state_get : t -> string
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
      module Workers : sig
        module Params : sig
          type struct_t = [`Workers_90d9932353f84420]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`Workers_b8456ef113abb4da]
          type t = struct_t reader_t
          val has_workers : t -> bool
          val workers_get : t -> (ro, WorkerInfo.t, array_t) Capnp.Array.t
          val workers_get_list : t -> WorkerInfo.t list
          val workers_get_array : t -> WorkerInfo.t array
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
      module SetActive : sig
        module Params : sig
          type struct_t = [`SetActive_c0d1f54a45d947da]
          type t = struct_t reader_t
          val has_worker : t -> bool
          val worker_get : t -> string
          val active_get : t -> bool
          val auto_create_get : t -> bool
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`SetActive_effa2cb4bfc19817]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
      module Worker : sig
        module Params : sig
          type struct_t = [`Worker_a4ed3d1a44716756]
          type t = struct_t reader_t
          val has_worker : t -> bool
          val worker_get : t -> string
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`Worker_910014b068840d2d]
          type t = struct_t reader_t
          val worker_get : t -> Worker.t MessageWrapper.Capability.t option
          val worker_get_pipelined : struct_t MessageWrapper.StructRef.t -> Worker.t MessageWrapper.Capability.t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
      module Update : sig
        module Params : sig
          type struct_t = [`Update_babe541222805aba]
          type t = struct_t reader_t
          val has_worker : t -> bool
          val worker_get : t -> string
          val progress_get : t -> Progress.t MessageWrapper.Capability.t option
          val progress_get_pipelined : struct_t MessageWrapper.StructRef.t -> Progress.t MessageWrapper.Capability.t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`Update_b4b276698d845933]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
      module SetRate : sig
        module Params : sig
          type struct_t = [`SetRate_a51fc59ba9d4a74c]
          type t = struct_t reader_t
          val has_id : t -> bool
          val id_get : t -> string
          val rate_get : t -> float
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`SetRate_a4191ce8f8a947b4]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
      module Forget : sig
        module Params : sig
          type struct_t = [`Forget_c8d77e1465500b9f]
          type t = struct_t reader_t
          val has_worker : t -> bool
          val worker_get : t -> string
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`Forget_b706558828ef0e92]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
      module Drain : sig
        module Params : sig
          type struct_t = [`Drain_eda98c6d16599d71]
          type t = struct_t reader_t
          val has_worker : t -> bool
          val worker_get : t -> string
          val progress_get : t -> Progress.t MessageWrapper.Capability.t option
          val progress_get_pipelined : struct_t MessageWrapper.StructRef.t -> Progress.t MessageWrapper.Capability.t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`Drain_f66636042afca0d6]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
    end
    module Admin : sig
      type t = [`Admin_dd51f35540dac6ad]
      module Pools : sig
        module Params : sig
          type struct_t = [`Pools_f2c2424ab87727b4]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`Pools_d0a9d0d20dd4cc17]
          type t = struct_t reader_t
          val has_names : t -> bool
          val names_get : t -> (ro, string, array_t) Capnp.Array.t
          val names_get_list : t -> string list
          val names_get_array : t -> string array
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
      module Pool : sig
        module Params : sig
          type struct_t = [`Pool_9d6a16f5eccc6c15]
          type t = struct_t reader_t
          val has_name : t -> bool
          val name_get : t -> string
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`Pool_99524b1b0c00d533]
          type t = struct_t reader_t
          val pool_get : t -> PoolAdmin.t MessageWrapper.Capability.t option
          val pool_get_pipelined : struct_t MessageWrapper.StructRef.t -> PoolAdmin.t MessageWrapper.Capability.t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
      module AddClient : sig
        module Params : sig
          type struct_t = [`AddClient_b939aee2e3e952e3]
          type t = struct_t reader_t
          val has_id : t -> bool
          val id_get : t -> string
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`AddClient_9f0245ffd4d76d34]
          type t = struct_t reader_t
          val cap_get : t -> Submission.t MessageWrapper.Capability.t option
          val cap_get_pipelined : struct_t MessageWrapper.StructRef.t -> Submission.t MessageWrapper.Capability.t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
      module RemoveClient : sig
        module Params : sig
          type struct_t = [`RemoveClient_a05fc26f1e08b666]
          type t = struct_t reader_t
          val has_id : t -> bool
          val id_get : t -> string
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`RemoveClient_dd6b877ef2fcbb6f]
          type t = struct_t reader_t
          val cap_get : t -> Submission.t MessageWrapper.Capability.t option
          val cap_get_pipelined : struct_t MessageWrapper.StructRef.t -> Submission.t MessageWrapper.Capability.t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
      module ListClients : sig
        module Params : sig
          type struct_t = [`ListClients_a672ef7cb50805ba]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`ListClients_8d50536b48de2056]
          type t = struct_t reader_t
          val has_clients : t -> bool
          val clients_get : t -> (ro, string, array_t) Capnp.Array.t
          val clients_get_list : t -> string list
          val clients_get_array : t -> string array
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
    end
  end

  module Builder : sig
    type array_t = Reader.builder_array_t
    type reader_array_t = Reader.array_t
    type pointer_t = rw MessageWrapper.Slice.t
    module DockerBuild : sig
      type struct_t = [`DockerBuild_b7cdf36259df55e5]
      type t = struct_t builder_t
      module Dockerfile : sig
        type struct_t = [`Dockerfile_82458b994ecd8ced]
        type t = struct_t builder_t
        type unnamed_union_t =
          | Contents of string
          | Path of string
          | Undefined of int
        val get : t -> unnamed_union_t
        val contents_set : t -> string -> unit
        val path_set : t -> string -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      val dockerfile_get : t -> Dockerfile.t
      val dockerfile_init : t -> Dockerfile.t
      val has_push_target : t -> bool
      val push_target_get : t -> string
      val push_target_set : t -> string -> unit
      val has_push_user : t -> bool
      val push_user_get : t -> string
      val push_user_set : t -> string -> unit
      val has_push_password : t -> bool
      val push_password_get : t -> string
      val push_password_set : t -> string -> unit
      val has_build_args : t -> bool
      val build_args_get : t -> (rw, string, array_t) Capnp.Array.t
      val build_args_get_list : t -> string list
      val build_args_get_array : t -> string array
      val build_args_set : t -> (rw, string, array_t) Capnp.Array.t -> (rw, string, array_t) Capnp.Array.t
      val build_args_set_list : t -> string list -> (rw, string, array_t) Capnp.Array.t
      val build_args_set_array : t -> string array -> (rw, string, array_t) Capnp.Array.t
      val build_args_init : t -> int -> (rw, string, array_t) Capnp.Array.t
      val squash_get : t -> bool
      val squash_set : t -> bool -> unit
      val buildkit_get : t -> bool
      val buildkit_set : t -> bool -> unit
      val include_git_get : t -> bool
      val include_git_set : t -> bool -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module OBuilder : sig
      type struct_t = [`OBuilder_8b282bf65fdee6f3]
      type t = struct_t builder_t
      val has_spec : t -> bool
      val spec_get : t -> string
      val spec_set : t -> string -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Custom : sig
      type struct_t = [`Custom_a5a1f28b5d271349]
      type t = struct_t builder_t
      val has_kind : t -> bool
      val kind_get : t -> string
      val kind_set : t -> string -> unit
      val payload_get : t -> pointer_t
      val payload_get_interface : t -> 'a MessageWrapper.Capability.t option
      val payload_set : t -> pointer_t -> pointer_t
      val payload_set_reader : t -> Reader.pointer_t -> pointer_t
      val payload_set_interface : t -> 'a MessageWrapper.Capability.t option -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Day10 : sig
      type struct_t = [`Day10_a3fa907d15d846a2]
      type t = struct_t builder_t
      val has_verb : t -> bool
      val verb_get : t -> string
      val verb_set : t -> string -> unit
      val has_opam_repository : t -> bool
      val opam_repository_get : t -> string
      val opam_repository_set : t -> string -> unit
      val has_opam_repository_commit : t -> bool
      val opam_repository_commit_get : t -> string
      val opam_repository_commit_set : t -> string -> unit
      val has_opam_repository_base : t -> bool
      val opam_repository_base_get : t -> string
      val opam_repository_base_set : t -> string -> unit
      val has_ocaml_version : t -> bool
      val ocaml_version_get : t -> string
      val ocaml_version_set : t -> string -> unit
      val has_package : t -> bool
      val package_get : t -> string
      val package_set : t -> string -> unit
      val with_test_get : t -> bool
      val with_test_set : t -> bool -> unit
      val with_doc_get : t -> bool
      val with_doc_set : t -> bool -> unit
      val lower_bound_get : t -> bool
      val lower_bound_set : t -> bool -> unit
      val has_dune_args : t -> bool
      val dune_args_get : t -> (rw, string, array_t) Capnp.Array.t
      val dune_args_get_list : t -> string list
      val dune_args_get_array : t -> string array
      val dune_args_set : t -> (rw, string, array_t) Capnp.Array.t -> (rw, string, array_t) Capnp.Array.t
      val dune_args_set_list : t -> string list -> (rw, string, array_t) Capnp.Array.t
      val dune_args_set_array : t -> string array -> (rw, string, array_t) Capnp.Array.t
      val dune_args_init : t -> int -> (rw, string, array_t) Capnp.Array.t
      val has_arch : t -> bool
      val arch_get : t -> string
      val arch_set : t -> string -> unit
      val has_os : t -> bool
      val os_get : t -> string
      val os_set : t -> string -> unit
      val has_os_family : t -> bool
      val os_family_get : t -> string
      val os_family_set : t -> string -> unit
      val has_os_distribution : t -> bool
      val os_distribution_get : t -> string
      val os_distribution_set : t -> string -> unit
      val has_os_version : t -> bool
      val os_version_get : t -> string
      val os_version_set : t -> string -> unit
      val has_only_packages : t -> bool
      val only_packages_get : t -> (rw, string, array_t) Capnp.Array.t
      val only_packages_get_list : t -> string list
      val only_packages_get_array : t -> string array
      val only_packages_set : t -> (rw, string, array_t) Capnp.Array.t -> (rw, string, array_t) Capnp.Array.t
      val only_packages_set_list : t -> string list -> (rw, string, array_t) Capnp.Array.t
      val only_packages_set_array : t -> string array -> (rw, string, array_t) Capnp.Array.t
      val only_packages_init : t -> int -> (rw, string, array_t) Capnp.Array.t
      val update_invariant_get : t -> bool
      val update_invariant_set : t -> bool -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Secret : sig
      type struct_t = [`Secret_d3d0bc06b7e0ea84]
      type t = struct_t builder_t
      val has_id : t -> bool
      val id_get : t -> string
      val id_set : t -> string -> unit
      val has_value : t -> bool
      val value_get : t -> string
      val value_set : t -> string -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module JobDescr : sig
      type struct_t = [`JobDescr_e8a11e4cd6914f3e]
      type t = struct_t builder_t
      module Action : sig
        type struct_t = [`Action_fd64455b116ae773]
        type t = struct_t builder_t
        type unnamed_union_t =
          | DockerBuild of DockerBuild.t
          | Obuilder of OBuilder.t
          | Custom of Custom.t
          | Undefined of int
        val get : t -> unnamed_union_t
        val docker_build_set_reader : t -> DockerBuild.struct_t reader_t -> DockerBuild.t
        val docker_build_set_builder : t -> DockerBuild.t -> DockerBuild.t
        val docker_build_init : t -> DockerBuild.t
        val obuilder_set_reader : t -> OBuilder.struct_t reader_t -> OBuilder.t
        val obuilder_set_builder : t -> OBuilder.t -> OBuilder.t
        val obuilder_init : t -> OBuilder.t
        val custom_set_reader : t -> Custom.struct_t reader_t -> Custom.t
        val custom_set_builder : t -> Custom.t -> Custom.t
        val custom_init : t -> Custom.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      val action_get : t -> Action.t
      val action_init : t -> Action.t
      val has_cache_hint : t -> bool
      val cache_hint_get : t -> string
      val cache_hint_set : t -> string -> unit
      val has_repository : t -> bool
      val repository_get : t -> string
      val repository_set : t -> string -> unit
      val has_commits : t -> bool
      val commits_get : t -> (rw, string, array_t) Capnp.Array.t
      val commits_get_list : t -> string list
      val commits_get_array : t -> string array
      val commits_set : t -> (rw, string, array_t) Capnp.Array.t -> (rw, string, array_t) Capnp.Array.t
      val commits_set_list : t -> string list -> (rw, string, array_t) Capnp.Array.t
      val commits_set_array : t -> string array -> (rw, string, array_t) Capnp.Array.t
      val commits_init : t -> int -> (rw, string, array_t) Capnp.Array.t
      val has_secrets : t -> bool
      val secrets_get : t -> (rw, Secret.t, array_t) Capnp.Array.t
      val secrets_get_list : t -> Secret.t list
      val secrets_get_array : t -> Secret.t array
      val secrets_set : t -> (rw, Secret.t, array_t) Capnp.Array.t -> (rw, Secret.t, array_t) Capnp.Array.t
      val secrets_set_list : t -> Secret.t list -> (rw, Secret.t, array_t) Capnp.Array.t
      val secrets_set_array : t -> Secret.t array -> (rw, Secret.t, array_t) Capnp.Array.t
      val secrets_init : t -> int -> (rw, Secret.t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Job : sig
      type t = [`Job_ca66fd1f972efccc]
      module Log : sig
        module Params : sig
          type struct_t = [`Log_ee48e526c3cf1ac6]
          type t = struct_t builder_t
          val start_get : t -> int64
          val start_get_int_exn : t -> int
          val start_set : t -> int64 -> unit
          val start_set_int : t -> int -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`Log_87ce774bfb720195]
          type t = struct_t builder_t
          val has_log : t -> bool
          val log_get : t -> string
          val log_set : t -> string -> unit
          val next_get : t -> int64
          val next_get_int_exn : t -> int
          val next_set : t -> int64 -> unit
          val next_set_int : t -> int -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
      module Result : sig
        module Params : sig
          type struct_t = [`Result_edc3bd08cf4a825e]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`Result_dda39f9d43376c0e]
          type t = struct_t builder_t
          val has_output : t -> bool
          val output_get : t -> string
          val output_set : t -> string -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
      module Cancel : sig
        module Params : sig
          type struct_t = [`Cancel_ec46c649d15aa0b9]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`Cancel_d3176f1e80316652]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
    end
    module Queue : sig
      type t = [`Queue_817f986f80263aaf]
      module Pop : sig
        module Params : sig
          type struct_t = [`Pop_ec17b04ccaad2552]
          type t = struct_t builder_t
          val job_get : t -> Job.t MessageWrapper.Capability.t option
          val job_set : t -> Job.t MessageWrapper.Capability.t option -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`Pop_f8aeb978990dd018]
          type t = struct_t builder_t
          val has_descr : t -> bool
          val descr_get : t -> JobDescr.t
          val descr_set_reader : t -> JobDescr.struct_t reader_t -> JobDescr.t
          val descr_set_builder : t -> JobDescr.t -> JobDescr.t
          val descr_init : t -> JobDescr.t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
      module SetActive : sig
        module Params : sig
          type struct_t = [`SetActive_8941f84c1670d4c6]
          type t = struct_t builder_t
          val active_get : t -> bool
          val active_set : t -> bool -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`SetActive_ecfd397f1b4ef2d7]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
    end
    module Metric : sig
      type struct_t = [`Metric_a6d34960e6d0feab]
      type t = struct_t builder_t
      val has_content_type : t -> bool
      val content_type_get : t -> string
      val content_type_set : t -> string -> unit
      val has_data : t -> bool
      val data_get : t -> string
      val data_set : t -> string -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module AdditionalMetric : sig
      type struct_t = [`AdditionalMetric_cf9916b2e02f841b]
      type t = struct_t builder_t
      type unnamed_union_t =
        | Metric of Metric.t
        | NotReported
        | Undefined of int
      val get : t -> unnamed_union_t
      val metric_set_reader : t -> Metric.struct_t reader_t -> Metric.t
      val metric_set_builder : t -> Metric.t -> Metric.t
      val metric_init : t -> Metric.t
      val not_reported_set : t -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Worker : sig
      type t = [`Worker_e06eedcf95bf7905]
      module MetricsSource : sig
        type t = MetricsSource_15417368036688066650.t =
          | Agent
          | Host
          | Undefined of int
      end
      module Metrics : sig
        module Params : sig
          type struct_t = [`Metrics_80b259789cf381c5]
          type t = struct_t builder_t
          val source_get : t -> MetricsSource_15417368036688066650.t
          val source_set : t -> MetricsSource_15417368036688066650.t -> unit
          val source_set_unsafe : t -> MetricsSource_15417368036688066650.t -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`Metrics_c9a605598c875b61]
          type t = struct_t builder_t
          val has_content_type : t -> bool
          val content_type_get : t -> string
          val content_type_set : t -> string -> unit
          val has_data : t -> bool
          val data_get : t -> string
          val data_set : t -> string -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
      module SelfUpdate : sig
        module Params : sig
          type struct_t = [`SelfUpdate_9e269f0be2d31020]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`SelfUpdate_a8cb29d2302f0044]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
      module AdditionalMetric : sig
        module Params : sig
          type struct_t = [`AdditionalMetric_c8efdd741d957fd2]
          type t = struct_t builder_t
          val has_source : t -> bool
          val source_get : t -> string
          val source_set : t -> string -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`AdditionalMetric_a1250062bda8585a]
          type t = struct_t builder_t
          val has_metric : t -> bool
          val metric_get : t -> AdditionalMetric.t
          val metric_set_reader : t -> AdditionalMetric.struct_t reader_t -> AdditionalMetric.t
          val metric_set_builder : t -> AdditionalMetric.t -> AdditionalMetric.t
          val metric_init : t -> AdditionalMetric.t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
    end
    module Registration : sig
      type t = [`Registration_e098a12b91e35740]
      module Register : sig
        module Params : sig
          type struct_t = [`Register_cd3903a89e7d4463]
          type t = struct_t builder_t
          val has_name : t -> bool
          val name_get : t -> string
          val name_set : t -> string -> unit
          val worker_get : t -> Worker.t MessageWrapper.Capability.t option
          val worker_set : t -> Worker.t MessageWrapper.Capability.t option -> unit
          val capacity_get : t -> int32
          val capacity_get_int_exn : t -> int
          val capacity_set : t -> int32 -> unit
          val capacity_set_int_exn : t -> int -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`Register_ed39dc99da78b704]
          type t = struct_t builder_t
          val queue_get : t -> Queue.t MessageWrapper.Capability.t option
          val queue_set : t -> Queue.t MessageWrapper.Capability.t option -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
    end
    module Ticket : sig
      type t = [`Ticket_9497c03b7ca5b27b]
      module Job : sig
        module Params : sig
          type struct_t = [`Job_fe1358984e6402b9]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`Job_aec939d3264640ca]
          type t = struct_t builder_t
          val job_get : t -> Job.t MessageWrapper.Capability.t option
          val job_set : t -> Job.t MessageWrapper.Capability.t option -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
      module Cancel : sig
        module Params : sig
          type struct_t = [`Cancel_cb46490a81e5e4f2]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`Cancel_82c6338a750c4a8b]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
    end
    module Submission : sig
      type t = [`Submission_f4e8a768b32a7c42]
      module Submit : sig
        module Params : sig
          type struct_t = [`Submit_97c091c41928fcd7]
          type t = struct_t builder_t
          val has_pool : t -> bool
          val pool_get : t -> string
          val pool_set : t -> string -> unit
          val has_descr : t -> bool
          val descr_get : t -> JobDescr.t
          val descr_set_reader : t -> JobDescr.struct_t reader_t -> JobDescr.t
          val descr_set_builder : t -> JobDescr.t -> JobDescr.t
          val descr_init : t -> JobDescr.t
          val urgent_get : t -> bool
          val urgent_set : t -> bool -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`Submit_d77a09dd23a9c879]
          type t = struct_t builder_t
          val ticket_get : t -> Ticket.t MessageWrapper.Capability.t option
          val ticket_set : t -> Ticket.t MessageWrapper.Capability.t option -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
    end
    module WorkerInfo : sig
      type struct_t = [`WorkerInfo_89d251d58984b1eb]
      type t = struct_t builder_t
      val has_name : t -> bool
      val name_get : t -> string
      val name_set : t -> string -> unit
      val active_get : t -> bool
      val active_set : t -> bool -> unit
      val connected_get : t -> bool
      val connected_set : t -> bool -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Progress : sig
      type t = [`Progress_d7529e2702cd958a]
      module Report : sig
        module Params : sig
          type struct_t = [`Report_9b1977ad3bcf968f]
          type t = struct_t builder_t
          val has_status : t -> bool
          val status_get : t -> string
          val status_set : t -> string -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`Report_c679d2ad6191ba6b]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
    end
    module PoolAdmin : sig
      type t = [`PoolAdmin_d906b771b4082451]
      module Show : sig
        module Params : sig
          type struct_t = [`Show_8986d669d533884f]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`Show_d6c449f873bf3b5d]
          type t = struct_t builder_t
          val has_state : t -> bool
          val state_get : t -> string
          val state_set : t -> string -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
      module Workers : sig
        module Params : sig
          type struct_t = [`Workers_90d9932353f84420]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`Workers_b8456ef113abb4da]
          type t = struct_t builder_t
          val has_workers : t -> bool
          val workers_get : t -> (rw, WorkerInfo.t, array_t) Capnp.Array.t
          val workers_get_list : t -> WorkerInfo.t list
          val workers_get_array : t -> WorkerInfo.t array
          val workers_set : t -> (rw, WorkerInfo.t, array_t) Capnp.Array.t -> (rw, WorkerInfo.t, array_t) Capnp.Array.t
          val workers_set_list : t -> WorkerInfo.t list -> (rw, WorkerInfo.t, array_t) Capnp.Array.t
          val workers_set_array : t -> WorkerInfo.t array -> (rw, WorkerInfo.t, array_t) Capnp.Array.t
          val workers_init : t -> int -> (rw, WorkerInfo.t, array_t) Capnp.Array.t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
      module SetActive : sig
        module Params : sig
          type struct_t = [`SetActive_c0d1f54a45d947da]
          type t = struct_t builder_t
          val has_worker : t -> bool
          val worker_get : t -> string
          val worker_set : t -> string -> unit
          val active_get : t -> bool
          val active_set : t -> bool -> unit
          val auto_create_get : t -> bool
          val auto_create_set : t -> bool -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`SetActive_effa2cb4bfc19817]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
      module Worker : sig
        module Params : sig
          type struct_t = [`Worker_a4ed3d1a44716756]
          type t = struct_t builder_t
          val has_worker : t -> bool
          val worker_get : t -> string
          val worker_set : t -> string -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`Worker_910014b068840d2d]
          type t = struct_t builder_t
          val worker_get : t -> Worker.t MessageWrapper.Capability.t option
          val worker_set : t -> Worker.t MessageWrapper.Capability.t option -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
      module Update : sig
        module Params : sig
          type struct_t = [`Update_babe541222805aba]
          type t = struct_t builder_t
          val has_worker : t -> bool
          val worker_get : t -> string
          val worker_set : t -> string -> unit
          val progress_get : t -> Progress.t MessageWrapper.Capability.t option
          val progress_set : t -> Progress.t MessageWrapper.Capability.t option -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`Update_b4b276698d845933]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
      module SetRate : sig
        module Params : sig
          type struct_t = [`SetRate_a51fc59ba9d4a74c]
          type t = struct_t builder_t
          val has_id : t -> bool
          val id_get : t -> string
          val id_set : t -> string -> unit
          val rate_get : t -> float
          val rate_set : t -> float -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`SetRate_a4191ce8f8a947b4]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
      module Forget : sig
        module Params : sig
          type struct_t = [`Forget_c8d77e1465500b9f]
          type t = struct_t builder_t
          val has_worker : t -> bool
          val worker_get : t -> string
          val worker_set : t -> string -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`Forget_b706558828ef0e92]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
      module Drain : sig
        module Params : sig
          type struct_t = [`Drain_eda98c6d16599d71]
          type t = struct_t builder_t
          val has_worker : t -> bool
          val worker_get : t -> string
          val worker_set : t -> string -> unit
          val progress_get : t -> Progress.t MessageWrapper.Capability.t option
          val progress_set : t -> Progress.t MessageWrapper.Capability.t option -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`Drain_f66636042afca0d6]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
    end
    module Admin : sig
      type t = [`Admin_dd51f35540dac6ad]
      module Pools : sig
        module Params : sig
          type struct_t = [`Pools_f2c2424ab87727b4]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`Pools_d0a9d0d20dd4cc17]
          type t = struct_t builder_t
          val has_names : t -> bool
          val names_get : t -> (rw, string, array_t) Capnp.Array.t
          val names_get_list : t -> string list
          val names_get_array : t -> string array
          val names_set : t -> (rw, string, array_t) Capnp.Array.t -> (rw, string, array_t) Capnp.Array.t
          val names_set_list : t -> string list -> (rw, string, array_t) Capnp.Array.t
          val names_set_array : t -> string array -> (rw, string, array_t) Capnp.Array.t
          val names_init : t -> int -> (rw, string, array_t) Capnp.Array.t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
      module Pool : sig
        module Params : sig
          type struct_t = [`Pool_9d6a16f5eccc6c15]
          type t = struct_t builder_t
          val has_name : t -> bool
          val name_get : t -> string
          val name_set : t -> string -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`Pool_99524b1b0c00d533]
          type t = struct_t builder_t
          val pool_get : t -> PoolAdmin.t MessageWrapper.Capability.t option
          val pool_set : t -> PoolAdmin.t MessageWrapper.Capability.t option -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
      module AddClient : sig
        module Params : sig
          type struct_t = [`AddClient_b939aee2e3e952e3]
          type t = struct_t builder_t
          val has_id : t -> bool
          val id_get : t -> string
          val id_set : t -> string -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`AddClient_9f0245ffd4d76d34]
          type t = struct_t builder_t
          val cap_get : t -> Submission.t MessageWrapper.Capability.t option
          val cap_set : t -> Submission.t MessageWrapper.Capability.t option -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
      module RemoveClient : sig
        module Params : sig
          type struct_t = [`RemoveClient_a05fc26f1e08b666]
          type t = struct_t builder_t
          val has_id : t -> bool
          val id_get : t -> string
          val id_set : t -> string -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`RemoveClient_dd6b877ef2fcbb6f]
          type t = struct_t builder_t
          val cap_get : t -> Submission.t MessageWrapper.Capability.t option
          val cap_set : t -> Submission.t MessageWrapper.Capability.t option -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
      module ListClients : sig
        module Params : sig
          type struct_t = [`ListClients_a672ef7cb50805ba]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`ListClients_8d50536b48de2056]
          type t = struct_t builder_t
          val has_clients : t -> bool
          val clients_get : t -> (rw, string, array_t) Capnp.Array.t
          val clients_get_list : t -> string list
          val clients_get_array : t -> string array
          val clients_set : t -> (rw, string, array_t) Capnp.Array.t -> (rw, string, array_t) Capnp.Array.t
          val clients_set_list : t -> string list -> (rw, string, array_t) Capnp.Array.t
          val clients_set_array : t -> string array -> (rw, string, array_t) Capnp.Array.t
          val clients_init : t -> int -> (rw, string, array_t) Capnp.Array.t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
    end
  end
end

module MakeRPC(MessageWrapper : Capnp.RPC.S) = struct
  type 'a reader_t = 'a MessageWrapper.StructStorage.reader_t
  type 'a builder_t = 'a MessageWrapper.StructStorage.builder_t
  module CamlBytes = Bytes
  module DefaultsMessage_ = Capnp.BytesMessage

  let _builder_defaults_message =
    let message_segments = [
      Bytes.unsafe_of_string "\
      ";
    ] in
    DefaultsMessage_.Message.readonly
      (DefaultsMessage_.Message.of_storage message_segments)

  let invalid_msg = Capnp.Message.invalid_msg

  include Capnp.Runtime.BuilderInc.Make(MessageWrapper)

  type 'cap message_t = 'cap MessageWrapper.Message.t

  module MetricsSource_15417368036688066650 = struct
    type t =
      | Agent
      | Host
      | Undefined of int
    let decode u16 = match u16 with
      | 0 -> Agent
      | 1 -> Host
      | v -> Undefined v
    let encode_safe enum = match enum with
      | Agent -> 0
      | Host -> 1
      | Undefined x -> invalid_msg "Cannot encode undefined enum value."
    let encode_unsafe enum = match enum with
      | Agent -> 0
      | Host -> 1
      | Undefined x -> x
  end
  module DefaultsCopier_ =
    Capnp.Runtime.BuilderOps.Make(Capnp.BytesMessage)(MessageWrapper)

  let _reader_defaults_message =
    MessageWrapper.Message.create
      (DefaultsMessage_.Message.total_size _builder_defaults_message)


  module Reader = struct
    type array_t = ro MessageWrapper.ListStorage.t
    type builder_array_t = rw MessageWrapper.ListStorage.t
    type pointer_t = ro MessageWrapper.Slice.t option
    let of_pointer = RA_.deref_opt_struct_pointer

    module DockerBuild = struct
      type struct_t = [`DockerBuild_b7cdf36259df55e5]
      type t = struct_t reader_t
      module Dockerfile = struct
        type struct_t = [`Dockerfile_82458b994ecd8ced]
        type t = struct_t reader_t
        let has_contents x =
          RA_.has_field x 0
        let contents_get x =
          RA_.get_text ~default:"" x 0
        let has_path x =
          RA_.has_field x 0
        let path_get x =
          RA_.get_text ~default:"" x 0
        type unnamed_union_t =
          | Contents of string
          | Path of string
          | Undefined of int
        let get x =
          match RA_.get_uint16 ~default:0 x 0 with
          | 0 -> Contents (contents_get x)
          | 1 -> Path (path_get x)
          | v -> Undefined v
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      let dockerfile_get x = RA_.cast_struct x
      let has_push_target x =
        RA_.has_field x 1
      let push_target_get x =
        RA_.get_text ~default:"" x 1
      let has_push_user x =
        RA_.has_field x 2
      let push_user_get x =
        RA_.get_text ~default:"" x 2
      let has_push_password x =
        RA_.has_field x 3
      let push_password_get x =
        RA_.get_text ~default:"" x 3
      let has_build_args x =
        RA_.has_field x 4
      let build_args_get x =
        RA_.get_text_list x 4
      let build_args_get_list x =
        Capnp.Array.to_list (build_args_get x)
      let build_args_get_array x =
        Capnp.Array.to_array (build_args_get x)
      let squash_get x =
        RA_.get_bit ~default:false x ~byte_ofs:2 ~bit_ofs:0
      let buildkit_get x =
        RA_.get_bit ~default:false x ~byte_ofs:2 ~bit_ofs:1
      let include_git_get x =
        RA_.get_bit ~default:false x ~byte_ofs:2 ~bit_ofs:2
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module OBuilder = struct
      type struct_t = [`OBuilder_8b282bf65fdee6f3]
      type t = struct_t reader_t
      let has_spec x =
        RA_.has_field x 0
      let spec_get x =
        RA_.get_text ~default:"" x 0
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Custom = struct
      type struct_t = [`Custom_a5a1f28b5d271349]
      type t = struct_t reader_t
      let has_kind x =
        RA_.has_field x 0
      let kind_get x =
        RA_.get_text ~default:"" x 0
      let payload_get x =
        RA_.get_pointer x 1
      let payload_get_interface x =
        RA_.get_interface x 1
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Day10 = struct
      type struct_t = [`Day10_a3fa907d15d846a2]
      type t = struct_t reader_t
      let has_verb x =
        RA_.has_field x 0
      let verb_get x =
        RA_.get_text ~default:"" x 0
      let has_opam_repository x =
        RA_.has_field x 1
      let opam_repository_get x =
        RA_.get_text ~default:"" x 1
      let has_opam_repository_commit x =
        RA_.has_field x 2
      let opam_repository_commit_get x =
        RA_.get_text ~default:"" x 2
      let has_opam_repository_base x =
        RA_.has_field x 3
      let opam_repository_base_get x =
        RA_.get_text ~default:"" x 3
      let has_ocaml_version x =
        RA_.has_field x 4
      let ocaml_version_get x =
        RA_.get_text ~default:"" x 4
      let has_package x =
        RA_.has_field x 5
      let package_get x =
        RA_.get_text ~default:"" x 5
      let with_test_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
      let with_doc_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:1
      let lower_bound_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:2
      let has_dune_args x =
        RA_.has_field x 6
      let dune_args_get x =
        RA_.get_text_list x 6
      let dune_args_get_list x =
        Capnp.Array.to_list (dune_args_get x)
      let dune_args_get_array x =
        Capnp.Array.to_array (dune_args_get x)
      let has_arch x =
        RA_.has_field x 7
      let arch_get x =
        RA_.get_text ~default:"" x 7
      let has_os x =
        RA_.has_field x 8
      let os_get x =
        RA_.get_text ~default:"" x 8
      let has_os_family x =
        RA_.has_field x 9
      let os_family_get x =
        RA_.get_text ~default:"" x 9
      let has_os_distribution x =
        RA_.has_field x 10
      let os_distribution_get x =
        RA_.get_text ~default:"" x 10
      let has_os_version x =
        RA_.has_field x 11
      let os_version_get x =
        RA_.get_text ~default:"" x 11
      let has_only_packages x =
        RA_.has_field x 12
      let only_packages_get x =
        RA_.get_text_list x 12
      let only_packages_get_list x =
        Capnp.Array.to_list (only_packages_get x)
      let only_packages_get_array x =
        Capnp.Array.to_array (only_packages_get x)
      let update_invariant_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:3
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Secret = struct
      type struct_t = [`Secret_d3d0bc06b7e0ea84]
      type t = struct_t reader_t
      let has_id x =
        RA_.has_field x 0
      let id_get x =
        RA_.get_text ~default:"" x 0
      let has_value x =
        RA_.has_field x 1
      let value_get x =
        RA_.get_text ~default:"" x 1
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module JobDescr = struct
      type struct_t = [`JobDescr_e8a11e4cd6914f3e]
      type t = struct_t reader_t
      module Action = struct
        type struct_t = [`Action_fd64455b116ae773]
        type t = struct_t reader_t
        let has_docker_build x =
          RA_.has_field x 0
        let docker_build_get x =
          RA_.get_struct x 0
        let docker_build_get_pipelined x =
          MessageWrapper.Untyped.struct_field x 0
        let has_obuilder x =
          RA_.has_field x 0
        let obuilder_get x =
          RA_.get_struct x 0
        let obuilder_get_pipelined x =
          MessageWrapper.Untyped.struct_field x 0
        let has_custom x =
          RA_.has_field x 0
        let custom_get x =
          RA_.get_struct x 0
        let custom_get_pipelined x =
          MessageWrapper.Untyped.struct_field x 0
        type unnamed_union_t =
          | DockerBuild of DockerBuild.t
          | Obuilder of OBuilder.t
          | Custom of Custom.t
          | Undefined of int
        let get x =
          match RA_.get_uint16 ~default:0 x 0 with
          | 0 -> DockerBuild (docker_build_get x)
          | 1 -> Obuilder (obuilder_get x)
          | 2 -> Custom (custom_get x)
          | v -> Undefined v
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      let action_get x = RA_.cast_struct x
      let has_cache_hint x =
        RA_.has_field x 1
      let cache_hint_get x =
        RA_.get_text ~default:"" x 1
      let has_repository x =
        RA_.has_field x 2
      let repository_get x =
        RA_.get_text ~default:"" x 2
      let has_commits x =
        RA_.has_field x 3
      let commits_get x =
        RA_.get_text_list x 3
      let commits_get_list x =
        Capnp.Array.to_list (commits_get x)
      let commits_get_array x =
        Capnp.Array.to_array (commits_get x)
      let has_secrets x =
        RA_.has_field x 4
      let secrets_get x = 
        RA_.get_struct_list x 4
      let secrets_get_list x =
        Capnp.Array.to_list (secrets_get x)
      let secrets_get_array x =
        Capnp.Array.to_array (secrets_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Job = struct
      type t = [`Job_ca66fd1f972efccc]
      module Log = struct
        module Params = struct
          type struct_t = [`Log_ee48e526c3cf1ac6]
          type t = struct_t reader_t
          let start_get x =
            RA_.get_int64 ~default:(0L) x 0
          let start_get_int_exn x =
            Capnp.Runtime.Util.int_of_int64_exn (start_get x)
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`Log_87ce774bfb720195]
          type t = struct_t reader_t
          let has_log x =
            RA_.has_field x 0
          let log_get x =
            RA_.get_blob ~default:"" x 0
          let next_get x =
            RA_.get_int64 ~default:(0L) x 0
          let next_get_int_exn x =
            Capnp.Runtime.Util.int_of_int64_exn (next_get x)
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
      module Result = struct
        module Params = struct
          type struct_t = [`Result_edc3bd08cf4a825e]
          type t = struct_t reader_t
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`Result_dda39f9d43376c0e]
          type t = struct_t reader_t
          let has_output x =
            RA_.has_field x 0
          let output_get x =
            RA_.get_text ~default:"" x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
      module Cancel = struct
        module Params = struct
          type struct_t = [`Cancel_ec46c649d15aa0b9]
          type t = struct_t reader_t
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`Cancel_d3176f1e80316652]
          type t = struct_t reader_t
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
    end
    module Queue = struct
      type t = [`Queue_817f986f80263aaf]
      module Pop = struct
        module Params = struct
          type struct_t = [`Pop_ec17b04ccaad2552]
          type t = struct_t reader_t
          let job_get x =
            RA_.get_interface x 0
          let job_get_pipelined x = 
            MessageWrapper.Untyped.capability_field x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`Pop_f8aeb978990dd018]
          type t = struct_t reader_t
          let has_descr x =
            RA_.has_field x 0
          let descr_get x =
            RA_.get_struct x 0
          let descr_get_pipelined x =
            MessageWrapper.Untyped.struct_field x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
      module SetActive = struct
        module Params = struct
          type struct_t = [`SetActive_8941f84c1670d4c6]
          type t = struct_t reader_t
          let active_get x =
            RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`SetActive_ecfd397f1b4ef2d7]
          type t = struct_t reader_t
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
    end
    module Metric = struct
      type struct_t = [`Metric_a6d34960e6d0feab]
      type t = struct_t reader_t
      let has_content_type x =
        RA_.has_field x 0
      let content_type_get x =
        RA_.get_text ~default:"" x 0
      let has_data x =
        RA_.has_field x 1
      let data_get x =
        RA_.get_text ~default:"" x 1
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module AdditionalMetric = struct
      type struct_t = [`AdditionalMetric_cf9916b2e02f841b]
      type t = struct_t reader_t
      let has_metric x =
        RA_.has_field x 0
      let metric_get x =
        RA_.get_struct x 0
      let metric_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let not_reported_get x = ()
      type unnamed_union_t =
        | Metric of Metric.t
        | NotReported
        | Undefined of int
      let get x =
        match RA_.get_uint16 ~default:0 x 0 with
        | 0 -> Metric (metric_get x)
        | 1 -> NotReported
        | v -> Undefined v
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Worker = struct
      type t = [`Worker_e06eedcf95bf7905]
      module MetricsSource = struct
        type t = MetricsSource_15417368036688066650.t =
          | Agent
          | Host
          | Undefined of int
      end
      module Metrics = struct
        module Params = struct
          type struct_t = [`Metrics_80b259789cf381c5]
          type t = struct_t reader_t
          let source_get x =
            let discr = RA_.get_uint16 ~default:0 x 0 in
            MetricsSource_15417368036688066650.decode discr
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`Metrics_c9a605598c875b61]
          type t = struct_t reader_t
          let has_content_type x =
            RA_.has_field x 0
          let content_type_get x =
            RA_.get_text ~default:"" x 0
          let has_data x =
            RA_.has_field x 1
          let data_get x =
            RA_.get_text ~default:"" x 1
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
      module SelfUpdate = struct
        module Params = struct
          type struct_t = [`SelfUpdate_9e269f0be2d31020]
          type t = struct_t reader_t
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`SelfUpdate_a8cb29d2302f0044]
          type t = struct_t reader_t
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
      module AdditionalMetric = struct
        module Params = struct
          type struct_t = [`AdditionalMetric_c8efdd741d957fd2]
          type t = struct_t reader_t
          let has_source x =
            RA_.has_field x 0
          let source_get x =
            RA_.get_text ~default:"" x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`AdditionalMetric_a1250062bda8585a]
          type t = struct_t reader_t
          let has_metric x =
            RA_.has_field x 0
          let metric_get x =
            RA_.get_struct x 0
          let metric_get_pipelined x =
            MessageWrapper.Untyped.struct_field x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
    end
    module Registration = struct
      type t = [`Registration_e098a12b91e35740]
      module Register = struct
        module Params = struct
          type struct_t = [`Register_cd3903a89e7d4463]
          type t = struct_t reader_t
          let has_name x =
            RA_.has_field x 0
          let name_get x =
            RA_.get_text ~default:"" x 0
          let worker_get x =
            RA_.get_interface x 1
          let worker_get_pipelined x = 
            MessageWrapper.Untyped.capability_field x 1
          let capacity_get x =
            RA_.get_int32 ~default:(0l) x 0
          let capacity_get_int_exn x =
            Capnp.Runtime.Util.int_of_int32_exn (capacity_get x)
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`Register_ed39dc99da78b704]
          type t = struct_t reader_t
          let queue_get x =
            RA_.get_interface x 0
          let queue_get_pipelined x = 
            MessageWrapper.Untyped.capability_field x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
    end
    module Ticket = struct
      type t = [`Ticket_9497c03b7ca5b27b]
      module Job = struct
        module Params = struct
          type struct_t = [`Job_fe1358984e6402b9]
          type t = struct_t reader_t
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`Job_aec939d3264640ca]
          type t = struct_t reader_t
          let job_get x =
            RA_.get_interface x 0
          let job_get_pipelined x = 
            MessageWrapper.Untyped.capability_field x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
      module Cancel = struct
        module Params = struct
          type struct_t = [`Cancel_cb46490a81e5e4f2]
          type t = struct_t reader_t
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`Cancel_82c6338a750c4a8b]
          type t = struct_t reader_t
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
    end
    module Submission = struct
      type t = [`Submission_f4e8a768b32a7c42]
      module Submit = struct
        module Params = struct
          type struct_t = [`Submit_97c091c41928fcd7]
          type t = struct_t reader_t
          let has_pool x =
            RA_.has_field x 0
          let pool_get x =
            RA_.get_text ~default:"" x 0
          let has_descr x =
            RA_.has_field x 1
          let descr_get x =
            RA_.get_struct x 1
          let descr_get_pipelined x =
            MessageWrapper.Untyped.struct_field x 1
          let urgent_get x =
            RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`Submit_d77a09dd23a9c879]
          type t = struct_t reader_t
          let ticket_get x =
            RA_.get_interface x 0
          let ticket_get_pipelined x = 
            MessageWrapper.Untyped.capability_field x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
    end
    module WorkerInfo = struct
      type struct_t = [`WorkerInfo_89d251d58984b1eb]
      type t = struct_t reader_t
      let has_name x =
        RA_.has_field x 0
      let name_get x =
        RA_.get_text ~default:"" x 0
      let active_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
      let connected_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:1
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Progress = struct
      type t = [`Progress_d7529e2702cd958a]
      module Report = struct
        module Params = struct
          type struct_t = [`Report_9b1977ad3bcf968f]
          type t = struct_t reader_t
          let has_status x =
            RA_.has_field x 0
          let status_get x =
            RA_.get_text ~default:"" x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`Report_c679d2ad6191ba6b]
          type t = struct_t reader_t
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
    end
    module PoolAdmin = struct
      type t = [`PoolAdmin_d906b771b4082451]
      module Show = struct
        module Params = struct
          type struct_t = [`Show_8986d669d533884f]
          type t = struct_t reader_t
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`Show_d6c449f873bf3b5d]
          type t = struct_t reader_t
          let has_state x =
            RA_.has_field x 0
          let state_get x =
            RA_.get_text ~default:"" x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
      module Workers = struct
        module Params = struct
          type struct_t = [`Workers_90d9932353f84420]
          type t = struct_t reader_t
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`Workers_b8456ef113abb4da]
          type t = struct_t reader_t
          let has_workers x =
            RA_.has_field x 0
          let workers_get x = 
            RA_.get_struct_list x 0
          let workers_get_list x =
            Capnp.Array.to_list (workers_get x)
          let workers_get_array x =
            Capnp.Array.to_array (workers_get x)
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
      module SetActive = struct
        module Params = struct
          type struct_t = [`SetActive_c0d1f54a45d947da]
          type t = struct_t reader_t
          let has_worker x =
            RA_.has_field x 0
          let worker_get x =
            RA_.get_text ~default:"" x 0
          let active_get x =
            RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
          let auto_create_get x =
            RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:1
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`SetActive_effa2cb4bfc19817]
          type t = struct_t reader_t
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
      module Worker = struct
        module Params = struct
          type struct_t = [`Worker_a4ed3d1a44716756]
          type t = struct_t reader_t
          let has_worker x =
            RA_.has_field x 0
          let worker_get x =
            RA_.get_text ~default:"" x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`Worker_910014b068840d2d]
          type t = struct_t reader_t
          let worker_get x =
            RA_.get_interface x 0
          let worker_get_pipelined x = 
            MessageWrapper.Untyped.capability_field x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
      module Update = struct
        module Params = struct
          type struct_t = [`Update_babe541222805aba]
          type t = struct_t reader_t
          let has_worker x =
            RA_.has_field x 0
          let worker_get x =
            RA_.get_text ~default:"" x 0
          let progress_get x =
            RA_.get_interface x 1
          let progress_get_pipelined x = 
            MessageWrapper.Untyped.capability_field x 1
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`Update_b4b276698d845933]
          type t = struct_t reader_t
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
      module SetRate = struct
        module Params = struct
          type struct_t = [`SetRate_a51fc59ba9d4a74c]
          type t = struct_t reader_t
          let has_id x =
            RA_.has_field x 0
          let id_get x =
            RA_.get_text ~default:"" x 0
          let rate_get x =
            RA_.get_float64 ~default_bits:(0L) x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`SetRate_a4191ce8f8a947b4]
          type t = struct_t reader_t
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
      module Forget = struct
        module Params = struct
          type struct_t = [`Forget_c8d77e1465500b9f]
          type t = struct_t reader_t
          let has_worker x =
            RA_.has_field x 0
          let worker_get x =
            RA_.get_text ~default:"" x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`Forget_b706558828ef0e92]
          type t = struct_t reader_t
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
      module Drain = struct
        module Params = struct
          type struct_t = [`Drain_eda98c6d16599d71]
          type t = struct_t reader_t
          let has_worker x =
            RA_.has_field x 0
          let worker_get x =
            RA_.get_text ~default:"" x 0
          let progress_get x =
            RA_.get_interface x 1
          let progress_get_pipelined x = 
            MessageWrapper.Untyped.capability_field x 1
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`Drain_f66636042afca0d6]
          type t = struct_t reader_t
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
    end
    module Admin = struct
      type t = [`Admin_dd51f35540dac6ad]
      module Pools = struct
        module Params = struct
          type struct_t = [`Pools_f2c2424ab87727b4]
          type t = struct_t reader_t
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`Pools_d0a9d0d20dd4cc17]
          type t = struct_t reader_t
          let has_names x =
            RA_.has_field x 0
          let names_get x =
            RA_.get_text_list x 0
          let names_get_list x =
            Capnp.Array.to_list (names_get x)
          let names_get_array x =
            Capnp.Array.to_array (names_get x)
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
      module Pool = struct
        module Params = struct
          type struct_t = [`Pool_9d6a16f5eccc6c15]
          type t = struct_t reader_t
          let has_name x =
            RA_.has_field x 0
          let name_get x =
            RA_.get_text ~default:"" x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`Pool_99524b1b0c00d533]
          type t = struct_t reader_t
          let pool_get x =
            RA_.get_interface x 0
          let pool_get_pipelined x = 
            MessageWrapper.Untyped.capability_field x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
      module AddClient = struct
        module Params = struct
          type struct_t = [`AddClient_b939aee2e3e952e3]
          type t = struct_t reader_t
          let has_id x =
            RA_.has_field x 0
          let id_get x =
            RA_.get_text ~default:"" x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`AddClient_9f0245ffd4d76d34]
          type t = struct_t reader_t
          let cap_get x =
            RA_.get_interface x 0
          let cap_get_pipelined x = 
            MessageWrapper.Untyped.capability_field x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
      module RemoveClient = struct
        module Params = struct
          type struct_t = [`RemoveClient_a05fc26f1e08b666]
          type t = struct_t reader_t
          let has_id x =
            RA_.has_field x 0
          let id_get x =
            RA_.get_text ~default:"" x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`RemoveClient_dd6b877ef2fcbb6f]
          type t = struct_t reader_t
          let cap_get x =
            RA_.get_interface x 0
          let cap_get_pipelined x = 
            MessageWrapper.Untyped.capability_field x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
      module ListClients = struct
        module Params = struct
          type struct_t = [`ListClients_a672ef7cb50805ba]
          type t = struct_t reader_t
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`ListClients_8d50536b48de2056]
          type t = struct_t reader_t
          let has_clients x =
            RA_.has_field x 0
          let clients_get x =
            RA_.get_text_list x 0
          let clients_get_list x =
            Capnp.Array.to_list (clients_get x)
          let clients_get_array x =
            Capnp.Array.to_array (clients_get x)
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
    end
  end

  module Builder = struct
    type array_t = Reader.builder_array_t
    type reader_array_t = Reader.array_t
    type pointer_t = rw MessageWrapper.Slice.t

    module DockerBuild = struct
      type struct_t = [`DockerBuild_b7cdf36259df55e5]
      type t = struct_t builder_t
      module Dockerfile = struct
        type struct_t = [`Dockerfile_82458b994ecd8ced]
        type t = struct_t builder_t
        let has_contents x =
          BA_.has_field x 0
        let contents_get x =
          BA_.get_text ~default:"" x 0
        let contents_set x v =
          BA_.set_text ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0 v
        let has_path x =
          BA_.has_field x 0
        let path_get x =
          BA_.get_text ~default:"" x 0
        let path_set x v =
          BA_.set_text ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x 0 v
        type unnamed_union_t =
          | Contents of string
          | Path of string
          | Undefined of int
        let get x =
          match BA_.get_uint16 ~default:0 x 0 with
          | 0 -> Contents (contents_get x)
          | 1 -> Path (path_get x)
          | v -> Undefined v
        let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:5 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:5 ()
        let init_pointer ptr =
          BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:5
      end
      let dockerfile_get x = BA_.cast_struct x
      let dockerfile_init x =
        let data = x.BA_.NM.StructStorage.data in
        let pointers = x.BA_.NM.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_int16 ~default:0 x 0 0 in
        let () =
          let ptr = {
            pointers with
            MessageWrapper.Slice.start = pointers.MessageWrapper.Slice.start + 0;
            MessageWrapper.Slice.len = 8;
          } in
          let () = BA_.BOps.deep_zero_pointer ptr in
          MessageWrapper.Slice.set_int64 ptr 0 0L
        in
        let () =
          let ptr = {
            pointers with
            MessageWrapper.Slice.start = pointers.MessageWrapper.Slice.start + 0;
            MessageWrapper.Slice.len = 8;
          } in
          let () = BA_.BOps.deep_zero_pointer ptr in
          MessageWrapper.Slice.set_int64 ptr 0 0L
        in
        BA_.cast_struct x
      let has_push_target x =
        BA_.has_field x 1
      let push_target_get x =
        BA_.get_text ~default:"" x 1
      let push_target_set x v =
        BA_.set_text x 1 v
      let has_push_user x =
        BA_.has_field x 2
      let push_user_get x =
        BA_.get_text ~default:"" x 2
      let push_user_set x v =
        BA_.set_text x 2 v
      let has_push_password x =
        BA_.has_field x 3
      let push_password_get x =
        BA_.get_text ~default:"" x 3
      let push_password_set x v =
        BA_.set_text x 3 v
      let has_build_args x =
        BA_.has_field x 4
      let build_args_get x =
        BA_.get_text_list x 4
      let build_args_get_list x =
        Capnp.Array.to_list (build_args_get x)
      let build_args_get_array x =
        Capnp.Array.to_array (build_args_get x)
      let build_args_set x v =
        BA_.set_text_list x 4 v
      let build_args_init x n =
        BA_.init_text_list x 4 n
      let build_args_set_list x v =
        let builder = build_args_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let build_args_set_array x v =
        let builder = build_args_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let squash_get x =
        BA_.get_bit ~default:false x ~byte_ofs:2 ~bit_ofs:0
      let squash_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:2 ~bit_ofs:0 v
      let buildkit_get x =
        BA_.get_bit ~default:false x ~byte_ofs:2 ~bit_ofs:1
      let buildkit_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:2 ~bit_ofs:1 v
      let include_git_get x =
        BA_.get_bit ~default:false x ~byte_ofs:2 ~bit_ofs:2
      let include_git_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:2 ~bit_ofs:2 v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:5 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:5 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:5
    end
    module OBuilder = struct
      type struct_t = [`OBuilder_8b282bf65fdee6f3]
      type t = struct_t builder_t
      let has_spec x =
        BA_.has_field x 0
      let spec_get x =
        BA_.get_text ~default:"" x 0
      let spec_set x v =
        BA_.set_text x 0 v
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
    end
    module Custom = struct
      type struct_t = [`Custom_a5a1f28b5d271349]
      type t = struct_t builder_t
      let has_kind x =
        BA_.has_field x 0
      let kind_get x =
        BA_.get_text ~default:"" x 0
      let kind_set x v =
        BA_.set_text x 0 v
      let payload_get x =
        BA_.get_pointer x 1
      let payload_get_interface x =
        BA_.get_interface x 1
      let payload_set x v =
        BA_.set_pointer x 1 (Some v)
      let payload_set_reader x v =
        BA_.set_pointer x 1 v
      let payload_set_interface x v =
        BA_.set_interface x 1 v
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:2 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:2 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:2
    end
    module Day10 = struct
      type struct_t = [`Day10_a3fa907d15d846a2]
      type t = struct_t builder_t
      let has_verb x =
        BA_.has_field x 0
      let verb_get x =
        BA_.get_text ~default:"" x 0
      let verb_set x v =
        BA_.set_text x 0 v
      let has_opam_repository x =
        BA_.has_field x 1
      let opam_repository_get x =
        BA_.get_text ~default:"" x 1
      let opam_repository_set x v =
        BA_.set_text x 1 v
      let has_opam_repository_commit x =
        BA_.has_field x 2
      let opam_repository_commit_get x =
        BA_.get_text ~default:"" x 2
      let opam_repository_commit_set x v =
        BA_.set_text x 2 v
      let has_opam_repository_base x =
        BA_.has_field x 3
      let opam_repository_base_get x =
        BA_.get_text ~default:"" x 3
      let opam_repository_base_set x v =
        BA_.set_text x 3 v
      let has_ocaml_version x =
        BA_.has_field x 4
      let ocaml_version_get x =
        BA_.get_text ~default:"" x 4
      let ocaml_version_set x v =
        BA_.set_text x 4 v
      let has_package x =
        BA_.has_field x 5
      let package_get x =
        BA_.get_text ~default:"" x 5
      let package_set x v =
        BA_.set_text x 5 v
      let with_test_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
      let with_test_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0 v
      let with_doc_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:1
      let with_doc_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:1 v
      let lower_bound_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:2
      let lower_bound_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:2 v
      let has_dune_args x =
        BA_.has_field x 6
      let dune_args_get x =
        BA_.get_text_list x 6
      let dune_args_get_list x =
        Capnp.Array.to_list (dune_args_get x)
      let dune_args_get_array x =
        Capnp.Array.to_array (dune_args_get x)
      let dune_args_set x v =
        BA_.set_text_list x 6 v
      let dune_args_init x n =
        BA_.init_text_list x 6 n
      let dune_args_set_list x v =
        let builder = dune_args_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let dune_args_set_array x v =
        let builder = dune_args_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_arch x =
        BA_.has_field x 7
      let arch_get x =
        BA_.get_text ~default:"" x 7
      let arch_set x v =
        BA_.set_text x 7 v
      let has_os x =
        BA_.has_field x 8
      let os_get x =
        BA_.get_text ~default:"" x 8
      let os_set x v =
        BA_.set_text x 8 v
      let has_os_family x =
        BA_.has_field x 9
      let os_family_get x =
        BA_.get_text ~default:"" x 9
      let os_family_set x v =
        BA_.set_text x 9 v
      let has_os_distribution x =
        BA_.has_field x 10
      let os_distribution_get x =
        BA_.get_text ~default:"" x 10
      let os_distribution_set x v =
        BA_.set_text x 10 v
      let has_os_version x =
        BA_.has_field x 11
      let os_version_get x =
        BA_.get_text ~default:"" x 11
      let os_version_set x v =
        BA_.set_text x 11 v
      let has_only_packages x =
        BA_.has_field x 12
      let only_packages_get x =
        BA_.get_text_list x 12
      let only_packages_get_list x =
        Capnp.Array.to_list (only_packages_get x)
      let only_packages_get_array x =
        Capnp.Array.to_array (only_packages_get x)
      let only_packages_set x v =
        BA_.set_text_list x 12 v
      let only_packages_init x n =
        BA_.init_text_list x 12 n
      let only_packages_set_list x v =
        let builder = only_packages_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let only_packages_set_array x v =
        let builder = only_packages_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let update_invariant_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:3
      let update_invariant_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:3 v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:13 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:13 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:13
    end
    module Secret = struct
      type struct_t = [`Secret_d3d0bc06b7e0ea84]
      type t = struct_t builder_t
      let has_id x =
        BA_.has_field x 0
      let id_get x =
        BA_.get_text ~default:"" x 0
      let id_set x v =
        BA_.set_text x 0 v
      let has_value x =
        BA_.has_field x 1
      let value_get x =
        BA_.get_text ~default:"" x 1
      let value_set x v =
        BA_.set_text x 1 v
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:2 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:2 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:2
    end
    module JobDescr = struct
      type struct_t = [`JobDescr_e8a11e4cd6914f3e]
      type t = struct_t builder_t
      module Action = struct
        type struct_t = [`Action_fd64455b116ae773]
        type t = struct_t builder_t
        let has_docker_build x =
          BA_.has_field x 0
        let docker_build_get x =
          BA_.get_struct ~data_words:1 ~pointer_words:5 x 0
        let docker_build_set_reader x v =
          BA_.set_struct ~data_words:1 ~pointer_words:5 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0 v
        let docker_build_set_builder x v =
          BA_.set_struct ~data_words:1 ~pointer_words:5 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0 (Some v)
        let docker_build_init x =
          BA_.init_struct ~data_words:1 ~pointer_words:5 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0
        let has_obuilder x =
          BA_.has_field x 0
        let obuilder_get x =
          BA_.get_struct ~data_words:0 ~pointer_words:1 x 0
        let obuilder_set_reader x v =
          BA_.set_struct ~data_words:0 ~pointer_words:1 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x 0 v
        let obuilder_set_builder x v =
          BA_.set_struct ~data_words:0 ~pointer_words:1 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x 0 (Some v)
        let obuilder_init x =
          BA_.init_struct ~data_words:0 ~pointer_words:1 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x 0
        let has_custom x =
          BA_.has_field x 0
        let custom_get x =
          BA_.get_struct ~data_words:0 ~pointer_words:2 x 0
        let custom_set_reader x v =
          BA_.set_struct ~data_words:0 ~pointer_words:2 ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} x 0 v
        let custom_set_builder x v =
          BA_.set_struct ~data_words:0 ~pointer_words:2 ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} x 0 (Some v)
        let custom_init x =
          BA_.init_struct ~data_words:0 ~pointer_words:2 ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} x 0
        type unnamed_union_t =
          | DockerBuild of DockerBuild.t
          | Obuilder of OBuilder.t
          | Custom of Custom.t
          | Undefined of int
        let get x =
          match BA_.get_uint16 ~default:0 x 0 with
          | 0 -> DockerBuild (docker_build_get x)
          | 1 -> Obuilder (obuilder_get x)
          | 2 -> Custom (custom_get x)
          | v -> Undefined v
        let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:5 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:5 ()
        let init_pointer ptr =
          BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:5
      end
      let action_get x = BA_.cast_struct x
      let action_init x =
        let data = x.BA_.NM.StructStorage.data in
        let pointers = x.BA_.NM.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_int16 ~default:0 x 0 0 in
        let () =
          let ptr = {
            pointers with
            MessageWrapper.Slice.start = pointers.MessageWrapper.Slice.start + 0;
            MessageWrapper.Slice.len = 8;
          } in
          let () = BA_.BOps.deep_zero_pointer ptr in
          MessageWrapper.Slice.set_int64 ptr 0 0L
        in
        let () =
          let ptr = {
            pointers with
            MessageWrapper.Slice.start = pointers.MessageWrapper.Slice.start + 0;
            MessageWrapper.Slice.len = 8;
          } in
          let () = BA_.BOps.deep_zero_pointer ptr in
          MessageWrapper.Slice.set_int64 ptr 0 0L
        in
        let () =
          let ptr = {
            pointers with
            MessageWrapper.Slice.start = pointers.MessageWrapper.Slice.start + 0;
            MessageWrapper.Slice.len = 8;
          } in
          let () = BA_.BOps.deep_zero_pointer ptr in
          MessageWrapper.Slice.set_int64 ptr 0 0L
        in
        BA_.cast_struct x
      let has_cache_hint x =
        BA_.has_field x 1
      let cache_hint_get x =
        BA_.get_text ~default:"" x 1
      let cache_hint_set x v =
        BA_.set_text x 1 v
      let has_repository x =
        BA_.has_field x 2
      let repository_get x =
        BA_.get_text ~default:"" x 2
      let repository_set x v =
        BA_.set_text x 2 v
      let has_commits x =
        BA_.has_field x 3
      let commits_get x =
        BA_.get_text_list x 3
      let commits_get_list x =
        Capnp.Array.to_list (commits_get x)
      let commits_get_array x =
        Capnp.Array.to_array (commits_get x)
      let commits_set x v =
        BA_.set_text_list x 3 v
      let commits_init x n =
        BA_.init_text_list x 3 n
      let commits_set_list x v =
        let builder = commits_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let commits_set_array x v =
        let builder = commits_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_secrets x =
        BA_.has_field x 4
      let secrets_get x = 
        BA_.get_struct_list ~data_words:0 ~pointer_words:2 x 4
      let secrets_get_list x =
        Capnp.Array.to_list (secrets_get x)
      let secrets_get_array x =
        Capnp.Array.to_array (secrets_get x)
      let secrets_set x v =
        BA_.set_struct_list ~data_words:0 ~pointer_words:2 x 4 v
      let secrets_init x n =
        BA_.init_struct_list ~data_words:0 ~pointer_words:2 x 4 n
      let secrets_set_list x v =
        let builder = secrets_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let secrets_set_array x v =
        let builder = secrets_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:5 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:5 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:5
    end
    module Job = struct
      type t = [`Job_ca66fd1f972efccc]
      module Log = struct
        module Params = struct
          type struct_t = [`Log_ee48e526c3cf1ac6]
          type t = struct_t builder_t
          let start_get x =
            BA_.get_int64 ~default:(0L) x 0
          let start_get_int_exn x =
            Capnp.Runtime.Util.int_of_int64_exn (start_get x)
          let start_set x v =
            BA_.set_int64 ~default:(0L) x 0 v
          let start_set_int x v = start_set x (Int64.of_int v)
          let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:0
        end
        module Results = struct
          type struct_t = [`Log_87ce774bfb720195]
          type t = struct_t builder_t
          let has_log x =
            BA_.has_field x 0
          let log_get x =
            BA_.get_blob ~default:"" x 0
          let log_set x v =
            BA_.set_blob x 0 v
          let next_get x =
            BA_.get_int64 ~default:(0L) x 0
          let next_get_int_exn x =
            Capnp.Runtime.Util.int_of_int64_exn (next_get x)
          let next_set x v =
            BA_.set_int64 ~default:(0L) x 0 v
          let next_set_int x v = next_set x (Int64.of_int v)
          let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
        end
      end
      module Result = struct
        module Params = struct
          type struct_t = [`Result_edc3bd08cf4a825e]
          type t = struct_t builder_t
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:0
        end
        module Results = struct
          type struct_t = [`Result_dda39f9d43376c0e]
          type t = struct_t builder_t
          let has_output x =
            BA_.has_field x 0
          let output_get x =
            BA_.get_text ~default:"" x 0
          let output_set x v =
            BA_.set_text x 0 v
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
      end
      module Cancel = struct
        module Params = struct
          type struct_t = [`Cancel_ec46c649d15aa0b9]
          type t = struct_t builder_t
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:0
        end
        module Results = struct
          type struct_t = [`Cancel_d3176f1e80316652]
          type t = struct_t builder_t
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:0
        end
      end
    end
    module Queue = struct
      type t = [`Queue_817f986f80263aaf]
      module Pop = struct
        module Params = struct
          type struct_t = [`Pop_ec17b04ccaad2552]
          type t = struct_t builder_t
          let job_get x =
            BA_.get_interface x 0
          let job_get_pipelined x = 
            MessageWrapper.Untyped.capability_field x 0
          let job_set x v =
            BA_.set_interface x 0 v
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
        module Results = struct
          type struct_t = [`Pop_f8aeb978990dd018]
          type t = struct_t builder_t
          let has_descr x =
            BA_.has_field x 0
          let descr_get x =
            BA_.get_struct ~data_words:1 ~pointer_words:5 x 0
          let descr_set_reader x v =
            BA_.set_struct ~data_words:1 ~pointer_words:5 x 0 v
          let descr_set_builder x v =
            BA_.set_struct ~data_words:1 ~pointer_words:5 x 0 (Some v)
          let descr_init x =
            BA_.init_struct ~data_words:1 ~pointer_words:5 x 0
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
      end
      module SetActive = struct
        module Params = struct
          type struct_t = [`SetActive_8941f84c1670d4c6]
          type t = struct_t builder_t
          let active_get x =
            BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
          let active_set x v =
            BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0 v
          let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:0
        end
        module Results = struct
          type struct_t = [`SetActive_ecfd397f1b4ef2d7]
          type t = struct_t builder_t
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:0
        end
      end
    end
    module Metric = struct
      type struct_t = [`Metric_a6d34960e6d0feab]
      type t = struct_t builder_t
      let has_content_type x =
        BA_.has_field x 0
      let content_type_get x =
        BA_.get_text ~default:"" x 0
      let content_type_set x v =
        BA_.set_text x 0 v
      let has_data x =
        BA_.has_field x 1
      let data_get x =
        BA_.get_text ~default:"" x 1
      let data_set x v =
        BA_.set_text x 1 v
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:2 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:2 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:2
    end
    module AdditionalMetric = struct
      type struct_t = [`AdditionalMetric_cf9916b2e02f841b]
      type t = struct_t builder_t
      let has_metric x =
        BA_.has_field x 0
      let metric_get x =
        BA_.get_struct ~data_words:0 ~pointer_words:2 x 0
      let metric_set_reader x v =
        BA_.set_struct ~data_words:0 ~pointer_words:2 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0 v
      let metric_set_builder x v =
        BA_.set_struct ~data_words:0 ~pointer_words:2 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let metric_init x =
        BA_.init_struct ~data_words:0 ~pointer_words:2 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0
      let not_reported_get x = ()
      let not_reported_set x =
        BA_.set_void ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x
      type unnamed_union_t =
        | Metric of Metric.t
        | NotReported
        | Undefined of int
      let get x =
        match BA_.get_uint16 ~default:0 x 0 with
        | 0 -> Metric (metric_get x)
        | 1 -> NotReported
        | v -> Undefined v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module Worker = struct
      type t = [`Worker_e06eedcf95bf7905]
      module MetricsSource = struct
        type t = MetricsSource_15417368036688066650.t =
          | Agent
          | Host
          | Undefined of int
      end
      module Metrics = struct
        module Params = struct
          type struct_t = [`Metrics_80b259789cf381c5]
          type t = struct_t builder_t
          let source_get x =
            let discr = BA_.get_uint16 ~default:0 x 0 in
            MetricsSource_15417368036688066650.decode discr
          let source_set x e =
            BA_.set_uint16 ~default:0 x 0 (MetricsSource_15417368036688066650.encode_safe e)
          let source_set_unsafe x e =
            BA_.set_uint16 ~default:0 x 0 (MetricsSource_15417368036688066650.encode_unsafe e)
          let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:0
        end
        module Results = struct
          type struct_t = [`Metrics_c9a605598c875b61]
          type t = struct_t builder_t
          let has_content_type x =
            BA_.has_field x 0
          let content_type_get x =
            BA_.get_text ~default:"" x 0
          let content_type_set x v =
            BA_.set_text x 0 v
          let has_data x =
            BA_.has_field x 1
          let data_get x =
            BA_.get_text ~default:"" x 1
          let data_set x v =
            BA_.set_text x 1 v
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:2 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:2 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:2
        end
      end
      module SelfUpdate = struct
        module Params = struct
          type struct_t = [`SelfUpdate_9e269f0be2d31020]
          type t = struct_t builder_t
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:0
        end
        module Results = struct
          type struct_t = [`SelfUpdate_a8cb29d2302f0044]
          type t = struct_t builder_t
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:0
        end
      end
      module AdditionalMetric = struct
        module Params = struct
          type struct_t = [`AdditionalMetric_c8efdd741d957fd2]
          type t = struct_t builder_t
          let has_source x =
            BA_.has_field x 0
          let source_get x =
            BA_.get_text ~default:"" x 0
          let source_set x v =
            BA_.set_text x 0 v
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
        module Results = struct
          type struct_t = [`AdditionalMetric_a1250062bda8585a]
          type t = struct_t builder_t
          let has_metric x =
            BA_.has_field x 0
          let metric_get x =
            BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
          let metric_set_reader x v =
            BA_.set_struct ~data_words:1 ~pointer_words:1 x 0 v
          let metric_set_builder x v =
            BA_.set_struct ~data_words:1 ~pointer_words:1 x 0 (Some v)
          let metric_init x =
            BA_.init_struct ~data_words:1 ~pointer_words:1 x 0
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
      end
    end
    module Registration = struct
      type t = [`Registration_e098a12b91e35740]
      module Register = struct
        module Params = struct
          type struct_t = [`Register_cd3903a89e7d4463]
          type t = struct_t builder_t
          let has_name x =
            BA_.has_field x 0
          let name_get x =
            BA_.get_text ~default:"" x 0
          let name_set x v =
            BA_.set_text x 0 v
          let worker_get x =
            BA_.get_interface x 1
          let worker_get_pipelined x = 
            MessageWrapper.Untyped.capability_field x 1
          let worker_set x v =
            BA_.set_interface x 1 v
          let capacity_get x =
            BA_.get_int32 ~default:(0l) x 0
          let capacity_get_int_exn x =
            Capnp.Runtime.Util.int_of_int32_exn (capacity_get x)
          let capacity_set x v =
            BA_.set_int32 ~default:(0l) x 0 v
          let capacity_set_int_exn x v = capacity_set x (Capnp.Runtime.Util.int32_of_int_exn v)
          let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:2 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:2 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:2
        end
        module Results = struct
          type struct_t = [`Register_ed39dc99da78b704]
          type t = struct_t builder_t
          let queue_get x =
            BA_.get_interface x 0
          let queue_get_pipelined x = 
            MessageWrapper.Untyped.capability_field x 0
          let queue_set x v =
            BA_.set_interface x 0 v
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
      end
    end
    module Ticket = struct
      type t = [`Ticket_9497c03b7ca5b27b]
      module Job = struct
        module Params = struct
          type struct_t = [`Job_fe1358984e6402b9]
          type t = struct_t builder_t
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:0
        end
        module Results = struct
          type struct_t = [`Job_aec939d3264640ca]
          type t = struct_t builder_t
          let job_get x =
            BA_.get_interface x 0
          let job_get_pipelined x = 
            MessageWrapper.Untyped.capability_field x 0
          let job_set x v =
            BA_.set_interface x 0 v
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
      end
      module Cancel = struct
        module Params = struct
          type struct_t = [`Cancel_cb46490a81e5e4f2]
          type t = struct_t builder_t
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:0
        end
        module Results = struct
          type struct_t = [`Cancel_82c6338a750c4a8b]
          type t = struct_t builder_t
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:0
        end
      end
    end
    module Submission = struct
      type t = [`Submission_f4e8a768b32a7c42]
      module Submit = struct
        module Params = struct
          type struct_t = [`Submit_97c091c41928fcd7]
          type t = struct_t builder_t
          let has_pool x =
            BA_.has_field x 0
          let pool_get x =
            BA_.get_text ~default:"" x 0
          let pool_set x v =
            BA_.set_text x 0 v
          let has_descr x =
            BA_.has_field x 1
          let descr_get x =
            BA_.get_struct ~data_words:1 ~pointer_words:5 x 1
          let descr_set_reader x v =
            BA_.set_struct ~data_words:1 ~pointer_words:5 x 1 v
          let descr_set_builder x v =
            BA_.set_struct ~data_words:1 ~pointer_words:5 x 1 (Some v)
          let descr_init x =
            BA_.init_struct ~data_words:1 ~pointer_words:5 x 1
          let urgent_get x =
            BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
          let urgent_set x v =
            BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0 v
          let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:2 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:2 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:2
        end
        module Results = struct
          type struct_t = [`Submit_d77a09dd23a9c879]
          type t = struct_t builder_t
          let ticket_get x =
            BA_.get_interface x 0
          let ticket_get_pipelined x = 
            MessageWrapper.Untyped.capability_field x 0
          let ticket_set x v =
            BA_.set_interface x 0 v
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
      end
    end
    module WorkerInfo = struct
      type struct_t = [`WorkerInfo_89d251d58984b1eb]
      type t = struct_t builder_t
      let has_name x =
        BA_.has_field x 0
      let name_get x =
        BA_.get_text ~default:"" x 0
      let name_set x v =
        BA_.set_text x 0 v
      let active_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
      let active_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0 v
      let connected_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:1
      let connected_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:1 v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module Progress = struct
      type t = [`Progress_d7529e2702cd958a]
      module Report = struct
        module Params = struct
          type struct_t = [`Report_9b1977ad3bcf968f]
          type t = struct_t builder_t
          let has_status x =
            BA_.has_field x 0
          let status_get x =
            BA_.get_text ~default:"" x 0
          let status_set x v =
            BA_.set_text x 0 v
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
        module Results = struct
          type struct_t = [`Report_c679d2ad6191ba6b]
          type t = struct_t builder_t
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:0
        end
      end
    end
    module PoolAdmin = struct
      type t = [`PoolAdmin_d906b771b4082451]
      module Show = struct
        module Params = struct
          type struct_t = [`Show_8986d669d533884f]
          type t = struct_t builder_t
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:0
        end
        module Results = struct
          type struct_t = [`Show_d6c449f873bf3b5d]
          type t = struct_t builder_t
          let has_state x =
            BA_.has_field x 0
          let state_get x =
            BA_.get_text ~default:"" x 0
          let state_set x v =
            BA_.set_text x 0 v
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
      end
      module Workers = struct
        module Params = struct
          type struct_t = [`Workers_90d9932353f84420]
          type t = struct_t builder_t
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:0
        end
        module Results = struct
          type struct_t = [`Workers_b8456ef113abb4da]
          type t = struct_t builder_t
          let has_workers x =
            BA_.has_field x 0
          let workers_get x = 
            BA_.get_struct_list ~data_words:1 ~pointer_words:1 x 0
          let workers_get_list x =
            Capnp.Array.to_list (workers_get x)
          let workers_get_array x =
            Capnp.Array.to_array (workers_get x)
          let workers_set x v =
            BA_.set_struct_list ~data_words:1 ~pointer_words:1 x 0 v
          let workers_init x n =
            BA_.init_struct_list ~data_words:1 ~pointer_words:1 x 0 n
          let workers_set_list x v =
            let builder = workers_init x (List.length v) in
            let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
            builder
          let workers_set_array x v =
            let builder = workers_init x (Array.length v) in
            let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
            builder
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
      end
      module SetActive = struct
        module Params = struct
          type struct_t = [`SetActive_c0d1f54a45d947da]
          type t = struct_t builder_t
          let has_worker x =
            BA_.has_field x 0
          let worker_get x =
            BA_.get_text ~default:"" x 0
          let worker_set x v =
            BA_.set_text x 0 v
          let active_get x =
            BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
          let active_set x v =
            BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0 v
          let auto_create_get x =
            BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:1
          let auto_create_set x v =
            BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:1 v
          let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
        end
        module Results = struct
          type struct_t = [`SetActive_effa2cb4bfc19817]
          type t = struct_t builder_t
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:0
        end
      end
      module Worker = struct
        module Params = struct
          type struct_t = [`Worker_a4ed3d1a44716756]
          type t = struct_t builder_t
          let has_worker x =
            BA_.has_field x 0
          let worker_get x =
            BA_.get_text ~default:"" x 0
          let worker_set x v =
            BA_.set_text x 0 v
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
        module Results = struct
          type struct_t = [`Worker_910014b068840d2d]
          type t = struct_t builder_t
          let worker_get x =
            BA_.get_interface x 0
          let worker_get_pipelined x = 
            MessageWrapper.Untyped.capability_field x 0
          let worker_set x v =
            BA_.set_interface x 0 v
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
      end
      module Update = struct
        module Params = struct
          type struct_t = [`Update_babe541222805aba]
          type t = struct_t builder_t
          let has_worker x =
            BA_.has_field x 0
          let worker_get x =
            BA_.get_text ~default:"" x 0
          let worker_set x v =
            BA_.set_text x 0 v
          let progress_get x =
            BA_.get_interface x 1
          let progress_get_pipelined x = 
            MessageWrapper.Untyped.capability_field x 1
          let progress_set x v =
            BA_.set_interface x 1 v
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:2 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:2 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:2
        end
        module Results = struct
          type struct_t = [`Update_b4b276698d845933]
          type t = struct_t builder_t
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:0
        end
      end
      module SetRate = struct
        module Params = struct
          type struct_t = [`SetRate_a51fc59ba9d4a74c]
          type t = struct_t builder_t
          let has_id x =
            BA_.has_field x 0
          let id_get x =
            BA_.get_text ~default:"" x 0
          let id_set x v =
            BA_.set_text x 0 v
          let rate_get x =
            BA_.get_float64 ~default_bits:(0L) x 0
          let rate_set x v =
            BA_.set_float64 ~default_bits:(0L) x 0 v
          let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
        end
        module Results = struct
          type struct_t = [`SetRate_a4191ce8f8a947b4]
          type t = struct_t builder_t
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:0
        end
      end
      module Forget = struct
        module Params = struct
          type struct_t = [`Forget_c8d77e1465500b9f]
          type t = struct_t builder_t
          let has_worker x =
            BA_.has_field x 0
          let worker_get x =
            BA_.get_text ~default:"" x 0
          let worker_set x v =
            BA_.set_text x 0 v
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
        module Results = struct
          type struct_t = [`Forget_b706558828ef0e92]
          type t = struct_t builder_t
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:0
        end
      end
      module Drain = struct
        module Params = struct
          type struct_t = [`Drain_eda98c6d16599d71]
          type t = struct_t builder_t
          let has_worker x =
            BA_.has_field x 0
          let worker_get x =
            BA_.get_text ~default:"" x 0
          let worker_set x v =
            BA_.set_text x 0 v
          let progress_get x =
            BA_.get_interface x 1
          let progress_get_pipelined x = 
            MessageWrapper.Untyped.capability_field x 1
          let progress_set x v =
            BA_.set_interface x 1 v
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:2 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:2 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:2
        end
        module Results = struct
          type struct_t = [`Drain_f66636042afca0d6]
          type t = struct_t builder_t
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:0
        end
      end
    end
    module Admin = struct
      type t = [`Admin_dd51f35540dac6ad]
      module Pools = struct
        module Params = struct
          type struct_t = [`Pools_f2c2424ab87727b4]
          type t = struct_t builder_t
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:0
        end
        module Results = struct
          type struct_t = [`Pools_d0a9d0d20dd4cc17]
          type t = struct_t builder_t
          let has_names x =
            BA_.has_field x 0
          let names_get x =
            BA_.get_text_list x 0
          let names_get_list x =
            Capnp.Array.to_list (names_get x)
          let names_get_array x =
            Capnp.Array.to_array (names_get x)
          let names_set x v =
            BA_.set_text_list x 0 v
          let names_init x n =
            BA_.init_text_list x 0 n
          let names_set_list x v =
            let builder = names_init x (List.length v) in
            let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
            builder
          let names_set_array x v =
            let builder = names_init x (Array.length v) in
            let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
            builder
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
      end
      module Pool = struct
        module Params = struct
          type struct_t = [`Pool_9d6a16f5eccc6c15]
          type t = struct_t builder_t
          let has_name x =
            BA_.has_field x 0
          let name_get x =
            BA_.get_text ~default:"" x 0
          let name_set x v =
            BA_.set_text x 0 v
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
        module Results = struct
          type struct_t = [`Pool_99524b1b0c00d533]
          type t = struct_t builder_t
          let pool_get x =
            BA_.get_interface x 0
          let pool_get_pipelined x = 
            MessageWrapper.Untyped.capability_field x 0
          let pool_set x v =
            BA_.set_interface x 0 v
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
      end
      module AddClient = struct
        module Params = struct
          type struct_t = [`AddClient_b939aee2e3e952e3]
          type t = struct_t builder_t
          let has_id x =
            BA_.has_field x 0
          let id_get x =
            BA_.get_text ~default:"" x 0
          let id_set x v =
            BA_.set_text x 0 v
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
        module Results = struct
          type struct_t = [`AddClient_9f0245ffd4d76d34]
          type t = struct_t builder_t
          let cap_get x =
            BA_.get_interface x 0
          let cap_get_pipelined x = 
            MessageWrapper.Untyped.capability_field x 0
          let cap_set x v =
            BA_.set_interface x 0 v
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
      end
      module RemoveClient = struct
        module Params = struct
          type struct_t = [`RemoveClient_a05fc26f1e08b666]
          type t = struct_t builder_t
          let has_id x =
            BA_.has_field x 0
          let id_get x =
            BA_.get_text ~default:"" x 0
          let id_set x v =
            BA_.set_text x 0 v
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
        module Results = struct
          type struct_t = [`RemoveClient_dd6b877ef2fcbb6f]
          type t = struct_t builder_t
          let cap_get x =
            BA_.get_interface x 0
          let cap_get_pipelined x = 
            MessageWrapper.Untyped.capability_field x 0
          let cap_set x v =
            BA_.set_interface x 0 v
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
      end
      module ListClients = struct
        module Params = struct
          type struct_t = [`ListClients_a672ef7cb50805ba]
          type t = struct_t builder_t
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:0
        end
        module Results = struct
          type struct_t = [`ListClients_8d50536b48de2056]
          type t = struct_t builder_t
          let has_clients x =
            BA_.has_field x 0
          let clients_get x =
            BA_.get_text_list x 0
          let clients_get_list x =
            Capnp.Array.to_list (clients_get x)
          let clients_get_array x =
            Capnp.Array.to_array (clients_get x)
          let clients_set x v =
            BA_.set_text_list x 0 v
          let clients_init x n =
            BA_.init_text_list x 0 n
          let clients_set_list x v =
            let builder = clients_init x (List.length v) in
            let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
            builder
          let clients_set_array x v =
            let builder = clients_init x (Array.length v) in
            let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
            builder
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
      end
    end
  end

  module Client = struct
    module Job = struct
      type t = [`Job_ca66fd1f972efccc]
      let interface_id = Stdint.Uint64.of_string "0xca66fd1f972efccc"
      module Log = struct
        module Params = Builder.Job.Log.Params
        module Results = Reader.Job.Log.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:0
      end
      module Result = struct
        module Params = Builder.Job.Result.Params
        module Results = Reader.Job.Result.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:1
      end
      module Cancel = struct
        module Params = Builder.Job.Cancel.Params
        module Results = Reader.Job.Cancel.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:2
      end
      let method_name = function
        | 0 -> Some "log"
        | 1 -> Some "result"
        | 2 -> Some "cancel"
        | _ -> None
      let () = Capnp.RPC.Registry.register ~interface_id ~name:"Job" method_name
    end
    module Queue = struct
      type t = [`Queue_817f986f80263aaf]
      let interface_id = Stdint.Uint64.of_string "0x817f986f80263aaf"
      module Pop = struct
        module Params = Builder.Queue.Pop.Params
        module Results = Reader.Queue.Pop.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:0
      end
      module SetActive = struct
        module Params = Builder.Queue.SetActive.Params
        module Results = Reader.Queue.SetActive.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:1
      end
      let method_name = function
        | 0 -> Some "pop"
        | 1 -> Some "setActive"
        | _ -> None
      let () = Capnp.RPC.Registry.register ~interface_id ~name:"Queue" method_name
    end
    module Worker = struct
      type t = [`Worker_e06eedcf95bf7905]
      let interface_id = Stdint.Uint64.of_string "0xe06eedcf95bf7905"
      module Metrics = struct
        module Params = Builder.Worker.Metrics.Params
        module Results = Reader.Worker.Metrics.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:0
      end
      module SelfUpdate = struct
        module Params = Builder.Worker.SelfUpdate.Params
        module Results = Reader.Worker.SelfUpdate.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:1
      end
      module AdditionalMetric = struct
        module Params = Builder.Worker.AdditionalMetric.Params
        module Results = Reader.Worker.AdditionalMetric.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:2
      end
      let method_name = function
        | 0 -> Some "metrics"
        | 1 -> Some "selfUpdate"
        | 2 -> Some "additionalMetric"
        | _ -> None
      let () = Capnp.RPC.Registry.register ~interface_id ~name:"Worker" method_name
    end
    module Registration = struct
      type t = [`Registration_e098a12b91e35740]
      let interface_id = Stdint.Uint64.of_string "0xe098a12b91e35740"
      module Register = struct
        module Params = Builder.Registration.Register.Params
        module Results = Reader.Registration.Register.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:0
      end
      let method_name = function
        | 0 -> Some "register"
        | _ -> None
      let () = Capnp.RPC.Registry.register ~interface_id ~name:"Registration" method_name
    end
    module Ticket = struct
      type t = [`Ticket_9497c03b7ca5b27b]
      let interface_id = Stdint.Uint64.of_string "0x9497c03b7ca5b27b"
      module Job = struct
        module Params = Builder.Ticket.Job.Params
        module Results = Reader.Ticket.Job.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:0
      end
      module Cancel = struct
        module Params = Builder.Ticket.Cancel.Params
        module Results = Reader.Ticket.Cancel.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:1
      end
      let method_name = function
        | 0 -> Some "job"
        | 1 -> Some "cancel"
        | _ -> None
      let () = Capnp.RPC.Registry.register ~interface_id ~name:"Ticket" method_name
    end
    module Submission = struct
      type t = [`Submission_f4e8a768b32a7c42]
      let interface_id = Stdint.Uint64.of_string "0xf4e8a768b32a7c42"
      module Submit = struct
        module Params = Builder.Submission.Submit.Params
        module Results = Reader.Submission.Submit.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:0
      end
      let method_name = function
        | 0 -> Some "submit"
        | _ -> None
      let () = Capnp.RPC.Registry.register ~interface_id ~name:"Submission" method_name
    end
    module Progress = struct
      type t = [`Progress_d7529e2702cd958a]
      let interface_id = Stdint.Uint64.of_string "0xd7529e2702cd958a"
      module Report = struct
        module Params = Builder.Progress.Report.Params
        module Results = Reader.Progress.Report.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:0
      end
      let method_name = function
        | 0 -> Some "report"
        | _ -> None
      let () = Capnp.RPC.Registry.register ~interface_id ~name:"Progress" method_name
    end
    module PoolAdmin = struct
      type t = [`PoolAdmin_d906b771b4082451]
      let interface_id = Stdint.Uint64.of_string "0xd906b771b4082451"
      module Show = struct
        module Params = Builder.PoolAdmin.Show.Params
        module Results = Reader.PoolAdmin.Show.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:0
      end
      module Workers = struct
        module Params = Builder.PoolAdmin.Workers.Params
        module Results = Reader.PoolAdmin.Workers.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:1
      end
      module SetActive = struct
        module Params = Builder.PoolAdmin.SetActive.Params
        module Results = Reader.PoolAdmin.SetActive.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:2
      end
      module Worker = struct
        module Params = Builder.PoolAdmin.Worker.Params
        module Results = Reader.PoolAdmin.Worker.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:3
      end
      module Update = struct
        module Params = Builder.PoolAdmin.Update.Params
        module Results = Reader.PoolAdmin.Update.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:4
      end
      module SetRate = struct
        module Params = Builder.PoolAdmin.SetRate.Params
        module Results = Reader.PoolAdmin.SetRate.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:5
      end
      module Forget = struct
        module Params = Builder.PoolAdmin.Forget.Params
        module Results = Reader.PoolAdmin.Forget.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:6
      end
      module Drain = struct
        module Params = Builder.PoolAdmin.Drain.Params
        module Results = Reader.PoolAdmin.Drain.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:7
      end
      let method_name = function
        | 0 -> Some "show"
        | 1 -> Some "workers"
        | 2 -> Some "setActive"
        | 3 -> Some "worker"
        | 4 -> Some "update"
        | 5 -> Some "setRate"
        | 6 -> Some "forget"
        | 7 -> Some "drain"
        | _ -> None
      let () = Capnp.RPC.Registry.register ~interface_id ~name:"PoolAdmin" method_name
    end
    module Admin = struct
      type t = [`Admin_dd51f35540dac6ad]
      let interface_id = Stdint.Uint64.of_string "0xdd51f35540dac6ad"
      module Pools = struct
        module Params = Builder.Admin.Pools.Params
        module Results = Reader.Admin.Pools.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:0
      end
      module Pool = struct
        module Params = Builder.Admin.Pool.Params
        module Results = Reader.Admin.Pool.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:1
      end
      module AddClient = struct
        module Params = Builder.Admin.AddClient.Params
        module Results = Reader.Admin.AddClient.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:2
      end
      module RemoveClient = struct
        module Params = Builder.Admin.RemoveClient.Params
        module Results = Reader.Admin.RemoveClient.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:3
      end
      module ListClients = struct
        module Params = Builder.Admin.ListClients.Params
        module Results = Reader.Admin.ListClients.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:4
      end
      let method_name = function
        | 0 -> Some "pools"
        | 1 -> Some "pool"
        | 2 -> Some "addClient"
        | 3 -> Some "removeClient"
        | 4 -> Some "listClients"
        | _ -> None
      let () = Capnp.RPC.Registry.register ~interface_id ~name:"Admin" method_name
    end
  end

  module Service = struct
    module Job = struct
      type t = [`Job_ca66fd1f972efccc]
      let interface_id = Stdint.Uint64.of_string "0xca66fd1f972efccc"
      module Log = struct
        module Params = Reader.Job.Log.Params
        module Results = Builder.Job.Log.Results
      end
      module Result = struct
        module Params = Reader.Job.Result.Params
        module Results = Builder.Job.Result.Results
      end
      module Cancel = struct
        module Params = Reader.Job.Cancel.Params
        module Results = Builder.Job.Cancel.Results
      end
      class virtual service = object (self)
        method release = ()
        method dispatch ~interface_id:i ~method_id =
          if i <> interface_id then MessageWrapper.Untyped.unknown_interface ~interface_id:i
          else match method_id with
          | 0 -> MessageWrapper.Untyped.abstract_method self#log_impl
          | 1 -> MessageWrapper.Untyped.abstract_method self#result_impl
          | 2 -> MessageWrapper.Untyped.abstract_method self#cancel_impl
          | x -> MessageWrapper.Untyped.unknown_method ~interface_id ~method_id
        method pp f = Format.pp_print_string f "Job"
        method virtual log_impl : (Log.Params.t, Log.Results.t) MessageWrapper.Service.method_t
        method virtual result_impl : (Result.Params.t, Result.Results.t) MessageWrapper.Service.method_t
        method virtual cancel_impl : (Cancel.Params.t, Cancel.Results.t) MessageWrapper.Service.method_t
      end
      let local (service:#service) =
        MessageWrapper.Untyped.local service
    end
    module Queue = struct
      type t = [`Queue_817f986f80263aaf]
      let interface_id = Stdint.Uint64.of_string "0x817f986f80263aaf"
      module Pop = struct
        module Params = Reader.Queue.Pop.Params
        module Results = Builder.Queue.Pop.Results
      end
      module SetActive = struct
        module Params = Reader.Queue.SetActive.Params
        module Results = Builder.Queue.SetActive.Results
      end
      class virtual service = object (self)
        method release = ()
        method dispatch ~interface_id:i ~method_id =
          if i <> interface_id then MessageWrapper.Untyped.unknown_interface ~interface_id:i
          else match method_id with
          | 0 -> MessageWrapper.Untyped.abstract_method self#pop_impl
          | 1 -> MessageWrapper.Untyped.abstract_method self#set_active_impl
          | x -> MessageWrapper.Untyped.unknown_method ~interface_id ~method_id
        method pp f = Format.pp_print_string f "Queue"
        method virtual pop_impl : (Pop.Params.t, Pop.Results.t) MessageWrapper.Service.method_t
        method virtual set_active_impl : (SetActive.Params.t, SetActive.Results.t) MessageWrapper.Service.method_t
      end
      let local (service:#service) =
        MessageWrapper.Untyped.local service
    end
    module Worker = struct
      type t = [`Worker_e06eedcf95bf7905]
      let interface_id = Stdint.Uint64.of_string "0xe06eedcf95bf7905"
      module Metrics = struct
        module Params = Reader.Worker.Metrics.Params
        module Results = Builder.Worker.Metrics.Results
      end
      module SelfUpdate = struct
        module Params = Reader.Worker.SelfUpdate.Params
        module Results = Builder.Worker.SelfUpdate.Results
      end
      module AdditionalMetric = struct
        module Params = Reader.Worker.AdditionalMetric.Params
        module Results = Builder.Worker.AdditionalMetric.Results
      end
      class virtual service = object (self)
        method release = ()
        method dispatch ~interface_id:i ~method_id =
          if i <> interface_id then MessageWrapper.Untyped.unknown_interface ~interface_id:i
          else match method_id with
          | 0 -> MessageWrapper.Untyped.abstract_method self#metrics_impl
          | 1 -> MessageWrapper.Untyped.abstract_method self#self_update_impl
          | 2 -> MessageWrapper.Untyped.abstract_method self#additional_metric_impl
          | x -> MessageWrapper.Untyped.unknown_method ~interface_id ~method_id
        method pp f = Format.pp_print_string f "Worker"
        method virtual metrics_impl : (Metrics.Params.t, Metrics.Results.t) MessageWrapper.Service.method_t
        method virtual self_update_impl : (SelfUpdate.Params.t, SelfUpdate.Results.t) MessageWrapper.Service.method_t
        method virtual additional_metric_impl : (AdditionalMetric.Params.t, AdditionalMetric.Results.t) MessageWrapper.Service.method_t
      end
      let local (service:#service) =
        MessageWrapper.Untyped.local service
    end
    module Registration = struct
      type t = [`Registration_e098a12b91e35740]
      let interface_id = Stdint.Uint64.of_string "0xe098a12b91e35740"
      module Register = struct
        module Params = Reader.Registration.Register.Params
        module Results = Builder.Registration.Register.Results
      end
      class virtual service = object (self)
        method release = ()
        method dispatch ~interface_id:i ~method_id =
          if i <> interface_id then MessageWrapper.Untyped.unknown_interface ~interface_id:i
          else match method_id with
          | 0 -> MessageWrapper.Untyped.abstract_method self#register_impl
          | x -> MessageWrapper.Untyped.unknown_method ~interface_id ~method_id
        method pp f = Format.pp_print_string f "Registration"
        method virtual register_impl : (Register.Params.t, Register.Results.t) MessageWrapper.Service.method_t
      end
      let local (service:#service) =
        MessageWrapper.Untyped.local service
    end
    module Ticket = struct
      type t = [`Ticket_9497c03b7ca5b27b]
      let interface_id = Stdint.Uint64.of_string "0x9497c03b7ca5b27b"
      module Job = struct
        module Params = Reader.Ticket.Job.Params
        module Results = Builder.Ticket.Job.Results
      end
      module Cancel = struct
        module Params = Reader.Ticket.Cancel.Params
        module Results = Builder.Ticket.Cancel.Results
      end
      class virtual service = object (self)
        method release = ()
        method dispatch ~interface_id:i ~method_id =
          if i <> interface_id then MessageWrapper.Untyped.unknown_interface ~interface_id:i
          else match method_id with
          | 0 -> MessageWrapper.Untyped.abstract_method self#job_impl
          | 1 -> MessageWrapper.Untyped.abstract_method self#cancel_impl
          | x -> MessageWrapper.Untyped.unknown_method ~interface_id ~method_id
        method pp f = Format.pp_print_string f "Ticket"
        method virtual job_impl : (Job.Params.t, Job.Results.t) MessageWrapper.Service.method_t
        method virtual cancel_impl : (Cancel.Params.t, Cancel.Results.t) MessageWrapper.Service.method_t
      end
      let local (service:#service) =
        MessageWrapper.Untyped.local service
    end
    module Submission = struct
      type t = [`Submission_f4e8a768b32a7c42]
      let interface_id = Stdint.Uint64.of_string "0xf4e8a768b32a7c42"
      module Submit = struct
        module Params = Reader.Submission.Submit.Params
        module Results = Builder.Submission.Submit.Results
      end
      class virtual service = object (self)
        method release = ()
        method dispatch ~interface_id:i ~method_id =
          if i <> interface_id then MessageWrapper.Untyped.unknown_interface ~interface_id:i
          else match method_id with
          | 0 -> MessageWrapper.Untyped.abstract_method self#submit_impl
          | x -> MessageWrapper.Untyped.unknown_method ~interface_id ~method_id
        method pp f = Format.pp_print_string f "Submission"
        method virtual submit_impl : (Submit.Params.t, Submit.Results.t) MessageWrapper.Service.method_t
      end
      let local (service:#service) =
        MessageWrapper.Untyped.local service
    end
    module Progress = struct
      type t = [`Progress_d7529e2702cd958a]
      let interface_id = Stdint.Uint64.of_string "0xd7529e2702cd958a"
      module Report = struct
        module Params = Reader.Progress.Report.Params
        module Results = Builder.Progress.Report.Results
      end
      class virtual service = object (self)
        method release = ()
        method dispatch ~interface_id:i ~method_id =
          if i <> interface_id then MessageWrapper.Untyped.unknown_interface ~interface_id:i
          else match method_id with
          | 0 -> MessageWrapper.Untyped.abstract_method self#report_impl
          | x -> MessageWrapper.Untyped.unknown_method ~interface_id ~method_id
        method pp f = Format.pp_print_string f "Progress"
        method virtual report_impl : (Report.Params.t, Report.Results.t) MessageWrapper.Service.method_t
      end
      let local (service:#service) =
        MessageWrapper.Untyped.local service
    end
    module PoolAdmin = struct
      type t = [`PoolAdmin_d906b771b4082451]
      let interface_id = Stdint.Uint64.of_string "0xd906b771b4082451"
      module Show = struct
        module Params = Reader.PoolAdmin.Show.Params
        module Results = Builder.PoolAdmin.Show.Results
      end
      module Workers = struct
        module Params = Reader.PoolAdmin.Workers.Params
        module Results = Builder.PoolAdmin.Workers.Results
      end
      module SetActive = struct
        module Params = Reader.PoolAdmin.SetActive.Params
        module Results = Builder.PoolAdmin.SetActive.Results
      end
      module Worker = struct
        module Params = Reader.PoolAdmin.Worker.Params
        module Results = Builder.PoolAdmin.Worker.Results
      end
      module Update = struct
        module Params = Reader.PoolAdmin.Update.Params
        module Results = Builder.PoolAdmin.Update.Results
      end
      module SetRate = struct
        module Params = Reader.PoolAdmin.SetRate.Params
        module Results = Builder.PoolAdmin.SetRate.Results
      end
      module Forget = struct
        module Params = Reader.PoolAdmin.Forget.Params
        module Results = Builder.PoolAdmin.Forget.Results
      end
      module Drain = struct
        module Params = Reader.PoolAdmin.Drain.Params
        module Results = Builder.PoolAdmin.Drain.Results
      end
      class virtual service = object (self)
        method release = ()
        method dispatch ~interface_id:i ~method_id =
          if i <> interface_id then MessageWrapper.Untyped.unknown_interface ~interface_id:i
          else match method_id with
          | 0 -> MessageWrapper.Untyped.abstract_method self#show_impl
          | 1 -> MessageWrapper.Untyped.abstract_method self#workers_impl
          | 2 -> MessageWrapper.Untyped.abstract_method self#set_active_impl
          | 3 -> MessageWrapper.Untyped.abstract_method self#worker_impl
          | 4 -> MessageWrapper.Untyped.abstract_method self#update_impl
          | 5 -> MessageWrapper.Untyped.abstract_method self#set_rate_impl
          | 6 -> MessageWrapper.Untyped.abstract_method self#forget_impl
          | 7 -> MessageWrapper.Untyped.abstract_method self#drain_impl
          | x -> MessageWrapper.Untyped.unknown_method ~interface_id ~method_id
        method pp f = Format.pp_print_string f "PoolAdmin"
        method virtual show_impl : (Show.Params.t, Show.Results.t) MessageWrapper.Service.method_t
        method virtual workers_impl : (Workers.Params.t, Workers.Results.t) MessageWrapper.Service.method_t
        method virtual set_active_impl : (SetActive.Params.t, SetActive.Results.t) MessageWrapper.Service.method_t
        method virtual worker_impl : (Worker.Params.t, Worker.Results.t) MessageWrapper.Service.method_t
        method virtual update_impl : (Update.Params.t, Update.Results.t) MessageWrapper.Service.method_t
        method virtual set_rate_impl : (SetRate.Params.t, SetRate.Results.t) MessageWrapper.Service.method_t
        method virtual forget_impl : (Forget.Params.t, Forget.Results.t) MessageWrapper.Service.method_t
        method virtual drain_impl : (Drain.Params.t, Drain.Results.t) MessageWrapper.Service.method_t
      end
      let local (service:#service) =
        MessageWrapper.Untyped.local service
    end
    module Admin = struct
      type t = [`Admin_dd51f35540dac6ad]
      let interface_id = Stdint.Uint64.of_string "0xdd51f35540dac6ad"
      module Pools = struct
        module Params = Reader.Admin.Pools.Params
        module Results = Builder.Admin.Pools.Results
      end
      module Pool = struct
        module Params = Reader.Admin.Pool.Params
        module Results = Builder.Admin.Pool.Results
      end
      module AddClient = struct
        module Params = Reader.Admin.AddClient.Params
        module Results = Builder.Admin.AddClient.Results
      end
      module RemoveClient = struct
        module Params = Reader.Admin.RemoveClient.Params
        module Results = Builder.Admin.RemoveClient.Results
      end
      module ListClients = struct
        module Params = Reader.Admin.ListClients.Params
        module Results = Builder.Admin.ListClients.Results
      end
      class virtual service = object (self)
        method release = ()
        method dispatch ~interface_id:i ~method_id =
          if i <> interface_id then MessageWrapper.Untyped.unknown_interface ~interface_id:i
          else match method_id with
          | 0 -> MessageWrapper.Untyped.abstract_method self#pools_impl
          | 1 -> MessageWrapper.Untyped.abstract_method self#pool_impl
          | 2 -> MessageWrapper.Untyped.abstract_method self#add_client_impl
          | 3 -> MessageWrapper.Untyped.abstract_method self#remove_client_impl
          | 4 -> MessageWrapper.Untyped.abstract_method self#list_clients_impl
          | x -> MessageWrapper.Untyped.unknown_method ~interface_id ~method_id
        method pp f = Format.pp_print_string f "Admin"
        method virtual pools_impl : (Pools.Params.t, Pools.Results.t) MessageWrapper.Service.method_t
        method virtual pool_impl : (Pool.Params.t, Pool.Results.t) MessageWrapper.Service.method_t
        method virtual add_client_impl : (AddClient.Params.t, AddClient.Results.t) MessageWrapper.Service.method_t
        method virtual remove_client_impl : (RemoveClient.Params.t, RemoveClient.Results.t) MessageWrapper.Service.method_t
        method virtual list_clients_impl : (ListClients.Params.t, ListClients.Results.t) MessageWrapper.Service.method_t
      end
      let local (service:#service) =
        MessageWrapper.Untyped.local service
    end
  end
  module MessageWrapper = MessageWrapper
end

module Make(M:Capnp.MessageSig.S) = MakeRPC(Capnp.RPC.None(M))
