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

module MakeRPC(MessageWrapper : Capnp.RPC.S) : sig
  include S with module MessageWrapper = MessageWrapper

  module Client : sig
    module Job : sig
      type t = [`Job_ca66fd1f972efccc]
      val interface_id : Stdint.Uint64.t
      module Log : sig
        module Params = Builder.Job.Log.Params
        module Results = Reader.Job.Log.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
      module Result : sig
        module Params = Builder.Job.Result.Params
        module Results = Reader.Job.Result.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
      module Cancel : sig
        module Params = Builder.Job.Cancel.Params
        module Results = Reader.Job.Cancel.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
    end
    module Queue : sig
      type t = [`Queue_817f986f80263aaf]
      val interface_id : Stdint.Uint64.t
      module Pop : sig
        module Params = Builder.Queue.Pop.Params
        module Results = Reader.Queue.Pop.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
      module SetActive : sig
        module Params = Builder.Queue.SetActive.Params
        module Results = Reader.Queue.SetActive.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
    end
    module Worker : sig
      type t = [`Worker_e06eedcf95bf7905]
      val interface_id : Stdint.Uint64.t
      module Metrics : sig
        module Params = Builder.Worker.Metrics.Params
        module Results = Reader.Worker.Metrics.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
      module SelfUpdate : sig
        module Params = Builder.Worker.SelfUpdate.Params
        module Results = Reader.Worker.SelfUpdate.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
      module AdditionalMetric : sig
        module Params = Builder.Worker.AdditionalMetric.Params
        module Results = Reader.Worker.AdditionalMetric.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
    end
    module Registration : sig
      type t = [`Registration_e098a12b91e35740]
      val interface_id : Stdint.Uint64.t
      module Register : sig
        module Params = Builder.Registration.Register.Params
        module Results = Reader.Registration.Register.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
    end
    module Ticket : sig
      type t = [`Ticket_9497c03b7ca5b27b]
      val interface_id : Stdint.Uint64.t
      module Job : sig
        module Params = Builder.Ticket.Job.Params
        module Results = Reader.Ticket.Job.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
      module Cancel : sig
        module Params = Builder.Ticket.Cancel.Params
        module Results = Reader.Ticket.Cancel.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
    end
    module Submission : sig
      type t = [`Submission_f4e8a768b32a7c42]
      val interface_id : Stdint.Uint64.t
      module Submit : sig
        module Params = Builder.Submission.Submit.Params
        module Results = Reader.Submission.Submit.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
    end
    module Progress : sig
      type t = [`Progress_d7529e2702cd958a]
      val interface_id : Stdint.Uint64.t
      module Report : sig
        module Params = Builder.Progress.Report.Params
        module Results = Reader.Progress.Report.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
    end
    module PoolAdmin : sig
      type t = [`PoolAdmin_d906b771b4082451]
      val interface_id : Stdint.Uint64.t
      module Show : sig
        module Params = Builder.PoolAdmin.Show.Params
        module Results = Reader.PoolAdmin.Show.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
      module Workers : sig
        module Params = Builder.PoolAdmin.Workers.Params
        module Results = Reader.PoolAdmin.Workers.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
      module SetActive : sig
        module Params = Builder.PoolAdmin.SetActive.Params
        module Results = Reader.PoolAdmin.SetActive.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
      module Worker : sig
        module Params = Builder.PoolAdmin.Worker.Params
        module Results = Reader.PoolAdmin.Worker.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
      module Update : sig
        module Params = Builder.PoolAdmin.Update.Params
        module Results = Reader.PoolAdmin.Update.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
      module SetRate : sig
        module Params = Builder.PoolAdmin.SetRate.Params
        module Results = Reader.PoolAdmin.SetRate.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
      module Forget : sig
        module Params = Builder.PoolAdmin.Forget.Params
        module Results = Reader.PoolAdmin.Forget.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
      module Drain : sig
        module Params = Builder.PoolAdmin.Drain.Params
        module Results = Reader.PoolAdmin.Drain.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
    end
    module Admin : sig
      type t = [`Admin_dd51f35540dac6ad]
      val interface_id : Stdint.Uint64.t
      module Pools : sig
        module Params = Builder.Admin.Pools.Params
        module Results = Reader.Admin.Pools.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
      module Pool : sig
        module Params = Builder.Admin.Pool.Params
        module Results = Reader.Admin.Pool.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
      module AddClient : sig
        module Params = Builder.Admin.AddClient.Params
        module Results = Reader.Admin.AddClient.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
      module RemoveClient : sig
        module Params = Builder.Admin.RemoveClient.Params
        module Results = Reader.Admin.RemoveClient.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
      module ListClients : sig
        module Params = Builder.Admin.ListClients.Params
        module Results = Reader.Admin.ListClients.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
    end
  end

  module Service : sig
    module Job : sig
      type t = [`Job_ca66fd1f972efccc]
      val interface_id : Stdint.Uint64.t
      module Log : sig
        module Params = Reader.Job.Log.Params
        module Results = Builder.Job.Log.Results
      end
      module Result : sig
        module Params = Reader.Job.Result.Params
        module Results = Builder.Job.Result.Results
      end
      module Cancel : sig
        module Params = Reader.Job.Cancel.Params
        module Results = Builder.Job.Cancel.Results
      end
      class virtual service : object
        inherit MessageWrapper.Untyped.generic_service
        method virtual log_impl : (Log.Params.t, Log.Results.t) MessageWrapper.Service.method_t
        method virtual result_impl : (Result.Params.t, Result.Results.t) MessageWrapper.Service.method_t
        method virtual cancel_impl : (Cancel.Params.t, Cancel.Results.t) MessageWrapper.Service.method_t
      end
      val local : #service -> t MessageWrapper.Capability.t
    end
    module Queue : sig
      type t = [`Queue_817f986f80263aaf]
      val interface_id : Stdint.Uint64.t
      module Pop : sig
        module Params = Reader.Queue.Pop.Params
        module Results = Builder.Queue.Pop.Results
      end
      module SetActive : sig
        module Params = Reader.Queue.SetActive.Params
        module Results = Builder.Queue.SetActive.Results
      end
      class virtual service : object
        inherit MessageWrapper.Untyped.generic_service
        method virtual pop_impl : (Pop.Params.t, Pop.Results.t) MessageWrapper.Service.method_t
        method virtual set_active_impl : (SetActive.Params.t, SetActive.Results.t) MessageWrapper.Service.method_t
      end
      val local : #service -> t MessageWrapper.Capability.t
    end
    module Worker : sig
      type t = [`Worker_e06eedcf95bf7905]
      val interface_id : Stdint.Uint64.t
      module Metrics : sig
        module Params = Reader.Worker.Metrics.Params
        module Results = Builder.Worker.Metrics.Results
      end
      module SelfUpdate : sig
        module Params = Reader.Worker.SelfUpdate.Params
        module Results = Builder.Worker.SelfUpdate.Results
      end
      module AdditionalMetric : sig
        module Params = Reader.Worker.AdditionalMetric.Params
        module Results = Builder.Worker.AdditionalMetric.Results
      end
      class virtual service : object
        inherit MessageWrapper.Untyped.generic_service
        method virtual metrics_impl : (Metrics.Params.t, Metrics.Results.t) MessageWrapper.Service.method_t
        method virtual self_update_impl : (SelfUpdate.Params.t, SelfUpdate.Results.t) MessageWrapper.Service.method_t
        method virtual additional_metric_impl : (AdditionalMetric.Params.t, AdditionalMetric.Results.t) MessageWrapper.Service.method_t
      end
      val local : #service -> t MessageWrapper.Capability.t
    end
    module Registration : sig
      type t = [`Registration_e098a12b91e35740]
      val interface_id : Stdint.Uint64.t
      module Register : sig
        module Params = Reader.Registration.Register.Params
        module Results = Builder.Registration.Register.Results
      end
      class virtual service : object
        inherit MessageWrapper.Untyped.generic_service
        method virtual register_impl : (Register.Params.t, Register.Results.t) MessageWrapper.Service.method_t
      end
      val local : #service -> t MessageWrapper.Capability.t
    end
    module Ticket : sig
      type t = [`Ticket_9497c03b7ca5b27b]
      val interface_id : Stdint.Uint64.t
      module Job : sig
        module Params = Reader.Ticket.Job.Params
        module Results = Builder.Ticket.Job.Results
      end
      module Cancel : sig
        module Params = Reader.Ticket.Cancel.Params
        module Results = Builder.Ticket.Cancel.Results
      end
      class virtual service : object
        inherit MessageWrapper.Untyped.generic_service
        method virtual job_impl : (Job.Params.t, Job.Results.t) MessageWrapper.Service.method_t
        method virtual cancel_impl : (Cancel.Params.t, Cancel.Results.t) MessageWrapper.Service.method_t
      end
      val local : #service -> t MessageWrapper.Capability.t
    end
    module Submission : sig
      type t = [`Submission_f4e8a768b32a7c42]
      val interface_id : Stdint.Uint64.t
      module Submit : sig
        module Params = Reader.Submission.Submit.Params
        module Results = Builder.Submission.Submit.Results
      end
      class virtual service : object
        inherit MessageWrapper.Untyped.generic_service
        method virtual submit_impl : (Submit.Params.t, Submit.Results.t) MessageWrapper.Service.method_t
      end
      val local : #service -> t MessageWrapper.Capability.t
    end
    module Progress : sig
      type t = [`Progress_d7529e2702cd958a]
      val interface_id : Stdint.Uint64.t
      module Report : sig
        module Params = Reader.Progress.Report.Params
        module Results = Builder.Progress.Report.Results
      end
      class virtual service : object
        inherit MessageWrapper.Untyped.generic_service
        method virtual report_impl : (Report.Params.t, Report.Results.t) MessageWrapper.Service.method_t
      end
      val local : #service -> t MessageWrapper.Capability.t
    end
    module PoolAdmin : sig
      type t = [`PoolAdmin_d906b771b4082451]
      val interface_id : Stdint.Uint64.t
      module Show : sig
        module Params = Reader.PoolAdmin.Show.Params
        module Results = Builder.PoolAdmin.Show.Results
      end
      module Workers : sig
        module Params = Reader.PoolAdmin.Workers.Params
        module Results = Builder.PoolAdmin.Workers.Results
      end
      module SetActive : sig
        module Params = Reader.PoolAdmin.SetActive.Params
        module Results = Builder.PoolAdmin.SetActive.Results
      end
      module Worker : sig
        module Params = Reader.PoolAdmin.Worker.Params
        module Results = Builder.PoolAdmin.Worker.Results
      end
      module Update : sig
        module Params = Reader.PoolAdmin.Update.Params
        module Results = Builder.PoolAdmin.Update.Results
      end
      module SetRate : sig
        module Params = Reader.PoolAdmin.SetRate.Params
        module Results = Builder.PoolAdmin.SetRate.Results
      end
      module Forget : sig
        module Params = Reader.PoolAdmin.Forget.Params
        module Results = Builder.PoolAdmin.Forget.Results
      end
      module Drain : sig
        module Params = Reader.PoolAdmin.Drain.Params
        module Results = Builder.PoolAdmin.Drain.Results
      end
      class virtual service : object
        inherit MessageWrapper.Untyped.generic_service
        method virtual show_impl : (Show.Params.t, Show.Results.t) MessageWrapper.Service.method_t
        method virtual workers_impl : (Workers.Params.t, Workers.Results.t) MessageWrapper.Service.method_t
        method virtual set_active_impl : (SetActive.Params.t, SetActive.Results.t) MessageWrapper.Service.method_t
        method virtual worker_impl : (Worker.Params.t, Worker.Results.t) MessageWrapper.Service.method_t
        method virtual update_impl : (Update.Params.t, Update.Results.t) MessageWrapper.Service.method_t
        method virtual set_rate_impl : (SetRate.Params.t, SetRate.Results.t) MessageWrapper.Service.method_t
        method virtual forget_impl : (Forget.Params.t, Forget.Results.t) MessageWrapper.Service.method_t
        method virtual drain_impl : (Drain.Params.t, Drain.Results.t) MessageWrapper.Service.method_t
      end
      val local : #service -> t MessageWrapper.Capability.t
    end
    module Admin : sig
      type t = [`Admin_dd51f35540dac6ad]
      val interface_id : Stdint.Uint64.t
      module Pools : sig
        module Params = Reader.Admin.Pools.Params
        module Results = Builder.Admin.Pools.Results
      end
      module Pool : sig
        module Params = Reader.Admin.Pool.Params
        module Results = Builder.Admin.Pool.Results
      end
      module AddClient : sig
        module Params = Reader.Admin.AddClient.Params
        module Results = Builder.Admin.AddClient.Results
      end
      module RemoveClient : sig
        module Params = Reader.Admin.RemoveClient.Params
        module Results = Builder.Admin.RemoveClient.Results
      end
      module ListClients : sig
        module Params = Reader.Admin.ListClients.Params
        module Results = Builder.Admin.ListClients.Results
      end
      class virtual service : object
        inherit MessageWrapper.Untyped.generic_service
        method virtual pools_impl : (Pools.Params.t, Pools.Results.t) MessageWrapper.Service.method_t
        method virtual pool_impl : (Pool.Params.t, Pool.Results.t) MessageWrapper.Service.method_t
        method virtual add_client_impl : (AddClient.Params.t, AddClient.Results.t) MessageWrapper.Service.method_t
        method virtual remove_client_impl : (RemoveClient.Params.t, RemoveClient.Results.t) MessageWrapper.Service.method_t
        method virtual list_clients_impl : (ListClients.Params.t, ListClients.Results.t) MessageWrapper.Service.method_t
      end
      val local : #service -> t MessageWrapper.Capability.t
    end
  end
end

module Make(M : Capnp.MessageSig.S) : module type of MakeRPC(Capnp.RPC.None(M))
