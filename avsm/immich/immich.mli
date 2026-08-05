(** {1 Immich}

    Immich API

    @version 2.4.1 *)

type t

val create :
  ?session:Requests.t ->
  sw:Eio.Switch.t ->
  < net : _ Eio.Net.t ; fs : Eio.Fs.dir_ty Eio.Path.t ; clock : _ Eio.Time.clock ; .. > ->
  base_url:string ->
  t

val base_url : t -> string
val session : t -> Requests.t

module WorkflowFilterItem : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : plugin_filter_id:string -> ?filter_config:Jsont.json -> unit -> t
    
    val filter_config : t -> Jsont.json option
    
    val plugin_filter_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module WorkflowFilter : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : id:string -> order:float -> plugin_filter_id:string -> workflow_id:string -> ?filter_config:Jsont.json -> unit -> t
    
    val filter_config : t -> Jsont.json option
    
    val id : t -> string
    
    val order : t -> float
    
    val plugin_filter_id : t -> string
    
    val workflow_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module WorkflowActionItem : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : plugin_action_id:string -> ?action_config:Jsont.json -> unit -> t
    
    val action_config : t -> Jsont.json option
    
    val plugin_action_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module WorkflowAction : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : id:string -> order:float -> plugin_action_id:string -> workflow_id:string -> ?action_config:Jsont.json -> unit -> t
    
    val action_config : t -> Jsont.json option
    
    val id : t -> string
    
    val order : t -> float
    
    val plugin_action_id : t -> string
    
    val workflow_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module VideoContainer : sig
  module T : sig
    type t = [
      | `Mov
      | `Mp4
      | `Ogg
      | `Webm
    ]
    
    val jsont : t Jsont.t
  end
end

module VideoCodec : sig
  module T : sig
    type t = [
      | `H264
      | `Hevc
      | `Vp9
      | `Av1
    ]
    
    val jsont : t Jsont.t
  end
end

module VersionCheckState : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : ?checked_at:string -> ?release_version:string -> unit -> t
    
    val checked_at : t -> string option
    
    val release_version : t -> string option
    
    val jsont : t Jsont.t
  end
  
  (** Get version check status
  
      Retrieve information about the last time the version check ran. *)
  val get_version_check : t -> unit -> ResponseDto.t
  
  (** Retrieve version check state
  
      Retrieve the current state of the version check process. *)
  val get_version_check_state : t -> unit -> ResponseDto.t
end

module ValidateLibraryImportPath : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : import_path:string -> ?is_valid:bool -> ?message:string -> unit -> t
    
    val import_path : t -> string
    
    val is_valid : t -> bool
    
    val message : t -> string option
    
    val jsont : t Jsont.t
  end
end

module ValidateLibrary : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : ?import_paths:ValidateLibraryImportPath.ResponseDto.t list -> unit -> t
    
    val import_paths : t -> ValidateLibraryImportPath.ResponseDto.t list option
    
    val jsont : t Jsont.t
  end
  
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : ?exclusion_patterns:string list -> ?import_paths:string list -> unit -> t
    
    val exclusion_patterns : t -> string list option
    
    val import_paths : t -> string list option
    
    val jsont : t Jsont.t
  end
  
  (** Validate library settings
  
      Validate the settings of an external library. *)
  val validate : id:string -> body:Dto.t -> t -> unit -> ResponseDto.t
end

module ValidateAccessToken : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : auth_status:bool -> unit -> t
    
    val auth_status : t -> bool
    
    val jsont : t Jsont.t
  end
  
  (** Validate access token
  
      Validate the current authorization method is still valid. *)
  val validate_access_token : t -> unit -> ResponseDto.t
end

module UserMetadataKey : sig
  module T : sig
    type t = [
      | `Preferences
      | `License
      | `Onboarding
    ]
    
    val jsont : t Jsont.t
  end
end

module SyncUserMetadataV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : key:UserMetadataKey.T.t -> user_id:string -> value:Jsont.json -> unit -> t
    
    val key : t -> UserMetadataKey.T.t
    
    val user_id : t -> string
    
    val value : t -> Jsont.json
    
    val jsont : t Jsont.t
  end
end

module SyncUserMetadataDeleteV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : key:UserMetadataKey.T.t -> user_id:string -> unit -> t
    
    val key : t -> UserMetadataKey.T.t
    
    val user_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module UserLicense : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : activated_at:Ptime.t -> activation_key:string -> license_key:string -> unit -> t
    
    val activated_at : t -> Ptime.t
    
    val activation_key : t -> string
    
    val license_key : t -> string
    
    val jsont : t Jsont.t
  end
end

module UserAvatarColor : sig
  module T : sig
    type t = [
      | `Primary
      | `Pink
      | `Red
      | `Yellow
      | `Blue
      | `Green
      | `Purple
      | `Orange
      | `Gray
      | `Amber
    ]
    
    val jsont : t Jsont.t
  end
end

module UserUpdateMe : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : ?avatar_color:UserAvatarColor.T.t -> ?email:string -> ?name:string -> ?password:string -> unit -> t
    
    val avatar_color : t -> UserAvatarColor.T.t option
    
    val email : t -> string option
    
    val name : t -> string option
    
    val password : t -> string option
    
    val jsont : t Jsont.t
  end
end

module User : sig
  module Status : sig
    type t = [
      | `Active
      | `Removing
      | `Deleted
    ]
    
    val jsont : t Jsont.t
  end
  
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : avatar_color:UserAvatarColor.T.t -> email:string -> id:string -> name:string -> profile_changed_at:Ptime.t -> profile_image_path:string -> unit -> t
    
    val avatar_color : t -> UserAvatarColor.T.t
    
    val email : t -> string
    
    val id : t -> string
    
    val name : t -> string
    
    val profile_changed_at : t -> Ptime.t
    
    val profile_image_path : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Get all users
  
      Retrieve a list of all users on the server. *)
  val search_users : t -> unit -> ResponseDto.t
  
  (** Retrieve a user
  
      Retrieve a specific user by their ID. *)
  val get_user : id:string -> t -> unit -> ResponseDto.t
end

module SyncUserV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : avatar_color:UserAvatarColor.T.t -> email:string -> has_profile_image:bool -> id:string -> name:string -> profile_changed_at:Ptime.t -> ?deleted_at:Ptime.t -> unit -> t
    
    val avatar_color : t -> UserAvatarColor.T.t
    
    val deleted_at : t -> Ptime.t option
    
    val email : t -> string
    
    val has_profile_image : t -> bool
    
    val id : t -> string
    
    val name : t -> string
    
    val profile_changed_at : t -> Ptime.t
    
    val jsont : t Jsont.t
  end
end

module SyncAuthUserV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : avatar_color:UserAvatarColor.T.t -> email:string -> has_profile_image:bool -> id:string -> is_admin:bool -> name:string -> oauth_id:string -> profile_changed_at:Ptime.t -> quota_usage_in_bytes:int -> ?deleted_at:Ptime.t -> ?pin_code:string -> ?quota_size_in_bytes:int -> ?storage_label:string -> unit -> t
    
    val avatar_color : t -> UserAvatarColor.T.t
    
    val deleted_at : t -> Ptime.t option
    
    val email : t -> string
    
    val has_profile_image : t -> bool
    
    val id : t -> string
    
    val is_admin : t -> bool
    
    val name : t -> string
    
    val oauth_id : t -> string
    
    val pin_code : t -> string option
    
    val profile_changed_at : t -> Ptime.t
    
    val quota_size_in_bytes : t -> int option
    
    val quota_usage_in_bytes : t -> int
    
    val storage_label : t -> string option
    
    val jsont : t Jsont.t
  end
end

module Partner : sig
  module UpdateDto : sig
    type t
    
    (** Construct a value *)
    val v : in_timeline:bool -> unit -> t
    
    val in_timeline : t -> bool
    
    val jsont : t Jsont.t
  end
  
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : avatar_color:UserAvatarColor.T.t -> email:string -> id:string -> name:string -> profile_changed_at:Ptime.t -> profile_image_path:string -> ?in_timeline:bool -> unit -> t
    
    val avatar_color : t -> UserAvatarColor.T.t
    
    val email : t -> string
    
    val id : t -> string
    
    val in_timeline : t -> bool option
    
    val name : t -> string
    
    val profile_changed_at : t -> Ptime.t
    
    val profile_image_path : t -> string
    
    val jsont : t Jsont.t
  end
  
  module CreateDto : sig
    type t
    
    (** Construct a value *)
    val v : shared_with_id:string -> unit -> t
    
    val shared_with_id : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve partners
  
      Retrieve a list of partners with whom assets are shared. *)
  val get_partners : direction:string -> t -> unit -> ResponseDto.t
  
  (** Create a partner
  
      Create a new partner to share assets with. *)
  val create_partner : body:CreateDto.t -> t -> unit -> ResponseDto.t
  
  (** Create a partner
  
      Create a new partner to share assets with. *)
  val create_partner_deprecated : id:string -> t -> unit -> ResponseDto.t
  
  (** Update a partner
  
      Specify whether a partner's assets should appear in the user's timeline. *)
  val update_partner : id:string -> body:UpdateDto.t -> t -> unit -> ResponseDto.t
end

module Avatar : sig
  module Update : sig
    type t
    
    (** Construct a value *)
    val v : ?color:UserAvatarColor.T.t -> unit -> t
    
    val color : t -> UserAvatarColor.T.t option
    
    val jsont : t Jsont.t
  end
end

module UserAdminDelete : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : ?force:bool -> unit -> t
    
    val force : t -> bool option
    
    val jsont : t Jsont.t
  end
end

module UsageByUser : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : photos:int -> usage:int64 -> usage_photos:int64 -> usage_videos:int64 -> user_id:string -> user_name:string -> videos:int -> ?quota_size_in_bytes:int64 -> unit -> t
    
    val photos : t -> int
    
    val quota_size_in_bytes : t -> int64 option
    
    val usage : t -> int64
    
    val usage_photos : t -> int64
    
    val usage_videos : t -> int64
    
    val user_id : t -> string
    
    val user_name : t -> string
    
    val videos : t -> int
    
    val jsont : t Jsont.t
  end
end

module ServerStats : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : ?photos:int -> ?usage:int64 -> ?usage_by_user:UsageByUser.Dto.t list -> ?usage_photos:int64 -> ?usage_videos:int64 -> ?videos:int -> unit -> t
    
    val photos : t -> int
    
    val usage : t -> int64
    
    val usage_by_user : t -> UsageByUser.Dto.t list
    
    val usage_photos : t -> int64
    
    val usage_videos : t -> int64
    
    val videos : t -> int
    
    val jsont : t Jsont.t
  end
  
  (** Get statistics
  
      Retrieve statistics about the entire Immich instance such as asset counts. *)
  val get_server_statistics : t -> unit -> ResponseDto.t
end

module UpdateLibrary : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : ?exclusion_patterns:string list -> ?import_paths:string list -> ?name:string -> unit -> t
    
    val exclusion_patterns : t -> string list option
    
    val import_paths : t -> string list option
    
    val name : t -> string option
    
    val jsont : t Jsont.t
  end
end

module TranscodePolicy : sig
  module T : sig
    type t = [
      | `All
      | `Optimal
      | `Bitrate
      | `Required
      | `Disabled
    ]
    
    val jsont : t Jsont.t
  end
end

module TranscodeHwaccel : sig
  module T : sig
    type t = [
      | `Nvenc
      | `Qsv
      | `Vaapi
      | `Rkmpp
      | `Disabled
    ]
    
    val jsont : t Jsont.t
  end
end

module ToneMapping : sig
  module T : sig
    type t = [
      | `Hable
      | `Mobius
      | `Reinhard
      | `Disabled
    ]
    
    val jsont : t Jsont.t
  end
end

module TimeBuckets : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value
        @param count Number of assets in this time bucket
        @param time_bucket Time bucket identifier in YYYY-MM-DD format representing the start of the time period
    *)
    val v : count:int -> time_bucket:string -> unit -> t
    
    (** Number of assets in this time bucket *)
    val count : t -> int
    
    (** Time bucket identifier in YYYY-MM-DD format representing the start of the time period *)
    val time_bucket : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Get time buckets
  
      Retrieve a list of all minimal time buckets. 
      @param album_id Filter assets belonging to a specific album
      @param is_favorite Filter by favorite status (true for favorites only, false for non-favorites only)
      @param is_trashed Filter by trash status (true for trashed assets only, false for non-trashed only)
      @param order Sort order for assets within time buckets (ASC for oldest first, DESC for newest first)
      @param person_id Filter assets containing a specific person (face recognition)
      @param tag_id Filter assets with a specific tag
      @param user_id Filter assets by specific user ID
      @param visibility Filter by asset visibility status (ARCHIVE, TIMELINE, HIDDEN, LOCKED)
      @param with_coordinates Include location data in the response
      @param with_partners Include assets shared by partners
      @param with_stacked Include stacked assets in the response. When true, only primary assets from stacks are returned.
  *)
  val get_time_buckets : ?album_id:string -> ?is_favorite:string -> ?is_trashed:string -> ?key:string -> ?order:string -> ?person_id:string -> ?slug:string -> ?tag_id:string -> ?user_id:string -> ?visibility:string -> ?with_coordinates:string -> ?with_partners:string -> ?with_stacked:string -> t -> unit -> ResponseDto.t
end

module Template : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : html:string -> name:string -> unit -> t
    
    val html : t -> string
    
    val name : t -> string
    
    val jsont : t Jsont.t
  end
  
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : template:string -> unit -> t
    
    val template : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Render email template
  
      Retrieve a preview of the provided email template. *)
  val get_notification_template_admin : name:string -> body:Dto.t -> t -> unit -> ResponseDto.t
end

module Tags : sig
  module Update : sig
    type t
    
    (** Construct a value *)
    val v : ?enabled:bool -> ?sidebar_web:bool -> unit -> t
    
    val enabled : t -> bool option
    
    val sidebar_web : t -> bool option
    
    val jsont : t Jsont.t
  end
  
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : ?enabled:bool -> ?sidebar_web:bool -> unit -> t
    
    val enabled : t -> bool
    
    val sidebar_web : t -> bool
    
    val jsont : t Jsont.t
  end
end

module TagUpsert : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : tags:string list -> unit -> t
    
    val tags : t -> string list
    
    val jsont : t Jsont.t
  end
end

module Tag : sig
  module UpdateDto : sig
    type t
    
    (** Construct a value *)
    val v : ?color:string -> unit -> t
    
    val color : t -> string option
    
    val jsont : t Jsont.t
  end
  
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : created_at:Ptime.t -> id:string -> name:string -> updated_at:Ptime.t -> value:string -> ?color:string -> ?parent_id:string -> unit -> t
    
    val color : t -> string option
    
    val created_at : t -> Ptime.t
    
    val id : t -> string
    
    val name : t -> string
    
    val parent_id : t -> string option
    
    val updated_at : t -> Ptime.t
    
    val value : t -> string
    
    val jsont : t Jsont.t
  end
  
  module CreateDto : sig
    type t
    
    (** Construct a value *)
    val v : name:string -> ?color:string -> ?parent_id:string -> unit -> t
    
    val color : t -> string option
    
    val name : t -> string
    
    val parent_id : t -> string option
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve tags
  
      Retrieve a list of all tags. *)
  val get_all_tags : t -> unit -> ResponseDto.t
  
  (** Create a tag
  
      Create a new tag by providing a name and optional color. *)
  val create_tag : body:CreateDto.t -> t -> unit -> ResponseDto.t
  
  (** Upsert tags
  
      Create or update multiple tags in a single request. *)
  val upsert_tags : body:TagUpsert.Dto.t -> t -> unit -> ResponseDto.t
  
  (** Retrieve a tag
  
      Retrieve a specific tag by its ID. *)
  val get_tag_by_id : id:string -> t -> unit -> ResponseDto.t
  
  (** Update a tag
  
      Update an existing tag identified by its ID. *)
  val update_tag : id:string -> body:UpdateDto.t -> t -> unit -> ResponseDto.t
end

module TagBulkAssets : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : count:int -> unit -> t
    
    val count : t -> int
    
    val jsont : t Jsont.t
  end
  
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : asset_ids:string list -> tag_ids:string list -> unit -> t
    
    val asset_ids : t -> string list
    
    val tag_ids : t -> string list
    
    val jsont : t Jsont.t
  end
  
  (** Tag assets
  
      Add multiple tags to multiple assets in a single request. *)
  val bulk_tag_assets : body:Dto.t -> t -> unit -> ResponseDto.t
end

module SystemConfigUser : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : delete_delay:int -> unit -> t
    
    val delete_delay : t -> int
    
    val jsont : t Jsont.t
  end
end

module SystemConfigTrash : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : days:int -> enabled:bool -> unit -> t
    
    val days : t -> int
    
    val enabled : t -> bool
    
    val jsont : t Jsont.t
  end
end

module SystemConfigTheme : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : custom_css:string -> unit -> t
    
    val custom_css : t -> string
    
    val jsont : t Jsont.t
  end
end

module SystemConfigTemplateStorageOption : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : day_options:string list -> hour_options:string list -> minute_options:string list -> month_options:string list -> preset_options:string list -> second_options:string list -> week_options:string list -> year_options:string list -> unit -> t
    
    val day_options : t -> string list
    
    val hour_options : t -> string list
    
    val minute_options : t -> string list
    
    val month_options : t -> string list
    
    val preset_options : t -> string list
    
    val second_options : t -> string list
    
    val week_options : t -> string list
    
    val year_options : t -> string list
    
    val jsont : t Jsont.t
  end
  
  (** Get storage template options
  
      Retrieve exemplary storage template options. *)
  val get_storage_template_options : t -> unit -> Dto.t
end

module SystemConfigTemplateEmails : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : album_invite_template:string -> album_update_template:string -> welcome_template:string -> unit -> t
    
    val album_invite_template : t -> string
    
    val album_update_template : t -> string
    
    val welcome_template : t -> string
    
    val jsont : t Jsont.t
  end
end

module SystemConfigTemplates : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : email:SystemConfigTemplateEmails.Dto.t -> unit -> t
    
    val email : t -> SystemConfigTemplateEmails.Dto.t
    
    val jsont : t Jsont.t
  end
end

module SystemConfigStorageTemplate : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : enabled:bool -> hash_verification_enabled:bool -> template:string -> unit -> t
    
    val enabled : t -> bool
    
    val hash_verification_enabled : t -> bool
    
    val template : t -> string
    
    val jsont : t Jsont.t
  end
end

module SystemConfigSmtpTransport : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : host:string -> ignore_cert:bool -> password:string -> port:float -> secure:bool -> username:string -> unit -> t
    
    val host : t -> string
    
    val ignore_cert : t -> bool
    
    val password : t -> string
    
    val port : t -> float
    
    val secure : t -> bool
    
    val username : t -> string
    
    val jsont : t Jsont.t
  end
end

module SystemConfigSmtp : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : enabled:bool -> from:string -> reply_to:string -> transport:SystemConfigSmtpTransport.Dto.t -> unit -> t
    
    val enabled : t -> bool
    
    val from : t -> string
    
    val reply_to : t -> string
    
    val transport : t -> SystemConfigSmtpTransport.Dto.t
    
    val jsont : t Jsont.t
  end
end

module TestEmail : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : message_id:string -> unit -> t
    
    val message_id : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Send test email
  
      Send a test email using the provided SMTP configuration. *)
  val send_test_email_admin : body:SystemConfigSmtp.Dto.t -> t -> unit -> ResponseDto.t
end

module SystemConfigNotifications : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : smtp:SystemConfigSmtp.Dto.t -> unit -> t
    
    val smtp : t -> SystemConfigSmtp.Dto.t
    
    val jsont : t Jsont.t
  end
end

module SystemConfigServer : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : external_domain:string -> login_page_message:string -> public_users:bool -> unit -> t
    
    val external_domain : t -> string
    
    val login_page_message : t -> string
    
    val public_users : t -> bool
    
    val jsont : t Jsont.t
  end
end

module SystemConfigReverseGeocoding : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : enabled:bool -> unit -> t
    
    val enabled : t -> bool
    
    val jsont : t Jsont.t
  end
end

module SystemConfigPasswordLogin : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : enabled:bool -> unit -> t
    
    val enabled : t -> bool
    
    val jsont : t Jsont.t
  end
end

module SystemConfigNightlyTasks : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : cluster_new_faces:bool -> database_cleanup:bool -> generate_memories:bool -> missing_thumbnails:bool -> start_time:string -> sync_quota_usage:bool -> unit -> t
    
    val cluster_new_faces : t -> bool
    
    val database_cleanup : t -> bool
    
    val generate_memories : t -> bool
    
    val missing_thumbnails : t -> bool
    
    val start_time : t -> string
    
    val sync_quota_usage : t -> bool
    
    val jsont : t Jsont.t
  end
end

module SystemConfigNewVersionCheck : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : enabled:bool -> unit -> t
    
    val enabled : t -> bool
    
    val jsont : t Jsont.t
  end
end

module SystemConfigMap : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : dark_style:string -> enabled:bool -> light_style:string -> unit -> t
    
    val dark_style : t -> string
    
    val enabled : t -> bool
    
    val light_style : t -> string
    
    val jsont : t Jsont.t
  end
end

module SystemConfigLibraryWatch : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : enabled:bool -> unit -> t
    
    val enabled : t -> bool
    
    val jsont : t Jsont.t
  end
end

module SystemConfigLibraryScan : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : cron_expression:string -> enabled:bool -> unit -> t
    
    val cron_expression : t -> string
    
    val enabled : t -> bool
    
    val jsont : t Jsont.t
  end
end

module SystemConfigLibrary : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : scan:SystemConfigLibraryScan.Dto.t -> watch:SystemConfigLibraryWatch.Dto.t -> unit -> t
    
    val scan : t -> SystemConfigLibraryScan.Dto.t
    
    val watch : t -> SystemConfigLibraryWatch.Dto.t
    
    val jsont : t Jsont.t
  end
end

module SystemConfigFaces : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : import:bool -> unit -> t
    
    val import : t -> bool
    
    val jsont : t Jsont.t
  end
end

module SystemConfigMetadata : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : faces:SystemConfigFaces.Dto.t -> unit -> t
    
    val faces : t -> SystemConfigFaces.Dto.t
    
    val jsont : t Jsont.t
  end
end

module SyncUserDeleteV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : user_id:string -> unit -> t
    
    val user_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module SyncStackV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : created_at:Ptime.t -> id:string -> owner_id:string -> primary_asset_id:string -> updated_at:Ptime.t -> unit -> t
    
    val created_at : t -> Ptime.t
    
    val id : t -> string
    
    val owner_id : t -> string
    
    val primary_asset_id : t -> string
    
    val updated_at : t -> Ptime.t
    
    val jsont : t Jsont.t
  end
end

module SyncStackDeleteV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : stack_id:string -> unit -> t
    
    val stack_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module SyncResetV1 : sig
  module T : sig
    type t = Jsont.json
    
    val jsont : t Jsont.t
    
    val v : unit -> t
  end
end

module SyncRequest : sig
  module Type : sig
    type t = [
      | `Albums_v1
      | `Album_users_v1
      | `Album_to_assets_v1
      | `Album_assets_v1
      | `Album_asset_exifs_v1
      | `Assets_v1
      | `Asset_exifs_v1
      | `Asset_metadata_v1
      | `Auth_users_v1
      | `Memories_v1
      | `Memory_to_assets_v1
      | `Partners_v1
      | `Partner_assets_v1
      | `Partner_asset_exifs_v1
      | `Partner_stacks_v1
      | `Stacks_v1
      | `Users_v1
      | `People_v1
      | `Asset_faces_v1
      | `User_metadata_v1
    ]
    
    val jsont : t Jsont.t
  end
end

module SyncStream : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : types:SyncRequest.Type.t list -> ?reset:bool -> unit -> t
    
    val reset : t -> bool option
    
    val types : t -> SyncRequest.Type.t list
    
    val jsont : t Jsont.t
  end
end

module SyncPersonV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : created_at:Ptime.t -> id:string -> is_favorite:bool -> is_hidden:bool -> name:string -> owner_id:string -> updated_at:Ptime.t -> ?birth_date:Ptime.t -> ?color:string -> ?face_asset_id:string -> unit -> t
    
    val birth_date : t -> Ptime.t option
    
    val color : t -> string option
    
    val created_at : t -> Ptime.t
    
    val face_asset_id : t -> string option
    
    val id : t -> string
    
    val is_favorite : t -> bool
    
    val is_hidden : t -> bool
    
    val name : t -> string
    
    val owner_id : t -> string
    
    val updated_at : t -> Ptime.t
    
    val jsont : t Jsont.t
  end
end

module SyncPersonDeleteV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : person_id:string -> unit -> t
    
    val person_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module SyncPartnerV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : in_timeline:bool -> shared_by_id:string -> shared_with_id:string -> unit -> t
    
    val in_timeline : t -> bool
    
    val shared_by_id : t -> string
    
    val shared_with_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module SyncPartnerDeleteV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : shared_by_id:string -> shared_with_id:string -> unit -> t
    
    val shared_by_id : t -> string
    
    val shared_with_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module SyncMemoryDeleteV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : memory_id:string -> unit -> t
    
    val memory_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module SyncMemoryAssetV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : asset_id:string -> memory_id:string -> unit -> t
    
    val asset_id : t -> string
    
    val memory_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module SyncMemoryAssetDeleteV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : asset_id:string -> memory_id:string -> unit -> t
    
    val asset_id : t -> string
    
    val memory_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module SyncEntity : sig
  module Type : sig
    type t = [
      | `Auth_user_v1
      | `User_v1
      | `User_delete_v1
      | `Asset_v1
      | `Asset_delete_v1
      | `Asset_exif_v1
      | `Asset_metadata_v1
      | `Asset_metadata_delete_v1
      | `Partner_v1
      | `Partner_delete_v1
      | `Partner_asset_v1
      | `Partner_asset_backfill_v1
      | `Partner_asset_delete_v1
      | `Partner_asset_exif_v1
      | `Partner_asset_exif_backfill_v1
      | `Partner_stack_backfill_v1
      | `Partner_stack_delete_v1
      | `Partner_stack_v1
      | `Album_v1
      | `Album_delete_v1
      | `Album_user_v1
      | `Album_user_backfill_v1
      | `Album_user_delete_v1
      | `Album_asset_create_v1
      | `Album_asset_update_v1
      | `Album_asset_backfill_v1
      | `Album_asset_exif_create_v1
      | `Album_asset_exif_update_v1
      | `Album_asset_exif_backfill_v1
      | `Album_to_asset_v1
      | `Album_to_asset_delete_v1
      | `Album_to_asset_backfill_v1
      | `Memory_v1
      | `Memory_delete_v1
      | `Memory_to_asset_v1
      | `Memory_to_asset_delete_v1
      | `Stack_v1
      | `Stack_delete_v1
      | `Person_v1
      | `Person_delete_v1
      | `Asset_face_v1
      | `Asset_face_delete_v1
      | `User_metadata_v1
      | `User_metadata_delete_v1
      | `Sync_ack_v1
      | `Sync_reset_v1
      | `Sync_complete_v1
    ]
    
    val jsont : t Jsont.t
  end
end

module SyncAckDelete : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : ?types:SyncEntity.Type.t list -> unit -> t
    
    val types : t -> SyncEntity.Type.t list option
    
    val jsont : t Jsont.t
  end
end

module SyncAck : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : ack:string -> type_:SyncEntity.Type.t -> unit -> t
    
    val ack : t -> string
    
    val type_ : t -> SyncEntity.Type.t
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve acknowledgements
  
      Retrieve the synchronization acknowledgments for the current session. *)
  val get_sync_ack : t -> unit -> Dto.t
end

module SyncCompleteV1 : sig
  module T : sig
    type t = Jsont.json
    
    val jsont : t Jsont.t
    
    val v : unit -> t
  end
end

module SyncAssetMetadataV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : asset_id:string -> key:string -> value:Jsont.json -> unit -> t
    
    val asset_id : t -> string
    
    val key : t -> string
    
    val value : t -> Jsont.json
    
    val jsont : t Jsont.t
  end
end

module SyncAssetMetadataDeleteV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : asset_id:string -> key:string -> unit -> t
    
    val asset_id : t -> string
    
    val key : t -> string
    
    val jsont : t Jsont.t
  end
end

module SyncAssetFaceV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : asset_id:string -> bounding_box_x1:int -> bounding_box_x2:int -> bounding_box_y1:int -> bounding_box_y2:int -> id:string -> image_height:int -> image_width:int -> source_type:string -> ?person_id:string -> unit -> t
    
    val asset_id : t -> string
    
    val bounding_box_x1 : t -> int
    
    val bounding_box_x2 : t -> int
    
    val bounding_box_y1 : t -> int
    
    val bounding_box_y2 : t -> int
    
    val id : t -> string
    
    val image_height : t -> int
    
    val image_width : t -> int
    
    val person_id : t -> string option
    
    val source_type : t -> string
    
    val jsont : t Jsont.t
  end
end

module SyncAssetFaceDeleteV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : asset_face_id:string -> unit -> t
    
    val asset_face_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module SyncAssetExifV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : asset_id:string -> ?city:string -> ?country:string -> ?date_time_original:Ptime.t -> ?description:string -> ?exif_image_height:int -> ?exif_image_width:int -> ?exposure_time:string -> ?f_number:float -> ?file_size_in_byte:int -> ?focal_length:float -> ?fps:float -> ?iso:int -> ?latitude:float -> ?lens_model:string -> ?longitude:float -> ?make:string -> ?model:string -> ?modify_date:Ptime.t -> ?orientation:string -> ?profile_description:string -> ?projection_type:string -> ?rating:int -> ?state:string -> ?time_zone:string -> unit -> t
    
    val asset_id : t -> string
    
    val city : t -> string option
    
    val country : t -> string option
    
    val date_time_original : t -> Ptime.t option
    
    val description : t -> string option
    
    val exif_image_height : t -> int option
    
    val exif_image_width : t -> int option
    
    val exposure_time : t -> string option
    
    val f_number : t -> float option
    
    val file_size_in_byte : t -> int option
    
    val focal_length : t -> float option
    
    val fps : t -> float option
    
    val iso : t -> int option
    
    val latitude : t -> float option
    
    val lens_model : t -> string option
    
    val longitude : t -> float option
    
    val make : t -> string option
    
    val model : t -> string option
    
    val modify_date : t -> Ptime.t option
    
    val orientation : t -> string option
    
    val profile_description : t -> string option
    
    val projection_type : t -> string option
    
    val rating : t -> int option
    
    val state : t -> string option
    
    val time_zone : t -> string option
    
    val jsont : t Jsont.t
  end
end

module SyncAssetDeleteV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : asset_id:string -> unit -> t
    
    val asset_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module SyncAlbumUserDeleteV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : album_id:string -> user_id:string -> unit -> t
    
    val album_id : t -> string
    
    val user_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module SyncAlbumToAssetV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : album_id:string -> asset_id:string -> unit -> t
    
    val album_id : t -> string
    
    val asset_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module SyncAlbumToAssetDeleteV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : album_id:string -> asset_id:string -> unit -> t
    
    val album_id : t -> string
    
    val asset_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module SyncAlbumDeleteV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : album_id:string -> unit -> t
    
    val album_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module SyncAckV1 : sig
  module T : sig
    type t = Jsont.json
    
    val jsont : t Jsont.t
    
    val v : unit -> t
  end
end

module SyncAckSet : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : acks:string list -> unit -> t
    
    val acks : t -> string list
    
    val jsont : t Jsont.t
  end
end

module StorageFolder : sig
  module T : sig
    type t = [
      | `Encoded_video
      | `Library
      | `Upload
      | `Profile
      | `Thumbs
      | `Backups
    ]
    
    val jsont : t Jsont.t
  end
end

module MaintenanceDetectInstallStorageFolder : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : files:float -> folder:StorageFolder.T.t -> readable:bool -> writable:bool -> unit -> t
    
    val files : t -> float
    
    val folder : t -> StorageFolder.T.t
    
    val readable : t -> bool
    
    val writable : t -> bool
    
    val jsont : t Jsont.t
  end
end

module MaintenanceDetectInstall : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : storage:MaintenanceDetectInstallStorageFolder.Dto.t list -> unit -> t
    
    val storage : t -> MaintenanceDetectInstallStorageFolder.Dto.t list
    
    val jsont : t Jsont.t
  end
  
  (** Detect existing install
  
      Collect integrity checks and other heuristics about local data. *)
  val detect_prior_install : t -> unit -> ResponseDto.t
end

module Source : sig
  module Type : sig
    type t = [
      | `Machine_learning
      | `Exif
      | `Manual
    ]
    
    val jsont : t Jsont.t
  end
end

module AssetFaceWithoutPerson : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : bounding_box_x1:int -> bounding_box_x2:int -> bounding_box_y1:int -> bounding_box_y2:int -> id:string -> image_height:int -> image_width:int -> ?source_type:Source.Type.t -> unit -> t
    
    val bounding_box_x1 : t -> int
    
    val bounding_box_x2 : t -> int
    
    val bounding_box_y1 : t -> int
    
    val bounding_box_y2 : t -> int
    
    val id : t -> string
    
    val image_height : t -> int
    
    val image_width : t -> int
    
    val source_type : t -> Source.Type.t option
    
    val jsont : t Jsont.t
  end
end

module PersonWithFaces : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : faces:AssetFaceWithoutPerson.ResponseDto.t list -> id:string -> is_hidden:bool -> name:string -> thumbnail_path:string -> ?birth_date:string -> ?color:string -> ?is_favorite:bool -> ?updated_at:Ptime.t -> unit -> t
    
    val birth_date : t -> string option
    
    val color : t -> string option
    
    val faces : t -> AssetFaceWithoutPerson.ResponseDto.t list
    
    val id : t -> string
    
    val is_favorite : t -> bool option
    
    val is_hidden : t -> bool
    
    val name : t -> string
    
    val thumbnail_path : t -> string
    
    val updated_at : t -> Ptime.t option
    
    val jsont : t Jsont.t
  end
end

module SignUp : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : email:string -> name:string -> password:string -> unit -> t
    
    val email : t -> string
    
    val name : t -> string
    
    val password : t -> string
    
    val jsont : t Jsont.t
  end
end

module SharedLinks : sig
  module Update : sig
    type t
    
    (** Construct a value *)
    val v : ?enabled:bool -> ?sidebar_web:bool -> unit -> t
    
    val enabled : t -> bool option
    
    val sidebar_web : t -> bool option
    
    val jsont : t Jsont.t
  end
  
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : ?enabled:bool -> ?sidebar_web:bool -> unit -> t
    
    val enabled : t -> bool
    
    val sidebar_web : t -> bool
    
    val jsont : t Jsont.t
  end
end

module SharedLinkEdit : sig
  module Dto : sig
    type t
    
    (** Construct a value
        @param change_expiry_time Few clients cannot send null to set the expiryTime to never.
    Setting this flag and not sending expiryAt is considered as null instead.
    Clients that can send null values can ignore this.
    *)
    val v : ?allow_download:bool -> ?allow_upload:bool -> ?change_expiry_time:bool -> ?description:string -> ?expires_at:Ptime.t -> ?password:string -> ?show_metadata:bool -> ?slug:string -> unit -> t
    
    val allow_download : t -> bool option
    
    val allow_upload : t -> bool option
    
    (** Few clients cannot send null to set the expiryTime to never.
    Setting this flag and not sending expiryAt is considered as null instead.
    Clients that can send null values can ignore this. *)
    val change_expiry_time : t -> bool option
    
    val description : t -> string option
    
    val expires_at : t -> Ptime.t option
    
    val password : t -> string option
    
    val show_metadata : t -> bool option
    
    val slug : t -> string option
    
    val jsont : t Jsont.t
  end
end

module SessionUnlock : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : ?password:string -> ?pin_code:string -> unit -> t
    
    val password : t -> string option
    
    val pin_code : t -> string option
    
    val jsont : t Jsont.t
  end
end

module Session : sig
  module UpdateDto : sig
    type t
    
    (** Construct a value *)
    val v : ?is_pending_sync_reset:bool -> unit -> t
    
    val is_pending_sync_reset : t -> bool option
    
    val jsont : t Jsont.t
  end
  
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : created_at:string -> current:bool -> device_os:string -> device_type:string -> id:string -> is_pending_sync_reset:bool -> updated_at:string -> ?app_version:string -> ?expires_at:string -> unit -> t
    
    val app_version : t -> string option
    
    val created_at : t -> string
    
    val current : t -> bool
    
    val device_os : t -> string
    
    val device_type : t -> string
    
    val expires_at : t -> string option
    
    val id : t -> string
    
    val is_pending_sync_reset : t -> bool
    
    val updated_at : t -> string
    
    val jsont : t Jsont.t
  end
  
  module CreateDto : sig
    type t
    
    (** Construct a value
        @param duration session duration, in seconds
    *)
    val v : ?device_os:string -> ?device_type:string -> ?duration:float -> unit -> t
    
    val device_os : t -> string option
    
    val device_type : t -> string option
    
    (** session duration, in seconds *)
    val duration : t -> float option
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve user sessions
  
      Retrieve all sessions for a specific user. *)
  val get_user_sessions_admin : id:string -> t -> unit -> ResponseDto.t
  
  (** Retrieve sessions
  
      Retrieve a list of sessions for the user. *)
  val get_sessions : t -> unit -> ResponseDto.t
  
  (** Update a session
  
      Update a specific session identified by id. *)
  val update_session : id:string -> body:UpdateDto.t -> t -> unit -> ResponseDto.t
end

module SessionCreate : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : created_at:string -> current:bool -> device_os:string -> device_type:string -> id:string -> is_pending_sync_reset:bool -> token:string -> updated_at:string -> ?app_version:string -> ?expires_at:string -> unit -> t
    
    val app_version : t -> string option
    
    val created_at : t -> string
    
    val current : t -> bool
    
    val device_os : t -> string
    
    val device_type : t -> string
    
    val expires_at : t -> string option
    
    val id : t -> string
    
    val is_pending_sync_reset : t -> bool
    
    val token : t -> string
    
    val updated_at : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Create a session
  
      Create a session as a child to the current session. This endpoint is used for casting. *)
  val create_session : body:Session.CreateDto.t -> t -> unit -> ResponseDto.t
end

module ServerVersionHistory : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : created_at:Ptime.t -> id:string -> version:string -> unit -> t
    
    val created_at : t -> Ptime.t
    
    val id : t -> string
    
    val version : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Get version history
  
      Retrieve a list of past versions the server has been on. *)
  val get_version_history : t -> unit -> ResponseDto.t
end

module ServerVersion : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : major:int -> minor:int -> patch:int -> unit -> t
    
    val major : t -> int
    
    val minor : t -> int
    
    val patch : t -> int
    
    val jsont : t Jsont.t
  end
  
  (** Get server version
  
      Retrieve the current server version in semantic versioning (semver) format. *)
  val get_server_version : t -> unit -> ResponseDto.t
end

module ServerTheme : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : custom_css:string -> unit -> t
    
    val custom_css : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Get theme
  
      Retrieve the custom CSS, if existent. *)
  val get_theme : t -> unit -> Dto.t
end

module ServerStorage : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : disk_available:string -> disk_available_raw:int64 -> disk_size:string -> disk_size_raw:int64 -> disk_usage_percentage:float -> disk_use:string -> disk_use_raw:int64 -> unit -> t
    
    val disk_available : t -> string
    
    val disk_available_raw : t -> int64
    
    val disk_size : t -> string
    
    val disk_size_raw : t -> int64
    
    val disk_usage_percentage : t -> float
    
    val disk_use : t -> string
    
    val disk_use_raw : t -> int64
    
    val jsont : t Jsont.t
  end
  
  (** Get storage
  
      Retrieve the current storage utilization information of the server. *)
  val get_storage : t -> unit -> ResponseDto.t
end

module ServerPing : sig
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : res:string -> unit -> t
    
    val res : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Ping
  
      Pong *)
  val ping_server : t -> unit -> Response.t
end

module ServerMediaTypes : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : image:string list -> sidecar:string list -> video:string list -> unit -> t
    
    val image : t -> string list
    
    val sidecar : t -> string list
    
    val video : t -> string list
    
    val jsont : t Jsont.t
  end
  
  (** Get supported media types
  
      Retrieve all media types supported by the server. *)
  val get_supported_media_types : t -> unit -> ResponseDto.t
end

module ServerFeatures : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : config_file:bool -> duplicate_detection:bool -> email:bool -> facial_recognition:bool -> import_faces:bool -> map:bool -> oauth:bool -> oauth_auto_launch:bool -> ocr:bool -> password_login:bool -> reverse_geocoding:bool -> search:bool -> sidecar:bool -> smart_search:bool -> trash:bool -> unit -> t
    
    val config_file : t -> bool
    
    val duplicate_detection : t -> bool
    
    val email : t -> bool
    
    val facial_recognition : t -> bool
    
    val import_faces : t -> bool
    
    val map : t -> bool
    
    val oauth : t -> bool
    
    val oauth_auto_launch : t -> bool
    
    val ocr : t -> bool
    
    val password_login : t -> bool
    
    val reverse_geocoding : t -> bool
    
    val search : t -> bool
    
    val sidecar : t -> bool
    
    val smart_search : t -> bool
    
    val trash : t -> bool
    
    val jsont : t Jsont.t
  end
  
  (** Get features
  
      Retrieve available features supported by this server. *)
  val get_server_features : t -> unit -> Dto.t
end

module ServerConfig : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : external_domain:string -> is_initialized:bool -> is_onboarded:bool -> login_page_message:string -> maintenance_mode:bool -> map_dark_style_url:string -> map_light_style_url:string -> oauth_button_text:string -> public_users:bool -> trash_days:int -> user_delete_delay:int -> unit -> t
    
    val external_domain : t -> string
    
    val is_initialized : t -> bool
    
    val is_onboarded : t -> bool
    
    val login_page_message : t -> string
    
    val maintenance_mode : t -> bool
    
    val map_dark_style_url : t -> string
    
    val map_light_style_url : t -> string
    
    val oauth_button_text : t -> string
    
    val public_users : t -> bool
    
    val trash_days : t -> int
    
    val user_delete_delay : t -> int
    
    val jsont : t Jsont.t
  end
  
  (** Get config
  
      Retrieve the current server configuration. *)
  val get_server_config : t -> unit -> Dto.t
end

module ServerApkLinks : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : arm64v8a:string -> armeabiv7a:string -> universal:string -> x86_64:string -> unit -> t
    
    val arm64v8a : t -> string
    
    val armeabiv7a : t -> string
    
    val universal : t -> string
    
    val x86_64 : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Get APK links
  
      Retrieve links to the APKs for the current server version. *)
  val get_apk_links : t -> unit -> Dto.t
end

module ServerAbout : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : licensed:bool -> version:string -> version_url:string -> ?build:string -> ?build_image:string -> ?build_image_url:string -> ?build_url:string -> ?exiftool:string -> ?ffmpeg:string -> ?imagemagick:string -> ?libvips:string -> ?nodejs:string -> ?repository:string -> ?repository_url:string -> ?source_commit:string -> ?source_ref:string -> ?source_url:string -> ?third_party_bug_feature_url:string -> ?third_party_documentation_url:string -> ?third_party_source_url:string -> ?third_party_support_url:string -> unit -> t
    
    val build : t -> string option
    
    val build_image : t -> string option
    
    val build_image_url : t -> string option
    
    val build_url : t -> string option
    
    val exiftool : t -> string option
    
    val ffmpeg : t -> string option
    
    val imagemagick : t -> string option
    
    val libvips : t -> string option
    
    val licensed : t -> bool
    
    val nodejs : t -> string option
    
    val repository : t -> string option
    
    val repository_url : t -> string option
    
    val source_commit : t -> string option
    
    val source_ref : t -> string option
    
    val source_url : t -> string option
    
    val third_party_bug_feature_url : t -> string option
    
    val third_party_documentation_url : t -> string option
    
    val third_party_source_url : t -> string option
    
    val third_party_support_url : t -> string option
    
    val version : t -> string
    
    val version_url : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Get server information
  
      Retrieve a list of information about the server. *)
  val get_about_info : t -> unit -> ResponseDto.t
end

module SearchSuggestion : sig
  module Type : sig
    type t = [
      | `Country
      | `State
      | `City
      | `Camera_make
      | `Camera_model
      | `Camera_lens_model
    ]
    
    val jsont : t Jsont.t
  end
end

module SearchFacetCount : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : count:int -> value:string -> unit -> t
    
    val count : t -> int
    
    val value : t -> string
    
    val jsont : t Jsont.t
  end
end

module SearchFacet : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : counts:SearchFacetCount.ResponseDto.t list -> field_name:string -> unit -> t
    
    val counts : t -> SearchFacetCount.ResponseDto.t list
    
    val field_name : t -> string
    
    val jsont : t Jsont.t
  end
end

module RotateParameters : sig
  module T : sig
    type t
    
    (** Construct a value
        @param angle Rotation angle in degrees
    *)
    val v : angle:float -> unit -> t
    
    (** Rotation angle in degrees *)
    val angle : t -> float
    
    val jsont : t Jsont.t
  end
end

module ReverseGeocodingState : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : ?last_import_file_name:string -> ?last_update:string -> unit -> t
    
    val last_import_file_name : t -> string option
    
    val last_update : t -> string option
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve reverse geocoding state
  
      Retrieve the current state of the reverse geocoding import. *)
  val get_reverse_geocoding_state : t -> unit -> ResponseDto.t
end

module ReactionLevel : sig
  module T : sig
    type t = [
      | `Album
      | `Asset
    ]
    
    val jsont : t Jsont.t
  end
end

module Reaction : sig
  module Type : sig
    type t = [
      | `Comment
      | `Like
    ]
    
    val jsont : t Jsont.t
  end
end

module Activity : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : created_at:Ptime.t -> id:string -> type_:Reaction.Type.t -> user:User.ResponseDto.t -> ?asset_id:string -> ?comment:string -> unit -> t
    
    val asset_id : t -> string option
    
    val comment : t -> string option
    
    val created_at : t -> Ptime.t
    
    val id : t -> string
    
    val type_ : t -> Reaction.Type.t
    
    val user : t -> User.ResponseDto.t
    
    val jsont : t Jsont.t
  end
  
  module CreateDto : sig
    type t
    
    (** Construct a value *)
    val v : album_id:string -> type_:Reaction.Type.t -> ?asset_id:string -> ?comment:string -> unit -> t
    
    val album_id : t -> string
    
    val asset_id : t -> string option
    
    val comment : t -> string option
    
    val type_ : t -> Reaction.Type.t
    
    val jsont : t Jsont.t
  end
  
  (** List all activities
  
      Returns a list of activities for the selected asset or album. The activities are returned in sorted order, with the oldest activities appearing first. *)
  val get_activities : album_id:string -> ?asset_id:string -> ?level:string -> ?type_:string -> ?user_id:string -> t -> unit -> ResponseDto.t
  
  (** Create an activity
  
      Create a like or a comment for an album, or an asset in an album. *)
  val create_activity : body:CreateDto.t -> t -> unit -> ResponseDto.t
end

module Ratings : sig
  module Update : sig
    type t
    
    (** Construct a value *)
    val v : ?enabled:bool -> unit -> t
    
    val enabled : t -> bool option
    
    val jsont : t Jsont.t
  end
  
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : ?enabled:bool -> unit -> t
    
    val enabled : t -> bool
    
    val jsont : t Jsont.t
  end
end

module QueueStatusLegacy : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : is_active:bool -> is_paused:bool -> unit -> t
    
    val is_active : t -> bool
    
    val is_paused : t -> bool
    
    val jsont : t Jsont.t
  end
end

module QueueStatistics : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : active:int -> completed:int -> delayed:int -> failed:int -> paused:int -> waiting:int -> unit -> t
    
    val active : t -> int
    
    val completed : t -> int
    
    val delayed : t -> int
    
    val failed : t -> int
    
    val paused : t -> int
    
    val waiting : t -> int
    
    val jsont : t Jsont.t
  end
end

module QueueName : sig
  module T : sig
    type t = [
      | `Thumbnail_generation
      | `Metadata_extraction
      | `Video_conversion
      | `Face_detection
      | `Facial_recognition
      | `Smart_search
      | `Duplicate_detection
      | `Background_task
      | `Storage_template_migration
      | `Migration
      | `Search
      | `Sidecar
      | `Library
      | `Notifications
      | `Backup_database
      | `Ocr
      | `Workflow
      | `Editor
    ]
    
    val jsont : t Jsont.t
  end
end

module Queue : sig
  module UpdateDto : sig
    type t
    
    (** Construct a value *)
    val v : ?is_paused:bool -> unit -> t
    
    val is_paused : t -> bool option
    
    val jsont : t Jsont.t
  end
  
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : is_paused:bool -> name:QueueName.T.t -> statistics:QueueStatistics.Dto.t -> unit -> t
    
    val is_paused : t -> bool
    
    val name : t -> QueueName.T.t
    
    val statistics : t -> QueueStatistics.Dto.t
    
    val jsont : t Jsont.t
  end
  
  (** List all queues
  
      Retrieves a list of queues. *)
  val get_queues : t -> unit -> ResponseDto.t
  
  (** Retrieve a queue
  
      Retrieves a specific queue by its name. *)
  val get_queue : name:string -> t -> unit -> ResponseDto.t
  
  (** Update a queue
  
      Change the paused status of a specific queue. *)
  val update_queue : name:string -> body:UpdateDto.t -> t -> unit -> ResponseDto.t
end

module QueueDelete : sig
  module Dto : sig
    type t
    
    (** Construct a value
        @param failed If true, will also remove failed jobs from the queue.
    *)
    val v : ?failed:bool -> unit -> t
    
    (** If true, will also remove failed jobs from the queue. *)
    val failed : t -> bool option
    
    val jsont : t Jsont.t
  end
end

module QueueCommand : sig
  module T : sig
    type t = [
      | `Start
      | `Pause
      | `Resume
      | `Empty
      | `Clear_failed
    ]
    
    val jsont : t Jsont.t
  end
  
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : command:T.t -> ?force:bool -> unit -> t
    
    val command : t -> T.t
    
    val force : t -> bool option
    
    val jsont : t Jsont.t
  end
end

module QueueResponseLegacy : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : job_counts:QueueStatistics.Dto.t -> queue_status:QueueStatusLegacy.Dto.t -> unit -> t
    
    val job_counts : t -> QueueStatistics.Dto.t
    
    val queue_status : t -> QueueStatusLegacy.Dto.t
    
    val jsont : t Jsont.t
  end
  
  (** Run jobs
  
      Queue all assets for a specific job type. Defaults to only queueing assets that have not yet been processed, but the force command can be used to re-process all assets. *)
  val run_queue_command_legacy : name:string -> body:QueueCommand.Dto.t -> t -> unit -> Dto.t
end

module QueuesResponseLegacy : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : background_task:QueueResponseLegacy.Dto.t -> backup_database:QueueResponseLegacy.Dto.t -> duplicate_detection:QueueResponseLegacy.Dto.t -> editor:QueueResponseLegacy.Dto.t -> face_detection:QueueResponseLegacy.Dto.t -> facial_recognition:QueueResponseLegacy.Dto.t -> library:QueueResponseLegacy.Dto.t -> metadata_extraction:QueueResponseLegacy.Dto.t -> migration:QueueResponseLegacy.Dto.t -> notifications:QueueResponseLegacy.Dto.t -> ocr:QueueResponseLegacy.Dto.t -> search:QueueResponseLegacy.Dto.t -> sidecar:QueueResponseLegacy.Dto.t -> smart_search:QueueResponseLegacy.Dto.t -> storage_template_migration:QueueResponseLegacy.Dto.t -> thumbnail_generation:QueueResponseLegacy.Dto.t -> video_conversion:QueueResponseLegacy.Dto.t -> workflow:QueueResponseLegacy.Dto.t -> unit -> t
    
    val background_task : t -> QueueResponseLegacy.Dto.t
    
    val backup_database : t -> QueueResponseLegacy.Dto.t
    
    val duplicate_detection : t -> QueueResponseLegacy.Dto.t
    
    val editor : t -> QueueResponseLegacy.Dto.t
    
    val face_detection : t -> QueueResponseLegacy.Dto.t
    
    val facial_recognition : t -> QueueResponseLegacy.Dto.t
    
    val library : t -> QueueResponseLegacy.Dto.t
    
    val metadata_extraction : t -> QueueResponseLegacy.Dto.t
    
    val migration : t -> QueueResponseLegacy.Dto.t
    
    val notifications : t -> QueueResponseLegacy.Dto.t
    
    val ocr : t -> QueueResponseLegacy.Dto.t
    
    val search : t -> QueueResponseLegacy.Dto.t
    
    val sidecar : t -> QueueResponseLegacy.Dto.t
    
    val smart_search : t -> QueueResponseLegacy.Dto.t
    
    val storage_template_migration : t -> QueueResponseLegacy.Dto.t
    
    val thumbnail_generation : t -> QueueResponseLegacy.Dto.t
    
    val video_conversion : t -> QueueResponseLegacy.Dto.t
    
    val workflow : t -> QueueResponseLegacy.Dto.t
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve queue counts and status
  
      Retrieve the counts of the current queue, as well as the current status. *)
  val get_queues_legacy : t -> unit -> Dto.t
end

module Purchase : sig
  module Update : sig
    type t
    
    (** Construct a value *)
    val v : ?hide_buy_button_until:string -> ?show_support_badge:bool -> unit -> t
    
    val hide_buy_button_until : t -> string option
    
    val show_support_badge : t -> bool option
    
    val jsont : t Jsont.t
  end
  
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : hide_buy_button_until:string -> show_support_badge:bool -> unit -> t
    
    val hide_buy_button_until : t -> string
    
    val show_support_badge : t -> bool
    
    val jsont : t Jsont.t
  end
end

module PluginContext : sig
  module Type : sig
    type t = [
      | `Asset
      | `Album
      | `Person
    ]
    
    val jsont : t Jsont.t
  end
end

module PluginTrigger : sig
  module Type : sig
    type t = [
      | `Asset_create
      | `Person_recognized
    ]
    
    val jsont : t Jsont.t
  end
  
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : context_type:PluginContext.Type.t -> type_:Type.t -> unit -> t
    
    val context_type : t -> PluginContext.Type.t
    
    val type_ : t -> Type.t
    
    val jsont : t Jsont.t
  end
  
  (** List all plugin triggers
  
      Retrieve a list of all available plugin triggers. *)
  val get_plugin_triggers : t -> unit -> ResponseDto.t
end

module Workflow : sig
  module UpdateDto : sig
    type t
    
    (** Construct a value *)
    val v : ?actions:WorkflowActionItem.Dto.t list -> ?description:string -> ?enabled:bool -> ?filters:WorkflowFilterItem.Dto.t list -> ?name:string -> ?trigger_type:PluginTrigger.Type.t -> unit -> t
    
    val actions : t -> WorkflowActionItem.Dto.t list option
    
    val description : t -> string option
    
    val enabled : t -> bool option
    
    val filters : t -> WorkflowFilterItem.Dto.t list option
    
    val name : t -> string option
    
    val trigger_type : t -> PluginTrigger.Type.t option
    
    val jsont : t Jsont.t
  end
  
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : actions:WorkflowAction.ResponseDto.t list -> created_at:string -> description:string -> enabled:bool -> filters:WorkflowFilter.ResponseDto.t list -> id:string -> owner_id:string -> trigger_type:PluginTrigger.Type.t -> ?name:string -> unit -> t
    
    val actions : t -> WorkflowAction.ResponseDto.t list
    
    val created_at : t -> string
    
    val description : t -> string
    
    val enabled : t -> bool
    
    val filters : t -> WorkflowFilter.ResponseDto.t list
    
    val id : t -> string
    
    val name : t -> string option
    
    val owner_id : t -> string
    
    val trigger_type : t -> PluginTrigger.Type.t
    
    val jsont : t Jsont.t
  end
  
  module CreateDto : sig
    type t
    
    (** Construct a value *)
    val v : actions:WorkflowActionItem.Dto.t list -> filters:WorkflowFilterItem.Dto.t list -> name:string -> trigger_type:PluginTrigger.Type.t -> ?description:string -> ?enabled:bool -> unit -> t
    
    val actions : t -> WorkflowActionItem.Dto.t list
    
    val description : t -> string option
    
    val enabled : t -> bool option
    
    val filters : t -> WorkflowFilterItem.Dto.t list
    
    val name : t -> string
    
    val trigger_type : t -> PluginTrigger.Type.t
    
    val jsont : t Jsont.t
  end
  
  (** List all workflows
  
      Retrieve a list of workflows available to the authenticated user. *)
  val get_workflows : t -> unit -> ResponseDto.t
  
  (** Create a workflow
  
      Create a new workflow, the workflow can also be created with empty filters and actions. *)
  val create_workflow : body:CreateDto.t -> t -> unit -> ResponseDto.t
  
  (** Retrieve a workflow
  
      Retrieve information about a specific workflow by its ID. *)
  val get_workflow : id:string -> t -> unit -> ResponseDto.t
  
  (** Update a workflow
  
      Update the information of a specific workflow by its ID. This endpoint can be used to update the workflow name, description, trigger type, filters and actions order, etc. *)
  val update_workflow : id:string -> body:UpdateDto.t -> t -> unit -> ResponseDto.t
end

module PluginFilter : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : description:string -> id:string -> method_name:string -> plugin_id:string -> supported_contexts:PluginContext.Type.t list -> title:string -> ?schema:Jsont.json -> unit -> t
    
    val description : t -> string
    
    val id : t -> string
    
    val method_name : t -> string
    
    val plugin_id : t -> string
    
    val schema : t -> Jsont.json option
    
    val supported_contexts : t -> PluginContext.Type.t list
    
    val title : t -> string
    
    val jsont : t Jsont.t
  end
end

module PluginAction : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : description:string -> id:string -> method_name:string -> plugin_id:string -> supported_contexts:PluginContext.Type.t list -> title:string -> ?schema:Jsont.json -> unit -> t
    
    val description : t -> string
    
    val id : t -> string
    
    val method_name : t -> string
    
    val plugin_id : t -> string
    
    val schema : t -> Jsont.json option
    
    val supported_contexts : t -> PluginContext.Type.t list
    
    val title : t -> string
    
    val jsont : t Jsont.t
  end
end

module Plugin : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : actions:PluginAction.ResponseDto.t list -> author:string -> created_at:string -> description:string -> filters:PluginFilter.ResponseDto.t list -> id:string -> name:string -> title:string -> updated_at:string -> version:string -> unit -> t
    
    val actions : t -> PluginAction.ResponseDto.t list
    
    val author : t -> string
    
    val created_at : t -> string
    
    val description : t -> string
    
    val filters : t -> PluginFilter.ResponseDto.t list
    
    val id : t -> string
    
    val name : t -> string
    
    val title : t -> string
    
    val updated_at : t -> string
    
    val version : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** List all plugins
  
      Retrieve a list of plugins available to the authenticated user. *)
  val get_plugins : t -> unit -> ResponseDto.t
  
  (** Retrieve a plugin
  
      Retrieve information about a specific plugin by its ID. *)
  val get_plugin : id:string -> t -> unit -> ResponseDto.t
end

module Places : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : latitude:float -> longitude:float -> name:string -> ?admin1name:string -> ?admin2name:string -> unit -> t
    
    val admin1name : t -> string option
    
    val admin2name : t -> string option
    
    val latitude : t -> float
    
    val longitude : t -> float
    
    val name : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Search places
  
      Search for places by name. *)
  val search_places : name:string -> t -> unit -> ResponseDto.t
end

module PinCodeSetup : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : pin_code:string -> unit -> t
    
    val pin_code : t -> string
    
    val jsont : t Jsont.t
  end
end

module PinCodeReset : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : ?password:string -> ?pin_code:string -> unit -> t
    
    val password : t -> string option
    
    val pin_code : t -> string option
    
    val jsont : t Jsont.t
  end
end

module PinCodeChange : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : new_pin_code:string -> ?password:string -> ?pin_code:string -> unit -> t
    
    val new_pin_code : t -> string
    
    val password : t -> string option
    
    val pin_code : t -> string option
    
    val jsont : t Jsont.t
  end
end

module PersonStatistics : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : assets:int -> unit -> t
    
    val assets : t -> int
    
    val jsont : t Jsont.t
  end
  
  (** Get person statistics
  
      Retrieve statistics about a specific person. *)
  val get_person_statistics : id:string -> t -> unit -> ResponseDto.t
end

module Permission : sig
  module T : sig
    type t = [
      | `All
      | `Activity_create
      | `Activity_read
      | `Activity_update
      | `Activity_delete
      | `Activity_statistics
      | `Api_key_create
      | `Api_key_read
      | `Api_key_update
      | `Api_key_delete
      | `Asset_read
      | `Asset_update
      | `Asset_delete
      | `Asset_statistics
      | `Asset_share
      | `Asset_view
      | `Asset_download
      | `Asset_upload
      | `Asset_replace
      | `Asset_copy
      | `Asset_derive
      | `Asset_edit_get
      | `Asset_edit_create
      | `Asset_edit_delete
      | `Album_create
      | `Album_read
      | `Album_update
      | `Album_delete
      | `Album_statistics
      | `Album_share
      | `Album_download
      | `Album_asset_create
      | `Album_asset_delete
      | `Album_user_create
      | `Album_user_update
      | `Album_user_delete
      | `Auth_change_password
      | `Auth_device_delete
      | `Archive_read
      | `Backup_list
      | `Backup_download
      | `Backup_upload
      | `Backup_delete
      | `Duplicate_read
      | `Duplicate_delete
      | `Face_create
      | `Face_read
      | `Face_update
      | `Face_delete
      | `Folder_read
      | `Job_create
      | `Job_read
      | `Library_create
      | `Library_read
      | `Library_update
      | `Library_delete
      | `Library_statistics
      | `Timeline_read
      | `Timeline_download
      | `Maintenance
      | `Map_read
      | `Map_search
      | `Memory_create
      | `Memory_read
      | `Memory_update
      | `Memory_delete
      | `Memory_statistics
      | `Memory_asset_create
      | `Memory_asset_delete
      | `Notification_create
      | `Notification_read
      | `Notification_update
      | `Notification_delete
      | `Partner_create
      | `Partner_read
      | `Partner_update
      | `Partner_delete
      | `Person_create
      | `Person_read
      | `Person_update
      | `Person_delete
      | `Person_statistics
      | `Person_merge
      | `Person_reassign
      | `Pin_code_create
      | `Pin_code_update
      | `Pin_code_delete
      | `Plugin_create
      | `Plugin_read
      | `Plugin_update
      | `Plugin_delete
      | `Server_about
      | `Server_apk_links
      | `Server_storage
      | `Server_statistics
      | `Server_version_check
      | `Server_license_read
      | `Server_license_update
      | `Server_license_delete
      | `Session_create
      | `Session_read
      | `Session_update
      | `Session_delete
      | `Session_lock
      | `Shared_link_create
      | `Shared_link_read
      | `Shared_link_update
      | `Shared_link_delete
      | `Stack_create
      | `Stack_read
      | `Stack_update
      | `Stack_delete
      | `Sync_stream
      | `Sync_checkpoint_read
      | `Sync_checkpoint_update
      | `Sync_checkpoint_delete
      | `System_config_read
      | `System_config_update
      | `System_metadata_read
      | `System_metadata_update
      | `Tag_create
      | `Tag_read
      | `Tag_update
      | `Tag_delete
      | `Tag_asset
      | `User_read
      | `User_update
      | `User_license_create
      | `User_license_read
      | `User_license_update
      | `User_license_delete
      | `User_onboarding_read
      | `User_onboarding_update
      | `User_onboarding_delete
      | `User_preference_read
      | `User_preference_update
      | `User_profile_image_create
      | `User_profile_image_read
      | `User_profile_image_update
      | `User_profile_image_delete
      | `Queue_read
      | `Queue_update
      | `Queue_job_create
      | `Queue_job_read
      | `Queue_job_update
      | `Queue_job_delete
      | `Workflow_create
      | `Workflow_read
      | `Workflow_update
      | `Workflow_delete
      | `Admin_user_create
      | `Admin_user_read
      | `Admin_user_update
      | `Admin_user_delete
      | `Admin_session_read
      | `Admin_auth_unlink_all
    ]
    
    val jsont : t Jsont.t
  end
end

module Apikey : sig
  module UpdateDto : sig
    type t
    
    (** Construct a value *)
    val v : ?name:string -> ?permissions:Permission.T.t list -> unit -> t
    
    val name : t -> string option
    
    val permissions : t -> Permission.T.t list option
    
    val jsont : t Jsont.t
  end
  
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : created_at:Ptime.t -> id:string -> name:string -> permissions:Permission.T.t list -> updated_at:Ptime.t -> unit -> t
    
    val created_at : t -> Ptime.t
    
    val id : t -> string
    
    val name : t -> string
    
    val permissions : t -> Permission.T.t list
    
    val updated_at : t -> Ptime.t
    
    val jsont : t Jsont.t
  end
  
  module CreateDto : sig
    type t
    
    (** Construct a value *)
    val v : permissions:Permission.T.t list -> ?name:string -> unit -> t
    
    val name : t -> string option
    
    val permissions : t -> Permission.T.t list
    
    val jsont : t Jsont.t
  end
  
  (** List all API keys
  
      Retrieve all API keys of the current user. *)
  val get_api_keys : t -> unit -> ResponseDto.t
  
  (** Retrieve the current API key
  
      Retrieve the API key that is used to access this endpoint. *)
  val get_my_api_key : t -> unit -> ResponseDto.t
  
  (** Retrieve an API key
  
      Retrieve an API key by its ID. The current user must own this API key. *)
  val get_api_key : id:string -> t -> unit -> ResponseDto.t
  
  (** Update an API key
  
      Updates the name and permissions of an API key by its ID. The current user must own this API key. *)
  val update_api_key : id:string -> body:UpdateDto.t -> t -> unit -> ResponseDto.t
end

module ApikeyCreate : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : api_key:Apikey.ResponseDto.t -> secret:string -> unit -> t
    
    val api_key : t -> Apikey.ResponseDto.t
    
    val secret : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Create an API key
  
      Creates a new API key. It will be limited to the permissions specified. *)
  val create_api_key : body:Apikey.CreateDto.t -> t -> unit -> ResponseDto.t
end

module PeopleUpdate : sig
  module Item : sig
    type t
    
    (** Construct a value
        @param id Person id.
        @param birth_date Person date of birth.
    Note: the mobile app cannot currently set the birth date to null.
        @param feature_face_asset_id Asset is used to get the feature face thumbnail.
        @param is_hidden Person visibility
        @param name Person name.
    *)
    val v : id:string -> ?birth_date:string -> ?color:string -> ?feature_face_asset_id:string -> ?is_favorite:bool -> ?is_hidden:bool -> ?name:string -> unit -> t
    
    (** Person date of birth.
    Note: the mobile app cannot currently set the birth date to null. *)
    val birth_date : t -> string option
    
    val color : t -> string option
    
    (** Asset is used to get the feature face thumbnail. *)
    val feature_face_asset_id : t -> string option
    
    (** Person id. *)
    val id : t -> string
    
    val is_favorite : t -> bool option
    
    (** Person visibility *)
    val is_hidden : t -> bool option
    
    (** Person name. *)
    val name : t -> string option
    
    val jsont : t Jsont.t
  end
end

module PartnerDirection : sig
  module T : sig
    type t = [
      | `Shared_by
      | `Shared_with
    ]
    
    val jsont : t Jsont.t
  end
end

module Onboarding : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : is_onboarded:bool -> unit -> t
    
    val is_onboarded : t -> bool
    
    val jsont : t Jsont.t
  end
  
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : is_onboarded:bool -> unit -> t
    
    val is_onboarded : t -> bool
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve user onboarding
  
      Retrieve the onboarding status of the current user. *)
  val get_user_onboarding : t -> unit -> ResponseDto.t
  
  (** Update user onboarding
  
      Update the onboarding status of the current user. *)
  val set_user_onboarding : body:Dto.t -> t -> unit -> ResponseDto.t
end

module OnThisDay : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : year:float -> unit -> t
    
    val year : t -> float
    
    val jsont : t Jsont.t
  end
end

module Ocr : sig
  module Config : sig
    type t
    
    (** Construct a value *)
    val v : enabled:bool -> max_resolution:int -> min_detection_score:float -> min_recognition_score:float -> model_name:string -> unit -> t
    
    val enabled : t -> bool
    
    val max_resolution : t -> int
    
    val min_detection_score : t -> float
    
    val min_recognition_score : t -> float
    
    val model_name : t -> string
    
    val jsont : t Jsont.t
  end
end

module OauthTokenEndpointAuthMethod : sig
  module T : sig
    type t = [
      | `Client_secret_post
      | `Client_secret_basic
    ]
    
    val jsont : t Jsont.t
  end
end

module SystemConfigOauth : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : auto_launch:bool -> auto_register:bool -> button_text:string -> client_id:string -> client_secret:string -> enabled:bool -> issuer_url:string -> mobile_override_enabled:bool -> mobile_redirect_uri:string -> profile_signing_algorithm:string -> role_claim:string -> scope:string -> signing_algorithm:string -> storage_label_claim:string -> storage_quota_claim:string -> timeout:int -> token_endpoint_auth_method:OauthTokenEndpointAuthMethod.T.t -> ?default_storage_quota:int64 -> unit -> t
    
    val auto_launch : t -> bool
    
    val auto_register : t -> bool
    
    val button_text : t -> string
    
    val client_id : t -> string
    
    val client_secret : t -> string
    
    val default_storage_quota : t -> int64 option
    
    val enabled : t -> bool
    
    val issuer_url : t -> string
    
    val mobile_override_enabled : t -> bool
    
    val mobile_redirect_uri : t -> string
    
    val profile_signing_algorithm : t -> string
    
    val role_claim : t -> string
    
    val scope : t -> string
    
    val signing_algorithm : t -> string
    
    val storage_label_claim : t -> string
    
    val storage_quota_claim : t -> string
    
    val timeout : t -> int
    
    val token_endpoint_auth_method : t -> OauthTokenEndpointAuthMethod.T.t
    
    val jsont : t Jsont.t
  end
end

module OauthConfig : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : redirect_uri:string -> ?code_challenge:string -> ?state:string -> unit -> t
    
    val code_challenge : t -> string option
    
    val redirect_uri : t -> string
    
    val state : t -> string option
    
    val jsont : t Jsont.t
  end
end

module OauthAuthorize : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : url:string -> unit -> t
    
    val url : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Start OAuth
  
      Initiate the OAuth authorization process. *)
  val start_oauth : body:OauthConfig.Dto.t -> t -> unit -> ResponseDto.t
end

module OauthCallback : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : url:string -> ?code_verifier:string -> ?state:string -> unit -> t
    
    val code_verifier : t -> string option
    
    val state : t -> string option
    
    val url : t -> string
    
    val jsont : t Jsont.t
  end
end

module NotificationUpdateAll : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : ids:string list -> ?read_at:Ptime.t -> unit -> t
    
    val ids : t -> string list
    
    val read_at : t -> Ptime.t option
    
    val jsont : t Jsont.t
  end
end

module NotificationLevel : sig
  module T : sig
    type t = [
      | `Success
      | `Error
      | `Warning
      | `Info
    ]
    
    val jsont : t Jsont.t
  end
end

module Notification : sig
  module UpdateDto : sig
    type t
    
    (** Construct a value *)
    val v : ?read_at:Ptime.t -> unit -> t
    
    val read_at : t -> Ptime.t option
    
    val jsont : t Jsont.t
  end
  
  module Type : sig
    type t = [
      | `Job_failed
      | `Backup_failed
      | `System_message
      | `Album_invite
      | `Album_update
      | `Custom
    ]
    
    val jsont : t Jsont.t
  end
  
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : created_at:Ptime.t -> id:string -> level:NotificationLevel.T.t -> title:string -> type_:Type.t -> ?data:Jsont.json -> ?description:string -> ?read_at:Ptime.t -> unit -> t
    
    val created_at : t -> Ptime.t
    
    val data : t -> Jsont.json option
    
    val description : t -> string option
    
    val id : t -> string
    
    val level : t -> NotificationLevel.T.t
    
    val read_at : t -> Ptime.t option
    
    val title : t -> string
    
    val type_ : t -> Type.t
    
    val jsont : t Jsont.t
  end
  
  module CreateDto : sig
    type t
    
    (** Construct a value *)
    val v : title:string -> user_id:string -> ?data:Jsont.json -> ?description:string -> ?level:NotificationLevel.T.t -> ?read_at:Ptime.t -> ?type_:Type.t -> unit -> t
    
    val data : t -> Jsont.json option
    
    val description : t -> string option
    
    val level : t -> NotificationLevel.T.t option
    
    val read_at : t -> Ptime.t option
    
    val title : t -> string
    
    val type_ : t -> Type.t option
    
    val user_id : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Create a notification
  
      Create a new notification for a specific user. *)
  val create_notification : body:CreateDto.t -> t -> unit -> Dto.t
  
  (** Retrieve notifications
  
      Retrieve a list of notifications. *)
  val get_notifications : ?id:string -> ?level:string -> ?type_:string -> ?unread:string -> t -> unit -> Dto.t
  
  (** Get a notification
  
      Retrieve a specific notification identified by id. *)
  val get_notification : id:string -> t -> unit -> Dto.t
  
  (** Update a notification
  
      Update a specific notification to set its read status. *)
  val update_notification : id:string -> body:UpdateDto.t -> t -> unit -> Dto.t
end

module NotificationDeleteAll : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : ids:string list -> unit -> t
    
    val ids : t -> string list
    
    val jsont : t Jsont.t
  end
end

module MirrorAxis : sig
  module T : sig
    (** Axis to mirror along *)
    type t = [
      | `Horizontal
      | `Vertical
    ]
    
    val jsont : t Jsont.t
  end
end

module MirrorParameters : sig
  module T : sig
    type t
    
    (** Construct a value
        @param axis Axis to mirror along
    *)
    val v : axis:MirrorAxis.T.t -> unit -> t
    
    (** Axis to mirror along *)
    val axis : t -> MirrorAxis.T.t
    
    val jsont : t Jsont.t
  end
end

module MergePerson : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : ids:string list -> unit -> t
    
    val ids : t -> string list
    
    val jsont : t Jsont.t
  end
end

module MemoryStatistics : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : total:int -> unit -> t
    
    val total : t -> int
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve memories statistics
  
      Retrieve statistics about memories, such as total count and other relevant metrics. 
      @param size Number of memories to return
  *)
  val memories_statistics : ?for_:string -> ?is_saved:string -> ?is_trashed:string -> ?order:string -> ?size:string -> ?type_:string -> t -> unit -> ResponseDto.t
end

module MemorySearchOrder : sig
  module T : sig
    type t = [
      | `Asc
      | `Desc
      | `Random
    ]
    
    val jsont : t Jsont.t
  end
end

module Memories : sig
  module Update : sig
    type t
    
    (** Construct a value *)
    val v : ?duration:int -> ?enabled:bool -> unit -> t
    
    val duration : t -> int option
    
    val enabled : t -> bool option
    
    val jsont : t Jsont.t
  end
  
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : ?duration:int -> ?enabled:bool -> unit -> t
    
    val duration : t -> int
    
    val enabled : t -> bool
    
    val jsont : t Jsont.t
  end
end

module MapReverseGeocode : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : ?city:string -> ?country:string -> ?state:string -> unit -> t
    
    val city : t -> string option
    
    val country : t -> string option
    
    val state : t -> string option
    
    val jsont : t Jsont.t
  end
  
  (** Reverse geocode coordinates
  
      Retrieve location information (e.g., city, country) for given latitude and longitude coordinates. *)
  val reverse_geocode : lat:string -> lon:string -> t -> unit -> ResponseDto.t
end

module MapMarker : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : id:string -> lat:float -> lon:float -> ?city:string -> ?country:string -> ?state:string -> unit -> t
    
    val city : t -> string option
    
    val country : t -> string option
    
    val id : t -> string
    
    val lat : t -> float
    
    val lon : t -> float
    
    val state : t -> string option
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve map markers
  
      Retrieve a list of latitude and longitude coordinates for every asset with location data. *)
  val get_map_markers : ?file_created_after:string -> ?file_created_before:string -> ?is_archived:string -> ?is_favorite:string -> ?with_partners:string -> ?with_shared_albums:string -> t -> unit -> ResponseDto.t
end

module ManualJobName : sig
  module T : sig
    type t = [
      | `Person_cleanup
      | `Tag_cleanup
      | `User_cleanup
      | `Memory_cleanup
      | `Memory_create
      | `Backup_database
    ]
    
    val jsont : t Jsont.t
  end
end

module Job : sig
  module CreateDto : sig
    type t
    
    (** Construct a value *)
    val v : name:ManualJobName.T.t -> unit -> t
    
    val name : t -> ManualJobName.T.t
    
    val jsont : t Jsont.t
  end
end

module MaintenanceLogin : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : ?token:string -> unit -> t
    
    val token : t -> string option
    
    val jsont : t Jsont.t
  end
end

module MaintenanceAuth : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : username:string -> unit -> t
    
    val username : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Log into maintenance mode
  
      Login with maintenance token or cookie to receive current information and perform further actions. *)
  val maintenance_login : body:MaintenanceLogin.Dto.t -> t -> unit -> Dto.t
end

module MaintenanceAction : sig
  module T : sig
    type t = [
      | `Start
      | `End_
      | `Select_database_restore
      | `Restore_database
    ]
    
    val jsont : t Jsont.t
  end
end

module SetMaintenanceMode : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : action:MaintenanceAction.T.t -> ?restore_backup_filename:string -> unit -> t
    
    val action : t -> MaintenanceAction.T.t
    
    val restore_backup_filename : t -> string option
    
    val jsont : t Jsont.t
  end
end

module MaintenanceStatus : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : action:MaintenanceAction.T.t -> active:bool -> ?error:string -> ?progress:float -> ?task:string -> unit -> t
    
    val action : t -> MaintenanceAction.T.t
    
    val active : t -> bool
    
    val error : t -> string option
    
    val progress : t -> float option
    
    val task : t -> string option
    
    val jsont : t Jsont.t
  end
  
  (** Get maintenance mode status
  
      Fetch information about the currently running maintenance action. *)
  val get_maintenance_status : t -> unit -> ResponseDto.t
end

module MachineLearningAvailabilityChecks : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : enabled:bool -> interval:float -> timeout:float -> unit -> t
    
    val enabled : t -> bool
    
    val interval : t -> float
    
    val timeout : t -> float
    
    val jsont : t Jsont.t
  end
end

module Logout : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : redirect_uri:string -> successful:bool -> unit -> t
    
    val redirect_uri : t -> string
    
    val successful : t -> bool
    
    val jsont : t Jsont.t
  end
  
  (** Logout
  
      Logout the current user and invalidate the session token. *)
  val logout : t -> unit -> ResponseDto.t
end

module LoginCredential : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : email:string -> password:string -> unit -> t
    
    val email : t -> string
    
    val password : t -> string
    
    val jsont : t Jsont.t
  end
end

module Login : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : access_token:string -> is_admin:bool -> is_onboarded:bool -> name:string -> profile_image_path:string -> should_change_password:bool -> user_email:string -> user_id:string -> unit -> t
    
    val access_token : t -> string
    
    val is_admin : t -> bool
    
    val is_onboarded : t -> bool
    
    val name : t -> string
    
    val profile_image_path : t -> string
    
    val should_change_password : t -> bool
    
    val user_email : t -> string
    
    val user_id : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Login
  
      Login with username and password and receive a session token. *)
  val login : body:LoginCredential.Dto.t -> t -> unit -> ResponseDto.t
  
  (** Finish OAuth
  
      Complete the OAuth authorization process by exchanging the authorization code for a session token. *)
  val finish_oauth : body:OauthCallback.Dto.t -> t -> unit -> ResponseDto.t
end

module LogLevel : sig
  module T : sig
    type t = [
      | `Verbose
      | `Debug
      | `Log
      | `Warn
      | `Error
      | `Fatal
    ]
    
    val jsont : t Jsont.t
  end
end

module SystemConfigLogging : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : enabled:bool -> level:LogLevel.T.t -> unit -> t
    
    val enabled : t -> bool
    
    val level : t -> LogLevel.T.t
    
    val jsont : t Jsont.t
  end
end

module LicenseKey : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : activation_key:string -> license_key:string -> unit -> t
    
    val activation_key : t -> string
    
    val license_key : t -> string
    
    val jsont : t Jsont.t
  end
end

module License : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : activated_at:Ptime.t -> activation_key:string -> license_key:string -> unit -> t
    
    val activated_at : t -> Ptime.t
    
    val activation_key : t -> string
    
    val license_key : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Get product key
  
      Retrieve information about whether the server currently has a product key registered. *)
  val get_server_license : t -> unit -> ResponseDto.t
  
  (** Set server product key
  
      Validate and set the server product key if successful. *)
  val set_server_license : body:LicenseKey.Dto.t -> t -> unit -> ResponseDto.t
  
  (** Retrieve user product key
  
      Retrieve information about whether the current user has a registered product key. *)
  val get_user_license : t -> unit -> ResponseDto.t
  
  (** Set user product key
  
      Register a product key for the current user. *)
  val set_user_license : body:LicenseKey.Dto.t -> t -> unit -> ResponseDto.t
end

module LibraryStats : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : ?photos:int -> ?total:int -> ?usage:int64 -> ?videos:int -> unit -> t
    
    val photos : t -> int
    
    val total : t -> int
    
    val usage : t -> int64
    
    val videos : t -> int
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve library statistics
  
      Retrieve statistics for a specific external library, including number of videos, images, and storage usage. *)
  val get_library_statistics : id:string -> t -> unit -> ResponseDto.t
end

module JobSettings : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : concurrency:int -> unit -> t
    
    val concurrency : t -> int
    
    val jsont : t Jsont.t
  end
end

module SystemConfigJob : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : background_task:JobSettings.Dto.t -> editor:JobSettings.Dto.t -> face_detection:JobSettings.Dto.t -> library:JobSettings.Dto.t -> metadata_extraction:JobSettings.Dto.t -> migration:JobSettings.Dto.t -> notifications:JobSettings.Dto.t -> ocr:JobSettings.Dto.t -> search:JobSettings.Dto.t -> sidecar:JobSettings.Dto.t -> smart_search:JobSettings.Dto.t -> thumbnail_generation:JobSettings.Dto.t -> video_conversion:JobSettings.Dto.t -> workflow:JobSettings.Dto.t -> unit -> t
    
    val background_task : t -> JobSettings.Dto.t
    
    val editor : t -> JobSettings.Dto.t
    
    val face_detection : t -> JobSettings.Dto.t
    
    val library : t -> JobSettings.Dto.t
    
    val metadata_extraction : t -> JobSettings.Dto.t
    
    val migration : t -> JobSettings.Dto.t
    
    val notifications : t -> JobSettings.Dto.t
    
    val ocr : t -> JobSettings.Dto.t
    
    val search : t -> JobSettings.Dto.t
    
    val sidecar : t -> JobSettings.Dto.t
    
    val smart_search : t -> JobSettings.Dto.t
    
    val thumbnail_generation : t -> JobSettings.Dto.t
    
    val video_conversion : t -> JobSettings.Dto.t
    
    val workflow : t -> JobSettings.Dto.t
    
    val jsont : t Jsont.t
  end
end

module JobName : sig
  module T : sig
    type t = [
      | `Asset_delete
      | `Asset_delete_check
      | `Asset_detect_faces_queue_all
      | `Asset_detect_faces
      | `Asset_detect_duplicates_queue_all
      | `Asset_detect_duplicates
      | `Asset_edit_thumbnail_generation
      | `Asset_encode_video_queue_all
      | `Asset_encode_video
      | `Asset_empty_trash
      | `Asset_extract_metadata_queue_all
      | `Asset_extract_metadata
      | `Asset_file_migration
      | `Asset_generate_thumbnails_queue_all
      | `Asset_generate_thumbnails
      | `Audit_log_cleanup
      | `Audit_table_cleanup
      | `Database_backup
      | `Facial_recognition_queue_all
      | `Facial_recognition
      | `File_delete
      | `File_migration_queue_all
      | `Library_delete_check
      | `Library_delete
      | `Library_remove_asset
      | `Library_scan_assets_queue_all
      | `Library_sync_assets
      | `Library_sync_files_queue_all
      | `Library_sync_files
      | `Library_scan_queue_all
      | `Memory_cleanup
      | `Memory_generate
      | `Notifications_cleanup
      | `Notify_user_signup
      | `Notify_album_invite
      | `Notify_album_update
      | `User_delete
      | `User_delete_check
      | `User_sync_usage
      | `Person_cleanup
      | `Person_file_migration
      | `Person_generate_thumbnail
      | `Session_cleanup
      | `Send_mail
      | `Sidecar_queue_all
      | `Sidecar_check
      | `Sidecar_write
      | `Smart_search_queue_all
      | `Smart_search
      | `Storage_template_migration
      | `Storage_template_migration_single
      | `Tag_cleanup
      | `Version_check
      | `Ocr_queue_all
      | `Ocr
      | `Workflow_run
    ]
    
    val jsont : t Jsont.t
  end
end

module QueueJob : sig
  module Status : sig
    type t = [
      | `Active
      | `Failed
      | `Completed
      | `Delayed
      | `Waiting
      | `Paused
    ]
    
    val jsont : t Jsont.t
  end
  
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : data:Jsont.json -> name:JobName.T.t -> timestamp:int -> ?id:string -> unit -> t
    
    val data : t -> Jsont.json
    
    val id : t -> string option
    
    val name : t -> JobName.T.t
    
    val timestamp : t -> int
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve queue jobs
  
      Retrieves a list of queue jobs from the specified queue. *)
  val get_queue_jobs : name:string -> ?status:string -> t -> unit -> ResponseDto.t
end

module ImageFormat : sig
  module T : sig
    type t = [
      | `Jpeg
      | `Webp
    ]
    
    val jsont : t Jsont.t
  end
end

module SystemConfigGeneratedImage : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : format:ImageFormat.T.t -> quality:int -> size:int -> ?progressive:bool -> unit -> t
    
    val format : t -> ImageFormat.T.t
    
    val progressive : t -> bool
    
    val quality : t -> int
    
    val size : t -> int
    
    val jsont : t Jsont.t
  end
end

module SystemConfigGeneratedFullsizeImage : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : enabled:bool -> format:ImageFormat.T.t -> quality:int -> ?progressive:bool -> unit -> t
    
    val enabled : t -> bool
    
    val format : t -> ImageFormat.T.t
    
    val progressive : t -> bool
    
    val quality : t -> int
    
    val jsont : t Jsont.t
  end
end

module Folders : sig
  module Update : sig
    type t
    
    (** Construct a value *)
    val v : ?enabled:bool -> ?sidebar_web:bool -> unit -> t
    
    val enabled : t -> bool option
    
    val sidebar_web : t -> bool option
    
    val jsont : t Jsont.t
  end
  
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : ?enabled:bool -> ?sidebar_web:bool -> unit -> t
    
    val enabled : t -> bool
    
    val sidebar_web : t -> bool
    
    val jsont : t Jsont.t
  end
end

module FacialRecognition : sig
  module Config : sig
    type t
    
    (** Construct a value *)
    val v : enabled:bool -> max_distance:float -> min_faces:int -> min_score:float -> model_name:string -> unit -> t
    
    val enabled : t -> bool
    
    val max_distance : t -> float
    
    val min_faces : t -> int
    
    val min_score : t -> float
    
    val model_name : t -> string
    
    val jsont : t Jsont.t
  end
end

module Face : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : id:string -> unit -> t
    
    val id : t -> string
    
    val jsont : t Jsont.t
  end
end

module Exif : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : ?city:string option -> ?country:string option -> ?date_time_original:Ptime.t option -> ?description:string option -> ?exif_image_height:float option -> ?exif_image_width:float option -> ?exposure_time:string option -> ?f_number:float option -> ?file_size_in_byte:int64 option -> ?focal_length:float option -> ?iso:float option -> ?latitude:float option -> ?lens_model:string option -> ?longitude:float option -> ?make:string option -> ?model:string option -> ?modify_date:Ptime.t option -> ?orientation:string option -> ?projection_type:string option -> ?rating:float option -> ?state:string option -> ?time_zone:string option -> unit -> t
    
    val city : t -> string option
    
    val country : t -> string option
    
    val date_time_original : t -> Ptime.t option
    
    val description : t -> string option
    
    val exif_image_height : t -> float option
    
    val exif_image_width : t -> float option
    
    val exposure_time : t -> string option
    
    val f_number : t -> float option
    
    val file_size_in_byte : t -> int64 option
    
    val focal_length : t -> float option
    
    val iso : t -> float option
    
    val latitude : t -> float option
    
    val lens_model : t -> string option
    
    val longitude : t -> float option
    
    val make : t -> string option
    
    val model : t -> string option
    
    val modify_date : t -> Ptime.t option
    
    val orientation : t -> string option
    
    val projection_type : t -> string option
    
    val rating : t -> float option
    
    val state : t -> string option
    
    val time_zone : t -> string option
    
    val jsont : t Jsont.t
  end
end

module EmailNotifications : sig
  module Update : sig
    type t
    
    (** Construct a value *)
    val v : ?album_invite:bool -> ?album_update:bool -> ?enabled:bool -> unit -> t
    
    val album_invite : t -> bool option
    
    val album_update : t -> bool option
    
    val enabled : t -> bool option
    
    val jsont : t Jsont.t
  end
  
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : album_invite:bool -> album_update:bool -> enabled:bool -> unit -> t
    
    val album_invite : t -> bool
    
    val album_update : t -> bool
    
    val enabled : t -> bool
    
    val jsont : t Jsont.t
  end
end

module DuplicateDetection : sig
  module Config : sig
    type t
    
    (** Construct a value *)
    val v : enabled:bool -> max_distance:float -> unit -> t
    
    val enabled : t -> bool
    
    val max_distance : t -> float
    
    val jsont : t Jsont.t
  end
end

module DownloadInfo : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : ?album_id:string -> ?archive_size:int -> ?asset_ids:string list -> ?user_id:string -> unit -> t
    
    val album_id : t -> string option
    
    val archive_size : t -> int option
    
    val asset_ids : t -> string list option
    
    val user_id : t -> string option
    
    val jsont : t Jsont.t
  end
end

module DownloadArchive : sig
  module Info : sig
    type t
    
    (** Construct a value *)
    val v : asset_ids:string list -> size:int -> unit -> t
    
    val asset_ids : t -> string list
    
    val size : t -> int
    
    val jsont : t Jsont.t
  end
end

module Download : sig
  module Update : sig
    type t
    
    (** Construct a value *)
    val v : ?archive_size:int -> ?include_embedded_videos:bool -> unit -> t
    
    val archive_size : t -> int option
    
    val include_embedded_videos : t -> bool option
    
    val jsont : t Jsont.t
  end
  
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : archives:DownloadArchive.Info.t list -> total_size:int -> unit -> t
    
    val archives : t -> DownloadArchive.Info.t list
    
    val total_size : t -> int
    
    val jsont : t Jsont.t
  end
  
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : archive_size:int -> ?include_embedded_videos:bool -> unit -> t
    
    val archive_size : t -> int
    
    val include_embedded_videos : t -> bool
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve download information
  
      Retrieve information about how to request a download for the specified assets or album. The response includes groups of assets that can be downloaded together. *)
  val get_download_info : ?key:string -> ?slug:string -> body:DownloadInfo.Dto.t -> t -> unit -> ResponseDto.t
end

module DatabaseBackupUpload : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : ?file:string -> unit -> t
    
    val file : t -> string option
    
    val jsont : t Jsont.t
  end
end

module DatabaseBackupDelete : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : backups:string list -> unit -> t
    
    val backups : t -> string list
    
    val jsont : t Jsont.t
  end
end

module DatabaseBackup : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : filename:string -> filesize:float -> unit -> t
    
    val filename : t -> string
    
    val filesize : t -> float
    
    val jsont : t Jsont.t
  end
  
  module Config : sig
    type t
    
    (** Construct a value *)
    val v : cron_expression:string -> enabled:bool -> keep_last_amount:float -> unit -> t
    
    val cron_expression : t -> string
    
    val enabled : t -> bool
    
    val keep_last_amount : t -> float
    
    val jsont : t Jsont.t
  end
end

module SystemConfigBackups : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : database:DatabaseBackup.Config.t -> unit -> t
    
    val database : t -> DatabaseBackup.Config.t
    
    val jsont : t Jsont.t
  end
end

module DatabaseBackupList : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : backups:DatabaseBackup.Dto.t list -> unit -> t
    
    val backups : t -> DatabaseBackup.Dto.t list
    
    val jsont : t Jsont.t
  end
  
  (** List database backups
  
      Get the list of the successful and failed backups *)
  val list_database_backups : t -> unit -> ResponseDto.t
end

module CropParameters : sig
  module T : sig
    type t
    
    (** Construct a value
        @param height Height of the crop
        @param width Width of the crop
        @param x Top-Left X coordinate of crop
        @param y Top-Left Y coordinate of crop
    *)
    val v : height:float -> width:float -> x:float -> y:float -> unit -> t
    
    (** Height of the crop *)
    val height : t -> float
    
    (** Width of the crop *)
    val width : t -> float
    
    (** Top-Left X coordinate of crop *)
    val x : t -> float
    
    (** Top-Left Y coordinate of crop *)
    val y : t -> float
    
    val jsont : t Jsont.t
  end
end

module CreateProfileImage : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : profile_changed_at:Ptime.t -> profile_image_path:string -> user_id:string -> unit -> t
    
    val profile_changed_at : t -> Ptime.t
    
    val profile_image_path : t -> string
    
    val user_id : t -> string
    
    val jsont : t Jsont.t
  end
  
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : file:string -> unit -> t
    
    val file : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Create user profile image
  
      Upload and set a new profile image for the current user. *)
  val create_profile_image : t -> unit -> ResponseDto.t
end

module CreateLibrary : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : owner_id:string -> ?exclusion_patterns:string list -> ?import_paths:string list -> ?name:string -> unit -> t
    
    val exclusion_patterns : t -> string list option
    
    val import_paths : t -> string list option
    
    val name : t -> string option
    
    val owner_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module Library : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : asset_count:int -> created_at:Ptime.t -> exclusion_patterns:string list -> id:string -> import_paths:string list -> name:string -> owner_id:string -> updated_at:Ptime.t -> ?refreshed_at:Ptime.t -> unit -> t
    
    val asset_count : t -> int
    
    val created_at : t -> Ptime.t
    
    val exclusion_patterns : t -> string list
    
    val id : t -> string
    
    val import_paths : t -> string list
    
    val name : t -> string
    
    val owner_id : t -> string
    
    val refreshed_at : t -> Ptime.t option
    
    val updated_at : t -> Ptime.t
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve libraries
  
      Retrieve a list of external libraries. *)
  val get_all_libraries : t -> unit -> ResponseDto.t
  
  (** Create a library
  
      Create a new external library. *)
  val create_library : body:CreateLibrary.Dto.t -> t -> unit -> ResponseDto.t
  
  (** Retrieve a library
  
      Retrieve an external library by its ID. *)
  val get_library : id:string -> t -> unit -> ResponseDto.t
  
  (** Update a library
  
      Update an existing external library. *)
  val update_library : id:string -> body:UpdateLibrary.Dto.t -> t -> unit -> ResponseDto.t
end

module Cqmode : sig
  module T : sig
    type t = [
      | `Auto
      | `Cqp
      | `Icq
    ]
    
    val jsont : t Jsont.t
  end
end

module ContributorCount : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : asset_count:int -> user_id:string -> unit -> t
    
    val asset_count : t -> int
    
    val user_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module Colorspace : sig
  module T : sig
    type t = [
      | `Srgb
      | `P3
    ]
    
    val jsont : t Jsont.t
  end
end

module SystemConfigImage : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : colorspace:Colorspace.T.t -> extract_embedded:bool -> fullsize:SystemConfigGeneratedFullsizeImage.Dto.t -> preview:SystemConfigGeneratedImage.Dto.t -> thumbnail:SystemConfigGeneratedImage.Dto.t -> unit -> t
    
    val colorspace : t -> Colorspace.T.t
    
    val extract_embedded : t -> bool
    
    val fullsize : t -> SystemConfigGeneratedFullsizeImage.Dto.t
    
    val preview : t -> SystemConfigGeneratedImage.Dto.t
    
    val thumbnail : t -> SystemConfigGeneratedImage.Dto.t
    
    val jsont : t Jsont.t
  end
end

module Clip : sig
  module Config : sig
    type t
    
    (** Construct a value *)
    val v : enabled:bool -> model_name:string -> unit -> t
    
    val enabled : t -> bool
    
    val model_name : t -> string
    
    val jsont : t Jsont.t
  end
end

module SystemConfigMachineLearning : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : availability_checks:MachineLearningAvailabilityChecks.Dto.t -> clip:Clip.Config.t -> duplicate_detection:DuplicateDetection.Config.t -> enabled:bool -> facial_recognition:FacialRecognition.Config.t -> ocr:Ocr.Config.t -> urls:string list -> unit -> t
    
    val availability_checks : t -> MachineLearningAvailabilityChecks.Dto.t
    
    val clip : t -> Clip.Config.t
    
    val duplicate_detection : t -> DuplicateDetection.Config.t
    
    val enabled : t -> bool
    
    val facial_recognition : t -> FacialRecognition.Config.t
    
    val ocr : t -> Ocr.Config.t
    
    val urls : t -> string list
    
    val jsont : t Jsont.t
  end
end

module CheckExistingAssets : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : existing_ids:string list -> unit -> t
    
    val existing_ids : t -> string list
    
    val jsont : t Jsont.t
  end
  
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : device_asset_ids:string list -> device_id:string -> unit -> t
    
    val device_asset_ids : t -> string list
    
    val device_id : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Check existing assets
  
      Checks if multiple assets exist on the server and returns all existing - used by background backup *)
  val check_existing_assets : body:Dto.t -> t -> unit -> ResponseDto.t
end

module ChangePassword : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : new_password:string -> password:string -> ?invalidate_sessions:bool -> unit -> t
    
    val invalidate_sessions : t -> bool
    
    val new_password : t -> string
    
    val password : t -> string
    
    val jsont : t Jsont.t
  end
end

module UserAdmin : sig
  module UpdateDto : sig
    type t
    
    (** Construct a value *)
    val v : ?avatar_color:UserAvatarColor.T.t -> ?email:string -> ?is_admin:bool -> ?name:string -> ?password:string -> ?pin_code:string -> ?quota_size_in_bytes:int64 -> ?should_change_password:bool -> ?storage_label:string -> unit -> t
    
    val avatar_color : t -> UserAvatarColor.T.t option
    
    val email : t -> string option
    
    val is_admin : t -> bool option
    
    val name : t -> string option
    
    val password : t -> string option
    
    val pin_code : t -> string option
    
    val quota_size_in_bytes : t -> int64 option
    
    val should_change_password : t -> bool option
    
    val storage_label : t -> string option
    
    val jsont : t Jsont.t
  end
  
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : avatar_color:UserAvatarColor.T.t -> created_at:Ptime.t -> email:string -> id:string -> is_admin:bool -> license:UserLicense.T.t -> name:string -> oauth_id:string -> profile_changed_at:Ptime.t -> profile_image_path:string -> should_change_password:bool -> status:User.Status.t -> updated_at:Ptime.t -> ?deleted_at:Ptime.t -> ?quota_size_in_bytes:int64 -> ?quota_usage_in_bytes:int64 -> ?storage_label:string -> unit -> t
    
    val avatar_color : t -> UserAvatarColor.T.t
    
    val created_at : t -> Ptime.t
    
    val deleted_at : t -> Ptime.t option
    
    val email : t -> string
    
    val id : t -> string
    
    val is_admin : t -> bool
    
    val license : t -> UserLicense.T.t
    
    val name : t -> string
    
    val oauth_id : t -> string
    
    val profile_changed_at : t -> Ptime.t
    
    val profile_image_path : t -> string
    
    val quota_size_in_bytes : t -> int64 option
    
    val quota_usage_in_bytes : t -> int64 option
    
    val should_change_password : t -> bool
    
    val status : t -> User.Status.t
    
    val storage_label : t -> string option
    
    val updated_at : t -> Ptime.t
    
    val jsont : t Jsont.t
  end
  
  module CreateDto : sig
    type t
    
    (** Construct a value *)
    val v : email:string -> name:string -> password:string -> ?avatar_color:UserAvatarColor.T.t -> ?is_admin:bool -> ?notify:bool -> ?quota_size_in_bytes:int64 -> ?should_change_password:bool -> ?storage_label:string -> unit -> t
    
    val avatar_color : t -> UserAvatarColor.T.t option
    
    val email : t -> string
    
    val is_admin : t -> bool option
    
    val name : t -> string
    
    val notify : t -> bool option
    
    val password : t -> string
    
    val quota_size_in_bytes : t -> int64 option
    
    val should_change_password : t -> bool option
    
    val storage_label : t -> string option
    
    val jsont : t Jsont.t
  end
  
  (** Search users
  
      Search for users. *)
  val search_users_admin : ?id:string -> ?with_deleted:string -> t -> unit -> ResponseDto.t
  
  (** Create a user
  
      Create a new user. *)
  val create_user_admin : body:CreateDto.t -> t -> unit -> ResponseDto.t
  
  (** Retrieve a user
  
      Retrieve  a specific user by their ID. *)
  val get_user_admin : id:string -> t -> unit -> ResponseDto.t
  
  (** Update a user
  
      Update an existing user. *)
  val update_user_admin : id:string -> body:UpdateDto.t -> t -> unit -> ResponseDto.t
  
  (** Delete a user
  
      Delete a user. *)
  val delete_user_admin : id:string -> t -> unit -> ResponseDto.t
  
  (** Restore a deleted user
  
      Restore a previously deleted user. *)
  val restore_user_admin : id:string -> t -> unit -> ResponseDto.t
  
  (** Register admin
  
      Create the first admin user in the system. *)
  val sign_up_admin : body:SignUp.Dto.t -> t -> unit -> ResponseDto.t
  
  (** Change password
  
      Change the password of the current user. *)
  val change_password : body:ChangePassword.Dto.t -> t -> unit -> ResponseDto.t
  
  (** Link OAuth account
  
      Link an OAuth account to the authenticated user. *)
  val link_oauth_account : body:OauthCallback.Dto.t -> t -> unit -> ResponseDto.t
  
  (** Unlink OAuth account
  
      Unlink the OAuth account from the authenticated user. *)
  val unlink_oauth_account : t -> unit -> ResponseDto.t
  
  (** Get current user
  
      Retrieve information about the user making the API request. *)
  val get_my_user : t -> unit -> ResponseDto.t
  
  (** Update current user
  
      Update the current user making teh API request. *)
  val update_my_user : body:UserUpdateMe.Dto.t -> t -> unit -> ResponseDto.t
end

module Cast : sig
  module Update : sig
    type t
    
    (** Construct a value *)
    val v : ?g_cast_enabled:bool -> unit -> t
    
    val g_cast_enabled : t -> bool option
    
    val jsont : t Jsont.t
  end
  
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : ?g_cast_enabled:bool -> unit -> t
    
    val g_cast_enabled : t -> bool
    
    val jsont : t Jsont.t
  end
end

module BulkIds : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : ids:string list -> unit -> t
    
    val ids : t -> string list
    
    val jsont : t Jsont.t
  end
end

module Trash : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : count:int -> unit -> t
    
    val count : t -> int
    
    val jsont : t Jsont.t
  end
  
  (** Empty trash
  
      Permanently delete all items in the trash. *)
  val empty_trash : t -> unit -> ResponseDto.t
  
  (** Restore trash
  
      Restore all items in the trash. *)
  val restore_trash : t -> unit -> ResponseDto.t
  
  (** Restore assets
  
      Restore specific assets from the trash. *)
  val restore_assets : body:BulkIds.Dto.t -> t -> unit -> ResponseDto.t
end

module BulkIdErrorReason : sig
  module T : sig
    type t = [
      | `Duplicate
      | `No_permission
      | `Not_found
      | `Unknown
    ]
    
    val jsont : t Jsont.t
  end
end

module AlbumsAddAssets : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : success:bool -> ?error:BulkIdErrorReason.T.t -> unit -> t
    
    val error : t -> BulkIdErrorReason.T.t option
    
    val success : t -> bool
    
    val jsont : t Jsont.t
  end
  
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : album_ids:string list -> asset_ids:string list -> unit -> t
    
    val album_ids : t -> string list
    
    val asset_ids : t -> string list
    
    val jsont : t Jsont.t
  end
  
  (** Add assets to albums
  
      Send a list of asset IDs and album IDs to add each asset to each album. *)
  val add_assets_to_albums : ?key:string -> ?slug:string -> body:Dto.t -> t -> unit -> ResponseDto.t
end

module AuthStatus : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : is_elevated:bool -> password:bool -> pin_code:bool -> ?expires_at:string -> ?pin_expires_at:string -> unit -> t
    
    val expires_at : t -> string option
    
    val is_elevated : t -> bool
    
    val password : t -> bool
    
    val pin_code : t -> bool
    
    val pin_expires_at : t -> string option
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve auth status
  
      Get information about the current session, including whether the user has a password, and if the session can access locked assets. *)
  val get_auth_status : t -> unit -> ResponseDto.t
end

module AudioCodec : sig
  module T : sig
    type t = [
      | `Mp3
      | `Aac
      | `Libopus
      | `Pcm_s16le
    ]
    
    val jsont : t Jsont.t
  end
end

module SystemConfigFfmpeg : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : accel:TranscodeHwaccel.T.t -> accel_decode:bool -> accepted_audio_codecs:AudioCodec.T.t list -> accepted_containers:VideoContainer.T.t list -> accepted_video_codecs:VideoCodec.T.t list -> bframes:int -> cq_mode:Cqmode.T.t -> crf:int -> gop_size:int -> max_bitrate:string -> preferred_hw_device:string -> preset:string -> refs:int -> target_audio_codec:AudioCodec.T.t -> target_resolution:string -> target_video_codec:VideoCodec.T.t -> temporal_aq:bool -> threads:int -> tonemap:ToneMapping.T.t -> transcode:TranscodePolicy.T.t -> two_pass:bool -> unit -> t
    
    val accel : t -> TranscodeHwaccel.T.t
    
    val accel_decode : t -> bool
    
    val accepted_audio_codecs : t -> AudioCodec.T.t list
    
    val accepted_containers : t -> VideoContainer.T.t list
    
    val accepted_video_codecs : t -> VideoCodec.T.t list
    
    val bframes : t -> int
    
    val cq_mode : t -> Cqmode.T.t
    
    val crf : t -> int
    
    val gop_size : t -> int
    
    val max_bitrate : t -> string
    
    val preferred_hw_device : t -> string
    
    val preset : t -> string
    
    val refs : t -> int
    
    val target_audio_codec : t -> AudioCodec.T.t
    
    val target_resolution : t -> string
    
    val target_video_codec : t -> VideoCodec.T.t
    
    val temporal_aq : t -> bool
    
    val threads : t -> int
    
    val tonemap : t -> ToneMapping.T.t
    
    val transcode : t -> TranscodePolicy.T.t
    
    val two_pass : t -> bool
    
    val jsont : t Jsont.t
  end
end

module SystemConfig : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : backup:SystemConfigBackups.Dto.t -> ffmpeg:SystemConfigFfmpeg.Dto.t -> image:SystemConfigImage.Dto.t -> job:SystemConfigJob.Dto.t -> library:SystemConfigLibrary.Dto.t -> logging:SystemConfigLogging.Dto.t -> machine_learning:SystemConfigMachineLearning.Dto.t -> map:SystemConfigMap.Dto.t -> metadata:SystemConfigMetadata.Dto.t -> new_version_check:SystemConfigNewVersionCheck.Dto.t -> nightly_tasks:SystemConfigNightlyTasks.Dto.t -> notifications:SystemConfigNotifications.Dto.t -> oauth:SystemConfigOauth.Dto.t -> password_login:SystemConfigPasswordLogin.Dto.t -> reverse_geocoding:SystemConfigReverseGeocoding.Dto.t -> server:SystemConfigServer.Dto.t -> storage_template:SystemConfigStorageTemplate.Dto.t -> templates:SystemConfigTemplates.Dto.t -> theme:SystemConfigTheme.Dto.t -> trash:SystemConfigTrash.Dto.t -> user:SystemConfigUser.Dto.t -> unit -> t
    
    val backup : t -> SystemConfigBackups.Dto.t
    
    val ffmpeg : t -> SystemConfigFfmpeg.Dto.t
    
    val image : t -> SystemConfigImage.Dto.t
    
    val job : t -> SystemConfigJob.Dto.t
    
    val library : t -> SystemConfigLibrary.Dto.t
    
    val logging : t -> SystemConfigLogging.Dto.t
    
    val machine_learning : t -> SystemConfigMachineLearning.Dto.t
    
    val map : t -> SystemConfigMap.Dto.t
    
    val metadata : t -> SystemConfigMetadata.Dto.t
    
    val new_version_check : t -> SystemConfigNewVersionCheck.Dto.t
    
    val nightly_tasks : t -> SystemConfigNightlyTasks.Dto.t
    
    val notifications : t -> SystemConfigNotifications.Dto.t
    
    val oauth : t -> SystemConfigOauth.Dto.t
    
    val password_login : t -> SystemConfigPasswordLogin.Dto.t
    
    val reverse_geocoding : t -> SystemConfigReverseGeocoding.Dto.t
    
    val server : t -> SystemConfigServer.Dto.t
    
    val storage_template : t -> SystemConfigStorageTemplate.Dto.t
    
    val templates : t -> SystemConfigTemplates.Dto.t
    
    val theme : t -> SystemConfigTheme.Dto.t
    
    val trash : t -> SystemConfigTrash.Dto.t
    
    val user : t -> SystemConfigUser.Dto.t
    
    val jsont : t Jsont.t
  end
  
  (** Get system configuration
  
      Retrieve the current system configuration. *)
  val get_config : t -> unit -> Dto.t
  
  (** Update system configuration
  
      Update the system configuration with a new system configuration. *)
  val update_config : body:Dto.t -> t -> unit -> Dto.t
  
  (** Get system configuration defaults
  
      Retrieve the default values for the system configuration. *)
  val get_config_defaults : t -> unit -> Dto.t
end

module AssetVisibility : sig
  module T : sig
    type t = [
      | `Archive
      | `Timeline
      | `Hidden
      | `Locked
    ]
    
    val jsont : t Jsont.t
  end
end

module UpdateAsset : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : ?date_time_original:string -> ?description:string -> ?is_favorite:bool -> ?latitude:float -> ?live_photo_video_id:string -> ?longitude:float -> ?rating:float -> ?visibility:AssetVisibility.T.t -> unit -> t
    
    val date_time_original : t -> string option
    
    val description : t -> string option
    
    val is_favorite : t -> bool option
    
    val latitude : t -> float option
    
    val live_photo_video_id : t -> string option
    
    val longitude : t -> float option
    
    val rating : t -> float option
    
    val visibility : t -> AssetVisibility.T.t option
    
    val jsont : t Jsont.t
  end
end

module TimeBucketAsset : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value
        @param city Array of city names extracted from EXIF GPS data
        @param country Array of country names extracted from EXIF GPS data
        @param duration Array of video durations in HH:MM:SS format (null for images)
        @param file_created_at Array of file creation timestamps in UTC (ISO 8601 format, without timezone)
        @param id Array of asset IDs in the time bucket
        @param is_favorite Array indicating whether each asset is favorited
        @param is_image Array indicating whether each asset is an image (false for videos)
        @param is_trashed Array indicating whether each asset is in the trash
        @param live_photo_video_id Array of live photo video asset IDs (null for non-live photos)
        @param local_offset_hours Array of UTC offset hours at the time each photo was taken. Positive values are east of UTC, negative values are west of UTC. Values may be fractional (e.g., 5.5 for +05:30, -9.75 for -09:45). Applying this offset to 'fileCreatedAt' will give you the time the photo was taken from the photographer's perspective.
        @param owner_id Array of owner IDs for each asset
        @param projection_type Array of projection types for 360° content (e.g., "EQUIRECTANGULAR", "CUBEFACE", "CYLINDRICAL")
        @param ratio Array of aspect ratios (width/height) for each asset
        @param thumbhash Array of BlurHash strings for generating asset previews (base64 encoded)
        @param visibility Array of visibility statuses for each asset (e.g., ARCHIVE, TIMELINE, HIDDEN, LOCKED)
        @param latitude Array of latitude coordinates extracted from EXIF GPS data
        @param longitude Array of longitude coordinates extracted from EXIF GPS data
        @param stack Array of stack information as [stackId, assetCount] tuples (null for non-stacked assets)
    *)
    val v : city:string list -> country:string list -> duration:string list -> file_created_at:string list -> id:string list -> is_favorite:bool list -> is_image:bool list -> is_trashed:bool list -> live_photo_video_id:string list -> local_offset_hours:float list -> owner_id:string list -> projection_type:string list -> ratio:float list -> thumbhash:string list -> visibility:AssetVisibility.T.t list -> ?latitude:float list -> ?longitude:float list -> ?stack:string list list -> unit -> t
    
    (** Array of city names extracted from EXIF GPS data *)
    val city : t -> string list
    
    (** Array of country names extracted from EXIF GPS data *)
    val country : t -> string list
    
    (** Array of video durations in HH:MM:SS format (null for images) *)
    val duration : t -> string list
    
    (** Array of file creation timestamps in UTC (ISO 8601 format, without timezone) *)
    val file_created_at : t -> string list
    
    (** Array of asset IDs in the time bucket *)
    val id : t -> string list
    
    (** Array indicating whether each asset is favorited *)
    val is_favorite : t -> bool list
    
    (** Array indicating whether each asset is an image (false for videos) *)
    val is_image : t -> bool list
    
    (** Array indicating whether each asset is in the trash *)
    val is_trashed : t -> bool list
    
    (** Array of latitude coordinates extracted from EXIF GPS data *)
    val latitude : t -> float list option
    
    (** Array of live photo video asset IDs (null for non-live photos) *)
    val live_photo_video_id : t -> string list
    
    (** Array of UTC offset hours at the time each photo was taken. Positive values are east of UTC, negative values are west of UTC. Values may be fractional (e.g., 5.5 for +05:30, -9.75 for -09:45). Applying this offset to 'fileCreatedAt' will give you the time the photo was taken from the photographer's perspective. *)
    val local_offset_hours : t -> float list
    
    (** Array of longitude coordinates extracted from EXIF GPS data *)
    val longitude : t -> float list option
    
    (** Array of owner IDs for each asset *)
    val owner_id : t -> string list
    
    (** Array of projection types for 360° content (e.g., "EQUIRECTANGULAR", "CUBEFACE", "CYLINDRICAL") *)
    val projection_type : t -> string list
    
    (** Array of aspect ratios (width/height) for each asset *)
    val ratio : t -> float list
    
    (** Array of stack information as [stackId, assetCount] tuples (null for non-stacked assets) *)
    val stack : t -> string list list option
    
    (** Array of BlurHash strings for generating asset previews (base64 encoded) *)
    val thumbhash : t -> string list
    
    (** Array of visibility statuses for each asset (e.g., ARCHIVE, TIMELINE, HIDDEN, LOCKED) *)
    val visibility : t -> AssetVisibility.T.t list
    
    val jsont : t Jsont.t
  end
  
  (** Get time bucket
  
      Retrieve a string of all asset ids in a given time bucket. 
      @param album_id Filter assets belonging to a specific album
      @param is_favorite Filter by favorite status (true for favorites only, false for non-favorites only)
      @param is_trashed Filter by trash status (true for trashed assets only, false for non-trashed only)
      @param order Sort order for assets within time buckets (ASC for oldest first, DESC for newest first)
      @param person_id Filter assets containing a specific person (face recognition)
      @param tag_id Filter assets with a specific tag
      @param time_bucket Time bucket identifier in YYYY-MM-DD format (e.g., "2024-01-01" for January 2024)
      @param user_id Filter assets by specific user ID
      @param visibility Filter by asset visibility status (ARCHIVE, TIMELINE, HIDDEN, LOCKED)
      @param with_coordinates Include location data in the response
      @param with_partners Include assets shared by partners
      @param with_stacked Include stacked assets in the response. When true, only primary assets from stacks are returned.
  *)
  val get_time_bucket : ?album_id:string -> ?is_favorite:string -> ?is_trashed:string -> ?key:string -> ?order:string -> ?person_id:string -> ?slug:string -> ?tag_id:string -> time_bucket:string -> ?user_id:string -> ?visibility:string -> ?with_coordinates:string -> ?with_partners:string -> ?with_stacked:string -> t -> unit -> ResponseDto.t
end

module AssetBulk : sig
  module UpdateDto : sig
    type t
    
    (** Construct a value *)
    val v : ids:string list -> ?date_time_original:string -> ?date_time_relative:float -> ?description:string -> ?duplicate_id:string -> ?is_favorite:bool -> ?latitude:float -> ?longitude:float -> ?rating:float -> ?time_zone:string -> ?visibility:AssetVisibility.T.t -> unit -> t
    
    val date_time_original : t -> string option
    
    val date_time_relative : t -> float option
    
    val description : t -> string option
    
    val duplicate_id : t -> string option
    
    val ids : t -> string list
    
    val is_favorite : t -> bool option
    
    val latitude : t -> float option
    
    val longitude : t -> float option
    
    val rating : t -> float option
    
    val time_zone : t -> string option
    
    val visibility : t -> AssetVisibility.T.t option
    
    val jsont : t Jsont.t
  end
end

module AssetTypeEnum : sig
  module T : sig
    type t = [
      | `Image
      | `Video
      | `Audio
      | `Other
    ]
    
    val jsont : t Jsont.t
  end
end

module SyncAssetV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : checksum:string -> id:string -> is_edited:bool -> is_favorite:bool -> original_file_name:string -> owner_id:string -> type_:AssetTypeEnum.T.t -> visibility:AssetVisibility.T.t -> ?deleted_at:Ptime.t -> ?duration:string -> ?file_created_at:Ptime.t -> ?file_modified_at:Ptime.t -> ?height:int -> ?library_id:string -> ?live_photo_video_id:string -> ?local_date_time:Ptime.t -> ?stack_id:string -> ?thumbhash:string -> ?width:int -> unit -> t
    
    val checksum : t -> string
    
    val deleted_at : t -> Ptime.t option
    
    val duration : t -> string option
    
    val file_created_at : t -> Ptime.t option
    
    val file_modified_at : t -> Ptime.t option
    
    val height : t -> int option
    
    val id : t -> string
    
    val is_edited : t -> bool
    
    val is_favorite : t -> bool
    
    val library_id : t -> string option
    
    val live_photo_video_id : t -> string option
    
    val local_date_time : t -> Ptime.t option
    
    val original_file_name : t -> string
    
    val owner_id : t -> string
    
    val stack_id : t -> string option
    
    val thumbhash : t -> string option
    
    val type_ : t -> AssetTypeEnum.T.t
    
    val visibility : t -> AssetVisibility.T.t
    
    val width : t -> int option
    
    val jsont : t Jsont.t
  end
end

module StatisticsSearch : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : ?album_ids:string list -> ?city:string -> ?country:string -> ?created_after:Ptime.t -> ?created_before:Ptime.t -> ?description:string -> ?device_id:string -> ?is_encoded:bool -> ?is_favorite:bool -> ?is_motion:bool -> ?is_not_in_album:bool -> ?is_offline:bool -> ?lens_model:string -> ?library_id:string -> ?make:string -> ?model:string -> ?ocr:string -> ?person_ids:string list -> ?rating:float -> ?state:string -> ?tag_ids:string list -> ?taken_after:Ptime.t -> ?taken_before:Ptime.t -> ?trashed_after:Ptime.t -> ?trashed_before:Ptime.t -> ?type_:AssetTypeEnum.T.t -> ?updated_after:Ptime.t -> ?updated_before:Ptime.t -> ?visibility:AssetVisibility.T.t -> unit -> t
    
    val album_ids : t -> string list option
    
    val city : t -> string option
    
    val country : t -> string option
    
    val created_after : t -> Ptime.t option
    
    val created_before : t -> Ptime.t option
    
    val description : t -> string option
    
    val device_id : t -> string option
    
    val is_encoded : t -> bool option
    
    val is_favorite : t -> bool option
    
    val is_motion : t -> bool option
    
    val is_not_in_album : t -> bool option
    
    val is_offline : t -> bool option
    
    val lens_model : t -> string option
    
    val library_id : t -> string option
    
    val make : t -> string option
    
    val model : t -> string option
    
    val ocr : t -> string option
    
    val person_ids : t -> string list option
    
    val rating : t -> float option
    
    val state : t -> string option
    
    val tag_ids : t -> string list option
    
    val taken_after : t -> Ptime.t option
    
    val taken_before : t -> Ptime.t option
    
    val trashed_after : t -> Ptime.t option
    
    val trashed_before : t -> Ptime.t option
    
    val type_ : t -> AssetTypeEnum.T.t option
    
    val updated_after : t -> Ptime.t option
    
    val updated_before : t -> Ptime.t option
    
    val visibility : t -> AssetVisibility.T.t option
    
    val jsont : t Jsont.t
  end
end

module SearchStatistics : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : total:int -> unit -> t
    
    val total : t -> int
    
    val jsont : t Jsont.t
  end
  
  (** Search asset statistics
  
      Retrieve statistical data about assets based on search criteria, such as the total matching count. *)
  val search_asset_statistics : body:StatisticsSearch.Dto.t -> t -> unit -> ResponseDto.t
end

module SmartSearch : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : ?album_ids:string list -> ?city:string -> ?country:string -> ?created_after:Ptime.t -> ?created_before:Ptime.t -> ?device_id:string -> ?is_encoded:bool -> ?is_favorite:bool -> ?is_motion:bool -> ?is_not_in_album:bool -> ?is_offline:bool -> ?language:string -> ?lens_model:string -> ?library_id:string -> ?make:string -> ?model:string -> ?ocr:string -> ?page:float -> ?person_ids:string list -> ?query:string -> ?query_asset_id:string -> ?rating:float -> ?size:float -> ?state:string -> ?tag_ids:string list -> ?taken_after:Ptime.t -> ?taken_before:Ptime.t -> ?trashed_after:Ptime.t -> ?trashed_before:Ptime.t -> ?type_:AssetTypeEnum.T.t -> ?updated_after:Ptime.t -> ?updated_before:Ptime.t -> ?visibility:AssetVisibility.T.t -> ?with_deleted:bool -> ?with_exif:bool -> unit -> t
    
    val album_ids : t -> string list option
    
    val city : t -> string option
    
    val country : t -> string option
    
    val created_after : t -> Ptime.t option
    
    val created_before : t -> Ptime.t option
    
    val device_id : t -> string option
    
    val is_encoded : t -> bool option
    
    val is_favorite : t -> bool option
    
    val is_motion : t -> bool option
    
    val is_not_in_album : t -> bool option
    
    val is_offline : t -> bool option
    
    val language : t -> string option
    
    val lens_model : t -> string option
    
    val library_id : t -> string option
    
    val make : t -> string option
    
    val model : t -> string option
    
    val ocr : t -> string option
    
    val page : t -> float option
    
    val person_ids : t -> string list option
    
    val query : t -> string option
    
    val query_asset_id : t -> string option
    
    val rating : t -> float option
    
    val size : t -> float option
    
    val state : t -> string option
    
    val tag_ids : t -> string list option
    
    val taken_after : t -> Ptime.t option
    
    val taken_before : t -> Ptime.t option
    
    val trashed_after : t -> Ptime.t option
    
    val trashed_before : t -> Ptime.t option
    
    val type_ : t -> AssetTypeEnum.T.t option
    
    val updated_after : t -> Ptime.t option
    
    val updated_before : t -> Ptime.t option
    
    val visibility : t -> AssetVisibility.T.t option
    
    val with_deleted : t -> bool option
    
    val with_exif : t -> bool option
    
    val jsont : t Jsont.t
  end
end

module RandomSearch : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : ?album_ids:string list -> ?city:string -> ?country:string -> ?created_after:Ptime.t -> ?created_before:Ptime.t -> ?device_id:string -> ?is_encoded:bool -> ?is_favorite:bool -> ?is_motion:bool -> ?is_not_in_album:bool -> ?is_offline:bool -> ?lens_model:string -> ?library_id:string -> ?make:string -> ?model:string -> ?ocr:string -> ?person_ids:string list -> ?rating:float -> ?size:float -> ?state:string -> ?tag_ids:string list -> ?taken_after:Ptime.t -> ?taken_before:Ptime.t -> ?trashed_after:Ptime.t -> ?trashed_before:Ptime.t -> ?type_:AssetTypeEnum.T.t -> ?updated_after:Ptime.t -> ?updated_before:Ptime.t -> ?visibility:AssetVisibility.T.t -> ?with_deleted:bool -> ?with_exif:bool -> ?with_people:bool -> ?with_stacked:bool -> unit -> t
    
    val album_ids : t -> string list option
    
    val city : t -> string option
    
    val country : t -> string option
    
    val created_after : t -> Ptime.t option
    
    val created_before : t -> Ptime.t option
    
    val device_id : t -> string option
    
    val is_encoded : t -> bool option
    
    val is_favorite : t -> bool option
    
    val is_motion : t -> bool option
    
    val is_not_in_album : t -> bool option
    
    val is_offline : t -> bool option
    
    val lens_model : t -> string option
    
    val library_id : t -> string option
    
    val make : t -> string option
    
    val model : t -> string option
    
    val ocr : t -> string option
    
    val person_ids : t -> string list option
    
    val rating : t -> float option
    
    val size : t -> float option
    
    val state : t -> string option
    
    val tag_ids : t -> string list option
    
    val taken_after : t -> Ptime.t option
    
    val taken_before : t -> Ptime.t option
    
    val trashed_after : t -> Ptime.t option
    
    val trashed_before : t -> Ptime.t option
    
    val type_ : t -> AssetTypeEnum.T.t option
    
    val updated_after : t -> Ptime.t option
    
    val updated_before : t -> Ptime.t option
    
    val visibility : t -> AssetVisibility.T.t option
    
    val with_deleted : t -> bool option
    
    val with_exif : t -> bool option
    
    val with_people : t -> bool option
    
    val with_stacked : t -> bool option
    
    val jsont : t Jsont.t
  end
end

module AssetStats : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : images:int -> total:int -> videos:int -> unit -> t
    
    val images : t -> int
    
    val total : t -> int
    
    val videos : t -> int
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve user statistics
  
      Retrieve asset statistics for a specific user. *)
  val get_user_statistics_admin : id:string -> ?is_favorite:string -> ?is_trashed:string -> ?visibility:string -> t -> unit -> ResponseDto.t
  
  (** Get asset statistics
  
      Retrieve various statistics about the assets owned by the authenticated user. *)
  val get_asset_statistics : ?is_favorite:string -> ?is_trashed:string -> ?visibility:string -> t -> unit -> ResponseDto.t
end

module AssetStack : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : asset_count:int -> id:string -> primary_asset_id:string -> unit -> t
    
    val asset_count : t -> int
    
    val id : t -> string
    
    val primary_asset_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module AssetOrder : sig
  module T : sig
    type t = [
      | `Asc
      | `Desc
    ]
    
    val jsont : t Jsont.t
  end
end

module UpdateAlbum : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : ?album_name:string -> ?album_thumbnail_asset_id:string -> ?description:string -> ?is_activity_enabled:bool -> ?order:AssetOrder.T.t -> unit -> t
    
    val album_name : t -> string option
    
    val album_thumbnail_asset_id : t -> string option
    
    val description : t -> string option
    
    val is_activity_enabled : t -> bool option
    
    val order : t -> AssetOrder.T.t option
    
    val jsont : t Jsont.t
  end
end

module SyncAlbumV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : created_at:Ptime.t -> description:string -> id:string -> is_activity_enabled:bool -> name:string -> order:AssetOrder.T.t -> owner_id:string -> updated_at:Ptime.t -> ?thumbnail_asset_id:string -> unit -> t
    
    val created_at : t -> Ptime.t
    
    val description : t -> string
    
    val id : t -> string
    
    val is_activity_enabled : t -> bool
    
    val name : t -> string
    
    val order : t -> AssetOrder.T.t
    
    val owner_id : t -> string
    
    val thumbnail_asset_id : t -> string option
    
    val updated_at : t -> Ptime.t
    
    val jsont : t Jsont.t
  end
end

module MetadataSearch : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : ?order:AssetOrder.T.t -> ?album_ids:string list -> ?checksum:string -> ?city:string -> ?country:string -> ?created_after:Ptime.t -> ?created_before:Ptime.t -> ?description:string -> ?device_asset_id:string -> ?device_id:string -> ?encoded_video_path:string -> ?id:string -> ?is_encoded:bool -> ?is_favorite:bool -> ?is_motion:bool -> ?is_not_in_album:bool -> ?is_offline:bool -> ?lens_model:string -> ?library_id:string -> ?make:string -> ?model:string -> ?ocr:string -> ?original_file_name:string -> ?original_path:string -> ?page:float -> ?person_ids:string list -> ?preview_path:string -> ?rating:float -> ?size:float -> ?state:string -> ?tag_ids:string list -> ?taken_after:Ptime.t -> ?taken_before:Ptime.t -> ?thumbnail_path:string -> ?trashed_after:Ptime.t -> ?trashed_before:Ptime.t -> ?type_:AssetTypeEnum.T.t -> ?updated_after:Ptime.t -> ?updated_before:Ptime.t -> ?visibility:AssetVisibility.T.t -> ?with_deleted:bool -> ?with_exif:bool -> ?with_people:bool -> ?with_stacked:bool -> unit -> t
    
    val album_ids : t -> string list option
    
    val checksum : t -> string option
    
    val city : t -> string option
    
    val country : t -> string option
    
    val created_after : t -> Ptime.t option
    
    val created_before : t -> Ptime.t option
    
    val description : t -> string option
    
    val device_asset_id : t -> string option
    
    val device_id : t -> string option
    
    val encoded_video_path : t -> string option
    
    val id : t -> string option
    
    val is_encoded : t -> bool option
    
    val is_favorite : t -> bool option
    
    val is_motion : t -> bool option
    
    val is_not_in_album : t -> bool option
    
    val is_offline : t -> bool option
    
    val lens_model : t -> string option
    
    val library_id : t -> string option
    
    val make : t -> string option
    
    val model : t -> string option
    
    val ocr : t -> string option
    
    val order : t -> AssetOrder.T.t
    
    val original_file_name : t -> string option
    
    val original_path : t -> string option
    
    val page : t -> float option
    
    val person_ids : t -> string list option
    
    val preview_path : t -> string option
    
    val rating : t -> float option
    
    val size : t -> float option
    
    val state : t -> string option
    
    val tag_ids : t -> string list option
    
    val taken_after : t -> Ptime.t option
    
    val taken_before : t -> Ptime.t option
    
    val thumbnail_path : t -> string option
    
    val trashed_after : t -> Ptime.t option
    
    val trashed_before : t -> Ptime.t option
    
    val type_ : t -> AssetTypeEnum.T.t option
    
    val updated_after : t -> Ptime.t option
    
    val updated_before : t -> Ptime.t option
    
    val visibility : t -> AssetVisibility.T.t option
    
    val with_deleted : t -> bool option
    
    val with_exif : t -> bool option
    
    val with_people : t -> bool option
    
    val with_stacked : t -> bool option
    
    val jsont : t Jsont.t
  end
end

module Albums : sig
  module Update : sig
    type t
    
    (** Construct a value *)
    val v : ?default_asset_order:AssetOrder.T.t -> unit -> t
    
    val default_asset_order : t -> AssetOrder.T.t option
    
    val jsont : t Jsont.t
  end
  
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : ?default_asset_order:AssetOrder.T.t -> unit -> t
    
    val default_asset_order : t -> AssetOrder.T.t
    
    val jsont : t Jsont.t
  end
end

module AssetOcr : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value
        @param box_score Confidence score for text detection box
        @param text Recognized text
        @param text_score Confidence score for text recognition
        @param x1 Normalized x coordinate of box corner 1 (0-1)
        @param x2 Normalized x coordinate of box corner 2 (0-1)
        @param x3 Normalized x coordinate of box corner 3 (0-1)
        @param x4 Normalized x coordinate of box corner 4 (0-1)
        @param y1 Normalized y coordinate of box corner 1 (0-1)
        @param y2 Normalized y coordinate of box corner 2 (0-1)
        @param y3 Normalized y coordinate of box corner 3 (0-1)
        @param y4 Normalized y coordinate of box corner 4 (0-1)
    *)
    val v : asset_id:string -> box_score:float -> id:string -> text:string -> text_score:float -> x1:float -> x2:float -> x3:float -> x4:float -> y1:float -> y2:float -> y3:float -> y4:float -> unit -> t
    
    val asset_id : t -> string
    
    (** Confidence score for text detection box *)
    val box_score : t -> float
    
    val id : t -> string
    
    (** Recognized text *)
    val text : t -> string
    
    (** Confidence score for text recognition *)
    val text_score : t -> float
    
    (** Normalized x coordinate of box corner 1 (0-1) *)
    val x1 : t -> float
    
    (** Normalized x coordinate of box corner 2 (0-1) *)
    val x2 : t -> float
    
    (** Normalized x coordinate of box corner 3 (0-1) *)
    val x3 : t -> float
    
    (** Normalized x coordinate of box corner 4 (0-1) *)
    val x4 : t -> float
    
    (** Normalized y coordinate of box corner 1 (0-1) *)
    val y1 : t -> float
    
    (** Normalized y coordinate of box corner 2 (0-1) *)
    val y2 : t -> float
    
    (** Normalized y coordinate of box corner 3 (0-1) *)
    val y3 : t -> float
    
    (** Normalized y coordinate of box corner 4 (0-1) *)
    val y4 : t -> float
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve asset OCR data
  
      Retrieve all OCR (Optical Character Recognition) data associated with the specified asset. *)
  val get_asset_ocr : id:string -> t -> unit -> ResponseDto.t
end

module AssetMetadataUpsertItem : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : key:string -> value:Jsont.json -> unit -> t
    
    val key : t -> string
    
    val value : t -> Jsont.json
    
    val jsont : t Jsont.t
  end
end

module AssetMetadataUpsert : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : items:AssetMetadataUpsertItem.Dto.t list -> unit -> t
    
    val items : t -> AssetMetadataUpsertItem.Dto.t list
    
    val jsont : t Jsont.t
  end
end

module AssetMetadata : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : key:string -> updated_at:Ptime.t -> value:Jsont.json -> unit -> t
    
    val key : t -> string
    
    val updated_at : t -> Ptime.t
    
    val value : t -> Jsont.json
    
    val jsont : t Jsont.t
  end
  
  (** Get asset metadata
  
      Retrieve all metadata key-value pairs associated with the specified asset. *)
  val get_asset_metadata : id:string -> t -> unit -> ResponseDto.t
  
  (** Update asset metadata
  
      Update or add metadata key-value pairs for the specified asset. *)
  val update_asset_metadata : id:string -> body:AssetMetadataUpsert.Dto.t -> t -> unit -> ResponseDto.t
  
  (** Retrieve asset metadata by key
  
      Retrieve the value of a specific metadata key associated with the specified asset. *)
  val get_asset_metadata_by_key : id:string -> key:string -> t -> unit -> ResponseDto.t
end

module AssetMedia : sig
  module Status : sig
    type t = [
      | `Created
      | `Replaced
      | `Duplicate
    ]
    
    val jsont : t Jsont.t
  end
  
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : id:string -> status:Status.t -> unit -> t
    
    val id : t -> string
    
    val status : t -> Status.t
    
    val jsont : t Jsont.t
  end
  
  module CreateDto : sig
    type t
    
    (** Construct a value *)
    val v : asset_data:string -> device_asset_id:string -> device_id:string -> file_created_at:Ptime.t -> file_modified_at:Ptime.t -> ?duration:string -> ?filename:string -> ?is_favorite:bool -> ?live_photo_video_id:string -> ?metadata:AssetMetadataUpsertItem.Dto.t list -> ?sidecar_data:string -> ?visibility:AssetVisibility.T.t -> unit -> t
    
    val asset_data : t -> string
    
    val device_asset_id : t -> string
    
    val device_id : t -> string
    
    val duration : t -> string option
    
    val file_created_at : t -> Ptime.t
    
    val file_modified_at : t -> Ptime.t
    
    val filename : t -> string option
    
    val is_favorite : t -> bool option
    
    val live_photo_video_id : t -> string option
    
    val metadata : t -> AssetMetadataUpsertItem.Dto.t list option
    
    val sidecar_data : t -> string option
    
    val visibility : t -> AssetVisibility.T.t option
    
    val jsont : t Jsont.t
  end
  
  (** Upload asset
  
      Uploads a new asset to the server. *)
  val upload_asset : ?key:string -> ?slug:string -> t -> unit -> ResponseDto.t
  
  (** Replace asset
  
      Replace the asset with new file, without changing its id. *)
  val replace_asset : id:string -> ?key:string -> ?slug:string -> t -> unit -> ResponseDto.t
end

module AssetMetadataBulkUpsertItem : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : asset_id:string -> key:string -> value:Jsont.json -> unit -> t
    
    val asset_id : t -> string
    
    val key : t -> string
    
    val value : t -> Jsont.json
    
    val jsont : t Jsont.t
  end
end

module AssetMetadataBulkUpsert : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : items:AssetMetadataBulkUpsertItem.Dto.t list -> unit -> t
    
    val items : t -> AssetMetadataBulkUpsertItem.Dto.t list
    
    val jsont : t Jsont.t
  end
end

module AssetMetadataBulk : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : asset_id:string -> key:string -> updated_at:Ptime.t -> value:Jsont.json -> unit -> t
    
    val asset_id : t -> string
    
    val key : t -> string
    
    val updated_at : t -> Ptime.t
    
    val value : t -> Jsont.json
    
    val jsont : t Jsont.t
  end
  
  (** Upsert asset metadata
  
      Upsert metadata key-value pairs for multiple assets. *)
  val update_bulk_asset_metadata : body:AssetMetadataBulkUpsert.Dto.t -> t -> unit -> ResponseDto.t
end

module AssetMetadataBulkDeleteItem : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : asset_id:string -> key:string -> unit -> t
    
    val asset_id : t -> string
    
    val key : t -> string
    
    val jsont : t Jsont.t
  end
end

module AssetMetadataBulkDelete : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : items:AssetMetadataBulkDeleteItem.Dto.t list -> unit -> t
    
    val items : t -> AssetMetadataBulkDeleteItem.Dto.t list
    
    val jsont : t Jsont.t
  end
end

module AssetMediaSize : sig
  module T : sig
    type t = [
      | `Original
      | `Fullsize
      | `Preview
      | `Thumbnail
    ]
    
    val jsont : t Jsont.t
  end
end

module AssetMediaReplace : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : asset_data:string -> device_asset_id:string -> device_id:string -> file_created_at:Ptime.t -> file_modified_at:Ptime.t -> ?duration:string -> ?filename:string -> unit -> t
    
    val asset_data : t -> string
    
    val device_asset_id : t -> string
    
    val device_id : t -> string
    
    val duration : t -> string option
    
    val file_created_at : t -> Ptime.t
    
    val file_modified_at : t -> Ptime.t
    
    val filename : t -> string option
    
    val jsont : t Jsont.t
  end
end

module AssetJobName : sig
  module T : sig
    type t = [
      | `Refresh_faces
      | `Refresh_metadata
      | `Regenerate_thumbnail
      | `Transcode_video
    ]
    
    val jsont : t Jsont.t
  end
end

module AssetJobs : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : asset_ids:string list -> name:AssetJobName.T.t -> unit -> t
    
    val asset_ids : t -> string list
    
    val name : t -> AssetJobName.T.t
    
    val jsont : t Jsont.t
  end
end

module AssetIds : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : asset_id:string -> success:bool -> ?error:string -> unit -> t
    
    val asset_id : t -> string
    
    val error : t -> string option
    
    val success : t -> bool
    
    val jsont : t Jsont.t
  end
  
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : asset_ids:string list -> unit -> t
    
    val asset_ids : t -> string list
    
    val jsont : t Jsont.t
  end
  
  (** Add assets to a shared link
  
      Add assets to a specific shared link by its ID. This endpoint is only relevant for shared link of type individual. *)
  val add_shared_link_assets : id:string -> ?key:string -> ?slug:string -> body:Dto.t -> t -> unit -> ResponseDto.t
  
  (** Remove assets from a shared link
  
      Remove assets from a specific shared link by its ID. This endpoint is only relevant for shared link of type individual. *)
  val remove_shared_link_assets : id:string -> ?key:string -> ?slug:string -> t -> unit -> ResponseDto.t
end

module AssetFullSync : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : limit:int -> updated_until:Ptime.t -> ?last_id:string -> ?user_id:string -> unit -> t
    
    val last_id : t -> string option
    
    val limit : t -> int
    
    val updated_until : t -> Ptime.t
    
    val user_id : t -> string option
    
    val jsont : t Jsont.t
  end
end

module Asset : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value
        @param checksum base64 encoded sha1 hash
        @param created_at The UTC timestamp when the asset was originally uploaded to Immich.
        @param file_created_at The actual UTC timestamp when the file was created/captured, preserving timezone information. This is the authoritative timestamp for chronological sorting within timeline groups. Combined with timezone data, this can be used to determine the exact moment the photo was taken.
        @param file_modified_at The UTC timestamp when the file was last modified on the filesystem. This reflects the last time the physical file was changed, which may be different from when the photo was originally taken.
        @param local_date_time The local date and time when the photo/video was taken, derived from EXIF metadata. This represents the photographer's local time regardless of timezone, stored as a timezone-agnostic timestamp. Used for timeline grouping by "local" days and months.
        @param updated_at The UTC timestamp when the asset record was last updated in the database. This is automatically maintained by the database and reflects when any field in the asset was last modified.
    *)
    val v : checksum:string -> created_at:Ptime.t -> device_asset_id:string -> device_id:string -> duration:string -> file_created_at:Ptime.t -> file_modified_at:Ptime.t -> has_metadata:bool -> id:string -> is_archived:bool -> is_edited:bool -> is_favorite:bool -> is_offline:bool -> is_trashed:bool -> local_date_time:Ptime.t -> original_file_name:string -> original_path:string -> owner_id:string -> type_:AssetTypeEnum.T.t -> updated_at:Ptime.t -> visibility:AssetVisibility.T.t -> ?duplicate_id:string -> ?exif_info:Exif.ResponseDto.t -> ?height:float -> ?library_id:string -> ?live_photo_video_id:string -> ?original_mime_type:string -> ?owner:User.ResponseDto.t -> ?people:PersonWithFaces.ResponseDto.t list -> ?resized:bool -> ?stack:AssetStack.ResponseDto.t -> ?tags:Tag.ResponseDto.t list -> ?thumbhash:string -> ?unassigned_faces:AssetFaceWithoutPerson.ResponseDto.t list -> ?width:float -> unit -> t
    
    (** base64 encoded sha1 hash *)
    val checksum : t -> string
    
    (** The UTC timestamp when the asset was originally uploaded to Immich. *)
    val created_at : t -> Ptime.t
    
    val device_asset_id : t -> string
    
    val device_id : t -> string
    
    val duplicate_id : t -> string option
    
    val duration : t -> string
    
    val exif_info : t -> Exif.ResponseDto.t option
    
    (** The actual UTC timestamp when the file was created/captured, preserving timezone information. This is the authoritative timestamp for chronological sorting within timeline groups. Combined with timezone data, this can be used to determine the exact moment the photo was taken. *)
    val file_created_at : t -> Ptime.t
    
    (** The UTC timestamp when the file was last modified on the filesystem. This reflects the last time the physical file was changed, which may be different from when the photo was originally taken. *)
    val file_modified_at : t -> Ptime.t
    
    val has_metadata : t -> bool
    
    val height : t -> float option
    
    val id : t -> string
    
    val is_archived : t -> bool
    
    val is_edited : t -> bool
    
    val is_favorite : t -> bool
    
    val is_offline : t -> bool
    
    val is_trashed : t -> bool
    
    val library_id : t -> string option
    
    val live_photo_video_id : t -> string option
    
    (** The local date and time when the photo/video was taken, derived from EXIF metadata. This represents the photographer's local time regardless of timezone, stored as a timezone-agnostic timestamp. Used for timeline grouping by "local" days and months. *)
    val local_date_time : t -> Ptime.t
    
    val original_file_name : t -> string
    
    val original_mime_type : t -> string option
    
    val original_path : t -> string
    
    val owner : t -> User.ResponseDto.t option
    
    val owner_id : t -> string
    
    val people : t -> PersonWithFaces.ResponseDto.t list option
    
    val resized : t -> bool option
    
    val stack : t -> AssetStack.ResponseDto.t option
    
    val tags : t -> Tag.ResponseDto.t list option
    
    val thumbhash : t -> string option
    
    val type_ : t -> AssetTypeEnum.T.t
    
    val unassigned_faces : t -> AssetFaceWithoutPerson.ResponseDto.t list option
    
    (** The UTC timestamp when the asset record was last updated in the database. This is automatically maintained by the database and reflects when any field in the asset was last modified. *)
    val updated_at : t -> Ptime.t
    
    val visibility : t -> AssetVisibility.T.t
    
    val width : t -> float option
    
    val jsont : t Jsont.t
  end
  
  (** Get random assets
  
      Retrieve a specified number of random assets for the authenticated user. *)
  val get_random : ?count:string -> t -> unit -> ResponseDto.t
  
  (** Retrieve an asset
  
      Retrieve detailed information about a specific asset. *)
  val get_asset_info : id:string -> ?key:string -> ?slug:string -> t -> unit -> ResponseDto.t
  
  (** Update an asset
  
      Update information of a specific asset. *)
  val update_asset : id:string -> body:UpdateAsset.Dto.t -> t -> unit -> ResponseDto.t
  
  (** Retrieve assets by city
  
      Retrieve a list of assets with each asset belonging to a different city. This endpoint is used on the places pages to show a single thumbnail for each city the user has assets in. *)
  val get_assets_by_city : t -> unit -> ResponseDto.t
  
  (** Search large assets
  
      Search for assets that are considered large based on specified criteria. *)
  val search_large_assets : ?album_ids:string -> ?city:string -> ?country:string -> ?created_after:string -> ?created_before:string -> ?device_id:string -> ?is_encoded:string -> ?is_favorite:string -> ?is_motion:string -> ?is_not_in_album:string -> ?is_offline:string -> ?lens_model:string -> ?library_id:string -> ?make:string -> ?min_file_size:string -> ?model:string -> ?ocr:string -> ?person_ids:string -> ?rating:string -> ?size:string -> ?state:string -> ?tag_ids:string -> ?taken_after:string -> ?taken_before:string -> ?trashed_after:string -> ?trashed_before:string -> ?type_:string -> ?updated_after:string -> ?updated_before:string -> ?visibility:string -> ?with_deleted:string -> ?with_exif:string -> t -> unit -> ResponseDto.t
  
  (** Search random assets
  
      Retrieve a random selection of assets based on the provided criteria. *)
  val search_random : body:RandomSearch.Dto.t -> t -> unit -> ResponseDto.t
  
  (** Get full sync for user
  
      Retrieve all assets for a full synchronization for the authenticated user. *)
  val get_full_sync_for_user : body:AssetFullSync.Dto.t -> t -> unit -> ResponseDto.t
  
  (** Retrieve assets by original path
  
      Retrieve assets that are children of a specific folder. *)
  val get_assets_by_original_path : path:string -> t -> unit -> ResponseDto.t
end

module Stack : sig
  module UpdateDto : sig
    type t
    
    (** Construct a value *)
    val v : ?primary_asset_id:string -> unit -> t
    
    val primary_asset_id : t -> string option
    
    val jsont : t Jsont.t
  end
  
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : assets:Asset.ResponseDto.t list -> id:string -> primary_asset_id:string -> unit -> t
    
    val assets : t -> Asset.ResponseDto.t list
    
    val id : t -> string
    
    val primary_asset_id : t -> string
    
    val jsont : t Jsont.t
  end
  
  module CreateDto : sig
    type t
    
    (** Construct a value
        @param asset_ids first asset becomes the primary
    *)
    val v : asset_ids:string list -> unit -> t
    
    (** first asset becomes the primary *)
    val asset_ids : t -> string list
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve stacks
  
      Retrieve a list of stacks. *)
  val search_stacks : ?primary_asset_id:string -> t -> unit -> ResponseDto.t
  
  (** Create a stack
  
      Create a new stack by providing a name and a list of asset IDs to include in the stack. If any of the provided asset IDs are primary assets of an existing stack, the existing stack will be merged into the newly created stack. *)
  val create_stack : body:CreateDto.t -> t -> unit -> ResponseDto.t
  
  (** Retrieve a stack
  
      Retrieve a specific stack by its ID. *)
  val get_stack : id:string -> t -> unit -> ResponseDto.t
  
  (** Update a stack
  
      Update an existing stack by its ID. *)
  val update_stack : id:string -> body:UpdateDto.t -> t -> unit -> ResponseDto.t
end

module SearchExplore : sig
  module Item : sig
    type t
    
    (** Construct a value *)
    val v : data:Asset.ResponseDto.t -> value:string -> unit -> t
    
    val data : t -> Asset.ResponseDto.t
    
    val value : t -> string
    
    val jsont : t Jsont.t
  end
  
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : field_name:string -> items:Item.t list -> unit -> t
    
    val field_name : t -> string
    
    val items : t -> Item.t list
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve explore data
  
      Retrieve data for the explore section, such as popular people and places. *)
  val get_explore_data : t -> unit -> ResponseDto.t
end

module SearchAsset : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : count:int -> facets:SearchFacet.ResponseDto.t list -> items:Asset.ResponseDto.t list -> total:int -> ?next_page:string -> unit -> t
    
    val count : t -> int
    
    val facets : t -> SearchFacet.ResponseDto.t list
    
    val items : t -> Asset.ResponseDto.t list
    
    val next_page : t -> string option
    
    val total : t -> int
    
    val jsont : t Jsont.t
  end
end

module Memory : sig
  module UpdateDto : sig
    type t
    
    (** Construct a value *)
    val v : ?is_saved:bool -> ?memory_at:Ptime.t -> ?seen_at:Ptime.t -> unit -> t
    
    val is_saved : t -> bool option
    
    val memory_at : t -> Ptime.t option
    
    val seen_at : t -> Ptime.t option
    
    val jsont : t Jsont.t
  end
  
  module Type : sig
    type t = [
      | `On_this_day
    ]
    
    val jsont : t Jsont.t
  end
  
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : assets:Asset.ResponseDto.t list -> created_at:Ptime.t -> data:OnThisDay.Dto.t -> id:string -> is_saved:bool -> memory_at:Ptime.t -> owner_id:string -> type_:Type.t -> updated_at:Ptime.t -> ?deleted_at:Ptime.t -> ?hide_at:Ptime.t -> ?seen_at:Ptime.t -> ?show_at:Ptime.t -> unit -> t
    
    val assets : t -> Asset.ResponseDto.t list
    
    val created_at : t -> Ptime.t
    
    val data : t -> OnThisDay.Dto.t
    
    val deleted_at : t -> Ptime.t option
    
    val hide_at : t -> Ptime.t option
    
    val id : t -> string
    
    val is_saved : t -> bool
    
    val memory_at : t -> Ptime.t
    
    val owner_id : t -> string
    
    val seen_at : t -> Ptime.t option
    
    val show_at : t -> Ptime.t option
    
    val type_ : t -> Type.t
    
    val updated_at : t -> Ptime.t
    
    val jsont : t Jsont.t
  end
  
  module CreateDto : sig
    type t
    
    (** Construct a value *)
    val v : data:OnThisDay.Dto.t -> memory_at:Ptime.t -> type_:Type.t -> ?asset_ids:string list -> ?is_saved:bool -> ?seen_at:Ptime.t -> unit -> t
    
    val asset_ids : t -> string list option
    
    val data : t -> OnThisDay.Dto.t
    
    val is_saved : t -> bool option
    
    val memory_at : t -> Ptime.t
    
    val seen_at : t -> Ptime.t option
    
    val type_ : t -> Type.t
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve memories
  
      Retrieve a list of memories. Memories are sorted descending by creation date by default, although they can also be sorted in ascending order, or randomly. 
      @param size Number of memories to return
  *)
  val search_memories : ?for_:string -> ?is_saved:string -> ?is_trashed:string -> ?order:string -> ?size:string -> ?type_:string -> t -> unit -> ResponseDto.t
  
  (** Create a memory
  
      Create a new memory by providing a name, description, and a list of asset IDs to include in the memory. *)
  val create_memory : body:CreateDto.t -> t -> unit -> ResponseDto.t
  
  (** Retrieve a memory
  
      Retrieve a specific memory by its ID. *)
  val get_memory : id:string -> t -> unit -> ResponseDto.t
  
  (** Update a memory
  
      Update an existing memory by its ID. *)
  val update_memory : id:string -> body:UpdateDto.t -> t -> unit -> ResponseDto.t
end

module SyncMemoryV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : created_at:Ptime.t -> data:Jsont.json -> id:string -> is_saved:bool -> memory_at:Ptime.t -> owner_id:string -> type_:Memory.Type.t -> updated_at:Ptime.t -> ?deleted_at:Ptime.t -> ?hide_at:Ptime.t -> ?seen_at:Ptime.t -> ?show_at:Ptime.t -> unit -> t
    
    val created_at : t -> Ptime.t
    
    val data : t -> Jsont.json
    
    val deleted_at : t -> Ptime.t option
    
    val hide_at : t -> Ptime.t option
    
    val id : t -> string
    
    val is_saved : t -> bool
    
    val memory_at : t -> Ptime.t
    
    val owner_id : t -> string
    
    val seen_at : t -> Ptime.t option
    
    val show_at : t -> Ptime.t option
    
    val type_ : t -> Memory.Type.t
    
    val updated_at : t -> Ptime.t
    
    val jsont : t Jsont.t
  end
end

module Duplicate : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : assets:Asset.ResponseDto.t list -> duplicate_id:string -> unit -> t
    
    val assets : t -> Asset.ResponseDto.t list
    
    val duplicate_id : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve duplicates
  
      Retrieve a list of duplicate assets available to the authenticated user. *)
  val get_asset_duplicates : t -> unit -> ResponseDto.t
end

module AssetDeltaSync : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : deleted:string list -> needs_full_sync:bool -> upserted:Asset.ResponseDto.t list -> unit -> t
    
    val deleted : t -> string list
    
    val needs_full_sync : t -> bool
    
    val upserted : t -> Asset.ResponseDto.t list
    
    val jsont : t Jsont.t
  end
  
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : updated_after:Ptime.t -> user_ids:string list -> unit -> t
    
    val updated_after : t -> Ptime.t
    
    val user_ids : t -> string list
    
    val jsont : t Jsont.t
  end
  
  (** Get delta sync for user
  
      Retrieve changed assets since the last sync for the authenticated user. *)
  val get_delta_sync : body:Dto.t -> t -> unit -> ResponseDto.t
end

module AssetFaceUpdate : sig
  module Item : sig
    type t
    
    (** Construct a value *)
    val v : asset_id:string -> person_id:string -> unit -> t
    
    val asset_id : t -> string
    
    val person_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module AssetFaceDelete : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : force:bool -> unit -> t
    
    val force : t -> bool
    
    val jsont : t Jsont.t
  end
end

module AssetEditActionList : sig
  module Dto : sig
    type t
    
    (** Construct a value
        @param edits list of edits
    *)
    val v : edits:Jsont.json list -> unit -> t
    
    (** list of edits *)
    val edits : t -> Jsont.json list
    
    val jsont : t Jsont.t
  end
end

module AssetEdits : sig
  module Dto : sig
    type t
    
    (** Construct a value
        @param edits list of edits
    *)
    val v : asset_id:string -> edits:Jsont.json list -> unit -> t
    
    val asset_id : t -> string
    
    (** list of edits *)
    val edits : t -> Jsont.json list
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve edits for an existing asset
  
      Retrieve a series of edit actions (crop, rotate, mirror) associated with the specified asset. *)
  val get_asset_edits : id:string -> t -> unit -> Dto.t
  
  (** Apply edits to an existing asset
  
      Apply a series of edit actions (crop, rotate, mirror) to the specified asset. *)
  val edit_asset : id:string -> body:AssetEditActionList.Dto.t -> t -> unit -> Dto.t
end

module AssetEditAction : sig
  module T : sig
    type t = [
      | `Crop
      | `Rotate
      | `Mirror
    ]
    
    val jsont : t Jsont.t
  end
end

module AssetEditActionRotate : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : action:AssetEditAction.T.t -> parameters:RotateParameters.T.t -> unit -> t
    
    val action : t -> AssetEditAction.T.t
    
    val parameters : t -> RotateParameters.T.t
    
    val jsont : t Jsont.t
  end
end

module AssetEditActionMirror : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : action:AssetEditAction.T.t -> parameters:MirrorParameters.T.t -> unit -> t
    
    val action : t -> AssetEditAction.T.t
    
    val parameters : t -> MirrorParameters.T.t
    
    val jsont : t Jsont.t
  end
end

module AssetEditActionCrop : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : action:AssetEditAction.T.t -> parameters:CropParameters.T.t -> unit -> t
    
    val action : t -> AssetEditAction.T.t
    
    val parameters : t -> CropParameters.T.t
    
    val jsont : t Jsont.t
  end
end

module AssetCopy : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : source_id:string -> target_id:string -> ?albums:bool -> ?favorite:bool -> ?shared_links:bool -> ?sidecar:bool -> ?stack:bool -> unit -> t
    
    val albums : t -> bool
    
    val favorite : t -> bool
    
    val shared_links : t -> bool
    
    val sidecar : t -> bool
    
    val source_id : t -> string
    
    val stack : t -> bool
    
    val target_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module AssetBulkUploadCheck : sig
  module Result : sig
    type t
    
    (** Construct a value *)
    val v : action:string -> id:string -> ?asset_id:string -> ?is_trashed:bool -> ?reason:string -> unit -> t
    
    val action : t -> string
    
    val asset_id : t -> string option
    
    val id : t -> string
    
    val is_trashed : t -> bool option
    
    val reason : t -> string option
    
    val jsont : t Jsont.t
  end
  
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : results:Result.t list -> unit -> t
    
    val results : t -> Result.t list
    
    val jsont : t Jsont.t
  end
  
  module Item : sig
    type t
    
    (** Construct a value
        @param checksum base64 or hex encoded sha1 hash
    *)
    val v : checksum:string -> id:string -> unit -> t
    
    (** base64 or hex encoded sha1 hash *)
    val checksum : t -> string
    
    val id : t -> string
    
    val jsont : t Jsont.t
  end
  
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : assets:Item.t list -> unit -> t
    
    val assets : t -> Item.t list
    
    val jsont : t Jsont.t
  end
  
  (** Check bulk upload
  
      Determine which assets have already been uploaded to the server based on their SHA1 checksums. *)
  val check_bulk_upload : body:Dto.t -> t -> unit -> ResponseDto.t
end

module AssetBulkDelete : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : ids:string list -> ?force:bool -> unit -> t
    
    val force : t -> bool option
    
    val ids : t -> string list
    
    val jsont : t Jsont.t
  end
end

module AlbumUserRole : sig
  module T : sig
    type t = [
      | `Editor
      | `Viewer
    ]
    
    val jsont : t Jsont.t
  end
end

module UpdateAlbumUser : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : role:AlbumUserRole.T.t -> unit -> t
    
    val role : t -> AlbumUserRole.T.t
    
    val jsont : t Jsont.t
  end
end

module SyncAlbumUserV1 : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : album_id:string -> role:AlbumUserRole.T.t -> user_id:string -> unit -> t
    
    val album_id : t -> string
    
    val role : t -> AlbumUserRole.T.t
    
    val user_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module AlbumUserAdd : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : user_id:string -> ?role:AlbumUserRole.T.t -> unit -> t
    
    val role : t -> AlbumUserRole.T.t
    
    val user_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module AddUsers : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : album_users:AlbumUserAdd.Dto.t list -> unit -> t
    
    val album_users : t -> AlbumUserAdd.Dto.t list
    
    val jsont : t Jsont.t
  end
end

module AlbumUser : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : role:AlbumUserRole.T.t -> user:User.ResponseDto.t -> unit -> t
    
    val role : t -> AlbumUserRole.T.t
    
    val user : t -> User.ResponseDto.t
    
    val jsont : t Jsont.t
  end
  
  module CreateDto : sig
    type t
    
    (** Construct a value *)
    val v : role:AlbumUserRole.T.t -> user_id:string -> unit -> t
    
    val role : t -> AlbumUserRole.T.t
    
    val user_id : t -> string
    
    val jsont : t Jsont.t
  end
end

module CreateAlbum : sig
  module Dto : sig
    type t
    
    (** Construct a value *)
    val v : album_name:string -> ?album_users:AlbumUser.CreateDto.t list -> ?asset_ids:string list -> ?description:string -> unit -> t
    
    val album_name : t -> string
    
    val album_users : t -> AlbumUser.CreateDto.t list option
    
    val asset_ids : t -> string list option
    
    val description : t -> string option
    
    val jsont : t Jsont.t
  end
end

module Album : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : album_name:string -> album_users:AlbumUser.ResponseDto.t list -> asset_count:int -> assets:Asset.ResponseDto.t list -> created_at:Ptime.t -> description:string -> has_shared_link:bool -> id:string -> is_activity_enabled:bool -> owner:User.ResponseDto.t -> owner_id:string -> shared:bool -> updated_at:Ptime.t -> ?album_thumbnail_asset_id:string -> ?contributor_counts:ContributorCount.ResponseDto.t list -> ?end_date:Ptime.t -> ?last_modified_asset_timestamp:Ptime.t -> ?order:AssetOrder.T.t -> ?start_date:Ptime.t -> unit -> t
    
    val album_name : t -> string
    
    val album_thumbnail_asset_id : t -> string option
    
    val album_users : t -> AlbumUser.ResponseDto.t list
    
    val asset_count : t -> int
    
    val assets : t -> Asset.ResponseDto.t list
    
    val contributor_counts : t -> ContributorCount.ResponseDto.t list option
    
    val created_at : t -> Ptime.t
    
    val description : t -> string
    
    val end_date : t -> Ptime.t option
    
    val has_shared_link : t -> bool
    
    val id : t -> string
    
    val is_activity_enabled : t -> bool
    
    val last_modified_asset_timestamp : t -> Ptime.t option
    
    val order : t -> AssetOrder.T.t option
    
    val owner : t -> User.ResponseDto.t
    
    val owner_id : t -> string
    
    val shared : t -> bool
    
    val start_date : t -> Ptime.t option
    
    val updated_at : t -> Ptime.t
    
    val jsont : t Jsont.t
  end
  
  (** List all albums
  
      Retrieve a list of albums available to the authenticated user. 
      @param asset_id Only returns albums that contain the asset
  Ignores the shared parameter
  undefined: get all albums
  *)
  val get_all_albums : ?asset_id:string -> ?shared:string -> t -> unit -> ResponseDto.t
  
  (** Create an album
  
      Create a new album. The album can also be created with initial users and assets. *)
  val create_album : body:CreateAlbum.Dto.t -> t -> unit -> ResponseDto.t
  
  (** Retrieve an album
  
      Retrieve information about a specific album by its ID. *)
  val get_album_info : id:string -> ?key:string -> ?slug:string -> ?without_assets:string -> t -> unit -> ResponseDto.t
  
  (** Update an album
  
      Update the information of a specific album by its ID. This endpoint can be used to update the album name, description, sort order, etc. However, it is not used to add or remove assets or users from the album. *)
  val update_album_info : id:string -> body:UpdateAlbum.Dto.t -> t -> unit -> ResponseDto.t
  
  (** Share album with users
  
      Share an album with multiple users. Each user can be given a specific role in the album. *)
  val add_users_to_album : id:string -> body:AddUsers.Dto.t -> t -> unit -> ResponseDto.t
end

module SharedLink : sig
  module Type : sig
    type t = [
      | `Album
      | `Individual
    ]
    
    val jsont : t Jsont.t
  end
  
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : allow_download:bool -> allow_upload:bool -> assets:Asset.ResponseDto.t list -> created_at:Ptime.t -> id:string -> key:string -> show_metadata:bool -> type_:Type.t -> user_id:string -> ?album:Album.ResponseDto.t -> ?description:string -> ?expires_at:Ptime.t -> ?password:string -> ?slug:string -> ?token:string -> unit -> t
    
    val album : t -> Album.ResponseDto.t option
    
    val allow_download : t -> bool
    
    val allow_upload : t -> bool
    
    val assets : t -> Asset.ResponseDto.t list
    
    val created_at : t -> Ptime.t
    
    val description : t -> string option
    
    val expires_at : t -> Ptime.t option
    
    val id : t -> string
    
    val key : t -> string
    
    val password : t -> string option
    
    val show_metadata : t -> bool
    
    val slug : t -> string option
    
    val token : t -> string option
    
    val type_ : t -> Type.t
    
    val user_id : t -> string
    
    val jsont : t Jsont.t
  end
  
  module CreateDto : sig
    type t
    
    (** Construct a value *)
    val v : type_:Type.t -> ?allow_download:bool -> ?expires_at:Ptime.t option -> ?show_metadata:bool -> ?album_id:string -> ?allow_upload:bool -> ?asset_ids:string list -> ?description:string -> ?password:string -> ?slug:string -> unit -> t
    
    val album_id : t -> string option
    
    val allow_download : t -> bool
    
    val allow_upload : t -> bool option
    
    val asset_ids : t -> string list option
    
    val description : t -> string option
    
    val expires_at : t -> Ptime.t option
    
    val password : t -> string option
    
    val show_metadata : t -> bool
    
    val slug : t -> string option
    
    val type_ : t -> Type.t
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve all shared links
  
      Retrieve a list of all shared links. *)
  val get_all_shared_links : ?album_id:string -> ?id:string -> t -> unit -> ResponseDto.t
  
  (** Create a shared link
  
      Create a new shared link. *)
  val create_shared_link : body:CreateDto.t -> t -> unit -> ResponseDto.t
  
  (** Retrieve current shared link
  
      Retrieve the current shared link associated with authentication method. *)
  val get_my_shared_link : ?key:string -> ?password:string -> ?slug:string -> ?token:string -> t -> unit -> ResponseDto.t
  
  (** Retrieve a shared link
  
      Retrieve a specific shared link by its ID. *)
  val get_shared_link_by_id : id:string -> t -> unit -> ResponseDto.t
  
  (** Update a shared link
  
      Update an existing shared link by its ID. *)
  val update_shared_link : id:string -> body:SharedLinkEdit.Dto.t -> t -> unit -> ResponseDto.t
end

module SearchAlbum : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : count:int -> facets:SearchFacet.ResponseDto.t list -> items:Album.ResponseDto.t list -> total:int -> unit -> t
    
    val count : t -> int
    
    val facets : t -> SearchFacet.ResponseDto.t list
    
    val items : t -> Album.ResponseDto.t list
    
    val total : t -> int
    
    val jsont : t Jsont.t
  end
end

module Search : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : albums:SearchAlbum.ResponseDto.t -> assets:SearchAsset.ResponseDto.t -> unit -> t
    
    val albums : t -> SearchAlbum.ResponseDto.t
    
    val assets : t -> SearchAsset.ResponseDto.t
    
    val jsont : t Jsont.t
  end
  
  (** Search assets by metadata
  
      Search for assets based on various metadata criteria. *)
  val search_assets : body:MetadataSearch.Dto.t -> t -> unit -> ResponseDto.t
  
  (** Smart asset search
  
      Perform a smart search for assets by using machine learning vectors to determine relevance. *)
  val search_smart : body:SmartSearch.Dto.t -> t -> unit -> ResponseDto.t
end

module AlbumStatistics : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : not_shared:int -> owned:int -> shared:int -> unit -> t
    
    val not_shared : t -> int
    
    val owned : t -> int
    
    val shared : t -> int
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve album statistics
  
      Returns statistics about the albums available to the authenticated user. *)
  val get_album_statistics : t -> unit -> ResponseDto.t
end

module AdminOnboarding : sig
  module UpdateDto : sig
    type t
    
    (** Construct a value *)
    val v : is_onboarded:bool -> unit -> t
    
    val is_onboarded : t -> bool
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve admin onboarding
  
      Retrieve the current admin onboarding status. *)
  val get_admin_onboarding : t -> unit -> UpdateDto.t
end

module ActivityStatistics : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : comments:int -> likes:int -> unit -> t
    
    val comments : t -> int
    
    val likes : t -> int
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve activity statistics
  
      Returns the number of likes and comments for a given album or asset in an album. *)
  val get_activity_statistics : album_id:string -> ?asset_id:string -> t -> unit -> ResponseDto.t
end

module UserPreferences : sig
  module UpdateDto : sig
    type t
    
    (** Construct a value *)
    val v : ?albums:Albums.Update.t -> ?avatar:Avatar.Update.t -> ?cast:Cast.Update.t -> ?download:Download.Update.t -> ?email_notifications:EmailNotifications.Update.t -> ?folders:Folders.Update.t -> ?memories:Memories.Update.t -> ?people:Jsont.json -> ?purchase:Purchase.Update.t -> ?ratings:Ratings.Update.t -> ?shared_links:SharedLinks.Update.t -> ?tags:Tags.Update.t -> unit -> t
    
    val albums : t -> Albums.Update.t option
    
    val avatar : t -> Avatar.Update.t option
    
    val cast : t -> Cast.Update.t option
    
    val download : t -> Download.Update.t option
    
    val email_notifications : t -> EmailNotifications.Update.t option
    
    val folders : t -> Folders.Update.t option
    
    val memories : t -> Memories.Update.t option
    
    val people : t -> Jsont.json option
    
    val purchase : t -> Purchase.Update.t option
    
    val ratings : t -> Ratings.Update.t option
    
    val shared_links : t -> SharedLinks.Update.t option
    
    val tags : t -> Tags.Update.t option
    
    val jsont : t Jsont.t
  end
  
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : albums:Albums.Response.t -> cast:Cast.Response.t -> download:Download.Response.t -> email_notifications:EmailNotifications.Response.t -> folders:Folders.Response.t -> memories:Memories.Response.t -> people:Jsont.json -> purchase:Purchase.Response.t -> ratings:Ratings.Response.t -> shared_links:SharedLinks.Response.t -> tags:Tags.Response.t -> unit -> t
    
    val albums : t -> Albums.Response.t
    
    val cast : t -> Cast.Response.t
    
    val download : t -> Download.Response.t
    
    val email_notifications : t -> EmailNotifications.Response.t
    
    val folders : t -> Folders.Response.t
    
    val memories : t -> Memories.Response.t
    
    val people : t -> Jsont.json
    
    val purchase : t -> Purchase.Response.t
    
    val ratings : t -> Ratings.Response.t
    
    val shared_links : t -> SharedLinks.Response.t
    
    val tags : t -> Tags.Response.t
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve user preferences
  
      Retrieve the preferences of a specific user. *)
  val get_user_preferences_admin : id:string -> t -> unit -> ResponseDto.t
  
  (** Update user preferences
  
      Update the preferences of a specific user. *)
  val update_user_preferences_admin : id:string -> body:UpdateDto.t -> t -> unit -> ResponseDto.t
  
  (** Get my preferences
  
      Retrieve the preferences for the current user. *)
  val get_my_preferences : t -> unit -> ResponseDto.t
  
  (** Update my preferences
  
      Update the preferences of the current user. *)
  val update_my_preferences : body:UpdateDto.t -> t -> unit -> ResponseDto.t
end

module Person : sig
  module UpdateDto : sig
    type t
    
    (** Construct a value
        @param birth_date Person date of birth.
    Note: the mobile app cannot currently set the birth date to null.
        @param feature_face_asset_id Asset is used to get the feature face thumbnail.
        @param is_hidden Person visibility
        @param name Person name.
    *)
    val v : ?birth_date:string -> ?color:string -> ?feature_face_asset_id:string -> ?is_favorite:bool -> ?is_hidden:bool -> ?name:string -> unit -> t
    
    (** Person date of birth.
    Note: the mobile app cannot currently set the birth date to null. *)
    val birth_date : t -> string option
    
    val color : t -> string option
    
    (** Asset is used to get the feature face thumbnail. *)
    val feature_face_asset_id : t -> string option
    
    val is_favorite : t -> bool option
    
    (** Person visibility *)
    val is_hidden : t -> bool option
    
    (** Person name. *)
    val name : t -> string option
    
    val jsont : t Jsont.t
  end
  
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : id:string -> is_hidden:bool -> name:string -> thumbnail_path:string -> ?birth_date:string -> ?color:string -> ?is_favorite:bool -> ?updated_at:Ptime.t -> unit -> t
    
    val birth_date : t -> string option
    
    val color : t -> string option
    
    val id : t -> string
    
    val is_favorite : t -> bool option
    
    val is_hidden : t -> bool
    
    val name : t -> string
    
    val thumbnail_path : t -> string
    
    val updated_at : t -> Ptime.t option
    
    val jsont : t Jsont.t
  end
  
  module CreateDto : sig
    type t
    
    (** Construct a value
        @param birth_date Person date of birth.
    Note: the mobile app cannot currently set the birth date to null.
        @param is_hidden Person visibility
        @param name Person name.
    *)
    val v : ?birth_date:string -> ?color:string -> ?is_favorite:bool -> ?is_hidden:bool -> ?name:string -> unit -> t
    
    (** Person date of birth.
    Note: the mobile app cannot currently set the birth date to null. *)
    val birth_date : t -> string option
    
    val color : t -> string option
    
    val is_favorite : t -> bool option
    
    (** Person visibility *)
    val is_hidden : t -> bool option
    
    (** Person name. *)
    val name : t -> string option
    
    val jsont : t Jsont.t
  end
  
  (** Re-assign a face to another person
  
      Re-assign the face provided in the body to the person identified by the id in the path parameter. *)
  val reassign_faces_by_id : id:string -> body:Face.Dto.t -> t -> unit -> ResponseDto.t
  
  (** Create a person
  
      Create a new person that can have multiple faces assigned to them. *)
  val create_person : body:CreateDto.t -> t -> unit -> ResponseDto.t
  
  (** Get a person
  
      Retrieve a person by id. *)
  val get_person : id:string -> t -> unit -> ResponseDto.t
  
  (** Update person
  
      Update an individual person. *)
  val update_person : id:string -> body:UpdateDto.t -> t -> unit -> ResponseDto.t
  
  (** Reassign faces
  
      Bulk reassign a list of faces to a different person. *)
  val reassign_faces : id:string -> body:Jsont.json -> t -> unit -> ResponseDto.t
  
  (** Search people
  
      Search for people by name. *)
  val search_person : name:string -> ?with_hidden:string -> t -> unit -> ResponseDto.t
end

module People : sig
  module UpdateDto : sig
    type t
    
    (** Construct a value *)
    val v : people:PeopleUpdate.Item.t list -> unit -> t
    
    val people : t -> PeopleUpdate.Item.t list
    
    val jsont : t Jsont.t
  end
  
  module Update : sig
    type t
    
    (** Construct a value *)
    val v : ?enabled:bool -> ?sidebar_web:bool -> unit -> t
    
    val enabled : t -> bool option
    
    val sidebar_web : t -> bool option
    
    val jsont : t Jsont.t
  end
  
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : hidden:int -> people:Person.ResponseDto.t list -> total:int -> ?has_next_page:bool -> unit -> t
    
    val has_next_page : t -> bool option
    
    val hidden : t -> int
    
    val people : t -> Person.ResponseDto.t list
    
    val total : t -> int
    
    val jsont : t Jsont.t
  end
  
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : ?enabled:bool -> ?sidebar_web:bool -> unit -> t
    
    val enabled : t -> bool
    
    val sidebar_web : t -> bool
    
    val jsont : t Jsont.t
  end
  
  (** Get all people
  
      Retrieve a list of all people. 
      @param page Page number for pagination
      @param size Number of items per page
  *)
  val get_all_people : ?closest_asset_id:string -> ?closest_person_id:string -> ?page:string -> ?size:string -> ?with_hidden:string -> t -> unit -> ResponseDto.t
end

module BulkId : sig
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : id:string -> success:bool -> ?error:string -> unit -> t
    
    val error : t -> string option
    
    val id : t -> string
    
    val success : t -> bool
    
    val jsont : t Jsont.t
  end
  
  (** Add assets to an album
  
      Add multiple assets to a specific album by its ID. *)
  val add_assets_to_album : id:string -> ?key:string -> ?slug:string -> body:BulkIds.Dto.t -> t -> unit -> ResponseDto.t
  
  (** Remove assets from an album
  
      Remove multiple assets from a specific album by its ID. *)
  val remove_asset_from_album : id:string -> t -> unit -> ResponseDto.t
  
  (** Add assets to a memory
  
      Add a list of asset IDs to a specific memory. *)
  val add_memory_assets : id:string -> body:BulkIds.Dto.t -> t -> unit -> ResponseDto.t
  
  (** Remove assets from a memory
  
      Remove a list of asset IDs from a specific memory. *)
  val remove_memory_assets : id:string -> t -> unit -> ResponseDto.t
  
  (** Update people
  
      Bulk update multiple people at once. *)
  val update_people : body:People.UpdateDto.t -> t -> unit -> ResponseDto.t
  
  (** Merge people
  
      Merge a list of people into the person specified in the path parameter. *)
  val merge_person : id:string -> body:MergePerson.Dto.t -> t -> unit -> ResponseDto.t
  
  (** Tag assets
  
      Add a tag to all the specified assets. *)
  val tag_assets : id:string -> body:BulkIds.Dto.t -> t -> unit -> ResponseDto.t
  
  (** Untag assets
  
      Remove a tag from all the specified assets. *)
  val untag_assets : id:string -> t -> unit -> ResponseDto.t
end

module AssetFace : sig
  module UpdateDto : sig
    type t
    
    (** Construct a value *)
    val v : data:AssetFaceUpdate.Item.t list -> unit -> t
    
    val data : t -> AssetFaceUpdate.Item.t list
    
    val jsont : t Jsont.t
  end
  
  module ResponseDto : sig
    type t
    
    (** Construct a value *)
    val v : bounding_box_x1:int -> bounding_box_x2:int -> bounding_box_y1:int -> bounding_box_y2:int -> id:string -> image_height:int -> image_width:int -> person:Person.ResponseDto.t -> ?source_type:Source.Type.t -> unit -> t
    
    val bounding_box_x1 : t -> int
    
    val bounding_box_x2 : t -> int
    
    val bounding_box_y1 : t -> int
    
    val bounding_box_y2 : t -> int
    
    val id : t -> string
    
    val image_height : t -> int
    
    val image_width : t -> int
    
    val person : t -> Person.ResponseDto.t
    
    val source_type : t -> Source.Type.t option
    
    val jsont : t Jsont.t
  end
  
  module CreateDto : sig
    type t
    
    (** Construct a value *)
    val v : asset_id:string -> height:int -> image_height:int -> image_width:int -> person_id:string -> width:int -> x:int -> y:int -> unit -> t
    
    val asset_id : t -> string
    
    val height : t -> int
    
    val image_height : t -> int
    
    val image_width : t -> int
    
    val person_id : t -> string
    
    val width : t -> int
    
    val x : t -> int
    
    val y : t -> int
    
    val jsont : t Jsont.t
  end
  
  (** Retrieve faces for asset
  
      Retrieve all faces belonging to an asset. *)
  val get_faces : id:string -> t -> unit -> ResponseDto.t
end

module Client : sig
  (** Delete an activity
  
      Removes a like or comment from a given album or asset in an album. *)
  val delete_activity : id:string -> t -> unit -> Jsont.json
  
  (** Unlink all OAuth accounts
  
      Unlinks all OAuth accounts associated with user accounts in the system. *)
  val unlink_all_oauth_accounts_admin : t -> unit -> Jsont.json
  
  (** Delete database backup
  
      Delete a backup by its filename *)
  val delete_database_backup : t -> unit -> Jsont.json
  
  (** Start database backup restore flow
  
      Put Immich into maintenance mode to restore a backup (Immich must not be configured) *)
  val start_database_restore_flow : t -> unit -> Jsont.json
  
  (** Upload database backup
  
      Uploads .sql/.sql.gz file to restore backup from *)
  val upload_database_backup : t -> unit -> Jsont.json
  
  (** Download database backup
  
      Downloads the database backup file *)
  val download_database_backup : filename:string -> t -> unit -> Jsont.json
  
  (** Set maintenance mode
  
      Put Immich into or take it out of maintenance mode *)
  val set_maintenance_mode : body:SetMaintenanceMode.Dto.t -> t -> unit -> Jsont.json
  
  (** Delete an album
  
      Delete a specific album by its ID. Note the album is initially trashed and then immediately scheduled for deletion, but relies on a background job to complete the process. *)
  val delete_album : id:string -> t -> unit -> Jsont.json
  
  (** Update user role
  
      Change the role for a specific user in a specific album. *)
  val update_album_user : id:string -> user_id:string -> body:UpdateAlbumUser.Dto.t -> t -> unit -> Jsont.json
  
  (** Remove user from album
  
      Remove a user from an album. Use an ID of "me" to leave a shared album. *)
  val remove_user_from_album : id:string -> user_id:string -> t -> unit -> Jsont.json
  
  (** Delete an API key
  
      Deletes an API key identified by its ID. The current user must own this API key. *)
  val delete_api_key : id:string -> t -> unit -> Jsont.json
  
  (** Update assets
  
      Updates multiple assets at the same time. *)
  val update_assets : body:AssetBulk.UpdateDto.t -> t -> unit -> Jsont.json
  
  (** Delete assets
  
      Deletes multiple assets at the same time. *)
  val delete_assets : t -> unit -> Jsont.json
  
  (** Copy asset
  
      Copy asset information like albums, tags, etc. from one asset to another. *)
  val copy_asset : body:AssetCopy.Dto.t -> t -> unit -> Jsont.json
  
  (** Retrieve assets by device ID
  
      Get all asset of a device that are in the database, ID only. *)
  val get_all_user_assets_by_device_id : device_id:string -> t -> unit -> Jsont.json
  
  (** Run an asset job
  
      Run a specific job on a set of assets. *)
  val run_asset_jobs : body:AssetJobs.Dto.t -> t -> unit -> Jsont.json
  
  (** Delete asset metadata
  
      Delete metadata key-value pairs for multiple assets. *)
  val delete_bulk_asset_metadata : t -> unit -> Jsont.json
  
  (** Remove edits from an existing asset
  
      Removes all edit actions (crop, rotate, mirror) associated with the specified asset. *)
  val remove_asset_edits : id:string -> t -> unit -> Jsont.json
  
  (** Delete asset metadata by key
  
      Delete a specific metadata key-value pair associated with the specified asset. *)
  val delete_asset_metadata : id:string -> key:string -> t -> unit -> Jsont.json
  
  (** Download original asset
  
      Downloads the original file of the specified asset. *)
  val download_asset : id:string -> ?edited:string -> ?key:string -> ?slug:string -> t -> unit -> Jsont.json
  
  (** View asset thumbnail
  
      Retrieve the thumbnail image for the specified asset. Viewing the fullsize thumbnail might redirect to downloadAsset, which requires a different permission. *)
  val view_asset : id:string -> ?edited:string -> ?key:string -> ?size:string -> ?slug:string -> t -> unit -> Jsont.json
  
  (** Play asset video
  
      Streams the video file for the specified asset. This endpoint also supports byte range requests. *)
  val play_asset_video : id:string -> ?key:string -> ?slug:string -> t -> unit -> Jsont.json
  
  (** Setup pin code
  
      Setup a new pin code for the current user. *)
  val setup_pin_code : body:PinCodeSetup.Dto.t -> t -> unit -> Jsont.json
  
  (** Change pin code
  
      Change the pin code for the current user. *)
  val change_pin_code : body:PinCodeChange.Dto.t -> t -> unit -> Jsont.json
  
  (** Reset pin code
  
      Reset the pin code for the current user by providing the account password *)
  val reset_pin_code : t -> unit -> Jsont.json
  
  (** Lock auth session
  
      Remove elevated access to locked assets from the current session. *)
  val lock_auth_session : t -> unit -> Jsont.json
  
  (** Unlock auth session
  
      Temporarily grant the session elevated access to locked assets by providing the correct PIN code. *)
  val unlock_auth_session : body:SessionUnlock.Dto.t -> t -> unit -> Jsont.json
  
  (** Download asset archive
  
      Download a ZIP archive containing the specified assets. The assets must have been previously requested via the "getDownloadInfo" endpoint. *)
  val download_archive : ?key:string -> ?slug:string -> body:AssetIds.Dto.t -> t -> unit -> Jsont.json
  
  (** Delete duplicates
  
      Delete multiple duplicate assets specified by their IDs. *)
  val delete_duplicates : t -> unit -> Jsont.json
  
  (** Delete a duplicate
  
      Delete a single duplicate asset specified by its ID. *)
  val delete_duplicate : id:string -> t -> unit -> Jsont.json
  
  (** Create a face
  
      Create a new face that has not been discovered by facial recognition. The content of the bounding box is considered a face. *)
  val create_face : body:AssetFace.CreateDto.t -> t -> unit -> Jsont.json
  
  (** Delete a face
  
      Delete a face identified by the id. Optionally can be force deleted. *)
  val delete_face : id:string -> t -> unit -> Jsont.json
  
  (** Create a manual job
  
      Run a specific job. Most jobs are queued automatically, but this endpoint allows for manual creation of a handful of jobs, including various cleanup tasks, as well as creating a new database backup. *)
  val create_job : body:Job.CreateDto.t -> t -> unit -> Jsont.json
  
  (** Delete a library
  
      Delete an external library by its ID. *)
  val delete_library : id:string -> t -> unit -> Jsont.json
  
  (** Scan a library
  
      Queue a scan for the external library to find and import new assets. *)
  val scan_library : id:string -> t -> unit -> Jsont.json
  
  (** Delete a memory
  
      Delete a specific memory by its ID. *)
  val delete_memory : id:string -> t -> unit -> Jsont.json
  
  (** Update notifications
  
      Update a list of notifications. Allows to bulk-set the read status of notifications. *)
  val update_notifications : body:NotificationUpdateAll.Dto.t -> t -> unit -> Jsont.json
  
  (** Delete notifications
  
      Delete a list of notifications at once. *)
  val delete_notifications : t -> unit -> Jsont.json
  
  (** Delete a notification
  
      Delete a specific notification. *)
  val delete_notification : id:string -> t -> unit -> Jsont.json
  
  (** Redirect OAuth to mobile
  
      Requests to this URL are automatically forwarded to the mobile app, and is used in some cases for OAuth redirecting. *)
  val redirect_oauth_to_mobile : t -> unit -> Jsont.json
  
  (** Remove a partner
  
      Stop sharing assets with a partner. *)
  val remove_partner : id:string -> t -> unit -> Jsont.json
  
  (** Delete people
  
      Bulk delete a list of people at once. *)
  val delete_people : t -> unit -> Jsont.json
  
  (** Delete person
  
      Delete an individual person. *)
  val delete_person : id:string -> t -> unit -> Jsont.json
  
  (** Get person thumbnail
  
      Retrieve the thumbnail file for a person. *)
  val get_person_thumbnail : id:string -> t -> unit -> Jsont.json
  
  (** Empty a queue
  
      Removes all jobs from the specified queue. *)
  val empty_queue : name:string -> t -> unit -> Jsont.json
  
  (** Retrieve search suggestions
  
      Retrieve search suggestions based on partial input. This endpoint is used for typeahead search features. *)
  val get_search_suggestions : ?country:string -> ?include_null:string -> ?lens_model:string -> ?make:string -> ?model:string -> ?state:string -> type_:string -> t -> unit -> Jsont.json
  
  (** Delete server product key
  
      Delete the currently set server product key. *)
  val delete_server_license : t -> unit -> Jsont.json
  
  (** Delete all sessions
  
      Delete all sessions for the user. This will not delete the current session. *)
  val delete_all_sessions : t -> unit -> Jsont.json
  
  (** Delete a session
  
      Delete a specific session by id. *)
  val delete_session : id:string -> t -> unit -> Jsont.json
  
  (** Lock a session
  
      Lock a specific session by id. *)
  val lock_session : id:string -> t -> unit -> Jsont.json
  
  (** Delete a shared link
  
      Delete a specific shared link by its ID. *)
  val remove_shared_link : id:string -> t -> unit -> Jsont.json
  
  (** Delete stacks
  
      Delete multiple stacks by providing a list of stack IDs. *)
  val delete_stacks : t -> unit -> Jsont.json
  
  (** Delete a stack
  
      Delete a specific stack by its ID. *)
  val delete_stack : id:string -> t -> unit -> Jsont.json
  
  (** Remove an asset from a stack
  
      Remove a specific asset from a stack by providing the stack ID and asset ID. *)
  val remove_asset_from_stack : asset_id:string -> id:string -> t -> unit -> Jsont.json
  
  (** Acknowledge changes
  
      Send a list of synchronization acknowledgements to confirm that the latest changes have been received. *)
  val send_sync_ack : body:SyncAckSet.Dto.t -> t -> unit -> Jsont.json
  
  (** Delete acknowledgements
  
      Delete specific synchronization acknowledgments. *)
  val delete_sync_ack : t -> unit -> Jsont.json
  
  (** Stream sync changes
  
      Retrieve a JSON lines streamed response of changes for synchronization. This endpoint is used by the mobile app to efficiently stay up to date with changes. *)
  val get_sync_stream : body:SyncStream.Dto.t -> t -> unit -> Jsont.json
  
  (** Update admin onboarding
  
      Update the admin onboarding status. *)
  val update_admin_onboarding : body:AdminOnboarding.UpdateDto.t -> t -> unit -> Jsont.json
  
  (** Delete a tag
  
      Delete a specific tag by its ID. *)
  val delete_tag : id:string -> t -> unit -> Jsont.json
  
  (** Delete user product key
  
      Delete the registered product key for the current user. *)
  val delete_user_license : t -> unit -> Jsont.json
  
  (** Delete user onboarding
  
      Delete the onboarding status of the current user. *)
  val delete_user_onboarding : t -> unit -> Jsont.json
  
  (** Delete user profile image
  
      Delete the profile image of the current user. *)
  val delete_profile_image : t -> unit -> Jsont.json
  
  (** Retrieve user profile image
  
      Retrieve the profile image file for a user. *)
  val get_profile_image : id:string -> t -> unit -> Jsont.json
  
  (** Retrieve unique paths
  
      Retrieve a list of unique folder paths from asset original paths. *)
  val get_unique_original_paths : t -> unit -> Jsont.json
  
  (** Delete a workflow
  
      Delete a workflow by its ID. *)
  val delete_workflow : id:string -> t -> unit -> Jsont.json
end
