(* Atp_lexicon_bsky - generated from atproto lexicons *)

(** AT Protocol lexicon types and Jsont codecs for Atp_lexicon_bsky. *)

(** Utility functions for resilient parsing. *)
module Filter : sig
  val filter_list : 'a Jsont.t -> Jsont.json list -> 'a list
  (** [filter_list jsont json_list] parses each element with [jsont],
      returning only successfully parsed elements. Non-compliant records
      are silently skipped. *)
end

module Com : sig
  module Atproto : sig
    module Repo : sig
      module StrongRef : sig

type main = {
  cid : string;
  uri : string;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
    end
    module Moderation : sig
      module Defs : sig
(** Appeal a previously taken moderation action *)

type reason_appeal = string
val reason_appeal_jsont : reason_appeal Jsont.t

(** Misleading identity, affiliation, or content. Prefer new lexicon definition `tools.ozone.report.defs#reasonMisleadingOther`. *)

type reason_misleading = string
val reason_misleading_jsont : reason_misleading Jsont.t

(** Reports not falling under another report category. Prefer new lexicon definition `tools.ozone.report.defs#reasonOther`. *)

type reason_other = string
val reason_other_jsont : reason_other Jsont.t

(** Rude, harassing, explicit, or otherwise unwelcoming behavior. Prefer new lexicon definition `tools.ozone.report.defs#reasonHarassmentOther`. *)

type reason_rude = string
val reason_rude_jsont : reason_rude Jsont.t

(** Unwanted or mislabeled sexual content. Prefer new lexicon definition `tools.ozone.report.defs#reasonSexualUnlabeled`. *)

type reason_sexual = string
val reason_sexual_jsont : reason_sexual Jsont.t

(** Spam: frequent unwanted promotion, replies, mentions. Prefer new lexicon definition `tools.ozone.report.defs#reasonMisleadingSpam`. *)

type reason_spam = string
val reason_spam_jsont : reason_spam Jsont.t


type reason_type = string
val reason_type_jsont : reason_type Jsont.t

(** Direct violation of server rules, laws, terms of service. Prefer new lexicon definition `tools.ozone.report.defs#reasonRuleOther`. *)

type reason_violation = string
val reason_violation_jsont : reason_violation Jsont.t

(** Tag describing a type of subject that might be reported. *)

type subject_type = string
val subject_type_jsont : subject_type Jsont.t

      end
    end
    module Label : sig
      module Defs : sig
(** Metadata tag on an atproto resource (eg, repo or record). *)

type label = {
  cid : string option;  (** Optionally, CID specifying the specific version of 'uri' resource this label applies to. *)
  cts : string;  (** Timestamp when this label was created. *)
  exp : string option;  (** Timestamp at which this label expires (no longer applies). *)
  neg : bool option;  (** If true, this is a negation label, overwriting a previous label. *)
  sig_ : string option;  (** Signature of dag-cbor encoded label. *)
  src : string;  (** DID of the actor who created this label. *)
  uri : string;  (** AT URI of the record, repository (account), or other resource that this label applies to. *)
  val_ : string;  (** The short string name of the value or type of this label. *)
  ver : int option;  (** The AT Protocol version of the label object. *)
}

(** Jsont codec for {!type:label}. *)
val label_jsont : label Jsont.t


type label_value = string
val label_value_jsont : label_value Jsont.t

(** Strings which describe the label in the UI, localized into a specific language. *)

type label_value_definition_strings = {
  description : string;  (** A longer description of what the label means and why it might be applied. *)
  lang : string;  (** The code of the language these strings are written in. *)
  name : string;  (** A short human-readable name for the label. *)
}

(** Jsont codec for {!type:label_value_definition_strings}. *)
val label_value_definition_strings_jsont : label_value_definition_strings Jsont.t

(** Metadata tag on an atproto record, published by the author within the record. Note that schemas should use #selfLabels, not #selfLabel. *)

type self_label = {
  val_ : string;  (** The short string name of the value or type of this label. *)
}

(** Jsont codec for {!type:self_label}. *)
val self_label_jsont : self_label Jsont.t

(** Declares a label value and its expected interpretations and behaviors. *)

type label_value_definition = {
  adult_only : bool option;  (** Does the user need to have adult content enabled in order to configure this label? *)
  blurs : string;  (** What should this label hide in the UI, if applied? 'content' hides all of the target; 'media' hides the images/video/audio; 'none' hides nothing. *)
  default_setting : string option;  (** The default setting for this label. *)
  identifier : string;  (** The value of the label being defined. Must only include lowercase ascii and the '-' character (\[a-z-\]+). *)
  locales : label_value_definition_strings list;
  severity : string;  (** How should a client visually convey this label? 'inform' means neutral and informational; 'alert' means negative and warning; 'none' means show nothing. *)
}

(** Jsont codec for {!type:label_value_definition}. *)
val label_value_definition_jsont : label_value_definition Jsont.t

(** Metadata tags on an atproto record, published by the author within the record. *)

type self_labels = {
  values : self_label list;
}

(** Jsont codec for {!type:self_labels}. *)
val self_labels_jsont : self_labels Jsont.t

      end
    end
  end
end
module App : sig
  module Bsky : sig
    module Video : sig
      module GetUploadLimits : sig
(** Get video upload limits for the authenticated user. *)


type output = {
  can_upload : bool;
  error : string option;
  message : string option;
  remaining_daily_bytes : int option;
  remaining_daily_videos : int option;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Defs : sig

type job_status = {
  blob : Atp.Blob_ref.t option;
  did : string;
  error : string option;
  job_id : string;
  message : string option;
  progress : int option;  (** Progress within the current processing state. *)
  state : string;  (** The state of the video processing job. All values not listed as a known value indicate that the job is in process. *)
}

(** Jsont codec for {!type:job_status}. *)
val job_status_jsont : job_status Jsont.t

      end
      module UploadVideo : sig
(** Upload a video to be processed then stored on the PDS. *)


type input = unit
val input_jsont : input Jsont.t


type output = {
  job_status : Defs.job_status;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetJobStatus : sig
(** Get status details for a video processing job. *)

(** Query/procedure parameters. *)
type params = {
  job_id : string;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  job_status : Defs.job_status;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Richtext : sig
      module Facet : sig
(** Specifies the sub-string range a facet feature applies to. Start index is inclusive, end index is exclusive. Indices are zero-indexed, counting bytes of the UTF-8 encoded text. NOTE: some languages, like Javascript, use UTF-16 or Unicode codepoints for string slice indexing; in these languages, convert to byte arrays before working with facets. *)

type byte_slice = {
  byte_end : int;
  byte_start : int;
}

(** Jsont codec for {!type:byte_slice}. *)
val byte_slice_jsont : byte_slice Jsont.t

(** Facet feature for a URL. The text URL may have been simplified or truncated, but the facet reference should be a complete URL. *)

type link = {
  uri : string;
}

(** Jsont codec for {!type:link}. *)
val link_jsont : link Jsont.t

(** Facet feature for mention of another account. The text is usually a handle, including a '\@' prefix, but the facet reference is a DID. *)

type mention = {
  did : string;
}

(** Jsont codec for {!type:mention}. *)
val mention_jsont : mention Jsont.t

(** Facet feature for a hashtag. The text usually includes a '#' prefix, but the facet reference should not (except in the case of 'double hash tags'). *)

type tag = {
  tag : string;
}

(** Jsont codec for {!type:tag}. *)
val tag_jsont : tag Jsont.t

(** Annotation of a sub-string within rich text. *)

type main = {
  features : Jsont.json list;
  index : byte_slice;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
    end
    module Notification : sig
      module UpdateSeen : sig
(** Notify server that the requesting account has seen notifications. Requires auth. *)


type input = {
  seen_at : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module UnregisterPush : sig
(** The inverse of registerPush - inform a specified service that push notifications should no longer be sent to the given token for the requesting account. Requires auth. *)


type input = {
  app_id : string;
  platform : string;
  service_did : string;
  token : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module RegisterPush : sig
(** Register to receive push notifications, via a specified service, for the requesting account. Requires auth. *)


type input = {
  age_restricted : bool option;  (** Set to true when the actor is age restricted *)
  app_id : string;
  platform : string;
  service_did : string;
  token : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module PutPreferences : sig
(** Set notification-related preferences for an account. Requires auth. *)


type input = {
  priority : bool;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module ListNotifications : sig

type notification = {
  author : Jsont.json;
  cid : string;
  indexed_at : string;
  is_read : bool;
  labels : Com.Atproto.Label.Defs.label list option;
  reason : string;  (** The reason why this notification was delivered - e.g. your post was liked, or you received a new follower. *)
  reason_subject : string option;
  record : Jsont.json;
  uri : string;
}

(** Jsont codec for {!type:notification}. *)
val notification_jsont : notification Jsont.t

(** Enumerate notifications for the requesting account. Requires auth. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;
  limit : int option;
  priority : bool option;
  reasons : string list option;  (** Notification reasons to include in response. *)
  seen_at : string option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  notifications : Jsont.json list;
  priority : bool option;
  seen_at : string option;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module ListActivitySubscriptions : sig
(** Enumerate all accounts to which the requesting account is subscribed to receive notifications for. Requires auth. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  subscriptions : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetUnreadCount : sig
(** Count the number of unread notifications for the requesting account. Requires auth. *)

(** Query/procedure parameters. *)
type params = {
  priority : bool option;
  seen_at : string option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  count : int;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Defs : sig

type activity_subscription = {
  post : bool;
  reply : bool;
}

(** Jsont codec for {!type:activity_subscription}. *)
val activity_subscription_jsont : activity_subscription Jsont.t


type chat_preference = {
  include_ : string;
  push : bool;
}

(** Jsont codec for {!type:chat_preference}. *)
val chat_preference_jsont : chat_preference Jsont.t


type filterable_preference = {
  include_ : string;
  list_ : bool;
  push : bool;
}

(** Jsont codec for {!type:filterable_preference}. *)
val filterable_preference_jsont : filterable_preference Jsont.t


type preference = {
  list_ : bool;
  push : bool;
}

(** Jsont codec for {!type:preference}. *)
val preference_jsont : preference Jsont.t


type record_deleted = unit

(** Jsont codec for {!type:record_deleted}. *)
val record_deleted_jsont : record_deleted Jsont.t


type preferences = {
  chat : Jsont.json;
  follow : Jsont.json;
  like : Jsont.json;
  like_via_repost : Jsont.json;
  mention : Jsont.json;
  quote : Jsont.json;
  reply : Jsont.json;
  repost : Jsont.json;
  repost_via_repost : Jsont.json;
  starterpack_joined : Jsont.json;
  subscribed_post : Jsont.json;
  unverified : Jsont.json;
  verified : Jsont.json;
}

(** Jsont codec for {!type:preferences}. *)
val preferences_jsont : preferences Jsont.t

(** Object used to store activity subscription data in stash. *)

type subject_activity_subscription = {
  activity_subscription : Jsont.json;
  subject : string;
}

(** Jsont codec for {!type:subject_activity_subscription}. *)
val subject_activity_subscription_jsont : subject_activity_subscription Jsont.t

      end
      module Declaration : sig
(** A declaration of the user's choices related to notifications that can be produced by them. *)

type main = {
  allow_subscriptions : string;  (** A declaration of the user's preference for allowing activity subscriptions from other users. Absence of a record implies 'followers'. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module PutPreferencesV2 : sig
(** Set notification-related preferences for an account. Requires auth. *)


type input = {
  chat : Jsont.json option;
  follow : Jsont.json option;
  like : Jsont.json option;
  like_via_repost : Jsont.json option;
  mention : Jsont.json option;
  quote : Jsont.json option;
  reply : Jsont.json option;
  repost : Jsont.json option;
  repost_via_repost : Jsont.json option;
  starterpack_joined : Jsont.json option;
  subscribed_post : Jsont.json option;
  unverified : Jsont.json option;
  verified : Jsont.json option;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  preferences : Jsont.json;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module PutActivitySubscription : sig
(** Puts an activity subscription entry. The key should be omitted for creation and provided for updates. Requires auth. *)


type input = {
  activity_subscription : Jsont.json;
  subject : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  activity_subscription : Jsont.json option;
  subject : string;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetPreferences : sig
(** Get notification-related preferences for an account. Requires auth. *)

(** Query/procedure parameters. *)
type params = unit

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  preferences : Jsont.json;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Labeler : sig
      module Defs : sig

type labeler_policies = {
  label_value_definitions : Com.Atproto.Label.Defs.label_value_definition list option;  (** Label values created by this labeler and scoped exclusively to it. Labels defined here will override global label definitions for this labeler. *)
  label_values : Com.Atproto.Label.Defs.label_value list;  (** The label values which this labeler publishes. May include global or custom labels. *)
}

(** Jsont codec for {!type:labeler_policies}. *)
val labeler_policies_jsont : labeler_policies Jsont.t


type labeler_viewer_state = {
  like : string option;
}

(** Jsont codec for {!type:labeler_viewer_state}. *)
val labeler_viewer_state_jsont : labeler_viewer_state Jsont.t


type labeler_view = {
  cid : string;
  creator : Jsont.json;
  indexed_at : string;
  labels : Com.Atproto.Label.Defs.label list option;
  like_count : int option;
  uri : string;
  viewer : Jsont.json option;
}

(** Jsont codec for {!type:labeler_view}. *)
val labeler_view_jsont : labeler_view Jsont.t


type labeler_view_detailed = {
  cid : string;
  creator : Jsont.json;
  indexed_at : string;
  labels : Com.Atproto.Label.Defs.label list option;
  like_count : int option;
  policies : Jsont.json;
  reason_types : Com.Atproto.Moderation.Defs.reason_type list option;  (** The set of report reason 'codes' which are in-scope for this service to review and action. These usually align to policy categories. If not defined (distinct from empty array), all reason types are allowed. *)
  subject_collections : string list option;  (** Set of record types (collection NSIDs) which can be reported to this service. If not defined (distinct from empty array), default is any record type. *)
  subject_types : Com.Atproto.Moderation.Defs.subject_type list option;  (** The set of subject types (account, record, etc) this service accepts reports on. *)
  uri : string;
  viewer : Jsont.json option;
}

(** Jsont codec for {!type:labeler_view_detailed}. *)
val labeler_view_detailed_jsont : labeler_view_detailed Jsont.t

      end
      module Service : sig
(** A declaration of the existence of labeler service. *)

type main = {
  created_at : string;
  labels : Com.Atproto.Label.Defs.self_labels option;
  policies : Jsont.json;
  reason_types : Com.Atproto.Moderation.Defs.reason_type list option;  (** The set of report reason 'codes' which are in-scope for this service to review and action. These usually align to policy categories. If not defined (distinct from empty array), all reason types are allowed. *)
  subject_collections : string list option;  (** Set of record types (collection NSIDs) which can be reported to this service. If not defined (distinct from empty array), default is any record type. *)
  subject_types : Com.Atproto.Moderation.Defs.subject_type list option;  (** The set of subject types (account, record, etc) this service accepts reports on. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module GetServices : sig
(** Get information about a list of labeler services. *)

(** Query/procedure parameters. *)
type params = {
  detailed : bool option;
  dids : string list;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  views : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Embed : sig
      module External : sig

type external_ = {
  description : string;
  thumb : Atp.Blob_ref.t option;
  title : string;
  uri : string;
}

(** Jsont codec for {!type:external_}. *)
val external__jsont : external_ Jsont.t


type view_external = {
  description : string;
  thumb : string option;
  title : string;
  uri : string;
}

(** Jsont codec for {!type:view_external}. *)
val view_external_jsont : view_external Jsont.t

(** A representation of some externally linked content (eg, a URL and 'card'), embedded in a Bluesky record (eg, a post). *)

type main = {
  external_ : Jsont.json;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t


type view = {
  external_ : Jsont.json;
}

(** Jsont codec for {!type:view}. *)
val view_jsont : view Jsont.t

      end
      module Defs : sig
(** width:height represents an aspect ratio. It may be approximate, and may not correspond to absolute dimensions in any given unit. *)

type aspect_ratio = {
  height : int;
  width : int;
}

(** Jsont codec for {!type:aspect_ratio}. *)
val aspect_ratio_jsont : aspect_ratio Jsont.t

      end
      module Video : sig

type caption = {
  file : Atp.Blob_ref.t;
  lang : string;
}

(** Jsont codec for {!type:caption}. *)
val caption_jsont : caption Jsont.t


type view = {
  alt : string option;
  aspect_ratio : Jsont.json option;
  cid : string;
  playlist : string;
  thumbnail : string option;
}

(** Jsont codec for {!type:view}. *)
val view_jsont : view Jsont.t


type main = {
  alt : string option;  (** Alt text description of the video, for accessibility. *)
  aspect_ratio : Jsont.json option;
  captions : Jsont.json list option;
  video : Atp.Blob_ref.t;  (** The mp4 video file. May be up to 100mb, formerly limited to 50mb. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Images : sig

type image = {
  alt : string;  (** Alt text description of the image, for accessibility. *)
  aspect_ratio : Jsont.json option;
  image : Atp.Blob_ref.t;
}

(** Jsont codec for {!type:image}. *)
val image_jsont : image Jsont.t


type view_image = {
  alt : string;  (** Alt text description of the image, for accessibility. *)
  aspect_ratio : Jsont.json option;
  fullsize : string;  (** Fully-qualified URL where a large version of the image can be fetched. May or may not be the exact original blob. For example, CDN location provided by the App View. *)
  thumb : string;  (** Fully-qualified URL where a thumbnail of the image can be fetched. For example, CDN location provided by the App View. *)
}

(** Jsont codec for {!type:view_image}. *)
val view_image_jsont : view_image Jsont.t


type main = {
  images : Jsont.json list;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t


type view = {
  images : Jsont.json list;
}

(** Jsont codec for {!type:view}. *)
val view_jsont : view Jsont.t

      end
      module RecordWithMedia : sig

type main = {
  media : Jsont.json;
  record : Jsont.json;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t


type view = {
  media : Jsont.json;
  record : Jsont.json;
}

(** Jsont codec for {!type:view}. *)
val view_jsont : view Jsont.t

      end
      module Record : sig

type main = {
  record : Com.Atproto.Repo.StrongRef.main;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t


type view = {
  record : Jsont.json;
}

(** Jsont codec for {!type:view}. *)
val view_jsont : view Jsont.t


type view_blocked = {
  author : Jsont.json;
  blocked : bool;
  uri : string;
}

(** Jsont codec for {!type:view_blocked}. *)
val view_blocked_jsont : view_blocked Jsont.t


type view_detached = {
  detached : bool;
  uri : string;
}

(** Jsont codec for {!type:view_detached}. *)
val view_detached_jsont : view_detached Jsont.t


type view_not_found = {
  not_found : bool;
  uri : string;
}

(** Jsont codec for {!type:view_not_found}. *)
val view_not_found_jsont : view_not_found Jsont.t


type view_record = {
  author : Jsont.json;
  cid : string;
  embeds : Jsont.json list option;
  indexed_at : string;
  labels : Com.Atproto.Label.Defs.label list option;
  like_count : int option;
  quote_count : int option;
  reply_count : int option;
  repost_count : int option;
  uri : string;
  value : Jsont.json;  (** The record data itself. *)
}

(** Jsont codec for {!type:view_record}. *)
val view_record_jsont : view_record Jsont.t

      end
    end
    module AuthViewAll : sig

type main = unit
val main_jsont : main Jsont.t

    end
    module AuthManageProfile : sig

type main = unit
val main_jsont : main Jsont.t

    end
    module AuthManageNotifications : sig

type main = unit
val main_jsont : main Jsont.t

    end
    module AuthManageModeration : sig

type main = unit
val main_jsont : main Jsont.t

    end
    module AuthManageLabelerService : sig

type main = unit
val main_jsont : main Jsont.t

    end
    module AuthManageFeedDeclarations : sig

type main = unit
val main_jsont : main Jsont.t

    end
    module AuthFullApp : sig

type main = unit
val main_jsont : main Jsont.t

    end
    module AuthCreatePosts : sig

type main = unit
val main_jsont : main Jsont.t

    end
    module Ageassurance : sig
      module Defs : sig
(** The access level granted based on Age Assurance data we've processed. *)

type access = string
val access_jsont : access Jsont.t

(** The Age Assurance configuration for a specific region. *)

type config_region = {
  country_code : string;  (** The ISO 3166-1 alpha-2 country code this configuration applies to. *)
  min_access_age : int;  (** The minimum age (as a whole integer) required to use Bluesky in this region. *)
  region_code : string option;  (** The ISO 3166-2 region code this configuration applies to. If omitted, the configuration applies to the entire country. *)
  rules : Jsont.json list;  (** The ordered list of Age Assurance rules that apply to this region. Rules should be applied in order, and the first matching rule determines the access level granted. The rules array should always include a default rule as the last item. *)
}

(** Jsont codec for {!type:config_region}. *)
val config_region_jsont : config_region Jsont.t

(** Object used to store Age Assurance data in stash. *)

type event = {
  access : string;  (** The access level granted based on Age Assurance data we've processed. *)
  attempt_id : string;  (** The unique identifier for this instance of the Age Assurance flow, in UUID format. *)
  complete_ip : string option;  (** The IP address used when completing the Age Assurance flow. *)
  complete_ua : string option;  (** The user agent used when completing the Age Assurance flow. *)
  country_code : string;  (** The ISO 3166-1 alpha-2 country code provided when beginning the Age Assurance flow. *)
  created_at : string;  (** The date and time of this write operation. *)
  email : string option;  (** The email used for Age Assurance. *)
  init_ip : string option;  (** The IP address used when initiating the Age Assurance flow. *)
  init_ua : string option;  (** The user agent used when initiating the Age Assurance flow. *)
  region_code : string option;  (** The ISO 3166-2 region code provided when beginning the Age Assurance flow. *)
  status : string;  (** The status of the Age Assurance process. *)
}

(** Jsont codec for {!type:event}. *)
val event_jsont : event Jsont.t

(** Additional metadata needed to compute Age Assurance state client-side. *)

type state_metadata = {
  account_created_at : string option;  (** The account creation timestamp. *)
}

(** Jsont codec for {!type:state_metadata}. *)
val state_metadata_jsont : state_metadata Jsont.t

(** The status of the Age Assurance process. *)

type status = string
val status_jsont : status Jsont.t


type config = {
  regions : config_region list;  (** The per-region Age Assurance configuration. *)
}

(** Jsont codec for {!type:config}. *)
val config_jsont : config Jsont.t

(** Age Assurance rule that applies by default. *)

type config_region_rule_default = {
  access : access;
}

(** Jsont codec for {!type:config_region_rule_default}. *)
val config_region_rule_default_jsont : config_region_rule_default Jsont.t

(** Age Assurance rule that applies if the account is equal-to or newer than a certain date. *)

type config_region_rule_if_account_newer_than = {
  access : access;
  date : string;  (** The date threshold as a datetime string. *)
}

(** Jsont codec for {!type:config_region_rule_if_account_newer_than}. *)
val config_region_rule_if_account_newer_than_jsont : config_region_rule_if_account_newer_than Jsont.t

(** Age Assurance rule that applies if the account is older than a certain date. *)

type config_region_rule_if_account_older_than = {
  access : access;
  date : string;  (** The date threshold as a datetime string. *)
}

(** Jsont codec for {!type:config_region_rule_if_account_older_than}. *)
val config_region_rule_if_account_older_than_jsont : config_region_rule_if_account_older_than Jsont.t

(** Age Assurance rule that applies if the user has been assured to be equal-to or over a certain age. *)

type config_region_rule_if_assured_over_age = {
  access : access;
  age : int;  (** The age threshold as a whole integer. *)
}

(** Jsont codec for {!type:config_region_rule_if_assured_over_age}. *)
val config_region_rule_if_assured_over_age_jsont : config_region_rule_if_assured_over_age Jsont.t

(** Age Assurance rule that applies if the user has been assured to be under a certain age. *)

type config_region_rule_if_assured_under_age = {
  access : access;
  age : int;  (** The age threshold as a whole integer. *)
}

(** Jsont codec for {!type:config_region_rule_if_assured_under_age}. *)
val config_region_rule_if_assured_under_age_jsont : config_region_rule_if_assured_under_age Jsont.t

(** Age Assurance rule that applies if the user has declared themselves equal-to or over a certain age. *)

type config_region_rule_if_declared_over_age = {
  access : access;
  age : int;  (** The age threshold as a whole integer. *)
}

(** Jsont codec for {!type:config_region_rule_if_declared_over_age}. *)
val config_region_rule_if_declared_over_age_jsont : config_region_rule_if_declared_over_age Jsont.t

(** Age Assurance rule that applies if the user has declared themselves under a certain age. *)

type config_region_rule_if_declared_under_age = {
  access : access;
  age : int;  (** The age threshold as a whole integer. *)
}

(** Jsont codec for {!type:config_region_rule_if_declared_under_age}. *)
val config_region_rule_if_declared_under_age_jsont : config_region_rule_if_declared_under_age Jsont.t

(** The user's computed Age Assurance state. *)

type state = {
  access : access;
  last_initiated_at : string option;  (** The timestamp when this state was last updated. *)
  status : status;
}

(** Jsont codec for {!type:state}. *)
val state_jsont : state Jsont.t

      end
      module GetState : sig
(** Returns server-computed Age Assurance state, if available, and any additional metadata needed to compute Age Assurance state client-side. *)

(** Query/procedure parameters. *)
type params = {
  country_code : string;
  region_code : string option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  metadata : Defs.state_metadata;
  state : Defs.state;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetConfig : sig
(** Returns Age Assurance configuration for use on the client. *)


type output = Defs.config

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Begin : sig
(** Initiate Age Assurance for an account. *)


type input = {
  country_code : string;  (** An ISO 3166-1 alpha-2 code of the user's location. *)
  email : string;  (** The user's email address to receive Age Assurance instructions. *)
  language : string;  (** The user's preferred language for communication during the Age Assurance process. *)
  region_code : string option;  (** An optional ISO 3166-2 code of the user's region or state within the country. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = Defs.state

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Actor : sig
      module Status : sig
(** Advertises an account as currently offering live content. *)

type live = string
val live_jsont : live Jsont.t

(** A declaration of a Bluesky account status. *)

type main = {
  created_at : string;
  duration_minutes : int option;  (** The duration of the status in minutes. Applications can choose to impose minimum and maximum limits. *)
  embed : Jsont.json option;  (** An optional embed associated with the status. *)
  status : string;  (** The status for the account. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Profile : sig
(** A declaration of a Bluesky account profile. *)

type main = {
  avatar : Atp.Blob_ref.t option;  (** Small image to be displayed next to posts from account. AKA, 'profile picture' *)
  banner : Atp.Blob_ref.t option;  (** Larger horizontal image to display behind profile view. *)
  created_at : string option;
  description : string option;  (** Free-form profile description text. *)
  display_name : string option;
  joined_via_starter_pack : Com.Atproto.Repo.StrongRef.main option;
  labels : Com.Atproto.Label.Defs.self_labels option;  (** Self-label values, specific to the Bluesky application, on the overall account. *)
  pinned_post : Com.Atproto.Repo.StrongRef.main option;
  pronouns : string option;  (** Free-form pronouns text. *)
  website : string option;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Defs : sig

type adult_content_pref = {
  enabled : bool;
}

(** Jsont codec for {!type:adult_content_pref}. *)
val adult_content_pref_jsont : adult_content_pref Jsont.t

(** If set, an active progress guide. Once completed, can be set to undefined. Should have unspecced fields tracking progress. *)

type bsky_app_progress_guide = {
  guide : string;
}

(** Jsont codec for {!type:bsky_app_progress_guide}. *)
val bsky_app_progress_guide_jsont : bsky_app_progress_guide Jsont.t


type content_label_pref = {
  label : string;
  labeler_did : string option;  (** Which labeler does this preference apply to? If undefined, applies globally. *)
  visibility : string;
}

(** Jsont codec for {!type:content_label_pref}. *)
val content_label_pref_jsont : content_label_pref Jsont.t

(** Read-only preference containing value(s) inferred from the user's declared birthdate. Absence of this preference object in the response indicates that the user has not made a declaration. *)

type declared_age_pref = {
  is_over_age13 : bool option;  (** Indicates if the user has declared that they are over 13 years of age. *)
  is_over_age16 : bool option;  (** Indicates if the user has declared that they are over 16 years of age. *)
  is_over_age18 : bool option;  (** Indicates if the user has declared that they are over 18 years of age. *)
}

(** Jsont codec for {!type:declared_age_pref}. *)
val declared_age_pref_jsont : declared_age_pref Jsont.t


type feed_view_pref = {
  feed : string;  (** The URI of the feed, or an identifier which describes the feed. *)
  hide_quote_posts : bool option;  (** Hide quote posts in the feed. *)
  hide_replies : bool option;  (** Hide replies in the feed. *)
  hide_replies_by_like_count : int option;  (** Hide replies in the feed if they do not have this number of likes. *)
  hide_replies_by_unfollowed : bool option;  (** Hide replies in the feed if they are not by followed users. *)
  hide_reposts : bool option;  (** Hide reposts in the feed. *)
}

(** Jsont codec for {!type:feed_view_pref}. *)
val feed_view_pref_jsont : feed_view_pref Jsont.t


type hidden_posts_pref = {
  items : string list;  (** A list of URIs of posts the account owner has hidden. *)
}

(** Jsont codec for {!type:hidden_posts_pref}. *)
val hidden_posts_pref_jsont : hidden_posts_pref Jsont.t


type interests_pref = {
  tags : string list;  (** A list of tags which describe the account owner's interests gathered during onboarding. *)
}

(** Jsont codec for {!type:interests_pref}. *)
val interests_pref_jsont : interests_pref Jsont.t


type labeler_pref_item = {
  did : string;
}

(** Jsont codec for {!type:labeler_pref_item}. *)
val labeler_pref_item_jsont : labeler_pref_item Jsont.t


type muted_word_target = string
val muted_word_target_jsont : muted_word_target Jsont.t

(** A new user experiences (NUX) storage object *)

type nux = {
  completed : bool;
  data : string option;  (** Arbitrary data for the NUX. The structure is defined by the NUX itself. Limited to 300 characters. *)
  expires_at : string option;  (** The date and time at which the NUX will expire and should be considered completed. *)
  id : string;
}

(** Jsont codec for {!type:nux}. *)
val nux_jsont : nux Jsont.t


type personal_details_pref = {
  birth_date : string option;  (** The birth date of account owner. *)
}

(** Jsont codec for {!type:personal_details_pref}. *)
val personal_details_pref_jsont : personal_details_pref Jsont.t

(** Default post interaction settings for the account. These values should be applied as default values when creating new posts. These refs should mirror the threadgate and postgate records exactly. *)

type post_interaction_settings_pref = {
  postgate_embedding_rules : Jsont.json list option;  (** Matches postgate record. List of rules defining who can embed this users posts. If value is an empty array or is undefined, no particular rules apply and anyone can embed. *)
  threadgate_allow_rules : Jsont.json list option;  (** Matches threadgate record. List of rules defining who can reply to this users posts. If value is an empty array, no one can reply. If value is undefined, anyone can reply. *)
}

(** Jsont codec for {!type:post_interaction_settings_pref}. *)
val post_interaction_settings_pref_jsont : post_interaction_settings_pref Jsont.t


type preferences = Jsont.json list
val preferences_jsont : preferences Jsont.t


type profile_associated_activity_subscription = {
  allow_subscriptions : string;
}

(** Jsont codec for {!type:profile_associated_activity_subscription}. *)
val profile_associated_activity_subscription_jsont : profile_associated_activity_subscription Jsont.t


type profile_associated_chat = {
  allow_incoming : string;
}

(** Jsont codec for {!type:profile_associated_chat}. *)
val profile_associated_chat_jsont : profile_associated_chat Jsont.t


type saved_feed = {
  id : string;
  pinned : bool;
  type_ : string;
  value : string;
}

(** Jsont codec for {!type:saved_feed}. *)
val saved_feed_jsont : saved_feed Jsont.t


type saved_feeds_pref = {
  pinned : string list;
  saved : string list;
  timeline_index : int option;
}

(** Jsont codec for {!type:saved_feeds_pref}. *)
val saved_feeds_pref_jsont : saved_feeds_pref Jsont.t


type status_view = {
  cid : string option;
  embed : Jsont.json option;  (** An optional embed associated with the status. *)
  expires_at : string option;  (** The date when this status will expire. The application might choose to no longer return the status after expiration. *)
  is_active : bool option;  (** True if the status is not expired, false if it is expired. Only present if expiration was set. *)
  is_disabled : bool option;  (** True if the user's go-live access has been disabled by a moderator, false otherwise. *)
  record : Jsont.json;
  status : string;  (** The status for the account. *)
  uri : string option;
}

(** Jsont codec for {!type:status_view}. *)
val status_view_jsont : status_view Jsont.t


type thread_view_pref = {
  sort : string option;  (** Sorting mode for threads. *)
}

(** Jsont codec for {!type:thread_view_pref}. *)
val thread_view_pref_jsont : thread_view_pref Jsont.t

(** Preferences for how verified accounts appear in the app. *)

type verification_prefs = {
  hide_badges : bool option;  (** Hide the blue check badges for verified accounts and trusted verifiers. *)
}

(** Jsont codec for {!type:verification_prefs}. *)
val verification_prefs_jsont : verification_prefs Jsont.t

(** An individual verification for an associated subject. *)

type verification_view = {
  created_at : string;  (** Timestamp when the verification was created. *)
  is_valid : bool;  (** True if the verification passes validation, otherwise false. *)
  issuer : string;  (** The user who issued this verification. *)
  uri : string;  (** The AT-URI of the verification record. *)
}

(** Jsont codec for {!type:verification_view}. *)
val verification_view_jsont : verification_view Jsont.t

(** A grab bag of state that's specific to the bsky.app program. Third-party apps shouldn't use this. *)

type bsky_app_state_pref = {
  active_progress_guide : Jsont.json option;
  nuxs : Jsont.json list option;  (** Storage for NUXs the user has encountered. *)
  queued_nudges : string list option;  (** An array of tokens which identify nudges (modals, popups, tours, highlight dots) that should be shown to the user. *)
}

(** Jsont codec for {!type:bsky_app_state_pref}. *)
val bsky_app_state_pref_jsont : bsky_app_state_pref Jsont.t


type labelers_pref = {
  labelers : Jsont.json list;
}

(** Jsont codec for {!type:labelers_pref}. *)
val labelers_pref_jsont : labelers_pref Jsont.t

(** A word that the account owner has muted. *)

type muted_word = {
  actor_target : string option;  (** Groups of users to apply the muted word to. If undefined, applies to all users. *)
  expires_at : string option;  (** The date and time at which the muted word will expire and no longer be applied. *)
  id : string option;
  targets : Jsont.json list;  (** The intended targets of the muted word. *)
  value : string;  (** The muted word itself. *)
}

(** Jsont codec for {!type:muted_word}. *)
val muted_word_jsont : muted_word Jsont.t


type profile_associated = {
  activity_subscription : Jsont.json option;
  chat : Jsont.json option;
  feedgens : int option;
  labeler : bool option;
  lists : int option;
  starter_packs : int option;
}

(** Jsont codec for {!type:profile_associated}. *)
val profile_associated_jsont : profile_associated Jsont.t


type saved_feeds_pref_v2 = {
  items : Jsont.json list;
}

(** Jsont codec for {!type:saved_feeds_pref_v2}. *)
val saved_feeds_pref_v2_jsont : saved_feeds_pref_v2 Jsont.t

(** Represents the verification information about the user this object is attached to. *)

type verification_state = {
  trusted_verifier_status : string;  (** The user's status as a trusted verifier. *)
  verifications : Jsont.json list;  (** All verifications issued by trusted verifiers on behalf of this user. Verifications by untrusted verifiers are not included. *)
  verified_status : string;  (** The user's status as a verified account. *)
}

(** Jsont codec for {!type:verification_state}. *)
val verification_state_jsont : verification_state Jsont.t


type muted_words_pref = {
  items : Jsont.json list;  (** A list of words the account owner has muted. *)
}

(** Jsont codec for {!type:muted_words_pref}. *)
val muted_words_pref_jsont : muted_words_pref Jsont.t

(** The subject's followers whom you also follow *)

type known_followers = {
  count : int;
  followers : Jsont.json list;
}

(** Jsont codec for {!type:known_followers}. *)
val known_followers_jsont : known_followers Jsont.t


type profile_view = {
  associated : Jsont.json option;
  avatar : string option;
  created_at : string option;
  debug : Jsont.json option;  (** Debug information for internal development *)
  description : string option;
  did : string;
  display_name : string option;
  handle : string;
  indexed_at : string option;
  labels : Com.Atproto.Label.Defs.label list option;
  pronouns : string option;
  status : Jsont.json option;
  verification : Jsont.json option;
  viewer : Jsont.json option;
}

(** Jsont codec for {!type:profile_view}. *)
val profile_view_jsont : profile_view Jsont.t


type profile_view_basic = {
  associated : Jsont.json option;
  avatar : string option;
  created_at : string option;
  debug : Jsont.json option;  (** Debug information for internal development *)
  did : string;
  display_name : string option;
  handle : string;
  labels : Com.Atproto.Label.Defs.label list option;
  pronouns : string option;
  status : Jsont.json option;
  verification : Jsont.json option;
  viewer : Jsont.json option;
}

(** Jsont codec for {!type:profile_view_basic}. *)
val profile_view_basic_jsont : profile_view_basic Jsont.t


type profile_view_detailed = {
  associated : Jsont.json option;
  avatar : string option;
  banner : string option;
  created_at : string option;
  debug : Jsont.json option;  (** Debug information for internal development *)
  description : string option;
  did : string;
  display_name : string option;
  followers_count : int option;
  follows_count : int option;
  handle : string;
  indexed_at : string option;
  joined_via_starter_pack : Jsont.json option;
  labels : Com.Atproto.Label.Defs.label list option;
  pinned_post : Com.Atproto.Repo.StrongRef.main option;
  posts_count : int option;
  pronouns : string option;
  status : Jsont.json option;
  verification : Jsont.json option;
  viewer : Jsont.json option;
  website : string option;
}

(** Jsont codec for {!type:profile_view_detailed}. *)
val profile_view_detailed_jsont : profile_view_detailed Jsont.t

(** Metadata about the requesting account's relationship with the subject account. Only has meaningful content for authed requests. *)

type viewer_state = {
  activity_subscription : Jsont.json option;  (** This property is present only in selected cases, as an optimization. *)
  blocked_by : bool option;
  blocking : string option;
  blocking_by_list : Jsont.json option;
  followed_by : string option;
  following : string option;
  known_followers : Jsont.json option;  (** This property is present only in selected cases, as an optimization. *)
  muted : bool option;
  muted_by_list : Jsont.json option;
}

(** Jsont codec for {!type:viewer_state}. *)
val viewer_state_jsont : viewer_state Jsont.t

      end
      module SearchActorsTypeahead : sig
(** Find actor suggestions for a prefix search term. Expected use is for auto-completion during text field entry. Does not require auth. *)

(** Query/procedure parameters. *)
type params = {
  limit : int option;
  q : string option;  (** Search query prefix; not a full query string. *)
  term : string option;  (** DEPRECATED: use 'q' instead. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  actors : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module SearchActors : sig
(** Find actors (profiles) matching search criteria. Does not require auth. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;
  limit : int option;
  q : string option;  (** Search query string. Syntax, phrase, boolean, and faceting is unspecified, but Lucene query syntax is recommended. *)
  term : string option;  (** DEPRECATED: use 'q' instead. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  actors : Jsont.json list;
  cursor : string option;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module PutPreferences : sig
(** Set the private preferences attached to the account. *)


type input = {
  preferences : Jsont.json;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module GetSuggestions : sig
(** Get a list of suggested actors. Expected use is discovery of accounts to follow during new account onboarding. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  actors : Jsont.json list;
  cursor : string option;
  rec_id : int option;  (** Snowflake for this recommendation, use when submitting recommendation events. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetProfiles : sig
(** Get detailed profile views of multiple actors. *)

(** Query/procedure parameters. *)
type params = {
  actors : string list;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  profiles : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetProfile : sig
(** Get detailed profile view of an actor. Does not require auth, but contains relevant metadata with auth. *)

(** Query/procedure parameters. *)
type params = {
  actor : string;  (** Handle or DID of account to fetch profile of. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = Jsont.json

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetPreferences : sig
(** Get private preferences attached to the current account. Expected use is synchronization between multiple devices, and import/export during account migration. Requires auth. *)

(** Query/procedure parameters. *)
type params = unit

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  preferences : Jsont.json;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Graph : sig
      module Verification : sig
(** Record declaring a verification relationship between two accounts. Verifications are only considered valid by an app if issued by an account the app considers trusted. *)

type main = {
  created_at : string;  (** Date of when the verification was created. *)
  display_name : string;  (** Display name of the subject the verification applies to at the moment of verifying, which might not be the same at the time of viewing. The verification is only valid if the current displayName matches the one at the time of verifying. *)
  handle : string;  (** Handle of the subject the verification applies to at the moment of verifying, which might not be the same at the time of viewing. The verification is only valid if the current handle matches the one at the time of verifying. *)
  subject : string;  (** DID of the subject the verification applies to. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module UnmuteThread : sig
(** Unmutes the specified thread. Requires auth. *)


type input = {
  root : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module UnmuteActorList : sig
(** Unmutes the specified list of accounts. Requires auth. *)


type input = {
  list_ : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module UnmuteActor : sig
(** Unmutes the specified account. Requires auth. *)


type input = {
  actor : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module Starterpack : sig

type feed_item = {
  uri : string;
}

(** Jsont codec for {!type:feed_item}. *)
val feed_item_jsont : feed_item Jsont.t

(** Record defining a starter pack of actors and feeds for new users. *)

type main = {
  created_at : string;
  description : string option;
  description_facets : Richtext.Facet.main list option;
  feeds : Jsont.json list option;
  list_ : string;  (** Reference (AT-URI) to the list record. *)
  name : string;  (** Display name for starter pack; can not be empty. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module MuteThread : sig
(** Mutes a thread preventing notifications from the thread and any of its children. Mutes are private in Bluesky. Requires auth. *)


type input = {
  root : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module MuteActorList : sig
(** Creates a mute relationship for the specified list of accounts. Mutes are private in Bluesky. Requires auth. *)


type input = {
  list_ : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module MuteActor : sig
(** Creates a mute relationship for the specified account. Mutes are private in Bluesky. Requires auth. *)


type input = {
  actor : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module Listitem : sig
(** Record representing an account's inclusion on a specific list. The AppView will ignore duplicate listitem records. *)

type main = {
  created_at : string;
  list_ : string;  (** Reference (AT-URI) to the list record (app.bsky.graph.list). *)
  subject : string;  (** The account which is included on the list. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Listblock : sig
(** Record representing a block relationship against an entire an entire list of accounts (actors). *)

type main = {
  created_at : string;
  subject : string;  (** Reference (AT-URI) to the mod list record. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module GetSuggestedFollowsByActor : sig
(** Enumerates follows similar to a given account (actor). Expected use is to recommend additional accounts immediately after following one account. *)

(** Query/procedure parameters. *)
type params = {
  actor : string;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  is_fallback : bool option;  (** If true, response has fallen-back to generic results, and is not scoped using relativeToDid *)
  rec_id : int option;  (** Snowflake for this recommendation, use when submitting recommendation events. *)
  suggestions : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetMutes : sig
(** Enumerates accounts that the requesting account (actor) currently has muted. Requires auth. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  mutes : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetKnownFollowers : sig
(** Enumerates accounts which follow a specified account (actor) and are followed by the viewer. *)

(** Query/procedure parameters. *)
type params = {
  actor : string;
  cursor : string option;
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  followers : Jsont.json list;
  subject : Jsont.json;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetFollows : sig
(** Enumerates accounts which a specified account (actor) follows. *)

(** Query/procedure parameters. *)
type params = {
  actor : string;
  cursor : string option;
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  follows : Jsont.json list;
  subject : Jsont.json;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetFollowers : sig
(** Enumerates accounts which follow a specified account (actor). *)

(** Query/procedure parameters. *)
type params = {
  actor : string;
  cursor : string option;
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  followers : Jsont.json list;
  subject : Jsont.json;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetBlocks : sig
(** Enumerates which accounts the requesting account is currently blocking. Requires auth. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  blocks : Jsont.json list;
  cursor : string option;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Follow : sig
(** Record declaring a social 'follow' relationship of another account. Duplicate follows will be ignored by the AppView. *)

type main = {
  created_at : string;
  subject : string;
  via : Com.Atproto.Repo.StrongRef.main option;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Defs : sig
(** A list of actors used for curation purposes such as list feeds or interaction gating. *)

type curatelist = string
val curatelist_jsont : curatelist Jsont.t


type list_item_view = {
  subject : Jsont.json;
  uri : string;
}

(** Jsont codec for {!type:list_item_view}. *)
val list_item_view_jsont : list_item_view Jsont.t


type list_purpose = string
val list_purpose_jsont : list_purpose Jsont.t


type list_viewer_state = {
  blocked : string option;
  muted : bool option;
}

(** Jsont codec for {!type:list_viewer_state}. *)
val list_viewer_state_jsont : list_viewer_state Jsont.t

(** A list of actors to apply an aggregate moderation action (mute/block) on. *)

type modlist = string
val modlist_jsont : modlist Jsont.t

(** indicates that a handle or DID could not be resolved *)

type not_found_actor = {
  actor : string;
  not_found : bool;
}

(** Jsont codec for {!type:not_found_actor}. *)
val not_found_actor_jsont : not_found_actor Jsont.t

(** A list of actors used for only for reference purposes such as within a starter pack. *)

type referencelist = string
val referencelist_jsont : referencelist Jsont.t

(** lists the bi-directional graph relationships between one actor (not indicated in the object), and the target actors (the DID included in the object) *)

type relationship = {
  blocked_by : string option;  (** if the actor is blocked by this DID, contains the AT-URI of the block record *)
  blocked_by_list : string option;  (** if the actor is blocked by this DID via a block list, contains the AT-URI of the listblock record *)
  blocking : string option;  (** if the actor blocks this DID, this is the AT-URI of the block record *)
  blocking_by_list : string option;  (** if the actor blocks this DID via a block list, this is the AT-URI of the listblock record *)
  did : string;
  followed_by : string option;  (** if the actor is followed by this DID, contains the AT-URI of the follow record *)
  following : string option;  (** if the actor follows this DID, this is the AT-URI of the follow record *)
}

(** Jsont codec for {!type:relationship}. *)
val relationship_jsont : relationship Jsont.t


type starter_pack_view_basic = {
  cid : string;
  creator : Jsont.json;
  indexed_at : string;
  joined_all_time_count : int option;
  joined_week_count : int option;
  labels : Com.Atproto.Label.Defs.label list option;
  list_item_count : int option;
  record : Jsont.json;
  uri : string;
}

(** Jsont codec for {!type:starter_pack_view_basic}. *)
val starter_pack_view_basic_jsont : starter_pack_view_basic Jsont.t


type list_view = {
  avatar : string option;
  cid : string;
  creator : Jsont.json;
  description : string option;
  description_facets : Richtext.Facet.main list option;
  indexed_at : string;
  labels : Com.Atproto.Label.Defs.label list option;
  list_item_count : int option;
  name : string;
  purpose : Jsont.json;
  uri : string;
  viewer : Jsont.json option;
}

(** Jsont codec for {!type:list_view}. *)
val list_view_jsont : list_view Jsont.t


type list_view_basic = {
  avatar : string option;
  cid : string;
  indexed_at : string option;
  labels : Com.Atproto.Label.Defs.label list option;
  list_item_count : int option;
  name : string;
  purpose : Jsont.json;
  uri : string;
  viewer : Jsont.json option;
}

(** Jsont codec for {!type:list_view_basic}. *)
val list_view_basic_jsont : list_view_basic Jsont.t


type starter_pack_view = {
  cid : string;
  creator : Jsont.json;
  feeds : Jsont.json list option;
  indexed_at : string;
  joined_all_time_count : int option;
  joined_week_count : int option;
  labels : Com.Atproto.Label.Defs.label list option;
  list_ : Jsont.json option;
  list_items_sample : Jsont.json list option;
  record : Jsont.json;
  uri : string;
}

(** Jsont codec for {!type:starter_pack_view}. *)
val starter_pack_view_jsont : starter_pack_view Jsont.t

      end
      module Block : sig
(** Record declaring a 'block' relationship against another account. NOTE: blocks are public in Bluesky; see blog posts for details. *)

type main = {
  created_at : string;
  subject : string;  (** DID of the account to be blocked. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module SearchStarterPacks : sig
(** Find starter packs matching search criteria. Does not require auth. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;
  limit : int option;
  q : string;  (** Search query string. Syntax, phrase, boolean, and faceting is unspecified, but Lucene query syntax is recommended. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  starter_packs : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module List : sig
(** Record representing a list of accounts (actors). Scope includes both moderation-oriented lists and curration-oriented lists. *)

type main = {
  avatar : Atp.Blob_ref.t option;
  created_at : string;
  description : string option;
  description_facets : Richtext.Facet.main list option;
  labels : Com.Atproto.Label.Defs.self_labels option;
  name : string;  (** Display name for list; can not be empty. *)
  purpose : Jsont.json;  (** Defines the purpose of the list (aka, moderation-oriented or curration-oriented) *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module GetStarterPacksWithMembership : sig
(** A starter pack and an optional list item indicating membership of a target user to that starter pack. *)

type starter_pack_with_membership = {
  list_item : Jsont.json option;
  starter_pack : Jsont.json;
}

(** Jsont codec for {!type:starter_pack_with_membership}. *)
val starter_pack_with_membership_jsont : starter_pack_with_membership Jsont.t

(** Enumerates the starter packs created by the session user, and includes membership information about `actor` in those starter packs. Requires auth. *)

(** Query/procedure parameters. *)
type params = {
  actor : string;  (** The account (actor) to check for membership. *)
  cursor : string option;
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  starter_packs_with_membership : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetStarterPacks : sig
(** Get views for a list of starter packs. *)

(** Query/procedure parameters. *)
type params = {
  uris : string list;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  starter_packs : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetStarterPack : sig
(** Gets a view of a starter pack. *)

(** Query/procedure parameters. *)
type params = {
  starter_pack : string;  (** Reference (AT-URI) of the starter pack record. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  starter_pack : Jsont.json;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetRelationships : sig
(** Enumerates public relationships between one account, and a list of other accounts. Does not require auth. *)

(** Query/procedure parameters. *)
type params = {
  actor : string;  (** Primary account requesting relationships for. *)
  others : string list option;  (** List of 'other' accounts to be related back to the primary. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  actor : string option;
  relationships : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetListsWithMembership : sig
(** A list and an optional list item indicating membership of a target user to that list. *)

type list_with_membership = {
  list_ : Jsont.json;
  list_item : Jsont.json option;
}

(** Jsont codec for {!type:list_with_membership}. *)
val list_with_membership_jsont : list_with_membership Jsont.t

(** Enumerates the lists created by the session user, and includes membership information about `actor` in those lists. Only supports curation and moderation lists (no reference lists, used in starter packs). Requires auth. *)

(** Query/procedure parameters. *)
type params = {
  actor : string;  (** The account (actor) to check for membership. *)
  cursor : string option;
  limit : int option;
  purposes : string list option;  (** Optional filter by list purpose. If not specified, all supported types are returned. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  lists_with_membership : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetLists : sig
(** Enumerates the lists created by a specified account (actor). *)

(** Query/procedure parameters. *)
type params = {
  actor : string;  (** The account (actor) to enumerate lists from. *)
  cursor : string option;
  limit : int option;
  purposes : string list option;  (** Optional filter by list purpose. If not specified, all supported types are returned. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  lists : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetListMutes : sig
(** Enumerates mod lists that the requesting account (actor) currently has muted. Requires auth. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  lists : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetListBlocks : sig
(** Get mod lists that the requesting account (actor) is blocking. Requires auth. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  lists : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetList : sig
(** Gets a 'view' (with additional context) of a specified list. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;
  limit : int option;
  list_ : string;  (** Reference (AT-URI) of the list record to hydrate. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  items : Jsont.json list;
  list_ : Jsont.json;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetActorStarterPacks : sig
(** Get a list of starter packs created by the actor. *)

(** Query/procedure parameters. *)
type params = {
  actor : string;
  cursor : string option;
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  starter_packs : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Feed : sig
      module Threadgate : sig
(** Allow replies from actors who follow you. *)

type follower_rule = unit

(** Jsont codec for {!type:follower_rule}. *)
val follower_rule_jsont : follower_rule Jsont.t

(** Allow replies from actors you follow. *)

type following_rule = unit

(** Jsont codec for {!type:following_rule}. *)
val following_rule_jsont : following_rule Jsont.t

(** Allow replies from actors on a list. *)

type list_rule = {
  list_ : string;
}

(** Jsont codec for {!type:list_rule}. *)
val list_rule_jsont : list_rule Jsont.t

(** Allow replies from actors mentioned in your post. *)

type mention_rule = unit

(** Jsont codec for {!type:mention_rule}. *)
val mention_rule_jsont : mention_rule Jsont.t

(** Record defining interaction gating rules for a thread (aka, reply controls). The record key (rkey) of the threadgate record must match the record key of the thread's root post, and that record must be in the same repository. *)

type main = {
  allow : Jsont.json list option;  (** List of rules defining who can reply to this post. If value is an empty array, no one can reply. If value is undefined, anyone can reply. *)
  created_at : string;
  hidden_replies : string list option;  (** List of hidden reply URIs. *)
  post : string;  (** Reference (AT-URI) to the post record. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Repost : sig
(** Record representing a 'repost' of an existing Bluesky post. *)

type main = {
  created_at : string;
  subject : Com.Atproto.Repo.StrongRef.main;
  via : Com.Atproto.Repo.StrongRef.main option;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Postgate : sig
(** Disables embedding of this post. *)

type disable_rule = unit

(** Jsont codec for {!type:disable_rule}. *)
val disable_rule_jsont : disable_rule Jsont.t

(** Record defining interaction rules for a post. The record key (rkey) of the postgate record must match the record key of the post, and that record must be in the same repository. *)

type main = {
  created_at : string;
  detached_embedding_uris : string list option;  (** List of AT-URIs embedding this post that the author has detached from. *)
  embedding_rules : Jsont.json list option;  (** List of rules defining who can embed this post. If value is an empty array or is undefined, no particular rules apply and anyone can embed. *)
  post : string;  (** Reference (AT-URI) to the post record. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Post : sig

type reply_ref = {
  parent : Com.Atproto.Repo.StrongRef.main;
  root : Com.Atproto.Repo.StrongRef.main;
}

(** Jsont codec for {!type:reply_ref}. *)
val reply_ref_jsont : reply_ref Jsont.t

(** Deprecated. Use app.bsky.richtext instead -- A text segment. Start is inclusive, end is exclusive. Indices are for utf16-encoded strings. *)

type text_slice = {
  end_ : int;
  start : int;
}

(** Jsont codec for {!type:text_slice}. *)
val text_slice_jsont : text_slice Jsont.t

(** Deprecated: use facets instead. *)

type entity = {
  index : Jsont.json;
  type_ : string;  (** Expected values are 'mention' and 'link'. *)
  value : string;
}

(** Jsont codec for {!type:entity}. *)
val entity_jsont : entity Jsont.t

(** Record containing a Bluesky post. *)

type main = {
  created_at : string;  (** Client-declared timestamp when this post was originally created. *)
  embed : Jsont.json option;
  entities : Jsont.json list option;  (** DEPRECATED: replaced by app.bsky.richtext.facet. *)
  facets : Richtext.Facet.main list option;  (** Annotations of text (mentions, URLs, hashtags, etc) *)
  labels : Com.Atproto.Label.Defs.self_labels option;  (** Self-label values for this post. Effectively content warnings. *)
  langs : string list option;  (** Indicates human language of post primary text content. *)
  reply : Jsont.json option;
  tags : string list option;  (** Additional hashtags, in addition to any included in post text and facets. *)
  text : string;  (** The primary post content. May be an empty string, if there are embeds. *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module Like : sig
(** Record declaring a 'like' of a piece of subject content. *)

type main = {
  created_at : string;
  subject : Com.Atproto.Repo.StrongRef.main;
  via : Com.Atproto.Repo.StrongRef.main option;
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module GetRepostedBy : sig
(** Get a list of reposts for a given post. *)

(** Query/procedure parameters. *)
type params = {
  cid : string option;  (** If supplied, filters to reposts of specific version (by CID) of the post record. *)
  cursor : string option;
  limit : int option;
  uri : string;  (** Reference (AT-URI) of post record *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cid : string option;
  cursor : string option;
  reposted_by : Jsont.json list;
  uri : string;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetLikes : sig

type like = {
  actor : Jsont.json;
  created_at : string;
  indexed_at : string;
}

(** Jsont codec for {!type:like}. *)
val like_jsont : like Jsont.t

(** Get like records which reference a subject (by AT-URI and CID). *)

(** Query/procedure parameters. *)
type params = {
  cid : string option;  (** CID of the subject record (aka, specific version of record), to filter likes. *)
  cursor : string option;
  limit : int option;
  uri : string;  (** AT-URI of the subject (eg, a post record). *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cid : string option;
  cursor : string option;
  likes : Jsont.json list;
  uri : string;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Generator : sig
(** Record declaring of the existence of a feed generator, and containing metadata about it. The record can exist in any repository. *)

type main = {
  accepts_interactions : bool option;  (** Declaration that a feed accepts feedback interactions from a client through app.bsky.feed.sendInteractions *)
  avatar : Atp.Blob_ref.t option;
  content_mode : string option;
  created_at : string;
  description : string option;
  description_facets : Richtext.Facet.main list option;
  did : string;
  display_name : string;
  labels : Com.Atproto.Label.Defs.self_labels option;  (** Self-label values *)
}

(** Jsont codec for {!type:main}. *)
val main_jsont : main Jsont.t

      end
      module DescribeFeedGenerator : sig

type feed = {
  uri : string;
}

(** Jsont codec for {!type:feed}. *)
val feed_jsont : feed Jsont.t


type links = {
  privacy_policy : string option;
  terms_of_service : string option;
}

(** Jsont codec for {!type:links}. *)
val links_jsont : links Jsont.t

(** Get information about a feed generator, including policies and offered feed URIs. Does not require auth; implemented by Feed Generator services (not App View). *)


type output = {
  did : string;
  feeds : Jsont.json list;
  links : Jsont.json option;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Defs : sig

type blocked_author = {
  did : string;
  viewer : Jsont.json option;
}

(** Jsont codec for {!type:blocked_author}. *)
val blocked_author_jsont : blocked_author Jsont.t

(** User clicked through to the author of the feed item *)

type clickthrough_author = string
val clickthrough_author_jsont : clickthrough_author Jsont.t

(** User clicked through to the embedded content of the feed item *)

type clickthrough_embed = string
val clickthrough_embed_jsont : clickthrough_embed Jsont.t

(** User clicked through to the feed item *)

type clickthrough_item = string
val clickthrough_item_jsont : clickthrough_item Jsont.t

(** User clicked through to the reposter of the feed item *)

type clickthrough_reposter = string
val clickthrough_reposter_jsont : clickthrough_reposter Jsont.t

(** Declares the feed generator returns any types of posts. *)

type content_mode_unspecified = string
val content_mode_unspecified_jsont : content_mode_unspecified Jsont.t

(** Declares the feed generator returns posts containing app.bsky.embed.video embeds. *)

type content_mode_video = string
val content_mode_video_jsont : content_mode_video Jsont.t


type generator_viewer_state = {
  like : string option;
}

(** Jsont codec for {!type:generator_viewer_state}. *)
val generator_viewer_state_jsont : generator_viewer_state Jsont.t


type interaction = {
  event : string option;
  feed_context : string option;  (** Context on a feed item that was originally supplied by the feed generator on getFeedSkeleton. *)
  item : string option;
  req_id : string option;  (** Unique identifier per request that may be passed back alongside interactions. *)
}

(** Jsont codec for {!type:interaction}. *)
val interaction_jsont : interaction Jsont.t

(** User liked the feed item *)

type interaction_like = string
val interaction_like_jsont : interaction_like Jsont.t

(** User quoted the feed item *)

type interaction_quote = string
val interaction_quote_jsont : interaction_quote Jsont.t

(** User replied to the feed item *)

type interaction_reply = string
val interaction_reply_jsont : interaction_reply Jsont.t

(** User reposted the feed item *)

type interaction_repost = string
val interaction_repost_jsont : interaction_repost Jsont.t

(** Feed item was seen by user *)

type interaction_seen = string
val interaction_seen_jsont : interaction_seen Jsont.t

(** User shared the feed item *)

type interaction_share = string
val interaction_share_jsont : interaction_share Jsont.t


type not_found_post = {
  not_found : bool;
  uri : string;
}

(** Jsont codec for {!type:not_found_post}. *)
val not_found_post_jsont : not_found_post Jsont.t


type reason_pin = unit

(** Jsont codec for {!type:reason_pin}. *)
val reason_pin_jsont : reason_pin Jsont.t


type reason_repost = {
  by : Jsont.json;
  cid : string option;
  indexed_at : string;
  uri : string option;
}

(** Jsont codec for {!type:reason_repost}. *)
val reason_repost_jsont : reason_repost Jsont.t


type reply_ref = {
  grandparent_author : Jsont.json option;  (** When parent is a reply to another post, this is the author of that post. *)
  parent : Jsont.json;
  root : Jsont.json;
}

(** Jsont codec for {!type:reply_ref}. *)
val reply_ref_jsont : reply_ref Jsont.t

(** Request that less content like the given feed item be shown in the feed *)

type request_less = string
val request_less_jsont : request_less Jsont.t

(** Request that more content like the given feed item be shown in the feed *)

type request_more = string
val request_more_jsont : request_more Jsont.t


type skeleton_feed_post = {
  feed_context : string option;  (** Context that will be passed through to client and may be passed to feed generator back alongside interactions. *)
  post : string;
  reason : Jsont.json option;
}

(** Jsont codec for {!type:skeleton_feed_post}. *)
val skeleton_feed_post_jsont : skeleton_feed_post Jsont.t


type skeleton_reason_pin = unit

(** Jsont codec for {!type:skeleton_reason_pin}. *)
val skeleton_reason_pin_jsont : skeleton_reason_pin Jsont.t


type skeleton_reason_repost = {
  repost : string;
}

(** Jsont codec for {!type:skeleton_reason_repost}. *)
val skeleton_reason_repost_jsont : skeleton_reason_repost Jsont.t

(** Metadata about this post within the context of the thread it is in. *)

type thread_context = {
  root_author_like : string option;
}

(** Jsont codec for {!type:thread_context}. *)
val thread_context_jsont : thread_context Jsont.t


type threadgate_view = {
  cid : string option;
  lists : Jsont.json list option;
  record : Jsont.json option;
  uri : string option;
}

(** Jsont codec for {!type:threadgate_view}. *)
val threadgate_view_jsont : threadgate_view Jsont.t

(** Metadata about the requesting account's relationship with the subject content. Only has meaningful content for authed requests. *)

type viewer_state = {
  bookmarked : bool option;
  embedding_disabled : bool option;
  like : string option;
  pinned : bool option;
  reply_disabled : bool option;
  repost : string option;
  thread_muted : bool option;
}

(** Jsont codec for {!type:viewer_state}. *)
val viewer_state_jsont : viewer_state Jsont.t


type blocked_post = {
  author : Jsont.json;
  blocked : bool;
  uri : string;
}

(** Jsont codec for {!type:blocked_post}. *)
val blocked_post_jsont : blocked_post Jsont.t


type generator_view = {
  accepts_interactions : bool option;
  avatar : string option;
  cid : string;
  content_mode : string option;
  creator : Jsont.json;
  description : string option;
  description_facets : Richtext.Facet.main list option;
  did : string;
  display_name : string;
  indexed_at : string;
  labels : Com.Atproto.Label.Defs.label list option;
  like_count : int option;
  uri : string;
  viewer : Jsont.json option;
}

(** Jsont codec for {!type:generator_view}. *)
val generator_view_jsont : generator_view Jsont.t


type post_view = {
  author : Jsont.json;
  bookmark_count : int option;
  cid : string;
  debug : Jsont.json option;  (** Debug information for internal development *)
  embed : Jsont.json option;
  indexed_at : string;
  labels : Com.Atproto.Label.Defs.label list option;
  like_count : int option;
  quote_count : int option;
  record : Jsont.json;
  reply_count : int option;
  repost_count : int option;
  threadgate : Jsont.json option;
  uri : string;
  viewer : Jsont.json option;
}

(** Jsont codec for {!type:post_view}. *)
val post_view_jsont : post_view Jsont.t


type feed_view_post = {
  feed_context : string option;  (** Context provided by feed generator that may be passed back alongside interactions. *)
  post : Jsont.json;
  reason : Jsont.json option;
  reply : Jsont.json option;
  req_id : string option;  (** Unique identifier per request that may be passed back alongside interactions. *)
}

(** Jsont codec for {!type:feed_view_post}. *)
val feed_view_post_jsont : feed_view_post Jsont.t


type thread_view_post = {
  parent : Jsont.json option;
  post : Jsont.json;
  replies : Jsont.json list option;
  thread_context : Jsont.json option;
}

(** Jsont codec for {!type:thread_view_post}. *)
val thread_view_post_jsont : thread_view_post Jsont.t

      end
      module SendInteractions : sig
(** Send information about interactions with feed items back to the feed generator that served them. *)


type input = {
  interactions : Jsont.json list;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = unit

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module SearchPosts : sig
(** Find posts matching search criteria, returning views of those posts. Note that this API endpoint may require authentication (eg, not public) for some service providers and implementations. *)

(** Query/procedure parameters. *)
type params = {
  author : string option;  (** Filter to posts by the given account. Handles are resolved to DID before query-time. *)
  cursor : string option;  (** Optional pagination mechanism; may not necessarily allow scrolling through entire result set. *)
  domain : string option;  (** Filter to posts with URLs (facet links or embeds) linking to the given domain (hostname). Server may apply hostname normalization. *)
  lang : string option;  (** Filter to posts in the given language. Expected to be based on post language field, though server may override language detection. *)
  limit : int option;
  mentions : string option;  (** Filter to posts which mention the given account. Handles are resolved to DID before query-time. Only matches rich-text facet mentions. *)
  q : string;  (** Search query string; syntax, phrase, boolean, and faceting is unspecified, but Lucene query syntax is recommended. *)
  since : string option;  (** Filter results for posts after the indicated datetime (inclusive). Expected to use 'sortAt' timestamp, which may not match 'createdAt'. Can be a datetime, or just an ISO date (YYYY-MM-DD). *)
  sort : string option;  (** Specifies the ranking order of results. *)
  tag : string list option;  (** Filter to posts with the given tag (hashtag), based on rich-text facet or tag field. Do not include the hash (#) prefix. Multiple tags can be specified, with 'AND' matching. *)
  until : string option;  (** Filter results for posts before the indicated datetime (not inclusive). Expected to use 'sortAt' timestamp, which may not match 'createdAt'. Can be a datetime, or just an ISO date (YYY-MM-DD). *)
  url : string option;  (** Filter to posts with links (facet links or embeds) pointing to this URL. Server may apply URL normalization or fuzzy matching. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  hits_total : int option;  (** Count of search hits. Optional, may be rounded/truncated, and may not be possible to paginate through all hits. *)
  posts : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetTimeline : sig
(** Get a view of the requesting account's home timeline. This is expected to be some form of reverse-chronological feed. *)

(** Query/procedure parameters. *)
type params = {
  algorithm : string option;  (** Variant 'algorithm' for timeline. Implementation-specific. NOTE: most feed flexibility has been moved to feed generator mechanism. *)
  cursor : string option;
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  feed : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetSuggestedFeeds : sig
(** Get a list of suggested feeds (feed generators) for the requesting account. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  feeds : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetQuotes : sig
(** Get a list of quotes for a given post. *)

(** Query/procedure parameters. *)
type params = {
  cid : string option;  (** If supplied, filters to quotes of specific version (by CID) of the post record. *)
  cursor : string option;
  limit : int option;
  uri : string;  (** Reference (AT-URI) of post record *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cid : string option;
  cursor : string option;
  posts : Jsont.json list;
  uri : string;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetPosts : sig
(** Gets post views for a specified list of posts (by AT-URI). This is sometimes referred to as 'hydrating' a 'feed skeleton'. *)

(** Query/procedure parameters. *)
type params = {
  uris : string list;  (** List of post AT-URIs to return hydrated views for. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  posts : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetPostThread : sig
(** Get posts in a thread. Does not require auth, but additional metadata and filtering will be applied for authed requests. *)

(** Query/procedure parameters. *)
type params = {
  depth : int option;  (** How many levels of reply depth should be included in response. *)
  parent_height : int option;  (** How many levels of parent (and grandparent, etc) post to include. *)
  uri : string;  (** Reference (AT-URI) to post record. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  thread : Jsont.json;
  threadgate : Jsont.json option;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetListFeed : sig
(** Get a feed of recent posts from a list (posts and reposts from any actors on the list). Does not require auth. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;
  limit : int option;
  list_ : string;  (** Reference (AT-URI) to the list record. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  feed : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetFeedSkeleton : sig
(** Get a skeleton of a feed provided by a feed generator. Auth is optional, depending on provider requirements, and provides the DID of the requester. Implemented by Feed Generator Service. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;
  feed : string;  (** Reference to feed generator record describing the specific feed being requested. *)
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  feed : Jsont.json list;
  req_id : string option;  (** Unique identifier per request that may be passed back alongside interactions. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetFeedGenerators : sig
(** Get information about a list of feed generators. *)

(** Query/procedure parameters. *)
type params = {
  feeds : string list;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  feeds : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetFeedGenerator : sig
(** Get information about a feed generator. Implemented by AppView. *)

(** Query/procedure parameters. *)
type params = {
  feed : string;  (** AT-URI of the feed generator record. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  is_online : bool;  (** Indicates whether the feed generator service has been online recently, or else seems to be inactive. *)
  is_valid : bool;  (** Indicates whether the feed generator service is compatible with the record declaration. *)
  view : Jsont.json;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetFeed : sig
(** Get a hydrated feed from an actor's selected feed generator. Implemented by App View. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;
  feed : string;
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  feed : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetAuthorFeed : sig
(** Get a view of an actor's 'author feed' (post and reposts by the author). Does not require auth. *)

(** Query/procedure parameters. *)
type params = {
  actor : string;
  cursor : string option;
  filter : string option;  (** Combinations of post/repost types to include in response. *)
  include_pins : bool option;
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  feed : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetActorLikes : sig
(** Get a list of posts liked by an actor. Requires auth, actor must be the requesting account. *)

(** Query/procedure parameters. *)
type params = {
  actor : string;
  cursor : string option;
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  feed : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetActorFeeds : sig
(** Get a list of feeds (feed generator records) created by the actor (in the actor's repo). *)

(** Query/procedure parameters. *)
type params = {
  actor : string;
  cursor : string option;
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  feeds : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Contact : sig
      module VerifyPhone : sig
(** Verifies control over a phone number with a code received via SMS and starts a contact import session. Requires authentication. *)


type input = {
  code : string;  (** The code received via SMS as a result of the call to `app.bsky.contact.startPhoneVerification`. *)
  phone : string;  (** The phone number to verify. Should be the same as the one passed to `app.bsky.contact.startPhoneVerification`. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  token : string;  (** JWT to be used in a call to `app.bsky.contact.importContacts`. It is only valid for a single call. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module StartPhoneVerification : sig
(** Starts a phone verification flow. The phone passed will receive a code via SMS that should be passed to `app.bsky.contact.verifyPhone`. Requires authentication. *)


type input = {
  phone : string;  (** The phone number to receive the code via SMS. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = unit

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module SendNotification : sig
(** System endpoint to send notifications related to contact imports. Requires role authentication. *)


type input = {
  from : string;  (** The DID of who this notification comes from. *)
  to_ : string;  (** The DID of who this notification should go to. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = unit

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module RemoveData : sig
(** Removes all stored hashes used for contact matching, existing matches, and sync status. Requires authentication. *)


type input = unit

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = unit

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetMatches : sig
(** Returns the matched contacts (contacts that were mutually imported). Excludes dismissed matches. Requires authentication. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  matches : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module DismissMatch : sig
(** Removes a match that was found via contact import. It shouldn't appear again if the same contact is re-imported. Requires authentication. *)


type input = {
  subject : string;  (** The subject's DID to dismiss the match with. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = unit

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Defs : sig
(** Associates a profile with the positional index of the contact import input in the call to `app.bsky.contact.importContacts`, so clients can know which phone caused a particular match. *)

type match_and_contact_index = {
  contact_index : int;  (** The index of this match in the import contact input. *)
  match_ : Jsont.json;  (** Profile of the matched user. *)
}

(** Jsont codec for {!type:match_and_contact_index}. *)
val match_and_contact_index_jsont : match_and_contact_index Jsont.t

(** A stash object to be sent via bsync representing a notification to be created. *)

type notification = {
  from : string;  (** The DID of who this notification comes from. *)
  to_ : string;  (** The DID of who this notification should go to. *)
}

(** Jsont codec for {!type:notification}. *)
val notification_jsont : notification Jsont.t


type sync_status = {
  matches_count : int;  (** Number of existing contact matches resulting of the user imports and of their imported contacts having imported the user. Matches stop being counted when the user either follows the matched contact or dismisses the match. *)
  synced_at : string;  (** Last date when contacts where imported. *)
}

(** Jsont codec for {!type:sync_status}. *)
val sync_status_jsont : sync_status Jsont.t

      end
      module ImportContacts : sig
(** Import contacts for securely matching with other users. This follows the protocol explained in https://docs.bsky.app/blog/contact-import-rfc. Requires authentication. *)


type input = {
  contacts : string list;  (** List of phone numbers in global E.164 format (e.g., '+12125550123'). Phone numbers that cannot be normalized into a valid phone number will be discarded. Should not repeat the 'phone' input used in `app.bsky.contact.verifyPhone`. *)
  token : string;  (** JWT to authenticate the call. Use the JWT received as a response to the call to `app.bsky.contact.verifyPhone`. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = {
  matches_and_contact_indexes : Defs.match_and_contact_index list;  (** The users that matched during import and their indexes on the input contacts, so the client can correlate with its local list. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetSyncStatus : sig
(** Gets the user's current contact import status. Requires authentication. *)

(** Query/procedure parameters. *)
type params = unit

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  sync_status : Defs.sync_status option;  (** If present, indicates the user has imported their contacts. If not present, indicates the user never used the feature or called `app.bsky.contact.removeData` and didn't import again since. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Unspecced : sig
      module GetTaggedSuggestions : sig

type suggestion = {
  subject : string;
  subject_type : string;
  tag : string;
}

(** Jsont codec for {!type:suggestion}. *)
val suggestion_jsont : suggestion Jsont.t

(** Get a list of suggestions (feeds and users) tagged with categories *)

(** Query/procedure parameters. *)
type params = unit

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  suggestions : suggestion list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetSuggestedUsersSkeleton : sig
(** Get a skeleton of suggested users. Intended to be called and hydrated by app.bsky.unspecced.getSuggestedUsers *)

(** Query/procedure parameters. *)
type params = {
  category : string option;  (** Category of users to get suggestions for. *)
  limit : int option;
  viewer : string option;  (** DID of the account making the request (not included for public/unauthenticated queries). *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  dids : string list;
  rec_id : int option;  (** Snowflake for this recommendation, use when submitting recommendation events. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetSuggestedUsers : sig
(** Get a list of suggested users *)

(** Query/procedure parameters. *)
type params = {
  category : string option;  (** Category of users to get suggestions for. *)
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  actors : Jsont.json list;
  rec_id : int option;  (** Snowflake for this recommendation, use when submitting recommendation events. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetSuggestedStarterPacksSkeleton : sig
(** Get a skeleton of suggested starterpacks. Intended to be called and hydrated by app.bsky.unspecced.getSuggestedStarterpacks *)

(** Query/procedure parameters. *)
type params = {
  limit : int option;
  viewer : string option;  (** DID of the account making the request (not included for public/unauthenticated queries). *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  starter_packs : string list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetSuggestedStarterPacks : sig
(** Get a list of suggested starterpacks *)

(** Query/procedure parameters. *)
type params = {
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  starter_packs : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetSuggestedFeedsSkeleton : sig
(** Get a skeleton of suggested feeds. Intended to be called and hydrated by app.bsky.unspecced.getSuggestedFeeds *)

(** Query/procedure parameters. *)
type params = {
  limit : int option;
  viewer : string option;  (** DID of the account making the request (not included for public/unauthenticated queries). *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  feeds : string list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetSuggestedFeeds : sig
(** Get a list of suggested feeds *)

(** Query/procedure parameters. *)
type params = {
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  feeds : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetPopularFeedGenerators : sig
(** An unspecced view of globally popular feed generators. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;
  limit : int option;
  query : string option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  feeds : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetOnboardingSuggestedStarterPacksSkeleton : sig
(** Get a skeleton of suggested starterpacks for onboarding. Intended to be called and hydrated by app.bsky.unspecced.getOnboardingSuggestedStarterPacks *)

(** Query/procedure parameters. *)
type params = {
  limit : int option;
  viewer : string option;  (** DID of the account making the request (not included for public/unauthenticated queries). *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  starter_packs : string list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetOnboardingSuggestedStarterPacks : sig
(** Get a list of suggested starterpacks for onboarding *)

(** Query/procedure parameters. *)
type params = {
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  starter_packs : Jsont.json list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetConfig : sig

type live_now_config = {
  did : string;
  domains : string list;
}

(** Jsont codec for {!type:live_now_config}. *)
val live_now_config_jsont : live_now_config Jsont.t

(** Get miscellaneous runtime configuration. *)


type output = {
  check_email_confirmed : bool option;
  live_now : live_now_config list option;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module Defs : sig
(** Object used to store age assurance data in stash. *)

type age_assurance_event = {
  attempt_id : string;  (** The unique identifier for this instance of the age assurance flow, in UUID format. *)
  complete_ip : string option;  (** The IP address used when completing the AA flow. *)
  complete_ua : string option;  (** The user agent used when completing the AA flow. *)
  created_at : string;  (** The date and time of this write operation. *)
  email : string option;  (** The email used for AA. *)
  init_ip : string option;  (** The IP address used when initiating the AA flow. *)
  init_ua : string option;  (** The user agent used when initiating the AA flow. *)
  status : string;  (** The status of the age assurance process. *)
}

(** Jsont codec for {!type:age_assurance_event}. *)
val age_assurance_event_jsont : age_assurance_event Jsont.t

(** The computed state of the age assurance process, returned to the user in question on certain authenticated requests. *)

type age_assurance_state = {
  last_initiated_at : string option;  (** The timestamp when this state was last updated. *)
  status : string;  (** The status of the age assurance process. *)
}

(** Jsont codec for {!type:age_assurance_state}. *)
val age_assurance_state_jsont : age_assurance_state Jsont.t


type skeleton_search_actor = {
  did : string;
}

(** Jsont codec for {!type:skeleton_search_actor}. *)
val skeleton_search_actor_jsont : skeleton_search_actor Jsont.t


type skeleton_search_post = {
  uri : string;
}

(** Jsont codec for {!type:skeleton_search_post}. *)
val skeleton_search_post_jsont : skeleton_search_post Jsont.t


type skeleton_search_starter_pack = {
  uri : string;
}

(** Jsont codec for {!type:skeleton_search_starter_pack}. *)
val skeleton_search_starter_pack_jsont : skeleton_search_starter_pack Jsont.t


type skeleton_trend = {
  category : string option;
  dids : string list;
  display_name : string;
  link : string;
  post_count : int;
  started_at : string;
  status : string option;
  topic : string;
}

(** Jsont codec for {!type:skeleton_trend}. *)
val skeleton_trend_jsont : skeleton_trend Jsont.t


type thread_item_blocked = {
  author : Jsont.json;
}

(** Jsont codec for {!type:thread_item_blocked}. *)
val thread_item_blocked_jsont : thread_item_blocked Jsont.t


type thread_item_no_unauthenticated = unit

(** Jsont codec for {!type:thread_item_no_unauthenticated}. *)
val thread_item_no_unauthenticated_jsont : thread_item_no_unauthenticated Jsont.t


type thread_item_not_found = unit

(** Jsont codec for {!type:thread_item_not_found}. *)
val thread_item_not_found_jsont : thread_item_not_found Jsont.t


type thread_item_post = {
  hidden_by_threadgate : bool;  (** The threadgate created by the author indicates this post as a reply to be hidden for everyone consuming the thread. *)
  more_parents : bool;  (** This post has more parents that were not present in the response. This is just a boolean, without the number of parents. *)
  more_replies : int;  (** This post has more replies that were not present in the response. This is a numeric value, which is best-effort and might not be accurate. *)
  muted_by_viewer : bool;  (** This is by an account muted by the viewer requesting it. *)
  op_thread : bool;  (** This post is part of a contiguous thread by the OP from the thread root. Many different OP threads can happen in the same thread. *)
  post : Jsont.json;
}

(** Jsont codec for {!type:thread_item_post}. *)
val thread_item_post_jsont : thread_item_post Jsont.t


type trend_view = {
  actors : Jsont.json list;
  category : string option;
  display_name : string;
  link : string;
  post_count : int;
  started_at : string;
  status : string option;
  topic : string;
}

(** Jsont codec for {!type:trend_view}. *)
val trend_view_jsont : trend_view Jsont.t


type trending_topic = {
  description : string option;
  display_name : string option;
  link : string;
  topic : string;
}

(** Jsont codec for {!type:trending_topic}. *)
val trending_topic_jsont : trending_topic Jsont.t

      end
      module SearchStarterPacksSkeleton : sig
(** Backend Starter Pack search, returns only skeleton. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Optional pagination mechanism; may not necessarily allow scrolling through entire result set. *)
  limit : int option;
  q : string;  (** Search query string; syntax, phrase, boolean, and faceting is unspecified, but Lucene query syntax is recommended. *)
  viewer : string option;  (** DID of the account making the request (not included for public/unauthenticated queries). *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  hits_total : int option;  (** Count of search hits. Optional, may be rounded/truncated, and may not be possible to paginate through all hits. *)
  starter_packs : Defs.skeleton_search_starter_pack list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module SearchPostsSkeleton : sig
(** Backend Posts search, returns only skeleton *)

(** Query/procedure parameters. *)
type params = {
  author : string option;  (** Filter to posts by the given account. Handles are resolved to DID before query-time. *)
  cursor : string option;  (** Optional pagination mechanism; may not necessarily allow scrolling through entire result set. *)
  domain : string option;  (** Filter to posts with URLs (facet links or embeds) linking to the given domain (hostname). Server may apply hostname normalization. *)
  lang : string option;  (** Filter to posts in the given language. Expected to be based on post language field, though server may override language detection. *)
  limit : int option;
  mentions : string option;  (** Filter to posts which mention the given account. Handles are resolved to DID before query-time. Only matches rich-text facet mentions. *)
  q : string;  (** Search query string; syntax, phrase, boolean, and faceting is unspecified, but Lucene query syntax is recommended. *)
  since : string option;  (** Filter results for posts after the indicated datetime (inclusive). Expected to use 'sortAt' timestamp, which may not match 'createdAt'. Can be a datetime, or just an ISO date (YYYY-MM-DD). *)
  sort : string option;  (** Specifies the ranking order of results. *)
  tag : string list option;  (** Filter to posts with the given tag (hashtag), based on rich-text facet or tag field. Do not include the hash (#) prefix. Multiple tags can be specified, with 'AND' matching. *)
  until : string option;  (** Filter results for posts before the indicated datetime (not inclusive). Expected to use 'sortAt' timestamp, which may not match 'createdAt'. Can be a datetime, or just an ISO date (YYY-MM-DD). *)
  url : string option;  (** Filter to posts with links (facet links or embeds) pointing to this URL. Server may apply URL normalization or fuzzy matching. *)
  viewer : string option;  (** DID of the account making the request (not included for public/unauthenticated queries). Used for 'from:me' queries. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  cursor : string option;
  hits_total : int option;  (** Count of search hits. Optional, may be rounded/truncated, and may not be possible to paginate through all hits. *)
  posts : Defs.skeleton_search_post list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module SearchActorsSkeleton : sig
(** Backend Actors (profile) search, returns only skeleton. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;  (** Optional pagination mechanism; may not necessarily allow scrolling through entire result set. *)
  limit : int option;
  q : string;  (** Search query string; syntax, phrase, boolean, and faceting is unspecified, but Lucene query syntax is recommended. For typeahead search, only simple term match is supported, not full syntax. *)
  typeahead : bool option;  (** If true, acts as fast/simple 'typeahead' query. *)
  viewer : string option;  (** DID of the account making the request (not included for public/unauthenticated queries). Used to boost followed accounts in ranking. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  actors : Defs.skeleton_search_actor list;
  cursor : string option;
  hits_total : int option;  (** Count of search hits. Optional, may be rounded/truncated, and may not be possible to paginate through all hits. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module InitAgeAssurance : sig
(** Initiate age assurance for an account. This is a one-time action that will start the process of verifying the user's age. *)


type input = {
  country_code : string;  (** An ISO 3166-1 alpha-2 code of the user's location. *)
  email : string;  (** The user's email address to receive assurance instructions. *)
  language : string;  (** The user's preferred language for communication during the assurance process. *)
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t


type output = Defs.age_assurance_state

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetTrendsSkeleton : sig
(** Get the skeleton of trends on the network. Intended to be called and then hydrated through app.bsky.unspecced.getTrends *)

(** Query/procedure parameters. *)
type params = {
  limit : int option;
  viewer : string option;  (** DID of the account making the request (not included for public/unauthenticated queries). *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  trends : Defs.skeleton_trend list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetTrends : sig
(** Get the current trends on the network *)

(** Query/procedure parameters. *)
type params = {
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  trends : Defs.trend_view list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetTrendingTopics : sig
(** Get a list of trending topics *)

(** Query/procedure parameters. *)
type params = {
  limit : int option;
  viewer : string option;  (** DID of the account making the request (not included for public/unauthenticated queries). Used to boost followed accounts in ranking. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  suggested : Defs.trending_topic list;
  topics : Defs.trending_topic list;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetSuggestionsSkeleton : sig
(** Get a skeleton of suggested actors. Intended to be called and then hydrated through app.bsky.actor.getSuggestions *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;
  limit : int option;
  relative_to_did : string option;  (** DID of the account to get suggestions relative to. If not provided, suggestions will be based on the viewer. *)
  viewer : string option;  (** DID of the account making the request (not included for public/unauthenticated queries). Used to boost followed accounts in ranking. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  actors : Defs.skeleton_search_actor list;
  cursor : string option;
  rec_id : int option;  (** Snowflake for this recommendation, use when submitting recommendation events. *)
  relative_to_did : string option;  (** DID of the account these suggestions are relative to. If this is returned undefined, suggestions are based on the viewer. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetPostThreadV2 : sig

type thread_item = {
  depth : int;  (** The nesting level of this item in the thread. Depth 0 means the anchor item. Items above have negative depths, items below have positive depths. *)
  uri : string;
  value : Jsont.json;
}

(** Jsont codec for {!type:thread_item}. *)
val thread_item_jsont : thread_item Jsont.t

(** (NOTE: this endpoint is under development and WILL change without notice. Don't use it until it is moved out of `unspecced` or your application WILL break) Get posts in a thread. It is based in an anchor post at any depth of the tree, and returns posts above it (recursively resolving the parent, without further branching to their replies) and below it (recursive replies, with branching to their replies). Does not require auth, but additional metadata and filtering will be applied for authed requests. *)

(** Query/procedure parameters. *)
type params = {
  above : bool option;  (** Whether to include parents above the anchor. *)
  anchor : string;  (** Reference (AT-URI) to post record. This is the anchor post, and the thread will be built around it. It can be any post in the tree, not necessarily a root post. *)
  below : int option;  (** How many levels of replies to include below the anchor. *)
  branching_factor : int option;  (** Maximum of replies to include at each level of the thread, except for the direct replies to the anchor, which are (NOTE: currently, during unspecced phase) all returned (NOTE: later they might be paginated). *)
  sort : string option;  (** Sorting for the thread replies. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  has_other_replies : bool;  (** Whether this thread has additional replies. If true, a call can be made to the `getPostThreadOtherV2` endpoint to retrieve them. *)
  thread : thread_item list;  (** A flat list of thread items. The depth of each item is indicated by the depth property inside the item. *)
  threadgate : Jsont.json option;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetPostThreadOtherV2 : sig

type thread_item = {
  depth : int;  (** The nesting level of this item in the thread. Depth 0 means the anchor item. Items above have negative depths, items below have positive depths. *)
  uri : string;
  value : Defs.thread_item_post;
}

(** Jsont codec for {!type:thread_item}. *)
val thread_item_jsont : thread_item Jsont.t

(** (NOTE: this endpoint is under development and WILL change without notice. Don't use it until it is moved out of `unspecced` or your application WILL break) Get additional posts under a thread e.g. replies hidden by threadgate. Based on an anchor post at any depth of the tree, returns top-level replies below that anchor. It does not include ancestors nor the anchor itself. This should be called after exhausting `app.bsky.unspecced.getPostThreadV2`. Does not require auth, but additional metadata and filtering will be applied for authed requests. *)

(** Query/procedure parameters. *)
type params = {
  anchor : string;  (** Reference (AT-URI) to post record. This is the anchor post. *)
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  thread : thread_item list;  (** A flat list of other thread items. The depth of each item is indicated by the depth property inside the item. *)
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
      module GetAgeAssuranceState : sig
(** Returns the current state of the age assurance process for an account. This is used to check if the user has completed age assurance or if further action is required. *)


type output = Defs.age_assurance_state

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
    module Bookmark : sig
      module DeleteBookmark : sig
(** Deletes a private bookmark for the specified record. Currently, only `app.bsky.feed.post` records are supported. Requires authentication. *)


type input = {
  uri : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module Defs : sig
(** Object used to store bookmark data in stash. *)

type bookmark = {
  subject : Com.Atproto.Repo.StrongRef.main;  (** A strong ref to the record to be bookmarked. Currently, only `app.bsky.feed.post` records are supported. *)
}

(** Jsont codec for {!type:bookmark}. *)
val bookmark_jsont : bookmark Jsont.t


type bookmark_view = {
  created_at : string option;
  item : Jsont.json;
  subject : Com.Atproto.Repo.StrongRef.main;  (** A strong ref to the bookmarked record. *)
}

(** Jsont codec for {!type:bookmark_view}. *)
val bookmark_view_jsont : bookmark_view Jsont.t

      end
      module CreateBookmark : sig
(** Creates a private bookmark for the specified record. Currently, only `app.bsky.feed.post` records are supported. Requires authentication. *)


type input = {
  cid : string;
  uri : string;
}

(** Jsont codec for {!type:input}. *)
val input_jsont : input Jsont.t

      end
      module GetBookmarks : sig
(** Gets views of records bookmarked by the authenticated user. Requires authentication. *)

(** Query/procedure parameters. *)
type params = {
  cursor : string option;
  limit : int option;
}

(** Jsont codec for {!type:params}. *)
val params_jsont : params Jsont.t


type output = {
  bookmarks : Defs.bookmark_view list;
  cursor : string option;
}

(** Jsont codec for {!type:output}. *)
val output_jsont : output Jsont.t

      end
    end
  end
end
