(* Atp_lexicon_bsky - generated from atproto lexicons *)

(** Utility functions for resilient parsing. *)
module Filter = struct
  (** [filter_list jsont json_list] parses each element with [jsont],
      returning only successfully parsed elements. Non-compliant records
      are silently skipped. *)
  let filter_list (type a) (jsont : a Jsont.t) (json_list : Jsont.json list) : a list =
    List.filter_map (fun json ->
      match Jsont.Json.decode jsont json with
      | Ok v -> Some v
      | Error _ -> None
    ) json_list
end

module Com = struct
  module Atproto = struct
    module Repo = struct
      module StrongRef = struct
type main = {
  cid : string;
  uri : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ cid uri -> { cid; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"com.atproto.repo.strongRef" ~enc:(fun _ -> "com.atproto.repo.strongRef")
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

      end
    end
    module Moderation = struct
      module Defs = struct
type reason_appeal = string
let reason_appeal_jsont = Jsont.string

type reason_misleading = string
let reason_misleading_jsont = Jsont.string

type reason_other = string
let reason_other_jsont = Jsont.string

type reason_rude = string
let reason_rude_jsont = Jsont.string

type reason_sexual = string
let reason_sexual_jsont = Jsont.string

type reason_spam = string
let reason_spam_jsont = Jsont.string

type reason_type = string
let reason_type_jsont = Jsont.string

type reason_violation = string
let reason_violation_jsont = Jsont.string

type subject_type = string
let subject_type_jsont = Jsont.string

      end
    end
    module Label = struct
      module Defs = struct
type label = {
  cid : string option;
  cts : string;
  exp : string option;
  neg : bool option;
  sig_ : string option;
  src : string;
  uri : string;
  val_ : string;
  ver : int option;
}

let label_jsont =
  Jsont.Object.map ~kind:"Label"
    (fun _typ cid cts exp neg sig_ src uri val_ ver -> { cid; cts; exp; neg; sig_; src; uri; val_; ver })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"com.atproto.label.defs#label" ~enc:(fun _ -> "com.atproto.label.defs#label")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "cts" Jsont.string ~enc:(fun r -> r.cts)
  |> Jsont.Object.opt_mem "exp" Jsont.string ~enc:(fun r -> r.exp)
  |> Jsont.Object.opt_mem "neg" Jsont.bool ~enc:(fun r -> r.neg)
  |> Jsont.Object.opt_mem "sig" Jsont.binary_string ~enc:(fun r -> r.sig_)
  |> Jsont.Object.mem "src" Jsont.string ~enc:(fun r -> r.src)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "val" Jsont.string ~enc:(fun r -> r.val_)
  |> Jsont.Object.opt_mem "ver" Jsont.int ~enc:(fun r -> r.ver)
  |> Jsont.Object.finish

type label_value = string
let label_value_jsont = Jsont.string

type label_value_definition_strings = {
  description : string;
  lang : string;
  name : string;
}

let label_value_definition_strings_jsont =
  Jsont.Object.map ~kind:"Label_value_definition_strings"
    (fun _typ description lang name -> { description; lang; name })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"com.atproto.label.defs#labelValueDefinitionStrings" ~enc:(fun _ -> "com.atproto.label.defs#labelValueDefinitionStrings")
  |> Jsont.Object.mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.mem "lang" Jsont.string ~enc:(fun r -> r.lang)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.finish

type self_label = {
  val_ : string;
}

let self_label_jsont =
  Jsont.Object.map ~kind:"Self_label"
    (fun _typ val_ -> { val_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"com.atproto.label.defs#selfLabel" ~enc:(fun _ -> "com.atproto.label.defs#selfLabel")
  |> Jsont.Object.mem "val" Jsont.string ~enc:(fun r -> r.val_)
  |> Jsont.Object.finish

type label_value_definition = {
  adult_only : bool option;
  blurs : string;
  default_setting : string option;
  identifier : string;
  locales : label_value_definition_strings list;
  severity : string;
}

let label_value_definition_jsont =
  Jsont.Object.map ~kind:"Label_value_definition"
    (fun _typ adult_only blurs default_setting identifier locales severity -> { adult_only; blurs; default_setting; identifier; locales; severity })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"com.atproto.label.defs#labelValueDefinition" ~enc:(fun _ -> "com.atproto.label.defs#labelValueDefinition")
  |> Jsont.Object.opt_mem "adultOnly" Jsont.bool ~enc:(fun r -> r.adult_only)
  |> Jsont.Object.mem "blurs" Jsont.string ~enc:(fun r -> r.blurs)
  |> Jsont.Object.opt_mem "defaultSetting" Jsont.string ~enc:(fun r -> r.default_setting)
  |> Jsont.Object.mem "identifier" Jsont.string ~enc:(fun r -> r.identifier)
  |> Jsont.Object.mem "locales" (Jsont.list label_value_definition_strings_jsont) ~enc:(fun r -> r.locales)
  |> Jsont.Object.mem "severity" Jsont.string ~enc:(fun r -> r.severity)
  |> Jsont.Object.finish

type self_labels = {
  values : self_label list;
}

let self_labels_jsont =
  Jsont.Object.map ~kind:"Self_labels"
    (fun _typ values -> { values })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"com.atproto.label.defs#selfLabels" ~enc:(fun _ -> "com.atproto.label.defs#selfLabels")
  |> Jsont.Object.mem "values" (Jsont.list self_label_jsont) ~enc:(fun r -> r.values)
  |> Jsont.Object.finish

      end
    end
  end
end
module App = struct
  module Bsky = struct
    module Video = struct
      module GetUploadLimits = struct
type output = {
  can_upload : bool;
  error : string option;
  message : string option;
  remaining_daily_bytes : int option;
  remaining_daily_videos : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ can_upload error message remaining_daily_bytes remaining_daily_videos -> { can_upload; error; message; remaining_daily_bytes; remaining_daily_videos })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.video.getUploadLimits#output" ~enc:(fun _ -> "app.bsky.video.getUploadLimits#output")
  |> Jsont.Object.mem "canUpload" Jsont.bool ~enc:(fun r -> r.can_upload)
  |> Jsont.Object.opt_mem "error" Jsont.string ~enc:(fun r -> r.error)
  |> Jsont.Object.opt_mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.opt_mem "remainingDailyBytes" Jsont.int ~enc:(fun r -> r.remaining_daily_bytes)
  |> Jsont.Object.opt_mem "remainingDailyVideos" Jsont.int ~enc:(fun r -> r.remaining_daily_videos)
  |> Jsont.Object.finish

      end
      module Defs = struct
type job_status = {
  blob : Atp.Blob_ref.t option;
  did : string;
  error : string option;
  job_id : string;
  message : string option;
  progress : int option;
  state : string;
}

let job_status_jsont =
  Jsont.Object.map ~kind:"Job_status"
    (fun _typ blob did error job_id message progress state -> { blob; did; error; job_id; message; progress; state })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.video.defs#jobStatus" ~enc:(fun _ -> "app.bsky.video.defs#jobStatus")
  |> Jsont.Object.opt_mem "blob" Atp.Blob_ref.jsont ~enc:(fun r -> r.blob)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.opt_mem "error" Jsont.string ~enc:(fun r -> r.error)
  |> Jsont.Object.mem "jobId" Jsont.string ~enc:(fun r -> r.job_id)
  |> Jsont.Object.opt_mem "message" Jsont.string ~enc:(fun r -> r.message)
  |> Jsont.Object.opt_mem "progress" Jsont.int ~enc:(fun r -> r.progress)
  |> Jsont.Object.mem "state" Jsont.string ~enc:(fun r -> r.state)
  |> Jsont.Object.finish

      end
      module UploadVideo = struct
type input = unit
let input_jsont = Jsont.ignore

type output = {
  job_status : Defs.job_status;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ job_status -> { job_status })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.video.uploadVideo#output" ~enc:(fun _ -> "app.bsky.video.uploadVideo#output")
  |> Jsont.Object.mem "jobStatus" Defs.job_status_jsont ~enc:(fun r -> r.job_status)
  |> Jsont.Object.finish

      end
      module GetJobStatus = struct
type params = {
  job_id : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun job_id -> {
      job_id;
    })
  |> Jsont.Object.mem "jobId" Jsont.string
       ~enc:(fun r -> r.job_id)
  |> Jsont.Object.finish

type output = {
  job_status : Defs.job_status;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ job_status -> { job_status })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.video.getJobStatus#output" ~enc:(fun _ -> "app.bsky.video.getJobStatus#output")
  |> Jsont.Object.mem "jobStatus" Defs.job_status_jsont ~enc:(fun r -> r.job_status)
  |> Jsont.Object.finish

      end
    end
    module Richtext = struct
      module Facet = struct
type byte_slice = {
  byte_end : int;
  byte_start : int;
}

let byte_slice_jsont =
  Jsont.Object.map ~kind:"Byte_slice"
    (fun _typ byte_end byte_start -> { byte_end; byte_start })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.richtext.facet#byteSlice" ~enc:(fun _ -> "app.bsky.richtext.facet#byteSlice")
  |> Jsont.Object.mem "byteEnd" Jsont.int ~enc:(fun r -> r.byte_end)
  |> Jsont.Object.mem "byteStart" Jsont.int ~enc:(fun r -> r.byte_start)
  |> Jsont.Object.finish

type link = {
  uri : string;
}

let link_jsont =
  Jsont.Object.map ~kind:"Link"
    (fun _typ uri -> { uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.richtext.facet#link" ~enc:(fun _ -> "app.bsky.richtext.facet#link")
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type mention = {
  did : string;
}

let mention_jsont =
  Jsont.Object.map ~kind:"Mention"
    (fun _typ did -> { did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.richtext.facet#mention" ~enc:(fun _ -> "app.bsky.richtext.facet#mention")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.finish

type tag = {
  tag : string;
}

let tag_jsont =
  Jsont.Object.map ~kind:"Tag"
    (fun _typ tag -> { tag })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.richtext.facet#tag" ~enc:(fun _ -> "app.bsky.richtext.facet#tag")
  |> Jsont.Object.mem "tag" Jsont.string ~enc:(fun r -> r.tag)
  |> Jsont.Object.finish

type main = {
  features : Jsont.json list;
  index : byte_slice;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ features index -> { features; index })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.richtext.facet" ~enc:(fun _ -> "app.bsky.richtext.facet")
  |> Jsont.Object.mem "features" (Jsont.list Jsont.json) ~enc:(fun r -> r.features)
  |> Jsont.Object.mem "index" byte_slice_jsont ~enc:(fun r -> r.index)
  |> Jsont.Object.finish

      end
    end
    module Notification = struct
      module UpdateSeen = struct
type input = {
  seen_at : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ seen_at -> { seen_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.notification.updateSeen#input" ~enc:(fun _ -> "app.bsky.notification.updateSeen#input")
  |> Jsont.Object.mem "seenAt" Jsont.string ~enc:(fun r -> r.seen_at)
  |> Jsont.Object.finish

      end
      module UnregisterPush = struct
type input = {
  app_id : string;
  platform : string;
  service_did : string;
  token : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ app_id platform service_did token -> { app_id; platform; service_did; token })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.notification.unregisterPush#input" ~enc:(fun _ -> "app.bsky.notification.unregisterPush#input")
  |> Jsont.Object.mem "appId" Jsont.string ~enc:(fun r -> r.app_id)
  |> Jsont.Object.mem "platform" Jsont.string ~enc:(fun r -> r.platform)
  |> Jsont.Object.mem "serviceDid" Jsont.string ~enc:(fun r -> r.service_did)
  |> Jsont.Object.mem "token" Jsont.string ~enc:(fun r -> r.token)
  |> Jsont.Object.finish

      end
      module RegisterPush = struct
type input = {
  age_restricted : bool option;
  app_id : string;
  platform : string;
  service_did : string;
  token : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ age_restricted app_id platform service_did token -> { age_restricted; app_id; platform; service_did; token })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.notification.registerPush#input" ~enc:(fun _ -> "app.bsky.notification.registerPush#input")
  |> Jsont.Object.opt_mem "ageRestricted" Jsont.bool ~enc:(fun r -> r.age_restricted)
  |> Jsont.Object.mem "appId" Jsont.string ~enc:(fun r -> r.app_id)
  |> Jsont.Object.mem "platform" Jsont.string ~enc:(fun r -> r.platform)
  |> Jsont.Object.mem "serviceDid" Jsont.string ~enc:(fun r -> r.service_did)
  |> Jsont.Object.mem "token" Jsont.string ~enc:(fun r -> r.token)
  |> Jsont.Object.finish

      end
      module PutPreferences = struct
type input = {
  priority : bool;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ priority -> { priority })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.notification.putPreferences#input" ~enc:(fun _ -> "app.bsky.notification.putPreferences#input")
  |> Jsont.Object.mem "priority" Jsont.bool ~enc:(fun r -> r.priority)
  |> Jsont.Object.finish

      end
      module ListNotifications = struct
type notification = {
  author : Jsont.json;
  cid : string;
  indexed_at : string;
  is_read : bool;
  labels : Com.Atproto.Label.Defs.label list option;
  reason : string;
  reason_subject : string option;
  record : Jsont.json;
  uri : string;
}

let notification_jsont =
  Jsont.Object.map ~kind:"Notification"
    (fun _typ author cid indexed_at is_read labels reason reason_subject record uri -> { author; cid; indexed_at; is_read; labels; reason; reason_subject; record; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.notification.listNotifications#notification" ~enc:(fun _ -> "app.bsky.notification.listNotifications#notification")
  |> Jsont.Object.mem "author" Jsont.json ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "indexedAt" Jsont.string ~enc:(fun r -> r.indexed_at)
  |> Jsont.Object.mem "isRead" Jsont.bool ~enc:(fun r -> r.is_read)
  |> Jsont.Object.opt_mem "labels" (Jsont.list Com.Atproto.Label.Defs.label_jsont) ~enc:(fun r -> r.labels)
  |> Jsont.Object.mem "reason" Jsont.string ~enc:(fun r -> r.reason)
  |> Jsont.Object.opt_mem "reasonSubject" Jsont.string ~enc:(fun r -> r.reason_subject)
  |> Jsont.Object.mem "record" Jsont.json ~enc:(fun r -> r.record)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type params = {
  cursor : string option;
  limit : int option;
  priority : bool option;
  reasons : string list option;
  seen_at : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit priority reasons seen_at -> {
      cursor;
      limit;
      priority;
      reasons;
      seen_at;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "priority" Jsont.bool
       ~enc:(fun r -> r.priority)
  |> Jsont.Object.opt_mem "reasons" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.reasons)
  |> Jsont.Object.opt_mem "seenAt" Jsont.string
       ~enc:(fun r -> r.seen_at)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  notifications : Jsont.json list;
  priority : bool option;
  seen_at : string option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor notifications priority seen_at -> { cursor; notifications; priority; seen_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.notification.listNotifications#output" ~enc:(fun _ -> "app.bsky.notification.listNotifications#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "notifications" (Jsont.list Jsont.json) ~enc:(fun r -> r.notifications)
  |> Jsont.Object.opt_mem "priority" Jsont.bool ~enc:(fun r -> r.priority)
  |> Jsont.Object.opt_mem "seenAt" Jsont.string ~enc:(fun r -> r.seen_at)
  |> Jsont.Object.finish

      end
      module ListActivitySubscriptions = struct
type params = {
  cursor : string option;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit -> {
      cursor;
      limit;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  subscriptions : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor subscriptions -> { cursor; subscriptions })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.notification.listActivitySubscriptions#output" ~enc:(fun _ -> "app.bsky.notification.listActivitySubscriptions#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "subscriptions" (Jsont.list Jsont.json) ~enc:(fun r -> r.subscriptions)
  |> Jsont.Object.finish

      end
      module GetUnreadCount = struct
type params = {
  priority : bool option;
  seen_at : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun priority seen_at -> {
      priority;
      seen_at;
    })
  |> Jsont.Object.opt_mem "priority" Jsont.bool
       ~enc:(fun r -> r.priority)
  |> Jsont.Object.opt_mem "seenAt" Jsont.string
       ~enc:(fun r -> r.seen_at)
  |> Jsont.Object.finish

type output = {
  count : int;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ count -> { count })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.notification.getUnreadCount#output" ~enc:(fun _ -> "app.bsky.notification.getUnreadCount#output")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.finish

      end
      module Defs = struct
type activity_subscription = {
  post : bool;
  reply : bool;
}

let activity_subscription_jsont =
  Jsont.Object.map ~kind:"Activity_subscription"
    (fun _typ post reply -> { post; reply })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.notification.defs#activitySubscription" ~enc:(fun _ -> "app.bsky.notification.defs#activitySubscription")
  |> Jsont.Object.mem "post" Jsont.bool ~enc:(fun r -> r.post)
  |> Jsont.Object.mem "reply" Jsont.bool ~enc:(fun r -> r.reply)
  |> Jsont.Object.finish

type chat_preference = {
  include_ : string;
  push : bool;
}

let chat_preference_jsont =
  Jsont.Object.map ~kind:"Chat_preference"
    (fun _typ include_ push -> { include_; push })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.notification.defs#chatPreference" ~enc:(fun _ -> "app.bsky.notification.defs#chatPreference")
  |> Jsont.Object.mem "include" Jsont.string ~enc:(fun r -> r.include_)
  |> Jsont.Object.mem "push" Jsont.bool ~enc:(fun r -> r.push)
  |> Jsont.Object.finish

type filterable_preference = {
  include_ : string;
  list_ : bool;
  push : bool;
}

let filterable_preference_jsont =
  Jsont.Object.map ~kind:"Filterable_preference"
    (fun _typ include_ list_ push -> { include_; list_; push })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.notification.defs#filterablePreference" ~enc:(fun _ -> "app.bsky.notification.defs#filterablePreference")
  |> Jsont.Object.mem "include" Jsont.string ~enc:(fun r -> r.include_)
  |> Jsont.Object.mem "list" Jsont.bool ~enc:(fun r -> r.list_)
  |> Jsont.Object.mem "push" Jsont.bool ~enc:(fun r -> r.push)
  |> Jsont.Object.finish

type preference = {
  list_ : bool;
  push : bool;
}

let preference_jsont =
  Jsont.Object.map ~kind:"Preference"
    (fun _typ list_ push -> { list_; push })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.notification.defs#preference" ~enc:(fun _ -> "app.bsky.notification.defs#preference")
  |> Jsont.Object.mem "list" Jsont.bool ~enc:(fun r -> r.list_)
  |> Jsont.Object.mem "push" Jsont.bool ~enc:(fun r -> r.push)
  |> Jsont.Object.finish

type record_deleted = unit

let record_deleted_jsont = Jsont.ignore

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

let preferences_jsont =
  Jsont.Object.map ~kind:"Preferences"
    (fun _typ chat follow like like_via_repost mention quote reply repost repost_via_repost starterpack_joined subscribed_post unverified verified -> { chat; follow; like; like_via_repost; mention; quote; reply; repost; repost_via_repost; starterpack_joined; subscribed_post; unverified; verified })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.notification.defs#preferences" ~enc:(fun _ -> "app.bsky.notification.defs#preferences")
  |> Jsont.Object.mem "chat" Jsont.json ~enc:(fun r -> r.chat)
  |> Jsont.Object.mem "follow" Jsont.json ~enc:(fun r -> r.follow)
  |> Jsont.Object.mem "like" Jsont.json ~enc:(fun r -> r.like)
  |> Jsont.Object.mem "likeViaRepost" Jsont.json ~enc:(fun r -> r.like_via_repost)
  |> Jsont.Object.mem "mention" Jsont.json ~enc:(fun r -> r.mention)
  |> Jsont.Object.mem "quote" Jsont.json ~enc:(fun r -> r.quote)
  |> Jsont.Object.mem "reply" Jsont.json ~enc:(fun r -> r.reply)
  |> Jsont.Object.mem "repost" Jsont.json ~enc:(fun r -> r.repost)
  |> Jsont.Object.mem "repostViaRepost" Jsont.json ~enc:(fun r -> r.repost_via_repost)
  |> Jsont.Object.mem "starterpackJoined" Jsont.json ~enc:(fun r -> r.starterpack_joined)
  |> Jsont.Object.mem "subscribedPost" Jsont.json ~enc:(fun r -> r.subscribed_post)
  |> Jsont.Object.mem "unverified" Jsont.json ~enc:(fun r -> r.unverified)
  |> Jsont.Object.mem "verified" Jsont.json ~enc:(fun r -> r.verified)
  |> Jsont.Object.finish

type subject_activity_subscription = {
  activity_subscription : Jsont.json;
  subject : string;
}

let subject_activity_subscription_jsont =
  Jsont.Object.map ~kind:"Subject_activity_subscription"
    (fun _typ activity_subscription subject -> { activity_subscription; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.notification.defs#subjectActivitySubscription" ~enc:(fun _ -> "app.bsky.notification.defs#subjectActivitySubscription")
  |> Jsont.Object.mem "activitySubscription" Jsont.json ~enc:(fun r -> r.activity_subscription)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module Declaration = struct
type main = {
  allow_subscriptions : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ allow_subscriptions -> { allow_subscriptions })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.notification.declaration" ~enc:(fun _ -> "app.bsky.notification.declaration")
  |> Jsont.Object.mem "allowSubscriptions" Jsont.string ~enc:(fun r -> r.allow_subscriptions)
  |> Jsont.Object.finish

      end
      module PutPreferencesV2 = struct
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

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ chat follow like like_via_repost mention quote reply repost repost_via_repost starterpack_joined subscribed_post unverified verified -> { chat; follow; like; like_via_repost; mention; quote; reply; repost; repost_via_repost; starterpack_joined; subscribed_post; unverified; verified })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.notification.putPreferencesV2#input" ~enc:(fun _ -> "app.bsky.notification.putPreferencesV2#input")
  |> Jsont.Object.opt_mem "chat" Jsont.json ~enc:(fun r -> r.chat)
  |> Jsont.Object.opt_mem "follow" Jsont.json ~enc:(fun r -> r.follow)
  |> Jsont.Object.opt_mem "like" Jsont.json ~enc:(fun r -> r.like)
  |> Jsont.Object.opt_mem "likeViaRepost" Jsont.json ~enc:(fun r -> r.like_via_repost)
  |> Jsont.Object.opt_mem "mention" Jsont.json ~enc:(fun r -> r.mention)
  |> Jsont.Object.opt_mem "quote" Jsont.json ~enc:(fun r -> r.quote)
  |> Jsont.Object.opt_mem "reply" Jsont.json ~enc:(fun r -> r.reply)
  |> Jsont.Object.opt_mem "repost" Jsont.json ~enc:(fun r -> r.repost)
  |> Jsont.Object.opt_mem "repostViaRepost" Jsont.json ~enc:(fun r -> r.repost_via_repost)
  |> Jsont.Object.opt_mem "starterpackJoined" Jsont.json ~enc:(fun r -> r.starterpack_joined)
  |> Jsont.Object.opt_mem "subscribedPost" Jsont.json ~enc:(fun r -> r.subscribed_post)
  |> Jsont.Object.opt_mem "unverified" Jsont.json ~enc:(fun r -> r.unverified)
  |> Jsont.Object.opt_mem "verified" Jsont.json ~enc:(fun r -> r.verified)
  |> Jsont.Object.finish

type output = {
  preferences : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ preferences -> { preferences })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.notification.putPreferencesV2#output" ~enc:(fun _ -> "app.bsky.notification.putPreferencesV2#output")
  |> Jsont.Object.mem "preferences" Jsont.json ~enc:(fun r -> r.preferences)
  |> Jsont.Object.finish

      end
      module PutActivitySubscription = struct
type input = {
  activity_subscription : Jsont.json;
  subject : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ activity_subscription subject -> { activity_subscription; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.notification.putActivitySubscription#input" ~enc:(fun _ -> "app.bsky.notification.putActivitySubscription#input")
  |> Jsont.Object.mem "activitySubscription" Jsont.json ~enc:(fun r -> r.activity_subscription)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = {
  activity_subscription : Jsont.json option;
  subject : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ activity_subscription subject -> { activity_subscription; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.notification.putActivitySubscription#output" ~enc:(fun _ -> "app.bsky.notification.putActivitySubscription#output")
  |> Jsont.Object.opt_mem "activitySubscription" Jsont.json ~enc:(fun r -> r.activity_subscription)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module GetPreferences = struct
type params = unit

let params_jsont = Jsont.ignore

type output = {
  preferences : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ preferences -> { preferences })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.notification.getPreferences#output" ~enc:(fun _ -> "app.bsky.notification.getPreferences#output")
  |> Jsont.Object.mem "preferences" Jsont.json ~enc:(fun r -> r.preferences)
  |> Jsont.Object.finish

      end
    end
    module Labeler = struct
      module Defs = struct
type labeler_policies = {
  label_value_definitions : Com.Atproto.Label.Defs.label_value_definition list option;
  label_values : Com.Atproto.Label.Defs.label_value list;
}

let labeler_policies_jsont =
  Jsont.Object.map ~kind:"Labeler_policies"
    (fun _typ label_value_definitions label_values -> { label_value_definitions; label_values })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.labeler.defs#labelerPolicies" ~enc:(fun _ -> "app.bsky.labeler.defs#labelerPolicies")
  |> Jsont.Object.opt_mem "labelValueDefinitions" (Jsont.list Com.Atproto.Label.Defs.label_value_definition_jsont) ~enc:(fun r -> r.label_value_definitions)
  |> Jsont.Object.mem "labelValues" (Jsont.list Com.Atproto.Label.Defs.label_value_jsont) ~enc:(fun r -> r.label_values)
  |> Jsont.Object.finish

type labeler_viewer_state = {
  like : string option;
}

let labeler_viewer_state_jsont =
  Jsont.Object.map ~kind:"Labeler_viewer_state"
    (fun _typ like -> { like })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.labeler.defs#labelerViewerState" ~enc:(fun _ -> "app.bsky.labeler.defs#labelerViewerState")
  |> Jsont.Object.opt_mem "like" Jsont.string ~enc:(fun r -> r.like)
  |> Jsont.Object.finish

type labeler_view = {
  cid : string;
  creator : Jsont.json;
  indexed_at : string;
  labels : Com.Atproto.Label.Defs.label list option;
  like_count : int option;
  uri : string;
  viewer : Jsont.json option;
}

let labeler_view_jsont =
  Jsont.Object.map ~kind:"Labeler_view"
    (fun _typ cid creator indexed_at labels like_count uri viewer -> { cid; creator; indexed_at; labels; like_count; uri; viewer })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.labeler.defs#labelerView" ~enc:(fun _ -> "app.bsky.labeler.defs#labelerView")
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "creator" Jsont.json ~enc:(fun r -> r.creator)
  |> Jsont.Object.mem "indexedAt" Jsont.string ~enc:(fun r -> r.indexed_at)
  |> Jsont.Object.opt_mem "labels" (Jsont.list Com.Atproto.Label.Defs.label_jsont) ~enc:(fun r -> r.labels)
  |> Jsont.Object.opt_mem "likeCount" Jsont.int ~enc:(fun r -> r.like_count)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.opt_mem "viewer" Jsont.json ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

type labeler_view_detailed = {
  cid : string;
  creator : Jsont.json;
  indexed_at : string;
  labels : Com.Atproto.Label.Defs.label list option;
  like_count : int option;
  policies : Jsont.json;
  reason_types : Com.Atproto.Moderation.Defs.reason_type list option;
  subject_collections : string list option;
  subject_types : Com.Atproto.Moderation.Defs.subject_type list option;
  uri : string;
  viewer : Jsont.json option;
}

let labeler_view_detailed_jsont =
  Jsont.Object.map ~kind:"Labeler_view_detailed"
    (fun _typ cid creator indexed_at labels like_count policies reason_types subject_collections subject_types uri viewer -> { cid; creator; indexed_at; labels; like_count; policies; reason_types; subject_collections; subject_types; uri; viewer })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.labeler.defs#labelerViewDetailed" ~enc:(fun _ -> "app.bsky.labeler.defs#labelerViewDetailed")
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "creator" Jsont.json ~enc:(fun r -> r.creator)
  |> Jsont.Object.mem "indexedAt" Jsont.string ~enc:(fun r -> r.indexed_at)
  |> Jsont.Object.opt_mem "labels" (Jsont.list Com.Atproto.Label.Defs.label_jsont) ~enc:(fun r -> r.labels)
  |> Jsont.Object.opt_mem "likeCount" Jsont.int ~enc:(fun r -> r.like_count)
  |> Jsont.Object.mem "policies" Jsont.json ~enc:(fun r -> r.policies)
  |> Jsont.Object.opt_mem "reasonTypes" (Jsont.list Com.Atproto.Moderation.Defs.reason_type_jsont) ~enc:(fun r -> r.reason_types)
  |> Jsont.Object.opt_mem "subjectCollections" (Jsont.list Jsont.string) ~enc:(fun r -> r.subject_collections)
  |> Jsont.Object.opt_mem "subjectTypes" (Jsont.list Com.Atproto.Moderation.Defs.subject_type_jsont) ~enc:(fun r -> r.subject_types)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.opt_mem "viewer" Jsont.json ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

      end
      module Service = struct
type main = {
  created_at : string;
  labels : Com.Atproto.Label.Defs.self_labels option;
  policies : Jsont.json;
  reason_types : Com.Atproto.Moderation.Defs.reason_type list option;
  subject_collections : string list option;
  subject_types : Com.Atproto.Moderation.Defs.subject_type list option;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at labels policies reason_types subject_collections subject_types -> { created_at; labels; policies; reason_types; subject_collections; subject_types })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.labeler.service" ~enc:(fun _ -> "app.bsky.labeler.service")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "labels" Com.Atproto.Label.Defs.self_labels_jsont ~enc:(fun r -> r.labels)
  |> Jsont.Object.mem "policies" Jsont.json ~enc:(fun r -> r.policies)
  |> Jsont.Object.opt_mem "reasonTypes" (Jsont.list Com.Atproto.Moderation.Defs.reason_type_jsont) ~enc:(fun r -> r.reason_types)
  |> Jsont.Object.opt_mem "subjectCollections" (Jsont.list Jsont.string) ~enc:(fun r -> r.subject_collections)
  |> Jsont.Object.opt_mem "subjectTypes" (Jsont.list Com.Atproto.Moderation.Defs.subject_type_jsont) ~enc:(fun r -> r.subject_types)
  |> Jsont.Object.finish

      end
      module GetServices = struct
type params = {
  detailed : bool option;
  dids : string list;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun detailed dids -> {
      detailed;
      dids;
    })
  |> Jsont.Object.opt_mem "detailed" Jsont.bool
       ~enc:(fun r -> r.detailed)
  |> Jsont.Object.mem "dids" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.dids)
  |> Jsont.Object.finish

type output = {
  views : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ views -> { views })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.labeler.getServices#output" ~enc:(fun _ -> "app.bsky.labeler.getServices#output")
  |> Jsont.Object.mem "views" (Jsont.list Jsont.json) ~enc:(fun r -> r.views)
  |> Jsont.Object.finish

      end
    end
    module Embed = struct
      module External = struct
type external_ = {
  description : string;
  thumb : Atp.Blob_ref.t option;
  title : string;
  uri : string;
}

let external__jsont =
  Jsont.Object.map ~kind:"External_"
    (fun _typ description thumb title uri -> { description; thumb; title; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.embed.external#external" ~enc:(fun _ -> "app.bsky.embed.external#external")
  |> Jsont.Object.mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.opt_mem "thumb" Atp.Blob_ref.jsont ~enc:(fun r -> r.thumb)
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun r -> r.title)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type view_external = {
  description : string;
  thumb : string option;
  title : string;
  uri : string;
}

let view_external_jsont =
  Jsont.Object.map ~kind:"View_external"
    (fun _typ description thumb title uri -> { description; thumb; title; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.embed.external#viewExternal" ~enc:(fun _ -> "app.bsky.embed.external#viewExternal")
  |> Jsont.Object.mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.opt_mem "thumb" Jsont.string ~enc:(fun r -> r.thumb)
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun r -> r.title)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type main = {
  external_ : Jsont.json;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ external_ -> { external_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.embed.external" ~enc:(fun _ -> "app.bsky.embed.external")
  |> Jsont.Object.mem "external" Jsont.json ~enc:(fun r -> r.external_)
  |> Jsont.Object.finish

type view = {
  external_ : Jsont.json;
}

let view_jsont =
  Jsont.Object.map ~kind:"View"
    (fun _typ external_ -> { external_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.embed.external#view" ~enc:(fun _ -> "app.bsky.embed.external#view")
  |> Jsont.Object.mem "external" Jsont.json ~enc:(fun r -> r.external_)
  |> Jsont.Object.finish

      end
      module Defs = struct
type aspect_ratio = {
  height : int;
  width : int;
}

let aspect_ratio_jsont =
  Jsont.Object.map ~kind:"Aspect_ratio"
    (fun _typ height width -> { height; width })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.embed.defs#aspectRatio" ~enc:(fun _ -> "app.bsky.embed.defs#aspectRatio")
  |> Jsont.Object.mem "height" Jsont.int ~enc:(fun r -> r.height)
  |> Jsont.Object.mem "width" Jsont.int ~enc:(fun r -> r.width)
  |> Jsont.Object.finish

      end
      module Video = struct
type caption = {
  file : Atp.Blob_ref.t;
  lang : string;
}

let caption_jsont =
  Jsont.Object.map ~kind:"Caption"
    (fun _typ file lang -> { file; lang })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.embed.video#caption" ~enc:(fun _ -> "app.bsky.embed.video#caption")
  |> Jsont.Object.mem "file" Atp.Blob_ref.jsont ~enc:(fun r -> r.file)
  |> Jsont.Object.mem "lang" Jsont.string ~enc:(fun r -> r.lang)
  |> Jsont.Object.finish

type view = {
  alt : string option;
  aspect_ratio : Jsont.json option;
  cid : string;
  playlist : string;
  thumbnail : string option;
}

let view_jsont =
  Jsont.Object.map ~kind:"View"
    (fun _typ alt aspect_ratio cid playlist thumbnail -> { alt; aspect_ratio; cid; playlist; thumbnail })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.embed.video#view" ~enc:(fun _ -> "app.bsky.embed.video#view")
  |> Jsont.Object.opt_mem "alt" Jsont.string ~enc:(fun r -> r.alt)
  |> Jsont.Object.opt_mem "aspectRatio" Jsont.json ~enc:(fun r -> r.aspect_ratio)
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "playlist" Jsont.string ~enc:(fun r -> r.playlist)
  |> Jsont.Object.opt_mem "thumbnail" Jsont.string ~enc:(fun r -> r.thumbnail)
  |> Jsont.Object.finish

type main = {
  alt : string option;
  aspect_ratio : Jsont.json option;
  captions : Jsont.json list option;
  video : Atp.Blob_ref.t;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ alt aspect_ratio captions video -> { alt; aspect_ratio; captions; video })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.embed.video" ~enc:(fun _ -> "app.bsky.embed.video")
  |> Jsont.Object.opt_mem "alt" Jsont.string ~enc:(fun r -> r.alt)
  |> Jsont.Object.opt_mem "aspectRatio" Jsont.json ~enc:(fun r -> r.aspect_ratio)
  |> Jsont.Object.opt_mem "captions" (Jsont.list Jsont.json) ~enc:(fun r -> r.captions)
  |> Jsont.Object.mem "video" Atp.Blob_ref.jsont ~enc:(fun r -> r.video)
  |> Jsont.Object.finish

      end
      module Images = struct
type image = {
  alt : string;
  aspect_ratio : Jsont.json option;
  image : Atp.Blob_ref.t;
}

let image_jsont =
  Jsont.Object.map ~kind:"Image"
    (fun _typ alt aspect_ratio image -> { alt; aspect_ratio; image })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.embed.images#image" ~enc:(fun _ -> "app.bsky.embed.images#image")
  |> Jsont.Object.mem "alt" Jsont.string ~enc:(fun r -> r.alt)
  |> Jsont.Object.opt_mem "aspectRatio" Jsont.json ~enc:(fun r -> r.aspect_ratio)
  |> Jsont.Object.mem "image" Atp.Blob_ref.jsont ~enc:(fun r -> r.image)
  |> Jsont.Object.finish

type view_image = {
  alt : string;
  aspect_ratio : Jsont.json option;
  fullsize : string;
  thumb : string;
}

let view_image_jsont =
  Jsont.Object.map ~kind:"View_image"
    (fun _typ alt aspect_ratio fullsize thumb -> { alt; aspect_ratio; fullsize; thumb })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.embed.images#viewImage" ~enc:(fun _ -> "app.bsky.embed.images#viewImage")
  |> Jsont.Object.mem "alt" Jsont.string ~enc:(fun r -> r.alt)
  |> Jsont.Object.opt_mem "aspectRatio" Jsont.json ~enc:(fun r -> r.aspect_ratio)
  |> Jsont.Object.mem "fullsize" Jsont.string ~enc:(fun r -> r.fullsize)
  |> Jsont.Object.mem "thumb" Jsont.string ~enc:(fun r -> r.thumb)
  |> Jsont.Object.finish

type main = {
  images : Jsont.json list;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ images -> { images })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.embed.images" ~enc:(fun _ -> "app.bsky.embed.images")
  |> Jsont.Object.mem "images" (Jsont.list Jsont.json) ~enc:(fun r -> r.images)
  |> Jsont.Object.finish

type view = {
  images : Jsont.json list;
}

let view_jsont =
  Jsont.Object.map ~kind:"View"
    (fun _typ images -> { images })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.embed.images#view" ~enc:(fun _ -> "app.bsky.embed.images#view")
  |> Jsont.Object.mem "images" (Jsont.list Jsont.json) ~enc:(fun r -> r.images)
  |> Jsont.Object.finish

      end
      module RecordWithMedia = struct
type main = {
  media : Jsont.json;
  record : Jsont.json;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ media record -> { media; record })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.embed.recordWithMedia" ~enc:(fun _ -> "app.bsky.embed.recordWithMedia")
  |> Jsont.Object.mem "media" Jsont.json ~enc:(fun r -> r.media)
  |> Jsont.Object.mem "record" Jsont.json ~enc:(fun r -> r.record)
  |> Jsont.Object.finish

type view = {
  media : Jsont.json;
  record : Jsont.json;
}

let view_jsont =
  Jsont.Object.map ~kind:"View"
    (fun _typ media record -> { media; record })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.embed.recordWithMedia#view" ~enc:(fun _ -> "app.bsky.embed.recordWithMedia#view")
  |> Jsont.Object.mem "media" Jsont.json ~enc:(fun r -> r.media)
  |> Jsont.Object.mem "record" Jsont.json ~enc:(fun r -> r.record)
  |> Jsont.Object.finish

      end
      module Record = struct
type main = {
  record : Com.Atproto.Repo.StrongRef.main;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ record -> { record })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.embed.record" ~enc:(fun _ -> "app.bsky.embed.record")
  |> Jsont.Object.mem "record" Com.Atproto.Repo.StrongRef.main_jsont ~enc:(fun r -> r.record)
  |> Jsont.Object.finish

type view = {
  record : Jsont.json;
}

let view_jsont =
  Jsont.Object.map ~kind:"View"
    (fun _typ record -> { record })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.embed.record#view" ~enc:(fun _ -> "app.bsky.embed.record#view")
  |> Jsont.Object.mem "record" Jsont.json ~enc:(fun r -> r.record)
  |> Jsont.Object.finish

type view_blocked = {
  author : Jsont.json;
  blocked : bool;
  uri : string;
}

let view_blocked_jsont =
  Jsont.Object.map ~kind:"View_blocked"
    (fun _typ author blocked uri -> { author; blocked; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.embed.record#viewBlocked" ~enc:(fun _ -> "app.bsky.embed.record#viewBlocked")
  |> Jsont.Object.mem "author" Jsont.json ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "blocked" Jsont.bool ~enc:(fun r -> r.blocked)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type view_detached = {
  detached : bool;
  uri : string;
}

let view_detached_jsont =
  Jsont.Object.map ~kind:"View_detached"
    (fun _typ detached uri -> { detached; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.embed.record#viewDetached" ~enc:(fun _ -> "app.bsky.embed.record#viewDetached")
  |> Jsont.Object.mem "detached" Jsont.bool ~enc:(fun r -> r.detached)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type view_not_found = {
  not_found : bool;
  uri : string;
}

let view_not_found_jsont =
  Jsont.Object.map ~kind:"View_not_found"
    (fun _typ not_found uri -> { not_found; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.embed.record#viewNotFound" ~enc:(fun _ -> "app.bsky.embed.record#viewNotFound")
  |> Jsont.Object.mem "notFound" Jsont.bool ~enc:(fun r -> r.not_found)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

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
  value : Jsont.json;
}

let view_record_jsont =
  Jsont.Object.map ~kind:"View_record"
    (fun _typ author cid embeds indexed_at labels like_count quote_count reply_count repost_count uri value -> { author; cid; embeds; indexed_at; labels; like_count; quote_count; reply_count; repost_count; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.embed.record#viewRecord" ~enc:(fun _ -> "app.bsky.embed.record#viewRecord")
  |> Jsont.Object.mem "author" Jsont.json ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.opt_mem "embeds" (Jsont.list Jsont.json) ~enc:(fun r -> r.embeds)
  |> Jsont.Object.mem "indexedAt" Jsont.string ~enc:(fun r -> r.indexed_at)
  |> Jsont.Object.opt_mem "labels" (Jsont.list Com.Atproto.Label.Defs.label_jsont) ~enc:(fun r -> r.labels)
  |> Jsont.Object.opt_mem "likeCount" Jsont.int ~enc:(fun r -> r.like_count)
  |> Jsont.Object.opt_mem "quoteCount" Jsont.int ~enc:(fun r -> r.quote_count)
  |> Jsont.Object.opt_mem "replyCount" Jsont.int ~enc:(fun r -> r.reply_count)
  |> Jsont.Object.opt_mem "repostCount" Jsont.int ~enc:(fun r -> r.repost_count)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

      end
    end
    module AuthViewAll = struct
type main = unit
let main_jsont = Jsont.ignore

    end
    module AuthManageProfile = struct
type main = unit
let main_jsont = Jsont.ignore

    end
    module AuthManageNotifications = struct
type main = unit
let main_jsont = Jsont.ignore

    end
    module AuthManageModeration = struct
type main = unit
let main_jsont = Jsont.ignore

    end
    module AuthManageLabelerService = struct
type main = unit
let main_jsont = Jsont.ignore

    end
    module AuthManageFeedDeclarations = struct
type main = unit
let main_jsont = Jsont.ignore

    end
    module AuthFullApp = struct
type main = unit
let main_jsont = Jsont.ignore

    end
    module AuthCreatePosts = struct
type main = unit
let main_jsont = Jsont.ignore

    end
    module Ageassurance = struct
      module Defs = struct
type access = string
let access_jsont = Jsont.string

type config_region = {
  country_code : string;
  min_access_age : int;
  region_code : string option;
  rules : Jsont.json list;
}

let config_region_jsont =
  Jsont.Object.map ~kind:"Config_region"
    (fun _typ country_code min_access_age region_code rules -> { country_code; min_access_age; region_code; rules })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.ageassurance.defs#configRegion" ~enc:(fun _ -> "app.bsky.ageassurance.defs#configRegion")
  |> Jsont.Object.mem "countryCode" Jsont.string ~enc:(fun r -> r.country_code)
  |> Jsont.Object.mem "minAccessAge" Jsont.int ~enc:(fun r -> r.min_access_age)
  |> Jsont.Object.opt_mem "regionCode" Jsont.string ~enc:(fun r -> r.region_code)
  |> Jsont.Object.mem "rules" (Jsont.list Jsont.json) ~enc:(fun r -> r.rules)
  |> Jsont.Object.finish

type event = {
  access : string;
  attempt_id : string;
  complete_ip : string option;
  complete_ua : string option;
  country_code : string;
  created_at : string;
  email : string option;
  init_ip : string option;
  init_ua : string option;
  region_code : string option;
  status : string;
}

let event_jsont =
  Jsont.Object.map ~kind:"Event"
    (fun _typ access attempt_id complete_ip complete_ua country_code created_at email init_ip init_ua region_code status -> { access; attempt_id; complete_ip; complete_ua; country_code; created_at; email; init_ip; init_ua; region_code; status })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.ageassurance.defs#event" ~enc:(fun _ -> "app.bsky.ageassurance.defs#event")
  |> Jsont.Object.mem "access" Jsont.string ~enc:(fun r -> r.access)
  |> Jsont.Object.mem "attemptId" Jsont.string ~enc:(fun r -> r.attempt_id)
  |> Jsont.Object.opt_mem "completeIp" Jsont.string ~enc:(fun r -> r.complete_ip)
  |> Jsont.Object.opt_mem "completeUa" Jsont.string ~enc:(fun r -> r.complete_ua)
  |> Jsont.Object.mem "countryCode" Jsont.string ~enc:(fun r -> r.country_code)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.opt_mem "initIp" Jsont.string ~enc:(fun r -> r.init_ip)
  |> Jsont.Object.opt_mem "initUa" Jsont.string ~enc:(fun r -> r.init_ua)
  |> Jsont.Object.opt_mem "regionCode" Jsont.string ~enc:(fun r -> r.region_code)
  |> Jsont.Object.mem "status" Jsont.string ~enc:(fun r -> r.status)
  |> Jsont.Object.finish

type state_metadata = {
  account_created_at : string option;
}

let state_metadata_jsont =
  Jsont.Object.map ~kind:"State_metadata"
    (fun _typ account_created_at -> { account_created_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.ageassurance.defs#stateMetadata" ~enc:(fun _ -> "app.bsky.ageassurance.defs#stateMetadata")
  |> Jsont.Object.opt_mem "accountCreatedAt" Jsont.string ~enc:(fun r -> r.account_created_at)
  |> Jsont.Object.finish

type status = string
let status_jsont = Jsont.string

type config = {
  regions : config_region list;
}

let config_jsont =
  Jsont.Object.map ~kind:"Config"
    (fun _typ regions -> { regions })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.ageassurance.defs#config" ~enc:(fun _ -> "app.bsky.ageassurance.defs#config")
  |> Jsont.Object.mem "regions" (Jsont.list config_region_jsont) ~enc:(fun r -> r.regions)
  |> Jsont.Object.finish

type config_region_rule_default = {
  access : access;
}

let config_region_rule_default_jsont =
  Jsont.Object.map ~kind:"Config_region_rule_default"
    (fun _typ access -> { access })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.ageassurance.defs#configRegionRuleDefault" ~enc:(fun _ -> "app.bsky.ageassurance.defs#configRegionRuleDefault")
  |> Jsont.Object.mem "access" access_jsont ~enc:(fun r -> r.access)
  |> Jsont.Object.finish

type config_region_rule_if_account_newer_than = {
  access : access;
  date : string;
}

let config_region_rule_if_account_newer_than_jsont =
  Jsont.Object.map ~kind:"Config_region_rule_if_account_newer_than"
    (fun _typ access date -> { access; date })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.ageassurance.defs#configRegionRuleIfAccountNewerThan" ~enc:(fun _ -> "app.bsky.ageassurance.defs#configRegionRuleIfAccountNewerThan")
  |> Jsont.Object.mem "access" access_jsont ~enc:(fun r -> r.access)
  |> Jsont.Object.mem "date" Jsont.string ~enc:(fun r -> r.date)
  |> Jsont.Object.finish

type config_region_rule_if_account_older_than = {
  access : access;
  date : string;
}

let config_region_rule_if_account_older_than_jsont =
  Jsont.Object.map ~kind:"Config_region_rule_if_account_older_than"
    (fun _typ access date -> { access; date })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.ageassurance.defs#configRegionRuleIfAccountOlderThan" ~enc:(fun _ -> "app.bsky.ageassurance.defs#configRegionRuleIfAccountOlderThan")
  |> Jsont.Object.mem "access" access_jsont ~enc:(fun r -> r.access)
  |> Jsont.Object.mem "date" Jsont.string ~enc:(fun r -> r.date)
  |> Jsont.Object.finish

type config_region_rule_if_assured_over_age = {
  access : access;
  age : int;
}

let config_region_rule_if_assured_over_age_jsont =
  Jsont.Object.map ~kind:"Config_region_rule_if_assured_over_age"
    (fun _typ access age -> { access; age })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.ageassurance.defs#configRegionRuleIfAssuredOverAge" ~enc:(fun _ -> "app.bsky.ageassurance.defs#configRegionRuleIfAssuredOverAge")
  |> Jsont.Object.mem "access" access_jsont ~enc:(fun r -> r.access)
  |> Jsont.Object.mem "age" Jsont.int ~enc:(fun r -> r.age)
  |> Jsont.Object.finish

type config_region_rule_if_assured_under_age = {
  access : access;
  age : int;
}

let config_region_rule_if_assured_under_age_jsont =
  Jsont.Object.map ~kind:"Config_region_rule_if_assured_under_age"
    (fun _typ access age -> { access; age })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.ageassurance.defs#configRegionRuleIfAssuredUnderAge" ~enc:(fun _ -> "app.bsky.ageassurance.defs#configRegionRuleIfAssuredUnderAge")
  |> Jsont.Object.mem "access" access_jsont ~enc:(fun r -> r.access)
  |> Jsont.Object.mem "age" Jsont.int ~enc:(fun r -> r.age)
  |> Jsont.Object.finish

type config_region_rule_if_declared_over_age = {
  access : access;
  age : int;
}

let config_region_rule_if_declared_over_age_jsont =
  Jsont.Object.map ~kind:"Config_region_rule_if_declared_over_age"
    (fun _typ access age -> { access; age })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.ageassurance.defs#configRegionRuleIfDeclaredOverAge" ~enc:(fun _ -> "app.bsky.ageassurance.defs#configRegionRuleIfDeclaredOverAge")
  |> Jsont.Object.mem "access" access_jsont ~enc:(fun r -> r.access)
  |> Jsont.Object.mem "age" Jsont.int ~enc:(fun r -> r.age)
  |> Jsont.Object.finish

type config_region_rule_if_declared_under_age = {
  access : access;
  age : int;
}

let config_region_rule_if_declared_under_age_jsont =
  Jsont.Object.map ~kind:"Config_region_rule_if_declared_under_age"
    (fun _typ access age -> { access; age })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.ageassurance.defs#configRegionRuleIfDeclaredUnderAge" ~enc:(fun _ -> "app.bsky.ageassurance.defs#configRegionRuleIfDeclaredUnderAge")
  |> Jsont.Object.mem "access" access_jsont ~enc:(fun r -> r.access)
  |> Jsont.Object.mem "age" Jsont.int ~enc:(fun r -> r.age)
  |> Jsont.Object.finish

type state = {
  access : access;
  last_initiated_at : string option;
  status : status;
}

let state_jsont =
  Jsont.Object.map ~kind:"State"
    (fun _typ access last_initiated_at status -> { access; last_initiated_at; status })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.ageassurance.defs#state" ~enc:(fun _ -> "app.bsky.ageassurance.defs#state")
  |> Jsont.Object.mem "access" access_jsont ~enc:(fun r -> r.access)
  |> Jsont.Object.opt_mem "lastInitiatedAt" Jsont.string ~enc:(fun r -> r.last_initiated_at)
  |> Jsont.Object.mem "status" status_jsont ~enc:(fun r -> r.status)
  |> Jsont.Object.finish

      end
      module GetState = struct
type params = {
  country_code : string;
  region_code : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun country_code region_code -> {
      country_code;
      region_code;
    })
  |> Jsont.Object.mem "countryCode" Jsont.string
       ~enc:(fun r -> r.country_code)
  |> Jsont.Object.opt_mem "regionCode" Jsont.string
       ~enc:(fun r -> r.region_code)
  |> Jsont.Object.finish

type output = {
  metadata : Defs.state_metadata;
  state : Defs.state;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ metadata state -> { metadata; state })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.ageassurance.getState#output" ~enc:(fun _ -> "app.bsky.ageassurance.getState#output")
  |> Jsont.Object.mem "metadata" Defs.state_metadata_jsont ~enc:(fun r -> r.metadata)
  |> Jsont.Object.mem "state" Defs.state_jsont ~enc:(fun r -> r.state)
  |> Jsont.Object.finish

      end
      module GetConfig = struct
type output = Defs.config

let output_jsont = Defs.config_jsont

      end
      module Begin = struct
type input = {
  country_code : string;
  email : string;
  language : string;
  region_code : string option;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ country_code email language region_code -> { country_code; email; language; region_code })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.ageassurance.begin#input" ~enc:(fun _ -> "app.bsky.ageassurance.begin#input")
  |> Jsont.Object.mem "countryCode" Jsont.string ~enc:(fun r -> r.country_code)
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.mem "language" Jsont.string ~enc:(fun r -> r.language)
  |> Jsont.Object.opt_mem "regionCode" Jsont.string ~enc:(fun r -> r.region_code)
  |> Jsont.Object.finish

type output = Defs.state

let output_jsont = Defs.state_jsont

      end
    end
    module Actor = struct
      module Status = struct
type live = string
let live_jsont = Jsont.string

type main = {
  created_at : string;
  duration_minutes : int option;
  embed : Jsont.json option;
  status : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at duration_minutes embed status -> { created_at; duration_minutes; embed; status })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.status" ~enc:(fun _ -> "app.bsky.actor.status")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "durationMinutes" Jsont.int ~enc:(fun r -> r.duration_minutes)
  |> Jsont.Object.opt_mem "embed" Jsont.json ~enc:(fun r -> r.embed)
  |> Jsont.Object.mem "status" Jsont.string ~enc:(fun r -> r.status)
  |> Jsont.Object.finish

      end
      module Profile = struct
type main = {
  avatar : Atp.Blob_ref.t option;
  banner : Atp.Blob_ref.t option;
  created_at : string option;
  description : string option;
  display_name : string option;
  joined_via_starter_pack : Com.Atproto.Repo.StrongRef.main option;
  labels : Com.Atproto.Label.Defs.self_labels option;
  pinned_post : Com.Atproto.Repo.StrongRef.main option;
  pronouns : string option;
  website : string option;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ avatar banner created_at description display_name joined_via_starter_pack labels pinned_post pronouns website -> { avatar; banner; created_at; description; display_name; joined_via_starter_pack; labels; pinned_post; pronouns; website })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.profile" ~enc:(fun _ -> "app.bsky.actor.profile")
  |> Jsont.Object.opt_mem "avatar" Atp.Blob_ref.jsont ~enc:(fun r -> r.avatar)
  |> Jsont.Object.opt_mem "banner" Atp.Blob_ref.jsont ~enc:(fun r -> r.banner)
  |> Jsont.Object.opt_mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.opt_mem "displayName" Jsont.string ~enc:(fun r -> r.display_name)
  |> Jsont.Object.opt_mem "joinedViaStarterPack" Com.Atproto.Repo.StrongRef.main_jsont ~enc:(fun r -> r.joined_via_starter_pack)
  |> Jsont.Object.opt_mem "labels" Com.Atproto.Label.Defs.self_labels_jsont ~enc:(fun r -> r.labels)
  |> Jsont.Object.opt_mem "pinnedPost" Com.Atproto.Repo.StrongRef.main_jsont ~enc:(fun r -> r.pinned_post)
  |> Jsont.Object.opt_mem "pronouns" Jsont.string ~enc:(fun r -> r.pronouns)
  |> Jsont.Object.opt_mem "website" Jsont.string ~enc:(fun r -> r.website)
  |> Jsont.Object.finish

      end
      module Defs = struct
type adult_content_pref = {
  enabled : bool;
}

let adult_content_pref_jsont =
  Jsont.Object.map ~kind:"Adult_content_pref"
    (fun _typ enabled -> { enabled })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#adultContentPref" ~enc:(fun _ -> "app.bsky.actor.defs#adultContentPref")
  |> Jsont.Object.mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
  |> Jsont.Object.finish

type bsky_app_progress_guide = {
  guide : string;
}

let bsky_app_progress_guide_jsont =
  Jsont.Object.map ~kind:"Bsky_app_progress_guide"
    (fun _typ guide -> { guide })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#bskyAppProgressGuide" ~enc:(fun _ -> "app.bsky.actor.defs#bskyAppProgressGuide")
  |> Jsont.Object.mem "guide" Jsont.string ~enc:(fun r -> r.guide)
  |> Jsont.Object.finish

type content_label_pref = {
  label : string;
  labeler_did : string option;
  visibility : string;
}

let content_label_pref_jsont =
  Jsont.Object.map ~kind:"Content_label_pref"
    (fun _typ label labeler_did visibility -> { label; labeler_did; visibility })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#contentLabelPref" ~enc:(fun _ -> "app.bsky.actor.defs#contentLabelPref")
  |> Jsont.Object.mem "label" Jsont.string ~enc:(fun r -> r.label)
  |> Jsont.Object.opt_mem "labelerDid" Jsont.string ~enc:(fun r -> r.labeler_did)
  |> Jsont.Object.mem "visibility" Jsont.string ~enc:(fun r -> r.visibility)
  |> Jsont.Object.finish

type declared_age_pref = {
  is_over_age13 : bool option;
  is_over_age16 : bool option;
  is_over_age18 : bool option;
}

let declared_age_pref_jsont =
  Jsont.Object.map ~kind:"Declared_age_pref"
    (fun _typ is_over_age13 is_over_age16 is_over_age18 -> { is_over_age13; is_over_age16; is_over_age18 })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#declaredAgePref" ~enc:(fun _ -> "app.bsky.actor.defs#declaredAgePref")
  |> Jsont.Object.opt_mem "isOverAge13" Jsont.bool ~enc:(fun r -> r.is_over_age13)
  |> Jsont.Object.opt_mem "isOverAge16" Jsont.bool ~enc:(fun r -> r.is_over_age16)
  |> Jsont.Object.opt_mem "isOverAge18" Jsont.bool ~enc:(fun r -> r.is_over_age18)
  |> Jsont.Object.finish

type feed_view_pref = {
  feed : string;
  hide_quote_posts : bool option;
  hide_replies : bool option;
  hide_replies_by_like_count : int option;
  hide_replies_by_unfollowed : bool option;
  hide_reposts : bool option;
}

let feed_view_pref_jsont =
  Jsont.Object.map ~kind:"Feed_view_pref"
    (fun _typ feed hide_quote_posts hide_replies hide_replies_by_like_count hide_replies_by_unfollowed hide_reposts -> { feed; hide_quote_posts; hide_replies; hide_replies_by_like_count; hide_replies_by_unfollowed; hide_reposts })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#feedViewPref" ~enc:(fun _ -> "app.bsky.actor.defs#feedViewPref")
  |> Jsont.Object.mem "feed" Jsont.string ~enc:(fun r -> r.feed)
  |> Jsont.Object.opt_mem "hideQuotePosts" Jsont.bool ~enc:(fun r -> r.hide_quote_posts)
  |> Jsont.Object.opt_mem "hideReplies" Jsont.bool ~enc:(fun r -> r.hide_replies)
  |> Jsont.Object.opt_mem "hideRepliesByLikeCount" Jsont.int ~enc:(fun r -> r.hide_replies_by_like_count)
  |> Jsont.Object.opt_mem "hideRepliesByUnfollowed" Jsont.bool ~enc:(fun r -> r.hide_replies_by_unfollowed)
  |> Jsont.Object.opt_mem "hideReposts" Jsont.bool ~enc:(fun r -> r.hide_reposts)
  |> Jsont.Object.finish

type hidden_posts_pref = {
  items : string list;
}

let hidden_posts_pref_jsont =
  Jsont.Object.map ~kind:"Hidden_posts_pref"
    (fun _typ items -> { items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#hiddenPostsPref" ~enc:(fun _ -> "app.bsky.actor.defs#hiddenPostsPref")
  |> Jsont.Object.mem "items" (Jsont.list Jsont.string) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

type interests_pref = {
  tags : string list;
}

let interests_pref_jsont =
  Jsont.Object.map ~kind:"Interests_pref"
    (fun _typ tags -> { tags })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#interestsPref" ~enc:(fun _ -> "app.bsky.actor.defs#interestsPref")
  |> Jsont.Object.mem "tags" (Jsont.list Jsont.string) ~enc:(fun r -> r.tags)
  |> Jsont.Object.finish

type labeler_pref_item = {
  did : string;
}

let labeler_pref_item_jsont =
  Jsont.Object.map ~kind:"Labeler_pref_item"
    (fun _typ did -> { did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#labelerPrefItem" ~enc:(fun _ -> "app.bsky.actor.defs#labelerPrefItem")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.finish

type muted_word_target = string
let muted_word_target_jsont = Jsont.string

type nux = {
  completed : bool;
  data : string option;
  expires_at : string option;
  id : string;
}

let nux_jsont =
  Jsont.Object.map ~kind:"Nux"
    (fun _typ completed data expires_at id -> { completed; data; expires_at; id })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#nux" ~enc:(fun _ -> "app.bsky.actor.defs#nux")
  |> Jsont.Object.mem "completed" Jsont.bool ~enc:(fun r -> r.completed)
  |> Jsont.Object.opt_mem "data" Jsont.string ~enc:(fun r -> r.data)
  |> Jsont.Object.opt_mem "expiresAt" Jsont.string ~enc:(fun r -> r.expires_at)
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
  |> Jsont.Object.finish

type personal_details_pref = {
  birth_date : string option;
}

let personal_details_pref_jsont =
  Jsont.Object.map ~kind:"Personal_details_pref"
    (fun _typ birth_date -> { birth_date })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#personalDetailsPref" ~enc:(fun _ -> "app.bsky.actor.defs#personalDetailsPref")
  |> Jsont.Object.opt_mem "birthDate" Jsont.string ~enc:(fun r -> r.birth_date)
  |> Jsont.Object.finish

type post_interaction_settings_pref = {
  postgate_embedding_rules : Jsont.json list option;
  threadgate_allow_rules : Jsont.json list option;
}

let post_interaction_settings_pref_jsont =
  Jsont.Object.map ~kind:"Post_interaction_settings_pref"
    (fun _typ postgate_embedding_rules threadgate_allow_rules -> { postgate_embedding_rules; threadgate_allow_rules })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#postInteractionSettingsPref" ~enc:(fun _ -> "app.bsky.actor.defs#postInteractionSettingsPref")
  |> Jsont.Object.opt_mem "postgateEmbeddingRules" (Jsont.list Jsont.json) ~enc:(fun r -> r.postgate_embedding_rules)
  |> Jsont.Object.opt_mem "threadgateAllowRules" (Jsont.list Jsont.json) ~enc:(fun r -> r.threadgate_allow_rules)
  |> Jsont.Object.finish

type preferences = Jsont.json list
let preferences_jsont = (Jsont.list Jsont.json)

type profile_associated_activity_subscription = {
  allow_subscriptions : string;
}

let profile_associated_activity_subscription_jsont =
  Jsont.Object.map ~kind:"Profile_associated_activity_subscription"
    (fun _typ allow_subscriptions -> { allow_subscriptions })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#profileAssociatedActivitySubscription" ~enc:(fun _ -> "app.bsky.actor.defs#profileAssociatedActivitySubscription")
  |> Jsont.Object.mem "allowSubscriptions" Jsont.string ~enc:(fun r -> r.allow_subscriptions)
  |> Jsont.Object.finish

type profile_associated_chat = {
  allow_incoming : string;
}

let profile_associated_chat_jsont =
  Jsont.Object.map ~kind:"Profile_associated_chat"
    (fun _typ allow_incoming -> { allow_incoming })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#profileAssociatedChat" ~enc:(fun _ -> "app.bsky.actor.defs#profileAssociatedChat")
  |> Jsont.Object.mem "allowIncoming" Jsont.string ~enc:(fun r -> r.allow_incoming)
  |> Jsont.Object.finish

type saved_feed = {
  id : string;
  pinned : bool;
  type_ : string;
  value : string;
}

let saved_feed_jsont =
  Jsont.Object.map ~kind:"Saved_feed"
    (fun _typ id pinned type_ value -> { id; pinned; type_; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#savedFeed" ~enc:(fun _ -> "app.bsky.actor.defs#savedFeed")
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
  |> Jsont.Object.mem "pinned" Jsont.bool ~enc:(fun r -> r.pinned)
  |> Jsont.Object.mem "type" Jsont.string ~enc:(fun r -> r.type_)
  |> Jsont.Object.mem "value" Jsont.string ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type saved_feeds_pref = {
  pinned : string list;
  saved : string list;
  timeline_index : int option;
}

let saved_feeds_pref_jsont =
  Jsont.Object.map ~kind:"Saved_feeds_pref"
    (fun _typ pinned saved timeline_index -> { pinned; saved; timeline_index })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#savedFeedsPref" ~enc:(fun _ -> "app.bsky.actor.defs#savedFeedsPref")
  |> Jsont.Object.mem "pinned" (Jsont.list Jsont.string) ~enc:(fun r -> r.pinned)
  |> Jsont.Object.mem "saved" (Jsont.list Jsont.string) ~enc:(fun r -> r.saved)
  |> Jsont.Object.opt_mem "timelineIndex" Jsont.int ~enc:(fun r -> r.timeline_index)
  |> Jsont.Object.finish

type status_view = {
  cid : string option;
  embed : Jsont.json option;
  expires_at : string option;
  is_active : bool option;
  is_disabled : bool option;
  record : Jsont.json;
  status : string;
  uri : string option;
}

let status_view_jsont =
  Jsont.Object.map ~kind:"Status_view"
    (fun _typ cid embed expires_at is_active is_disabled record status uri -> { cid; embed; expires_at; is_active; is_disabled; record; status; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#statusView" ~enc:(fun _ -> "app.bsky.actor.defs#statusView")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.opt_mem "embed" Jsont.json ~enc:(fun r -> r.embed)
  |> Jsont.Object.opt_mem "expiresAt" Jsont.string ~enc:(fun r -> r.expires_at)
  |> Jsont.Object.opt_mem "isActive" Jsont.bool ~enc:(fun r -> r.is_active)
  |> Jsont.Object.opt_mem "isDisabled" Jsont.bool ~enc:(fun r -> r.is_disabled)
  |> Jsont.Object.mem "record" Jsont.json ~enc:(fun r -> r.record)
  |> Jsont.Object.mem "status" Jsont.string ~enc:(fun r -> r.status)
  |> Jsont.Object.opt_mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type thread_view_pref = {
  sort : string option;
}

let thread_view_pref_jsont =
  Jsont.Object.map ~kind:"Thread_view_pref"
    (fun _typ sort -> { sort })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#threadViewPref" ~enc:(fun _ -> "app.bsky.actor.defs#threadViewPref")
  |> Jsont.Object.opt_mem "sort" Jsont.string ~enc:(fun r -> r.sort)
  |> Jsont.Object.finish

type verification_prefs = {
  hide_badges : bool option;
}

let verification_prefs_jsont =
  Jsont.Object.map ~kind:"Verification_prefs"
    (fun _typ hide_badges -> { hide_badges })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#verificationPrefs" ~enc:(fun _ -> "app.bsky.actor.defs#verificationPrefs")
  |> Jsont.Object.opt_mem "hideBadges" Jsont.bool ~enc:(fun r -> r.hide_badges)
  |> Jsont.Object.finish

type verification_view = {
  created_at : string;
  is_valid : bool;
  issuer : string;
  uri : string;
}

let verification_view_jsont =
  Jsont.Object.map ~kind:"Verification_view"
    (fun _typ created_at is_valid issuer uri -> { created_at; is_valid; issuer; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#verificationView" ~enc:(fun _ -> "app.bsky.actor.defs#verificationView")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "isValid" Jsont.bool ~enc:(fun r -> r.is_valid)
  |> Jsont.Object.mem "issuer" Jsont.string ~enc:(fun r -> r.issuer)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type bsky_app_state_pref = {
  active_progress_guide : Jsont.json option;
  nuxs : Jsont.json list option;
  queued_nudges : string list option;
}

let bsky_app_state_pref_jsont =
  Jsont.Object.map ~kind:"Bsky_app_state_pref"
    (fun _typ active_progress_guide nuxs queued_nudges -> { active_progress_guide; nuxs; queued_nudges })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#bskyAppStatePref" ~enc:(fun _ -> "app.bsky.actor.defs#bskyAppStatePref")
  |> Jsont.Object.opt_mem "activeProgressGuide" Jsont.json ~enc:(fun r -> r.active_progress_guide)
  |> Jsont.Object.opt_mem "nuxs" (Jsont.list Jsont.json) ~enc:(fun r -> r.nuxs)
  |> Jsont.Object.opt_mem "queuedNudges" (Jsont.list Jsont.string) ~enc:(fun r -> r.queued_nudges)
  |> Jsont.Object.finish

type labelers_pref = {
  labelers : Jsont.json list;
}

let labelers_pref_jsont =
  Jsont.Object.map ~kind:"Labelers_pref"
    (fun _typ labelers -> { labelers })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#labelersPref" ~enc:(fun _ -> "app.bsky.actor.defs#labelersPref")
  |> Jsont.Object.mem "labelers" (Jsont.list Jsont.json) ~enc:(fun r -> r.labelers)
  |> Jsont.Object.finish

type muted_word = {
  actor_target : string option;
  expires_at : string option;
  id : string option;
  targets : Jsont.json list;
  value : string;
}

let muted_word_jsont =
  Jsont.Object.map ~kind:"Muted_word"
    (fun _typ actor_target expires_at id targets value -> { actor_target; expires_at; id; targets; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#mutedWord" ~enc:(fun _ -> "app.bsky.actor.defs#mutedWord")
  |> Jsont.Object.opt_mem "actorTarget" Jsont.string ~enc:(fun r -> r.actor_target)
  |> Jsont.Object.opt_mem "expiresAt" Jsont.string ~enc:(fun r -> r.expires_at)
  |> Jsont.Object.opt_mem "id" Jsont.string ~enc:(fun r -> r.id)
  |> Jsont.Object.mem "targets" (Jsont.list Jsont.json) ~enc:(fun r -> r.targets)
  |> Jsont.Object.mem "value" Jsont.string ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type profile_associated = {
  activity_subscription : Jsont.json option;
  chat : Jsont.json option;
  feedgens : int option;
  labeler : bool option;
  lists : int option;
  starter_packs : int option;
}

let profile_associated_jsont =
  Jsont.Object.map ~kind:"Profile_associated"
    (fun _typ activity_subscription chat feedgens labeler lists starter_packs -> { activity_subscription; chat; feedgens; labeler; lists; starter_packs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#profileAssociated" ~enc:(fun _ -> "app.bsky.actor.defs#profileAssociated")
  |> Jsont.Object.opt_mem "activitySubscription" Jsont.json ~enc:(fun r -> r.activity_subscription)
  |> Jsont.Object.opt_mem "chat" Jsont.json ~enc:(fun r -> r.chat)
  |> Jsont.Object.opt_mem "feedgens" Jsont.int ~enc:(fun r -> r.feedgens)
  |> Jsont.Object.opt_mem "labeler" Jsont.bool ~enc:(fun r -> r.labeler)
  |> Jsont.Object.opt_mem "lists" Jsont.int ~enc:(fun r -> r.lists)
  |> Jsont.Object.opt_mem "starterPacks" Jsont.int ~enc:(fun r -> r.starter_packs)
  |> Jsont.Object.finish

type saved_feeds_pref_v2 = {
  items : Jsont.json list;
}

let saved_feeds_pref_v2_jsont =
  Jsont.Object.map ~kind:"Saved_feeds_pref_v2"
    (fun _typ items -> { items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#savedFeedsPrefV2" ~enc:(fun _ -> "app.bsky.actor.defs#savedFeedsPrefV2")
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

type verification_state = {
  trusted_verifier_status : string;
  verifications : Jsont.json list;
  verified_status : string;
}

let verification_state_jsont =
  Jsont.Object.map ~kind:"Verification_state"
    (fun _typ trusted_verifier_status verifications verified_status -> { trusted_verifier_status; verifications; verified_status })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#verificationState" ~enc:(fun _ -> "app.bsky.actor.defs#verificationState")
  |> Jsont.Object.mem "trustedVerifierStatus" Jsont.string ~enc:(fun r -> r.trusted_verifier_status)
  |> Jsont.Object.mem "verifications" (Jsont.list Jsont.json) ~enc:(fun r -> r.verifications)
  |> Jsont.Object.mem "verifiedStatus" Jsont.string ~enc:(fun r -> r.verified_status)
  |> Jsont.Object.finish

type muted_words_pref = {
  items : Jsont.json list;
}

let muted_words_pref_jsont =
  Jsont.Object.map ~kind:"Muted_words_pref"
    (fun _typ items -> { items })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#mutedWordsPref" ~enc:(fun _ -> "app.bsky.actor.defs#mutedWordsPref")
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.finish

type known_followers = {
  count : int;
  followers : Jsont.json list;
}

let known_followers_jsont =
  Jsont.Object.map ~kind:"Known_followers"
    (fun _typ count followers -> { count; followers })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#knownFollowers" ~enc:(fun _ -> "app.bsky.actor.defs#knownFollowers")
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun r -> r.count)
  |> Jsont.Object.mem "followers" (Jsont.list Jsont.json) ~enc:(fun r -> r.followers)
  |> Jsont.Object.finish

type profile_view = {
  associated : Jsont.json option;
  avatar : string option;
  created_at : string option;
  debug : Jsont.json option;
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

let profile_view_jsont =
  Jsont.Object.map ~kind:"Profile_view"
    (fun _typ associated avatar created_at debug description did display_name handle indexed_at labels pronouns status verification viewer -> { associated; avatar; created_at; debug; description; did; display_name; handle; indexed_at; labels; pronouns; status; verification; viewer })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#profileView" ~enc:(fun _ -> "app.bsky.actor.defs#profileView")
  |> Jsont.Object.opt_mem "associated" Jsont.json ~enc:(fun r -> r.associated)
  |> Jsont.Object.opt_mem "avatar" Jsont.string ~enc:(fun r -> r.avatar)
  |> Jsont.Object.opt_mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "debug" Jsont.json ~enc:(fun r -> r.debug)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.opt_mem "displayName" Jsont.string ~enc:(fun r -> r.display_name)
  |> Jsont.Object.mem "handle" Jsont.string ~enc:(fun r -> r.handle)
  |> Jsont.Object.opt_mem "indexedAt" Jsont.string ~enc:(fun r -> r.indexed_at)
  |> Jsont.Object.opt_mem "labels" (Jsont.list Com.Atproto.Label.Defs.label_jsont) ~enc:(fun r -> r.labels)
  |> Jsont.Object.opt_mem "pronouns" Jsont.string ~enc:(fun r -> r.pronouns)
  |> Jsont.Object.opt_mem "status" Jsont.json ~enc:(fun r -> r.status)
  |> Jsont.Object.opt_mem "verification" Jsont.json ~enc:(fun r -> r.verification)
  |> Jsont.Object.opt_mem "viewer" Jsont.json ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

type profile_view_basic = {
  associated : Jsont.json option;
  avatar : string option;
  created_at : string option;
  debug : Jsont.json option;
  did : string;
  display_name : string option;
  handle : string;
  labels : Com.Atproto.Label.Defs.label list option;
  pronouns : string option;
  status : Jsont.json option;
  verification : Jsont.json option;
  viewer : Jsont.json option;
}

let profile_view_basic_jsont =
  Jsont.Object.map ~kind:"Profile_view_basic"
    (fun _typ associated avatar created_at debug did display_name handle labels pronouns status verification viewer -> { associated; avatar; created_at; debug; did; display_name; handle; labels; pronouns; status; verification; viewer })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#profileViewBasic" ~enc:(fun _ -> "app.bsky.actor.defs#profileViewBasic")
  |> Jsont.Object.opt_mem "associated" Jsont.json ~enc:(fun r -> r.associated)
  |> Jsont.Object.opt_mem "avatar" Jsont.string ~enc:(fun r -> r.avatar)
  |> Jsont.Object.opt_mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "debug" Jsont.json ~enc:(fun r -> r.debug)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.opt_mem "displayName" Jsont.string ~enc:(fun r -> r.display_name)
  |> Jsont.Object.mem "handle" Jsont.string ~enc:(fun r -> r.handle)
  |> Jsont.Object.opt_mem "labels" (Jsont.list Com.Atproto.Label.Defs.label_jsont) ~enc:(fun r -> r.labels)
  |> Jsont.Object.opt_mem "pronouns" Jsont.string ~enc:(fun r -> r.pronouns)
  |> Jsont.Object.opt_mem "status" Jsont.json ~enc:(fun r -> r.status)
  |> Jsont.Object.opt_mem "verification" Jsont.json ~enc:(fun r -> r.verification)
  |> Jsont.Object.opt_mem "viewer" Jsont.json ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

type profile_view_detailed = {
  associated : Jsont.json option;
  avatar : string option;
  banner : string option;
  created_at : string option;
  debug : Jsont.json option;
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

let profile_view_detailed_jsont =
  Jsont.Object.map ~kind:"Profile_view_detailed"
    (fun _typ associated avatar banner created_at debug description did display_name followers_count follows_count handle indexed_at joined_via_starter_pack labels pinned_post posts_count pronouns status verification viewer website -> { associated; avatar; banner; created_at; debug; description; did; display_name; followers_count; follows_count; handle; indexed_at; joined_via_starter_pack; labels; pinned_post; posts_count; pronouns; status; verification; viewer; website })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#profileViewDetailed" ~enc:(fun _ -> "app.bsky.actor.defs#profileViewDetailed")
  |> Jsont.Object.opt_mem "associated" Jsont.json ~enc:(fun r -> r.associated)
  |> Jsont.Object.opt_mem "avatar" Jsont.string ~enc:(fun r -> r.avatar)
  |> Jsont.Object.opt_mem "banner" Jsont.string ~enc:(fun r -> r.banner)
  |> Jsont.Object.opt_mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "debug" Jsont.json ~enc:(fun r -> r.debug)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.opt_mem "displayName" Jsont.string ~enc:(fun r -> r.display_name)
  |> Jsont.Object.opt_mem "followersCount" Jsont.int ~enc:(fun r -> r.followers_count)
  |> Jsont.Object.opt_mem "followsCount" Jsont.int ~enc:(fun r -> r.follows_count)
  |> Jsont.Object.mem "handle" Jsont.string ~enc:(fun r -> r.handle)
  |> Jsont.Object.opt_mem "indexedAt" Jsont.string ~enc:(fun r -> r.indexed_at)
  |> Jsont.Object.opt_mem "joinedViaStarterPack" Jsont.json ~enc:(fun r -> r.joined_via_starter_pack)
  |> Jsont.Object.opt_mem "labels" (Jsont.list Com.Atproto.Label.Defs.label_jsont) ~enc:(fun r -> r.labels)
  |> Jsont.Object.opt_mem "pinnedPost" Com.Atproto.Repo.StrongRef.main_jsont ~enc:(fun r -> r.pinned_post)
  |> Jsont.Object.opt_mem "postsCount" Jsont.int ~enc:(fun r -> r.posts_count)
  |> Jsont.Object.opt_mem "pronouns" Jsont.string ~enc:(fun r -> r.pronouns)
  |> Jsont.Object.opt_mem "status" Jsont.json ~enc:(fun r -> r.status)
  |> Jsont.Object.opt_mem "verification" Jsont.json ~enc:(fun r -> r.verification)
  |> Jsont.Object.opt_mem "viewer" Jsont.json ~enc:(fun r -> r.viewer)
  |> Jsont.Object.opt_mem "website" Jsont.string ~enc:(fun r -> r.website)
  |> Jsont.Object.finish

type viewer_state = {
  activity_subscription : Jsont.json option;
  blocked_by : bool option;
  blocking : string option;
  blocking_by_list : Jsont.json option;
  followed_by : string option;
  following : string option;
  known_followers : Jsont.json option;
  muted : bool option;
  muted_by_list : Jsont.json option;
}

let viewer_state_jsont =
  Jsont.Object.map ~kind:"Viewer_state"
    (fun _typ activity_subscription blocked_by blocking blocking_by_list followed_by following known_followers muted muted_by_list -> { activity_subscription; blocked_by; blocking; blocking_by_list; followed_by; following; known_followers; muted; muted_by_list })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.defs#viewerState" ~enc:(fun _ -> "app.bsky.actor.defs#viewerState")
  |> Jsont.Object.opt_mem "activitySubscription" Jsont.json ~enc:(fun r -> r.activity_subscription)
  |> Jsont.Object.opt_mem "blockedBy" Jsont.bool ~enc:(fun r -> r.blocked_by)
  |> Jsont.Object.opt_mem "blocking" Jsont.string ~enc:(fun r -> r.blocking)
  |> Jsont.Object.opt_mem "blockingByList" Jsont.json ~enc:(fun r -> r.blocking_by_list)
  |> Jsont.Object.opt_mem "followedBy" Jsont.string ~enc:(fun r -> r.followed_by)
  |> Jsont.Object.opt_mem "following" Jsont.string ~enc:(fun r -> r.following)
  |> Jsont.Object.opt_mem "knownFollowers" Jsont.json ~enc:(fun r -> r.known_followers)
  |> Jsont.Object.opt_mem "muted" Jsont.bool ~enc:(fun r -> r.muted)
  |> Jsont.Object.opt_mem "mutedByList" Jsont.json ~enc:(fun r -> r.muted_by_list)
  |> Jsont.Object.finish

      end
      module SearchActorsTypeahead = struct
type params = {
  limit : int option;
  q : string option;
  term : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun limit q term -> {
      limit;
      q;
      term;
    })
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "q" Jsont.string
       ~enc:(fun r -> r.q)
  |> Jsont.Object.opt_mem "term" Jsont.string
       ~enc:(fun r -> r.term)
  |> Jsont.Object.finish

type output = {
  actors : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ actors -> { actors })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.searchActorsTypeahead#output" ~enc:(fun _ -> "app.bsky.actor.searchActorsTypeahead#output")
  |> Jsont.Object.mem "actors" (Jsont.list Jsont.json) ~enc:(fun r -> r.actors)
  |> Jsont.Object.finish

      end
      module SearchActors = struct
type params = {
  cursor : string option;
  limit : int option;
  q : string option;
  term : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit q term -> {
      cursor;
      limit;
      q;
      term;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "q" Jsont.string
       ~enc:(fun r -> r.q)
  |> Jsont.Object.opt_mem "term" Jsont.string
       ~enc:(fun r -> r.term)
  |> Jsont.Object.finish

type output = {
  actors : Jsont.json list;
  cursor : string option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ actors cursor -> { actors; cursor })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.searchActors#output" ~enc:(fun _ -> "app.bsky.actor.searchActors#output")
  |> Jsont.Object.mem "actors" (Jsont.list Jsont.json) ~enc:(fun r -> r.actors)
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.finish

      end
      module PutPreferences = struct
type input = {
  preferences : Jsont.json;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ preferences -> { preferences })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.putPreferences#input" ~enc:(fun _ -> "app.bsky.actor.putPreferences#input")
  |> Jsont.Object.mem "preferences" Jsont.json ~enc:(fun r -> r.preferences)
  |> Jsont.Object.finish

      end
      module GetSuggestions = struct
type params = {
  cursor : string option;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit -> {
      cursor;
      limit;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  actors : Jsont.json list;
  cursor : string option;
  rec_id : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ actors cursor rec_id -> { actors; cursor; rec_id })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.getSuggestions#output" ~enc:(fun _ -> "app.bsky.actor.getSuggestions#output")
  |> Jsont.Object.mem "actors" (Jsont.list Jsont.json) ~enc:(fun r -> r.actors)
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "recId" Jsont.int ~enc:(fun r -> r.rec_id)
  |> Jsont.Object.finish

      end
      module GetProfiles = struct
type params = {
  actors : string list;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun actors -> {
      actors;
    })
  |> Jsont.Object.mem "actors" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.actors)
  |> Jsont.Object.finish

type output = {
  profiles : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ profiles -> { profiles })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.getProfiles#output" ~enc:(fun _ -> "app.bsky.actor.getProfiles#output")
  |> Jsont.Object.mem "profiles" (Jsont.list Jsont.json) ~enc:(fun r -> r.profiles)
  |> Jsont.Object.finish

      end
      module GetProfile = struct
type params = {
  actor : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun actor -> {
      actor;
    })
  |> Jsont.Object.mem "actor" Jsont.string
       ~enc:(fun r -> r.actor)
  |> Jsont.Object.finish

type output = Jsont.json

let output_jsont = Jsont.json

      end
      module GetPreferences = struct
type params = unit

let params_jsont = Jsont.ignore

type output = {
  preferences : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ preferences -> { preferences })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.actor.getPreferences#output" ~enc:(fun _ -> "app.bsky.actor.getPreferences#output")
  |> Jsont.Object.mem "preferences" Jsont.json ~enc:(fun r -> r.preferences)
  |> Jsont.Object.finish

      end
    end
    module Graph = struct
      module Verification = struct
type main = {
  created_at : string;
  display_name : string;
  handle : string;
  subject : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at display_name handle subject -> { created_at; display_name; handle; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.verification" ~enc:(fun _ -> "app.bsky.graph.verification")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "displayName" Jsont.string ~enc:(fun r -> r.display_name)
  |> Jsont.Object.mem "handle" Jsont.string ~enc:(fun r -> r.handle)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module UnmuteThread = struct
type input = {
  root : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ root -> { root })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.unmuteThread#input" ~enc:(fun _ -> "app.bsky.graph.unmuteThread#input")
  |> Jsont.Object.mem "root" Jsont.string ~enc:(fun r -> r.root)
  |> Jsont.Object.finish

      end
      module UnmuteActorList = struct
type input = {
  list_ : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ list_ -> { list_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.unmuteActorList#input" ~enc:(fun _ -> "app.bsky.graph.unmuteActorList#input")
  |> Jsont.Object.mem "list" Jsont.string ~enc:(fun r -> r.list_)
  |> Jsont.Object.finish

      end
      module UnmuteActor = struct
type input = {
  actor : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ actor -> { actor })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.unmuteActor#input" ~enc:(fun _ -> "app.bsky.graph.unmuteActor#input")
  |> Jsont.Object.mem "actor" Jsont.string ~enc:(fun r -> r.actor)
  |> Jsont.Object.finish

      end
      module Starterpack = struct
type feed_item = {
  uri : string;
}

let feed_item_jsont =
  Jsont.Object.map ~kind:"Feed_item"
    (fun _typ uri -> { uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.starterpack#feedItem" ~enc:(fun _ -> "app.bsky.graph.starterpack#feedItem")
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type main = {
  created_at : string;
  description : string option;
  description_facets : Richtext.Facet.main list option;
  feeds : Jsont.json list option;
  list_ : string;
  name : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at description description_facets feeds list_ name -> { created_at; description; description_facets; feeds; list_; name })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.starterpack" ~enc:(fun _ -> "app.bsky.graph.starterpack")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.opt_mem "descriptionFacets" (Jsont.list Richtext.Facet.main_jsont) ~enc:(fun r -> r.description_facets)
  |> Jsont.Object.opt_mem "feeds" (Jsont.list Jsont.json) ~enc:(fun r -> r.feeds)
  |> Jsont.Object.mem "list" Jsont.string ~enc:(fun r -> r.list_)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.finish

      end
      module MuteThread = struct
type input = {
  root : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ root -> { root })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.muteThread#input" ~enc:(fun _ -> "app.bsky.graph.muteThread#input")
  |> Jsont.Object.mem "root" Jsont.string ~enc:(fun r -> r.root)
  |> Jsont.Object.finish

      end
      module MuteActorList = struct
type input = {
  list_ : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ list_ -> { list_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.muteActorList#input" ~enc:(fun _ -> "app.bsky.graph.muteActorList#input")
  |> Jsont.Object.mem "list" Jsont.string ~enc:(fun r -> r.list_)
  |> Jsont.Object.finish

      end
      module MuteActor = struct
type input = {
  actor : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ actor -> { actor })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.muteActor#input" ~enc:(fun _ -> "app.bsky.graph.muteActor#input")
  |> Jsont.Object.mem "actor" Jsont.string ~enc:(fun r -> r.actor)
  |> Jsont.Object.finish

      end
      module Listitem = struct
type main = {
  created_at : string;
  list_ : string;
  subject : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at list_ subject -> { created_at; list_; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.listitem" ~enc:(fun _ -> "app.bsky.graph.listitem")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "list" Jsont.string ~enc:(fun r -> r.list_)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module Listblock = struct
type main = {
  created_at : string;
  subject : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at subject -> { created_at; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.listblock" ~enc:(fun _ -> "app.bsky.graph.listblock")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module GetSuggestedFollowsByActor = struct
type params = {
  actor : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun actor -> {
      actor;
    })
  |> Jsont.Object.mem "actor" Jsont.string
       ~enc:(fun r -> r.actor)
  |> Jsont.Object.finish

type output = {
  is_fallback : bool option;
  rec_id : int option;
  suggestions : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ is_fallback rec_id suggestions -> { is_fallback; rec_id; suggestions })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.getSuggestedFollowsByActor#output" ~enc:(fun _ -> "app.bsky.graph.getSuggestedFollowsByActor#output")
  |> Jsont.Object.opt_mem "isFallback" Jsont.bool ~enc:(fun r -> r.is_fallback)
  |> Jsont.Object.opt_mem "recId" Jsont.int ~enc:(fun r -> r.rec_id)
  |> Jsont.Object.mem "suggestions" (Jsont.list Jsont.json) ~enc:(fun r -> r.suggestions)
  |> Jsont.Object.finish

      end
      module GetMutes = struct
type params = {
  cursor : string option;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit -> {
      cursor;
      limit;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  mutes : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor mutes -> { cursor; mutes })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.getMutes#output" ~enc:(fun _ -> "app.bsky.graph.getMutes#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "mutes" (Jsont.list Jsont.json) ~enc:(fun r -> r.mutes)
  |> Jsont.Object.finish

      end
      module GetKnownFollowers = struct
type params = {
  actor : string;
  cursor : string option;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun actor cursor limit -> {
      actor;
      cursor;
      limit;
    })
  |> Jsont.Object.mem "actor" Jsont.string
       ~enc:(fun r -> r.actor)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  followers : Jsont.json list;
  subject : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor followers subject -> { cursor; followers; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.getKnownFollowers#output" ~enc:(fun _ -> "app.bsky.graph.getKnownFollowers#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "followers" (Jsont.list Jsont.json) ~enc:(fun r -> r.followers)
  |> Jsont.Object.mem "subject" Jsont.json ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module GetFollows = struct
type params = {
  actor : string;
  cursor : string option;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun actor cursor limit -> {
      actor;
      cursor;
      limit;
    })
  |> Jsont.Object.mem "actor" Jsont.string
       ~enc:(fun r -> r.actor)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  follows : Jsont.json list;
  subject : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor follows subject -> { cursor; follows; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.getFollows#output" ~enc:(fun _ -> "app.bsky.graph.getFollows#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "follows" (Jsont.list Jsont.json) ~enc:(fun r -> r.follows)
  |> Jsont.Object.mem "subject" Jsont.json ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module GetFollowers = struct
type params = {
  actor : string;
  cursor : string option;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun actor cursor limit -> {
      actor;
      cursor;
      limit;
    })
  |> Jsont.Object.mem "actor" Jsont.string
       ~enc:(fun r -> r.actor)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  followers : Jsont.json list;
  subject : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor followers subject -> { cursor; followers; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.getFollowers#output" ~enc:(fun _ -> "app.bsky.graph.getFollowers#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "followers" (Jsont.list Jsont.json) ~enc:(fun r -> r.followers)
  |> Jsont.Object.mem "subject" Jsont.json ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module GetBlocks = struct
type params = {
  cursor : string option;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit -> {
      cursor;
      limit;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  blocks : Jsont.json list;
  cursor : string option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ blocks cursor -> { blocks; cursor })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.getBlocks#output" ~enc:(fun _ -> "app.bsky.graph.getBlocks#output")
  |> Jsont.Object.mem "blocks" (Jsont.list Jsont.json) ~enc:(fun r -> r.blocks)
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.finish

      end
      module Follow = struct
type main = {
  created_at : string;
  subject : string;
  via : Com.Atproto.Repo.StrongRef.main option;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at subject via -> { created_at; subject; via })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.follow" ~enc:(fun _ -> "app.bsky.graph.follow")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.opt_mem "via" Com.Atproto.Repo.StrongRef.main_jsont ~enc:(fun r -> r.via)
  |> Jsont.Object.finish

      end
      module Defs = struct
type curatelist = string
let curatelist_jsont = Jsont.string

type list_item_view = {
  subject : Jsont.json;
  uri : string;
}

let list_item_view_jsont =
  Jsont.Object.map ~kind:"List_item_view"
    (fun _typ subject uri -> { subject; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.defs#listItemView" ~enc:(fun _ -> "app.bsky.graph.defs#listItemView")
  |> Jsont.Object.mem "subject" Jsont.json ~enc:(fun r -> r.subject)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type list_purpose = string
let list_purpose_jsont = Jsont.string

type list_viewer_state = {
  blocked : string option;
  muted : bool option;
}

let list_viewer_state_jsont =
  Jsont.Object.map ~kind:"List_viewer_state"
    (fun _typ blocked muted -> { blocked; muted })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.defs#listViewerState" ~enc:(fun _ -> "app.bsky.graph.defs#listViewerState")
  |> Jsont.Object.opt_mem "blocked" Jsont.string ~enc:(fun r -> r.blocked)
  |> Jsont.Object.opt_mem "muted" Jsont.bool ~enc:(fun r -> r.muted)
  |> Jsont.Object.finish

type modlist = string
let modlist_jsont = Jsont.string

type not_found_actor = {
  actor : string;
  not_found : bool;
}

let not_found_actor_jsont =
  Jsont.Object.map ~kind:"Not_found_actor"
    (fun _typ actor not_found -> { actor; not_found })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.defs#notFoundActor" ~enc:(fun _ -> "app.bsky.graph.defs#notFoundActor")
  |> Jsont.Object.mem "actor" Jsont.string ~enc:(fun r -> r.actor)
  |> Jsont.Object.mem "notFound" Jsont.bool ~enc:(fun r -> r.not_found)
  |> Jsont.Object.finish

type referencelist = string
let referencelist_jsont = Jsont.string

type relationship = {
  blocked_by : string option;
  blocked_by_list : string option;
  blocking : string option;
  blocking_by_list : string option;
  did : string;
  followed_by : string option;
  following : string option;
}

let relationship_jsont =
  Jsont.Object.map ~kind:"Relationship"
    (fun _typ blocked_by blocked_by_list blocking blocking_by_list did followed_by following -> { blocked_by; blocked_by_list; blocking; blocking_by_list; did; followed_by; following })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.defs#relationship" ~enc:(fun _ -> "app.bsky.graph.defs#relationship")
  |> Jsont.Object.opt_mem "blockedBy" Jsont.string ~enc:(fun r -> r.blocked_by)
  |> Jsont.Object.opt_mem "blockedByList" Jsont.string ~enc:(fun r -> r.blocked_by_list)
  |> Jsont.Object.opt_mem "blocking" Jsont.string ~enc:(fun r -> r.blocking)
  |> Jsont.Object.opt_mem "blockingByList" Jsont.string ~enc:(fun r -> r.blocking_by_list)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.opt_mem "followedBy" Jsont.string ~enc:(fun r -> r.followed_by)
  |> Jsont.Object.opt_mem "following" Jsont.string ~enc:(fun r -> r.following)
  |> Jsont.Object.finish

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

let starter_pack_view_basic_jsont =
  Jsont.Object.map ~kind:"Starter_pack_view_basic"
    (fun _typ cid creator indexed_at joined_all_time_count joined_week_count labels list_item_count record uri -> { cid; creator; indexed_at; joined_all_time_count; joined_week_count; labels; list_item_count; record; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.defs#starterPackViewBasic" ~enc:(fun _ -> "app.bsky.graph.defs#starterPackViewBasic")
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "creator" Jsont.json ~enc:(fun r -> r.creator)
  |> Jsont.Object.mem "indexedAt" Jsont.string ~enc:(fun r -> r.indexed_at)
  |> Jsont.Object.opt_mem "joinedAllTimeCount" Jsont.int ~enc:(fun r -> r.joined_all_time_count)
  |> Jsont.Object.opt_mem "joinedWeekCount" Jsont.int ~enc:(fun r -> r.joined_week_count)
  |> Jsont.Object.opt_mem "labels" (Jsont.list Com.Atproto.Label.Defs.label_jsont) ~enc:(fun r -> r.labels)
  |> Jsont.Object.opt_mem "listItemCount" Jsont.int ~enc:(fun r -> r.list_item_count)
  |> Jsont.Object.mem "record" Jsont.json ~enc:(fun r -> r.record)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

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

let list_view_jsont =
  Jsont.Object.map ~kind:"List_view"
    (fun _typ avatar cid creator description description_facets indexed_at labels list_item_count name purpose uri viewer -> { avatar; cid; creator; description; description_facets; indexed_at; labels; list_item_count; name; purpose; uri; viewer })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.defs#listView" ~enc:(fun _ -> "app.bsky.graph.defs#listView")
  |> Jsont.Object.opt_mem "avatar" Jsont.string ~enc:(fun r -> r.avatar)
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "creator" Jsont.json ~enc:(fun r -> r.creator)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.opt_mem "descriptionFacets" (Jsont.list Richtext.Facet.main_jsont) ~enc:(fun r -> r.description_facets)
  |> Jsont.Object.mem "indexedAt" Jsont.string ~enc:(fun r -> r.indexed_at)
  |> Jsont.Object.opt_mem "labels" (Jsont.list Com.Atproto.Label.Defs.label_jsont) ~enc:(fun r -> r.labels)
  |> Jsont.Object.opt_mem "listItemCount" Jsont.int ~enc:(fun r -> r.list_item_count)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "purpose" Jsont.json ~enc:(fun r -> r.purpose)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.opt_mem "viewer" Jsont.json ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

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

let list_view_basic_jsont =
  Jsont.Object.map ~kind:"List_view_basic"
    (fun _typ avatar cid indexed_at labels list_item_count name purpose uri viewer -> { avatar; cid; indexed_at; labels; list_item_count; name; purpose; uri; viewer })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.defs#listViewBasic" ~enc:(fun _ -> "app.bsky.graph.defs#listViewBasic")
  |> Jsont.Object.opt_mem "avatar" Jsont.string ~enc:(fun r -> r.avatar)
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.opt_mem "indexedAt" Jsont.string ~enc:(fun r -> r.indexed_at)
  |> Jsont.Object.opt_mem "labels" (Jsont.list Com.Atproto.Label.Defs.label_jsont) ~enc:(fun r -> r.labels)
  |> Jsont.Object.opt_mem "listItemCount" Jsont.int ~enc:(fun r -> r.list_item_count)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "purpose" Jsont.json ~enc:(fun r -> r.purpose)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.opt_mem "viewer" Jsont.json ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

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

let starter_pack_view_jsont =
  Jsont.Object.map ~kind:"Starter_pack_view"
    (fun _typ cid creator feeds indexed_at joined_all_time_count joined_week_count labels list_ list_items_sample record uri -> { cid; creator; feeds; indexed_at; joined_all_time_count; joined_week_count; labels; list_; list_items_sample; record; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.defs#starterPackView" ~enc:(fun _ -> "app.bsky.graph.defs#starterPackView")
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "creator" Jsont.json ~enc:(fun r -> r.creator)
  |> Jsont.Object.opt_mem "feeds" (Jsont.list Jsont.json) ~enc:(fun r -> r.feeds)
  |> Jsont.Object.mem "indexedAt" Jsont.string ~enc:(fun r -> r.indexed_at)
  |> Jsont.Object.opt_mem "joinedAllTimeCount" Jsont.int ~enc:(fun r -> r.joined_all_time_count)
  |> Jsont.Object.opt_mem "joinedWeekCount" Jsont.int ~enc:(fun r -> r.joined_week_count)
  |> Jsont.Object.opt_mem "labels" (Jsont.list Com.Atproto.Label.Defs.label_jsont) ~enc:(fun r -> r.labels)
  |> Jsont.Object.opt_mem "list" Jsont.json ~enc:(fun r -> r.list_)
  |> Jsont.Object.opt_mem "listItemsSample" (Jsont.list Jsont.json) ~enc:(fun r -> r.list_items_sample)
  |> Jsont.Object.mem "record" Jsont.json ~enc:(fun r -> r.record)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

      end
      module Block = struct
type main = {
  created_at : string;
  subject : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at subject -> { created_at; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.block" ~enc:(fun _ -> "app.bsky.graph.block")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module SearchStarterPacks = struct
type params = {
  cursor : string option;
  limit : int option;
  q : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit q -> {
      cursor;
      limit;
      q;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "q" Jsont.string
       ~enc:(fun r -> r.q)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  starter_packs : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor starter_packs -> { cursor; starter_packs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.searchStarterPacks#output" ~enc:(fun _ -> "app.bsky.graph.searchStarterPacks#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "starterPacks" (Jsont.list Jsont.json) ~enc:(fun r -> r.starter_packs)
  |> Jsont.Object.finish

      end
      module List = struct
type main = {
  avatar : Atp.Blob_ref.t option;
  created_at : string;
  description : string option;
  description_facets : Richtext.Facet.main list option;
  labels : Com.Atproto.Label.Defs.self_labels option;
  name : string;
  purpose : Jsont.json;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ avatar created_at description description_facets labels name purpose -> { avatar; created_at; description; description_facets; labels; name; purpose })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.list" ~enc:(fun _ -> "app.bsky.graph.list")
  |> Jsont.Object.opt_mem "avatar" Atp.Blob_ref.jsont ~enc:(fun r -> r.avatar)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.opt_mem "descriptionFacets" (Jsont.list Richtext.Facet.main_jsont) ~enc:(fun r -> r.description_facets)
  |> Jsont.Object.opt_mem "labels" Com.Atproto.Label.Defs.self_labels_jsont ~enc:(fun r -> r.labels)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "purpose" Jsont.json ~enc:(fun r -> r.purpose)
  |> Jsont.Object.finish

      end
      module GetStarterPacksWithMembership = struct
type starter_pack_with_membership = {
  list_item : Jsont.json option;
  starter_pack : Jsont.json;
}

let starter_pack_with_membership_jsont =
  Jsont.Object.map ~kind:"Starter_pack_with_membership"
    (fun _typ list_item starter_pack -> { list_item; starter_pack })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.getStarterPacksWithMembership#starterPackWithMembership" ~enc:(fun _ -> "app.bsky.graph.getStarterPacksWithMembership#starterPackWithMembership")
  |> Jsont.Object.opt_mem "listItem" Jsont.json ~enc:(fun r -> r.list_item)
  |> Jsont.Object.mem "starterPack" Jsont.json ~enc:(fun r -> r.starter_pack)
  |> Jsont.Object.finish

type params = {
  actor : string;
  cursor : string option;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun actor cursor limit -> {
      actor;
      cursor;
      limit;
    })
  |> Jsont.Object.mem "actor" Jsont.string
       ~enc:(fun r -> r.actor)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  starter_packs_with_membership : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor starter_packs_with_membership -> { cursor; starter_packs_with_membership })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.getStarterPacksWithMembership#output" ~enc:(fun _ -> "app.bsky.graph.getStarterPacksWithMembership#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "starterPacksWithMembership" (Jsont.list Jsont.json) ~enc:(fun r -> r.starter_packs_with_membership)
  |> Jsont.Object.finish

      end
      module GetStarterPacks = struct
type params = {
  uris : string list;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun uris -> {
      uris;
    })
  |> Jsont.Object.mem "uris" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.uris)
  |> Jsont.Object.finish

type output = {
  starter_packs : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ starter_packs -> { starter_packs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.getStarterPacks#output" ~enc:(fun _ -> "app.bsky.graph.getStarterPacks#output")
  |> Jsont.Object.mem "starterPacks" (Jsont.list Jsont.json) ~enc:(fun r -> r.starter_packs)
  |> Jsont.Object.finish

      end
      module GetStarterPack = struct
type params = {
  starter_pack : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun starter_pack -> {
      starter_pack;
    })
  |> Jsont.Object.mem "starterPack" Jsont.string
       ~enc:(fun r -> r.starter_pack)
  |> Jsont.Object.finish

type output = {
  starter_pack : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ starter_pack -> { starter_pack })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.getStarterPack#output" ~enc:(fun _ -> "app.bsky.graph.getStarterPack#output")
  |> Jsont.Object.mem "starterPack" Jsont.json ~enc:(fun r -> r.starter_pack)
  |> Jsont.Object.finish

      end
      module GetRelationships = struct
type params = {
  actor : string;
  others : string list option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun actor others -> {
      actor;
      others;
    })
  |> Jsont.Object.mem "actor" Jsont.string
       ~enc:(fun r -> r.actor)
  |> Jsont.Object.opt_mem "others" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.others)
  |> Jsont.Object.finish

type output = {
  actor : string option;
  relationships : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ actor relationships -> { actor; relationships })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.getRelationships#output" ~enc:(fun _ -> "app.bsky.graph.getRelationships#output")
  |> Jsont.Object.opt_mem "actor" Jsont.string ~enc:(fun r -> r.actor)
  |> Jsont.Object.mem "relationships" (Jsont.list Jsont.json) ~enc:(fun r -> r.relationships)
  |> Jsont.Object.finish

      end
      module GetListsWithMembership = struct
type list_with_membership = {
  list_ : Jsont.json;
  list_item : Jsont.json option;
}

let list_with_membership_jsont =
  Jsont.Object.map ~kind:"List_with_membership"
    (fun _typ list_ list_item -> { list_; list_item })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.getListsWithMembership#listWithMembership" ~enc:(fun _ -> "app.bsky.graph.getListsWithMembership#listWithMembership")
  |> Jsont.Object.mem "list" Jsont.json ~enc:(fun r -> r.list_)
  |> Jsont.Object.opt_mem "listItem" Jsont.json ~enc:(fun r -> r.list_item)
  |> Jsont.Object.finish

type params = {
  actor : string;
  cursor : string option;
  limit : int option;
  purposes : string list option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun actor cursor limit purposes -> {
      actor;
      cursor;
      limit;
      purposes;
    })
  |> Jsont.Object.mem "actor" Jsont.string
       ~enc:(fun r -> r.actor)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "purposes" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.purposes)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  lists_with_membership : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor lists_with_membership -> { cursor; lists_with_membership })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.getListsWithMembership#output" ~enc:(fun _ -> "app.bsky.graph.getListsWithMembership#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "listsWithMembership" (Jsont.list Jsont.json) ~enc:(fun r -> r.lists_with_membership)
  |> Jsont.Object.finish

      end
      module GetLists = struct
type params = {
  actor : string;
  cursor : string option;
  limit : int option;
  purposes : string list option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun actor cursor limit purposes -> {
      actor;
      cursor;
      limit;
      purposes;
    })
  |> Jsont.Object.mem "actor" Jsont.string
       ~enc:(fun r -> r.actor)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "purposes" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.purposes)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  lists : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor lists -> { cursor; lists })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.getLists#output" ~enc:(fun _ -> "app.bsky.graph.getLists#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "lists" (Jsont.list Jsont.json) ~enc:(fun r -> r.lists)
  |> Jsont.Object.finish

      end
      module GetListMutes = struct
type params = {
  cursor : string option;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit -> {
      cursor;
      limit;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  lists : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor lists -> { cursor; lists })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.getListMutes#output" ~enc:(fun _ -> "app.bsky.graph.getListMutes#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "lists" (Jsont.list Jsont.json) ~enc:(fun r -> r.lists)
  |> Jsont.Object.finish

      end
      module GetListBlocks = struct
type params = {
  cursor : string option;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit -> {
      cursor;
      limit;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  lists : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor lists -> { cursor; lists })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.getListBlocks#output" ~enc:(fun _ -> "app.bsky.graph.getListBlocks#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "lists" (Jsont.list Jsont.json) ~enc:(fun r -> r.lists)
  |> Jsont.Object.finish

      end
      module GetList = struct
type params = {
  cursor : string option;
  limit : int option;
  list_ : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit list_ -> {
      cursor;
      limit;
      list_;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "list" Jsont.string
       ~enc:(fun r -> r.list_)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  items : Jsont.json list;
  list_ : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor items list_ -> { cursor; items; list_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.getList#output" ~enc:(fun _ -> "app.bsky.graph.getList#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "items" (Jsont.list Jsont.json) ~enc:(fun r -> r.items)
  |> Jsont.Object.mem "list" Jsont.json ~enc:(fun r -> r.list_)
  |> Jsont.Object.finish

      end
      module GetActorStarterPacks = struct
type params = {
  actor : string;
  cursor : string option;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun actor cursor limit -> {
      actor;
      cursor;
      limit;
    })
  |> Jsont.Object.mem "actor" Jsont.string
       ~enc:(fun r -> r.actor)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  starter_packs : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor starter_packs -> { cursor; starter_packs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.graph.getActorStarterPacks#output" ~enc:(fun _ -> "app.bsky.graph.getActorStarterPacks#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "starterPacks" (Jsont.list Jsont.json) ~enc:(fun r -> r.starter_packs)
  |> Jsont.Object.finish

      end
    end
    module Feed = struct
      module Threadgate = struct
type follower_rule = unit

let follower_rule_jsont = Jsont.ignore

type following_rule = unit

let following_rule_jsont = Jsont.ignore

type list_rule = {
  list_ : string;
}

let list_rule_jsont =
  Jsont.Object.map ~kind:"List_rule"
    (fun _typ list_ -> { list_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.threadgate#listRule" ~enc:(fun _ -> "app.bsky.feed.threadgate#listRule")
  |> Jsont.Object.mem "list" Jsont.string ~enc:(fun r -> r.list_)
  |> Jsont.Object.finish

type mention_rule = unit

let mention_rule_jsont = Jsont.ignore

type main = {
  allow : Jsont.json list option;
  created_at : string;
  hidden_replies : string list option;
  post : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ allow created_at hidden_replies post -> { allow; created_at; hidden_replies; post })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.threadgate" ~enc:(fun _ -> "app.bsky.feed.threadgate")
  |> Jsont.Object.opt_mem "allow" (Jsont.list Jsont.json) ~enc:(fun r -> r.allow)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "hiddenReplies" (Jsont.list Jsont.string) ~enc:(fun r -> r.hidden_replies)
  |> Jsont.Object.mem "post" Jsont.string ~enc:(fun r -> r.post)
  |> Jsont.Object.finish

      end
      module Repost = struct
type main = {
  created_at : string;
  subject : Com.Atproto.Repo.StrongRef.main;
  via : Com.Atproto.Repo.StrongRef.main option;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at subject via -> { created_at; subject; via })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.repost" ~enc:(fun _ -> "app.bsky.feed.repost")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "subject" Com.Atproto.Repo.StrongRef.main_jsont ~enc:(fun r -> r.subject)
  |> Jsont.Object.opt_mem "via" Com.Atproto.Repo.StrongRef.main_jsont ~enc:(fun r -> r.via)
  |> Jsont.Object.finish

      end
      module Postgate = struct
type disable_rule = unit

let disable_rule_jsont = Jsont.ignore

type main = {
  created_at : string;
  detached_embedding_uris : string list option;
  embedding_rules : Jsont.json list option;
  post : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at detached_embedding_uris embedding_rules post -> { created_at; detached_embedding_uris; embedding_rules; post })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.postgate" ~enc:(fun _ -> "app.bsky.feed.postgate")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "detachedEmbeddingUris" (Jsont.list Jsont.string) ~enc:(fun r -> r.detached_embedding_uris)
  |> Jsont.Object.opt_mem "embeddingRules" (Jsont.list Jsont.json) ~enc:(fun r -> r.embedding_rules)
  |> Jsont.Object.mem "post" Jsont.string ~enc:(fun r -> r.post)
  |> Jsont.Object.finish

      end
      module Post = struct
type reply_ref = {
  parent : Com.Atproto.Repo.StrongRef.main;
  root : Com.Atproto.Repo.StrongRef.main;
}

let reply_ref_jsont =
  Jsont.Object.map ~kind:"Reply_ref"
    (fun _typ parent root -> { parent; root })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.post#replyRef" ~enc:(fun _ -> "app.bsky.feed.post#replyRef")
  |> Jsont.Object.mem "parent" Com.Atproto.Repo.StrongRef.main_jsont ~enc:(fun r -> r.parent)
  |> Jsont.Object.mem "root" Com.Atproto.Repo.StrongRef.main_jsont ~enc:(fun r -> r.root)
  |> Jsont.Object.finish

type text_slice = {
  end_ : int;
  start : int;
}

let text_slice_jsont =
  Jsont.Object.map ~kind:"Text_slice"
    (fun _typ end_ start -> { end_; start })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.post#textSlice" ~enc:(fun _ -> "app.bsky.feed.post#textSlice")
  |> Jsont.Object.mem "end" Jsont.int ~enc:(fun r -> r.end_)
  |> Jsont.Object.mem "start" Jsont.int ~enc:(fun r -> r.start)
  |> Jsont.Object.finish

type entity = {
  index : Jsont.json;
  type_ : string;
  value : string;
}

let entity_jsont =
  Jsont.Object.map ~kind:"Entity"
    (fun _typ index type_ value -> { index; type_; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.post#entity" ~enc:(fun _ -> "app.bsky.feed.post#entity")
  |> Jsont.Object.mem "index" Jsont.json ~enc:(fun r -> r.index)
  |> Jsont.Object.mem "type" Jsont.string ~enc:(fun r -> r.type_)
  |> Jsont.Object.mem "value" Jsont.string ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type main = {
  created_at : string;
  embed : Jsont.json option;
  entities : Jsont.json list option;
  facets : Richtext.Facet.main list option;
  labels : Com.Atproto.Label.Defs.self_labels option;
  langs : string list option;
  reply : Jsont.json option;
  tags : string list option;
  text : string;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at embed entities facets labels langs reply tags text -> { created_at; embed; entities; facets; labels; langs; reply; tags; text })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.post" ~enc:(fun _ -> "app.bsky.feed.post")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "embed" Jsont.json ~enc:(fun r -> r.embed)
  |> Jsont.Object.opt_mem "entities" (Jsont.list Jsont.json) ~enc:(fun r -> r.entities)
  |> Jsont.Object.opt_mem "facets" (Jsont.list Richtext.Facet.main_jsont) ~enc:(fun r -> r.facets)
  |> Jsont.Object.opt_mem "labels" Com.Atproto.Label.Defs.self_labels_jsont ~enc:(fun r -> r.labels)
  |> Jsont.Object.opt_mem "langs" (Jsont.list Jsont.string) ~enc:(fun r -> r.langs)
  |> Jsont.Object.opt_mem "reply" Jsont.json ~enc:(fun r -> r.reply)
  |> Jsont.Object.opt_mem "tags" (Jsont.list Jsont.string) ~enc:(fun r -> r.tags)
  |> Jsont.Object.mem "text" Jsont.string ~enc:(fun r -> r.text)
  |> Jsont.Object.finish

      end
      module Like = struct
type main = {
  created_at : string;
  subject : Com.Atproto.Repo.StrongRef.main;
  via : Com.Atproto.Repo.StrongRef.main option;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ created_at subject via -> { created_at; subject; via })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.like" ~enc:(fun _ -> "app.bsky.feed.like")
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "subject" Com.Atproto.Repo.StrongRef.main_jsont ~enc:(fun r -> r.subject)
  |> Jsont.Object.opt_mem "via" Com.Atproto.Repo.StrongRef.main_jsont ~enc:(fun r -> r.via)
  |> Jsont.Object.finish

      end
      module GetRepostedBy = struct
type params = {
  cid : string option;
  cursor : string option;
  limit : int option;
  uri : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cid cursor limit uri -> {
      cid;
      cursor;
      limit;
      uri;
    })
  |> Jsont.Object.opt_mem "cid" Jsont.string
       ~enc:(fun r -> r.cid)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "uri" Jsont.string
       ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type output = {
  cid : string option;
  cursor : string option;
  reposted_by : Jsont.json list;
  uri : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cid cursor reposted_by uri -> { cid; cursor; reposted_by; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.getRepostedBy#output" ~enc:(fun _ -> "app.bsky.feed.getRepostedBy#output")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "repostedBy" (Jsont.list Jsont.json) ~enc:(fun r -> r.reposted_by)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

      end
      module GetLikes = struct
type like = {
  actor : Jsont.json;
  created_at : string;
  indexed_at : string;
}

let like_jsont =
  Jsont.Object.map ~kind:"Like"
    (fun _typ actor created_at indexed_at -> { actor; created_at; indexed_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.getLikes#like" ~enc:(fun _ -> "app.bsky.feed.getLikes#like")
  |> Jsont.Object.mem "actor" Jsont.json ~enc:(fun r -> r.actor)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "indexedAt" Jsont.string ~enc:(fun r -> r.indexed_at)
  |> Jsont.Object.finish

type params = {
  cid : string option;
  cursor : string option;
  limit : int option;
  uri : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cid cursor limit uri -> {
      cid;
      cursor;
      limit;
      uri;
    })
  |> Jsont.Object.opt_mem "cid" Jsont.string
       ~enc:(fun r -> r.cid)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "uri" Jsont.string
       ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type output = {
  cid : string option;
  cursor : string option;
  likes : Jsont.json list;
  uri : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cid cursor likes uri -> { cid; cursor; likes; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.getLikes#output" ~enc:(fun _ -> "app.bsky.feed.getLikes#output")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "likes" (Jsont.list Jsont.json) ~enc:(fun r -> r.likes)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

      end
      module Generator = struct
type main = {
  accepts_interactions : bool option;
  avatar : Atp.Blob_ref.t option;
  content_mode : string option;
  created_at : string;
  description : string option;
  description_facets : Richtext.Facet.main list option;
  did : string;
  display_name : string;
  labels : Com.Atproto.Label.Defs.self_labels option;
}

let main_jsont =
  Jsont.Object.map ~kind:"Main"
    (fun _typ accepts_interactions avatar content_mode created_at description description_facets did display_name labels -> { accepts_interactions; avatar; content_mode; created_at; description; description_facets; did; display_name; labels })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.generator" ~enc:(fun _ -> "app.bsky.feed.generator")
  |> Jsont.Object.opt_mem "acceptsInteractions" Jsont.bool ~enc:(fun r -> r.accepts_interactions)
  |> Jsont.Object.opt_mem "avatar" Atp.Blob_ref.jsont ~enc:(fun r -> r.avatar)
  |> Jsont.Object.opt_mem "contentMode" Jsont.string ~enc:(fun r -> r.content_mode)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.opt_mem "descriptionFacets" (Jsont.list Richtext.Facet.main_jsont) ~enc:(fun r -> r.description_facets)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "displayName" Jsont.string ~enc:(fun r -> r.display_name)
  |> Jsont.Object.opt_mem "labels" Com.Atproto.Label.Defs.self_labels_jsont ~enc:(fun r -> r.labels)
  |> Jsont.Object.finish

      end
      module DescribeFeedGenerator = struct
type feed = {
  uri : string;
}

let feed_jsont =
  Jsont.Object.map ~kind:"Feed"
    (fun _typ uri -> { uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.describeFeedGenerator#feed" ~enc:(fun _ -> "app.bsky.feed.describeFeedGenerator#feed")
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type links = {
  privacy_policy : string option;
  terms_of_service : string option;
}

let links_jsont =
  Jsont.Object.map ~kind:"Links"
    (fun _typ privacy_policy terms_of_service -> { privacy_policy; terms_of_service })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.describeFeedGenerator#links" ~enc:(fun _ -> "app.bsky.feed.describeFeedGenerator#links")
  |> Jsont.Object.opt_mem "privacyPolicy" Jsont.string ~enc:(fun r -> r.privacy_policy)
  |> Jsont.Object.opt_mem "termsOfService" Jsont.string ~enc:(fun r -> r.terms_of_service)
  |> Jsont.Object.finish

type output = {
  did : string;
  feeds : Jsont.json list;
  links : Jsont.json option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ did feeds links -> { did; feeds; links })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.describeFeedGenerator#output" ~enc:(fun _ -> "app.bsky.feed.describeFeedGenerator#output")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "feeds" (Jsont.list Jsont.json) ~enc:(fun r -> r.feeds)
  |> Jsont.Object.opt_mem "links" Jsont.json ~enc:(fun r -> r.links)
  |> Jsont.Object.finish

      end
      module Defs = struct
type blocked_author = {
  did : string;
  viewer : Jsont.json option;
}

let blocked_author_jsont =
  Jsont.Object.map ~kind:"Blocked_author"
    (fun _typ did viewer -> { did; viewer })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.defs#blockedAuthor" ~enc:(fun _ -> "app.bsky.feed.defs#blockedAuthor")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.opt_mem "viewer" Jsont.json ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

type clickthrough_author = string
let clickthrough_author_jsont = Jsont.string

type clickthrough_embed = string
let clickthrough_embed_jsont = Jsont.string

type clickthrough_item = string
let clickthrough_item_jsont = Jsont.string

type clickthrough_reposter = string
let clickthrough_reposter_jsont = Jsont.string

type content_mode_unspecified = string
let content_mode_unspecified_jsont = Jsont.string

type content_mode_video = string
let content_mode_video_jsont = Jsont.string

type generator_viewer_state = {
  like : string option;
}

let generator_viewer_state_jsont =
  Jsont.Object.map ~kind:"Generator_viewer_state"
    (fun _typ like -> { like })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.defs#generatorViewerState" ~enc:(fun _ -> "app.bsky.feed.defs#generatorViewerState")
  |> Jsont.Object.opt_mem "like" Jsont.string ~enc:(fun r -> r.like)
  |> Jsont.Object.finish

type interaction = {
  event : string option;
  feed_context : string option;
  item : string option;
  req_id : string option;
}

let interaction_jsont =
  Jsont.Object.map ~kind:"Interaction"
    (fun _typ event feed_context item req_id -> { event; feed_context; item; req_id })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.defs#interaction" ~enc:(fun _ -> "app.bsky.feed.defs#interaction")
  |> Jsont.Object.opt_mem "event" Jsont.string ~enc:(fun r -> r.event)
  |> Jsont.Object.opt_mem "feedContext" Jsont.string ~enc:(fun r -> r.feed_context)
  |> Jsont.Object.opt_mem "item" Jsont.string ~enc:(fun r -> r.item)
  |> Jsont.Object.opt_mem "reqId" Jsont.string ~enc:(fun r -> r.req_id)
  |> Jsont.Object.finish

type interaction_like = string
let interaction_like_jsont = Jsont.string

type interaction_quote = string
let interaction_quote_jsont = Jsont.string

type interaction_reply = string
let interaction_reply_jsont = Jsont.string

type interaction_repost = string
let interaction_repost_jsont = Jsont.string

type interaction_seen = string
let interaction_seen_jsont = Jsont.string

type interaction_share = string
let interaction_share_jsont = Jsont.string

type not_found_post = {
  not_found : bool;
  uri : string;
}

let not_found_post_jsont =
  Jsont.Object.map ~kind:"Not_found_post"
    (fun _typ not_found uri -> { not_found; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.defs#notFoundPost" ~enc:(fun _ -> "app.bsky.feed.defs#notFoundPost")
  |> Jsont.Object.mem "notFound" Jsont.bool ~enc:(fun r -> r.not_found)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type reason_pin = unit

let reason_pin_jsont = Jsont.ignore

type reason_repost = {
  by : Jsont.json;
  cid : string option;
  indexed_at : string;
  uri : string option;
}

let reason_repost_jsont =
  Jsont.Object.map ~kind:"Reason_repost"
    (fun _typ by cid indexed_at uri -> { by; cid; indexed_at; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.defs#reasonRepost" ~enc:(fun _ -> "app.bsky.feed.defs#reasonRepost")
  |> Jsont.Object.mem "by" Jsont.json ~enc:(fun r -> r.by)
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "indexedAt" Jsont.string ~enc:(fun r -> r.indexed_at)
  |> Jsont.Object.opt_mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type reply_ref = {
  grandparent_author : Jsont.json option;
  parent : Jsont.json;
  root : Jsont.json;
}

let reply_ref_jsont =
  Jsont.Object.map ~kind:"Reply_ref"
    (fun _typ grandparent_author parent root -> { grandparent_author; parent; root })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.defs#replyRef" ~enc:(fun _ -> "app.bsky.feed.defs#replyRef")
  |> Jsont.Object.opt_mem "grandparentAuthor" Jsont.json ~enc:(fun r -> r.grandparent_author)
  |> Jsont.Object.mem "parent" Jsont.json ~enc:(fun r -> r.parent)
  |> Jsont.Object.mem "root" Jsont.json ~enc:(fun r -> r.root)
  |> Jsont.Object.finish

type request_less = string
let request_less_jsont = Jsont.string

type request_more = string
let request_more_jsont = Jsont.string

type skeleton_feed_post = {
  feed_context : string option;
  post : string;
  reason : Jsont.json option;
}

let skeleton_feed_post_jsont =
  Jsont.Object.map ~kind:"Skeleton_feed_post"
    (fun _typ feed_context post reason -> { feed_context; post; reason })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.defs#skeletonFeedPost" ~enc:(fun _ -> "app.bsky.feed.defs#skeletonFeedPost")
  |> Jsont.Object.opt_mem "feedContext" Jsont.string ~enc:(fun r -> r.feed_context)
  |> Jsont.Object.mem "post" Jsont.string ~enc:(fun r -> r.post)
  |> Jsont.Object.opt_mem "reason" Jsont.json ~enc:(fun r -> r.reason)
  |> Jsont.Object.finish

type skeleton_reason_pin = unit

let skeleton_reason_pin_jsont = Jsont.ignore

type skeleton_reason_repost = {
  repost : string;
}

let skeleton_reason_repost_jsont =
  Jsont.Object.map ~kind:"Skeleton_reason_repost"
    (fun _typ repost -> { repost })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.defs#skeletonReasonRepost" ~enc:(fun _ -> "app.bsky.feed.defs#skeletonReasonRepost")
  |> Jsont.Object.mem "repost" Jsont.string ~enc:(fun r -> r.repost)
  |> Jsont.Object.finish

type thread_context = {
  root_author_like : string option;
}

let thread_context_jsont =
  Jsont.Object.map ~kind:"Thread_context"
    (fun _typ root_author_like -> { root_author_like })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.defs#threadContext" ~enc:(fun _ -> "app.bsky.feed.defs#threadContext")
  |> Jsont.Object.opt_mem "rootAuthorLike" Jsont.string ~enc:(fun r -> r.root_author_like)
  |> Jsont.Object.finish

type threadgate_view = {
  cid : string option;
  lists : Jsont.json list option;
  record : Jsont.json option;
  uri : string option;
}

let threadgate_view_jsont =
  Jsont.Object.map ~kind:"Threadgate_view"
    (fun _typ cid lists record uri -> { cid; lists; record; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.defs#threadgateView" ~enc:(fun _ -> "app.bsky.feed.defs#threadgateView")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.opt_mem "lists" (Jsont.list Jsont.json) ~enc:(fun r -> r.lists)
  |> Jsont.Object.opt_mem "record" Jsont.json ~enc:(fun r -> r.record)
  |> Jsont.Object.opt_mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type viewer_state = {
  bookmarked : bool option;
  embedding_disabled : bool option;
  like : string option;
  pinned : bool option;
  reply_disabled : bool option;
  repost : string option;
  thread_muted : bool option;
}

let viewer_state_jsont =
  Jsont.Object.map ~kind:"Viewer_state"
    (fun _typ bookmarked embedding_disabled like pinned reply_disabled repost thread_muted -> { bookmarked; embedding_disabled; like; pinned; reply_disabled; repost; thread_muted })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.defs#viewerState" ~enc:(fun _ -> "app.bsky.feed.defs#viewerState")
  |> Jsont.Object.opt_mem "bookmarked" Jsont.bool ~enc:(fun r -> r.bookmarked)
  |> Jsont.Object.opt_mem "embeddingDisabled" Jsont.bool ~enc:(fun r -> r.embedding_disabled)
  |> Jsont.Object.opt_mem "like" Jsont.string ~enc:(fun r -> r.like)
  |> Jsont.Object.opt_mem "pinned" Jsont.bool ~enc:(fun r -> r.pinned)
  |> Jsont.Object.opt_mem "replyDisabled" Jsont.bool ~enc:(fun r -> r.reply_disabled)
  |> Jsont.Object.opt_mem "repost" Jsont.string ~enc:(fun r -> r.repost)
  |> Jsont.Object.opt_mem "threadMuted" Jsont.bool ~enc:(fun r -> r.thread_muted)
  |> Jsont.Object.finish

type blocked_post = {
  author : Jsont.json;
  blocked : bool;
  uri : string;
}

let blocked_post_jsont =
  Jsont.Object.map ~kind:"Blocked_post"
    (fun _typ author blocked uri -> { author; blocked; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.defs#blockedPost" ~enc:(fun _ -> "app.bsky.feed.defs#blockedPost")
  |> Jsont.Object.mem "author" Jsont.json ~enc:(fun r -> r.author)
  |> Jsont.Object.mem "blocked" Jsont.bool ~enc:(fun r -> r.blocked)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

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

let generator_view_jsont =
  Jsont.Object.map ~kind:"Generator_view"
    (fun _typ accepts_interactions avatar cid content_mode creator description description_facets did display_name indexed_at labels like_count uri viewer -> { accepts_interactions; avatar; cid; content_mode; creator; description; description_facets; did; display_name; indexed_at; labels; like_count; uri; viewer })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.defs#generatorView" ~enc:(fun _ -> "app.bsky.feed.defs#generatorView")
  |> Jsont.Object.opt_mem "acceptsInteractions" Jsont.bool ~enc:(fun r -> r.accepts_interactions)
  |> Jsont.Object.opt_mem "avatar" Jsont.string ~enc:(fun r -> r.avatar)
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.opt_mem "contentMode" Jsont.string ~enc:(fun r -> r.content_mode)
  |> Jsont.Object.mem "creator" Jsont.json ~enc:(fun r -> r.creator)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.opt_mem "descriptionFacets" (Jsont.list Richtext.Facet.main_jsont) ~enc:(fun r -> r.description_facets)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "displayName" Jsont.string ~enc:(fun r -> r.display_name)
  |> Jsont.Object.mem "indexedAt" Jsont.string ~enc:(fun r -> r.indexed_at)
  |> Jsont.Object.opt_mem "labels" (Jsont.list Com.Atproto.Label.Defs.label_jsont) ~enc:(fun r -> r.labels)
  |> Jsont.Object.opt_mem "likeCount" Jsont.int ~enc:(fun r -> r.like_count)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.opt_mem "viewer" Jsont.json ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

type post_view = {
  author : Jsont.json;
  bookmark_count : int option;
  cid : string;
  debug : Jsont.json option;
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

let post_view_jsont =
  Jsont.Object.map ~kind:"Post_view"
    (fun _typ author bookmark_count cid debug embed indexed_at labels like_count quote_count record reply_count repost_count threadgate uri viewer -> { author; bookmark_count; cid; debug; embed; indexed_at; labels; like_count; quote_count; record; reply_count; repost_count; threadgate; uri; viewer })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.defs#postView" ~enc:(fun _ -> "app.bsky.feed.defs#postView")
  |> Jsont.Object.mem "author" Jsont.json ~enc:(fun r -> r.author)
  |> Jsont.Object.opt_mem "bookmarkCount" Jsont.int ~enc:(fun r -> r.bookmark_count)
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.opt_mem "debug" Jsont.json ~enc:(fun r -> r.debug)
  |> Jsont.Object.opt_mem "embed" Jsont.json ~enc:(fun r -> r.embed)
  |> Jsont.Object.mem "indexedAt" Jsont.string ~enc:(fun r -> r.indexed_at)
  |> Jsont.Object.opt_mem "labels" (Jsont.list Com.Atproto.Label.Defs.label_jsont) ~enc:(fun r -> r.labels)
  |> Jsont.Object.opt_mem "likeCount" Jsont.int ~enc:(fun r -> r.like_count)
  |> Jsont.Object.opt_mem "quoteCount" Jsont.int ~enc:(fun r -> r.quote_count)
  |> Jsont.Object.mem "record" Jsont.json ~enc:(fun r -> r.record)
  |> Jsont.Object.opt_mem "replyCount" Jsont.int ~enc:(fun r -> r.reply_count)
  |> Jsont.Object.opt_mem "repostCount" Jsont.int ~enc:(fun r -> r.repost_count)
  |> Jsont.Object.opt_mem "threadgate" Jsont.json ~enc:(fun r -> r.threadgate)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.opt_mem "viewer" Jsont.json ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

type feed_view_post = {
  feed_context : string option;
  post : Jsont.json;
  reason : Jsont.json option;
  reply : Jsont.json option;
  req_id : string option;
}

let feed_view_post_jsont =
  Jsont.Object.map ~kind:"Feed_view_post"
    (fun _typ feed_context post reason reply req_id -> { feed_context; post; reason; reply; req_id })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.defs#feedViewPost" ~enc:(fun _ -> "app.bsky.feed.defs#feedViewPost")
  |> Jsont.Object.opt_mem "feedContext" Jsont.string ~enc:(fun r -> r.feed_context)
  |> Jsont.Object.mem "post" Jsont.json ~enc:(fun r -> r.post)
  |> Jsont.Object.opt_mem "reason" Jsont.json ~enc:(fun r -> r.reason)
  |> Jsont.Object.opt_mem "reply" Jsont.json ~enc:(fun r -> r.reply)
  |> Jsont.Object.opt_mem "reqId" Jsont.string ~enc:(fun r -> r.req_id)
  |> Jsont.Object.finish

type thread_view_post = {
  parent : Jsont.json option;
  post : Jsont.json;
  replies : Jsont.json list option;
  thread_context : Jsont.json option;
}

let thread_view_post_jsont =
  Jsont.Object.map ~kind:"Thread_view_post"
    (fun _typ parent post replies thread_context -> { parent; post; replies; thread_context })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.defs#threadViewPost" ~enc:(fun _ -> "app.bsky.feed.defs#threadViewPost")
  |> Jsont.Object.opt_mem "parent" Jsont.json ~enc:(fun r -> r.parent)
  |> Jsont.Object.mem "post" Jsont.json ~enc:(fun r -> r.post)
  |> Jsont.Object.opt_mem "replies" (Jsont.list Jsont.json) ~enc:(fun r -> r.replies)
  |> Jsont.Object.opt_mem "threadContext" Jsont.json ~enc:(fun r -> r.thread_context)
  |> Jsont.Object.finish

      end
      module SendInteractions = struct
type input = {
  interactions : Jsont.json list;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ interactions -> { interactions })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.sendInteractions#input" ~enc:(fun _ -> "app.bsky.feed.sendInteractions#input")
  |> Jsont.Object.mem "interactions" (Jsont.list Jsont.json) ~enc:(fun r -> r.interactions)
  |> Jsont.Object.finish

type output = unit

let output_jsont = Jsont.ignore

      end
      module SearchPosts = struct
type params = {
  author : string option;
  cursor : string option;
  domain : string option;
  lang : string option;
  limit : int option;
  mentions : string option;
  q : string;
  since : string option;
  sort : string option;
  tag : string list option;
  until : string option;
  url : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun author cursor domain lang limit mentions q since sort tag until url -> {
      author;
      cursor;
      domain;
      lang;
      limit;
      mentions;
      q;
      since;
      sort;
      tag;
      until;
      url;
    })
  |> Jsont.Object.opt_mem "author" Jsont.string
       ~enc:(fun r -> r.author)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "domain" Jsont.string
       ~enc:(fun r -> r.domain)
  |> Jsont.Object.opt_mem "lang" Jsont.string
       ~enc:(fun r -> r.lang)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "mentions" Jsont.string
       ~enc:(fun r -> r.mentions)
  |> Jsont.Object.mem "q" Jsont.string
       ~enc:(fun r -> r.q)
  |> Jsont.Object.opt_mem "since" Jsont.string
       ~enc:(fun r -> r.since)
  |> Jsont.Object.opt_mem "sort" Jsont.string
       ~enc:(fun r -> r.sort)
  |> Jsont.Object.opt_mem "tag" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.tag)
  |> Jsont.Object.opt_mem "until" Jsont.string
       ~enc:(fun r -> r.until)
  |> Jsont.Object.opt_mem "url" Jsont.string
       ~enc:(fun r -> r.url)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  hits_total : int option;
  posts : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor hits_total posts -> { cursor; hits_total; posts })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.searchPosts#output" ~enc:(fun _ -> "app.bsky.feed.searchPosts#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "hitsTotal" Jsont.int ~enc:(fun r -> r.hits_total)
  |> Jsont.Object.mem "posts" (Jsont.list Jsont.json) ~enc:(fun r -> r.posts)
  |> Jsont.Object.finish

      end
      module GetTimeline = struct
type params = {
  algorithm : string option;
  cursor : string option;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun algorithm cursor limit -> {
      algorithm;
      cursor;
      limit;
    })
  |> Jsont.Object.opt_mem "algorithm" Jsont.string
       ~enc:(fun r -> r.algorithm)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  feed : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor feed -> { cursor; feed })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.getTimeline#output" ~enc:(fun _ -> "app.bsky.feed.getTimeline#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "feed" (Jsont.list Jsont.json) ~enc:(fun r -> r.feed)
  |> Jsont.Object.finish

      end
      module GetSuggestedFeeds = struct
type params = {
  cursor : string option;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit -> {
      cursor;
      limit;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  feeds : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor feeds -> { cursor; feeds })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.getSuggestedFeeds#output" ~enc:(fun _ -> "app.bsky.feed.getSuggestedFeeds#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "feeds" (Jsont.list Jsont.json) ~enc:(fun r -> r.feeds)
  |> Jsont.Object.finish

      end
      module GetQuotes = struct
type params = {
  cid : string option;
  cursor : string option;
  limit : int option;
  uri : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cid cursor limit uri -> {
      cid;
      cursor;
      limit;
      uri;
    })
  |> Jsont.Object.opt_mem "cid" Jsont.string
       ~enc:(fun r -> r.cid)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "uri" Jsont.string
       ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type output = {
  cid : string option;
  cursor : string option;
  posts : Jsont.json list;
  uri : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cid cursor posts uri -> { cid; cursor; posts; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.getQuotes#output" ~enc:(fun _ -> "app.bsky.feed.getQuotes#output")
  |> Jsont.Object.opt_mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "posts" (Jsont.list Jsont.json) ~enc:(fun r -> r.posts)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

      end
      module GetPosts = struct
type params = {
  uris : string list;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun uris -> {
      uris;
    })
  |> Jsont.Object.mem "uris" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.uris)
  |> Jsont.Object.finish

type output = {
  posts : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ posts -> { posts })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.getPosts#output" ~enc:(fun _ -> "app.bsky.feed.getPosts#output")
  |> Jsont.Object.mem "posts" (Jsont.list Jsont.json) ~enc:(fun r -> r.posts)
  |> Jsont.Object.finish

      end
      module GetPostThread = struct
type params = {
  depth : int option;
  parent_height : int option;
  uri : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun depth parent_height uri -> {
      depth;
      parent_height;
      uri;
    })
  |> Jsont.Object.opt_mem "depth" Jsont.int
       ~enc:(fun r -> r.depth)
  |> Jsont.Object.opt_mem "parentHeight" Jsont.int
       ~enc:(fun r -> r.parent_height)
  |> Jsont.Object.mem "uri" Jsont.string
       ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type output = {
  thread : Jsont.json;
  threadgate : Jsont.json option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ thread threadgate -> { thread; threadgate })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.getPostThread#output" ~enc:(fun _ -> "app.bsky.feed.getPostThread#output")
  |> Jsont.Object.mem "thread" Jsont.json ~enc:(fun r -> r.thread)
  |> Jsont.Object.opt_mem "threadgate" Jsont.json ~enc:(fun r -> r.threadgate)
  |> Jsont.Object.finish

      end
      module GetListFeed = struct
type params = {
  cursor : string option;
  limit : int option;
  list_ : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit list_ -> {
      cursor;
      limit;
      list_;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "list" Jsont.string
       ~enc:(fun r -> r.list_)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  feed : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor feed -> { cursor; feed })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.getListFeed#output" ~enc:(fun _ -> "app.bsky.feed.getListFeed#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "feed" (Jsont.list Jsont.json) ~enc:(fun r -> r.feed)
  |> Jsont.Object.finish

      end
      module GetFeedSkeleton = struct
type params = {
  cursor : string option;
  feed : string;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor feed limit -> {
      cursor;
      feed;
      limit;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "feed" Jsont.string
       ~enc:(fun r -> r.feed)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  feed : Jsont.json list;
  req_id : string option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor feed req_id -> { cursor; feed; req_id })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.getFeedSkeleton#output" ~enc:(fun _ -> "app.bsky.feed.getFeedSkeleton#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "feed" (Jsont.list Jsont.json) ~enc:(fun r -> r.feed)
  |> Jsont.Object.opt_mem "reqId" Jsont.string ~enc:(fun r -> r.req_id)
  |> Jsont.Object.finish

      end
      module GetFeedGenerators = struct
type params = {
  feeds : string list;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun feeds -> {
      feeds;
    })
  |> Jsont.Object.mem "feeds" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.feeds)
  |> Jsont.Object.finish

type output = {
  feeds : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ feeds -> { feeds })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.getFeedGenerators#output" ~enc:(fun _ -> "app.bsky.feed.getFeedGenerators#output")
  |> Jsont.Object.mem "feeds" (Jsont.list Jsont.json) ~enc:(fun r -> r.feeds)
  |> Jsont.Object.finish

      end
      module GetFeedGenerator = struct
type params = {
  feed : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun feed -> {
      feed;
    })
  |> Jsont.Object.mem "feed" Jsont.string
       ~enc:(fun r -> r.feed)
  |> Jsont.Object.finish

type output = {
  is_online : bool;
  is_valid : bool;
  view : Jsont.json;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ is_online is_valid view -> { is_online; is_valid; view })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.getFeedGenerator#output" ~enc:(fun _ -> "app.bsky.feed.getFeedGenerator#output")
  |> Jsont.Object.mem "isOnline" Jsont.bool ~enc:(fun r -> r.is_online)
  |> Jsont.Object.mem "isValid" Jsont.bool ~enc:(fun r -> r.is_valid)
  |> Jsont.Object.mem "view" Jsont.json ~enc:(fun r -> r.view)
  |> Jsont.Object.finish

      end
      module GetFeed = struct
type params = {
  cursor : string option;
  feed : string;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor feed limit -> {
      cursor;
      feed;
      limit;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "feed" Jsont.string
       ~enc:(fun r -> r.feed)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  feed : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor feed -> { cursor; feed })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.getFeed#output" ~enc:(fun _ -> "app.bsky.feed.getFeed#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "feed" (Jsont.list Jsont.json) ~enc:(fun r -> r.feed)
  |> Jsont.Object.finish

      end
      module GetAuthorFeed = struct
type params = {
  actor : string;
  cursor : string option;
  filter : string option;
  include_pins : bool option;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun actor cursor filter include_pins limit -> {
      actor;
      cursor;
      filter;
      include_pins;
      limit;
    })
  |> Jsont.Object.mem "actor" Jsont.string
       ~enc:(fun r -> r.actor)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "filter" Jsont.string
       ~enc:(fun r -> r.filter)
  |> Jsont.Object.opt_mem "includePins" Jsont.bool
       ~enc:(fun r -> r.include_pins)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  feed : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor feed -> { cursor; feed })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.getAuthorFeed#output" ~enc:(fun _ -> "app.bsky.feed.getAuthorFeed#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "feed" (Jsont.list Jsont.json) ~enc:(fun r -> r.feed)
  |> Jsont.Object.finish

      end
      module GetActorLikes = struct
type params = {
  actor : string;
  cursor : string option;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun actor cursor limit -> {
      actor;
      cursor;
      limit;
    })
  |> Jsont.Object.mem "actor" Jsont.string
       ~enc:(fun r -> r.actor)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  feed : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor feed -> { cursor; feed })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.getActorLikes#output" ~enc:(fun _ -> "app.bsky.feed.getActorLikes#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "feed" (Jsont.list Jsont.json) ~enc:(fun r -> r.feed)
  |> Jsont.Object.finish

      end
      module GetActorFeeds = struct
type params = {
  actor : string;
  cursor : string option;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun actor cursor limit -> {
      actor;
      cursor;
      limit;
    })
  |> Jsont.Object.mem "actor" Jsont.string
       ~enc:(fun r -> r.actor)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  feeds : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor feeds -> { cursor; feeds })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.feed.getActorFeeds#output" ~enc:(fun _ -> "app.bsky.feed.getActorFeeds#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "feeds" (Jsont.list Jsont.json) ~enc:(fun r -> r.feeds)
  |> Jsont.Object.finish

      end
    end
    module Contact = struct
      module VerifyPhone = struct
type input = {
  code : string;
  phone : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ code phone -> { code; phone })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.contact.verifyPhone#input" ~enc:(fun _ -> "app.bsky.contact.verifyPhone#input")
  |> Jsont.Object.mem "code" Jsont.string ~enc:(fun r -> r.code)
  |> Jsont.Object.mem "phone" Jsont.string ~enc:(fun r -> r.phone)
  |> Jsont.Object.finish

type output = {
  token : string;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ token -> { token })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.contact.verifyPhone#output" ~enc:(fun _ -> "app.bsky.contact.verifyPhone#output")
  |> Jsont.Object.mem "token" Jsont.string ~enc:(fun r -> r.token)
  |> Jsont.Object.finish

      end
      module StartPhoneVerification = struct
type input = {
  phone : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ phone -> { phone })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.contact.startPhoneVerification#input" ~enc:(fun _ -> "app.bsky.contact.startPhoneVerification#input")
  |> Jsont.Object.mem "phone" Jsont.string ~enc:(fun r -> r.phone)
  |> Jsont.Object.finish

type output = unit

let output_jsont = Jsont.ignore

      end
      module SendNotification = struct
type input = {
  from : string;
  to_ : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ from to_ -> { from; to_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.contact.sendNotification#input" ~enc:(fun _ -> "app.bsky.contact.sendNotification#input")
  |> Jsont.Object.mem "from" Jsont.string ~enc:(fun r -> r.from)
  |> Jsont.Object.mem "to" Jsont.string ~enc:(fun r -> r.to_)
  |> Jsont.Object.finish

type output = unit

let output_jsont = Jsont.ignore

      end
      module RemoveData = struct
type input = unit

let input_jsont = Jsont.ignore

type output = unit

let output_jsont = Jsont.ignore

      end
      module GetMatches = struct
type params = {
  cursor : string option;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit -> {
      cursor;
      limit;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  matches : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor matches -> { cursor; matches })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.contact.getMatches#output" ~enc:(fun _ -> "app.bsky.contact.getMatches#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "matches" (Jsont.list Jsont.json) ~enc:(fun r -> r.matches)
  |> Jsont.Object.finish

      end
      module DismissMatch = struct
type input = {
  subject : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ subject -> { subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.contact.dismissMatch#input" ~enc:(fun _ -> "app.bsky.contact.dismissMatch#input")
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type output = unit

let output_jsont = Jsont.ignore

      end
      module Defs = struct
type match_and_contact_index = {
  contact_index : int;
  match_ : Jsont.json;
}

let match_and_contact_index_jsont =
  Jsont.Object.map ~kind:"Match_and_contact_index"
    (fun _typ contact_index match_ -> { contact_index; match_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.contact.defs#matchAndContactIndex" ~enc:(fun _ -> "app.bsky.contact.defs#matchAndContactIndex")
  |> Jsont.Object.mem "contactIndex" Jsont.int ~enc:(fun r -> r.contact_index)
  |> Jsont.Object.mem "match" Jsont.json ~enc:(fun r -> r.match_)
  |> Jsont.Object.finish

type notification = {
  from : string;
  to_ : string;
}

let notification_jsont =
  Jsont.Object.map ~kind:"Notification"
    (fun _typ from to_ -> { from; to_ })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.contact.defs#notification" ~enc:(fun _ -> "app.bsky.contact.defs#notification")
  |> Jsont.Object.mem "from" Jsont.string ~enc:(fun r -> r.from)
  |> Jsont.Object.mem "to" Jsont.string ~enc:(fun r -> r.to_)
  |> Jsont.Object.finish

type sync_status = {
  matches_count : int;
  synced_at : string;
}

let sync_status_jsont =
  Jsont.Object.map ~kind:"Sync_status"
    (fun _typ matches_count synced_at -> { matches_count; synced_at })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.contact.defs#syncStatus" ~enc:(fun _ -> "app.bsky.contact.defs#syncStatus")
  |> Jsont.Object.mem "matchesCount" Jsont.int ~enc:(fun r -> r.matches_count)
  |> Jsont.Object.mem "syncedAt" Jsont.string ~enc:(fun r -> r.synced_at)
  |> Jsont.Object.finish

      end
      module ImportContacts = struct
type input = {
  contacts : string list;
  token : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ contacts token -> { contacts; token })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.contact.importContacts#input" ~enc:(fun _ -> "app.bsky.contact.importContacts#input")
  |> Jsont.Object.mem "contacts" (Jsont.list Jsont.string) ~enc:(fun r -> r.contacts)
  |> Jsont.Object.mem "token" Jsont.string ~enc:(fun r -> r.token)
  |> Jsont.Object.finish

type output = {
  matches_and_contact_indexes : Defs.match_and_contact_index list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ matches_and_contact_indexes -> { matches_and_contact_indexes })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.contact.importContacts#output" ~enc:(fun _ -> "app.bsky.contact.importContacts#output")
  |> Jsont.Object.mem "matchesAndContactIndexes" (Jsont.list Defs.match_and_contact_index_jsont) ~enc:(fun r -> r.matches_and_contact_indexes)
  |> Jsont.Object.finish

      end
      module GetSyncStatus = struct
type params = unit

let params_jsont = Jsont.ignore

type output = {
  sync_status : Defs.sync_status option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ sync_status -> { sync_status })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.contact.getSyncStatus#output" ~enc:(fun _ -> "app.bsky.contact.getSyncStatus#output")
  |> Jsont.Object.opt_mem "syncStatus" Defs.sync_status_jsont ~enc:(fun r -> r.sync_status)
  |> Jsont.Object.finish

      end
    end
    module Unspecced = struct
      module GetTaggedSuggestions = struct
type suggestion = {
  subject : string;
  subject_type : string;
  tag : string;
}

let suggestion_jsont =
  Jsont.Object.map ~kind:"Suggestion"
    (fun _typ subject subject_type tag -> { subject; subject_type; tag })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.getTaggedSuggestions#suggestion" ~enc:(fun _ -> "app.bsky.unspecced.getTaggedSuggestions#suggestion")
  |> Jsont.Object.mem "subject" Jsont.string ~enc:(fun r -> r.subject)
  |> Jsont.Object.mem "subjectType" Jsont.string ~enc:(fun r -> r.subject_type)
  |> Jsont.Object.mem "tag" Jsont.string ~enc:(fun r -> r.tag)
  |> Jsont.Object.finish

type params = unit

let params_jsont = Jsont.ignore

type output = {
  suggestions : suggestion list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ suggestions -> { suggestions })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.getTaggedSuggestions#output" ~enc:(fun _ -> "app.bsky.unspecced.getTaggedSuggestions#output")
  |> Jsont.Object.mem "suggestions" (Jsont.list suggestion_jsont) ~enc:(fun r -> r.suggestions)
  |> Jsont.Object.finish

      end
      module GetSuggestedUsersSkeleton = struct
type params = {
  category : string option;
  limit : int option;
  viewer : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun category limit viewer -> {
      category;
      limit;
      viewer;
    })
  |> Jsont.Object.opt_mem "category" Jsont.string
       ~enc:(fun r -> r.category)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "viewer" Jsont.string
       ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

type output = {
  dids : string list;
  rec_id : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ dids rec_id -> { dids; rec_id })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.getSuggestedUsersSkeleton#output" ~enc:(fun _ -> "app.bsky.unspecced.getSuggestedUsersSkeleton#output")
  |> Jsont.Object.mem "dids" (Jsont.list Jsont.string) ~enc:(fun r -> r.dids)
  |> Jsont.Object.opt_mem "recId" Jsont.int ~enc:(fun r -> r.rec_id)
  |> Jsont.Object.finish

      end
      module GetSuggestedUsers = struct
type params = {
  category : string option;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun category limit -> {
      category;
      limit;
    })
  |> Jsont.Object.opt_mem "category" Jsont.string
       ~enc:(fun r -> r.category)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  actors : Jsont.json list;
  rec_id : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ actors rec_id -> { actors; rec_id })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.getSuggestedUsers#output" ~enc:(fun _ -> "app.bsky.unspecced.getSuggestedUsers#output")
  |> Jsont.Object.mem "actors" (Jsont.list Jsont.json) ~enc:(fun r -> r.actors)
  |> Jsont.Object.opt_mem "recId" Jsont.int ~enc:(fun r -> r.rec_id)
  |> Jsont.Object.finish

      end
      module GetSuggestedStarterPacksSkeleton = struct
type params = {
  limit : int option;
  viewer : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun limit viewer -> {
      limit;
      viewer;
    })
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "viewer" Jsont.string
       ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

type output = {
  starter_packs : string list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ starter_packs -> { starter_packs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.getSuggestedStarterPacksSkeleton#output" ~enc:(fun _ -> "app.bsky.unspecced.getSuggestedStarterPacksSkeleton#output")
  |> Jsont.Object.mem "starterPacks" (Jsont.list Jsont.string) ~enc:(fun r -> r.starter_packs)
  |> Jsont.Object.finish

      end
      module GetSuggestedStarterPacks = struct
type params = {
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun limit -> {
      limit;
    })
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  starter_packs : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ starter_packs -> { starter_packs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.getSuggestedStarterPacks#output" ~enc:(fun _ -> "app.bsky.unspecced.getSuggestedStarterPacks#output")
  |> Jsont.Object.mem "starterPacks" (Jsont.list Jsont.json) ~enc:(fun r -> r.starter_packs)
  |> Jsont.Object.finish

      end
      module GetSuggestedFeedsSkeleton = struct
type params = {
  limit : int option;
  viewer : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun limit viewer -> {
      limit;
      viewer;
    })
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "viewer" Jsont.string
       ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

type output = {
  feeds : string list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ feeds -> { feeds })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.getSuggestedFeedsSkeleton#output" ~enc:(fun _ -> "app.bsky.unspecced.getSuggestedFeedsSkeleton#output")
  |> Jsont.Object.mem "feeds" (Jsont.list Jsont.string) ~enc:(fun r -> r.feeds)
  |> Jsont.Object.finish

      end
      module GetSuggestedFeeds = struct
type params = {
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun limit -> {
      limit;
    })
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  feeds : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ feeds -> { feeds })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.getSuggestedFeeds#output" ~enc:(fun _ -> "app.bsky.unspecced.getSuggestedFeeds#output")
  |> Jsont.Object.mem "feeds" (Jsont.list Jsont.json) ~enc:(fun r -> r.feeds)
  |> Jsont.Object.finish

      end
      module GetPopularFeedGenerators = struct
type params = {
  cursor : string option;
  limit : int option;
  query : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit query -> {
      cursor;
      limit;
      query;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "query" Jsont.string
       ~enc:(fun r -> r.query)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  feeds : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor feeds -> { cursor; feeds })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.getPopularFeedGenerators#output" ~enc:(fun _ -> "app.bsky.unspecced.getPopularFeedGenerators#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.mem "feeds" (Jsont.list Jsont.json) ~enc:(fun r -> r.feeds)
  |> Jsont.Object.finish

      end
      module GetOnboardingSuggestedStarterPacksSkeleton = struct
type params = {
  limit : int option;
  viewer : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun limit viewer -> {
      limit;
      viewer;
    })
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "viewer" Jsont.string
       ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

type output = {
  starter_packs : string list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ starter_packs -> { starter_packs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.getOnboardingSuggestedStarterPacksSkeleton#output" ~enc:(fun _ -> "app.bsky.unspecced.getOnboardingSuggestedStarterPacksSkeleton#output")
  |> Jsont.Object.mem "starterPacks" (Jsont.list Jsont.string) ~enc:(fun r -> r.starter_packs)
  |> Jsont.Object.finish

      end
      module GetOnboardingSuggestedStarterPacks = struct
type params = {
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun limit -> {
      limit;
    })
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  starter_packs : Jsont.json list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ starter_packs -> { starter_packs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.getOnboardingSuggestedStarterPacks#output" ~enc:(fun _ -> "app.bsky.unspecced.getOnboardingSuggestedStarterPacks#output")
  |> Jsont.Object.mem "starterPacks" (Jsont.list Jsont.json) ~enc:(fun r -> r.starter_packs)
  |> Jsont.Object.finish

      end
      module GetConfig = struct
type live_now_config = {
  did : string;
  domains : string list;
}

let live_now_config_jsont =
  Jsont.Object.map ~kind:"Live_now_config"
    (fun _typ did domains -> { did; domains })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.getConfig#liveNowConfig" ~enc:(fun _ -> "app.bsky.unspecced.getConfig#liveNowConfig")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.mem "domains" (Jsont.list Jsont.string) ~enc:(fun r -> r.domains)
  |> Jsont.Object.finish

type output = {
  check_email_confirmed : bool option;
  live_now : live_now_config list option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ check_email_confirmed live_now -> { check_email_confirmed; live_now })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.getConfig#output" ~enc:(fun _ -> "app.bsky.unspecced.getConfig#output")
  |> Jsont.Object.opt_mem "checkEmailConfirmed" Jsont.bool ~enc:(fun r -> r.check_email_confirmed)
  |> Jsont.Object.opt_mem "liveNow" (Jsont.list live_now_config_jsont) ~enc:(fun r -> r.live_now)
  |> Jsont.Object.finish

      end
      module Defs = struct
type age_assurance_event = {
  attempt_id : string;
  complete_ip : string option;
  complete_ua : string option;
  created_at : string;
  email : string option;
  init_ip : string option;
  init_ua : string option;
  status : string;
}

let age_assurance_event_jsont =
  Jsont.Object.map ~kind:"Age_assurance_event"
    (fun _typ attempt_id complete_ip complete_ua created_at email init_ip init_ua status -> { attempt_id; complete_ip; complete_ua; created_at; email; init_ip; init_ua; status })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.defs#ageAssuranceEvent" ~enc:(fun _ -> "app.bsky.unspecced.defs#ageAssuranceEvent")
  |> Jsont.Object.mem "attemptId" Jsont.string ~enc:(fun r -> r.attempt_id)
  |> Jsont.Object.opt_mem "completeIp" Jsont.string ~enc:(fun r -> r.complete_ip)
  |> Jsont.Object.opt_mem "completeUa" Jsont.string ~enc:(fun r -> r.complete_ua)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.opt_mem "initIp" Jsont.string ~enc:(fun r -> r.init_ip)
  |> Jsont.Object.opt_mem "initUa" Jsont.string ~enc:(fun r -> r.init_ua)
  |> Jsont.Object.mem "status" Jsont.string ~enc:(fun r -> r.status)
  |> Jsont.Object.finish

type age_assurance_state = {
  last_initiated_at : string option;
  status : string;
}

let age_assurance_state_jsont =
  Jsont.Object.map ~kind:"Age_assurance_state"
    (fun _typ last_initiated_at status -> { last_initiated_at; status })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.defs#ageAssuranceState" ~enc:(fun _ -> "app.bsky.unspecced.defs#ageAssuranceState")
  |> Jsont.Object.opt_mem "lastInitiatedAt" Jsont.string ~enc:(fun r -> r.last_initiated_at)
  |> Jsont.Object.mem "status" Jsont.string ~enc:(fun r -> r.status)
  |> Jsont.Object.finish

type skeleton_search_actor = {
  did : string;
}

let skeleton_search_actor_jsont =
  Jsont.Object.map ~kind:"Skeleton_search_actor"
    (fun _typ did -> { did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.defs#skeletonSearchActor" ~enc:(fun _ -> "app.bsky.unspecced.defs#skeletonSearchActor")
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.finish

type skeleton_search_post = {
  uri : string;
}

let skeleton_search_post_jsont =
  Jsont.Object.map ~kind:"Skeleton_search_post"
    (fun _typ uri -> { uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.defs#skeletonSearchPost" ~enc:(fun _ -> "app.bsky.unspecced.defs#skeletonSearchPost")
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

type skeleton_search_starter_pack = {
  uri : string;
}

let skeleton_search_starter_pack_jsont =
  Jsont.Object.map ~kind:"Skeleton_search_starter_pack"
    (fun _typ uri -> { uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.defs#skeletonSearchStarterPack" ~enc:(fun _ -> "app.bsky.unspecced.defs#skeletonSearchStarterPack")
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

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

let skeleton_trend_jsont =
  Jsont.Object.map ~kind:"Skeleton_trend"
    (fun _typ category dids display_name link post_count started_at status topic -> { category; dids; display_name; link; post_count; started_at; status; topic })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.defs#skeletonTrend" ~enc:(fun _ -> "app.bsky.unspecced.defs#skeletonTrend")
  |> Jsont.Object.opt_mem "category" Jsont.string ~enc:(fun r -> r.category)
  |> Jsont.Object.mem "dids" (Jsont.list Jsont.string) ~enc:(fun r -> r.dids)
  |> Jsont.Object.mem "displayName" Jsont.string ~enc:(fun r -> r.display_name)
  |> Jsont.Object.mem "link" Jsont.string ~enc:(fun r -> r.link)
  |> Jsont.Object.mem "postCount" Jsont.int ~enc:(fun r -> r.post_count)
  |> Jsont.Object.mem "startedAt" Jsont.string ~enc:(fun r -> r.started_at)
  |> Jsont.Object.opt_mem "status" Jsont.string ~enc:(fun r -> r.status)
  |> Jsont.Object.mem "topic" Jsont.string ~enc:(fun r -> r.topic)
  |> Jsont.Object.finish

type thread_item_blocked = {
  author : Jsont.json;
}

let thread_item_blocked_jsont =
  Jsont.Object.map ~kind:"Thread_item_blocked"
    (fun _typ author -> { author })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.defs#threadItemBlocked" ~enc:(fun _ -> "app.bsky.unspecced.defs#threadItemBlocked")
  |> Jsont.Object.mem "author" Jsont.json ~enc:(fun r -> r.author)
  |> Jsont.Object.finish

type thread_item_no_unauthenticated = unit

let thread_item_no_unauthenticated_jsont = Jsont.ignore

type thread_item_not_found = unit

let thread_item_not_found_jsont = Jsont.ignore

type thread_item_post = {
  hidden_by_threadgate : bool;
  more_parents : bool;
  more_replies : int;
  muted_by_viewer : bool;
  op_thread : bool;
  post : Jsont.json;
}

let thread_item_post_jsont =
  Jsont.Object.map ~kind:"Thread_item_post"
    (fun _typ hidden_by_threadgate more_parents more_replies muted_by_viewer op_thread post -> { hidden_by_threadgate; more_parents; more_replies; muted_by_viewer; op_thread; post })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.defs#threadItemPost" ~enc:(fun _ -> "app.bsky.unspecced.defs#threadItemPost")
  |> Jsont.Object.mem "hiddenByThreadgate" Jsont.bool ~enc:(fun r -> r.hidden_by_threadgate)
  |> Jsont.Object.mem "moreParents" Jsont.bool ~enc:(fun r -> r.more_parents)
  |> Jsont.Object.mem "moreReplies" Jsont.int ~enc:(fun r -> r.more_replies)
  |> Jsont.Object.mem "mutedByViewer" Jsont.bool ~enc:(fun r -> r.muted_by_viewer)
  |> Jsont.Object.mem "opThread" Jsont.bool ~enc:(fun r -> r.op_thread)
  |> Jsont.Object.mem "post" Jsont.json ~enc:(fun r -> r.post)
  |> Jsont.Object.finish

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

let trend_view_jsont =
  Jsont.Object.map ~kind:"Trend_view"
    (fun _typ actors category display_name link post_count started_at status topic -> { actors; category; display_name; link; post_count; started_at; status; topic })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.defs#trendView" ~enc:(fun _ -> "app.bsky.unspecced.defs#trendView")
  |> Jsont.Object.mem "actors" (Jsont.list Jsont.json) ~enc:(fun r -> r.actors)
  |> Jsont.Object.opt_mem "category" Jsont.string ~enc:(fun r -> r.category)
  |> Jsont.Object.mem "displayName" Jsont.string ~enc:(fun r -> r.display_name)
  |> Jsont.Object.mem "link" Jsont.string ~enc:(fun r -> r.link)
  |> Jsont.Object.mem "postCount" Jsont.int ~enc:(fun r -> r.post_count)
  |> Jsont.Object.mem "startedAt" Jsont.string ~enc:(fun r -> r.started_at)
  |> Jsont.Object.opt_mem "status" Jsont.string ~enc:(fun r -> r.status)
  |> Jsont.Object.mem "topic" Jsont.string ~enc:(fun r -> r.topic)
  |> Jsont.Object.finish

type trending_topic = {
  description : string option;
  display_name : string option;
  link : string;
  topic : string;
}

let trending_topic_jsont =
  Jsont.Object.map ~kind:"Trending_topic"
    (fun _typ description display_name link topic -> { description; display_name; link; topic })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.defs#trendingTopic" ~enc:(fun _ -> "app.bsky.unspecced.defs#trendingTopic")
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.opt_mem "displayName" Jsont.string ~enc:(fun r -> r.display_name)
  |> Jsont.Object.mem "link" Jsont.string ~enc:(fun r -> r.link)
  |> Jsont.Object.mem "topic" Jsont.string ~enc:(fun r -> r.topic)
  |> Jsont.Object.finish

      end
      module SearchStarterPacksSkeleton = struct
type params = {
  cursor : string option;
  limit : int option;
  q : string;
  viewer : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit q viewer -> {
      cursor;
      limit;
      q;
      viewer;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "q" Jsont.string
       ~enc:(fun r -> r.q)
  |> Jsont.Object.opt_mem "viewer" Jsont.string
       ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  hits_total : int option;
  starter_packs : Defs.skeleton_search_starter_pack list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor hits_total starter_packs -> { cursor; hits_total; starter_packs })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.searchStarterPacksSkeleton#output" ~enc:(fun _ -> "app.bsky.unspecced.searchStarterPacksSkeleton#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "hitsTotal" Jsont.int ~enc:(fun r -> r.hits_total)
  |> Jsont.Object.mem "starterPacks" (Jsont.list Defs.skeleton_search_starter_pack_jsont) ~enc:(fun r -> r.starter_packs)
  |> Jsont.Object.finish

      end
      module SearchPostsSkeleton = struct
type params = {
  author : string option;
  cursor : string option;
  domain : string option;
  lang : string option;
  limit : int option;
  mentions : string option;
  q : string;
  since : string option;
  sort : string option;
  tag : string list option;
  until : string option;
  url : string option;
  viewer : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun author cursor domain lang limit mentions q since sort tag until url viewer -> {
      author;
      cursor;
      domain;
      lang;
      limit;
      mentions;
      q;
      since;
      sort;
      tag;
      until;
      url;
      viewer;
    })
  |> Jsont.Object.opt_mem "author" Jsont.string
       ~enc:(fun r -> r.author)
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "domain" Jsont.string
       ~enc:(fun r -> r.domain)
  |> Jsont.Object.opt_mem "lang" Jsont.string
       ~enc:(fun r -> r.lang)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "mentions" Jsont.string
       ~enc:(fun r -> r.mentions)
  |> Jsont.Object.mem "q" Jsont.string
       ~enc:(fun r -> r.q)
  |> Jsont.Object.opt_mem "since" Jsont.string
       ~enc:(fun r -> r.since)
  |> Jsont.Object.opt_mem "sort" Jsont.string
       ~enc:(fun r -> r.sort)
  |> Jsont.Object.opt_mem "tag" (Jsont.list Jsont.string)
       ~enc:(fun r -> r.tag)
  |> Jsont.Object.opt_mem "until" Jsont.string
       ~enc:(fun r -> r.until)
  |> Jsont.Object.opt_mem "url" Jsont.string
       ~enc:(fun r -> r.url)
  |> Jsont.Object.opt_mem "viewer" Jsont.string
       ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

type output = {
  cursor : string option;
  hits_total : int option;
  posts : Defs.skeleton_search_post list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ cursor hits_total posts -> { cursor; hits_total; posts })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.searchPostsSkeleton#output" ~enc:(fun _ -> "app.bsky.unspecced.searchPostsSkeleton#output")
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "hitsTotal" Jsont.int ~enc:(fun r -> r.hits_total)
  |> Jsont.Object.mem "posts" (Jsont.list Defs.skeleton_search_post_jsont) ~enc:(fun r -> r.posts)
  |> Jsont.Object.finish

      end
      module SearchActorsSkeleton = struct
type params = {
  cursor : string option;
  limit : int option;
  q : string;
  typeahead : bool option;
  viewer : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit q typeahead viewer -> {
      cursor;
      limit;
      q;
      typeahead;
      viewer;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.mem "q" Jsont.string
       ~enc:(fun r -> r.q)
  |> Jsont.Object.opt_mem "typeahead" Jsont.bool
       ~enc:(fun r -> r.typeahead)
  |> Jsont.Object.opt_mem "viewer" Jsont.string
       ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

type output = {
  actors : Defs.skeleton_search_actor list;
  cursor : string option;
  hits_total : int option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ actors cursor hits_total -> { actors; cursor; hits_total })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.searchActorsSkeleton#output" ~enc:(fun _ -> "app.bsky.unspecced.searchActorsSkeleton#output")
  |> Jsont.Object.mem "actors" (Jsont.list Defs.skeleton_search_actor_jsont) ~enc:(fun r -> r.actors)
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "hitsTotal" Jsont.int ~enc:(fun r -> r.hits_total)
  |> Jsont.Object.finish

      end
      module InitAgeAssurance = struct
type input = {
  country_code : string;
  email : string;
  language : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ country_code email language -> { country_code; email; language })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.initAgeAssurance#input" ~enc:(fun _ -> "app.bsky.unspecced.initAgeAssurance#input")
  |> Jsont.Object.mem "countryCode" Jsont.string ~enc:(fun r -> r.country_code)
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
  |> Jsont.Object.mem "language" Jsont.string ~enc:(fun r -> r.language)
  |> Jsont.Object.finish

type output = Defs.age_assurance_state

let output_jsont = Defs.age_assurance_state_jsont

      end
      module GetTrendsSkeleton = struct
type params = {
  limit : int option;
  viewer : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun limit viewer -> {
      limit;
      viewer;
    })
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "viewer" Jsont.string
       ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

type output = {
  trends : Defs.skeleton_trend list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ trends -> { trends })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.getTrendsSkeleton#output" ~enc:(fun _ -> "app.bsky.unspecced.getTrendsSkeleton#output")
  |> Jsont.Object.mem "trends" (Jsont.list Defs.skeleton_trend_jsont) ~enc:(fun r -> r.trends)
  |> Jsont.Object.finish

      end
      module GetTrends = struct
type params = {
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun limit -> {
      limit;
    })
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  trends : Defs.trend_view list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ trends -> { trends })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.getTrends#output" ~enc:(fun _ -> "app.bsky.unspecced.getTrends#output")
  |> Jsont.Object.mem "trends" (Jsont.list Defs.trend_view_jsont) ~enc:(fun r -> r.trends)
  |> Jsont.Object.finish

      end
      module GetTrendingTopics = struct
type params = {
  limit : int option;
  viewer : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun limit viewer -> {
      limit;
      viewer;
    })
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "viewer" Jsont.string
       ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

type output = {
  suggested : Defs.trending_topic list;
  topics : Defs.trending_topic list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ suggested topics -> { suggested; topics })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.getTrendingTopics#output" ~enc:(fun _ -> "app.bsky.unspecced.getTrendingTopics#output")
  |> Jsont.Object.mem "suggested" (Jsont.list Defs.trending_topic_jsont) ~enc:(fun r -> r.suggested)
  |> Jsont.Object.mem "topics" (Jsont.list Defs.trending_topic_jsont) ~enc:(fun r -> r.topics)
  |> Jsont.Object.finish

      end
      module GetSuggestionsSkeleton = struct
type params = {
  cursor : string option;
  limit : int option;
  relative_to_did : string option;
  viewer : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit relative_to_did viewer -> {
      cursor;
      limit;
      relative_to_did;
      viewer;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.opt_mem "relativeToDid" Jsont.string
       ~enc:(fun r -> r.relative_to_did)
  |> Jsont.Object.opt_mem "viewer" Jsont.string
       ~enc:(fun r -> r.viewer)
  |> Jsont.Object.finish

type output = {
  actors : Defs.skeleton_search_actor list;
  cursor : string option;
  rec_id : int option;
  relative_to_did : string option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ actors cursor rec_id relative_to_did -> { actors; cursor; rec_id; relative_to_did })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.getSuggestionsSkeleton#output" ~enc:(fun _ -> "app.bsky.unspecced.getSuggestionsSkeleton#output")
  |> Jsont.Object.mem "actors" (Jsont.list Defs.skeleton_search_actor_jsont) ~enc:(fun r -> r.actors)
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "recId" Jsont.int ~enc:(fun r -> r.rec_id)
  |> Jsont.Object.opt_mem "relativeToDid" Jsont.string ~enc:(fun r -> r.relative_to_did)
  |> Jsont.Object.finish

      end
      module GetPostThreadV2 = struct
type thread_item = {
  depth : int;
  uri : string;
  value : Jsont.json;
}

let thread_item_jsont =
  Jsont.Object.map ~kind:"Thread_item"
    (fun _typ depth uri value -> { depth; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.getPostThreadV2#threadItem" ~enc:(fun _ -> "app.bsky.unspecced.getPostThreadV2#threadItem")
  |> Jsont.Object.mem "depth" Jsont.int ~enc:(fun r -> r.depth)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  above : bool option;
  anchor : string;
  below : int option;
  branching_factor : int option;
  sort : string option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun above anchor below branching_factor sort -> {
      above;
      anchor;
      below;
      branching_factor;
      sort;
    })
  |> Jsont.Object.opt_mem "above" Jsont.bool
       ~enc:(fun r -> r.above)
  |> Jsont.Object.mem "anchor" Jsont.string
       ~enc:(fun r -> r.anchor)
  |> Jsont.Object.opt_mem "below" Jsont.int
       ~enc:(fun r -> r.below)
  |> Jsont.Object.opt_mem "branchingFactor" Jsont.int
       ~enc:(fun r -> r.branching_factor)
  |> Jsont.Object.opt_mem "sort" Jsont.string
       ~enc:(fun r -> r.sort)
  |> Jsont.Object.finish

type output = {
  has_other_replies : bool;
  thread : thread_item list;
  threadgate : Jsont.json option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ has_other_replies thread threadgate -> { has_other_replies; thread; threadgate })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.getPostThreadV2#output" ~enc:(fun _ -> "app.bsky.unspecced.getPostThreadV2#output")
  |> Jsont.Object.mem "hasOtherReplies" Jsont.bool ~enc:(fun r -> r.has_other_replies)
  |> Jsont.Object.mem "thread" (Jsont.list thread_item_jsont) ~enc:(fun r -> r.thread)
  |> Jsont.Object.opt_mem "threadgate" Jsont.json ~enc:(fun r -> r.threadgate)
  |> Jsont.Object.finish

      end
      module GetPostThreadOtherV2 = struct
type thread_item = {
  depth : int;
  uri : string;
  value : Defs.thread_item_post;
}

let thread_item_jsont =
  Jsont.Object.map ~kind:"Thread_item"
    (fun _typ depth uri value -> { depth; uri; value })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.getPostThreadOtherV2#threadItem" ~enc:(fun _ -> "app.bsky.unspecced.getPostThreadOtherV2#threadItem")
  |> Jsont.Object.mem "depth" Jsont.int ~enc:(fun r -> r.depth)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Defs.thread_item_post_jsont ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

type params = {
  anchor : string;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun anchor -> {
      anchor;
    })
  |> Jsont.Object.mem "anchor" Jsont.string
       ~enc:(fun r -> r.anchor)
  |> Jsont.Object.finish

type output = {
  thread : thread_item list;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ thread -> { thread })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.unspecced.getPostThreadOtherV2#output" ~enc:(fun _ -> "app.bsky.unspecced.getPostThreadOtherV2#output")
  |> Jsont.Object.mem "thread" (Jsont.list thread_item_jsont) ~enc:(fun r -> r.thread)
  |> Jsont.Object.finish

      end
      module GetAgeAssuranceState = struct
type output = Defs.age_assurance_state

let output_jsont = Defs.age_assurance_state_jsont

      end
    end
    module Bookmark = struct
      module DeleteBookmark = struct
type input = {
  uri : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ uri -> { uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.bookmark.deleteBookmark#input" ~enc:(fun _ -> "app.bsky.bookmark.deleteBookmark#input")
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

      end
      module Defs = struct
type bookmark = {
  subject : Com.Atproto.Repo.StrongRef.main;
}

let bookmark_jsont =
  Jsont.Object.map ~kind:"Bookmark"
    (fun _typ subject -> { subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.bookmark.defs#bookmark" ~enc:(fun _ -> "app.bsky.bookmark.defs#bookmark")
  |> Jsont.Object.mem "subject" Com.Atproto.Repo.StrongRef.main_jsont ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

type bookmark_view = {
  created_at : string option;
  item : Jsont.json;
  subject : Com.Atproto.Repo.StrongRef.main;
}

let bookmark_view_jsont =
  Jsont.Object.map ~kind:"Bookmark_view"
    (fun _typ created_at item subject -> { created_at; item; subject })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.bookmark.defs#bookmarkView" ~enc:(fun _ -> "app.bsky.bookmark.defs#bookmarkView")
  |> Jsont.Object.opt_mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.mem "item" Jsont.json ~enc:(fun r -> r.item)
  |> Jsont.Object.mem "subject" Com.Atproto.Repo.StrongRef.main_jsont ~enc:(fun r -> r.subject)
  |> Jsont.Object.finish

      end
      module CreateBookmark = struct
type input = {
  cid : string;
  uri : string;
}

let input_jsont =
  Jsont.Object.map ~kind:"Input"
    (fun _typ cid uri -> { cid; uri })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.bookmark.createBookmark#input" ~enc:(fun _ -> "app.bsky.bookmark.createBookmark#input")
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.finish

      end
      module GetBookmarks = struct
type params = {
  cursor : string option;
  limit : int option;
}

let params_jsont =
  Jsont.Object.map ~kind:"Params"
    (fun cursor limit -> {
      cursor;
      limit;
    })
  |> Jsont.Object.opt_mem "cursor" Jsont.string
       ~enc:(fun r -> r.cursor)
  |> Jsont.Object.opt_mem "limit" Jsont.int
       ~enc:(fun r -> r.limit)
  |> Jsont.Object.finish

type output = {
  bookmarks : Defs.bookmark_view list;
  cursor : string option;
}

let output_jsont =
  Jsont.Object.map ~kind:"Output"
    (fun _typ bookmarks cursor -> { bookmarks; cursor })
  |> Jsont.Object.mem "$type" Jsont.string ~dec_absent:"app.bsky.bookmark.getBookmarks#output" ~enc:(fun _ -> "app.bsky.bookmark.getBookmarks#output")
  |> Jsont.Object.mem "bookmarks" (Jsont.list Defs.bookmark_view_jsont) ~enc:(fun r -> r.bookmarks)
  |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun r -> r.cursor)
  |> Jsont.Object.finish

      end
    end
  end
end
