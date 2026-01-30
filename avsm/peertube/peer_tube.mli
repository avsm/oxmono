(** {1 PeerTube}

    The PeerTube API is built on HTTP(S) and is RESTful. You can use your favorite
HTTP/REST library for your programming language to use PeerTube.

See the [REST API quick start](https://docs.joinpeertube.org/api/rest-getting-started) for a few
examples of using the PeerTube API.

# Authentication

When you sign up for an account on a PeerTube instance, you are given the possibility
to generate sessions on it, and authenticate there using an access token. Only __one
access token can currently be used at a time__.

## Roles

Accounts are given permissions based on their role. There are three roles on
PeerTube: Administrator, Moderator, and User. See the [roles guide](https://docs.joinpeertube.org/admin/managing-users#roles) for a detail of their permissions.

# Errors

The API uses standard HTTP status codes to indicate the success or failure
of the API call, completed by a [RFC7807-compliant](https://tools.ietf.org/html/rfc7807) response body.

```
HTTP 1.1 404 Not Found
Content-Type: application/problem+json; charset=utf-8

\{
  "detail": "Video not found",
  "docs": "https://docs.joinpeertube.org/api-rest-reference.html#operation/getVideo",
  "status": 404,
  "title": "Not Found",
  "type": "about:blank"
\}
```

We provide error `type` (following RFC7807) and `code` (internal PeerTube code) values for [a growing number of cases](https://github.com/Chocobozzz/PeerTube/blob/develop/packages/models/src/server/server-error-code.enum.ts),
but it is still optional. Types are used to disambiguate errors that bear the same status code
and are non-obvious:

```
HTTP 1.1 403 Forbidden
Content-Type: application/problem+json; charset=utf-8

\{
  "detail": "Cannot get this video regarding follow constraints",
  "docs": "https://docs.joinpeertube.org/api-rest-reference.html#operation/getVideo",
  "status": 403,
  "title": "Forbidden",
  "type": "https://docs.joinpeertube.org/api-rest-reference.html#section/Errors/does_not_respect_follow_constraints"
\}
```

Here a 403 error could otherwise mean that the video is private or blocklisted.

### Validation errors

Each parameter is evaluated on its own against a set of rules before the route validator
proceeds with potential testing involving parameter combinations. Errors coming from validation
errors appear earlier and benefit from a more detailed error description:

```
HTTP 1.1 400 Bad Request
Content-Type: application/problem+json; charset=utf-8

\{
  "detail": "Incorrect request parameters: id",
  "docs": "https://docs.joinpeertube.org/api-rest-reference.html#operation/getVideo",
  "instance": "/api/v1/videos/9c9de5e8-0a1e-484a-b099-e80766180",
  "invalid-params": \{
    "id": \{
      "location": "params",
      "msg": "Invalid value",
      "param": "id",
      "value": "9c9de5e8-0a1e-484a-b099-e80766180"
    \}
  \},
  "status": 400,
  "title": "Bad Request",
  "type": "about:blank"
\}
```

Where `id` is the name of the field concerned by the error, within the route definition.
`invalid-params.<field>.location` can be either 'params', 'body', 'header', 'query' or 'cookies', and
`invalid-params.<field>.value` reports the value that didn't pass validation whose `invalid-params.<field>.msg`
is about.

### Deprecated error fields

Some fields could be included with previous versions. They are still included but their use is deprecated:
- `error`: superseded by `detail`

# Rate limits

We are rate-limiting all endpoints of PeerTube's API. Custom values can be set by administrators:

| Endpoint (prefix: `/api/v1`) | Calls         | Time frame   |
|------------------------------|---------------|--------------|
| `/*`                         | 50            | 10 seconds   |
| `POST /users/token`          | 15            | 5 minutes    |
| `POST /users/register`       | 2<sup>*</sup> | 5 minutes    |
| `POST /users/ask-send-verify-email` | 3      | 5 minutes    |

Depending on the endpoint, <sup>*</sup>failed requests are not taken into account. A service
limit is announced by a `429 Too Many Requests` status code.

You can get details about the current state of your rate limit by reading the
following headers:

| Header                  | Description                                                |
|-------------------------|------------------------------------------------------------|
| `X-RateLimit-Limit`     | Number of max requests allowed in the current time period  |
| `X-RateLimit-Remaining` | Number of remaining requests in the current time period    |
| `X-RateLimit-Reset`     | Timestamp of end of current time period as UNIX timestamp  |
| `Retry-After`           | Seconds to delay after the first `429` is received         |

# CORS

This API features [Cross-Origin Resource Sharing (CORS)](https://fetch.spec.whatwg.org/),
allowing cross-domain communication from the browser for some routes:

| Endpoint                    |
|------------------------- ---|
| `/api/*`                    |
| `/download/*`               |
| `/lazy-static/*`            |
| `/.well-known/webfinger`    |

In addition, all routes serving ActivityPub are CORS-enabled for all origins.


    @version 8.0.0 *)

type t

val create :
  ?session:Requests.t ->
  sw:Eio.Switch.t ->
  < net : _ Eio.Net.t ; fs : Eio.Fs.dir_ty Eio.Path.t ; clock : _ Eio.Time.clock ; .. > ->
  base_url:string ->
  t

val base_url : t -> string
val session : t -> Requests.t

module VideosForXml : sig
  module T : sig
    type t = Jsont.json
    
    val jsont : t Jsont.t
    
    val v : unit -> t
  end
end

module VideoUpload : sig
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : ?video:Jsont.json -> unit -> t
    
    val video : t -> Jsont.json option
    
    val jsont : t Jsont.t
  end
  
  (** Import a video
  
      Import a torrent or magnetURI or HTTP resource (if enabled by the instance administrator) *)
  val import_video : t -> unit -> Response.t
  
  (** Create a live *)
  val add_live : t -> unit -> Response.t
  
  (** Upload a video
  
      Uses a single request to upload a video. *)
  val upload_legacy : t -> unit -> Response.t
  
  (** Send chunk for the resumable upload of a video
  
      Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to continue, pause or resume the upload of a video 
      @param upload_id Created session id to proceed with. If you didn't send chunks in the last hour, it is
  not valid anymore and you need to initialize a new upload.
  
  *)
  val upload_resumable : upload_id:string -> t -> unit -> Response.t
end

module VideoToken : sig
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : ?files:Jsont.json -> unit -> t
    
    val files : t -> Jsont.json option
    
    val jsont : t Jsont.t
  end
  
  (** Request video token
  
      Request special tokens that expire quickly to use them in some context (like accessing private static files) 
      @param id The object id, uuid or short uuid
  *)
  val request_video_token : id:string -> t -> unit -> Response.t
end

module VideoStudioCreateTask : sig
  module T : sig
    type t = Jsont.json
    
    val jsont : t Jsont.t
    
    val v : unit -> t
  end
end

module VideoStatsUserAgentDevice : sig
  module T : sig
    type t = [
      | `Console
      | `Embedded
      | `Mobile
      | `Smarttv
      | `Tablet
      | `Wearable
      | `Xr
      | `Desktop
    ]
    
    val jsont : t Jsont.t
  end
end

module UserViewingVideo : sig
  module T : sig
    type t
    
    (** Construct a value
        @param current_time timestamp within the video, in seconds
        @param client Client software used to watch the video. For example "Firefox", "PeerTube Approval Android", etc.
    
        @param device Device used to watch the video. For example "desktop", "mobile", "smarttv", etc.
    
        @param operating_system Operating system used to watch the video. For example "Windows", "Ubuntu", etc.
    
        @param session_id Optional param to represent the current viewer session. Used by the backend to properly count one view per session per video. PeerTube admin can configure the server to not trust this `sessionId` parameter but use the request IP address instead to identify a viewer.
    
        @param view_event Event since last viewing call:
     * `seek` - If the user seeked the video
    
    *)
    val v : current_time:int -> ?client:string -> ?device:VideoStatsUserAgentDevice.T.t -> ?operating_system:string -> ?session_id:string -> ?view_event:string -> unit -> t
    
    (** Client software used to watch the video. For example "Firefox", "PeerTube Approval Android", etc.
     *)
    val client : t -> string option
    
    (** timestamp within the video, in seconds *)
    val current_time : t -> int
    
    (** Device used to watch the video. For example "desktop", "mobile", "smarttv", etc.
     *)
    val device : t -> VideoStatsUserAgentDevice.T.t option
    
    (** Operating system used to watch the video. For example "Windows", "Ubuntu", etc.
     *)
    val operating_system : t -> string option
    
    (** Optional param to represent the current viewer session. Used by the backend to properly count one view per session per video. PeerTube admin can configure the server to not trust this `sessionId` parameter but use the request IP address instead to identify a viewer.
     *)
    val session_id : t -> string option
    
    (** Event since last viewing call:
     * `seek` - If the user seeked the video
     *)
    val view_event : t -> string option
    
    val jsont : t Jsont.t
  end
end

module VideoStatsUserAgent : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?clients:Jsont.json list -> ?devices:Jsont.json list -> ?operating_system:Jsont.json list -> unit -> t
    
    val clients : t -> Jsont.json list option
    
    val devices : t -> Jsont.json list option
    
    val operating_system : t -> Jsont.json list option
    
    val jsont : t Jsont.t
  end
  
  (** Get user agent stats of a video 
      @param id The object id, uuid or short uuid
      @param start_date Filter stats by start date
      @param end_date Filter stats by end date
  *)
  val get_api_v1_videos_stats_user_agent : id:string -> ?start_date:string -> ?end_date:string -> t -> unit -> T.t
end

module VideoStatsTimeserie : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?data:Jsont.json list -> unit -> t
    
    val data : t -> Jsont.json list option
    
    val jsont : t Jsont.t
  end
  
  (** Get timeserie stats of a video 
      @param id The object id, uuid or short uuid
      @param metric The metric to get
      @param start_date Filter stats by start date
      @param end_date Filter stats by end date
  *)
  val get_api_v1_videos_stats_timeseries : id:string -> metric:string -> ?start_date:string -> ?end_date:string -> t -> unit -> T.t
end

module VideoStatsRetention : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?data:Jsont.json list -> unit -> t
    
    val data : t -> Jsont.json list option
    
    val jsont : t Jsont.t
  end
  
  (** Get retention stats of a video 
      @param id The object id, uuid or short uuid
  *)
  val get_api_v1_videos_stats_retention : id:string -> t -> unit -> T.t
end

module VideoStatsOverall : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?average_watch_time:float -> ?countries:Jsont.json list -> ?subdivisions:Jsont.json list -> ?total_viewers:float -> ?total_watch_time:float -> ?viewers_peak:float -> ?viewers_peak_date:Ptime.t -> unit -> t
    
    val average_watch_time : t -> float option
    
    val countries : t -> Jsont.json list option
    
    val subdivisions : t -> Jsont.json list option
    
    val total_viewers : t -> float option
    
    val total_watch_time : t -> float option
    
    val viewers_peak : t -> float option
    
    val viewers_peak_date : t -> Ptime.t option
    
    val jsont : t Jsont.t
  end
  
  (** Get overall stats of a video 
      @param id The object id, uuid or short uuid
      @param start_date Filter stats by start date
      @param end_date Filter stats by end date
  *)
  val get_api_v1_videos_stats_overall : id:string -> ?start_date:string -> ?end_date:string -> t -> unit -> T.t
end

module VideoStateConstant : sig
  module T : sig
    type t
    
    (** Construct a value
        @param id The video state:
    - `1`: Published
    - `2`: To transcode
    - `3`: To import
    - `4`: Waiting for live stream
    - `5`: Live ended
    - `6`: To move to an external storage (object storage...)
    - `7`: Transcoding failed
    - `8`: Moving to an external storage failed
    - `9`: To edit using studio edition feature
    
    *)
    val v : ?id:int -> ?label:string -> unit -> t
    
    (** The video state:
    - `1`: Published
    - `2`: To transcode
    - `3`: To import
    - `4`: Waiting for live stream
    - `5`: Live ended
    - `6`: To move to an external storage (object storage...)
    - `7`: Transcoding failed
    - `8`: Moving to an external storage failed
    - `9`: To edit using studio edition feature
     *)
    val id : t -> int option
    
    val label : t -> string option
    
    val jsont : t Jsont.t
  end
end

module VideoResolutionSet : sig
  module T : sig
    (** Video resolution (`0`, `240`, `360`, `720`, `1080`, `1440` or `2160`)
    
    `0` is used as a special value for stillimage videos dedicated to audio, a.k.a. audio-only videos.
     *)
    type t = Jsont.json
    
    val jsont : t Jsont.t
    
    val v : unit -> t
  end
end

module VideoResolutionConstant : sig
  module T : sig
    (** resolutions and their labels for the video *)
    type t
    
    (** Construct a value *)
    val v : ?id:VideoResolutionSet.T.t -> ?label:string -> unit -> t
    
    val id : t -> VideoResolutionSet.T.t option
    
    val label : t -> string option
    
    val jsont : t Jsont.t
  end
end

module VideoSource : sig
  module T : sig
    type t
    
    (** Construct a value
        @param file_download_url **PeerTube >= 6.1** If enabled by the admin, the video source file is kept on the server and can be downloaded by the owner
        @param fps **PeerTube >= 6.1** Frames per second of the video file
        @param height **PeerTube >= 6.1** Video stream height
        @param input_filename Uploaded/imported filename
        @param resolution **PeerTube >= 6.1**
        @param size **PeerTube >= 6.1** Video file size in bytes
        @param width **PeerTube >= 6.1** Video stream width
    *)
    val v : ?created_at:Ptime.t -> ?file_download_url:string -> ?fps:float -> ?height:int -> ?input_filename:string -> ?resolution:VideoResolutionConstant.T.t -> ?size:int -> ?width:int -> unit -> t
    
    val created_at : t -> Ptime.t option
    
    (** **PeerTube >= 6.1** If enabled by the admin, the video source file is kept on the server and can be downloaded by the owner *)
    val file_download_url : t -> string option
    
    (** **PeerTube >= 6.1** Frames per second of the video file *)
    val fps : t -> float option
    
    (** **PeerTube >= 6.1** Video stream height *)
    val height : t -> int option
    
    (** Uploaded/imported filename *)
    val input_filename : t -> string option
    
    (** **PeerTube >= 6.1** *)
    val resolution : t -> VideoResolutionConstant.T.t option
    
    (** **PeerTube >= 6.1** Video file size in bytes *)
    val size : t -> int option
    
    (** **PeerTube >= 6.1** Video stream width *)
    val width : t -> int option
    
    val jsont : t Jsont.t
  end
  
  (** Get video source file metadata
  
      Get metadata and download link of original video file 
      @param id The object id, uuid or short uuid
  *)
  val get_video_source : id:string -> t -> unit -> T.t
end

module VideoReplaceSourceRequestResumable : sig
  module T : sig
    type t
    
    (** Construct a value
        @param filename Video filename including extension
    *)
    val v : ?filename:string -> unit -> t
    
    (** Video filename including extension *)
    val filename : t -> string option
    
    val jsont : t Jsont.t
  end
end

module VideoPrivacySet : sig
  module T : sig
    (** privacy id of the video (see [/videos/privacies](#operation/getVideoPrivacyPolicies)) *)
    type t = string
    
    val jsont : t Jsont.t
  end
end

module VideoScheduled : sig
  module Update : sig
    type t
    
    (** Construct a value
        @param update_at When to update the video
    *)
    val v : update_at:Ptime.t -> ?privacy:VideoPrivacySet.T.t -> unit -> t
    
    val privacy : t -> VideoPrivacySet.T.t option
    
    (** When to update the video *)
    val update_at : t -> Ptime.t
    
    val jsont : t Jsont.t
  end
end

module VideoPrivacyConstant : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?id:VideoPrivacySet.T.t -> ?label:string -> unit -> t
    
    val id : t -> VideoPrivacySet.T.t option
    
    val label : t -> string option
    
    val jsont : t Jsont.t
  end
end

module LiveVideoReplaySettings : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?privacy:VideoPrivacySet.T.t -> unit -> t
    
    val privacy : t -> VideoPrivacySet.T.t option
    
    val jsont : t Jsont.t
  end
end

module VideoPlaylistTypeSet : sig
  module T : sig
    (** The video playlist type (Regular = `1`, Watch Later = `2`) *)
    type t = string
    
    val jsont : t Jsont.t
  end
end

module VideoPlaylistTypeConstant : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?id:VideoPlaylistTypeSet.T.t -> ?label:string -> unit -> t
    
    val id : t -> VideoPlaylistTypeSet.T.t option
    
    val label : t -> string option
    
    val jsont : t Jsont.t
  end
end

module VideoPlaylistPrivacySet : sig
  module T : sig
    (** Video playlist privacy policy (see [/video-playlists/privacies](#operation/getPlaylistPrivacyPolicies)) *)
    type t = string
    
    val jsont : t Jsont.t
  end
end

module VideoPlaylistPrivacyConstant : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?id:VideoPlaylistPrivacySet.T.t -> ?label:string -> unit -> t
    
    val id : t -> VideoPlaylistPrivacySet.T.t option
    
    val label : t -> string option
    
    val jsont : t Jsont.t
  end
end

module VideoLicenceSet : sig
  module T : sig
    (** licence id of the video (see [/videos/licences](#operation/getLicences)) *)
    type t = Jsont.json
    
    val jsont : t Jsont.t
    
    val v : unit -> t
  end
end

module VideoConstantNumberLicence : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?id:VideoLicenceSet.T.t -> ?label:string -> unit -> t
    
    val id : t -> VideoLicenceSet.T.t option
    
    val label : t -> string option
    
    val jsont : t Jsont.t
  end
end

module VideoLanguageSet : sig
  module T : sig
    (** language id of the video (see [/videos/languages](#operation/getLanguages)) *)
    type t = Jsont.json
    
    val jsont : t Jsont.t
    
    val v : unit -> t
  end
end

module VideoConstantStringLanguage : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?id:VideoLanguageSet.T.t -> ?label:string -> unit -> t
    
    val id : t -> VideoLanguageSet.T.t option
    
    val label : t -> string option
    
    val jsont : t Jsont.t
  end
end

module VideoCaption : sig
  module T : sig
    type t
    
    (** Construct a value
        @param caption_path Deprecated in PeerTube v8.0, use fileUrl instead
        @param file_url **PeerTube >= 7.1**
    *)
    val v : ?automatically_generated:bool -> ?caption_path:string -> ?file_url:string -> ?language:VideoConstantStringLanguage.T.t -> ?m3u8_url:string -> ?updated_at:Ptime.t -> unit -> t
    
    val automatically_generated : t -> bool option
    
    (** Deprecated in PeerTube v8.0, use fileUrl instead *)
    val caption_path : t -> string option
    
    (** **PeerTube >= 7.1** *)
    val file_url : t -> string option
    
    val language : t -> VideoConstantStringLanguage.T.t option
    
    val m3u8_url : t -> string option
    
    val updated_at : t -> Ptime.t option
    
    val jsont : t Jsont.t
  end
end

module VideoImportStateConstant : sig
  module T : sig
    type t
    
    (** Construct a value
        @param id The video import state (Pending = `1`, Success = `2`, Failed = `3`)
    *)
    val v : ?id:int -> ?label:string -> unit -> t
    
    (** The video import state (Pending = `1`, Success = `2`, Failed = `3`) *)
    val id : t -> int option
    
    val label : t -> string option
    
    val jsont : t Jsont.t
  end
end

module VideoCommentsPolicySet : sig
  module T : sig
    (** Comments policy of the video (Enabled = `1`, Disabled = `2`, Requires Approval = `3`) *)
    type t = string
    
    val jsont : t Jsont.t
  end
end

module VideoCommentsPolicyConstant : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?id:VideoCommentsPolicySet.T.t -> ?label:string -> unit -> t
    
    val id : t -> VideoCommentsPolicySet.T.t option
    
    val label : t -> string option
    
    val jsont : t Jsont.t
  end
end

module VideoCommentsForXml : sig
  module T : sig
    type t = Jsont.json
    
    val jsont : t Jsont.t
    
    val v : unit -> t
  end
end

module VideoChapters : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?chapters:Jsont.json -> unit -> t
    
    val chapters : t -> Jsont.json option
    
    val jsont : t Jsont.t
  end
  
  (** Get chapters of a video
  
      **PeerTube >= 6.0** 
      @param id The object id, uuid or short uuid
  *)
  val get_video_chapters : id:string -> t -> unit -> T.t
end

module VideoChannelEdit : sig
  module T : sig
    type t
    
    (** Construct a value
        @param description Channel description
        @param display_name Channel display name
        @param support How to support/fund the channel
    *)
    val v : ?description:Jsont.json -> ?display_name:Jsont.json -> ?support:Jsont.json -> unit -> t
    
    (** Channel description *)
    val description : t -> Jsont.json option
    
    (** Channel display name *)
    val display_name : t -> Jsont.json option
    
    (** How to support/fund the channel *)
    val support : t -> Jsont.json option
    
    val jsont : t Jsont.t
  end
end

module VideoChannelCollaboratorState : sig
  module T : sig
    (** The user import state:
      - `1`: Pending
      - `2`: Accepted
     *)
    type t = string
    
    val jsont : t Jsont.t
  end
end

module VideoChannelActivityTarget : sig
  module T : sig
    (** The activity target:
      - VIDEO: 1,
      - PLAYLIST: 2,
      - CHANNEL: 3,
      - CHANNEL_SYNC: 4,
      - VIDEO_IMPORT: 5
     *)
    type t = string
    
    val jsont : t Jsont.t
  end
end

module VideoChannelActivityAction : sig
  module T : sig
    (** The activity action:
      - CREATE: 1
      - UPDATE: 2
      - DELETE: 3
      - UPDATE_CAPTIONS: 4
      - UPDATE_CHAPTERS: 5
      - UPDATE_PASSWORDS: 6
      - CREATE_STUDIO_TASKS: 7
      - UPDATE_SOURCE_FILE: 8
      - UPDATE_ELEMENTS: 9
      - REMOVE_CHANNEL_OWNERSHIP: 10
      - CREATE_CHANNEL_OWNERSHIP: 11
     *)
    type t = string
    
    val jsont : t Jsont.t
  end
end

module VideoCategorySet : sig
  module T : sig
    (** category id of the video (see [/videos/categories](#operation/getCategories)) *)
    type t = Jsont.json
    
    val jsont : t Jsont.t
    
    val v : unit -> t
  end
end

module VideoConstantNumberCategory : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?id:VideoCategorySet.T.t -> ?label:string -> unit -> t
    
    val id : t -> VideoCategorySet.T.t option
    
    val label : t -> string option
    
    val jsont : t Jsont.t
  end
end

module Uuidv4 : sig
  module T : sig
    type t = Jsont.json
    
    val jsont : t Jsont.t
    
    val v : unit -> t
  end
end

module UsernameChannel : sig
  module T : sig
    (** immutable name of the channel, used to interact with its actor *)
    type t = Jsont.json
    
    val jsont : t Jsont.t
    
    val v : unit -> t
  end
end

module Username : sig
  module T : sig
    (** immutable name of the user, used to find or mention its actor *)
    type t = Jsont.json
    
    val jsont : t Jsont.t
    
    val v : unit -> t
  end
end

module UserRole : sig
  module T : sig
    (** The user role (Admin = `0`, Moderator = `1`, User = `2`) *)
    type t = string
    
    val jsont : t Jsont.t
  end
end

module UserRegistrationAcceptOrReject : sig
  module T : sig
    type t
    
    (** Construct a value
        @param moderation_response Moderation response to send to the user
        @param prevent_email_delivery Set it to true if you don't want PeerTube to send an email to the user
    *)
    val v : moderation_response:string -> ?prevent_email_delivery:bool -> unit -> t
    
    (** Moderation response to send to the user *)
    val moderation_response : t -> string
    
    (** Set it to true if you don't want PeerTube to send an email to the user *)
    val prevent_email_delivery : t -> bool option
    
    val jsont : t Jsont.t
  end
end

module UserImportState : sig
  module T : sig
    (** The user import state:
      - `1`: Pending
      - `2`: Processing
      - `3`: Completed
      - `4`: Errored
     *)
    type t = string
    
    val jsont : t Jsont.t
  end
end

module UserImportResumable : sig
  module T : sig
    type t
    
    (** Construct a value
        @param filename Archive filename including extension
    *)
    val v : ?filename:string -> unit -> t
    
    (** Archive filename including extension *)
    val filename : t -> string option
    
    val jsont : t Jsont.t
  end
end

module UserExportState : sig
  module T : sig
    (** The user export state:
      - `1`: Pending
      - `2`: Processing
      - `3`: Completed
      - `4`: Errored
     *)
    type t = string
    
    val jsont : t Jsont.t
  end
end

module UserAdminFlags : sig
  module T : sig
    (** Admin flags for the user (None = `0`, Bypass video blocklist = `1`) *)
    type t = string
    
    val jsont : t Jsont.t
  end
end

module TokenSession : sig
  module T : sig
    type t
    
    (** Construct a value
        @param current_session Is this session the current one?
        @param login_date Date of the login
        @param login_device Device used to login
        @param login_ip IP address used to login
    *)
    val v : ?created_at:Ptime.t -> ?current_session:bool -> ?id:int -> ?last_activity_date:Ptime.t -> ?last_activity_device:string -> ?last_activity_ip:string -> ?login_date:Ptime.t -> ?login_device:string -> ?login_ip:string -> unit -> t
    
    val created_at : t -> Ptime.t option
    
    (** Is this session the current one? *)
    val current_session : t -> bool option
    
    val id : t -> int option
    
    val last_activity_date : t -> Ptime.t option
    
    val last_activity_device : t -> string option
    
    val last_activity_ip : t -> string option
    
    (** Date of the login *)
    val login_date : t -> Ptime.t option
    
    (** Device used to login *)
    val login_device : t -> string option
    
    (** IP address used to login *)
    val login_ip : t -> string option
    
    val jsont : t Jsont.t
  end
end

module Storyboard : sig
  module T : sig
    type t
    
    (** Construct a value
        @param file_url **PeerTube >= 7.1**
        @param storyboard_path Deprecated in PeerTube v8.0, use fileUrl instead
    *)
    val v : ?file_url:string -> ?sprite_duration:int -> ?sprite_height:int -> ?sprite_width:int -> ?storyboard_path:string -> ?total_height:int -> ?total_width:int -> unit -> t
    
    (** **PeerTube >= 7.1** *)
    val file_url : t -> string option
    
    val sprite_duration : t -> int option
    
    val sprite_height : t -> int option
    
    val sprite_width : t -> int option
    
    (** Deprecated in PeerTube v8.0, use fileUrl instead *)
    val storyboard_path : t -> string option
    
    val total_height : t -> int option
    
    val total_width : t -> int option
    
    val jsont : t Jsont.t
  end
end

module ShortUuid : sig
  module T : sig
    (** translation of a uuid v4 with a bigger alphabet to have a shorter uuid *)
    type t = Jsont.json
    
    val jsont : t Jsont.t
    
    val v : unit -> t
  end
end

module ServerStats : sig
  module T : sig
    type t
    
    (** Construct a value
        @param average_abuse_response_time_ms **PeerTube >= 6.1** Value is null if the admin disabled abuses stats
        @param average_registration_request_response_time_ms **PeerTube >= 6.1** Value is null if the admin disabled registration requests stats
        @param total_abuses **PeerTube >= 6.1** Value is null if the admin disabled abuses stats
        @param total_abuses_processed **PeerTube >= 6.1** Value is null if the admin disabled abuses stats
        @param total_admins **PeerTube >= 6.1** Value is null if the admin disabled total admins stats
        @param total_local_video_comments Total comments made by local users
        @param total_local_video_views Total video views made on the instance
        @param total_moderators **PeerTube >= 6.1** Value is null if the admin disabled total moderators stats
        @param total_registration_requests **PeerTube >= 6.1** Value is null if the admin disabled registration requests stats
        @param total_registration_requests_processed **PeerTube >= 6.1** Value is null if the admin disabled registration requests stats
    *)
    val v : ?activity_pub_messages_processed_per_second:float -> ?average_abuse_response_time_ms:float -> ?average_registration_request_response_time_ms:float -> ?total_abuses:float -> ?total_abuses_processed:float -> ?total_activity_pub_messages_errors:float -> ?total_activity_pub_messages_processed:float -> ?total_activity_pub_messages_successes:float -> ?total_activity_pub_messages_waiting:float -> ?total_admins:float -> ?total_daily_active_users:float -> ?total_instance_followers:float -> ?total_instance_following:float -> ?total_local_daily_active_video_channels:float -> ?total_local_monthly_active_video_channels:float -> ?total_local_playlists:float -> ?total_local_video_channels:float -> ?total_local_video_comments:float -> ?total_local_video_files_size:float -> ?total_local_video_views:float -> ?total_local_videos:float -> ?total_local_weekly_active_video_channels:float -> ?total_moderators:float -> ?total_monthly_active_users:float -> ?total_registration_requests:float -> ?total_registration_requests_processed:float -> ?total_users:float -> ?total_video_comments:float -> ?total_videos:float -> ?total_weekly_active_users:float -> ?videos_redundancy:Jsont.json list -> unit -> t
    
    val activity_pub_messages_processed_per_second : t -> float option
    
    (** **PeerTube >= 6.1** Value is null if the admin disabled abuses stats *)
    val average_abuse_response_time_ms : t -> float option
    
    (** **PeerTube >= 6.1** Value is null if the admin disabled registration requests stats *)
    val average_registration_request_response_time_ms : t -> float option
    
    (** **PeerTube >= 6.1** Value is null if the admin disabled abuses stats *)
    val total_abuses : t -> float option
    
    (** **PeerTube >= 6.1** Value is null if the admin disabled abuses stats *)
    val total_abuses_processed : t -> float option
    
    val total_activity_pub_messages_errors : t -> float option
    
    val total_activity_pub_messages_processed : t -> float option
    
    val total_activity_pub_messages_successes : t -> float option
    
    val total_activity_pub_messages_waiting : t -> float option
    
    (** **PeerTube >= 6.1** Value is null if the admin disabled total admins stats *)
    val total_admins : t -> float option
    
    val total_daily_active_users : t -> float option
    
    val total_instance_followers : t -> float option
    
    val total_instance_following : t -> float option
    
    val total_local_daily_active_video_channels : t -> float option
    
    val total_local_monthly_active_video_channels : t -> float option
    
    val total_local_playlists : t -> float option
    
    val total_local_video_channels : t -> float option
    
    (** Total comments made by local users *)
    val total_local_video_comments : t -> float option
    
    val total_local_video_files_size : t -> float option
    
    (** Total video views made on the instance *)
    val total_local_video_views : t -> float option
    
    val total_local_videos : t -> float option
    
    val total_local_weekly_active_video_channels : t -> float option
    
    (** **PeerTube >= 6.1** Value is null if the admin disabled total moderators stats *)
    val total_moderators : t -> float option
    
    val total_monthly_active_users : t -> float option
    
    (** **PeerTube >= 6.1** Value is null if the admin disabled registration requests stats *)
    val total_registration_requests : t -> float option
    
    (** **PeerTube >= 6.1** Value is null if the admin disabled registration requests stats *)
    val total_registration_requests_processed : t -> float option
    
    val total_users : t -> float option
    
    val total_video_comments : t -> float option
    
    val total_videos : t -> float option
    
    val total_weekly_active_users : t -> float option
    
    val videos_redundancy : t -> Jsont.json list option
    
    val jsont : t Jsont.t
  end
  
  (** Get instance stats
  
      Get instance public statistics. This endpoint is cached. *)
  val get_instance_stats : t -> unit -> T.t
end

module ServerError : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?code:string -> ?detail:string -> ?status:int -> ?type_:string -> unit -> t
    
    val code : t -> string option
    
    val detail : t -> string option
    
    val status : t -> int option
    
    val type_ : t -> string option
    
    val jsont : t Jsont.t
  end
end

module ServerConfigCustom : sig
  module T : sig
    type t
    
    (** Construct a value
        @param transcoding Settings pertaining to transcoding jobs
        @param user Settings that apply to new users, if registration is enabled
    *)
    val v : ?admin:Jsont.json -> ?auto_blacklist:Jsont.json -> ?cache:Jsont.json -> ?contact_form:Jsont.json -> ?defaults:Jsont.json -> ?followers:Jsont.json -> ?import:Jsont.json -> ?instance:Jsont.json -> ?services:Jsont.json -> ?signup:Jsont.json -> ?storyboard:Jsont.json -> ?theme:Jsont.json -> ?transcoding:Jsont.json -> ?user:Jsont.json -> unit -> t
    
    val admin : t -> Jsont.json option
    
    val auto_blacklist : t -> Jsont.json option
    
    val cache : t -> Jsont.json option
    
    val contact_form : t -> Jsont.json option
    
    val defaults : t -> Jsont.json option
    
    val followers : t -> Jsont.json option
    
    val import : t -> Jsont.json option
    
    val instance : t -> Jsont.json option
    
    val services : t -> Jsont.json option
    
    val signup : t -> Jsont.json option
    
    val storyboard : t -> Jsont.json option
    
    val theme : t -> Jsont.json option
    
    (** Settings pertaining to transcoding jobs *)
    val transcoding : t -> Jsont.json option
    
    (** Settings that apply to new users, if registration is enabled *)
    val user : t -> Jsont.json option
    
    val jsont : t Jsont.t
  end
  
  (** Get instance runtime configuration *)
  val get_custom_config : t -> unit -> T.t
end

module ServerConfigAbout : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?instance:Jsont.json -> unit -> t
    
    val instance : t -> Jsont.json option
    
    val jsont : t Jsont.t
  end
  
  (** Get instance "About" information *)
  val get_about : t -> unit -> T.t
end

module Server : sig
  module Config : sig
    type t
    
    (** Construct a value
        @param open_telemetry PeerTube >= 6.1
        @param views PeerTube >= 6.1
    *)
    val v : ?auto_blacklist:Jsont.json -> ?avatar:Jsont.json -> ?contact_form:Jsont.json -> ?email:Jsont.json -> ?export:Jsont.json -> ?federation:Jsont.json -> ?followings:Jsont.json -> ?homepage:Jsont.json -> ?import:Jsont.json -> ?instance:Jsont.json -> ?open_telemetry:Jsont.json -> ?plugin:Jsont.json -> ?search:Jsont.json -> ?server_commit:string -> ?server_version:string -> ?signup:Jsont.json -> ?theme:Jsont.json -> ?tracker:Jsont.json -> ?transcoding:Jsont.json -> ?trending:Jsont.json -> ?user:Jsont.json -> ?video:Jsont.json -> ?video_caption:Jsont.json -> ?views:Jsont.json -> unit -> t
    
    val auto_blacklist : t -> Jsont.json option
    
    val avatar : t -> Jsont.json option
    
    val contact_form : t -> Jsont.json option
    
    val email : t -> Jsont.json option
    
    val export : t -> Jsont.json option
    
    val federation : t -> Jsont.json option
    
    val followings : t -> Jsont.json option
    
    val homepage : t -> Jsont.json option
    
    val import : t -> Jsont.json option
    
    val instance : t -> Jsont.json option
    
    (** PeerTube >= 6.1 *)
    val open_telemetry : t -> Jsont.json option
    
    val plugin : t -> Jsont.json option
    
    val search : t -> Jsont.json option
    
    val server_commit : t -> string option
    
    val server_version : t -> string option
    
    val signup : t -> Jsont.json option
    
    val theme : t -> Jsont.json option
    
    val tracker : t -> Jsont.json option
    
    val transcoding : t -> Jsont.json option
    
    val trending : t -> Jsont.json option
    
    val user : t -> Jsont.json option
    
    val video : t -> Jsont.json option
    
    val video_caption : t -> Jsont.json option
    
    (** PeerTube >= 6.1 *)
    val views : t -> Jsont.json option
    
    val jsont : t Jsont.t
  end
  
  (** Get instance public configuration *)
  val get_config : t -> unit -> Config.t
end

module SendClientLog : sig
  module T : sig
    type t
    
    (** Construct a value
        @param url URL of the current user page
        @param meta Additional information regarding this log
        @param stack_trace Stack trace of the error if there is one
        @param user_agent User agent of the web browser that sends the message
    *)
    val v : level:Jsont.json -> message:string -> url:string -> ?meta:string -> ?stack_trace:string -> ?user_agent:string -> unit -> t
    
    val level : t -> Jsont.json
    
    val message : t -> string
    
    (** Additional information regarding this log *)
    val meta : t -> string option
    
    (** Stack trace of the error if there is one *)
    val stack_trace : t -> string option
    
    (** URL of the current user page *)
    val url : t -> string
    
    (** User agent of the web browser that sends the message *)
    val user_agent : t -> string option
    
    val jsont : t Jsont.t
  end
end

module RunnerRegistrationToken : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?created_at:Ptime.t -> ?id:int -> ?registered_runners_count:int -> ?registration_token:string -> ?updated_at:Ptime.t -> unit -> t
    
    val created_at : t -> Ptime.t option
    
    val id : t -> int option
    
    val registered_runners_count : t -> int option
    
    val registration_token : t -> string option
    
    val updated_at : t -> Ptime.t option
    
    val jsont : t Jsont.t
  end
end

module RunnerJobState : sig
  module T : sig
    (** The runner job state:
      - `1` Pending
      - `2` Processing
      - `3` Completed
      - `4` Errored
      - `5` Waiting for a parent job
      - `6` Cancelled
      - `7` Parent had an error
      - `8` Parent has been cancelled
     *)
    type t = string
    
    val jsont : t Jsont.t
  end
end

module RunnerJobStateConstant : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?id:RunnerJobState.T.t -> ?label:string -> unit -> t
    
    val id : t -> RunnerJobState.T.t option
    
    val label : t -> string option
    
    val jsont : t Jsont.t
  end
end

module RunnerJobPayload : sig
  module T : sig
    type t = Jsont.json
    
    val jsont : t Jsont.t
    
    val v : unit -> t
  end
end

module RunnerJob : sig
  module Type : sig
    type t = [
      | `Vod_web_video_transcoding
      | `Vod_hls_transcoding
      | `Vod_audio_merge_transcoding
      | `Live_rtmp_hls_transcoding
    ]
    
    val jsont : t Jsont.t
  end
  
  module T : sig
    type t
    
    (** Construct a value
        @param error Error message if the job is errored
        @param failures Number of times a remote runner failed to process this job. After too many failures, the job in "error" state
        @param parent If job has a parent job
        @param priority Job priority (less has more priority)
        @param progress Percentage progress
        @param runner If job is associated to a runner
    *)
    val v : ?created_at:Ptime.t -> ?error:string -> ?failures:int -> ?finished_at:Ptime.t -> ?parent:Jsont.json -> ?payload:RunnerJobPayload.T.t -> ?priority:int -> ?progress:int -> ?runner:Jsont.json -> ?started_at:Ptime.t -> ?state:RunnerJobStateConstant.T.t -> ?type_:Type.t -> ?updated_at:Ptime.t -> ?uuid:Uuidv4.T.t -> unit -> t
    
    val created_at : t -> Ptime.t option
    
    (** Error message if the job is errored *)
    val error : t -> string option
    
    (** Number of times a remote runner failed to process this job. After too many failures, the job in "error" state *)
    val failures : t -> int option
    
    val finished_at : t -> Ptime.t option
    
    (** If job has a parent job *)
    val parent : t -> Jsont.json option
    
    val payload : t -> RunnerJobPayload.T.t option
    
    (** Job priority (less has more priority) *)
    val priority : t -> int option
    
    (** Percentage progress *)
    val progress : t -> int option
    
    (** If job is associated to a runner *)
    val runner : t -> Jsont.json option
    
    val started_at : t -> Ptime.t option
    
    val state : t -> RunnerJobStateConstant.T.t option
    
    val type_ : t -> Type.t option
    
    val updated_at : t -> Ptime.t option
    
    val uuid : t -> Uuidv4.T.t option
    
    val jsont : t Jsont.t
  end
end

module RunnerJobAdmin : sig
  module T : sig
    type t
    
    (** Construct a value
        @param error Error message if the job is errored
        @param failures Number of times a remote runner failed to process this job. After too many failures, the job in "error" state
        @param parent If job has a parent job
        @param priority Job priority (less has more priority)
        @param progress Percentage progress
        @param runner If job is associated to a runner
    *)
    val v : ?created_at:Ptime.t -> ?error:string -> ?failures:int -> ?finished_at:Ptime.t -> ?parent:Jsont.json -> ?payload:RunnerJobPayload.T.t -> ?priority:int -> ?progress:int -> ?runner:Jsont.json -> ?started_at:Ptime.t -> ?state:RunnerJobStateConstant.T.t -> ?type_:RunnerJob.Type.t -> ?updated_at:Ptime.t -> ?uuid:Uuidv4.T.t -> ?private_payload:Jsont.json -> unit -> t
    
    val created_at : t -> Ptime.t option
    
    (** Error message if the job is errored *)
    val error : t -> string option
    
    (** Number of times a remote runner failed to process this job. After too many failures, the job in "error" state *)
    val failures : t -> int option
    
    val finished_at : t -> Ptime.t option
    
    (** If job has a parent job *)
    val parent : t -> Jsont.json option
    
    val payload : t -> RunnerJobPayload.T.t option
    
    (** Job priority (less has more priority) *)
    val priority : t -> int option
    
    (** Percentage progress *)
    val progress : t -> int option
    
    (** If job is associated to a runner *)
    val runner : t -> Jsont.json option
    
    val started_at : t -> Ptime.t option
    
    val state : t -> RunnerJobStateConstant.T.t option
    
    val type_ : t -> RunnerJob.Type.t option
    
    val updated_at : t -> Ptime.t option
    
    val uuid : t -> Uuidv4.T.t option
    
    val private_payload : t -> Jsont.json option
    
    val jsont : t Jsont.t
  end
end

module Runner : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?created_at:Ptime.t -> ?description:string -> ?id:int -> ?ip:string -> ?last_contact:Ptime.t -> ?name:string -> ?updated_at:Ptime.t -> unit -> t
    
    val created_at : t -> Ptime.t option
    
    val description : t -> string option
    
    val id : t -> int option
    
    val ip : t -> string option
    
    val last_contact : t -> Ptime.t option
    
    val name : t -> string option
    
    val updated_at : t -> Ptime.t option
    
    val jsont : t Jsont.t
  end
end

module RequestTwoFactor : sig
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : ?otp_request:Jsont.json -> unit -> t
    
    val otp_request : t -> Jsont.json option
    
    val jsont : t Jsont.t
  end
  
  (** Request two factor auth
  
      Request two factor authentication for a user 
      @param id Entity id
  *)
  val request_two_factor : id:string -> t -> unit -> Response.t
end

module PredefinedAbuseReasons : sig
  module T : sig
    (** Reason categories that help triage reports *)
    type t = Jsont.json
    
    val jsont : t Jsont.t
    
    val v : unit -> t
  end
end

module Plugin : sig
  module T : sig
    type t
    
    (** Construct a value
        @param type_ - `1`: PLUGIN
    - `2`: THEME
    
    *)
    val v : ?created_at:Ptime.t -> ?description:string -> ?enabled:bool -> ?homepage:string -> ?latest_version:string -> ?name:string -> ?peertube_engine:string -> ?settings:Jsont.json -> ?type_:int -> ?uninstalled:bool -> ?updated_at:Ptime.t -> ?version:string -> unit -> t
    
    val created_at : t -> Ptime.t option
    
    val description : t -> string option
    
    val enabled : t -> bool option
    
    val homepage : t -> string option
    
    val latest_version : t -> string option
    
    val name : t -> string option
    
    val peertube_engine : t -> string option
    
    val settings : t -> Jsont.json option
    
    (** - `1`: PLUGIN
    - `2`: THEME
     *)
    val type_ : t -> int option
    
    val uninstalled : t -> bool option
    
    val updated_at : t -> Ptime.t option
    
    val version : t -> string option
    
    val jsont : t Jsont.t
  end
  
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : ?data:T.t list -> ?total:int -> unit -> t
    
    val data : t -> T.t list option
    
    val total : t -> int option
    
    val jsont : t Jsont.t
  end
  
  (** List plugins 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  val get_plugins : ?plugin_type:string -> ?uninstalled:string -> ?start:string -> ?count:string -> ?sort:string -> t -> unit -> Response.t
  
  (** List available plugins 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  val get_available_plugins : ?search:string -> ?plugin_type:string -> ?current_peer_tube_engine:string -> ?start:string -> ?count:string -> ?sort:string -> t -> unit -> Response.t
  
  (** Get a plugin 
      @param npm_name name of the plugin/theme on npmjs.com or in its package.json
  *)
  val get_plugin : npm_name:string -> t -> unit -> T.t
end

module PlayerThemeVideoSetting : sig
  module T : sig
    (** Player theme setting for a video:
      - `channel-default` Use the channel default theme
      - `instance-default` Use the instance default theme
      - `galaxy` Use the galaxy theme
      - `lucide` Use the lucide theme
     *)
    type t = [
      | `Channel_default
      | `Instance_default
      | `Galaxy
      | `Lucide
    ]
    
    val jsont : t Jsont.t
  end
end

module PlayerVideoSettings : sig
  module Update : sig
    (** Player settings update for a video *)
    type t
    
    (** Construct a value *)
    val v : theme:PlayerThemeVideoSetting.T.t -> unit -> t
    
    val theme : t -> PlayerThemeVideoSetting.T.t
    
    val jsont : t Jsont.t
  end
  
  module T : sig
    (** Player settings for a video *)
    type t
    
    (** Construct a value *)
    val v : ?theme:PlayerThemeVideoSetting.T.t -> unit -> t
    
    val theme : t -> PlayerThemeVideoSetting.T.t option
    
    val jsont : t Jsont.t
  end
  
  (** Get video player settings
  
      Get player settings for a specific video. Returns video-specific settings merged with channel player settings. 
      @param id The object id, uuid or short uuid
      @param raw Return raw settings without merging channel defaults
  *)
  val get_video_player_settings : id:string -> ?raw:string -> t -> unit -> T.t
  
  (** Update video player settings
  
      Update player settings for a specific video 
      @param id The object id, uuid or short uuid
  *)
  val update_video_player_settings : id:string -> body:Update.t -> t -> unit -> T.t
end

module PlayerThemeChannelSetting : sig
  module T : sig
    (** Player theme setting for a channel:
      - `instance-default` Use the instance default theme
      - `galaxy` Use the galaxy theme
      - `lucide` Use the lucide theme
     *)
    type t = [
      | `Instance_default
      | `Galaxy
      | `Lucide
    ]
    
    val jsont : t Jsont.t
  end
end

module PlayerChannelSettings : sig
  module Update : sig
    (** Player settings update for a channel *)
    type t
    
    (** Construct a value *)
    val v : theme:PlayerThemeChannelSetting.T.t -> unit -> t
    
    val theme : t -> PlayerThemeChannelSetting.T.t
    
    val jsont : t Jsont.t
  end
  
  module T : sig
    (** Player settings for a channel *)
    type t
    
    (** Construct a value *)
    val v : ?theme:PlayerThemeChannelSetting.T.t -> unit -> t
    
    val theme : t -> PlayerThemeChannelSetting.T.t option
    
    val jsont : t Jsont.t
  end
  
  (** Get channel player settings
  
      Get player settings for a video channel. 
      @param channel_handle The video channel handle
      @param raw Return raw settings without applying instance defaults
  *)
  val get_channel_player_settings : channel_handle:string -> ?raw:string -> t -> unit -> T.t
  
  (** Update channel player settings
  
      Update default player settings for a video channel. 
      @param channel_handle The video channel handle
  *)
  val update_channel_player_settings : channel_handle:string -> body:Update.t -> t -> unit -> T.t
end

module PlayerTheme : sig
  module T : sig
    (** The player theme to use *)
    type t = [
      | `Galaxy
      | `Lucide
    ]
    
    val jsont : t Jsont.t
  end
end

module PlaybackMetric : sig
  module Create : sig
    type t
    
    (** Construct a value
        @param downloaded_bytes_http How many bytes were downloaded with HTTP since the last metric creation
        @param downloaded_bytes_p2_p How many bytes were downloaded with P2P since the last metric creation
        @param errors How many errors occurred since the last metric creation
        @param resolution_changes How many resolution changes occurred since the last metric creation
        @param uploaded_bytes_p2_p How many bytes were uploaded with P2P since the last metric creation
        @param buffer_stalled How many times buffer has been stalled since the last metric creation
        @param fps Current player video fps
        @param p2p_peers P2P peers connected (doesn't include WebSeed peers)
        @param resolution Current player video resolution
    *)
    val v : downloaded_bytes_http:float -> downloaded_bytes_p2_p:float -> errors:float -> p2p_enabled:bool -> player_mode:string -> resolution_changes:float -> uploaded_bytes_p2_p:float -> video_id:Jsont.json -> ?buffer_stalled:float -> ?fps:float -> ?p2p_peers:float -> ?resolution:float -> unit -> t
    
    (** How many times buffer has been stalled since the last metric creation *)
    val buffer_stalled : t -> float option
    
    (** How many bytes were downloaded with HTTP since the last metric creation *)
    val downloaded_bytes_http : t -> float
    
    (** How many bytes were downloaded with P2P since the last metric creation *)
    val downloaded_bytes_p2_p : t -> float
    
    (** How many errors occurred since the last metric creation *)
    val errors : t -> float
    
    (** Current player video fps *)
    val fps : t -> float option
    
    val p2p_enabled : t -> bool
    
    (** P2P peers connected (doesn't include WebSeed peers) *)
    val p2p_peers : t -> float option
    
    val player_mode : t -> string
    
    (** Current player video resolution *)
    val resolution : t -> float option
    
    (** How many resolution changes occurred since the last metric creation *)
    val resolution_changes : t -> float
    
    (** How many bytes were uploaded with P2P since the last metric creation *)
    val uploaded_bytes_p2_p : t -> float
    
    val video_id : t -> Jsont.json
    
    val jsont : t Jsont.t
  end
end

module Password : sig
  module T : sig
    type t = Jsont.json
    
    val jsont : t Jsont.t
    
    val v : unit -> t
  end
end

module UpdateUser : sig
  module T : sig
    type t
    
    (** Construct a value
        @param email The updated email of the user
        @param email_verified Set the email as verified
        @param plugin_auth The auth plugin to use to authenticate the user
        @param video_quota The updated video quota of the user in bytes
        @param video_quota_daily The updated daily video quota of the user in bytes
    *)
    val v : ?admin_flags:UserAdminFlags.T.t -> ?email:Jsont.json -> ?email_verified:bool -> ?password:Password.T.t -> ?plugin_auth:string -> ?role:UserRole.T.t -> ?video_quota:int -> ?video_quota_daily:int -> unit -> t
    
    val admin_flags : t -> UserAdminFlags.T.t option
    
    (** The updated email of the user *)
    val email : t -> Jsont.json option
    
    (** Set the email as verified *)
    val email_verified : t -> bool option
    
    val password : t -> Password.T.t option
    
    (** The auth plugin to use to authenticate the user *)
    val plugin_auth : t -> string option
    
    val role : t -> UserRole.T.t option
    
    (** The updated video quota of the user in bytes *)
    val video_quota : t -> int option
    
    (** The updated daily video quota of the user in bytes *)
    val video_quota_daily : t -> int option
    
    val jsont : t Jsont.t
  end
end

module RegisterUser : sig
  module T : sig
    type t
    
    (** Construct a value
        @param email email of the user, used for login or service communications
        @param username immutable name of the user, used to find or mention its actor
        @param channel channel base information used to create the first channel of the user
        @param display_name editable name of the user, displayed in its representations
    *)
    val v : email:string -> password:Password.T.t -> username:Username.T.t -> ?channel:Jsont.json -> ?display_name:string -> unit -> t
    
    (** channel base information used to create the first channel of the user *)
    val channel : t -> Jsont.json option
    
    (** editable name of the user, displayed in its representations *)
    val display_name : t -> string option
    
    (** email of the user, used for login or service communications *)
    val email : t -> string
    
    val password : t -> Password.T.t
    
    (** immutable name of the user, used to find or mention its actor *)
    val username : t -> Username.T.t
    
    val jsont : t Jsont.t
  end
end

module OauthTokenPassword : sig
  module T : sig
    type t
    
    (** Construct a value
        @param external_auth_token If you want to authenticate using an external authentication token you got from an auth plugin (like `peertube-plugin-auth-openid-connect` for example) instead of a password or a refresh token, provide it here.
    *)
    val v : client_id:string -> client_secret:string -> grant_type:string -> username:Jsont.json -> ?password:Password.T.t -> ?external_auth_token:string -> unit -> t
    
    val client_id : t -> string
    
    val client_secret : t -> string
    
    val grant_type : t -> string
    
    val username : t -> Jsont.json
    
    val password : t -> Password.T.t option
    
    (** If you want to authenticate using an external authentication token you got from an auth plugin (like `peertube-plugin-auth-openid-connect` for example) instead of a password or a refresh token, provide it here. *)
    val external_auth_token : t -> string option
    
    val jsont : t Jsont.t
  end
end

module AddUser : sig
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : ?user:Jsont.json -> unit -> t
    
    val user : t -> Jsont.json option
    
    val jsont : t Jsont.t
  end
  
  module T : sig
    type t
    
    (** Construct a value
        @param email The user email
        @param video_quota The user video quota in bytes
        @param video_quota_daily The user daily video quota in bytes
    *)
    val v : email:string -> password:Password.T.t -> role:UserRole.T.t -> username:Username.T.t -> ?admin_flags:UserAdminFlags.T.t -> ?channel_name:UsernameChannel.T.t -> ?video_quota:int -> ?video_quota_daily:int -> unit -> t
    
    val admin_flags : t -> UserAdminFlags.T.t option
    
    val channel_name : t -> UsernameChannel.T.t option
    
    (** The user email *)
    val email : t -> string
    
    val password : t -> Password.T.t
    
    val role : t -> UserRole.T.t
    
    val username : t -> Username.T.t
    
    (** The user video quota in bytes *)
    val video_quota : t -> int option
    
    (** The user daily video quota in bytes *)
    val video_quota_daily : t -> int option
    
    val jsont : t Jsont.t
  end
  
  (** Create a user *)
  val add_user : body:T.t -> t -> unit -> Response.t
end

module OauthTokenRefreshToken : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : client_id:string -> client_secret:string -> grant_type:string -> refresh_token:string -> unit -> t
    
    val client_id : t -> string
    
    val client_secret : t -> string
    
    val grant_type : t -> string
    
    val refresh_token : t -> string
    
    val jsont : t Jsont.t
  end
end

module OauthClient : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?client_id:string -> ?client_secret:string -> unit -> t
    
    val client_id : t -> string option
    
    val client_secret : t -> string option
    
    val jsont : t Jsont.t
  end
  
  (** Login prerequisite
  
      You need to retrieve a client id and secret before [logging in](#operation/getOAuthToken). *)
  val get_oauth_client : t -> unit -> T.t
end

module Nsfwpolicy : sig
  module T : sig
    type t = [
      | `Display
      | `Warn
      | `Do_not_list
    ]
    
    val jsont : t Jsont.t
  end
end

module Nsfwflag : sig
  module T : sig
    (** 
    NSFW flags (can be combined using bitwise or operator)
    - `0` NONE
    - `1` VIOLENT
    - `2` EXPLICIT_SEX
     *)
    type t = string
    
    val jsont : t Jsont.t
  end
end

module UpdateMe : sig
  module T : sig
    type t
    
    (** Construct a value
        @param auto_play_next_video new preference regarding playing following videos automatically
        @param auto_play_next_video_playlist new preference regarding playing following playlist videos automatically
        @param auto_play_video new preference regarding playing videos automatically
        @param display_name new name of the user in its representations
        @param email new email used for login and service communications
        @param language default language for this user
        @param nsfw_policy new NSFW display policy
        @param p2p_enabled whether to enable P2P in the player or not
        @param video_languages list of languages to filter videos down to
        @param videos_history_enabled whether to keep track of watched history or not
    *)
    val v : ?auto_play_next_video:bool -> ?auto_play_next_video_playlist:bool -> ?auto_play_video:bool -> ?current_password:Password.T.t -> ?display_name:string -> ?email:Jsont.json -> ?language:string -> ?no_account_setup_warning_modal:bool -> ?no_instance_config_warning_modal:bool -> ?no_welcome_modal:bool -> ?nsfw_flags_blurred:Nsfwflag.T.t -> ?nsfw_flags_displayed:Nsfwflag.T.t -> ?nsfw_flags_hidden:Nsfwflag.T.t -> ?nsfw_flags_warned:Nsfwflag.T.t -> ?nsfw_policy:string -> ?p2p_enabled:bool -> ?password:Password.T.t -> ?theme:string -> ?video_languages:string list -> ?videos_history_enabled:bool -> unit -> t
    
    (** new preference regarding playing following videos automatically *)
    val auto_play_next_video : t -> bool option
    
    (** new preference regarding playing following playlist videos automatically *)
    val auto_play_next_video_playlist : t -> bool option
    
    (** new preference regarding playing videos automatically *)
    val auto_play_video : t -> bool option
    
    val current_password : t -> Password.T.t option
    
    (** new name of the user in its representations *)
    val display_name : t -> string option
    
    (** new email used for login and service communications *)
    val email : t -> Jsont.json option
    
    (** default language for this user *)
    val language : t -> string option
    
    val no_account_setup_warning_modal : t -> bool option
    
    val no_instance_config_warning_modal : t -> bool option
    
    val no_welcome_modal : t -> bool option
    
    val nsfw_flags_blurred : t -> Nsfwflag.T.t option
    
    val nsfw_flags_displayed : t -> Nsfwflag.T.t option
    
    val nsfw_flags_hidden : t -> Nsfwflag.T.t option
    
    val nsfw_flags_warned : t -> Nsfwflag.T.t option
    
    (** new NSFW display policy *)
    val nsfw_policy : t -> string option
    
    (** whether to enable P2P in the player or not *)
    val p2p_enabled : t -> bool option
    
    val password : t -> Password.T.t option
    
    val theme : t -> string option
    
    (** list of languages to filter videos down to *)
    val video_languages : t -> string list option
    
    (** whether to keep track of watched history or not *)
    val videos_history_enabled : t -> bool option
    
    val jsont : t Jsont.t
  end
end

module NotificationSettingValue : sig
  module T : sig
    (** Notification type. One of the following values, or a sum of multiple values:
    - `0` NONE
    - `1` WEB
    - `2` EMAIL
     *)
    type t = Jsont.json
    
    val jsont : t Jsont.t
    
    val v : unit -> t
  end
end

module UserNotificationSettings : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?abuse_as_moderator:NotificationSettingValue.T.t -> ?abuse_new_message:NotificationSettingValue.T.t -> ?abuse_state_change:NotificationSettingValue.T.t -> ?auto_instance_following:NotificationSettingValue.T.t -> ?blacklist_on_my_video:NotificationSettingValue.T.t -> ?comment_mention:NotificationSettingValue.T.t -> ?my_video_import_finished:NotificationSettingValue.T.t -> ?my_video_published:NotificationSettingValue.T.t -> ?my_video_studio_edition_finished:NotificationSettingValue.T.t -> ?my_video_transcription_generated:NotificationSettingValue.T.t -> ?new_comment_on_my_video:NotificationSettingValue.T.t -> ?new_follow:NotificationSettingValue.T.t -> ?new_instance_follower:NotificationSettingValue.T.t -> ?new_peer_tube_version:NotificationSettingValue.T.t -> ?new_plugin_version:NotificationSettingValue.T.t -> ?new_user_registration:NotificationSettingValue.T.t -> ?new_video_from_subscription:NotificationSettingValue.T.t -> ?video_auto_blacklist_as_moderator:NotificationSettingValue.T.t -> unit -> t
    
    val abuse_as_moderator : t -> NotificationSettingValue.T.t option
    
    val abuse_new_message : t -> NotificationSettingValue.T.t option
    
    val abuse_state_change : t -> NotificationSettingValue.T.t option
    
    val auto_instance_following : t -> NotificationSettingValue.T.t option
    
    val blacklist_on_my_video : t -> NotificationSettingValue.T.t option
    
    val comment_mention : t -> NotificationSettingValue.T.t option
    
    val my_video_import_finished : t -> NotificationSettingValue.T.t option
    
    val my_video_published : t -> NotificationSettingValue.T.t option
    
    val my_video_studio_edition_finished : t -> NotificationSettingValue.T.t option
    
    val my_video_transcription_generated : t -> NotificationSettingValue.T.t option
    
    val new_comment_on_my_video : t -> NotificationSettingValue.T.t option
    
    val new_follow : t -> NotificationSettingValue.T.t option
    
    val new_instance_follower : t -> NotificationSettingValue.T.t option
    
    val new_peer_tube_version : t -> NotificationSettingValue.T.t option
    
    val new_plugin_version : t -> NotificationSettingValue.T.t option
    
    val new_user_registration : t -> NotificationSettingValue.T.t option
    
    val new_video_from_subscription : t -> NotificationSettingValue.T.t option
    
    val video_auto_blacklist_as_moderator : t -> NotificationSettingValue.T.t option
    
    val jsont : t Jsont.t
  end
end

module NewFeatureInfo : sig
  module Type : sig
    (** Represent a new feature that can be displayed to inform users. One of the following values:
    
      - `1` CHANNEL_COLLABORATION
     *)
    type t = string
    
    val jsont : t Jsont.t
  end
end

module MrsspeerLink : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?href:string -> ?type_:string -> unit -> t
    
    val href : t -> string option
    
    val type_ : t -> string option
    
    val jsont : t Jsont.t
  end
end

module MrssgroupContent : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?duration:int -> ?file_size:int -> ?framerate:int -> ?height:int -> ?lang:string -> ?type_:string -> ?url:string -> unit -> t
    
    val duration : t -> int option
    
    val file_size : t -> int option
    
    val framerate : t -> int option
    
    val height : t -> int option
    
    val lang : t -> string option
    
    val type_ : t -> string option
    
    val url : t -> string option
    
    val jsont : t Jsont.t
  end
end

module LiveVideoSession : sig
  module Response : sig
    type t
    
    (** Construct a value
        @param end_date End date of the live session
        @param error Error type if an error occurred during the live session:
      - `1`: Bad socket health (transcoding is too slow)
      - `2`: Max duration exceeded
      - `3`: Quota exceeded
      - `4`: Quota FFmpeg error
      - `5`: Video has been blacklisted during the live
    
        @param replay_video Video replay information
        @param start_date Start date of the live session
    *)
    val v : ?end_date:Ptime.t -> ?error:int -> ?id:int -> ?replay_video:Jsont.json -> ?start_date:Ptime.t -> unit -> t
    
    (** End date of the live session *)
    val end_date : t -> Ptime.t option
    
    (** Error type if an error occurred during the live session:
      - `1`: Bad socket health (transcoding is too slow)
      - `2`: Max duration exceeded
      - `3`: Quota exceeded
      - `4`: Quota FFmpeg error
      - `5`: Video has been blacklisted during the live
     *)
    val error : t -> int option
    
    val id : t -> int option
    
    (** Video replay information *)
    val replay_video : t -> Jsont.json option
    
    (** Start date of the live session *)
    val start_date : t -> Ptime.t option
    
    val jsont : t Jsont.t
  end
  
  (** Get live session of a replay
  
      If the video is a replay of a live, you can find the associated live session using this endpoint 
      @param id The object id, uuid or short uuid
  *)
  val get_api_v1_videos_live_session : id:string -> t -> unit -> Response.t
end

module LiveVideoLatencyMode : sig
  module T : sig
    (** The live latency mode (Default = `1`, High latency = `2`, Small Latency = `3`) *)
    type t = string
    
    val jsont : t Jsont.t
  end
end

module LiveSchedule : sig
  module T : sig
    type t
    
    (** Construct a value
        @param start_at Date when the stream is scheduled to air at
    *)
    val v : ?start_at:Ptime.t -> unit -> t
    
    (** Date when the stream is scheduled to air at *)
    val start_at : t -> Ptime.t option
    
    val jsont : t Jsont.t
  end
end

module LiveVideo : sig
  module Update : sig
    type t
    
    (** Construct a value
        @param latency_mode User can select live latency mode if enabled by the instance
        @param permanent_live User can stream multiple times in a permanent live
    *)
    val v : ?latency_mode:LiveVideoLatencyMode.T.t -> ?permanent_live:bool -> ?replay_settings:LiveVideoReplaySettings.T.t -> ?save_replay:bool -> ?schedules:LiveSchedule.T.t list -> unit -> t
    
    (** User can select live latency mode if enabled by the instance *)
    val latency_mode : t -> LiveVideoLatencyMode.T.t option
    
    (** User can stream multiple times in a permanent live *)
    val permanent_live : t -> bool option
    
    val replay_settings : t -> LiveVideoReplaySettings.T.t option
    
    val save_replay : t -> bool option
    
    val schedules : t -> LiveSchedule.T.t list option
    
    val jsont : t Jsont.t
  end
  
  module Response : sig
    type t
    
    (** Construct a value
        @param latency_mode User can select live latency mode if enabled by the instance
        @param permanent_live User can stream multiple times in a permanent live
        @param rtmp_url Included in the response if an appropriate token is provided
        @param rtmps_url Included in the response if an appropriate token is provided
        @param stream_key RTMP stream key to use to stream into this live video. Included in the response if an appropriate token is provided
    *)
    val v : ?latency_mode:LiveVideoLatencyMode.T.t -> ?permanent_live:bool -> ?replay_settings:LiveVideoReplaySettings.T.t -> ?rtmp_url:string -> ?rtmps_url:string -> ?save_replay:bool -> ?schedules:LiveSchedule.T.t list -> ?stream_key:string -> unit -> t
    
    (** User can select live latency mode if enabled by the instance *)
    val latency_mode : t -> LiveVideoLatencyMode.T.t option
    
    (** User can stream multiple times in a permanent live *)
    val permanent_live : t -> bool option
    
    val replay_settings : t -> LiveVideoReplaySettings.T.t option
    
    (** Included in the response if an appropriate token is provided *)
    val rtmp_url : t -> string option
    
    (** Included in the response if an appropriate token is provided *)
    val rtmps_url : t -> string option
    
    val save_replay : t -> bool option
    
    val schedules : t -> LiveSchedule.T.t list option
    
    (** RTMP stream key to use to stream into this live video. Included in the response if an appropriate token is provided *)
    val stream_key : t -> string option
    
    val jsont : t Jsont.t
  end
  
  (** Get information about a live 
      @param id The object id, uuid or short uuid
  *)
  val get_live_id : id:string -> t -> unit -> Response.t
end

module ImportVideosInChannel : sig
  module Create : sig
    type t
    
    (** Construct a value
        @param video_channel_sync_id If part of a channel sync process, specify its id to assign video imports to this channel synchronization
    *)
    val v : external_channel_url:string -> ?video_channel_sync_id:int -> unit -> t
    
    val external_channel_url : t -> string
    
    (** If part of a channel sync process, specify its id to assign video imports to this channel synchronization *)
    val video_channel_sync_id : t -> int option
    
    val jsont : t Jsont.t
  end
end

module Id : sig
  module T : sig
    type t = Jsont.json
    
    val jsont : t Jsont.t
    
    val v : unit -> t
  end
end

module WatchedWordsLists : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?created_at:Ptime.t -> ?id:Id.T.t -> ?list_name:string -> ?updated_at:Ptime.t -> ?words:string list -> unit -> t
    
    val created_at : t -> Ptime.t option
    
    val id : t -> Id.T.t option
    
    val list_name : t -> string option
    
    val updated_at : t -> Ptime.t option
    
    val words : t -> string list option
    
    val jsont : t Jsont.t
  end
end

module VideoRedundancy : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?id:Id.T.t -> ?name:string -> ?redundancies:Jsont.json -> ?url:string -> ?uuid:Uuidv4.T.t -> unit -> t
    
    val id : t -> Id.T.t option
    
    val name : t -> string option
    
    val redundancies : t -> Jsont.json option
    
    val url : t -> string option
    
    val uuid : t -> Uuidv4.T.t option
    
    val jsont : t Jsont.t
  end
  
  (** List videos being mirrored 
      @param target direction of the mirror
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort abuses by criteria
  *)
  val get_mirrored_videos : target:string -> ?start:string -> ?count:string -> ?sort:string -> t -> unit -> T.t
end

module VideoPassword : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?id:Id.T.t -> ?password:string -> ?video_id:Id.T.t -> unit -> t
    
    val id : t -> Id.T.t option
    
    val password : t -> string option
    
    val video_id : t -> Id.T.t option
    
    val jsont : t Jsont.t
  end
end

module VideoPasswordList : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?data:VideoPassword.T.t list -> ?total:int -> unit -> t
    
    val data : t -> VideoPassword.T.t list option
    
    val total : t -> int option
    
    val jsont : t Jsont.t
  end
end

module VideoBlacklist : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?created_at:Ptime.t -> ?description:string -> ?dislikes:int -> ?duration:int -> ?id:Id.T.t -> ?likes:int -> ?name:string -> ?nsfw:bool -> ?updated_at:Ptime.t -> ?uuid:Uuidv4.T.t -> ?video_id:Jsont.json -> ?views:int -> unit -> t
    
    val created_at : t -> Ptime.t option
    
    val description : t -> string option
    
    val dislikes : t -> int option
    
    val duration : t -> int option
    
    val id : t -> Id.T.t option
    
    val likes : t -> int option
    
    val name : t -> string option
    
    val nsfw : t -> bool option
    
    val updated_at : t -> Ptime.t option
    
    val uuid : t -> Uuidv4.T.t option
    
    val video_id : t -> Jsont.json option
    
    val views : t -> int option
    
    val jsont : t Jsont.t
  end
end

module UserRegistration : sig
  module Request : sig
    type t
    
    (** Construct a value
        @param email email of the user, used for login or service communications
        @param username immutable name of the user, used to find or mention its actor
        @param registration_reason reason for the user to register on the instance
        @param channel channel base information used to create the first channel of the user
        @param display_name editable name of the user, displayed in its representations
    *)
    val v : email:string -> password:Password.T.t -> username:Username.T.t -> registration_reason:string -> ?channel:Jsont.json -> ?display_name:string -> unit -> t
    
    (** channel base information used to create the first channel of the user *)
    val channel : t -> Jsont.json option
    
    (** editable name of the user, displayed in its representations *)
    val display_name : t -> string option
    
    (** email of the user, used for login or service communications *)
    val email : t -> string
    
    val password : t -> Password.T.t
    
    (** immutable name of the user, used to find or mention its actor *)
    val username : t -> Username.T.t
    
    (** reason for the user to register on the instance *)
    val registration_reason : t -> string
    
    val jsont : t Jsont.t
  end
  
  module T : sig
    type t
    
    (** Construct a value
        @param user If the registration has been accepted, this is a partial user object created by the registration
    *)
    val v : ?account_display_name:string -> ?channel_display_name:string -> ?channel_handle:string -> ?created_at:Ptime.t -> ?email:string -> ?email_verified:bool -> ?id:Id.T.t -> ?moderation_response:string -> ?registration_reason:string -> ?state:Jsont.json -> ?updated_at:Ptime.t -> ?user:Jsont.json -> ?username:string -> unit -> t
    
    val account_display_name : t -> string option
    
    val channel_display_name : t -> string option
    
    val channel_handle : t -> string option
    
    val created_at : t -> Ptime.t option
    
    val email : t -> string option
    
    val email_verified : t -> bool option
    
    val id : t -> Id.T.t option
    
    val moderation_response : t -> string option
    
    val registration_reason : t -> string option
    
    val state : t -> Jsont.json option
    
    val updated_at : t -> Ptime.t option
    
    (** If the registration has been accepted, this is a partial user object created by the registration *)
    val user : t -> Jsont.json option
    
    val username : t -> string option
    
    val jsont : t Jsont.t
  end
  
  (** Request registration
  
      Signup has to be enabled and require approval on the instance *)
  val request_registration : body:Request.t -> t -> unit -> T.t
end

module Job : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?created_at:Ptime.t -> ?data:Jsont.json -> ?error:Jsont.json -> ?finished_on:Ptime.t -> ?id:Id.T.t -> ?processed_on:Ptime.t -> ?state:string -> ?type_:string -> unit -> t
    
    val created_at : t -> Ptime.t option
    
    val data : t -> Jsont.json option
    
    val error : t -> Jsont.json option
    
    val finished_on : t -> Ptime.t option
    
    val id : t -> Id.T.t option
    
    val processed_on : t -> Ptime.t option
    
    val state : t -> string option
    
    val type_ : t -> string option
    
    val jsont : t Jsont.t
  end
end

module GetMeVideoRating : sig
  module T : sig
    type t
    
    (** Construct a value
        @param rating Rating of the video
    *)
    val v : id:Id.T.t -> rating:string -> unit -> t
    
    val id : t -> Id.T.t
    
    (** Rating of the video *)
    val rating : t -> string
    
    val jsont : t Jsont.t
  end
  
  (** Get rate of my user for a video 
      @param video_id The video id
  *)
  val get_api_v1_users_me_videos_rating : video_id:string -> t -> unit -> T.t
end

module FileRedundancyInformation : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?created_at:Ptime.t -> ?expires_on:Ptime.t -> ?file_url:string -> ?id:Id.T.t -> ?size:int -> ?strategy:string -> ?updated_at:Ptime.t -> unit -> t
    
    val created_at : t -> Ptime.t option
    
    val expires_on : t -> Ptime.t option
    
    val file_url : t -> string option
    
    val id : t -> Id.T.t option
    
    val size : t -> int option
    
    val strategy : t -> string option
    
    val updated_at : t -> Ptime.t option
    
    val jsont : t Jsont.t
  end
end

module FileStorage : sig
  module T : sig
    (** The file storage type:
      - `0` File system
      - `1` Object storage
     *)
    type t = string
    
    val jsont : t Jsont.t
  end
end

module VideoFile : sig
  module T : sig
    type t
    
    (** Construct a value
        @param file_download_url URL endpoint that transfers the video file as an attachment (so that the browser opens a download dialog)
        @param file_url Direct URL of the video
        @param fps Frames per second of the video file
        @param has_audio **PeerTube >= 6.2** The file container has an audio stream
        @param has_video **PeerTube >= 6.2** The file container has a video stream
        @param height **PeerTube >= 6.1** Video stream height
        @param magnet_uri magnet URI allowing to resolve the video via BitTorrent without a metainfo file
        @param metadata_url URL dereferencing the output of ffprobe on the file
        @param playlist_url Playlist URL of the file if it is owned by a playlist
        @param size Video file size in bytes
        @param torrent_download_url URL endpoint that transfers the torrent file as an attachment (so that the browser opens a download dialog)
        @param torrent_url Direct URL of the torrent file
        @param width **PeerTube >= 6.1** Video stream width
    *)
    val v : ?file_download_url:string -> ?file_url:string -> ?fps:float -> ?has_audio:bool -> ?has_video:bool -> ?height:float -> ?id:Id.T.t -> ?magnet_uri:string -> ?metadata_url:string -> ?playlist_url:string -> ?resolution:VideoResolutionConstant.T.t -> ?size:int -> ?storage:FileStorage.T.t -> ?torrent_download_url:string -> ?torrent_url:string -> ?width:float -> unit -> t
    
    (** URL endpoint that transfers the video file as an attachment (so that the browser opens a download dialog) *)
    val file_download_url : t -> string option
    
    (** Direct URL of the video *)
    val file_url : t -> string option
    
    (** Frames per second of the video file *)
    val fps : t -> float option
    
    (** **PeerTube >= 6.2** The file container has an audio stream *)
    val has_audio : t -> bool option
    
    (** **PeerTube >= 6.2** The file container has a video stream *)
    val has_video : t -> bool option
    
    (** **PeerTube >= 6.1** Video stream height *)
    val height : t -> float option
    
    val id : t -> Id.T.t option
    
    (** magnet URI allowing to resolve the video via BitTorrent without a metainfo file *)
    val magnet_uri : t -> string option
    
    (** URL dereferencing the output of ffprobe on the file *)
    val metadata_url : t -> string option
    
    (** Playlist URL of the file if it is owned by a playlist *)
    val playlist_url : t -> string option
    
    val resolution : t -> VideoResolutionConstant.T.t option
    
    (** Video file size in bytes *)
    val size : t -> int option
    
    val storage : t -> FileStorage.T.t option
    
    (** URL endpoint that transfers the torrent file as an attachment (so that the browser opens a download dialog) *)
    val torrent_download_url : t -> string option
    
    (** Direct URL of the torrent file *)
    val torrent_url : t -> string option
    
    (** **PeerTube >= 6.1** Video stream width *)
    val width : t -> float option
    
    val jsont : t Jsont.t
  end
end

module VideoStreamingPlaylistsHls : sig
  module T : sig
    type t
    
    (** Construct a value
        @param files Video files associated to this playlist.
    
    The difference with the root `files` property is that these files are fragmented, so they can be used in this streaming playlist (HLS, etc.)
    
    *)
    val v : ?files:VideoFile.T.t list -> ?playlist_url:string -> ?redundancies:Jsont.json list -> ?segments_sha256_url:string -> unit -> t
    
    (** Video files associated to this playlist.
    
    The difference with the root `files` property is that these files are fragmented, so they can be used in this streaming playlist (HLS, etc.)
     *)
    val files : t -> VideoFile.T.t list option
    
    val playlist_url : t -> string option
    
    val redundancies : t -> Jsont.json list option
    
    val segments_sha256_url : t -> string option
    
    val jsont : t Jsont.t
  end
end

module VideoStreamingPlaylists : sig
  module T : sig
    type t
    
    (** Construct a value
        @param type_ Playlist type:
    - `1`: HLS
    
        @param files Video files associated to this playlist.
    
    The difference with the root `files` property is that these files are fragmented, so they can be used in this streaming playlist (HLS, etc.)
    
    *)
    val v : ?id:Id.T.t -> ?type_:int -> ?files:VideoFile.T.t list -> ?playlist_url:string -> ?redundancies:Jsont.json list -> ?segments_sha256_url:string -> unit -> t
    
    val id : t -> Id.T.t option
    
    (** Playlist type:
    - `1`: HLS
     *)
    val type_ : t -> int option
    
    (** Video files associated to this playlist.
    
    The difference with the root `files` property is that these files are fragmented, so they can be used in this streaming playlist (HLS, etc.)
     *)
    val files : t -> VideoFile.T.t list option
    
    val playlist_url : t -> string option
    
    val redundancies : t -> Jsont.json list option
    
    val segments_sha256_url : t -> string option
    
    val jsont : t Jsont.t
  end
end

module CustomHomepage : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?content:string -> unit -> t
    
    val content : t -> string option
    
    val jsont : t Jsont.t
  end
  
  (** Get instance custom homepage *)
  val get_api_v1_custom_pages_homepage_instance : t -> unit -> T.t
end

module CommentAutoTagPolicies : sig
  module T : sig
    type t
    
    (** Construct a value
        @param review Auto tags that automatically set the comment in review state
    *)
    val v : ?review:string list -> unit -> t
    
    (** Auto tags that automatically set the comment in review state *)
    val review : t -> string list option
    
    val jsont : t Jsont.t
  end
  
  (** Get account auto tag policies on comments
  
      **PeerTube >= 6.2** 
      @param account_name account name to get auto tag policies
  *)
  val get_api_v1_automatic_tags_policies_accounts_comments : account_name:string -> t -> unit -> T.t
end

module ChannelActivityList : sig
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : ?data:Jsont.json list -> ?total:int -> unit -> t
    
    val data : t -> Jsont.json list option
    
    val total : t -> int option
    
    val jsont : t Jsont.t
  end
  
  (** List activities of a video channel
  
      **PeerTube >= 8.0** 
      @param channel_handle The video channel handle
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  val list_video_channel_activities : channel_handle:string -> ?start:string -> ?count:string -> ?sort:string -> t -> unit -> Response.t
end

module Block : sig
  module Status : sig
    type t
    
    (** Construct a value *)
    val v : ?accounts:Jsont.json -> ?hosts:Jsont.json -> unit -> t
    
    val accounts : t -> Jsont.json option
    
    val hosts : t -> Jsont.json option
    
    val jsont : t Jsont.t
  end
  
  (** Get block status of accounts/hosts 
      @param accounts Check if these accounts are blocked
      @param hosts Check if these hosts are blocked
  *)
  val get_api_v1_blocklist_status : ?accounts:string -> ?hosts:string -> t -> unit -> Status.t
end

module AutomaticTagAvailable : sig
  module T : sig
    type t
    
    (** Construct a value
        @param available Available auto tags that can be used to filter objects or set a comment in review state
    *)
    val v : ?available:Jsont.json list -> unit -> t
    
    (** Available auto tags that can be used to filter objects or set a comment in review state *)
    val available : t -> Jsont.json list option
    
    val jsont : t Jsont.t
  end
  
  (** Get account available auto tags
  
      **PeerTube >= 6.2** 
      @param account_name account name to get auto tag policies
  *)
  val get_api_v1_automatic_tags_accounts_available : account_name:string -> t -> unit -> T.t
  
  (** Get server available auto tags
  
      **PeerTube >= 6.2** *)
  val get_api_v1_automatic_tags_server_available : t -> unit -> T.t
end

module AddVideoPasswords : sig
  module T : sig
    type t = Jsont.json
    
    val jsont : t Jsont.t
    
    val v : unit -> t
  end
end

module VideoUploadRequestResumable : sig
  module T : sig
    type t
    
    (** Construct a value
        @param channel_id Channel id that will contain this video
        @param name Video name
        @param filename Video filename including extension
        @param description Video description
        @param download_enabled Enable or disable downloading for this video
        @param generate_transcription **PeerTube >= 6.2** If enabled by the admin, automatically generate a subtitle of the video
        @param nsfw Whether or not this video contains sensitive content
        @param nsfw_summary More information about the sensitive content of the video
        @param originally_published_at Date when the content was originally published
        @param support A text tell the audience how to support the video creator
        @param tags Video tags (maximum 5 tags each between 2 and 30 characters)
        @param wait_transcoding Whether or not we wait transcoding before publish the video
        @param thumbnailfile Video thumbnail file
        @param previewfile Video preview file
    *)
    val v : channel_id:int -> name:string -> filename:string -> ?category:VideoCategorySet.T.t -> ?comments_policy:VideoCommentsPolicySet.T.t -> ?description:string -> ?download_enabled:bool -> ?generate_transcription:bool -> ?language:VideoLanguageSet.T.t -> ?licence:VideoLicenceSet.T.t -> ?nsfw:bool -> ?nsfw_flags:Nsfwflag.T.t -> ?nsfw_summary:Jsont.json -> ?originally_published_at:Ptime.t -> ?privacy:VideoPrivacySet.T.t -> ?schedule_update:VideoScheduled.Update.t -> ?support:string -> ?tags:string list -> ?video_passwords:AddVideoPasswords.T.t -> ?wait_transcoding:bool -> ?thumbnailfile:string -> ?previewfile:string -> unit -> t
    
    val category : t -> VideoCategorySet.T.t option
    
    (** Channel id that will contain this video *)
    val channel_id : t -> int
    
    val comments_policy : t -> VideoCommentsPolicySet.T.t option
    
    (** Video description *)
    val description : t -> string option
    
    (** Enable or disable downloading for this video *)
    val download_enabled : t -> bool option
    
    (** **PeerTube >= 6.2** If enabled by the admin, automatically generate a subtitle of the video *)
    val generate_transcription : t -> bool option
    
    val language : t -> VideoLanguageSet.T.t option
    
    val licence : t -> VideoLicenceSet.T.t option
    
    (** Video name *)
    val name : t -> string
    
    (** Whether or not this video contains sensitive content *)
    val nsfw : t -> bool option
    
    val nsfw_flags : t -> Nsfwflag.T.t option
    
    (** More information about the sensitive content of the video *)
    val nsfw_summary : t -> Jsont.json option
    
    (** Date when the content was originally published *)
    val originally_published_at : t -> Ptime.t option
    
    val privacy : t -> VideoPrivacySet.T.t option
    
    val schedule_update : t -> VideoScheduled.Update.t option
    
    (** A text tell the audience how to support the video creator *)
    val support : t -> string option
    
    (** Video tags (maximum 5 tags each between 2 and 30 characters) *)
    val tags : t -> string list option
    
    val video_passwords : t -> AddVideoPasswords.T.t option
    
    (** Whether or not we wait transcoding before publish the video *)
    val wait_transcoding : t -> bool option
    
    (** Video filename including extension *)
    val filename : t -> string
    
    (** Video thumbnail file *)
    val thumbnailfile : t -> string option
    
    (** Video preview file *)
    val previewfile : t -> string option
    
    val jsont : t Jsont.t
  end
end

module VideoUploadRequestLegacy : sig
  module T : sig
    type t
    
    (** Construct a value
        @param channel_id Channel id that will contain this video
        @param name Video name
        @param videofile Video file
        @param description Video description
        @param download_enabled Enable or disable downloading for this video
        @param generate_transcription **PeerTube >= 6.2** If enabled by the admin, automatically generate a subtitle of the video
        @param nsfw Whether or not this video contains sensitive content
        @param nsfw_summary More information about the sensitive content of the video
        @param originally_published_at Date when the content was originally published
        @param previewfile Video preview file
        @param support A text tell the audience how to support the video creator
        @param tags Video tags (maximum 5 tags each between 2 and 30 characters)
        @param thumbnailfile Video thumbnail file
        @param wait_transcoding Whether or not we wait transcoding before publish the video
    *)
    val v : channel_id:int -> name:string -> videofile:string -> ?category:VideoCategorySet.T.t -> ?comments_policy:VideoCommentsPolicySet.T.t -> ?description:string -> ?download_enabled:bool -> ?generate_transcription:bool -> ?language:VideoLanguageSet.T.t -> ?licence:VideoLicenceSet.T.t -> ?nsfw:bool -> ?nsfw_flags:Nsfwflag.T.t -> ?nsfw_summary:Jsont.json -> ?originally_published_at:Ptime.t -> ?previewfile:string -> ?privacy:VideoPrivacySet.T.t -> ?schedule_update:VideoScheduled.Update.t -> ?support:string -> ?tags:string list -> ?thumbnailfile:string -> ?video_passwords:AddVideoPasswords.T.t -> ?wait_transcoding:bool -> unit -> t
    
    val category : t -> VideoCategorySet.T.t option
    
    (** Channel id that will contain this video *)
    val channel_id : t -> int
    
    val comments_policy : t -> VideoCommentsPolicySet.T.t option
    
    (** Video description *)
    val description : t -> string option
    
    (** Enable or disable downloading for this video *)
    val download_enabled : t -> bool option
    
    (** **PeerTube >= 6.2** If enabled by the admin, automatically generate a subtitle of the video *)
    val generate_transcription : t -> bool option
    
    val language : t -> VideoLanguageSet.T.t option
    
    val licence : t -> VideoLicenceSet.T.t option
    
    (** Video name *)
    val name : t -> string
    
    (** Whether or not this video contains sensitive content *)
    val nsfw : t -> bool option
    
    val nsfw_flags : t -> Nsfwflag.T.t option
    
    (** More information about the sensitive content of the video *)
    val nsfw_summary : t -> Jsont.json option
    
    (** Date when the content was originally published *)
    val originally_published_at : t -> Ptime.t option
    
    (** Video preview file *)
    val previewfile : t -> string option
    
    val privacy : t -> VideoPrivacySet.T.t option
    
    val schedule_update : t -> VideoScheduled.Update.t option
    
    (** A text tell the audience how to support the video creator *)
    val support : t -> string option
    
    (** Video tags (maximum 5 tags each between 2 and 30 characters) *)
    val tags : t -> string list option
    
    (** Video thumbnail file *)
    val thumbnailfile : t -> string option
    
    val video_passwords : t -> AddVideoPasswords.T.t option
    
    (** Whether or not we wait transcoding before publish the video *)
    val wait_transcoding : t -> bool option
    
    (** Video file *)
    val videofile : t -> string
    
    val jsont : t Jsont.t
  end
end

module VideoUploadRequestCommon : sig
  module T : sig
    type t
    
    (** Construct a value
        @param channel_id Channel id that will contain this video
        @param name Video name
        @param description Video description
        @param download_enabled Enable or disable downloading for this video
        @param generate_transcription **PeerTube >= 6.2** If enabled by the admin, automatically generate a subtitle of the video
        @param nsfw Whether or not this video contains sensitive content
        @param nsfw_summary More information about the sensitive content of the video
        @param originally_published_at Date when the content was originally published
        @param previewfile Video preview file
        @param support A text tell the audience how to support the video creator
        @param tags Video tags (maximum 5 tags each between 2 and 30 characters)
        @param thumbnailfile Video thumbnail file
        @param wait_transcoding Whether or not we wait transcoding before publish the video
    *)
    val v : channel_id:int -> name:string -> ?category:VideoCategorySet.T.t -> ?comments_policy:VideoCommentsPolicySet.T.t -> ?description:string -> ?download_enabled:bool -> ?generate_transcription:bool -> ?language:VideoLanguageSet.T.t -> ?licence:VideoLicenceSet.T.t -> ?nsfw:bool -> ?nsfw_flags:Nsfwflag.T.t -> ?nsfw_summary:Jsont.json -> ?originally_published_at:Ptime.t -> ?previewfile:string -> ?privacy:VideoPrivacySet.T.t -> ?schedule_update:VideoScheduled.Update.t -> ?support:string -> ?tags:string list -> ?thumbnailfile:string -> ?video_passwords:AddVideoPasswords.T.t -> ?wait_transcoding:bool -> unit -> t
    
    val category : t -> VideoCategorySet.T.t option
    
    (** Channel id that will contain this video *)
    val channel_id : t -> int
    
    val comments_policy : t -> VideoCommentsPolicySet.T.t option
    
    (** Video description *)
    val description : t -> string option
    
    (** Enable or disable downloading for this video *)
    val download_enabled : t -> bool option
    
    (** **PeerTube >= 6.2** If enabled by the admin, automatically generate a subtitle of the video *)
    val generate_transcription : t -> bool option
    
    val language : t -> VideoLanguageSet.T.t option
    
    val licence : t -> VideoLicenceSet.T.t option
    
    (** Video name *)
    val name : t -> string
    
    (** Whether or not this video contains sensitive content *)
    val nsfw : t -> bool option
    
    val nsfw_flags : t -> Nsfwflag.T.t option
    
    (** More information about the sensitive content of the video *)
    val nsfw_summary : t -> Jsont.json option
    
    (** Date when the content was originally published *)
    val originally_published_at : t -> Ptime.t option
    
    (** Video preview file *)
    val previewfile : t -> string option
    
    val privacy : t -> VideoPrivacySet.T.t option
    
    val schedule_update : t -> VideoScheduled.Update.t option
    
    (** A text tell the audience how to support the video creator *)
    val support : t -> string option
    
    (** Video tags (maximum 5 tags each between 2 and 30 characters) *)
    val tags : t -> string list option
    
    (** Video thumbnail file *)
    val thumbnailfile : t -> string option
    
    val video_passwords : t -> AddVideoPasswords.T.t option
    
    (** Whether or not we wait transcoding before publish the video *)
    val wait_transcoding : t -> bool option
    
    val jsont : t Jsont.t
  end
end

module VideoCreateImport : sig
  module T : sig
    type t
    
    (** Construct a value
        @param channel_id Channel id that will contain this video
        @param name Video name
        @param description Video description
        @param download_enabled Enable or disable downloading for this video
        @param generate_transcription **PeerTube >= 6.2** If enabled by the admin, automatically generate a subtitle of the video
        @param nsfw Whether or not this video contains sensitive content
        @param nsfw_summary More information about the sensitive content of the video
        @param originally_published_at Date when the content was originally published
        @param previewfile Video preview file
        @param support A text tell the audience how to support the video creator
        @param tags Video tags (maximum 5 tags each between 2 and 30 characters)
        @param thumbnailfile Video thumbnail file
        @param wait_transcoding Whether or not we wait transcoding before publish the video
    *)
    val v : channel_id:int -> name:string -> ?category:VideoCategorySet.T.t -> ?comments_policy:VideoCommentsPolicySet.T.t -> ?description:string -> ?download_enabled:bool -> ?generate_transcription:bool -> ?language:VideoLanguageSet.T.t -> ?licence:VideoLicenceSet.T.t -> ?nsfw:bool -> ?nsfw_flags:Nsfwflag.T.t -> ?nsfw_summary:Jsont.json -> ?originally_published_at:Ptime.t -> ?previewfile:string -> ?privacy:VideoPrivacySet.T.t -> ?schedule_update:VideoScheduled.Update.t -> ?support:string -> ?tags:string list -> ?thumbnailfile:string -> ?video_passwords:AddVideoPasswords.T.t -> ?wait_transcoding:bool -> unit -> t
    
    val category : t -> VideoCategorySet.T.t option
    
    (** Channel id that will contain this video *)
    val channel_id : t -> int
    
    val comments_policy : t -> VideoCommentsPolicySet.T.t option
    
    (** Video description *)
    val description : t -> string option
    
    (** Enable or disable downloading for this video *)
    val download_enabled : t -> bool option
    
    (** **PeerTube >= 6.2** If enabled by the admin, automatically generate a subtitle of the video *)
    val generate_transcription : t -> bool option
    
    val language : t -> VideoLanguageSet.T.t option
    
    val licence : t -> VideoLicenceSet.T.t option
    
    (** Video name *)
    val name : t -> string
    
    (** Whether or not this video contains sensitive content *)
    val nsfw : t -> bool option
    
    val nsfw_flags : t -> Nsfwflag.T.t option
    
    (** More information about the sensitive content of the video *)
    val nsfw_summary : t -> Jsont.json option
    
    (** Date when the content was originally published *)
    val originally_published_at : t -> Ptime.t option
    
    (** Video preview file *)
    val previewfile : t -> string option
    
    val privacy : t -> VideoPrivacySet.T.t option
    
    val schedule_update : t -> VideoScheduled.Update.t option
    
    (** A text tell the audience how to support the video creator *)
    val support : t -> string option
    
    (** Video tags (maximum 5 tags each between 2 and 30 characters) *)
    val tags : t -> string list option
    
    (** Video thumbnail file *)
    val thumbnailfile : t -> string option
    
    val video_passwords : t -> AddVideoPasswords.T.t option
    
    (** Whether or not we wait transcoding before publish the video *)
    val wait_transcoding : t -> bool option
    
    val jsont : t Jsont.t
  end
end

module ActorImage : sig
  module T : sig
    type t
    
    (** Construct a value
        @param file_url **PeerTube >= 7.1**
        @param height **PeerTube >= 7.3**
        @param path Deprecated in PeerTube v8.0, use fileUrl instead
    *)
    val v : ?created_at:Ptime.t -> ?file_url:string -> ?height:int -> ?path:string -> ?updated_at:Ptime.t -> ?width:int -> unit -> t
    
    val created_at : t -> Ptime.t option
    
    (** **PeerTube >= 7.1** *)
    val file_url : t -> string option
    
    (** **PeerTube >= 7.3** *)
    val height : t -> int option
    
    (** Deprecated in PeerTube v8.0, use fileUrl instead *)
    val path : t -> string option
    
    val updated_at : t -> Ptime.t option
    
    val width : t -> int option
    
    val jsont : t Jsont.t
  end
end

module VideoChannelSummary : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?avatars:ActorImage.T.t list -> ?display_name:string -> ?host:string -> ?id:Id.T.t -> ?name:string -> ?url:string -> unit -> t
    
    val avatars : t -> ActorImage.T.t list option
    
    val display_name : t -> string option
    
    val host : t -> string option
    
    val id : t -> Id.T.t option
    
    val name : t -> string option
    
    val url : t -> string option
    
    val jsont : t Jsont.t
  end
end

module Actor : sig
  module Info : sig
    type t
    
    (** Construct a value *)
    val v : ?avatars:ActorImage.T.t list -> ?display_name:string -> ?host:string -> ?id:Id.T.t -> ?name:string -> unit -> t
    
    val avatars : t -> ActorImage.T.t list option
    
    val display_name : t -> string option
    
    val host : t -> string option
    
    val id : t -> Id.T.t option
    
    val name : t -> string option
    
    val jsont : t Jsont.t
  end
  
  module T : sig
    type t
    
    (** Construct a value
        @param followers_count number of followers of this actor, as seen by this instance
        @param following_count number of actors subscribed to by this actor, as seen by this instance
        @param host server on which the actor is resident
        @param host_redundancy_allowed whether this actor's host allows redundancy of its videos
        @param name immutable name of the actor, used to find or mention it
    *)
    val v : ?avatars:ActorImage.T.t list -> ?created_at:Ptime.t -> ?followers_count:int -> ?following_count:int -> ?host:string -> ?host_redundancy_allowed:bool -> ?id:Id.T.t -> ?name:Username.T.t -> ?updated_at:Ptime.t -> ?url:string -> unit -> t
    
    val avatars : t -> ActorImage.T.t list option
    
    val created_at : t -> Ptime.t option
    
    (** number of followers of this actor, as seen by this instance *)
    val followers_count : t -> int option
    
    (** number of actors subscribed to by this actor, as seen by this instance *)
    val following_count : t -> int option
    
    (** server on which the actor is resident *)
    val host : t -> string option
    
    (** whether this actor's host allows redundancy of its videos *)
    val host_redundancy_allowed : t -> bool option
    
    val id : t -> Id.T.t option
    
    (** immutable name of the actor, used to find or mention it *)
    val name : t -> Username.T.t option
    
    val updated_at : t -> Ptime.t option
    
    val url : t -> string option
    
    val jsont : t Jsont.t
  end
end

module Follow : sig
  module T : sig
    type t
    
    (** Construct a value
        @param score score reflecting the reachability of the actor, with steps of `10` and a base score of `1000`.
    *)
    val v : ?created_at:Ptime.t -> ?follower:Actor.T.t -> ?following:Actor.T.t -> ?id:Id.T.t -> ?score:float -> ?state:string -> ?updated_at:Ptime.t -> unit -> t
    
    val created_at : t -> Ptime.t option
    
    val follower : t -> Actor.T.t option
    
    val following : t -> Actor.T.t option
    
    val id : t -> Id.T.t option
    
    (** score reflecting the reachability of the actor, with steps of `10` and a base score of `1000`. *)
    val score : t -> float option
    
    val state : t -> string option
    
    val updated_at : t -> Ptime.t option
    
    val jsont : t Jsont.t
  end
end

module AccountSummary : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?avatars:ActorImage.T.t list -> ?display_name:string -> ?host:string -> ?id:int -> ?name:string -> ?url:string -> unit -> t
    
    val avatars : t -> ActorImage.T.t list option
    
    val display_name : t -> string option
    
    val host : t -> string option
    
    val id : t -> int option
    
    val name : t -> string option
    
    val url : t -> string option
    
    val jsont : t Jsont.t
  end
end

module VideoPlaylist : sig
  module T : sig
    type t
    
    (** Construct a value
        @param video_channel_position Position of the playlist in the channel
    *)
    val v : ?created_at:Ptime.t -> ?description:string -> ?display_name:string -> ?id:Id.T.t -> ?is_local:bool -> ?owner_account:AccountSummary.T.t -> ?privacy:VideoPlaylistPrivacyConstant.T.t -> ?short_uuid:ShortUuid.T.t -> ?thumbnail_path:string -> ?type_:VideoPlaylistTypeConstant.T.t -> ?updated_at:Ptime.t -> ?uuid:Uuidv4.T.t -> ?video_channel:VideoChannelSummary.T.t -> ?video_channel_position:int -> ?video_length:int -> unit -> t
    
    val created_at : t -> Ptime.t option
    
    val description : t -> string option
    
    val display_name : t -> string option
    
    val id : t -> Id.T.t option
    
    val is_local : t -> bool option
    
    val owner_account : t -> AccountSummary.T.t option
    
    val privacy : t -> VideoPlaylistPrivacyConstant.T.t option
    
    val short_uuid : t -> ShortUuid.T.t option
    
    val thumbnail_path : t -> string option
    
    val type_ : t -> VideoPlaylistTypeConstant.T.t option
    
    val updated_at : t -> Ptime.t option
    
    val uuid : t -> Uuidv4.T.t option
    
    val video_channel : t -> VideoChannelSummary.T.t option
    
    (** Position of the playlist in the channel *)
    val video_channel_position : t -> int option
    
    val video_length : t -> int option
    
    val jsont : t Jsont.t
  end
  
  (** Get a video playlist 
      @param playlist_id Playlist id
  *)
  val get_api_v1_video_playlists : playlist_id:string -> t -> unit -> T.t
end

module VideoChannelCollaborator : sig
  module T : sig
    (** Representation of a channel collaboration *)
    type t
    
    (** Construct a value *)
    val v : ?account:AccountSummary.T.t -> ?created_at:Ptime.t -> ?id:Id.T.t -> ?state:Jsont.json -> ?updated_at:Ptime.t -> unit -> t
    
    val account : t -> AccountSummary.T.t option
    
    val created_at : t -> Ptime.t option
    
    val id : t -> Id.T.t option
    
    val state : t -> Jsont.json option
    
    val updated_at : t -> Ptime.t option
    
    val jsont : t Jsont.t
  end
end

module Video : sig
  module Info : sig
    type t
    
    (** Construct a value *)
    val v : ?id:Jsont.json -> ?name:Jsont.json -> ?state:Jsont.json -> ?uuid:Jsont.json -> unit -> t
    
    val id : t -> Jsont.json option
    
    val name : t -> Jsont.json option
    
    val state : t -> Jsont.json option
    
    val uuid : t -> Jsont.json option
    
    val jsont : t Jsont.t
  end
  
  module T : sig
    type t
    
    (** Construct a value
        @param aspect_ratio **PeerTube >= 6.1** Aspect ratio of the video stream
        @param category category in which the video is classified
        @param comments **PeerTube >= 7.2** Number of comments on the video
        @param created_at time at which the video object was first drafted
        @param duration duration of the video in seconds
        @param id object id for the video
        @param language main language used in the video
        @param licence licence under which the video is distributed
        @param name title of the video
        @param nsfw_summary **PeerTube >= 7.2** More information about the sensitive content of the video
        @param originally_published_at used to represent a date of first publication, prior to the practical publication date of `publishedAt`
        @param privacy privacy policy used to distribute the video
        @param published_at time at which the video was marked as ready for playback (with restrictions depending on `privacy`). Usually set after a `state` evolution.
        @param state represents the internal state of the video processing within the PeerTube instance
        @param truncated_description truncated description of the video, written in Markdown.
    
        @param updated_at last time the video's metadata was modified
        @param uuid universal identifier for the video, that can be used across instances
    *)
    val v : ?account:AccountSummary.T.t -> ?aspect_ratio:float -> ?blacklisted:bool -> ?blacklisted_reason:string -> ?category:VideoConstantNumberCategory.T.t -> ?channel:VideoChannelSummary.T.t -> ?comments:int -> ?created_at:Ptime.t -> ?dislikes:int -> ?duration:int -> ?embed_path:string -> ?id:Id.T.t -> ?is_live:bool -> ?is_local:bool -> ?language:VideoConstantStringLanguage.T.t -> ?licence:VideoConstantNumberLicence.T.t -> ?likes:int -> ?live_schedules:LiveSchedule.T.t list -> ?name:string -> ?nsfw:bool -> ?nsfw_flags:Nsfwflag.T.t -> ?nsfw_summary:string -> ?originally_published_at:Ptime.t -> ?preview_path:string -> ?privacy:VideoPrivacyConstant.T.t -> ?published_at:Ptime.t -> ?scheduled_update:VideoScheduled.Update.t -> ?short_uuid:ShortUuid.T.t -> ?state:VideoStateConstant.T.t -> ?thumbnail_path:string -> ?truncated_description:string -> ?updated_at:Ptime.t -> ?user_history:Jsont.json -> ?uuid:Uuidv4.T.t -> ?views:int -> ?wait_transcoding:bool -> unit -> t
    
    val account : t -> AccountSummary.T.t option
    
    (** **PeerTube >= 6.1** Aspect ratio of the video stream *)
    val aspect_ratio : t -> float option
    
    val blacklisted : t -> bool option
    
    val blacklisted_reason : t -> string option
    
    (** category in which the video is classified *)
    val category : t -> VideoConstantNumberCategory.T.t option
    
    val channel : t -> VideoChannelSummary.T.t option
    
    (** **PeerTube >= 7.2** Number of comments on the video *)
    val comments : t -> int option
    
    (** time at which the video object was first drafted *)
    val created_at : t -> Ptime.t option
    
    val dislikes : t -> int option
    
    (** duration of the video in seconds *)
    val duration : t -> int option
    
    val embed_path : t -> string option
    
    (** object id for the video *)
    val id : t -> Id.T.t option
    
    val is_live : t -> bool option
    
    val is_local : t -> bool option
    
    (** main language used in the video *)
    val language : t -> VideoConstantStringLanguage.T.t option
    
    (** licence under which the video is distributed *)
    val licence : t -> VideoConstantNumberLicence.T.t option
    
    val likes : t -> int option
    
    val live_schedules : t -> LiveSchedule.T.t list option
    
    (** title of the video *)
    val name : t -> string option
    
    val nsfw : t -> bool option
    
    val nsfw_flags : t -> Nsfwflag.T.t option
    
    (** **PeerTube >= 7.2** More information about the sensitive content of the video *)
    val nsfw_summary : t -> string option
    
    (** used to represent a date of first publication, prior to the practical publication date of `publishedAt` *)
    val originally_published_at : t -> Ptime.t option
    
    val preview_path : t -> string option
    
    (** privacy policy used to distribute the video *)
    val privacy : t -> VideoPrivacyConstant.T.t option
    
    (** time at which the video was marked as ready for playback (with restrictions depending on `privacy`). Usually set after a `state` evolution. *)
    val published_at : t -> Ptime.t option
    
    val scheduled_update : t -> VideoScheduled.Update.t option
    
    val short_uuid : t -> ShortUuid.T.t option
    
    (** represents the internal state of the video processing within the PeerTube instance *)
    val state : t -> VideoStateConstant.T.t option
    
    val thumbnail_path : t -> string option
    
    (** truncated description of the video, written in Markdown.
     *)
    val truncated_description : t -> string option
    
    (** last time the video's metadata was modified *)
    val updated_at : t -> Ptime.t option
    
    val user_history : t -> Jsont.json option
    
    (** universal identifier for the video, that can be used across instances *)
    val uuid : t -> Uuidv4.T.t option
    
    val views : t -> int option
    
    val wait_transcoding : t -> bool option
    
    val jsont : t Jsont.t
  end
end

module VideoRating : sig
  module T : sig
    type t
    
    (** Construct a value
        @param rating Rating of the video
    *)
    val v : rating:string -> video:Video.T.t -> unit -> t
    
    (** Rating of the video *)
    val rating : t -> string
    
    val video : t -> Video.T.t
    
    val jsont : t Jsont.t
  end
  
  (** List ratings of an account 
      @param name The username or handle of the account
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
      @param rating Optionally filter which ratings to retrieve
  *)
  val get_api_v1_accounts_ratings : name:string -> ?start:string -> ?count:string -> ?sort:string -> ?rating:string -> t -> unit -> T.t
end

module VideoList : sig
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : ?data:Video.T.t list -> ?total:int -> unit -> t
    
    val data : t -> Video.T.t list option
    
    val total : t -> int option
    
    val jsont : t Jsont.t
  end
  
  (** List videos of an account 
      @param name The username or handle of the account
      @param start Offset used to paginate results
      @param count Number of items to return
      @param skip_count if you don't need the `total` in the response
      @param nsfw whether to include nsfw videos, if any
      @param is_live whether or not the video is a live
      @param include_scheduled_live whether or not include live that are scheduled for later
      @param category_one_of category id of the video (see [/videos/categories](#operation/getCategories))
      @param licence_one_of licence id of the video (see [/videos/licences](#operation/getLicences))
      @param language_one_of language id of the video (see [/videos/languages](#operation/getLanguages)). Use `_unknown` to filter on videos that don't have a video language
      @param tags_one_of tag(s) of the video
      @param tags_all_of tag(s) of the video, where all should be present in the video
      @param is_local **PeerTube >= 4.0** Display only local or remote objects
      @param include_ **Only administrators and moderators can use this parameter**
  
  Include additional videos in results (can be combined using bitwise or operator)
  - `0` NONE
  - `1` NOT_PUBLISHED_STATE
  - `2` BLACKLISTED
  - `4` BLOCKED_OWNER
  - `8` FILES
  - `16` CAPTIONS
  - `32` VIDEO SOURCE
  
      @param has_hlsfiles **PeerTube >= 4.0** Display only videos that have HLS files
      @param has_web_video_files **PeerTube >= 6.0** Display only videos that have Web Video files
      @param host Find elements owned by this host
      @param auto_tag_one_of **PeerTube >= 6.2** **Admins and moderators only** filter on videos that contain one of these automatic tags
      @param privacy_one_of **PeerTube >= 4.0** Display only videos in this specific privacy/privacies
      @param exclude_already_watched Whether or not to exclude videos that are in the user's video history
      @param search Plain text search, applied to various parts of the model depending on endpoint
  *)
  val get_account_videos : name:string -> ?start:string -> ?count:string -> ?skip_count:string -> ?sort:string -> ?nsfw:string -> ?nsfw_flags_included:string -> ?nsfw_flags_excluded:string -> ?is_live:string -> ?include_scheduled_live:string -> ?category_one_of:string -> ?licence_one_of:string -> ?language_one_of:string -> ?tags_one_of:string -> ?tags_all_of:string -> ?is_local:string -> ?include_:string -> ?has_hlsfiles:string -> ?has_web_video_files:string -> ?host:string -> ?auto_tag_one_of:string -> ?privacy_one_of:string -> ?exclude_already_watched:string -> ?search:string -> t -> unit -> Response.t
  
  (** Search videos 
      @param search String to search. If the user can make a remote URI search, and the string is an URI then the PeerTube instance will fetch the remote object and add it to its database. Then, you can use the REST API to fetch the complete video information and interact with it.
  
      @param uuids Find elements with specific UUIDs
      @param search_target If the administrator enabled search index support, you can override the default search target.
  
  **Warning**: If you choose to make an index search, PeerTube will get results from a third party service. It means the instance may not yet know the objects you fetched. If you want to load video/channel information:
    * If the current user has the ability to make a remote URI search (this information is available in the config endpoint),
    then reuse the search API to make a search using the object URI so PeerTube instance fetches the remote object and fill its database.
    After that, you can use the classic REST API endpoints to fetch the complete object or interact with it
    * If the current user doesn't have the ability to make a remote URI search, then redirect the user on the origin instance or fetch
    the data from the origin instance API
  
      @param start Offset used to paginate results
      @param count Number of items to return
      @param skip_count if you don't need the `total` in the response
      @param nsfw whether to include nsfw videos, if any
      @param is_live whether or not the video is a live
      @param include_scheduled_live whether or not include live that are scheduled for later
      @param category_one_of category id of the video (see [/videos/categories](#operation/getCategories))
      @param licence_one_of licence id of the video (see [/videos/licences](#operation/getLicences))
      @param language_one_of language id of the video (see [/videos/languages](#operation/getLanguages)). Use `_unknown` to filter on videos that don't have a video language
      @param tags_one_of tag(s) of the video
      @param tags_all_of tag(s) of the video, where all should be present in the video
      @param is_local **PeerTube >= 4.0** Display only local or remote objects
      @param include_ **Only administrators and moderators can use this parameter**
  
  Include additional videos in results (can be combined using bitwise or operator)
  - `0` NONE
  - `1` NOT_PUBLISHED_STATE
  - `2` BLACKLISTED
  - `4` BLOCKED_OWNER
  - `8` FILES
  - `16` CAPTIONS
  - `32` VIDEO SOURCE
  
      @param has_hlsfiles **PeerTube >= 4.0** Display only videos that have HLS files
      @param has_web_video_files **PeerTube >= 6.0** Display only videos that have Web Video files
      @param host Find elements owned by this host
      @param auto_tag_one_of **PeerTube >= 6.2** **Admins and moderators only** filter on videos that contain one of these automatic tags
      @param privacy_one_of **PeerTube >= 4.0** Display only videos in this specific privacy/privacies
      @param exclude_already_watched Whether or not to exclude videos that are in the user's video history
      @param start_date Get videos that are published after this date
      @param end_date Get videos that are published before this date
      @param originally_published_start_date Get videos that are originally published after this date
      @param originally_published_end_date Get videos that are originally published before this date
      @param duration_min Get videos that have this minimum duration
      @param duration_max Get videos that have this maximum duration
  *)
  val search_videos : search:string -> ?uuids:string -> ?search_target:string -> ?start:string -> ?count:string -> ?skip_count:string -> ?sort:string -> ?nsfw:string -> ?nsfw_flags_included:string -> ?nsfw_flags_excluded:string -> ?is_live:string -> ?include_scheduled_live:string -> ?category_one_of:string -> ?licence_one_of:string -> ?language_one_of:string -> ?tags_one_of:string -> ?tags_all_of:string -> ?is_local:string -> ?include_:string -> ?has_hlsfiles:string -> ?has_web_video_files:string -> ?host:string -> ?auto_tag_one_of:string -> ?privacy_one_of:string -> ?exclude_already_watched:string -> ?start_date:string -> ?end_date:string -> ?originally_published_start_date:string -> ?originally_published_end_date:string -> ?duration_min:string -> ?duration_max:string -> t -> unit -> Response.t
  
  (** List watched videos history 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param search Plain text search, applied to various parts of the model depending on endpoint
  *)
  val get_api_v1_users_me_history_videos : ?start:string -> ?count:string -> ?search:string -> t -> unit -> Response.t
  
  (** List videos of subscriptions of my user 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param skip_count if you don't need the `total` in the response
      @param nsfw whether to include nsfw videos, if any
      @param is_live whether or not the video is a live
      @param include_scheduled_live whether or not include live that are scheduled for later
      @param category_one_of category id of the video (see [/videos/categories](#operation/getCategories))
      @param licence_one_of licence id of the video (see [/videos/licences](#operation/getLicences))
      @param language_one_of language id of the video (see [/videos/languages](#operation/getLanguages)). Use `_unknown` to filter on videos that don't have a video language
      @param tags_one_of tag(s) of the video
      @param tags_all_of tag(s) of the video, where all should be present in the video
      @param is_local **PeerTube >= 4.0** Display only local or remote objects
      @param include_ **Only administrators and moderators can use this parameter**
  
  Include additional videos in results (can be combined using bitwise or operator)
  - `0` NONE
  - `1` NOT_PUBLISHED_STATE
  - `2` BLACKLISTED
  - `4` BLOCKED_OWNER
  - `8` FILES
  - `16` CAPTIONS
  - `32` VIDEO SOURCE
  
      @param has_hlsfiles **PeerTube >= 4.0** Display only videos that have HLS files
      @param has_web_video_files **PeerTube >= 6.0** Display only videos that have Web Video files
      @param host Find elements owned by this host
      @param auto_tag_one_of **PeerTube >= 6.2** **Admins and moderators only** filter on videos that contain one of these automatic tags
      @param privacy_one_of **PeerTube >= 4.0** Display only videos in this specific privacy/privacies
      @param exclude_already_watched Whether or not to exclude videos that are in the user's video history
      @param search Plain text search, applied to various parts of the model depending on endpoint
  *)
  val get_api_v1_users_me_subscriptions_videos : ?start:string -> ?count:string -> ?skip_count:string -> ?sort:string -> ?nsfw:string -> ?nsfw_flags_included:string -> ?nsfw_flags_excluded:string -> ?is_live:string -> ?include_scheduled_live:string -> ?category_one_of:string -> ?licence_one_of:string -> ?language_one_of:string -> ?tags_one_of:string -> ?tags_all_of:string -> ?is_local:string -> ?include_:string -> ?has_hlsfiles:string -> ?has_web_video_files:string -> ?host:string -> ?auto_tag_one_of:string -> ?privacy_one_of:string -> ?exclude_already_watched:string -> ?search:string -> t -> unit -> Response.t
  
  (** List videos of my user 
      @param channel_name_one_of **PeerTube >= 7.2** Filter on videos that are published by a channel with one of these names
      @param start Offset used to paginate results
      @param count Number of items to return
      @param skip_count if you don't need the `total` in the response
      @param nsfw whether to include nsfw videos, if any
      @param is_live whether or not the video is a live
      @param include_scheduled_live whether or not include live that are scheduled for later
      @param category_one_of category id of the video (see [/videos/categories](#operation/getCategories))
      @param licence_one_of licence id of the video (see [/videos/licences](#operation/getLicences))
      @param language_one_of language id of the video (see [/videos/languages](#operation/getLanguages)). Use `_unknown` to filter on videos that don't have a video language
      @param tags_one_of tag(s) of the video
      @param tags_all_of tag(s) of the video, where all should be present in the video
      @param is_local **PeerTube >= 4.0** Display only local or remote objects
      @param include_ **Only administrators and moderators can use this parameter**
  
  Include additional videos in results (can be combined using bitwise or operator)
  - `0` NONE
  - `1` NOT_PUBLISHED_STATE
  - `2` BLACKLISTED
  - `4` BLOCKED_OWNER
  - `8` FILES
  - `16` CAPTIONS
  - `32` VIDEO SOURCE
  
      @param has_hlsfiles **PeerTube >= 4.0** Display only videos that have HLS files
      @param has_web_video_files **PeerTube >= 6.0** Display only videos that have Web Video files
      @param host Find elements owned by this host
      @param auto_tag_one_of **PeerTube >= 6.2** **Admins and moderators only** filter on videos that contain one of these automatic tags
      @param privacy_one_of **PeerTube >= 4.0** Display only videos in this specific privacy/privacies
      @param exclude_already_watched Whether or not to exclude videos that are in the user's video history
      @param search Plain text search, applied to various parts of the model depending on endpoint
      @param include_collaborations **PeerTube >= 8.0** Include objects from collaborated channels
  *)
  val get_api_v1_users_me_videos : ?channel_name_one_of:string -> ?start:string -> ?count:string -> ?skip_count:string -> ?sort:string -> ?nsfw:string -> ?nsfw_flags_included:string -> ?nsfw_flags_excluded:string -> ?is_live:string -> ?include_scheduled_live:string -> ?category_one_of:string -> ?licence_one_of:string -> ?language_one_of:string -> ?tags_one_of:string -> ?tags_all_of:string -> ?is_local:string -> ?include_:string -> ?has_hlsfiles:string -> ?has_web_video_files:string -> ?host:string -> ?auto_tag_one_of:string -> ?privacy_one_of:string -> ?exclude_already_watched:string -> ?search:string -> ?include_collaborations:string -> t -> unit -> Response.t
  
  (** List videos of a video channel 
      @param channel_handle The video channel handle
      @param start Offset used to paginate results
      @param count Number of items to return
      @param skip_count if you don't need the `total` in the response
      @param nsfw whether to include nsfw videos, if any
      @param is_live whether or not the video is a live
      @param include_scheduled_live whether or not include live that are scheduled for later
      @param category_one_of category id of the video (see [/videos/categories](#operation/getCategories))
      @param licence_one_of licence id of the video (see [/videos/licences](#operation/getLicences))
      @param language_one_of language id of the video (see [/videos/languages](#operation/getLanguages)). Use `_unknown` to filter on videos that don't have a video language
      @param tags_one_of tag(s) of the video
      @param tags_all_of tag(s) of the video, where all should be present in the video
      @param is_local **PeerTube >= 4.0** Display only local or remote objects
      @param include_ **Only administrators and moderators can use this parameter**
  
  Include additional videos in results (can be combined using bitwise or operator)
  - `0` NONE
  - `1` NOT_PUBLISHED_STATE
  - `2` BLACKLISTED
  - `4` BLOCKED_OWNER
  - `8` FILES
  - `16` CAPTIONS
  - `32` VIDEO SOURCE
  
      @param has_hlsfiles **PeerTube >= 4.0** Display only videos that have HLS files
      @param has_web_video_files **PeerTube >= 6.0** Display only videos that have Web Video files
      @param host Find elements owned by this host
      @param auto_tag_one_of **PeerTube >= 6.2** **Admins and moderators only** filter on videos that contain one of these automatic tags
      @param privacy_one_of **PeerTube >= 4.0** Display only videos in this specific privacy/privacies
      @param exclude_already_watched Whether or not to exclude videos that are in the user's video history
      @param search Plain text search, applied to various parts of the model depending on endpoint
  *)
  val get_video_channel_videos : channel_handle:string -> ?start:string -> ?count:string -> ?skip_count:string -> ?sort:string -> ?nsfw:string -> ?nsfw_flags_included:string -> ?nsfw_flags_excluded:string -> ?is_live:string -> ?include_scheduled_live:string -> ?category_one_of:string -> ?licence_one_of:string -> ?language_one_of:string -> ?tags_one_of:string -> ?tags_all_of:string -> ?is_local:string -> ?include_:string -> ?has_hlsfiles:string -> ?has_web_video_files:string -> ?host:string -> ?auto_tag_one_of:string -> ?privacy_one_of:string -> ?exclude_already_watched:string -> ?search:string -> t -> unit -> Response.t
  
  (** List videos 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param skip_count if you don't need the `total` in the response
      @param nsfw whether to include nsfw videos, if any
      @param is_live whether or not the video is a live
      @param include_scheduled_live whether or not include live that are scheduled for later
      @param category_one_of category id of the video (see [/videos/categories](#operation/getCategories))
      @param licence_one_of licence id of the video (see [/videos/licences](#operation/getLicences))
      @param language_one_of language id of the video (see [/videos/languages](#operation/getLanguages)). Use `_unknown` to filter on videos that don't have a video language
      @param tags_one_of tag(s) of the video
      @param tags_all_of tag(s) of the video, where all should be present in the video
      @param is_local **PeerTube >= 4.0** Display only local or remote objects
      @param include_ **Only administrators and moderators can use this parameter**
  
  Include additional videos in results (can be combined using bitwise or operator)
  - `0` NONE
  - `1` NOT_PUBLISHED_STATE
  - `2` BLACKLISTED
  - `4` BLOCKED_OWNER
  - `8` FILES
  - `16` CAPTIONS
  - `32` VIDEO SOURCE
  
      @param has_hlsfiles **PeerTube >= 4.0** Display only videos that have HLS files
      @param has_web_video_files **PeerTube >= 6.0** Display only videos that have Web Video files
      @param host Find elements owned by this host
      @param auto_tag_one_of **PeerTube >= 6.2** **Admins and moderators only** filter on videos that contain one of these automatic tags
      @param privacy_one_of **PeerTube >= 4.0** Display only videos in this specific privacy/privacies
      @param exclude_already_watched Whether or not to exclude videos that are in the user's video history
      @param search Plain text search, applied to various parts of the model depending on endpoint
  *)
  val get_videos : ?start:string -> ?count:string -> ?skip_count:string -> ?sort:string -> ?nsfw:string -> ?nsfw_flags_included:string -> ?nsfw_flags_excluded:string -> ?is_live:string -> ?include_scheduled_live:string -> ?category_one_of:string -> ?licence_one_of:string -> ?language_one_of:string -> ?tags_one_of:string -> ?tags_all_of:string -> ?is_local:string -> ?include_:string -> ?has_hlsfiles:string -> ?has_web_video_files:string -> ?host:string -> ?auto_tag_one_of:string -> ?privacy_one_of:string -> ?exclude_already_watched:string -> ?search:string -> t -> unit -> Response.t
end

module VideoImport : sig
  module T : sig
    type t
    
    (** Construct a value
        @param magnet_uri magnet URI allowing to resolve the import's source video
        @param target_url remote URL where to find the import's source video
        @param torrentfile Torrent file containing only the video file
    *)
    val v : ?created_at:Ptime.t -> ?error:string -> ?id:Id.T.t -> ?magnet_uri:string -> ?state:VideoImportStateConstant.T.t -> ?target_url:string -> ?torrent_name:string -> ?torrentfile:string -> ?updated_at:Ptime.t -> ?video:Video.T.t -> unit -> t
    
    val created_at : t -> Ptime.t option
    
    val error : t -> string option
    
    val id : t -> Id.T.t option
    
    (** magnet URI allowing to resolve the import's source video *)
    val magnet_uri : t -> string option
    
    val state : t -> VideoImportStateConstant.T.t option
    
    (** remote URL where to find the import's source video *)
    val target_url : t -> string option
    
    val torrent_name : t -> string option
    
    (** Torrent file containing only the video file *)
    val torrentfile : t -> string option
    
    val updated_at : t -> Ptime.t option
    
    val video : t -> Video.T.t option
    
    val jsont : t Jsont.t
  end
end

module VideoImportsList : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?data:VideoImport.T.t list -> ?total:int -> unit -> t
    
    val data : t -> VideoImport.T.t list option
    
    val total : t -> int option
    
    val jsont : t Jsont.t
  end
  
  (** Get video imports of my user 
      @param id Entity id
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
      @param include_collaborations **PeerTube >= 8.0** Include objects from collaborated channels
      @param video_id Filter on import video ID
      @param target_url Filter on import target URL
      @param video_channel_sync_id Filter on imports created by a specific channel synchronization
      @param search Search in video names
  *)
  val get_api_v1_users_me_videos_imports : id:string -> ?start:string -> ?count:string -> ?sort:string -> ?include_collaborations:string -> ?video_id:string -> ?target_url:string -> ?video_channel_sync_id:string -> ?search:string -> t -> unit -> T.t
end

module VideoCommentForOwnerOrAdmin : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?account:Jsont.json -> ?automatic_tags:string list -> ?created_at:Jsont.json -> ?held_for_review:Jsont.json -> ?id:Id.T.t -> ?in_reply_to_comment_id:Jsont.json -> ?text:Jsont.json -> ?thread_id:Jsont.json -> ?updated_at:Jsont.json -> ?url:Jsont.json -> ?video:Video.Info.t -> unit -> t
    
    val account : t -> Jsont.json option
    
    val automatic_tags : t -> string list option
    
    val created_at : t -> Jsont.json option
    
    val held_for_review : t -> Jsont.json option
    
    val id : t -> Id.T.t option
    
    val in_reply_to_comment_id : t -> Jsont.json option
    
    val text : t -> Jsont.json option
    
    val thread_id : t -> Jsont.json option
    
    val updated_at : t -> Jsont.json option
    
    val url : t -> Jsont.json option
    
    val video : t -> Video.Info.t option
    
    val jsont : t Jsont.t
  end
end

module PlaylistElement : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?position:int -> ?start_timestamp:int -> ?stop_timestamp:int -> ?video:Video.T.t -> unit -> t
    
    val position : t -> int option
    
    val start_timestamp : t -> int option
    
    val stop_timestamp : t -> int option
    
    val video : t -> Video.T.t option
    
    val jsont : t Jsont.t
  end
end

module Notification : sig
  module Type : sig
    (** Notification type. One of the following values:
    
      - `1` NEW_VIDEO_FROM_SUBSCRIPTION
    
      - `2` NEW_COMMENT_ON_MY_VIDEO
    
      - `3` NEW_ABUSE_FOR_MODERATORS
    
      - `4` BLACKLIST_ON_MY_VIDEO
    
      - `5` UNBLACKLIST_ON_MY_VIDEO
    
      - `6` MY_VIDEO_PUBLISHED
    
      - `7` MY_VIDEO_IMPORT_SUCCESS
    
      - `8` MY_VIDEO_IMPORT_ERROR
    
      - `9` NEW_USER_REGISTRATION
    
      - `10` NEW_FOLLOW
    
      - `11` COMMENT_MENTION
    
      - `12` VIDEO_AUTO_BLACKLIST_FOR_MODERATORS
    
      - `13` NEW_INSTANCE_FOLLOWER
    
      - `14` AUTO_INSTANCE_FOLLOWING
    
      - `15` ABUSE_STATE_CHANGE
    
      - `16` ABUSE_NEW_MESSAGE
    
      - `17` NEW_PLUGIN_VERSION
    
      - `18` NEW_PEERTUBE_VERSION
    
      - `19` MY_VIDEO_STUDIO_EDITION_FINISHED
    
      - `20` NEW_USER_REGISTRATION_REQUEST
    
      - `21` NEW_LIVE_FROM_SUBSCRIPTION
    
      - `22` MY_VIDEO_TRANSCRIPTION_GENERATED
     *)
    type t = string
    
    val jsont : t Jsont.t
  end
  
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?account:Actor.Info.t -> ?actor_follow:Jsont.json -> ?comment:Jsont.json -> ?created_at:Ptime.t -> ?id:Id.T.t -> ?read:bool -> ?type_:Type.t -> ?updated_at:Ptime.t -> ?video:Video.Info.t -> ?video_abuse:Jsont.json -> ?video_blacklist:Jsont.json -> ?video_import:Jsont.json -> unit -> t
    
    val account : t -> Actor.Info.t option
    
    val actor_follow : t -> Jsont.json option
    
    val comment : t -> Jsont.json option
    
    val created_at : t -> Ptime.t option
    
    val id : t -> Id.T.t option
    
    val read : t -> bool option
    
    val type_ : t -> Type.t option
    
    val updated_at : t -> Ptime.t option
    
    val video : t -> Video.Info.t option
    
    val video_abuse : t -> Jsont.json option
    
    val video_blacklist : t -> Jsont.json option
    
    val video_import : t -> Jsont.json option
    
    val jsont : t Jsont.t
  end
end

module NotificationList : sig
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : ?data:Notification.T.t list -> ?total:int -> unit -> t
    
    val data : t -> Notification.T.t list option
    
    val total : t -> int option
    
    val jsont : t Jsont.t
  end
  
  (** List my notifications 
      @param type_one_of only list notifications of these types
      @param unread only list unread notifications
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  val get_api_v1_users_me_notifications : ?type_one_of:string -> ?unread:string -> ?start:string -> ?count:string -> ?sort:string -> t -> unit -> Response.t
end

module AbuseMessage : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?account:AccountSummary.T.t -> ?by_moderator:bool -> ?created_at:Ptime.t -> ?id:Id.T.t -> ?message:string -> unit -> t
    
    val account : t -> AccountSummary.T.t option
    
    val by_moderator : t -> bool option
    
    val created_at : t -> Ptime.t option
    
    val id : t -> Id.T.t option
    
    val message : t -> string option
    
    val jsont : t Jsont.t
  end
end

module Account : sig
  module T : sig
    type t
    
    (** Construct a value
        @param followers_count number of followers of this actor, as seen by this instance
        @param following_count number of actors subscribed to by this actor, as seen by this instance
        @param host server on which the actor is resident
        @param host_redundancy_allowed whether this actor's host allows redundancy of its videos
        @param name immutable name of the actor, used to find or mention it
        @param user_id object id for the user tied to this account
        @param display_name editable name of the account, displayed in its representations
        @param description text or bio displayed on the account's profile
    *)
    val v : ?avatars:ActorImage.T.t list -> ?created_at:Ptime.t -> ?followers_count:int -> ?following_count:int -> ?host:string -> ?host_redundancy_allowed:bool -> ?id:Id.T.t -> ?name:Username.T.t -> ?updated_at:Ptime.t -> ?url:string -> ?user_id:Jsont.json -> ?display_name:string -> ?description:string -> unit -> t
    
    val avatars : t -> ActorImage.T.t list option
    
    val created_at : t -> Ptime.t option
    
    (** number of followers of this actor, as seen by this instance *)
    val followers_count : t -> int option
    
    (** number of actors subscribed to by this actor, as seen by this instance *)
    val following_count : t -> int option
    
    (** server on which the actor is resident *)
    val host : t -> string option
    
    (** whether this actor's host allows redundancy of its videos *)
    val host_redundancy_allowed : t -> bool option
    
    val id : t -> Id.T.t option
    
    (** immutable name of the actor, used to find or mention it *)
    val name : t -> Username.T.t option
    
    val updated_at : t -> Ptime.t option
    
    val url : t -> string option
    
    (** object id for the user tied to this account *)
    val user_id : t -> Jsont.json option
    
    (** editable name of the account, displayed in its representations *)
    val display_name : t -> string option
    
    (** text or bio displayed on the account's profile *)
    val description : t -> string option
    
    val jsont : t Jsont.t
  end
  
  (** Get an account 
      @param name The username or handle of the account
  *)
  val get_account : name:string -> t -> unit -> T.t
end

module VideoComment : sig
  module T : sig
    type t
    
    (** Construct a value
        @param text Text of the comment
    *)
    val v : ?deleted_at:Ptime.t option -> ?is_deleted:bool -> ?account:Account.T.t -> ?created_at:Ptime.t -> ?held_for_review:bool -> ?id:Id.T.t -> ?in_reply_to_comment_id:Id.T.t -> ?text:string -> ?thread_id:Id.T.t -> ?total_replies:int -> ?total_replies_from_video_author:int -> ?updated_at:Ptime.t -> ?url:string -> ?video_id:Jsont.json -> unit -> t
    
    val account : t -> Account.T.t option
    
    val created_at : t -> Ptime.t option
    
    val deleted_at : t -> Ptime.t option
    
    val held_for_review : t -> bool option
    
    val id : t -> Id.T.t option
    
    val in_reply_to_comment_id : t -> Id.T.t option
    
    val is_deleted : t -> bool
    
    (** Text of the comment *)
    val text : t -> string option
    
    val thread_id : t -> Id.T.t option
    
    val total_replies : t -> int option
    
    val total_replies_from_video_author : t -> int option
    
    val updated_at : t -> Ptime.t option
    
    val url : t -> string option
    
    val video_id : t -> Jsont.json option
    
    val jsont : t Jsont.t
  end
end

module VideoCommentThreadTree : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?children:Jsont.json list -> ?comment:VideoComment.T.t -> unit -> t
    
    val children : t -> Jsont.json list option
    
    val comment : t -> VideoComment.T.t option
    
    val jsont : t Jsont.t
  end
  
  (** Get a thread 
      @param id The object id, uuid or short uuid
      @param thread_id The thread id (root comment id)
  *)
  val get_api_v1_videos_comment_threads : id:string -> thread_id:string -> t -> unit -> T.t
end

module CommentThreadPost : sig
  module Response : sig
    type t
    
    (** Construct a value *)
    val v : ?comment:VideoComment.T.t -> unit -> t
    
    val comment : t -> VideoComment.T.t option
    
    val jsont : t Jsont.t
  end
  
  (** Create a thread 
      @param id The object id, uuid or short uuid
  *)
  val post_api_v1_videos_comment_threads : id:string -> t -> unit -> Response.t
  
  (** Reply to a thread of a video 
      @param id The object id, uuid or short uuid
      @param comment_id The comment id
  *)
  val post_api_v1_videos_comments : id:string -> comment_id:string -> t -> unit -> Response.t
end

module CommentThread : sig
  module Response : sig
    type t
    
    (** Construct a value
        @param total Total threads (included deleted ones) on this video
        @param total_not_deleted_comments Total not-deleted threads (included deleted ones) on this video
    *)
    val v : ?data:VideoComment.T.t list -> ?total:int -> ?total_not_deleted_comments:int -> unit -> t
    
    val data : t -> VideoComment.T.t list option
    
    (** Total threads (included deleted ones) on this video *)
    val total : t -> int option
    
    (** Total not-deleted threads (included deleted ones) on this video *)
    val total_not_deleted_comments : t -> int option
    
    val jsont : t Jsont.t
  end
  
  (** List threads of a video 
      @param id The object id, uuid or short uuid
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort comments by criteria
  *)
  val get_api_v1_videos_comment_threads : id:string -> ?start:string -> ?count:string -> ?sort:string -> t -> unit -> Response.t
end

module VideoChannel : sig
  module Update : sig
    type t
    
    (** Construct a value
        @param description Channel description
        @param display_name Channel display name
        @param support How to support/fund the channel
        @param bulk_videos_support_update Update the support field for all videos of this channel
    *)
    val v : ?description:Jsont.json -> ?display_name:Jsont.json -> ?support:Jsont.json -> ?bulk_videos_support_update:bool -> unit -> t
    
    (** Channel description *)
    val description : t -> Jsont.json option
    
    (** Channel display name *)
    val display_name : t -> Jsont.json option
    
    (** How to support/fund the channel *)
    val support : t -> Jsont.json option
    
    (** Update the support field for all videos of this channel *)
    val bulk_videos_support_update : t -> bool option
    
    val jsont : t Jsont.t
  end
  
  module Create : sig
    type t
    
    (** Construct a value
        @param display_name Channel display name
        @param name username of the channel to create
        @param description Channel description
        @param support How to support/fund the channel
    *)
    val v : display_name:Jsont.json -> name:UsernameChannel.T.t -> ?description:Jsont.json -> ?support:Jsont.json -> unit -> t
    
    (** Channel description *)
    val description : t -> Jsont.json option
    
    (** Channel display name *)
    val display_name : t -> Jsont.json
    
    (** How to support/fund the channel *)
    val support : t -> Jsont.json option
    
    (** username of the channel to create *)
    val name : t -> UsernameChannel.T.t
    
    val jsont : t Jsont.t
  end
  
  module T : sig
    type t
    
    (** Construct a value
        @param followers_count number of followers of this actor, as seen by this instance
        @param following_count number of actors subscribed to by this actor, as seen by this instance
        @param host server on which the actor is resident
        @param host_redundancy_allowed whether this actor's host allows redundancy of its videos
        @param name immutable name of the actor, used to find or mention it
        @param display_name editable name of the channel, displayed in its representations
        @param support text shown by default on all videos of this channel, to tell the audience how to support it
    *)
    val v : ?avatars:ActorImage.T.t list -> ?created_at:Ptime.t -> ?followers_count:int -> ?following_count:int -> ?host:string -> ?host_redundancy_allowed:bool -> ?id:Id.T.t -> ?name:Username.T.t -> ?url:string -> ?display_name:string -> ?description:string -> ?support:string -> ?is_local:bool -> ?updated_at:Ptime.t -> ?banners:ActorImage.T.t list -> ?owner_account:Account.T.t -> unit -> t
    
    val avatars : t -> ActorImage.T.t list option
    
    val created_at : t -> Ptime.t option
    
    (** number of followers of this actor, as seen by this instance *)
    val followers_count : t -> int option
    
    (** number of actors subscribed to by this actor, as seen by this instance *)
    val following_count : t -> int option
    
    (** server on which the actor is resident *)
    val host : t -> string option
    
    (** whether this actor's host allows redundancy of its videos *)
    val host_redundancy_allowed : t -> bool option
    
    val id : t -> Id.T.t option
    
    (** immutable name of the actor, used to find or mention it *)
    val name : t -> Username.T.t option
    
    val url : t -> string option
    
    (** editable name of the channel, displayed in its representations *)
    val display_name : t -> string option
    
    val description : t -> string option
    
    (** text shown by default on all videos of this channel, to tell the audience how to support it *)
    val support : t -> string option
    
    val is_local : t -> bool option
    
    val updated_at : t -> Ptime.t option
    
    val banners : t -> ActorImage.T.t list option
    
    val owner_account : t -> Account.T.t option
    
    val jsont : t Jsont.t
  end
  
  (** Get subscription of my user 
      @param subscription_handle The subscription handle
  *)
  val get_api_v1_users_me_subscriptions : subscription_handle:string -> t -> unit -> T.t
  
  (** Get a video channel 
      @param channel_handle The video channel handle
  *)
  val get_video_channel : channel_handle:string -> t -> unit -> T.t
end

module VideoDetails : sig
  module T : sig
    type t
    
    (** Construct a value
        @param aspect_ratio **PeerTube >= 6.1** Aspect ratio of the video stream
        @param category category in which the video is classified
        @param comments **PeerTube >= 7.2** Number of comments on the video
        @param created_at time at which the video object was first drafted
        @param duration duration of the video in seconds
        @param id object id for the video
        @param language main language used in the video
        @param licence licence under which the video is distributed
        @param name title of the video
        @param nsfw_summary **PeerTube >= 7.2** More information about the sensitive content of the video
        @param originally_published_at used to represent a date of first publication, prior to the practical publication date of `publishedAt`
        @param privacy privacy policy used to distribute the video
        @param published_at time at which the video was marked as ready for playback (with restrictions depending on `privacy`). Usually set after a `state` evolution.
        @param state represents the internal state of the video processing within the PeerTube instance
        @param truncated_description truncated description of the video, written in Markdown.
    
        @param updated_at last time the video's metadata was modified
        @param uuid universal identifier for the video, that can be used across instances
        @param viewers If the video is a live, you have the amount of current viewers
        @param description full description of the video, written in Markdown.
    
        @param support A text tell the audience how to support the video creator
        @param input_file_updated_at Latest input file update. Null if the file has never been replaced since the original upload
        @param files Web compatible video files. If Web Video is disabled on the server:
    
    - field will be empty
    - video files will be found in `streamingPlaylists[].files` field
    
        @param streaming_playlists HLS playlists/manifest files. If HLS is disabled on the server:
    
    - field will be empty
    - video files will be found in `files` field
    
    *)
    val v : ?aspect_ratio:float -> ?blacklisted:bool -> ?blacklisted_reason:string -> ?category:VideoConstantNumberCategory.T.t -> ?comments:int -> ?created_at:Ptime.t -> ?dislikes:int -> ?duration:int -> ?embed_path:string -> ?id:Id.T.t -> ?is_live:bool -> ?is_local:bool -> ?language:VideoConstantStringLanguage.T.t -> ?licence:VideoConstantNumberLicence.T.t -> ?likes:int -> ?live_schedules:LiveSchedule.T.t list -> ?name:string -> ?nsfw:bool -> ?nsfw_flags:Nsfwflag.T.t -> ?nsfw_summary:string -> ?originally_published_at:Ptime.t -> ?preview_path:string -> ?privacy:VideoPrivacyConstant.T.t -> ?published_at:Ptime.t -> ?scheduled_update:VideoScheduled.Update.t -> ?short_uuid:ShortUuid.T.t -> ?state:VideoStateConstant.T.t -> ?thumbnail_path:string -> ?truncated_description:string -> ?updated_at:Ptime.t -> ?user_history:Jsont.json -> ?uuid:Uuidv4.T.t -> ?views:int -> ?wait_transcoding:bool -> ?viewers:int -> ?description:string -> ?support:string -> ?channel:VideoChannel.T.t -> ?account:Account.T.t -> ?tags:string list -> ?comments_policy:VideoCommentsPolicyConstant.T.t -> ?download_enabled:bool -> ?input_file_updated_at:Ptime.t -> ?tracker_urls:string list -> ?files:VideoFile.T.t list -> ?streaming_playlists:VideoStreamingPlaylists.T.t list -> unit -> t
    
    (** **PeerTube >= 6.1** Aspect ratio of the video stream *)
    val aspect_ratio : t -> float option
    
    val blacklisted : t -> bool option
    
    val blacklisted_reason : t -> string option
    
    (** category in which the video is classified *)
    val category : t -> VideoConstantNumberCategory.T.t option
    
    (** **PeerTube >= 7.2** Number of comments on the video *)
    val comments : t -> int option
    
    (** time at which the video object was first drafted *)
    val created_at : t -> Ptime.t option
    
    val dislikes : t -> int option
    
    (** duration of the video in seconds *)
    val duration : t -> int option
    
    val embed_path : t -> string option
    
    (** object id for the video *)
    val id : t -> Id.T.t option
    
    val is_live : t -> bool option
    
    val is_local : t -> bool option
    
    (** main language used in the video *)
    val language : t -> VideoConstantStringLanguage.T.t option
    
    (** licence under which the video is distributed *)
    val licence : t -> VideoConstantNumberLicence.T.t option
    
    val likes : t -> int option
    
    val live_schedules : t -> LiveSchedule.T.t list option
    
    (** title of the video *)
    val name : t -> string option
    
    val nsfw : t -> bool option
    
    val nsfw_flags : t -> Nsfwflag.T.t option
    
    (** **PeerTube >= 7.2** More information about the sensitive content of the video *)
    val nsfw_summary : t -> string option
    
    (** used to represent a date of first publication, prior to the practical publication date of `publishedAt` *)
    val originally_published_at : t -> Ptime.t option
    
    val preview_path : t -> string option
    
    (** privacy policy used to distribute the video *)
    val privacy : t -> VideoPrivacyConstant.T.t option
    
    (** time at which the video was marked as ready for playback (with restrictions depending on `privacy`). Usually set after a `state` evolution. *)
    val published_at : t -> Ptime.t option
    
    val scheduled_update : t -> VideoScheduled.Update.t option
    
    val short_uuid : t -> ShortUuid.T.t option
    
    (** represents the internal state of the video processing within the PeerTube instance *)
    val state : t -> VideoStateConstant.T.t option
    
    val thumbnail_path : t -> string option
    
    (** truncated description of the video, written in Markdown.
     *)
    val truncated_description : t -> string option
    
    (** last time the video's metadata was modified *)
    val updated_at : t -> Ptime.t option
    
    val user_history : t -> Jsont.json option
    
    (** universal identifier for the video, that can be used across instances *)
    val uuid : t -> Uuidv4.T.t option
    
    val views : t -> int option
    
    val wait_transcoding : t -> bool option
    
    (** If the video is a live, you have the amount of current viewers *)
    val viewers : t -> int option
    
    (** full description of the video, written in Markdown.
     *)
    val description : t -> string option
    
    (** A text tell the audience how to support the video creator *)
    val support : t -> string option
    
    val channel : t -> VideoChannel.T.t option
    
    val account : t -> Account.T.t option
    
    val tags : t -> string list option
    
    val comments_policy : t -> VideoCommentsPolicyConstant.T.t option
    
    val download_enabled : t -> bool option
    
    (** Latest input file update. Null if the file has never been replaced since the original upload *)
    val input_file_updated_at : t -> Ptime.t option
    
    val tracker_urls : t -> string list option
    
    (** Web compatible video files. If Web Video is disabled on the server:
    
    - field will be empty
    - video files will be found in `streamingPlaylists[].files` field
     *)
    val files : t -> VideoFile.T.t list option
    
    (** HLS playlists/manifest files. If HLS is disabled on the server:
    
    - field will be empty
    - video files will be found in `files` field
     *)
    val streaming_playlists : t -> VideoStreamingPlaylists.T.t list option
    
    val jsont : t Jsont.t
  end
  
  (** Get a video 
      @param id The object id, uuid or short uuid
  *)
  val get_video : id:string -> t -> unit -> T.t
end

module VideoChannelSync : sig
  module Create : sig
    type t
    
    (** Construct a value *)
    val v : ?external_channel_url:string -> ?video_channel_id:Id.T.t -> unit -> t
    
    val external_channel_url : t -> string option
    
    val video_channel_id : t -> Id.T.t option
    
    val jsont : t Jsont.t
  end
  
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?channel:VideoChannel.T.t -> ?created_at:Ptime.t -> ?external_channel_url:string -> ?id:Id.T.t -> ?last_sync_at:Ptime.t -> ?state:Jsont.json -> unit -> t
    
    val channel : t -> VideoChannel.T.t option
    
    val created_at : t -> Ptime.t option
    
    val external_channel_url : t -> string option
    
    val id : t -> Id.T.t option
    
    val last_sync_at : t -> Ptime.t option
    
    val state : t -> Jsont.json option
    
    val jsont : t Jsont.t
  end
end

module VideoChannelSyncList : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?data:VideoChannelSync.T.t list -> ?total:int -> unit -> t
    
    val data : t -> VideoChannelSync.T.t list option
    
    val total : t -> int option
    
    val jsont : t Jsont.t
  end
  
  (** List the synchronizations of video channels of an account 
      @param name The username or handle of the account
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
      @param include_collaborations **PeerTube >= 8.0** Include objects from collaborated channels
  *)
  val get_api_v1_accounts_video_channel_syncs : name:string -> ?start:string -> ?count:string -> ?sort:string -> ?include_collaborations:string -> t -> unit -> T.t
end

module Client : sig
  (** List abuses 
      @param id only list the report with this id
      @param predefined_reason predefined reason the listed reports should contain
      @param search plain search that will match with video titles, reporter names and more
      @param search_reporter only list reports of a specific reporter
      @param search_reportee only list reports of a specific reportee
      @param search_video only list reports of a specific video
      @param search_video_channel only list reports of a specific video channel
      @param video_is only list deleted or blocklisted videos
      @param filter only list account, comment or video reports
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort abuses by criteria
  *)
  val get_abuses : ?id:string -> ?predefined_reason:string -> ?search:string -> ?state:string -> ?search_reporter:string -> ?search_reportee:string -> ?search_video:string -> ?search_video_channel:string -> ?video_is:string -> ?filter:string -> ?start:string -> ?count:string -> ?sort:string -> t -> unit -> Jsont.json
  
  (** Report an abuse *)
  val post_api_v1_abuses : t -> unit -> Jsont.json
  
  (** Update an abuse 
      @param abuse_id Abuse id
  *)
  val put_api_v1_abuses : abuse_id:string -> t -> unit -> Jsont.json
  
  (** Delete an abuse 
      @param abuse_id Abuse id
  *)
  val delete_api_v1_abuses : abuse_id:string -> t -> unit -> Jsont.json
  
  (** List messages of an abuse 
      @param abuse_id Abuse id
  *)
  val get_api_v1_abuses_messages : abuse_id:string -> t -> unit -> Jsont.json
  
  (** Add message to an abuse 
      @param abuse_id Abuse id
  *)
  val post_api_v1_abuses_messages : abuse_id:string -> t -> unit -> Jsont.json
  
  (** Delete an abuse message 
      @param abuse_id Abuse id
      @param abuse_message_id Abuse message id
  *)
  val delete_api_v1_abuses_messages : abuse_id:string -> abuse_message_id:string -> t -> unit -> Jsont.json
  
  (** List accounts 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  val get_accounts : ?start:string -> ?count:string -> ?sort:string -> t -> unit -> Jsont.json
  
  (** List followers of an account 
      @param name The username or handle of the account
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort followers by criteria
      @param search Plain text search, applied to various parts of the model depending on endpoint
  *)
  val get_account_followers : name:string -> ?start:string -> ?count:string -> ?sort:string -> ?search:string -> t -> unit -> Jsont.json
  
  (** List playlists of an account 
      @param name The username or handle of the account
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
      @param search Plain text search, applied to various parts of the model depending on endpoint
      @param include_collaborations **PeerTube >= 8.0** Include objects from collaborated channels
      @param channel_name_one_of **PeerTube >= 8.0** Filter on playlists that are published on a channel with one of these names
  *)
  val get_api_v1_accounts_video_playlists : name:string -> ?start:string -> ?count:string -> ?sort:string -> ?search:string -> ?playlist_type:string -> ?include_collaborations:string -> ?channel_name_one_of:string -> t -> unit -> Jsont.json
  
  (** Update account auto tag policies on comments
  
      **PeerTube >= 6.2** 
      @param account_name account name to update auto tag policies
  *)
  val put_api_v1_automatic_tags_policies_accounts_comments : account_name:string -> t -> unit -> Jsont.json
  
  (** Update client language
  
      Set a cookie so that, the next time the client refreshes the HTML of the web interface, PeerTube will use the next language *)
  val update_client_language : t -> unit -> Jsont.json
  
  (** Set instance runtime configuration *)
  val put_custom_config : t -> unit -> Jsont.json
  
  (** Delete instance runtime configuration *)
  val del_custom_config : t -> unit -> Jsont.json
  
  (** Delete instance avatar *)
  val delete_api_v1_config_instance_avatar : t -> unit -> Jsont.json
  
  (** Update instance avatar *)
  val post_api_v1_config_instance_avatar_pick : t -> unit -> Jsont.json
  
  (** Delete instance banner *)
  val delete_api_v1_config_instance_banner : t -> unit -> Jsont.json
  
  (** Update instance banner *)
  val post_api_v1_config_instance_banner_pick : t -> unit -> Jsont.json
  
  (** Delete instance logo *)
  val delete_api_v1_config_instance_logo_logo_type : logo_type:string -> t -> unit -> Jsont.json
  
  (** Update instance logo *)
  val post_api_v1_config_instance_logo_logo_type_pick : logo_type:string -> t -> unit -> Jsont.json
  
  (** Set instance custom homepage *)
  val put_api_v1_custom_pages_homepage_instance : t -> unit -> Jsont.json
  
  (** Pause job queue *)
  val post_api_v1_jobs_pause : t -> unit -> Jsont.json
  
  (** Resume job queue *)
  val post_api_v1_jobs_resume : t -> unit -> Jsont.json
  
  (** List instance jobs 
      @param state The state of the job ('' for for no filter)
      @param job_type job type
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  val get_jobs : state:string -> ?job_type:string -> ?start:string -> ?count:string -> ?sort:string -> t -> unit -> Jsont.json
  
  (** Create playback metrics
  
      These metrics are exposed by OpenTelemetry metrics exporter if enabled. *)
  val post_api_v1_metrics_playback : body:PlaybackMetric.Create.t -> t -> unit -> Jsont.json
  
  (** Install a plugin *)
  val add_plugin : t -> unit -> Jsont.json
  
  (** Uninstall a plugin *)
  val uninstall_plugin : t -> unit -> Jsont.json
  
  (** Update a plugin *)
  val update_plugin : t -> unit -> Jsont.json
  
  (** Get a plugin's public settings 
      @param npm_name name of the plugin/theme on npmjs.com or in its package.json
  *)
  val get_api_v1_plugins_public_settings : npm_name:string -> t -> unit -> Jsont.json
  
  (** Get a plugin's registered settings 
      @param npm_name name of the plugin/theme on npmjs.com or in its package.json
  *)
  val get_api_v1_plugins_registered_settings : npm_name:string -> t -> unit -> Jsont.json
  
  (** Set a plugin's settings 
      @param npm_name name of the plugin/theme on npmjs.com or in its package.json
  *)
  val put_api_v1_plugins_settings : npm_name:string -> t -> unit -> Jsont.json
  
  (** List runners 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort runners by criteria
  *)
  val get_api_v1_runners : ?start:string -> ?count:string -> ?sort:string -> t -> unit -> Jsont.json
  
  (** List jobs 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort runner jobs by criteria
      @param search Plain text search, applied to various parts of the model depending on endpoint
  *)
  val get_api_v1_runners_jobs : ?start:string -> ?count:string -> ?sort:string -> ?search:string -> ?state_one_of:string -> t -> unit -> Jsont.json
  
  (** Request a new job
  
      API used by PeerTube runners *)
  val post_api_v1_runners_jobs_request : t -> unit -> Jsont.json
  
  (** Delete a job
  
      The endpoint will first cancel the job if needed, and then remove it from the database. Children jobs will also be removed *)
  val delete_api_v1_runners_jobs : job_uuid:string -> t -> unit -> Jsont.json
  
  (** Abort job
  
      API used by PeerTube runners *)
  val post_api_v1_runners_jobs_abort : job_uuid:string -> t -> unit -> Jsont.json
  
  (** Accept job
  
      API used by PeerTube runners *)
  val post_api_v1_runners_jobs_accept : job_uuid:string -> t -> unit -> Jsont.json
  
  (** Cancel a job *)
  val get_api_v1_runners_jobs_cancel : job_uuid:string -> t -> unit -> Jsont.json
  
  (** Post job error
  
      API used by PeerTube runners *)
  val post_api_v1_runners_jobs_error : job_uuid:string -> t -> unit -> Jsont.json
  
  (** Post job success
  
      API used by PeerTube runners *)
  val post_api_v1_runners_jobs_success : job_uuid:string -> t -> unit -> Jsont.json
  
  (** Update job
  
      API used by PeerTube runners *)
  val post_api_v1_runners_jobs_update : job_uuid:string -> t -> unit -> Jsont.json
  
  (** Register a new runner
  
      API used by PeerTube runners *)
  val post_api_v1_runners_register : t -> unit -> Jsont.json
  
  (** List registration tokens 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort registration tokens by criteria
  *)
  val get_api_v1_runners_registration_tokens : ?start:string -> ?count:string -> ?sort:string -> t -> unit -> Jsont.json
  
  (** Generate registration token
  
      Generate a new runner registration token *)
  val post_api_v1_runners_registration_tokens_generate : t -> unit -> Jsont.json
  
  (** Remove registration token
  
      Remove a registration token. Runners that used this token for their registration are automatically removed. *)
  val delete_api_v1_runners_registration_tokens : registration_token_id:string -> t -> unit -> Jsont.json
  
  (** Unregister a runner
  
      API used by PeerTube runners *)
  val post_api_v1_runners_unregister : t -> unit -> Jsont.json
  
  (** Delete a runner *)
  val delete_api_v1_runners : runner_id:string -> t -> unit -> Jsont.json
  
  (** Search playlists 
      @param search String to search. If the user can make a remote URI search, and the string is an URI then the PeerTube instance will fetch the remote object and add it to its database. Then, you can use the REST API to fetch the complete playlist information and interact with it.
  
      @param start Offset used to paginate results
      @param count Number of items to return
      @param search_target If the administrator enabled search index support, you can override the default search target.
  
  **Warning**: If you choose to make an index search, PeerTube will get results from a third party service. It means the instance may not yet know the objects you fetched. If you want to load video/channel information:
    * If the current user has the ability to make a remote URI search (this information is available in the config endpoint),
    then reuse the search API to make a search using the object URI so PeerTube instance fetches the remote object and fill its database.
    After that, you can use the classic REST API endpoints to fetch the complete object or interact with it
    * If the current user doesn't have the ability to make a remote URI search, then redirect the user on the origin instance or fetch
    the data from the origin instance API
  
      @param sort Sort column
      @param host Find elements owned by this host
      @param uuids Find elements with specific UUIDs
  *)
  val search_playlists : search:string -> ?start:string -> ?count:string -> ?search_target:string -> ?sort:string -> ?host:string -> ?uuids:string -> t -> unit -> Jsont.json
  
  (** Get instance audit logs *)
  val get_instance_audit_logs : t -> unit -> Jsont.json
  
  (** List account blocks 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  val get_api_v1_server_blocklist_accounts : ?start:string -> ?count:string -> ?sort:string -> t -> unit -> Jsont.json
  
  (** Block an account *)
  val post_api_v1_server_blocklist_accounts : t -> unit -> Jsont.json
  
  (** Unblock an account by its handle 
      @param account_name account to unblock, in the form `username@domain`
  *)
  val delete_api_v1_server_blocklist_accounts : account_name:string -> t -> unit -> Jsont.json
  
  (** List server blocks 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  val get_api_v1_server_blocklist_servers : ?start:string -> ?count:string -> ?sort:string -> t -> unit -> Jsont.json
  
  (** Block a server *)
  val post_api_v1_server_blocklist_servers : t -> unit -> Jsont.json
  
  (** Unblock a server by its domain 
      @param host server domain to unblock
  *)
  val delete_api_v1_server_blocklist_servers : host:string -> t -> unit -> Jsont.json
  
  (** List instances following the server 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  val get_api_v1_server_followers : ?state:string -> ?actor_type:string -> ?start:string -> ?count:string -> ?sort:string -> t -> unit -> Jsont.json
  
  (** Remove or reject a follower to your server 
      @param handle The remote actor handle to remove from your followers
  *)
  val delete_api_v1_server_followers : handle:string -> t -> unit -> Jsont.json
  
  (** Accept a pending follower to your server 
      @param handle The remote actor handle to remove from your followers
  *)
  val post_api_v1_server_followers_accept : handle:string -> t -> unit -> Jsont.json
  
  (** Reject a pending follower to your server 
      @param handle The remote actor handle to remove from your followers
  *)
  val post_api_v1_server_followers_reject : handle:string -> t -> unit -> Jsont.json
  
  (** List instances followed by the server 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  val get_api_v1_server_following : ?state:string -> ?actor_type:string -> ?start:string -> ?count:string -> ?sort:string -> t -> unit -> Jsont.json
  
  (** Follow a list of actors (PeerTube instance, channel or account) *)
  val post_api_v1_server_following : t -> unit -> Jsont.json
  
  (** Unfollow an actor (PeerTube instance, channel or account) 
      @param host_or_handle The hostOrHandle to unfollow
  *)
  val delete_api_v1_server_following : host_or_handle:string -> t -> unit -> Jsont.json
  
  (** Get instance logs *)
  val get_instance_logs : t -> unit -> Jsont.json
  
  (** Send client log *)
  val send_client_log : body:SendClientLog.T.t -> t -> unit -> Jsont.json
  
  (** Mirror a video *)
  val put_mirrored_video : t -> unit -> Jsont.json
  
  (** Delete a mirror done on a video 
      @param redundancy_id id of an existing redundancy on a video
  *)
  val del_mirrored_video : redundancy_id:string -> t -> unit -> Jsont.json
  
  (** Update a server redundancy policy 
      @param host server domain to mirror
  *)
  val put_api_v1_server_redundancy : host:string -> t -> unit -> Jsont.json
  
  (** Ask to reset password
  
      An email containing a reset password link *)
  val post_api_v1_users_ask_reset_password : t -> unit -> Jsont.json
  
  (** Resend user verification link *)
  val resend_email_to_verify_user : t -> unit -> Jsont.json
  
  (** Update my user information *)
  val put_user_info : body:UpdateMe.T.t -> t -> unit -> Jsont.json
  
  (** List my abuses 
      @param id only list the report with this id
      @param sort Sort abuses by criteria
      @param start Offset used to paginate results
      @param count Number of items to return
  *)
  val get_my_abuses : ?id:string -> ?state:string -> ?sort:string -> ?start:string -> ?count:string -> t -> unit -> Jsont.json
  
  (** Delete my avatar *)
  val delete_api_v1_users_me_avatar : t -> unit -> Jsont.json
  
  (** Update my user avatar *)
  val post_api_v1_users_me_avatar_pick : t -> unit -> Jsont.json
  
  (** Clear video history *)
  val post_api_v1_users_me_history_videos_remove : t -> unit -> Jsont.json
  
  (** Delete history element *)
  val delete_api_v1_users_me_history_videos : video_id:string -> t -> unit -> Jsont.json
  
  (** Mark feature info as read
  
      **PeerTube >= v8.0.0 *)
  val post_api_v1_users_me_new_feature_info_read : t -> unit -> Jsont.json
  
  (** Update my notification settings *)
  val put_api_v1_users_me_notification_settings : body:UserNotificationSettings.T.t -> t -> unit -> Jsont.json
  
  (** Mark notifications as read by their id *)
  val post_api_v1_users_me_notifications_read : t -> unit -> Jsont.json
  
  (** Mark all my notification as read *)
  val post_api_v1_users_me_notifications_read_all : t -> unit -> Jsont.json
  
  (** Add subscription to my user *)
  val post_api_v1_users_me_subscriptions : t -> unit -> Jsont.json
  
  (** Get if subscriptions exist for my user 
      @param uris list of uris to check if each is part of the user subscriptions
  *)
  val get_api_v1_users_me_subscriptions_exist : uris:string -> t -> unit -> Jsont.json
  
  (** Delete subscription of my user 
      @param subscription_handle The subscription handle
  *)
  val delete_api_v1_users_me_subscriptions : subscription_handle:string -> t -> unit -> Jsont.json
  
  (** Check video exists in my playlists 
      @param video_ids The video ids to check
  *)
  val get_api_v1_users_me_video_playlists_videos_exist : video_ids:string -> t -> unit -> Jsont.json
  
  (** Get my user used quota *)
  val get_api_v1_users_me_video_quota_used : t -> unit -> Jsont.json
  
  (** List comments on user's videos
  
      **PeerTube >= 6.2** 
      @param search Plain text search, applied to various parts of the model depending on endpoint
      @param search_account Filter comments by searching on the account
      @param search_video Filter comments by searching on the video
      @param video_id Limit results on this specific video
      @param video_channel_id Limit results on this specific video channel
      @param auto_tag_one_of **PeerTube >= 6.2** filter on comments that contain one of these automatic tags
      @param is_held_for_review only display comments that are held for review
      @param include_collaborations **PeerTube >= 8.0** Include objects from collaborated channels
  *)
  val get_api_v1_users_me_videos_comments : ?search:string -> ?search_account:string -> ?search_video:string -> ?video_id:string -> ?video_channel_id:string -> ?auto_tag_one_of:string -> ?is_held_for_review:string -> ?include_collaborations:string -> t -> unit -> Jsont.json
  
  (** Register a user
  
      Signup has to be enabled and signup approval is not required *)
  val register_user : body:RegisterUser.T.t -> t -> unit -> Jsont.json
  
  (** List registrations 
      @param start Offset used to paginate results
      @param count Number of items to return
  *)
  val list_registrations : ?start:string -> ?count:string -> ?search:string -> ?sort:string -> t -> unit -> Jsont.json
  
  (** Resend verification link to registration request email *)
  val resend_email_to_verify_registration : t -> unit -> Jsont.json
  
  (** Delete registration
  
      Delete the registration entry. It will not remove the user associated with this registration (if any) 
      @param registration_id Registration ID
  *)
  val delete_registration : registration_id:string -> t -> unit -> Jsont.json
  
  (** Accept registration 
      @param registration_id Registration ID
  *)
  val accept_registration : registration_id:string -> body:UserRegistrationAcceptOrReject.T.t -> t -> unit -> Jsont.json
  
  (** Reject registration 
      @param registration_id Registration ID
  *)
  val reject_registration : registration_id:string -> body:UserRegistrationAcceptOrReject.T.t -> t -> unit -> Jsont.json
  
  (** Verify a registration email
  
      Following a user registration request, the user will receive an email asking to click a link
  containing a secret.
   
      @param registration_id Registration ID
  *)
  val verify_registration_email : registration_id:string -> t -> unit -> Jsont.json
  
  (** Logout
  
      Revokes your access token and its associated refresh token, destroying your current session. *)
  val revoke_oauth_token : t -> unit -> Jsont.json
  
  (** Login
  
      With your [client id and secret](#operation/getOAuthClient), you can retrieve an access and refresh tokens. *)
  val get_oauth_token : t -> unit -> Jsont.json
  
  (** Get a user 
      @param id Entity id
      @param with_stats include statistics about the user (only available as a moderator/admin)
  *)
  val get_user : id:string -> ?with_stats:string -> t -> unit -> Jsont.json
  
  (** Update a user 
      @param id Entity id
  *)
  val put_user : id:string -> body:UpdateUser.T.t -> t -> unit -> Jsont.json
  
  (** Delete a user 
      @param id Entity id
  *)
  val del_user : id:string -> t -> unit -> Jsont.json
  
  (** Reset password 
      @param id Entity id
  *)
  val post_api_v1_users_reset_password : id:string -> t -> unit -> Jsont.json
  
  (** List token sessions 
      @param id Entity id
  *)
  val get_api_v1_users_token_sessions : id:string -> t -> unit -> Jsont.json
  
  (** List token sessions 
      @param id Entity id
      @param token_session_id Token session Id
  *)
  val get_api_v1_users_token_sessions_revoke : id:string -> token_session_id:string -> t -> unit -> Jsont.json
  
  (** Confirm two factor auth
  
      Confirm a two factor authentication request 
      @param id Entity id
  *)
  val confirm_two_factor_request : id:string -> t -> unit -> Jsont.json
  
  (** Disable two factor auth
  
      Disable two factor authentication of a user 
      @param id Entity id
  *)
  val disable_two_factor : id:string -> t -> unit -> Jsont.json
  
  (** Verify a user
  
      Following a user registration, the new user will receive an email asking to click a link
  containing a secret.
  This endpoint can also be used to verify a new email set in the user account.
   
      @param id Entity id
  *)
  val verify_user : id:string -> t -> unit -> Jsont.json
  
  (** List user exports
  
      **PeerTube >= 6.1** 
      @param user_id User id
  *)
  val list_user_exports : user_id:string -> t -> unit -> Jsont.json
  
  (** Request user export
  
      Request an archive of user data. An email is sent when the archive is ready. 
      @param user_id User id
  *)
  val request_user_export : user_id:string -> t -> unit -> Jsont.json
  
  (** Delete a user export
  
      **PeerTube >= 6.1** 
      @param user_id User id
      @param id Entity id
  *)
  val delete_user_export : user_id:string -> id:string -> t -> unit -> Jsont.json
  
  (** Initialize the resumable user import
  
      **PeerTube >= 6.1** Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to initialize the import of the archive 
      @param user_id User id
  *)
  val user_import_resumable_init : user_id:string -> body:UserImportResumable.T.t -> t -> unit -> Jsont.json
  
  (** Send chunk for the resumable user import
  
      **PeerTube >= 6.1** Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to continue, pause or resume the import of the archive 
      @param user_id User id
      @param upload_id Created session id to proceed with. If you didn't send chunks in the last hour, it is
  not valid anymore and you need to initialize a new upload.
  
  *)
  val user_import_resumable : user_id:string -> upload_id:string -> t -> unit -> Jsont.json
  
  (** Cancel the resumable user import
  
      **PeerTube >= 6.1** Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to cancel the resumable user import 
      @param user_id User id
      @param upload_id Created session id to proceed with. If you didn't send chunks in the last hour, it is
  not valid anymore and you need to initialize a new upload.
  
  *)
  val user_import_resumable_cancel : user_id:string -> upload_id:string -> t -> unit -> Jsont.json
  
  (** Get latest user import
  
      **PeerTube >= 6.1** 
      @param user_id User id
  *)
  val get_latest_user_import : user_id:string -> t -> unit -> Jsont.json
  
  (** Create a synchronization for a video channel *)
  val add_video_channel_sync : body:VideoChannelSync.Create.t -> t -> unit -> Jsont.json
  
  (** Delete a video channel synchronization 
      @param channel_sync_id Channel Sync id
  *)
  val del_video_channel_sync : channel_sync_id:string -> t -> unit -> Jsont.json
  
  (** Triggers the channel synchronization job, fetching all the videos from the remote channel 
      @param channel_sync_id Channel Sync id
  *)
  val trigger_video_channel_sync : channel_sync_id:string -> t -> unit -> Jsont.json
  
  (** Create a video channel *)
  val add_video_channel : body:VideoChannel.Create.t -> t -> unit -> Jsont.json
  
  (** Update a video channel 
      @param channel_handle The video channel handle
  *)
  val put_video_channel : channel_handle:string -> body:VideoChannel.Update.t -> t -> unit -> Jsont.json
  
  (** Delete a video channel 
      @param channel_handle The video channel handle
  *)
  val del_video_channel : channel_handle:string -> t -> unit -> Jsont.json
  
  (** Delete channel avatar 
      @param channel_handle The video channel handle
  *)
  val delete_api_v1_video_channels_avatar : channel_handle:string -> t -> unit -> Jsont.json
  
  (** Update channel avatar 
      @param channel_handle The video channel handle
  *)
  val post_api_v1_video_channels_avatar_pick : channel_handle:string -> t -> unit -> Jsont.json
  
  (** Delete channel banner 
      @param channel_handle The video channel handle
  *)
  val delete_api_v1_video_channels_banner : channel_handle:string -> t -> unit -> Jsont.json
  
  (** Update channel banner 
      @param channel_handle The video channel handle
  *)
  val post_api_v1_video_channels_banner_pick : channel_handle:string -> t -> unit -> Jsont.json
  
  (** *List channel collaborators
  
      **PeerTube >= 8.0** 
      @param channel_handle The video channel handle
  *)
  val list_video_channel_collaborators : channel_handle:string -> t -> unit -> Jsont.json
  
  (** Invite a collaborator
  
      **PeerTube >= 8.0**  Invite a local user to collaborate on the specified video channel. 
      @param channel_handle The video channel handle
  *)
  val invite_video_channel_collaborator : channel_handle:string -> t -> unit -> Jsont.json
  
  (** Remove a channel collaborator
  
      **PeerTube >= 8.0** Only the channel owner or the collaborator themselves can remove a collaborator from a channel 
      @param channel_handle The video channel handle
      @param collaborator_id The collaborator id
  *)
  val remove_video_channel_collaborator : channel_handle:string -> collaborator_id:string -> t -> unit -> Jsont.json
  
  (** Accept a collaboration invitation
  
      **PeerTube >= 8.0** 
      @param channel_handle The video channel handle
      @param collaborator_id The collaborator id
  *)
  val accept_video_channel_collaborator : channel_handle:string -> collaborator_id:string -> t -> unit -> Jsont.json
  
  (** Reject a collaboration invitation
  
      **PeerTube >= 8.0** 
      @param channel_handle The video channel handle
      @param collaborator_id The collaborator id
  *)
  val reject_video_channel_collaborator : channel_handle:string -> collaborator_id:string -> t -> unit -> Jsont.json
  
  (** List followers of a video channel 
      @param channel_handle The video channel handle
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort followers by criteria
      @param search Plain text search, applied to various parts of the model depending on endpoint
  *)
  val get_video_channel_followers : channel_handle:string -> ?start:string -> ?count:string -> ?sort:string -> ?search:string -> t -> unit -> Jsont.json
  
  (** Import videos in channel
  
      Import a remote channel/playlist videos into a channel 
      @param channel_handle The video channel handle
  *)
  val post_api_v1_video_channels_import_videos : channel_handle:string -> body:ImportVideosInChannel.Create.t -> t -> unit -> Jsont.json
  
  (** List playlists of a channel 
      @param channel_handle The video channel handle
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  val get_api_v1_video_channels_video_playlists : channel_handle:string -> ?start:string -> ?count:string -> ?sort:string -> ?playlist_type:string -> t -> unit -> Jsont.json
  
  (** Reorder channel playlists 
      @param channel_handle The video channel handle
  *)
  val reorder_video_playlists_of_channel : channel_handle:string -> t -> unit -> Jsont.json
  
  (** List video playlists 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  val get_playlists : ?start:string -> ?count:string -> ?sort:string -> ?playlist_type:string -> t -> unit -> Jsont.json
  
  (** Create a video playlist
  
      If the video playlist is set as public, `videoChannelId` is mandatory. *)
  val add_playlist : t -> unit -> Jsont.json
  
  (** List available playlist privacy policies *)
  val get_playlist_privacy_policies : t -> unit -> Jsont.json
  
  (** Update a video playlist
  
      If the video playlist is set as public, the playlist must have a assigned channel. 
      @param playlist_id Playlist id
  *)
  val put_api_v1_video_playlists : playlist_id:string -> t -> unit -> Jsont.json
  
  (** Delete a video playlist 
      @param playlist_id Playlist id
  *)
  val delete_api_v1_video_playlists : playlist_id:string -> t -> unit -> Jsont.json
  
  (** List videos of a playlist 
      @param playlist_id Playlist id
      @param start Offset used to paginate results
      @param count Number of items to return
  *)
  val get_video_playlist_videos : playlist_id:string -> ?start:string -> ?count:string -> t -> unit -> Jsont.json
  
  (** Add a video in a playlist 
      @param playlist_id Playlist id
  *)
  val add_video_playlist_video : playlist_id:string -> t -> unit -> Jsont.json
  
  (** Reorder playlist elements 
      @param playlist_id Playlist id
  *)
  val reorder_video_playlist : playlist_id:string -> t -> unit -> Jsont.json
  
  (** Update a playlist element 
      @param playlist_id Playlist id
      @param playlist_element_id Playlist element id
  *)
  val put_video_playlist_video : playlist_id:string -> playlist_element_id:string -> t -> unit -> Jsont.json
  
  (** Delete an element from a playlist 
      @param playlist_id Playlist id
      @param playlist_element_id Playlist element id
  *)
  val del_video_playlist_video : playlist_id:string -> playlist_element_id:string -> t -> unit -> Jsont.json
  
  (** List video blocks 
      @param type_ list only blocks that match this type:
  - `1`: manual block
  - `2`: automatic block that needs review
  
      @param search plain search that will match with video titles, and more
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort blocklists by criteria
  *)
  val get_video_blocks : ?type_:string -> ?search:string -> ?start:string -> ?count:string -> ?sort:string -> t -> unit -> Jsont.json
  
  (** List available video categories *)
  val get_categories : t -> unit -> Jsont.json
  
  (** List instance comments 
      @param search Plain text search, applied to various parts of the model depending on endpoint
      @param search_account Filter comments by searching on the account
      @param search_video Filter comments by searching on the video
      @param video_id Limit results on this specific video
      @param video_channel_id Limit results on this specific video channel
      @param auto_tag_one_of **PeerTube >= 6.2** filter on comments that contain one of these automatic tags
      @param is_local **PeerTube >= 4.0** Display only local or remote objects
      @param on_local_video Display only objects of local or remote videos
  *)
  val get_api_v1_videos_comments : ?search:string -> ?search_account:string -> ?search_video:string -> ?video_id:string -> ?video_channel_id:string -> ?auto_tag_one_of:string -> ?is_local:string -> ?on_local_video:string -> t -> unit -> Jsont.json
  
  (** Delete video import
  
      Delete ended video import 
      @param id Entity id
  *)
  val delete_api_v1_videos_imports : id:string -> t -> unit -> Jsont.json
  
  (** Cancel video import
  
      Cancel a pending video import 
      @param id Entity id
  *)
  val post_api_v1_videos_imports_cancel : id:string -> t -> unit -> Jsont.json
  
  (** Retry video import
  
      **PeerTube >= 8.0** Retry a pending video import 
      @param id Entity id
  *)
  val post_api_v1_videos_imports_retry : id:string -> t -> unit -> Jsont.json
  
  (** List available video languages *)
  val get_languages : t -> unit -> Jsont.json
  
  (** List available video licences *)
  val get_licences : t -> unit -> Jsont.json
  
  (** Update information about a live 
      @param id The object id, uuid or short uuid
  *)
  val update_live_id : id:string -> body:LiveVideo.Update.t -> t -> unit -> Jsont.json
  
  (** List live sessions
  
      List all sessions created in a particular live 
      @param id The object id, uuid or short uuid
      @param sort Sort column
  *)
  val get_api_v1_videos_live_sessions : id:string -> ?sort:string -> t -> unit -> Jsont.json
  
  (** List video ownership changes *)
  val get_api_v1_videos_ownership : t -> unit -> Jsont.json
  
  (** Accept ownership change request 
      @param id The object id, uuid or short uuid
  *)
  val post_api_v1_videos_ownership_accept : id:string -> t -> unit -> Jsont.json
  
  (** Refuse ownership change request 
      @param id The object id, uuid or short uuid
  *)
  val post_api_v1_videos_ownership_refuse : id:string -> t -> unit -> Jsont.json
  
  (** List available video privacy policies *)
  val get_video_privacy_policies : t -> unit -> Jsont.json
  
  (** Initialize the resumable upload of a video
  
      Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to initialize the upload of a video *)
  val upload_resumable_init : body:VideoUploadRequestResumable.T.t -> t -> unit -> Jsont.json
  
  (** Cancel the resumable upload of a video, deleting any data uploaded so far
  
      Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to cancel the upload of a video 
      @param upload_id Created session id to proceed with. If you didn't send chunks in the last hour, it is
  not valid anymore and you need to initialize a new upload.
  
  *)
  val upload_resumable_cancel : upload_id:string -> t -> unit -> Jsont.json
  
  (** Update a video 
      @param id The object id, uuid or short uuid
  *)
  val put_video : id:string -> t -> unit -> Jsont.json
  
  (** Delete a video 
      @param id The object id, uuid or short uuid
  *)
  val del_video : id:string -> t -> unit -> Jsont.json
  
  (** Block a video 
      @param id The object id, uuid or short uuid
  *)
  val add_video_block : id:string -> t -> unit -> Jsont.json
  
  (** Unblock a video by its id 
      @param id The object id, uuid or short uuid
  *)
  val del_video_block : id:string -> t -> unit -> Jsont.json
  
  (** List captions of a video 
      @param id The object id, uuid or short uuid
  *)
  val get_video_captions : id:string -> t -> unit -> Jsont.json
  
  (** Generate a video caption
  
      **PeerTube >= 6.2** This feature has to be enabled by the administrator 
      @param id The object id, uuid or short uuid
  *)
  val generate_video_caption : id:string -> t -> unit -> Jsont.json
  
  (** Add or replace a video caption 
      @param id The object id, uuid or short uuid
      @param caption_language The caption language
  *)
  val add_video_caption : id:string -> caption_language:string -> t -> unit -> Jsont.json
  
  (** Delete a video caption 
      @param id The object id, uuid or short uuid
      @param caption_language The caption language
  *)
  val del_video_caption : id:string -> caption_language:string -> t -> unit -> Jsont.json
  
  (** Replace video chapters
  
      **PeerTube >= 6.0** 
      @param id The object id, uuid or short uuid
  *)
  val replace_video_chapters : id:string -> t -> unit -> Jsont.json
  
  (** Delete a comment or a reply 
      @param id The object id, uuid or short uuid
      @param comment_id The comment id
  *)
  val delete_api_v1_videos_comments : id:string -> comment_id:string -> t -> unit -> Jsont.json
  
  (** Approve a comment
  
      **PeerTube >= 6.2** Approve a comment that requires a review 
      @param id The object id, uuid or short uuid
      @param comment_id The comment id
  *)
  val post_api_v1_videos_comments_approve : id:string -> comment_id:string -> t -> unit -> Jsont.json
  
  (** Get complete video description 
      @param id The object id, uuid or short uuid
  *)
  val get_video_desc : id:string -> t -> unit -> Jsont.json
  
  (** Request ownership change 
      @param id The object id, uuid or short uuid
  *)
  val post_api_v1_videos_give_ownership : id:string -> t -> unit -> Jsont.json
  
  (** Delete video HLS files 
      @param id The object id, uuid or short uuid
  *)
  val del_video_hls : id:string -> t -> unit -> Jsont.json
  
  (** List video passwords
  
      **PeerTube >= 6.0** 
      @param id The object id, uuid or short uuid
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  val list_video_passwords : id:string -> ?start:string -> ?count:string -> ?sort:string -> t -> unit -> Jsont.json
  
  (** Add a video password
  
      **PeerTube >= 8.0** 
      @param id The object id, uuid or short uuid
  *)
  val add_video_password : id:string -> t -> unit -> Jsont.json
  
  (** Update video passwords
  
      **PeerTube >= 6.0** 
      @param id The object id, uuid or short uuid
  *)
  val update_video_password_list : id:string -> t -> unit -> Jsont.json
  
  (** Delete a video password
  
      **PeerTube >= 6.0** 
      @param id The object id, uuid or short uuid
      @param video_password_id The video password id
  *)
  val remove_video_password : id:string -> video_password_id:string -> t -> unit -> Jsont.json
  
  (** Like/dislike a video 
      @param id The object id, uuid or short uuid
  *)
  val put_api_v1_videos_rate : id:string -> t -> unit -> Jsont.json
  
  (** Delete video source file 
      @param id The object id, uuid or short uuid
  *)
  val delete_video_source_file : id:string -> t -> unit -> Jsont.json
  
  (** Initialize the resumable replacement of a video
  
      **PeerTube >= 6.0** Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to initialize the replacement of a video 
      @param id The object id, uuid or short uuid
  *)
  val replace_video_source_resumable_init : id:string -> body:VideoReplaceSourceRequestResumable.T.t -> t -> unit -> Jsont.json
  
  (** Send chunk for the resumable replacement of a video
  
      **PeerTube >= 6.0** Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to continue, pause or resume the replacement of a video 
      @param id The object id, uuid or short uuid
      @param upload_id Created session id to proceed with. If you didn't send chunks in the last hour, it is
  not valid anymore and you need to initialize a new upload.
  
  *)
  val replace_video_source_resumable : id:string -> upload_id:string -> t -> unit -> Jsont.json
  
  (** Cancel the resumable replacement of a video
  
      **PeerTube >= 6.0** Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to cancel the replacement of a video 
      @param id The object id, uuid or short uuid
      @param upload_id Created session id to proceed with. If you didn't send chunks in the last hour, it is
  not valid anymore and you need to initialize a new upload.
  
  *)
  val replace_video_source_resumable_cancel : id:string -> upload_id:string -> t -> unit -> Jsont.json
  
  (** List storyboards of a video
  
      **PeerTube >= 6.0** 
      @param id The object id, uuid or short uuid
  *)
  val list_video_storyboards : id:string -> t -> unit -> Jsont.json
  
  (** Create a studio task
  
      Create a task to edit a video  (cut, add intro/outro etc) 
      @param id The object id, uuid or short uuid
  *)
  val post_api_v1_videos_studio_edit : id:string -> t -> unit -> Jsont.json
  
  (** Create a transcoding job 
      @param id The object id, uuid or short uuid
  *)
  val create_video_transcoding : id:string -> t -> unit -> Jsont.json
  
  (** Notify user is watching a video
  
      Call this endpoint regularly (every 5-10 seconds for example) to notify the server the user is watching the video. After a while, PeerTube will increase video's viewers counter. If the user is authenticated, PeerTube will also store the current player time. 
      @param id The object id, uuid or short uuid
  *)
  val add_view : id:string -> body:UserViewingVideo.T.t -> t -> unit -> Jsont.json
  
  (** Delete video Web Video files
  
      **PeerTube >= 6.0** 
      @param id The object id, uuid or short uuid
  *)
  val del_video_web_videos : id:string -> t -> unit -> Jsont.json
  
  (** List account watched words
  
      **PeerTube >= 6.2** 
      @param account_name account name to list watched words
  *)
  val get_api_v1_watched_words_accounts_lists : account_name:string -> t -> unit -> Jsont.json
  
  (** Add account watched words
  
      **PeerTube >= 6.2** *)
  val post_api_v1_watched_words_accounts_lists : account_name:string -> t -> unit -> Jsont.json
  
  (** Update account watched words
  
      **PeerTube >= 6.2** 
      @param list_id list of watched words to update
  *)
  val put_api_v1_watched_words_accounts_lists : account_name:string -> list_id:string -> t -> unit -> Jsont.json
  
  (** Delete account watched words
  
      **PeerTube >= 6.2** 
      @param list_id list of watched words to delete
  *)
  val delete_api_v1_watched_words_accounts_lists : account_name:string -> list_id:string -> t -> unit -> Jsont.json
  
  (** List server watched words
  
      **PeerTube >= 6.2** *)
  val get_api_v1_watched_words_server_lists : t -> unit -> Jsont.json
  
  (** Add server watched words
  
      **PeerTube >= 6.2** *)
  val post_api_v1_watched_words_server_lists : t -> unit -> Jsont.json
  
  (** Update server watched words
  
      **PeerTube >= 6.2** 
      @param list_id list of watched words to update
  *)
  val put_api_v1_watched_words_server_lists : list_id:string -> t -> unit -> Jsont.json
  
  (** Delete server watched words
  
      **PeerTube >= 6.2** 
      @param list_id list of watched words to delete
  *)
  val delete_api_v1_watched_words_server_lists : list_id:string -> t -> unit -> Jsont.json
  
  (** Download video file
  
      Generate a mp4 container that contains at most 1 video stream and at most 1 audio stream. Mainly used to merge the HLS audio only video file and the HLS video only resolution file. 
      @param video_id The video id
      @param video_file_ids streams of video files to mux in the output
      @param video_file_token Video file token [generated](#operation/requestVideoToken) by PeerTube so you don't need to provide an OAuth token in the request header.
  *)
  val get_download_videos_generate : video_id:string -> video_file_ids:string -> ?video_file_token:string -> t -> unit -> Jsont.json
  
  (** Videos podcast feed 
      @param video_channel_id Limit listing to a specific video channel
  *)
  val get_videos_podcast_feed : video_channel_id:string -> t -> unit -> Jsont.json
  
  (** Videos of subscriptions feeds 
      @param format format expected (we focus on making `rss` the most feature-rich ; it serves [Media RSS](https://www.rssboard.org/media-rss))
      @param account_id limit listing to a specific account
      @param token private token allowing access
      @param sort Sort column
      @param nsfw whether to include nsfw videos, if any
      @param is_local **PeerTube >= 4.0** Display only local or remote objects
      @param include_ **Only administrators and moderators can use this parameter**
  
  Include additional videos in results (can be combined using bitwise or operator)
  - `0` NONE
  - `1` NOT_PUBLISHED_STATE
  - `2` BLACKLISTED
  - `4` BLOCKED_OWNER
  - `8` FILES
  - `16` CAPTIONS
  - `32` VIDEO SOURCE
  
      @param privacy_one_of **PeerTube >= 4.0** Display only videos in this specific privacy/privacies
      @param has_hlsfiles **PeerTube >= 4.0** Display only videos that have HLS files
      @param has_web_video_files **PeerTube >= 6.0** Display only videos that have Web Video files
  *)
  val get_syndicated_subscription_videos : format:string -> account_id:string -> token:string -> ?sort:string -> ?nsfw:string -> ?is_local:string -> ?include_:string -> ?privacy_one_of:string -> ?has_hlsfiles:string -> ?has_web_video_files:string -> t -> unit -> Jsont.json
  
  (** Comments on videos feeds 
      @param format format expected (we focus on making `rss` the most feature-rich ; it serves [Media RSS](https://www.rssboard.org/media-rss))
      @param video_id limit listing comments to a specific video
      @param account_id limit listing comments to videos of a specific account
      @param account_name limit listing comments to videos of a specific account
      @param video_channel_id limit listing comments to videos of a specific video channel
      @param video_channel_name limit listing comments to videos of a specific video channel
  *)
  val get_syndicated_comments : format:string -> ?video_id:string -> ?account_id:string -> ?account_name:string -> ?video_channel_id:string -> ?video_channel_name:string -> t -> unit -> Jsont.json
  
  (** Common videos feeds 
      @param format format expected (we focus on making `rss` the most feature-rich ; it serves [Media RSS](https://www.rssboard.org/media-rss))
      @param account_id limit listing to a specific account
      @param account_name limit listing to a specific account
      @param video_channel_id limit listing to a specific video channel
      @param video_channel_name limit listing to a specific video channel
      @param sort Sort column
      @param nsfw whether to include nsfw videos, if any
      @param is_local **PeerTube >= 4.0** Display only local or remote objects
      @param include_ **Only administrators and moderators can use this parameter**
  
  Include additional videos in results (can be combined using bitwise or operator)
  - `0` NONE
  - `1` NOT_PUBLISHED_STATE
  - `2` BLACKLISTED
  - `4` BLOCKED_OWNER
  - `8` FILES
  - `16` CAPTIONS
  - `32` VIDEO SOURCE
  
      @param privacy_one_of **PeerTube >= 4.0** Display only videos in this specific privacy/privacies
      @param has_hlsfiles **PeerTube >= 4.0** Display only videos that have HLS files
      @param has_web_video_files **PeerTube >= 6.0** Display only videos that have Web Video files
  *)
  val get_syndicated_videos : format:string -> ?account_id:string -> ?account_name:string -> ?video_channel_id:string -> ?video_channel_name:string -> ?sort:string -> ?nsfw:string -> ?is_local:string -> ?include_:string -> ?privacy_one_of:string -> ?has_hlsfiles:string -> ?has_web_video_files:string -> t -> unit -> Jsont.json
  
  (** Get private HLS video file 
      @param filename Filename
      @param video_file_token Video file token [generated](#operation/requestVideoToken) by PeerTube so you don't need to provide an OAuth token in the request header.
      @param reinject_video_file_token Ask the server to reinject videoFileToken in URLs in m3u8 playlist
  *)
  val get_static_streaming_playlists_hls_private_ : filename:string -> ?video_file_token:string -> ?reinject_video_file_token:string -> t -> unit -> Jsont.json
  
  (** Get public HLS video file 
      @param filename Filename
  *)
  val get_static_streaming_playlists_hls : filename:string -> t -> unit -> Jsont.json
  
  (** Get private Web Video file
  
      **PeerTube >= 6.0** 
      @param filename Filename
      @param video_file_token Video file token [generated](#operation/requestVideoToken) by PeerTube so you don't need to provide an OAuth token in the request header.
  *)
  val get_static_web_videos_private_ : filename:string -> ?video_file_token:string -> t -> unit -> Jsont.json
  
  (** Get public Web Video file
  
      **PeerTube >= 6.0** 
      @param filename Filename
  *)
  val get_static_web_videos : filename:string -> t -> unit -> Jsont.json
end

module VideoChannelList : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?data:VideoChannel.T.t list -> ?total:int -> unit -> t
    
    val data : t -> VideoChannel.T.t list option
    
    val total : t -> int option
    
    val jsont : t Jsont.t
  end
  
  (** List video channels of an account 
      @param name The username or handle of the account
      @param with_stats include daily view statistics for the last 30 days and total views (only if authenticated as the account user)
      @param start Offset used to paginate results
      @param count Number of items to return
      @param search Plain text search, applied to various parts of the model depending on endpoint
      @param sort Sort column
      @param include_collaborations **PeerTube >= 8.0** Include objects from collaborated channels
  *)
  val get_api_v1_accounts_video_channels : name:string -> ?with_stats:string -> ?start:string -> ?count:string -> ?search:string -> ?sort:string -> ?include_collaborations:string -> t -> unit -> T.t
  
  (** Search channels 
      @param search String to search. If the user can make a remote URI search, and the string is an URI then the PeerTube instance will fetch the remote object and add it to its database. Then, you can use the REST API to fetch the complete channel information and interact with it.
  
      @param start Offset used to paginate results
      @param count Number of items to return
      @param search_target If the administrator enabled search index support, you can override the default search target.
  
  **Warning**: If you choose to make an index search, PeerTube will get results from a third party service. It means the instance may not yet know the objects you fetched. If you want to load video/channel information:
    * If the current user has the ability to make a remote URI search (this information is available in the config endpoint),
    then reuse the search API to make a search using the object URI so PeerTube instance fetches the remote object and fill its database.
    After that, you can use the classic REST API endpoints to fetch the complete object or interact with it
    * If the current user doesn't have the ability to make a remote URI search, then redirect the user on the origin instance or fetch
    the data from the origin instance API
  
      @param sort Sort column
      @param host Find elements owned by this host
      @param handles Find elements with these handles
  *)
  val search_channels : search:string -> ?start:string -> ?count:string -> ?search_target:string -> ?sort:string -> ?host:string -> ?handles:string -> t -> unit -> T.t
  
  (** List my user subscriptions 
      @param start Offset used to paginate results
      @param count Number of items to return
  *)
  val get_api_v1_users_me_subscriptions : ?start:string -> ?count:string -> ?sort:string -> t -> unit -> T.t
  
  (** List video channels 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  val get_video_channels : ?start:string -> ?count:string -> ?sort:string -> t -> unit -> T.t
end

module UserWithStats : sig
  module T : sig
    type t
    
    (** Construct a value
        @param auto_play_next_video Automatically start playing the upcoming video after the currently playing video
        @param auto_play_next_video_playlist Automatically start playing the video on the playlist after the currently playing video
        @param auto_play_video Automatically start playing the video on the watch page
        @param email The user email
        @param email_public Has the user accepted to display the email publicly?
        @param email_verified Has the user confirmed their email address?
        @param language default language for this user
        @param new_features_info_read New features information the user has read
        @param p2p_enabled whether to enable P2P in the player or not
        @param plugin_auth Auth plugin to use to authenticate the user
        @param theme Theme enabled by this user
        @param two_factor_enabled Whether the user has enabled two-factor authentication or not
        @param video_languages list of languages to filter videos down to
        @param video_quota The user video quota in bytes
        @param video_quota_daily The user daily video quota in bytes
        @param videos_history_enabled whether to keep track of watched history or not
        @param videos_count Count of videos published
        @param abuses_count Count of reports/abuses of which the user is a target
        @param abuses_accepted_count Count of reports/abuses created by the user and accepted/acted upon by the moderation team
        @param abuses_created_count Count of reports/abuses created by the user
        @param video_comments_count Count of comments published
    *)
    val v : ?account:Account.T.t -> ?admin_flags:UserAdminFlags.T.t -> ?auto_play_next_video:bool -> ?auto_play_next_video_playlist:bool -> ?auto_play_video:bool -> ?blocked:bool -> ?blocked_reason:string -> ?created_at:string -> ?email:string -> ?email_public:bool -> ?email_verified:bool -> ?id:Id.T.t -> ?language:string -> ?last_login_date:Ptime.t -> ?new_features_info_read:float -> ?no_account_setup_warning_modal:bool -> ?no_instance_config_warning_modal:bool -> ?no_welcome_modal:bool -> ?notification_settings:UserNotificationSettings.T.t -> ?nsfw_flags_blurred:Nsfwflag.T.t -> ?nsfw_flags_displayed:Nsfwflag.T.t -> ?nsfw_flags_hidden:Nsfwflag.T.t -> ?nsfw_flags_warned:Nsfwflag.T.t -> ?nsfw_policy:Nsfwpolicy.T.t -> ?p2p_enabled:bool -> ?plugin_auth:string -> ?role:Jsont.json -> ?theme:string -> ?two_factor_enabled:bool -> ?username:Username.T.t -> ?video_channels:VideoChannel.T.t list -> ?video_languages:string list -> ?video_quota:int -> ?video_quota_daily:int -> ?videos_history_enabled:bool -> ?videos_count:int -> ?abuses_count:int -> ?abuses_accepted_count:int -> ?abuses_created_count:int -> ?video_comments_count:int -> unit -> t
    
    val account : t -> Account.T.t option
    
    val admin_flags : t -> UserAdminFlags.T.t option
    
    (** Automatically start playing the upcoming video after the currently playing video *)
    val auto_play_next_video : t -> bool option
    
    (** Automatically start playing the video on the playlist after the currently playing video *)
    val auto_play_next_video_playlist : t -> bool option
    
    (** Automatically start playing the video on the watch page *)
    val auto_play_video : t -> bool option
    
    val blocked : t -> bool option
    
    val blocked_reason : t -> string option
    
    val created_at : t -> string option
    
    (** The user email *)
    val email : t -> string option
    
    (** Has the user accepted to display the email publicly? *)
    val email_public : t -> bool option
    
    (** Has the user confirmed their email address? *)
    val email_verified : t -> bool option
    
    val id : t -> Id.T.t option
    
    (** default language for this user *)
    val language : t -> string option
    
    val last_login_date : t -> Ptime.t option
    
    (** New features information the user has read *)
    val new_features_info_read : t -> float option
    
    val no_account_setup_warning_modal : t -> bool option
    
    val no_instance_config_warning_modal : t -> bool option
    
    val no_welcome_modal : t -> bool option
    
    val notification_settings : t -> UserNotificationSettings.T.t option
    
    val nsfw_flags_blurred : t -> Nsfwflag.T.t option
    
    val nsfw_flags_displayed : t -> Nsfwflag.T.t option
    
    val nsfw_flags_hidden : t -> Nsfwflag.T.t option
    
    val nsfw_flags_warned : t -> Nsfwflag.T.t option
    
    val nsfw_policy : t -> Nsfwpolicy.T.t option
    
    (** whether to enable P2P in the player or not *)
    val p2p_enabled : t -> bool option
    
    (** Auth plugin to use to authenticate the user *)
    val plugin_auth : t -> string option
    
    val role : t -> Jsont.json option
    
    (** Theme enabled by this user *)
    val theme : t -> string option
    
    (** Whether the user has enabled two-factor authentication or not *)
    val two_factor_enabled : t -> bool option
    
    val username : t -> Username.T.t option
    
    val video_channels : t -> VideoChannel.T.t list option
    
    (** list of languages to filter videos down to *)
    val video_languages : t -> string list option
    
    (** The user video quota in bytes *)
    val video_quota : t -> int option
    
    (** The user daily video quota in bytes *)
    val video_quota_daily : t -> int option
    
    (** whether to keep track of watched history or not *)
    val videos_history_enabled : t -> bool option
    
    (** Count of videos published *)
    val videos_count : t -> int option
    
    (** Count of reports/abuses of which the user is a target *)
    val abuses_count : t -> int option
    
    (** Count of reports/abuses created by the user and accepted/acted upon by the moderation team *)
    val abuses_accepted_count : t -> int option
    
    (** Count of reports/abuses created by the user *)
    val abuses_created_count : t -> int option
    
    (** Count of comments published *)
    val video_comments_count : t -> int option
    
    val jsont : t Jsont.t
  end
end

module User : sig
  module T : sig
    type t
    
    (** Construct a value
        @param auto_play_next_video Automatically start playing the upcoming video after the currently playing video
        @param auto_play_next_video_playlist Automatically start playing the video on the playlist after the currently playing video
        @param auto_play_video Automatically start playing the video on the watch page
        @param email The user email
        @param email_public Has the user accepted to display the email publicly?
        @param email_verified Has the user confirmed their email address?
        @param language default language for this user
        @param new_features_info_read New features information the user has read
        @param p2p_enabled whether to enable P2P in the player or not
        @param plugin_auth Auth plugin to use to authenticate the user
        @param theme Theme enabled by this user
        @param two_factor_enabled Whether the user has enabled two-factor authentication or not
        @param video_languages list of languages to filter videos down to
        @param video_quota The user video quota in bytes
        @param video_quota_daily The user daily video quota in bytes
        @param videos_history_enabled whether to keep track of watched history or not
    *)
    val v : ?account:Account.T.t -> ?admin_flags:UserAdminFlags.T.t -> ?auto_play_next_video:bool -> ?auto_play_next_video_playlist:bool -> ?auto_play_video:bool -> ?blocked:bool -> ?blocked_reason:string -> ?created_at:string -> ?email:string -> ?email_public:bool -> ?email_verified:bool -> ?id:Id.T.t -> ?language:string -> ?last_login_date:Ptime.t -> ?new_features_info_read:float -> ?no_account_setup_warning_modal:bool -> ?no_instance_config_warning_modal:bool -> ?no_welcome_modal:bool -> ?notification_settings:UserNotificationSettings.T.t -> ?nsfw_flags_blurred:Nsfwflag.T.t -> ?nsfw_flags_displayed:Nsfwflag.T.t -> ?nsfw_flags_hidden:Nsfwflag.T.t -> ?nsfw_flags_warned:Nsfwflag.T.t -> ?nsfw_policy:Nsfwpolicy.T.t -> ?p2p_enabled:bool -> ?plugin_auth:string -> ?role:Jsont.json -> ?theme:string -> ?two_factor_enabled:bool -> ?username:Username.T.t -> ?video_channels:VideoChannel.T.t list -> ?video_languages:string list -> ?video_quota:int -> ?video_quota_daily:int -> ?videos_history_enabled:bool -> unit -> t
    
    val account : t -> Account.T.t option
    
    val admin_flags : t -> UserAdminFlags.T.t option
    
    (** Automatically start playing the upcoming video after the currently playing video *)
    val auto_play_next_video : t -> bool option
    
    (** Automatically start playing the video on the playlist after the currently playing video *)
    val auto_play_next_video_playlist : t -> bool option
    
    (** Automatically start playing the video on the watch page *)
    val auto_play_video : t -> bool option
    
    val blocked : t -> bool option
    
    val blocked_reason : t -> string option
    
    val created_at : t -> string option
    
    (** The user email *)
    val email : t -> string option
    
    (** Has the user accepted to display the email publicly? *)
    val email_public : t -> bool option
    
    (** Has the user confirmed their email address? *)
    val email_verified : t -> bool option
    
    val id : t -> Id.T.t option
    
    (** default language for this user *)
    val language : t -> string option
    
    val last_login_date : t -> Ptime.t option
    
    (** New features information the user has read *)
    val new_features_info_read : t -> float option
    
    val no_account_setup_warning_modal : t -> bool option
    
    val no_instance_config_warning_modal : t -> bool option
    
    val no_welcome_modal : t -> bool option
    
    val notification_settings : t -> UserNotificationSettings.T.t option
    
    val nsfw_flags_blurred : t -> Nsfwflag.T.t option
    
    val nsfw_flags_displayed : t -> Nsfwflag.T.t option
    
    val nsfw_flags_hidden : t -> Nsfwflag.T.t option
    
    val nsfw_flags_warned : t -> Nsfwflag.T.t option
    
    val nsfw_policy : t -> Nsfwpolicy.T.t option
    
    (** whether to enable P2P in the player or not *)
    val p2p_enabled : t -> bool option
    
    (** Auth plugin to use to authenticate the user *)
    val plugin_auth : t -> string option
    
    val role : t -> Jsont.json option
    
    (** Theme enabled by this user *)
    val theme : t -> string option
    
    (** Whether the user has enabled two-factor authentication or not *)
    val two_factor_enabled : t -> bool option
    
    val username : t -> Username.T.t option
    
    val video_channels : t -> VideoChannel.T.t list option
    
    (** list of languages to filter videos down to *)
    val video_languages : t -> string list option
    
    (** The user video quota in bytes *)
    val video_quota : t -> int option
    
    (** The user daily video quota in bytes *)
    val video_quota_daily : t -> int option
    
    (** whether to keep track of watched history or not *)
    val videos_history_enabled : t -> bool option
    
    val jsont : t Jsont.t
  end
  
  (** List users 
      @param search Plain text search that will match with user usernames or emails
      @param blocked Filter results down to (un)banned users
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort users by criteria
  *)
  val get_users : ?search:string -> ?blocked:string -> ?start:string -> ?count:string -> ?sort:string -> t -> unit -> T.t
  
  (** Get my user information *)
  val get_user_info : t -> unit -> T.t
end

module AbuseStateSet : sig
  module T : sig
    (** The abuse state (Pending = `1`, Rejected = `2`, Accepted = `3`) *)
    type t = string
    
    val jsont : t Jsont.t
  end
end

module AbuseStateConstant : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?id:AbuseStateSet.T.t -> ?label:string -> unit -> t
    
    val id : t -> AbuseStateSet.T.t option
    
    val label : t -> string option
    
    val jsont : t Jsont.t
  end
end

module AbusePredefinedReasons : sig
  module T : sig
    type t = Jsont.json
    
    val jsont : t Jsont.t
    
    val v : unit -> t
  end
end

module Abuse : sig
  module T : sig
    type t
    
    (** Construct a value *)
    val v : ?created_at:Ptime.t -> ?id:Id.T.t -> ?moderation_comment:string -> ?predefined_reasons:AbusePredefinedReasons.T.t -> ?reason:string -> ?reporter_account:Account.T.t -> ?state:AbuseStateConstant.T.t -> ?video:Jsont.json -> unit -> t
    
    val created_at : t -> Ptime.t option
    
    val id : t -> Id.T.t option
    
    val moderation_comment : t -> string option
    
    val predefined_reasons : t -> AbusePredefinedReasons.T.t option
    
    val reason : t -> string option
    
    val reporter_account : t -> Account.T.t option
    
    val state : t -> AbuseStateConstant.T.t option
    
    val video : t -> Jsont.json option
    
    val jsont : t Jsont.t
  end
end
