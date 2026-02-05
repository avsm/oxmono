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

type t = {
  session : Requests.t;
  base_url : string;
}

let create ?session ~sw env ~base_url =
  let session = match session with
    | Some s -> s
    | None -> Requests.create ~sw env
  in
  { session; base_url }

let base_url t = t.base_url
let session t = t.session

module VideosForXml = struct
  module Types = struct
    module T = struct
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module VideoUpload = struct
  module Types = struct
    module Response = struct
      type t = {
        video : Jsont.json option;
      }
    end
  end
  
  module Response = struct
    include Types.Response
    
    let v ?video () = { video }
    
    let video t = t.video
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoUploadResponse"
        (fun video -> { video })
      |> Jsont.Object.opt_mem "video" Jsont.json ~enc:(fun r -> r.video)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Import a video
  
      Import a torrent or magnetURI or HTTP resource (if enabled by the instance administrator) *)
  let import_video client () =
    let op_name = "import_video" in
    let url_path = "/api/v1/videos/imports" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create a live *)
  let add_live client () =
    let op_name = "add_live" in
    let url_path = "/api/v1/videos/live" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Upload a video
  
      Uses a single request to upload a video. *)
  let upload_legacy client () =
    let op_name = "upload_legacy" in
    let url_path = "/api/v1/videos/upload" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Send chunk for the resumable upload of a video
  
      Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to continue, pause or resume the upload of a video 
      @param upload_id Created session id to proceed with. If you didn't send chunks in the last hour, it is
  not valid anymore and you need to initialize a new upload.
  
  *)
  let upload_resumable ~upload_id client () =
    let op_name = "upload_resumable" in
    let url_path = "/api/v1/videos/upload-resumable" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"upload_id" ~value:upload_id]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module VideoToken = struct
  module Types = struct
    module Response = struct
      type t = {
        files : Jsont.json option;
      }
    end
  end
  
  module Response = struct
    include Types.Response
    
    let v ?files () = { files }
    
    let files t = t.files
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoTokenResponse"
        (fun files -> { files })
      |> Jsont.Object.opt_mem "files" Jsont.json ~enc:(fun r -> r.files)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Request video token
  
      Request special tokens that expire quickly to use them in some context (like accessing private static files) 
      @param id The object id, uuid or short uuid
  *)
  let request_video_token ~id client () =
    let op_name = "request_video_token" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/token" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module VideoStudioCreateTask = struct
  module Types = struct
    module T = struct
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module VideoStatsUserAgentDevice = struct
  module Types = struct
    module T = struct
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
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"VideoStatsUserAgentDevice"
        ~dec:(function
          | "console" -> `Console
          | "embedded" -> `Embedded
          | "mobile" -> `Mobile
          | "smarttv" -> `Smarttv
          | "tablet" -> `Tablet
          | "wearable" -> `Wearable
          | "xr" -> `Xr
          | "desktop" -> `Desktop
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Console -> "console"
          | `Embedded -> "embedded"
          | `Mobile -> "mobile"
          | `Smarttv -> "smarttv"
          | `Tablet -> "tablet"
          | `Wearable -> "wearable"
          | `Xr -> "xr"
          | `Desktop -> "desktop")
  end
end

module UserViewingVideo = struct
  module Types = struct
    module T = struct
      type t = {
        client : string option;  (** Client software used to watch the video. For example "Firefox", "PeerTube Approval Android", etc.
       *)
        current_time : int;  (** timestamp within the video, in seconds *)
        device : VideoStatsUserAgentDevice.T.t option;  (** Device used to watch the video. For example "desktop", "mobile", "smarttv", etc.
       *)
        operating_system : string option;  (** Operating system used to watch the video. For example "Windows", "Ubuntu", etc.
       *)
        session_id : string option;  (** Optional param to represent the current viewer session. Used by the backend to properly count one view per session per video. PeerTube admin can configure the server to not trust this `sessionId` parameter but use the request IP address instead to identify a viewer.
       *)
        view_event : string option;  (** Event since last viewing call:
       * `seek` - If the user seeked the video
       *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~current_time ?client ?device ?operating_system ?session_id ?view_event () = { client; current_time; device; operating_system; session_id; view_event }
    
    let client t = t.client
    let current_time t = t.current_time
    let device t = t.device
    let operating_system t = t.operating_system
    let session_id t = t.session_id
    let view_event t = t.view_event
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"UserViewingVideo"
        (fun client current_time device operating_system session_id view_event -> { client; current_time; device; operating_system; session_id; view_event })
      |> Jsont.Object.opt_mem "client" Jsont.string ~enc:(fun r -> r.client)
      |> Jsont.Object.mem "currentTime" Jsont.int ~enc:(fun r -> r.current_time)
      |> Jsont.Object.opt_mem "device" VideoStatsUserAgentDevice.T.jsont ~enc:(fun r -> r.device)
      |> Jsont.Object.opt_mem "operatingSystem" Jsont.string ~enc:(fun r -> r.operating_system)
      |> Jsont.Object.opt_mem "sessionId" Jsont.string ~enc:(fun r -> r.session_id)
      |> Jsont.Object.opt_mem "viewEvent" Jsont.string ~enc:(fun r -> r.view_event)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoStatsUserAgent = struct
  module Types = struct
    module T = struct
      type t = {
        clients : Jsont.json list option;
        devices : Jsont.json list option;
        operating_system : Jsont.json list option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?clients ?devices ?operating_system () = { clients; devices; operating_system }
    
    let clients t = t.clients
    let devices t = t.devices
    let operating_system t = t.operating_system
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoStatsUserAgent"
        (fun clients devices operating_system -> { clients; devices; operating_system })
      |> Jsont.Object.opt_mem "clients" (Jsont.list Jsont.json) ~enc:(fun r -> r.clients)
      |> Jsont.Object.opt_mem "devices" (Jsont.list Jsont.json) ~enc:(fun r -> r.devices)
      |> Jsont.Object.opt_mem "operatingSystem" (Jsont.list Jsont.json) ~enc:(fun r -> r.operating_system)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get user agent stats of a video 
      @param id The object id, uuid or short uuid
      @param start_date Filter stats by start date
      @param end_date Filter stats by end date
  *)
  let get_api_v1_videos_stats_user_agent ~id ?start_date ?end_date client () =
    let op_name = "get_api_v1_videos_stats_user_agent" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/stats/user-agent" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"startDate" ~value:start_date; Openapi.Runtime.Query.optional ~key:"endDate" ~value:end_date]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module VideoStatsTimeserie = struct
  module Types = struct
    module T = struct
      type t = {
        data : Jsont.json list option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?data () = { data }
    
    let data t = t.data
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoStatsTimeserie"
        (fun data -> { data })
      |> Jsont.Object.opt_mem "data" (Jsont.list Jsont.json) ~enc:(fun r -> r.data)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get timeserie stats of a video 
      @param id The object id, uuid or short uuid
      @param metric The metric to get
      @param start_date Filter stats by start date
      @param end_date Filter stats by end date
  *)
  let get_api_v1_videos_stats_timeseries ~id ~metric ?start_date ?end_date client () =
    let op_name = "get_api_v1_videos_stats_timeseries" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id); ("metric", metric)] "/api/v1/videos/{id}/stats/timeseries/{metric}" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"startDate" ~value:start_date; Openapi.Runtime.Query.optional ~key:"endDate" ~value:end_date]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module VideoStatsRetention = struct
  module Types = struct
    module T = struct
      type t = {
        data : Jsont.json list option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?data () = { data }
    
    let data t = t.data
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoStatsRetention"
        (fun data -> { data })
      |> Jsont.Object.opt_mem "data" (Jsont.list Jsont.json) ~enc:(fun r -> r.data)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get retention stats of a video 
      @param id The object id, uuid or short uuid
  *)
  let get_api_v1_videos_stats_retention ~id client () =
    let op_name = "get_api_v1_videos_stats_retention" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/stats/retention" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module VideoStatsOverall = struct
  module Types = struct
    module T = struct
      type t = {
        average_watch_time : float option;
        countries : Jsont.json list option;
        subdivisions : Jsont.json list option;
        total_viewers : float option;
        total_watch_time : float option;
        viewers_peak : float option;
        viewers_peak_date : Ptime.t option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?average_watch_time ?countries ?subdivisions ?total_viewers ?total_watch_time ?viewers_peak ?viewers_peak_date () = { average_watch_time; countries; subdivisions; total_viewers; total_watch_time; viewers_peak; viewers_peak_date }
    
    let average_watch_time t = t.average_watch_time
    let countries t = t.countries
    let subdivisions t = t.subdivisions
    let total_viewers t = t.total_viewers
    let total_watch_time t = t.total_watch_time
    let viewers_peak t = t.viewers_peak
    let viewers_peak_date t = t.viewers_peak_date
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoStatsOverall"
        (fun average_watch_time countries subdivisions total_viewers total_watch_time viewers_peak viewers_peak_date -> { average_watch_time; countries; subdivisions; total_viewers; total_watch_time; viewers_peak; viewers_peak_date })
      |> Jsont.Object.opt_mem "averageWatchTime" Jsont.number ~enc:(fun r -> r.average_watch_time)
      |> Jsont.Object.opt_mem "countries" (Jsont.list Jsont.json) ~enc:(fun r -> r.countries)
      |> Jsont.Object.opt_mem "subdivisions" (Jsont.list Jsont.json) ~enc:(fun r -> r.subdivisions)
      |> Jsont.Object.opt_mem "totalViewers" Jsont.number ~enc:(fun r -> r.total_viewers)
      |> Jsont.Object.opt_mem "totalWatchTime" Jsont.number ~enc:(fun r -> r.total_watch_time)
      |> Jsont.Object.opt_mem "viewersPeak" Jsont.number ~enc:(fun r -> r.viewers_peak)
      |> Jsont.Object.opt_mem "viewersPeakDate" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.viewers_peak_date)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get overall stats of a video 
      @param id The object id, uuid or short uuid
      @param start_date Filter stats by start date
      @param end_date Filter stats by end date
  *)
  let get_api_v1_videos_stats_overall ~id ?start_date ?end_date client () =
    let op_name = "get_api_v1_videos_stats_overall" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/stats/overall" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"startDate" ~value:start_date; Openapi.Runtime.Query.optional ~key:"endDate" ~value:end_date]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module VideoStateConstant = struct
  module Types = struct
    module T = struct
      type t = {
        id : int option;  (** The video state:
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
        label : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?id ?label () = { id; label }
    
    let id t = t.id
    let label t = t.label
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoStateConstant"
        (fun id label -> { id; label })
      |> Jsont.Object.opt_mem "id" Jsont.int ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "label" Jsont.string ~enc:(fun r -> r.label)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoResolutionSet = struct
  module Types = struct
    module T = struct
      (** Video resolution (`0`, `240`, `360`, `720`, `1080`, `1440` or `2160`)
      
      `0` is used as a special value for stillimage videos dedicated to audio, a.k.a. audio-only videos.
       *)
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module VideoResolutionConstant = struct
  module Types = struct
    module T = struct
      (** resolutions and their labels for the video *)
      type t = {
        id : VideoResolutionSet.T.t option;
        label : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?id ?label () = { id; label }
    
    let id t = t.id
    let label t = t.label
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoResolutionConstant"
        (fun id label -> { id; label })
      |> Jsont.Object.opt_mem "id" VideoResolutionSet.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "label" Jsont.string ~enc:(fun r -> r.label)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoSource = struct
  module Types = struct
    module T = struct
      type t = {
        created_at : Ptime.t option;
        file_download_url : string option;  (** **PeerTube >= 6.1** If enabled by the admin, the video source file is kept on the server and can be downloaded by the owner *)
        fps : float option;  (** **PeerTube >= 6.1** Frames per second of the video file *)
        height : int option;  (** **PeerTube >= 6.1** Video stream height *)
        input_filename : string option;  (** Uploaded/imported filename *)
        resolution : VideoResolutionConstant.T.t option;  (** **PeerTube >= 6.1** *)
        size : int option;  (** **PeerTube >= 6.1** Video file size in bytes *)
        width : int option;  (** **PeerTube >= 6.1** Video stream width *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?created_at ?file_download_url ?fps ?height ?input_filename ?resolution ?size ?width () = { created_at; file_download_url; fps; height; input_filename; resolution; size; width }
    
    let created_at t = t.created_at
    let file_download_url t = t.file_download_url
    let fps t = t.fps
    let height t = t.height
    let input_filename t = t.input_filename
    let resolution t = t.resolution
    let size t = t.size
    let width t = t.width
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoSource"
        (fun created_at file_download_url fps height input_filename resolution size width -> { created_at; file_download_url; fps; height; input_filename; resolution; size; width })
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "fileDownloadUrl" Jsont.string ~enc:(fun r -> r.file_download_url)
      |> Jsont.Object.opt_mem "fps" Jsont.number ~enc:(fun r -> r.fps)
      |> Jsont.Object.opt_mem "height" Jsont.int ~enc:(fun r -> r.height)
      |> Jsont.Object.opt_mem "inputFilename" Jsont.string ~enc:(fun r -> r.input_filename)
      |> Jsont.Object.opt_mem "resolution" VideoResolutionConstant.T.jsont ~enc:(fun r -> r.resolution)
      |> Jsont.Object.opt_mem "size" Jsont.int ~enc:(fun r -> r.size)
      |> Jsont.Object.opt_mem "width" Jsont.int ~enc:(fun r -> r.width)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get video source file metadata
  
      Get metadata and download link of original video file 
      @param id The object id, uuid or short uuid
  *)
  let get_video_source ~id client () =
    let op_name = "get_video_source" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/source" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module VideoReplaceSourceRequestResumable = struct
  module Types = struct
    module T = struct
      type t = {
        filename : string option;  (** Video filename including extension *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?filename () = { filename }
    
    let filename t = t.filename
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoReplaceSourceRequestResumable"
        (fun filename -> { filename })
      |> Jsont.Object.opt_mem "filename" Jsont.string ~enc:(fun r -> r.filename)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoPrivacySet = struct
  module Types = struct
    module T = struct
      (** privacy id of the video (see [/videos/privacies](#operation/getVideoPrivacyPolicies)) *)
      type t = int
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.int
  end
end

module VideoScheduled = struct
  module Types = struct
    module Update = struct
      type t = {
        privacy : VideoPrivacySet.T.t option;
        update_at : Ptime.t;  (** When to update the video *)
      }
    end
  end
  
  module Update = struct
    include Types.Update
    
    let v ~update_at ?privacy () = { privacy; update_at }
    
    let privacy t = t.privacy
    let update_at t = t.update_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoScheduledUpdate"
        (fun privacy update_at -> { privacy; update_at })
      |> Jsont.Object.opt_mem "privacy" VideoPrivacySet.T.jsont ~enc:(fun r -> r.privacy)
      |> Jsont.Object.mem "updateAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.update_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoPrivacyConstant = struct
  module Types = struct
    module T = struct
      type t = {
        id : VideoPrivacySet.T.t option;
        label : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?id ?label () = { id; label }
    
    let id t = t.id
    let label t = t.label
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoPrivacyConstant"
        (fun id label -> { id; label })
      |> Jsont.Object.opt_mem "id" VideoPrivacySet.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "label" Jsont.string ~enc:(fun r -> r.label)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module LiveVideoReplaySettings = struct
  module Types = struct
    module T = struct
      type t = {
        privacy : VideoPrivacySet.T.t option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?privacy () = { privacy }
    
    let privacy t = t.privacy
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"LiveVideoReplaySettings"
        (fun privacy -> { privacy })
      |> Jsont.Object.opt_mem "privacy" VideoPrivacySet.T.jsont ~enc:(fun r -> r.privacy)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoPlaylistTypeSet = struct
  module Types = struct
    module T = struct
      (** The video playlist type (Regular = `1`, Watch Later = `2`) *)
      type t = int
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.int
  end
end

module VideoPlaylistTypeConstant = struct
  module Types = struct
    module T = struct
      type t = {
        id : VideoPlaylistTypeSet.T.t option;
        label : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?id ?label () = { id; label }
    
    let id t = t.id
    let label t = t.label
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoPlaylistTypeConstant"
        (fun id label -> { id; label })
      |> Jsont.Object.opt_mem "id" VideoPlaylistTypeSet.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "label" Jsont.string ~enc:(fun r -> r.label)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoPlaylistPrivacySet = struct
  module Types = struct
    module T = struct
      (** Video playlist privacy policy (see [/video-playlists/privacies](#operation/getPlaylistPrivacyPolicies)) *)
      type t = int
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.int
  end
end

module VideoPlaylistPrivacyConstant = struct
  module Types = struct
    module T = struct
      type t = {
        id : VideoPlaylistPrivacySet.T.t option;
        label : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?id ?label () = { id; label }
    
    let id t = t.id
    let label t = t.label
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoPlaylistPrivacyConstant"
        (fun id label -> { id; label })
      |> Jsont.Object.opt_mem "id" VideoPlaylistPrivacySet.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "label" Jsont.string ~enc:(fun r -> r.label)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoLicenceSet = struct
  module Types = struct
    module T = struct
      (** licence id of the video (see [/videos/licences](#operation/getLicences)) *)
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module VideoConstantNumberLicence = struct
  module Types = struct
    module T = struct
      type t = {
        id : VideoLicenceSet.T.t option;
        label : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?id ?label () = { id; label }
    
    let id t = t.id
    let label t = t.label
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoConstantNumber-Licence"
        (fun id label -> { id; label })
      |> Jsont.Object.opt_mem "id" VideoLicenceSet.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "label" Jsont.string ~enc:(fun r -> r.label)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoLanguageSet = struct
  module Types = struct
    module T = struct
      (** language id of the video (see [/videos/languages](#operation/getLanguages)) *)
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module VideoConstantStringLanguage = struct
  module Types = struct
    module T = struct
      type t = {
        id : VideoLanguageSet.T.t option;
        label : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?id ?label () = { id; label }
    
    let id t = t.id
    let label t = t.label
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoConstantString-Language"
        (fun id label -> { id; label })
      |> Jsont.Object.opt_mem "id" VideoLanguageSet.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "label" Jsont.string ~enc:(fun r -> r.label)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoCaption = struct
  module Types = struct
    module T = struct
      type t = {
        automatically_generated : bool option;
        caption_path : string option;  (** Deprecated in PeerTube v8.0, use fileUrl instead *)
        file_url : string option;  (** **PeerTube >= 7.1** *)
        language : VideoConstantStringLanguage.T.t option;
        m3u8_url : string option;
        updated_at : Ptime.t option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?automatically_generated ?caption_path ?file_url ?language ?m3u8_url ?updated_at () = { automatically_generated; caption_path; file_url; language; m3u8_url; updated_at }
    
    let automatically_generated t = t.automatically_generated
    let caption_path t = t.caption_path
    let file_url t = t.file_url
    let language t = t.language
    let m3u8_url t = t.m3u8_url
    let updated_at t = t.updated_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoCaption"
        (fun automatically_generated caption_path file_url language m3u8_url updated_at -> { automatically_generated; caption_path; file_url; language; m3u8_url; updated_at })
      |> Jsont.Object.opt_mem "automaticallyGenerated" Jsont.bool ~enc:(fun r -> r.automatically_generated)
      |> Jsont.Object.opt_mem "captionPath" Jsont.string ~enc:(fun r -> r.caption_path)
      |> Jsont.Object.opt_mem "fileUrl" Jsont.string ~enc:(fun r -> r.file_url)
      |> Jsont.Object.opt_mem "language" VideoConstantStringLanguage.T.jsont ~enc:(fun r -> r.language)
      |> Jsont.Object.opt_mem "m3u8Url" Jsont.string ~enc:(fun r -> r.m3u8_url)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoImportStateConstant = struct
  module Types = struct
    module T = struct
      type t = {
        id : int option;  (** The video import state (Pending = `1`, Success = `2`, Failed = `3`) *)
        label : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?id ?label () = { id; label }
    
    let id t = t.id
    let label t = t.label
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoImportStateConstant"
        (fun id label -> { id; label })
      |> Jsont.Object.opt_mem "id" Jsont.int ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "label" Jsont.string ~enc:(fun r -> r.label)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoCommentsPolicySet = struct
  module Types = struct
    module T = struct
      (** Comments policy of the video (Enabled = `1`, Disabled = `2`, Requires Approval = `3`) *)
      type t = int
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.int
  end
end

module VideoCommentsPolicyConstant = struct
  module Types = struct
    module T = struct
      type t = {
        id : VideoCommentsPolicySet.T.t option;
        label : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?id ?label () = { id; label }
    
    let id t = t.id
    let label t = t.label
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoCommentsPolicyConstant"
        (fun id label -> { id; label })
      |> Jsont.Object.opt_mem "id" VideoCommentsPolicySet.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "label" Jsont.string ~enc:(fun r -> r.label)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoCommentsForXml = struct
  module Types = struct
    module T = struct
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module VideoChapters = struct
  module Types = struct
    module T = struct
      type t = {
        chapters : Jsont.json option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?chapters () = { chapters }
    
    let chapters t = t.chapters
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoChapters"
        (fun chapters -> { chapters })
      |> Jsont.Object.opt_mem "chapters" Jsont.json ~enc:(fun r -> r.chapters)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get chapters of a video
  
      **PeerTube >= 6.0** 
      @param id The object id, uuid or short uuid
  *)
  let get_video_chapters ~id client () =
    let op_name = "get_video_chapters" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/chapters" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module VideoChannelEdit = struct
  module Types = struct
    module T = struct
      type t = {
        description : Jsont.json option;  (** Channel description *)
        display_name : Jsont.json option;  (** Channel display name *)
        support : Jsont.json option;  (** How to support/fund the channel *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?description ?display_name ?support () = { description; display_name; support }
    
    let description t = t.description
    let display_name t = t.display_name
    let support t = t.support
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoChannelEdit"
        (fun description display_name support -> { description; display_name; support })
      |> Jsont.Object.opt_mem "description" Jsont.json ~enc:(fun r -> r.description)
      |> Jsont.Object.opt_mem "displayName" Jsont.json ~enc:(fun r -> r.display_name)
      |> Jsont.Object.opt_mem "support" Jsont.json ~enc:(fun r -> r.support)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoChannelCollaboratorState = struct
  module Types = struct
    module T = struct
      (** The user import state:
        - `1`: Pending
        - `2`: Accepted
       *)
      type t = int
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.int
  end
end

module VideoChannelActivityTarget = struct
  module Types = struct
    module T = struct
      (** The activity target:
        - VIDEO: 1,
        - PLAYLIST: 2,
        - CHANNEL: 3,
        - CHANNEL_SYNC: 4,
        - VIDEO_IMPORT: 5
       *)
      type t = int
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.int
  end
end

module VideoChannelActivityAction = struct
  module Types = struct
    module T = struct
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
      type t = int
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.int
  end
end

module VideoCategorySet = struct
  module Types = struct
    module T = struct
      (** category id of the video (see [/videos/categories](#operation/getCategories)) *)
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module VideoConstantNumberCategory = struct
  module Types = struct
    module T = struct
      type t = {
        id : VideoCategorySet.T.t option;
        label : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?id ?label () = { id; label }
    
    let id t = t.id
    let label t = t.label
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoConstantNumber-Category"
        (fun id label -> { id; label })
      |> Jsont.Object.opt_mem "id" VideoCategorySet.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "label" Jsont.string ~enc:(fun r -> r.label)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Uuidv4 = struct
  module Types = struct
    module T = struct
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module UsernameChannel = struct
  module Types = struct
    module T = struct
      (** immutable name of the channel, used to interact with its actor *)
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module Username = struct
  module Types = struct
    module T = struct
      (** immutable name of the user, used to find or mention its actor *)
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module UserRole = struct
  module Types = struct
    module T = struct
      (** The user role (Admin = `0`, Moderator = `1`, User = `2`) *)
      type t = int
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.int
  end
end

module UserRegistrationAcceptOrReject = struct
  module Types = struct
    module T = struct
      type t = {
        moderation_response : string;  (** Moderation response to send to the user *)
        prevent_email_delivery : bool option;  (** Set it to true if you don't want PeerTube to send an email to the user *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~moderation_response ?prevent_email_delivery () = { moderation_response; prevent_email_delivery }
    
    let moderation_response t = t.moderation_response
    let prevent_email_delivery t = t.prevent_email_delivery
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"UserRegistrationAcceptOrReject"
        (fun moderation_response prevent_email_delivery -> { moderation_response; prevent_email_delivery })
      |> Jsont.Object.mem "moderationResponse" Jsont.string ~enc:(fun r -> r.moderation_response)
      |> Jsont.Object.opt_mem "preventEmailDelivery" Jsont.bool ~enc:(fun r -> r.prevent_email_delivery)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module UserImportState = struct
  module Types = struct
    module T = struct
      (** The user import state:
        - `1`: Pending
        - `2`: Processing
        - `3`: Completed
        - `4`: Errored
       *)
      type t = int
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.int
  end
end

module UserImportResumable = struct
  module Types = struct
    module T = struct
      type t = {
        filename : string option;  (** Archive filename including extension *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?filename () = { filename }
    
    let filename t = t.filename
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"UserImportResumable"
        (fun filename -> { filename })
      |> Jsont.Object.opt_mem "filename" Jsont.string ~enc:(fun r -> r.filename)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module UserExportState = struct
  module Types = struct
    module T = struct
      (** The user export state:
        - `1`: Pending
        - `2`: Processing
        - `3`: Completed
        - `4`: Errored
       *)
      type t = int
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.int
  end
end

module UserAdminFlags = struct
  module Types = struct
    module T = struct
      (** Admin flags for the user (None = `0`, Bypass video blocklist = `1`) *)
      type t = int
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.int
  end
end

module TokenSession = struct
  module Types = struct
    module T = struct
      type t = {
        created_at : Ptime.t option;
        current_session : bool option;  (** Is this session the current one? *)
        id : int option;
        last_activity_date : Ptime.t option;
        last_activity_device : string option;
        last_activity_ip : string option;
        login_date : Ptime.t option;  (** Date of the login *)
        login_device : string option;  (** Device used to login *)
        login_ip : string option;  (** IP address used to login *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?created_at ?current_session ?id ?last_activity_date ?last_activity_device ?last_activity_ip ?login_date ?login_device ?login_ip () = { created_at; current_session; id; last_activity_date; last_activity_device; last_activity_ip; login_date; login_device; login_ip }
    
    let created_at t = t.created_at
    let current_session t = t.current_session
    let id t = t.id
    let last_activity_date t = t.last_activity_date
    let last_activity_device t = t.last_activity_device
    let last_activity_ip t = t.last_activity_ip
    let login_date t = t.login_date
    let login_device t = t.login_device
    let login_ip t = t.login_ip
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"TokenSession"
        (fun created_at current_session id last_activity_date last_activity_device last_activity_ip login_date login_device login_ip -> { created_at; current_session; id; last_activity_date; last_activity_device; last_activity_ip; login_date; login_device; login_ip })
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "currentSession" Jsont.bool ~enc:(fun r -> r.current_session)
      |> Jsont.Object.opt_mem "id" Jsont.int ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "lastActivityDate" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.last_activity_date)
      |> Jsont.Object.opt_mem "lastActivityDevice" Jsont.string ~enc:(fun r -> r.last_activity_device)
      |> Jsont.Object.opt_mem "lastActivityIP" Jsont.string ~enc:(fun r -> r.last_activity_ip)
      |> Jsont.Object.opt_mem "loginDate" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.login_date)
      |> Jsont.Object.opt_mem "loginDevice" Jsont.string ~enc:(fun r -> r.login_device)
      |> Jsont.Object.opt_mem "loginIP" Jsont.string ~enc:(fun r -> r.login_ip)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Storyboard = struct
  module Types = struct
    module T = struct
      type t = {
        file_url : string option;  (** **PeerTube >= 7.1** *)
        sprite_duration : int option;
        sprite_height : int option;
        sprite_width : int option;
        storyboard_path : string option;  (** Deprecated in PeerTube v8.0, use fileUrl instead *)
        total_height : int option;
        total_width : int option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?file_url ?sprite_duration ?sprite_height ?sprite_width ?storyboard_path ?total_height ?total_width () = { file_url; sprite_duration; sprite_height; sprite_width; storyboard_path; total_height; total_width }
    
    let file_url t = t.file_url
    let sprite_duration t = t.sprite_duration
    let sprite_height t = t.sprite_height
    let sprite_width t = t.sprite_width
    let storyboard_path t = t.storyboard_path
    let total_height t = t.total_height
    let total_width t = t.total_width
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Storyboard"
        (fun file_url sprite_duration sprite_height sprite_width storyboard_path total_height total_width -> { file_url; sprite_duration; sprite_height; sprite_width; storyboard_path; total_height; total_width })
      |> Jsont.Object.opt_mem "fileUrl" Jsont.string ~enc:(fun r -> r.file_url)
      |> Jsont.Object.opt_mem "spriteDuration" Jsont.int ~enc:(fun r -> r.sprite_duration)
      |> Jsont.Object.opt_mem "spriteHeight" Jsont.int ~enc:(fun r -> r.sprite_height)
      |> Jsont.Object.opt_mem "spriteWidth" Jsont.int ~enc:(fun r -> r.sprite_width)
      |> Jsont.Object.opt_mem "storyboardPath" Jsont.string ~enc:(fun r -> r.storyboard_path)
      |> Jsont.Object.opt_mem "totalHeight" Jsont.int ~enc:(fun r -> r.total_height)
      |> Jsont.Object.opt_mem "totalWidth" Jsont.int ~enc:(fun r -> r.total_width)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module ShortUuid = struct
  module Types = struct
    module T = struct
      (** translation of a uuid v4 with a bigger alphabet to have a shorter uuid *)
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module ServerStats = struct
  module Types = struct
    module T = struct
      type t = {
        activity_pub_messages_processed_per_second : float option;
        average_abuse_response_time_ms : float option;  (** **PeerTube >= 6.1** Value is null if the admin disabled abuses stats *)
        average_registration_request_response_time_ms : float option;  (** **PeerTube >= 6.1** Value is null if the admin disabled registration requests stats *)
        total_abuses : float option;  (** **PeerTube >= 6.1** Value is null if the admin disabled abuses stats *)
        total_abuses_processed : float option;  (** **PeerTube >= 6.1** Value is null if the admin disabled abuses stats *)
        total_activity_pub_messages_errors : float option;
        total_activity_pub_messages_processed : float option;
        total_activity_pub_messages_successes : float option;
        total_activity_pub_messages_waiting : float option;
        total_admins : float option;  (** **PeerTube >= 6.1** Value is null if the admin disabled total admins stats *)
        total_daily_active_users : float option;
        total_instance_followers : float option;
        total_instance_following : float option;
        total_local_daily_active_video_channels : float option;
        total_local_monthly_active_video_channels : float option;
        total_local_playlists : float option;
        total_local_video_channels : float option;
        total_local_video_comments : float option;  (** Total comments made by local users *)
        total_local_video_files_size : float option;
        total_local_video_views : float option;  (** Total video views made on the instance *)
        total_local_videos : float option;
        total_local_weekly_active_video_channels : float option;
        total_moderators : float option;  (** **PeerTube >= 6.1** Value is null if the admin disabled total moderators stats *)
        total_monthly_active_users : float option;
        total_registration_requests : float option;  (** **PeerTube >= 6.1** Value is null if the admin disabled registration requests stats *)
        total_registration_requests_processed : float option;  (** **PeerTube >= 6.1** Value is null if the admin disabled registration requests stats *)
        total_users : float option;
        total_video_comments : float option;
        total_videos : float option;
        total_weekly_active_users : float option;
        videos_redundancy : Jsont.json list option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?activity_pub_messages_processed_per_second ?average_abuse_response_time_ms ?average_registration_request_response_time_ms ?total_abuses ?total_abuses_processed ?total_activity_pub_messages_errors ?total_activity_pub_messages_processed ?total_activity_pub_messages_successes ?total_activity_pub_messages_waiting ?total_admins ?total_daily_active_users ?total_instance_followers ?total_instance_following ?total_local_daily_active_video_channels ?total_local_monthly_active_video_channels ?total_local_playlists ?total_local_video_channels ?total_local_video_comments ?total_local_video_files_size ?total_local_video_views ?total_local_videos ?total_local_weekly_active_video_channels ?total_moderators ?total_monthly_active_users ?total_registration_requests ?total_registration_requests_processed ?total_users ?total_video_comments ?total_videos ?total_weekly_active_users ?videos_redundancy () = { activity_pub_messages_processed_per_second; average_abuse_response_time_ms; average_registration_request_response_time_ms; total_abuses; total_abuses_processed; total_activity_pub_messages_errors; total_activity_pub_messages_processed; total_activity_pub_messages_successes; total_activity_pub_messages_waiting; total_admins; total_daily_active_users; total_instance_followers; total_instance_following; total_local_daily_active_video_channels; total_local_monthly_active_video_channels; total_local_playlists; total_local_video_channels; total_local_video_comments; total_local_video_files_size; total_local_video_views; total_local_videos; total_local_weekly_active_video_channels; total_moderators; total_monthly_active_users; total_registration_requests; total_registration_requests_processed; total_users; total_video_comments; total_videos; total_weekly_active_users; videos_redundancy }
    
    let activity_pub_messages_processed_per_second t = t.activity_pub_messages_processed_per_second
    let average_abuse_response_time_ms t = t.average_abuse_response_time_ms
    let average_registration_request_response_time_ms t = t.average_registration_request_response_time_ms
    let total_abuses t = t.total_abuses
    let total_abuses_processed t = t.total_abuses_processed
    let total_activity_pub_messages_errors t = t.total_activity_pub_messages_errors
    let total_activity_pub_messages_processed t = t.total_activity_pub_messages_processed
    let total_activity_pub_messages_successes t = t.total_activity_pub_messages_successes
    let total_activity_pub_messages_waiting t = t.total_activity_pub_messages_waiting
    let total_admins t = t.total_admins
    let total_daily_active_users t = t.total_daily_active_users
    let total_instance_followers t = t.total_instance_followers
    let total_instance_following t = t.total_instance_following
    let total_local_daily_active_video_channels t = t.total_local_daily_active_video_channels
    let total_local_monthly_active_video_channels t = t.total_local_monthly_active_video_channels
    let total_local_playlists t = t.total_local_playlists
    let total_local_video_channels t = t.total_local_video_channels
    let total_local_video_comments t = t.total_local_video_comments
    let total_local_video_files_size t = t.total_local_video_files_size
    let total_local_video_views t = t.total_local_video_views
    let total_local_videos t = t.total_local_videos
    let total_local_weekly_active_video_channels t = t.total_local_weekly_active_video_channels
    let total_moderators t = t.total_moderators
    let total_monthly_active_users t = t.total_monthly_active_users
    let total_registration_requests t = t.total_registration_requests
    let total_registration_requests_processed t = t.total_registration_requests_processed
    let total_users t = t.total_users
    let total_video_comments t = t.total_video_comments
    let total_videos t = t.total_videos
    let total_weekly_active_users t = t.total_weekly_active_users
    let videos_redundancy t = t.videos_redundancy
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ServerStats"
        (fun activity_pub_messages_processed_per_second average_abuse_response_time_ms average_registration_request_response_time_ms total_abuses total_abuses_processed total_activity_pub_messages_errors total_activity_pub_messages_processed total_activity_pub_messages_successes total_activity_pub_messages_waiting total_admins total_daily_active_users total_instance_followers total_instance_following total_local_daily_active_video_channels total_local_monthly_active_video_channels total_local_playlists total_local_video_channels total_local_video_comments total_local_video_files_size total_local_video_views total_local_videos total_local_weekly_active_video_channels total_moderators total_monthly_active_users total_registration_requests total_registration_requests_processed total_users total_video_comments total_videos total_weekly_active_users videos_redundancy -> { activity_pub_messages_processed_per_second; average_abuse_response_time_ms; average_registration_request_response_time_ms; total_abuses; total_abuses_processed; total_activity_pub_messages_errors; total_activity_pub_messages_processed; total_activity_pub_messages_successes; total_activity_pub_messages_waiting; total_admins; total_daily_active_users; total_instance_followers; total_instance_following; total_local_daily_active_video_channels; total_local_monthly_active_video_channels; total_local_playlists; total_local_video_channels; total_local_video_comments; total_local_video_files_size; total_local_video_views; total_local_videos; total_local_weekly_active_video_channels; total_moderators; total_monthly_active_users; total_registration_requests; total_registration_requests_processed; total_users; total_video_comments; total_videos; total_weekly_active_users; videos_redundancy })
      |> Jsont.Object.opt_mem "activityPubMessagesProcessedPerSecond" Jsont.number ~enc:(fun r -> r.activity_pub_messages_processed_per_second)
      |> Jsont.Object.opt_mem "averageAbuseResponseTimeMs" Jsont.number ~enc:(fun r -> r.average_abuse_response_time_ms)
      |> Jsont.Object.opt_mem "averageRegistrationRequestResponseTimeMs" Jsont.number ~enc:(fun r -> r.average_registration_request_response_time_ms)
      |> Jsont.Object.opt_mem "totalAbuses" Jsont.number ~enc:(fun r -> r.total_abuses)
      |> Jsont.Object.opt_mem "totalAbusesProcessed" Jsont.number ~enc:(fun r -> r.total_abuses_processed)
      |> Jsont.Object.opt_mem "totalActivityPubMessagesErrors" Jsont.number ~enc:(fun r -> r.total_activity_pub_messages_errors)
      |> Jsont.Object.opt_mem "totalActivityPubMessagesProcessed" Jsont.number ~enc:(fun r -> r.total_activity_pub_messages_processed)
      |> Jsont.Object.opt_mem "totalActivityPubMessagesSuccesses" Jsont.number ~enc:(fun r -> r.total_activity_pub_messages_successes)
      |> Jsont.Object.opt_mem "totalActivityPubMessagesWaiting" Jsont.number ~enc:(fun r -> r.total_activity_pub_messages_waiting)
      |> Jsont.Object.opt_mem "totalAdmins" Jsont.number ~enc:(fun r -> r.total_admins)
      |> Jsont.Object.opt_mem "totalDailyActiveUsers" Jsont.number ~enc:(fun r -> r.total_daily_active_users)
      |> Jsont.Object.opt_mem "totalInstanceFollowers" Jsont.number ~enc:(fun r -> r.total_instance_followers)
      |> Jsont.Object.opt_mem "totalInstanceFollowing" Jsont.number ~enc:(fun r -> r.total_instance_following)
      |> Jsont.Object.opt_mem "totalLocalDailyActiveVideoChannels" Jsont.number ~enc:(fun r -> r.total_local_daily_active_video_channels)
      |> Jsont.Object.opt_mem "totalLocalMonthlyActiveVideoChannels" Jsont.number ~enc:(fun r -> r.total_local_monthly_active_video_channels)
      |> Jsont.Object.opt_mem "totalLocalPlaylists" Jsont.number ~enc:(fun r -> r.total_local_playlists)
      |> Jsont.Object.opt_mem "totalLocalVideoChannels" Jsont.number ~enc:(fun r -> r.total_local_video_channels)
      |> Jsont.Object.opt_mem "totalLocalVideoComments" Jsont.number ~enc:(fun r -> r.total_local_video_comments)
      |> Jsont.Object.opt_mem "totalLocalVideoFilesSize" Jsont.number ~enc:(fun r -> r.total_local_video_files_size)
      |> Jsont.Object.opt_mem "totalLocalVideoViews" Jsont.number ~enc:(fun r -> r.total_local_video_views)
      |> Jsont.Object.opt_mem "totalLocalVideos" Jsont.number ~enc:(fun r -> r.total_local_videos)
      |> Jsont.Object.opt_mem "totalLocalWeeklyActiveVideoChannels" Jsont.number ~enc:(fun r -> r.total_local_weekly_active_video_channels)
      |> Jsont.Object.opt_mem "totalModerators" Jsont.number ~enc:(fun r -> r.total_moderators)
      |> Jsont.Object.opt_mem "totalMonthlyActiveUsers" Jsont.number ~enc:(fun r -> r.total_monthly_active_users)
      |> Jsont.Object.opt_mem "totalRegistrationRequests" Jsont.number ~enc:(fun r -> r.total_registration_requests)
      |> Jsont.Object.opt_mem "totalRegistrationRequestsProcessed" Jsont.number ~enc:(fun r -> r.total_registration_requests_processed)
      |> Jsont.Object.opt_mem "totalUsers" Jsont.number ~enc:(fun r -> r.total_users)
      |> Jsont.Object.opt_mem "totalVideoComments" Jsont.number ~enc:(fun r -> r.total_video_comments)
      |> Jsont.Object.opt_mem "totalVideos" Jsont.number ~enc:(fun r -> r.total_videos)
      |> Jsont.Object.opt_mem "totalWeeklyActiveUsers" Jsont.number ~enc:(fun r -> r.total_weekly_active_users)
      |> Jsont.Object.opt_mem "videosRedundancy" (Jsont.list Jsont.json) ~enc:(fun r -> r.videos_redundancy)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get instance stats
  
      Get instance public statistics. This endpoint is cached. *)
  let get_instance_stats client () =
    let op_name = "get_instance_stats" in
    let url_path = "/api/v1/server/stats" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ServerError = struct
  module Types = struct
    module T = struct
      type t = {
        code : string option;
        detail : string option;
        status : int option;
        type_ : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?code ?detail ?status ?type_ () = { code; detail; status; type_ }
    
    let code t = t.code
    let detail t = t.detail
    let status t = t.status
    let type_ t = t.type_
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ServerError"
        (fun code detail status type_ -> { code; detail; status; type_ })
      |> Jsont.Object.opt_mem "code" Jsont.string ~enc:(fun r -> r.code)
      |> Jsont.Object.opt_mem "detail" Jsont.string ~enc:(fun r -> r.detail)
      |> Jsont.Object.opt_mem "status" Jsont.int ~enc:(fun r -> r.status)
      |> Jsont.Object.opt_mem "type" Jsont.string ~enc:(fun r -> r.type_)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module ServerConfigCustom = struct
  module Types = struct
    module T = struct
      type t = {
        admin : Jsont.json option;
        auto_blacklist : Jsont.json option;
        cache : Jsont.json option;
        contact_form : Jsont.json option;
        defaults : Jsont.json option;
        followers : Jsont.json option;
        import : Jsont.json option;
        instance : Jsont.json option;
        services : Jsont.json option;
        signup : Jsont.json option;
        storyboard : Jsont.json option;
        theme : Jsont.json option;
        transcoding : Jsont.json option;  (** Settings pertaining to transcoding jobs *)
        user : Jsont.json option;  (** Settings that apply to new users, if registration is enabled *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?admin ?auto_blacklist ?cache ?contact_form ?defaults ?followers ?import ?instance ?services ?signup ?storyboard ?theme ?transcoding ?user () = { admin; auto_blacklist; cache; contact_form; defaults; followers; import; instance; services; signup; storyboard; theme; transcoding; user }
    
    let admin t = t.admin
    let auto_blacklist t = t.auto_blacklist
    let cache t = t.cache
    let contact_form t = t.contact_form
    let defaults t = t.defaults
    let followers t = t.followers
    let import t = t.import
    let instance t = t.instance
    let services t = t.services
    let signup t = t.signup
    let storyboard t = t.storyboard
    let theme t = t.theme
    let transcoding t = t.transcoding
    let user t = t.user
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ServerConfigCustom"
        (fun admin auto_blacklist cache contact_form defaults followers import instance services signup storyboard theme transcoding user -> { admin; auto_blacklist; cache; contact_form; defaults; followers; import; instance; services; signup; storyboard; theme; transcoding; user })
      |> Jsont.Object.opt_mem "admin" Jsont.json ~enc:(fun r -> r.admin)
      |> Jsont.Object.opt_mem "autoBlacklist" Jsont.json ~enc:(fun r -> r.auto_blacklist)
      |> Jsont.Object.opt_mem "cache" Jsont.json ~enc:(fun r -> r.cache)
      |> Jsont.Object.opt_mem "contactForm" Jsont.json ~enc:(fun r -> r.contact_form)
      |> Jsont.Object.opt_mem "defaults" Jsont.json ~enc:(fun r -> r.defaults)
      |> Jsont.Object.opt_mem "followers" Jsont.json ~enc:(fun r -> r.followers)
      |> Jsont.Object.opt_mem "import" Jsont.json ~enc:(fun r -> r.import)
      |> Jsont.Object.opt_mem "instance" Jsont.json ~enc:(fun r -> r.instance)
      |> Jsont.Object.opt_mem "services" Jsont.json ~enc:(fun r -> r.services)
      |> Jsont.Object.opt_mem "signup" Jsont.json ~enc:(fun r -> r.signup)
      |> Jsont.Object.opt_mem "storyboard" Jsont.json ~enc:(fun r -> r.storyboard)
      |> Jsont.Object.opt_mem "theme" Jsont.json ~enc:(fun r -> r.theme)
      |> Jsont.Object.opt_mem "transcoding" Jsont.json ~enc:(fun r -> r.transcoding)
      |> Jsont.Object.opt_mem "user" Jsont.json ~enc:(fun r -> r.user)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get instance runtime configuration *)
  let get_custom_config client () =
    let op_name = "get_custom_config" in
    let url_path = "/api/v1/config/custom" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ServerConfigAbout = struct
  module Types = struct
    module T = struct
      type t = {
        instance : Jsont.json option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?instance () = { instance }
    
    let instance t = t.instance
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ServerConfigAbout"
        (fun instance -> { instance })
      |> Jsont.Object.opt_mem "instance" Jsont.json ~enc:(fun r -> r.instance)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get instance "About" information *)
  let get_about client () =
    let op_name = "get_about" in
    let url_path = "/api/v1/config/about" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module Server = struct
  module Types = struct
    module Config = struct
      type t = {
        auto_blacklist : Jsont.json option;
        avatar : Jsont.json option;
        contact_form : Jsont.json option;
        email : Jsont.json option;
        export : Jsont.json option;
        federation : Jsont.json option;
        followings : Jsont.json option;
        homepage : Jsont.json option;
        import : Jsont.json option;
        instance : Jsont.json option;
        open_telemetry : Jsont.json option;  (** PeerTube >= 6.1 *)
        plugin : Jsont.json option;
        search : Jsont.json option;
        server_commit : string option;
        server_version : string option;
        signup : Jsont.json option;
        theme : Jsont.json option;
        tracker : Jsont.json option;
        transcoding : Jsont.json option;
        trending : Jsont.json option;
        user : Jsont.json option;
        video : Jsont.json option;
        video_caption : Jsont.json option;
        views : Jsont.json option;  (** PeerTube >= 6.1 *)
      }
    end
  end
  
  module Config = struct
    include Types.Config
    
    let v ?auto_blacklist ?avatar ?contact_form ?email ?export ?federation ?followings ?homepage ?import ?instance ?open_telemetry ?plugin ?search ?server_commit ?server_version ?signup ?theme ?tracker ?transcoding ?trending ?user ?video ?video_caption ?views () = { auto_blacklist; avatar; contact_form; email; export; federation; followings; homepage; import; instance; open_telemetry; plugin; search; server_commit; server_version; signup; theme; tracker; transcoding; trending; user; video; video_caption; views }
    
    let auto_blacklist t = t.auto_blacklist
    let avatar t = t.avatar
    let contact_form t = t.contact_form
    let email t = t.email
    let export t = t.export
    let federation t = t.federation
    let followings t = t.followings
    let homepage t = t.homepage
    let import t = t.import
    let instance t = t.instance
    let open_telemetry t = t.open_telemetry
    let plugin t = t.plugin
    let search t = t.search
    let server_commit t = t.server_commit
    let server_version t = t.server_version
    let signup t = t.signup
    let theme t = t.theme
    let tracker t = t.tracker
    let transcoding t = t.transcoding
    let trending t = t.trending
    let user t = t.user
    let video t = t.video
    let video_caption t = t.video_caption
    let views t = t.views
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ServerConfig"
        (fun auto_blacklist avatar contact_form email export federation followings homepage import instance open_telemetry plugin search server_commit server_version signup theme tracker transcoding trending user video video_caption views -> { auto_blacklist; avatar; contact_form; email; export; federation; followings; homepage; import; instance; open_telemetry; plugin; search; server_commit; server_version; signup; theme; tracker; transcoding; trending; user; video; video_caption; views })
      |> Jsont.Object.opt_mem "autoBlacklist" Jsont.json ~enc:(fun r -> r.auto_blacklist)
      |> Jsont.Object.opt_mem "avatar" Jsont.json ~enc:(fun r -> r.avatar)
      |> Jsont.Object.opt_mem "contactForm" Jsont.json ~enc:(fun r -> r.contact_form)
      |> Jsont.Object.opt_mem "email" Jsont.json ~enc:(fun r -> r.email)
      |> Jsont.Object.opt_mem "export" Jsont.json ~enc:(fun r -> r.export)
      |> Jsont.Object.opt_mem "federation" Jsont.json ~enc:(fun r -> r.federation)
      |> Jsont.Object.opt_mem "followings" Jsont.json ~enc:(fun r -> r.followings)
      |> Jsont.Object.opt_mem "homepage" Jsont.json ~enc:(fun r -> r.homepage)
      |> Jsont.Object.opt_mem "import" Jsont.json ~enc:(fun r -> r.import)
      |> Jsont.Object.opt_mem "instance" Jsont.json ~enc:(fun r -> r.instance)
      |> Jsont.Object.opt_mem "openTelemetry" Jsont.json ~enc:(fun r -> r.open_telemetry)
      |> Jsont.Object.opt_mem "plugin" Jsont.json ~enc:(fun r -> r.plugin)
      |> Jsont.Object.opt_mem "search" Jsont.json ~enc:(fun r -> r.search)
      |> Jsont.Object.opt_mem "serverCommit" Jsont.string ~enc:(fun r -> r.server_commit)
      |> Jsont.Object.opt_mem "serverVersion" Jsont.string ~enc:(fun r -> r.server_version)
      |> Jsont.Object.opt_mem "signup" Jsont.json ~enc:(fun r -> r.signup)
      |> Jsont.Object.opt_mem "theme" Jsont.json ~enc:(fun r -> r.theme)
      |> Jsont.Object.opt_mem "tracker" Jsont.json ~enc:(fun r -> r.tracker)
      |> Jsont.Object.opt_mem "transcoding" Jsont.json ~enc:(fun r -> r.transcoding)
      |> Jsont.Object.opt_mem "trending" Jsont.json ~enc:(fun r -> r.trending)
      |> Jsont.Object.opt_mem "user" Jsont.json ~enc:(fun r -> r.user)
      |> Jsont.Object.opt_mem "video" Jsont.json ~enc:(fun r -> r.video)
      |> Jsont.Object.opt_mem "videoCaption" Jsont.json ~enc:(fun r -> r.video_caption)
      |> Jsont.Object.opt_mem "views" Jsont.json ~enc:(fun r -> r.views)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get instance public configuration *)
  let get_config client () =
    let op_name = "get_config" in
    let url_path = "/api/v1/config" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Config.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module SendClientLog = struct
  module Types = struct
    module T = struct
      type t = {
        level : Jsont.json;
        message : string;
        meta : string option;  (** Additional information regarding this log *)
        stack_trace : string option;  (** Stack trace of the error if there is one *)
        url : string;  (** URL of the current user page *)
        user_agent : string option;  (** User agent of the web browser that sends the message *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~level ~message ~url ?meta ?stack_trace ?user_agent () = { level; message; meta; stack_trace; url; user_agent }
    
    let level t = t.level
    let message t = t.message
    let meta t = t.meta
    let stack_trace t = t.stack_trace
    let url t = t.url
    let user_agent t = t.user_agent
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"SendClientLog"
        (fun level message meta stack_trace url user_agent -> { level; message; meta; stack_trace; url; user_agent })
      |> Jsont.Object.mem "level" Jsont.json ~enc:(fun r -> r.level)
      |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.message)
      |> Jsont.Object.opt_mem "meta" Jsont.string ~enc:(fun r -> r.meta)
      |> Jsont.Object.opt_mem "stackTrace" Jsont.string ~enc:(fun r -> r.stack_trace)
      |> Jsont.Object.mem "url" Jsont.string ~enc:(fun r -> r.url)
      |> Jsont.Object.opt_mem "userAgent" Jsont.string ~enc:(fun r -> r.user_agent)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module RunnerRegistrationToken = struct
  module Types = struct
    module T = struct
      type t = {
        created_at : Ptime.t option;
        id : int option;
        registered_runners_count : int option;
        registration_token : string option;
        updated_at : Ptime.t option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?created_at ?id ?registered_runners_count ?registration_token ?updated_at () = { created_at; id; registered_runners_count; registration_token; updated_at }
    
    let created_at t = t.created_at
    let id t = t.id
    let registered_runners_count t = t.registered_runners_count
    let registration_token t = t.registration_token
    let updated_at t = t.updated_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"RunnerRegistrationToken"
        (fun created_at id registered_runners_count registration_token updated_at -> { created_at; id; registered_runners_count; registration_token; updated_at })
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "id" Jsont.int ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "registeredRunnersCount" Jsont.int ~enc:(fun r -> r.registered_runners_count)
      |> Jsont.Object.opt_mem "registrationToken" Jsont.string ~enc:(fun r -> r.registration_token)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module RunnerJobState = struct
  module Types = struct
    module T = struct
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
      type t = int
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.int
  end
end

module RunnerJobStateConstant = struct
  module Types = struct
    module T = struct
      type t = {
        id : RunnerJobState.T.t option;
        label : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?id ?label () = { id; label }
    
    let id t = t.id
    let label t = t.label
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"RunnerJobStateConstant"
        (fun id label -> { id; label })
      |> Jsont.Object.opt_mem "id" RunnerJobState.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "label" Jsont.string ~enc:(fun r -> r.label)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module RunnerJobPayload = struct
  module Types = struct
    module T = struct
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module RunnerJob = struct
  module Types = struct
    module Type = struct
      type t = [
        | `Vod_web_video_transcoding
        | `Vod_hls_transcoding
        | `Vod_audio_merge_transcoding
        | `Live_rtmp_hls_transcoding
      ]
    end
  
    module T = struct
      type t = {
        created_at : Ptime.t option;
        error : string option;  (** Error message if the job is errored *)
        failures : int option;  (** Number of times a remote runner failed to process this job. After too many failures, the job in "error" state *)
        finished_at : Ptime.t option;
        parent : Jsont.json option;  (** If job has a parent job *)
        payload : RunnerJobPayload.T.t option;
        priority : int option;  (** Job priority (less has more priority) *)
        progress : int option;  (** Percentage progress *)
        runner : Jsont.json option;  (** If job is associated to a runner *)
        started_at : Ptime.t option;
        state : RunnerJobStateConstant.T.t option;
        type_ : Type.t option;
        updated_at : Ptime.t option;
        uuid : Uuidv4.T.t option;
      }
    end
  end
  
  module Type = struct
    include Types.Type
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"RunnerJobType"
        ~dec:(function
          | "vod-web-video-transcoding" -> `Vod_web_video_transcoding
          | "vod-hls-transcoding" -> `Vod_hls_transcoding
          | "vod-audio-merge-transcoding" -> `Vod_audio_merge_transcoding
          | "live-rtmp-hls-transcoding" -> `Live_rtmp_hls_transcoding
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Vod_web_video_transcoding -> "vod-web-video-transcoding"
          | `Vod_hls_transcoding -> "vod-hls-transcoding"
          | `Vod_audio_merge_transcoding -> "vod-audio-merge-transcoding"
          | `Live_rtmp_hls_transcoding -> "live-rtmp-hls-transcoding")
  end
  
  module T = struct
    include Types.T
    
    let v ?created_at ?error ?failures ?finished_at ?parent ?payload ?priority ?progress ?runner ?started_at ?state ?type_ ?updated_at ?uuid () = { created_at; error; failures; finished_at; parent; payload; priority; progress; runner; started_at; state; type_; updated_at; uuid }
    
    let created_at t = t.created_at
    let error t = t.error
    let failures t = t.failures
    let finished_at t = t.finished_at
    let parent t = t.parent
    let payload t = t.payload
    let priority t = t.priority
    let progress t = t.progress
    let runner t = t.runner
    let started_at t = t.started_at
    let state t = t.state
    let type_ t = t.type_
    let updated_at t = t.updated_at
    let uuid t = t.uuid
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"RunnerJob"
        (fun created_at error failures finished_at parent payload priority progress runner started_at state type_ updated_at uuid -> { created_at; error; failures; finished_at; parent; payload; priority; progress; runner; started_at; state; type_; updated_at; uuid })
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "error" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.error)
      |> Jsont.Object.opt_mem "failures" Jsont.int ~enc:(fun r -> r.failures)
      |> Jsont.Object.opt_mem "finishedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.finished_at)
      |> Jsont.Object.mem "parent" (Openapi.Runtime.nullable_any Jsont.json)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.parent)
      |> Jsont.Object.opt_mem "payload" RunnerJobPayload.T.jsont ~enc:(fun r -> r.payload)
      |> Jsont.Object.opt_mem "priority" Jsont.int ~enc:(fun r -> r.priority)
      |> Jsont.Object.opt_mem "progress" Jsont.int ~enc:(fun r -> r.progress)
      |> Jsont.Object.mem "runner" (Openapi.Runtime.nullable_any Jsont.json)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.runner)
      |> Jsont.Object.opt_mem "startedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.started_at)
      |> Jsont.Object.opt_mem "state" RunnerJobStateConstant.T.jsont ~enc:(fun r -> r.state)
      |> Jsont.Object.opt_mem "type" Type.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "uuid" Uuidv4.T.jsont ~enc:(fun r -> r.uuid)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module RunnerJobAdmin = struct
  module Types = struct
    module T = struct
      type t = {
        created_at : Ptime.t option;
        error : string option;  (** Error message if the job is errored *)
        failures : int option;  (** Number of times a remote runner failed to process this job. After too many failures, the job in "error" state *)
        finished_at : Ptime.t option;
        parent : Jsont.json option;  (** If job has a parent job *)
        payload : RunnerJobPayload.T.t option;
        priority : int option;  (** Job priority (less has more priority) *)
        progress : int option;  (** Percentage progress *)
        runner : Jsont.json option;  (** If job is associated to a runner *)
        started_at : Ptime.t option;
        state : RunnerJobStateConstant.T.t option;
        type_ : RunnerJob.Type.t option;
        updated_at : Ptime.t option;
        uuid : Uuidv4.T.t option;
        private_payload : Jsont.json option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?created_at ?error ?failures ?finished_at ?parent ?payload ?priority ?progress ?runner ?started_at ?state ?type_ ?updated_at ?uuid ?private_payload () = { created_at; error; failures; finished_at; parent; payload; priority; progress; runner; started_at; state; type_; updated_at; uuid; private_payload }
    
    let created_at t = t.created_at
    let error t = t.error
    let failures t = t.failures
    let finished_at t = t.finished_at
    let parent t = t.parent
    let payload t = t.payload
    let priority t = t.priority
    let progress t = t.progress
    let runner t = t.runner
    let started_at t = t.started_at
    let state t = t.state
    let type_ t = t.type_
    let updated_at t = t.updated_at
    let uuid t = t.uuid
    let private_payload t = t.private_payload
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"RunnerJobAdmin"
        (fun created_at error failures finished_at parent payload priority progress runner started_at state type_ updated_at uuid private_payload -> { created_at; error; failures; finished_at; parent; payload; priority; progress; runner; started_at; state; type_; updated_at; uuid; private_payload })
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "error" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.error)
      |> Jsont.Object.opt_mem "failures" Jsont.int ~enc:(fun r -> r.failures)
      |> Jsont.Object.opt_mem "finishedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.finished_at)
      |> Jsont.Object.mem "parent" (Openapi.Runtime.nullable_any Jsont.json)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.parent)
      |> Jsont.Object.opt_mem "payload" RunnerJobPayload.T.jsont ~enc:(fun r -> r.payload)
      |> Jsont.Object.opt_mem "priority" Jsont.int ~enc:(fun r -> r.priority)
      |> Jsont.Object.opt_mem "progress" Jsont.int ~enc:(fun r -> r.progress)
      |> Jsont.Object.mem "runner" (Openapi.Runtime.nullable_any Jsont.json)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.runner)
      |> Jsont.Object.opt_mem "startedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.started_at)
      |> Jsont.Object.opt_mem "state" RunnerJobStateConstant.T.jsont ~enc:(fun r -> r.state)
      |> Jsont.Object.opt_mem "type" RunnerJob.Type.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "uuid" Uuidv4.T.jsont ~enc:(fun r -> r.uuid)
      |> Jsont.Object.opt_mem "privatePayload" Jsont.json ~enc:(fun r -> r.private_payload)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Runner = struct
  module Types = struct
    module T = struct
      type t = {
        created_at : Ptime.t option;
        description : string option;
        id : int option;
        ip : string option;
        last_contact : Ptime.t option;
        name : string option;
        updated_at : Ptime.t option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?created_at ?description ?id ?ip ?last_contact ?name ?updated_at () = { created_at; description; id; ip; last_contact; name; updated_at }
    
    let created_at t = t.created_at
    let description t = t.description
    let id t = t.id
    let ip t = t.ip
    let last_contact t = t.last_contact
    let name t = t.name
    let updated_at t = t.updated_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Runner"
        (fun created_at description id ip last_contact name updated_at -> { created_at; description; id; ip; last_contact; name; updated_at })
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.opt_mem "id" Jsont.int ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "ip" Jsont.string ~enc:(fun r -> r.ip)
      |> Jsont.Object.opt_mem "lastContact" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.last_contact)
      |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module RequestTwoFactor = struct
  module Types = struct
    module Response = struct
      type t = {
        otp_request : Jsont.json option;
      }
    end
  end
  
  module Response = struct
    include Types.Response
    
    let v ?otp_request () = { otp_request }
    
    let otp_request t = t.otp_request
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"RequestTwoFactorResponse"
        (fun otp_request -> { otp_request })
      |> Jsont.Object.opt_mem "otpRequest" Jsont.json ~enc:(fun r -> r.otp_request)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Request two factor auth
  
      Request two factor authentication for a user 
      @param id Entity id
  *)
  let request_two_factor ~id client () =
    let op_name = "request_two_factor" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/users/{id}/two-factor/request" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module PredefinedAbuseReasons = struct
  module Types = struct
    module T = struct
      (** Reason categories that help triage reports *)
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module Plugin = struct
  module Types = struct
    module T = struct
      type t = {
        created_at : Ptime.t option;
        description : string option;
        enabled : bool option;
        homepage : string option;
        latest_version : string option;
        name : string option;
        peertube_engine : string option;
        settings : Jsont.json option;
        type_ : int option;  (** - `1`: PLUGIN
      - `2`: THEME
       *)
        uninstalled : bool option;
        updated_at : Ptime.t option;
        version : string option;
      }
    end
  
    module Response = struct
      type t = {
        data : T.t list option;
        total : int option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?created_at ?description ?enabled ?homepage ?latest_version ?name ?peertube_engine ?settings ?type_ ?uninstalled ?updated_at ?version () = { created_at; description; enabled; homepage; latest_version; name; peertube_engine; settings; type_; uninstalled; updated_at; version }
    
    let created_at t = t.created_at
    let description t = t.description
    let enabled t = t.enabled
    let homepage t = t.homepage
    let latest_version t = t.latest_version
    let name t = t.name
    let peertube_engine t = t.peertube_engine
    let settings t = t.settings
    let type_ t = t.type_
    let uninstalled t = t.uninstalled
    let updated_at t = t.updated_at
    let version t = t.version
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Plugin"
        (fun created_at description enabled homepage latest_version name peertube_engine settings type_ uninstalled updated_at version -> { created_at; description; enabled; homepage; latest_version; name; peertube_engine; settings; type_; uninstalled; updated_at; version })
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.opt_mem "enabled" Jsont.bool ~enc:(fun r -> r.enabled)
      |> Jsont.Object.opt_mem "homepage" Jsont.string ~enc:(fun r -> r.homepage)
      |> Jsont.Object.opt_mem "latestVersion" Jsont.string ~enc:(fun r -> r.latest_version)
      |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "peertubeEngine" Jsont.string ~enc:(fun r -> r.peertube_engine)
      |> Jsont.Object.opt_mem "settings" Jsont.json ~enc:(fun r -> r.settings)
      |> Jsont.Object.opt_mem "type" Jsont.int ~enc:(fun r -> r.type_)
      |> Jsont.Object.opt_mem "uninstalled" Jsont.bool ~enc:(fun r -> r.uninstalled)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "version" Jsont.string ~enc:(fun r -> r.version)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Response = struct
    include Types.Response
    
    let v ?data ?total () = { data; total }
    
    let data t = t.data
    let total t = t.total
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PluginResponse"
        (fun data total -> { data; total })
      |> Jsont.Object.opt_mem "data" (Openapi.Runtime.validated_list ~max_items:100 T.jsont) ~enc:(fun r -> r.data)
      |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List plugins 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_plugins ?plugin_type ?uninstalled ?start ?count ?sort client () =
    let op_name = "get_plugins" in
    let url_path = "/api/v1/plugins" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"pluginType" ~value:plugin_type; Openapi.Runtime.Query.optional ~key:"uninstalled" ~value:uninstalled; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List available plugins 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_available_plugins ?search ?plugin_type ?current_peer_tube_engine ?start ?count ?sort client () =
    let op_name = "get_available_plugins" in
    let url_path = "/api/v1/plugins/available" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"pluginType" ~value:plugin_type; Openapi.Runtime.Query.optional ~key:"currentPeerTubeEngine" ~value:current_peer_tube_engine; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get a plugin 
      @param npm_name name of the plugin/theme on npmjs.com or in its package.json
  *)
  let get_plugin ~npm_name client () =
    let op_name = "get_plugin" in
    let url_path = Openapi.Runtime.Path.render ~params:[("npmName", npm_name)] "/api/v1/plugins/{npmName}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module PlayerThemeVideoSetting = struct
  module Types = struct
    module T = struct
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
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"PlayerThemeVideoSetting"
        ~dec:(function
          | "channel-default" -> `Channel_default
          | "instance-default" -> `Instance_default
          | "galaxy" -> `Galaxy
          | "lucide" -> `Lucide
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Channel_default -> "channel-default"
          | `Instance_default -> "instance-default"
          | `Galaxy -> "galaxy"
          | `Lucide -> "lucide")
  end
end

module PlayerVideoSettings = struct
  module Types = struct
    module Update = struct
      (** Player settings update for a video *)
      type t = {
        theme : PlayerThemeVideoSetting.T.t;
      }
    end
  
    module T = struct
      (** Player settings for a video *)
      type t = {
        theme : PlayerThemeVideoSetting.T.t option;
      }
    end
  end
  
  module Update = struct
    include Types.Update
    
    let v ~theme () = { theme }
    
    let theme t = t.theme
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PlayerVideoSettingsUpdate"
        (fun theme -> { theme })
      |> Jsont.Object.mem "theme" PlayerThemeVideoSetting.T.jsont ~enc:(fun r -> r.theme)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module T = struct
    include Types.T
    
    let v ?theme () = { theme }
    
    let theme t = t.theme
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PlayerVideoSettings"
        (fun theme -> { theme })
      |> Jsont.Object.opt_mem "theme" PlayerThemeVideoSetting.T.jsont ~enc:(fun r -> r.theme)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get video player settings
  
      Get player settings for a specific video. Returns video-specific settings merged with channel player settings. 
      @param id The object id, uuid or short uuid
      @param raw Return raw settings without merging channel defaults
  *)
  let get_video_player_settings ~id ?raw client () =
    let op_name = "get_video_player_settings" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/player-settings/videos/{id}" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"raw" ~value:raw]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update video player settings
  
      Update player settings for a specific video 
      @param id The object id, uuid or short uuid
  *)
  let update_video_player_settings ~id ~body client () =
    let op_name = "update_video_player_settings" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/player-settings/videos/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json Update.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module PlayerThemeChannelSetting = struct
  module Types = struct
    module T = struct
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
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"PlayerThemeChannelSetting"
        ~dec:(function
          | "instance-default" -> `Instance_default
          | "galaxy" -> `Galaxy
          | "lucide" -> `Lucide
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Instance_default -> "instance-default"
          | `Galaxy -> "galaxy"
          | `Lucide -> "lucide")
  end
end

module PlayerChannelSettings = struct
  module Types = struct
    module Update = struct
      (** Player settings update for a channel *)
      type t = {
        theme : PlayerThemeChannelSetting.T.t;
      }
    end
  
    module T = struct
      (** Player settings for a channel *)
      type t = {
        theme : PlayerThemeChannelSetting.T.t option;
      }
    end
  end
  
  module Update = struct
    include Types.Update
    
    let v ~theme () = { theme }
    
    let theme t = t.theme
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PlayerChannelSettingsUpdate"
        (fun theme -> { theme })
      |> Jsont.Object.mem "theme" PlayerThemeChannelSetting.T.jsont ~enc:(fun r -> r.theme)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module T = struct
    include Types.T
    
    let v ?theme () = { theme }
    
    let theme t = t.theme
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PlayerChannelSettings"
        (fun theme -> { theme })
      |> Jsont.Object.opt_mem "theme" PlayerThemeChannelSetting.T.jsont ~enc:(fun r -> r.theme)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get channel player settings
  
      Get player settings for a video channel. 
      @param channel_handle The video channel handle
      @param raw Return raw settings without applying instance defaults
  *)
  let get_channel_player_settings ~channel_handle ?raw client () =
    let op_name = "get_channel_player_settings" in
    let url_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/player-settings/video-channels/{channelHandle}" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"raw" ~value:raw]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update channel player settings
  
      Update default player settings for a video channel. 
      @param channel_handle The video channel handle
  *)
  let update_channel_player_settings ~channel_handle ~body client () =
    let op_name = "update_channel_player_settings" in
    let url_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/player-settings/video-channels/{channelHandle}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json Update.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module PlayerTheme = struct
  module Types = struct
    module T = struct
      (** The player theme to use *)
      type t = [
        | `Galaxy
        | `Lucide
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"PlayerTheme"
        ~dec:(function
          | "galaxy" -> `Galaxy
          | "lucide" -> `Lucide
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Galaxy -> "galaxy"
          | `Lucide -> "lucide")
  end
end

module PlaybackMetric = struct
  module Types = struct
    module Create = struct
      type t = {
        buffer_stalled : float option;  (** How many times buffer has been stalled since the last metric creation *)
        downloaded_bytes_http : float;  (** How many bytes were downloaded with HTTP since the last metric creation *)
        downloaded_bytes_p2_p : float;  (** How many bytes were downloaded with P2P since the last metric creation *)
        errors : float;  (** How many errors occurred since the last metric creation *)
        fps : float option;  (** Current player video fps *)
        p2p_enabled : bool;
        p2p_peers : float option;  (** P2P peers connected (doesn't include WebSeed peers) *)
        player_mode : string;
        resolution : float option;  (** Current player video resolution *)
        resolution_changes : float;  (** How many resolution changes occurred since the last metric creation *)
        uploaded_bytes_p2_p : float;  (** How many bytes were uploaded with P2P since the last metric creation *)
        video_id : Jsont.json;
      }
    end
  end
  
  module Create = struct
    include Types.Create
    
    let v ~downloaded_bytes_http ~downloaded_bytes_p2_p ~errors ~p2p_enabled ~player_mode ~resolution_changes ~uploaded_bytes_p2_p ~video_id ?buffer_stalled ?fps ?p2p_peers ?resolution () = { buffer_stalled; downloaded_bytes_http; downloaded_bytes_p2_p; errors; fps; p2p_enabled; p2p_peers; player_mode; resolution; resolution_changes; uploaded_bytes_p2_p; video_id }
    
    let buffer_stalled t = t.buffer_stalled
    let downloaded_bytes_http t = t.downloaded_bytes_http
    let downloaded_bytes_p2_p t = t.downloaded_bytes_p2_p
    let errors t = t.errors
    let fps t = t.fps
    let p2p_enabled t = t.p2p_enabled
    let p2p_peers t = t.p2p_peers
    let player_mode t = t.player_mode
    let resolution t = t.resolution
    let resolution_changes t = t.resolution_changes
    let uploaded_bytes_p2_p t = t.uploaded_bytes_p2_p
    let video_id t = t.video_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PlaybackMetricCreate"
        (fun buffer_stalled downloaded_bytes_http downloaded_bytes_p2_p errors fps p2p_enabled p2p_peers player_mode resolution resolution_changes uploaded_bytes_p2_p video_id -> { buffer_stalled; downloaded_bytes_http; downloaded_bytes_p2_p; errors; fps; p2p_enabled; p2p_peers; player_mode; resolution; resolution_changes; uploaded_bytes_p2_p; video_id })
      |> Jsont.Object.opt_mem "bufferStalled" Jsont.number ~enc:(fun r -> r.buffer_stalled)
      |> Jsont.Object.mem "downloadedBytesHTTP" Jsont.number ~enc:(fun r -> r.downloaded_bytes_http)
      |> Jsont.Object.mem "downloadedBytesP2P" Jsont.number ~enc:(fun r -> r.downloaded_bytes_p2_p)
      |> Jsont.Object.mem "errors" Jsont.number ~enc:(fun r -> r.errors)
      |> Jsont.Object.opt_mem "fps" Jsont.number ~enc:(fun r -> r.fps)
      |> Jsont.Object.mem "p2pEnabled" Jsont.bool ~enc:(fun r -> r.p2p_enabled)
      |> Jsont.Object.opt_mem "p2pPeers" Jsont.number ~enc:(fun r -> r.p2p_peers)
      |> Jsont.Object.mem "playerMode" Jsont.string ~enc:(fun r -> r.player_mode)
      |> Jsont.Object.opt_mem "resolution" Jsont.number ~enc:(fun r -> r.resolution)
      |> Jsont.Object.mem "resolutionChanges" Jsont.number ~enc:(fun r -> r.resolution_changes)
      |> Jsont.Object.mem "uploadedBytesP2P" Jsont.number ~enc:(fun r -> r.uploaded_bytes_p2_p)
      |> Jsont.Object.mem "videoId" Jsont.json ~enc:(fun r -> r.video_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Password = struct
  module Types = struct
    module T = struct
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module UpdateUser = struct
  module Types = struct
    module T = struct
      type t = {
        admin_flags : UserAdminFlags.T.t option;
        email : Jsont.json option;  (** The updated email of the user *)
        email_verified : bool option;  (** Set the email as verified *)
        password : Password.T.t option;
        plugin_auth : string option;  (** The auth plugin to use to authenticate the user *)
        role : UserRole.T.t option;
        video_quota : int option;  (** The updated video quota of the user in bytes *)
        video_quota_daily : int option;  (** The updated daily video quota of the user in bytes *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?admin_flags ?email ?email_verified ?password ?plugin_auth ?role ?video_quota ?video_quota_daily () = { admin_flags; email; email_verified; password; plugin_auth; role; video_quota; video_quota_daily }
    
    let admin_flags t = t.admin_flags
    let email t = t.email
    let email_verified t = t.email_verified
    let password t = t.password
    let plugin_auth t = t.plugin_auth
    let role t = t.role
    let video_quota t = t.video_quota
    let video_quota_daily t = t.video_quota_daily
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"UpdateUser"
        (fun admin_flags email email_verified password plugin_auth role video_quota video_quota_daily -> { admin_flags; email; email_verified; password; plugin_auth; role; video_quota; video_quota_daily })
      |> Jsont.Object.opt_mem "adminFlags" UserAdminFlags.T.jsont ~enc:(fun r -> r.admin_flags)
      |> Jsont.Object.opt_mem "email" Jsont.json ~enc:(fun r -> r.email)
      |> Jsont.Object.opt_mem "emailVerified" Jsont.bool ~enc:(fun r -> r.email_verified)
      |> Jsont.Object.opt_mem "password" Password.T.jsont ~enc:(fun r -> r.password)
      |> Jsont.Object.mem "pluginAuth" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.plugin_auth)
      |> Jsont.Object.opt_mem "role" UserRole.T.jsont ~enc:(fun r -> r.role)
      |> Jsont.Object.opt_mem "videoQuota" Jsont.int ~enc:(fun r -> r.video_quota)
      |> Jsont.Object.opt_mem "videoQuotaDaily" Jsont.int ~enc:(fun r -> r.video_quota_daily)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module RegisterUser = struct
  module Types = struct
    module T = struct
      type t = {
        channel : Jsont.json option;  (** channel base information used to create the first channel of the user *)
        display_name : string option;  (** editable name of the user, displayed in its representations *)
        email : string;  (** email of the user, used for login or service communications *)
        password : Password.T.t;
        username : Username.T.t;  (** immutable name of the user, used to find or mention its actor *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~email ~password ~username ?channel ?display_name () = { channel; display_name; email; password; username }
    
    let channel t = t.channel
    let display_name t = t.display_name
    let email t = t.email
    let password t = t.password
    let username t = t.username
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"RegisterUser"
        (fun channel display_name email password username -> { channel; display_name; email; password; username })
      |> Jsont.Object.opt_mem "channel" Jsont.json ~enc:(fun r -> r.channel)
      |> Jsont.Object.opt_mem "displayName" (Openapi.Runtime.validated_string ~min_length:1 ~max_length:120 Jsont.string) ~enc:(fun r -> r.display_name)
      |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
      |> Jsont.Object.mem "password" Password.T.jsont ~enc:(fun r -> r.password)
      |> Jsont.Object.mem "username" Username.T.jsont ~enc:(fun r -> r.username)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module OauthTokenPassword = struct
  module Types = struct
    module T = struct
      type t = {
        client_id : string;
        client_secret : string;
        grant_type : string;
        username : Jsont.json;
        password : Password.T.t option;
        external_auth_token : string option;  (** If you want to authenticate using an external authentication token you got from an auth plugin (like `peertube-plugin-auth-openid-connect` for example) instead of a password or a refresh token, provide it here. *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~client_id ~client_secret ~grant_type ~username ?password ?external_auth_token () = { client_id; client_secret; grant_type; username; password; external_auth_token }
    
    let client_id t = t.client_id
    let client_secret t = t.client_secret
    let grant_type t = t.grant_type
    let username t = t.username
    let password t = t.password
    let external_auth_token t = t.external_auth_token
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"OAuthToken-password"
        (fun client_id client_secret grant_type username password external_auth_token -> { client_id; client_secret; grant_type; username; password; external_auth_token })
      |> Jsont.Object.mem "client_id" (Openapi.Runtime.validated_string ~min_length:32 ~max_length:32 ~pattern:"/^[a-z0-9]$/" Jsont.string) ~enc:(fun r -> r.client_id)
      |> Jsont.Object.mem "client_secret" (Openapi.Runtime.validated_string ~min_length:32 ~max_length:32 ~pattern:"/^[a-zA-Z0-9]$/" Jsont.string) ~enc:(fun r -> r.client_secret)
      |> Jsont.Object.mem "grant_type" Jsont.string ~enc:(fun r -> r.grant_type)
      |> Jsont.Object.mem "username" Jsont.json ~enc:(fun r -> r.username)
      |> Jsont.Object.opt_mem "password" Password.T.jsont ~enc:(fun r -> r.password)
      |> Jsont.Object.opt_mem "externalAuthToken" Jsont.string ~enc:(fun r -> r.external_auth_token)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AddUser = struct
  module Types = struct
    module Response = struct
      type t = {
        user : Jsont.json option;
      }
    end
  
    module T = struct
      type t = {
        admin_flags : UserAdminFlags.T.t option;
        channel_name : UsernameChannel.T.t option;
        email : string;  (** The user email *)
        password : Password.T.t;
        role : UserRole.T.t;
        username : Username.T.t;
        video_quota : int option;  (** The user video quota in bytes *)
        video_quota_daily : int option;  (** The user daily video quota in bytes *)
      }
    end
  end
  
  module Response = struct
    include Types.Response
    
    let v ?user () = { user }
    
    let user t = t.user
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AddUserResponse"
        (fun user -> { user })
      |> Jsont.Object.opt_mem "user" Jsont.json ~enc:(fun r -> r.user)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module T = struct
    include Types.T
    
    let v ~email ~password ~role ~username ?admin_flags ?channel_name ?video_quota ?video_quota_daily () = { admin_flags; channel_name; email; password; role; username; video_quota; video_quota_daily }
    
    let admin_flags t = t.admin_flags
    let channel_name t = t.channel_name
    let email t = t.email
    let password t = t.password
    let role t = t.role
    let username t = t.username
    let video_quota t = t.video_quota
    let video_quota_daily t = t.video_quota_daily
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AddUser"
        (fun admin_flags channel_name email password role username video_quota video_quota_daily -> { admin_flags; channel_name; email; password; role; username; video_quota; video_quota_daily })
      |> Jsont.Object.opt_mem "adminFlags" UserAdminFlags.T.jsont ~enc:(fun r -> r.admin_flags)
      |> Jsont.Object.opt_mem "channelName" UsernameChannel.T.jsont ~enc:(fun r -> r.channel_name)
      |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
      |> Jsont.Object.mem "password" Password.T.jsont ~enc:(fun r -> r.password)
      |> Jsont.Object.mem "role" UserRole.T.jsont ~enc:(fun r -> r.role)
      |> Jsont.Object.mem "username" Username.T.jsont ~enc:(fun r -> r.username)
      |> Jsont.Object.opt_mem "videoQuota" Jsont.int ~enc:(fun r -> r.video_quota)
      |> Jsont.Object.opt_mem "videoQuotaDaily" Jsont.int ~enc:(fun r -> r.video_quota_daily)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Create a user *)
  let add_user ~body client () =
    let op_name = "add_user" in
    let url_path = "/api/v1/users" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module OauthTokenRefreshToken = struct
  module Types = struct
    module T = struct
      type t = {
        client_id : string;
        client_secret : string;
        grant_type : string;
        refresh_token : string;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~client_id ~client_secret ~grant_type ~refresh_token () = { client_id; client_secret; grant_type; refresh_token }
    
    let client_id t = t.client_id
    let client_secret t = t.client_secret
    let grant_type t = t.grant_type
    let refresh_token t = t.refresh_token
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"OAuthToken-refresh_token"
        (fun client_id client_secret grant_type refresh_token -> { client_id; client_secret; grant_type; refresh_token })
      |> Jsont.Object.mem "client_id" (Openapi.Runtime.validated_string ~min_length:32 ~max_length:32 ~pattern:"/^[a-z0-9]$/" Jsont.string) ~enc:(fun r -> r.client_id)
      |> Jsont.Object.mem "client_secret" (Openapi.Runtime.validated_string ~min_length:32 ~max_length:32 ~pattern:"/^[a-zA-Z0-9]$/" Jsont.string) ~enc:(fun r -> r.client_secret)
      |> Jsont.Object.mem "grant_type" Jsont.string ~enc:(fun r -> r.grant_type)
      |> Jsont.Object.mem "refresh_token" Jsont.string ~enc:(fun r -> r.refresh_token)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module OauthClient = struct
  module Types = struct
    module T = struct
      type t = {
        client_id : string option;
        client_secret : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?client_id ?client_secret () = { client_id; client_secret }
    
    let client_id t = t.client_id
    let client_secret t = t.client_secret
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"OAuthClient"
        (fun client_id client_secret -> { client_id; client_secret })
      |> Jsont.Object.opt_mem "client_id" (Openapi.Runtime.validated_string ~min_length:32 ~max_length:32 ~pattern:"/^[a-z0-9]$/" Jsont.string) ~enc:(fun r -> r.client_id)
      |> Jsont.Object.opt_mem "client_secret" (Openapi.Runtime.validated_string ~min_length:32 ~max_length:32 ~pattern:"/^[a-zA-Z0-9]$/" Jsont.string) ~enc:(fun r -> r.client_secret)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Login prerequisite
  
      You need to retrieve a client id and secret before [logging in](#operation/getOAuthToken). *)
  let get_oauth_client client () =
    let op_name = "get_oauth_client" in
    let url_path = "/api/v1/oauth-clients/local" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module Nsfwpolicy = struct
  module Types = struct
    module T = struct
      type t = [
        | `Display
        | `Warn
        | `Do_not_list
      ]
    end
  end
  
  module T = struct
    include Types.T
    
    let jsont : t Jsont.t =
      Jsont.map Jsont.string ~kind:"NSFWPolicy"
        ~dec:(function
          | "display" -> `Display
          | "warn" -> `Warn
          | "do_not_list" -> `Do_not_list
          | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %s" s)
        ~enc:(function
          | `Display -> "display"
          | `Warn -> "warn"
          | `Do_not_list -> "do_not_list")
  end
end

module Nsfwflag = struct
  module Types = struct
    module T = struct
      (** 
      NSFW flags (can be combined using bitwise or operator)
      - `0` NONE
      - `1` VIOLENT
      - `2` EXPLICIT_SEX
       *)
      type t = int
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.int
  end
end

module UpdateMe = struct
  module Types = struct
    module T = struct
      type t = {
        auto_play_next_video : bool option;  (** new preference regarding playing following videos automatically *)
        auto_play_next_video_playlist : bool option;  (** new preference regarding playing following playlist videos automatically *)
        auto_play_video : bool option;  (** new preference regarding playing videos automatically *)
        current_password : Password.T.t option;
        display_name : string option;  (** new name of the user in its representations *)
        email : Jsont.json option;  (** new email used for login and service communications *)
        language : string option;  (** default language for this user *)
        no_account_setup_warning_modal : bool option;
        no_instance_config_warning_modal : bool option;
        no_welcome_modal : bool option;
        nsfw_flags_blurred : Nsfwflag.T.t option;
        nsfw_flags_displayed : Nsfwflag.T.t option;
        nsfw_flags_hidden : Nsfwflag.T.t option;
        nsfw_flags_warned : Nsfwflag.T.t option;
        nsfw_policy : string option;  (** new NSFW display policy *)
        p2p_enabled : bool option;  (** whether to enable P2P in the player or not *)
        password : Password.T.t option;
        theme : string option;
        video_languages : string list option;  (** list of languages to filter videos down to *)
        videos_history_enabled : bool option;  (** whether to keep track of watched history or not *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?auto_play_next_video ?auto_play_next_video_playlist ?auto_play_video ?current_password ?display_name ?email ?language ?no_account_setup_warning_modal ?no_instance_config_warning_modal ?no_welcome_modal ?nsfw_flags_blurred ?nsfw_flags_displayed ?nsfw_flags_hidden ?nsfw_flags_warned ?nsfw_policy ?p2p_enabled ?password ?theme ?video_languages ?videos_history_enabled () = { auto_play_next_video; auto_play_next_video_playlist; auto_play_video; current_password; display_name; email; language; no_account_setup_warning_modal; no_instance_config_warning_modal; no_welcome_modal; nsfw_flags_blurred; nsfw_flags_displayed; nsfw_flags_hidden; nsfw_flags_warned; nsfw_policy; p2p_enabled; password; theme; video_languages; videos_history_enabled }
    
    let auto_play_next_video t = t.auto_play_next_video
    let auto_play_next_video_playlist t = t.auto_play_next_video_playlist
    let auto_play_video t = t.auto_play_video
    let current_password t = t.current_password
    let display_name t = t.display_name
    let email t = t.email
    let language t = t.language
    let no_account_setup_warning_modal t = t.no_account_setup_warning_modal
    let no_instance_config_warning_modal t = t.no_instance_config_warning_modal
    let no_welcome_modal t = t.no_welcome_modal
    let nsfw_flags_blurred t = t.nsfw_flags_blurred
    let nsfw_flags_displayed t = t.nsfw_flags_displayed
    let nsfw_flags_hidden t = t.nsfw_flags_hidden
    let nsfw_flags_warned t = t.nsfw_flags_warned
    let nsfw_policy t = t.nsfw_policy
    let p2p_enabled t = t.p2p_enabled
    let password t = t.password
    let theme t = t.theme
    let video_languages t = t.video_languages
    let videos_history_enabled t = t.videos_history_enabled
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"UpdateMe"
        (fun auto_play_next_video auto_play_next_video_playlist auto_play_video current_password display_name email language no_account_setup_warning_modal no_instance_config_warning_modal no_welcome_modal nsfw_flags_blurred nsfw_flags_displayed nsfw_flags_hidden nsfw_flags_warned nsfw_policy p2p_enabled password theme video_languages videos_history_enabled -> { auto_play_next_video; auto_play_next_video_playlist; auto_play_video; current_password; display_name; email; language; no_account_setup_warning_modal; no_instance_config_warning_modal; no_welcome_modal; nsfw_flags_blurred; nsfw_flags_displayed; nsfw_flags_hidden; nsfw_flags_warned; nsfw_policy; p2p_enabled; password; theme; video_languages; videos_history_enabled })
      |> Jsont.Object.opt_mem "autoPlayNextVideo" Jsont.bool ~enc:(fun r -> r.auto_play_next_video)
      |> Jsont.Object.opt_mem "autoPlayNextVideoPlaylist" Jsont.bool ~enc:(fun r -> r.auto_play_next_video_playlist)
      |> Jsont.Object.opt_mem "autoPlayVideo" Jsont.bool ~enc:(fun r -> r.auto_play_video)
      |> Jsont.Object.opt_mem "currentPassword" Password.T.jsont ~enc:(fun r -> r.current_password)
      |> Jsont.Object.opt_mem "displayName" (Openapi.Runtime.validated_string ~min_length:3 ~max_length:120 Jsont.string) ~enc:(fun r -> r.display_name)
      |> Jsont.Object.opt_mem "email" Jsont.json ~enc:(fun r -> r.email)
      |> Jsont.Object.opt_mem "language" Jsont.string ~enc:(fun r -> r.language)
      |> Jsont.Object.opt_mem "noAccountSetupWarningModal" Jsont.bool ~enc:(fun r -> r.no_account_setup_warning_modal)
      |> Jsont.Object.opt_mem "noInstanceConfigWarningModal" Jsont.bool ~enc:(fun r -> r.no_instance_config_warning_modal)
      |> Jsont.Object.opt_mem "noWelcomeModal" Jsont.bool ~enc:(fun r -> r.no_welcome_modal)
      |> Jsont.Object.opt_mem "nsfwFlagsBlurred" Nsfwflag.T.jsont ~enc:(fun r -> r.nsfw_flags_blurred)
      |> Jsont.Object.opt_mem "nsfwFlagsDisplayed" Nsfwflag.T.jsont ~enc:(fun r -> r.nsfw_flags_displayed)
      |> Jsont.Object.opt_mem "nsfwFlagsHidden" Nsfwflag.T.jsont ~enc:(fun r -> r.nsfw_flags_hidden)
      |> Jsont.Object.opt_mem "nsfwFlagsWarned" Nsfwflag.T.jsont ~enc:(fun r -> r.nsfw_flags_warned)
      |> Jsont.Object.opt_mem "nsfwPolicy" Jsont.string ~enc:(fun r -> r.nsfw_policy)
      |> Jsont.Object.opt_mem "p2pEnabled" Jsont.bool ~enc:(fun r -> r.p2p_enabled)
      |> Jsont.Object.opt_mem "password" Password.T.jsont ~enc:(fun r -> r.password)
      |> Jsont.Object.opt_mem "theme" Jsont.string ~enc:(fun r -> r.theme)
      |> Jsont.Object.opt_mem "videoLanguages" (Jsont.list Jsont.string) ~enc:(fun r -> r.video_languages)
      |> Jsont.Object.opt_mem "videosHistoryEnabled" Jsont.bool ~enc:(fun r -> r.videos_history_enabled)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module NotificationSettingValue = struct
  module Types = struct
    module T = struct
      (** Notification type. One of the following values, or a sum of multiple values:
      - `0` NONE
      - `1` WEB
      - `2` EMAIL
       *)
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module UserNotificationSettings = struct
  module Types = struct
    module T = struct
      type t = {
        abuse_as_moderator : NotificationSettingValue.T.t option;
        abuse_new_message : NotificationSettingValue.T.t option;
        abuse_state_change : NotificationSettingValue.T.t option;
        auto_instance_following : NotificationSettingValue.T.t option;
        blacklist_on_my_video : NotificationSettingValue.T.t option;
        comment_mention : NotificationSettingValue.T.t option;
        my_video_import_finished : NotificationSettingValue.T.t option;
        my_video_published : NotificationSettingValue.T.t option;
        my_video_studio_edition_finished : NotificationSettingValue.T.t option;
        my_video_transcription_generated : NotificationSettingValue.T.t option;
        new_comment_on_my_video : NotificationSettingValue.T.t option;
        new_follow : NotificationSettingValue.T.t option;
        new_instance_follower : NotificationSettingValue.T.t option;
        new_peer_tube_version : NotificationSettingValue.T.t option;
        new_plugin_version : NotificationSettingValue.T.t option;
        new_user_registration : NotificationSettingValue.T.t option;
        new_video_from_subscription : NotificationSettingValue.T.t option;
        video_auto_blacklist_as_moderator : NotificationSettingValue.T.t option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?abuse_as_moderator ?abuse_new_message ?abuse_state_change ?auto_instance_following ?blacklist_on_my_video ?comment_mention ?my_video_import_finished ?my_video_published ?my_video_studio_edition_finished ?my_video_transcription_generated ?new_comment_on_my_video ?new_follow ?new_instance_follower ?new_peer_tube_version ?new_plugin_version ?new_user_registration ?new_video_from_subscription ?video_auto_blacklist_as_moderator () = { abuse_as_moderator; abuse_new_message; abuse_state_change; auto_instance_following; blacklist_on_my_video; comment_mention; my_video_import_finished; my_video_published; my_video_studio_edition_finished; my_video_transcription_generated; new_comment_on_my_video; new_follow; new_instance_follower; new_peer_tube_version; new_plugin_version; new_user_registration; new_video_from_subscription; video_auto_blacklist_as_moderator }
    
    let abuse_as_moderator t = t.abuse_as_moderator
    let abuse_new_message t = t.abuse_new_message
    let abuse_state_change t = t.abuse_state_change
    let auto_instance_following t = t.auto_instance_following
    let blacklist_on_my_video t = t.blacklist_on_my_video
    let comment_mention t = t.comment_mention
    let my_video_import_finished t = t.my_video_import_finished
    let my_video_published t = t.my_video_published
    let my_video_studio_edition_finished t = t.my_video_studio_edition_finished
    let my_video_transcription_generated t = t.my_video_transcription_generated
    let new_comment_on_my_video t = t.new_comment_on_my_video
    let new_follow t = t.new_follow
    let new_instance_follower t = t.new_instance_follower
    let new_peer_tube_version t = t.new_peer_tube_version
    let new_plugin_version t = t.new_plugin_version
    let new_user_registration t = t.new_user_registration
    let new_video_from_subscription t = t.new_video_from_subscription
    let video_auto_blacklist_as_moderator t = t.video_auto_blacklist_as_moderator
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"UserNotificationSettings"
        (fun abuse_as_moderator abuse_new_message abuse_state_change auto_instance_following blacklist_on_my_video comment_mention my_video_import_finished my_video_published my_video_studio_edition_finished my_video_transcription_generated new_comment_on_my_video new_follow new_instance_follower new_peer_tube_version new_plugin_version new_user_registration new_video_from_subscription video_auto_blacklist_as_moderator -> { abuse_as_moderator; abuse_new_message; abuse_state_change; auto_instance_following; blacklist_on_my_video; comment_mention; my_video_import_finished; my_video_published; my_video_studio_edition_finished; my_video_transcription_generated; new_comment_on_my_video; new_follow; new_instance_follower; new_peer_tube_version; new_plugin_version; new_user_registration; new_video_from_subscription; video_auto_blacklist_as_moderator })
      |> Jsont.Object.opt_mem "abuseAsModerator" NotificationSettingValue.T.jsont ~enc:(fun r -> r.abuse_as_moderator)
      |> Jsont.Object.opt_mem "abuseNewMessage" NotificationSettingValue.T.jsont ~enc:(fun r -> r.abuse_new_message)
      |> Jsont.Object.opt_mem "abuseStateChange" NotificationSettingValue.T.jsont ~enc:(fun r -> r.abuse_state_change)
      |> Jsont.Object.opt_mem "autoInstanceFollowing" NotificationSettingValue.T.jsont ~enc:(fun r -> r.auto_instance_following)
      |> Jsont.Object.opt_mem "blacklistOnMyVideo" NotificationSettingValue.T.jsont ~enc:(fun r -> r.blacklist_on_my_video)
      |> Jsont.Object.opt_mem "commentMention" NotificationSettingValue.T.jsont ~enc:(fun r -> r.comment_mention)
      |> Jsont.Object.opt_mem "myVideoImportFinished" NotificationSettingValue.T.jsont ~enc:(fun r -> r.my_video_import_finished)
      |> Jsont.Object.opt_mem "myVideoPublished" NotificationSettingValue.T.jsont ~enc:(fun r -> r.my_video_published)
      |> Jsont.Object.opt_mem "myVideoStudioEditionFinished" NotificationSettingValue.T.jsont ~enc:(fun r -> r.my_video_studio_edition_finished)
      |> Jsont.Object.opt_mem "myVideoTranscriptionGenerated" NotificationSettingValue.T.jsont ~enc:(fun r -> r.my_video_transcription_generated)
      |> Jsont.Object.opt_mem "newCommentOnMyVideo" NotificationSettingValue.T.jsont ~enc:(fun r -> r.new_comment_on_my_video)
      |> Jsont.Object.opt_mem "newFollow" NotificationSettingValue.T.jsont ~enc:(fun r -> r.new_follow)
      |> Jsont.Object.opt_mem "newInstanceFollower" NotificationSettingValue.T.jsont ~enc:(fun r -> r.new_instance_follower)
      |> Jsont.Object.opt_mem "newPeerTubeVersion" NotificationSettingValue.T.jsont ~enc:(fun r -> r.new_peer_tube_version)
      |> Jsont.Object.opt_mem "newPluginVersion" NotificationSettingValue.T.jsont ~enc:(fun r -> r.new_plugin_version)
      |> Jsont.Object.opt_mem "newUserRegistration" NotificationSettingValue.T.jsont ~enc:(fun r -> r.new_user_registration)
      |> Jsont.Object.opt_mem "newVideoFromSubscription" NotificationSettingValue.T.jsont ~enc:(fun r -> r.new_video_from_subscription)
      |> Jsont.Object.opt_mem "videoAutoBlacklistAsModerator" NotificationSettingValue.T.jsont ~enc:(fun r -> r.video_auto_blacklist_as_moderator)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module NewFeatureInfo = struct
  module Types = struct
    module Type = struct
      (** Represent a new feature that can be displayed to inform users. One of the following values:
      
        - `1` CHANNEL_COLLABORATION
       *)
      type t = int
    end
  end
  
  module Type = struct
    include Types.Type
    let jsont = Jsont.int
  end
end

module MrsspeerLink = struct
  module Types = struct
    module T = struct
      type t = {
        href : string option;
        type_ : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?href ?type_ () = { href; type_ }
    
    let href t = t.href
    let type_ t = t.type_
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MRSSPeerLink"
        (fun href type_ -> { href; type_ })
      |> Jsont.Object.opt_mem "href" Jsont.string ~enc:(fun r -> r.href)
      |> Jsont.Object.opt_mem "type" Jsont.string ~enc:(fun r -> r.type_)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module MrssgroupContent = struct
  module Types = struct
    module T = struct
      type t = {
        duration : int option;
        file_size : int option;
        framerate : int option;
        height : int option;
        lang : string option;
        type_ : string option;
        url : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?duration ?file_size ?framerate ?height ?lang ?type_ ?url () = { duration; file_size; framerate; height; lang; type_; url }
    
    let duration t = t.duration
    let file_size t = t.file_size
    let framerate t = t.framerate
    let height t = t.height
    let lang t = t.lang
    let type_ t = t.type_
    let url t = t.url
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"MRSSGroupContent"
        (fun duration file_size framerate height lang type_ url -> { duration; file_size; framerate; height; lang; type_; url })
      |> Jsont.Object.opt_mem "duration" Jsont.int ~enc:(fun r -> r.duration)
      |> Jsont.Object.opt_mem "fileSize" Jsont.int ~enc:(fun r -> r.file_size)
      |> Jsont.Object.opt_mem "framerate" Jsont.int ~enc:(fun r -> r.framerate)
      |> Jsont.Object.opt_mem "height" Jsont.int ~enc:(fun r -> r.height)
      |> Jsont.Object.opt_mem "lang" Jsont.string ~enc:(fun r -> r.lang)
      |> Jsont.Object.opt_mem "type" Jsont.string ~enc:(fun r -> r.type_)
      |> Jsont.Object.opt_mem "url" Jsont.string ~enc:(fun r -> r.url)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module LiveVideoSession = struct
  module Types = struct
    module Response = struct
      type t = {
        end_date : Ptime.t option;  (** End date of the live session *)
        error : int option;  (** Error type if an error occurred during the live session:
        - `1`: Bad socket health (transcoding is too slow)
        - `2`: Max duration exceeded
        - `3`: Quota exceeded
        - `4`: Quota FFmpeg error
        - `5`: Video has been blacklisted during the live
       *)
        id : int option;
        replay_video : Jsont.json option;  (** Video replay information *)
        start_date : Ptime.t option;  (** Start date of the live session *)
      }
    end
  end
  
  module Response = struct
    include Types.Response
    
    let v ?end_date ?error ?id ?replay_video ?start_date () = { end_date; error; id; replay_video; start_date }
    
    let end_date t = t.end_date
    let error t = t.error
    let id t = t.id
    let replay_video t = t.replay_video
    let start_date t = t.start_date
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"LiveVideoSessionResponse"
        (fun end_date error id replay_video start_date -> { end_date; error; id; replay_video; start_date })
      |> Jsont.Object.mem "endDate" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.end_date)
      |> Jsont.Object.mem "error" Openapi.Runtime.nullable_int
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.error)
      |> Jsont.Object.opt_mem "id" Jsont.int ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "replayVideo" Jsont.json ~enc:(fun r -> r.replay_video)
      |> Jsont.Object.opt_mem "startDate" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.start_date)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get live session of a replay
  
      If the video is a replay of a live, you can find the associated live session using this endpoint 
      @param id The object id, uuid or short uuid
  *)
  let get_api_v1_videos_live_session ~id client () =
    let op_name = "get_api_v1_videos_live_session" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/live-session" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module LiveVideoLatencyMode = struct
  module Types = struct
    module T = struct
      (** The live latency mode (Default = `1`, High latency = `2`, Small Latency = `3`) *)
      type t = int
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.int
  end
end

module LiveSchedule = struct
  module Types = struct
    module T = struct
      type t = {
        start_at : Ptime.t option;  (** Date when the stream is scheduled to air at *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?start_at () = { start_at }
    
    let start_at t = t.start_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"LiveSchedule"
        (fun start_at -> { start_at })
      |> Jsont.Object.opt_mem "startAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.start_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module LiveVideo = struct
  module Types = struct
    module Update = struct
      type t = {
        latency_mode : LiveVideoLatencyMode.T.t option;  (** User can select live latency mode if enabled by the instance *)
        permanent_live : bool option;  (** User can stream multiple times in a permanent live *)
        replay_settings : LiveVideoReplaySettings.T.t option;
        save_replay : bool option;
        schedules : LiveSchedule.T.t list option;
      }
    end
  
    module Response = struct
      type t = {
        latency_mode : LiveVideoLatencyMode.T.t option;  (** User can select live latency mode if enabled by the instance *)
        permanent_live : bool option;  (** User can stream multiple times in a permanent live *)
        replay_settings : LiveVideoReplaySettings.T.t option;
        rtmp_url : string option;  (** Included in the response if an appropriate token is provided *)
        rtmps_url : string option;  (** Included in the response if an appropriate token is provided *)
        save_replay : bool option;
        schedules : LiveSchedule.T.t list option;
        stream_key : string option;  (** RTMP stream key to use to stream into this live video. Included in the response if an appropriate token is provided *)
      }
    end
  end
  
  module Update = struct
    include Types.Update
    
    let v ?latency_mode ?permanent_live ?replay_settings ?save_replay ?schedules () = { latency_mode; permanent_live; replay_settings; save_replay; schedules }
    
    let latency_mode t = t.latency_mode
    let permanent_live t = t.permanent_live
    let replay_settings t = t.replay_settings
    let save_replay t = t.save_replay
    let schedules t = t.schedules
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"LiveVideoUpdate"
        (fun latency_mode permanent_live replay_settings save_replay schedules -> { latency_mode; permanent_live; replay_settings; save_replay; schedules })
      |> Jsont.Object.opt_mem "latencyMode" LiveVideoLatencyMode.T.jsont ~enc:(fun r -> r.latency_mode)
      |> Jsont.Object.opt_mem "permanentLive" Jsont.bool ~enc:(fun r -> r.permanent_live)
      |> Jsont.Object.opt_mem "replaySettings" LiveVideoReplaySettings.T.jsont ~enc:(fun r -> r.replay_settings)
      |> Jsont.Object.opt_mem "saveReplay" Jsont.bool ~enc:(fun r -> r.save_replay)
      |> Jsont.Object.opt_mem "schedules" (Jsont.list LiveSchedule.T.jsont) ~enc:(fun r -> r.schedules)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Response = struct
    include Types.Response
    
    let v ?latency_mode ?permanent_live ?replay_settings ?rtmp_url ?rtmps_url ?save_replay ?schedules ?stream_key () = { latency_mode; permanent_live; replay_settings; rtmp_url; rtmps_url; save_replay; schedules; stream_key }
    
    let latency_mode t = t.latency_mode
    let permanent_live t = t.permanent_live
    let replay_settings t = t.replay_settings
    let rtmp_url t = t.rtmp_url
    let rtmps_url t = t.rtmps_url
    let save_replay t = t.save_replay
    let schedules t = t.schedules
    let stream_key t = t.stream_key
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"LiveVideoResponse"
        (fun latency_mode permanent_live replay_settings rtmp_url rtmps_url save_replay schedules stream_key -> { latency_mode; permanent_live; replay_settings; rtmp_url; rtmps_url; save_replay; schedules; stream_key })
      |> Jsont.Object.opt_mem "latencyMode" LiveVideoLatencyMode.T.jsont ~enc:(fun r -> r.latency_mode)
      |> Jsont.Object.opt_mem "permanentLive" Jsont.bool ~enc:(fun r -> r.permanent_live)
      |> Jsont.Object.opt_mem "replaySettings" LiveVideoReplaySettings.T.jsont ~enc:(fun r -> r.replay_settings)
      |> Jsont.Object.opt_mem "rtmpUrl" Jsont.string ~enc:(fun r -> r.rtmp_url)
      |> Jsont.Object.opt_mem "rtmpsUrl" Jsont.string ~enc:(fun r -> r.rtmps_url)
      |> Jsont.Object.opt_mem "saveReplay" Jsont.bool ~enc:(fun r -> r.save_replay)
      |> Jsont.Object.opt_mem "schedules" (Jsont.list LiveSchedule.T.jsont) ~enc:(fun r -> r.schedules)
      |> Jsont.Object.opt_mem "streamKey" Jsont.string ~enc:(fun r -> r.stream_key)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get information about a live 
      @param id The object id, uuid or short uuid
  *)
  let get_live_id ~id client () =
    let op_name = "get_live_id" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/live/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ImportVideosInChannel = struct
  module Types = struct
    module Create = struct
      type t = {
        external_channel_url : string;
        video_channel_sync_id : int option;  (** If part of a channel sync process, specify its id to assign video imports to this channel synchronization *)
      }
    end
  end
  
  module Create = struct
    include Types.Create
    
    let v ~external_channel_url ?video_channel_sync_id () = { external_channel_url; video_channel_sync_id }
    
    let external_channel_url t = t.external_channel_url
    let video_channel_sync_id t = t.video_channel_sync_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ImportVideosInChannelCreate"
        (fun external_channel_url video_channel_sync_id -> { external_channel_url; video_channel_sync_id })
      |> Jsont.Object.mem "externalChannelUrl" Jsont.string ~enc:(fun r -> r.external_channel_url)
      |> Jsont.Object.opt_mem "videoChannelSyncId" Jsont.int ~enc:(fun r -> r.video_channel_sync_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Id = struct
  module Types = struct
    module T = struct
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module WatchedWordsLists = struct
  module Types = struct
    module T = struct
      type t = {
        created_at : Ptime.t option;
        id : Id.T.t option;
        list_name : string option;
        updated_at : Ptime.t option;
        words : string list option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?created_at ?id ?list_name ?updated_at ?words () = { created_at; id; list_name; updated_at; words }
    
    let created_at t = t.created_at
    let id t = t.id
    let list_name t = t.list_name
    let updated_at t = t.updated_at
    let words t = t.words
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"WatchedWordsLists"
        (fun created_at id list_name updated_at words -> { created_at; id; list_name; updated_at; words })
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "listName" Jsont.string ~enc:(fun r -> r.list_name)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "words" (Jsont.list Jsont.string) ~enc:(fun r -> r.words)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoRedundancy = struct
  module Types = struct
    module T = struct
      type t = {
        id : Id.T.t option;
        name : string option;
        redundancies : Jsont.json option;
        url : string option;
        uuid : Uuidv4.T.t option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?id ?name ?redundancies ?url ?uuid () = { id; name; redundancies; url; uuid }
    
    let id t = t.id
    let name t = t.name
    let redundancies t = t.redundancies
    let url t = t.url
    let uuid t = t.uuid
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoRedundancy"
        (fun id name redundancies url uuid -> { id; name; redundancies; url; uuid })
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "redundancies" Jsont.json ~enc:(fun r -> r.redundancies)
      |> Jsont.Object.opt_mem "url" Jsont.string ~enc:(fun r -> r.url)
      |> Jsont.Object.opt_mem "uuid" Uuidv4.T.jsont ~enc:(fun r -> r.uuid)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List videos being mirrored 
      @param target direction of the mirror
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort abuses by criteria
  *)
  let get_mirrored_videos ~target ?start ?count ?sort client () =
    let op_name = "get_mirrored_videos" in
    let url_path = "/api/v1/server/redundancy/videos" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"target" ~value:target; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module VideoPassword = struct
  module Types = struct
    module T = struct
      type t = {
        id : Id.T.t option;
        password : string option;
        video_id : Id.T.t option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?id ?password ?video_id () = { id; password; video_id }
    
    let id t = t.id
    let password t = t.password
    let video_id t = t.video_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoPassword"
        (fun id password video_id -> { id; password; video_id })
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "password" (Openapi.Runtime.validated_string ~min_length:2 Jsont.string) ~enc:(fun r -> r.password)
      |> Jsont.Object.opt_mem "videoId" Id.T.jsont ~enc:(fun r -> r.video_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoPasswordList = struct
  module Types = struct
    module T = struct
      type t = {
        data : VideoPassword.T.t list option;
        total : int option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?data ?total () = { data; total }
    
    let data t = t.data
    let total t = t.total
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoPasswordList"
        (fun data total -> { data; total })
      |> Jsont.Object.opt_mem "data" (Jsont.list VideoPassword.T.jsont) ~enc:(fun r -> r.data)
      |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoBlacklist = struct
  module Types = struct
    module T = struct
      type t = {
        created_at : Ptime.t option;
        description : string option;
        dislikes : int option;
        duration : int option;
        id : Id.T.t option;
        likes : int option;
        name : string option;
        nsfw : bool option;
        updated_at : Ptime.t option;
        uuid : Uuidv4.T.t option;
        video_id : Jsont.json option;
        views : int option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?created_at ?description ?dislikes ?duration ?id ?likes ?name ?nsfw ?updated_at ?uuid ?video_id ?views () = { created_at; description; dislikes; duration; id; likes; name; nsfw; updated_at; uuid; video_id; views }
    
    let created_at t = t.created_at
    let description t = t.description
    let dislikes t = t.dislikes
    let duration t = t.duration
    let id t = t.id
    let likes t = t.likes
    let name t = t.name
    let nsfw t = t.nsfw
    let updated_at t = t.updated_at
    let uuid t = t.uuid
    let video_id t = t.video_id
    let views t = t.views
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoBlacklist"
        (fun created_at description dislikes duration id likes name nsfw updated_at uuid video_id views -> { created_at; description; dislikes; duration; id; likes; name; nsfw; updated_at; uuid; video_id; views })
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "description" (Openapi.Runtime.validated_string ~min_length:3 ~max_length:10000 Jsont.string) ~enc:(fun r -> r.description)
      |> Jsont.Object.opt_mem "dislikes" Jsont.int ~enc:(fun r -> r.dislikes)
      |> Jsont.Object.opt_mem "duration" Jsont.int ~enc:(fun r -> r.duration)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "likes" Jsont.int ~enc:(fun r -> r.likes)
      |> Jsont.Object.opt_mem "name" (Openapi.Runtime.validated_string ~min_length:3 ~max_length:120 Jsont.string) ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "nsfw" Jsont.bool ~enc:(fun r -> r.nsfw)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "uuid" Uuidv4.T.jsont ~enc:(fun r -> r.uuid)
      |> Jsont.Object.opt_mem "videoId" Jsont.json ~enc:(fun r -> r.video_id)
      |> Jsont.Object.opt_mem "views" Jsont.int ~enc:(fun r -> r.views)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module UserRegistration = struct
  module Types = struct
    module Request = struct
      type t = {
        channel : Jsont.json option;  (** channel base information used to create the first channel of the user *)
        display_name : string option;  (** editable name of the user, displayed in its representations *)
        email : string;  (** email of the user, used for login or service communications *)
        password : Password.T.t;
        username : Username.T.t;  (** immutable name of the user, used to find or mention its actor *)
        registration_reason : string;  (** reason for the user to register on the instance *)
      }
    end
  
    module T = struct
      type t = {
        account_display_name : string option;
        channel_display_name : string option;
        channel_handle : string option;
        created_at : Ptime.t option;
        email : string option;
        email_verified : bool option;
        id : Id.T.t option;
        moderation_response : string option;
        registration_reason : string option;
        state : Jsont.json option;
        updated_at : Ptime.t option;
        user : Jsont.json option;  (** If the registration has been accepted, this is a partial user object created by the registration *)
        username : string option;
      }
    end
  end
  
  module Request = struct
    include Types.Request
    
    let v ~email ~password ~username ~registration_reason ?channel ?display_name () = { channel; display_name; email; password; username; registration_reason }
    
    let channel t = t.channel
    let display_name t = t.display_name
    let email t = t.email
    let password t = t.password
    let username t = t.username
    let registration_reason t = t.registration_reason
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"UserRegistrationRequest"
        (fun channel display_name email password username registration_reason -> { channel; display_name; email; password; username; registration_reason })
      |> Jsont.Object.opt_mem "channel" Jsont.json ~enc:(fun r -> r.channel)
      |> Jsont.Object.opt_mem "displayName" (Openapi.Runtime.validated_string ~min_length:1 ~max_length:120 Jsont.string) ~enc:(fun r -> r.display_name)
      |> Jsont.Object.mem "email" Jsont.string ~enc:(fun r -> r.email)
      |> Jsont.Object.mem "password" Password.T.jsont ~enc:(fun r -> r.password)
      |> Jsont.Object.mem "username" Username.T.jsont ~enc:(fun r -> r.username)
      |> Jsont.Object.mem "registrationReason" Jsont.string ~enc:(fun r -> r.registration_reason)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module T = struct
    include Types.T
    
    let v ?account_display_name ?channel_display_name ?channel_handle ?created_at ?email ?email_verified ?id ?moderation_response ?registration_reason ?state ?updated_at ?user ?username () = { account_display_name; channel_display_name; channel_handle; created_at; email; email_verified; id; moderation_response; registration_reason; state; updated_at; user; username }
    
    let account_display_name t = t.account_display_name
    let channel_display_name t = t.channel_display_name
    let channel_handle t = t.channel_handle
    let created_at t = t.created_at
    let email t = t.email
    let email_verified t = t.email_verified
    let id t = t.id
    let moderation_response t = t.moderation_response
    let registration_reason t = t.registration_reason
    let state t = t.state
    let updated_at t = t.updated_at
    let user t = t.user
    let username t = t.username
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"UserRegistration"
        (fun account_display_name channel_display_name channel_handle created_at email email_verified id moderation_response registration_reason state updated_at user username -> { account_display_name; channel_display_name; channel_handle; created_at; email; email_verified; id; moderation_response; registration_reason; state; updated_at; user; username })
      |> Jsont.Object.opt_mem "accountDisplayName" Jsont.string ~enc:(fun r -> r.account_display_name)
      |> Jsont.Object.opt_mem "channelDisplayName" Jsont.string ~enc:(fun r -> r.channel_display_name)
      |> Jsont.Object.opt_mem "channelHandle" Jsont.string ~enc:(fun r -> r.channel_handle)
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "email" Jsont.string ~enc:(fun r -> r.email)
      |> Jsont.Object.opt_mem "emailVerified" Jsont.bool ~enc:(fun r -> r.email_verified)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "moderationResponse" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.moderation_response)
      |> Jsont.Object.opt_mem "registrationReason" Jsont.string ~enc:(fun r -> r.registration_reason)
      |> Jsont.Object.opt_mem "state" Jsont.json ~enc:(fun r -> r.state)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.mem "user" (Openapi.Runtime.nullable_any Jsont.json)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.user)
      |> Jsont.Object.opt_mem "username" Jsont.string ~enc:(fun r -> r.username)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Request registration
  
      Signup has to be enabled and require approval on the instance *)
  let request_registration ~body client () =
    let op_name = "request_registration" in
    let url_path = "/api/v1/users/registrations/request" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json Request.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module Job = struct
  module Types = struct
    module T = struct
      type t = {
        created_at : Ptime.t option;
        data : Jsont.json option;
        error : Jsont.json option;
        finished_on : Ptime.t option;
        id : Id.T.t option;
        processed_on : Ptime.t option;
        state : string option;
        type_ : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?created_at ?data ?error ?finished_on ?id ?processed_on ?state ?type_ () = { created_at; data; error; finished_on; id; processed_on; state; type_ }
    
    let created_at t = t.created_at
    let data t = t.data
    let error t = t.error
    let finished_on t = t.finished_on
    let id t = t.id
    let processed_on t = t.processed_on
    let state t = t.state
    let type_ t = t.type_
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Job"
        (fun created_at data error finished_on id processed_on state type_ -> { created_at; data; error; finished_on; id; processed_on; state; type_ })
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "data" Jsont.json ~enc:(fun r -> r.data)
      |> Jsont.Object.opt_mem "error" Jsont.json ~enc:(fun r -> r.error)
      |> Jsont.Object.opt_mem "finishedOn" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.finished_on)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "processedOn" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.processed_on)
      |> Jsont.Object.opt_mem "state" Jsont.string ~enc:(fun r -> r.state)
      |> Jsont.Object.opt_mem "type" Jsont.string ~enc:(fun r -> r.type_)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module GetMeVideoRating = struct
  module Types = struct
    module T = struct
      type t = {
        id : Id.T.t;
        rating : string;  (** Rating of the video *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~id ~rating () = { id; rating }
    
    let id t = t.id
    let rating t = t.rating
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"GetMeVideoRating"
        (fun id rating -> { id; rating })
      |> Jsont.Object.mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "rating" Jsont.string ~enc:(fun r -> r.rating)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get rate of my user for a video 
      @param video_id The video id
  *)
  let get_api_v1_users_me_videos_rating ~video_id client () =
    let op_name = "get_api_v1_users_me_videos_rating" in
    let url_path = Openapi.Runtime.Path.render ~params:[("videoId", video_id)] "/api/v1/users/me/videos/{videoId}/rating" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module FileRedundancyInformation = struct
  module Types = struct
    module T = struct
      type t = {
        created_at : Ptime.t option;
        expires_on : Ptime.t option;
        file_url : string option;
        id : Id.T.t option;
        size : int option;
        strategy : string option;
        updated_at : Ptime.t option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?created_at ?expires_on ?file_url ?id ?size ?strategy ?updated_at () = { created_at; expires_on; file_url; id; size; strategy; updated_at }
    
    let created_at t = t.created_at
    let expires_on t = t.expires_on
    let file_url t = t.file_url
    let id t = t.id
    let size t = t.size
    let strategy t = t.strategy
    let updated_at t = t.updated_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"FileRedundancyInformation"
        (fun created_at expires_on file_url id size strategy updated_at -> { created_at; expires_on; file_url; id; size; strategy; updated_at })
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "expiresOn" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.expires_on)
      |> Jsont.Object.opt_mem "fileUrl" Jsont.string ~enc:(fun r -> r.file_url)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "size" Jsont.int ~enc:(fun r -> r.size)
      |> Jsont.Object.opt_mem "strategy" Jsont.string ~enc:(fun r -> r.strategy)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module FileStorage = struct
  module Types = struct
    module T = struct
      (** The file storage type:
        - `0` File system
        - `1` Object storage
       *)
      type t = int
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.int
  end
end

module VideoFile = struct
  module Types = struct
    module T = struct
      type t = {
        file_download_url : string option;  (** URL endpoint that transfers the video file as an attachment (so that the browser opens a download dialog) *)
        file_url : string option;  (** Direct URL of the video *)
        fps : float option;  (** Frames per second of the video file *)
        has_audio : bool option;  (** **PeerTube >= 6.2** The file container has an audio stream *)
        has_video : bool option;  (** **PeerTube >= 6.2** The file container has a video stream *)
        height : float option;  (** **PeerTube >= 6.1** Video stream height *)
        id : Id.T.t option;
        magnet_uri : string option;  (** magnet URI allowing to resolve the video via BitTorrent without a metainfo file *)
        metadata_url : string option;  (** URL dereferencing the output of ffprobe on the file *)
        playlist_url : string option;  (** Playlist URL of the file if it is owned by a playlist *)
        resolution : VideoResolutionConstant.T.t option;
        size : int option;  (** Video file size in bytes *)
        storage : FileStorage.T.t option;
        torrent_download_url : string option;  (** URL endpoint that transfers the torrent file as an attachment (so that the browser opens a download dialog) *)
        torrent_url : string option;  (** Direct URL of the torrent file *)
        width : float option;  (** **PeerTube >= 6.1** Video stream width *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?file_download_url ?file_url ?fps ?has_audio ?has_video ?height ?id ?magnet_uri ?metadata_url ?playlist_url ?resolution ?size ?storage ?torrent_download_url ?torrent_url ?width () = { file_download_url; file_url; fps; has_audio; has_video; height; id; magnet_uri; metadata_url; playlist_url; resolution; size; storage; torrent_download_url; torrent_url; width }
    
    let file_download_url t = t.file_download_url
    let file_url t = t.file_url
    let fps t = t.fps
    let has_audio t = t.has_audio
    let has_video t = t.has_video
    let height t = t.height
    let id t = t.id
    let magnet_uri t = t.magnet_uri
    let metadata_url t = t.metadata_url
    let playlist_url t = t.playlist_url
    let resolution t = t.resolution
    let size t = t.size
    let storage t = t.storage
    let torrent_download_url t = t.torrent_download_url
    let torrent_url t = t.torrent_url
    let width t = t.width
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoFile"
        (fun file_download_url file_url fps has_audio has_video height id magnet_uri metadata_url playlist_url resolution size storage torrent_download_url torrent_url width -> { file_download_url; file_url; fps; has_audio; has_video; height; id; magnet_uri; metadata_url; playlist_url; resolution; size; storage; torrent_download_url; torrent_url; width })
      |> Jsont.Object.opt_mem "fileDownloadUrl" Jsont.string ~enc:(fun r -> r.file_download_url)
      |> Jsont.Object.opt_mem "fileUrl" Jsont.string ~enc:(fun r -> r.file_url)
      |> Jsont.Object.opt_mem "fps" Jsont.number ~enc:(fun r -> r.fps)
      |> Jsont.Object.opt_mem "hasAudio" Jsont.bool ~enc:(fun r -> r.has_audio)
      |> Jsont.Object.opt_mem "hasVideo" Jsont.bool ~enc:(fun r -> r.has_video)
      |> Jsont.Object.opt_mem "height" Jsont.number ~enc:(fun r -> r.height)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "magnetUri" (Openapi.Runtime.validated_string ~pattern:"/magnet:\\?xt=urn:[a-z0-9]+:[a-z0-9]{32}/i" Jsont.string) ~enc:(fun r -> r.magnet_uri)
      |> Jsont.Object.opt_mem "metadataUrl" Jsont.string ~enc:(fun r -> r.metadata_url)
      |> Jsont.Object.opt_mem "playlistUrl" Jsont.string ~enc:(fun r -> r.playlist_url)
      |> Jsont.Object.opt_mem "resolution" VideoResolutionConstant.T.jsont ~enc:(fun r -> r.resolution)
      |> Jsont.Object.opt_mem "size" Jsont.int ~enc:(fun r -> r.size)
      |> Jsont.Object.opt_mem "storage" FileStorage.T.jsont ~enc:(fun r -> r.storage)
      |> Jsont.Object.opt_mem "torrentDownloadUrl" Jsont.string ~enc:(fun r -> r.torrent_download_url)
      |> Jsont.Object.opt_mem "torrentUrl" Jsont.string ~enc:(fun r -> r.torrent_url)
      |> Jsont.Object.opt_mem "width" Jsont.number ~enc:(fun r -> r.width)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoStreamingPlaylistsHls = struct
  module Types = struct
    module T = struct
      type t = {
        files : VideoFile.T.t list option;  (** Video files associated to this playlist.
      
      The difference with the root `files` property is that these files are fragmented, so they can be used in this streaming playlist (HLS, etc.)
       *)
        playlist_url : string option;
        redundancies : Jsont.json list option;
        segments_sha256_url : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?files ?playlist_url ?redundancies ?segments_sha256_url () = { files; playlist_url; redundancies; segments_sha256_url }
    
    let files t = t.files
    let playlist_url t = t.playlist_url
    let redundancies t = t.redundancies
    let segments_sha256_url t = t.segments_sha256_url
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoStreamingPlaylists-HLS"
        (fun files playlist_url redundancies segments_sha256_url -> { files; playlist_url; redundancies; segments_sha256_url })
      |> Jsont.Object.opt_mem "files" (Jsont.list VideoFile.T.jsont) ~enc:(fun r -> r.files)
      |> Jsont.Object.opt_mem "playlistUrl" Jsont.string ~enc:(fun r -> r.playlist_url)
      |> Jsont.Object.opt_mem "redundancies" (Jsont.list Jsont.json) ~enc:(fun r -> r.redundancies)
      |> Jsont.Object.opt_mem "segmentsSha256Url" Jsont.string ~enc:(fun r -> r.segments_sha256_url)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoStreamingPlaylists = struct
  module Types = struct
    module T = struct
      type t = {
        id : Id.T.t option;
        type_ : int option;  (** Playlist type:
      - `1`: HLS
       *)
        files : VideoFile.T.t list option;  (** Video files associated to this playlist.
      
      The difference with the root `files` property is that these files are fragmented, so they can be used in this streaming playlist (HLS, etc.)
       *)
        playlist_url : string option;
        redundancies : Jsont.json list option;
        segments_sha256_url : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?id ?type_ ?files ?playlist_url ?redundancies ?segments_sha256_url () = { id; type_; files; playlist_url; redundancies; segments_sha256_url }
    
    let id t = t.id
    let type_ t = t.type_
    let files t = t.files
    let playlist_url t = t.playlist_url
    let redundancies t = t.redundancies
    let segments_sha256_url t = t.segments_sha256_url
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoStreamingPlaylists"
        (fun id type_ files playlist_url redundancies segments_sha256_url -> { id; type_; files; playlist_url; redundancies; segments_sha256_url })
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "type" Jsont.int ~enc:(fun r -> r.type_)
      |> Jsont.Object.opt_mem "files" (Jsont.list VideoFile.T.jsont) ~enc:(fun r -> r.files)
      |> Jsont.Object.opt_mem "playlistUrl" Jsont.string ~enc:(fun r -> r.playlist_url)
      |> Jsont.Object.opt_mem "redundancies" (Jsont.list Jsont.json) ~enc:(fun r -> r.redundancies)
      |> Jsont.Object.opt_mem "segmentsSha256Url" Jsont.string ~enc:(fun r -> r.segments_sha256_url)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module CustomHomepage = struct
  module Types = struct
    module T = struct
      type t = {
        content : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?content () = { content }
    
    let content t = t.content
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CustomHomepage"
        (fun content -> { content })
      |> Jsont.Object.opt_mem "content" Jsont.string ~enc:(fun r -> r.content)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get instance custom homepage *)
  let get_api_v1_custom_pages_homepage_instance client () =
    let op_name = "get_api_v1_custom_pages_homepage_instance" in
    let url_path = "/api/v1/custom-pages/homepage/instance" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module CommentAutoTagPolicies = struct
  module Types = struct
    module T = struct
      type t = {
        review : string list option;  (** Auto tags that automatically set the comment in review state *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?review () = { review }
    
    let review t = t.review
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CommentAutoTagPolicies"
        (fun review -> { review })
      |> Jsont.Object.opt_mem "review" (Jsont.list Jsont.string) ~enc:(fun r -> r.review)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get account auto tag policies on comments
  
      **PeerTube >= 6.2** 
      @param account_name account name to get auto tag policies
  *)
  let get_api_v1_automatic_tags_policies_accounts_comments ~account_name client () =
    let op_name = "get_api_v1_automatic_tags_policies_accounts_comments" in
    let url_path = Openapi.Runtime.Path.render ~params:[("accountName", account_name)] "/api/v1/automatic-tags/policies/accounts/{accountName}/comments" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module ChannelActivityList = struct
  module Types = struct
    module Response = struct
      type t = {
        data : Jsont.json list option;
        total : int option;
      }
    end
  end
  
  module Response = struct
    include Types.Response
    
    let v ?data ?total () = { data; total }
    
    let data t = t.data
    let total t = t.total
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ChannelActivityListResponse"
        (fun data total -> { data; total })
      |> Jsont.Object.opt_mem "data" (Jsont.list Jsont.json) ~enc:(fun r -> r.data)
      |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List activities of a video channel
  
      **PeerTube >= 8.0** 
      @param channel_handle The video channel handle
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let list_video_channel_activities ~channel_handle ?start ?count ?sort client () =
    let op_name = "list_video_channel_activities" in
    let url_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/activities" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module Block = struct
  module Types = struct
    module Status = struct
      type t = {
        accounts : Jsont.json option;
        hosts : Jsont.json option;
      }
    end
  end
  
  module Status = struct
    include Types.Status
    
    let v ?accounts ?hosts () = { accounts; hosts }
    
    let accounts t = t.accounts
    let hosts t = t.hosts
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"BlockStatus"
        (fun accounts hosts -> { accounts; hosts })
      |> Jsont.Object.opt_mem "accounts" Jsont.json ~enc:(fun r -> r.accounts)
      |> Jsont.Object.opt_mem "hosts" Jsont.json ~enc:(fun r -> r.hosts)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get block status of accounts/hosts 
      @param accounts Check if these accounts are blocked
      @param hosts Check if these hosts are blocked
  *)
  let get_api_v1_blocklist_status ?accounts ?hosts client () =
    let op_name = "get_api_v1_blocklist_status" in
    let url_path = "/api/v1/blocklist/status" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"accounts" ~value:accounts; Openapi.Runtime.Query.optional ~key:"hosts" ~value:hosts]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Status.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module AutomaticTagAvailable = struct
  module Types = struct
    module T = struct
      type t = {
        available : Jsont.json list option;  (** Available auto tags that can be used to filter objects or set a comment in review state *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?available () = { available }
    
    let available t = t.available
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AutomaticTagAvailable"
        (fun available -> { available })
      |> Jsont.Object.opt_mem "available" (Jsont.list Jsont.json) ~enc:(fun r -> r.available)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get account available auto tags
  
      **PeerTube >= 6.2** 
      @param account_name account name to get auto tag policies
  *)
  let get_api_v1_automatic_tags_accounts_available ~account_name client () =
    let op_name = "get_api_v1_automatic_tags_accounts_available" in
    let url_path = Openapi.Runtime.Path.render ~params:[("accountName", account_name)] "/api/v1/automatic-tags/accounts/{accountName}/available" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get server available auto tags
  
      **PeerTube >= 6.2** *)
  let get_api_v1_automatic_tags_server_available client () =
    let op_name = "get_api_v1_automatic_tags_server_available" in
    let url_path = "/api/v1/automatic-tags/server/available" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module AddVideoPasswords = struct
  module Types = struct
    module T = struct
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module VideoUploadRequestResumable = struct
  module Types = struct
    module T = struct
      type t = {
        category : VideoCategorySet.T.t option;
        channel_id : int;  (** Channel id that will contain this video *)
        comments_policy : VideoCommentsPolicySet.T.t option;
        description : string option;  (** Video description *)
        download_enabled : bool option;  (** Enable or disable downloading for this video *)
        generate_transcription : bool option;  (** **PeerTube >= 6.2** If enabled by the admin, automatically generate a subtitle of the video *)
        language : VideoLanguageSet.T.t option;
        licence : VideoLicenceSet.T.t option;
        name : string;  (** Video name *)
        nsfw : bool option;  (** Whether or not this video contains sensitive content *)
        nsfw_flags : Nsfwflag.T.t option;
        nsfw_summary : Jsont.json option;  (** More information about the sensitive content of the video *)
        originally_published_at : Ptime.t option;  (** Date when the content was originally published *)
        privacy : VideoPrivacySet.T.t option;
        schedule_update : VideoScheduled.Update.t option;
        support : string option;  (** A text tell the audience how to support the video creator *)
        tags : string list option;  (** Video tags (maximum 5 tags each between 2 and 30 characters) *)
        video_passwords : AddVideoPasswords.T.t option;
        wait_transcoding : bool option;  (** Whether or not we wait transcoding before publish the video *)
        filename : string;  (** Video filename including extension *)
        thumbnailfile : string option;  (** Video thumbnail file *)
        previewfile : string option;  (** Video preview file *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~channel_id ~name ~filename ?category ?comments_policy ?description ?download_enabled ?generate_transcription ?language ?licence ?nsfw ?nsfw_flags ?nsfw_summary ?originally_published_at ?privacy ?schedule_update ?support ?tags ?video_passwords ?wait_transcoding ?thumbnailfile ?previewfile () = { category; channel_id; comments_policy; description; download_enabled; generate_transcription; language; licence; name; nsfw; nsfw_flags; nsfw_summary; originally_published_at; privacy; schedule_update; support; tags; video_passwords; wait_transcoding; filename; thumbnailfile; previewfile }
    
    let category t = t.category
    let channel_id t = t.channel_id
    let comments_policy t = t.comments_policy
    let description t = t.description
    let download_enabled t = t.download_enabled
    let generate_transcription t = t.generate_transcription
    let language t = t.language
    let licence t = t.licence
    let name t = t.name
    let nsfw t = t.nsfw
    let nsfw_flags t = t.nsfw_flags
    let nsfw_summary t = t.nsfw_summary
    let originally_published_at t = t.originally_published_at
    let privacy t = t.privacy
    let schedule_update t = t.schedule_update
    let support t = t.support
    let tags t = t.tags
    let video_passwords t = t.video_passwords
    let wait_transcoding t = t.wait_transcoding
    let filename t = t.filename
    let thumbnailfile t = t.thumbnailfile
    let previewfile t = t.previewfile
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoUploadRequestResumable"
        (fun category channel_id comments_policy description download_enabled generate_transcription language licence name nsfw nsfw_flags nsfw_summary originally_published_at privacy schedule_update support tags video_passwords wait_transcoding filename thumbnailfile previewfile -> { category; channel_id; comments_policy; description; download_enabled; generate_transcription; language; licence; name; nsfw; nsfw_flags; nsfw_summary; originally_published_at; privacy; schedule_update; support; tags; video_passwords; wait_transcoding; filename; thumbnailfile; previewfile })
      |> Jsont.Object.opt_mem "category" VideoCategorySet.T.jsont ~enc:(fun r -> r.category)
      |> Jsont.Object.mem "channelId" (Openapi.Runtime.validated_int ~minimum:1. Jsont.int) ~enc:(fun r -> r.channel_id)
      |> Jsont.Object.opt_mem "commentsPolicy" VideoCommentsPolicySet.T.jsont ~enc:(fun r -> r.comments_policy)
      |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.opt_mem "downloadEnabled" Jsont.bool ~enc:(fun r -> r.download_enabled)
      |> Jsont.Object.opt_mem "generateTranscription" Jsont.bool ~enc:(fun r -> r.generate_transcription)
      |> Jsont.Object.opt_mem "language" VideoLanguageSet.T.jsont ~enc:(fun r -> r.language)
      |> Jsont.Object.opt_mem "licence" VideoLicenceSet.T.jsont ~enc:(fun r -> r.licence)
      |> Jsont.Object.mem "name" (Openapi.Runtime.validated_string ~min_length:3 ~max_length:120 Jsont.string) ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "nsfw" Jsont.bool ~enc:(fun r -> r.nsfw)
      |> Jsont.Object.opt_mem "nsfwFlags" Nsfwflag.T.jsont ~enc:(fun r -> r.nsfw_flags)
      |> Jsont.Object.opt_mem "nsfwSummary" Jsont.json ~enc:(fun r -> r.nsfw_summary)
      |> Jsont.Object.opt_mem "originallyPublishedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.originally_published_at)
      |> Jsont.Object.opt_mem "privacy" VideoPrivacySet.T.jsont ~enc:(fun r -> r.privacy)
      |> Jsont.Object.opt_mem "scheduleUpdate" VideoScheduled.Update.jsont ~enc:(fun r -> r.schedule_update)
      |> Jsont.Object.opt_mem "support" Jsont.string ~enc:(fun r -> r.support)
      |> Jsont.Object.opt_mem "tags" (Openapi.Runtime.validated_list ~min_items:1 ~max_items:5 ~unique_items:true Jsont.string) ~enc:(fun r -> r.tags)
      |> Jsont.Object.opt_mem "videoPasswords" AddVideoPasswords.T.jsont ~enc:(fun r -> r.video_passwords)
      |> Jsont.Object.opt_mem "waitTranscoding" Jsont.bool ~enc:(fun r -> r.wait_transcoding)
      |> Jsont.Object.mem "filename" Jsont.string ~enc:(fun r -> r.filename)
      |> Jsont.Object.opt_mem "thumbnailfile" Jsont.string ~enc:(fun r -> r.thumbnailfile)
      |> Jsont.Object.opt_mem "previewfile" Jsont.string ~enc:(fun r -> r.previewfile)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoUploadRequestLegacy = struct
  module Types = struct
    module T = struct
      type t = {
        category : VideoCategorySet.T.t option;
        channel_id : int;  (** Channel id that will contain this video *)
        comments_policy : VideoCommentsPolicySet.T.t option;
        description : string option;  (** Video description *)
        download_enabled : bool option;  (** Enable or disable downloading for this video *)
        generate_transcription : bool option;  (** **PeerTube >= 6.2** If enabled by the admin, automatically generate a subtitle of the video *)
        language : VideoLanguageSet.T.t option;
        licence : VideoLicenceSet.T.t option;
        name : string;  (** Video name *)
        nsfw : bool option;  (** Whether or not this video contains sensitive content *)
        nsfw_flags : Nsfwflag.T.t option;
        nsfw_summary : Jsont.json option;  (** More information about the sensitive content of the video *)
        originally_published_at : Ptime.t option;  (** Date when the content was originally published *)
        previewfile : string option;  (** Video preview file *)
        privacy : VideoPrivacySet.T.t option;
        schedule_update : VideoScheduled.Update.t option;
        support : string option;  (** A text tell the audience how to support the video creator *)
        tags : string list option;  (** Video tags (maximum 5 tags each between 2 and 30 characters) *)
        thumbnailfile : string option;  (** Video thumbnail file *)
        video_passwords : AddVideoPasswords.T.t option;
        wait_transcoding : bool option;  (** Whether or not we wait transcoding before publish the video *)
        videofile : string;  (** Video file *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~channel_id ~name ~videofile ?category ?comments_policy ?description ?download_enabled ?generate_transcription ?language ?licence ?nsfw ?nsfw_flags ?nsfw_summary ?originally_published_at ?previewfile ?privacy ?schedule_update ?support ?tags ?thumbnailfile ?video_passwords ?wait_transcoding () = { category; channel_id; comments_policy; description; download_enabled; generate_transcription; language; licence; name; nsfw; nsfw_flags; nsfw_summary; originally_published_at; previewfile; privacy; schedule_update; support; tags; thumbnailfile; video_passwords; wait_transcoding; videofile }
    
    let category t = t.category
    let channel_id t = t.channel_id
    let comments_policy t = t.comments_policy
    let description t = t.description
    let download_enabled t = t.download_enabled
    let generate_transcription t = t.generate_transcription
    let language t = t.language
    let licence t = t.licence
    let name t = t.name
    let nsfw t = t.nsfw
    let nsfw_flags t = t.nsfw_flags
    let nsfw_summary t = t.nsfw_summary
    let originally_published_at t = t.originally_published_at
    let previewfile t = t.previewfile
    let privacy t = t.privacy
    let schedule_update t = t.schedule_update
    let support t = t.support
    let tags t = t.tags
    let thumbnailfile t = t.thumbnailfile
    let video_passwords t = t.video_passwords
    let wait_transcoding t = t.wait_transcoding
    let videofile t = t.videofile
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoUploadRequestLegacy"
        (fun category channel_id comments_policy description download_enabled generate_transcription language licence name nsfw nsfw_flags nsfw_summary originally_published_at previewfile privacy schedule_update support tags thumbnailfile video_passwords wait_transcoding videofile -> { category; channel_id; comments_policy; description; download_enabled; generate_transcription; language; licence; name; nsfw; nsfw_flags; nsfw_summary; originally_published_at; previewfile; privacy; schedule_update; support; tags; thumbnailfile; video_passwords; wait_transcoding; videofile })
      |> Jsont.Object.opt_mem "category" VideoCategorySet.T.jsont ~enc:(fun r -> r.category)
      |> Jsont.Object.mem "channelId" (Openapi.Runtime.validated_int ~minimum:1. Jsont.int) ~enc:(fun r -> r.channel_id)
      |> Jsont.Object.opt_mem "commentsPolicy" VideoCommentsPolicySet.T.jsont ~enc:(fun r -> r.comments_policy)
      |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.opt_mem "downloadEnabled" Jsont.bool ~enc:(fun r -> r.download_enabled)
      |> Jsont.Object.opt_mem "generateTranscription" Jsont.bool ~enc:(fun r -> r.generate_transcription)
      |> Jsont.Object.opt_mem "language" VideoLanguageSet.T.jsont ~enc:(fun r -> r.language)
      |> Jsont.Object.opt_mem "licence" VideoLicenceSet.T.jsont ~enc:(fun r -> r.licence)
      |> Jsont.Object.mem "name" (Openapi.Runtime.validated_string ~min_length:3 ~max_length:120 Jsont.string) ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "nsfw" Jsont.bool ~enc:(fun r -> r.nsfw)
      |> Jsont.Object.opt_mem "nsfwFlags" Nsfwflag.T.jsont ~enc:(fun r -> r.nsfw_flags)
      |> Jsont.Object.opt_mem "nsfwSummary" Jsont.json ~enc:(fun r -> r.nsfw_summary)
      |> Jsont.Object.opt_mem "originallyPublishedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.originally_published_at)
      |> Jsont.Object.opt_mem "previewfile" Jsont.string ~enc:(fun r -> r.previewfile)
      |> Jsont.Object.opt_mem "privacy" VideoPrivacySet.T.jsont ~enc:(fun r -> r.privacy)
      |> Jsont.Object.opt_mem "scheduleUpdate" VideoScheduled.Update.jsont ~enc:(fun r -> r.schedule_update)
      |> Jsont.Object.opt_mem "support" Jsont.string ~enc:(fun r -> r.support)
      |> Jsont.Object.opt_mem "tags" (Openapi.Runtime.validated_list ~min_items:1 ~max_items:5 ~unique_items:true Jsont.string) ~enc:(fun r -> r.tags)
      |> Jsont.Object.opt_mem "thumbnailfile" Jsont.string ~enc:(fun r -> r.thumbnailfile)
      |> Jsont.Object.opt_mem "videoPasswords" AddVideoPasswords.T.jsont ~enc:(fun r -> r.video_passwords)
      |> Jsont.Object.opt_mem "waitTranscoding" Jsont.bool ~enc:(fun r -> r.wait_transcoding)
      |> Jsont.Object.mem "videofile" Jsont.string ~enc:(fun r -> r.videofile)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoUploadRequestCommon = struct
  module Types = struct
    module T = struct
      type t = {
        category : VideoCategorySet.T.t option;
        channel_id : int;  (** Channel id that will contain this video *)
        comments_policy : VideoCommentsPolicySet.T.t option;
        description : string option;  (** Video description *)
        download_enabled : bool option;  (** Enable or disable downloading for this video *)
        generate_transcription : bool option;  (** **PeerTube >= 6.2** If enabled by the admin, automatically generate a subtitle of the video *)
        language : VideoLanguageSet.T.t option;
        licence : VideoLicenceSet.T.t option;
        name : string;  (** Video name *)
        nsfw : bool option;  (** Whether or not this video contains sensitive content *)
        nsfw_flags : Nsfwflag.T.t option;
        nsfw_summary : Jsont.json option;  (** More information about the sensitive content of the video *)
        originally_published_at : Ptime.t option;  (** Date when the content was originally published *)
        previewfile : string option;  (** Video preview file *)
        privacy : VideoPrivacySet.T.t option;
        schedule_update : VideoScheduled.Update.t option;
        support : string option;  (** A text tell the audience how to support the video creator *)
        tags : string list option;  (** Video tags (maximum 5 tags each between 2 and 30 characters) *)
        thumbnailfile : string option;  (** Video thumbnail file *)
        video_passwords : AddVideoPasswords.T.t option;
        wait_transcoding : bool option;  (** Whether or not we wait transcoding before publish the video *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~channel_id ~name ?category ?comments_policy ?description ?download_enabled ?generate_transcription ?language ?licence ?nsfw ?nsfw_flags ?nsfw_summary ?originally_published_at ?previewfile ?privacy ?schedule_update ?support ?tags ?thumbnailfile ?video_passwords ?wait_transcoding () = { category; channel_id; comments_policy; description; download_enabled; generate_transcription; language; licence; name; nsfw; nsfw_flags; nsfw_summary; originally_published_at; previewfile; privacy; schedule_update; support; tags; thumbnailfile; video_passwords; wait_transcoding }
    
    let category t = t.category
    let channel_id t = t.channel_id
    let comments_policy t = t.comments_policy
    let description t = t.description
    let download_enabled t = t.download_enabled
    let generate_transcription t = t.generate_transcription
    let language t = t.language
    let licence t = t.licence
    let name t = t.name
    let nsfw t = t.nsfw
    let nsfw_flags t = t.nsfw_flags
    let nsfw_summary t = t.nsfw_summary
    let originally_published_at t = t.originally_published_at
    let previewfile t = t.previewfile
    let privacy t = t.privacy
    let schedule_update t = t.schedule_update
    let support t = t.support
    let tags t = t.tags
    let thumbnailfile t = t.thumbnailfile
    let video_passwords t = t.video_passwords
    let wait_transcoding t = t.wait_transcoding
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoUploadRequestCommon"
        (fun category channel_id comments_policy description download_enabled generate_transcription language licence name nsfw nsfw_flags nsfw_summary originally_published_at previewfile privacy schedule_update support tags thumbnailfile video_passwords wait_transcoding -> { category; channel_id; comments_policy; description; download_enabled; generate_transcription; language; licence; name; nsfw; nsfw_flags; nsfw_summary; originally_published_at; previewfile; privacy; schedule_update; support; tags; thumbnailfile; video_passwords; wait_transcoding })
      |> Jsont.Object.opt_mem "category" VideoCategorySet.T.jsont ~enc:(fun r -> r.category)
      |> Jsont.Object.mem "channelId" (Openapi.Runtime.validated_int ~minimum:1. Jsont.int) ~enc:(fun r -> r.channel_id)
      |> Jsont.Object.opt_mem "commentsPolicy" VideoCommentsPolicySet.T.jsont ~enc:(fun r -> r.comments_policy)
      |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.opt_mem "downloadEnabled" Jsont.bool ~enc:(fun r -> r.download_enabled)
      |> Jsont.Object.opt_mem "generateTranscription" Jsont.bool ~enc:(fun r -> r.generate_transcription)
      |> Jsont.Object.opt_mem "language" VideoLanguageSet.T.jsont ~enc:(fun r -> r.language)
      |> Jsont.Object.opt_mem "licence" VideoLicenceSet.T.jsont ~enc:(fun r -> r.licence)
      |> Jsont.Object.mem "name" (Openapi.Runtime.validated_string ~min_length:3 ~max_length:120 Jsont.string) ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "nsfw" Jsont.bool ~enc:(fun r -> r.nsfw)
      |> Jsont.Object.opt_mem "nsfwFlags" Nsfwflag.T.jsont ~enc:(fun r -> r.nsfw_flags)
      |> Jsont.Object.opt_mem "nsfwSummary" Jsont.json ~enc:(fun r -> r.nsfw_summary)
      |> Jsont.Object.opt_mem "originallyPublishedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.originally_published_at)
      |> Jsont.Object.opt_mem "previewfile" Jsont.string ~enc:(fun r -> r.previewfile)
      |> Jsont.Object.opt_mem "privacy" VideoPrivacySet.T.jsont ~enc:(fun r -> r.privacy)
      |> Jsont.Object.opt_mem "scheduleUpdate" VideoScheduled.Update.jsont ~enc:(fun r -> r.schedule_update)
      |> Jsont.Object.opt_mem "support" Jsont.string ~enc:(fun r -> r.support)
      |> Jsont.Object.opt_mem "tags" (Openapi.Runtime.validated_list ~min_items:1 ~max_items:5 ~unique_items:true Jsont.string) ~enc:(fun r -> r.tags)
      |> Jsont.Object.opt_mem "thumbnailfile" Jsont.string ~enc:(fun r -> r.thumbnailfile)
      |> Jsont.Object.opt_mem "videoPasswords" AddVideoPasswords.T.jsont ~enc:(fun r -> r.video_passwords)
      |> Jsont.Object.opt_mem "waitTranscoding" Jsont.bool ~enc:(fun r -> r.wait_transcoding)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoCreateImport = struct
  module Types = struct
    module T = struct
      type t = {
        category : VideoCategorySet.T.t option;
        channel_id : int;  (** Channel id that will contain this video *)
        comments_policy : VideoCommentsPolicySet.T.t option;
        description : string option;  (** Video description *)
        download_enabled : bool option;  (** Enable or disable downloading for this video *)
        generate_transcription : bool option;  (** **PeerTube >= 6.2** If enabled by the admin, automatically generate a subtitle of the video *)
        language : VideoLanguageSet.T.t option;
        licence : VideoLicenceSet.T.t option;
        name : string;  (** Video name *)
        nsfw : bool option;  (** Whether or not this video contains sensitive content *)
        nsfw_flags : Nsfwflag.T.t option;
        nsfw_summary : Jsont.json option;  (** More information about the sensitive content of the video *)
        originally_published_at : Ptime.t option;  (** Date when the content was originally published *)
        previewfile : string option;  (** Video preview file *)
        privacy : VideoPrivacySet.T.t option;
        schedule_update : VideoScheduled.Update.t option;
        support : string option;  (** A text tell the audience how to support the video creator *)
        tags : string list option;  (** Video tags (maximum 5 tags each between 2 and 30 characters) *)
        thumbnailfile : string option;  (** Video thumbnail file *)
        video_passwords : AddVideoPasswords.T.t option;
        wait_transcoding : bool option;  (** Whether or not we wait transcoding before publish the video *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~channel_id ~name ?category ?comments_policy ?description ?download_enabled ?generate_transcription ?language ?licence ?nsfw ?nsfw_flags ?nsfw_summary ?originally_published_at ?previewfile ?privacy ?schedule_update ?support ?tags ?thumbnailfile ?video_passwords ?wait_transcoding () = { category; channel_id; comments_policy; description; download_enabled; generate_transcription; language; licence; name; nsfw; nsfw_flags; nsfw_summary; originally_published_at; previewfile; privacy; schedule_update; support; tags; thumbnailfile; video_passwords; wait_transcoding }
    
    let category t = t.category
    let channel_id t = t.channel_id
    let comments_policy t = t.comments_policy
    let description t = t.description
    let download_enabled t = t.download_enabled
    let generate_transcription t = t.generate_transcription
    let language t = t.language
    let licence t = t.licence
    let name t = t.name
    let nsfw t = t.nsfw
    let nsfw_flags t = t.nsfw_flags
    let nsfw_summary t = t.nsfw_summary
    let originally_published_at t = t.originally_published_at
    let previewfile t = t.previewfile
    let privacy t = t.privacy
    let schedule_update t = t.schedule_update
    let support t = t.support
    let tags t = t.tags
    let thumbnailfile t = t.thumbnailfile
    let video_passwords t = t.video_passwords
    let wait_transcoding t = t.wait_transcoding
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoCreateImport"
        (fun category channel_id comments_policy description download_enabled generate_transcription language licence name nsfw nsfw_flags nsfw_summary originally_published_at previewfile privacy schedule_update support tags thumbnailfile video_passwords wait_transcoding -> { category; channel_id; comments_policy; description; download_enabled; generate_transcription; language; licence; name; nsfw; nsfw_flags; nsfw_summary; originally_published_at; previewfile; privacy; schedule_update; support; tags; thumbnailfile; video_passwords; wait_transcoding })
      |> Jsont.Object.opt_mem "category" VideoCategorySet.T.jsont ~enc:(fun r -> r.category)
      |> Jsont.Object.mem "channelId" (Openapi.Runtime.validated_int ~minimum:1. Jsont.int) ~enc:(fun r -> r.channel_id)
      |> Jsont.Object.opt_mem "commentsPolicy" VideoCommentsPolicySet.T.jsont ~enc:(fun r -> r.comments_policy)
      |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r -> r.description)
      |> Jsont.Object.opt_mem "downloadEnabled" Jsont.bool ~enc:(fun r -> r.download_enabled)
      |> Jsont.Object.opt_mem "generateTranscription" Jsont.bool ~enc:(fun r -> r.generate_transcription)
      |> Jsont.Object.opt_mem "language" VideoLanguageSet.T.jsont ~enc:(fun r -> r.language)
      |> Jsont.Object.opt_mem "licence" VideoLicenceSet.T.jsont ~enc:(fun r -> r.licence)
      |> Jsont.Object.mem "name" (Openapi.Runtime.validated_string ~min_length:3 ~max_length:120 Jsont.string) ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "nsfw" Jsont.bool ~enc:(fun r -> r.nsfw)
      |> Jsont.Object.opt_mem "nsfwFlags" Nsfwflag.T.jsont ~enc:(fun r -> r.nsfw_flags)
      |> Jsont.Object.opt_mem "nsfwSummary" Jsont.json ~enc:(fun r -> r.nsfw_summary)
      |> Jsont.Object.opt_mem "originallyPublishedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.originally_published_at)
      |> Jsont.Object.opt_mem "previewfile" Jsont.string ~enc:(fun r -> r.previewfile)
      |> Jsont.Object.opt_mem "privacy" VideoPrivacySet.T.jsont ~enc:(fun r -> r.privacy)
      |> Jsont.Object.opt_mem "scheduleUpdate" VideoScheduled.Update.jsont ~enc:(fun r -> r.schedule_update)
      |> Jsont.Object.opt_mem "support" Jsont.string ~enc:(fun r -> r.support)
      |> Jsont.Object.opt_mem "tags" (Openapi.Runtime.validated_list ~min_items:1 ~max_items:5 ~unique_items:true Jsont.string) ~enc:(fun r -> r.tags)
      |> Jsont.Object.opt_mem "thumbnailfile" Jsont.string ~enc:(fun r -> r.thumbnailfile)
      |> Jsont.Object.opt_mem "videoPasswords" AddVideoPasswords.T.jsont ~enc:(fun r -> r.video_passwords)
      |> Jsont.Object.opt_mem "waitTranscoding" Jsont.bool ~enc:(fun r -> r.wait_transcoding)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module ActorImage = struct
  module Types = struct
    module T = struct
      type t = {
        created_at : Ptime.t option;
        file_url : string option;  (** **PeerTube >= 7.1** *)
        height : int option;  (** **PeerTube >= 7.3** *)
        path : string option;  (** Deprecated in PeerTube v8.0, use fileUrl instead *)
        updated_at : Ptime.t option;
        width : int option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?created_at ?file_url ?height ?path ?updated_at ?width () = { created_at; file_url; height; path; updated_at; width }
    
    let created_at t = t.created_at
    let file_url t = t.file_url
    let height t = t.height
    let path t = t.path
    let updated_at t = t.updated_at
    let width t = t.width
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ActorImage"
        (fun created_at file_url height path updated_at width -> { created_at; file_url; height; path; updated_at; width })
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "fileUrl" Jsont.string ~enc:(fun r -> r.file_url)
      |> Jsont.Object.opt_mem "height" Jsont.int ~enc:(fun r -> r.height)
      |> Jsont.Object.opt_mem "path" Jsont.string ~enc:(fun r -> r.path)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "width" Jsont.int ~enc:(fun r -> r.width)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoChannelSummary = struct
  module Types = struct
    module T = struct
      type t = {
        avatars : ActorImage.T.t list option;
        display_name : string option;
        host : string option;
        id : Id.T.t option;
        name : string option;
        url : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?avatars ?display_name ?host ?id ?name ?url () = { avatars; display_name; host; id; name; url }
    
    let avatars t = t.avatars
    let display_name t = t.display_name
    let host t = t.host
    let id t = t.id
    let name t = t.name
    let url t = t.url
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoChannelSummary"
        (fun avatars display_name host id name url -> { avatars; display_name; host; id; name; url })
      |> Jsont.Object.opt_mem "avatars" (Jsont.list ActorImage.T.jsont) ~enc:(fun r -> r.avatars)
      |> Jsont.Object.opt_mem "displayName" Jsont.string ~enc:(fun r -> r.display_name)
      |> Jsont.Object.opt_mem "host" Jsont.string ~enc:(fun r -> r.host)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "url" Jsont.string ~enc:(fun r -> r.url)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Actor = struct
  module Types = struct
    module Info = struct
      type t = {
        avatars : ActorImage.T.t list option;
        display_name : string option;
        host : string option;
        id : Id.T.t option;
        name : string option;
      }
    end
  
    module T = struct
      type t = {
        avatars : ActorImage.T.t list option;
        created_at : Ptime.t option;
        followers_count : int option;  (** number of followers of this actor, as seen by this instance *)
        following_count : int option;  (** number of actors subscribed to by this actor, as seen by this instance *)
        host : string option;  (** server on which the actor is resident *)
        host_redundancy_allowed : bool option;  (** whether this actor's host allows redundancy of its videos *)
        id : Id.T.t option;
        name : Username.T.t option;  (** immutable name of the actor, used to find or mention it *)
        updated_at : Ptime.t option;
        url : string option;
      }
    end
  end
  
  module Info = struct
    include Types.Info
    
    let v ?avatars ?display_name ?host ?id ?name () = { avatars; display_name; host; id; name }
    
    let avatars t = t.avatars
    let display_name t = t.display_name
    let host t = t.host
    let id t = t.id
    let name t = t.name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"ActorInfo"
        (fun avatars display_name host id name -> { avatars; display_name; host; id; name })
      |> Jsont.Object.opt_mem "avatars" (Jsont.list ActorImage.T.jsont) ~enc:(fun r -> r.avatars)
      |> Jsont.Object.opt_mem "displayName" Jsont.string ~enc:(fun r -> r.display_name)
      |> Jsont.Object.opt_mem "host" Jsont.string ~enc:(fun r -> r.host)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module T = struct
    include Types.T
    
    let v ?avatars ?created_at ?followers_count ?following_count ?host ?host_redundancy_allowed ?id ?name ?updated_at ?url () = { avatars; created_at; followers_count; following_count; host; host_redundancy_allowed; id; name; updated_at; url }
    
    let avatars t = t.avatars
    let created_at t = t.created_at
    let followers_count t = t.followers_count
    let following_count t = t.following_count
    let host t = t.host
    let host_redundancy_allowed t = t.host_redundancy_allowed
    let id t = t.id
    let name t = t.name
    let updated_at t = t.updated_at
    let url t = t.url
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Actor"
        (fun avatars created_at followers_count following_count host host_redundancy_allowed id name updated_at url -> { avatars; created_at; followers_count; following_count; host; host_redundancy_allowed; id; name; updated_at; url })
      |> Jsont.Object.opt_mem "avatars" (Jsont.list ActorImage.T.jsont) ~enc:(fun r -> r.avatars)
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "followersCount" (Openapi.Runtime.validated_int ~minimum:0. Jsont.int) ~enc:(fun r -> r.followers_count)
      |> Jsont.Object.opt_mem "followingCount" (Openapi.Runtime.validated_int ~minimum:0. Jsont.int) ~enc:(fun r -> r.following_count)
      |> Jsont.Object.opt_mem "host" Jsont.string ~enc:(fun r -> r.host)
      |> Jsont.Object.mem "hostRedundancyAllowed" Openapi.Runtime.nullable_bool
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.host_redundancy_allowed)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "name" Username.T.jsont ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "url" Jsont.string ~enc:(fun r -> r.url)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Follow = struct
  module Types = struct
    module T = struct
      type t = {
        created_at : Ptime.t option;
        follower : Actor.T.t option;
        following : Actor.T.t option;
        id : Id.T.t option;
        score : float option;  (** score reflecting the reachability of the actor, with steps of `10` and a base score of `1000`. *)
        state : string option;
        updated_at : Ptime.t option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?created_at ?follower ?following ?id ?score ?state ?updated_at () = { created_at; follower; following; id; score; state; updated_at }
    
    let created_at t = t.created_at
    let follower t = t.follower
    let following t = t.following
    let id t = t.id
    let score t = t.score
    let state t = t.state
    let updated_at t = t.updated_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Follow"
        (fun created_at follower following id score state updated_at -> { created_at; follower; following; id; score; state; updated_at })
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "follower" Actor.T.jsont ~enc:(fun r -> r.follower)
      |> Jsont.Object.opt_mem "following" Actor.T.jsont ~enc:(fun r -> r.following)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "score" Jsont.number ~enc:(fun r -> r.score)
      |> Jsont.Object.opt_mem "state" Jsont.string ~enc:(fun r -> r.state)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AccountSummary = struct
  module Types = struct
    module T = struct
      type t = {
        avatars : ActorImage.T.t list option;
        display_name : string option;
        host : string option;
        id : int option;
        name : string option;
        url : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?avatars ?display_name ?host ?id ?name ?url () = { avatars; display_name; host; id; name; url }
    
    let avatars t = t.avatars
    let display_name t = t.display_name
    let host t = t.host
    let id t = t.id
    let name t = t.name
    let url t = t.url
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AccountSummary"
        (fun avatars display_name host id name url -> { avatars; display_name; host; id; name; url })
      |> Jsont.Object.opt_mem "avatars" (Jsont.list ActorImage.T.jsont) ~enc:(fun r -> r.avatars)
      |> Jsont.Object.opt_mem "displayName" Jsont.string ~enc:(fun r -> r.display_name)
      |> Jsont.Object.opt_mem "host" Jsont.string ~enc:(fun r -> r.host)
      |> Jsont.Object.opt_mem "id" Jsont.int ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "url" Jsont.string ~enc:(fun r -> r.url)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoPlaylist = struct
  module Types = struct
    module T = struct
      type t = {
        created_at : Ptime.t option;
        description : string option;
        display_name : string option;
        id : Id.T.t option;
        is_local : bool option;
        owner_account : AccountSummary.T.t option;
        privacy : VideoPlaylistPrivacyConstant.T.t option;
        short_uuid : ShortUuid.T.t option;
        thumbnail_path : string option;
        type_ : VideoPlaylistTypeConstant.T.t option;
        updated_at : Ptime.t option;
        uuid : Uuidv4.T.t option;
        video_channel : VideoChannelSummary.T.t option;
        video_channel_position : int option;  (** Position of the playlist in the channel *)
        video_length : int option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?created_at ?description ?display_name ?id ?is_local ?owner_account ?privacy ?short_uuid ?thumbnail_path ?type_ ?updated_at ?uuid ?video_channel ?video_channel_position ?video_length () = { created_at; description; display_name; id; is_local; owner_account; privacy; short_uuid; thumbnail_path; type_; updated_at; uuid; video_channel; video_channel_position; video_length }
    
    let created_at t = t.created_at
    let description t = t.description
    let display_name t = t.display_name
    let id t = t.id
    let is_local t = t.is_local
    let owner_account t = t.owner_account
    let privacy t = t.privacy
    let short_uuid t = t.short_uuid
    let thumbnail_path t = t.thumbnail_path
    let type_ t = t.type_
    let updated_at t = t.updated_at
    let uuid t = t.uuid
    let video_channel t = t.video_channel
    let video_channel_position t = t.video_channel_position
    let video_length t = t.video_length
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoPlaylist"
        (fun created_at description display_name id is_local owner_account privacy short_uuid thumbnail_path type_ updated_at uuid video_channel video_channel_position video_length -> { created_at; description; display_name; id; is_local; owner_account; privacy; short_uuid; thumbnail_path; type_; updated_at; uuid; video_channel; video_channel_position; video_length })
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "description" (Openapi.Runtime.validated_string ~min_length:3 ~max_length:1000 Jsont.string) ~enc:(fun r -> r.description)
      |> Jsont.Object.opt_mem "displayName" (Openapi.Runtime.validated_string ~min_length:1 ~max_length:120 Jsont.string) ~enc:(fun r -> r.display_name)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "isLocal" Jsont.bool ~enc:(fun r -> r.is_local)
      |> Jsont.Object.opt_mem "ownerAccount" AccountSummary.T.jsont ~enc:(fun r -> r.owner_account)
      |> Jsont.Object.opt_mem "privacy" VideoPlaylistPrivacyConstant.T.jsont ~enc:(fun r -> r.privacy)
      |> Jsont.Object.opt_mem "shortUUID" ShortUuid.T.jsont ~enc:(fun r -> r.short_uuid)
      |> Jsont.Object.opt_mem "thumbnailPath" Jsont.string ~enc:(fun r -> r.thumbnail_path)
      |> Jsont.Object.opt_mem "type" VideoPlaylistTypeConstant.T.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "uuid" Uuidv4.T.jsont ~enc:(fun r -> r.uuid)
      |> Jsont.Object.opt_mem "videoChannel" VideoChannelSummary.T.jsont ~enc:(fun r -> r.video_channel)
      |> Jsont.Object.opt_mem "videoChannelPosition" (Openapi.Runtime.validated_int ~minimum:1. Jsont.int) ~enc:(fun r -> r.video_channel_position)
      |> Jsont.Object.opt_mem "videoLength" (Openapi.Runtime.validated_int ~minimum:0. Jsont.int) ~enc:(fun r -> r.video_length)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get a video playlist 
      @param playlist_id Playlist id
  *)
  let get_api_v1_video_playlists ~playlist_id client () =
    let op_name = "get_api_v1_video_playlists" in
    let url_path = Openapi.Runtime.Path.render ~params:[("playlistId", playlist_id)] "/api/v1/video-playlists/{playlistId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module VideoChannelCollaborator = struct
  module Types = struct
    module T = struct
      (** Representation of a channel collaboration *)
      type t = {
        account : AccountSummary.T.t option;
        created_at : Ptime.t option;
        id : Id.T.t option;
        state : Jsont.json option;
        updated_at : Ptime.t option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?account ?created_at ?id ?state ?updated_at () = { account; created_at; id; state; updated_at }
    
    let account t = t.account
    let created_at t = t.created_at
    let id t = t.id
    let state t = t.state
    let updated_at t = t.updated_at
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoChannelCollaborator"
        (fun account created_at id state updated_at -> { account; created_at; id; state; updated_at })
      |> Jsont.Object.opt_mem "account" AccountSummary.T.jsont ~enc:(fun r -> r.account)
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "state" Jsont.json ~enc:(fun r -> r.state)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Video = struct
  module Types = struct
    module Info = struct
      type t = {
        id : Jsont.json option;
        name : Jsont.json option;
        state : Jsont.json option;
        uuid : Jsont.json option;
      }
    end
  
    module T = struct
      type t = {
        account : AccountSummary.T.t option;
        aspect_ratio : float option;  (** **PeerTube >= 6.1** Aspect ratio of the video stream *)
        blacklisted : bool option;
        blacklisted_reason : string option;
        category : VideoConstantNumberCategory.T.t option;  (** category in which the video is classified *)
        channel : VideoChannelSummary.T.t option;
        comments : int option;  (** **PeerTube >= 7.2** Number of comments on the video *)
        created_at : Ptime.t option;  (** time at which the video object was first drafted *)
        dislikes : int option;
        duration : int option;  (** duration of the video in seconds *)
        embed_path : string option;
        id : Id.T.t option;  (** object id for the video *)
        is_live : bool option;
        is_local : bool option;
        language : VideoConstantStringLanguage.T.t option;  (** main language used in the video *)
        licence : VideoConstantNumberLicence.T.t option;  (** licence under which the video is distributed *)
        likes : int option;
        live_schedules : LiveSchedule.T.t list option;
        name : string option;  (** title of the video *)
        nsfw : bool option;
        nsfw_flags : Nsfwflag.T.t option;
        nsfw_summary : string option;  (** **PeerTube >= 7.2** More information about the sensitive content of the video *)
        originally_published_at : Ptime.t option;  (** used to represent a date of first publication, prior to the practical publication date of `publishedAt` *)
        preview_path : string option;
        privacy : VideoPrivacyConstant.T.t option;  (** privacy policy used to distribute the video *)
        published_at : Ptime.t option;  (** time at which the video was marked as ready for playback (with restrictions depending on `privacy`). Usually set after a `state` evolution. *)
        scheduled_update : VideoScheduled.Update.t option;
        short_uuid : ShortUuid.T.t option;
        state : VideoStateConstant.T.t option;  (** represents the internal state of the video processing within the PeerTube instance *)
        thumbnail_path : string option;
        truncated_description : string option;  (** truncated description of the video, written in Markdown.
       *)
        updated_at : Ptime.t option;  (** last time the video's metadata was modified *)
        user_history : Jsont.json option;
        uuid : Uuidv4.T.t option;  (** universal identifier for the video, that can be used across instances *)
        views : int option;
        wait_transcoding : bool option;
      }
    end
  end
  
  module Info = struct
    include Types.Info
    
    let v ?id ?name ?state ?uuid () = { id; name; state; uuid }
    
    let id t = t.id
    let name t = t.name
    let state t = t.state
    let uuid t = t.uuid
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoInfo"
        (fun id name state uuid -> { id; name; state; uuid })
      |> Jsont.Object.opt_mem "id" Jsont.json ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "name" Jsont.json ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "state" Jsont.json ~enc:(fun r -> r.state)
      |> Jsont.Object.opt_mem "uuid" Jsont.json ~enc:(fun r -> r.uuid)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module T = struct
    include Types.T
    
    let v ?account ?aspect_ratio ?blacklisted ?blacklisted_reason ?category ?channel ?comments ?created_at ?dislikes ?duration ?embed_path ?id ?is_live ?is_local ?language ?licence ?likes ?live_schedules ?name ?nsfw ?nsfw_flags ?nsfw_summary ?originally_published_at ?preview_path ?privacy ?published_at ?scheduled_update ?short_uuid ?state ?thumbnail_path ?truncated_description ?updated_at ?user_history ?uuid ?views ?wait_transcoding () = { account; aspect_ratio; blacklisted; blacklisted_reason; category; channel; comments; created_at; dislikes; duration; embed_path; id; is_live; is_local; language; licence; likes; live_schedules; name; nsfw; nsfw_flags; nsfw_summary; originally_published_at; preview_path; privacy; published_at; scheduled_update; short_uuid; state; thumbnail_path; truncated_description; updated_at; user_history; uuid; views; wait_transcoding }
    
    let account t = t.account
    let aspect_ratio t = t.aspect_ratio
    let blacklisted t = t.blacklisted
    let blacklisted_reason t = t.blacklisted_reason
    let category t = t.category
    let channel t = t.channel
    let comments t = t.comments
    let created_at t = t.created_at
    let dislikes t = t.dislikes
    let duration t = t.duration
    let embed_path t = t.embed_path
    let id t = t.id
    let is_live t = t.is_live
    let is_local t = t.is_local
    let language t = t.language
    let licence t = t.licence
    let likes t = t.likes
    let live_schedules t = t.live_schedules
    let name t = t.name
    let nsfw t = t.nsfw
    let nsfw_flags t = t.nsfw_flags
    let nsfw_summary t = t.nsfw_summary
    let originally_published_at t = t.originally_published_at
    let preview_path t = t.preview_path
    let privacy t = t.privacy
    let published_at t = t.published_at
    let scheduled_update t = t.scheduled_update
    let short_uuid t = t.short_uuid
    let state t = t.state
    let thumbnail_path t = t.thumbnail_path
    let truncated_description t = t.truncated_description
    let updated_at t = t.updated_at
    let user_history t = t.user_history
    let uuid t = t.uuid
    let views t = t.views
    let wait_transcoding t = t.wait_transcoding
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Video"
        (fun account aspect_ratio blacklisted blacklisted_reason category channel comments created_at dislikes duration embed_path id is_live is_local language licence likes live_schedules name nsfw nsfw_flags nsfw_summary originally_published_at preview_path privacy published_at scheduled_update short_uuid state thumbnail_path truncated_description updated_at user_history uuid views wait_transcoding -> { account; aspect_ratio; blacklisted; blacklisted_reason; category; channel; comments; created_at; dislikes; duration; embed_path; id; is_live; is_local; language; licence; likes; live_schedules; name; nsfw; nsfw_flags; nsfw_summary; originally_published_at; preview_path; privacy; published_at; scheduled_update; short_uuid; state; thumbnail_path; truncated_description; updated_at; user_history; uuid; views; wait_transcoding })
      |> Jsont.Object.opt_mem "account" AccountSummary.T.jsont ~enc:(fun r -> r.account)
      |> Jsont.Object.mem "aspectRatio" Openapi.Runtime.nullable_float
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.aspect_ratio)
      |> Jsont.Object.mem "blacklisted" Openapi.Runtime.nullable_bool
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.blacklisted)
      |> Jsont.Object.mem "blacklistedReason" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.blacklisted_reason)
      |> Jsont.Object.opt_mem "category" VideoConstantNumberCategory.T.jsont ~enc:(fun r -> r.category)
      |> Jsont.Object.opt_mem "channel" VideoChannelSummary.T.jsont ~enc:(fun r -> r.channel)
      |> Jsont.Object.opt_mem "comments" Jsont.int ~enc:(fun r -> r.comments)
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "dislikes" Jsont.int ~enc:(fun r -> r.dislikes)
      |> Jsont.Object.opt_mem "duration" Jsont.int ~enc:(fun r -> r.duration)
      |> Jsont.Object.opt_mem "embedPath" Jsont.string ~enc:(fun r -> r.embed_path)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "isLive" Jsont.bool ~enc:(fun r -> r.is_live)
      |> Jsont.Object.opt_mem "isLocal" Jsont.bool ~enc:(fun r -> r.is_local)
      |> Jsont.Object.opt_mem "language" VideoConstantStringLanguage.T.jsont ~enc:(fun r -> r.language)
      |> Jsont.Object.opt_mem "licence" VideoConstantNumberLicence.T.jsont ~enc:(fun r -> r.licence)
      |> Jsont.Object.opt_mem "likes" Jsont.int ~enc:(fun r -> r.likes)
      |> Jsont.Object.opt_mem "liveSchedules" (Jsont.list LiveSchedule.T.jsont) ~enc:(fun r -> r.live_schedules)
      |> Jsont.Object.opt_mem "name" (Openapi.Runtime.validated_string ~min_length:3 ~max_length:120 Jsont.string) ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "nsfw" Jsont.bool ~enc:(fun r -> r.nsfw)
      |> Jsont.Object.opt_mem "nsfwFlags" Nsfwflag.T.jsont ~enc:(fun r -> r.nsfw_flags)
      |> Jsont.Object.mem "nsfwSummary" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.nsfw_summary)
      |> Jsont.Object.mem "originallyPublishedAt" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.originally_published_at)
      |> Jsont.Object.opt_mem "previewPath" Jsont.string ~enc:(fun r -> r.preview_path)
      |> Jsont.Object.opt_mem "privacy" VideoPrivacyConstant.T.jsont ~enc:(fun r -> r.privacy)
      |> Jsont.Object.opt_mem "publishedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.published_at)
      |> Jsont.Object.opt_mem "scheduledUpdate" VideoScheduled.Update.jsont ~enc:(fun r -> r.scheduled_update)
      |> Jsont.Object.opt_mem "shortUUID" ShortUuid.T.jsont ~enc:(fun r -> r.short_uuid)
      |> Jsont.Object.opt_mem "state" VideoStateConstant.T.jsont ~enc:(fun r -> r.state)
      |> Jsont.Object.opt_mem "thumbnailPath" Jsont.string ~enc:(fun r -> r.thumbnail_path)
      |> Jsont.Object.mem "truncatedDescription" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.truncated_description)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.mem "userHistory" (Openapi.Runtime.nullable_any Jsont.json)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.user_history)
      |> Jsont.Object.opt_mem "uuid" Uuidv4.T.jsont ~enc:(fun r -> r.uuid)
      |> Jsont.Object.opt_mem "views" Jsont.int ~enc:(fun r -> r.views)
      |> Jsont.Object.mem "waitTranscoding" Openapi.Runtime.nullable_bool
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.wait_transcoding)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoRating = struct
  module Types = struct
    module T = struct
      type t = {
        rating : string;  (** Rating of the video *)
        video : Video.T.t;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ~rating ~video () = { rating; video }
    
    let rating t = t.rating
    let video t = t.video
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoRating"
        (fun rating video -> { rating; video })
      |> Jsont.Object.mem "rating" Jsont.string ~enc:(fun r -> r.rating)
      |> Jsont.Object.mem "video" Video.T.jsont ~enc:(fun r -> r.video)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List ratings of an account 
      @param name The username or handle of the account
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
      @param rating Optionally filter which ratings to retrieve
  *)
  let get_api_v1_accounts_ratings ~name ?start ?count ?sort ?rating client () =
    let op_name = "get_api_v1_accounts_ratings" in
    let url_path = Openapi.Runtime.Path.render ~params:[("name", name)] "/api/v1/accounts/{name}/ratings" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"rating" ~value:rating]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module VideoList = struct
  module Types = struct
    module Response = struct
      type t = {
        data : Video.T.t list option;
        total : int option;
      }
    end
  end
  
  module Response = struct
    include Types.Response
    
    let v ?data ?total () = { data; total }
    
    let data t = t.data
    let total t = t.total
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoListResponse"
        (fun data total -> { data; total })
      |> Jsont.Object.opt_mem "data" (Openapi.Runtime.validated_list ~max_items:100 Video.T.jsont) ~enc:(fun r -> r.data)
      |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
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
  let get_account_videos ~name ?start ?count ?skip_count ?sort ?nsfw ?nsfw_flags_included ?nsfw_flags_excluded ?is_live ?include_scheduled_live ?category_one_of ?licence_one_of ?language_one_of ?tags_one_of ?tags_all_of ?is_local ?include_ ?has_hlsfiles ?has_web_video_files ?host ?auto_tag_one_of ?privacy_one_of ?exclude_already_watched ?search client () =
    let op_name = "get_account_videos" in
    let url_path = Openapi.Runtime.Path.render ~params:[("name", name)] "/api/v1/accounts/{name}/videos" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"skipCount" ~value:skip_count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"nsfw" ~value:nsfw; Openapi.Runtime.Query.optional ~key:"nsfwFlagsIncluded" ~value:nsfw_flags_included; Openapi.Runtime.Query.optional ~key:"nsfwFlagsExcluded" ~value:nsfw_flags_excluded; Openapi.Runtime.Query.optional ~key:"isLive" ~value:is_live; Openapi.Runtime.Query.optional ~key:"includeScheduledLive" ~value:include_scheduled_live; Openapi.Runtime.Query.optional ~key:"categoryOneOf" ~value:category_one_of; Openapi.Runtime.Query.optional ~key:"licenceOneOf" ~value:licence_one_of; Openapi.Runtime.Query.optional ~key:"languageOneOf" ~value:language_one_of; Openapi.Runtime.Query.optional ~key:"tagsOneOf" ~value:tags_one_of; Openapi.Runtime.Query.optional ~key:"tagsAllOf" ~value:tags_all_of; Openapi.Runtime.Query.optional ~key:"isLocal" ~value:is_local; Openapi.Runtime.Query.optional ~key:"include" ~value:include_; Openapi.Runtime.Query.optional ~key:"hasHLSFiles" ~value:has_hlsfiles; Openapi.Runtime.Query.optional ~key:"hasWebVideoFiles" ~value:has_web_video_files; Openapi.Runtime.Query.optional ~key:"host" ~value:host; Openapi.Runtime.Query.optional ~key:"autoTagOneOf" ~value:auto_tag_one_of; Openapi.Runtime.Query.optional ~key:"privacyOneOf" ~value:privacy_one_of; Openapi.Runtime.Query.optional ~key:"excludeAlreadyWatched" ~value:exclude_already_watched; Openapi.Runtime.Query.optional ~key:"search" ~value:search]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
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
  let search_videos ~search ?uuids ?search_target ?start ?count ?skip_count ?sort ?nsfw ?nsfw_flags_included ?nsfw_flags_excluded ?is_live ?include_scheduled_live ?category_one_of ?licence_one_of ?language_one_of ?tags_one_of ?tags_all_of ?is_local ?include_ ?has_hlsfiles ?has_web_video_files ?host ?auto_tag_one_of ?privacy_one_of ?exclude_already_watched ?start_date ?end_date ?originally_published_start_date ?originally_published_end_date ?duration_min ?duration_max client () =
    let op_name = "search_videos" in
    let url_path = "/api/v1/search/videos" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"uuids" ~value:uuids; Openapi.Runtime.Query.optional ~key:"searchTarget" ~value:search_target; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"skipCount" ~value:skip_count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"nsfw" ~value:nsfw; Openapi.Runtime.Query.optional ~key:"nsfwFlagsIncluded" ~value:nsfw_flags_included; Openapi.Runtime.Query.optional ~key:"nsfwFlagsExcluded" ~value:nsfw_flags_excluded; Openapi.Runtime.Query.optional ~key:"isLive" ~value:is_live; Openapi.Runtime.Query.optional ~key:"includeScheduledLive" ~value:include_scheduled_live; Openapi.Runtime.Query.optional ~key:"categoryOneOf" ~value:category_one_of; Openapi.Runtime.Query.optional ~key:"licenceOneOf" ~value:licence_one_of; Openapi.Runtime.Query.optional ~key:"languageOneOf" ~value:language_one_of; Openapi.Runtime.Query.optional ~key:"tagsOneOf" ~value:tags_one_of; Openapi.Runtime.Query.optional ~key:"tagsAllOf" ~value:tags_all_of; Openapi.Runtime.Query.optional ~key:"isLocal" ~value:is_local; Openapi.Runtime.Query.optional ~key:"include" ~value:include_; Openapi.Runtime.Query.optional ~key:"hasHLSFiles" ~value:has_hlsfiles; Openapi.Runtime.Query.optional ~key:"hasWebVideoFiles" ~value:has_web_video_files; Openapi.Runtime.Query.optional ~key:"host" ~value:host; Openapi.Runtime.Query.optional ~key:"autoTagOneOf" ~value:auto_tag_one_of; Openapi.Runtime.Query.optional ~key:"privacyOneOf" ~value:privacy_one_of; Openapi.Runtime.Query.optional ~key:"excludeAlreadyWatched" ~value:exclude_already_watched; Openapi.Runtime.Query.optional ~key:"startDate" ~value:start_date; Openapi.Runtime.Query.optional ~key:"endDate" ~value:end_date; Openapi.Runtime.Query.optional ~key:"originallyPublishedStartDate" ~value:originally_published_start_date; Openapi.Runtime.Query.optional ~key:"originallyPublishedEndDate" ~value:originally_published_end_date; Openapi.Runtime.Query.optional ~key:"durationMin" ~value:duration_min; Openapi.Runtime.Query.optional ~key:"durationMax" ~value:duration_max]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List watched videos history 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param search Plain text search, applied to various parts of the model depending on endpoint
  *)
  let get_api_v1_users_me_history_videos ?start ?count ?search client () =
    let op_name = "get_api_v1_users_me_history_videos" in
    let url_path = "/api/v1/users/me/history/videos" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"search" ~value:search]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
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
  let get_api_v1_users_me_subscriptions_videos ?start ?count ?skip_count ?sort ?nsfw ?nsfw_flags_included ?nsfw_flags_excluded ?is_live ?include_scheduled_live ?category_one_of ?licence_one_of ?language_one_of ?tags_one_of ?tags_all_of ?is_local ?include_ ?has_hlsfiles ?has_web_video_files ?host ?auto_tag_one_of ?privacy_one_of ?exclude_already_watched ?search client () =
    let op_name = "get_api_v1_users_me_subscriptions_videos" in
    let url_path = "/api/v1/users/me/subscriptions/videos" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"skipCount" ~value:skip_count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"nsfw" ~value:nsfw; Openapi.Runtime.Query.optional ~key:"nsfwFlagsIncluded" ~value:nsfw_flags_included; Openapi.Runtime.Query.optional ~key:"nsfwFlagsExcluded" ~value:nsfw_flags_excluded; Openapi.Runtime.Query.optional ~key:"isLive" ~value:is_live; Openapi.Runtime.Query.optional ~key:"includeScheduledLive" ~value:include_scheduled_live; Openapi.Runtime.Query.optional ~key:"categoryOneOf" ~value:category_one_of; Openapi.Runtime.Query.optional ~key:"licenceOneOf" ~value:licence_one_of; Openapi.Runtime.Query.optional ~key:"languageOneOf" ~value:language_one_of; Openapi.Runtime.Query.optional ~key:"tagsOneOf" ~value:tags_one_of; Openapi.Runtime.Query.optional ~key:"tagsAllOf" ~value:tags_all_of; Openapi.Runtime.Query.optional ~key:"isLocal" ~value:is_local; Openapi.Runtime.Query.optional ~key:"include" ~value:include_; Openapi.Runtime.Query.optional ~key:"hasHLSFiles" ~value:has_hlsfiles; Openapi.Runtime.Query.optional ~key:"hasWebVideoFiles" ~value:has_web_video_files; Openapi.Runtime.Query.optional ~key:"host" ~value:host; Openapi.Runtime.Query.optional ~key:"autoTagOneOf" ~value:auto_tag_one_of; Openapi.Runtime.Query.optional ~key:"privacyOneOf" ~value:privacy_one_of; Openapi.Runtime.Query.optional ~key:"excludeAlreadyWatched" ~value:exclude_already_watched; Openapi.Runtime.Query.optional ~key:"search" ~value:search]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
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
  let get_api_v1_users_me_videos ?channel_name_one_of ?start ?count ?skip_count ?sort ?nsfw ?nsfw_flags_included ?nsfw_flags_excluded ?is_live ?include_scheduled_live ?category_one_of ?licence_one_of ?language_one_of ?tags_one_of ?tags_all_of ?is_local ?include_ ?has_hlsfiles ?has_web_video_files ?host ?auto_tag_one_of ?privacy_one_of ?exclude_already_watched ?search ?include_collaborations client () =
    let op_name = "get_api_v1_users_me_videos" in
    let url_path = "/api/v1/users/me/videos" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"channelNameOneOf" ~value:channel_name_one_of; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"skipCount" ~value:skip_count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"nsfw" ~value:nsfw; Openapi.Runtime.Query.optional ~key:"nsfwFlagsIncluded" ~value:nsfw_flags_included; Openapi.Runtime.Query.optional ~key:"nsfwFlagsExcluded" ~value:nsfw_flags_excluded; Openapi.Runtime.Query.optional ~key:"isLive" ~value:is_live; Openapi.Runtime.Query.optional ~key:"includeScheduledLive" ~value:include_scheduled_live; Openapi.Runtime.Query.optional ~key:"categoryOneOf" ~value:category_one_of; Openapi.Runtime.Query.optional ~key:"licenceOneOf" ~value:licence_one_of; Openapi.Runtime.Query.optional ~key:"languageOneOf" ~value:language_one_of; Openapi.Runtime.Query.optional ~key:"tagsOneOf" ~value:tags_one_of; Openapi.Runtime.Query.optional ~key:"tagsAllOf" ~value:tags_all_of; Openapi.Runtime.Query.optional ~key:"isLocal" ~value:is_local; Openapi.Runtime.Query.optional ~key:"include" ~value:include_; Openapi.Runtime.Query.optional ~key:"hasHLSFiles" ~value:has_hlsfiles; Openapi.Runtime.Query.optional ~key:"hasWebVideoFiles" ~value:has_web_video_files; Openapi.Runtime.Query.optional ~key:"host" ~value:host; Openapi.Runtime.Query.optional ~key:"autoTagOneOf" ~value:auto_tag_one_of; Openapi.Runtime.Query.optional ~key:"privacyOneOf" ~value:privacy_one_of; Openapi.Runtime.Query.optional ~key:"excludeAlreadyWatched" ~value:exclude_already_watched; Openapi.Runtime.Query.optional ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"includeCollaborations" ~value:include_collaborations]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
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
  let get_video_channel_videos ~channel_handle ?start ?count ?skip_count ?sort ?nsfw ?nsfw_flags_included ?nsfw_flags_excluded ?is_live ?include_scheduled_live ?category_one_of ?licence_one_of ?language_one_of ?tags_one_of ?tags_all_of ?is_local ?include_ ?has_hlsfiles ?has_web_video_files ?host ?auto_tag_one_of ?privacy_one_of ?exclude_already_watched ?search client () =
    let op_name = "get_video_channel_videos" in
    let url_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/videos" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"skipCount" ~value:skip_count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"nsfw" ~value:nsfw; Openapi.Runtime.Query.optional ~key:"nsfwFlagsIncluded" ~value:nsfw_flags_included; Openapi.Runtime.Query.optional ~key:"nsfwFlagsExcluded" ~value:nsfw_flags_excluded; Openapi.Runtime.Query.optional ~key:"isLive" ~value:is_live; Openapi.Runtime.Query.optional ~key:"includeScheduledLive" ~value:include_scheduled_live; Openapi.Runtime.Query.optional ~key:"categoryOneOf" ~value:category_one_of; Openapi.Runtime.Query.optional ~key:"licenceOneOf" ~value:licence_one_of; Openapi.Runtime.Query.optional ~key:"languageOneOf" ~value:language_one_of; Openapi.Runtime.Query.optional ~key:"tagsOneOf" ~value:tags_one_of; Openapi.Runtime.Query.optional ~key:"tagsAllOf" ~value:tags_all_of; Openapi.Runtime.Query.optional ~key:"isLocal" ~value:is_local; Openapi.Runtime.Query.optional ~key:"include" ~value:include_; Openapi.Runtime.Query.optional ~key:"hasHLSFiles" ~value:has_hlsfiles; Openapi.Runtime.Query.optional ~key:"hasWebVideoFiles" ~value:has_web_video_files; Openapi.Runtime.Query.optional ~key:"host" ~value:host; Openapi.Runtime.Query.optional ~key:"autoTagOneOf" ~value:auto_tag_one_of; Openapi.Runtime.Query.optional ~key:"privacyOneOf" ~value:privacy_one_of; Openapi.Runtime.Query.optional ~key:"excludeAlreadyWatched" ~value:exclude_already_watched; Openapi.Runtime.Query.optional ~key:"search" ~value:search]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
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
  let get_videos ?start ?count ?skip_count ?sort ?nsfw ?nsfw_flags_included ?nsfw_flags_excluded ?is_live ?include_scheduled_live ?category_one_of ?licence_one_of ?language_one_of ?tags_one_of ?tags_all_of ?is_local ?include_ ?has_hlsfiles ?has_web_video_files ?host ?auto_tag_one_of ?privacy_one_of ?exclude_already_watched ?search client () =
    let op_name = "get_videos" in
    let url_path = "/api/v1/videos" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"skipCount" ~value:skip_count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"nsfw" ~value:nsfw; Openapi.Runtime.Query.optional ~key:"nsfwFlagsIncluded" ~value:nsfw_flags_included; Openapi.Runtime.Query.optional ~key:"nsfwFlagsExcluded" ~value:nsfw_flags_excluded; Openapi.Runtime.Query.optional ~key:"isLive" ~value:is_live; Openapi.Runtime.Query.optional ~key:"includeScheduledLive" ~value:include_scheduled_live; Openapi.Runtime.Query.optional ~key:"categoryOneOf" ~value:category_one_of; Openapi.Runtime.Query.optional ~key:"licenceOneOf" ~value:licence_one_of; Openapi.Runtime.Query.optional ~key:"languageOneOf" ~value:language_one_of; Openapi.Runtime.Query.optional ~key:"tagsOneOf" ~value:tags_one_of; Openapi.Runtime.Query.optional ~key:"tagsAllOf" ~value:tags_all_of; Openapi.Runtime.Query.optional ~key:"isLocal" ~value:is_local; Openapi.Runtime.Query.optional ~key:"include" ~value:include_; Openapi.Runtime.Query.optional ~key:"hasHLSFiles" ~value:has_hlsfiles; Openapi.Runtime.Query.optional ~key:"hasWebVideoFiles" ~value:has_web_video_files; Openapi.Runtime.Query.optional ~key:"host" ~value:host; Openapi.Runtime.Query.optional ~key:"autoTagOneOf" ~value:auto_tag_one_of; Openapi.Runtime.Query.optional ~key:"privacyOneOf" ~value:privacy_one_of; Openapi.Runtime.Query.optional ~key:"excludeAlreadyWatched" ~value:exclude_already_watched; Openapi.Runtime.Query.optional ~key:"search" ~value:search]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module VideoImport = struct
  module Types = struct
    module T = struct
      type t = {
        created_at : Ptime.t option;
        error : string option;
        id : Id.T.t option;
        magnet_uri : string option;  (** magnet URI allowing to resolve the import's source video *)
        state : VideoImportStateConstant.T.t option;
        target_url : string option;  (** remote URL where to find the import's source video *)
        torrent_name : string option;
        torrentfile : string option;  (** Torrent file containing only the video file *)
        updated_at : Ptime.t option;
        video : Video.T.t option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?created_at ?error ?id ?magnet_uri ?state ?target_url ?torrent_name ?torrentfile ?updated_at ?video () = { created_at; error; id; magnet_uri; state; target_url; torrent_name; torrentfile; updated_at; video }
    
    let created_at t = t.created_at
    let error t = t.error
    let id t = t.id
    let magnet_uri t = t.magnet_uri
    let state t = t.state
    let target_url t = t.target_url
    let torrent_name t = t.torrent_name
    let torrentfile t = t.torrentfile
    let updated_at t = t.updated_at
    let video t = t.video
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoImport"
        (fun created_at error id magnet_uri state target_url torrent_name torrentfile updated_at video -> { created_at; error; id; magnet_uri; state; target_url; torrent_name; torrentfile; updated_at; video })
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "error" Jsont.string ~enc:(fun r -> r.error)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "magnetUri" (Openapi.Runtime.validated_string ~pattern:"/magnet:\\?xt=urn:[a-z0-9]+:[a-z0-9]{32}/i" Jsont.string) ~enc:(fun r -> r.magnet_uri)
      |> Jsont.Object.opt_mem "state" VideoImportStateConstant.T.jsont ~enc:(fun r -> r.state)
      |> Jsont.Object.opt_mem "targetUrl" Jsont.string ~enc:(fun r -> r.target_url)
      |> Jsont.Object.opt_mem "torrentName" Jsont.string ~enc:(fun r -> r.torrent_name)
      |> Jsont.Object.opt_mem "torrentfile" Jsont.string ~enc:(fun r -> r.torrentfile)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "video" Video.T.jsont ~enc:(fun r -> r.video)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoImportsList = struct
  module Types = struct
    module T = struct
      type t = {
        data : VideoImport.T.t list option;
        total : int option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?data ?total () = { data; total }
    
    let data t = t.data
    let total t = t.total
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoImportsList"
        (fun data total -> { data; total })
      |> Jsont.Object.opt_mem "data" (Openapi.Runtime.validated_list ~max_items:100 VideoImport.T.jsont) ~enc:(fun r -> r.data)
      |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
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
  let get_api_v1_users_me_videos_imports ~id ?start ?count ?sort ?include_collaborations ?video_id ?target_url ?video_channel_sync_id ?search client () =
    let op_name = "get_api_v1_users_me_videos_imports" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/users/me/videos/imports" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"includeCollaborations" ~value:include_collaborations; Openapi.Runtime.Query.optional ~key:"videoId" ~value:video_id; Openapi.Runtime.Query.optional ~key:"targetUrl" ~value:target_url; Openapi.Runtime.Query.optional ~key:"videoChannelSyncId" ~value:video_channel_sync_id; Openapi.Runtime.Query.optional ~key:"search" ~value:search]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module VideoCommentForOwnerOrAdmin = struct
  module Types = struct
    module T = struct
      type t = {
        account : Jsont.json option;
        automatic_tags : string list option;
        created_at : Jsont.json option;
        held_for_review : Jsont.json option;
        id : Id.T.t option;
        in_reply_to_comment_id : Jsont.json option;
        text : Jsont.json option;
        thread_id : Jsont.json option;
        updated_at : Jsont.json option;
        url : Jsont.json option;
        video : Video.Info.t option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?account ?automatic_tags ?created_at ?held_for_review ?id ?in_reply_to_comment_id ?text ?thread_id ?updated_at ?url ?video () = { account; automatic_tags; created_at; held_for_review; id; in_reply_to_comment_id; text; thread_id; updated_at; url; video }
    
    let account t = t.account
    let automatic_tags t = t.automatic_tags
    let created_at t = t.created_at
    let held_for_review t = t.held_for_review
    let id t = t.id
    let in_reply_to_comment_id t = t.in_reply_to_comment_id
    let text t = t.text
    let thread_id t = t.thread_id
    let updated_at t = t.updated_at
    let url t = t.url
    let video t = t.video
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoCommentForOwnerOrAdmin"
        (fun account automatic_tags created_at held_for_review id in_reply_to_comment_id text thread_id updated_at url video -> { account; automatic_tags; created_at; held_for_review; id; in_reply_to_comment_id; text; thread_id; updated_at; url; video })
      |> Jsont.Object.opt_mem "account" Jsont.json ~enc:(fun r -> r.account)
      |> Jsont.Object.opt_mem "automaticTags" (Jsont.list Jsont.string) ~enc:(fun r -> r.automatic_tags)
      |> Jsont.Object.opt_mem "createdAt" Jsont.json ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "heldForReview" Jsont.json ~enc:(fun r -> r.held_for_review)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "inReplyToCommentId" Jsont.json ~enc:(fun r -> r.in_reply_to_comment_id)
      |> Jsont.Object.opt_mem "text" Jsont.json ~enc:(fun r -> r.text)
      |> Jsont.Object.opt_mem "threadId" Jsont.json ~enc:(fun r -> r.thread_id)
      |> Jsont.Object.opt_mem "updatedAt" Jsont.json ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "url" Jsont.json ~enc:(fun r -> r.url)
      |> Jsont.Object.opt_mem "video" Video.Info.jsont ~enc:(fun r -> r.video)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module PlaylistElement = struct
  module Types = struct
    module T = struct
      type t = {
        position : int option;
        start_timestamp : int option;
        stop_timestamp : int option;
        video : Video.T.t option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?position ?start_timestamp ?stop_timestamp ?video () = { position; start_timestamp; stop_timestamp; video }
    
    let position t = t.position
    let start_timestamp t = t.start_timestamp
    let stop_timestamp t = t.stop_timestamp
    let video t = t.video
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"PlaylistElement"
        (fun position start_timestamp stop_timestamp video -> { position; start_timestamp; stop_timestamp; video })
      |> Jsont.Object.opt_mem "position" Jsont.int ~enc:(fun r -> r.position)
      |> Jsont.Object.opt_mem "startTimestamp" Jsont.int ~enc:(fun r -> r.start_timestamp)
      |> Jsont.Object.opt_mem "stopTimestamp" Jsont.int ~enc:(fun r -> r.stop_timestamp)
      |> Jsont.Object.opt_mem "video" Video.T.jsont ~enc:(fun r -> r.video)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Notification = struct
  module Types = struct
    module Type = struct
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
      type t = int
    end
  
    module T = struct
      type t = {
        account : Actor.Info.t option;
        actor_follow : Jsont.json option;
        comment : Jsont.json option;
        created_at : Ptime.t option;
        id : Id.T.t option;
        read : bool option;
        type_ : Type.t option;
        updated_at : Ptime.t option;
        video : Video.Info.t option;
        video_abuse : Jsont.json option;
        video_blacklist : Jsont.json option;
        video_import : Jsont.json option;
      }
    end
  end
  
  module Type = struct
    include Types.Type
    let jsont = Jsont.int
  end
  
  module T = struct
    include Types.T
    
    let v ?account ?actor_follow ?comment ?created_at ?id ?read ?type_ ?updated_at ?video ?video_abuse ?video_blacklist ?video_import () = { account; actor_follow; comment; created_at; id; read; type_; updated_at; video; video_abuse; video_blacklist; video_import }
    
    let account t = t.account
    let actor_follow t = t.actor_follow
    let comment t = t.comment
    let created_at t = t.created_at
    let id t = t.id
    let read t = t.read
    let type_ t = t.type_
    let updated_at t = t.updated_at
    let video t = t.video
    let video_abuse t = t.video_abuse
    let video_blacklist t = t.video_blacklist
    let video_import t = t.video_import
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Notification"
        (fun account actor_follow comment created_at id read type_ updated_at video video_abuse video_blacklist video_import -> { account; actor_follow; comment; created_at; id; read; type_; updated_at; video; video_abuse; video_blacklist; video_import })
      |> Jsont.Object.opt_mem "account" Actor.Info.jsont ~enc:(fun r -> r.account)
      |> Jsont.Object.mem "actorFollow" (Openapi.Runtime.nullable_any Jsont.json)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.actor_follow)
      |> Jsont.Object.mem "comment" (Openapi.Runtime.nullable_any Jsont.json)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.comment)
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "read" Jsont.bool ~enc:(fun r -> r.read)
      |> Jsont.Object.opt_mem "type" Type.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.mem "video" (Openapi.Runtime.nullable_any Video.Info.jsont)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.video)
      |> Jsont.Object.mem "videoAbuse" (Openapi.Runtime.nullable_any Jsont.json)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.video_abuse)
      |> Jsont.Object.mem "videoBlacklist" (Openapi.Runtime.nullable_any Jsont.json)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.video_blacklist)
      |> Jsont.Object.mem "videoImport" (Openapi.Runtime.nullable_any Jsont.json)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.video_import)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module NotificationList = struct
  module Types = struct
    module Response = struct
      type t = {
        data : Notification.T.t list option;
        total : int option;
      }
    end
  end
  
  module Response = struct
    include Types.Response
    
    let v ?data ?total () = { data; total }
    
    let data t = t.data
    let total t = t.total
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"NotificationListResponse"
        (fun data total -> { data; total })
      |> Jsont.Object.opt_mem "data" (Openapi.Runtime.validated_list ~max_items:100 Notification.T.jsont) ~enc:(fun r -> r.data)
      |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List my notifications 
      @param type_one_of only list notifications of these types
      @param unread only list unread notifications
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_api_v1_users_me_notifications ?type_one_of ?unread ?start ?count ?sort client () =
    let op_name = "get_api_v1_users_me_notifications" in
    let url_path = "/api/v1/users/me/notifications" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"typeOneOf" ~value:type_one_of; Openapi.Runtime.Query.optional ~key:"unread" ~value:unread; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module AbuseMessage = struct
  module Types = struct
    module T = struct
      type t = {
        account : AccountSummary.T.t option;
        by_moderator : bool option;
        created_at : Ptime.t option;
        id : Id.T.t option;
        message : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?account ?by_moderator ?created_at ?id ?message () = { account; by_moderator; created_at; id; message }
    
    let account t = t.account
    let by_moderator t = t.by_moderator
    let created_at t = t.created_at
    let id t = t.id
    let message t = t.message
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AbuseMessage"
        (fun account by_moderator created_at id message -> { account; by_moderator; created_at; id; message })
      |> Jsont.Object.opt_mem "account" AccountSummary.T.jsont ~enc:(fun r -> r.account)
      |> Jsont.Object.opt_mem "byModerator" Jsont.bool ~enc:(fun r -> r.by_moderator)
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "message" (Openapi.Runtime.validated_string ~min_length:2 ~max_length:3000 Jsont.string) ~enc:(fun r -> r.message)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module Account = struct
  module Types = struct
    module T = struct
      type t = {
        avatars : ActorImage.T.t list option;
        created_at : Ptime.t option;
        followers_count : int option;  (** number of followers of this actor, as seen by this instance *)
        following_count : int option;  (** number of actors subscribed to by this actor, as seen by this instance *)
        host : string option;  (** server on which the actor is resident *)
        host_redundancy_allowed : bool option;  (** whether this actor's host allows redundancy of its videos *)
        id : Id.T.t option;
        name : Username.T.t option;  (** immutable name of the actor, used to find or mention it *)
        updated_at : Ptime.t option;
        url : string option;
        user_id : Jsont.json option;  (** object id for the user tied to this account *)
        display_name : string option;  (** editable name of the account, displayed in its representations *)
        description : string option;  (** text or bio displayed on the account's profile *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?avatars ?created_at ?followers_count ?following_count ?host ?host_redundancy_allowed ?id ?name ?updated_at ?url ?user_id ?display_name ?description () = { avatars; created_at; followers_count; following_count; host; host_redundancy_allowed; id; name; updated_at; url; user_id; display_name; description }
    
    let avatars t = t.avatars
    let created_at t = t.created_at
    let followers_count t = t.followers_count
    let following_count t = t.following_count
    let host t = t.host
    let host_redundancy_allowed t = t.host_redundancy_allowed
    let id t = t.id
    let name t = t.name
    let updated_at t = t.updated_at
    let url t = t.url
    let user_id t = t.user_id
    let display_name t = t.display_name
    let description t = t.description
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Account"
        (fun avatars created_at followers_count following_count host host_redundancy_allowed id name updated_at url user_id display_name description -> { avatars; created_at; followers_count; following_count; host; host_redundancy_allowed; id; name; updated_at; url; user_id; display_name; description })
      |> Jsont.Object.opt_mem "avatars" (Jsont.list ActorImage.T.jsont) ~enc:(fun r -> r.avatars)
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "followersCount" (Openapi.Runtime.validated_int ~minimum:0. Jsont.int) ~enc:(fun r -> r.followers_count)
      |> Jsont.Object.opt_mem "followingCount" (Openapi.Runtime.validated_int ~minimum:0. Jsont.int) ~enc:(fun r -> r.following_count)
      |> Jsont.Object.opt_mem "host" Jsont.string ~enc:(fun r -> r.host)
      |> Jsont.Object.mem "hostRedundancyAllowed" Openapi.Runtime.nullable_bool
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.host_redundancy_allowed)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "name" Username.T.jsont ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "url" Jsont.string ~enc:(fun r -> r.url)
      |> Jsont.Object.opt_mem "userId" Jsont.json ~enc:(fun r -> r.user_id)
      |> Jsont.Object.opt_mem "displayName" (Openapi.Runtime.validated_string ~min_length:3 ~max_length:120 Jsont.string) ~enc:(fun r -> r.display_name)
      |> Jsont.Object.mem "description" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.description)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get an account 
      @param name The username or handle of the account
  *)
  let get_account ~name client () =
    let op_name = "get_account" in
    let url_path = Openapi.Runtime.Path.render ~params:[("name", name)] "/api/v1/accounts/{name}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module VideoComment = struct
  module Types = struct
    module T = struct
      type t = {
        account : Account.T.t option;
        created_at : Ptime.t option;
        deleted_at : Ptime.t option;
        held_for_review : bool option;
        id : Id.T.t option;
        in_reply_to_comment_id : Id.T.t option;
        is_deleted : bool;
        text : string option;  (** Text of the comment *)
        thread_id : Id.T.t option;
        total_replies : int option;
        total_replies_from_video_author : int option;
        updated_at : Ptime.t option;
        url : string option;
        video_id : Jsont.json option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?(deleted_at=None) ?(is_deleted=false) ?account ?created_at ?held_for_review ?id ?in_reply_to_comment_id ?text ?thread_id ?total_replies ?total_replies_from_video_author ?updated_at ?url ?video_id () = { account; created_at; deleted_at; held_for_review; id; in_reply_to_comment_id; is_deleted; text; thread_id; total_replies; total_replies_from_video_author; updated_at; url; video_id }
    
    let account t = t.account
    let created_at t = t.created_at
    let deleted_at t = t.deleted_at
    let held_for_review t = t.held_for_review
    let id t = t.id
    let in_reply_to_comment_id t = t.in_reply_to_comment_id
    let is_deleted t = t.is_deleted
    let text t = t.text
    let thread_id t = t.thread_id
    let total_replies t = t.total_replies
    let total_replies_from_video_author t = t.total_replies_from_video_author
    let updated_at t = t.updated_at
    let url t = t.url
    let video_id t = t.video_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoComment"
        (fun account created_at deleted_at held_for_review id in_reply_to_comment_id is_deleted text thread_id total_replies total_replies_from_video_author updated_at url video_id -> { account; created_at; deleted_at; held_for_review; id; in_reply_to_comment_id; is_deleted; text; thread_id; total_replies; total_replies_from_video_author; updated_at; url; video_id })
      |> Jsont.Object.opt_mem "account" Account.T.jsont ~enc:(fun r -> r.account)
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.mem "deletedAt" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.deleted_at)
      |> Jsont.Object.opt_mem "heldForReview" Jsont.bool ~enc:(fun r -> r.held_for_review)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "inReplyToCommentId" Id.T.jsont ~enc:(fun r -> r.in_reply_to_comment_id)
      |> Jsont.Object.mem "isDeleted" Jsont.bool ~dec_absent:false ~enc:(fun r -> r.is_deleted)
      |> Jsont.Object.opt_mem "text" (Openapi.Runtime.validated_string ~min_length:1 Jsont.string) ~enc:(fun r -> r.text)
      |> Jsont.Object.opt_mem "threadId" Id.T.jsont ~enc:(fun r -> r.thread_id)
      |> Jsont.Object.opt_mem "totalReplies" (Openapi.Runtime.validated_int ~minimum:0. Jsont.int) ~enc:(fun r -> r.total_replies)
      |> Jsont.Object.opt_mem "totalRepliesFromVideoAuthor" (Openapi.Runtime.validated_int ~minimum:0. Jsont.int) ~enc:(fun r -> r.total_replies_from_video_author)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "url" Jsont.string ~enc:(fun r -> r.url)
      |> Jsont.Object.opt_mem "videoId" Jsont.json ~enc:(fun r -> r.video_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoCommentThreadTree = struct
  module Types = struct
    module T = struct
      type t = {
        children : Jsont.json list option;
        comment : VideoComment.T.t option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?children ?comment () = { children; comment }
    
    let children t = t.children
    let comment t = t.comment
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoCommentThreadTree"
        (fun children comment -> { children; comment })
      |> Jsont.Object.opt_mem "children" (Jsont.list Jsont.json) ~enc:(fun r -> r.children)
      |> Jsont.Object.opt_mem "comment" VideoComment.T.jsont ~enc:(fun r -> r.comment)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get a thread 
      @param id The object id, uuid or short uuid
      @param thread_id The thread id (root comment id)
  *)
  let get_api_v1_videos_comment_threads ~id ~thread_id client () =
    let op_name = "get_api_v1_videos_comment_threads" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id); ("threadId", thread_id)] "/api/v1/videos/{id}/comment-threads/{threadId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module CommentThreadPost = struct
  module Types = struct
    module Response = struct
      type t = {
        comment : VideoComment.T.t option;
      }
    end
  end
  
  module Response = struct
    include Types.Response
    
    let v ?comment () = { comment }
    
    let comment t = t.comment
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CommentThreadPostResponse"
        (fun comment -> { comment })
      |> Jsont.Object.opt_mem "comment" VideoComment.T.jsont ~enc:(fun r -> r.comment)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Create a thread 
      @param id The object id, uuid or short uuid
  *)
  let post_api_v1_videos_comment_threads ~id client () =
    let op_name = "post_api_v1_videos_comment_threads" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/comment-threads" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Reply to a thread of a video 
      @param id The object id, uuid or short uuid
      @param comment_id The comment id
  *)
  let post_api_v1_videos_comments ~id ~comment_id client () =
    let op_name = "post_api_v1_videos_comments" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id); ("commentId", comment_id)] "/api/v1/videos/{id}/comments/{commentId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module CommentThread = struct
  module Types = struct
    module Response = struct
      type t = {
        data : VideoComment.T.t list option;
        total : int option;  (** Total threads (included deleted ones) on this video *)
        total_not_deleted_comments : int option;  (** Total not-deleted threads (included deleted ones) on this video *)
      }
    end
  end
  
  module Response = struct
    include Types.Response
    
    let v ?data ?total ?total_not_deleted_comments () = { data; total; total_not_deleted_comments }
    
    let data t = t.data
    let total t = t.total
    let total_not_deleted_comments t = t.total_not_deleted_comments
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"CommentThreadResponse"
        (fun data total total_not_deleted_comments -> { data; total; total_not_deleted_comments })
      |> Jsont.Object.opt_mem "data" (Openapi.Runtime.validated_list ~max_items:100 VideoComment.T.jsont) ~enc:(fun r -> r.data)
      |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
      |> Jsont.Object.opt_mem "totalNotDeletedComments" Jsont.int ~enc:(fun r -> r.total_not_deleted_comments)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List threads of a video 
      @param id The object id, uuid or short uuid
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort comments by criteria
  *)
  let get_api_v1_videos_comment_threads ~id ?start ?count ?sort client () =
    let op_name = "get_api_v1_videos_comment_threads" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/comment-threads" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn Response.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module VideoChannel = struct
  module Types = struct
    module Update = struct
      type t = {
        description : Jsont.json option;  (** Channel description *)
        display_name : Jsont.json option;  (** Channel display name *)
        support : Jsont.json option;  (** How to support/fund the channel *)
        bulk_videos_support_update : bool option;  (** Update the support field for all videos of this channel *)
      }
    end
  
    module Create = struct
      type t = {
        description : Jsont.json option;  (** Channel description *)
        display_name : Jsont.json;  (** Channel display name *)
        support : Jsont.json option;  (** How to support/fund the channel *)
        name : UsernameChannel.T.t;  (** username of the channel to create *)
      }
    end
  
    module T = struct
      type t = {
        avatars : ActorImage.T.t list option;
        created_at : Ptime.t option;
        followers_count : int option;  (** number of followers of this actor, as seen by this instance *)
        following_count : int option;  (** number of actors subscribed to by this actor, as seen by this instance *)
        host : string option;  (** server on which the actor is resident *)
        host_redundancy_allowed : bool option;  (** whether this actor's host allows redundancy of its videos *)
        id : Id.T.t option;
        name : Username.T.t option;  (** immutable name of the actor, used to find or mention it *)
        url : string option;
        display_name : string option;  (** editable name of the channel, displayed in its representations *)
        description : string option;
        support : string option;  (** text shown by default on all videos of this channel, to tell the audience how to support it *)
        is_local : bool option;
        updated_at : Ptime.t option;
        banners : ActorImage.T.t list option;
        owner_account : Account.T.t option;
      }
    end
  end
  
  module Update = struct
    include Types.Update
    
    let v ?description ?display_name ?support ?bulk_videos_support_update () = { description; display_name; support; bulk_videos_support_update }
    
    let description t = t.description
    let display_name t = t.display_name
    let support t = t.support
    let bulk_videos_support_update t = t.bulk_videos_support_update
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoChannelUpdate"
        (fun description display_name support bulk_videos_support_update -> { description; display_name; support; bulk_videos_support_update })
      |> Jsont.Object.opt_mem "description" Jsont.json ~enc:(fun r -> r.description)
      |> Jsont.Object.opt_mem "displayName" Jsont.json ~enc:(fun r -> r.display_name)
      |> Jsont.Object.opt_mem "support" Jsont.json ~enc:(fun r -> r.support)
      |> Jsont.Object.opt_mem "bulkVideosSupportUpdate" Jsont.bool ~enc:(fun r -> r.bulk_videos_support_update)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module Create = struct
    include Types.Create
    
    let v ~display_name ~name ?description ?support () = { description; display_name; support; name }
    
    let description t = t.description
    let display_name t = t.display_name
    let support t = t.support
    let name t = t.name
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoChannelCreate"
        (fun description display_name support name -> { description; display_name; support; name })
      |> Jsont.Object.opt_mem "description" Jsont.json ~enc:(fun r -> r.description)
      |> Jsont.Object.mem "displayName" Jsont.json ~enc:(fun r -> r.display_name)
      |> Jsont.Object.opt_mem "support" Jsont.json ~enc:(fun r -> r.support)
      |> Jsont.Object.mem "name" UsernameChannel.T.jsont ~enc:(fun r -> r.name)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module T = struct
    include Types.T
    
    let v ?avatars ?created_at ?followers_count ?following_count ?host ?host_redundancy_allowed ?id ?name ?url ?display_name ?description ?support ?is_local ?updated_at ?banners ?owner_account () = { avatars; created_at; followers_count; following_count; host; host_redundancy_allowed; id; name; url; display_name; description; support; is_local; updated_at; banners; owner_account }
    
    let avatars t = t.avatars
    let created_at t = t.created_at
    let followers_count t = t.followers_count
    let following_count t = t.following_count
    let host t = t.host
    let host_redundancy_allowed t = t.host_redundancy_allowed
    let id t = t.id
    let name t = t.name
    let url t = t.url
    let display_name t = t.display_name
    let description t = t.description
    let support t = t.support
    let is_local t = t.is_local
    let updated_at t = t.updated_at
    let banners t = t.banners
    let owner_account t = t.owner_account
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoChannel"
        (fun avatars created_at followers_count following_count host host_redundancy_allowed id name url display_name description support is_local updated_at banners owner_account -> { avatars; created_at; followers_count; following_count; host; host_redundancy_allowed; id; name; url; display_name; description; support; is_local; updated_at; banners; owner_account })
      |> Jsont.Object.opt_mem "avatars" (Jsont.list ActorImage.T.jsont) ~enc:(fun r -> r.avatars)
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "followersCount" (Openapi.Runtime.validated_int ~minimum:0. Jsont.int) ~enc:(fun r -> r.followers_count)
      |> Jsont.Object.opt_mem "followingCount" (Openapi.Runtime.validated_int ~minimum:0. Jsont.int) ~enc:(fun r -> r.following_count)
      |> Jsont.Object.opt_mem "host" Jsont.string ~enc:(fun r -> r.host)
      |> Jsont.Object.mem "hostRedundancyAllowed" Openapi.Runtime.nullable_bool
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.host_redundancy_allowed)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "name" Username.T.jsont ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "url" Jsont.string ~enc:(fun r -> r.url)
      |> Jsont.Object.opt_mem "displayName" (Openapi.Runtime.validated_string ~min_length:1 ~max_length:120 Jsont.string) ~enc:(fun r -> r.display_name)
      |> Jsont.Object.mem "description" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.description)
      |> Jsont.Object.mem "support" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.support)
      |> Jsont.Object.opt_mem "isLocal" Jsont.bool ~enc:(fun r -> r.is_local)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "banners" (Jsont.list ActorImage.T.jsont) ~enc:(fun r -> r.banners)
      |> Jsont.Object.opt_mem "ownerAccount" Account.T.jsont ~enc:(fun r -> r.owner_account)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get subscription of my user 
      @param subscription_handle The subscription handle
  *)
  let get_api_v1_users_me_subscriptions ~subscription_handle client () =
    let op_name = "get_api_v1_users_me_subscriptions" in
    let url_path = Openapi.Runtime.Path.render ~params:[("subscriptionHandle", subscription_handle)] "/api/v1/users/me/subscriptions/{subscriptionHandle}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get a video channel 
      @param channel_handle The video channel handle
  *)
  let get_video_channel ~channel_handle client () =
    let op_name = "get_video_channel" in
    let url_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module VideoDetails = struct
  module Types = struct
    module T = struct
      type t = {
        aspect_ratio : float option;  (** **PeerTube >= 6.1** Aspect ratio of the video stream *)
        blacklisted : bool option;
        blacklisted_reason : string option;
        category : VideoConstantNumberCategory.T.t option;  (** category in which the video is classified *)
        comments : int option;  (** **PeerTube >= 7.2** Number of comments on the video *)
        created_at : Ptime.t option;  (** time at which the video object was first drafted *)
        dislikes : int option;
        duration : int option;  (** duration of the video in seconds *)
        embed_path : string option;
        id : Id.T.t option;  (** object id for the video *)
        is_live : bool option;
        is_local : bool option;
        language : VideoConstantStringLanguage.T.t option;  (** main language used in the video *)
        licence : VideoConstantNumberLicence.T.t option;  (** licence under which the video is distributed *)
        likes : int option;
        live_schedules : LiveSchedule.T.t list option;
        name : string option;  (** title of the video *)
        nsfw : bool option;
        nsfw_flags : Nsfwflag.T.t option;
        nsfw_summary : string option;  (** **PeerTube >= 7.2** More information about the sensitive content of the video *)
        originally_published_at : Ptime.t option;  (** used to represent a date of first publication, prior to the practical publication date of `publishedAt` *)
        preview_path : string option;
        privacy : VideoPrivacyConstant.T.t option;  (** privacy policy used to distribute the video *)
        published_at : Ptime.t option;  (** time at which the video was marked as ready for playback (with restrictions depending on `privacy`). Usually set after a `state` evolution. *)
        scheduled_update : VideoScheduled.Update.t option;
        short_uuid : ShortUuid.T.t option;
        state : VideoStateConstant.T.t option;  (** represents the internal state of the video processing within the PeerTube instance *)
        thumbnail_path : string option;
        truncated_description : string option;  (** truncated description of the video, written in Markdown.
       *)
        updated_at : Ptime.t option;  (** last time the video's metadata was modified *)
        user_history : Jsont.json option;
        uuid : Uuidv4.T.t option;  (** universal identifier for the video, that can be used across instances *)
        views : int option;
        wait_transcoding : bool option;
        viewers : int option;  (** If the video is a live, you have the amount of current viewers *)
        description : string option;  (** full description of the video, written in Markdown.
       *)
        support : string option;  (** A text tell the audience how to support the video creator *)
        channel : VideoChannel.T.t option;
        account : Account.T.t option;
        tags : string list option;
        comments_policy : VideoCommentsPolicyConstant.T.t option;
        download_enabled : bool option;
        input_file_updated_at : Ptime.t option;  (** Latest input file update. Null if the file has never been replaced since the original upload *)
        tracker_urls : string list option;
        files : VideoFile.T.t list option;  (** Web compatible video files. If Web Video is disabled on the server:
      
      - field will be empty
      - video files will be found in `streamingPlaylists[].files` field
       *)
        streaming_playlists : VideoStreamingPlaylists.T.t list option;  (** HLS playlists/manifest files. If HLS is disabled on the server:
      
      - field will be empty
      - video files will be found in `files` field
       *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?aspect_ratio ?blacklisted ?blacklisted_reason ?category ?comments ?created_at ?dislikes ?duration ?embed_path ?id ?is_live ?is_local ?language ?licence ?likes ?live_schedules ?name ?nsfw ?nsfw_flags ?nsfw_summary ?originally_published_at ?preview_path ?privacy ?published_at ?scheduled_update ?short_uuid ?state ?thumbnail_path ?truncated_description ?updated_at ?user_history ?uuid ?views ?wait_transcoding ?viewers ?description ?support ?channel ?account ?tags ?comments_policy ?download_enabled ?input_file_updated_at ?tracker_urls ?files ?streaming_playlists () = { aspect_ratio; blacklisted; blacklisted_reason; category; comments; created_at; dislikes; duration; embed_path; id; is_live; is_local; language; licence; likes; live_schedules; name; nsfw; nsfw_flags; nsfw_summary; originally_published_at; preview_path; privacy; published_at; scheduled_update; short_uuid; state; thumbnail_path; truncated_description; updated_at; user_history; uuid; views; wait_transcoding; viewers; description; support; channel; account; tags; comments_policy; download_enabled; input_file_updated_at; tracker_urls; files; streaming_playlists }
    
    let aspect_ratio t = t.aspect_ratio
    let blacklisted t = t.blacklisted
    let blacklisted_reason t = t.blacklisted_reason
    let category t = t.category
    let comments t = t.comments
    let created_at t = t.created_at
    let dislikes t = t.dislikes
    let duration t = t.duration
    let embed_path t = t.embed_path
    let id t = t.id
    let is_live t = t.is_live
    let is_local t = t.is_local
    let language t = t.language
    let licence t = t.licence
    let likes t = t.likes
    let live_schedules t = t.live_schedules
    let name t = t.name
    let nsfw t = t.nsfw
    let nsfw_flags t = t.nsfw_flags
    let nsfw_summary t = t.nsfw_summary
    let originally_published_at t = t.originally_published_at
    let preview_path t = t.preview_path
    let privacy t = t.privacy
    let published_at t = t.published_at
    let scheduled_update t = t.scheduled_update
    let short_uuid t = t.short_uuid
    let state t = t.state
    let thumbnail_path t = t.thumbnail_path
    let truncated_description t = t.truncated_description
    let updated_at t = t.updated_at
    let user_history t = t.user_history
    let uuid t = t.uuid
    let views t = t.views
    let wait_transcoding t = t.wait_transcoding
    let viewers t = t.viewers
    let description t = t.description
    let support t = t.support
    let channel t = t.channel
    let account t = t.account
    let tags t = t.tags
    let comments_policy t = t.comments_policy
    let download_enabled t = t.download_enabled
    let input_file_updated_at t = t.input_file_updated_at
    let tracker_urls t = t.tracker_urls
    let files t = t.files
    let streaming_playlists t = t.streaming_playlists
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoDetails"
        (fun aspect_ratio blacklisted blacklisted_reason category comments created_at dislikes duration embed_path id is_live is_local language licence likes live_schedules name nsfw nsfw_flags nsfw_summary originally_published_at preview_path privacy published_at scheduled_update short_uuid state thumbnail_path truncated_description updated_at user_history uuid views wait_transcoding viewers description support channel account tags comments_policy download_enabled input_file_updated_at tracker_urls files streaming_playlists -> { aspect_ratio; blacklisted; blacklisted_reason; category; comments; created_at; dislikes; duration; embed_path; id; is_live; is_local; language; licence; likes; live_schedules; name; nsfw; nsfw_flags; nsfw_summary; originally_published_at; preview_path; privacy; published_at; scheduled_update; short_uuid; state; thumbnail_path; truncated_description; updated_at; user_history; uuid; views; wait_transcoding; viewers; description; support; channel; account; tags; comments_policy; download_enabled; input_file_updated_at; tracker_urls; files; streaming_playlists })
      |> Jsont.Object.mem "aspectRatio" Openapi.Runtime.nullable_float
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.aspect_ratio)
      |> Jsont.Object.mem "blacklisted" Openapi.Runtime.nullable_bool
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.blacklisted)
      |> Jsont.Object.mem "blacklistedReason" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.blacklisted_reason)
      |> Jsont.Object.opt_mem "category" VideoConstantNumberCategory.T.jsont ~enc:(fun r -> r.category)
      |> Jsont.Object.opt_mem "comments" Jsont.int ~enc:(fun r -> r.comments)
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "dislikes" Jsont.int ~enc:(fun r -> r.dislikes)
      |> Jsont.Object.opt_mem "duration" Jsont.int ~enc:(fun r -> r.duration)
      |> Jsont.Object.opt_mem "embedPath" Jsont.string ~enc:(fun r -> r.embed_path)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "isLive" Jsont.bool ~enc:(fun r -> r.is_live)
      |> Jsont.Object.opt_mem "isLocal" Jsont.bool ~enc:(fun r -> r.is_local)
      |> Jsont.Object.opt_mem "language" VideoConstantStringLanguage.T.jsont ~enc:(fun r -> r.language)
      |> Jsont.Object.opt_mem "licence" VideoConstantNumberLicence.T.jsont ~enc:(fun r -> r.licence)
      |> Jsont.Object.opt_mem "likes" Jsont.int ~enc:(fun r -> r.likes)
      |> Jsont.Object.opt_mem "liveSchedules" (Jsont.list LiveSchedule.T.jsont) ~enc:(fun r -> r.live_schedules)
      |> Jsont.Object.opt_mem "name" (Openapi.Runtime.validated_string ~min_length:3 ~max_length:120 Jsont.string) ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "nsfw" Jsont.bool ~enc:(fun r -> r.nsfw)
      |> Jsont.Object.opt_mem "nsfwFlags" Nsfwflag.T.jsont ~enc:(fun r -> r.nsfw_flags)
      |> Jsont.Object.mem "nsfwSummary" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.nsfw_summary)
      |> Jsont.Object.mem "originallyPublishedAt" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.originally_published_at)
      |> Jsont.Object.opt_mem "previewPath" Jsont.string ~enc:(fun r -> r.preview_path)
      |> Jsont.Object.opt_mem "privacy" VideoPrivacyConstant.T.jsont ~enc:(fun r -> r.privacy)
      |> Jsont.Object.opt_mem "publishedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.published_at)
      |> Jsont.Object.opt_mem "scheduledUpdate" VideoScheduled.Update.jsont ~enc:(fun r -> r.scheduled_update)
      |> Jsont.Object.opt_mem "shortUUID" ShortUuid.T.jsont ~enc:(fun r -> r.short_uuid)
      |> Jsont.Object.opt_mem "state" VideoStateConstant.T.jsont ~enc:(fun r -> r.state)
      |> Jsont.Object.opt_mem "thumbnailPath" Jsont.string ~enc:(fun r -> r.thumbnail_path)
      |> Jsont.Object.mem "truncatedDescription" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.truncated_description)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.mem "userHistory" (Openapi.Runtime.nullable_any Jsont.json)
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.user_history)
      |> Jsont.Object.opt_mem "uuid" Uuidv4.T.jsont ~enc:(fun r -> r.uuid)
      |> Jsont.Object.opt_mem "views" Jsont.int ~enc:(fun r -> r.views)
      |> Jsont.Object.mem "waitTranscoding" Openapi.Runtime.nullable_bool
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.wait_transcoding)
      |> Jsont.Object.opt_mem "viewers" Jsont.int ~enc:(fun r -> r.viewers)
      |> Jsont.Object.mem "description" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.description)
      |> Jsont.Object.mem "support" Openapi.Runtime.nullable_string
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.support)
      |> Jsont.Object.opt_mem "channel" VideoChannel.T.jsont ~enc:(fun r -> r.channel)
      |> Jsont.Object.opt_mem "account" Account.T.jsont ~enc:(fun r -> r.account)
      |> Jsont.Object.opt_mem "tags" (Openapi.Runtime.validated_list ~min_items:1 ~max_items:5 Jsont.string) ~enc:(fun r -> r.tags)
      |> Jsont.Object.opt_mem "commentsPolicy" VideoCommentsPolicyConstant.T.jsont ~enc:(fun r -> r.comments_policy)
      |> Jsont.Object.opt_mem "downloadEnabled" Jsont.bool ~enc:(fun r -> r.download_enabled)
      |> Jsont.Object.mem "inputFileUpdatedAt" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.input_file_updated_at)
      |> Jsont.Object.opt_mem "trackerUrls" (Jsont.list Jsont.string) ~enc:(fun r -> r.tracker_urls)
      |> Jsont.Object.opt_mem "files" (Jsont.list VideoFile.T.jsont) ~enc:(fun r -> r.files)
      |> Jsont.Object.opt_mem "streamingPlaylists" (Jsont.list VideoStreamingPlaylists.T.jsont) ~enc:(fun r -> r.streaming_playlists)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** Get a video 
      @param id The object id, uuid or short uuid
  *)
  let get_video ~id client () =
    let op_name = "get_video" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let status = Requests.Response.status_code response in
      let parsed_body = match status with
        | 401 ->
            (match Openapi.Runtime.Json.decode_json ServerError.T.jsont (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ServerError", Openapi.Runtime.Json.encode_json ServerError.T.jsont v))
             | Error _ -> None)
        | 403 ->
            (match Openapi.Runtime.Json.decode_json ServerError.T.jsont (Requests.Response.json response) with
             | Ok v -> Some (Openapi.Runtime.Typed ("ServerError", Openapi.Runtime.Json.encode_json ServerError.T.jsont v))
             | Error _ -> None)
        | _ ->
            (match Jsont_bytesrw.decode_string Jsont.json body with
             | Ok json -> Some (Openapi.Runtime.Json json)
             | Error _ -> Some (Openapi.Runtime.Raw body))
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status;
        body;
        parsed_body;
      })
end

module VideoChannelSync = struct
  module Types = struct
    module Create = struct
      type t = {
        external_channel_url : string option;
        video_channel_id : Id.T.t option;
      }
    end
  
    module T = struct
      type t = {
        channel : VideoChannel.T.t option;
        created_at : Ptime.t option;
        external_channel_url : string option;
        id : Id.T.t option;
        last_sync_at : Ptime.t option;
        state : Jsont.json option;
      }
    end
  end
  
  module Create = struct
    include Types.Create
    
    let v ?external_channel_url ?video_channel_id () = { external_channel_url; video_channel_id }
    
    let external_channel_url t = t.external_channel_url
    let video_channel_id t = t.video_channel_id
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoChannelSyncCreate"
        (fun external_channel_url video_channel_id -> { external_channel_url; video_channel_id })
      |> Jsont.Object.opt_mem "externalChannelUrl" Jsont.string ~enc:(fun r -> r.external_channel_url)
      |> Jsont.Object.opt_mem "videoChannelId" Id.T.jsont ~enc:(fun r -> r.video_channel_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  module T = struct
    include Types.T
    
    let v ?channel ?created_at ?external_channel_url ?id ?last_sync_at ?state () = { channel; created_at; external_channel_url; id; last_sync_at; state }
    
    let channel t = t.channel
    let created_at t = t.created_at
    let external_channel_url t = t.external_channel_url
    let id t = t.id
    let last_sync_at t = t.last_sync_at
    let state t = t.state
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoChannelSync"
        (fun channel created_at external_channel_url id last_sync_at state -> { channel; created_at; external_channel_url; id; last_sync_at; state })
      |> Jsont.Object.opt_mem "channel" VideoChannel.T.jsont ~enc:(fun r -> r.channel)
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "externalChannelUrl" Jsont.string ~enc:(fun r -> r.external_channel_url)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.mem "lastSyncAt" Openapi.Runtime.nullable_ptime
           ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun r -> r.last_sync_at)
      |> Jsont.Object.opt_mem "state" Jsont.json ~enc:(fun r -> r.state)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module VideoChannelSyncList = struct
  module Types = struct
    module T = struct
      type t = {
        data : VideoChannelSync.T.t list option;
        total : int option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?data ?total () = { data; total }
    
    let data t = t.data
    let total t = t.total
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoChannelSyncList"
        (fun data total -> { data; total })
      |> Jsont.Object.opt_mem "data" (Jsont.list VideoChannelSync.T.jsont) ~enc:(fun r -> r.data)
      |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List the synchronizations of video channels of an account 
      @param name The username or handle of the account
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
      @param include_collaborations **PeerTube >= 8.0** Include objects from collaborated channels
  *)
  let get_api_v1_accounts_video_channel_syncs ~name ?start ?count ?sort ?include_collaborations client () =
    let op_name = "get_api_v1_accounts_video_channel_syncs" in
    let url_path = Openapi.Runtime.Path.render ~params:[("name", name)] "/api/v1/accounts/{name}/video-channel-syncs" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"includeCollaborations" ~value:include_collaborations]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module Client = struct
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
  let get_abuses ?id ?predefined_reason ?search ?state ?search_reporter ?search_reportee ?search_video ?search_video_channel ?video_is ?filter ?start ?count ?sort client () =
    let op_name = "get_abuses" in
    let url_path = "/api/v1/abuses" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"id" ~value:id; Openapi.Runtime.Query.optional ~key:"predefinedReason" ~value:predefined_reason; Openapi.Runtime.Query.optional ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"state" ~value:state; Openapi.Runtime.Query.optional ~key:"searchReporter" ~value:search_reporter; Openapi.Runtime.Query.optional ~key:"searchReportee" ~value:search_reportee; Openapi.Runtime.Query.optional ~key:"searchVideo" ~value:search_video; Openapi.Runtime.Query.optional ~key:"searchVideoChannel" ~value:search_video_channel; Openapi.Runtime.Query.optional ~key:"videoIs" ~value:video_is; Openapi.Runtime.Query.optional ~key:"filter" ~value:filter; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Report an abuse *)
  let post_api_v1_abuses client () =
    let op_name = "post_api_v1_abuses" in
    let url_path = "/api/v1/abuses" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update an abuse 
      @param abuse_id Abuse id
  *)
  let put_api_v1_abuses ~abuse_id client () =
    let op_name = "put_api_v1_abuses" in
    let url_path = Openapi.Runtime.Path.render ~params:[("abuseId", abuse_id)] "/api/v1/abuses/{abuseId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete an abuse 
      @param abuse_id Abuse id
  *)
  let delete_api_v1_abuses ~abuse_id client () =
    let op_name = "delete_api_v1_abuses" in
    let url_path = Openapi.Runtime.Path.render ~params:[("abuseId", abuse_id)] "/api/v1/abuses/{abuseId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List messages of an abuse 
      @param abuse_id Abuse id
  *)
  let get_api_v1_abuses_messages ~abuse_id client () =
    let op_name = "get_api_v1_abuses_messages" in
    let url_path = Openapi.Runtime.Path.render ~params:[("abuseId", abuse_id)] "/api/v1/abuses/{abuseId}/messages" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Add message to an abuse 
      @param abuse_id Abuse id
  *)
  let post_api_v1_abuses_messages ~abuse_id client () =
    let op_name = "post_api_v1_abuses_messages" in
    let url_path = Openapi.Runtime.Path.render ~params:[("abuseId", abuse_id)] "/api/v1/abuses/{abuseId}/messages" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete an abuse message 
      @param abuse_id Abuse id
      @param abuse_message_id Abuse message id
  *)
  let delete_api_v1_abuses_messages ~abuse_id ~abuse_message_id client () =
    let op_name = "delete_api_v1_abuses_messages" in
    let url_path = Openapi.Runtime.Path.render ~params:[("abuseId", abuse_id); ("abuseMessageId", abuse_message_id)] "/api/v1/abuses/{abuseId}/messages/{abuseMessageId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List accounts 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_accounts ?start ?count ?sort client () =
    let op_name = "get_accounts" in
    let url_path = "/api/v1/accounts" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List followers of an account 
      @param name The username or handle of the account
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort followers by criteria
      @param search Plain text search, applied to various parts of the model depending on endpoint
  *)
  let get_account_followers ~name ?start ?count ?sort ?search client () =
    let op_name = "get_account_followers" in
    let url_path = Openapi.Runtime.Path.render ~params:[("name", name)] "/api/v1/accounts/{name}/followers" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"search" ~value:search]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List playlists of an account 
      @param name The username or handle of the account
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
      @param search Plain text search, applied to various parts of the model depending on endpoint
      @param include_collaborations **PeerTube >= 8.0** Include objects from collaborated channels
      @param channel_name_one_of **PeerTube >= 8.0** Filter on playlists that are published on a channel with one of these names
  *)
  let get_api_v1_accounts_video_playlists ~name ?start ?count ?sort ?search ?playlist_type ?include_collaborations ?channel_name_one_of client () =
    let op_name = "get_api_v1_accounts_video_playlists" in
    let url_path = Openapi.Runtime.Path.render ~params:[("name", name)] "/api/v1/accounts/{name}/video-playlists" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"playlistType" ~value:playlist_type; Openapi.Runtime.Query.optional ~key:"includeCollaborations" ~value:include_collaborations; Openapi.Runtime.Query.optional ~key:"channelNameOneOf" ~value:channel_name_one_of]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update account auto tag policies on comments
  
      **PeerTube >= 6.2** 
      @param account_name account name to update auto tag policies
  *)
  let put_api_v1_automatic_tags_policies_accounts_comments ~account_name client () =
    let op_name = "put_api_v1_automatic_tags_policies_accounts_comments" in
    let url_path = Openapi.Runtime.Path.render ~params:[("accountName", account_name)] "/api/v1/automatic-tags/policies/accounts/{accountName}/comments" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update client language
  
      Set a cookie so that, the next time the client refreshes the HTML of the web interface, PeerTube will use the next language *)
  let update_client_language client () =
    let op_name = "update_client_language" in
    let url_path = "/api/v1/client-config/update-language" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Set instance runtime configuration *)
  let put_custom_config client () =
    let op_name = "put_custom_config" in
    let url_path = "/api/v1/config/custom" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete instance runtime configuration *)
  let del_custom_config client () =
    let op_name = "del_custom_config" in
    let url_path = "/api/v1/config/custom" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete instance avatar *)
  let delete_api_v1_config_instance_avatar client () =
    let op_name = "delete_api_v1_config_instance_avatar" in
    let url_path = "/api/v1/config/instance-avatar" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update instance avatar *)
  let post_api_v1_config_instance_avatar_pick client () =
    let op_name = "post_api_v1_config_instance_avatar_pick" in
    let url_path = "/api/v1/config/instance-avatar/pick" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete instance banner *)
  let delete_api_v1_config_instance_banner client () =
    let op_name = "delete_api_v1_config_instance_banner" in
    let url_path = "/api/v1/config/instance-banner" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update instance banner *)
  let post_api_v1_config_instance_banner_pick client () =
    let op_name = "post_api_v1_config_instance_banner_pick" in
    let url_path = "/api/v1/config/instance-banner/pick" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete instance logo *)
  let delete_api_v1_config_instance_logo_logo_type ~logo_type client () =
    let op_name = "delete_api_v1_config_instance_logo_logo_type" in
    let url_path = Openapi.Runtime.Path.render ~params:[("logoType", logo_type)] "/api/v1/config/instance-logo/:logoType" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update instance logo *)
  let post_api_v1_config_instance_logo_logo_type_pick ~logo_type client () =
    let op_name = "post_api_v1_config_instance_logo_logo_type_pick" in
    let url_path = Openapi.Runtime.Path.render ~params:[("logoType", logo_type)] "/api/v1/config/instance-logo/:logoType/pick" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Set instance custom homepage *)
  let put_api_v1_custom_pages_homepage_instance client () =
    let op_name = "put_api_v1_custom_pages_homepage_instance" in
    let url_path = "/api/v1/custom-pages/homepage/instance" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Pause job queue *)
  let post_api_v1_jobs_pause client () =
    let op_name = "post_api_v1_jobs_pause" in
    let url_path = "/api/v1/jobs/pause" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Resume job queue *)
  let post_api_v1_jobs_resume client () =
    let op_name = "post_api_v1_jobs_resume" in
    let url_path = "/api/v1/jobs/resume" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List instance jobs 
      @param state The state of the job ('' for for no filter)
      @param job_type job type
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_jobs ~state ?job_type ?start ?count ?sort client () =
    let op_name = "get_jobs" in
    let url_path = Openapi.Runtime.Path.render ~params:[("state", state)] "/api/v1/jobs/{state}" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"jobType" ~value:job_type; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create playback metrics
  
      These metrics are exposed by OpenTelemetry metrics exporter if enabled. *)
  let post_api_v1_metrics_playback ~body client () =
    let op_name = "post_api_v1_metrics_playback" in
    let url_path = "/api/v1/metrics/playback" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json PlaybackMetric.Create.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Install a plugin *)
  let add_plugin client () =
    let op_name = "add_plugin" in
    let url_path = "/api/v1/plugins/install" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Uninstall a plugin *)
  let uninstall_plugin client () =
    let op_name = "uninstall_plugin" in
    let url_path = "/api/v1/plugins/uninstall" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update a plugin *)
  let update_plugin client () =
    let op_name = "update_plugin" in
    let url_path = "/api/v1/plugins/update" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get a plugin's public settings 
      @param npm_name name of the plugin/theme on npmjs.com or in its package.json
  *)
  let get_api_v1_plugins_public_settings ~npm_name client () =
    let op_name = "get_api_v1_plugins_public_settings" in
    let url_path = Openapi.Runtime.Path.render ~params:[("npmName", npm_name)] "/api/v1/plugins/{npmName}/public-settings" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get a plugin's registered settings 
      @param npm_name name of the plugin/theme on npmjs.com or in its package.json
  *)
  let get_api_v1_plugins_registered_settings ~npm_name client () =
    let op_name = "get_api_v1_plugins_registered_settings" in
    let url_path = Openapi.Runtime.Path.render ~params:[("npmName", npm_name)] "/api/v1/plugins/{npmName}/registered-settings" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Set a plugin's settings 
      @param npm_name name of the plugin/theme on npmjs.com or in its package.json
  *)
  let put_api_v1_plugins_settings ~npm_name client () =
    let op_name = "put_api_v1_plugins_settings" in
    let url_path = Openapi.Runtime.Path.render ~params:[("npmName", npm_name)] "/api/v1/plugins/{npmName}/settings" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List runners 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort runners by criteria
  *)
  let get_api_v1_runners ?start ?count ?sort client () =
    let op_name = "get_api_v1_runners" in
    let url_path = "/api/v1/runners" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List jobs 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort runner jobs by criteria
      @param search Plain text search, applied to various parts of the model depending on endpoint
  *)
  let get_api_v1_runners_jobs ?start ?count ?sort ?search ?state_one_of client () =
    let op_name = "get_api_v1_runners_jobs" in
    let url_path = "/api/v1/runners/jobs" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"stateOneOf" ~value:state_one_of]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Request a new job
  
      API used by PeerTube runners *)
  let post_api_v1_runners_jobs_request client () =
    let op_name = "post_api_v1_runners_jobs_request" in
    let url_path = "/api/v1/runners/jobs/request" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a job
  
      The endpoint will first cancel the job if needed, and then remove it from the database. Children jobs will also be removed *)
  let delete_api_v1_runners_jobs ~job_uuid client () =
    let op_name = "delete_api_v1_runners_jobs" in
    let url_path = Openapi.Runtime.Path.render ~params:[("jobUUID", job_uuid)] "/api/v1/runners/jobs/{jobUUID}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Abort job
  
      API used by PeerTube runners *)
  let post_api_v1_runners_jobs_abort ~job_uuid client () =
    let op_name = "post_api_v1_runners_jobs_abort" in
    let url_path = Openapi.Runtime.Path.render ~params:[("jobUUID", job_uuid)] "/api/v1/runners/jobs/{jobUUID}/abort" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Accept job
  
      API used by PeerTube runners *)
  let post_api_v1_runners_jobs_accept ~job_uuid client () =
    let op_name = "post_api_v1_runners_jobs_accept" in
    let url_path = Openapi.Runtime.Path.render ~params:[("jobUUID", job_uuid)] "/api/v1/runners/jobs/{jobUUID}/accept" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Cancel a job *)
  let get_api_v1_runners_jobs_cancel ~job_uuid client () =
    let op_name = "get_api_v1_runners_jobs_cancel" in
    let url_path = Openapi.Runtime.Path.render ~params:[("jobUUID", job_uuid)] "/api/v1/runners/jobs/{jobUUID}/cancel" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Post job error
  
      API used by PeerTube runners *)
  let post_api_v1_runners_jobs_error ~job_uuid client () =
    let op_name = "post_api_v1_runners_jobs_error" in
    let url_path = Openapi.Runtime.Path.render ~params:[("jobUUID", job_uuid)] "/api/v1/runners/jobs/{jobUUID}/error" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Post job success
  
      API used by PeerTube runners *)
  let post_api_v1_runners_jobs_success ~job_uuid client () =
    let op_name = "post_api_v1_runners_jobs_success" in
    let url_path = Openapi.Runtime.Path.render ~params:[("jobUUID", job_uuid)] "/api/v1/runners/jobs/{jobUUID}/success" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update job
  
      API used by PeerTube runners *)
  let post_api_v1_runners_jobs_update ~job_uuid client () =
    let op_name = "post_api_v1_runners_jobs_update" in
    let url_path = Openapi.Runtime.Path.render ~params:[("jobUUID", job_uuid)] "/api/v1/runners/jobs/{jobUUID}/update" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Register a new runner
  
      API used by PeerTube runners *)
  let post_api_v1_runners_register client () =
    let op_name = "post_api_v1_runners_register" in
    let url_path = "/api/v1/runners/register" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List registration tokens 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort registration tokens by criteria
  *)
  let get_api_v1_runners_registration_tokens ?start ?count ?sort client () =
    let op_name = "get_api_v1_runners_registration_tokens" in
    let url_path = "/api/v1/runners/registration-tokens" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Generate registration token
  
      Generate a new runner registration token *)
  let post_api_v1_runners_registration_tokens_generate client () =
    let op_name = "post_api_v1_runners_registration_tokens_generate" in
    let url_path = "/api/v1/runners/registration-tokens/generate" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Remove registration token
  
      Remove a registration token. Runners that used this token for their registration are automatically removed. *)
  let delete_api_v1_runners_registration_tokens ~registration_token_id client () =
    let op_name = "delete_api_v1_runners_registration_tokens" in
    let url_path = Openapi.Runtime.Path.render ~params:[("registrationTokenId", registration_token_id)] "/api/v1/runners/registration-tokens/{registrationTokenId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Unregister a runner
  
      API used by PeerTube runners *)
  let post_api_v1_runners_unregister client () =
    let op_name = "post_api_v1_runners_unregister" in
    let url_path = "/api/v1/runners/unregister" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a runner *)
  let delete_api_v1_runners ~runner_id client () =
    let op_name = "delete_api_v1_runners" in
    let url_path = Openapi.Runtime.Path.render ~params:[("runnerId", runner_id)] "/api/v1/runners/{runnerId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
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
  let search_playlists ~search ?start ?count ?search_target ?sort ?host ?uuids client () =
    let op_name = "search_playlists" in
    let url_path = "/api/v1/search/video-playlists" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"searchTarget" ~value:search_target; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"host" ~value:host; Openapi.Runtime.Query.optional ~key:"uuids" ~value:uuids]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get instance audit logs *)
  let get_instance_audit_logs client () =
    let op_name = "get_instance_audit_logs" in
    let url_path = "/api/v1/server/audit-logs" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List account blocks 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_api_v1_server_blocklist_accounts ?start ?count ?sort client () =
    let op_name = "get_api_v1_server_blocklist_accounts" in
    let url_path = "/api/v1/server/blocklist/accounts" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Block an account *)
  let post_api_v1_server_blocklist_accounts client () =
    let op_name = "post_api_v1_server_blocklist_accounts" in
    let url_path = "/api/v1/server/blocklist/accounts" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Unblock an account by its handle 
      @param account_name account to unblock, in the form `username@domain`
  *)
  let delete_api_v1_server_blocklist_accounts ~account_name client () =
    let op_name = "delete_api_v1_server_blocklist_accounts" in
    let url_path = Openapi.Runtime.Path.render ~params:[("accountName", account_name)] "/api/v1/server/blocklist/accounts/{accountName}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List server blocks 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_api_v1_server_blocklist_servers ?start ?count ?sort client () =
    let op_name = "get_api_v1_server_blocklist_servers" in
    let url_path = "/api/v1/server/blocklist/servers" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Block a server *)
  let post_api_v1_server_blocklist_servers client () =
    let op_name = "post_api_v1_server_blocklist_servers" in
    let url_path = "/api/v1/server/blocklist/servers" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Unblock a server by its domain 
      @param host server domain to unblock
  *)
  let delete_api_v1_server_blocklist_servers ~host client () =
    let op_name = "delete_api_v1_server_blocklist_servers" in
    let url_path = Openapi.Runtime.Path.render ~params:[("host", host)] "/api/v1/server/blocklist/servers/{host}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List instances following the server 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_api_v1_server_followers ?state ?actor_type ?start ?count ?sort client () =
    let op_name = "get_api_v1_server_followers" in
    let url_path = "/api/v1/server/followers" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"state" ~value:state; Openapi.Runtime.Query.optional ~key:"actorType" ~value:actor_type; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Remove or reject a follower to your server 
      @param handle The remote actor handle to remove from your followers
  *)
  let delete_api_v1_server_followers ~handle client () =
    let op_name = "delete_api_v1_server_followers" in
    let url_path = Openapi.Runtime.Path.render ~params:[("handle", handle)] "/api/v1/server/followers/{handle}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Accept a pending follower to your server 
      @param handle The remote actor handle to remove from your followers
  *)
  let post_api_v1_server_followers_accept ~handle client () =
    let op_name = "post_api_v1_server_followers_accept" in
    let url_path = Openapi.Runtime.Path.render ~params:[("handle", handle)] "/api/v1/server/followers/{handle}/accept" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Reject a pending follower to your server 
      @param handle The remote actor handle to remove from your followers
  *)
  let post_api_v1_server_followers_reject ~handle client () =
    let op_name = "post_api_v1_server_followers_reject" in
    let url_path = Openapi.Runtime.Path.render ~params:[("handle", handle)] "/api/v1/server/followers/{handle}/reject" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List instances followed by the server 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_api_v1_server_following ?state ?actor_type ?start ?count ?sort client () =
    let op_name = "get_api_v1_server_following" in
    let url_path = "/api/v1/server/following" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"state" ~value:state; Openapi.Runtime.Query.optional ~key:"actorType" ~value:actor_type; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Follow a list of actors (PeerTube instance, channel or account) *)
  let post_api_v1_server_following client () =
    let op_name = "post_api_v1_server_following" in
    let url_path = "/api/v1/server/following" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Unfollow an actor (PeerTube instance, channel or account) 
      @param host_or_handle The hostOrHandle to unfollow
  *)
  let delete_api_v1_server_following ~host_or_handle client () =
    let op_name = "delete_api_v1_server_following" in
    let url_path = Openapi.Runtime.Path.render ~params:[("hostOrHandle", host_or_handle)] "/api/v1/server/following/{hostOrHandle}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get instance logs *)
  let get_instance_logs client () =
    let op_name = "get_instance_logs" in
    let url_path = "/api/v1/server/logs" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Send client log *)
  let send_client_log ~body client () =
    let op_name = "send_client_log" in
    let url_path = "/api/v1/server/logs/client" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json SendClientLog.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Mirror a video *)
  let put_mirrored_video client () =
    let op_name = "put_mirrored_video" in
    let url_path = "/api/v1/server/redundancy/videos" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a mirror done on a video 
      @param redundancy_id id of an existing redundancy on a video
  *)
  let del_mirrored_video ~redundancy_id client () =
    let op_name = "del_mirrored_video" in
    let url_path = Openapi.Runtime.Path.render ~params:[("redundancyId", redundancy_id)] "/api/v1/server/redundancy/videos/{redundancyId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update a server redundancy policy 
      @param host server domain to mirror
  *)
  let put_api_v1_server_redundancy ~host client () =
    let op_name = "put_api_v1_server_redundancy" in
    let url_path = Openapi.Runtime.Path.render ~params:[("host", host)] "/api/v1/server/redundancy/{host}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Ask to reset password
  
      An email containing a reset password link *)
  let post_api_v1_users_ask_reset_password client () =
    let op_name = "post_api_v1_users_ask_reset_password" in
    let url_path = "/api/v1/users/ask-reset-password" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Resend user verification link *)
  let resend_email_to_verify_user client () =
    let op_name = "resend_email_to_verify_user" in
    let url_path = "/api/v1/users/ask-send-verify-email" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update my user information *)
  let put_user_info ~body client () =
    let op_name = "put_user_info" in
    let url_path = "/api/v1/users/me" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UpdateMe.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List my abuses 
      @param id only list the report with this id
      @param sort Sort abuses by criteria
      @param start Offset used to paginate results
      @param count Number of items to return
  *)
  let get_my_abuses ?id ?state ?sort ?start ?count client () =
    let op_name = "get_my_abuses" in
    let url_path = "/api/v1/users/me/abuses" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"id" ~value:id; Openapi.Runtime.Query.optional ~key:"state" ~value:state; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete my avatar *)
  let delete_api_v1_users_me_avatar client () =
    let op_name = "delete_api_v1_users_me_avatar" in
    let url_path = "/api/v1/users/me/avatar" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update my user avatar *)
  let post_api_v1_users_me_avatar_pick client () =
    let op_name = "post_api_v1_users_me_avatar_pick" in
    let url_path = "/api/v1/users/me/avatar/pick" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Clear video history *)
  let post_api_v1_users_me_history_videos_remove client () =
    let op_name = "post_api_v1_users_me_history_videos_remove" in
    let url_path = "/api/v1/users/me/history/videos/remove" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete history element *)
  let delete_api_v1_users_me_history_videos ~video_id client () =
    let op_name = "delete_api_v1_users_me_history_videos" in
    let url_path = Openapi.Runtime.Path.render ~params:[("videoId", video_id)] "/api/v1/users/me/history/videos/{videoId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Mark feature info as read
  
      **PeerTube >= v8.0.0 *)
  let post_api_v1_users_me_new_feature_info_read client () =
    let op_name = "post_api_v1_users_me_new_feature_info_read" in
    let url_path = "/api/v1/users/me/new-feature-info/read" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update my notification settings *)
  let put_api_v1_users_me_notification_settings ~body client () =
    let op_name = "put_api_v1_users_me_notification_settings" in
    let url_path = "/api/v1/users/me/notification-settings" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UserNotificationSettings.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Mark notifications as read by their id *)
  let post_api_v1_users_me_notifications_read client () =
    let op_name = "post_api_v1_users_me_notifications_read" in
    let url_path = "/api/v1/users/me/notifications/read" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Mark all my notification as read *)
  let post_api_v1_users_me_notifications_read_all client () =
    let op_name = "post_api_v1_users_me_notifications_read_all" in
    let url_path = "/api/v1/users/me/notifications/read-all" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Add subscription to my user *)
  let post_api_v1_users_me_subscriptions client () =
    let op_name = "post_api_v1_users_me_subscriptions" in
    let url_path = "/api/v1/users/me/subscriptions" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get if subscriptions exist for my user 
      @param uris list of uris to check if each is part of the user subscriptions
  *)
  let get_api_v1_users_me_subscriptions_exist ~uris client () =
    let op_name = "get_api_v1_users_me_subscriptions_exist" in
    let url_path = "/api/v1/users/me/subscriptions/exist" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"uris" ~value:uris]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete subscription of my user 
      @param subscription_handle The subscription handle
  *)
  let delete_api_v1_users_me_subscriptions ~subscription_handle client () =
    let op_name = "delete_api_v1_users_me_subscriptions" in
    let url_path = Openapi.Runtime.Path.render ~params:[("subscriptionHandle", subscription_handle)] "/api/v1/users/me/subscriptions/{subscriptionHandle}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Check video exists in my playlists 
      @param video_ids The video ids to check
  *)
  let get_api_v1_users_me_video_playlists_videos_exist ~video_ids client () =
    let op_name = "get_api_v1_users_me_video_playlists_videos_exist" in
    let url_path = "/api/v1/users/me/video-playlists/videos-exist" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"videoIds" ~value:video_ids]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get my user used quota *)
  let get_api_v1_users_me_video_quota_used client () =
    let op_name = "get_api_v1_users_me_video_quota_used" in
    let url_path = "/api/v1/users/me/video-quota-used" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
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
  let get_api_v1_users_me_videos_comments ?search ?search_account ?search_video ?video_id ?video_channel_id ?auto_tag_one_of ?is_held_for_review ?include_collaborations client () =
    let op_name = "get_api_v1_users_me_videos_comments" in
    let url_path = "/api/v1/users/me/videos/comments" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"searchAccount" ~value:search_account; Openapi.Runtime.Query.optional ~key:"searchVideo" ~value:search_video; Openapi.Runtime.Query.optional ~key:"videoId" ~value:video_id; Openapi.Runtime.Query.optional ~key:"videoChannelId" ~value:video_channel_id; Openapi.Runtime.Query.optional ~key:"autoTagOneOf" ~value:auto_tag_one_of; Openapi.Runtime.Query.optional ~key:"isHeldForReview" ~value:is_held_for_review; Openapi.Runtime.Query.optional ~key:"includeCollaborations" ~value:include_collaborations]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Register a user
  
      Signup has to be enabled and signup approval is not required *)
  let register_user ~body client () =
    let op_name = "register_user" in
    let url_path = "/api/v1/users/register" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json RegisterUser.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List registrations 
      @param start Offset used to paginate results
      @param count Number of items to return
  *)
  let list_registrations ?start ?count ?search ?sort client () =
    let op_name = "list_registrations" in
    let url_path = "/api/v1/users/registrations" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Resend verification link to registration request email *)
  let resend_email_to_verify_registration client () =
    let op_name = "resend_email_to_verify_registration" in
    let url_path = "/api/v1/users/registrations/ask-send-verify-email" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete registration
  
      Delete the registration entry. It will not remove the user associated with this registration (if any) 
      @param registration_id Registration ID
  *)
  let delete_registration ~registration_id client () =
    let op_name = "delete_registration" in
    let url_path = Openapi.Runtime.Path.render ~params:[("registrationId", registration_id)] "/api/v1/users/registrations/{registrationId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Accept registration 
      @param registration_id Registration ID
  *)
  let accept_registration ~registration_id ~body client () =
    let op_name = "accept_registration" in
    let url_path = Openapi.Runtime.Path.render ~params:[("registrationId", registration_id)] "/api/v1/users/registrations/{registrationId}/accept" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UserRegistrationAcceptOrReject.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Reject registration 
      @param registration_id Registration ID
  *)
  let reject_registration ~registration_id ~body client () =
    let op_name = "reject_registration" in
    let url_path = Openapi.Runtime.Path.render ~params:[("registrationId", registration_id)] "/api/v1/users/registrations/{registrationId}/reject" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UserRegistrationAcceptOrReject.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Verify a registration email
  
      Following a user registration request, the user will receive an email asking to click a link
  containing a secret.
   
      @param registration_id Registration ID
  *)
  let verify_registration_email ~registration_id client () =
    let op_name = "verify_registration_email" in
    let url_path = Openapi.Runtime.Path.render ~params:[("registrationId", registration_id)] "/api/v1/users/registrations/{registrationId}/verify-email" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Logout
  
      Revokes your access token and its associated refresh token, destroying your current session. *)
  let revoke_oauth_token client () =
    let op_name = "revoke_oauth_token" in
    let url_path = "/api/v1/users/revoke-token" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Login
  
      With your [client id and secret](#operation/getOAuthClient), you can retrieve an access and refresh tokens. *)
  let get_oauth_token client () =
    let op_name = "get_oauth_token" in
    let url_path = "/api/v1/users/token" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get a user 
      @param id Entity id
      @param with_stats include statistics about the user (only available as a moderator/admin)
  *)
  let get_user ~id ?with_stats client () =
    let op_name = "get_user" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/users/{id}" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"withStats" ~value:with_stats]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update a user 
      @param id Entity id
  *)
  let put_user ~id ~body client () =
    let op_name = "put_user" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/users/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UpdateUser.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a user 
      @param id Entity id
  *)
  let del_user ~id client () =
    let op_name = "del_user" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/users/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Reset password 
      @param id Entity id
  *)
  let post_api_v1_users_reset_password ~id client () =
    let op_name = "post_api_v1_users_reset_password" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/users/{id}/reset-password" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List token sessions 
      @param id Entity id
  *)
  let get_api_v1_users_token_sessions ~id client () =
    let op_name = "get_api_v1_users_token_sessions" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/users/{id}/token-sessions" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List token sessions 
      @param id Entity id
      @param token_session_id Token session Id
  *)
  let get_api_v1_users_token_sessions_revoke ~id ~token_session_id client () =
    let op_name = "get_api_v1_users_token_sessions_revoke" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id); ("tokenSessionId", token_session_id)] "/api/v1/users/{id}/token-sessions/{tokenSessionId}/revoke" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Confirm two factor auth
  
      Confirm a two factor authentication request 
      @param id Entity id
  *)
  let confirm_two_factor_request ~id client () =
    let op_name = "confirm_two_factor_request" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/users/{id}/two-factor/confirm-request" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Disable two factor auth
  
      Disable two factor authentication of a user 
      @param id Entity id
  *)
  let disable_two_factor ~id client () =
    let op_name = "disable_two_factor" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/users/{id}/two-factor/disable" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Verify a user
  
      Following a user registration, the new user will receive an email asking to click a link
  containing a secret.
  This endpoint can also be used to verify a new email set in the user account.
   
      @param id Entity id
  *)
  let verify_user ~id client () =
    let op_name = "verify_user" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/users/{id}/verify-email" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List user exports
  
      **PeerTube >= 6.1** 
      @param user_id User id
  *)
  let list_user_exports ~user_id client () =
    let op_name = "list_user_exports" in
    let url_path = Openapi.Runtime.Path.render ~params:[("userId", user_id)] "/api/v1/users/{userId}/exports" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Request user export
  
      Request an archive of user data. An email is sent when the archive is ready. 
      @param user_id User id
  *)
  let request_user_export ~user_id client () =
    let op_name = "request_user_export" in
    let url_path = Openapi.Runtime.Path.render ~params:[("userId", user_id)] "/api/v1/users/{userId}/exports/request" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a user export
  
      **PeerTube >= 6.1** 
      @param user_id User id
      @param id Entity id
  *)
  let delete_user_export ~user_id ~id client () =
    let op_name = "delete_user_export" in
    let url_path = Openapi.Runtime.Path.render ~params:[("userId", user_id); ("id", id)] "/api/v1/users/{userId}/exports/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Initialize the resumable user import
  
      **PeerTube >= 6.1** Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to initialize the import of the archive 
      @param user_id User id
  *)
  let user_import_resumable_init ~user_id ~body client () =
    let op_name = "user_import_resumable_init" in
    let url_path = Openapi.Runtime.Path.render ~params:[("userId", user_id)] "/api/v1/users/{userId}/imports/import-resumable" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UserImportResumable.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Send chunk for the resumable user import
  
      **PeerTube >= 6.1** Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to continue, pause or resume the import of the archive 
      @param user_id User id
      @param upload_id Created session id to proceed with. If you didn't send chunks in the last hour, it is
  not valid anymore and you need to initialize a new upload.
  
  *)
  let user_import_resumable ~user_id ~upload_id client () =
    let op_name = "user_import_resumable" in
    let url_path = Openapi.Runtime.Path.render ~params:[("userId", user_id)] "/api/v1/users/{userId}/imports/import-resumable" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"upload_id" ~value:upload_id]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Cancel the resumable user import
  
      **PeerTube >= 6.1** Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to cancel the resumable user import 
      @param user_id User id
      @param upload_id Created session id to proceed with. If you didn't send chunks in the last hour, it is
  not valid anymore and you need to initialize a new upload.
  
  *)
  let user_import_resumable_cancel ~user_id ~upload_id client () =
    let op_name = "user_import_resumable_cancel" in
    let url_path = Openapi.Runtime.Path.render ~params:[("userId", user_id)] "/api/v1/users/{userId}/imports/import-resumable" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"upload_id" ~value:upload_id]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get latest user import
  
      **PeerTube >= 6.1** 
      @param user_id User id
  *)
  let get_latest_user_import ~user_id client () =
    let op_name = "get_latest_user_import" in
    let url_path = Openapi.Runtime.Path.render ~params:[("userId", user_id)] "/api/v1/users/{userId}/imports/latest" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create a synchronization for a video channel *)
  let add_video_channel_sync ~body client () =
    let op_name = "add_video_channel_sync" in
    let url_path = "/api/v1/video-channel-syncs" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json VideoChannelSync.Create.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a video channel synchronization 
      @param channel_sync_id Channel Sync id
  *)
  let del_video_channel_sync ~channel_sync_id client () =
    let op_name = "del_video_channel_sync" in
    let url_path = Openapi.Runtime.Path.render ~params:[("channelSyncId", channel_sync_id)] "/api/v1/video-channel-syncs/{channelSyncId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Triggers the channel synchronization job, fetching all the videos from the remote channel 
      @param channel_sync_id Channel Sync id
  *)
  let trigger_video_channel_sync ~channel_sync_id client () =
    let op_name = "trigger_video_channel_sync" in
    let url_path = Openapi.Runtime.Path.render ~params:[("channelSyncId", channel_sync_id)] "/api/v1/video-channel-syncs/{channelSyncId}/sync" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create a video channel *)
  let add_video_channel ~body client () =
    let op_name = "add_video_channel" in
    let url_path = "/api/v1/video-channels" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json VideoChannel.Create.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update a video channel 
      @param channel_handle The video channel handle
  *)
  let put_video_channel ~channel_handle ~body client () =
    let op_name = "put_video_channel" in
    let url_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json VideoChannel.Update.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a video channel 
      @param channel_handle The video channel handle
  *)
  let del_video_channel ~channel_handle client () =
    let op_name = "del_video_channel" in
    let url_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete channel avatar 
      @param channel_handle The video channel handle
  *)
  let delete_api_v1_video_channels_avatar ~channel_handle client () =
    let op_name = "delete_api_v1_video_channels_avatar" in
    let url_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/avatar" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update channel avatar 
      @param channel_handle The video channel handle
  *)
  let post_api_v1_video_channels_avatar_pick ~channel_handle client () =
    let op_name = "post_api_v1_video_channels_avatar_pick" in
    let url_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/avatar/pick" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete channel banner 
      @param channel_handle The video channel handle
  *)
  let delete_api_v1_video_channels_banner ~channel_handle client () =
    let op_name = "delete_api_v1_video_channels_banner" in
    let url_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/banner" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update channel banner 
      @param channel_handle The video channel handle
  *)
  let post_api_v1_video_channels_banner_pick ~channel_handle client () =
    let op_name = "post_api_v1_video_channels_banner_pick" in
    let url_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/banner/pick" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** *List channel collaborators
  
      **PeerTube >= 8.0** 
      @param channel_handle The video channel handle
  *)
  let list_video_channel_collaborators ~channel_handle client () =
    let op_name = "list_video_channel_collaborators" in
    let url_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/collaborators" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Invite a collaborator
  
      **PeerTube >= 8.0**  Invite a local user to collaborate on the specified video channel. 
      @param channel_handle The video channel handle
  *)
  let invite_video_channel_collaborator ~channel_handle client () =
    let op_name = "invite_video_channel_collaborator" in
    let url_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/collaborators/invite" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Remove a channel collaborator
  
      **PeerTube >= 8.0** Only the channel owner or the collaborator themselves can remove a collaborator from a channel 
      @param channel_handle The video channel handle
      @param collaborator_id The collaborator id
  *)
  let remove_video_channel_collaborator ~channel_handle ~collaborator_id client () =
    let op_name = "remove_video_channel_collaborator" in
    let url_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle); ("collaboratorId", collaborator_id)] "/api/v1/video-channels/{channelHandle}/collaborators/{collaboratorId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Accept a collaboration invitation
  
      **PeerTube >= 8.0** 
      @param channel_handle The video channel handle
      @param collaborator_id The collaborator id
  *)
  let accept_video_channel_collaborator ~channel_handle ~collaborator_id client () =
    let op_name = "accept_video_channel_collaborator" in
    let url_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle); ("collaboratorId", collaborator_id)] "/api/v1/video-channels/{channelHandle}/collaborators/{collaboratorId}/accept" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Reject a collaboration invitation
  
      **PeerTube >= 8.0** 
      @param channel_handle The video channel handle
      @param collaborator_id The collaborator id
  *)
  let reject_video_channel_collaborator ~channel_handle ~collaborator_id client () =
    let op_name = "reject_video_channel_collaborator" in
    let url_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle); ("collaboratorId", collaborator_id)] "/api/v1/video-channels/{channelHandle}/collaborators/{collaboratorId}/reject" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List followers of a video channel 
      @param channel_handle The video channel handle
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort followers by criteria
      @param search Plain text search, applied to various parts of the model depending on endpoint
  *)
  let get_video_channel_followers ~channel_handle ?start ?count ?sort ?search client () =
    let op_name = "get_video_channel_followers" in
    let url_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/followers" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"search" ~value:search]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Import videos in channel
  
      Import a remote channel/playlist videos into a channel 
      @param channel_handle The video channel handle
  *)
  let post_api_v1_video_channels_import_videos ~channel_handle ~body client () =
    let op_name = "post_api_v1_video_channels_import_videos" in
    let url_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/import-videos" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json ImportVideosInChannel.Create.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List playlists of a channel 
      @param channel_handle The video channel handle
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_api_v1_video_channels_video_playlists ~channel_handle ?start ?count ?sort ?playlist_type client () =
    let op_name = "get_api_v1_video_channels_video_playlists" in
    let url_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/video-playlists" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"playlistType" ~value:playlist_type]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Reorder channel playlists 
      @param channel_handle The video channel handle
  *)
  let reorder_video_playlists_of_channel ~channel_handle client () =
    let op_name = "reorder_video_playlists_of_channel" in
    let url_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/video-playlists/reorder" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List video playlists 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_playlists ?start ?count ?sort ?playlist_type client () =
    let op_name = "get_playlists" in
    let url_path = "/api/v1/video-playlists" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"playlistType" ~value:playlist_type]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create a video playlist
  
      If the video playlist is set as public, `videoChannelId` is mandatory. *)
  let add_playlist client () =
    let op_name = "add_playlist" in
    let url_path = "/api/v1/video-playlists" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List available playlist privacy policies *)
  let get_playlist_privacy_policies client () =
    let op_name = "get_playlist_privacy_policies" in
    let url_path = "/api/v1/video-playlists/privacies" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update a video playlist
  
      If the video playlist is set as public, the playlist must have a assigned channel. 
      @param playlist_id Playlist id
  *)
  let put_api_v1_video_playlists ~playlist_id client () =
    let op_name = "put_api_v1_video_playlists" in
    let url_path = Openapi.Runtime.Path.render ~params:[("playlistId", playlist_id)] "/api/v1/video-playlists/{playlistId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a video playlist 
      @param playlist_id Playlist id
  *)
  let delete_api_v1_video_playlists ~playlist_id client () =
    let op_name = "delete_api_v1_video_playlists" in
    let url_path = Openapi.Runtime.Path.render ~params:[("playlistId", playlist_id)] "/api/v1/video-playlists/{playlistId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List videos of a playlist 
      @param playlist_id Playlist id
      @param start Offset used to paginate results
      @param count Number of items to return
  *)
  let get_video_playlist_videos ~playlist_id ?start ?count client () =
    let op_name = "get_video_playlist_videos" in
    let url_path = Openapi.Runtime.Path.render ~params:[("playlistId", playlist_id)] "/api/v1/video-playlists/{playlistId}/videos" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Add a video in a playlist 
      @param playlist_id Playlist id
  *)
  let add_video_playlist_video ~playlist_id client () =
    let op_name = "add_video_playlist_video" in
    let url_path = Openapi.Runtime.Path.render ~params:[("playlistId", playlist_id)] "/api/v1/video-playlists/{playlistId}/videos" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Reorder playlist elements 
      @param playlist_id Playlist id
  *)
  let reorder_video_playlist ~playlist_id client () =
    let op_name = "reorder_video_playlist" in
    let url_path = Openapi.Runtime.Path.render ~params:[("playlistId", playlist_id)] "/api/v1/video-playlists/{playlistId}/videos/reorder" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update a playlist element 
      @param playlist_id Playlist id
      @param playlist_element_id Playlist element id
  *)
  let put_video_playlist_video ~playlist_id ~playlist_element_id client () =
    let op_name = "put_video_playlist_video" in
    let url_path = Openapi.Runtime.Path.render ~params:[("playlistId", playlist_id); ("playlistElementId", playlist_element_id)] "/api/v1/video-playlists/{playlistId}/videos/{playlistElementId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete an element from a playlist 
      @param playlist_id Playlist id
      @param playlist_element_id Playlist element id
  *)
  let del_video_playlist_video ~playlist_id ~playlist_element_id client () =
    let op_name = "del_video_playlist_video" in
    let url_path = Openapi.Runtime.Path.render ~params:[("playlistId", playlist_id); ("playlistElementId", playlist_element_id)] "/api/v1/video-playlists/{playlistId}/videos/{playlistElementId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List video blocks 
      @param type_ list only blocks that match this type:
  - `1`: manual block
  - `2`: automatic block that needs review
  
      @param search plain search that will match with video titles, and more
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort blocklists by criteria
  *)
  let get_video_blocks ?type_ ?search ?start ?count ?sort client () =
    let op_name = "get_video_blocks" in
    let url_path = "/api/v1/videos/blacklist" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"type" ~value:type_; Openapi.Runtime.Query.optional ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List available video categories *)
  let get_categories client () =
    let op_name = "get_categories" in
    let url_path = "/api/v1/videos/categories" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
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
  let get_api_v1_videos_comments ?search ?search_account ?search_video ?video_id ?video_channel_id ?auto_tag_one_of ?is_local ?on_local_video client () =
    let op_name = "get_api_v1_videos_comments" in
    let url_path = "/api/v1/videos/comments" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"searchAccount" ~value:search_account; Openapi.Runtime.Query.optional ~key:"searchVideo" ~value:search_video; Openapi.Runtime.Query.optional ~key:"videoId" ~value:video_id; Openapi.Runtime.Query.optional ~key:"videoChannelId" ~value:video_channel_id; Openapi.Runtime.Query.optional ~key:"autoTagOneOf" ~value:auto_tag_one_of; Openapi.Runtime.Query.optional ~key:"isLocal" ~value:is_local; Openapi.Runtime.Query.optional ~key:"onLocalVideo" ~value:on_local_video]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete video import
  
      Delete ended video import 
      @param id Entity id
  *)
  let delete_api_v1_videos_imports ~id client () =
    let op_name = "delete_api_v1_videos_imports" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/imports/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Cancel video import
  
      Cancel a pending video import 
      @param id Entity id
  *)
  let post_api_v1_videos_imports_cancel ~id client () =
    let op_name = "post_api_v1_videos_imports_cancel" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/imports/{id}/cancel" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Retry video import
  
      **PeerTube >= 8.0** Retry a pending video import 
      @param id Entity id
  *)
  let post_api_v1_videos_imports_retry ~id client () =
    let op_name = "post_api_v1_videos_imports_retry" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/imports/{id}/retry" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List available video languages *)
  let get_languages client () =
    let op_name = "get_languages" in
    let url_path = "/api/v1/videos/languages" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List available video licences *)
  let get_licences client () =
    let op_name = "get_licences" in
    let url_path = "/api/v1/videos/licences" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update information about a live 
      @param id The object id, uuid or short uuid
  *)
  let update_live_id ~id ~body client () =
    let op_name = "update_live_id" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/live/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json LiveVideo.Update.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List live sessions
  
      List all sessions created in a particular live 
      @param id The object id, uuid or short uuid
      @param sort Sort column
  *)
  let get_api_v1_videos_live_sessions ~id ?sort client () =
    let op_name = "get_api_v1_videos_live_sessions" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/live/{id}/sessions" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List video ownership changes *)
  let get_api_v1_videos_ownership client () =
    let op_name = "get_api_v1_videos_ownership" in
    let url_path = "/api/v1/videos/ownership" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Accept ownership change request 
      @param id The object id, uuid or short uuid
  *)
  let post_api_v1_videos_ownership_accept ~id client () =
    let op_name = "post_api_v1_videos_ownership_accept" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/ownership/{id}/accept" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Refuse ownership change request 
      @param id The object id, uuid or short uuid
  *)
  let post_api_v1_videos_ownership_refuse ~id client () =
    let op_name = "post_api_v1_videos_ownership_refuse" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/ownership/{id}/refuse" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List available video privacy policies *)
  let get_video_privacy_policies client () =
    let op_name = "get_video_privacy_policies" in
    let url_path = "/api/v1/videos/privacies" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Initialize the resumable upload of a video
  
      Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to initialize the upload of a video *)
  let upload_resumable_init ~body client () =
    let op_name = "upload_resumable_init" in
    let url_path = "/api/v1/videos/upload-resumable" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json VideoUploadRequestResumable.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Cancel the resumable upload of a video, deleting any data uploaded so far
  
      Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to cancel the upload of a video 
      @param upload_id Created session id to proceed with. If you didn't send chunks in the last hour, it is
  not valid anymore and you need to initialize a new upload.
  
  *)
  let upload_resumable_cancel ~upload_id client () =
    let op_name = "upload_resumable_cancel" in
    let url_path = "/api/v1/videos/upload-resumable" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"upload_id" ~value:upload_id]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update a video 
      @param id The object id, uuid or short uuid
  *)
  let put_video ~id client () =
    let op_name = "put_video" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a video 
      @param id The object id, uuid or short uuid
  *)
  let del_video ~id client () =
    let op_name = "del_video" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Block a video 
      @param id The object id, uuid or short uuid
  *)
  let add_video_block ~id client () =
    let op_name = "add_video_block" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/blacklist" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Unblock a video by its id 
      @param id The object id, uuid or short uuid
  *)
  let del_video_block ~id client () =
    let op_name = "del_video_block" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/blacklist" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List captions of a video 
      @param id The object id, uuid or short uuid
  *)
  let get_video_captions ~id client () =
    let op_name = "get_video_captions" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/captions" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Generate a video caption
  
      **PeerTube >= 6.2** This feature has to be enabled by the administrator 
      @param id The object id, uuid or short uuid
  *)
  let generate_video_caption ~id client () =
    let op_name = "generate_video_caption" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/captions/generate" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Add or replace a video caption 
      @param id The object id, uuid or short uuid
      @param caption_language The caption language
  *)
  let add_video_caption ~id ~caption_language client () =
    let op_name = "add_video_caption" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id); ("captionLanguage", caption_language)] "/api/v1/videos/{id}/captions/{captionLanguage}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a video caption 
      @param id The object id, uuid or short uuid
      @param caption_language The caption language
  *)
  let del_video_caption ~id ~caption_language client () =
    let op_name = "del_video_caption" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id); ("captionLanguage", caption_language)] "/api/v1/videos/{id}/captions/{captionLanguage}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Replace video chapters
  
      **PeerTube >= 6.0** 
      @param id The object id, uuid or short uuid
  *)
  let replace_video_chapters ~id client () =
    let op_name = "replace_video_chapters" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/chapters" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a comment or a reply 
      @param id The object id, uuid or short uuid
      @param comment_id The comment id
  *)
  let delete_api_v1_videos_comments ~id ~comment_id client () =
    let op_name = "delete_api_v1_videos_comments" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id); ("commentId", comment_id)] "/api/v1/videos/{id}/comments/{commentId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Approve a comment
  
      **PeerTube >= 6.2** Approve a comment that requires a review 
      @param id The object id, uuid or short uuid
      @param comment_id The comment id
  *)
  let post_api_v1_videos_comments_approve ~id ~comment_id client () =
    let op_name = "post_api_v1_videos_comments_approve" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id); ("commentId", comment_id)] "/api/v1/videos/{id}/comments/{commentId}/approve" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get complete video description 
      @param id The object id, uuid or short uuid
  *)
  let get_video_desc ~id client () =
    let op_name = "get_video_desc" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/description" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Request ownership change 
      @param id The object id, uuid or short uuid
  *)
  let post_api_v1_videos_give_ownership ~id client () =
    let op_name = "post_api_v1_videos_give_ownership" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/give-ownership" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete video HLS files 
      @param id The object id, uuid or short uuid
  *)
  let del_video_hls ~id client () =
    let op_name = "del_video_hls" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/hls" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List video passwords
  
      **PeerTube >= 6.0** 
      @param id The object id, uuid or short uuid
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let list_video_passwords ~id ?start ?count ?sort client () =
    let op_name = "list_video_passwords" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/passwords" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Add a video password
  
      **PeerTube >= 8.0** 
      @param id The object id, uuid or short uuid
  *)
  let add_video_password ~id client () =
    let op_name = "add_video_password" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/passwords" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update video passwords
  
      **PeerTube >= 6.0** 
      @param id The object id, uuid or short uuid
  *)
  let update_video_password_list ~id client () =
    let op_name = "update_video_password_list" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/passwords" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete a video password
  
      **PeerTube >= 6.0** 
      @param id The object id, uuid or short uuid
      @param video_password_id The video password id
  *)
  let remove_video_password ~id ~video_password_id client () =
    let op_name = "remove_video_password" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id); ("videoPasswordId", video_password_id)] "/api/v1/videos/{id}/passwords/{videoPasswordId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Like/dislike a video 
      @param id The object id, uuid or short uuid
  *)
  let put_api_v1_videos_rate ~id client () =
    let op_name = "put_api_v1_videos_rate" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/rate" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete video source file 
      @param id The object id, uuid or short uuid
  *)
  let delete_video_source_file ~id client () =
    let op_name = "delete_video_source_file" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/source/file" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Initialize the resumable replacement of a video
  
      **PeerTube >= 6.0** Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to initialize the replacement of a video 
      @param id The object id, uuid or short uuid
  *)
  let replace_video_source_resumable_init ~id ~body client () =
    let op_name = "replace_video_source_resumable_init" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/source/replace-resumable" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json VideoReplaceSourceRequestResumable.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Send chunk for the resumable replacement of a video
  
      **PeerTube >= 6.0** Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to continue, pause or resume the replacement of a video 
      @param id The object id, uuid or short uuid
      @param upload_id Created session id to proceed with. If you didn't send chunks in the last hour, it is
  not valid anymore and you need to initialize a new upload.
  
  *)
  let replace_video_source_resumable ~id ~upload_id client () =
    let op_name = "replace_video_source_resumable" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/source/replace-resumable" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"upload_id" ~value:upload_id]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Cancel the resumable replacement of a video
  
      **PeerTube >= 6.0** Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to cancel the replacement of a video 
      @param id The object id, uuid or short uuid
      @param upload_id Created session id to proceed with. If you didn't send chunks in the last hour, it is
  not valid anymore and you need to initialize a new upload.
  
  *)
  let replace_video_source_resumable_cancel ~id ~upload_id client () =
    let op_name = "replace_video_source_resumable_cancel" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/source/replace-resumable" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"upload_id" ~value:upload_id]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List storyboards of a video
  
      **PeerTube >= 6.0** 
      @param id The object id, uuid or short uuid
  *)
  let list_video_storyboards ~id client () =
    let op_name = "list_video_storyboards" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/storyboards" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create a studio task
  
      Create a task to edit a video  (cut, add intro/outro etc) 
      @param id The object id, uuid or short uuid
  *)
  let post_api_v1_videos_studio_edit ~id client () =
    let op_name = "post_api_v1_videos_studio_edit" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/studio/edit" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Create a transcoding job 
      @param id The object id, uuid or short uuid
  *)
  let create_video_transcoding ~id client () =
    let op_name = "create_video_transcoding" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/transcoding" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Notify user is watching a video
  
      Call this endpoint regularly (every 5-10 seconds for example) to notify the server the user is watching the video. After a while, PeerTube will increase video's viewers counter. If the user is authenticated, PeerTube will also store the current player time. 
      @param id The object id, uuid or short uuid
  *)
  let add_view ~id ~body client () =
    let op_name = "add_view" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/views" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json UserViewingVideo.T.jsont body)) url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete video Web Video files
  
      **PeerTube >= 6.0** 
      @param id The object id, uuid or short uuid
  *)
  let del_video_web_videos ~id client () =
    let op_name = "del_video_web_videos" in
    let url_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/web-videos" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List account watched words
  
      **PeerTube >= 6.2** 
      @param account_name account name to list watched words
  *)
  let get_api_v1_watched_words_accounts_lists ~account_name client () =
    let op_name = "get_api_v1_watched_words_accounts_lists" in
    let url_path = Openapi.Runtime.Path.render ~params:[("accountName", account_name)] "/api/v1/watched-words/accounts/{accountName}/lists" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Add account watched words
  
      **PeerTube >= 6.2** *)
  let post_api_v1_watched_words_accounts_lists ~account_name client () =
    let op_name = "post_api_v1_watched_words_accounts_lists" in
    let url_path = Openapi.Runtime.Path.render ~params:[("accountName", account_name)] "/api/v1/watched-words/accounts/{accountName}/lists" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update account watched words
  
      **PeerTube >= 6.2** 
      @param list_id list of watched words to update
  *)
  let put_api_v1_watched_words_accounts_lists ~account_name ~list_id client () =
    let op_name = "put_api_v1_watched_words_accounts_lists" in
    let url_path = Openapi.Runtime.Path.render ~params:[("accountName", account_name); ("listId", list_id)] "/api/v1/watched-words/accounts/{accountName}/lists/{listId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete account watched words
  
      **PeerTube >= 6.2** 
      @param list_id list of watched words to delete
  *)
  let delete_api_v1_watched_words_accounts_lists ~account_name ~list_id client () =
    let op_name = "delete_api_v1_watched_words_accounts_lists" in
    let url_path = Openapi.Runtime.Path.render ~params:[("accountName", account_name); ("listId", list_id)] "/api/v1/watched-words/accounts/{accountName}/lists/{listId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List server watched words
  
      **PeerTube >= 6.2** *)
  let get_api_v1_watched_words_server_lists client () =
    let op_name = "get_api_v1_watched_words_server_lists" in
    let url_path = "/api/v1/watched-words/server/lists" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Add server watched words
  
      **PeerTube >= 6.2** *)
  let post_api_v1_watched_words_server_lists client () =
    let op_name = "post_api_v1_watched_words_server_lists" in
    let url_path = "/api/v1/watched-words/server/lists" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.post client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "POST" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "POST";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Update server watched words
  
      **PeerTube >= 6.2** 
      @param list_id list of watched words to update
  *)
  let put_api_v1_watched_words_server_lists ~list_id client () =
    let op_name = "put_api_v1_watched_words_server_lists" in
    let url_path = Openapi.Runtime.Path.render ~params:[("listId", list_id)] "/api/v1/watched-words/server/lists/{listId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.put client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "PUT" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "PUT";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Delete server watched words
  
      **PeerTube >= 6.2** 
      @param list_id list of watched words to delete
  *)
  let delete_api_v1_watched_words_server_lists ~list_id client () =
    let op_name = "delete_api_v1_watched_words_server_lists" in
    let url_path = Openapi.Runtime.Path.render ~params:[("listId", list_id)] "/api/v1/watched-words/server/lists/{listId}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.delete client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "DELETE" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "DELETE";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Download video file
  
      Generate a mp4 container that contains at most 1 video stream and at most 1 audio stream. Mainly used to merge the HLS audio only video file and the HLS video only resolution file. 
      @param video_id The video id
      @param video_file_ids streams of video files to mux in the output
      @param video_file_token Video file token [generated](#operation/requestVideoToken) by PeerTube so you don't need to provide an OAuth token in the request header.
  *)
  let get_download_videos_generate ~video_id ~video_file_ids ?video_file_token client () =
    let op_name = "get_download_videos_generate" in
    let url_path = Openapi.Runtime.Path.render ~params:[("videoId", video_id)] "/download/videos/generate/{videoId}" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"videoFileIds" ~value:video_file_ids; Openapi.Runtime.Query.optional ~key:"videoFileToken" ~value:video_file_token]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Videos podcast feed 
      @param video_channel_id Limit listing to a specific video channel
  *)
  let get_videos_podcast_feed ~video_channel_id client () =
    let op_name = "get_videos_podcast_feed" in
    let url_path = "/feeds/podcast/videos.xml" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"videoChannelId" ~value:video_channel_id]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
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
  let get_syndicated_subscription_videos ~format ~account_id ~token ?sort ?nsfw ?is_local ?include_ ?privacy_one_of ?has_hlsfiles ?has_web_video_files client () =
    let op_name = "get_syndicated_subscription_videos" in
    let url_path = Openapi.Runtime.Path.render ~params:[("format", format)] "/feeds/subscriptions.{format}" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"accountId" ~value:account_id; Openapi.Runtime.Query.singleton ~key:"token" ~value:token; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"nsfw" ~value:nsfw; Openapi.Runtime.Query.optional ~key:"isLocal" ~value:is_local; Openapi.Runtime.Query.optional ~key:"include" ~value:include_; Openapi.Runtime.Query.optional ~key:"privacyOneOf" ~value:privacy_one_of; Openapi.Runtime.Query.optional ~key:"hasHLSFiles" ~value:has_hlsfiles; Openapi.Runtime.Query.optional ~key:"hasWebVideoFiles" ~value:has_web_video_files]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Comments on videos feeds 
      @param format format expected (we focus on making `rss` the most feature-rich ; it serves [Media RSS](https://www.rssboard.org/media-rss))
      @param video_id limit listing comments to a specific video
      @param account_id limit listing comments to videos of a specific account
      @param account_name limit listing comments to videos of a specific account
      @param video_channel_id limit listing comments to videos of a specific video channel
      @param video_channel_name limit listing comments to videos of a specific video channel
  *)
  let get_syndicated_comments ~format ?video_id ?account_id ?account_name ?video_channel_id ?video_channel_name client () =
    let op_name = "get_syndicated_comments" in
    let url_path = Openapi.Runtime.Path.render ~params:[("format", format)] "/feeds/video-comments.{format}" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"videoId" ~value:video_id; Openapi.Runtime.Query.optional ~key:"accountId" ~value:account_id; Openapi.Runtime.Query.optional ~key:"accountName" ~value:account_name; Openapi.Runtime.Query.optional ~key:"videoChannelId" ~value:video_channel_id; Openapi.Runtime.Query.optional ~key:"videoChannelName" ~value:video_channel_name]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
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
  let get_syndicated_videos ~format ?account_id ?account_name ?video_channel_id ?video_channel_name ?sort ?nsfw ?is_local ?include_ ?privacy_one_of ?has_hlsfiles ?has_web_video_files client () =
    let op_name = "get_syndicated_videos" in
    let url_path = Openapi.Runtime.Path.render ~params:[("format", format)] "/feeds/videos.{format}" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"accountId" ~value:account_id; Openapi.Runtime.Query.optional ~key:"accountName" ~value:account_name; Openapi.Runtime.Query.optional ~key:"videoChannelId" ~value:video_channel_id; Openapi.Runtime.Query.optional ~key:"videoChannelName" ~value:video_channel_name; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"nsfw" ~value:nsfw; Openapi.Runtime.Query.optional ~key:"isLocal" ~value:is_local; Openapi.Runtime.Query.optional ~key:"include" ~value:include_; Openapi.Runtime.Query.optional ~key:"privacyOneOf" ~value:privacy_one_of; Openapi.Runtime.Query.optional ~key:"hasHLSFiles" ~value:has_hlsfiles; Openapi.Runtime.Query.optional ~key:"hasWebVideoFiles" ~value:has_web_video_files]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get private HLS video file 
      @param filename Filename
      @param video_file_token Video file token [generated](#operation/requestVideoToken) by PeerTube so you don't need to provide an OAuth token in the request header.
      @param reinject_video_file_token Ask the server to reinject videoFileToken in URLs in m3u8 playlist
  *)
  let get_static_streaming_playlists_hls_private_ ~filename ?video_file_token ?reinject_video_file_token client () =
    let op_name = "get_static_streaming_playlists_hls_private_" in
    let url_path = Openapi.Runtime.Path.render ~params:[("filename", filename)] "/static/streaming-playlists/hls/private/{filename}" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"videoFileToken" ~value:video_file_token; Openapi.Runtime.Query.optional ~key:"reinjectVideoFileToken" ~value:reinject_video_file_token]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get public HLS video file 
      @param filename Filename
  *)
  let get_static_streaming_playlists_hls ~filename client () =
    let op_name = "get_static_streaming_playlists_hls" in
    let url_path = Openapi.Runtime.Path.render ~params:[("filename", filename)] "/static/streaming-playlists/hls/{filename}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get private Web Video file
  
      **PeerTube >= 6.0** 
      @param filename Filename
      @param video_file_token Video file token [generated](#operation/requestVideoToken) by PeerTube so you don't need to provide an OAuth token in the request header.
  *)
  let get_static_web_videos_private_ ~filename ?video_file_token client () =
    let op_name = "get_static_web_videos_private_" in
    let url_path = Openapi.Runtime.Path.render ~params:[("filename", filename)] "/static/web-videos/private/{filename}" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"videoFileToken" ~value:video_file_token]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get public Web Video file
  
      **PeerTube >= 6.0** 
      @param filename Filename
  *)
  let get_static_web_videos ~filename client () =
    let op_name = "get_static_web_videos" in
    let url_path = Openapi.Runtime.Path.render ~params:[("filename", filename)] "/static/web-videos/{filename}" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Requests.Response.json response
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module VideoChannelList = struct
  module Types = struct
    module T = struct
      type t = {
        data : VideoChannel.T.t list option;
        total : int option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?data ?total () = { data; total }
    
    let data t = t.data
    let total t = t.total
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"VideoChannelList"
        (fun data total -> { data; total })
      |> Jsont.Object.opt_mem "data" (Jsont.list VideoChannel.T.jsont) ~enc:(fun r -> r.data)
      |> Jsont.Object.opt_mem "total" Jsont.int ~enc:(fun r -> r.total)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
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
  let get_api_v1_accounts_video_channels ~name ?with_stats ?start ?count ?search ?sort ?include_collaborations client () =
    let op_name = "get_api_v1_accounts_video_channels" in
    let url_path = Openapi.Runtime.Path.render ~params:[("name", name)] "/api/v1/accounts/{name}/video-channels" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"withStats" ~value:with_stats; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"includeCollaborations" ~value:include_collaborations]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
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
  let search_channels ~search ?start ?count ?search_target ?sort ?host ?handles client () =
    let op_name = "search_channels" in
    let url_path = "/api/v1/search/video-channels" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.singleton ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"searchTarget" ~value:search_target; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"host" ~value:host; Openapi.Runtime.Query.optional ~key:"handles" ~value:handles]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List my user subscriptions 
      @param start Offset used to paginate results
      @param count Number of items to return
  *)
  let get_api_v1_users_me_subscriptions ?start ?count ?sort client () =
    let op_name = "get_api_v1_users_me_subscriptions" in
    let url_path = "/api/v1/users/me/subscriptions" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** List video channels 
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_video_channels ?start ?count ?sort client () =
    let op_name = "get_video_channels" in
    let url_path = "/api/v1/video-channels" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module UserWithStats = struct
  module Types = struct
    module T = struct
      type t = {
        account : Account.T.t option;
        admin_flags : UserAdminFlags.T.t option;
        auto_play_next_video : bool option;  (** Automatically start playing the upcoming video after the currently playing video *)
        auto_play_next_video_playlist : bool option;  (** Automatically start playing the video on the playlist after the currently playing video *)
        auto_play_video : bool option;  (** Automatically start playing the video on the watch page *)
        blocked : bool option;
        blocked_reason : string option;
        created_at : string option;
        email : string option;  (** The user email *)
        email_public : bool option;  (** Has the user accepted to display the email publicly? *)
        email_verified : bool option;  (** Has the user confirmed their email address? *)
        id : Id.T.t option;
        language : string option;  (** default language for this user *)
        last_login_date : Ptime.t option;
        new_features_info_read : float option;  (** New features information the user has read *)
        no_account_setup_warning_modal : bool option;
        no_instance_config_warning_modal : bool option;
        no_welcome_modal : bool option;
        notification_settings : UserNotificationSettings.T.t option;
        nsfw_flags_blurred : Nsfwflag.T.t option;
        nsfw_flags_displayed : Nsfwflag.T.t option;
        nsfw_flags_hidden : Nsfwflag.T.t option;
        nsfw_flags_warned : Nsfwflag.T.t option;
        nsfw_policy : Nsfwpolicy.T.t option;
        p2p_enabled : bool option;  (** whether to enable P2P in the player or not *)
        plugin_auth : string option;  (** Auth plugin to use to authenticate the user *)
        role : Jsont.json option;
        theme : string option;  (** Theme enabled by this user *)
        two_factor_enabled : bool option;  (** Whether the user has enabled two-factor authentication or not *)
        username : Username.T.t option;
        video_channels : VideoChannel.T.t list option;
        video_languages : string list option;  (** list of languages to filter videos down to *)
        video_quota : int option;  (** The user video quota in bytes *)
        video_quota_daily : int option;  (** The user daily video quota in bytes *)
        videos_history_enabled : bool option;  (** whether to keep track of watched history or not *)
        videos_count : int option;  (** Count of videos published *)
        abuses_count : int option;  (** Count of reports/abuses of which the user is a target *)
        abuses_accepted_count : int option;  (** Count of reports/abuses created by the user and accepted/acted upon by the moderation team *)
        abuses_created_count : int option;  (** Count of reports/abuses created by the user *)
        video_comments_count : int option;  (** Count of comments published *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?account ?admin_flags ?auto_play_next_video ?auto_play_next_video_playlist ?auto_play_video ?blocked ?blocked_reason ?created_at ?email ?email_public ?email_verified ?id ?language ?last_login_date ?new_features_info_read ?no_account_setup_warning_modal ?no_instance_config_warning_modal ?no_welcome_modal ?notification_settings ?nsfw_flags_blurred ?nsfw_flags_displayed ?nsfw_flags_hidden ?nsfw_flags_warned ?nsfw_policy ?p2p_enabled ?plugin_auth ?role ?theme ?two_factor_enabled ?username ?video_channels ?video_languages ?video_quota ?video_quota_daily ?videos_history_enabled ?videos_count ?abuses_count ?abuses_accepted_count ?abuses_created_count ?video_comments_count () = { account; admin_flags; auto_play_next_video; auto_play_next_video_playlist; auto_play_video; blocked; blocked_reason; created_at; email; email_public; email_verified; id; language; last_login_date; new_features_info_read; no_account_setup_warning_modal; no_instance_config_warning_modal; no_welcome_modal; notification_settings; nsfw_flags_blurred; nsfw_flags_displayed; nsfw_flags_hidden; nsfw_flags_warned; nsfw_policy; p2p_enabled; plugin_auth; role; theme; two_factor_enabled; username; video_channels; video_languages; video_quota; video_quota_daily; videos_history_enabled; videos_count; abuses_count; abuses_accepted_count; abuses_created_count; video_comments_count }
    
    let account t = t.account
    let admin_flags t = t.admin_flags
    let auto_play_next_video t = t.auto_play_next_video
    let auto_play_next_video_playlist t = t.auto_play_next_video_playlist
    let auto_play_video t = t.auto_play_video
    let blocked t = t.blocked
    let blocked_reason t = t.blocked_reason
    let created_at t = t.created_at
    let email t = t.email
    let email_public t = t.email_public
    let email_verified t = t.email_verified
    let id t = t.id
    let language t = t.language
    let last_login_date t = t.last_login_date
    let new_features_info_read t = t.new_features_info_read
    let no_account_setup_warning_modal t = t.no_account_setup_warning_modal
    let no_instance_config_warning_modal t = t.no_instance_config_warning_modal
    let no_welcome_modal t = t.no_welcome_modal
    let notification_settings t = t.notification_settings
    let nsfw_flags_blurred t = t.nsfw_flags_blurred
    let nsfw_flags_displayed t = t.nsfw_flags_displayed
    let nsfw_flags_hidden t = t.nsfw_flags_hidden
    let nsfw_flags_warned t = t.nsfw_flags_warned
    let nsfw_policy t = t.nsfw_policy
    let p2p_enabled t = t.p2p_enabled
    let plugin_auth t = t.plugin_auth
    let role t = t.role
    let theme t = t.theme
    let two_factor_enabled t = t.two_factor_enabled
    let username t = t.username
    let video_channels t = t.video_channels
    let video_languages t = t.video_languages
    let video_quota t = t.video_quota
    let video_quota_daily t = t.video_quota_daily
    let videos_history_enabled t = t.videos_history_enabled
    let videos_count t = t.videos_count
    let abuses_count t = t.abuses_count
    let abuses_accepted_count t = t.abuses_accepted_count
    let abuses_created_count t = t.abuses_created_count
    let video_comments_count t = t.video_comments_count
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"UserWithStats"
        (fun account admin_flags auto_play_next_video auto_play_next_video_playlist auto_play_video blocked blocked_reason created_at email email_public email_verified id language last_login_date new_features_info_read no_account_setup_warning_modal no_instance_config_warning_modal no_welcome_modal notification_settings nsfw_flags_blurred nsfw_flags_displayed nsfw_flags_hidden nsfw_flags_warned nsfw_policy p2p_enabled plugin_auth role theme two_factor_enabled username video_channels video_languages video_quota video_quota_daily videos_history_enabled videos_count abuses_count abuses_accepted_count abuses_created_count video_comments_count -> { account; admin_flags; auto_play_next_video; auto_play_next_video_playlist; auto_play_video; blocked; blocked_reason; created_at; email; email_public; email_verified; id; language; last_login_date; new_features_info_read; no_account_setup_warning_modal; no_instance_config_warning_modal; no_welcome_modal; notification_settings; nsfw_flags_blurred; nsfw_flags_displayed; nsfw_flags_hidden; nsfw_flags_warned; nsfw_policy; p2p_enabled; plugin_auth; role; theme; two_factor_enabled; username; video_channels; video_languages; video_quota; video_quota_daily; videos_history_enabled; videos_count; abuses_count; abuses_accepted_count; abuses_created_count; video_comments_count })
      |> Jsont.Object.opt_mem "account" Account.T.jsont ~enc:(fun r -> r.account)
      |> Jsont.Object.opt_mem "adminFlags" UserAdminFlags.T.jsont ~enc:(fun r -> r.admin_flags)
      |> Jsont.Object.opt_mem "autoPlayNextVideo" Jsont.bool ~enc:(fun r -> r.auto_play_next_video)
      |> Jsont.Object.opt_mem "autoPlayNextVideoPlaylist" Jsont.bool ~enc:(fun r -> r.auto_play_next_video_playlist)
      |> Jsont.Object.opt_mem "autoPlayVideo" Jsont.bool ~enc:(fun r -> r.auto_play_video)
      |> Jsont.Object.opt_mem "blocked" Jsont.bool ~enc:(fun r -> r.blocked)
      |> Jsont.Object.opt_mem "blockedReason" Jsont.string ~enc:(fun r -> r.blocked_reason)
      |> Jsont.Object.opt_mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "email" Jsont.string ~enc:(fun r -> r.email)
      |> Jsont.Object.opt_mem "emailPublic" Jsont.bool ~enc:(fun r -> r.email_public)
      |> Jsont.Object.opt_mem "emailVerified" Jsont.bool ~enc:(fun r -> r.email_verified)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "language" Jsont.string ~enc:(fun r -> r.language)
      |> Jsont.Object.opt_mem "lastLoginDate" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.last_login_date)
      |> Jsont.Object.opt_mem "newFeaturesInfoRead" Jsont.number ~enc:(fun r -> r.new_features_info_read)
      |> Jsont.Object.opt_mem "noAccountSetupWarningModal" Jsont.bool ~enc:(fun r -> r.no_account_setup_warning_modal)
      |> Jsont.Object.opt_mem "noInstanceConfigWarningModal" Jsont.bool ~enc:(fun r -> r.no_instance_config_warning_modal)
      |> Jsont.Object.opt_mem "noWelcomeModal" Jsont.bool ~enc:(fun r -> r.no_welcome_modal)
      |> Jsont.Object.opt_mem "notificationSettings" UserNotificationSettings.T.jsont ~enc:(fun r -> r.notification_settings)
      |> Jsont.Object.opt_mem "nsfwFlagsBlurred" Nsfwflag.T.jsont ~enc:(fun r -> r.nsfw_flags_blurred)
      |> Jsont.Object.opt_mem "nsfwFlagsDisplayed" Nsfwflag.T.jsont ~enc:(fun r -> r.nsfw_flags_displayed)
      |> Jsont.Object.opt_mem "nsfwFlagsHidden" Nsfwflag.T.jsont ~enc:(fun r -> r.nsfw_flags_hidden)
      |> Jsont.Object.opt_mem "nsfwFlagsWarned" Nsfwflag.T.jsont ~enc:(fun r -> r.nsfw_flags_warned)
      |> Jsont.Object.opt_mem "nsfwPolicy" Nsfwpolicy.T.jsont ~enc:(fun r -> r.nsfw_policy)
      |> Jsont.Object.opt_mem "p2pEnabled" Jsont.bool ~enc:(fun r -> r.p2p_enabled)
      |> Jsont.Object.opt_mem "pluginAuth" Jsont.string ~enc:(fun r -> r.plugin_auth)
      |> Jsont.Object.opt_mem "role" Jsont.json ~enc:(fun r -> r.role)
      |> Jsont.Object.opt_mem "theme" Jsont.string ~enc:(fun r -> r.theme)
      |> Jsont.Object.opt_mem "twoFactorEnabled" Jsont.bool ~enc:(fun r -> r.two_factor_enabled)
      |> Jsont.Object.opt_mem "username" Username.T.jsont ~enc:(fun r -> r.username)
      |> Jsont.Object.opt_mem "videoChannels" (Jsont.list VideoChannel.T.jsont) ~enc:(fun r -> r.video_channels)
      |> Jsont.Object.opt_mem "videoLanguages" (Jsont.list Jsont.string) ~enc:(fun r -> r.video_languages)
      |> Jsont.Object.opt_mem "videoQuota" Jsont.int ~enc:(fun r -> r.video_quota)
      |> Jsont.Object.opt_mem "videoQuotaDaily" Jsont.int ~enc:(fun r -> r.video_quota_daily)
      |> Jsont.Object.opt_mem "videosHistoryEnabled" Jsont.bool ~enc:(fun r -> r.videos_history_enabled)
      |> Jsont.Object.opt_mem "videosCount" Jsont.int ~enc:(fun r -> r.videos_count)
      |> Jsont.Object.opt_mem "abusesCount" Jsont.int ~enc:(fun r -> r.abuses_count)
      |> Jsont.Object.opt_mem "abusesAcceptedCount" Jsont.int ~enc:(fun r -> r.abuses_accepted_count)
      |> Jsont.Object.opt_mem "abusesCreatedCount" Jsont.int ~enc:(fun r -> r.abuses_created_count)
      |> Jsont.Object.opt_mem "videoCommentsCount" Jsont.int ~enc:(fun r -> r.video_comments_count)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module User = struct
  module Types = struct
    module T = struct
      type t = {
        account : Account.T.t option;
        admin_flags : UserAdminFlags.T.t option;
        auto_play_next_video : bool option;  (** Automatically start playing the upcoming video after the currently playing video *)
        auto_play_next_video_playlist : bool option;  (** Automatically start playing the video on the playlist after the currently playing video *)
        auto_play_video : bool option;  (** Automatically start playing the video on the watch page *)
        blocked : bool option;
        blocked_reason : string option;
        created_at : string option;
        email : string option;  (** The user email *)
        email_public : bool option;  (** Has the user accepted to display the email publicly? *)
        email_verified : bool option;  (** Has the user confirmed their email address? *)
        id : Id.T.t option;
        language : string option;  (** default language for this user *)
        last_login_date : Ptime.t option;
        new_features_info_read : float option;  (** New features information the user has read *)
        no_account_setup_warning_modal : bool option;
        no_instance_config_warning_modal : bool option;
        no_welcome_modal : bool option;
        notification_settings : UserNotificationSettings.T.t option;
        nsfw_flags_blurred : Nsfwflag.T.t option;
        nsfw_flags_displayed : Nsfwflag.T.t option;
        nsfw_flags_hidden : Nsfwflag.T.t option;
        nsfw_flags_warned : Nsfwflag.T.t option;
        nsfw_policy : Nsfwpolicy.T.t option;
        p2p_enabled : bool option;  (** whether to enable P2P in the player or not *)
        plugin_auth : string option;  (** Auth plugin to use to authenticate the user *)
        role : Jsont.json option;
        theme : string option;  (** Theme enabled by this user *)
        two_factor_enabled : bool option;  (** Whether the user has enabled two-factor authentication or not *)
        username : Username.T.t option;
        video_channels : VideoChannel.T.t list option;
        video_languages : string list option;  (** list of languages to filter videos down to *)
        video_quota : int option;  (** The user video quota in bytes *)
        video_quota_daily : int option;  (** The user daily video quota in bytes *)
        videos_history_enabled : bool option;  (** whether to keep track of watched history or not *)
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?account ?admin_flags ?auto_play_next_video ?auto_play_next_video_playlist ?auto_play_video ?blocked ?blocked_reason ?created_at ?email ?email_public ?email_verified ?id ?language ?last_login_date ?new_features_info_read ?no_account_setup_warning_modal ?no_instance_config_warning_modal ?no_welcome_modal ?notification_settings ?nsfw_flags_blurred ?nsfw_flags_displayed ?nsfw_flags_hidden ?nsfw_flags_warned ?nsfw_policy ?p2p_enabled ?plugin_auth ?role ?theme ?two_factor_enabled ?username ?video_channels ?video_languages ?video_quota ?video_quota_daily ?videos_history_enabled () = { account; admin_flags; auto_play_next_video; auto_play_next_video_playlist; auto_play_video; blocked; blocked_reason; created_at; email; email_public; email_verified; id; language; last_login_date; new_features_info_read; no_account_setup_warning_modal; no_instance_config_warning_modal; no_welcome_modal; notification_settings; nsfw_flags_blurred; nsfw_flags_displayed; nsfw_flags_hidden; nsfw_flags_warned; nsfw_policy; p2p_enabled; plugin_auth; role; theme; two_factor_enabled; username; video_channels; video_languages; video_quota; video_quota_daily; videos_history_enabled }
    
    let account t = t.account
    let admin_flags t = t.admin_flags
    let auto_play_next_video t = t.auto_play_next_video
    let auto_play_next_video_playlist t = t.auto_play_next_video_playlist
    let auto_play_video t = t.auto_play_video
    let blocked t = t.blocked
    let blocked_reason t = t.blocked_reason
    let created_at t = t.created_at
    let email t = t.email
    let email_public t = t.email_public
    let email_verified t = t.email_verified
    let id t = t.id
    let language t = t.language
    let last_login_date t = t.last_login_date
    let new_features_info_read t = t.new_features_info_read
    let no_account_setup_warning_modal t = t.no_account_setup_warning_modal
    let no_instance_config_warning_modal t = t.no_instance_config_warning_modal
    let no_welcome_modal t = t.no_welcome_modal
    let notification_settings t = t.notification_settings
    let nsfw_flags_blurred t = t.nsfw_flags_blurred
    let nsfw_flags_displayed t = t.nsfw_flags_displayed
    let nsfw_flags_hidden t = t.nsfw_flags_hidden
    let nsfw_flags_warned t = t.nsfw_flags_warned
    let nsfw_policy t = t.nsfw_policy
    let p2p_enabled t = t.p2p_enabled
    let plugin_auth t = t.plugin_auth
    let role t = t.role
    let theme t = t.theme
    let two_factor_enabled t = t.two_factor_enabled
    let username t = t.username
    let video_channels t = t.video_channels
    let video_languages t = t.video_languages
    let video_quota t = t.video_quota
    let video_quota_daily t = t.video_quota_daily
    let videos_history_enabled t = t.videos_history_enabled
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"User"
        (fun account admin_flags auto_play_next_video auto_play_next_video_playlist auto_play_video blocked blocked_reason created_at email email_public email_verified id language last_login_date new_features_info_read no_account_setup_warning_modal no_instance_config_warning_modal no_welcome_modal notification_settings nsfw_flags_blurred nsfw_flags_displayed nsfw_flags_hidden nsfw_flags_warned nsfw_policy p2p_enabled plugin_auth role theme two_factor_enabled username video_channels video_languages video_quota video_quota_daily videos_history_enabled -> { account; admin_flags; auto_play_next_video; auto_play_next_video_playlist; auto_play_video; blocked; blocked_reason; created_at; email; email_public; email_verified; id; language; last_login_date; new_features_info_read; no_account_setup_warning_modal; no_instance_config_warning_modal; no_welcome_modal; notification_settings; nsfw_flags_blurred; nsfw_flags_displayed; nsfw_flags_hidden; nsfw_flags_warned; nsfw_policy; p2p_enabled; plugin_auth; role; theme; two_factor_enabled; username; video_channels; video_languages; video_quota; video_quota_daily; videos_history_enabled })
      |> Jsont.Object.opt_mem "account" Account.T.jsont ~enc:(fun r -> r.account)
      |> Jsont.Object.opt_mem "adminFlags" UserAdminFlags.T.jsont ~enc:(fun r -> r.admin_flags)
      |> Jsont.Object.opt_mem "autoPlayNextVideo" Jsont.bool ~enc:(fun r -> r.auto_play_next_video)
      |> Jsont.Object.opt_mem "autoPlayNextVideoPlaylist" Jsont.bool ~enc:(fun r -> r.auto_play_next_video_playlist)
      |> Jsont.Object.opt_mem "autoPlayVideo" Jsont.bool ~enc:(fun r -> r.auto_play_video)
      |> Jsont.Object.opt_mem "blocked" Jsont.bool ~enc:(fun r -> r.blocked)
      |> Jsont.Object.opt_mem "blockedReason" Jsont.string ~enc:(fun r -> r.blocked_reason)
      |> Jsont.Object.opt_mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "email" Jsont.string ~enc:(fun r -> r.email)
      |> Jsont.Object.opt_mem "emailPublic" Jsont.bool ~enc:(fun r -> r.email_public)
      |> Jsont.Object.opt_mem "emailVerified" Jsont.bool ~enc:(fun r -> r.email_verified)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "language" Jsont.string ~enc:(fun r -> r.language)
      |> Jsont.Object.opt_mem "lastLoginDate" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.last_login_date)
      |> Jsont.Object.opt_mem "newFeaturesInfoRead" Jsont.number ~enc:(fun r -> r.new_features_info_read)
      |> Jsont.Object.opt_mem "noAccountSetupWarningModal" Jsont.bool ~enc:(fun r -> r.no_account_setup_warning_modal)
      |> Jsont.Object.opt_mem "noInstanceConfigWarningModal" Jsont.bool ~enc:(fun r -> r.no_instance_config_warning_modal)
      |> Jsont.Object.opt_mem "noWelcomeModal" Jsont.bool ~enc:(fun r -> r.no_welcome_modal)
      |> Jsont.Object.opt_mem "notificationSettings" UserNotificationSettings.T.jsont ~enc:(fun r -> r.notification_settings)
      |> Jsont.Object.opt_mem "nsfwFlagsBlurred" Nsfwflag.T.jsont ~enc:(fun r -> r.nsfw_flags_blurred)
      |> Jsont.Object.opt_mem "nsfwFlagsDisplayed" Nsfwflag.T.jsont ~enc:(fun r -> r.nsfw_flags_displayed)
      |> Jsont.Object.opt_mem "nsfwFlagsHidden" Nsfwflag.T.jsont ~enc:(fun r -> r.nsfw_flags_hidden)
      |> Jsont.Object.opt_mem "nsfwFlagsWarned" Nsfwflag.T.jsont ~enc:(fun r -> r.nsfw_flags_warned)
      |> Jsont.Object.opt_mem "nsfwPolicy" Nsfwpolicy.T.jsont ~enc:(fun r -> r.nsfw_policy)
      |> Jsont.Object.opt_mem "p2pEnabled" Jsont.bool ~enc:(fun r -> r.p2p_enabled)
      |> Jsont.Object.opt_mem "pluginAuth" Jsont.string ~enc:(fun r -> r.plugin_auth)
      |> Jsont.Object.opt_mem "role" Jsont.json ~enc:(fun r -> r.role)
      |> Jsont.Object.opt_mem "theme" Jsont.string ~enc:(fun r -> r.theme)
      |> Jsont.Object.opt_mem "twoFactorEnabled" Jsont.bool ~enc:(fun r -> r.two_factor_enabled)
      |> Jsont.Object.opt_mem "username" Username.T.jsont ~enc:(fun r -> r.username)
      |> Jsont.Object.opt_mem "videoChannels" (Jsont.list VideoChannel.T.jsont) ~enc:(fun r -> r.video_channels)
      |> Jsont.Object.opt_mem "videoLanguages" (Jsont.list Jsont.string) ~enc:(fun r -> r.video_languages)
      |> Jsont.Object.opt_mem "videoQuota" Jsont.int ~enc:(fun r -> r.video_quota)
      |> Jsont.Object.opt_mem "videoQuotaDaily" Jsont.int ~enc:(fun r -> r.video_quota_daily)
      |> Jsont.Object.opt_mem "videosHistoryEnabled" Jsont.bool ~enc:(fun r -> r.videos_history_enabled)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
  
  (** List users 
      @param search Plain text search that will match with user usernames or emails
      @param blocked Filter results down to (un)banned users
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort users by criteria
  *)
  let get_users ?search ?blocked ?start ?count ?sort client () =
    let op_name = "get_users" in
    let url_path = "/api/v1/users" in
    let query = Openapi.Runtime.Query.encode (List.concat [Openapi.Runtime.Query.optional ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"blocked" ~value:blocked; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
  
  (** Get my user information *)
  let get_user_info client () =
    let op_name = "get_user_info" in
    let url_path = "/api/v1/users/me" in
    let query = "" in
    let url = client.base_url ^ url_path ^ query in
    let response =
      try Requests.get client.session url
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "calling %s %s" "GET" url
    in
    if Requests.Response.ok response then
      Openapi.Runtime.Json.decode_json_exn T.jsont (Requests.Response.json response)
    else
      let body = Requests.Response.text response in
      let parsed_body =
        match Jsont_bytesrw.decode_string Jsont.json body with
        | Ok json -> Some (Openapi.Runtime.Json json)
        | Error _ -> Some (Openapi.Runtime.Raw body)
      in
      raise (Openapi.Runtime.Api_error {
        operation = op_name;
        method_ = "GET";
        url;
        status = Requests.Response.status_code response;
        body;
        parsed_body;
      })
end

module AbuseStateSet = struct
  module Types = struct
    module T = struct
      (** The abuse state (Pending = `1`, Rejected = `2`, Accepted = `3`) *)
      type t = int
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.int
  end
end

module AbuseStateConstant = struct
  module Types = struct
    module T = struct
      type t = {
        id : AbuseStateSet.T.t option;
        label : string option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?id ?label () = { id; label }
    
    let id t = t.id
    let label t = t.label
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"AbuseStateConstant"
        (fun id label -> { id; label })
      |> Jsont.Object.opt_mem "id" AbuseStateSet.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "label" Jsont.string ~enc:(fun r -> r.label)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end

module AbusePredefinedReasons = struct
  module Types = struct
    module T = struct
      type t = Jsont.json
    end
  end
  
  module T = struct
    include Types.T
    let jsont = Jsont.json
    let v () = Jsont.Null ((), Jsont.Meta.none)
  end
end

module Abuse = struct
  module Types = struct
    module T = struct
      type t = {
        created_at : Ptime.t option;
        id : Id.T.t option;
        moderation_comment : string option;
        predefined_reasons : AbusePredefinedReasons.T.t option;
        reason : string option;
        reporter_account : Account.T.t option;
        state : AbuseStateConstant.T.t option;
        video : Jsont.json option;
      }
    end
  end
  
  module T = struct
    include Types.T
    
    let v ?created_at ?id ?moderation_comment ?predefined_reasons ?reason ?reporter_account ?state ?video () = { created_at; id; moderation_comment; predefined_reasons; reason; reporter_account; state; video }
    
    let created_at t = t.created_at
    let id t = t.id
    let moderation_comment t = t.moderation_comment
    let predefined_reasons t = t.predefined_reasons
    let reason t = t.reason
    let reporter_account t = t.reporter_account
    let state t = t.state
    let video t = t.video
    
    let jsont : t Jsont.t =
      Jsont.Object.map ~kind:"Abuse"
        (fun created_at id moderation_comment predefined_reasons reason reporter_account state video -> { created_at; id; moderation_comment; predefined_reasons; reason; reporter_account; state; video })
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "moderationComment" (Openapi.Runtime.validated_string ~min_length:2 ~max_length:3000 Jsont.string) ~enc:(fun r -> r.moderation_comment)
      |> Jsont.Object.opt_mem "predefinedReasons" AbusePredefinedReasons.T.jsont ~enc:(fun r -> r.predefined_reasons)
      |> Jsont.Object.opt_mem "reason" (Openapi.Runtime.validated_string ~min_length:2 ~max_length:3000 Jsont.string) ~enc:(fun r -> r.reason)
      |> Jsont.Object.opt_mem "reporterAccount" Account.T.jsont ~enc:(fun r -> r.reporter_account)
      |> Jsont.Object.opt_mem "state" AbuseStateConstant.T.jsont ~enc:(fun r -> r.state)
      |> Jsont.Object.opt_mem "video" Jsont.json ~enc:(fun r -> r.video)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish
  end
end
