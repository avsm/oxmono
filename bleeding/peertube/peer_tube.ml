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

let __openapi_schemas = Openapi.Schema.of_string ~version:"3.0.3" "{\"Abuse\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"id\":{\"$ref\":\"#/components/schemas/id\"},\"moderationComment\":{\"type\":\"string\",\"example\":\"Decided to ban the server since it spams us regularly\",\"minLength\":2,\"maxLength\":3000},\"predefinedReasons\":{\"$ref\":\"#/components/schemas/AbusePredefinedReasons\"},\"reason\":{\"type\":\"string\",\"example\":\"The video is a spam\",\"minLength\":2,\"maxLength\":3000},\"reporterAccount\":{\"$ref\":\"#/components/schemas/Account\"},\"state\":{\"$ref\":\"#/components/schemas/AbuseStateConstant\"},\"video\":{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"integer\"},\"uuid\":{\"$ref\":\"#/components/schemas/UUIDv4\"},\"shortUUID\":{\"$ref\":\"#/components/schemas/shortUUID\"},\"name\":{\"type\":\"string\"},\"nsfw\":{\"type\":\"boolean\"},\"startAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"endAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"deleted\":{\"type\":\"boolean\"},\"blacklisted\":{\"type\":\"boolean\"},\"thumbnailPath\":{\"type\":\"string\"},\"channel\":{\"$ref\":\"#/components/schemas/VideoChannel\"}}}},\"required\":[]},\"AbuseMessage\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"account\":{\"$ref\":\"#/components/schemas/AccountSummary\"},\"byModerator\":{\"type\":\"boolean\"},\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"id\":{\"$ref\":\"#/components/schemas/id\"},\"message\":{\"type\":\"string\",\"minLength\":2,\"maxLength\":3000}},\"required\":[]},\"AbusePredefinedReasons\":{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"string\",\"enum\":[\"violentOrAbusive\",\"hatefulOrAbusive\",\"spamOrMisleading\",\"privacy\",\"rights\",\"serverRules\",\"thumbnails\",\"captions\"]},\"example\":[\"spamOrMisleading\"]},\"AbuseStateConstant\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/AbuseStateSet\"},\"label\":{\"type\":\"string\"}},\"required\":[]},\"AbuseStateSet\":{\"description\":\"The abuse state (Pending = `1`, Rejected = `2`, Accepted = `3`)\",\"type\":\"integer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[1,2,3],\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"Account\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"allOf\":[{\"$ref\":\"#/components/schemas/Actor\"},{\"properties\":{\"userId\":{\"description\":\"object id for the user tied to this account\",\"nullable\":true,\"allOf\":[{\"$ref\":\"#/components/schemas/User/properties/id\"}]},\"displayName\":{\"type\":\"string\",\"description\":\"editable name of the account, displayed in its representations\",\"minLength\":3,\"maxLength\":120},\"description\":{\"type\":\"string\",\"nullable\":true,\"description\":\"text or bio displayed on the account's profile\"}}}],\"properties\":{},\"required\":[]},\"AccountSummary\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"avatars\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/ActorImage\"}},\"displayName\":{\"type\":\"string\"},\"host\":{\"type\":\"string\",\"format\":\"hostname\"},\"id\":{\"type\":\"integer\"},\"name\":{\"type\":\"string\"},\"url\":{\"type\":\"string\",\"format\":\"url\"}},\"required\":[]},\"Actor\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"avatars\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/ActorImage\"}},\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"followersCount\":{\"type\":\"integer\",\"minimum\":0,\"description\":\"number of followers of this actor, as seen by this instance\"},\"followingCount\":{\"type\":\"integer\",\"minimum\":0,\"description\":\"number of actors subscribed to by this actor, as seen by this instance\"},\"host\":{\"type\":\"string\",\"format\":\"hostname\",\"description\":\"server on which the actor is resident\"},\"hostRedundancyAllowed\":{\"type\":\"boolean\",\"nullable\":true,\"description\":\"whether this actor's host allows redundancy of its videos\"},\"id\":{\"$ref\":\"#/components/schemas/id\"},\"name\":{\"description\":\"immutable name of the actor, used to find or mention it\",\"allOf\":[{\"$ref\":\"#/components/schemas/username\"}]},\"updatedAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"url\":{\"type\":\"string\",\"format\":\"url\"}},\"required\":[]},\"ActorImage\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"fileUrl\":{\"description\":\"**PeerTube >= 7.1**\",\"type\":\"string\"},\"height\":{\"type\":\"integer\",\"description\":\"**PeerTube >= 7.3**\"},\"path\":{\"description\":\"Deprecated in PeerTube v8.0, use fileUrl instead\",\"deprecated\":true,\"type\":\"string\"},\"updatedAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"width\":{\"type\":\"integer\"}},\"required\":[]},\"ActorInfo\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"avatars\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/ActorImage\"}},\"displayName\":{\"type\":\"string\"},\"host\":{\"type\":\"string\",\"format\":\"hostname\"},\"id\":{\"$ref\":\"#/components/schemas/id\"},\"name\":{\"type\":\"string\"}},\"required\":[]},\"AddUser\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"adminFlags\":{\"$ref\":\"#/components/schemas/UserAdminFlags\"},\"channelName\":{\"$ref\":\"#/components/schemas/usernameChannel\"},\"email\":{\"type\":\"string\",\"format\":\"email\",\"description\":\"The user email\"},\"password\":{\"$ref\":\"#/components/schemas/password\"},\"role\":{\"$ref\":\"#/components/schemas/UserRole\"},\"username\":{\"$ref\":\"#/components/schemas/username\"},\"videoQuota\":{\"type\":\"integer\",\"description\":\"The user video quota in bytes\",\"example\":-1},\"videoQuotaDaily\":{\"type\":\"integer\",\"description\":\"The user daily video quota in bytes\",\"example\":-1}},\"required\":[\"username\",\"password\",\"email\",\"role\"]},\"AddUserResponse\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"user\":{\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/id\"},\"account\":{\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/id\"}}}}}},\"required\":[]},\"AddVideoPasswords\":{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":true,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/VideoPassword/properties/password\"}},\"AutomaticTagAvailable\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"available\":{\"type\":\"array\",\"description\":\"Available auto tags that can be used to filter objects or set a comment in review state\",\"items\":{\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\",\"description\":\"tag name\"},\"type\":{\"type\":\"string\",\"enum\":[\"core\",\"watched-words-list\"]}}}}},\"required\":[]},\"BlockStatus\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"accounts\":{\"type\":\"object\",\"additionalProperties\":{\"x-additionalPropertiesName\":\"account\",\"type\":\"object\",\"properties\":{\"blockedByServer\":{\"type\":\"boolean\"},\"blockedByUser\":{\"type\":\"boolean\"}}}},\"hosts\":{\"type\":\"object\",\"additionalProperties\":{\"x-additionalPropertiesName\":\"host\",\"type\":\"object\",\"properties\":{\"blockedByServer\":{\"type\":\"boolean\"},\"blockedByUser\":{\"type\":\"boolean\"}}}}},\"required\":[]},\"ChannelActivityListResponse\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"integer\"},\"account\":{\"nullable\":true,\"description\":\"The account may have been deleted\",\"$ref\":\"#/components/schemas/AccountSummary\"},\"action\":{\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/VideoChannelActivityAction\"},\"label\":{\"type\":\"string\"}}},\"targetType\":{\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/VideoChannelActivityTarget\"},\"label\":{\"type\":\"string\"}}},\"details\":{\"type\":\"object\",\"additionalProperties\":true},\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"channel\":{\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/id\"},\"name\":{\"type\":\"string\"},\"displayName\":{\"type\":\"string\"},\"url\":{\"type\":\"string\",\"format\":\"url\"}}},\"video\":{\"nullable\":true,\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/id\"},\"name\":{\"type\":\"string\"},\"uuid\":{\"$ref\":\"#/components/schemas/UUIDv4\"},\"shortUUID\":{\"$ref\":\"#/components/schemas/shortUUID\"},\"url\":{\"type\":\"string\",\"format\":\"url\"},\"isLive\":{\"type\":\"boolean\"}}},\"videoImport\":{\"nullable\":true,\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/id\"},\"name\":{\"type\":\"string\"},\"uuid\":{\"$ref\":\"#/components/schemas/UUIDv4\"},\"shortUUID\":{\"$ref\":\"#/components/schemas/shortUUID\"},\"url\":{\"type\":\"string\",\"format\":\"url\"},\"targetUrl\":{\"type\":\"string\",\"format\":\"uri\"}}},\"playlist\":{\"nullable\":true,\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/id\"},\"name\":{\"type\":\"string\"},\"uuid\":{\"$ref\":\"#/components/schemas/UUIDv4\"},\"shortUUID\":{\"$ref\":\"#/components/schemas/shortUUID\"},\"url\":{\"type\":\"string\",\"format\":\"url\"}}},\"channelSync\":{\"nullable\":true,\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/id\"},\"externalChannelUrl\":{\"type\":\"string\",\"format\":\"uri\"}}}}}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]},\"CommentAutoTagPolicies\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"review\":{\"type\":\"array\",\"description\":\"Auto tags that automatically set the comment in review state\",\"items\":{\"type\":\"string\"}}},\"required\":[]},\"CommentThreadPostResponse\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"comment\":{\"$ref\":\"#/components/schemas/VideoComment\"}},\"required\":[]},\"CommentThreadResponse\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"maxItems\":100,\"items\":{\"$ref\":\"#/components/schemas/VideoComment\"}},\"total\":{\"type\":\"integer\",\"description\":\"Total threads (included deleted ones) on this video\"},\"totalNotDeletedComments\":{\"type\":\"integer\",\"description\":\"Total not-deleted threads (included deleted ones) on this video\"}},\"required\":[]},\"CustomHomepage\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"content\":{\"type\":\"string\"}},\"required\":[]},\"FileRedundancyInformation\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"expiresOn\":{\"type\":\"string\",\"format\":\"date-time\"},\"fileUrl\":{\"type\":\"string\",\"format\":\"url\"},\"id\":{\"$ref\":\"#/components/schemas/id\"},\"size\":{\"type\":\"integer\"},\"strategy\":{\"type\":\"string\",\"enum\":[\"manual\",\"most-views\",\"trending\",\"recently-added\"]},\"updatedAt\":{\"type\":\"string\",\"format\":\"date-time\"}},\"required\":[]},\"FileStorage\":{\"description\":\"The file storage type:\\n  - `0` File system\\n  - `1` Object storage\\n\",\"type\":\"integer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[0,1],\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"Follow\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"follower\":{\"$ref\":\"#/components/schemas/Actor\"},\"following\":{\"$ref\":\"#/components/schemas/Actor\"},\"id\":{\"$ref\":\"#/components/schemas/id\"},\"score\":{\"type\":\"number\",\"description\":\"score reflecting the reachability of the actor, with steps of `10` and a base score of `1000`.\"},\"state\":{\"type\":\"string\",\"enum\":[\"pending\",\"accepted\"]},\"updatedAt\":{\"type\":\"string\",\"format\":\"date-time\"}},\"required\":[]},\"GetMeVideoRating\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/id\"},\"rating\":{\"type\":\"string\",\"enum\":[\"like\",\"dislike\",\"none\"],\"description\":\"Rating of the video\"}},\"required\":[\"id\",\"rating\"]},\"ImportVideosInChannelCreate\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"externalChannelUrl\":{\"type\":\"string\",\"example\":\"https://youtube.com/c/UC_myfancychannel\"},\"videoChannelSyncId\":{\"type\":\"integer\",\"description\":\"If part of a channel sync process, specify its id to assign video imports to this channel synchronization\"}},\"required\":[\"externalChannelUrl\"]},\"Job\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"data\":{\"type\":\"object\",\"additionalProperties\":true},\"error\":{\"type\":\"object\",\"additionalProperties\":true},\"finishedOn\":{\"type\":\"string\",\"format\":\"date-time\"},\"id\":{\"$ref\":\"#/components/schemas/id\"},\"processedOn\":{\"type\":\"string\",\"format\":\"date-time\"},\"state\":{\"type\":\"string\",\"enum\":[\"active\",\"completed\",\"failed\",\"waiting\",\"delayed\"]},\"type\":{\"type\":\"string\",\"enum\":[\"activitypub-http-unicast\",\"activitypub-http-broadcast\",\"activitypub-http-fetcher\",\"activitypub-follow\",\"video-file-import\",\"video-transcoding\",\"email\",\"video-import\",\"videos-views-stats\",\"activitypub-refresher\",\"video-redundancy\",\"video-channel-import\"]}},\"required\":[]},\"LiveSchedule\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"startAt\":{\"type\":\"string\",\"format\":\"date-time\",\"description\":\"Date when the stream is scheduled to air at\"}},\"required\":[]},\"LiveVideoLatencyMode\":{\"description\":\"The live latency mode (Default = `1`, High latency = `2`, Small Latency = `3`)\",\"type\":\"integer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[1,2,3],\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"LiveVideoReplaySettings\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"privacy\":{\"$ref\":\"#/components/schemas/VideoPrivacySet\"}},\"required\":[]},\"LiveVideoResponse\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"latencyMode\":{\"description\":\"User can select live latency mode if enabled by the instance\",\"allOf\":[{\"$ref\":\"#/components/schemas/LiveVideoLatencyMode\"}]},\"permanentLive\":{\"description\":\"User can stream multiple times in a permanent live\",\"type\":\"boolean\"},\"replaySettings\":{\"$ref\":\"#/components/schemas/LiveVideoReplaySettings\"},\"rtmpUrl\":{\"type\":\"string\",\"description\":\"Included in the response if an appropriate token is provided\"},\"rtmpsUrl\":{\"type\":\"string\",\"description\":\"Included in the response if an appropriate token is provided\"},\"saveReplay\":{\"type\":\"boolean\"},\"schedules\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/LiveSchedule\"}},\"streamKey\":{\"type\":\"string\",\"description\":\"RTMP stream key to use to stream into this live video. Included in the response if an appropriate token is provided\"}},\"required\":[]},\"LiveVideoSessionResponse\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"endDate\":{\"type\":\"string\",\"format\":\"date-time\",\"nullable\":true,\"description\":\"End date of the live session\"},\"error\":{\"type\":\"integer\",\"enum\":[1,2,3,4,5],\"nullable\":true,\"description\":\"Error type if an error occurred during the live session:\\n  - `1`: Bad socket health (transcoding is too slow)\\n  - `2`: Max duration exceeded\\n  - `3`: Quota exceeded\\n  - `4`: Quota FFmpeg error\\n  - `5`: Video has been blacklisted during the live\\n\"},\"id\":{\"type\":\"integer\"},\"replayVideo\":{\"type\":\"object\",\"description\":\"Video replay information\",\"properties\":{\"id\":{\"type\":\"number\"},\"uuid\":{\"$ref\":\"#/components/schemas/UUIDv4\"},\"shortUUID\":{\"$ref\":\"#/components/schemas/shortUUID\"}}},\"startDate\":{\"type\":\"string\",\"format\":\"date-time\",\"description\":\"Start date of the live session\"}},\"required\":[]},\"LiveVideoUpdate\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"latencyMode\":{\"description\":\"User can select live latency mode if enabled by the instance\",\"allOf\":[{\"$ref\":\"#/components/schemas/LiveVideoLatencyMode\"}]},\"permanentLive\":{\"description\":\"User can stream multiple times in a permanent live\",\"type\":\"boolean\"},\"replaySettings\":{\"$ref\":\"#/components/schemas/LiveVideoReplaySettings\"},\"saveReplay\":{\"type\":\"boolean\"},\"schedules\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/LiveSchedule\"}}},\"required\":[]},\"MRSSGroupContent\":{\"xml\":{\"name\":\"media:content\"},\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"duration\":{\"type\":\"integer\",\"xml\":{\"attribute\":true}},\"fileSize\":{\"type\":\"integer\",\"xml\":{\"attribute\":true}},\"framerate\":{\"type\":\"integer\",\"xml\":{\"attribute\":true}},\"height\":{\"type\":\"integer\",\"xml\":{\"attribute\":true}},\"lang\":{\"type\":\"string\",\"xml\":{\"attribute\":true}},\"type\":{\"type\":\"string\",\"xml\":{\"attribute\":true}},\"url\":{\"type\":\"string\",\"format\":\"url\",\"xml\":{\"attribute\":true}}},\"required\":[]},\"MRSSPeerLink\":{\"xml\":{\"name\":\"media:peerLink\"},\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"href\":{\"type\":\"string\",\"xml\":{\"attribute\":true}},\"type\":{\"type\":\"string\",\"enum\":[\"application/x-bittorrent\"],\"xml\":{\"attribute\":true}}},\"required\":[]},\"NSFWFlag\":{\"description\":\"\\nNSFW flags (can be combined using bitwise or operator)\\n- `0` NONE\\n- `1` VIOLENT\\n- `2` EXPLICIT_SEX\\n\",\"type\":\"integer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[0,1,2,4],\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"NSFWPolicy\":{\"type\":\"string\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[\"display\",\"warn\",\"do_not_list\"],\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"NewFeatureInfoType\":{\"description\":\"Represent a new feature that can be displayed to inform users. One of the following values:\\n\\n  - `1` CHANNEL_COLLABORATION\\n\",\"type\":\"integer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[1],\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"Notification\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"account\":{\"nullable\":true,\"allOf\":[{\"$ref\":\"#/components/schemas/ActorInfo\"}]},\"actorFollow\":{\"type\":\"object\",\"nullable\":true,\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/id\"},\"follower\":{\"$ref\":\"#/components/schemas/ActorInfo\"},\"state\":{\"type\":\"string\",\"enum\":[\"pending\",\"accepted\"]},\"following\":{\"type\":\"object\",\"properties\":{\"type\":{\"type\":\"string\",\"enum\":[\"account\",\"channel\",\"instance\"]},\"name\":{\"type\":\"string\"},\"displayName\":{\"type\":\"string\"},\"host\":{\"type\":\"string\",\"format\":\"hostname\"}}}}},\"comment\":{\"nullable\":true,\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/id\"},\"threadId\":{\"type\":\"integer\"},\"video\":{\"$ref\":\"#/components/schemas/VideoInfo\"},\"account\":{\"$ref\":\"#/components/schemas/ActorInfo\"},\"heldForReview\":{\"type\":\"boolean\"}}},\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"id\":{\"$ref\":\"#/components/schemas/id\"},\"read\":{\"type\":\"boolean\"},\"type\":{\"$ref\":\"#/components/schemas/NotificationType\"},\"updatedAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"video\":{\"nullable\":true,\"allOf\":[{\"$ref\":\"#/components/schemas/VideoInfo\"},{\"type\":\"object\",\"properties\":{\"channel\":{\"$ref\":\"#/components/schemas/ActorInfo\"}}}]},\"videoAbuse\":{\"nullable\":true,\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/id\"},\"video\":{\"allOf\":[{\"$ref\":\"#/components/schemas/VideoInfo\"}]}}},\"videoBlacklist\":{\"nullable\":true,\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/id\"},\"video\":{\"allOf\":[{\"$ref\":\"#/components/schemas/VideoInfo\"}]}}},\"videoImport\":{\"nullable\":true,\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/id\"},\"video\":{\"nullable\":true,\"allOf\":[{\"$ref\":\"#/components/schemas/VideoInfo\"}]},\"torrentName\":{\"type\":\"string\",\"nullable\":true},\"magnetUri\":{\"$ref\":\"#/components/schemas/VideoImport/properties/magnetUri\"},\"targetUri\":{\"type\":\"string\",\"format\":\"uri\",\"nullable\":true}}}},\"required\":[]},\"NotificationListResponse\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"maxItems\":100,\"items\":{\"$ref\":\"#/components/schemas/Notification\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]},\"NotificationSettingValue\":{\"description\":\"Notification type. One of the following values, or a sum of multiple values:\\n- `0` NONE\\n- `1` WEB\\n- `2` EMAIL\\n\",\"type\":\"integer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"NotificationType\":{\"description\":\"Notification type. One of the following values:\\n\\n  - `1` NEW_VIDEO_FROM_SUBSCRIPTION\\n\\n  - `2` NEW_COMMENT_ON_MY_VIDEO\\n\\n  - `3` NEW_ABUSE_FOR_MODERATORS\\n\\n  - `4` BLACKLIST_ON_MY_VIDEO\\n\\n  - `5` UNBLACKLIST_ON_MY_VIDEO\\n\\n  - `6` MY_VIDEO_PUBLISHED\\n\\n  - `7` MY_VIDEO_IMPORT_SUCCESS\\n\\n  - `8` MY_VIDEO_IMPORT_ERROR\\n\\n  - `9` NEW_USER_REGISTRATION\\n\\n  - `10` NEW_FOLLOW\\n\\n  - `11` COMMENT_MENTION\\n\\n  - `12` VIDEO_AUTO_BLACKLIST_FOR_MODERATORS\\n\\n  - `13` NEW_INSTANCE_FOLLOWER\\n\\n  - `14` AUTO_INSTANCE_FOLLOWING\\n\\n  - `15` ABUSE_STATE_CHANGE\\n\\n  - `16` ABUSE_NEW_MESSAGE\\n\\n  - `17` NEW_PLUGIN_VERSION\\n\\n  - `18` NEW_PEERTUBE_VERSION\\n\\n  - `19` MY_VIDEO_STUDIO_EDITION_FINISHED\\n\\n  - `20` NEW_USER_REGISTRATION_REQUEST\\n\\n  - `21` NEW_LIVE_FROM_SUBSCRIPTION\\n\\n  - `22` MY_VIDEO_TRANSCRIPTION_GENERATED\\n\",\"type\":\"integer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22],\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"OAuthClient\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"client_id\":{\"type\":\"string\",\"pattern\":\"/^[a-z0-9]$/\",\"maxLength\":32,\"minLength\":32,\"example\":\"v1ikx5hnfop4mdpnci8nsqh93c45rldf\"},\"client_secret\":{\"type\":\"string\",\"pattern\":\"/^[a-zA-Z0-9]$/\",\"maxLength\":32,\"minLength\":32,\"example\":\"AjWiOapPltI6EnsWQwlFarRtLh4u8tDt\"}},\"required\":[]},\"OAuthToken-password\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"allOf\":[{\"$ref\":\"#/components/schemas/OAuthClient\"},{\"type\":\"object\",\"properties\":{\"grant_type\":{\"type\":\"string\",\"enum\":[\"password\"]},\"username\":{\"$ref\":\"#/components/schemas/User/properties/username\"},\"password\":{\"$ref\":\"#/components/schemas/password\"},\"externalAuthToken\":{\"type\":\"string\",\"description\":\"If you want to authenticate using an external authentication token you got from an auth plugin (like `peertube-plugin-auth-openid-connect` for example) instead of a password or a refresh token, provide it here.\"}}}],\"properties\":{},\"required\":[\"client_id\",\"client_secret\",\"grant_type\",\"username\"]},\"OAuthToken-refresh_token\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"allOf\":[{\"$ref\":\"#/components/schemas/OAuthClient\"},{\"type\":\"object\",\"properties\":{\"grant_type\":{\"type\":\"string\",\"enum\":[\"refresh_token\"]},\"refresh_token\":{\"type\":\"string\",\"example\":\"2e0d675df9fc96d2e4ec8a3ebbbf45eca9137bb7\"}}}],\"properties\":{},\"required\":[\"client_id\",\"client_secret\",\"grant_type\",\"refresh_token\"]},\"PlaybackMetricCreate\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"bufferStalled\":{\"type\":\"number\",\"description\":\"How many times buffer has been stalled since the last metric creation\"},\"downloadedBytesHTTP\":{\"type\":\"number\",\"description\":\"How many bytes were downloaded with HTTP since the last metric creation\"},\"downloadedBytesP2P\":{\"type\":\"number\",\"description\":\"How many bytes were downloaded with P2P since the last metric creation\"},\"errors\":{\"type\":\"number\",\"description\":\"How many errors occurred since the last metric creation\"},\"fps\":{\"type\":\"number\",\"description\":\"Current player video fps\"},\"p2pEnabled\":{\"type\":\"boolean\"},\"p2pPeers\":{\"type\":\"number\",\"description\":\"P2P peers connected (doesn't include WebSeed peers)\"},\"playerMode\":{\"type\":\"string\",\"enum\":[\"p2p-media-loader\",\"web-video\"]},\"resolution\":{\"type\":\"number\",\"description\":\"Current player video resolution\"},\"resolutionChanges\":{\"type\":\"number\",\"description\":\"How many resolution changes occurred since the last metric creation\"},\"uploadedBytesP2P\":{\"type\":\"number\",\"description\":\"How many bytes were uploaded with P2P since the last metric creation\"},\"videoId\":{\"oneOf\":[{\"$ref\":\"#/components/schemas/id\"},{\"$ref\":\"#/components/schemas/UUIDv4\"},{\"$ref\":\"#/components/schemas/shortUUID\"}]}},\"required\":[\"playerMode\",\"resolutionChanges\",\"errors\",\"downloadedBytesP2P\",\"downloadedBytesHTTP\",\"uploadedBytesP2P\",\"p2pEnabled\",\"videoId\"]},\"PlayerChannelSettings\":{\"description\":\"Player settings for a channel\",\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"theme\":{\"$ref\":\"#/components/schemas/PlayerThemeChannelSetting\"}},\"required\":[]},\"PlayerChannelSettingsUpdate\":{\"description\":\"Player settings update for a channel\",\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"theme\":{\"$ref\":\"#/components/schemas/PlayerThemeChannelSetting\"}},\"required\":[\"theme\"]},\"PlayerTheme\":{\"description\":\"The player theme to use\",\"type\":\"string\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[\"galaxy\",\"lucide\"],\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"PlayerThemeChannelSetting\":{\"description\":\"Player theme setting for a channel:\\n  - `instance-default` Use the instance default theme\\n  - `galaxy` Use the galaxy theme\\n  - `lucide` Use the lucide theme\\n\",\"type\":\"string\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[\"instance-default\",\"galaxy\",\"lucide\"],\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"PlayerThemeVideoSetting\":{\"description\":\"Player theme setting for a video:\\n  - `channel-default` Use the channel default theme\\n  - `instance-default` Use the instance default theme\\n  - `galaxy` Use the galaxy theme\\n  - `lucide` Use the lucide theme\\n\",\"type\":\"string\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[\"channel-default\",\"instance-default\",\"galaxy\",\"lucide\"],\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"PlayerVideoSettings\":{\"description\":\"Player settings for a video\",\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"theme\":{\"$ref\":\"#/components/schemas/PlayerThemeVideoSetting\"}},\"required\":[]},\"PlayerVideoSettingsUpdate\":{\"description\":\"Player settings update for a video\",\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"theme\":{\"$ref\":\"#/components/schemas/PlayerThemeVideoSetting\"}},\"required\":[\"theme\"]},\"PlaylistElement\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"position\":{\"type\":\"integer\"},\"startTimestamp\":{\"type\":\"integer\",\"format\":\"seconds\"},\"stopTimestamp\":{\"type\":\"integer\",\"format\":\"seconds\"},\"video\":{\"nullable\":true,\"allOf\":[{\"$ref\":\"#/components/schemas/Video\"}]}},\"required\":[]},\"Plugin\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"description\":{\"type\":\"string\"},\"enabled\":{\"type\":\"boolean\"},\"homepage\":{\"type\":\"string\",\"format\":\"url\",\"example\":\"https://framagit.org/framasoft/peertube/official-plugins/tree/master/peertube-plugin-auth-ldap\"},\"latestVersion\":{\"type\":\"string\",\"example\":\"0.0.3\"},\"name\":{\"type\":\"string\",\"example\":\"peertube-plugin-auth-ldap\"},\"peertubeEngine\":{\"type\":\"string\",\"example\":\"2.2.0\"},\"settings\":{\"type\":\"object\",\"additionalProperties\":true},\"type\":{\"type\":\"integer\",\"description\":\"- `1`: PLUGIN\\n- `2`: THEME\\n\",\"enum\":[1,2]},\"uninstalled\":{\"type\":\"boolean\"},\"updatedAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"version\":{\"type\":\"string\",\"example\":\"0.0.1\"}},\"required\":[]},\"PluginResponse\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"maxItems\":100,\"items\":{\"$ref\":\"#/components/schemas/Plugin\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]},\"PredefinedAbuseReasons\":{\"description\":\"Reason categories that help triage reports\",\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"maxItems\":8,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"string\",\"enum\":[\"violentOrAbusive\",\"hatefulOrAbusive\",\"spamOrMisleading\",\"privacy\",\"rights\",\"serverRules\",\"thumbnails\",\"captions\"]}},\"RegisterUser\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"channel\":{\"type\":\"object\",\"description\":\"channel base information used to create the first channel of the user\",\"properties\":{\"name\":{\"$ref\":\"#/components/schemas/usernameChannel\"},\"displayName\":{\"type\":\"string\"}}},\"displayName\":{\"type\":\"string\",\"description\":\"editable name of the user, displayed in its representations\",\"minLength\":1,\"maxLength\":120},\"email\":{\"type\":\"string\",\"format\":\"email\",\"description\":\"email of the user, used for login or service communications\"},\"password\":{\"$ref\":\"#/components/schemas/password\"},\"username\":{\"description\":\"immutable name of the user, used to find or mention its actor\",\"allOf\":[{\"$ref\":\"#/components/schemas/username\"}]}},\"required\":[\"username\",\"password\",\"email\"]},\"RequestTwoFactorResponse\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"otpRequest\":{\"type\":\"object\",\"properties\":{\"requestToken\":{\"type\":\"string\",\"description\":\"The token to send to confirm this request\"},\"secret\":{\"type\":\"string\",\"description\":\"The OTP secret\"},\"uri\":{\"type\":\"string\",\"description\":\"The OTP URI\"}}}},\"required\":[]},\"Runner\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"description\":{\"type\":\"string\"},\"id\":{\"type\":\"integer\"},\"ip\":{\"type\":\"string\"},\"lastContact\":{\"type\":\"string\",\"format\":\"date-time\"},\"name\":{\"type\":\"string\"},\"updatedAt\":{\"type\":\"string\",\"format\":\"date-time\"}},\"required\":[]},\"RunnerJob\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"error\":{\"nullable\":true,\"type\":\"string\",\"description\":\"Error message if the job is errored\"},\"failures\":{\"type\":\"integer\",\"description\":\"Number of times a remote runner failed to process this job. After too many failures, the job in \\\"error\\\" state\"},\"finishedAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"parent\":{\"nullable\":true,\"description\":\"If job has a parent job\",\"type\":\"object\",\"properties\":{\"type\":{\"$ref\":\"#/components/schemas/RunnerJobType\"},\"state\":{\"$ref\":\"#/components/schemas/RunnerJobStateConstant\"},\"uuid\":{\"$ref\":\"#/components/schemas/UUIDv4\"}}},\"payload\":{\"$ref\":\"#/components/schemas/RunnerJobPayload\"},\"priority\":{\"type\":\"integer\",\"description\":\"Job priority (less has more priority)\"},\"progress\":{\"type\":\"integer\",\"description\":\"Percentage progress\"},\"runner\":{\"nullable\":true,\"description\":\"If job is associated to a runner\",\"properties\":{\"id\":{\"type\":\"number\"},\"name\":{\"type\":\"string\"},\"description\":{\"type\":\"string\"}}},\"startedAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"state\":{\"$ref\":\"#/components/schemas/RunnerJobStateConstant\"},\"type\":{\"$ref\":\"#/components/schemas/RunnerJobType\"},\"updatedAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"uuid\":{\"$ref\":\"#/components/schemas/UUIDv4\"}},\"required\":[]},\"RunnerJobAdmin\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"allOf\":[{\"$ref\":\"#/components/schemas/RunnerJob\"},{\"type\":\"object\",\"properties\":{\"privatePayload\":{\"type\":\"object\"}}}],\"properties\":{},\"required\":[]},\"RunnerJobPayload\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"anyOf\":[{\"type\":\"object\",\"title\":\"VOD web video transcoding\",\"properties\":{\"input\":{\"type\":\"object\",\"properties\":{\"videoFileUrl\":{\"type\":\"string\"}}},\"output\":{\"type\":\"object\",\"properties\":{\"resolution\":{\"type\":\"number\"},\"fps\":{\"type\":\"number\"}}}}},{\"type\":\"object\",\"title\":\"VOD HLS transcoding\",\"properties\":{\"input\":{\"type\":\"object\",\"properties\":{\"videoFileUrl\":{\"type\":\"string\"}}},\"output\":{\"type\":\"object\",\"properties\":{\"resolution\":{\"type\":\"number\"},\"fps\":{\"type\":\"number\"}}}}},{\"type\":\"object\",\"title\":\"VOD audio merge transcoding\",\"properties\":{\"input\":{\"type\":\"object\",\"properties\":{\"audioFileUrl\":{\"type\":\"string\"},\"previewFileUrl\":{\"type\":\"string\"}}},\"output\":{\"type\":\"object\",\"properties\":{\"resolution\":{\"type\":\"number\"},\"fps\":{\"type\":\"number\"}}}}}],\"properties\":{},\"required\":[]},\"RunnerJobState\":{\"description\":\"The runner job state:\\n  - `1` Pending\\n  - `2` Processing\\n  - `3` Completed\\n  - `4` Errored\\n  - `5` Waiting for a parent job\\n  - `6` Cancelled\\n  - `7` Parent had an error\\n  - `8` Parent has been cancelled\\n\",\"type\":\"integer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[1,2,3,4,5,6,7,8],\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"RunnerJobStateConstant\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/RunnerJobState\"},\"label\":{\"type\":\"string\",\"example\":\"Processing\"}},\"required\":[]},\"RunnerJobType\":{\"type\":\"string\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[\"vod-web-video-transcoding\",\"vod-hls-transcoding\",\"vod-audio-merge-transcoding\",\"live-rtmp-hls-transcoding\"],\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"RunnerRegistrationToken\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"id\":{\"type\":\"integer\"},\"registeredRunnersCount\":{\"type\":\"integer\"},\"registrationToken\":{\"type\":\"string\"},\"updatedAt\":{\"type\":\"string\",\"format\":\"date-time\"}},\"required\":[]},\"SendClientLog\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"level\":{\"enum\":[\"error\",\"warn\"]},\"message\":{\"type\":\"string\"},\"meta\":{\"type\":\"string\",\"description\":\"Additional information regarding this log\"},\"stackTrace\":{\"type\":\"string\",\"description\":\"Stack trace of the error if there is one\"},\"url\":{\"type\":\"string\",\"description\":\"URL of the current user page\"},\"userAgent\":{\"type\":\"string\",\"description\":\"User agent of the web browser that sends the message\"}},\"required\":[\"message\",\"url\",\"level\"]},\"ServerConfig\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"autoBlacklist\":{\"type\":\"object\",\"properties\":{\"videos\":{\"type\":\"object\",\"properties\":{\"ofUsers\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"}}}}}}},\"avatar\":{\"type\":\"object\",\"properties\":{\"file\":{\"type\":\"object\",\"properties\":{\"size\":{\"type\":\"object\",\"properties\":{\"max\":{\"type\":\"integer\"}}}}},\"extensions\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}}}},\"contactForm\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"}}},\"email\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"}}},\"export\":{\"type\":\"object\",\"properties\":{\"users\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"},\"exportExpiration\":{\"type\":\"number\",\"description\":\"In milliseconds\"},\"maxUserVideoQuota\":{\"type\":\"number\",\"description\":\"In bytes\"}}}}},\"federation\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"}}},\"followings\":{\"type\":\"object\",\"properties\":{\"instance\":{\"type\":\"object\",\"properties\":{\"autoFollowIndex\":{\"type\":\"object\",\"properties\":{\"indexUrl\":{\"type\":\"string\",\"format\":\"url\"}}}}}}},\"homepage\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"}}},\"import\":{\"type\":\"object\",\"properties\":{\"videos\":{\"type\":\"object\",\"properties\":{\"http\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"}}},\"torrent\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"}}}}},\"videoChannelSynchronization\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"}}},\"users\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"}}}}},\"instance\":{\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\"},\"shortDescription\":{\"type\":\"string\"},\"defaultClientRoute\":{\"type\":\"string\"},\"isNSFW\":{\"type\":\"boolean\"},\"defaultNSFWPolicy\":{\"type\":\"string\"},\"serverCountry\":{\"type\":\"string\"},\"defaultLanguage\":{\"type\":\"string\"},\"support\":{\"type\":\"object\",\"properties\":{\"text\":{\"type\":\"string\"}}},\"social\":{\"type\":\"object\",\"properties\":{\"externalLink\":{\"type\":\"string\"},\"mastodonLink\":{\"type\":\"string\"},\"blueskyLink\":{\"type\":\"string\"},\"xLink\":{\"type\":\"string\"}}},\"customizations\":{\"type\":\"object\",\"properties\":{\"javascript\":{\"type\":\"string\"},\"css\":{\"type\":\"string\"}}},\"avatars\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/ActorImage\"}},\"banners\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/ActorImage\"}}}},\"openTelemetry\":{\"type\":\"object\",\"description\":\"PeerTube >= 6.1\",\"properties\":{\"metrics\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"},\"playbackStatsInterval\":{\"type\":\"number\",\"description\":\"Milliseconds\"}}}}},\"plugin\":{\"type\":\"object\",\"properties\":{\"registered\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}}}},\"search\":{\"type\":\"object\",\"properties\":{\"remoteUri\":{\"type\":\"object\",\"properties\":{\"users\":{\"type\":\"boolean\"},\"anonymous\":{\"type\":\"boolean\"}}}}},\"serverCommit\":{\"type\":\"string\"},\"serverVersion\":{\"type\":\"string\"},\"signup\":{\"type\":\"object\",\"properties\":{\"allowed\":{\"type\":\"boolean\"},\"allowedForCurrentIP\":{\"type\":\"boolean\"},\"requiresEmailVerification\":{\"type\":\"boolean\"}}},\"theme\":{\"type\":\"object\",\"properties\":{\"registered\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}}}},\"tracker\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"}}},\"transcoding\":{\"type\":\"object\",\"properties\":{\"hls\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"}}},\"web_videos\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"}}},\"enabledResolutions\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoResolutionSet\"}}}},\"trending\":{\"type\":\"object\",\"properties\":{\"videos\":{\"type\":\"object\",\"properties\":{\"intervalDays\":{\"type\":\"integer\"}}}}},\"user\":{\"type\":\"object\",\"properties\":{\"videoQuota\":{\"type\":\"integer\",\"description\":\"In bytes\",\"example\":16810141515},\"videoQuotaDaily\":{\"type\":\"integer\",\"description\":\"In bytes\",\"example\":1681014151}}},\"video\":{\"type\":\"object\",\"properties\":{\"image\":{\"type\":\"object\",\"properties\":{\"extensions\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}},\"size\":{\"type\":\"object\",\"properties\":{\"max\":{\"type\":\"integer\"}}}}},\"file\":{\"type\":\"object\",\"properties\":{\"extensions\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}}}}}},\"videoCaption\":{\"type\":\"object\",\"properties\":{\"file\":{\"type\":\"object\",\"properties\":{\"size\":{\"type\":\"object\",\"properties\":{\"max\":{\"type\":\"integer\"}}},\"extensions\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}}}}}},\"views\":{\"type\":\"object\",\"description\":\"PeerTube >= 6.1\",\"properties\":{\"views\":{\"type\":\"object\",\"properties\":{\"watchingInterval\":{\"type\":\"object\",\"properties\":{\"anonymous\":{\"type\":\"number\",\"description\":\"Milliseconds\"},\"users\":{\"type\":\"number\",\"description\":\"Milliseconds\"}}}}}}}},\"required\":[]},\"ServerConfigAbout\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"instance\":{\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\"},\"shortDescription\":{\"type\":\"string\"},\"description\":{\"type\":\"string\"},\"terms\":{\"type\":\"string\"},\"codeOfConduct\":{\"type\":\"string\"},\"hardwareInformation\":{\"type\":\"string\"},\"creationReason\":{\"type\":\"string\"},\"moderationInformation\":{\"type\":\"string\"},\"administrator\":{\"type\":\"string\"},\"maintenanceLifetime\":{\"type\":\"string\"},\"businessModel\":{\"type\":\"string\"},\"languages\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}},\"categories\":{\"type\":\"array\",\"items\":{\"type\":\"integer\"}},\"avatars\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/ActorImage\"}},\"banners\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/ActorImage\"}}}}},\"required\":[]},\"ServerConfigCustom\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"admin\":{\"type\":\"object\",\"properties\":{\"email\":{\"type\":\"string\",\"format\":\"email\"}}},\"autoBlacklist\":{\"type\":\"object\",\"properties\":{\"videos\":{\"type\":\"object\",\"properties\":{\"ofUsers\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"}}}}}}},\"cache\":{\"type\":\"object\",\"properties\":{\"previews\":{\"type\":\"object\",\"properties\":{\"size\":{\"type\":\"integer\"}}},\"captions\":{\"type\":\"object\",\"properties\":{\"size\":{\"type\":\"integer\"}}}}},\"contactForm\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"}}},\"defaults\":{\"type\":\"object\",\"properties\":{\"publish\":{\"type\":\"object\",\"properties\":{\"downloadEnabled\":{\"type\":\"boolean\"},\"commentsPolicy\":{\"$ref\":\"#/components/schemas/VideoCommentsPolicySet\"},\"privacy\":{\"$ref\":\"#/components/schemas/VideoPrivacySet\"},\"licence\":{\"$ref\":\"#/components/schemas/VideoLicenceSet\"}}},\"p2p\":{\"type\":\"object\",\"properties\":{\"webapp\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"}}},\"embed\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"}}}}},\"player\":{\"type\":\"object\",\"properties\":{\"autoPlay\":{\"type\":\"boolean\"}}}}},\"followers\":{\"type\":\"object\",\"properties\":{\"instance\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"},\"manualApproval\":{\"type\":\"boolean\"}}}}},\"import\":{\"type\":\"object\",\"properties\":{\"videos\":{\"type\":\"object\",\"properties\":{\"http\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"}}},\"torrent\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"}}}}},\"video_channel_synchronization\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"}}}}},\"instance\":{\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\"},\"shortDescription\":{\"type\":\"string\"},\"description\":{\"type\":\"string\"},\"terms\":{\"type\":\"string\"},\"codeOfConduct\":{\"type\":\"string\"},\"creationReason\":{\"type\":\"string\"},\"moderationInformation\":{\"type\":\"string\"},\"administrator\":{\"type\":\"string\"},\"maintenanceLifetime\":{\"type\":\"string\"},\"businessModel\":{\"type\":\"string\"},\"hardwareInformation\":{\"type\":\"string\"},\"languages\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}},\"categories\":{\"type\":\"array\",\"items\":{\"type\":\"number\"}},\"isNSFW\":{\"type\":\"boolean\"},\"defaultNSFWPolicy\":{\"type\":\"string\"},\"serverCountry\":{\"type\":\"string\"},\"support\":{\"type\":\"object\",\"properties\":{\"text\":{\"type\":\"string\"}}},\"social\":{\"type\":\"object\",\"properties\":{\"externalLink\":{\"type\":\"string\"},\"mastodonLink\":{\"type\":\"string\"},\"blueskyLink\":{\"type\":\"string\"},\"xLink\":{\"type\":\"string\"}}},\"defaultClientRoute\":{\"type\":\"string\"},\"customizations\":{\"type\":\"object\",\"properties\":{\"javascript\":{\"type\":\"string\"},\"css\":{\"type\":\"string\"}}}}},\"services\":{\"type\":\"object\",\"properties\":{\"twitter\":{\"type\":\"object\",\"properties\":{\"username\":{\"type\":\"string\"}}}}},\"signup\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"},\"limit\":{\"type\":\"integer\"},\"requiresEmailVerification\":{\"type\":\"boolean\"}}},\"storyboard\":{\"type\":\"object\",\"properties\":{\"enabled\":{\"type\":\"boolean\"}}},\"theme\":{\"type\":\"object\",\"properties\":{\"default\":{\"type\":\"string\"}}},\"transcoding\":{\"type\":\"object\",\"description\":\"Settings pertaining to transcoding jobs\",\"properties\":{\"enabled\":{\"type\":\"boolean\"},\"originalFile\":{\"type\":\"object\",\"properties\":{\"keep\":{\"type\":\"boolean\"}}},\"allowAdditionalExtensions\":{\"type\":\"boolean\",\"description\":\"Allow your users to upload .mkv, .mov, .avi, .wmv, .flv, .f4v, .3g2, .3gp, .mts, m2ts, .mxf, .nut videos\"},\"allowAudioFiles\":{\"type\":\"boolean\",\"description\":\"If a user uploads an audio file, PeerTube will create a video by merging the preview file and the audio file\"},\"threads\":{\"type\":\"integer\",\"description\":\"Amount of threads used by ffmpeg for 1 transcoding job\"},\"concurrency\":{\"type\":\"number\",\"description\":\"Amount of transcoding jobs to execute in parallel\"},\"profile\":{\"type\":\"string\",\"enum\":[\"default\"],\"description\":\"New profiles can be added by plugins ; available in core PeerTube: 'default'.\\n\"},\"resolutions\":{\"type\":\"object\",\"description\":\"Resolutions to transcode _new videos_ to\",\"properties\":{\"0p\":{\"type\":\"boolean\"},\"144p\":{\"type\":\"boolean\"},\"240p\":{\"type\":\"boolean\"},\"360p\":{\"type\":\"boolean\"},\"480p\":{\"type\":\"boolean\"},\"720p\":{\"type\":\"boolean\"},\"1080p\":{\"type\":\"boolean\"},\"1440p\":{\"type\":\"boolean\"},\"2160p\":{\"type\":\"boolean\"}}},\"web_videos\":{\"type\":\"object\",\"description\":\"Web Video specific settings\",\"properties\":{\"enabled\":{\"type\":\"boolean\"}}},\"hls\":{\"type\":\"object\",\"description\":\"HLS specific settings\",\"properties\":{\"enabled\":{\"type\":\"boolean\"},\"splitAudioAndVideo\":{\"type\":\"boolean\"}}}}},\"user\":{\"type\":\"object\",\"description\":\"Settings that apply to new users, if registration is enabled\",\"properties\":{\"videoQuota\":{\"type\":\"integer\",\"example\":16810141515},\"videoQuotaDaily\":{\"type\":\"integer\",\"example\":1681014151}}}},\"required\":[]},\"ServerError\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"code\":{\"type\":\"string\",\"example\":\"video_requires_password\"},\"detail\":{\"type\":\"string\",\"example\":\"Please provide a password to access this password protected video\"},\"status\":{\"type\":\"integer\",\"example\":403},\"type\":{\"type\":\"string\",\"example\":\"https://docs.joinpeertube.org/api-rest-reference.html#section/Errors/video_requires_password\"}},\"required\":[]},\"ServerStats\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"activityPubMessagesProcessedPerSecond\":{\"type\":\"number\"},\"averageAbuseResponseTimeMs\":{\"type\":\"number\",\"description\":\"**PeerTube >= 6.1** Value is null if the admin disabled abuses stats\"},\"averageRegistrationRequestResponseTimeMs\":{\"type\":\"number\",\"description\":\"**PeerTube >= 6.1** Value is null if the admin disabled registration requests stats\"},\"totalAbuses\":{\"type\":\"number\",\"description\":\"**PeerTube >= 6.1** Value is null if the admin disabled abuses stats\"},\"totalAbusesProcessed\":{\"type\":\"number\",\"description\":\"**PeerTube >= 6.1** Value is null if the admin disabled abuses stats\"},\"totalActivityPubMessagesErrors\":{\"type\":\"number\"},\"totalActivityPubMessagesProcessed\":{\"type\":\"number\"},\"totalActivityPubMessagesSuccesses\":{\"type\":\"number\"},\"totalActivityPubMessagesWaiting\":{\"type\":\"number\"},\"totalAdmins\":{\"type\":\"number\",\"description\":\"**PeerTube >= 6.1** Value is null if the admin disabled total admins stats\"},\"totalDailyActiveUsers\":{\"type\":\"number\"},\"totalInstanceFollowers\":{\"type\":\"number\"},\"totalInstanceFollowing\":{\"type\":\"number\"},\"totalLocalDailyActiveVideoChannels\":{\"type\":\"number\"},\"totalLocalMonthlyActiveVideoChannels\":{\"type\":\"number\"},\"totalLocalPlaylists\":{\"type\":\"number\"},\"totalLocalVideoChannels\":{\"type\":\"number\"},\"totalLocalVideoComments\":{\"type\":\"number\",\"description\":\"Total comments made by local users\"},\"totalLocalVideoFilesSize\":{\"type\":\"number\"},\"totalLocalVideoViews\":{\"type\":\"number\",\"description\":\"Total video views made on the instance\"},\"totalLocalVideos\":{\"type\":\"number\"},\"totalLocalWeeklyActiveVideoChannels\":{\"type\":\"number\"},\"totalModerators\":{\"type\":\"number\",\"description\":\"**PeerTube >= 6.1** Value is null if the admin disabled total moderators stats\"},\"totalMonthlyActiveUsers\":{\"type\":\"number\"},\"totalRegistrationRequests\":{\"type\":\"number\",\"description\":\"**PeerTube >= 6.1** Value is null if the admin disabled registration requests stats\"},\"totalRegistrationRequestsProcessed\":{\"type\":\"number\",\"description\":\"**PeerTube >= 6.1** Value is null if the admin disabled registration requests stats\"},\"totalUsers\":{\"type\":\"number\"},\"totalVideoComments\":{\"type\":\"number\"},\"totalVideos\":{\"type\":\"number\"},\"totalWeeklyActiveUsers\":{\"type\":\"number\"},\"videosRedundancy\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"strategy\":{\"type\":\"string\"},\"totalSize\":{\"type\":\"number\"},\"totalUsed\":{\"type\":\"number\"},\"totalVideoFiles\":{\"type\":\"number\"},\"totalVideos\":{\"type\":\"number\"}}}}},\"required\":[]},\"Storyboard\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"fileUrl\":{\"description\":\"**PeerTube >= 7.1**\",\"type\":\"string\"},\"spriteDuration\":{\"type\":\"integer\"},\"spriteHeight\":{\"type\":\"integer\"},\"spriteWidth\":{\"type\":\"integer\"},\"storyboardPath\":{\"description\":\"Deprecated in PeerTube v8.0, use fileUrl instead\",\"deprecated\":true,\"type\":\"string\"},\"totalHeight\":{\"type\":\"integer\"},\"totalWidth\":{\"type\":\"integer\"}},\"required\":[]},\"TokenSession\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"currentSession\":{\"type\":\"boolean\",\"description\":\"Is this session the current one?\"},\"id\":{\"type\":\"integer\"},\"lastActivityDate\":{\"type\":\"string\",\"format\":\"date-time\"},\"lastActivityDevice\":{\"type\":\"string\"},\"lastActivityIP\":{\"type\":\"string\",\"format\":\"ipv4\"},\"loginDate\":{\"type\":\"string\",\"format\":\"date-time\",\"description\":\"Date of the login\"},\"loginDevice\":{\"type\":\"string\",\"description\":\"Device used to login\"},\"loginIP\":{\"type\":\"string\",\"format\":\"ipv4\",\"description\":\"IP address used to login\"}},\"required\":[]},\"UUIDv4\":{\"type\":\"string\",\"format\":\"uuid\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"minLength\":36,\"maxLength\":36,\"pattern\":\"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$\",\"uniqueItems\":false,\"properties\":{},\"required\":[],\"example\":\"9c9de5e8-0a1e-484a-b099-e80766180a6d\"},\"UpdateMe\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"autoPlayNextVideo\":{\"type\":\"boolean\",\"description\":\"new preference regarding playing following videos automatically\"},\"autoPlayNextVideoPlaylist\":{\"type\":\"boolean\",\"description\":\"new preference regarding playing following playlist videos automatically\"},\"autoPlayVideo\":{\"type\":\"boolean\",\"description\":\"new preference regarding playing videos automatically\"},\"currentPassword\":{\"$ref\":\"#/components/schemas/password\"},\"displayName\":{\"type\":\"string\",\"description\":\"new name of the user in its representations\",\"minLength\":3,\"maxLength\":120},\"email\":{\"description\":\"new email used for login and service communications\",\"allOf\":[{\"$ref\":\"#/components/schemas/User/properties/email\"}]},\"language\":{\"type\":\"string\",\"description\":\"default language for this user\"},\"noAccountSetupWarningModal\":{\"type\":\"boolean\"},\"noInstanceConfigWarningModal\":{\"type\":\"boolean\"},\"noWelcomeModal\":{\"type\":\"boolean\"},\"nsfwFlagsBlurred\":{\"$ref\":\"#/components/schemas/NSFWFlag\"},\"nsfwFlagsDisplayed\":{\"$ref\":\"#/components/schemas/NSFWFlag\"},\"nsfwFlagsHidden\":{\"$ref\":\"#/components/schemas/NSFWFlag\"},\"nsfwFlagsWarned\":{\"$ref\":\"#/components/schemas/NSFWFlag\"},\"nsfwPolicy\":{\"type\":\"string\",\"description\":\"new NSFW display policy\",\"enum\":[\"true\",\"false\",\"both\"]},\"p2pEnabled\":{\"type\":\"boolean\",\"description\":\"whether to enable P2P in the player or not\"},\"password\":{\"$ref\":\"#/components/schemas/password\"},\"theme\":{\"type\":\"string\"},\"videoLanguages\":{\"type\":\"array\",\"items\":{\"type\":\"string\"},\"description\":\"list of languages to filter videos down to\"},\"videosHistoryEnabled\":{\"type\":\"boolean\",\"description\":\"whether to keep track of watched history or not\"}},\"required\":[]},\"UpdateUser\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"adminFlags\":{\"$ref\":\"#/components/schemas/UserAdminFlags\"},\"email\":{\"description\":\"The updated email of the user\",\"allOf\":[{\"$ref\":\"#/components/schemas/User/properties/email\"}]},\"emailVerified\":{\"type\":\"boolean\",\"description\":\"Set the email as verified\"},\"password\":{\"$ref\":\"#/components/schemas/password\"},\"pluginAuth\":{\"type\":\"string\",\"nullable\":true,\"description\":\"The auth plugin to use to authenticate the user\",\"example\":\"peertube-plugin-auth-saml2\"},\"role\":{\"$ref\":\"#/components/schemas/UserRole\"},\"videoQuota\":{\"type\":\"integer\",\"description\":\"The updated video quota of the user in bytes\"},\"videoQuotaDaily\":{\"type\":\"integer\",\"description\":\"The updated daily video quota of the user in bytes\"}},\"required\":[]},\"User\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"account\":{\"$ref\":\"#/components/schemas/Account\"},\"adminFlags\":{\"$ref\":\"#/components/schemas/UserAdminFlags\"},\"autoPlayNextVideo\":{\"type\":\"boolean\",\"description\":\"Automatically start playing the upcoming video after the currently playing video\"},\"autoPlayNextVideoPlaylist\":{\"type\":\"boolean\",\"description\":\"Automatically start playing the video on the playlist after the currently playing video\"},\"autoPlayVideo\":{\"type\":\"boolean\",\"description\":\"Automatically start playing the video on the watch page\"},\"blocked\":{\"type\":\"boolean\"},\"blockedReason\":{\"type\":\"string\"},\"createdAt\":{\"type\":\"string\"},\"email\":{\"type\":\"string\",\"format\":\"email\",\"description\":\"The user email\"},\"emailPublic\":{\"type\":\"boolean\",\"description\":\"Has the user accepted to display the email publicly?\"},\"emailVerified\":{\"type\":\"boolean\",\"description\":\"Has the user confirmed their email address?\"},\"id\":{\"$ref\":\"#/components/schemas/id\"},\"language\":{\"type\":\"string\",\"description\":\"default language for this user\"},\"lastLoginDate\":{\"type\":\"string\",\"format\":\"date-time\"},\"newFeaturesInfoRead\":{\"type\":\"number\",\"description\":\"New features information the user has read\"},\"noAccountSetupWarningModal\":{\"type\":\"boolean\"},\"noInstanceConfigWarningModal\":{\"type\":\"boolean\"},\"noWelcomeModal\":{\"type\":\"boolean\"},\"notificationSettings\":{\"$ref\":\"#/components/schemas/UserNotificationSettings\"},\"nsfwFlagsBlurred\":{\"$ref\":\"#/components/schemas/NSFWFlag\"},\"nsfwFlagsDisplayed\":{\"$ref\":\"#/components/schemas/NSFWFlag\"},\"nsfwFlagsHidden\":{\"$ref\":\"#/components/schemas/NSFWFlag\"},\"nsfwFlagsWarned\":{\"$ref\":\"#/components/schemas/NSFWFlag\"},\"nsfwPolicy\":{\"$ref\":\"#/components/schemas/NSFWPolicy\"},\"p2pEnabled\":{\"type\":\"boolean\",\"description\":\"whether to enable P2P in the player or not\"},\"pluginAuth\":{\"type\":\"string\",\"description\":\"Auth plugin to use to authenticate the user\"},\"role\":{\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/UserRole\"},\"label\":{\"type\":\"string\",\"enum\":[\"User\",\"Moderator\",\"Administrator\"]}}},\"theme\":{\"type\":\"string\",\"description\":\"Theme enabled by this user\"},\"twoFactorEnabled\":{\"type\":\"boolean\",\"description\":\"Whether the user has enabled two-factor authentication or not\"},\"username\":{\"$ref\":\"#/components/schemas/username\"},\"videoChannels\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoChannel\"}},\"videoLanguages\":{\"type\":\"array\",\"items\":{\"type\":\"string\"},\"description\":\"list of languages to filter videos down to\"},\"videoQuota\":{\"type\":\"integer\",\"description\":\"The user video quota in bytes\",\"example\":-1},\"videoQuotaDaily\":{\"type\":\"integer\",\"description\":\"The user daily video quota in bytes\",\"example\":-1},\"videosHistoryEnabled\":{\"type\":\"boolean\",\"description\":\"whether to keep track of watched history or not\"}},\"required\":[]},\"UserAdminFlags\":{\"description\":\"Admin flags for the user (None = `0`, Bypass video blocklist = `1`)\",\"type\":\"integer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[0,1],\"uniqueItems\":false,\"properties\":{},\"required\":[],\"example\":1},\"UserExportState\":{\"description\":\"The user export state:\\n  - `1`: Pending\\n  - `2`: Processing\\n  - `3`: Completed\\n  - `4`: Errored\\n\",\"type\":\"integer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[1,2,3,4],\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"UserImportResumable\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"filename\":{\"description\":\"Archive filename including extension\",\"type\":\"string\",\"format\":\"filename\",\"example\":\"user-export-6-2024-02-09T10_12_11.682Z\"}},\"required\":[]},\"UserImportState\":{\"description\":\"The user import state:\\n  - `1`: Pending\\n  - `2`: Processing\\n  - `3`: Completed\\n  - `4`: Errored\\n\",\"type\":\"integer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[1,2,3,4],\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"UserNotificationSettings\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"abuseAsModerator\":{\"$ref\":\"#/components/schemas/NotificationSettingValue\"},\"abuseNewMessage\":{\"$ref\":\"#/components/schemas/NotificationSettingValue\"},\"abuseStateChange\":{\"$ref\":\"#/components/schemas/NotificationSettingValue\"},\"autoInstanceFollowing\":{\"$ref\":\"#/components/schemas/NotificationSettingValue\"},\"blacklistOnMyVideo\":{\"$ref\":\"#/components/schemas/NotificationSettingValue\"},\"commentMention\":{\"$ref\":\"#/components/schemas/NotificationSettingValue\"},\"myVideoImportFinished\":{\"$ref\":\"#/components/schemas/NotificationSettingValue\"},\"myVideoPublished\":{\"$ref\":\"#/components/schemas/NotificationSettingValue\"},\"myVideoStudioEditionFinished\":{\"$ref\":\"#/components/schemas/NotificationSettingValue\"},\"myVideoTranscriptionGenerated\":{\"$ref\":\"#/components/schemas/NotificationSettingValue\"},\"newCommentOnMyVideo\":{\"$ref\":\"#/components/schemas/NotificationSettingValue\"},\"newFollow\":{\"$ref\":\"#/components/schemas/NotificationSettingValue\"},\"newInstanceFollower\":{\"$ref\":\"#/components/schemas/NotificationSettingValue\"},\"newPeerTubeVersion\":{\"$ref\":\"#/components/schemas/NotificationSettingValue\"},\"newPluginVersion\":{\"$ref\":\"#/components/schemas/NotificationSettingValue\"},\"newUserRegistration\":{\"$ref\":\"#/components/schemas/NotificationSettingValue\"},\"newVideoFromSubscription\":{\"$ref\":\"#/components/schemas/NotificationSettingValue\"},\"videoAutoBlacklistAsModerator\":{\"$ref\":\"#/components/schemas/NotificationSettingValue\"}},\"required\":[]},\"UserRegistration\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"accountDisplayName\":{\"type\":\"string\"},\"channelDisplayName\":{\"type\":\"string\"},\"channelHandle\":{\"type\":\"string\"},\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"email\":{\"type\":\"string\",\"format\":\"email\"},\"emailVerified\":{\"type\":\"boolean\"},\"id\":{\"$ref\":\"#/components/schemas/id\"},\"moderationResponse\":{\"type\":\"string\",\"nullable\":true},\"registrationReason\":{\"type\":\"string\"},\"state\":{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"integer\",\"enum\":[1,2,3],\"description\":\"The registration state (Pending = `1`, Rejected = `2`, Accepted = `3`)\"},\"label\":{\"type\":\"string\"}}},\"updatedAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"user\":{\"type\":\"object\",\"nullable\":true,\"description\":\"If the registration has been accepted, this is a partial user object created by the registration\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/id\"}}},\"username\":{\"type\":\"string\"}},\"required\":[]},\"UserRegistrationAcceptOrReject\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"moderationResponse\":{\"type\":\"string\",\"description\":\"Moderation response to send to the user\"},\"preventEmailDelivery\":{\"type\":\"boolean\",\"description\":\"Set it to true if you don't want PeerTube to send an email to the user\"}},\"required\":[\"moderationResponse\"]},\"UserRegistrationRequest\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"allOf\":[{\"$ref\":\"#/components/schemas/RegisterUser\"},{\"type\":\"object\",\"properties\":{\"registrationReason\":{\"type\":\"string\",\"description\":\"reason for the user to register on the instance\"}},\"required\":[\"registrationReason\"]}],\"properties\":{},\"required\":[]},\"UserRole\":{\"description\":\"The user role (Admin = `0`, Moderator = `1`, User = `2`)\",\"type\":\"integer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[0,1,2],\"uniqueItems\":false,\"properties\":{},\"required\":[],\"example\":2},\"UserViewingVideo\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"client\":{\"type\":\"string\",\"description\":\"Client software used to watch the video. For example \\\"Firefox\\\", \\\"PeerTube Approval Android\\\", etc.\\n\"},\"currentTime\":{\"type\":\"integer\",\"format\":\"seconds\",\"description\":\"timestamp within the video, in seconds\",\"example\":5},\"device\":{\"description\":\"Device used to watch the video. For example \\\"desktop\\\", \\\"mobile\\\", \\\"smarttv\\\", etc.\\n\",\"allOf\":[{\"$ref\":\"#/components/schemas/VideoStatsUserAgentDevice\"}]},\"operatingSystem\":{\"type\":\"string\",\"description\":\"Operating system used to watch the video. For example \\\"Windows\\\", \\\"Ubuntu\\\", etc.\\n\"},\"sessionId\":{\"type\":\"string\",\"description\":\"Optional param to represent the current viewer session. Used by the backend to properly count one view per session per video. PeerTube admin can configure the server to not trust this `sessionId` parameter but use the request IP address instead to identify a viewer.\\n\"},\"viewEvent\":{\"type\":\"string\",\"enum\":[\"seek\"],\"description\":\"Event since last viewing call:\\n * `seek` - If the user seeked the video\\n\"}},\"required\":[\"currentTime\"]},\"UserWithStats\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"allOf\":[{\"$ref\":\"#/components/schemas/User\"},{\"properties\":{\"videosCount\":{\"type\":\"integer\",\"description\":\"Count of videos published\"},\"abusesCount\":{\"type\":\"integer\",\"description\":\"Count of reports/abuses of which the user is a target\"},\"abusesAcceptedCount\":{\"type\":\"integer\",\"description\":\"Count of reports/abuses created by the user and accepted/acted upon by the moderation team\"},\"abusesCreatedCount\":{\"type\":\"integer\",\"description\":\"Count of reports/abuses created by the user\"},\"videoCommentsCount\":{\"type\":\"integer\",\"description\":\"Count of comments published\"}}}],\"properties\":{},\"required\":[]},\"Video\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"account\":{\"$ref\":\"#/components/schemas/AccountSummary\"},\"aspectRatio\":{\"type\":\"number\",\"nullable\":true,\"format\":\"float\",\"example\":1.778,\"description\":\"**PeerTube >= 6.1** Aspect ratio of the video stream\"},\"blacklisted\":{\"nullable\":true,\"type\":\"boolean\"},\"blacklistedReason\":{\"nullable\":true,\"type\":\"string\"},\"category\":{\"allOf\":[{\"$ref\":\"#/components/schemas/VideoConstantNumber-Category\"}],\"description\":\"category in which the video is classified\"},\"channel\":{\"$ref\":\"#/components/schemas/VideoChannelSummary\"},\"comments\":{\"description\":\"**PeerTube >= 7.2** Number of comments on the video\",\"type\":\"integer\"},\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\",\"example\":\"2017-10-01T10:52:46.396Z\",\"description\":\"time at which the video object was first drafted\"},\"dislikes\":{\"type\":\"integer\",\"example\":7},\"duration\":{\"type\":\"integer\",\"example\":1419,\"format\":\"seconds\",\"description\":\"duration of the video in seconds\"},\"embedPath\":{\"type\":\"string\",\"example\":\"/videos/embed/a65bc12f-9383-462e-81ae-8207e8b434ee\"},\"id\":{\"description\":\"object id for the video\",\"allOf\":[{\"$ref\":\"#/components/schemas/id\"}]},\"isLive\":{\"type\":\"boolean\"},\"isLocal\":{\"type\":\"boolean\"},\"language\":{\"allOf\":[{\"$ref\":\"#/components/schemas/VideoConstantString-Language\"}],\"description\":\"main language used in the video\"},\"licence\":{\"allOf\":[{\"$ref\":\"#/components/schemas/VideoConstantNumber-Licence\"}],\"description\":\"licence under which the video is distributed\"},\"likes\":{\"type\":\"integer\",\"example\":42},\"liveSchedules\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/LiveSchedule\"}},\"name\":{\"type\":\"string\",\"description\":\"title of the video\",\"example\":\"What is PeerTube?\",\"minLength\":3,\"maxLength\":120},\"nsfw\":{\"type\":\"boolean\"},\"nsfwFlags\":{\"allOf\":[{\"$ref\":\"#/components/schemas/NSFWFlag\"}]},\"nsfwSummary\":{\"type\":\"string\",\"nullable\":true,\"description\":\"**PeerTube >= 7.2** More information about the sensitive content of the video\"},\"originallyPublishedAt\":{\"type\":\"string\",\"nullable\":true,\"format\":\"date-time\",\"example\":\"2010-10-01T10:52:46.396Z\",\"description\":\"used to represent a date of first publication, prior to the practical publication date of `publishedAt`\"},\"previewPath\":{\"type\":\"string\",\"example\":\"/lazy-static/previews/a65bc12f-9383-462e-81ae-8207e8b434ee.jpg\"},\"privacy\":{\"allOf\":[{\"$ref\":\"#/components/schemas/VideoPrivacyConstant\"}],\"description\":\"privacy policy used to distribute the video\"},\"publishedAt\":{\"type\":\"string\",\"format\":\"date-time\",\"example\":\"2018-10-01T10:52:46.396Z\",\"description\":\"time at which the video was marked as ready for playback (with restrictions depending on `privacy`). Usually set after a `state` evolution.\"},\"scheduledUpdate\":{\"nullable\":true,\"allOf\":[{\"$ref\":\"#/components/schemas/VideoScheduledUpdate\"}]},\"shortUUID\":{\"allOf\":[{\"$ref\":\"#/components/schemas/shortUUID\"}]},\"state\":{\"allOf\":[{\"$ref\":\"#/components/schemas/VideoStateConstant\"}],\"description\":\"represents the internal state of the video processing within the PeerTube instance\"},\"thumbnailPath\":{\"type\":\"string\",\"example\":\"/lazy-static/thumbnails/a65bc12f-9383-462e-81ae-8207e8b434ee.jpg\"},\"truncatedDescription\":{\"type\":\"string\",\"nullable\":true,\"example\":\"**[Want to help to translate this video?](https://weblate.framasoft.org/projects/what-is-peertube-video/)**\\\\r\\\\n\\\\r\\\\n\\n**Take back the control of your videos! [#JoinPeertube](https://joinpeertube.org)**\\\\r\\\\n*A decentralized video hosting network, based on fr...\\n\",\"minLength\":3,\"maxLength\":250,\"description\":\"truncated description of the video, written in Markdown.\\n\"},\"updatedAt\":{\"type\":\"string\",\"format\":\"date-time\",\"example\":\"2021-05-04T08:01:01.502Z\",\"description\":\"last time the video's metadata was modified\"},\"userHistory\":{\"nullable\":true,\"type\":\"object\",\"properties\":{\"currentTime\":{\"type\":\"integer\"}}},\"uuid\":{\"description\":\"universal identifier for the video, that can be used across instances\",\"allOf\":[{\"$ref\":\"#/components/schemas/UUIDv4\"}]},\"views\":{\"type\":\"integer\",\"example\":1337},\"waitTranscoding\":{\"type\":\"boolean\",\"nullable\":true}},\"required\":[]},\"VideoBlacklist\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"description\":{\"type\":\"string\",\"minLength\":3,\"maxLength\":10000},\"dislikes\":{\"type\":\"integer\"},\"duration\":{\"type\":\"integer\"},\"id\":{\"$ref\":\"#/components/schemas/id\"},\"likes\":{\"type\":\"integer\"},\"name\":{\"type\":\"string\",\"minLength\":3,\"maxLength\":120},\"nsfw\":{\"type\":\"boolean\"},\"updatedAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"uuid\":{\"$ref\":\"#/components/schemas/UUIDv4\"},\"videoId\":{\"$ref\":\"#/components/schemas/Video/properties/id\"},\"views\":{\"type\":\"integer\"}},\"required\":[]},\"VideoCaption\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"automaticallyGenerated\":{\"type\":\"boolean\"},\"captionPath\":{\"type\":\"string\",\"deprecated\":true,\"description\":\"Deprecated in PeerTube v8.0, use fileUrl instead\"},\"fileUrl\":{\"description\":\"**PeerTube >= 7.1**\",\"type\":\"string\"},\"language\":{\"$ref\":\"#/components/schemas/VideoConstantString-Language\"},\"m3u8Url\":{\"type\":\"string\"},\"updatedAt\":{\"type\":\"string\",\"format\":\"date-time\"}},\"required\":[]},\"VideoCategorySet\":{\"description\":\"category id of the video (see [/videos/categories](#operation/getCategories))\",\"type\":\"integer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"example\":15},\"VideoChannel\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"allOf\":[{\"$ref\":\"#/components/schemas/Actor\"},{\"type\":\"object\",\"properties\":{\"displayName\":{\"type\":\"string\",\"description\":\"editable name of the channel, displayed in its representations\",\"example\":\"Videos of Framasoft\",\"minLength\":1,\"maxLength\":120},\"description\":{\"type\":\"string\",\"nullable\":true,\"example\":\"Videos made with <3 by Framasoft\",\"minLength\":3,\"maxLength\":1000},\"support\":{\"type\":\"string\",\"nullable\":true,\"description\":\"text shown by default on all videos of this channel, to tell the audience how to support it\",\"example\":\"Please support our work on https://soutenir.framasoft.org/en/ <3\",\"minLength\":3,\"maxLength\":1000},\"isLocal\":{\"type\":\"boolean\"},\"updatedAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"banners\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/ActorImage\"}},\"ownerAccount\":{\"$ref\":\"#/components/schemas/Account\"}}}],\"properties\":{},\"required\":[]},\"VideoChannelActivityAction\":{\"description\":\"The activity action:\\n  - CREATE: 1\\n  - UPDATE: 2\\n  - DELETE: 3\\n  - UPDATE_CAPTIONS: 4\\n  - UPDATE_CHAPTERS: 5\\n  - UPDATE_PASSWORDS: 6\\n  - CREATE_STUDIO_TASKS: 7\\n  - UPDATE_SOURCE_FILE: 8\\n  - UPDATE_ELEMENTS: 9\\n  - REMOVE_CHANNEL_OWNERSHIP: 10\\n  - CREATE_CHANNEL_OWNERSHIP: 11\\n\",\"type\":\"integer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[1,2,3,4,5,6,7,8,9,10,11],\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"VideoChannelActivityTarget\":{\"description\":\"The activity target:\\n  - VIDEO: 1,\\n  - PLAYLIST: 2,\\n  - CHANNEL: 3,\\n  - CHANNEL_SYNC: 4,\\n  - VIDEO_IMPORT: 5\\n\",\"type\":\"integer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[1,2,3,4,5],\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"VideoChannelCollaborator\":{\"description\":\"Representation of a channel collaboration\",\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"account\":{\"$ref\":\"#/components/schemas/AccountSummary\"},\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"id\":{\"$ref\":\"#/components/schemas/id\"},\"state\":{\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/VideoChannelCollaboratorState\"},\"label\":{\"type\":\"string\"}}},\"updatedAt\":{\"type\":\"string\",\"format\":\"date-time\"}},\"required\":[]},\"VideoChannelCollaboratorState\":{\"description\":\"The user import state:\\n  - `1`: Pending\\n  - `2`: Accepted\\n\",\"type\":\"integer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[1,2],\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"VideoChannelCreate\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"allOf\":[{\"$ref\":\"#/components/schemas/VideoChannelEdit\"},{\"properties\":{\"name\":{\"description\":\"username of the channel to create\",\"allOf\":[{\"$ref\":\"#/components/schemas/usernameChannel\"}]}}}],\"properties\":{},\"required\":[\"name\",\"displayName\"]},\"VideoChannelEdit\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"description\":{\"description\":\"Channel description\"},\"displayName\":{\"description\":\"Channel display name\"},\"support\":{\"description\":\"How to support/fund the channel\"}},\"required\":[]},\"VideoChannelList\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"allOf\":[{\"$ref\":\"#/components/schemas/VideoChannel\"},{\"$ref\":\"#/components/schemas/Actor\"}]}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]},\"VideoChannelSummary\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"avatars\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/ActorImage\"}},\"displayName\":{\"type\":\"string\"},\"host\":{\"type\":\"string\",\"format\":\"hostname\"},\"id\":{\"$ref\":\"#/components/schemas/id\"},\"name\":{\"type\":\"string\"},\"url\":{\"type\":\"string\",\"format\":\"url\"}},\"required\":[]},\"VideoChannelSync\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"channel\":{\"$ref\":\"#/components/schemas/VideoChannel\"},\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"externalChannelUrl\":{\"type\":\"string\",\"example\":\"https://youtube.com/c/UC_myfancychannel\"},\"id\":{\"$ref\":\"#/components/schemas/id\"},\"lastSyncAt\":{\"type\":\"string\",\"format\":\"date-time\",\"nullable\":true},\"state\":{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"integer\",\"example\":2},\"label\":{\"type\":\"string\",\"example\":\"PROCESSING\"}}}},\"required\":[]},\"VideoChannelSyncCreate\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"externalChannelUrl\":{\"type\":\"string\",\"example\":\"https://youtube.com/c/UC_myfancychannel\"},\"videoChannelId\":{\"$ref\":\"#/components/schemas/id\"}},\"required\":[]},\"VideoChannelSyncList\":{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"allOf\":[{\"$ref\":\"#/components/schemas/VideoChannelSync\"}]}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]},\"VideoChannelUpdate\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"allOf\":[{\"$ref\":\"#/components/schemas/VideoChannelEdit\"},{\"properties\":{\"bulkVideosSupportUpdate\":{\"type\":\"boolean\",\"description\":\"Update the support field for all videos of this channel\"}}}],\"properties\":{},\"required\":[]},\"VideoChapters\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"chapters\":{\"type\":\"object\",\"properties\":{\"title\":{\"type\":\"string\"},\"timecode\":{\"type\":\"integer\"}}}},\"required\":[]},\"VideoComment\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"account\":{\"$ref\":\"#/components/schemas/Account\"},\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"deletedAt\":{\"nullable\":true,\"type\":\"string\",\"format\":\"date-time\",\"default\":null},\"heldForReview\":{\"type\":\"boolean\"},\"id\":{\"$ref\":\"#/components/schemas/id\"},\"inReplyToCommentId\":{\"nullable\":true,\"allOf\":[{\"$ref\":\"#/components/schemas/id\"}]},\"isDeleted\":{\"type\":\"boolean\",\"default\":false},\"text\":{\"type\":\"string\",\"format\":\"html\",\"description\":\"Text of the comment\",\"minLength\":1,\"example\":\"This video is wonderful!\"},\"threadId\":{\"$ref\":\"#/components/schemas/id\"},\"totalReplies\":{\"type\":\"integer\",\"minimum\":0},\"totalRepliesFromVideoAuthor\":{\"type\":\"integer\",\"minimum\":0},\"updatedAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"url\":{\"type\":\"string\",\"format\":\"url\"},\"videoId\":{\"$ref\":\"#/components/schemas/Video/properties/id\"}},\"required\":[]},\"VideoCommentForOwnerOrAdmin\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"account\":{\"$ref\":\"#/components/schemas/VideoComment/properties/account\"},\"automaticTags\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}},\"createdAt\":{\"$ref\":\"#/components/schemas/VideoComment/properties/createdAt\"},\"heldForReview\":{\"$ref\":\"#/components/schemas/VideoComment/properties/heldForReview\"},\"id\":{\"$ref\":\"#/components/schemas/id\"},\"inReplyToCommentId\":{\"$ref\":\"#/components/schemas/VideoComment/properties/inReplyToCommentId\"},\"text\":{\"$ref\":\"#/components/schemas/VideoComment/properties/text\"},\"threadId\":{\"$ref\":\"#/components/schemas/VideoComment/properties/threadId\"},\"updatedAt\":{\"$ref\":\"#/components/schemas/VideoComment/properties/updatedAt\"},\"url\":{\"$ref\":\"#/components/schemas/VideoComment/properties/url\"},\"video\":{\"$ref\":\"#/components/schemas/VideoInfo\"}},\"required\":[]},\"VideoCommentThreadTree\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"children\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoCommentThreadTree\"}},\"comment\":{\"$ref\":\"#/components/schemas/VideoComment\"}},\"required\":[]},\"VideoCommentsForXML\":{\"xml\":{\"wrapped\":true,\"name\":\"channel\"},\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"object\",\"xml\":{\"name\":\"item\"},\"properties\":{\"link\":{\"type\":\"string\",\"format\":\"url\"},\"guid\":{\"type\":\"string\"},\"pubDate\":{\"type\":\"string\",\"format\":\"date-time\"},\"content:encoded\":{\"type\":\"string\"},\"dc:creator\":{\"type\":\"string\"}}}},\"VideoCommentsPolicyConstant\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/VideoCommentsPolicySet\"},\"label\":{\"type\":\"string\"}},\"required\":[]},\"VideoCommentsPolicySet\":{\"description\":\"Comments policy of the video (Enabled = `1`, Disabled = `2`, Requires Approval = `3`)\",\"type\":\"integer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[1,2,3],\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"VideoConstantNumber-Category\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/VideoCategorySet\"},\"label\":{\"type\":\"string\",\"example\":\"Science & Technology\"}},\"required\":[]},\"VideoConstantNumber-Licence\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/VideoLicenceSet\"},\"label\":{\"type\":\"string\",\"example\":\"Attribution - Share Alike\"}},\"required\":[]},\"VideoConstantString-Language\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/VideoLanguageSet\"},\"label\":{\"type\":\"string\",\"example\":\"English\"}},\"required\":[]},\"VideoCreateImport\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"allOf\":[{\"type\":\"object\",\"additionalProperties\":false,\"oneOf\":[{\"properties\":{\"targetUrl\":{\"$ref\":\"#/components/schemas/VideoImport/properties/targetUrl\"}},\"required\":[\"targetUrl\"]},{\"properties\":{\"magnetUri\":{\"$ref\":\"#/components/schemas/VideoImport/properties/magnetUri\"}},\"required\":[\"magnetUri\"]},{\"properties\":{\"torrentfile\":{\"$ref\":\"#/components/schemas/VideoImport/properties/torrentfile\"}},\"required\":[\"torrentfile\"]}]},{\"$ref\":\"#/components/schemas/VideoUploadRequestCommon\"}],\"properties\":{},\"required\":[\"channelId\",\"name\"]},\"VideoDetails\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"allOf\":[{\"$ref\":\"#/components/schemas/Video\"},{\"type\":\"object\",\"properties\":{\"viewers\":{\"type\":\"integer\",\"description\":\"If the video is a live, you have the amount of current viewers\"},\"description\":{\"type\":\"string\",\"nullable\":true,\"example\":\"\\\"**[Want to help to translate this video?](https://weblate.framasoft.org/projects/what-is-peertube-video/)**\\\\r\\\\n\\\\r\\\\n\\n**Take back the control of your videos! [#JoinPeertube](https://joinpeertube.org)**\\\\r\\\\n*A decentralized video hosting network,\\nbased on free/libre software!*\\\\r\\\\n\\\\r\\\\n**Animation Produced by:** [LILA](https://libreart.info) - [ZeMarmot Team](https://film.zemarmot.net)\\\\r\\\\n\\n*Directed by* Aryeom\\\\r\\\\n*Assistant* Jehan\\\\r\\\\n**Licence**: [CC-By-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/)\\\\r\\\\n\\\\r\\\\n\\n**Sponsored by** [Framasoft](https://framasoft.org)\\\\r\\\\n\\\\r\\\\n**Music**: [Red Step Forward](http://play.dogmazic.net/song.php?song_id=52491) - CC-By Ken Bushima\\\\r\\\\n\\\\r\\\\n\\n**Movie Clip**: [Caminades 3: Llamigos](http://www.caminandes.com/) CC-By Blender Institute\\\\r\\\\n\\\\r\\\\n**Video sources**: https://gitlab.gnome.org/Jehan/what-is-peertube/\\\"\\n\",\"minLength\":3,\"maxLength\":1000,\"description\":\"full description of the video, written in Markdown.\\n\"},\"support\":{\"type\":\"string\",\"nullable\":true,\"description\":\"A text tell the audience how to support the video creator\",\"example\":\"Please support our work on https://soutenir.framasoft.org/en/ <3\",\"minLength\":3,\"maxLength\":1000},\"channel\":{\"$ref\":\"#/components/schemas/VideoChannel\"},\"account\":{\"$ref\":\"#/components/schemas/Account\"},\"tags\":{\"example\":[\"flowers\",\"gardening\"],\"type\":\"array\",\"minItems\":1,\"maxItems\":5,\"items\":{\"type\":\"string\",\"minLength\":2,\"maxLength\":30}},\"commentsPolicy\":{\"$ref\":\"#/components/schemas/VideoCommentsPolicyConstant\"},\"downloadEnabled\":{\"type\":\"boolean\"},\"inputFileUpdatedAt\":{\"type\":\"string\",\"format\":\"date-time\",\"nullable\":true,\"description\":\"Latest input file update. Null if the file has never been replaced since the original upload\"},\"trackerUrls\":{\"type\":\"array\",\"items\":{\"type\":\"string\",\"format\":\"url\"},\"example\":[\"https://peertube2.cpy.re/tracker/announce\",\"wss://peertube2.cpy.re/tracker/socket\"]},\"files\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoFile\"},\"description\":\"Web compatible video files. If Web Video is disabled on the server:\\n\\n- field will be empty\\n- video files will be found in `streamingPlaylists[].files` field\\n\"},\"streamingPlaylists\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoStreamingPlaylists\"},\"description\":\"HLS playlists/manifest files. If HLS is disabled on the server:\\n\\n- field will be empty\\n- video files will be found in `files` field\\n\"}}}],\"properties\":{},\"required\":[]},\"VideoFile\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"fileDownloadUrl\":{\"type\":\"string\",\"description\":\"URL endpoint that transfers the video file as an attachment (so that the browser opens a download dialog)\",\"format\":\"url\"},\"fileUrl\":{\"type\":\"string\",\"description\":\"Direct URL of the video\",\"format\":\"url\"},\"fps\":{\"type\":\"number\",\"description\":\"Frames per second of the video file\"},\"hasAudio\":{\"type\":\"boolean\",\"description\":\"**PeerTube >= 6.2** The file container has an audio stream\"},\"hasVideo\":{\"type\":\"boolean\",\"description\":\"**PeerTube >= 6.2** The file container has a video stream\"},\"height\":{\"type\":\"number\",\"description\":\"**PeerTube >= 6.1** Video stream height\"},\"id\":{\"$ref\":\"#/components/schemas/id\"},\"magnetUri\":{\"type\":\"string\",\"format\":\"uri\",\"description\":\"magnet URI allowing to resolve the video via BitTorrent without a metainfo file\",\"pattern\":\"/magnet:\\\\?xt=urn:[a-z0-9]+:[a-z0-9]{32}/i\"},\"metadataUrl\":{\"type\":\"string\",\"format\":\"url\",\"description\":\"URL dereferencing the output of ffprobe on the file\"},\"playlistUrl\":{\"type\":\"string\",\"description\":\"Playlist URL of the file if it is owned by a playlist\",\"format\":\"url\"},\"resolution\":{\"$ref\":\"#/components/schemas/VideoResolutionConstant\"},\"size\":{\"type\":\"integer\",\"description\":\"Video file size in bytes\"},\"storage\":{\"$ref\":\"#/components/schemas/FileStorage\"},\"torrentDownloadUrl\":{\"type\":\"string\",\"description\":\"URL endpoint that transfers the torrent file as an attachment (so that the browser opens a download dialog)\",\"format\":\"url\"},\"torrentUrl\":{\"type\":\"string\",\"description\":\"Direct URL of the torrent file\",\"format\":\"url\"},\"width\":{\"type\":\"number\",\"description\":\"**PeerTube >= 6.1** Video stream width\"}},\"required\":[]},\"VideoImport\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"error\":{\"type\":\"string\"},\"id\":{\"$ref\":\"#/components/schemas/id\"},\"magnetUri\":{\"type\":\"string\",\"format\":\"uri\",\"description\":\"magnet URI allowing to resolve the import's source video\",\"pattern\":\"/magnet:\\\\?xt=urn:[a-z0-9]+:[a-z0-9]{32}/i\"},\"state\":{\"allOf\":[{\"$ref\":\"#/components/schemas/VideoImportStateConstant\"}]},\"targetUrl\":{\"type\":\"string\",\"format\":\"url\",\"description\":\"remote URL where to find the import's source video\",\"example\":\"https://framatube.org/videos/watch/9c9de5e8-0a1e-484a-b099-e80766180a6d\"},\"torrentName\":{\"type\":\"string\"},\"torrentfile\":{\"type\":\"string\",\"format\":\"binary\",\"description\":\"Torrent file containing only the video file\"},\"updatedAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"video\":{\"nullable\":true,\"allOf\":[{\"$ref\":\"#/components/schemas/Video\"}]}},\"required\":[]},\"VideoImportStateConstant\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"id\":{\"type\":\"integer\",\"enum\":[1,2,3],\"description\":\"The video import state (Pending = `1`, Success = `2`, Failed = `3`)\"},\"label\":{\"type\":\"string\",\"example\":\"Pending\"}},\"required\":[]},\"VideoImportsList\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"maxItems\":100,\"items\":{\"$ref\":\"#/components/schemas/VideoImport\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]},\"VideoInfo\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/Video/properties/id\"},\"name\":{\"$ref\":\"#/components/schemas/Video/properties/name\"},\"state\":{\"$ref\":\"#/components/schemas/Video/properties/state\"},\"uuid\":{\"$ref\":\"#/components/schemas/Video/properties/uuid\"}},\"required\":[]},\"VideoLanguageSet\":{\"description\":\"language id of the video (see [/videos/languages](#operation/getLanguages))\",\"type\":\"string\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"example\":\"en\"},\"VideoLicenceSet\":{\"description\":\"licence id of the video (see [/videos/licences](#operation/getLicences))\",\"type\":\"integer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"example\":2},\"VideoListResponse\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"maxItems\":100,\"items\":{\"$ref\":\"#/components/schemas/Video\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]},\"VideoPassword\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/id\"},\"password\":{\"type\":\"string\",\"minLength\":2},\"videoId\":{\"$ref\":\"#/components/schemas/id\"}},\"required\":[]},\"VideoPasswordList\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoPassword\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]},\"VideoPlaylist\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"description\":{\"type\":\"string\",\"minLength\":3,\"maxLength\":1000},\"displayName\":{\"type\":\"string\",\"minLength\":1,\"maxLength\":120},\"id\":{\"$ref\":\"#/components/schemas/id\"},\"isLocal\":{\"type\":\"boolean\"},\"ownerAccount\":{\"$ref\":\"#/components/schemas/AccountSummary\"},\"privacy\":{\"$ref\":\"#/components/schemas/VideoPlaylistPrivacyConstant\"},\"shortUUID\":{\"allOf\":[{\"$ref\":\"#/components/schemas/shortUUID\"}]},\"thumbnailPath\":{\"type\":\"string\"},\"type\":{\"$ref\":\"#/components/schemas/VideoPlaylistTypeConstant\"},\"updatedAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"uuid\":{\"$ref\":\"#/components/schemas/UUIDv4\"},\"videoChannel\":{\"$ref\":\"#/components/schemas/VideoChannelSummary\"},\"videoChannelPosition\":{\"type\":\"integer\",\"minimum\":1,\"description\":\"Position of the playlist in the channel\"},\"videoLength\":{\"type\":\"integer\",\"minimum\":0}},\"required\":[]},\"VideoPlaylistPrivacyConstant\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/VideoPlaylistPrivacySet\"},\"label\":{\"type\":\"string\"}},\"required\":[]},\"VideoPlaylistPrivacySet\":{\"description\":\"Video playlist privacy policy (see [/video-playlists/privacies](#operation/getPlaylistPrivacyPolicies))\",\"type\":\"integer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[1,2,3],\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"VideoPlaylistTypeConstant\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/VideoPlaylistTypeSet\"},\"label\":{\"type\":\"string\"}},\"required\":[]},\"VideoPlaylistTypeSet\":{\"description\":\"The video playlist type (Regular = `1`, Watch Later = `2`)\",\"type\":\"integer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[1,2],\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"VideoPrivacyConstant\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/VideoPrivacySet\"},\"label\":{\"type\":\"string\"}},\"required\":[]},\"VideoPrivacySet\":{\"description\":\"privacy id of the video (see [/videos/privacies](#operation/getVideoPrivacyPolicies))\",\"type\":\"integer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[1,2,3,4,5],\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"VideoRating\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"rating\":{\"type\":\"string\",\"enum\":[\"like\",\"dislike\",\"none\"],\"description\":\"Rating of the video\"},\"video\":{\"$ref\":\"#/components/schemas/Video\"}},\"required\":[\"video\",\"rating\"]},\"VideoRedundancy\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/id\"},\"name\":{\"type\":\"string\"},\"redundancies\":{\"type\":\"object\",\"properties\":{\"streamingPlaylists\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/FileRedundancyInformation\"}}}},\"url\":{\"type\":\"string\",\"format\":\"url\"},\"uuid\":{\"$ref\":\"#/components/schemas/UUIDv4\"}},\"required\":[]},\"VideoReplaceSourceRequestResumable\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"filename\":{\"description\":\"Video filename including extension\",\"type\":\"string\",\"format\":\"filename\",\"example\":\"what_is_peertube.mp4\"}},\"required\":[]},\"VideoResolutionConstant\":{\"description\":\"resolutions and their labels for the video\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/VideoResolutionSet\"},\"label\":{\"type\":\"string\",\"example\":\"240p\"}},\"required\":[]},\"VideoResolutionSet\":{\"description\":\"Video resolution (`0`, `240`, `360`, `720`, `1080`, `1440` or `2160`)\\n\\n`0` is used as a special value for stillimage videos dedicated to audio, a.k.a. audio-only videos.\\n\",\"type\":\"integer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"example\":240},\"VideoScheduledUpdate\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"privacy\":{\"$ref\":\"#/components/schemas/VideoPrivacySet\"},\"updateAt\":{\"type\":\"string\",\"format\":\"date-time\",\"description\":\"When to update the video\"}},\"required\":[\"updateAt\"]},\"VideoSource\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"fileDownloadUrl\":{\"type\":\"string\",\"description\":\"**PeerTube >= 6.1** If enabled by the admin, the video source file is kept on the server and can be downloaded by the owner\"},\"fps\":{\"type\":\"number\",\"description\":\"**PeerTube >= 6.1** Frames per second of the video file\"},\"height\":{\"type\":\"integer\",\"description\":\"**PeerTube >= 6.1** Video stream height\"},\"inputFilename\":{\"type\":\"string\",\"description\":\"Uploaded/imported filename\"},\"resolution\":{\"description\":\"**PeerTube >= 6.1**\",\"allOf\":[{\"$ref\":\"#/components/schemas/VideoResolutionConstant\"}]},\"size\":{\"type\":\"integer\",\"description\":\"**PeerTube >= 6.1** Video file size in bytes\"},\"width\":{\"type\":\"integer\",\"description\":\"**PeerTube >= 6.1** Video stream width\"}},\"required\":[]},\"VideoStateConstant\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"id\":{\"type\":\"integer\",\"enum\":[1,2,3,4,5,6,7,8,9],\"description\":\"The video state:\\n- `1`: Published\\n- `2`: To transcode\\n- `3`: To import\\n- `4`: Waiting for live stream\\n- `5`: Live ended\\n- `6`: To move to an external storage (object storage...)\\n- `7`: Transcoding failed\\n- `8`: Moving to an external storage failed\\n- `9`: To edit using studio edition feature\\n\"},\"label\":{\"type\":\"string\"}},\"required\":[]},\"VideoStatsOverall\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"averageWatchTime\":{\"type\":\"number\"},\"countries\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"isoCode\":{\"type\":\"string\"},\"viewers\":{\"type\":\"number\"}}}},\"subdivisions\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\"},\"viewers\":{\"type\":\"number\"}}}},\"totalViewers\":{\"type\":\"number\"},\"totalWatchTime\":{\"type\":\"number\"},\"viewersPeak\":{\"type\":\"number\"},\"viewersPeakDate\":{\"type\":\"string\",\"format\":\"date-time\"}},\"required\":[]},\"VideoStatsRetention\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"second\":{\"type\":\"number\"},\"retentionPercent\":{\"type\":\"number\"}}}}},\"required\":[]},\"VideoStatsTimeserie\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"date\":{\"type\":\"string\"},\"value\":{\"type\":\"number\"}}}}},\"required\":[]},\"VideoStatsUserAgent\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"clients\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\"},\"viewers\":{\"type\":\"number\"}}}},\"devices\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"name\":{\"$ref\":\"#/components/schemas/VideoStatsUserAgentDevice\"},\"viewers\":{\"type\":\"number\"}}}},\"operatingSystem\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\"},\"viewers\":{\"type\":\"number\"}}}}},\"required\":[]},\"VideoStatsUserAgentDevice\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"enum\":[\"console\",\"embedded\",\"mobile\",\"smarttv\",\"tablet\",\"wearable\",\"xr\",\"desktop\"],\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"VideoStreamingPlaylists\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"allOf\":[{\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/id\"},\"type\":{\"type\":\"integer\",\"enum\":[1],\"description\":\"Playlist type:\\n- `1`: HLS\\n\"}}},{\"$ref\":\"#/components/schemas/VideoStreamingPlaylists-HLS\"}],\"properties\":{},\"required\":[]},\"VideoStreamingPlaylists-HLS\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"files\":{\"type\":\"array\",\"description\":\"Video files associated to this playlist.\\n\\nThe difference with the root `files` property is that these files are fragmented, so they can be used in this streaming playlist (HLS, etc.)\\n\",\"items\":{\"$ref\":\"#/components/schemas/VideoFile\"}},\"playlistUrl\":{\"type\":\"string\",\"format\":\"url\"},\"redundancies\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"baseUrl\":{\"type\":\"string\",\"format\":\"url\"}}}},\"segmentsSha256Url\":{\"type\":\"string\",\"format\":\"url\"}},\"required\":[]},\"VideoStudioCreateTask\":{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"anyOf\":[{\"title\":\"cut\",\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\",\"enum\":[\"cut\"]},\"options\":{\"type\":\"object\",\"properties\":{\"start\":{\"type\":\"integer\"},\"end\":{\"type\":\"integer\"}}}}},{\"title\":\"add-intro\",\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\",\"enum\":[\"add-intro\"]},\"options\":{\"type\":\"object\",\"properties\":{\"file\":{\"type\":\"string\",\"format\":\"binary\"}}}}},{\"title\":\"add-outro\",\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\",\"enum\":[\"add-outro\"]},\"options\":{\"type\":\"object\",\"properties\":{\"file\":{\"type\":\"string\",\"format\":\"binary\"}}}}},{\"title\":\"add-watermark\",\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\",\"enum\":[\"add-watermark\"]},\"options\":{\"type\":\"object\",\"properties\":{\"file\":{\"type\":\"string\",\"format\":\"binary\"}}}}}]}},\"VideoTokenResponse\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"files\":{\"type\":\"object\",\"properties\":{\"token\":{\"type\":\"string\"},\"expires\":{\"type\":\"string\",\"format\":\"date-time\"}}}},\"required\":[]},\"VideoUploadRequestCommon\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"category\":{\"$ref\":\"#/components/schemas/VideoCategorySet\"},\"channelId\":{\"description\":\"Channel id that will contain this video\",\"type\":\"integer\",\"example\":3,\"minimum\":1},\"commentsPolicy\":{\"$ref\":\"#/components/schemas/VideoCommentsPolicySet\"},\"description\":{\"description\":\"Video description\",\"type\":\"string\",\"example\":\"**[Want to help to translate this video?](https://weblate.framasoft.org/projects/what-is-peertube-video/)**\\\\r\\\\n\\\\r\\\\n**Take back the control of your videos! [#JoinPeertube](https://joinpeertube.org)**\\n\"},\"downloadEnabled\":{\"description\":\"Enable or disable downloading for this video\",\"type\":\"boolean\"},\"generateTranscription\":{\"description\":\"**PeerTube >= 6.2** If enabled by the admin, automatically generate a subtitle of the video\",\"type\":\"boolean\"},\"language\":{\"$ref\":\"#/components/schemas/VideoLanguageSet\"},\"licence\":{\"$ref\":\"#/components/schemas/VideoLicenceSet\"},\"name\":{\"description\":\"Video name\",\"type\":\"string\",\"example\":\"What is PeerTube?\",\"minLength\":3,\"maxLength\":120},\"nsfw\":{\"description\":\"Whether or not this video contains sensitive content\",\"type\":\"boolean\"},\"nsfwFlags\":{\"$ref\":\"#/components/schemas/NSFWFlag\"},\"nsfwSummary\":{\"description\":\"More information about the sensitive content of the video\"},\"originallyPublishedAt\":{\"description\":\"Date when the content was originally published\",\"type\":\"string\",\"format\":\"date-time\"},\"previewfile\":{\"description\":\"Video preview file\",\"type\":\"string\",\"format\":\"binary\"},\"privacy\":{\"$ref\":\"#/components/schemas/VideoPrivacySet\"},\"scheduleUpdate\":{\"$ref\":\"#/components/schemas/VideoScheduledUpdate\"},\"support\":{\"description\":\"A text tell the audience how to support the video creator\",\"example\":\"Please support our work on https://soutenir.framasoft.org/en/ <3\",\"type\":\"string\"},\"tags\":{\"description\":\"Video tags (maximum 5 tags each between 2 and 30 characters)\",\"type\":\"array\",\"minItems\":1,\"maxItems\":5,\"uniqueItems\":true,\"example\":[\"framasoft\",\"peertube\"],\"items\":{\"type\":\"string\",\"minLength\":2,\"maxLength\":30}},\"thumbnailfile\":{\"description\":\"Video thumbnail file\",\"type\":\"string\",\"format\":\"binary\"},\"videoPasswords\":{\"$ref\":\"#/components/schemas/AddVideoPasswords\"},\"waitTranscoding\":{\"description\":\"Whether or not we wait transcoding before publish the video\",\"type\":\"boolean\"}},\"required\":[\"channelId\",\"name\"]},\"VideoUploadRequestLegacy\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"allOf\":[{\"$ref\":\"#/components/schemas/VideoUploadRequestCommon\"},{\"type\":\"object\",\"required\":[\"videofile\"],\"properties\":{\"videofile\":{\"description\":\"Video file\",\"type\":\"string\",\"format\":\"binary\"}}}],\"properties\":{},\"required\":[]},\"VideoUploadRequestResumable\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"allOf\":[{\"$ref\":\"#/components/schemas/VideoUploadRequestCommon\"},{\"type\":\"object\",\"required\":[\"filename\"],\"properties\":{\"filename\":{\"description\":\"Video filename including extension\",\"type\":\"string\",\"format\":\"filename\",\"example\":\"what_is_peertube.mp4\"},\"thumbnailfile\":{\"description\":\"Video thumbnail file\",\"type\":\"string\",\"format\":\"binary\"},\"previewfile\":{\"description\":\"Video preview file\",\"type\":\"string\",\"format\":\"binary\"}}}],\"properties\":{},\"required\":[]},\"VideoUploadResponse\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"video\":{\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/Video/properties/id\"},\"uuid\":{\"$ref\":\"#/components/schemas/Video/properties/uuid\"},\"shortUUID\":{\"$ref\":\"#/components/schemas/Video/properties/shortUUID\"}}}},\"required\":[]},\"VideosForXML\":{\"xml\":{\"wrapped\":true,\"name\":\"channel\"},\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"object\",\"xml\":{\"name\":\"item\"},\"properties\":{\"link\":{\"type\":\"string\",\"format\":\"url\",\"description\":\"video watch page URL\"},\"guid\":{\"type\":\"string\",\"description\":\"video canonical URL\"},\"pubDate\":{\"type\":\"string\",\"format\":\"date-time\",\"description\":\"video publication date\"},\"description\":{\"type\":\"string\",\"description\":\"video description\"},\"content:encoded\":{\"type\":\"string\",\"description\":\"video description\"},\"dc:creator\":{\"type\":\"string\",\"description\":\"publisher user name\"},\"media:category\":{\"type\":\"integer\",\"description\":\"video category (MRSS)\"},\"media:community\":{\"type\":\"object\",\"description\":\"see [media:community](https://www.rssboard.org/media-rss#media-community) (MRSS)\",\"properties\":{\"media:statistics\":{\"type\":\"object\",\"properties\":{\"views\":{\"type\":\"integer\",\"xml\":{\"attribute\":true}}}}}},\"media:embed\":{\"type\":\"object\",\"properties\":{\"url\":{\"type\":\"string\",\"format\":\"url\",\"description\":\"video embed path, relative to the canonical URL domain (MRSS)\",\"xml\":{\"attribute\":true}}}},\"media:player\":{\"type\":\"object\",\"properties\":{\"url\":{\"type\":\"string\",\"format\":\"url\",\"description\":\"video watch path, relative to the canonical URL domain (MRSS)\",\"xml\":{\"attribute\":true}}}},\"media:thumbnail\":{\"type\":\"object\",\"properties\":{\"url\":{\"type\":\"string\",\"format\":\"url\",\"xml\":{\"attribute\":true}},\"height\":{\"type\":\"integer\",\"xml\":{\"attribute\":true}},\"width\":{\"type\":\"integer\",\"xml\":{\"attribute\":true}}}},\"media:title\":{\"type\":\"string\",\"description\":\"see [media:title](https://www.rssboard.org/media-rss#media-title) (MRSS). We only use `plain` titles.\"},\"media:description\":{\"type\":\"string\"},\"media:rating\":{\"type\":\"string\",\"enum\":[\"nonadult\",\"adult\"],\"description\":\"see [media:rating](https://www.rssboard.org/media-rss#media-rating) (MRSS)\"},\"enclosure\":{\"type\":\"object\",\"description\":\"main streamable file for the video\",\"properties\":{\"url\":{\"type\":\"string\",\"format\":\"url\",\"xml\":{\"attribute\":true}},\"type\":{\"type\":\"string\",\"enum\":[\"application/x-bittorrent\"],\"xml\":{\"attribute\":true}},\"length\":{\"type\":\"integer\",\"xml\":{\"attribute\":true}}}},\"media:group\":{\"type\":\"array\",\"description\":\"list of streamable files for the video. see [media:peerLink](https://www.rssboard.org/media-rss#media-peerlink) and [media:content](https://www.rssboard.org/media-rss#media-content) or  (MRSS)\",\"items\":{\"anyOf\":[{\"$ref\":\"#/components/schemas/MRSSPeerLink\"},{\"$ref\":\"#/components/schemas/MRSSGroupContent\"}]}}}}},\"WatchedWordsLists\":{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\",\"example\":\"2021-05-04T08:01:01.502Z\"},\"id\":{\"$ref\":\"#/components/schemas/id\"},\"listName\":{\"type\":\"string\"},\"updatedAt\":{\"type\":\"string\",\"format\":\"date-time\",\"example\":\"2021-05-04T08:01:01.502Z\"},\"words\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}}},\"required\":[]},\"id\":{\"type\":\"integer\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"minimum\":1,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"example\":42},\"password\":{\"type\":\"string\",\"format\":\"password\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"minLength\":6,\"maxLength\":50,\"uniqueItems\":false,\"properties\":{},\"required\":[]},\"shortUUID\":{\"description\":\"translation of a uuid v4 with a bigger alphabet to have a shorter uuid\",\"type\":\"string\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"example\":\"2y84q2MQUMWPbiEcxNXMgC\"},\"username\":{\"description\":\"immutable name of the user, used to find or mention its actor\",\"type\":\"string\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"minLength\":1,\"maxLength\":50,\"pattern\":\"/^[a-z0-9._]+$/\",\"uniqueItems\":false,\"properties\":{},\"required\":[],\"example\":\"chocobozzz\"},\"usernameChannel\":{\"description\":\"immutable name of the channel, used to interact with its actor\",\"type\":\"string\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"minLength\":1,\"maxLength\":50,\"pattern\":\"/^[a-zA-Z0-9\\\\\\\\-_.:]+$/\",\"uniqueItems\":false,\"properties\":{},\"required\":[],\"example\":\"framasoft_videos\"}}"

type t = Openapi.Runtime.Client.t

let of_fetch ?max_response_bytes ~base_url session =
  Openapi.Runtime.Client.of_fetch ?max_response_bytes ~base_url session

let create ?session ?max_response_bytes ~sw env ~base_url =
  let session = match session with
    | Some s -> Fetch.restrict s
    | None -> Fetch_curl.std ~sw env
  in
  of_fetch ?max_response_bytes ~base_url session

let base_url = Openapi.Runtime.Client.base_url
let session = Openapi.Runtime.Client.session

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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoUploadResponse" jsont
  end

  (** Import a video

      Import a torrent or magnetURI or HTTP resource (if enabled by the instance administrator) *)
  let import_video ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/videos/imports" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.Form.multipart body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoUploadResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoUploadResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Response.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None)); ("403", (fun _ -> None)); ("409", (fun _ -> None))]
      ~operation:"import_video" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Create a live *)
  let add_live ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/videos/live" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.Form.multipart body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoUploadResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoUploadResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Response.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None)); ("403", (fun _ -> None))]
      ~operation:"add_live" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Upload a video

      Uses a single request to upload a video. *)
  let upload_legacy ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/videos/upload" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.Form.multipart body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoUploadResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoUploadResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Response.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("403", (fun _ -> None)); ("408", (fun _ -> None)); ("413", (fun _ -> None)); ("415", (fun _ -> None)); ("422", (fun _ -> None))]
      ~operation:"upload_legacy" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Send chunk for the resumable upload of a video

      Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to continue, pause or resume the upload of a video
      @param upload_id Created session id to proceed with. If you didn't send chunks in the last hour, it is
  not valid anymore and you need to initialize a new upload.

      @param content_range Specifies the bytes in the file that the request is uploading.

  For example, a value of `bytes 0-262143/1000000` shows that the request is sending the first
  262144 bytes (256 x 1024) in a 2,469,036 byte file.

      @param content_length Size of the chunk that the request is sending.

  Remember that larger chunks are more efficient. PeerTube's web client uses chunks varying from
  1048576 bytes (~1MB) and increases or reduces size depending on connection health.

  *)
  let upload_resumable ~upload_id ~content_range ~content_length ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/videos/upload-resumable" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.singleton ~key:"upload_id" ~value:upload_id]) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.Header.[ content_type, media "application/octet-stream" ], body in headers, Some body
    in
    let __openapi_headers = let cell = Fetch.Header.raw "Content-Range" content_range in Fetch.Header.(cell :: __openapi_headers) in
    let __openapi_headers = let cell = Fetch.Header.raw "Content-Length" content_length in Fetch.Header.(cell :: __openapi_headers) in
    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoUploadResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoUploadResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Response.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("308", (fun _ -> None)); ("403", (fun _ -> None)); ("404", (fun _ -> None)); ("409", (fun _ -> None)); ("422", (fun _ -> None)); ("429", (fun _ -> None)); ("503", (fun _ -> None))]
      ~operation:"upload_resumable" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoTokenResponse" jsont
  end

  (** Request video token

      Request special tokens that expire quickly to use them in some context (like accessing private static files)
      @param id The object id, uuid or short uuid
      @param x_peertube_video_password Required on password protected video
  *)
  let request_video_token ~id ?x_peertube_video_password client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/token" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in
    let __openapi_headers = match x_peertube_video_password with None -> __openapi_headers | Some value -> let cell = Fetch.Header.raw "x-peertube-video-password" value in Fetch.Header.(cell :: __openapi_headers) in
    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoTokenResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoTokenResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Response.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None)); ("404", (fun _ -> None))]
      ~operation:"request_video_token" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST
end

module VideoStudioCreateTask = struct
  module Types = struct
    module T = struct
      type t = Jsont.json list
    end
  end

  module T = struct
    include Types.T
    let jsont = (Jsont.list Jsont.json)
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoStudioCreateTask" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoStatsUserAgentDevice" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoStatsUserAgent" jsont
  end

  (** Get user agent stats of a video
      @param id The object id, uuid or short uuid
      @param start_date Filter stats by start date
      @param end_date Filter stats by end date
  *)
  let get_api_v1_videos_by_id_stats_user_agent ~id ?start_date ?end_date client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/stats/user-agent" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"startDate" ~value:start_date; Openapi.Runtime.Query.optional ~key:"endDate" ~value:end_date]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoStatsUserAgent\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoStatsUserAgent\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_videos_by_id_stats_user_agent" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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
      |> Jsont.Object.mem "currentTime" Openapi.Runtime.int_jsont ~enc:(fun r -> r.current_time)
      |> Jsont.Object.opt_mem "device" VideoStatsUserAgentDevice.T.jsont ~enc:(fun r -> r.device)
      |> Jsont.Object.opt_mem "operatingSystem" Jsont.string ~enc:(fun r -> r.operating_system)
      |> Jsont.Object.opt_mem "sessionId" Jsont.string ~enc:(fun r -> r.session_id)
      |> Jsont.Object.opt_mem "viewEvent" Jsont.string ~enc:(fun r -> r.view_event)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "UserViewingVideo" jsont
  end
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoStatsTimeserie" jsont
  end

  (** Get timeserie stats of a video
      @param id The object id, uuid or short uuid
      @param metric The metric to get
      @param start_date Filter stats by start date
      @param end_date Filter stats by end date
  *)
  let get_api_v1_videos_by_id_stats_timeseries_by_metric ~id ~metric ?start_date ?end_date client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id); ("metric", metric)] "/api/v1/videos/{id}/stats/timeseries/{metric}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"startDate" ~value:start_date; Openapi.Runtime.Query.optional ~key:"endDate" ~value:end_date]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoStatsTimeserie\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoStatsTimeserie\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_videos_by_id_stats_timeseries_by_metric" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoStatsRetention" jsont
  end

  (** Get retention stats of a video
      @param id The object id, uuid or short uuid
  *)
  let get_api_v1_videos_by_id_stats_retention ~id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/stats/retention" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoStatsRetention\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoStatsRetention\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_videos_by_id_stats_retention" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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
      |> Jsont.Object.opt_mem "averageWatchTime" Openapi.Runtime.number_jsont ~enc:(fun r -> r.average_watch_time)
      |> Jsont.Object.opt_mem "countries" (Jsont.list Jsont.json) ~enc:(fun r -> r.countries)
      |> Jsont.Object.opt_mem "subdivisions" (Jsont.list Jsont.json) ~enc:(fun r -> r.subdivisions)
      |> Jsont.Object.opt_mem "totalViewers" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_viewers)
      |> Jsont.Object.opt_mem "totalWatchTime" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_watch_time)
      |> Jsont.Object.opt_mem "viewersPeak" Openapi.Runtime.number_jsont ~enc:(fun r -> r.viewers_peak)
      |> Jsont.Object.opt_mem "viewersPeakDate" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.viewers_peak_date)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoStatsOverall" jsont
  end

  (** Get overall stats of a video
      @param id The object id, uuid or short uuid
      @param start_date Filter stats by start date
      @param end_date Filter stats by end date
  *)
  let get_api_v1_videos_by_id_stats_overall ~id ?start_date ?end_date client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/stats/overall" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"startDate" ~value:start_date; Openapi.Runtime.Query.optional ~key:"endDate" ~value:end_date]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoStatsOverall\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoStatsOverall\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_videos_by_id_stats_overall" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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
      |> Jsont.Object.opt_mem "id" Openapi.Runtime.int_jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "label" Jsont.string ~enc:(fun r -> r.label)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoStateConstant" jsont
  end
end

module VideoResolutionSet = struct
  module Types = struct
    module T = struct
      (** Video resolution (`0`, `240`, `360`, `720`, `1080`, `1440` or `2160`)

      `0` is used as a special value for stillimage videos dedicated to audio, a.k.a. audio-only videos.
       *)
      type t = int
    end
  end

  module T = struct
    include Types.T
    let jsont = Openapi.Runtime.int_jsont
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoResolutionSet" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoResolutionConstant" jsont
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
      |> Jsont.Object.opt_mem "fps" Openapi.Runtime.number_jsont ~enc:(fun r -> r.fps)
      |> Jsont.Object.opt_mem "height" Openapi.Runtime.int_jsont ~enc:(fun r -> r.height)
      |> Jsont.Object.opt_mem "inputFilename" Jsont.string ~enc:(fun r -> r.input_filename)
      |> Jsont.Object.opt_mem "resolution" VideoResolutionConstant.T.jsont ~enc:(fun r -> r.resolution)
      |> Jsont.Object.opt_mem "size" Openapi.Runtime.int_jsont ~enc:(fun r -> r.size)
      |> Jsont.Object.opt_mem "width" Openapi.Runtime.int_jsont ~enc:(fun r -> r.width)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoSource" jsont
  end

  (** Get video source file metadata

      Get metadata and download link of original video file
      @param id The object id, uuid or short uuid
  *)
  let get_video_source ~id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/source" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoSource\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoSource\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_video_source" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoReplaceSourceRequestResumable" jsont
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
    let jsont = Openapi.Runtime.int_jsont
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoPrivacySet" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoScheduledUpdate" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoPrivacyConstant" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "LiveVideoReplaySettings" jsont
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
    let jsont = Openapi.Runtime.int_jsont
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoPlaylistTypeSet" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoPlaylistTypeConstant" jsont
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
    let jsont = Openapi.Runtime.int_jsont
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoPlaylistPrivacySet" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoPlaylistPrivacyConstant" jsont
  end
end

module VideoLicenceSet = struct
  module Types = struct
    module T = struct
      (** licence id of the video (see [/videos/licences](#operation/getLicences)) *)
      type t = int
    end
  end

  module T = struct
    include Types.T
    let jsont = Openapi.Runtime.int_jsont
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoLicenceSet" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoConstantNumber-Licence" jsont
  end
end

module VideoLanguageSet = struct
  module Types = struct
    module T = struct
      (** language id of the video (see [/videos/languages](#operation/getLanguages)) *)
      type t = string
    end
  end

  module T = struct
    include Types.T
    let jsont = Jsont.string
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoLanguageSet" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoConstantString-Language" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoCaption" jsont
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
      |> Jsont.Object.opt_mem "id" Openapi.Runtime.int_jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "label" Jsont.string ~enc:(fun r -> r.label)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoImportStateConstant" jsont
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
    let jsont = Openapi.Runtime.int_jsont
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoCommentsPolicySet" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoCommentsPolicyConstant" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "ServerConfigCustom" jsont
  end

  (** Get instance runtime configuration *)
  let get_custom_config client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/config/custom" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/ServerConfigCustom\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/ServerConfigCustom\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_custom_config" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
end

module VideoCommentsForXml = struct
  module Types = struct
    module T = struct
      type t = Jsont.json list
    end
  end

  module T = struct
    include Types.T
    let jsont = (Jsont.list Jsont.json)
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoCommentsForXML" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoChapters" jsont
  end

  (** Get chapters of a video

      **PeerTube >= 6.0**
      @param id The object id, uuid or short uuid
      @param x_peertube_video_password Required on password protected video
  *)
  let get_video_chapters ~id ?x_peertube_video_password client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/chapters" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in
    let __openapi_headers = match x_peertube_video_password with None -> __openapi_headers | Some value -> let cell = Fetch.Header.raw "x-peertube-video-password" value in Fetch.Header.(cell :: __openapi_headers) in
    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoChapters\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoChapters\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_video_chapters" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoChannelEdit" jsont
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
    let jsont = Openapi.Runtime.int_jsont
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoChannelCollaboratorState" jsont
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
    let jsont = Openapi.Runtime.int_jsont
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoChannelActivityTarget" jsont
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
    let jsont = Openapi.Runtime.int_jsont
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoChannelActivityAction" jsont
  end
end

module VideoCategorySet = struct
  module Types = struct
    module T = struct
      (** category id of the video (see [/videos/categories](#operation/getCategories)) *)
      type t = int
    end
  end

  module T = struct
    include Types.T
    let jsont = Openapi.Runtime.int_jsont
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoCategorySet" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoConstantNumber-Category" jsont
  end
end

module Uuidv4 = struct
  module Types = struct
    module T = struct
      type t = string
    end
  end

  module T = struct
    include Types.T
    let jsont = Jsont.string
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "UUIDv4" jsont
  end
end

module UsernameChannel = struct
  module Types = struct
    module T = struct
      (** immutable name of the channel, used to interact with its actor *)
      type t = string
    end
  end

  module T = struct
    include Types.T
    let jsont = Jsont.string
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "usernameChannel" jsont
  end
end

module Username = struct
  module Types = struct
    module T = struct
      (** immutable name of the user, used to find or mention its actor *)
      type t = string
    end
  end

  module T = struct
    include Types.T
    let jsont = Jsont.string
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "username" jsont
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
    let jsont = Openapi.Runtime.int_jsont
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "UserRole" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "UserRegistrationAcceptOrReject" jsont
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
    let jsont = Openapi.Runtime.int_jsont
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "UserImportState" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "UserImportResumable" jsont
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
    let jsont = Openapi.Runtime.int_jsont
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "UserExportState" jsont
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
    let jsont = Openapi.Runtime.int_jsont
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "UserAdminFlags" jsont
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
      |> Jsont.Object.opt_mem "id" Openapi.Runtime.int_jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "lastActivityDate" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.last_activity_date)
      |> Jsont.Object.opt_mem "lastActivityDevice" Jsont.string ~enc:(fun r -> r.last_activity_device)
      |> Jsont.Object.opt_mem "lastActivityIP" Jsont.string ~enc:(fun r -> r.last_activity_ip)
      |> Jsont.Object.opt_mem "loginDate" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.login_date)
      |> Jsont.Object.opt_mem "loginDevice" Jsont.string ~enc:(fun r -> r.login_device)
      |> Jsont.Object.opt_mem "loginIP" Jsont.string ~enc:(fun r -> r.login_ip)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "TokenSession" jsont
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
      |> Jsont.Object.opt_mem "spriteDuration" Openapi.Runtime.int_jsont ~enc:(fun r -> r.sprite_duration)
      |> Jsont.Object.opt_mem "spriteHeight" Openapi.Runtime.int_jsont ~enc:(fun r -> r.sprite_height)
      |> Jsont.Object.opt_mem "spriteWidth" Openapi.Runtime.int_jsont ~enc:(fun r -> r.sprite_width)
      |> Jsont.Object.opt_mem "storyboardPath" Jsont.string ~enc:(fun r -> r.storyboard_path)
      |> Jsont.Object.opt_mem "totalHeight" Openapi.Runtime.int_jsont ~enc:(fun r -> r.total_height)
      |> Jsont.Object.opt_mem "totalWidth" Openapi.Runtime.int_jsont ~enc:(fun r -> r.total_width)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "Storyboard" jsont
  end
end

module ShortUuid = struct
  module Types = struct
    module T = struct
      (** translation of a uuid v4 with a bigger alphabet to have a shorter uuid *)
      type t = string
    end
  end

  module T = struct
    include Types.T
    let jsont = Jsont.string
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "shortUUID" jsont
  end
end

module LiveVideoSession = struct
  module Types = struct
    module Response = struct
      type t = {
        end_date : Ptime.t option option;  (** End date of the live session *)
        error : int option option;  (** Error type if an error occurred during the live session:
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
      |> Jsont.Object.opt_mem "endDate" (Jsont.option Openapi.Runtime.ptime_jsont) ~enc:(fun r -> r.end_date)
      |> Jsont.Object.opt_mem "error" (Jsont.option Openapi.Runtime.int_jsont) ~enc:(fun r -> r.error)
      |> Jsont.Object.opt_mem "id" Openapi.Runtime.int_jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "replayVideo" Jsont.json ~enc:(fun r -> r.replay_video)
      |> Jsont.Object.opt_mem "startDate" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.start_date)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "LiveVideoSessionResponse" jsont
  end

  (** Get live session of a replay

      If the video is a replay of a live, you can find the associated live session using this endpoint
      @param id The object id, uuid or short uuid
      @param x_peertube_video_password Required on password protected video
  *)
  let get_api_v1_videos_by_id_live_session ~id ?x_peertube_video_password client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/live-session" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in
    let __openapi_headers = match x_peertube_video_password with None -> __openapi_headers | Some value -> let cell = Fetch.Header.raw "x-peertube-video-password" value in Fetch.Header.(cell :: __openapi_headers) in
    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/LiveVideoSessionResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/LiveVideoSessionResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Response.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_videos_by_id_live_session" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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
      |> Jsont.Object.opt_mem "activityPubMessagesProcessedPerSecond" Openapi.Runtime.number_jsont ~enc:(fun r -> r.activity_pub_messages_processed_per_second)
      |> Jsont.Object.opt_mem "averageAbuseResponseTimeMs" Openapi.Runtime.number_jsont ~enc:(fun r -> r.average_abuse_response_time_ms)
      |> Jsont.Object.opt_mem "averageRegistrationRequestResponseTimeMs" Openapi.Runtime.number_jsont ~enc:(fun r -> r.average_registration_request_response_time_ms)
      |> Jsont.Object.opt_mem "totalAbuses" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_abuses)
      |> Jsont.Object.opt_mem "totalAbusesProcessed" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_abuses_processed)
      |> Jsont.Object.opt_mem "totalActivityPubMessagesErrors" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_activity_pub_messages_errors)
      |> Jsont.Object.opt_mem "totalActivityPubMessagesProcessed" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_activity_pub_messages_processed)
      |> Jsont.Object.opt_mem "totalActivityPubMessagesSuccesses" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_activity_pub_messages_successes)
      |> Jsont.Object.opt_mem "totalActivityPubMessagesWaiting" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_activity_pub_messages_waiting)
      |> Jsont.Object.opt_mem "totalAdmins" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_admins)
      |> Jsont.Object.opt_mem "totalDailyActiveUsers" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_daily_active_users)
      |> Jsont.Object.opt_mem "totalInstanceFollowers" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_instance_followers)
      |> Jsont.Object.opt_mem "totalInstanceFollowing" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_instance_following)
      |> Jsont.Object.opt_mem "totalLocalDailyActiveVideoChannels" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_local_daily_active_video_channels)
      |> Jsont.Object.opt_mem "totalLocalMonthlyActiveVideoChannels" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_local_monthly_active_video_channels)
      |> Jsont.Object.opt_mem "totalLocalPlaylists" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_local_playlists)
      |> Jsont.Object.opt_mem "totalLocalVideoChannels" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_local_video_channels)
      |> Jsont.Object.opt_mem "totalLocalVideoComments" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_local_video_comments)
      |> Jsont.Object.opt_mem "totalLocalVideoFilesSize" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_local_video_files_size)
      |> Jsont.Object.opt_mem "totalLocalVideoViews" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_local_video_views)
      |> Jsont.Object.opt_mem "totalLocalVideos" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_local_videos)
      |> Jsont.Object.opt_mem "totalLocalWeeklyActiveVideoChannels" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_local_weekly_active_video_channels)
      |> Jsont.Object.opt_mem "totalModerators" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_moderators)
      |> Jsont.Object.opt_mem "totalMonthlyActiveUsers" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_monthly_active_users)
      |> Jsont.Object.opt_mem "totalRegistrationRequests" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_registration_requests)
      |> Jsont.Object.opt_mem "totalRegistrationRequestsProcessed" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_registration_requests_processed)
      |> Jsont.Object.opt_mem "totalUsers" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_users)
      |> Jsont.Object.opt_mem "totalVideoComments" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_video_comments)
      |> Jsont.Object.opt_mem "totalVideos" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_videos)
      |> Jsont.Object.opt_mem "totalWeeklyActiveUsers" Openapi.Runtime.number_jsont ~enc:(fun r -> r.total_weekly_active_users)
      |> Jsont.Object.opt_mem "videosRedundancy" (Jsont.list Jsont.json) ~enc:(fun r -> r.videos_redundancy)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "ServerStats" jsont
  end

  (** Get instance stats

      Get instance public statistics. This endpoint is cached. *)
  let get_instance_stats client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/server/stats" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/ServerStats\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/ServerStats\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_instance_stats" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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
      |> Jsont.Object.opt_mem "status" Openapi.Runtime.int_jsont ~enc:(fun r -> r.status)
      |> Jsont.Object.opt_mem "type" Jsont.string ~enc:(fun r -> r.type_)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "ServerError" jsont
  end
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "SendClientLog" jsont
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
      |> Jsont.Object.opt_mem "id" Openapi.Runtime.int_jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "registeredRunnersCount" Openapi.Runtime.int_jsont ~enc:(fun r -> r.registered_runners_count)
      |> Jsont.Object.opt_mem "registrationToken" Jsont.string ~enc:(fun r -> r.registration_token)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "RunnerRegistrationToken" jsont
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
    let jsont = Openapi.Runtime.int_jsont
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "RunnerJobState" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "RunnerJobStateConstant" jsont
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
    let v () = Jsont.Object ([], Jsont.Meta.none)

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "RunnerJobPayload" jsont
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
        error : string option option;  (** Error message if the job is errored *)
        failures : int option;  (** Number of times a remote runner failed to process this job. After too many failures, the job in "error" state *)
        finished_at : Ptime.t option;
        parent : Jsont.json option option;  (** If job has a parent job *)
        payload : RunnerJobPayload.T.t option;
        priority : int option;  (** Job priority (less has more priority) *)
        progress : int option;  (** Percentage progress *)
        runner : Jsont.json option option;  (** If job is associated to a runner *)
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "RunnerJobType" jsont
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
      |> Jsont.Object.opt_mem "error" (Jsont.option Jsont.string) ~enc:(fun r -> r.error)
      |> Jsont.Object.opt_mem "failures" Openapi.Runtime.int_jsont ~enc:(fun r -> r.failures)
      |> Jsont.Object.opt_mem "finishedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.finished_at)
      |> Jsont.Object.opt_mem "parent" (Jsont.option Jsont.json) ~enc:(fun r -> r.parent)
      |> Jsont.Object.opt_mem "payload" RunnerJobPayload.T.jsont ~enc:(fun r -> r.payload)
      |> Jsont.Object.opt_mem "priority" Openapi.Runtime.int_jsont ~enc:(fun r -> r.priority)
      |> Jsont.Object.opt_mem "progress" Openapi.Runtime.int_jsont ~enc:(fun r -> r.progress)
      |> Jsont.Object.opt_mem "runner" (Jsont.option Jsont.json) ~enc:(fun r -> r.runner)
      |> Jsont.Object.opt_mem "startedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.started_at)
      |> Jsont.Object.opt_mem "state" RunnerJobStateConstant.T.jsont ~enc:(fun r -> r.state)
      |> Jsont.Object.opt_mem "type" Type.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "uuid" Uuidv4.T.jsont ~enc:(fun r -> r.uuid)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "RunnerJob" jsont
  end
end

module RunnerJobAdmin = struct
  module Types = struct
    module T = struct
      type t = {
        created_at : Ptime.t option;
        error : string option option;  (** Error message if the job is errored *)
        failures : int option;  (** Number of times a remote runner failed to process this job. After too many failures, the job in "error" state *)
        finished_at : Ptime.t option;
        parent : Jsont.json option option;  (** If job has a parent job *)
        payload : RunnerJobPayload.T.t option;
        priority : int option;  (** Job priority (less has more priority) *)
        progress : int option;  (** Percentage progress *)
        runner : Jsont.json option option;  (** If job is associated to a runner *)
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
      |> Jsont.Object.opt_mem "error" (Jsont.option Jsont.string) ~enc:(fun r -> r.error)
      |> Jsont.Object.opt_mem "failures" Openapi.Runtime.int_jsont ~enc:(fun r -> r.failures)
      |> Jsont.Object.opt_mem "finishedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.finished_at)
      |> Jsont.Object.opt_mem "parent" (Jsont.option Jsont.json) ~enc:(fun r -> r.parent)
      |> Jsont.Object.opt_mem "payload" RunnerJobPayload.T.jsont ~enc:(fun r -> r.payload)
      |> Jsont.Object.opt_mem "priority" Openapi.Runtime.int_jsont ~enc:(fun r -> r.priority)
      |> Jsont.Object.opt_mem "progress" Openapi.Runtime.int_jsont ~enc:(fun r -> r.progress)
      |> Jsont.Object.opt_mem "runner" (Jsont.option Jsont.json) ~enc:(fun r -> r.runner)
      |> Jsont.Object.opt_mem "startedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.started_at)
      |> Jsont.Object.opt_mem "state" RunnerJobStateConstant.T.jsont ~enc:(fun r -> r.state)
      |> Jsont.Object.opt_mem "type" RunnerJob.Type.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "uuid" Uuidv4.T.jsont ~enc:(fun r -> r.uuid)
      |> Jsont.Object.opt_mem "privatePayload" Jsont.json ~enc:(fun r -> r.private_payload)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "RunnerJobAdmin" jsont
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
      |> Jsont.Object.opt_mem "id" Openapi.Runtime.int_jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "ip" Jsont.string ~enc:(fun r -> r.ip)
      |> Jsont.Object.opt_mem "lastContact" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.last_contact)
      |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "Runner" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "RequestTwoFactorResponse" jsont
  end

  (** Request two factor auth

      Request two factor authentication for a user
      @param id Entity id
  *)
  let request_two_factor ~id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/users/{id}/two-factor/request" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"currentPassword\":{\"type\":\"string\",\"description\":\"Password of the currently authenticated user\"}},\"required\":[]}" Jsont.json)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/RequestTwoFactorResponse\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/RequestTwoFactorResponse\"}}" (Jsont.list Response.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("403", (fun _ -> None)); ("404", (fun _ -> None))]
      ~operation:"request_two_factor" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST
end

module PredefinedAbuseReasons = struct
  module Types = struct
    module T = struct
      (** Reason categories that help triage reports *)
      type t = string list
    end
  end

  module T = struct
    include Types.T
    let jsont = (Jsont.list Jsont.string)
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "PredefinedAbuseReasons" jsont
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
      |> Jsont.Object.opt_mem "type" Openapi.Runtime.int_jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.opt_mem "uninstalled" Jsont.bool ~enc:(fun r -> r.uninstalled)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "version" Jsont.string ~enc:(fun r -> r.version)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "Plugin" jsont
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
      |> Jsont.Object.opt_mem "total" Openapi.Runtime.int_jsont ~enc:(fun r -> r.total)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "PluginResponse" jsont
  end

  (** List plugins
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_plugins ?plugin_type ?uninstalled ?start ?count ?sort client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/plugins" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"pluginType" ~value:plugin_type; Openapi.Runtime.Query.optional ~key:"uninstalled" ~value:uninstalled; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/PluginResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/PluginResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Response.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_plugins" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** List available plugins
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_available_plugins ?search ?plugin_type ?current_peer_tube_engine ?start ?count ?sort client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/plugins/available" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"pluginType" ~value:plugin_type; Openapi.Runtime.Query.optional ~key:"currentPeerTubeEngine" ~value:current_peer_tube_engine; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/PluginResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/PluginResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Response.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("503", (fun _ -> None))]
      ~operation:"get_available_plugins" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Get a plugin
      @param npm_name name of the plugin/theme on npmjs.com or in its package.json
  *)
  let get_plugin ~npm_name client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("npmName", npm_name)] "/api/v1/plugins/{npmName}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/Plugin\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/Plugin\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"get_plugin" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "PlayerThemeVideoSetting" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "PlayerVideoSettingsUpdate" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "PlayerVideoSettings" jsont
  end

  (** Get video player settings

      Get player settings for a specific video. Returns video-specific settings merged with channel player settings.
      @param id The object id, uuid or short uuid
      @param raw Return raw settings without merging channel defaults
  *)
  let get_video_player_settings ~id ?raw client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/player-settings/videos/{id}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"raw" ~value:raw]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/PlayerVideoSettings\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/PlayerVideoSettings\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"get_video_player_settings" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Update video player settings

      Update player settings for a specific video
      @param id The object id, uuid or short uuid
  *)
  let update_video_player_settings ~id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/player-settings/videos/{id}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/PlayerVideoSettingsUpdate\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Update.jsont)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/PlayerVideoSettings\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/PlayerVideoSettings\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"update_video_player_settings" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "PlayerThemeChannelSetting" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "PlayerChannelSettingsUpdate" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "PlayerChannelSettings" jsont
  end

  (** Get channel player settings

      Get player settings for a video channel.
      @param channel_handle The video channel handle
      @param raw Return raw settings without applying instance defaults
  *)
  let get_channel_player_settings ~channel_handle ?raw client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/player-settings/video-channels/{channelHandle}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"raw" ~value:raw]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/PlayerChannelSettings\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/PlayerChannelSettings\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"get_channel_player_settings" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Update channel player settings

      Update default player settings for a video channel.
      @param channel_handle The video channel handle
  *)
  let update_channel_player_settings ~channel_handle ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/player-settings/video-channels/{channelHandle}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/PlayerChannelSettingsUpdate\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Update.jsont)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/PlayerChannelSettings\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/PlayerChannelSettings\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"update_channel_player_settings" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "PlayerTheme" jsont
  end
end

module Password = struct
  module Types = struct
    module T = struct
      type t = string
    end
  end

  module T = struct
    include Types.T
    let jsont = Jsont.string
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "password" jsont
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
        plugin_auth : string option option;  (** The auth plugin to use to authenticate the user *)
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
      |> Jsont.Object.opt_mem "pluginAuth" (Jsont.option Jsont.string) ~enc:(fun r -> r.plugin_auth)
      |> Jsont.Object.opt_mem "role" UserRole.T.jsont ~enc:(fun r -> r.role)
      |> Jsont.Object.opt_mem "videoQuota" Openapi.Runtime.int_jsont ~enc:(fun r -> r.video_quota)
      |> Jsont.Object.opt_mem "videoQuotaDaily" Openapi.Runtime.int_jsont ~enc:(fun r -> r.video_quota_daily)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "UpdateUser" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "RegisterUser" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "OAuthToken-password" jsont
  end
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "OAuthToken-refresh_token" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "OAuthClient" jsont
  end

  (** Login prerequisite

      You need to retrieve a client id and secret before [logging in](#operation/getOAuthToken). *)
  let get_oauth_client client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/oauth-clients/local" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/OAuthClient\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/OAuthClient\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_oauth_client" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "NSFWPolicy" jsont
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
    let jsont = Openapi.Runtime.int_jsont
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "NSFWFlag" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "UpdateMe" jsont
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
      type t = int
    end
  end

  module T = struct
    include Types.T
    let jsont = Openapi.Runtime.int_jsont
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "NotificationSettingValue" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "UserNotificationSettings" jsont
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
    let jsont = Openapi.Runtime.int_jsont
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "NewFeatureInfoType" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "MRSSPeerLink" jsont
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
      |> Jsont.Object.opt_mem "duration" Openapi.Runtime.int_jsont ~enc:(fun r -> r.duration)
      |> Jsont.Object.opt_mem "fileSize" Openapi.Runtime.int_jsont ~enc:(fun r -> r.file_size)
      |> Jsont.Object.opt_mem "framerate" Openapi.Runtime.int_jsont ~enc:(fun r -> r.framerate)
      |> Jsont.Object.opt_mem "height" Openapi.Runtime.int_jsont ~enc:(fun r -> r.height)
      |> Jsont.Object.opt_mem "lang" Jsont.string ~enc:(fun r -> r.lang)
      |> Jsont.Object.opt_mem "type" Jsont.string ~enc:(fun r -> r.type_)
      |> Jsont.Object.opt_mem "url" Jsont.string ~enc:(fun r -> r.url)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "MRSSGroupContent" jsont
  end
end

module VideosForXml = struct
  module Types = struct
    module T = struct
      type t = Jsont.json list
    end
  end

  module T = struct
    include Types.T
    let jsont = (Jsont.list Jsont.json)
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideosForXML" jsont
  end
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
    let jsont = Openapi.Runtime.int_jsont
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "LiveVideoLatencyMode" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "LiveSchedule" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "LiveVideoUpdate" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "LiveVideoResponse" jsont
  end

  (** Get information about a live
      @param id The object id, uuid or short uuid
  *)
  let get_live_id ~id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/live/{id}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/LiveVideoResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/LiveVideoResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Response.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_live_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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
      |> Jsont.Object.opt_mem "videoChannelSyncId" Openapi.Runtime.int_jsont ~enc:(fun r -> r.video_channel_sync_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "ImportVideosInChannelCreate" jsont
  end
end

module Id = struct
  module Types = struct
    module T = struct
      type t = int
    end
  end

  module T = struct
    include Types.T
    let jsont = Openapi.Runtime.int_jsont
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "id" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "WatchedWordsLists" jsont
  end
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoPassword" jsont
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
      |> Jsont.Object.opt_mem "total" Openapi.Runtime.int_jsont ~enc:(fun r -> r.total)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoPasswordList" jsont
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
      |> Jsont.Object.opt_mem "dislikes" Openapi.Runtime.int_jsont ~enc:(fun r -> r.dislikes)
      |> Jsont.Object.opt_mem "duration" Openapi.Runtime.int_jsont ~enc:(fun r -> r.duration)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "likes" Openapi.Runtime.int_jsont ~enc:(fun r -> r.likes)
      |> Jsont.Object.opt_mem "name" (Openapi.Runtime.validated_string ~min_length:3 ~max_length:120 Jsont.string) ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "nsfw" Jsont.bool ~enc:(fun r -> r.nsfw)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "uuid" Uuidv4.T.jsont ~enc:(fun r -> r.uuid)
      |> Jsont.Object.opt_mem "videoId" Jsont.json ~enc:(fun r -> r.video_id)
      |> Jsont.Object.opt_mem "views" Openapi.Runtime.int_jsont ~enc:(fun r -> r.views)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoBlacklist" jsont
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
        moderation_response : string option option;
        registration_reason : string option;
        state : Jsont.json option;
        updated_at : Ptime.t option;
        user : Jsont.json option option;  (** If the registration has been accepted, this is a partial user object created by the registration *)
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "UserRegistrationRequest" jsont
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
      |> Jsont.Object.opt_mem "moderationResponse" (Jsont.option Jsont.string) ~enc:(fun r -> r.moderation_response)
      |> Jsont.Object.opt_mem "registrationReason" Jsont.string ~enc:(fun r -> r.registration_reason)
      |> Jsont.Object.opt_mem "state" Jsont.json ~enc:(fun r -> r.state)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "user" (Jsont.option Jsont.json) ~enc:(fun r -> r.user)
      |> Jsont.Object.opt_mem "username" Jsont.string ~enc:(fun r -> r.username)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "UserRegistration" jsont
  end

  (** Request registration

      Signup has to be enabled and require approval on the instance *)
  let request_registration ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/registrations/request" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/UserRegistrationRequest\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Request.jsont)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/UserRegistration\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/UserRegistration\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None)); ("403", (fun _ -> None)); ("409", (fun _ -> None))]
      ~operation:"request_registration" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST
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
      |> Jsont.Object.opt_mem "bufferStalled" Openapi.Runtime.number_jsont ~enc:(fun r -> r.buffer_stalled)
      |> Jsont.Object.mem "downloadedBytesHTTP" Openapi.Runtime.number_jsont ~enc:(fun r -> r.downloaded_bytes_http)
      |> Jsont.Object.mem "downloadedBytesP2P" Openapi.Runtime.number_jsont ~enc:(fun r -> r.downloaded_bytes_p2_p)
      |> Jsont.Object.mem "errors" Openapi.Runtime.number_jsont ~enc:(fun r -> r.errors)
      |> Jsont.Object.opt_mem "fps" Openapi.Runtime.number_jsont ~enc:(fun r -> r.fps)
      |> Jsont.Object.mem "p2pEnabled" Jsont.bool ~enc:(fun r -> r.p2p_enabled)
      |> Jsont.Object.opt_mem "p2pPeers" Openapi.Runtime.number_jsont ~enc:(fun r -> r.p2p_peers)
      |> Jsont.Object.mem "playerMode" Jsont.string ~enc:(fun r -> r.player_mode)
      |> Jsont.Object.opt_mem "resolution" Openapi.Runtime.number_jsont ~enc:(fun r -> r.resolution)
      |> Jsont.Object.mem "resolutionChanges" Openapi.Runtime.number_jsont ~enc:(fun r -> r.resolution_changes)
      |> Jsont.Object.mem "uploadedBytesP2P" Openapi.Runtime.number_jsont ~enc:(fun r -> r.uploaded_bytes_p2_p)
      |> Jsont.Object.mem "videoId" Jsont.json ~enc:(fun r -> r.video_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "PlaybackMetricCreate" jsont
  end
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "Job" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "GetMeVideoRating" jsont
  end

  (** Get rate of my user for a video
      @param video_id The video id
  *)
  let get_api_v1_users_me_videos_by_video_id_rating ~video_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("videoId", video_id)] "/api/v1/users/me/videos/{videoId}/rating" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/GetMeVideoRating\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/GetMeVideoRating\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_users_me_videos_by_video_id_rating" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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
      |> Jsont.Object.opt_mem "size" Openapi.Runtime.int_jsont ~enc:(fun r -> r.size)
      |> Jsont.Object.opt_mem "strategy" Jsont.string ~enc:(fun r -> r.strategy)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "FileRedundancyInformation" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoRedundancy" jsont
  end

  (** List videos being mirrored
      @param target direction of the mirror
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort abuses by criteria
  *)
  let get_mirrored_videos ~target ?start ?count ?sort client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/server/redundancy/videos" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.singleton ~key:"target" ~value:target; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/VideoRedundancy\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/VideoRedundancy\"}}" (Jsont.list T.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_mirrored_videos" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "AddUserResponse" jsont
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
      |> Jsont.Object.opt_mem "videoQuota" Openapi.Runtime.int_jsont ~enc:(fun r -> r.video_quota)
      |> Jsont.Object.opt_mem "videoQuotaDaily" Openapi.Runtime.int_jsont ~enc:(fun r -> r.video_quota_daily)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "AddUser" jsont
  end

  (** Create a user *)
  let add_user ~body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/AddUser\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont)) body in
    let __openapi_body = Some __openapi_body in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/AddUserResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/AddUserResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Response.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("403", (fun _ -> None))]
      ~operation:"add_user" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST
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
    let jsont = Openapi.Runtime.int_jsont
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "FileStorage" jsont
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
      |> Jsont.Object.opt_mem "fps" Openapi.Runtime.number_jsont ~enc:(fun r -> r.fps)
      |> Jsont.Object.opt_mem "hasAudio" Jsont.bool ~enc:(fun r -> r.has_audio)
      |> Jsont.Object.opt_mem "hasVideo" Jsont.bool ~enc:(fun r -> r.has_video)
      |> Jsont.Object.opt_mem "height" Openapi.Runtime.number_jsont ~enc:(fun r -> r.height)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "magnetUri" (Openapi.Runtime.validated_string ~pattern:"/magnet:\\?xt=urn:[a-z0-9]+:[a-z0-9]{32}/i" Jsont.string) ~enc:(fun r -> r.magnet_uri)
      |> Jsont.Object.opt_mem "metadataUrl" Jsont.string ~enc:(fun r -> r.metadata_url)
      |> Jsont.Object.opt_mem "playlistUrl" Jsont.string ~enc:(fun r -> r.playlist_url)
      |> Jsont.Object.opt_mem "resolution" VideoResolutionConstant.T.jsont ~enc:(fun r -> r.resolution)
      |> Jsont.Object.opt_mem "size" Openapi.Runtime.int_jsont ~enc:(fun r -> r.size)
      |> Jsont.Object.opt_mem "storage" FileStorage.T.jsont ~enc:(fun r -> r.storage)
      |> Jsont.Object.opt_mem "torrentDownloadUrl" Jsont.string ~enc:(fun r -> r.torrent_download_url)
      |> Jsont.Object.opt_mem "torrentUrl" Jsont.string ~enc:(fun r -> r.torrent_url)
      |> Jsont.Object.opt_mem "width" Openapi.Runtime.number_jsont ~enc:(fun r -> r.width)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoFile" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoStreamingPlaylists-HLS" jsont
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
      |> Jsont.Object.opt_mem "type" Openapi.Runtime.int_jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.opt_mem "files" (Jsont.list VideoFile.T.jsont) ~enc:(fun r -> r.files)
      |> Jsont.Object.opt_mem "playlistUrl" Jsont.string ~enc:(fun r -> r.playlist_url)
      |> Jsont.Object.opt_mem "redundancies" (Jsont.list Jsont.json) ~enc:(fun r -> r.redundancies)
      |> Jsont.Object.opt_mem "segmentsSha256Url" Jsont.string ~enc:(fun r -> r.segments_sha256_url)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoStreamingPlaylists" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "CustomHomepage" jsont
  end

  (** Get instance custom homepage *)
  let get_api_v1_custom_pages_homepage_instance client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/custom-pages/homepage/instance" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/CustomHomepage\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/CustomHomepage\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"get_api_v1_custom_pages_homepage_instance" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "CommentAutoTagPolicies" jsont
  end

  (** Get account auto tag policies on comments

      **PeerTube >= 6.2**
      @param account_name account name to get auto tag policies
  *)
  let get_api_v1_automatic_tags_policies_accounts_by_account_name_comments ~account_name client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("accountName", account_name)] "/api/v1/automatic-tags/policies/accounts/{accountName}/comments" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/CommentAutoTagPolicies\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/CommentAutoTagPolicies\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_automatic_tags_policies_accounts_by_account_name_comments" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "BlockStatus" jsont
  end

  (** Get block status of accounts/hosts
      @param accounts Check if these accounts are blocked
      @param hosts Check if these hosts are blocked
  *)
  let get_api_v1_blocklist_status ?accounts ?hosts client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/blocklist/status" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"accounts" ~value:accounts; Openapi.Runtime.Query.optional ~key:"hosts" ~value:hosts]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/BlockStatus\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/BlockStatus\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Status.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_blocklist_status" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "AutomaticTagAvailable" jsont
  end

  (** Get account available auto tags

      **PeerTube >= 6.2**
      @param account_name account name to get auto tag policies
  *)
  let get_api_v1_automatic_tags_accounts_by_account_name_available ~account_name client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("accountName", account_name)] "/api/v1/automatic-tags/accounts/{accountName}/available" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/AutomaticTagAvailable\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/AutomaticTagAvailable\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_automatic_tags_accounts_by_account_name_available" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Get server available auto tags

      **PeerTube >= 6.2** *)
  let get_api_v1_automatic_tags_server_available client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/automatic-tags/server/available" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/AutomaticTagAvailable\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/AutomaticTagAvailable\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_automatic_tags_server_available" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
end

module AddVideoPasswords = struct
  module Types = struct
    module T = struct
      type t = Jsont.json list
    end
  end

  module T = struct
    include Types.T
    let jsont = (Jsont.list Jsont.json)
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "AddVideoPasswords" jsont
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
      |> Jsont.Object.mem "channelId" (Openapi.Runtime.validated_int ~minimum:1. Openapi.Runtime.int_jsont) ~enc:(fun r -> r.channel_id)
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoUploadRequestResumable" jsont
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
      |> Jsont.Object.mem "channelId" (Openapi.Runtime.validated_int ~minimum:1. Openapi.Runtime.int_jsont) ~enc:(fun r -> r.channel_id)
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoUploadRequestLegacy" jsont
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
      |> Jsont.Object.mem "channelId" (Openapi.Runtime.validated_int ~minimum:1. Openapi.Runtime.int_jsont) ~enc:(fun r -> r.channel_id)
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoUploadRequestCommon" jsont
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
      |> Jsont.Object.mem "channelId" (Openapi.Runtime.validated_int ~minimum:1. Openapi.Runtime.int_jsont) ~enc:(fun r -> r.channel_id)
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoCreateImport" jsont
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
      |> Jsont.Object.opt_mem "height" Openapi.Runtime.int_jsont ~enc:(fun r -> r.height)
      |> Jsont.Object.opt_mem "path" Jsont.string ~enc:(fun r -> r.path)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "width" Openapi.Runtime.int_jsont ~enc:(fun r -> r.width)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "ActorImage" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoChannelSummary" jsont
  end
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "ServerConfigAbout" jsont
  end

  (** Get instance "About" information *)
  let get_about client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/config/about" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/ServerConfigAbout\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/ServerConfigAbout\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_about" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "ServerConfig" jsont
  end

  (** Get instance public configuration *)
  let get_config client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/config" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/ServerConfig\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/ServerConfig\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Config.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_config" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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
        host_redundancy_allowed : bool option option;  (** whether this actor's host allows redundancy of its videos *)
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "ActorInfo" jsont
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
      |> Jsont.Object.opt_mem "followersCount" (Openapi.Runtime.validated_int ~minimum:0. Openapi.Runtime.int_jsont) ~enc:(fun r -> r.followers_count)
      |> Jsont.Object.opt_mem "followingCount" (Openapi.Runtime.validated_int ~minimum:0. Openapi.Runtime.int_jsont) ~enc:(fun r -> r.following_count)
      |> Jsont.Object.opt_mem "host" Jsont.string ~enc:(fun r -> r.host)
      |> Jsont.Object.opt_mem "hostRedundancyAllowed" (Jsont.option Jsont.bool) ~enc:(fun r -> r.host_redundancy_allowed)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "name" Username.T.jsont ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "url" Jsont.string ~enc:(fun r -> r.url)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "Actor" jsont
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
      |> Jsont.Object.opt_mem "score" Openapi.Runtime.number_jsont ~enc:(fun r -> r.score)
      |> Jsont.Object.opt_mem "state" Jsont.string ~enc:(fun r -> r.state)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "Follow" jsont
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
      |> Jsont.Object.opt_mem "id" Openapi.Runtime.int_jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "url" Jsont.string ~enc:(fun r -> r.url)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "AccountSummary" jsont
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
      |> Jsont.Object.opt_mem "videoChannelPosition" (Openapi.Runtime.validated_int ~minimum:1. Openapi.Runtime.int_jsont) ~enc:(fun r -> r.video_channel_position)
      |> Jsont.Object.opt_mem "videoLength" (Openapi.Runtime.validated_int ~minimum:0. Openapi.Runtime.int_jsont) ~enc:(fun r -> r.video_length)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoPlaylist" jsont
  end

  (** Get a video playlist
      @param playlist_id Playlist id
  *)
  let get_api_v1_video_playlists_by_playlist_id ~playlist_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("playlistId", playlist_id)] "/api/v1/video-playlists/{playlistId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoPlaylist\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoPlaylist\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_video_playlists_by_playlist_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoChannelCollaborator" jsont
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
        aspect_ratio : float option option;  (** **PeerTube >= 6.1** Aspect ratio of the video stream *)
        blacklisted : bool option option;
        blacklisted_reason : string option option;
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
        nsfw_summary : string option option;  (** **PeerTube >= 7.2** More information about the sensitive content of the video *)
        originally_published_at : Ptime.t option option;  (** used to represent a date of first publication, prior to the practical publication date of `publishedAt` *)
        preview_path : string option;
        privacy : VideoPrivacyConstant.T.t option;  (** privacy policy used to distribute the video *)
        published_at : Ptime.t option;  (** time at which the video was marked as ready for playback (with restrictions depending on `privacy`). Usually set after a `state` evolution. *)
        scheduled_update : VideoScheduled.Update.t option;
        short_uuid : ShortUuid.T.t option;
        state : VideoStateConstant.T.t option;  (** represents the internal state of the video processing within the PeerTube instance *)
        thumbnail_path : string option;
        truncated_description : string option option;  (** truncated description of the video, written in Markdown.
       *)
        updated_at : Ptime.t option;  (** last time the video's metadata was modified *)
        user_history : Jsont.json option option;
        uuid : Uuidv4.T.t option;  (** universal identifier for the video, that can be used across instances *)
        views : int option;
        wait_transcoding : bool option option;
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoInfo" jsont
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
      |> Jsont.Object.opt_mem "aspectRatio" (Jsont.option Openapi.Runtime.number_jsont) ~enc:(fun r -> r.aspect_ratio)
      |> Jsont.Object.opt_mem "blacklisted" (Jsont.option Jsont.bool) ~enc:(fun r -> r.blacklisted)
      |> Jsont.Object.opt_mem "blacklistedReason" (Jsont.option Jsont.string) ~enc:(fun r -> r.blacklisted_reason)
      |> Jsont.Object.opt_mem "category" VideoConstantNumberCategory.T.jsont ~enc:(fun r -> r.category)
      |> Jsont.Object.opt_mem "channel" VideoChannelSummary.T.jsont ~enc:(fun r -> r.channel)
      |> Jsont.Object.opt_mem "comments" Openapi.Runtime.int_jsont ~enc:(fun r -> r.comments)
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "dislikes" Openapi.Runtime.int_jsont ~enc:(fun r -> r.dislikes)
      |> Jsont.Object.opt_mem "duration" Openapi.Runtime.int_jsont ~enc:(fun r -> r.duration)
      |> Jsont.Object.opt_mem "embedPath" Jsont.string ~enc:(fun r -> r.embed_path)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "isLive" Jsont.bool ~enc:(fun r -> r.is_live)
      |> Jsont.Object.opt_mem "isLocal" Jsont.bool ~enc:(fun r -> r.is_local)
      |> Jsont.Object.opt_mem "language" VideoConstantStringLanguage.T.jsont ~enc:(fun r -> r.language)
      |> Jsont.Object.opt_mem "licence" VideoConstantNumberLicence.T.jsont ~enc:(fun r -> r.licence)
      |> Jsont.Object.opt_mem "likes" Openapi.Runtime.int_jsont ~enc:(fun r -> r.likes)
      |> Jsont.Object.opt_mem "liveSchedules" (Jsont.list LiveSchedule.T.jsont) ~enc:(fun r -> r.live_schedules)
      |> Jsont.Object.opt_mem "name" (Openapi.Runtime.validated_string ~min_length:3 ~max_length:120 Jsont.string) ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "nsfw" Jsont.bool ~enc:(fun r -> r.nsfw)
      |> Jsont.Object.opt_mem "nsfwFlags" Nsfwflag.T.jsont ~enc:(fun r -> r.nsfw_flags)
      |> Jsont.Object.opt_mem "nsfwSummary" (Jsont.option Jsont.string) ~enc:(fun r -> r.nsfw_summary)
      |> Jsont.Object.opt_mem "originallyPublishedAt" (Jsont.option Openapi.Runtime.ptime_jsont) ~enc:(fun r -> r.originally_published_at)
      |> Jsont.Object.opt_mem "previewPath" Jsont.string ~enc:(fun r -> r.preview_path)
      |> Jsont.Object.opt_mem "privacy" VideoPrivacyConstant.T.jsont ~enc:(fun r -> r.privacy)
      |> Jsont.Object.opt_mem "publishedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.published_at)
      |> Jsont.Object.opt_mem "scheduledUpdate" VideoScheduled.Update.jsont ~enc:(fun r -> r.scheduled_update)
      |> Jsont.Object.opt_mem "shortUUID" ShortUuid.T.jsont ~enc:(fun r -> r.short_uuid)
      |> Jsont.Object.opt_mem "state" VideoStateConstant.T.jsont ~enc:(fun r -> r.state)
      |> Jsont.Object.opt_mem "thumbnailPath" Jsont.string ~enc:(fun r -> r.thumbnail_path)
      |> Jsont.Object.opt_mem "truncatedDescription" (Jsont.option (Openapi.Runtime.validated_string ~min_length:3 ~max_length:250 Jsont.string)) ~enc:(fun r -> r.truncated_description)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "userHistory" (Jsont.option Jsont.json) ~enc:(fun r -> r.user_history)
      |> Jsont.Object.opt_mem "uuid" Uuidv4.T.jsont ~enc:(fun r -> r.uuid)
      |> Jsont.Object.opt_mem "views" Openapi.Runtime.int_jsont ~enc:(fun r -> r.views)
      |> Jsont.Object.opt_mem "waitTranscoding" (Jsont.option Jsont.bool) ~enc:(fun r -> r.wait_transcoding)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "Video" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoRating" jsont
  end

  (** List ratings of an account
      @param name The username or handle of the account
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
      @param rating Optionally filter which ratings to retrieve
  *)
  let get_api_v1_accounts_by_name_ratings ~name ?start ?count ?sort ?rating client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("name", name)] "/api/v1/accounts/{name}/ratings" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"rating" ~value:rating]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/VideoRating\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/VideoRating\"}}" (Jsont.list T.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_accounts_by_name_ratings" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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
      |> Jsont.Object.opt_mem "total" Openapi.Runtime.int_jsont ~enc:(fun r -> r.total)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoListResponse" jsont
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
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("name", name)] "/api/v1/accounts/{name}/videos" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"skipCount" ~value:skip_count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"nsfw" ~value:nsfw; Openapi.Runtime.Query.optional ~key:"nsfwFlagsIncluded" ~value:nsfw_flags_included; Openapi.Runtime.Query.optional ~key:"nsfwFlagsExcluded" ~value:nsfw_flags_excluded; Openapi.Runtime.Query.optional ~key:"isLive" ~value:is_live; Openapi.Runtime.Query.optional ~key:"includeScheduledLive" ~value:include_scheduled_live; Openapi.Runtime.Query.optional ~key:"categoryOneOf" ~value:category_one_of; Openapi.Runtime.Query.optional ~key:"licenceOneOf" ~value:licence_one_of; Openapi.Runtime.Query.optional ~key:"languageOneOf" ~value:language_one_of; Openapi.Runtime.Query.optional ~key:"tagsOneOf" ~value:tags_one_of; Openapi.Runtime.Query.optional ~key:"tagsAllOf" ~value:tags_all_of; Openapi.Runtime.Query.optional ~key:"isLocal" ~value:is_local; Openapi.Runtime.Query.optional ~key:"include" ~value:include_; Openapi.Runtime.Query.optional ~key:"hasHLSFiles" ~value:has_hlsfiles; Openapi.Runtime.Query.optional ~key:"hasWebVideoFiles" ~value:has_web_video_files; Openapi.Runtime.Query.optional ~key:"host" ~value:host; Openapi.Runtime.Query.optional ~key:"autoTagOneOf" ~value:auto_tag_one_of; Openapi.Runtime.Query.optional ~key:"privacyOneOf" ~value:privacy_one_of; Openapi.Runtime.Query.optional ~key:"excludeAlreadyWatched" ~value:exclude_already_watched; Openapi.Runtime.Query.optional ~key:"search" ~value:search]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoListResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoListResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Response.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_account_videos" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

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
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/search/videos" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.singleton ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"uuids" ~value:uuids; Openapi.Runtime.Query.optional ~key:"searchTarget" ~value:search_target; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"skipCount" ~value:skip_count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"nsfw" ~value:nsfw; Openapi.Runtime.Query.optional ~key:"nsfwFlagsIncluded" ~value:nsfw_flags_included; Openapi.Runtime.Query.optional ~key:"nsfwFlagsExcluded" ~value:nsfw_flags_excluded; Openapi.Runtime.Query.optional ~key:"isLive" ~value:is_live; Openapi.Runtime.Query.optional ~key:"includeScheduledLive" ~value:include_scheduled_live; Openapi.Runtime.Query.optional ~key:"categoryOneOf" ~value:category_one_of; Openapi.Runtime.Query.optional ~key:"licenceOneOf" ~value:licence_one_of; Openapi.Runtime.Query.optional ~key:"languageOneOf" ~value:language_one_of; Openapi.Runtime.Query.optional ~key:"tagsOneOf" ~value:tags_one_of; Openapi.Runtime.Query.optional ~key:"tagsAllOf" ~value:tags_all_of; Openapi.Runtime.Query.optional ~key:"isLocal" ~value:is_local; Openapi.Runtime.Query.optional ~key:"include" ~value:include_; Openapi.Runtime.Query.optional ~key:"hasHLSFiles" ~value:has_hlsfiles; Openapi.Runtime.Query.optional ~key:"hasWebVideoFiles" ~value:has_web_video_files; Openapi.Runtime.Query.optional ~key:"host" ~value:host; Openapi.Runtime.Query.optional ~key:"autoTagOneOf" ~value:auto_tag_one_of; Openapi.Runtime.Query.optional ~key:"privacyOneOf" ~value:privacy_one_of; Openapi.Runtime.Query.optional ~key:"excludeAlreadyWatched" ~value:exclude_already_watched; Openapi.Runtime.Query.optional ~key:"startDate" ~value:start_date; Openapi.Runtime.Query.optional ~key:"endDate" ~value:end_date; Openapi.Runtime.Query.optional ~key:"originallyPublishedStartDate" ~value:originally_published_start_date; Openapi.Runtime.Query.optional ~key:"originallyPublishedEndDate" ~value:originally_published_end_date; Openapi.Runtime.Query.optional ~key:"durationMin" ~value:duration_min; Openapi.Runtime.Query.optional ~key:"durationMax" ~value:duration_max]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoListResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoListResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Response.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("500", (fun _ -> None))]
      ~operation:"search_videos" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** List watched videos history
      @param start Offset used to paginate results
      @param count Number of items to return
      @param search Plain text search, applied to various parts of the model depending on endpoint
  *)
  let get_api_v1_users_me_history_videos ?start ?count ?search client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/me/history/videos" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"search" ~value:search]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoListResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoListResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Response.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_users_me_history_videos" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

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
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/me/subscriptions/videos" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"skipCount" ~value:skip_count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"nsfw" ~value:nsfw; Openapi.Runtime.Query.optional ~key:"nsfwFlagsIncluded" ~value:nsfw_flags_included; Openapi.Runtime.Query.optional ~key:"nsfwFlagsExcluded" ~value:nsfw_flags_excluded; Openapi.Runtime.Query.optional ~key:"isLive" ~value:is_live; Openapi.Runtime.Query.optional ~key:"includeScheduledLive" ~value:include_scheduled_live; Openapi.Runtime.Query.optional ~key:"categoryOneOf" ~value:category_one_of; Openapi.Runtime.Query.optional ~key:"licenceOneOf" ~value:licence_one_of; Openapi.Runtime.Query.optional ~key:"languageOneOf" ~value:language_one_of; Openapi.Runtime.Query.optional ~key:"tagsOneOf" ~value:tags_one_of; Openapi.Runtime.Query.optional ~key:"tagsAllOf" ~value:tags_all_of; Openapi.Runtime.Query.optional ~key:"isLocal" ~value:is_local; Openapi.Runtime.Query.optional ~key:"include" ~value:include_; Openapi.Runtime.Query.optional ~key:"hasHLSFiles" ~value:has_hlsfiles; Openapi.Runtime.Query.optional ~key:"hasWebVideoFiles" ~value:has_web_video_files; Openapi.Runtime.Query.optional ~key:"host" ~value:host; Openapi.Runtime.Query.optional ~key:"autoTagOneOf" ~value:auto_tag_one_of; Openapi.Runtime.Query.optional ~key:"privacyOneOf" ~value:privacy_one_of; Openapi.Runtime.Query.optional ~key:"excludeAlreadyWatched" ~value:exclude_already_watched; Openapi.Runtime.Query.optional ~key:"search" ~value:search]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoListResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoListResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Response.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_users_me_subscriptions_videos" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

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
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/me/videos" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"channelNameOneOf" ~value:channel_name_one_of; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"skipCount" ~value:skip_count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"nsfw" ~value:nsfw; Openapi.Runtime.Query.optional ~key:"nsfwFlagsIncluded" ~value:nsfw_flags_included; Openapi.Runtime.Query.optional ~key:"nsfwFlagsExcluded" ~value:nsfw_flags_excluded; Openapi.Runtime.Query.optional ~key:"isLive" ~value:is_live; Openapi.Runtime.Query.optional ~key:"includeScheduledLive" ~value:include_scheduled_live; Openapi.Runtime.Query.optional ~key:"categoryOneOf" ~value:category_one_of; Openapi.Runtime.Query.optional ~key:"licenceOneOf" ~value:licence_one_of; Openapi.Runtime.Query.optional ~key:"languageOneOf" ~value:language_one_of; Openapi.Runtime.Query.optional ~key:"tagsOneOf" ~value:tags_one_of; Openapi.Runtime.Query.optional ~key:"tagsAllOf" ~value:tags_all_of; Openapi.Runtime.Query.optional ~key:"isLocal" ~value:is_local; Openapi.Runtime.Query.optional ~key:"include" ~value:include_; Openapi.Runtime.Query.optional ~key:"hasHLSFiles" ~value:has_hlsfiles; Openapi.Runtime.Query.optional ~key:"hasWebVideoFiles" ~value:has_web_video_files; Openapi.Runtime.Query.optional ~key:"host" ~value:host; Openapi.Runtime.Query.optional ~key:"autoTagOneOf" ~value:auto_tag_one_of; Openapi.Runtime.Query.optional ~key:"privacyOneOf" ~value:privacy_one_of; Openapi.Runtime.Query.optional ~key:"excludeAlreadyWatched" ~value:exclude_already_watched; Openapi.Runtime.Query.optional ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"includeCollaborations" ~value:include_collaborations]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoListResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoListResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Response.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_users_me_videos" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

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
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/videos" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"skipCount" ~value:skip_count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"nsfw" ~value:nsfw; Openapi.Runtime.Query.optional ~key:"nsfwFlagsIncluded" ~value:nsfw_flags_included; Openapi.Runtime.Query.optional ~key:"nsfwFlagsExcluded" ~value:nsfw_flags_excluded; Openapi.Runtime.Query.optional ~key:"isLive" ~value:is_live; Openapi.Runtime.Query.optional ~key:"includeScheduledLive" ~value:include_scheduled_live; Openapi.Runtime.Query.optional ~key:"categoryOneOf" ~value:category_one_of; Openapi.Runtime.Query.optional ~key:"licenceOneOf" ~value:licence_one_of; Openapi.Runtime.Query.optional ~key:"languageOneOf" ~value:language_one_of; Openapi.Runtime.Query.optional ~key:"tagsOneOf" ~value:tags_one_of; Openapi.Runtime.Query.optional ~key:"tagsAllOf" ~value:tags_all_of; Openapi.Runtime.Query.optional ~key:"isLocal" ~value:is_local; Openapi.Runtime.Query.optional ~key:"include" ~value:include_; Openapi.Runtime.Query.optional ~key:"hasHLSFiles" ~value:has_hlsfiles; Openapi.Runtime.Query.optional ~key:"hasWebVideoFiles" ~value:has_web_video_files; Openapi.Runtime.Query.optional ~key:"host" ~value:host; Openapi.Runtime.Query.optional ~key:"autoTagOneOf" ~value:auto_tag_one_of; Openapi.Runtime.Query.optional ~key:"privacyOneOf" ~value:privacy_one_of; Openapi.Runtime.Query.optional ~key:"excludeAlreadyWatched" ~value:exclude_already_watched; Openapi.Runtime.Query.optional ~key:"search" ~value:search]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoListResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoListResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Response.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_video_channel_videos" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

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
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/videos" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"skipCount" ~value:skip_count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"nsfw" ~value:nsfw; Openapi.Runtime.Query.optional ~key:"nsfwFlagsIncluded" ~value:nsfw_flags_included; Openapi.Runtime.Query.optional ~key:"nsfwFlagsExcluded" ~value:nsfw_flags_excluded; Openapi.Runtime.Query.optional ~key:"isLive" ~value:is_live; Openapi.Runtime.Query.optional ~key:"includeScheduledLive" ~value:include_scheduled_live; Openapi.Runtime.Query.optional ~key:"categoryOneOf" ~value:category_one_of; Openapi.Runtime.Query.optional ~key:"licenceOneOf" ~value:licence_one_of; Openapi.Runtime.Query.optional ~key:"languageOneOf" ~value:language_one_of; Openapi.Runtime.Query.optional ~key:"tagsOneOf" ~value:tags_one_of; Openapi.Runtime.Query.optional ~key:"tagsAllOf" ~value:tags_all_of; Openapi.Runtime.Query.optional ~key:"isLocal" ~value:is_local; Openapi.Runtime.Query.optional ~key:"include" ~value:include_; Openapi.Runtime.Query.optional ~key:"hasHLSFiles" ~value:has_hlsfiles; Openapi.Runtime.Query.optional ~key:"hasWebVideoFiles" ~value:has_web_video_files; Openapi.Runtime.Query.optional ~key:"host" ~value:host; Openapi.Runtime.Query.optional ~key:"autoTagOneOf" ~value:auto_tag_one_of; Openapi.Runtime.Query.optional ~key:"privacyOneOf" ~value:privacy_one_of; Openapi.Runtime.Query.optional ~key:"excludeAlreadyWatched" ~value:exclude_already_watched; Openapi.Runtime.Query.optional ~key:"search" ~value:search]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoListResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoListResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Response.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_videos" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoImport" jsont
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
      |> Jsont.Object.opt_mem "total" Openapi.Runtime.int_jsont ~enc:(fun r -> r.total)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoImportsList" jsont
  end

  (** Get video imports of my user
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
      @param include_collaborations **PeerTube >= 8.0** Include objects from collaborated channels
      @param video_id Filter on import video ID
      @param target_url Filter on import target URL
      @param video_channel_sync_id Filter on imports created by a specific channel synchronization
      @param search Search in video names
  *)
  let get_api_v1_users_me_videos_imports ?start ?count ?sort ?include_collaborations ?video_id ?target_url ?video_channel_sync_id ?search client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/me/videos/imports" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"includeCollaborations" ~value:include_collaborations; Openapi.Runtime.Query.optional ~key:"videoId" ~value:video_id; Openapi.Runtime.Query.optional ~key:"targetUrl" ~value:target_url; Openapi.Runtime.Query.optional ~key:"videoChannelSyncId" ~value:video_channel_sync_id; Openapi.Runtime.Query.optional ~key:"search" ~value:search]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoImportsList\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoImportsList\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_users_me_videos_imports" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoCommentForOwnerOrAdmin" jsont
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
      |> Jsont.Object.opt_mem "position" Openapi.Runtime.int_jsont ~enc:(fun r -> r.position)
      |> Jsont.Object.opt_mem "startTimestamp" Openapi.Runtime.int_jsont ~enc:(fun r -> r.start_timestamp)
      |> Jsont.Object.opt_mem "stopTimestamp" Openapi.Runtime.int_jsont ~enc:(fun r -> r.stop_timestamp)
      |> Jsont.Object.opt_mem "video" Video.T.jsont ~enc:(fun r -> r.video)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "PlaylistElement" jsont
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
        actor_follow : Jsont.json option option;
        comment : Jsont.json option option;
        created_at : Ptime.t option;
        id : Id.T.t option;
        read : bool option;
        type_ : Type.t option;
        updated_at : Ptime.t option;
        video : Video.Info.t option option;
        video_abuse : Jsont.json option option;
        video_blacklist : Jsont.json option option;
        video_import : Jsont.json option option;
      }
    end
  end

  module Type = struct
    include Types.Type
    let jsont = Openapi.Runtime.int_jsont
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "NotificationType" jsont
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
      |> Jsont.Object.opt_mem "actorFollow" (Jsont.option Jsont.json) ~enc:(fun r -> r.actor_follow)
      |> Jsont.Object.opt_mem "comment" (Jsont.option Jsont.json) ~enc:(fun r -> r.comment)
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "read" Jsont.bool ~enc:(fun r -> r.read)
      |> Jsont.Object.opt_mem "type" Type.jsont ~enc:(fun r -> r.type_)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "video" (Jsont.option Video.Info.jsont) ~enc:(fun r -> r.video)
      |> Jsont.Object.opt_mem "videoAbuse" (Jsont.option Jsont.json) ~enc:(fun r -> r.video_abuse)
      |> Jsont.Object.opt_mem "videoBlacklist" (Jsont.option Jsont.json) ~enc:(fun r -> r.video_blacklist)
      |> Jsont.Object.opt_mem "videoImport" (Jsont.option Jsont.json) ~enc:(fun r -> r.video_import)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "Notification" jsont
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
      |> Jsont.Object.opt_mem "total" Openapi.Runtime.int_jsont ~enc:(fun r -> r.total)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "NotificationListResponse" jsont
  end

  (** List my notifications
      @param type_one_of only list notifications of these types
      @param unread only list unread notifications
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_api_v1_users_me_notifications ?type_one_of ?unread ?start ?count ?sort client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/me/notifications" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"typeOneOf" ~value:type_one_of; Openapi.Runtime.Query.optional ~key:"unread" ~value:unread; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/NotificationListResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/NotificationListResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Response.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_users_me_notifications" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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
      |> Jsont.Object.opt_mem "total" Openapi.Runtime.int_jsont ~enc:(fun r -> r.total)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "ChannelActivityListResponse" jsont
  end

  (** List activities of a video channel

      **PeerTube >= 8.0**
      @param channel_handle The video channel handle
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let list_video_channel_activities ~channel_handle ?start ?count ?sort client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/activities" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/ChannelActivityListResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/ChannelActivityListResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Response.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"list_video_channel_activities" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "AbuseMessage" jsont
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
        host_redundancy_allowed : bool option option;  (** whether this actor's host allows redundancy of its videos *)
        id : Id.T.t option;
        name : Username.T.t option;  (** immutable name of the actor, used to find or mention it *)
        updated_at : Ptime.t option;
        url : string option;
        user_id : Jsont.json option;  (** object id for the user tied to this account *)
        display_name : string option;  (** editable name of the account, displayed in its representations *)
        description : string option option;  (** text or bio displayed on the account's profile *)
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
      |> Jsont.Object.opt_mem "followersCount" (Openapi.Runtime.validated_int ~minimum:0. Openapi.Runtime.int_jsont) ~enc:(fun r -> r.followers_count)
      |> Jsont.Object.opt_mem "followingCount" (Openapi.Runtime.validated_int ~minimum:0. Openapi.Runtime.int_jsont) ~enc:(fun r -> r.following_count)
      |> Jsont.Object.opt_mem "host" Jsont.string ~enc:(fun r -> r.host)
      |> Jsont.Object.opt_mem "hostRedundancyAllowed" (Jsont.option Jsont.bool) ~enc:(fun r -> r.host_redundancy_allowed)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "name" Username.T.jsont ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "url" Jsont.string ~enc:(fun r -> r.url)
      |> Jsont.Object.opt_mem "userId" Jsont.json ~enc:(fun r -> r.user_id)
      |> Jsont.Object.opt_mem "displayName" (Openapi.Runtime.validated_string ~min_length:3 ~max_length:120 Jsont.string) ~enc:(fun r -> r.display_name)
      |> Jsont.Object.opt_mem "description" (Jsont.option Jsont.string) ~enc:(fun r -> r.description)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "Account" jsont
  end

  (** Get an account
      @param name The username or handle of the account
  *)
  let get_account ~name client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("name", name)] "/api/v1/accounts/{name}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/Account\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/Account\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"get_account" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
end

module VideoComment = struct
  module Types = struct
    module T = struct
      type t = {
        account : Account.T.t option;
        created_at : Ptime.t option;
        deleted_at : Ptime.t option option;
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

    let v ?(is_deleted=false) ?account ?created_at ?deleted_at ?held_for_review ?id ?in_reply_to_comment_id ?text ?thread_id ?total_replies ?total_replies_from_video_author ?updated_at ?url ?video_id () = { account; created_at; deleted_at; held_for_review; id; in_reply_to_comment_id; is_deleted; text; thread_id; total_replies; total_replies_from_video_author; updated_at; url; video_id }

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
      |> Jsont.Object.opt_mem "deletedAt" (Jsont.option Openapi.Runtime.ptime_jsont) ~enc:(fun r -> r.deleted_at)
      |> Jsont.Object.opt_mem "heldForReview" Jsont.bool ~enc:(fun r -> r.held_for_review)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "inReplyToCommentId" Id.T.jsont ~enc:(fun r -> r.in_reply_to_comment_id)
      |> Jsont.Object.mem "isDeleted" Jsont.bool ~dec_absent:(fun () -> false) ~enc:(fun r -> r.is_deleted)
      |> Jsont.Object.opt_mem "text" (Openapi.Runtime.validated_string ~min_length:1 Jsont.string) ~enc:(fun r -> r.text)
      |> Jsont.Object.opt_mem "threadId" Id.T.jsont ~enc:(fun r -> r.thread_id)
      |> Jsont.Object.opt_mem "totalReplies" (Openapi.Runtime.validated_int ~minimum:0. Openapi.Runtime.int_jsont) ~enc:(fun r -> r.total_replies)
      |> Jsont.Object.opt_mem "totalRepliesFromVideoAuthor" (Openapi.Runtime.validated_int ~minimum:0. Openapi.Runtime.int_jsont) ~enc:(fun r -> r.total_replies_from_video_author)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "url" Jsont.string ~enc:(fun r -> r.url)
      |> Jsont.Object.opt_mem "videoId" Jsont.json ~enc:(fun r -> r.video_id)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoComment" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoCommentThreadTree" jsont
  end

  (** Get a thread
      @param id The object id, uuid or short uuid
      @param thread_id The thread id (root comment id)
      @param x_peertube_video_password Required on password protected video
  *)
  let get_api_v1_videos_by_id_comment_threads_by_thread_id ~id ~thread_id ?x_peertube_video_password client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id); ("threadId", thread_id)] "/api/v1/videos/{id}/comment-threads/{threadId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in
    let __openapi_headers = match x_peertube_video_password with None -> __openapi_headers | Some value -> let cell = Fetch.Header.raw "x-peertube-video-password" value in Fetch.Header.(cell :: __openapi_headers) in
    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoCommentThreadTree\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoCommentThreadTree\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_videos_by_id_comment_threads_by_thread_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "CommentThreadPostResponse" jsont
  end

  (** Create a thread
      @param id The object id, uuid or short uuid
  *)
  let post_api_v1_videos_by_id_comment_threads ~id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/comment-threads" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"text\":{\"allOf\":[{\"$ref\":\"#/components/schemas/VideoComment/properties/text\"}],\"format\":\"markdown\",\"maxLength\":10000}},\"required\":[\"text\"]}" Jsont.json)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/CommentThreadPostResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/CommentThreadPostResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Response.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"post_api_v1_videos_by_id_comment_threads" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Reply to a thread of a video
      @param id The object id, uuid or short uuid
      @param comment_id The comment id
      @param x_peertube_video_password Required on password protected video
  *)
  let post_api_v1_videos_by_id_comments_by_comment_id ~id ~comment_id ?x_peertube_video_password ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id); ("commentId", comment_id)] "/api/v1/videos/{id}/comments/{commentId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"text\":{\"allOf\":[{\"$ref\":\"#/components/schemas/VideoComment/properties/text\"}],\"format\":\"markdown\",\"maxLength\":10000}},\"required\":[\"text\"]}" Jsont.json)) body in headers, Some body
    in
    let __openapi_headers = match x_peertube_video_password with None -> __openapi_headers | Some value -> let cell = Fetch.Header.raw "x-peertube-video-password" value in Fetch.Header.(cell :: __openapi_headers) in
    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/CommentThreadPostResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/CommentThreadPostResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Response.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"post_api_v1_videos_by_id_comments_by_comment_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST
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
      |> Jsont.Object.opt_mem "total" Openapi.Runtime.int_jsont ~enc:(fun r -> r.total)
      |> Jsont.Object.opt_mem "totalNotDeletedComments" Openapi.Runtime.int_jsont ~enc:(fun r -> r.total_not_deleted_comments)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "CommentThreadResponse" jsont
  end

  (** List threads of a video
      @param id The object id, uuid or short uuid
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort comments by criteria
      @param x_peertube_video_password Required on password protected video
  *)
  let get_api_v1_videos_by_id_comment_threads ~id ?start ?count ?sort ?x_peertube_video_password client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/comment-threads" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in
    let __openapi_headers = match x_peertube_video_password with None -> __openapi_headers | Some value -> let cell = Fetch.Header.raw "x-peertube-video-password" value in Fetch.Header.(cell :: __openapi_headers) in
    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/CommentThreadResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/CommentThreadResponse\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Response.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_videos_by_id_comment_threads" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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
        host_redundancy_allowed : bool option option;  (** whether this actor's host allows redundancy of its videos *)
        id : Id.T.t option;
        name : Username.T.t option;  (** immutable name of the actor, used to find or mention it *)
        url : string option;
        display_name : string option;  (** editable name of the channel, displayed in its representations *)
        description : string option option;
        support : string option option;  (** text shown by default on all videos of this channel, to tell the audience how to support it *)
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoChannelUpdate" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoChannelCreate" jsont
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
      |> Jsont.Object.opt_mem "followersCount" (Openapi.Runtime.validated_int ~minimum:0. Openapi.Runtime.int_jsont) ~enc:(fun r -> r.followers_count)
      |> Jsont.Object.opt_mem "followingCount" (Openapi.Runtime.validated_int ~minimum:0. Openapi.Runtime.int_jsont) ~enc:(fun r -> r.following_count)
      |> Jsont.Object.opt_mem "host" Jsont.string ~enc:(fun r -> r.host)
      |> Jsont.Object.opt_mem "hostRedundancyAllowed" (Jsont.option Jsont.bool) ~enc:(fun r -> r.host_redundancy_allowed)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "name" Username.T.jsont ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "url" Jsont.string ~enc:(fun r -> r.url)
      |> Jsont.Object.opt_mem "displayName" (Openapi.Runtime.validated_string ~min_length:1 ~max_length:120 Jsont.string) ~enc:(fun r -> r.display_name)
      |> Jsont.Object.opt_mem "description" (Jsont.option (Openapi.Runtime.validated_string ~min_length:3 ~max_length:1000 Jsont.string)) ~enc:(fun r -> r.description)
      |> Jsont.Object.opt_mem "support" (Jsont.option (Openapi.Runtime.validated_string ~min_length:3 ~max_length:1000 Jsont.string)) ~enc:(fun r -> r.support)
      |> Jsont.Object.opt_mem "isLocal" Jsont.bool ~enc:(fun r -> r.is_local)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "banners" (Jsont.list ActorImage.T.jsont) ~enc:(fun r -> r.banners)
      |> Jsont.Object.opt_mem "ownerAccount" Account.T.jsont ~enc:(fun r -> r.owner_account)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoChannel" jsont
  end

  (** Get subscription of my user
      @param subscription_handle The subscription handle
  *)
  let get_api_v1_users_me_subscriptions_by_subscription_handle ~subscription_handle client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("subscriptionHandle", subscription_handle)] "/api/v1/users/me/subscriptions/{subscriptionHandle}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoChannel\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoChannel\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_users_me_subscriptions_by_subscription_handle" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Get a video channel
      @param channel_handle The video channel handle
  *)
  let get_video_channel ~channel_handle client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoChannel\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoChannel\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_video_channel" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
end

module VideoDetails = struct
  module Types = struct
    module T = struct
      type t = {
        aspect_ratio : float option option;  (** **PeerTube >= 6.1** Aspect ratio of the video stream *)
        blacklisted : bool option option;
        blacklisted_reason : string option option;
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
        nsfw_summary : string option option;  (** **PeerTube >= 7.2** More information about the sensitive content of the video *)
        originally_published_at : Ptime.t option option;  (** used to represent a date of first publication, prior to the practical publication date of `publishedAt` *)
        preview_path : string option;
        privacy : VideoPrivacyConstant.T.t option;  (** privacy policy used to distribute the video *)
        published_at : Ptime.t option;  (** time at which the video was marked as ready for playback (with restrictions depending on `privacy`). Usually set after a `state` evolution. *)
        scheduled_update : VideoScheduled.Update.t option;
        short_uuid : ShortUuid.T.t option;
        state : VideoStateConstant.T.t option;  (** represents the internal state of the video processing within the PeerTube instance *)
        thumbnail_path : string option;
        truncated_description : string option option;  (** truncated description of the video, written in Markdown.
       *)
        updated_at : Ptime.t option;  (** last time the video's metadata was modified *)
        user_history : Jsont.json option option;
        uuid : Uuidv4.T.t option;  (** universal identifier for the video, that can be used across instances *)
        views : int option;
        wait_transcoding : bool option option;
        viewers : int option;  (** If the video is a live, you have the amount of current viewers *)
        description : string option option;  (** full description of the video, written in Markdown.
       *)
        support : string option option;  (** A text tell the audience how to support the video creator *)
        channel : VideoChannel.T.t option;
        account : Account.T.t option;
        tags : string list option;
        comments_policy : VideoCommentsPolicyConstant.T.t option;
        download_enabled : bool option;
        input_file_updated_at : Ptime.t option option;  (** Latest input file update. Null if the file has never been replaced since the original upload *)
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
      |> Jsont.Object.opt_mem "aspectRatio" (Jsont.option Openapi.Runtime.number_jsont) ~enc:(fun r -> r.aspect_ratio)
      |> Jsont.Object.opt_mem "blacklisted" (Jsont.option Jsont.bool) ~enc:(fun r -> r.blacklisted)
      |> Jsont.Object.opt_mem "blacklistedReason" (Jsont.option Jsont.string) ~enc:(fun r -> r.blacklisted_reason)
      |> Jsont.Object.opt_mem "category" VideoConstantNumberCategory.T.jsont ~enc:(fun r -> r.category)
      |> Jsont.Object.opt_mem "comments" Openapi.Runtime.int_jsont ~enc:(fun r -> r.comments)
      |> Jsont.Object.opt_mem "createdAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.created_at)
      |> Jsont.Object.opt_mem "dislikes" Openapi.Runtime.int_jsont ~enc:(fun r -> r.dislikes)
      |> Jsont.Object.opt_mem "duration" Openapi.Runtime.int_jsont ~enc:(fun r -> r.duration)
      |> Jsont.Object.opt_mem "embedPath" Jsont.string ~enc:(fun r -> r.embed_path)
      |> Jsont.Object.opt_mem "id" Id.T.jsont ~enc:(fun r -> r.id)
      |> Jsont.Object.opt_mem "isLive" Jsont.bool ~enc:(fun r -> r.is_live)
      |> Jsont.Object.opt_mem "isLocal" Jsont.bool ~enc:(fun r -> r.is_local)
      |> Jsont.Object.opt_mem "language" VideoConstantStringLanguage.T.jsont ~enc:(fun r -> r.language)
      |> Jsont.Object.opt_mem "licence" VideoConstantNumberLicence.T.jsont ~enc:(fun r -> r.licence)
      |> Jsont.Object.opt_mem "likes" Openapi.Runtime.int_jsont ~enc:(fun r -> r.likes)
      |> Jsont.Object.opt_mem "liveSchedules" (Jsont.list LiveSchedule.T.jsont) ~enc:(fun r -> r.live_schedules)
      |> Jsont.Object.opt_mem "name" (Openapi.Runtime.validated_string ~min_length:3 ~max_length:120 Jsont.string) ~enc:(fun r -> r.name)
      |> Jsont.Object.opt_mem "nsfw" Jsont.bool ~enc:(fun r -> r.nsfw)
      |> Jsont.Object.opt_mem "nsfwFlags" Nsfwflag.T.jsont ~enc:(fun r -> r.nsfw_flags)
      |> Jsont.Object.opt_mem "nsfwSummary" (Jsont.option Jsont.string) ~enc:(fun r -> r.nsfw_summary)
      |> Jsont.Object.opt_mem "originallyPublishedAt" (Jsont.option Openapi.Runtime.ptime_jsont) ~enc:(fun r -> r.originally_published_at)
      |> Jsont.Object.opt_mem "previewPath" Jsont.string ~enc:(fun r -> r.preview_path)
      |> Jsont.Object.opt_mem "privacy" VideoPrivacyConstant.T.jsont ~enc:(fun r -> r.privacy)
      |> Jsont.Object.opt_mem "publishedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.published_at)
      |> Jsont.Object.opt_mem "scheduledUpdate" VideoScheduled.Update.jsont ~enc:(fun r -> r.scheduled_update)
      |> Jsont.Object.opt_mem "shortUUID" ShortUuid.T.jsont ~enc:(fun r -> r.short_uuid)
      |> Jsont.Object.opt_mem "state" VideoStateConstant.T.jsont ~enc:(fun r -> r.state)
      |> Jsont.Object.opt_mem "thumbnailPath" Jsont.string ~enc:(fun r -> r.thumbnail_path)
      |> Jsont.Object.opt_mem "truncatedDescription" (Jsont.option (Openapi.Runtime.validated_string ~min_length:3 ~max_length:250 Jsont.string)) ~enc:(fun r -> r.truncated_description)
      |> Jsont.Object.opt_mem "updatedAt" Openapi.Runtime.ptime_jsont ~enc:(fun r -> r.updated_at)
      |> Jsont.Object.opt_mem "userHistory" (Jsont.option Jsont.json) ~enc:(fun r -> r.user_history)
      |> Jsont.Object.opt_mem "uuid" Uuidv4.T.jsont ~enc:(fun r -> r.uuid)
      |> Jsont.Object.opt_mem "views" Openapi.Runtime.int_jsont ~enc:(fun r -> r.views)
      |> Jsont.Object.opt_mem "waitTranscoding" (Jsont.option Jsont.bool) ~enc:(fun r -> r.wait_transcoding)
      |> Jsont.Object.opt_mem "viewers" Openapi.Runtime.int_jsont ~enc:(fun r -> r.viewers)
      |> Jsont.Object.opt_mem "description" (Jsont.option (Openapi.Runtime.validated_string ~min_length:3 ~max_length:1000 Jsont.string)) ~enc:(fun r -> r.description)
      |> Jsont.Object.opt_mem "support" (Jsont.option (Openapi.Runtime.validated_string ~min_length:3 ~max_length:1000 Jsont.string)) ~enc:(fun r -> r.support)
      |> Jsont.Object.opt_mem "channel" VideoChannel.T.jsont ~enc:(fun r -> r.channel)
      |> Jsont.Object.opt_mem "account" Account.T.jsont ~enc:(fun r -> r.account)
      |> Jsont.Object.opt_mem "tags" (Openapi.Runtime.validated_list ~min_items:1 ~max_items:5 Jsont.string) ~enc:(fun r -> r.tags)
      |> Jsont.Object.opt_mem "commentsPolicy" VideoCommentsPolicyConstant.T.jsont ~enc:(fun r -> r.comments_policy)
      |> Jsont.Object.opt_mem "downloadEnabled" Jsont.bool ~enc:(fun r -> r.download_enabled)
      |> Jsont.Object.opt_mem "inputFileUpdatedAt" (Jsont.option Openapi.Runtime.ptime_jsont) ~enc:(fun r -> r.input_file_updated_at)
      |> Jsont.Object.opt_mem "trackerUrls" (Jsont.list Jsont.string) ~enc:(fun r -> r.tracker_urls)
      |> Jsont.Object.opt_mem "files" (Jsont.list VideoFile.T.jsont) ~enc:(fun r -> r.files)
      |> Jsont.Object.opt_mem "streamingPlaylists" (Jsont.list VideoStreamingPlaylists.T.jsont) ~enc:(fun r -> r.streaming_playlists)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoDetails" jsont
  end

  (** Get a video
      @param id The object id, uuid or short uuid
      @param x_peertube_video_password Required on password protected video
  *)
  let get_video ~id ?x_peertube_video_password client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in
    let __openapi_headers = match x_peertube_video_password with None -> __openapi_headers | Some value -> let cell = Fetch.Header.raw "x-peertube-video-password" value in Fetch.Header.(cell :: __openapi_headers) in
    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoDetails\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoDetails\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("401", Openapi.Runtime.Client.typed_error "ServerError" ServerError.T.jsont); ("403", Openapi.Runtime.Client.typed_error "ServerError" ServerError.T.jsont)]
      ~operation:"get_video" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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
        last_sync_at : Ptime.t option option;
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoChannelSyncCreate" jsont
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
      |> Jsont.Object.opt_mem "lastSyncAt" (Jsont.option Openapi.Runtime.ptime_jsont) ~enc:(fun r -> r.last_sync_at)
      |> Jsont.Object.opt_mem "state" Jsont.json ~enc:(fun r -> r.state)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoChannelSync" jsont
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
      |> Jsont.Object.opt_mem "total" Openapi.Runtime.int_jsont ~enc:(fun r -> r.total)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoChannelSyncList" jsont
  end

  (** List the synchronizations of video channels of an account
      @param name The username or handle of the account
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
      @param include_collaborations **PeerTube >= 8.0** Include objects from collaborated channels
  *)
  let get_api_v1_accounts_by_name_video_channel_syncs ~name ?start ?count ?sort ?include_collaborations client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("name", name)] "/api/v1/accounts/{name}/video-channel-syncs" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"includeCollaborations" ~value:include_collaborations]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoChannelSyncList\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoChannelSyncList\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_accounts_by_name_video_channel_syncs" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/abuses" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"id" ~value:id; Openapi.Runtime.Query.optional ~key:"predefinedReason" ~value:predefined_reason; Openapi.Runtime.Query.optional ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"state" ~value:state; Openapi.Runtime.Query.optional ~key:"searchReporter" ~value:search_reporter; Openapi.Runtime.Query.optional ~key:"searchReportee" ~value:search_reportee; Openapi.Runtime.Query.optional ~key:"searchVideo" ~value:search_video; Openapi.Runtime.Query.optional ~key:"searchVideoChannel" ~value:search_video_channel; Openapi.Runtime.Query.optional ~key:"videoIs" ~value:video_is; Openapi.Runtime.Query.optional ~key:"filter" ~value:filter; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Abuse\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Abuse\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_abuses" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Report an abuse *)
  let post_api_v1_abuses ~body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/abuses" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"account\":{\"type\":\"object\",\"properties\":{\"id\":{\"description\":\"Account id to report\",\"type\":\"integer\"}}},\"comment\":{\"type\":\"object\",\"properties\":{\"id\":{\"description\":\"Comment id to report\",\"allOf\":[{\"$ref\":\"#/components/schemas/VideoComment/properties/id\"}]}}},\"predefinedReasons\":{\"$ref\":\"#/components/schemas/PredefinedAbuseReasons\"},\"reason\":{\"description\":\"Reason why the user reports this video\",\"type\":\"string\",\"minLength\":2,\"maxLength\":3000},\"video\":{\"type\":\"object\",\"properties\":{\"id\":{\"description\":\"Video id to report\",\"allOf\":[{\"$ref\":\"#/components/schemas/Video/properties/id\"}]},\"startAt\":{\"type\":\"integer\",\"format\":\"seconds\",\"description\":\"Timestamp in the video that marks the beginning of the report\",\"minimum\":0},\"endAt\":{\"type\":\"integer\",\"format\":\"seconds\",\"description\":\"Timestamp in the video that marks the ending of the report\",\"minimum\":0}}}},\"required\":[\"reason\"]}" Jsont.json)) body in
    let __openapi_body = Some __openapi_body in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"abuse\":{\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/id\"}}}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"abuse\":{\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/id\"}}}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None))]
      ~operation:"post_api_v1_abuses" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Update an abuse
      @param abuse_id Abuse id
  *)
  let put_api_v1_abuses_by_abuse_id ~abuse_id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("abuseId", abuse_id)] "/api/v1/abuses/{abuseId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"moderationComment\":{\"type\":\"string\",\"description\":\"Update the report comment visible only to the moderation team\",\"minLength\":2,\"maxLength\":3000},\"state\":{\"$ref\":\"#/components/schemas/AbuseStateSet\"}},\"required\":[]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"put_api_v1_abuses_by_abuse_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** Delete an abuse
      @param abuse_id Abuse id
  *)
  let delete_api_v1_abuses_by_abuse_id ~abuse_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("abuseId", abuse_id)] "/api/v1/abuses/{abuseId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"delete_api_v1_abuses_by_abuse_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** List messages of an abuse
      @param abuse_id Abuse id
  *)
  let get_api_v1_abuses_by_abuse_id_messages ~abuse_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("abuseId", abuse_id)] "/api/v1/abuses/{abuseId}/messages" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/AbuseMessage\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/AbuseMessage\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_abuses_by_abuse_id_messages" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Add message to an abuse
      @param abuse_id Abuse id
  *)
  let post_api_v1_abuses_by_abuse_id_messages ~abuse_id ~body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("abuseId", abuse_id)] "/api/v1/abuses/{abuseId}/messages" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"message\":{\"description\":\"Message to send\",\"type\":\"string\",\"minLength\":2,\"maxLength\":3000}},\"required\":[\"message\"]}" Jsont.json)) body in
    let __openapi_body = Some __openapi_body in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None))]
      ~operation:"post_api_v1_abuses_by_abuse_id_messages" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Delete an abuse message
      @param abuse_id Abuse id
      @param abuse_message_id Abuse message id
  *)
  let delete_api_v1_abuses_by_abuse_id_messages_by_abuse_message_id ~abuse_id ~abuse_message_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("abuseId", abuse_id); ("abuseMessageId", abuse_message_id)] "/api/v1/abuses/{abuseId}/messages/{abuseMessageId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"delete_api_v1_abuses_by_abuse_id_messages_by_abuse_message_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** List accounts
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_accounts ?start ?count ?sort client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/accounts" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Account\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Account\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_accounts" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** List followers of an account
      @param name The username or handle of the account
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort followers by criteria
      @param search Plain text search, applied to various parts of the model depending on endpoint
  *)
  let get_account_followers ~name ?start ?count ?sort ?search client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("name", name)] "/api/v1/accounts/{name}/followers" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"search" ~value:search]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Follow\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Follow\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_account_followers" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** List playlists of an account
      @param name The username or handle of the account
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
      @param search Plain text search, applied to various parts of the model depending on endpoint
      @param include_collaborations **PeerTube >= 8.0** Include objects from collaborated channels
      @param channel_name_one_of **PeerTube >= 8.0** Filter on playlists that are published on a channel with one of these names
  *)
  let get_api_v1_accounts_by_name_video_playlists ~name ?start ?count ?sort ?search ?playlist_type ?include_collaborations ?channel_name_one_of client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("name", name)] "/api/v1/accounts/{name}/video-playlists" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"playlistType" ~value:playlist_type; Openapi.Runtime.Query.optional ~key:"includeCollaborations" ~value:include_collaborations; Openapi.Runtime.Query.optional ~key:"channelNameOneOf" ~value:channel_name_one_of]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoPlaylist\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoPlaylist\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_accounts_by_name_video_playlists" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Update account auto tag policies on comments

      **PeerTube >= 6.2**
      @param account_name account name to update auto tag policies
  *)
  let put_api_v1_automatic_tags_policies_accounts_by_account_name_comments ~account_name ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("accountName", account_name)] "/api/v1/automatic-tags/policies/accounts/{accountName}/comments" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"review\":{\"description\":\"Auto tags that automatically set the comment in review state\",\"type\":\"array\",\"items\":{\"type\":\"string\"}}},\"required\":[]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"put_api_v1_automatic_tags_policies_accounts_by_account_name_comments" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** Update client language

      Set a cookie so that, the next time the client refreshes the HTML of the web interface, PeerTube will use the next language *)
  let update_client_language ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/client-config/update-language" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"language\":{\"type\":\"string\",\"description\":\"Language code to set\",\"example\":\"en-US\"}},\"required\":[\"language\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"update_client_language" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Set instance runtime configuration *)
  let put_custom_config client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/config/custom" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None))]
      ~operation:"put_custom_config" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** Delete instance runtime configuration *)
  let del_custom_config client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/config/custom" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"del_custom_config" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Delete instance avatar *)
  let delete_api_v1_config_instance_avatar client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/config/instance-avatar" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"delete_api_v1_config_instance_avatar" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Update instance avatar *)
  let post_api_v1_config_instance_avatar_pick ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/config/instance-avatar/pick" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.Form.multipart body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("413", (fun _ -> None))]
      ~operation:"post_api_v1_config_instance_avatar_pick" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Delete instance banner *)
  let delete_api_v1_config_instance_banner client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/config/instance-banner" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"delete_api_v1_config_instance_banner" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Update instance banner *)
  let post_api_v1_config_instance_banner_pick ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/config/instance-banner/pick" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.Form.multipart body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("413", (fun _ -> None))]
      ~operation:"post_api_v1_config_instance_banner_pick" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Delete instance logo *)
  let delete_api_v1_config_instance_logo_logo_type client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/config/instance-logo/:logoType" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"delete_api_v1_config_instance_logo_logo_type" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Update instance logo *)
  let post_api_v1_config_instance_logo_logo_type_pick ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/config/instance-logo/:logoType/pick" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.Form.multipart body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("413", (fun _ -> None))]
      ~operation:"post_api_v1_config_instance_logo_logo_type_pick" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Set instance custom homepage *)
  let put_api_v1_custom_pages_homepage_instance ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/custom-pages/homepage/instance" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"content\":{\"type\":\"string\",\"description\":\"content of the homepage, that will be injected in the client\"}},\"required\":[]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"put_api_v1_custom_pages_homepage_instance" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** Pause job queue *)
  let post_api_v1_jobs_pause client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/jobs/pause" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_jobs_pause" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Resume job queue *)
  let post_api_v1_jobs_resume client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/jobs/resume" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_jobs_resume" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** List instance jobs
      @param state The state of the job ('' for for no filter)
      @param job_type job type
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_jobs ~state ?job_type ?start ?count ?sort client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("state", state)] "/api/v1/jobs/{state}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"jobType" ~value:job_type; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"maxItems\":100,\"items\":{\"$ref\":\"#/components/schemas/Job\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"maxItems\":100,\"items\":{\"$ref\":\"#/components/schemas/Job\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_jobs" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Create playback metrics

      These metrics are exposed by OpenTelemetry metrics exporter if enabled. *)
  let post_api_v1_metrics_playback ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/metrics/playback" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/PlaybackMetricCreate\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" PlaybackMetric.Create.jsont)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_metrics_playback" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Install a plugin *)
  let add_plugin ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/plugins/install" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"oneOf\":[{\"type\":\"object\",\"properties\":{\"npmName\":{\"type\":\"string\",\"example\":\"peertube-plugin-auth-ldap\"}},\"required\":[\"npmName\"],\"additionalProperties\":false},{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"}},\"required\":[\"path\"],\"additionalProperties\":false}],\"properties\":{},\"required\":[]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None))]
      ~operation:"add_plugin" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Uninstall a plugin *)
  let uninstall_plugin ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/plugins/uninstall" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"npmName\":{\"type\":\"string\",\"description\":\"name of the plugin/theme in its package.json\",\"example\":\"peertube-plugin-auth-ldap\"}},\"required\":[\"npmName\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"uninstall_plugin" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Update a plugin *)
  let update_plugin ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/plugins/update" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"oneOf\":[{\"type\":\"object\",\"properties\":{\"npmName\":{\"type\":\"string\",\"example\":\"peertube-plugin-auth-ldap\"}},\"required\":[\"npmName\"],\"additionalProperties\":false},{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"}},\"required\":[\"path\"],\"additionalProperties\":false}],\"properties\":{},\"required\":[]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None)); ("404", (fun _ -> None))]
      ~operation:"update_plugin" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Get a plugin's public settings
      @param npm_name name of the plugin/theme on npmjs.com or in its package.json
  *)
  let get_api_v1_plugins_by_npm_name_public_settings ~npm_name client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("npmName", npm_name)] "/api/v1/plugins/{npmName}/public-settings" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"additionalProperties\":true}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"additionalProperties\":true}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"get_api_v1_plugins_by_npm_name_public_settings" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Get a plugin's registered settings
      @param npm_name name of the plugin/theme on npmjs.com or in its package.json
  *)
  let get_api_v1_plugins_by_npm_name_registered_settings ~npm_name client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("npmName", npm_name)] "/api/v1/plugins/{npmName}/registered-settings" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"additionalProperties\":true}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"additionalProperties\":true}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"get_api_v1_plugins_by_npm_name_registered_settings" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Set a plugin's settings
      @param npm_name name of the plugin/theme on npmjs.com or in its package.json
  *)
  let put_api_v1_plugins_by_npm_name_settings ~npm_name ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("npmName", npm_name)] "/api/v1/plugins/{npmName}/settings" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"settings\":{\"type\":\"object\",\"additionalProperties\":true}},\"required\":[]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"put_api_v1_plugins_by_npm_name_settings" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** List runners
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort runners by criteria
  *)
  let get_api_v1_runners ?start ?count ?sort client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/runners" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Runner\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Runner\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_runners" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** List jobs
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort runner jobs by criteria
      @param search Plain text search, applied to various parts of the model depending on endpoint
  *)
  let get_api_v1_runners_jobs ?start ?count ?sort ?search ?state_one_of client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/runners/jobs" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"stateOneOf" ~value:state_one_of]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/RunnerJobAdmin\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/RunnerJobAdmin\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_runners_jobs" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Request a new job

      API used by PeerTube runners *)
  let post_api_v1_runners_jobs_request ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/runners/jobs/request" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"jobTypes\":{\"type\":\"array\",\"description\":\"Filter jobs depending on their types\",\"items\":{\"type\":\"string\"}},\"runnerToken\":{\"type\":\"string\"}},\"required\":[\"runnerToken\"]}" Jsont.json)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"availableJobs\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"uuid\":{\"$ref\":\"#/components/schemas/UUIDv4\"},\"type\":{\"$ref\":\"#/components/schemas/RunnerJobType\"},\"payload\":{\"$ref\":\"#/components/schemas/RunnerJobPayload\"}}}}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"availableJobs\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"uuid\":{\"$ref\":\"#/components/schemas/UUIDv4\"},\"type\":{\"$ref\":\"#/components/schemas/RunnerJobType\"},\"payload\":{\"$ref\":\"#/components/schemas/RunnerJobPayload\"}}}}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_runners_jobs_request" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Delete a job

      The endpoint will first cancel the job if needed, and then remove it from the database. Children jobs will also be removed *)
  let delete_api_v1_runners_jobs_by_job_uuid ~job_uuid client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("jobUUID", job_uuid)] "/api/v1/runners/jobs/{jobUUID}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"delete_api_v1_runners_jobs_by_job_uuid" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Abort job

      API used by PeerTube runners *)
  let post_api_v1_runners_jobs_by_job_uuid_abort ~job_uuid ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("jobUUID", job_uuid)] "/api/v1/runners/jobs/{jobUUID}/abort" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"jobToken\":{\"type\":\"string\"},\"reason\":{\"type\":\"string\",\"description\":\"Why the runner aborts this job\"},\"runnerToken\":{\"type\":\"string\"}},\"required\":[\"runnerToken\",\"jobToken\",\"reason\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_runners_jobs_by_job_uuid_abort" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Accept job

      API used by PeerTube runners *)
  let post_api_v1_runners_jobs_by_job_uuid_accept ~job_uuid ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("jobUUID", job_uuid)] "/api/v1/runners/jobs/{jobUUID}/accept" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"runnerToken\":{\"type\":\"string\"}},\"required\":[\"runnerToken\"]}" Jsont.json)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"job\":{\"allOf\":[{\"$ref\":\"#/components/schemas/RunnerJob\"},{\"type\":\"object\",\"properties\":{\"jobToken\":{\"type\":\"string\"}}}]}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"job\":{\"allOf\":[{\"$ref\":\"#/components/schemas/RunnerJob\"},{\"type\":\"object\",\"properties\":{\"jobToken\":{\"type\":\"string\"}}}]}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_runners_jobs_by_job_uuid_accept" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Cancel a job *)
  let get_api_v1_runners_jobs_by_job_uuid_cancel ~job_uuid client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("jobUUID", job_uuid)] "/api/v1/runners/jobs/{jobUUID}/cancel" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_runners_jobs_by_job_uuid_cancel" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Post job error

      API used by PeerTube runners *)
  let post_api_v1_runners_jobs_by_job_uuid_error ~job_uuid ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("jobUUID", job_uuid)] "/api/v1/runners/jobs/{jobUUID}/error" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"jobToken\":{\"type\":\"string\"},\"message\":{\"type\":\"string\",\"description\":\"Why the runner failed to process this job\"},\"runnerToken\":{\"type\":\"string\"}},\"required\":[\"runnerToken\",\"jobToken\",\"message\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_runners_jobs_by_job_uuid_error" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Post job success

      API used by PeerTube runners *)
  let post_api_v1_runners_jobs_by_job_uuid_success ~job_uuid ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("jobUUID", job_uuid)] "/api/v1/runners/jobs/{jobUUID}/success" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"jobToken\":{\"type\":\"string\"},\"payload\":{\"anyOf\":[{\"type\":\"object\",\"title\":\"VOD web video transcoding\",\"properties\":{\"videoFile\":{\"type\":\"string\",\"format\":\"binary\"}}},{\"type\":\"object\",\"title\":\"VOD HLS transcoding\",\"properties\":{\"videoFile\":{\"type\":\"string\",\"format\":\"binary\"},\"resolutionPlaylistFile\":{\"type\":\"string\",\"format\":\"binary\"}}},{\"type\":\"object\",\"title\":\"VOD audio merge transcoding\",\"properties\":{\"videoFile\":{\"type\":\"string\",\"format\":\"binary\"}}},{\"type\":\"object\",\"title\":\"Live RTMP to HLS transcoding\"}]},\"runnerToken\":{\"type\":\"string\"}},\"required\":[\"runnerToken\",\"jobToken\",\"payload\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_runners_jobs_by_job_uuid_success" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Update job

      API used by PeerTube runners *)
  let post_api_v1_runners_jobs_by_job_uuid_update ~job_uuid ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("jobUUID", job_uuid)] "/api/v1/runners/jobs/{jobUUID}/update" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"jobToken\":{\"type\":\"string\"},\"payload\":{\"anyOf\":[{\"type\":\"object\",\"description\":\"Provide live transcoding chunks update\",\"properties\":{\"type\":{\"type\":\"string\",\"enum\":[\"add-chunk\",\"remove-chunk\"]},\"masterPlaylistFile\":{\"type\":\"string\",\"format\":\"binary\"},\"resolutionPlaylistFile\":{\"type\":\"string\",\"format\":\"binary\"},\"resolutionPlaylistFilename\":{\"type\":\"string\"},\"videoChunkFile\":{\"type\":\"string\",\"format\":\"binary\"},\"videoChunkFilename\":{\"type\":\"string\"}}}]},\"progress\":{\"type\":\"integer\",\"description\":\"Update job progression percentage (optional)\"},\"runnerToken\":{\"type\":\"string\"}},\"required\":[\"runnerToken\",\"jobToken\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_runners_jobs_by_job_uuid_update" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Register a new runner

      API used by PeerTube runners *)
  let post_api_v1_runners_register ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/runners/register" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"description\":{\"type\":\"string\"},\"name\":{\"type\":\"string\"},\"registrationToken\":{\"type\":\"string\"}},\"required\":[\"registrationToken\",\"name\"]}" Jsont.json)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"id\":{\"type\":\"integer\",\"description\":\"Runner id\"},\"runnerToken\":{\"type\":\"string\"}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"id\":{\"type\":\"integer\",\"description\":\"Runner id\"},\"runnerToken\":{\"type\":\"string\"}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_runners_register" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** List registration tokens
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort registration tokens by criteria
  *)
  let get_api_v1_runners_registration_tokens ?start ?count ?sort client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/runners/registration-tokens" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/RunnerRegistrationToken\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/RunnerRegistrationToken\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_runners_registration_tokens" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Generate registration token

      Generate a new runner registration token *)
  let post_api_v1_runners_registration_tokens_generate client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/runners/registration-tokens/generate" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_runners_registration_tokens_generate" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Remove registration token

      Remove a registration token. Runners that used this token for their registration are automatically removed. *)
  let delete_api_v1_runners_registration_tokens_by_registration_token_id ~registration_token_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registrationTokenId", registration_token_id)] "/api/v1/runners/registration-tokens/{registrationTokenId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"delete_api_v1_runners_registration_tokens_by_registration_token_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Unregister a runner

      API used by PeerTube runners *)
  let post_api_v1_runners_unregister ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/runners/unregister" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"runnerToken\":{\"type\":\"string\"}},\"required\":[\"runnerToken\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_runners_unregister" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Delete a runner *)
  let delete_api_v1_runners_by_runner_id ~runner_id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("runnerId", runner_id)] "/api/v1/runners/{runnerId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"runnerToken\":{\"type\":\"string\"}},\"required\":[\"runnerToken\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"delete_api_v1_runners_by_runner_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

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
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/search/video-playlists" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.singleton ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"searchTarget" ~value:search_target; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"host" ~value:host; Openapi.Runtime.Query.optional ~key:"uuids" ~value:uuids]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoPlaylist\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoPlaylist\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("500", (fun _ -> None))]
      ~operation:"search_playlists" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Get instance audit logs *)
  let get_instance_audit_logs client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/server/audit-logs" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"string\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"string\"}}" (Jsont.list Jsont.string)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_instance_audit_logs" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** List account blocks
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_api_v1_server_blocklist_accounts ?start ?count ?sort client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/server/blocklist/accounts" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_server_blocklist_accounts" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Block an account *)
  let post_api_v1_server_blocklist_accounts ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/server/blocklist/accounts" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"accountName\":{\"type\":\"string\",\"example\":\"chocobozzz@example.org\",\"description\":\"account to block, in the form `username@domain`\"}},\"required\":[\"accountName\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("409", (fun _ -> None))]
      ~operation:"post_api_v1_server_blocklist_accounts" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Unblock an account by its handle
      @param account_name account to unblock, in the form `username@domain`
  *)
  let delete_api_v1_server_blocklist_accounts_by_account_name ~account_name client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("accountName", account_name)] "/api/v1/server/blocklist/accounts/{accountName}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"delete_api_v1_server_blocklist_accounts_by_account_name" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** List server blocks
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_api_v1_server_blocklist_servers ?start ?count ?sort client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/server/blocklist/servers" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_server_blocklist_servers" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Block a server *)
  let post_api_v1_server_blocklist_servers ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/server/blocklist/servers" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"host\":{\"type\":\"string\",\"format\":\"hostname\",\"description\":\"server domain to block\"}},\"required\":[\"host\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("409", (fun _ -> None))]
      ~operation:"post_api_v1_server_blocklist_servers" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Unblock a server by its domain
      @param host server domain to unblock
  *)
  let delete_api_v1_server_blocklist_servers_by_host ~host client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("host", host)] "/api/v1/server/blocklist/servers/{host}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"delete_api_v1_server_blocklist_servers_by_host" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** List instances following the server
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_api_v1_server_followers ?state ?actor_type ?start ?count ?sort client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/server/followers" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"state" ~value:state; Openapi.Runtime.Query.optional ~key:"actorType" ~value:actor_type; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Follow\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Follow\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_server_followers" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Remove or reject a follower to your server
      @param handle The remote actor handle to remove from your followers
  *)
  let delete_api_v1_server_followers_by_handle ~handle client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("handle", handle)] "/api/v1/server/followers/{handle}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"delete_api_v1_server_followers_by_handle" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Accept a pending follower to your server
      @param handle The remote actor handle to remove from your followers
  *)
  let post_api_v1_server_followers_by_handle_accept ~handle client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("handle", handle)] "/api/v1/server/followers/{handle}/accept" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"post_api_v1_server_followers_by_handle_accept" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Reject a pending follower to your server
      @param handle The remote actor handle to remove from your followers
  *)
  let post_api_v1_server_followers_by_handle_reject ~handle client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("handle", handle)] "/api/v1/server/followers/{handle}/reject" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"post_api_v1_server_followers_by_handle_reject" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** List instances followed by the server
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_api_v1_server_following ?state ?actor_type ?start ?count ?sort client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/server/following" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"state" ~value:state; Openapi.Runtime.Query.optional ~key:"actorType" ~value:actor_type; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Follow\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Follow\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_server_following" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Follow a list of actors (PeerTube instance, channel or account) *)
  let post_api_v1_server_following ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/server/following" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"handles\":{\"type\":\"array\",\"items\":{\"type\":\"string\"},\"uniqueItems\":true},\"hosts\":{\"type\":\"array\",\"items\":{\"type\":\"string\",\"format\":\"hostname\"},\"uniqueItems\":true}},\"required\":[]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("500", (fun _ -> None))]
      ~operation:"post_api_v1_server_following" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Unfollow an actor (PeerTube instance, channel or account)
      @param host_or_handle The hostOrHandle to unfollow
  *)
  let delete_api_v1_server_following_by_host_or_handle ~host_or_handle client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("hostOrHandle", host_or_handle)] "/api/v1/server/following/{hostOrHandle}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"delete_api_v1_server_following_by_host_or_handle" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Get instance logs *)
  let get_instance_logs client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/server/logs" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"string\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"string\"}}" (Jsont.list Jsont.string)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_instance_logs" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Send client log *)
  let send_client_log ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/server/logs/client" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/SendClientLog\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" SendClientLog.T.jsont)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"send_client_log" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Mirror a video *)
  let put_mirrored_video ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/server/redundancy/videos" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"videoId\":{\"$ref\":\"#/components/schemas/Video/properties/id\"}},\"required\":[\"videoId\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None)); ("404", (fun _ -> None)); ("409", (fun _ -> None))]
      ~operation:"put_mirrored_video" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Delete a mirror done on a video
      @param redundancy_id id of an existing redundancy on a video
  *)
  let del_mirrored_video ~redundancy_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("redundancyId", redundancy_id)] "/api/v1/server/redundancy/videos/{redundancyId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"del_mirrored_video" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Update a server redundancy policy
      @param host server domain to mirror
  *)
  let put_api_v1_server_redundancy_by_host ~host ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("host", host)] "/api/v1/server/redundancy/{host}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"redundancyAllowed\":{\"type\":\"boolean\",\"description\":\"allow mirroring of the host's local videos\"}},\"required\":[\"redundancyAllowed\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"put_api_v1_server_redundancy_by_host" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** Ask to reset password

      An email containing a reset password link *)
  let post_api_v1_users_ask_reset_password ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/ask-reset-password" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"email\":{\"type\":\"string\",\"description\":\"User email\"}},\"required\":[\"email\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_users_ask_reset_password" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Resend user verification link *)
  let resend_email_to_verify_user ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/ask-send-verify-email" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"email\":{\"type\":\"string\",\"description\":\"User email\"}},\"required\":[\"email\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"resend_email_to_verify_user" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Update my user information *)
  let put_user_info ~body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/me" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/UpdateMe\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" UpdateMe.T.jsont)) body in
    let __openapi_body = Some __openapi_body in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"put_user_info" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** List my abuses
      @param id only list the report with this id
      @param sort Sort abuses by criteria
      @param start Offset used to paginate results
      @param count Number of items to return
  *)
  let get_my_abuses ?id ?state ?sort ?start ?count client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/me/abuses" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"id" ~value:id; Openapi.Runtime.Query.optional ~key:"state" ~value:state; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Abuse\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Abuse\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_my_abuses" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Delete my avatar *)
  let delete_api_v1_users_me_avatar client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/me/avatar" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"delete_api_v1_users_me_avatar" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Update my user avatar *)
  let post_api_v1_users_me_avatar_pick ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/me/avatar/pick" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.Form.multipart body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"avatars\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/ActorImage\"}}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"avatars\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/ActorImage\"}}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("413", (fun _ -> None))]
      ~operation:"post_api_v1_users_me_avatar_pick" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Clear video history *)
  let post_api_v1_users_me_history_videos_remove ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/me/history/videos/remove" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.Form.multipart body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_users_me_history_videos_remove" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Delete history element *)
  let delete_api_v1_users_me_history_videos_by_video_id ~video_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("videoId", video_id)] "/api/v1/users/me/history/videos/{videoId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"delete_api_v1_users_me_history_videos_by_video_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Mark feature info as read

      **PeerTube >= v8.0.0 *)
  let post_api_v1_users_me_new_feature_info_read ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/me/new-feature-info/read" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"feature\":{\"$ref\":\"#/components/schemas/NewFeatureInfoType\"}},\"required\":[\"feature\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_users_me_new_feature_info_read" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Update my notification settings *)
  let put_api_v1_users_me_notification_settings ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/me/notification-settings" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/UserNotificationSettings\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" UserNotificationSettings.T.jsont)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"put_api_v1_users_me_notification_settings" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** Mark notifications as read by their id *)
  let post_api_v1_users_me_notifications_read ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/me/notifications/read" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"ids\":{\"type\":\"array\",\"description\":\"ids of the notifications to mark as read\",\"items\":{\"type\":\"integer\"}}},\"required\":[\"ids\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_users_me_notifications_read" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Mark all my notification as read *)
  let post_api_v1_users_me_notifications_read_all client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/me/notifications/read-all" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_users_me_notifications_read_all" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Add subscription to my user *)
  let post_api_v1_users_me_subscriptions ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/me/subscriptions" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"uri\":{\"type\":\"string\",\"format\":\"uri\",\"description\":\"uri of the video channels to subscribe to\"}},\"required\":[\"uri\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_users_me_subscriptions" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Get if subscriptions exist for my user
      @param uris list of uris to check if each is part of the user subscriptions
  *)
  let get_api_v1_users_me_subscriptions_exist ~uris client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/me/subscriptions/exist" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.singleton ~key:"uris" ~value:uris]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_users_me_subscriptions_exist" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Delete subscription of my user
      @param subscription_handle The subscription handle
  *)
  let delete_api_v1_users_me_subscriptions_by_subscription_handle ~subscription_handle client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("subscriptionHandle", subscription_handle)] "/api/v1/users/me/subscriptions/{subscriptionHandle}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"delete_api_v1_users_me_subscriptions_by_subscription_handle" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Check video exists in my playlists
      @param video_ids The video ids to check
  *)
  let get_api_v1_users_me_video_playlists_videos_exist ~video_ids client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/me/video-playlists/videos-exist" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.singleton ~key:"videoIds" ~value:video_ids]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"videoId\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"playlistElementId\":{\"type\":\"integer\"},\"playlistId\":{\"type\":\"integer\"},\"startTimestamp\":{\"type\":\"integer\",\"format\":\"seconds\",\"nullable\":true},\"stopTimestamp\":{\"type\":\"integer\",\"format\":\"seconds\",\"nullable\":true}}}}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"videoId\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"playlistElementId\":{\"type\":\"integer\"},\"playlistId\":{\"type\":\"integer\"},\"startTimestamp\":{\"type\":\"integer\",\"format\":\"seconds\",\"nullable\":true},\"stopTimestamp\":{\"type\":\"integer\",\"format\":\"seconds\",\"nullable\":true}}}}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_users_me_video_playlists_videos_exist" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Get my user used quota *)
  let get_api_v1_users_me_video_quota_used client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/me/video-quota-used" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"videoQuotaUsed\":{\"type\":\"number\",\"description\":\"The user video quota used so far in bytes\",\"example\":16810141515},\"videoQuotaUsedDaily\":{\"type\":\"number\",\"description\":\"The user video quota used today in bytes\",\"example\":1681014151}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"videoQuotaUsed\":{\"type\":\"number\",\"description\":\"The user video quota used so far in bytes\",\"example\":16810141515},\"videoQuotaUsedDaily\":{\"type\":\"number\",\"description\":\"The user video quota used today in bytes\",\"example\":1681014151}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_users_me_video_quota_used" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

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
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/me/videos/comments" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"searchAccount" ~value:search_account; Openapi.Runtime.Query.optional ~key:"searchVideo" ~value:search_video; Openapi.Runtime.Query.optional ~key:"videoId" ~value:video_id; Openapi.Runtime.Query.optional ~key:"videoChannelId" ~value:video_channel_id; Openapi.Runtime.Query.optional ~key:"autoTagOneOf" ~value:auto_tag_one_of; Openapi.Runtime.Query.optional ~key:"isHeldForReview" ~value:is_held_for_review; Openapi.Runtime.Query.optional ~key:"includeCollaborations" ~value:include_collaborations]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoCommentForOwnerOrAdmin\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoCommentForOwnerOrAdmin\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_users_me_videos_comments" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Register a user

      Signup has to be enabled and signup approval is not required *)
  let register_user ~body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/register" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/RegisterUser\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" RegisterUser.T.jsont)) body in
    let __openapi_body = Some __openapi_body in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None)); ("403", (fun _ -> None)); ("409", (fun _ -> None))]
      ~operation:"register_user" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** List registrations
      @param start Offset used to paginate results
      @param count Number of items to return
  *)
  let list_registrations ?start ?count ?search ?sort client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/registrations" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/UserRegistration\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/UserRegistration\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"list_registrations" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Resend verification link to registration request email *)
  let resend_email_to_verify_registration ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/registrations/ask-send-verify-email" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"email\":{\"type\":\"string\",\"description\":\"Registration email\"}},\"required\":[\"email\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"resend_email_to_verify_registration" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Delete registration

      Delete the registration entry. It will not remove the user associated with this registration (if any)
      @param registration_id Registration ID
  *)
  let delete_registration ~registration_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registrationId", registration_id)] "/api/v1/users/registrations/{registrationId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"delete_registration" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Accept registration
      @param registration_id Registration ID
  *)
  let accept_registration ~registration_id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registrationId", registration_id)] "/api/v1/users/registrations/{registrationId}/accept" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/UserRegistrationAcceptOrReject\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" UserRegistrationAcceptOrReject.T.jsont)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"accept_registration" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Reject registration
      @param registration_id Registration ID
  *)
  let reject_registration ~registration_id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registrationId", registration_id)] "/api/v1/users/registrations/{registrationId}/reject" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/UserRegistrationAcceptOrReject\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" UserRegistrationAcceptOrReject.T.jsont)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"reject_registration" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Verify a registration email

      Following a user registration request, the user will receive an email asking to click a link
  containing a secret.

      @param registration_id Registration ID
  *)
  let verify_registration_email ~registration_id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("registrationId", registration_id)] "/api/v1/users/registrations/{registrationId}/verify-email" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"verificationString\":{\"type\":\"string\",\"format\":\"url\"}},\"required\":[\"verificationString\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("403", (fun _ -> None)); ("404", (fun _ -> None))]
      ~operation:"verify_registration_email" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Logout

      Revokes your access token and its associated refresh token, destroying your current session. *)
  let revoke_oauth_token client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/revoke-token" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"revoke_oauth_token" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Login

      With your [client id and secret](#operation/getOAuthClient), you can retrieve an access and refresh tokens.
      @param x_peertube_otp If the user enabled two factor authentication, you need to provide the OTP code in this header
  *)
  let get_oauth_token ?x_peertube_otp ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/token" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.Form.urlencoded body in headers, Some body
    in
    let __openapi_headers = match x_peertube_otp with None -> __openapi_headers | Some value -> let cell = Fetch.Header.raw "x-peertube-otp" value in Fetch.Header.(cell :: __openapi_headers) in
    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"access_token\":{\"type\":\"string\",\"example\":\"90286a0bdf0f7315d9d3fe8dabf9e1d2be9c97d0\",\"description\":\"valid for 1 day\"},\"expires_in\":{\"type\":\"integer\",\"minimum\":0,\"example\":14399},\"refresh_token\":{\"type\":\"string\",\"example\":\"2e0d675df9fc96d2e4ec8a3ebbbf45eca9137bb7\",\"description\":\"valid for 2 weeks\"},\"refresh_token_expires_in\":{\"type\":\"integer\",\"minimum\":0,\"example\":1209600},\"token_type\":{\"type\":\"string\",\"example\":\"Bearer\"}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"access_token\":{\"type\":\"string\",\"example\":\"90286a0bdf0f7315d9d3fe8dabf9e1d2be9c97d0\",\"description\":\"valid for 1 day\"},\"expires_in\":{\"type\":\"integer\",\"minimum\":0,\"example\":14399},\"refresh_token\":{\"type\":\"string\",\"example\":\"2e0d675df9fc96d2e4ec8a3ebbbf45eca9137bb7\",\"description\":\"valid for 2 weeks\"},\"refresh_token_expires_in\":{\"type\":\"integer\",\"minimum\":0,\"example\":1209600},\"token_type\":{\"type\":\"string\",\"example\":\"Bearer\"}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None)); ("401", (fun _ -> None))]
      ~operation:"get_oauth_token" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Get a user
      @param id Entity id
      @param with_stats include statistics about the user (only available as a moderator/admin)
  *)
  let get_user ~id ?with_stats client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/users/{id}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"withStats" ~value:with_stats]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"oneOf\":[{\"$ref\":\"#/components/schemas/User\"},{\"$ref\":\"#/components/schemas/UserWithStats\"}],\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"oneOf\":[{\"$ref\":\"#/components/schemas/User\"},{\"$ref\":\"#/components/schemas/UserWithStats\"}],\"properties\":{},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_user" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Update a user
      @param id Entity id
  *)
  let put_user ~id ~body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/users/{id}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/UpdateUser\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" UpdateUser.T.jsont)) body in
    let __openapi_body = Some __openapi_body in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"put_user" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** Delete a user
      @param id Entity id
  *)
  let del_user ~id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/users/{id}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"del_user" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Reset password
      @param id Entity id
  *)
  let post_api_v1_users_by_id_reset_password ~id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/users/{id}/reset-password" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"password\":{\"type\":\"string\"},\"verificationString\":{\"type\":\"string\",\"format\":\"url\"}},\"required\":[\"verificationString\",\"password\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("403", (fun _ -> None)); ("404", (fun _ -> None))]
      ~operation:"post_api_v1_users_by_id_reset_password" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** List token sessions
      @param id Entity id
  *)
  let get_api_v1_users_by_id_token_sessions ~id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/users/{id}/token-sessions" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/TokenSession\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/TokenSession\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_users_by_id_token_sessions" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** List token sessions
      @param id Entity id
      @param token_session_id Token session Id
  *)
  let get_api_v1_users_by_id_token_sessions_by_token_session_id_revoke ~id ~token_session_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id); ("tokenSessionId", token_session_id)] "/api/v1/users/{id}/token-sessions/{tokenSessionId}/revoke" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_users_by_id_token_sessions_by_token_session_id_revoke" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Confirm two factor auth

      Confirm a two factor authentication request
      @param id Entity id
  *)
  let confirm_two_factor_request ~id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/users/{id}/two-factor/confirm-request" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"otpToken\":{\"type\":\"string\",\"description\":\"OTP token generated by the app\"},\"requestToken\":{\"type\":\"string\",\"description\":\"Token to identify the two factor request\"}},\"required\":[\"requestToken\",\"otpToken\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("403", (fun _ -> None)); ("404", (fun _ -> None))]
      ~operation:"confirm_two_factor_request" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Disable two factor auth

      Disable two factor authentication of a user
      @param id Entity id
  *)
  let disable_two_factor ~id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/users/{id}/two-factor/disable" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"currentPassword\":{\"type\":\"string\",\"description\":\"Password of the currently authenticated user\"}},\"required\":[]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("403", (fun _ -> None)); ("404", (fun _ -> None))]
      ~operation:"disable_two_factor" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Verify a user

      Following a user registration, the new user will receive an email asking to click a link
  containing a secret.
  This endpoint can also be used to verify a new email set in the user account.

      @param id Entity id
  *)
  let verify_user ~id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/users/{id}/verify-email" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"isPendingEmail\":{\"type\":\"boolean\"},\"verificationString\":{\"type\":\"string\",\"format\":\"url\"}},\"required\":[\"verificationString\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("403", (fun _ -> None)); ("404", (fun _ -> None))]
      ~operation:"verify_user" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** List user exports

      **PeerTube >= 6.1**
      @param user_id User id
  *)
  let list_user_exports ~user_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("userId", user_id)] "/api/v1/users/{userId}/exports" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"expiresOn\":{\"type\":\"string\",\"format\":\"date-time\"},\"id\":{\"type\":\"integer\"},\"privateDownloadUrl\":{\"type\":\"string\",\"description\":\"This URL already contains the JWT token, so no additional authentication credentials are required\"},\"size\":{\"type\":\"integer\",\"description\":\"Size of the archive file in bytes\"},\"state\":{\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/UserExportState\"},\"label\":{\"type\":\"string\"}}}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"expiresOn\":{\"type\":\"string\",\"format\":\"date-time\"},\"id\":{\"type\":\"integer\"},\"privateDownloadUrl\":{\"type\":\"string\",\"description\":\"This URL already contains the JWT token, so no additional authentication credentials are required\"},\"size\":{\"type\":\"integer\",\"description\":\"Size of the archive file in bytes\"},\"state\":{\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/UserExportState\"},\"label\":{\"type\":\"string\"}}}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"list_user_exports" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Request user export

      Request an archive of user data. An email is sent when the archive is ready.
      @param user_id User id
  *)
  let request_user_export ~user_id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("userId", user_id)] "/api/v1/users/{userId}/exports/request" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"withVideoFiles\":{\"type\":\"boolean\",\"description\":\"Whether to include video files in the archive\"}},\"required\":[]}" Jsont.json)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"export\":{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"integer\"}}}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"export\":{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"integer\"}}}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"request_user_export" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Delete a user export

      **PeerTube >= 6.1**
      @param user_id User id
      @param id Entity id
  *)
  let delete_user_export ~user_id ~id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("userId", user_id); ("id", id)] "/api/v1/users/{userId}/exports/{id}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"delete_user_export" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Initialize the resumable user import

      **PeerTube >= 6.1** Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to initialize the import of the archive
      @param user_id User id
      @param x_upload_content_length Number of bytes that will be uploaded in subsequent requests. Set this value to the size of the file you are uploading.
      @param x_upload_content_type MIME type of the file that you are uploading. Depending on your instance settings, acceptable values might vary.
  *)
  let user_import_resumable_init ~user_id ~x_upload_content_length ~x_upload_content_type ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("userId", user_id)] "/api/v1/users/{userId}/imports/import-resumable" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/UserImportResumable\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" UserImportResumable.T.jsont)) body in headers, Some body
    in
    let __openapi_headers = let cell = Fetch.Header.raw "X-Upload-Content-Length" x_upload_content_length in Fetch.Header.(cell :: __openapi_headers) in
    let __openapi_headers = let cell = Fetch.Header.raw "X-Upload-Content-Type" x_upload_content_type in Fetch.Header.(cell :: __openapi_headers) in

    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"user_import_resumable_init" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Send chunk for the resumable user import

      **PeerTube >= 6.1** Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to continue, pause or resume the import of the archive
      @param user_id User id
      @param upload_id Created session id to proceed with. If you didn't send chunks in the last hour, it is
  not valid anymore and you need to initialize a new upload.

      @param content_range Specifies the bytes in the file that the request is uploading.

  For example, a value of `bytes 0-262143/1000000` shows that the request is sending the first
  262144 bytes (256 x 1024) in a 2,469,036 byte file.

      @param content_length Size of the chunk that the request is sending.

  Remember that larger chunks are more efficient. PeerTube's web client uses chunks varying from
  1048576 bytes (~1MB) and increases or reduces size depending on connection health.

  *)
  let user_import_resumable ~user_id ~upload_id ~content_range ~content_length ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("userId", user_id)] "/api/v1/users/{userId}/imports/import-resumable" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.singleton ~key:"upload_id" ~value:upload_id]) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.Header.[ content_type, media "application/octet-stream" ], body in headers, Some body
    in
    let __openapi_headers = let cell = Fetch.Header.raw "Content-Range" content_range in Fetch.Header.(cell :: __openapi_headers) in
    let __openapi_headers = let cell = Fetch.Header.raw "Content-Length" content_length in Fetch.Header.(cell :: __openapi_headers) in

    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("308", (fun _ -> None))]
      ~operation:"user_import_resumable" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** Cancel the resumable user import

      **PeerTube >= 6.1** Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to cancel the resumable user import
      @param user_id User id
      @param upload_id Created session id to proceed with. If you didn't send chunks in the last hour, it is
  not valid anymore and you need to initialize a new upload.

  *)
  let user_import_resumable_cancel ~user_id ~upload_id ~content_length client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("userId", user_id)] "/api/v1/users/{userId}/imports/import-resumable" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.singleton ~key:"upload_id" ~value:upload_id]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in
    let __openapi_headers = let cell = Fetch.Header.raw "Content-Length" content_length in Fetch.Header.(cell :: __openapi_headers) in

    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"user_import_resumable_cancel" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Get latest user import

      **PeerTube >= 6.1**
      @param user_id User id
  *)
  let get_latest_user_import ~user_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("userId", user_id)] "/api/v1/users/{userId}/imports/latest" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"id\":{\"type\":\"integer\"},\"state\":{\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/UserImportState\"},\"label\":{\"type\":\"string\"}}}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"createdAt\":{\"type\":\"string\",\"format\":\"date-time\"},\"id\":{\"type\":\"integer\"},\"state\":{\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/UserImportState\"},\"label\":{\"type\":\"string\"}}}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_latest_user_import" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Create a synchronization for a video channel *)
  let add_video_channel_sync ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/video-channel-syncs" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoChannelSyncCreate\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" VideoChannelSync.Create.jsont)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"videoChannelSync\":{\"$ref\":\"#/components/schemas/VideoChannelSync\"}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"videoChannelSync\":{\"$ref\":\"#/components/schemas/VideoChannelSync\"}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"add_video_channel_sync" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Delete a video channel synchronization
      @param channel_sync_id Channel Sync id
  *)
  let del_video_channel_sync ~channel_sync_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("channelSyncId", channel_sync_id)] "/api/v1/video-channel-syncs/{channelSyncId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"del_video_channel_sync" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Triggers the channel synchronization job, fetching all the videos from the remote channel
      @param channel_sync_id Channel Sync id
  *)
  let trigger_video_channel_sync ~channel_sync_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("channelSyncId", channel_sync_id)] "/api/v1/video-channel-syncs/{channelSyncId}/sync" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"trigger_video_channel_sync" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Create a video channel *)
  let add_video_channel ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/video-channels" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoChannelCreate\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" VideoChannel.Create.jsont)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"videoChannel\":{\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/id\"}}}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"videoChannel\":{\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/id\"}}}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"add_video_channel" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Update a video channel
      @param channel_handle The video channel handle
  *)
  let put_video_channel ~channel_handle ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoChannelUpdate\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" VideoChannel.Update.jsont)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"put_video_channel" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** Delete a video channel
      @param channel_handle The video channel handle
  *)
  let del_video_channel ~channel_handle client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"del_video_channel" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Delete channel avatar
      @param channel_handle The video channel handle
  *)
  let delete_api_v1_video_channels_by_channel_handle_avatar ~channel_handle client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/avatar" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"delete_api_v1_video_channels_by_channel_handle_avatar" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Update channel avatar
      @param channel_handle The video channel handle
  *)
  let post_api_v1_video_channels_by_channel_handle_avatar_pick ~channel_handle ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/avatar/pick" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.Form.multipart body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"avatars\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/ActorImage\"}}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"avatars\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/ActorImage\"}}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("413", (fun _ -> None))]
      ~operation:"post_api_v1_video_channels_by_channel_handle_avatar_pick" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Delete channel banner
      @param channel_handle The video channel handle
  *)
  let delete_api_v1_video_channels_by_channel_handle_banner ~channel_handle client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/banner" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"delete_api_v1_video_channels_by_channel_handle_banner" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Update channel banner
      @param channel_handle The video channel handle
  *)
  let post_api_v1_video_channels_by_channel_handle_banner_pick ~channel_handle ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/banner/pick" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.Form.multipart body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"banners\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/ActorImage\"}}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"banners\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/ActorImage\"}}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("413", (fun _ -> None))]
      ~operation:"post_api_v1_video_channels_by_channel_handle_banner_pick" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** *List channel collaborators

      **PeerTube >= 8.0**
      @param channel_handle The video channel handle
  *)
  let list_video_channel_collaborators ~channel_handle client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/collaborators" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoChannelCollaborator\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoChannelCollaborator\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"list_video_channel_collaborators" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Invite a collaborator

      **PeerTube >= 8.0**  Invite a local user to collaborate on the specified video channel.
      @param channel_handle The video channel handle
  *)
  let invite_video_channel_collaborator ~channel_handle ~body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/collaborators/invite" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"accountHandle\":{\"type\":\"string\",\"description\":\"Local user username to invite\"}},\"required\":[]}" Jsont.json)) body in
    let __openapi_body = Some __openapi_body in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"collaborator\":{\"$ref\":\"#/components/schemas/VideoChannelCollaborator\"}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"collaborator\":{\"$ref\":\"#/components/schemas/VideoChannelCollaborator\"}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"invite_video_channel_collaborator" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Remove a channel collaborator

      **PeerTube >= 8.0** Only the channel owner or the collaborator themselves can remove a collaborator from a channel
      @param channel_handle The video channel handle
      @param collaborator_id The collaborator id
  *)
  let remove_video_channel_collaborator ~channel_handle ~collaborator_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle); ("collaboratorId", collaborator_id)] "/api/v1/video-channels/{channelHandle}/collaborators/{collaboratorId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"remove_video_channel_collaborator" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Accept a collaboration invitation

      **PeerTube >= 8.0**
      @param channel_handle The video channel handle
      @param collaborator_id The collaborator id
  *)
  let accept_video_channel_collaborator ~channel_handle ~collaborator_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle); ("collaboratorId", collaborator_id)] "/api/v1/video-channels/{channelHandle}/collaborators/{collaboratorId}/accept" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"accept_video_channel_collaborator" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Reject a collaboration invitation

      **PeerTube >= 8.0**
      @param channel_handle The video channel handle
      @param collaborator_id The collaborator id
  *)
  let reject_video_channel_collaborator ~channel_handle ~collaborator_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle); ("collaboratorId", collaborator_id)] "/api/v1/video-channels/{channelHandle}/collaborators/{collaboratorId}/reject" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"reject_video_channel_collaborator" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** List followers of a video channel
      @param channel_handle The video channel handle
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort followers by criteria
      @param search Plain text search, applied to various parts of the model depending on endpoint
  *)
  let get_video_channel_followers ~channel_handle ?start ?count ?sort ?search client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/followers" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"search" ~value:search]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Follow\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Follow\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_video_channel_followers" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Import videos in channel

      Import a remote channel/playlist videos into a channel
      @param channel_handle The video channel handle
  *)
  let post_api_v1_video_channels_by_channel_handle_import_videos ~channel_handle ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/import-videos" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/ImportVideosInChannelCreate\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" ImportVideosInChannel.Create.jsont)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_video_channels_by_channel_handle_import_videos" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** List playlists of a channel
      @param channel_handle The video channel handle
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_api_v1_video_channels_by_channel_handle_video_playlists ~channel_handle ?start ?count ?sort ?playlist_type client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/video-playlists" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"playlistType" ~value:playlist_type]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoPlaylist\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoPlaylist\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_video_channels_by_channel_handle_video_playlists" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Reorder channel playlists
      @param channel_handle The video channel handle
  *)
  let reorder_video_playlists_of_channel ~channel_handle ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("channelHandle", channel_handle)] "/api/v1/video-channels/{channelHandle}/video-playlists/reorder" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"insertAfterPosition\":{\"type\":\"integer\",\"description\":\"New position for the block to reorder, to add the block before the first element\",\"minimum\":0},\"reorderLength\":{\"type\":\"integer\",\"description\":\"How many element from `startPosition` to reorder\",\"minimum\":1},\"startPosition\":{\"type\":\"integer\",\"description\":\"Start position of the element to reorder\",\"minimum\":1}},\"required\":[\"startPosition\",\"insertAfterPosition\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"reorder_video_playlists_of_channel" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** List video playlists
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_playlists ?start ?count ?sort ?playlist_type client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/video-playlists" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"playlistType" ~value:playlist_type]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoPlaylist\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoPlaylist\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_playlists" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Create a video playlist

      If the video playlist is set as public, `videoChannelId` is mandatory. *)
  let add_playlist ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/video-playlists" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.Form.multipart body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"videoPlaylist\":{\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/VideoPlaylist/properties/id\"},\"uuid\":{\"$ref\":\"#/components/schemas/VideoPlaylist/properties/uuid\"},\"shortUUID\":{\"$ref\":\"#/components/schemas/VideoPlaylist/properties/shortUUID\"}}}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"videoPlaylist\":{\"type\":\"object\",\"properties\":{\"id\":{\"$ref\":\"#/components/schemas/VideoPlaylist/properties/id\"},\"uuid\":{\"$ref\":\"#/components/schemas/VideoPlaylist/properties/uuid\"},\"shortUUID\":{\"$ref\":\"#/components/schemas/VideoPlaylist/properties/shortUUID\"}}}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"add_playlist" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** List available playlist privacy policies *)
  let get_playlist_privacy_policies client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/video-playlists/privacies" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"string\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"string\"}}" (Jsont.list Jsont.string)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_playlist_privacy_policies" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Update a video playlist

      If the video playlist is set as public, the playlist must have a assigned channel.
      @param playlist_id Playlist id
  *)
  let put_api_v1_video_playlists_by_playlist_id ~playlist_id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("playlistId", playlist_id)] "/api/v1/video-playlists/{playlistId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.Form.multipart body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"put_api_v1_video_playlists_by_playlist_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** Delete a video playlist
      @param playlist_id Playlist id
  *)
  let delete_api_v1_video_playlists_by_playlist_id ~playlist_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("playlistId", playlist_id)] "/api/v1/video-playlists/{playlistId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"delete_api_v1_video_playlists_by_playlist_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** List videos of a playlist
      @param playlist_id Playlist id
      @param start Offset used to paginate results
      @param count Number of items to return
  *)
  let get_video_playlist_videos ~playlist_id ?start ?count client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("playlistId", playlist_id)] "/api/v1/video-playlists/{playlistId}/videos" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"maxItems\":100,\"items\":{\"type\":\"object\",\"properties\":{\"id\":{\"description\":\"Playlist element id\",\"allOf\":[{\"$ref\":\"#/components/schemas/id\"}]},\"position\":{\"type\":\"integer\",\"example\":2},\"startTimestamp\":{\"type\":\"integer\",\"nullable\":true,\"example\":10,\"format\":\"seconds\"},\"stopTimestamp\":{\"type\":\"integer\",\"nullable\":true,\"example\":41,\"format\":\"seconds\"},\"video\":{\"$ref\":\"#/components/schemas/Video\"}}}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"maxItems\":100,\"items\":{\"type\":\"object\",\"properties\":{\"id\":{\"description\":\"Playlist element id\",\"allOf\":[{\"$ref\":\"#/components/schemas/id\"}]},\"position\":{\"type\":\"integer\",\"example\":2},\"startTimestamp\":{\"type\":\"integer\",\"nullable\":true,\"example\":10,\"format\":\"seconds\"},\"stopTimestamp\":{\"type\":\"integer\",\"nullable\":true,\"example\":41,\"format\":\"seconds\"},\"video\":{\"$ref\":\"#/components/schemas/Video\"}}}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_video_playlist_videos" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Add a video in a playlist
      @param playlist_id Playlist id
  *)
  let add_video_playlist_video ~playlist_id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("playlistId", playlist_id)] "/api/v1/video-playlists/{playlistId}/videos" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"startTimestamp\":{\"type\":\"integer\",\"format\":\"seconds\",\"description\":\"Start the video at this specific timestamp\"},\"stopTimestamp\":{\"type\":\"integer\",\"format\":\"seconds\",\"description\":\"Stop the video at this specific timestamp\"},\"videoId\":{\"oneOf\":[{\"$ref\":\"#/components/schemas/Video/properties/uuid\"},{\"$ref\":\"#/components/schemas/Video/properties/id\"}],\"description\":\"Video to add in the playlist\"}},\"required\":[\"videoId\"]}" Jsont.json)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"videoPlaylistElement\":{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"integer\",\"example\":2}}}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"videoPlaylistElement\":{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"integer\",\"example\":2}}}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"add_video_playlist_video" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Reorder playlist elements
      @param playlist_id Playlist id
  *)
  let reorder_video_playlist ~playlist_id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("playlistId", playlist_id)] "/api/v1/video-playlists/{playlistId}/videos/reorder" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"insertAfterPosition\":{\"type\":\"integer\",\"description\":\"New position for the block to reorder, to add the block before the first element\",\"minimum\":0},\"reorderLength\":{\"type\":\"integer\",\"description\":\"How many element from `startPosition` to reorder\",\"minimum\":1},\"startPosition\":{\"type\":\"integer\",\"description\":\"Start position of the element to reorder\",\"minimum\":1}},\"required\":[\"startPosition\",\"insertAfterPosition\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"reorder_video_playlist" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Update a playlist element
      @param playlist_id Playlist id
      @param playlist_element_id Playlist element id
  *)
  let put_video_playlist_video ~playlist_id ~playlist_element_id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("playlistId", playlist_id); ("playlistElementId", playlist_element_id)] "/api/v1/video-playlists/{playlistId}/videos/{playlistElementId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"startTimestamp\":{\"type\":\"integer\",\"format\":\"seconds\",\"description\":\"Start the video at this specific timestamp\"},\"stopTimestamp\":{\"type\":\"integer\",\"format\":\"seconds\",\"description\":\"Stop the video at this specific timestamp\"}},\"required\":[]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"put_video_playlist_video" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** Delete an element from a playlist
      @param playlist_id Playlist id
      @param playlist_element_id Playlist element id
  *)
  let del_video_playlist_video ~playlist_id ~playlist_element_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("playlistId", playlist_id); ("playlistElementId", playlist_element_id)] "/api/v1/video-playlists/{playlistId}/videos/{playlistElementId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"del_video_playlist_video" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

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
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/videos/blacklist" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"type" ~value:type_; Openapi.Runtime.Query.optional ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoBlacklist\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoBlacklist\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_video_blocks" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** List available video categories *)
  let get_categories client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/videos/categories" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"string\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"string\"}}" (Jsont.list Jsont.string)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_categories" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

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
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/videos/comments" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"searchAccount" ~value:search_account; Openapi.Runtime.Query.optional ~key:"searchVideo" ~value:search_video; Openapi.Runtime.Query.optional ~key:"videoId" ~value:video_id; Openapi.Runtime.Query.optional ~key:"videoChannelId" ~value:video_channel_id; Openapi.Runtime.Query.optional ~key:"autoTagOneOf" ~value:auto_tag_one_of; Openapi.Runtime.Query.optional ~key:"isLocal" ~value:is_local; Openapi.Runtime.Query.optional ~key:"onLocalVideo" ~value:on_local_video]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoCommentForOwnerOrAdmin\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoCommentForOwnerOrAdmin\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_videos_comments" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Delete video import

      Delete ended video import
      @param id Entity id
  *)
  let delete_api_v1_videos_imports_by_id ~id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/imports/{id}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"delete_api_v1_videos_imports_by_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Cancel video import

      Cancel a pending video import
      @param id Entity id
  *)
  let post_api_v1_videos_imports_by_id_cancel ~id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/imports/{id}/cancel" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_videos_imports_by_id_cancel" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Retry video import

      **PeerTube >= 8.0** Retry a pending video import
      @param id Entity id
  *)
  let post_api_v1_videos_imports_by_id_retry ~id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/imports/{id}/retry" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_videos_imports_by_id_retry" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** List available video languages *)
  let get_languages client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/videos/languages" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"string\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"string\"}}" (Jsont.list Jsont.string)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_languages" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** List available video licences *)
  let get_licences client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/videos/licences" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"string\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"string\"}}" (Jsont.list Jsont.string)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_licences" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Update information about a live
      @param id The object id, uuid or short uuid
  *)
  let update_live_id ~id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/live/{id}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/LiveVideoUpdate\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" LiveVideo.Update.jsont)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None)); ("403", (fun _ -> None))]
      ~operation:"update_live_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** List live sessions

      List all sessions created in a particular live
      @param id The object id, uuid or short uuid
      @param sort Sort column
  *)
  let get_api_v1_videos_live_by_id_sessions ~id ?sort client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/live/{id}/sessions" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/LiveVideoSessionResponse\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/LiveVideoSessionResponse\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_videos_live_by_id_sessions" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** List video ownership changes *)
  let get_api_v1_videos_ownership client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/videos/ownership" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_videos_ownership" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Accept ownership change request
      @param id The object id, uuid or short uuid
  *)
  let post_api_v1_videos_ownership_by_id_accept ~id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/ownership/{id}/accept" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("403", (fun _ -> None)); ("404", (fun _ -> None))]
      ~operation:"post_api_v1_videos_ownership_by_id_accept" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Refuse ownership change request
      @param id The object id, uuid or short uuid
  *)
  let post_api_v1_videos_ownership_by_id_refuse ~id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/ownership/{id}/refuse" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("403", (fun _ -> None)); ("404", (fun _ -> None))]
      ~operation:"post_api_v1_videos_ownership_by_id_refuse" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** List available video privacy policies *)
  let get_video_privacy_policies client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/videos/privacies" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"string\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"type\":\"string\"}}" (Jsont.list Jsont.string)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_video_privacy_policies" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Initialize the resumable upload of a video

      Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to initialize the upload of a video
      @param x_upload_content_length Number of bytes that will be uploaded in subsequent requests. Set this value to the size of the file you are uploading.
      @param x_upload_content_type MIME type of the file that you are uploading. Depending on your instance settings, acceptable values might vary.
  *)
  let upload_resumable_init ~x_upload_content_length ~x_upload_content_type ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/videos/upload-resumable" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoUploadRequestResumable\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" VideoUploadRequestResumable.T.jsont)) body in headers, Some body
    in
    let __openapi_headers = let cell = Fetch.Header.raw "X-Upload-Content-Length" x_upload_content_length in Fetch.Header.(cell :: __openapi_headers) in
    let __openapi_headers = let cell = Fetch.Header.raw "X-Upload-Content-Type" x_upload_content_type in Fetch.Header.(cell :: __openapi_headers) in

    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("413", (fun _ -> None)); ("415", (fun _ -> None))]
      ~operation:"upload_resumable_init" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Cancel the resumable upload of a video, deleting any data uploaded so far

      Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to cancel the upload of a video
      @param upload_id Created session id to proceed with. If you didn't send chunks in the last hour, it is
  not valid anymore and you need to initialize a new upload.

  *)
  let upload_resumable_cancel ~upload_id ~content_length client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/videos/upload-resumable" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.singleton ~key:"upload_id" ~value:upload_id]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in
    let __openapi_headers = let cell = Fetch.Header.raw "Content-Length" content_length in Fetch.Header.(cell :: __openapi_headers) in

    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"upload_resumable_cancel" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Update a video
      @param id The object id, uuid or short uuid
  *)
  let put_video ~id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.Form.multipart body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"put_video" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** Delete a video
      @param id The object id, uuid or short uuid
  *)
  let del_video ~id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"del_video" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Block a video
      @param id The object id, uuid or short uuid
  *)
  let add_video_block ~id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/blacklist" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"add_video_block" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Unblock a video by its id
      @param id The object id, uuid or short uuid
  *)
  let del_video_block ~id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/blacklist" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"del_video_block" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** List captions of a video
      @param id The object id, uuid or short uuid
      @param x_peertube_video_password Required on password protected video
  *)
  let get_video_captions ~id ?x_peertube_video_password client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/captions" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in
    let __openapi_headers = match x_peertube_video_password with None -> __openapi_headers | Some value -> let cell = Fetch.Header.raw "x-peertube-video-password" value in Fetch.Header.(cell :: __openapi_headers) in
    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoCaption\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/VideoCaption\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_video_captions" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Generate a video caption

      **PeerTube >= 6.2** This feature has to be enabled by the administrator
      @param id The object id, uuid or short uuid
  *)
  let generate_video_caption ~id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/captions/generate" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"forceTranscription\":{\"type\":\"boolean\",\"default\":false}},\"required\":[]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"generate_video_caption" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Add or replace a video caption
      @param id The object id, uuid or short uuid
      @param caption_language The caption language
  *)
  let add_video_caption ~id ~caption_language ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id); ("captionLanguage", caption_language)] "/api/v1/videos/{id}/captions/{captionLanguage}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.Form.multipart body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"add_video_caption" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** Delete a video caption
      @param id The object id, uuid or short uuid
      @param caption_language The caption language
  *)
  let del_video_caption ~id ~caption_language client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id); ("captionLanguage", caption_language)] "/api/v1/videos/{id}/captions/{captionLanguage}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"del_video_caption" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Replace video chapters

      **PeerTube >= 6.0**
      @param id The object id, uuid or short uuid
  *)
  let replace_video_chapters ~id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/chapters" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"chapters\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"title\":{\"type\":\"string\"},\"timecode\":{\"type\":\"integer\"}}}}},\"required\":[]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"replace_video_chapters" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** Delete a comment or a reply
      @param id The object id, uuid or short uuid
      @param comment_id The comment id
  *)
  let delete_api_v1_videos_by_id_comments_by_comment_id ~id ~comment_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id); ("commentId", comment_id)] "/api/v1/videos/{id}/comments/{commentId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("403", (fun _ -> None)); ("404", (fun _ -> None)); ("409", (fun _ -> None))]
      ~operation:"delete_api_v1_videos_by_id_comments_by_comment_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Approve a comment

      **PeerTube >= 6.2** Approve a comment that requires a review
      @param id The object id, uuid or short uuid
      @param comment_id The comment id
  *)
  let post_api_v1_videos_by_id_comments_by_comment_id_approve ~id ~comment_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id); ("commentId", comment_id)] "/api/v1/videos/{id}/comments/{commentId}/approve" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_videos_by_id_comments_by_comment_id_approve" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Get complete video description
      @param id The object id, uuid or short uuid
      @param x_peertube_video_password Required on password protected video
  *)
  let get_video_desc ~id ?x_peertube_video_password client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/description" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in
    let __openapi_headers = match x_peertube_video_password with None -> __openapi_headers | Some value -> let cell = Fetch.Header.raw "x-peertube-video-password" value in Fetch.Header.(cell :: __openapi_headers) in
    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"string\",\"nullable\":true,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"minLength\":3,\"maxLength\":10000,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"example\":\"**[Want to help to translate this video?](https://weblate.framasoft.org/projects/what-is-peertube-video/)**\\\\r\\\\n\\\\r\\\\n**Take back the control of your videos! [#JoinPeertube](https://joinpeertube.org)**\\n\"}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"string\",\"nullable\":true,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"minLength\":3,\"maxLength\":10000,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"example\":\"**[Want to help to translate this video?](https://weblate.framasoft.org/projects/what-is-peertube-video/)**\\\\r\\\\n\\\\r\\\\n**Take back the control of your videos! [#JoinPeertube](https://joinpeertube.org)**\\n\"}" (Jsont.option Jsont.string)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_video_desc" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Request ownership change
      @param id The object id, uuid or short uuid
  *)
  let post_api_v1_videos_by_id_give_ownership ~id ~body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/give-ownership" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"username\":{\"type\":\"string\"}},\"required\":[\"username\"]}" Jsont.json)) body in
    let __openapi_body = Some __openapi_body in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None)); ("404", (fun _ -> None))]
      ~operation:"post_api_v1_videos_by_id_give_ownership" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Delete video HLS files
      @param id The object id, uuid or short uuid
  *)
  let del_video_hls ~id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/hls" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"del_video_hls" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** List video passwords

      **PeerTube >= 6.0**
      @param id The object id, uuid or short uuid
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let list_video_passwords ~id ?start ?count ?sort client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/passwords" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None))]
      ~operation:"list_video_passwords" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Add a video password

      **PeerTube >= 8.0**
      @param id The object id, uuid or short uuid
  *)
  let add_video_password ~id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/passwords" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"password\":{\"type\":\"string\"}},\"required\":[]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None))]
      ~operation:"add_video_password" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Update video passwords

      **PeerTube >= 6.0**
      @param id The object id, uuid or short uuid
  *)
  let update_video_password_list ~id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/passwords" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"passwords\":{\"$ref\":\"#/components/schemas/AddVideoPasswords\"}},\"required\":[]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None))]
      ~operation:"update_video_password_list" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** Delete a video password

      **PeerTube >= 6.0**
      @param id The object id, uuid or short uuid
      @param video_password_id The video password id
  *)
  let remove_video_password ~id ~video_password_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id); ("videoPasswordId", video_password_id)] "/api/v1/videos/{id}/passwords/{videoPasswordId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None)); ("403", (fun _ -> None))]
      ~operation:"remove_video_password" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Like/dislike a video
      @param id The object id, uuid or short uuid
      @param x_peertube_video_password Required on password protected video
  *)
  let put_api_v1_videos_by_id_rate ~id ?x_peertube_video_password ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/rate" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"rating\":{\"type\":\"string\",\"enum\":[\"like\",\"dislike\"]}},\"required\":[\"rating\"]}" Jsont.json)) body in headers, Some body
    in
    let __openapi_headers = match x_peertube_video_password with None -> __openapi_headers | Some value -> let cell = Fetch.Header.raw "x-peertube-video-password" value in Fetch.Header.(cell :: __openapi_headers) in

    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"put_api_v1_videos_by_id_rate" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** Delete video source file
      @param id The object id, uuid or short uuid
  *)
  let delete_video_source_file ~id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/source/file" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"delete_video_source_file" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Initialize the resumable replacement of a video

      **PeerTube >= 6.0** Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to initialize the replacement of a video
      @param id The object id, uuid or short uuid
      @param x_upload_content_length Number of bytes that will be uploaded in subsequent requests. Set this value to the size of the file you are uploading.
      @param x_upload_content_type MIME type of the file that you are uploading. Depending on your instance settings, acceptable values might vary.
  *)
  let replace_video_source_resumable_init ~id ~x_upload_content_length ~x_upload_content_type ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/source/replace-resumable" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoReplaceSourceRequestResumable\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" VideoReplaceSourceRequestResumable.T.jsont)) body in headers, Some body
    in
    let __openapi_headers = let cell = Fetch.Header.raw "X-Upload-Content-Length" x_upload_content_length in Fetch.Header.(cell :: __openapi_headers) in
    let __openapi_headers = let cell = Fetch.Header.raw "X-Upload-Content-Type" x_upload_content_type in Fetch.Header.(cell :: __openapi_headers) in

    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("413", (fun _ -> None)); ("415", (fun _ -> None))]
      ~operation:"replace_video_source_resumable_init" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Send chunk for the resumable replacement of a video

      **PeerTube >= 6.0** Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to continue, pause or resume the replacement of a video
      @param id The object id, uuid or short uuid
      @param upload_id Created session id to proceed with. If you didn't send chunks in the last hour, it is
  not valid anymore and you need to initialize a new upload.

      @param content_range Specifies the bytes in the file that the request is uploading.

  For example, a value of `bytes 0-262143/1000000` shows that the request is sending the first
  262144 bytes (256 x 1024) in a 2,469,036 byte file.

      @param content_length Size of the chunk that the request is sending.

  Remember that larger chunks are more efficient. PeerTube's web client uses chunks varying from
  1048576 bytes (~1MB) and increases or reduces size depending on connection health.

  *)
  let replace_video_source_resumable ~id ~upload_id ~content_range ~content_length ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/source/replace-resumable" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.singleton ~key:"upload_id" ~value:upload_id]) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.Header.[ content_type, media "application/octet-stream" ], body in headers, Some body
    in
    let __openapi_headers = let cell = Fetch.Header.raw "Content-Range" content_range in Fetch.Header.(cell :: __openapi_headers) in
    let __openapi_headers = let cell = Fetch.Header.raw "Content-Length" content_length in Fetch.Header.(cell :: __openapi_headers) in

    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("308", (fun _ -> None)); ("403", (fun _ -> None)); ("404", (fun _ -> None)); ("409", (fun _ -> None)); ("422", (fun _ -> None)); ("429", (fun _ -> None)); ("503", (fun _ -> None))]
      ~operation:"replace_video_source_resumable" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** Cancel the resumable replacement of a video

      **PeerTube >= 6.0** Uses [a resumable protocol](https://github.com/kukhariev/node-uploadx/blob/master/proto.md) to cancel the replacement of a video
      @param id The object id, uuid or short uuid
      @param upload_id Created session id to proceed with. If you didn't send chunks in the last hour, it is
  not valid anymore and you need to initialize a new upload.

  *)
  let replace_video_source_resumable_cancel ~id ~upload_id ~content_length client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/source/replace-resumable" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.singleton ~key:"upload_id" ~value:upload_id]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in
    let __openapi_headers = let cell = Fetch.Header.raw "Content-Length" content_length in Fetch.Header.(cell :: __openapi_headers) in

    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"replace_video_source_resumable_cancel" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** List storyboards of a video

      **PeerTube >= 6.0**
      @param id The object id, uuid or short uuid
  *)
  let list_video_storyboards ~id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/storyboards" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"storyboards\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Storyboard\"}}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"storyboards\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/Storyboard\"}}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"list_video_storyboards" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Create a studio task

      Create a task to edit a video  (cut, add intro/outro etc)
      @param id The object id, uuid or short uuid
  *)
  let post_api_v1_videos_by_id_studio_edit ~id ~body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/studio/edit" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = Fetch.Form.multipart body in
    let __openapi_body = Some __openapi_body in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None)); ("404", (fun _ -> None))]
      ~operation:"post_api_v1_videos_by_id_studio_edit" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Create a transcoding job
      @param id The object id, uuid or short uuid
  *)
  let create_video_transcoding ~id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/transcoding" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"forceTranscoding\":{\"type\":\"boolean\",\"default\":false,\"description\":\"If the video is stuck in transcoding state, do it anyway\"},\"transcodingType\":{\"type\":\"string\",\"enum\":[\"hls\",\"web-video\"]}},\"required\":[\"transcodingType\"]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"create_video_transcoding" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Notify user is watching a video

      Call this endpoint regularly (every 5-10 seconds for example) to notify the server the user is watching the video. After a while, PeerTube will increase video's viewers counter. If the user is authenticated, PeerTube will also store the current player time.
      @param id The object id, uuid or short uuid
  *)
  let add_view ~id ~body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/views" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/UserViewingVideo\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" UserViewingVideo.T.jsont)) body in
    let __openapi_body = Some __openapi_body in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"add_view" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Delete video Web Video files

      **PeerTube >= 6.0**
      @param id The object id, uuid or short uuid
  *)
  let del_video_web_videos ~id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("id", id)] "/api/v1/videos/{id}/web-videos" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"del_video_web_videos" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** List account watched words

      **PeerTube >= 6.2**
      @param account_name account name to list watched words
  *)
  let get_api_v1_watched_words_accounts_by_account_name_lists ~account_name client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("accountName", account_name)] "/api/v1/watched-words/accounts/{accountName}/lists" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/WatchedWordsLists\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/WatchedWordsLists\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_watched_words_accounts_by_account_name_lists" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Add account watched words

      **PeerTube >= 6.2** *)
  let post_api_v1_watched_words_accounts_by_account_name_lists ~account_name ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("accountName", account_name)] "/api/v1/watched-words/accounts/{accountName}/lists" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"listName\":{\"type\":\"string\"},\"words\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}}},\"required\":[]}" Jsont.json)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"watchedWordsList\":{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"integer\"}}}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"watchedWordsList\":{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"integer\"}}}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_watched_words_accounts_by_account_name_lists" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Update account watched words

      **PeerTube >= 6.2**
      @param list_id list of watched words to update
  *)
  let put_api_v1_watched_words_accounts_by_account_name_lists_by_list_id ~account_name ~list_id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("accountName", account_name); ("listId", list_id)] "/api/v1/watched-words/accounts/{accountName}/lists/{listId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"listName\":{\"type\":\"string\"},\"words\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}}},\"required\":[]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"put_api_v1_watched_words_accounts_by_account_name_lists_by_list_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** Delete account watched words

      **PeerTube >= 6.2**
      @param list_id list of watched words to delete
  *)
  let delete_api_v1_watched_words_accounts_by_account_name_lists_by_list_id ~account_name ~list_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("accountName", account_name); ("listId", list_id)] "/api/v1/watched-words/accounts/{accountName}/lists/{listId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"delete_api_v1_watched_words_accounts_by_account_name_lists_by_list_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** List server watched words

      **PeerTube >= 6.2** *)
  let get_api_v1_watched_words_server_lists client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/watched-words/server/lists" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/WatchedWordsLists\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"data\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/components/schemas/WatchedWordsLists\"}},\"total\":{\"type\":\"integer\",\"example\":1}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_watched_words_server_lists" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Add server watched words

      **PeerTube >= 6.2** *)
  let post_api_v1_watched_words_server_lists ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/watched-words/server/lists" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"listName\":{\"type\":\"string\"},\"words\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}}},\"required\":[]}" Jsont.json)) body in headers, Some body
    in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"watchedWordsList\":{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"integer\"}}}},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"watchedWordsList\":{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"integer\"}}}},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"post_api_v1_watched_words_server_lists" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `POST

  (** Update server watched words

      **PeerTube >= 6.2**
      @param list_id list of watched words to update
  *)
  let put_api_v1_watched_words_server_lists_by_list_id ~list_id ?body client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("listId", list_id)] "/api/v1/watched-words/server/lists/{listId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers, __openapi_body = match body with
      | None -> Fetch.Header.[], None
      | Some body -> let headers, body = Fetch.encode (Fetch.Json.v ~media:"application/json" (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{\"listName\":{\"type\":\"string\"},\"words\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}}},\"required\":[]}" Jsont.json)) body in headers, Some body
    in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"put_api_v1_watched_words_server_lists_by_list_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `PUT

  (** Delete server watched words

      **PeerTube >= 6.2**
      @param list_id list of watched words to delete
  *)
  let delete_api_v1_watched_words_server_lists_by_list_id ~list_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("listId", list_id)] "/api/v1/watched-words/server/lists/{listId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"delete_api_v1_watched_words_server_lists_by_list_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `DELETE

  (** Download video file

      Generate a mp4 container that contains at most 1 video stream and at most 1 audio stream. Mainly used to merge the HLS audio only video file and the HLS video only resolution file.
      @param video_id The video id
      @param video_file_ids streams of video files to mux in the output
      @param video_file_token Video file token [generated](#operation/requestVideoToken) by PeerTube so you don't need to provide an OAuth token in the request header.
  *)
  let get_download_videos_generate_by_video_id ~video_id ~video_file_ids ?video_file_token client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("videoId", video_id)] "/download/videos/generate/{videoId}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.singleton ~key:"videoFileIds" ~value:video_file_ids; Openapi.Runtime.Query.optional ~key:"videoFileToken" ~value:video_file_token]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_download_videos_generate_by_video_id" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Videos podcast feed
      @param video_channel_id Limit listing to a specific video channel
  *)
  let get_videos_podcast_feed ~video_channel_id client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/feeds/podcast/videos.xml" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.singleton ~key:"videoChannelId" ~value:video_channel_id]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"get_videos_podcast_feed" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

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
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("format", format)] "/feeds/subscriptions.{format}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.singleton ~key:"accountId" ~value:account_id; Openapi.Runtime.Query.singleton ~key:"token" ~value:token; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"nsfw" ~value:nsfw; Openapi.Runtime.Query.optional ~key:"isLocal" ~value:is_local; Openapi.Runtime.Query.optional ~key:"include" ~value:include_; Openapi.Runtime.Query.optional ~key:"privacyOneOf" ~value:privacy_one_of; Openapi.Runtime.Query.optional ~key:"hasHLSFiles" ~value:has_hlsfiles; Openapi.Runtime.Query.optional ~key:"hasWebVideoFiles" ~value:has_web_video_files]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("406", (fun _ -> None))]
      ~operation:"get_syndicated_subscription_videos" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Comments on videos feeds
      @param format format expected (we focus on making `rss` the most feature-rich ; it serves [Media RSS](https://www.rssboard.org/media-rss))
      @param video_id limit listing comments to a specific video
      @param account_id limit listing comments to videos of a specific account
      @param account_name limit listing comments to videos of a specific account
      @param video_channel_id limit listing comments to videos of a specific video channel
      @param video_channel_name limit listing comments to videos of a specific video channel
  *)
  let get_syndicated_comments ~format ?video_id ?account_id ?account_name ?video_channel_id ?video_channel_name client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("format", format)] "/feeds/video-comments.{format}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"videoId" ~value:video_id; Openapi.Runtime.Query.optional ~key:"accountId" ~value:account_id; Openapi.Runtime.Query.optional ~key:"accountName" ~value:account_name; Openapi.Runtime.Query.optional ~key:"videoChannelId" ~value:video_channel_id; Openapi.Runtime.Query.optional ~key:"videoChannelName" ~value:video_channel_name]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("400", (fun _ -> None)); ("404", (fun _ -> None)); ("406", (fun _ -> None))]
      ~operation:"get_syndicated_comments" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

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
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("format", format)] "/feeds/videos.{format}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"accountId" ~value:account_id; Openapi.Runtime.Query.optional ~key:"accountName" ~value:account_name; Openapi.Runtime.Query.optional ~key:"videoChannelId" ~value:video_channel_id; Openapi.Runtime.Query.optional ~key:"videoChannelName" ~value:video_channel_name; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"nsfw" ~value:nsfw; Openapi.Runtime.Query.optional ~key:"isLocal" ~value:is_local; Openapi.Runtime.Query.optional ~key:"include" ~value:include_; Openapi.Runtime.Query.optional ~key:"privacyOneOf" ~value:privacy_one_of; Openapi.Runtime.Query.optional ~key:"hasHLSFiles" ~value:has_hlsfiles; Openapi.Runtime.Query.optional ~key:"hasWebVideoFiles" ~value:has_web_video_files]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"object\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" Jsont.json))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None)); ("406", (fun _ -> None))]
      ~operation:"get_syndicated_videos" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Get private HLS video file
      @param filename Filename
      @param video_file_token Video file token [generated](#operation/requestVideoToken) by PeerTube so you don't need to provide an OAuth token in the request header.
      @param reinject_video_file_token Ask the server to reinject videoFileToken in URLs in m3u8 playlist
  *)
  let get_static_streaming_playlists_hls_private__by_filename ~filename ?video_file_token ?reinject_video_file_token client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("filename", filename)] "/static/streaming-playlists/hls/private/{filename}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"videoFileToken" ~value:video_file_token; Openapi.Runtime.Query.optional ~key:"reinjectVideoFileToken" ~value:reinject_video_file_token]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("403", (fun _ -> None)); ("404", (fun _ -> None))]
      ~operation:"get_static_streaming_playlists_hls_private__by_filename" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Get public HLS video file
      @param filename Filename
  *)
  let get_static_streaming_playlists_hls_by_filename ~filename client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("filename", filename)] "/static/streaming-playlists/hls/{filename}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("403", (fun _ -> None)); ("404", (fun _ -> None))]
      ~operation:"get_static_streaming_playlists_hls_by_filename" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Get private Web Video file

      **PeerTube >= 6.0**
      @param filename Filename
      @param video_file_token Video file token [generated](#operation/requestVideoToken) by PeerTube so you don't need to provide an OAuth token in the request header.
  *)
  let get_static_web_videos_private__by_filename ~filename ?video_file_token client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("filename", filename)] "/static/web-videos/private/{filename}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"videoFileToken" ~value:video_file_token]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("403", (fun _ -> None)); ("404", (fun _ -> None))]
      ~operation:"get_static_web_videos_private__by_filename" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Get public Web Video file

      **PeerTube >= 6.0**
      @param filename Filename
  *)
  let get_static_web_videos_by_filename ~filename client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("filename", filename)] "/static/web-videos/{filename}" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in


    let __openapi_decode ~limit:_ _response = () in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("404", (fun _ -> None))]
      ~operation:"get_static_web_videos_by_filename" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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
      |> Jsont.Object.opt_mem "total" Openapi.Runtime.int_jsont ~enc:(fun r -> r.total)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "VideoChannelList" jsont
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
  let get_api_v1_accounts_by_name_video_channels ~name ?with_stats ?start ?count ?search ?sort ?include_collaborations client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[("name", name)] "/api/v1/accounts/{name}/video-channels" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"withStats" ~value:with_stats; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"includeCollaborations" ~value:include_collaborations]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoChannelList\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoChannelList\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_accounts_by_name_video_channels" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

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
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/search/video-channels" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.singleton ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"searchTarget" ~value:search_target; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort; Openapi.Runtime.Query.optional ~key:"host" ~value:host; Openapi.Runtime.Query.optional ~key:"handles" ~value:handles]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoChannelList\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoChannelList\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[("500", (fun _ -> None))]
      ~operation:"search_channels" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** List my user subscriptions
      @param start Offset used to paginate results
      @param count Number of items to return
  *)
  let get_api_v1_users_me_subscriptions ?start ?count ?sort client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/me/subscriptions" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoChannelList\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoChannelList\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_api_v1_users_me_subscriptions" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** List video channels
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort column
  *)
  let get_video_channels ?start ?count ?sort client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/video-channels" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"$ref\":\"#/components/schemas/VideoChannelList\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"$ref\":\"#/components/schemas/VideoChannelList\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[]}" T.jsont))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_video_channels" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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
      |> Jsont.Object.opt_mem "newFeaturesInfoRead" Openapi.Runtime.number_jsont ~enc:(fun r -> r.new_features_info_read)
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
      |> Jsont.Object.opt_mem "videoQuota" Openapi.Runtime.int_jsont ~enc:(fun r -> r.video_quota)
      |> Jsont.Object.opt_mem "videoQuotaDaily" Openapi.Runtime.int_jsont ~enc:(fun r -> r.video_quota_daily)
      |> Jsont.Object.opt_mem "videosHistoryEnabled" Jsont.bool ~enc:(fun r -> r.videos_history_enabled)
      |> Jsont.Object.opt_mem "videosCount" Openapi.Runtime.int_jsont ~enc:(fun r -> r.videos_count)
      |> Jsont.Object.opt_mem "abusesCount" Openapi.Runtime.int_jsont ~enc:(fun r -> r.abuses_count)
      |> Jsont.Object.opt_mem "abusesAcceptedCount" Openapi.Runtime.int_jsont ~enc:(fun r -> r.abuses_accepted_count)
      |> Jsont.Object.opt_mem "abusesCreatedCount" Openapi.Runtime.int_jsont ~enc:(fun r -> r.abuses_created_count)
      |> Jsont.Object.opt_mem "videoCommentsCount" Openapi.Runtime.int_jsont ~enc:(fun r -> r.video_comments_count)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "UserWithStats" jsont
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
      |> Jsont.Object.opt_mem "newFeaturesInfoRead" Openapi.Runtime.number_jsont ~enc:(fun r -> r.new_features_info_read)
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
      |> Jsont.Object.opt_mem "videoQuota" Openapi.Runtime.int_jsont ~enc:(fun r -> r.video_quota)
      |> Jsont.Object.opt_mem "videoQuotaDaily" Openapi.Runtime.int_jsont ~enc:(fun r -> r.video_quota_daily)
      |> Jsont.Object.opt_mem "videosHistoryEnabled" Jsont.bool ~enc:(fun r -> r.videos_history_enabled)
      |> Jsont.Object.skip_unknown
      |> Jsont.Object.finish

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "User" jsont
  end

  (** List users
      @param search Plain text search that will match with user usernames or emails
      @param blocked Filter results down to (un)banned users
      @param start Offset used to paginate results
      @param count Number of items to return
      @param sort Sort users by criteria
  *)
  let get_users ?search ?blocked ?start ?count ?sort client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat [Openapi.Runtime.Query.optional ~key:"search" ~value:search; Openapi.Runtime.Query.optional ~key:"blocked" ~value:blocked; Openapi.Runtime.Query.optional ~key:"start" ~value:start; Openapi.Runtime.Query.optional ~key:"count" ~value:count; Openapi.Runtime.Query.optional ~key:"sort" ~value:sort]) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/User\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/User\"}}" (Jsont.list T.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_users" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET

  (** Get my user information *)
  let get_user_info client () =
    let __openapi_path = Openapi.Runtime.Path.render ~params:[] "/api/v1/users/me" in
    let __openapi_query = Openapi.Runtime.Query.encode (Stdlib.List.concat []) in
    let __openapi_headers = Fetch.Header.[] in
    let __openapi_body = None in

    let __openapi_headers = Fetch.Header.((accept, [pref "application/json"]) :: __openapi_headers) in
    let __openapi_decode ~limit response = Fetch.decode ~limit (Fetch.Json.v ~media:"application/json" ~accept:["application/json"] (Openapi.Schema.guard_response __openapi_schemas [("200", "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/User\"}}")] (Fetch.status response) (Openapi.Schema.guard_string __openapi_schemas "{\"type\":\"array\",\"nullable\":false,\"readOnly\":false,\"writeOnly\":false,\"deprecated\":false,\"uniqueItems\":false,\"properties\":{},\"required\":[],\"items\":{\"$ref\":\"#/components/schemas/User\"}}" (Jsont.list T.jsont)))) response in
    Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[]
      ~operation:"get_user_info" ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `GET
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
    let jsont = Openapi.Runtime.int_jsont
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "AbuseStateSet" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "AbuseStateConstant" jsont
  end
end

module AbusePredefinedReasons = struct
  module Types = struct
    module T = struct
      type t = string list
    end
  end

  module T = struct
    include Types.T
    let jsont = (Jsont.list Jsont.string)
    let v value = value

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "AbusePredefinedReasons" jsont
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

    let jsont = Openapi.Schema.guard_ref __openapi_schemas "Abuse" jsont
  end
end
