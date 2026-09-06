(** Client policy and response consumption. *)

module Duration = Duration

type 'tag ty = [ `Fetch | `Platform of 'tag ]

type 'a t = 'a Eio.Resource.t constraint 'a = [> [> `Generic ] ty ]

type plain = [ `Generic ] ty Eio.Resource.t

type body = Middleware.body =
  | Empty
  | String of string
  | Stream of {
      length : int64 option;
      flow : Eio.Flow.source_ty Eio.Resource.t;
    }

type response = Middleware.response

module Header = Header

module Redirect = Redirect

val get :
  sw:Eio.Switch.t ->
  ?headers:Header.headers ->
  ?redirects:int ->
  _ t ->
  string ->
  response

val head :
  sw:Eio.Switch.t ->
  ?headers:Header.headers ->
  ?redirects:int ->
  _ t ->
  string ->
  response

val post :
  sw:Eio.Switch.t ->
  ?headers:Header.headers ->
  ?redirects:int ->
  body:body ->
  _ t ->
  string ->
  response

val put :
  sw:Eio.Switch.t ->
  ?headers:Header.headers ->
  ?redirects:int ->
  body:body ->
  _ t ->
  string ->
  response

val delete :
  sw:Eio.Switch.t ->
  ?headers:Header.headers ->
  ?redirects:int ->
  _ t ->
  string ->
  response

val patch :
  sw:Eio.Switch.t ->
  ?headers:Header.headers ->
  ?redirects:int ->
  body:body ->
  _ t ->
  string ->
  response

val options :
  sw:Eio.Switch.t ->
  ?headers:Header.headers ->
  ?redirects:int ->
  _ t ->
  string ->
  response

val read : ?limit:int -> _ t -> string -> string

val fetch :
  sw:Eio.Switch.t ->
  ?headers:Header.headers ->
  ?body:body ->
  ?redirects:int ->
  ?allow_downgrade:bool ->
  ?redirect:Redirect.config ->
  ?sensitive:string list ->
  _ t ->
  Http.Method.t ->
  string ->
  response

val with_response :
  ?headers:Header.headers ->
  ?body:body ->
  ?redirects:int ->
  ?allow_downgrade:bool ->
  ?redirect:Redirect.config ->
  ?sensitive:string list ->
  _ t ->
  Http.Method.t ->
  string ->
  (response -> 'a) ->
  'a

val stream : ?length:int64 -> _ Eio.Flow.source -> body

exception Idle_timeout of float

val with_idle_timeout :
  clock:_ Eio.Time.clock ->
  seconds:Duration.t ->
  _ Eio.Flow.source ->
  Eio.Flow.source_ty Eio.Resource.t

module Form = Form

module Media = Httpz_media

module Json = Httpz_media_jsont

module Markdown = Httpz_media_cmarkit

val encode : 'a Media.t -> 'a -> Header.headers * body

val decode : ?limit:int -> 'a Media.t -> response -> 'a

val get_as :
  sw:Eio.Switch.t ->
  ?headers:Header.headers ->
  ?redirects:int ->
  _ t ->
  'a Media.t ->
  string ->
  ('a, response) result

val read_as : ?limit:int -> _ t -> 'a Media.t -> string -> ('a, response) result

exception Rejected of response

val expect : ('a, response) result -> 'a

val decode_seq : ?max_line:int -> 'a Media.seq -> response -> 'a Seq.t

val encode_seq : 'a Media.seq -> 'a Seq.t -> Header.headers * body

val status : response -> int

val headers : response -> Http.Header.t

val body : response -> Eio.Flow.source_ty Eio.Resource.t

val url : response -> string

val close : response -> unit

val scope : response -> string list

type version = [ Http.Version.t | `HTTP_2 ]

val version : response -> version

val trailers : response -> Http.Header.t option

val header : 'a Header.t -> response -> 'a option

val pp_response : response Fmt.t

val restrict :
  ?under:string list ->
  ?methods:Http.Method.t list ->
  ?filter:(Middleware.request -> [ `Allow | `Reject of string ]) ->
  _ t ->
  plain

val read_only : _ t -> plain

val with_headers :
  ?scope:string list ->
  ?mode:[ `Set | `Add | `If_absent ] ->
  Header.headers ->
  _ t ->
  plain

module Credential = Credential

val with_credentials :
  scope:string list ->
  ?allow_insecure:bool ->
  ?extend:bool ->
  Credential.t list ->
  _ t ->
  plain

val with_limits :
  clock:_ Eio.Time.Mono.t ->
  ?scope:string list ->
  ?min_interval:Duration.t ->
  ?max_concurrent:int ->
  _ t ->
  plain

module Retry = Retry

val with_retry :
  clock:_ Eio.Time.Mono.t ->
  random:_ Eio.Flow.source ->
  ?wall:_ Eio.Time.clock ->
  ?config:Retry.config ->
  _ t ->
  plain

type error = Middleware.error =
  | Invalid_url of string

  | Invalid_request of string

  | Denied of string

  | Connection_failure of Eio.Net.connection_failure

  | Tls_failure of string

  | Protocol_error of string

  | Too_many_redirects

  | Body_not_replayable

  | Decode_failure of { media : string; error : Media.error }

type Eio.Exn.err +=
  | E of error

val err : error -> exn

module Middleware = Middleware

val with_accept : string -> Header.headers option -> Header.headers
val decode_failure : response -> string -> Media.error -> exn
val content_type : response -> string option
val is_success : response -> bool
