module Media = Httpz.Media
module Json = Httpz.Json
module Markdown = Media_cmarkit
module Method = Method
module Status = Status
module Headers = Headers
module Mime = Mime
module Req = Req
module Multipart = Multipart
module Cache_control = Cache_control
module Body = Body
module Etag = Etag
module Resp = Resp
module Sse = struct
  type sink = Body.Sink.t

  let emit sink value = Body.Sink.write sink value
  let send sink ?name ?id data = Httpz.Sse.send (emit sink) ?name ?id data
  let comment sink text = Httpz.Sse.comment (emit sink) text
  let retry sink milliseconds = Httpz.Sse.retry (emit sink) milliseconds

  let respond respond ?retry:retry_ms write =
    Option.iter
      (fun milliseconds ->
        if milliseconds < 0 then
          invalid_arg "Proffer.Sse.respond: retry is negative")
      retry_ms;
    Resp.stream respond ~cache:Cache_control.no_store Httpz.Sse.media_type
      (fun sink ->
        Option.iter (retry sink) retry_ms;
        write sink)
end
module Route = Route
module Site = Site
module Negotiate = Negotiate
module Static = Static
module Cache = Cache
module Backend = Backend
