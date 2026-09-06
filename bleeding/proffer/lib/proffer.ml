module Duration = Duration

module Media = Httpz_media
module Json = Httpz_media_jsont
module Markdown = Httpz_media_cmarkit
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
  let emit_sub sink #(value, off, len) =
    if off = 0 && len = String.length value then Body.Sink.write sink value
    else Body.Sink.write_sub sink (Bytes.unsafe_of_string value) ~off ~len
  let send sink ?name ?id data = Httpz_media.Sse.send_sub (emit_sub sink) ?name ?id data
  let comment sink text = Httpz_media.Sse.comment_sub (emit_sub sink) text
  let retry sink milliseconds = Httpz_media.Sse.retry (emit sink) milliseconds

  let respond respond ?retry:retry_ms write =
    Option.iter
      (fun milliseconds ->
        if milliseconds < 0 then
          invalid_arg "Proffer.Sse.respond: retry is negative")
      retry_ms;
    Resp.stream respond ~cache:Cache_control.no_store Httpz_media.Sse.media_type
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
