open Taposaur
open Proffer
module A = Taposaur_archive

type range = Full | Part of int64 * int64 | Unsatisfiable
let range size = function
  | None -> Full
  | Some s when not (String.starts_with ~prefix:"bytes=" s) -> Full
  | Some s ->
      let s = String.sub s 6 (String.length s - 6) in
      let integer s =
        if s = "" || not (String.for_all (function
          | '0'..'9' -> true | _ -> false) s) then None
        else Int64.of_string_opt s in
      match String.split_on_char '-' s with
      | ["";suffix] ->
          (match integer suffix with
           | Some n when n > 0L && size > 0L ->
               let n = min n size in Part (Int64.sub size n, n)
           | Some _ -> Unsatisfiable | None -> Full)
      | [first;last] ->
          (match integer first, (if last = "" then Some (Int64.pred size)
            else integer last) with
           | Some first, Some last when first >= size || last < first ->
               Unsatisfiable
           | Some first, Some last ->
               Part (first,
                 Int64.succ (Int64.sub (min last (Int64.pred size)) first))
           | _ -> Full)
      | _ -> Full

let content_type (a : Manifest.Artifact.t) = match a.format with
  | Manifest.Format.Pdf -> "application/pdf"
  | Json -> "application/json" | Xml | Tei -> "application/xml"

let stream p (a : Manifest.Artifact.t) requested (respond @ local) =
  let observed = A.with_artifact p a.file Eio.File.stat in
  let size = Optint.Int63.to_int64 observed.size in
  let r = range size requested in
  let common = [Resp.other "Accept-Ranges" "bytes";
    Resp.other "X-Content-Type-Options" "nosniff";
    Resp.other "Content-Security-Policy"
      (if a.format = Manifest.Format.Pdf then "frame-ancestors 'self'"
       else "sandbox")] in
  match r with
  | Unsatisfiable ->
      Resp.text respond ~status:Httpz.Res.Range_not_satisfiable
        ~headers:(Resp.other "Content-Range"
          ("bytes */" ^ Int64.to_string size) :: common) "Range not satisfiable"
  | Full | Part _ ->
      let offset, length, status, headers = match r with
        | Part (offset, length) ->
            let finish = Int64.pred (Int64.add offset length) in
            offset, length, Httpz.Res.Partial_content,
            Resp.other "Content-Range"
              (Printf.sprintf "bytes %Ld-%Ld/%Ld" offset finish size) :: common
        | _ -> 0L, size, Httpz.Res.Success, common in
      Resp.stream respond ~status ~headers ~length (content_type a) (fun sink ->
        A.with_artifact p a.file @@ fun flow ->
        let current = Eio.File.stat flow in
        if current.ino <> observed.ino || current.size <> observed.size ||
          current.mtime <> observed.mtime then
          failwith "Artifact changed before streaming";
        let buffer = Cstruct.create 65536 in
        let rec copy offset remaining =
          if remaining > 0L then begin
            let n = Int64.to_int (min 65536L remaining) in
            let part = Cstruct.sub buffer 0 n in
            Eio.File.pread_exact flow
              ~file_offset:(Optint.Int63.of_int64 offset)
              [part];
            Body.Sink.write sink (Cstruct.to_string part);
            copy (Int64.add offset (Int64.of_int n))
              (Int64.sub remaining (Int64.of_int n))
          end
        in copy offset length)

let send paper a req (respond @ local) =
  let requested =
    if Req.meth req <> Httpz.Method.Get ||
      Req.header req Httpz.Header_name.If_range <> None then None
    else match Req.header req Httpz.Header_name.Range with
      | None -> None | Some s -> Some (Req.globalize s)
  in
  stream paper a requested respond
