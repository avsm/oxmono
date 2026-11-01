open Taposaur
module A = Taposaur_archive
module D = Taposaur_document

type t = {
  uid : string; file : string; converter : string option;
  status : string; detail : string; sha256 : string;
  observed_bytes : int; source_pdf : string option; pdf_status : string;
  source_stamp : string; source_changed : bool; action : string;
}

let codec =
  Jsont.Object.map (fun uid file converter status detail sha256 observed_bytes
    source_pdf pdf_status source_stamp source_changed action ->
    {uid; file; converter; status; detail; sha256; observed_bytes; source_pdf;
     pdf_status; source_stamp; source_changed; action})
  |> Jsont.Object.mem "uid" Jsont.string ~enc:(fun r -> r.uid)
  |> Jsont.Object.mem "file" Jsont.string ~enc:(fun r -> r.file)
  |> Jsont.Object.opt_mem "converter" Jsont.string ~enc:(fun r -> r.converter)
  |> Jsont.Object.mem "status" Jsont.string ~enc:(fun r -> r.status)
  |> Jsont.Object.mem "detail" Jsont.string ~enc:(fun r -> r.detail)
  |> Jsont.Object.mem "sha256" Jsont.string ~enc:(fun r -> r.sha256)
  |> Jsont.Object.mem "observed_bytes" Jsont.int ~enc:(fun r -> r.observed_bytes)
  |> Jsont.Object.opt_mem "source_pdf" Jsont.string ~enc:(fun r -> r.source_pdf)
  |> Jsont.Object.mem "pdf_status" Jsont.string ~enc:(fun r -> r.pdf_status)
  |> Jsont.Object.mem "source_stamp" Jsont.string ~enc:(fun r -> r.source_stamp)
  |> Jsont.Object.mem "source_changed" Jsont.bool
       ~enc:(fun r -> r.source_changed)
  |> Jsont.Object.mem "action" Jsont.string ~enc:(fun r -> r.action)
  |> Jsont.Object.finish

let to_json r = match Jsont_bytesrw.encode_string codec r with
  | Ok s -> s | Error e -> failwith e

let paper archive uid =
  let p = match A.find archive uid with
    | Some p -> p | None -> failwith ("Missing paper: " ^ Uid.to_string uid) in
  let artifacts = (A.manifest p).artifacts in
  let before = A.fingerprint p in
  let reports = artifacts |> List.filter_map (fun (a : Manifest.Artifact.t) ->
    if a.format <> Manifest.Format.Tei then None else
    let pdfs = List.filter (fun (a : Manifest.Artifact.t) ->
      a.format = Manifest.Format.Pdf) artifacts in
    let pdf = match a.from with
      | Some name -> List.find_opt (fun (p : Manifest.Artifact.t) ->
          p.file = name) pdfs
      | None -> (match pdfs with [p] -> Some p | _ -> None) in
    let pdf_status = match pdf with
      | None -> "no_unambiguous_pdf_input"
      | Some pdf ->
          try A.with_artifact p pdf.file (fun flow ->
            let header = Eio.Buf_read.(take 5 (of_flow ~max_size:16 flow)) in
            if header <> "%PDF-" then "invalid_pdf_signature"
            else if Optint.Int63.to_int64 (Eio.File.size flow) <> pdf.bytes then
              "pdf_size_mismatch"
            else "pdf_signature_and_size_match")
          with End_of_file -> "truncated_pdf"
          | Eio.Io _ as ex -> "unreadable_pdf: " ^ Printexc.to_string ex in
    let status, detail, sha256, observed_bytes = try
      let text = A.read_artifact ~max_bytes:(16 * 1024 * 1024) p a.file in
      let sha256 = Digestif.SHA256.(to_hex (digest_string text)) in
      let trimmed = String.trim text in
      let code = List.find_opt (fun code ->
        String.starts_with ~prefix:("[" ^ code ^ "] ") trimmed)
        ["NO_BLOCKS"; "GENERAL"] in
      let status, detail = match a.converter, code with
        | Some "grobid", Some code -> "grobid_error_" ^ code,
            String.sub trimmed 0 (min 1024 (String.length trimmed))
        | _ -> match D.Tei.validate_string text with
          | Error error -> "invalid_tei", error
          | Ok () -> match D.Tei.metadata (Eio.Flow.string_source text) with
            | Ok _ -> "well_formed_tei", "Complete XML document checked"
            | Error e -> "well_formed_tei_metadata_error", e in
      status, detail, sha256, String.length text
    with
    | Eio.Buf_read.Buffer_limit_exceeded ->
        "validation_limit", "Artifact exceeds 16 MiB", "", 0
    | Eio.Io _ as ex -> "unreadable_tei", Printexc.to_string ex, "", 0 in
    let action = match status with
      | "well_formed_tei" -> "none"
      | "grobid_error_NO_BLOCKS" -> "inspect_pdf_for_ocr_or_replacement"
      | "grobid_error_GENERAL"
        when pdf_status = "pdf_signature_and_size_match" ->
          "candidate_for_grobid_regeneration"
      | "grobid_error_GENERAL" -> "repair_pdf_input_before_regeneration"
      | _ -> "manual_review" in
    Some {uid = Uid.to_string uid; file = a.file; converter = a.converter;
      status; detail; sha256; observed_bytes;
      source_pdf = Option.map (fun (a : Manifest.Artifact.t) -> a.file) pdf;
      pdf_status; source_stamp = before; source_changed = false; action}) in
  let changed = match A.find archive uid with
    | None -> true | Some fresh -> A.fingerprint fresh <> before in
  List.map (fun r -> if changed then
    {r with source_changed = true; action = "reaudit_changed_source"} else r)
    reports
