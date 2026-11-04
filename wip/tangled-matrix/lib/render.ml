(* SPDX-License-Identifier: ISC *)
module W = Tangled.Watch

type t = { plain : string; html : string }

let icon = function
  | "success" -> "✅"
  | "failed" -> "❌"
  | "timeout" -> "⏱️"
  | "cancelled" -> "🚫"
  | "running" -> "🔄"
  | "blocked" -> "✋"
  | _ -> "⏳"

let label = function
  | "success" -> "passed"
  | "failed" -> "failed"
  | "timeout" -> "timed out"
  | "cancelled" -> "cancelled"
  | "running" -> "running"
  | "blocked" -> "waiting for approval"
  | _ -> "pending"

let escape s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '"' -> Buffer.add_string b "&quot;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let short sha = if String.length sha > 7 then String.sub sha 0 7 else sha

(* Errors are build output, so a long one is cut to its first line. *)
let first_line ?(max = 200) s =
  let s =
    match String.index_opt s '\n' with Some i -> String.sub s 0 i | None -> s
  in
  if String.length s <= max then s else String.sub s 0 max ^ "..."

let pipeline ?appview ~name (p : W.pipeline) =
  let status = W.status p in
  let url = W.web_url ?appview p in
  let passed =
    List.length
      (List.filter (fun (w : W.workflow) -> w.status = "success") p.workflows)
  in
  let total = List.length p.workflows in
  let what =
    W.trigger_kind p
    ^ Option.fold ~none:"" ~some:(fun r -> " " ^ r) (W.trigger_ref p)
  in
  let plain =
    Printf.sprintf "%s %s: %s %s %s, %d of %d passed\n" (icon status) name what
      (short p.commit) (label status) passed total
    ^ String.concat ""
        (List.map
           (fun (w : W.workflow) ->
             Printf.sprintf "%s %s%s\n" (icon w.status) w.name
               (Option.fold ~none:""
                  ~some:(fun e -> ": " ^ first_line e)
                  w.error))
           p.workflows)
    ^ url
  in
  let html =
    Printf.sprintf
      "<p>%s <b><a href=\"%s\">%s</a></b> %s <code>%s</code>: <b>%s</b>, %d of \
       %d passed</p>"
      (icon status) (escape url) (escape name) (escape what)
      (escape (short p.commit))
      (label status) passed total
    ^
    if p.workflows = [] then ""
    else
      "<ul>"
      ^ String.concat ""
          (List.map
             (fun (w : W.workflow) ->
               Printf.sprintf "<li>%s <code>%s</code>%s</li>" (icon w.status)
                 (escape w.name)
                 (Option.fold ~none:""
                    ~some:(fun e -> ": " ^ escape (first_line e))
                    w.error))
             p.workflows)
      ^ "</ul>"
  in
  { plain; html }
