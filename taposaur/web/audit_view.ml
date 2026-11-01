module A = Taposaur_audit
module Ui = Taposaur_ui
open View
let render params events =
  let value name = Option.value ~default:"" (List.assoc_opt name params) in
  let field name label = Ui.input ~name ~label ~kind:"text" ~value:(value name) in
  let format_time seconds =
    let t = Unix.gmtime seconds in
    Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d UTC"
      (t.tm_year+1900) (t.tm_mon+1) t.tm_mday t.tm_hour t.tm_min t.tm_sec in
  let node = Ui.node in
  let filters = List.filter (fun (k,_) -> k <> "before" && k <> "format") params in
  let rows = List.map (fun (e : A.event) ->
    let phase = A.phase_name e.phase in
    let tone = match e.phase with A.Succeeded -> `Good
      | Failed | Cancelled -> `Warning | _ -> `Neutral in
    [Ui.cell (Int64.to_string e.id) [txt (Int64.to_string e.id)];
     Ui.cell (Printf.sprintf "%.6f" e.time) [txt (format_time e.time)];
     Ui.cell e.actor [link (url "/audit" ["actor",e.actor]) e.actor];
     Ui.cell e.action [txt e.action];
     Ui.cell e.subject [link (url "/audit" ["subject",e.subject]) e.subject];
     Ui.cell phase [Ui.badge ~tone phase];
     Ui.cell e.details [el "details" [el "summary" [txt "Details"];
       p ("Component: " ^ e.component);
       el "p" [txt "Run: ";link (url "/audit" ["run",e.run]) e.run];
       el "p" [txt "Operation: ";link (url "/audit" ["operation",e.operation]) e.operation];
       Option.fold ~none:Htmlit.El.void ~some:(fun id -> p ("Parent: " ^ id)) e.parent;
       el "pre" [txt e.details]]]]) events in
  page ~kind:"audit-page" "Audit log" [
    p "Newest first. Audit events are append-only. Failed operations may have partial effects; an incomplete operation may still be running or need recovery.";
    node "form" ["method","get";"action","/audit"]
      [node "div" ["class","filters"]
        [field "actor" "Actor";field "action" "Action";
         field "subject" "Subject / paper UID";field "run" "Run ID";
         field "operation" "Operation ID";
         Ui.select ~name:"phase" ~label:"Phase" ~value:(value "phase")
           ["","Any";"started","Started";"succeeded","Succeeded";
            "failed","Failed";"cancelled","Cancelled";"notice","Notice"];
         Ui.select ~name:"incomplete" ~label:"Completion" ~value:(value "incomplete")
           ["","All events";"1","Incomplete starts"];
         el "button" [txt "Filter"]]];
    el "p" [link (url "/audit" (("format","jsonl")::filters)) "Export this page as JSONL";
      txt " · ";link "/audit?incomplete=1" "Incomplete operations"];
    (if rows=[] then p "No matching audit events." else
      Ui.table ["ID",true;"Time",true;"Actor",false;"Action",false;
        "Subject",false;"Phase",false;"Details",false] rows);
    (if List.length events < 100 then Htmlit.El.void else
      let last = List.hd (List.rev events) in
      el "p" [link (url "/audit" (("before",Int64.to_string last.id)::filters))
        "Older events →"])]
