open Htmlit
let css = Stylesheet.source
let javascript = Widgets.source
let node name attrs children =
  El.v name ~at:(List.map (fun (k,v) -> At.v k v) attrs) children
let txt = El.txt
let badge ?(tone=`Neutral) label =
  node "span" ["class", "ui-badge " ^ (match tone with
    | `Neutral -> "neutral" | `Good -> "good" | `Warning -> "warning")]
    [txt label]
let metric ~label value = node "div" ["class","ui-metric"]
  [node "strong" [] [value];node "span" [] [txt label]]
let input ~name ~label ~kind ~value = node "label" [] [txt label;
  node "input" ["name",name;"type",kind;"value",value] []]
let select ~name ~label ~value options = node "label" [] [txt label;
  node "select" ["name",name] (List.map (fun (v,label) ->
    node "option" (["value",v] @ if v = value then ["selected",""] else [])
      [txt label]) options)]
let checkbox ~id ~label ~checked = node "label" ["class","ui-check";"for",id]
  [node "input" (["type","checkbox";"id",id] @
    if checked then ["checked",""] else []) [];txt label]
let cell ?ratio value children =
  node "td" (["data-sort-value",value] @ Option.fold ~none:[]
    ~some:(fun r -> ["data-sort-ratio",Printf.sprintf "%.12g" r]) ratio) children
let table ?(class_name="") ?(gap_sort=false) headers rows =
  node "div" ["class","snapshot-table-wrap ui-table " ^ class_name] (
    (if gap_sort then [node "label" ["class","sort-basis"]
      [txt "Sort coverage by";node "select" ["data-sort-basis",""]
        [node "option" ["value","count"] [txt "Paper count"];
         node "option" ["value","ratio"] [txt "Percentage"]]]] else []) @
    [node "div" ["class","table-wrap"] [node "table"
      ["class","snapshot-table";"data-sortable",""]
      [node "thead" [] [node "tr" [] (List.mapi (fun i (label,numeric) ->
        node "th" ["scope","col";"aria-sort","none"] [node "button"
          ["type","button";"data-sort-column",string_of_int i;
           "data-sort-type",if numeric then "number" else "text"]
          [txt label;node "span" ["class","sort-arrow";"aria-hidden","true"]
            [txt "↕"]]]) headers)];
       node "tbody" [] (List.map (fun cells -> node "tr" [] cells) rows)]];
     node "p" ["class","sort-status sr-only";"role","status";
       "aria-live","polite"] []])
let coverage ~present ~total =
  let pct = if total = 0 then 0. else 100. *. float present /. float total in
  node "span" ["class","coverage-spark" ^ (if total = 0 then " empty" else "");
    "role","img";"aria-label",(if total = 0 then "No papers" else
      Printf.sprintf "%d present (%.1f%%), %d missing (%.1f%%)"
        present pct (total-present) (100.-.pct));
    "style",Printf.sprintf "--present:%.4f%%" pct] []
