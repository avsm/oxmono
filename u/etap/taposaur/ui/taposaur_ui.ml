open Htmlit

(* The generic widgets are Sitekit's. What stays here is Taposaur's own
   styling and the coverage table with its count or percentage choice. *)
let css = Stylesheet.source
let node name attrs children =
  El.v name ~at:(List.map (fun (k,v) -> At.v k v) attrs) children
let txt = El.txt
let badge ?(tone=`Neutral) label =
  Sitekit.badge ~tone:(tone :> [ `Neutral | `Good | `Warning | `Danger ]) label
let metric ~label value = Sitekit.metric ~label value
let input ~name ~label ~kind ~value = Sitekit.input ~name ~label ~kind ~value ()
let select ~name ~label ~value options = Sitekit.select ~name ~label ~value options
let checkbox ~id ~label ~checked = Sitekit.checkbox ~id ~label ~checked
let cell ?ratio value children = Sitekit.cell ~sort:value ?ratio children
let table ?(class_name="") ?(gap_sort=false) headers rows =
  node "div" ["class","snapshot-table-wrap ui-table " ^ class_name;
    "data-sort-scope",""] (
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
(* The bar is Sitekit's image, inside the element Taposaur's layout sizes. *)
let coverage ~present ~total =
  node "span" ["class","coverage-spark" ^ (if total = 0 then " empty" else "")]
    [Sitekit.coverage ~present ~total]
let state_sprite = Sitekit.state_sprite
let state_icon ?label state = Sitekit.state_icon ?label state
