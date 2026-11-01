(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Vertical bars: a week's load as a bar chart in a panel, then a few
   metrics, each with a sparkline of its last readings and its current
   value. [--theme] draws them in another theme. *)

open Console

let week =
  [
    ("Mon", 12.);
    ("Tue", 35.);
    ("Wed", 81.);
    ("Thu", 55.);
    ("Fri", 93.);
    ("Sat", 41.);
    ("Sun", 7.);
  ]

let metrics =
  [
    ("cpu", [ 12.; 18.; 35.; 61.; 88.; 74.; 52.; 40.; 33.; 47.; 66.; 62. ]);
    ("memory", [ 30.; 31.; 35.; 38.; 41.; 44.; 45.; 47.; 46.; 48.; 48.; 48. ]);
    ("disk", [ 5.; 5.; 6.; 7.; 7.; 8.; 9.; 9.; 10.; 11.; 12.; 12. ]);
  ]

let current values = Fmt.str "%.0f%%" (List.nth values (List.length values - 1))

(* The readings are percentages: every sparkline spans 0 to 100, so a disk
   12% full is a low bar, in the colour of a low value. *)
let sparkline values = Bar.sparkline ~min:0. ~max:100. values

let readings =
  Table.view ~border:Border.none
    [
      Table.column (View.text "");
      Table.column (View.text "last hour");
      Table.column ~align:`Right (View.text "now");
    ]
    (List.map
       (fun (name, values) ->
         [ View.text name; sparkline values; View.text (current values) ])
       metrics)

let main theme =
  Console_tty.setup ~theme ();
  let title s = View.role `Header (View.text s) in
  Fmt.pr "%a@.%a@.@.%a@.%a@." View.pp (title "Load this week") View.pp
    (Panel.view (Bar.chart ~height:6 week))
    View.pp (title "Machine") View.pp readings

let theme =
  Cmdliner.Arg.(value & opt (enum Theme.all) Theme.default & info [ "theme" ])

let cmd = Cmdliner.Cmd.(v (info "charts") Cmdliner.Term.(const main $ theme))
let () = exit (Cmdliner.Cmd.eval cmd)
