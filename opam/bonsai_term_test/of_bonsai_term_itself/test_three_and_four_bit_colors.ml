open! Core
open Bonsai_test
open Bonsai_term

let app ~dimensions:_ (local_ _graph) =
  Bonsai.return
    (View.vcat
       [ View.text ~attrs:[ Attr.fg Attr.Color.Expert.black ] "black"
       ; View.text ~attrs:[ Attr.fg Attr.Color.Expert.red ] "red"
       ; View.text ~attrs:[ Attr.fg Attr.Color.Expert.green ] "green"
       ; View.text ~attrs:[ Attr.fg Attr.Color.Expert.yellow ] "yellow"
       ; View.text ~attrs:[ Attr.fg Attr.Color.Expert.blue ] "blue"
       ; View.text ~attrs:[ Attr.fg Attr.Color.Expert.magenta ] "magenta"
       ; View.text ~attrs:[ Attr.fg Attr.Color.Expert.cyan ] "cyan"
       ; View.text ~attrs:[ Attr.fg Attr.Color.Expert.white ] "white"
       ; View.text ~attrs:[ Attr.fg Attr.Color.Expert.lightblack ] "lightblack"
       ; View.text ~attrs:[ Attr.fg Attr.Color.Expert.lightred ] "lightred"
       ; View.text ~attrs:[ Attr.fg Attr.Color.Expert.lightgreen ] "lightgreen"
       ; View.text ~attrs:[ Attr.fg Attr.Color.Expert.lightyellow ] "lightyellow"
       ; View.text ~attrs:[ Attr.fg Attr.Color.Expert.lightblue ] "lightblue"
       ; View.text ~attrs:[ Attr.fg Attr.Color.Expert.lightmagenta ] "lightmagenta"
       ; View.text ~attrs:[ Attr.fg Attr.Color.Expert.lightcyan ] "lightcyan"
       ; View.text ~attrs:[ Attr.fg Attr.Color.Expert.lightwhite ] "lightwhite"
       ; View.text ~attrs:[ Attr.fg Attr.Color.Expert.default ] "default"
       ; View.text ~attrs:[ Attr.bg Attr.Color.Expert.blue ] "bg blue"
       ; View.text
           ~attrs:[ Attr.fg Attr.Color.Expert.yellow; Attr.bg Attr.Color.Expert.magenta ]
           "yellow on magenta"
       ])
;;

let%expect_test "Not_ansi" =
  let handle =
    Bonsai_term_test.create_handle_without_handler
      ~capability:Not_ansi
      ~initial_dimensions:{ width = 40; height = 19 }
      app
  in
  Handle.show handle;
  [%expect
    {|
    ┌────────────────────────────────────────┐
    │black                                   │
    │red                                     │
    │green                                   │
    │yellow                                  │
    │blue                                    │
    │magenta                                 │
    │cyan                                    │
    │white                                   │
    │lightblack                              │
    │lightred                                │
    │lightgreen                              │
    │lightyellow                             │
    │lightblue                               │
    │lightmagenta                            │
    │lightcyan                               │
    │lightwhite                              │
    │default                                 │
    │bg blue                                 │
    │yellow on magenta                       │
    └────────────────────────────────────────┘
    |}]
;;

let%expect_test "Ansi" =
  let handle =
    Bonsai_term_test.create_handle_without_handler
      ~capability:Ansi
      ~initial_dimensions:{ width = 40; height = 19 }
      app
  in
  Handle.show handle;
  [%expect
    {|
    (off fg:black)black(off)(EraseLine:ToEnd)
    (off fg:red)red(off)(EraseLine:ToEnd)
    (off fg:green)green(off)(EraseLine:ToEnd)
    (off fg:yellow)yellow(off)(EraseLine:ToEnd)
    (off fg:blue)blue(off)(EraseLine:ToEnd)
    (off fg:magenta)magenta(off)(EraseLine:ToEnd)
    (off fg:cyan)cyan(off)(EraseLine:ToEnd)
    (off fg:white)white(off)(EraseLine:ToEnd)
    (off fg:gray)lightblack(off)(EraseLine:ToEnd)
    (off fg:bright-red)lightred(off)(EraseLine:ToEnd)
    (off fg:bright-green)lightgreen(off)(EraseLine:ToEnd)
    (off fg:bright-yellow)lightyellow(off)(EraseLine:ToEnd)
    (off fg:bright-blue)lightblue(off)(EraseLine:ToEnd)
    (off fg:bright-magenta)lightmagenta(off)(EraseLine:ToEnd)
    (off fg:bright-cyan)lightcyan(off)(EraseLine:ToEnd)
    (off fg:bright-white)lightwhite(off)(EraseLine:ToEnd)
    (off)default(off)(EraseLine:ToEnd)
    (off bg:blue)bg blue(off)(EraseLine:ToEnd)
    (off fg:yellow bg:magenta)yellow on magenta(off)(EraseLine:ToEnd)(off)
    |}]
;;
