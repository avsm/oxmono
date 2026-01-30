open! Core
open! Async
open! Import

(* Useful online HTML editor to visualize the output: https://html-online.com/editor/ *)
let make_input_csv ?sep filename =
  make_input_csv
    ?sep
    filename
    [ [ "fruit"; "quantity"; "owner" ]
    ; [ "apple"; "4"; "Abraham" ]
    ; [ "apple"; "6"; "Bathsheba" ]
    ; [ "orange"; "2"; "Cyrus" ]
    ; [ {|<a href="https://en.wikipedia.org/wiki/Kiwifruit">kiwi</a>|}; "3"; "Darragh" ]
    ]
;;

let test args =
  let%bind output = Process.run_exn ~prog:"csv" ~args:("to-html-table" :: args) () in
  run "xml_pp" [] ~stdin:output
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = test [ "input.csv" ] in
    [%expect
      {|
      <table border="0">
        <tr>
          <th>fruit</th>
          <th>quantity</th>
          <th>owner</th>
        </tr>
        <tr>
          <td>apple</td>
          <td>4</td>
          <td>Abraham</td>
        </tr>
        <tr>
          <td>apple</td>
          <td>6</td>
          <td>Bathsheba</td>
        </tr>
        <tr>
          <td>orange</td>
          <td>2</td>
          <td>Cyrus</td>
        </tr>
        <tr>
          <td>&lt;a href=&quot;https://en.wikipedia.org/wiki/Kiwifruit&quot;&gt;kiwi&lt;/a&gt;</td>
          <td>3</td>
          <td>Darragh</td>
        </tr>
      </table>
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = test [ "-border"; "-td"; "align=center"; "input.csv" ] in
    [%expect
      {|
      <table border="1">
        <tr>
          <th>fruit</th>
          <th>quantity</th>
          <th>owner</th>
        </tr>
        <tr>
          <td align="center">apple</td>
          <td align="center">4</td>
          <td align="center">Abraham</td>
        </tr>
        <tr>
          <td align="center">apple</td>
          <td align="center">6</td>
          <td align="center">Bathsheba</td>
        </tr>
        <tr>
          <td align="center">orange</td>
          <td align="center">2</td>
          <td align="center">Cyrus</td>
        </tr>
        <tr>
          <td align="center">&lt;a href=&quot;https://en.wikipedia.org/wiki/Kiwifruit&quot;&gt;kiwi&lt;/a&gt;</td>
          <td align="center">3</td>
          <td align="center">Darragh</td>
        </tr>
      </table>
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv ~sep:'.' "input.csv" in
    let%bind () = test [ "-sep"; "."; "-th"; "bgcolor=red"; "input.csv" ] in
    [%expect
      {|
      <table border="0">
        <tr>
          <th bgcolor="red">fruit</th>
          <th bgcolor="red">quantity</th>
          <th bgcolor="red">owner</th>
        </tr>
        <tr>
          <td>apple</td>
          <td>4</td>
          <td>Abraham</td>
        </tr>
        <tr>
          <td>apple</td>
          <td>6</td>
          <td>Bathsheba</td>
        </tr>
        <tr>
          <td>orange</td>
          <td>2</td>
          <td>Cyrus</td>
        </tr>
        <tr>
          <td>&lt;a href=&quot;https://en.wikipedia.org/wiki/Kiwifruit&quot;&gt;kiwi&lt;/a&gt;</td>
          <td>3</td>
          <td>Darragh</td>
        </tr>
      </table>
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = test [ "-nh"; "-table"; "cellpadding=5"; "input.csv" ] in
    [%expect
      {|
      <table border="0" cellpadding="5">
        <tr>
          <td>fruit</td>
          <td>quantity</td>
          <td>owner</td>
        </tr>
        <tr>
          <td>apple</td>
          <td>4</td>
          <td>Abraham</td>
        </tr>
        <tr>
          <td>apple</td>
          <td>6</td>
          <td>Bathsheba</td>
        </tr>
        <tr>
          <td>orange</td>
          <td>2</td>
          <td>Cyrus</td>
        </tr>
        <tr>
          <td>&lt;a href=&quot;https://en.wikipedia.org/wiki/Kiwifruit&quot;&gt;kiwi&lt;/a&gt;</td>
          <td>3</td>
          <td>Darragh</td>
        </tr>
      </table>
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = test [ "-sh"; "input.csv" ] in
    [%expect
      {|
      <table border="0">
        <tr>
          <td>apple</td>
          <td>4</td>
          <td>Abraham</td>
        </tr>
        <tr>
          <td>apple</td>
          <td>6</td>
          <td>Bathsheba</td>
        </tr>
        <tr>
          <td>orange</td>
          <td>2</td>
          <td>Cyrus</td>
        </tr>
        <tr>
          <td>&lt;a href=&quot;https://en.wikipedia.org/wiki/Kiwifruit&quot;&gt;kiwi&lt;/a&gt;</td>
          <td>3</td>
          <td>Darragh</td>
        </tr>
      </table>
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = test [ "-unescaped-html"; "input.csv" ] in
    [%expect
      {|
      <table border="0">
        <tr>
          <th>fruit</th>
          <th>quantity</th>
          <th>owner</th>
        </tr>
        <tr>
          <td>apple</td>
          <td>4</td>
          <td>Abraham</td>
        </tr>
        <tr>
          <td>apple</td>
          <td>6</td>
          <td>Bathsheba</td>
        </tr>
        <tr>
          <td>orange</td>
          <td>2</td>
          <td>Cyrus</td>
        </tr>
        <tr>
          <td>
            <a href="https://en.wikipedia.org/wiki/Kiwifruit">kiwi</a>
          </td>
          <td>3</td>
          <td>Darragh</td>
        </tr>
      </table>
      |}];
    return ())
;;

let%expect_test "suppress & no header interaction" =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = test [ "-sh"; "-nh"; "input.csv" ] in
    [%expect
      {|
      <table border="0">
        <tr>
          <td>apple</td>
          <td>4</td>
          <td>Abraham</td>
        </tr>
        <tr>
          <td>apple</td>
          <td>6</td>
          <td>Bathsheba</td>
        </tr>
        <tr>
          <td>orange</td>
          <td>2</td>
          <td>Cyrus</td>
        </tr>
        <tr>
          <td>&lt;a href=&quot;https://en.wikipedia.org/wiki/Kiwifruit&quot;&gt;kiwi&lt;/a&gt;</td>
          <td>3</td>
          <td>Darragh</td>
        </tr>
      </table>
      |}];
    return ())
;;
