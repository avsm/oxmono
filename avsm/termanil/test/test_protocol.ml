(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open! Core
module P = Termanil_protocol

let%expect_test
    "versioned protocol round trips text without executable interpretation" =
  let m =
    {
      (List.hd_exn Termanil_demo.messages) with
      subject = "quote \" newline\n$(command) \027";
    }
  in
  let req = Termanil_model.Capture m in
  printf "request: %b\n" (Poly.equal (P.parse_request (P.request req)) (Ok req));
  let response = Ok (Termanil_model.Message_loaded (m, "body\nwith\nlines")) in
  printf "response: %b\n"
    (Poly.equal (P.parse_response (P.response response)) (Ok response));
  print_s
    [%sexp
      (P.parse_response "(termanil/v99 (error no))"
        : ((Termanil_model.response, string) result, string) result)];
  [%expect
    {|
    request: true
    response: true
    (Error "Incompatible termanil worker protocol")
    |}]
