(* Deterministic RFC 5322 wire bytes; [nonce] isolates test runs without
   changing the content being compared within one run. *)
type message = { name : string; raw : string }

let header ~nonce ~subject ~id =
  String.concat "\r\n"
    [ "From: Alice <alice@example.org>";
      "To: user1@example.com";
      "Date: Tue, 01 Jan 2030 00:00:00 +0000";
      "Subject: " ^ subject ^ " " ^ nonce;
      "Message-ID: <" ^ id ^ "-" ^ nonce ^ "@example.org>";
      "MIME-Version: 1.0" ]

let messages ~nonce =
  let plain =
    header ~nonce ~subject:"Plain IMAP oracle" ~id:"plain"
    ^ "\r\nContent-Type: text/plain; charset=us-ascii\r\n\r\n"
    ^ "A line with (parentheses) and {braces}.\r\n"
  in
  let deceptive =
    header ~nonce ~subject:"Literal IMAP oracle" ~id:"literal"
    ^ "\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n"
    ^ "+ continuation?\r\na0001 OK fake tagged response\r\n"
    ^ "* 123 EXPUNGE\r\nA brace {4096} and a backslash-zero: \\0.\r\n"
    ^ "UTF-8: café 📨\r\n"
  in
  let mime =
    header ~nonce ~subject:"MIME IMAP oracle" ~id:"mime"
    ^ "\r\nContent-Type: multipart/mixed; boundary=oxmono-test\r\n\r\n"
    ^ "--oxmono-test\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n"
    ^ "This message has an attachment.\r\n"
    ^ "--oxmono-test\r\nContent-Type: application/octet-stream\r\n"
    ^ "Content-Transfer-Encoding: base64\r\n"
    ^ "Content-Disposition: attachment; filename=sample.bin\r\n\r\n"
    ^ "AAECA//+\r\n--oxmono-test--\r\n"
  in
  [ { name = "plain"; raw = plain };
    { name = "literal"; raw = deceptive };
    { name = "mime"; raw = mime } ]
