let check name expected subject =
  Alcotest.(check (option (triple string string string)))
    name expected
    (Jmapq_subject.parse subject)

let notifications () =
  List.iter
    (fun (subject, expected) -> check subject (Some expected) subject)
    [
      ("#project > topic [server]", ("project", "topic", "server"));
      ("#project > topic [PR] [server]", ("project", "topic [PR]", "server"));
      ("#project > a > b [server]", ("project", "a > b", "server"));
      ( "# project \t>\t topic \t[Server Name]",
        ("project", "topic", "Server Name") );
      ("#a>b[c]", ("a", "b", "c"));
      ("#café > évolution [développeurs]", ("café", "évolution", "développeurs"));
      ("#project > topic [ Server Name ]", ("project", "topic", "Server Name"));
    ]

let malformed () =
  List.iter
    (fun subject -> check subject None subject)
    [
      "";
      "ordinary subject";
      "project > topic [server]";
      "# > topic [server]";
      "#project > [server]";
      "#project > topic []";
      "#project > topic [ ]";
      "#project > topic [\t]";
      "#project > topic [server]]";
      "#project > topic [server] trailing";
      "#project > topic [server]\n";
      "#project > to\npic [server]";
      "#pro\rject > topic [server]";
    ]

let hostile_subjects () =
  let megabyte = 1024 * 1024 in
  check "long subject without a separator" None
    ("#" ^ String.make megabyte 'a' ^ "[server]");
  check "long whitespace-only topic" None
    ("#project >" ^ String.make megabyte ' ' ^ "[server]");
  check "long run of brackets without a server" None
    ("#project > " ^ String.make megabyte '[' ^ "]");
  let topic = String.make megabyte 't' in
  check "long valid topic is preserved"
    (Some ("project", topic, "server"))
    ("#project > " ^ topic ^ " [server]")

let () =
  Alcotest.run "Zulip subjects"
    [
      ( "subjects",
        [
          Alcotest.test_case "notifications" `Quick notifications;
          Alcotest.test_case "malformed" `Quick malformed;
          Alcotest.test_case "hostile megabyte subjects" `Quick hostile_subjects;
        ] );
    ]
