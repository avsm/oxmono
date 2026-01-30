(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Yamlrw
module TL = Test_suite_lib.Test_suite_loader
module TF = Test_suite_lib.Tree_format
module JF = Test_suite_lib.Json_format
module JC = Test_suite_lib.Json_compare

let test_suite_path = "yaml-test-suite"

(* HTML escape function *)
let html_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '&' -> Buffer.add_string buf "&amp;"
      | '"' -> Buffer.add_string buf "&quot;"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let normalize_tree s =
  let lines = String.split_on_char '\n' s in
  let lines = List.filter (fun l -> String.trim l <> "") lines in
  String.concat "\n" lines

type test_result = {
  id : string;
  name : string;
  yaml : string;
  is_error_test : bool;
  status : [ `Pass | `Fail of string | `Skip ];
  output : string;
  json_status : [ `Pass | `Fail of string | `Skip ];
  json_expected : string;
  json_actual : string;
}

let compare_json expected actual =
  (* Parse both JSON strings and compare the resulting structures.
     This handles formatting differences and object key ordering. *)
  JC.compare_json_strings expected actual

let run_json_test (test : TL.test_case) :
    [ `Pass | `Fail of string | `Skip ] * string =
  match test.json with
  | None -> (`Skip, "")
  | Some expected_json -> (
      if test.fail then
        (* Error tests shouldn't have JSON comparison *)
        (`Skip, "")
      else
        try
          (* Handle multi-document YAML by using documents_of_string *)
          let docs = Loader.documents_of_string test.yaml in
          let values =
            List.filter_map
              (fun doc ->
                match Document.root doc with
                | None -> None
                | Some yaml ->
                    Some (Yaml.to_value ~resolve_aliases_first:true yaml))
              docs
          in
          let actual_json =
            match values with
            | [] -> "" (* Empty document produces empty JSON *)
            | [ v ] -> JF.to_json v
            | vs -> JF.documents_to_json vs
          in
          if compare_json expected_json actual_json then (`Pass, actual_json)
          else (`Fail "JSON mismatch", actual_json)
        with
        | Yamlrw_error e ->
            (`Fail (Format.asprintf "Parse error: %a" Error.pp e), "")
        | exn ->
            (`Fail (Printf.sprintf "Exception: %s" (Printexc.to_string exn)), "")
      )

let run_test (test : TL.test_case) : test_result =
  let json_status, json_actual = run_json_test test in
  let base =
    {
      id = test.id;
      name = test.name;
      yaml = test.yaml;
      is_error_test = test.fail;
      status = `Skip;
      output = "";
      json_status;
      json_expected = Option.value ~default:"" test.json;
      json_actual;
    }
  in
  if test.fail then begin
    try
      let parser = Parser.of_string test.yaml in
      let events = Parser.to_list parser in
      let tree = TF.of_spanned_events events in
      { base with status = `Fail "Expected parsing to fail"; output = tree }
    with
    | Yamlrw_error e ->
        { base with status = `Pass; output = Format.asprintf "%a" Error.pp e }
    | exn -> { base with status = `Pass; output = Printexc.to_string exn }
  end
  else begin
    match test.tree with
    | None -> (
        (* No expected tree - check if json indicates expected success *)
        match test.json with
        | Some _ -> (
            (* Has json output, so should parse successfully *)
            try
              let parser = Parser.of_string test.yaml in
              let events = Parser.to_list parser in
              let tree = TF.of_spanned_events events in
              { base with status = `Pass; output = tree }
            with exn ->
              {
                base with
                status =
                  `Fail
                    (Printf.sprintf "Should parse but got: %s"
                       (Printexc.to_string exn));
                output = Printexc.to_string exn;
              })
        | None ->
            (* No tree, no json, no fail - ambiguous edge case, skip *)
            { base with status = `Skip; output = "(no expected tree or json)" })
    | Some expected -> (
        try
          let parser = Parser.of_string test.yaml in
          let events = Parser.to_list parser in
          let actual = TF.of_spanned_events events in
          let expected_norm = normalize_tree expected in
          let actual_norm = normalize_tree actual in
          if expected_norm = actual_norm then
            { base with status = `Pass; output = actual }
          else
            {
              base with
              status = `Fail (Printf.sprintf "Tree mismatch");
              output =
                Printf.sprintf "Expected:\n%s\n\nActual:\n%s" expected_norm
                  actual_norm;
            }
        with exn ->
          {
            base with
            status =
              `Fail (Printf.sprintf "Exception: %s" (Printexc.to_string exn));
            output = Printexc.to_string exn;
          })
  end

let status_class = function
  | `Pass -> "pass"
  | `Fail _ -> "fail"
  | `Skip -> "skip"

let status_text = function
  | `Pass -> "PASS"
  | `Fail _ -> "FAIL"
  | `Skip -> "SKIP"

let generate_html results output_file =
  let oc = open_out output_file in
  let pass_count =
    List.length (List.filter (fun r -> r.status = `Pass) results)
  in
  let fail_count =
    List.length
      (List.filter
         (fun r -> match r.status with `Fail _ -> true | _ -> false)
         results)
  in
  let skip_count =
    List.length (List.filter (fun r -> r.status = `Skip) results)
  in
  let total = List.length results in
  let json_pass_count =
    List.length (List.filter (fun r -> r.json_status = `Pass) results)
  in
  let json_fail_count =
    List.length
      (List.filter
         (fun r -> match r.json_status with `Fail _ -> true | _ -> false)
         results)
  in
  let json_skip_count =
    List.length (List.filter (fun r -> r.json_status = `Skip) results)
  in

  Printf.fprintf oc
    {|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Yamlrw Test Results</title>
  <style>
    :root {
      --pass-color: #22c55e;
      --fail-color: #ef4444;
      --skip-color: #f59e0b;
      --bg-color: #1a1a2e;
      --card-bg: #16213e;
      --text-color: #e2e8f0;
      --border-color: #334155;
    }
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
      background: var(--bg-color);
      color: var(--text-color);
      line-height: 1.6;
      padding: 2rem;
    }
    .container { max-width: 1400px; margin: 0 auto; }
    h1 { margin-bottom: 1.5rem; font-size: 2rem; }
    .summary {
      display: flex;
      gap: 1rem;
      margin-bottom: 2rem;
      flex-wrap: wrap;
    }
    .stat {
      background: var(--card-bg);
      padding: 1rem 1.5rem;
      border-radius: 8px;
      border-left: 4px solid var(--border-color);
    }
    .stat.pass { border-left-color: var(--pass-color); }
    .stat.fail { border-left-color: var(--fail-color); }
    .stat.skip { border-left-color: var(--skip-color); }
    .stat-value { font-size: 2rem; font-weight: bold; }
    .stat-label { font-size: 0.875rem; opacity: 0.8; }
    .filters {
      margin-bottom: 1.5rem;
      display: flex;
      gap: 0.5rem;
      flex-wrap: wrap;
    }
    .filter-btn {
      padding: 0.5rem 1rem;
      border: 1px solid var(--border-color);
      background: var(--card-bg);
      color: var(--text-color);
      border-radius: 4px;
      cursor: pointer;
      transition: all 0.2s;
    }
    .filter-btn:hover { border-color: var(--text-color); }
    .filter-btn.active { background: var(--text-color); color: var(--bg-color); }
    .search {
      padding: 0.5rem 1rem;
      border: 1px solid var(--border-color);
      background: var(--card-bg);
      color: var(--text-color);
      border-radius: 4px;
      width: 200px;
    }
    .tests { display: flex; flex-direction: column; gap: 1rem; }
    .test {
      background: var(--card-bg);
      border-radius: 8px;
      border: 1px solid var(--border-color);
      overflow: hidden;
    }
    .test-header {
      padding: 1rem;
      display: flex;
      align-items: center;
      gap: 1rem;
      cursor: pointer;
      border-bottom: 1px solid var(--border-color);
    }
    .test-header:hover { background: rgba(255,255,255,0.05); }
    .badge {
      padding: 0.25rem 0.5rem;
      border-radius: 4px;
      font-size: 0.75rem;
      font-weight: bold;
      text-transform: uppercase;
    }
    .badge.pass { background: var(--pass-color); color: #000; }
    .badge.fail { background: var(--fail-color); color: #fff; }
    .badge.skip { background: var(--skip-color); color: #000; }
    .badge.error-test { background: #8b5cf6; color: #fff; margin-left: auto; }
    .test-id { font-family: monospace; font-weight: bold; }
    .test-name { opacity: 0.8; flex: 1; }
    .test-content { display: none; padding: 1rem; }
    .test.expanded .test-content { display: block; }
    .section { margin-bottom: 1rem; }
    .section-title {
      font-size: 0.875rem;
      text-transform: uppercase;
      opacity: 0.6;
      margin-bottom: 0.5rem;
      letter-spacing: 0.05em;
    }
    pre {
      background: #0f172a;
      padding: 1rem;
      border-radius: 4px;
      overflow-x: auto;
      font-size: 0.875rem;
      white-space: pre-wrap;
      word-break: break-all;
    }
    .expand-icon { transition: transform 0.2s; }
    .test.expanded .expand-icon { transform: rotate(90deg); }
  </style>
</head>
<body>
  <div class="container">
    <h1>Yamlrw Test Results</h1>
    <div class="summary">
      <div class="stat pass">
        <div class="stat-value">%d</div>
        <div class="stat-label">Passed</div>
      </div>
      <div class="stat fail">
        <div class="stat-value">%d</div>
        <div class="stat-label">Failed</div>
      </div>
      <div class="stat skip">
        <div class="stat-value">%d</div>
        <div class="stat-label">Skipped</div>
      </div>
      <div class="stat">
        <div class="stat-value">%d</div>
        <div class="stat-label">Total</div>
      </div>
    </div>
    <h2 style="margin: 1.5rem 0 1rem;">JSON Output Comparison</h2>
    <div class="summary">
      <div class="stat pass">
        <div class="stat-value">%d</div>
        <div class="stat-label">JSON Pass</div>
      </div>
      <div class="stat fail">
        <div class="stat-value">%d</div>
        <div class="stat-label">JSON Fail</div>
      </div>
      <div class="stat skip">
        <div class="stat-value">%d</div>
        <div class="stat-label">JSON Skip</div>
      </div>
    </div>
    <div class="filters">
      <button class="filter-btn active" data-filter="all">All</button>
      <button class="filter-btn" data-filter="pass">Pass</button>
      <button class="filter-btn" data-filter="fail">Fail</button>
      <button class="filter-btn" data-filter="skip">Skip</button>
      <input type="text" class="search" placeholder="Search by ID or name...">
    </div>
    <div class="tests">
|}
    pass_count fail_count skip_count total json_pass_count json_fail_count
    json_skip_count;

  List.iter
    (fun result ->
      let error_badge =
        if result.is_error_test then
          {|<span class="badge error-test">Error Test</span>|}
        else ""
      in
      let json_badge =
        Printf.sprintf
          {|<span class="badge %s" style="margin-left: 4px;">JSON: %s</span>|}
          (status_class result.json_status)
          (status_text result.json_status)
      in
      let json_section =
        if result.json_expected <> "" || result.json_actual <> "" then
          Printf.sprintf
            {|
          <div class="section">
            <div class="section-title">Expected JSON</div>
            <pre>%s</pre>
          </div>
          <div class="section">
            <div class="section-title">Actual JSON</div>
            <pre>%s</pre>
          </div>|}
            (html_escape result.json_expected)
            (html_escape result.json_actual)
        else ""
      in
      Printf.fprintf oc
        {|      <div class="test" data-status="%s" data-json-status="%s" data-id="%s" data-name="%s">
        <div class="test-header" onclick="this.parentElement.classList.toggle('expanded')">
          <span class="expand-icon">▶</span>
          <span class="badge %s">%s</span>
          %s
          <span class="test-id">%s</span>
          <span class="test-name">%s</span>
          %s
        </div>
        <div class="test-content">
          <div class="section">
            <div class="section-title">YAML Input</div>
            <pre>%s</pre>
          </div>
          <div class="section">
            <div class="section-title">Event Tree Output</div>
            <pre>%s</pre>
          </div>%s
        </div>
      </div>
|}
        (status_class result.status)
        (status_class result.json_status)
        (html_escape result.id)
        (html_escape (String.lowercase_ascii result.name))
        (status_class result.status)
        (status_text result.status)
        json_badge (html_escape result.id) (html_escape result.name) error_badge
        (html_escape result.yaml)
        (html_escape result.output)
        json_section)
    results;

  Printf.fprintf oc
    {|    </div>
  </div>
  <script>
    document.querySelectorAll('.filter-btn').forEach(btn => {
      btn.addEventListener('click', () => {
        document.querySelectorAll('.filter-btn').forEach(b => b.classList.remove('active'));
        btn.classList.add('active');
        filterTests();
      });
    });
    document.querySelector('.search').addEventListener('input', filterTests);
    function filterTests() {
      const filter = document.querySelector('.filter-btn.active').dataset.filter;
      const search = document.querySelector('.search').value.toLowerCase();
      document.querySelectorAll('.test').forEach(test => {
        const status = test.dataset.status;
        const id = test.dataset.id.toLowerCase();
        const name = test.dataset.name;
        const matchesFilter = filter === 'all' || status === filter;
        const matchesSearch = !search || id.includes(search) || name.includes(search);
        test.style.display = matchesFilter && matchesSearch ? '' : 'none';
      });
    }
  </script>
</body>
</html>
|};
  close_out oc

let () =
  let html_output = ref None in
  let show_skipped = ref false in
  let test_suite_path_ref = ref test_suite_path in
  let args =
    [
      ( "--html",
        Arg.String (fun s -> html_output := Some s),
        "<file> Generate HTML report to file" );
      ("--show-skipped", Arg.Set show_skipped, " Show details of skipped tests");
      ( "--test-suite-path",
        Arg.Set_string test_suite_path_ref,
        "<path> Path to yaml-test-suite directory" );
    ]
  in
  Arg.parse args
    (fun _ -> ())
    "Usage: run_all_tests [--html <file>] [--show-skipped] [--test-suite-path \
     <path>]";

  let all_tests = TL.load_directory !test_suite_path_ref in
  Printf.printf "Total tests loaded: %d\n%!" (List.length all_tests);

  let results = List.map run_test all_tests in

  let pass_count =
    List.length (List.filter (fun r -> r.status = `Pass) results)
  in
  let fail_count =
    List.length
      (List.filter
         (fun r -> match r.status with `Fail _ -> true | _ -> false)
         results)
  in
  let skip_count =
    List.length (List.filter (fun r -> r.status = `Skip) results)
  in

  let json_pass_count =
    List.length (List.filter (fun r -> r.json_status = `Pass) results)
  in
  let json_fail_count =
    List.length
      (List.filter
         (fun r -> match r.json_status with `Fail _ -> true | _ -> false)
         results)
  in
  let json_skip_count =
    List.length (List.filter (fun r -> r.json_status = `Skip) results)
  in

  Printf.printf
    "\nEvent Tree Results: %d pass, %d fail, %d skip (total: %d)\n%!" pass_count
    fail_count skip_count
    (pass_count + fail_count + skip_count);

  Printf.printf "JSON Results: %d pass, %d fail, %d skip\n%!" json_pass_count
    json_fail_count json_skip_count;

  if fail_count > 0 then begin
    Printf.printf "\nFailing event tree tests:\n";
    List.iter
      (fun r ->
        match r.status with
        | `Fail msg -> Printf.printf "  %s: %s - %s\n" r.id r.name msg
        | _ -> ())
      results
  end;

  if json_fail_count > 0 then begin
    Printf.printf "\nFailing JSON tests:\n";
    List.iter
      (fun r ->
        match r.json_status with
        | `Fail msg -> Printf.printf "  %s: %s - %s\n" r.id r.name msg
        | _ -> ())
      results
  end;

  if !show_skipped && skip_count > 0 then begin
    Printf.printf "\nSkipped tests (no expected tree):\n";
    List.iter
      (fun r ->
        if r.status = `Skip then begin
          Printf.printf "  %s: %s\n" r.id r.name;
          Printf.printf "    YAML (%d chars): %S\n" (String.length r.yaml)
            (if String.length r.yaml <= 60 then r.yaml
             else String.sub r.yaml 0 60 ^ "...")
        end)
      results
  end;

  (match !html_output with
  | Some file ->
      generate_html results file;
      Printf.printf "\nHTML report generated: %s\n" file
  | None -> ());

  (* Exit with non-zero code if any tests failed *)
  if fail_count > 0 || json_fail_count > 0 then exit 1
