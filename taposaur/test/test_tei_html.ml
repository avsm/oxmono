module T = Taposaur_document.Tei
let get = function Ok v -> v | Error e -> Alcotest.fail e
let has hay needle =
  let n = String.length needle in
  let rec loop i = i+n <= String.length hay &&
    (String.sub hay i n = needle || loop (i+1)) in loop 0
let check label expected body needle =
  Alcotest.(check bool) label expected (has body needle)
let render ?resolve_doi ?journal_url ?source_url s =
  get (T.render_html ?resolve_doi ?journal_url ?source_url (Eio.Flow.string_source s))
let fixture = {|<TEI xmlns="http://www.tei-c.org/ns/1.0"
 xmlns:m="http://www.w3.org/1998/Math/MathML">
<teiHeader><fileDesc><titleStmt><title>A &amp; B</title></titleStmt></fileDesc>
<profileDesc><abstract><p>Header abstract</p></abstract></profileDesc></teiHeader>
<text><body><div n="1"><head>Methods</head><p>Some <hi rend="bold italic">emphasis</hi>
<hi rend="superscript">2</hi><ref target="#b0">[1]</ref><ref target="#note">*</ref>
<formula notation="LaTeX" rend="inline">x^2</formula></p>
<div><head>Details</head><p><choice><sic>eror</sic><corr>error</corr></choice></p></div>
<list type="ordered"><item>First</item><item>Second</item></list>
<list type="gloss"><label>Term</label><item>Definition</item></list>
<table><head>Data</head><row role="label"><cell cols="2">Header</cell></row>
<row><cell rows="2">A</cell><cell>B</cell></row></table>
<figure xml:id="fig"><head>Figure 1</head><graphic url="https://example.org/a.png"/>
<figDesc>Example diagram</figDesc></figure>
<formula xml:id="eq" n="1"><m:math display="block"><m:mfrac><m:mi>x</m:mi>
<m:msqrt><m:mi>y</m:mi></m:msqrt></m:mfrac></m:math></formula>
<p><ref target="#fig #eq">See figure and equation</ref><ref target="#missing">Missing</ref></p>
</div></body><back><note xml:id="note" n="1"><p>A footnote.</p></note>
<listBibl><biblStruct xml:id="b0"><analytic><author><persName><forename>Ada</forename>
<surname>Bird</surname></persName></author><title level="a">Paper title</title></analytic>
<monogr><title level="j">Example Journal</title><imprint><date when="2020"/>
<biblScope unit="page" from="12" to="18"/></imprint></monogr>
<idno type="DOI">10.1000/ref</idno></biblStruct></listBibl></back></text></TEI>|}
let structures () =
  let html = render ~journal_url:(function "Example Journal" ->
    Some "https://openalex.org/S123" | _ -> None) fixture in
  List.iter (fun (name,needle) -> check name true html needle)
    ["title","<h1>A &amp; B</h1>"; "abstract","Header abstract";
     "contents",">Contents</summary>";"numbering",">1 Methods</h2>";
     "nested heading",">Details</h3>";"bold italic","<strong><em>emphasis</em></strong>";
     "superscript","<sup>2</sup>";"ordered list","<ol id=";
     "glossary","<dl id=";"table header","colspan=\"2\">Header</th>";
     "rowspan","rowspan=\"2\">A</td>";"MathML","<mfrac id=";
     "TeX","data-tex=\"\" data-display=\"false\">x^2";
     "figure link","class=\"tei-image-link\"";
     "DOI","href=\"https://doi.org/10.1000/ref\"";
     "journal","href=\"https://openalex.org/S123\"";
     "date attribute",">2020</span>";"page range",">12–18</span>";
     "backlink","Return to citation 1";"note","role=\"note\"";
     "unresolved link","class=\"tei-unresolved\">Missing</span>"];
  check "uncorrected choice omitted" false html "eror";
  check "no remote image fetched automatically" false html "<img"
let safety () =
  let html = render {|<TEI><text><body><p><ref target="javascript:alert(1)">bad</ref>
<ref target="https://example.org/?q=&quot;bad">bad URL</ref>
<hi rend="bold&quot; onclick=&quot;alert(1)">text</hi><script>evil()</script></p>
<graphic url="data:image/svg+xml,bad"/>
<formula><math xmlns="http://www.w3.org/1998/Math/MathML"><mi onclick="bad">x</mi>
<annotation-xml encoding="text/html"><script xmlns="http://www.w3.org/1999/xhtml">evil</script>
</annotation-xml></math></formula>
<p xml:id="x">One</p><p xml:id="x">Two</p><ref target="#x">Go</ref>
</body></text></TEI>|} in
  List.iter (fun needle -> check ("blocked " ^ needle) false html needle)
    ["javascript:";"onclick=";"<script";"<annotation-xml";"src=\"data:";"evil()"];
  check "first duplicate is target" true html "id=\"tei-78\">One";
  check "duplicate gets unique ID" false html "id=\"tei-78\">Two";
  check "oversized output refused" true
    (match T.render_html ~max_output:10 (Eio.Flow.string_source fixture) with
     | Error _ -> "error" | Ok _ -> "") "error"
let bibliography () =
  let bib = {Taposaur.Bibliography.empty with title=Some "Resolved title";
    journal=Some "Example Journal";doi=Some "10.1000/ref"} in
  let resolve_doi = function "10.1000/ref" -> Some bib | _ -> None in
  let html = render ~resolve_doi
    ~source_url:(function "10.1000/ref" -> Some "https://openalex.org/S1"
      | _ -> None) {|<TEI><text><back><listBibl>
<biblStruct xml:id="b0"><note type="raw_reference">Paper. A Bird, 10.1000/refJournal2020</note></biblStruct>
<biblStruct><note type="raw_reference">Thesis. I Faille, 1992Université -Grenoble 1PhD thesis</note></biblStruct>
</listBibl></back></text></TEI>|} in
  check "verified raw DOI resolved" true html ">Resolved title</a>";
  check "source retained" true html "10.1000/refJournal2020";
  check "raw spacing" true html "1992 Université -Grenoble 1 PhD";
  check "original raw evidence" true html "1992Université -Grenoble 1PhD";
  check "resolved attribution" true html "Cached catalogue metadata";
  check "DOI source link" true html "href=\"https://openalex.org/S1\""
let journals () =
  let module J = Taposaur_web.Journal_links in
  let links = get (J.of_jsonl
    "{\"journal\":\" Example  Journal \",\"source_id\":\"S123\"}\n") in
  Alcotest.(check (option string)) "name normalization"
    (Some "https://openalex.org/S123") (J.find links "example journal");
  let links = get (J.of_jsonl
    "{\"journal\":\"J\",\"source_id\":\"S1\"}\n{\"journal\":\"J\",\"source_id\":\"S2\"}") in
  Alcotest.(check (option string)) "ambiguous names unlinked" None (J.find links "J");
  let dois = get (J.of_jsonl
    "{\"doi\":\"10.1000/REF\",\"source_id\":\"S1\"}") in
  Alcotest.(check bool) "known DOI" true (J.known_doi dois "10.1000/ref");
  Alcotest.(check (option string)) "DOI source"
    (Some "https://openalex.org/S1") (J.source dois "10.1000/ref");
  Alcotest.(check bool) "source injection refused" true
    (Result.is_error (J.of_jsonl "{\"journal\":\"J\",\"source_id\":\"S1/evil\"}"))
let () =
  (match Sys.getenv_opt "TEI_RENDER_CAPTURE" with
   | None -> () | Some path -> let ch = open_out path in
       output_string ch (render fixture); close_out ch);
  Alcotest.run "TEI reader" ["render",[
    Alcotest.test_case "scholarly structures" `Quick structures;
    Alcotest.test_case "untrusted input and limits" `Quick safety;
    Alcotest.test_case "reference evidence" `Quick bibliography;
    Alcotest.test_case "OpenAlex journal links" `Quick journals]]
