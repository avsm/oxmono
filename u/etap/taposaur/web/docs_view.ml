(* Reference pages for the data the website and CLI publish. The corpus
   schema below is the contract of [taposaur review corpus] rows and the
   Parquet files converted from them; a test checks that every column it
   documents is a key of an encoded row and nothing is left undocumented. *)
open View
module Ui = Taposaur_ui
let h2 s = el "h2" [txt s]
let h3 s = el "h3" [txt s]
let code s = el "code" [txt s]
let pre s = el "pre" [code s]
let para nodes = el "p" nodes
let table headers rows = Ui.table (List.map (fun h -> h, false) headers)
  (List.map (List.map (fun nodes -> Ui.node "td" [] nodes)) rows)
let ul items = el "ul" (List.map (fun nodes -> el "li" nodes) items)

type column = { name : string; parquet : string; meaning : string }
let c name parquet meaning = { name; parquet; meaning }
let provenance_type = "provenance STRUCT"
let groups = [
  "Identity", "One row per snapshot member, in DOI order.", [
    c "snapshot" "INTEGER" "Review snapshot the row was exported from.";
    c "doi" "VARCHAR" "Member DOI, lower case, as recorded in the snapshot.";
    c "uid" "VARCHAR" "Catalogue identifier (doi:…); null when the paper is not in the archive.";
    c "in_catalogue" "BOOLEAN" "Whether the catalogue held the paper when the export ran (the live catalogue with --live, else the frozen holding)."];
  "Bibliography", "Frozen snapshot values, superseded by the live catalogue's nonblank values with --live and filled from the evidence store with --evidence. Null when unknown.", [
    c "title" "VARCHAR" "Title.";
    c "abstract" "VARCHAR" "Abstract text.";
    c "authors" "VARCHAR[]" "Author names in order, \"Family, Given\".";
    c "journal" "VARCHAR" "Journal or venue name as captured.";
    c "publisher" "VARCHAR" "Publisher name.";
    c "year" "INTEGER" "Publication year.";
    c "published" "VARCHAR" "Publication date as captured (ISO date or partial).";
    c "volume" "VARCHAR" "Volume."; c "issue" "VARCHAR" "Issue.";
    c "pages" "VARCHAR" "Page range, joined with an en dash.";
    c "issn" "VARCHAR" "ISSN as captured; converter output sometimes leaves surrounding text here.";
    c "pmid" "VARCHAR" "PubMed identifier."; c "pmcid" "VARCHAR" "PubMed Central identifier, with the PMC prefix."];
  "Field provenance", "One provenance record per bibliographic field, describing where the value in that column came from. Null when the field is null.", [
    c "title_provenance" provenance_type "Provenance of title.";
    c "abstract_provenance" provenance_type "Provenance of abstract.";
    c "authors_provenance" provenance_type "Provenance of authors.";
    c "journal_provenance" provenance_type "Provenance of journal.";
    c "publisher_provenance" provenance_type "Provenance of publisher.";
    c "year_provenance" provenance_type "Provenance of year.";
    c "pmid_provenance" provenance_type "Provenance of pmid.";
    c "pmcid_provenance" provenance_type "Provenance of pmcid."];
  "Source labels", "The coarse labels the snapshot froze, kept for compatibility with earlier exports. The provenance records refine them.", [
    c "title_source" "VARCHAR" "Frozen label of the title's source.";
    c "abstract_source" "VARCHAR" "Frozen label of the abstract's source.";
    c "publisher_source" "VARCHAR" "Frozen label of the publisher's source.";
    c "sources" "MAP(VARCHAR, VARCHAR)" "Field name to frozen label for every field that has one, including volume, issue, pages, issn and published."];
  "OpenAlex journal", "The OpenAlex source behind the journal, best effort for every row.", [
    c "journal_openalex_id" "VARCHAR" "OpenAlex source id, such as S194752149.";
    c "journal_openalex_name" "VARCHAR" "OpenAlex display name of that source.";
    c "journal_openalex_match" "VARCHAR" "How it was found: work (the member's own OpenAlex record), issn (the row's ISSN) or name (the folded journal name). Null when nothing matched."];
  "Holdings and full text", "Archive holdings and the body prose extracted when the export ran.", [
    c "pdf" "BOOLEAN" "A PDF is held."; c "xml" "BOOLEAN" "Publisher XML is held.";
    c "tei" "BOOLEAN" "A TEI document is held."; c "s2orc" "BOOLEAN" "An S2ORC document is held.";
    c "tei_file" "VARCHAR" "File in the paper directory the text was read from (Pub2TEI before GROBID, published versions first, the first with prose wins).";
    c "tei_converter" "VARCHAR" "Converter that produced that file: pub2tei, grobid or s2orc.";
    c "tei_sha256" "VARCHAR" "SHA-256 of that file as read.";
    c "tei_bytes" "BIGINT" "Size of that file in bytes.";
    c "text" "VARCHAR" "Body prose: section text in document order, without front matter or references.";
    c "text_error" "VARCHAR" "Why extraction failed when text is null and a TEI was held, such as an empty or truncated file.";
    c "text_provenance" provenance_type "Provenance of the TEI the text was read from.";
    c "text_acquisition" "acquisition STRUCT" "How the PDF, XML or JSON that TEI was converted from entered the archive."]]
let corpus_columns =
  List.concat_map (fun (_, _, cols) -> List.map (fun c -> c.name) cols) groups

let provenance_fields = [
  "kind", "VARCHAR", "Source class, listed below.";
  "provider", "VARCHAR", "Who supplied it: an acquisition strategy such as elsevier or glutton-oa, an index source, a mirror name, or openalex.";
  "artifact", "VARCHAR", "File in the paper directory the value was read from, such as publisher.xml or tei-2.xml.";
  "converter", "VARCHAR", "Converter that produced that file, such as grobid or pub2tei.";
  "version", "VARCHAR", "Converter or artifact version when recorded.";
  "digest", "VARCHAR", "SHA-256 of the artifact, or of the retained OpenAlex response body.";
  "retrieved", "TIMESTAMP (UTC)", "When the artifact was written to the archive, or when the OpenAlex response was fetched."]
let kinds = [
  "publisher_xml", "Publisher XML (JATS or Elsevier) held in the archive.";
  "pub2tei", "TEI converted from publisher XML by Pub2TEI.";
  "grobid", "TEI converted from a PDF by GROBID.";
  "s2orc", "S2ORC JSON, or TEI derived from it.";
  "index", "The imported index row (provider names its source).";
  "manifest", "The paper manifest's own record, with no artifact behind it.";
  "correction", "A recorded correction (provider is who made it, retrieved when).";
  "crossref", "Crossref DOI evidence from the mirror store.";
  "datacite", "DataCite DOI evidence from the mirror store.";
  "openalex", "An OpenAlex work retained in the evidence store; digest and retrieved identify the response.";
  "review_input", "A value from the review's own input file (provider is the input key, artifact its line or record).";
  "catalogue", "The snapshot froze the value from the catalogue and the live resolution no longer selects it, so nothing finer is known."]
let acquisition_fields = [
  "file", "VARCHAR", "File in the paper directory, such as publisher.pdf.";
  "format", "VARCHAR", "pdf, xml or json.";
  "source", "VARCHAR", "Where it came from: publisher, oa, core, s2orc, url or tei.";
  "strategy", "VARCHAR", "Acquisition strategy, such as elsevier, cup, wiley or glutton-oa; null for files inherited from the legacy tree.";
  "origin", "VARCHAR", "Legacy path or upstream location it was copied from.";
  "url", "VARCHAR", "Download URL when fetched over HTTP.";
  "status", "INTEGER", "HTTP status of that download.";
  "version", "VARCHAR", "Publisher version when known.";
  "confidence", "VARCHAR", "observed when the manifest recorded the acquisition, assumed when inferred.";
  "digest", "VARCHAR", "SHA-256 of the acquired file.";
  "retrieved", "TIMESTAMP (UTC)", "When it entered the archive."]

let fields rows = table ["Field"; "Type"; "Meaning"]
  (List.map (fun (f, t, m) -> [[code f]; [txt t]; [txt m]]) rows)

let index = page "Documentation" [
  p "Reference pages for the data this server and the taposaur CLI publish.";
  ul [[link "/docs/corpus" "Corpus export schema";
       txt " · the columns of a review corpus (JSON Lines and Parquet): bibliography, per-field provenance, OpenAlex journal, full text and its acquisition trail."]];
  p "Command-line usage is documented in the taposaur README alongside the source."]

let corpus = page ~kind:"docs" "Corpus export schema" ([
  para [txt "A corpus export is one row per member of a review snapshot, in DOI order, written by ";
    code "taposaur review corpus SNAPSHOT --output FILE.jsonl";
    txt " as JSON Lines and converted to Parquet with declared column types, so a file whose optional values are all null keeps the same schema. Every key is present in every row; absent values are null. Snapshot files are named ";
    code "SLUG-snapshot-N.parquet"; txt " and are never rewritten: an export made later against a newer catalogue is a new file."];
  para [txt "Three options change what a row holds. "; code "--live CATALOG";
    txt " supersedes frozen values with the live catalogue's nonblank fields and holdings and reads each catalogued member's cached resolution for field provenance. ";
    code "--evidence"; txt " fills still-blank fields from the OpenAlex evidence store and names OpenAlex venues from it. ";
    code "--journal-links FILE"; txt " adds journal-name to OpenAlex-source links for venue matching. Without ";
    code "--live"; txt " catalogue values keep the coarse provenance kind catalogue."];
  h2 "Columns"] @
  List.concat_map (fun (title, intro, cols) ->
    [h3 title; p intro;
     table ["Column"; "Type"; "Meaning"]
       (List.map (fun c -> [[code c.name]; [txt c.parquet]; [txt c.meaning]]) cols)])
    groups @ [
  h2 "Provenance record";
  p "Every *_provenance column is this struct. Fields that do not apply are null; for example a GROBID title has an artifact, converter and retrieval time but a digest only when the manifest recorded one, and an OpenAlex title has a digest and retrieval time but no artifact.";
  fields provenance_fields;
  h3 "Kinds";
  table ["Kind"; "Meaning"] (List.map (fun (k, m) -> [[code k]; [txt m]]) kinds);
  h2 "Acquisition record";
  p "text_acquisition describes the file the TEI was converted from: the manifest entry the TEI names as its origin, else the first held file of the format its converter reads (a PDF for GROBID, publisher XML for Pub2TEI, JSON for S2ORC).";
  fields acquisition_fields;
  h2 "Frozen source labels";
  p "The snapshot records a label for every field it froze. The sources map and the three *_source columns carry them unchanged:";
  ul [[code "catalogue"; txt " · the value came from the catalogue at capture."];
      [code "input:KEY:line:N"; txt " or "; code "input:KEY:record:N"; txt " · the value came from that line or record of a review input."];
      [code "PROVIDER · SHA256"; txt " · the value came from a retained evidence response, such as openalex, identified by the digest of its body."]];
  h2 "Reading the file";
  p "Any Parquet reader works. With duckdb:";
  pre "SELECT title_provenance.kind, count(*)\nFROM 'ce-butterflies-snapshot-8.parquet'\nWHERE title IS NOT NULL GROUP BY 1 ORDER BY 2 DESC;\n\nSELECT doi, journal_openalex_id, journal_openalex_name, text_acquisition.strategy\nFROM 'ce-butterflies-snapshot-8.parquet'\nWHERE text IS NOT NULL AND journal_openalex_match = 'issn' LIMIT 10;";
  h2 "Two typical rows";
  ul [[txt "A catalogued paper: title, authors and journal from publisher.xml (kind publisher_xml), text from tei-2.xml (kind grobid, converter grobid, retrieved when the TEI was written), acquired as publisher.pdf from a legacy archive path, journal matched to its OpenAlex source by ISSN."];
      [txt "A member outside the archive: title, authors, journal and year all kind openalex with the response digest and fetch time, no holdings or text, journal matched from the member's own OpenAlex work."]]])
