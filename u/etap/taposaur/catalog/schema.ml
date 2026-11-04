let schema = {|
CREATE TABLE papers (
 id INTEGER PRIMARY KEY, uid TEXT NOT NULL UNIQUE, shard TEXT NOT NULL,
 scheme TEXT NOT NULL, title TEXT NOT NULL, authors TEXT NOT NULL,
 journal TEXT NOT NULL, publisher TEXT NOT NULL, year INTEGER,
 abstract TEXT NOT NULL, pdf INTEGER NOT NULL, xml INTEGER NOT NULL,
 tei INTEGER NOT NULL, s2orc INTEGER NOT NULL, json TEXT NOT NULL);
CREATE INDEX papers_shard ON papers(shard);
CREATE INDEX papers_year ON papers(year,uid);
CREATE INDEX papers_scheme ON papers(scheme,uid);
CREATE INDEX papers_publisher ON papers(publisher,uid);
CREATE INDEX papers_journal ON papers(journal,uid);
CREATE VIRTUAL TABLE papers_fts USING fts5(
 title,authors,journal,publisher,abstract,content='papers',content_rowid='id');
CREATE TRIGGER papers_ai AFTER INSERT ON papers BEGIN
 INSERT INTO papers_fts(rowid,title,authors,journal,publisher,abstract)
 VALUES(new.id,new.title,new.authors,new.journal,new.publisher,new.abstract);
END;
CREATE TRIGGER papers_ad AFTER DELETE ON papers BEGIN
 INSERT INTO papers_fts(
  papers_fts,rowid,title,authors,journal,publisher,abstract)
 VALUES('delete',old.id,old.title,old.authors,
  old.journal,old.publisher,old.abstract);
END;
CREATE TABLE shards (
 path TEXT PRIMARY KEY, size INTEGER NOT NULL, mtime REAL NOT NULL,
 rows INTEGER NOT NULL, pdf INTEGER NOT NULL, xml INTEGER NOT NULL,
 tei INTEGER NOT NULL, s2orc INTEGER NOT NULL, synced REAL NOT NULL);
CREATE TABLE import_errors (
 path TEXT PRIMARY KEY, message TEXT NOT NULL, at REAL NOT NULL);
CREATE TABLE settings (key TEXT PRIMARY KEY, value INTEGER NOT NULL);
PRAGMA user_version=1;
PRAGMA application_id=1413566547;
|}

let metadata_schema = {|
ALTER TABLE papers ADD COLUMN resolved_json TEXT;
ALTER TABLE papers ADD COLUMN metadata_version INTEGER NOT NULL DEFAULT 0;
ALTER TABLE papers ADD COLUMN metadata_error TEXT;
ALTER TABLE papers ADD COLUMN metadata_at REAL;
CREATE INDEX papers_metadata ON papers(metadata_version,uid);
CREATE TRIGGER papers_au AFTER UPDATE OF
 title,authors,journal,publisher,abstract ON papers BEGIN
 INSERT INTO papers_fts(
  papers_fts,rowid,title,authors,journal,publisher,abstract)
 VALUES('delete',old.id,old.title,old.authors,
  old.journal,old.publisher,old.abstract);
 INSERT INTO papers_fts(rowid,title,authors,journal,publisher,abstract)
 VALUES(new.id,new.title,new.authors,new.journal,new.publisher,new.abstract);
END;
PRAGMA user_version=2;
|}

let bibliography_schema = {|
ALTER TABLE papers ADD COLUMN bibliography_json TEXT;
ALTER TABLE papers ADD COLUMN generation INTEGER NOT NULL DEFAULT 0;
INSERT INTO settings(key,value) VALUES('generation',0);
PRAGMA user_version=3;
|}

let freshness_schema = {|
ALTER TABLE papers ADD COLUMN source_stamp TEXT;
ALTER TABLE papers ADD COLUMN metadata_diagnostics TEXT NOT NULL DEFAULT '[]';
ALTER TABLE papers ADD COLUMN metadata_stale INTEGER NOT NULL DEFAULT 0;
ALTER TABLE papers ADD COLUMN metadata_revision INTEGER NOT NULL DEFAULT 0;
ALTER TABLE shards ADD COLUMN loader_version INTEGER NOT NULL DEFAULT 0;
CREATE TABLE metadata_counts (
 singleton INTEGER PRIMARY KEY CHECK(singleton=1),
 titles INTEGER NOT NULL, abstracts INTEGER NOT NULL,
 publishers INTEGER NOT NULL, enriched INTEGER NOT NULL,
 errors INTEGER NOT NULL, partial INTEGER NOT NULL, stale INTEGER NOT NULL);
INSERT INTO metadata_counts
 SELECT 1,count(nullif(title,'')),count(nullif(abstract,'')),
 count(nullif(publisher,'')),
 coalesce(sum(metadata_version>0 AND metadata_error IS NULL),0),
 count(metadata_error),0,0 FROM papers;
CREATE TRIGGER metadata_counts_ai AFTER INSERT ON papers BEGIN
 UPDATE metadata_counts SET
 titles=titles+(new.title<>''), abstracts=abstracts+(new.abstract<>''),
 publishers=publishers+(new.publisher<>''),
 enriched=enriched+(new.metadata_version>0 AND new.metadata_error IS NULL
   AND new.metadata_diagnostics='[]' AND new.metadata_stale=0),
 errors=errors+(new.metadata_error IS NOT NULL),
 partial=partial+(new.metadata_diagnostics<>'[]'),
 stale=stale+new.metadata_stale;
END;
CREATE TRIGGER metadata_counts_ad AFTER DELETE ON papers BEGIN
 UPDATE metadata_counts SET
 titles=titles-(old.title<>''), abstracts=abstracts-(old.abstract<>''),
 publishers=publishers-(old.publisher<>''),
 enriched=enriched-(old.metadata_version>0 AND old.metadata_error IS NULL
   AND old.metadata_diagnostics='[]' AND old.metadata_stale=0),
 errors=errors-(old.metadata_error IS NOT NULL),
 partial=partial-(old.metadata_diagnostics<>'[]'),
 stale=stale-old.metadata_stale;
END;
CREATE TRIGGER metadata_counts_au AFTER UPDATE ON papers BEGIN
 UPDATE metadata_counts SET
 titles=titles+(new.title<>'')-(old.title<>''),
 abstracts=abstracts+(new.abstract<>'')-(old.abstract<>''),
 publishers=publishers+(new.publisher<>'')-(old.publisher<>''),
 enriched=enriched+(new.metadata_version>0 AND new.metadata_error IS NULL
   AND new.metadata_diagnostics='[]' AND new.metadata_stale=0)
   -(old.metadata_version>0 AND old.metadata_error IS NULL
   AND old.metadata_diagnostics='[]' AND old.metadata_stale=0),
 errors=errors+(new.metadata_error IS NOT NULL)-(old.metadata_error IS NOT NULL),
 partial=partial+(new.metadata_diagnostics<>'[]')-(old.metadata_diagnostics<>'[]'),
 stale=stale+new.metadata_stale-old.metadata_stale;
END;
PRAGMA user_version=4;
|}

let journal_schema = {|
CREATE TABLE journals (name TEXT PRIMARY KEY, papers INTEGER NOT NULL);
INSERT INTO journals SELECT journal,count(*) FROM papers
 WHERE trim(journal)<>'' GROUP BY journal;
CREATE TRIGGER journals_ai AFTER INSERT ON papers
 WHEN trim(new.journal)<>'' BEGIN
 INSERT INTO journals(name,papers) VALUES(new.journal,1)
 ON CONFLICT(name) DO UPDATE SET papers=papers+1;
END;
CREATE TRIGGER journals_ad AFTER DELETE ON papers
 WHEN trim(old.journal)<>'' BEGIN
 UPDATE journals SET papers=papers-1 WHERE name=old.journal;
 DELETE FROM journals WHERE name=old.journal AND papers=0;
END;
CREATE TRIGGER journals_au AFTER UPDATE OF journal ON papers
 WHEN old.journal<>new.journal BEGIN
 UPDATE journals SET papers=papers-1 WHERE name=old.journal;
 DELETE FROM journals WHERE name=old.journal AND papers=0;
 INSERT INTO journals(name,papers)
 SELECT new.journal,1 WHERE trim(new.journal)<>''
 ON CONFLICT(name) DO UPDATE SET papers=papers+1;
END;
PRAGMA user_version=5;
|}

let detail_schema = {|
ALTER TABLE papers ADD COLUMN detail_json TEXT;
ALTER TABLE papers ADD COLUMN detail_at REAL;
CREATE TABLE detail_counts (
 singleton INTEGER PRIMARY KEY CHECK(singleton=1), papers INTEGER NOT NULL);
INSERT INTO detail_counts VALUES(1,0);
CREATE TRIGGER detail_counts_ai AFTER INSERT ON papers
 WHEN new.detail_json IS NOT NULL BEGIN
 UPDATE detail_counts SET papers=papers+1 WHERE singleton=1;
END;
CREATE TRIGGER detail_counts_ad AFTER DELETE ON papers
 WHEN old.detail_json IS NOT NULL BEGIN
 UPDATE detail_counts SET papers=papers-1 WHERE singleton=1;
END;
CREATE TRIGGER detail_counts_au AFTER UPDATE OF detail_json ON papers
 WHEN (old.detail_json IS NULL)<>(new.detail_json IS NULL) BEGIN
 UPDATE detail_counts SET papers=papers+(new.detail_json IS NOT NULL)
   -(old.detail_json IS NOT NULL) WHERE singleton=1;
END;
PRAGMA user_version=6;
|}

(* Body text presence from the resolver: 1, 0 or NULL when unresolved
   or when no document reader could tell. *)
let text_schema = {|
ALTER TABLE papers ADD COLUMN text INTEGER;
PRAGMA user_version=7;
|}

let current_version = 7
let migrations = [1, schema; 2, metadata_schema; 3, bibliography_schema;
  4, freshness_schema; 5, journal_schema; 6, detail_schema; 7, text_schema]
