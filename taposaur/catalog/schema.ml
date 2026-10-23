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

let current_version = 3
let migrations = [1, schema; 2, metadata_schema; 3, bibliography_schema]
