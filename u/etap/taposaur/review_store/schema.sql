PRAGMA application_id=1414682198;
CREATE TABLE objects (
 sha TEXT PRIMARY KEY CHECK(length(sha)=64), body BLOB NOT NULL);
CREATE TABLE reviews (
 slug TEXT PRIMARY KEY, title TEXT NOT NULL, requester TEXT NOT NULL,
 owner TEXT NOT NULL, created REAL NOT NULL, head INTEGER NOT NULL);
CREATE TABLE revisions (
 slug TEXT NOT NULL REFERENCES reviews(slug), number INTEGER NOT NULL,
 definition_sha TEXT NOT NULL REFERENCES objects(sha),
 actor TEXT NOT NULL, reason TEXT NOT NULL, created REAL NOT NULL,
 PRIMARY KEY(slug,number));
CREATE TABLE runs (
 id INTEGER PRIMARY KEY, slug TEXT NOT NULL, revision INTEGER NOT NULL,
 actor TEXT NOT NULL, created REAL NOT NULL,
 partial INTEGER NOT NULL CHECK(partial IN (0,1)), members INTEGER NOT NULL,
 FOREIGN KEY(slug,revision) REFERENCES revisions(slug,number));
CREATE INDEX runs_review ON runs(slug,id);
CREATE TABLE run_inputs (
 run_id INTEGER NOT NULL REFERENCES runs(id), key TEXT NOT NULL,
 blob TEXT NOT NULL REFERENCES objects(sha),
 result TEXT NOT NULL, PRIMARY KEY(run_id,key));
CREATE TABLE run_members (
 run_id INTEGER NOT NULL REFERENCES runs(id), doi TEXT NOT NULL,
 payload TEXT NOT NULL, PRIMARY KEY(run_id,doi));
CREATE TABLE snapshots (
 id INTEGER PRIMARY KEY, run_id INTEGER NOT NULL REFERENCES runs(id),
 parent INTEGER, label TEXT NOT NULL, actor TEXT NOT NULL,
 created REAL NOT NULL, title TEXT NOT NULL, requester TEXT NOT NULL,
 owner TEXT NOT NULL, members INTEGER NOT NULL, titles INTEGER NOT NULL,
 abstracts INTEGER NOT NULL, held INTEGER NOT NULL,
 manifest TEXT NOT NULL REFERENCES objects(sha),
 UNIQUE(id,run_id), FOREIGN KEY(parent,run_id) REFERENCES snapshots(id,run_id));
CREATE INDEX snapshots_run ON snapshots(run_id,id);
CREATE TABLE snapshot_rows (
 snapshot_id INTEGER NOT NULL REFERENCES snapshots(id), doi TEXT NOT NULL,
 observation TEXT NOT NULL REFERENCES objects(sha),
 PRIMARY KEY(snapshot_id,doi));
CREATE TABLE exports (
 snapshot_id INTEGER NOT NULL REFERENCES snapshots(id), format TEXT NOT NULL,
 blob TEXT NOT NULL REFERENCES objects(sha),
 PRIMARY KEY(snapshot_id,format));
PRAGMA user_version=1;
