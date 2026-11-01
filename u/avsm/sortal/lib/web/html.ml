let add_escaped b s =
  String.iter
    (fun c ->
      match c with
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '"' -> Buffer.add_string b "&quot;"
      | '\'' -> Buffer.add_string b "&#39;"
      | c -> Buffer.add_char b c)
    s

let escape s =
  let b = Buffer.create (String.length s + 16) in
  add_escaped b s;
  Buffer.contents b

let pct_segment s = Uriz.pct_encode ~component:`Unreserved s

let page ~title ~query body =
  let b = Buffer.create (String.length body + 1024) in
  Buffer.add_string b
    "<!DOCTYPE html>\n\
     <html lang=\"en\">\n\
     <head>\n\
     <meta charset=\"utf-8\">\n\
     <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n\
     <title>";
  add_escaped b title;
  Buffer.add_string b
    "</title>\n\
     <link rel=\"stylesheet\" href=\"/static/style.css\">\n\
     </head>\n\
     <body>\n\
     <header class=\"bar\">\n\
     <a class=\"brand\" href=\"/\">sortal</a>\n\
     <form class=\"search\" method=\"get\" action=\"/\">\n\
     <input type=\"search\" name=\"q\" placeholder=\"Search contacts\" \
     value=\"";
  add_escaped b query;
  Buffer.add_string b
    "\">\n\
     <button type=\"submit\">Search</button>\n\
     </form>\n\
     <a class=\"btn\" href=\"/new\">New contact</a>\n\
     </header>\n\
     <main>\n";
  Buffer.add_string b body;
  Buffer.add_string b "</main>\n</body>\n</html>\n";
  Buffer.contents b

let css =
  {css|:root {
  --bg: #f7f7f5;
  --fg: #1b1b1b;
  --muted: #6a6a68;
  --line: #deddda;
  --card: #ffffff;
  --accent: #2a5d8f;
  --danger: #9a2b2b;
}

*, *::before, *::after { box-sizing: border-box; }

body {
  margin: 0;
  background: var(--bg);
  color: var(--fg);
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto,
    "Helvetica Neue", Arial, sans-serif;
  font-size: 16px;
  line-height: 1.5;
}

a { color: var(--accent); }
a:hover { text-decoration: none; }

.bar {
  display: flex;
  flex-wrap: wrap;
  gap: 0.75rem;
  align-items: center;
  padding: 0.75rem 1.25rem;
  background: var(--card);
  border-bottom: 1px solid var(--line);
}

.bar .brand {
  font-weight: 600;
  font-size: 1.15rem;
  letter-spacing: 0.02em;
  color: var(--fg);
  text-decoration: none;
}

.bar .search {
  display: flex;
  gap: 0.4rem;
  flex: 1 1 16rem;
  margin: 0;
}

.bar .search input { flex: 1 1 auto; }

main {
  max-width: 60rem;
  margin: 0 auto;
  padding: 1.5rem 1.25rem 4rem;
}

h1 { font-size: 1.6rem; margin: 0 0 0.25rem; }
h2 { font-size: 1.1rem; margin: 0 0 0.6rem; }

.count { color: var(--muted); margin: 0 0 1.25rem; }
.muted { color: var(--muted); }
.empty { color: var(--muted); font-style: italic; }

.card {
  background: var(--card);
  border: 1px solid var(--line);
  border-radius: 6px;
  padding: 1rem 1.25rem;
  margin: 0 0 1.25rem;
}

table {
  width: 100%;
  border-collapse: collapse;
  background: var(--card);
  border: 1px solid var(--line);
  border-radius: 6px;
  overflow: hidden;
}

th, td {
  text-align: left;
  padding: 0.6rem 0.75rem;
  border-bottom: 1px solid var(--line);
  vertical-align: middle;
}

th {
  font-size: 0.8rem;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  color: var(--muted);
}

tr:last-child td { border-bottom: none; }
tr:hover td { background: #fafaf8; }

td.thumb { width: 3rem; }

.avatar {
  width: 2.25rem;
  height: 2.25rem;
  border-radius: 50%;
  object-fit: cover;
  display: block;
}

.avatar-lg {
  width: 6rem;
  height: 6rem;
  border-radius: 6px;
  object-fit: cover;
}

dl.facts { margin: 0; display: grid; grid-template-columns: 8rem 1fr; gap: 0.35rem 1rem; }
dl.facts dt { color: var(--muted); }
dl.facts dd { margin: 0; }

ul.rows { list-style: none; margin: 0; padding: 0; }

ul.rows li {
  padding: 0.5rem 0;
  border-bottom: 1px solid var(--line);
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;
  align-items: baseline;
}

ul.rows li:last-child { border-bottom: none; }

.note, .range { color: var(--muted); font-size: 0.9rem; }

input, select, textarea, button {
  font: inherit;
  color: inherit;
}

input, select, textarea {
  padding: 0.4rem 0.55rem;
  border: 1px solid var(--line);
  border-radius: 4px;
  background: #fff;
}

input:focus, select:focus, textarea:focus {
  outline: 2px solid var(--accent);
  outline-offset: -1px;
}

textarea { width: 100%; min-height: 6rem; resize: vertical; }

label { display: block; margin: 0 0 0.9rem; }
label .name { display: block; font-size: 0.85rem; color: var(--muted); margin-bottom: 0.2rem; }
label input, label select { width: 100%; }

.btn, button {
  display: inline-block;
  padding: 0.4rem 0.85rem;
  border: 1px solid var(--accent);
  border-radius: 4px;
  background: var(--accent);
  color: #fff;
  text-decoration: none;
  cursor: pointer;
}

.btn:hover, button:hover { filter: brightness(1.08); }

.btn.plain, button.plain {
  background: #fff;
  color: var(--fg);
  border-color: var(--line);
}

.btn.danger, button.danger {
  background: #fff;
  color: var(--danger);
  border-color: #e6c9c9;
}

.btn.danger:hover, button.danger:hover {
  background: var(--danger);
  color: #fff;
  filter: none;
}

.actions { display: flex; gap: 0.5rem; align-items: center; margin-top: 1rem; }
.actions form { margin: 0; }

.inline { display: flex; flex-wrap: wrap; gap: 0.5rem; align-items: center; margin: 0.75rem 0 0; }
.inline input { width: auto; flex: 1 1 10rem; }
.inline button { flex: 0 0 auto; }
.inline label { margin: 0; }

.error {
  background: #fdf0f0;
  border: 1px solid #e6c9c9;
  color: var(--danger);
  border-radius: 4px;
  padding: 0.6rem 0.85rem;
  margin: 0 0 1.25rem;
}
|css}
