# TEI browser check

`test_tei_html.exe` tests renderer structure, limits, unsafe input, citation
provenance and OpenAlex cache semantics. It can also capture the scholarly
fixture for Chromium:

```sh
TEI_RENDER_CAPTURE=/tmp/taposaur-structured-tei.html \
  _build/default/u/etap/taposaur/test/test_tei_html.exe
```

`tei_browser.cjs` checks the live reader on port 8766 using
`doi:10.0000/anziamj.v56i0.9365`, then checks the captured fixture. The server
must have the verified OpenAlex cache loaded. Install Playwright outside the
repository and make it available through `NODE_PATH`. `CHROMIUM` optionally
selects an existing Chromium executable. For example:

```sh
NODE_PATH=/tmp/taposaur-browser-check/node_modules \
  node u/etap/taposaur/test/tei_browser.cjs
```

The check covers citation and return navigation, DOI and source links,
standalone anchors, raw reference spacing, native MathML, TeX typesetting,
table headers, and user-activated images. It saves a reader screenshot to
`/tmp/taposaur-references.png`. The synthetic image request is intercepted.
