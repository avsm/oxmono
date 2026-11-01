# Taposaur UI

`taposaur.ui` contains Htmlit widgets, the shared visual styles and a small
browser script. It depends only on Htmlit. Views supply text, links and cells;
components have no catalogue, review or HTTP dependencies.

Use `table` with text/numeric column definitions and `cell` with explicit raw
sort values when the displayed value has units or other markup. Sorting is
stable, keyboard accessible and limited to the displayed page. Coverage cells
may supply a ratio for the count/percentage selector. Load `javascript` with
`defer`. Dispatch `taposaur:content` after inserting new tables.

`badge`, `metric`, `coverage`, `input`, `select` and `checkbox` share a compact
palette: forest green for actions and present coverage, amber for gaps, slate
for neutral formats. Checkboxes remain native controls with visible labels.
Colours supplement text and accessible labels.

The website owns journal autocomplete, abstract visibility, reader panels and
other domain behaviour. Its browser checks are in `test/ui_browser.cjs` and
`test/snapshot_browser.cjs`. Set `TAPOSAUR_URL` to test a preview instance.
