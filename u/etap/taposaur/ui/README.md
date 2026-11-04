# Taposaur UI

`taposaur.ui` is Taposaur's styling over [Sitekit](../../../../wip/sitekit),
which supplies the page shell and the generic widgets: badges, metrics,
labelled fields, state icons and coverage bars. What remains here is
Taposaur's stylesheet and its coverage table, whose sorting can switch
between paper counts and percentages.

Use `table` with text/numeric column definitions and `cell` with explicit raw
sort values when the displayed value has units or other markup. Sorting is
Sitekit's script: stable, keyboard accessible and limited to the displayed
page. Coverage cells may supply a ratio for the count/percentage selector.
Dispatch `sitekit:content` after inserting new tables.

The palette is light only: forest green for actions and present coverage,
amber for gaps, slate for neutral formats. Checkboxes remain native controls
with visible labels. Colours supplement text and accessible labels.

The website owns journal autocomplete, abstract visibility, reader panels and
other domain behaviour. Its stylesheet and scripts are served as Sitekit
assets at content-hashed paths, so pages carry no inline style or script and
the Content Security Policy allows neither. The navigation's current section
is still marked in the browser, since pages are built before the request
path reaches them. Its browser checks are in `test/ui_browser.cjs` and
`test/snapshot_browser.cjs`. Set `TAPOSAUR_URL` to test a preview instance.
