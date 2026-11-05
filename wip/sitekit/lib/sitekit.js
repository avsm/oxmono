// Sitekit: sorts tables marked data-sortable. Rows are reordered in place,
// so only what the page rendered is sorted. Content added later is enhanced
// when the page dispatches a sitekit:content event on the document.
(() => {
  const initialize = () => document.querySelectorAll('table[data-sortable]').forEach(table => {
    if (table.dataset.sortReady) return;
    table.dataset.sortReady = 'true';
    const wrapper = table.closest('[data-sort-scope]') || table.closest('.sk-table') || table.parentElement;
    const body = table.tBodies[0];
    if (!body) return;
    const original = [...body.rows];
    const buttons = [...table.querySelectorAll('thead button[data-sort-column]')];
    const collator = new Intl.Collator(undefined, {numeric: true, sensitivity: 'base'});
    let selected = null, descending = false;
    const sort = () => {
      if (!selected) return;
      const column = Number(selected.dataset.sortColumn);
      const numeric = selected.dataset.sortType === 'number';
      const ratio = wrapper.querySelector('[data-sort-basis]')?.value === 'ratio';
      const value = row => {
        const cell = row.cells[column];
        if (!cell) return numeric ? 0 : '';
        const raw = ratio && cell.dataset.sortRatio !== undefined ?
          cell.dataset.sortRatio : (cell.dataset.sortValue ?? cell.textContent.trim());
        return numeric ? Number(raw.replaceAll(',', '')) : raw;
      };
      const rows = original.map((row, index) => ({row, index}));
      rows.sort((a, b) => {
        const cmp = numeric ? value(a.row) - value(b.row) :
          collator.compare(value(a.row), value(b.row));
        return (descending ? -cmp : cmp) || a.index - b.index;
      });
      const fragment = document.createDocumentFragment();
      rows.forEach(({row}) => fragment.append(row));
      body.append(fragment);
      buttons.forEach(button => {
        const active = button === selected;
        button.closest('th').setAttribute('aria-sort', active ?
          (descending ? 'descending' : 'ascending') : 'none');
        const arrow = button.querySelector('.sk-arrow, .sort-arrow');
        if (arrow) arrow.textContent = active ? (descending ? '↓' : '↑') : '↕';
      });
      const status = wrapper.querySelector('.sk-sort-status, .sort-status');
      if (status) status.textContent =
        `Sorted by ${selected.textContent.slice(0, -1).trim()}, ` +
        `${descending ? 'descending' : 'ascending'}` +
        (numeric && ratio ? ', by percentage where available.' : '.');
    };
    buttons.forEach(button => button.addEventListener('click', () => {
      descending = selected === button ? !descending : button.dataset.sortType === 'number';
      selected = button;
      sort();
    }));
    wrapper.querySelector('[data-sort-basis]')?.addEventListener('change', sort);
  });
  if (document.readyState === 'loading')
    document.addEventListener('DOMContentLoaded', initialize);
  else initialize();
  document.addEventListener('sitekit:content', initialize);
})();

// A page that sends its reader on, such as to an OAuth authorization server
// after a form, marks the link with data-sk-follow. Following a link is not
// subject to the form-action policy that a redirect after a form would be.
(() => {
  const follow = () => {
    const link = document.querySelector('a[data-sk-follow]');
    if (link && /^https?:\/\//.test(link.href)) window.location.assign(link.href);
  };
  if (document.readyState === 'loading')
    document.addEventListener('DOMContentLoaded', follow);
  else follow();
})();
// An open account menu closes on a click outside it, or on Escape, which
// returns focus to its trigger.
(() => {
  document.addEventListener('click', event => {
    document.querySelectorAll('details.sk-menu[open]').forEach(menu => {
      if (!menu.contains(event.target)) menu.open = false;
    });
  });
  document.addEventListener('keydown', event => {
    if (event.key !== 'Escape') return;
    document.querySelectorAll('details.sk-menu[open]').forEach(menu => {
      menu.open = false;
      menu.querySelector('summary')?.focus();
    });
  });
})();
