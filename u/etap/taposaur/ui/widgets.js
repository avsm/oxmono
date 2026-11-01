(() => {
  const initialize = () => document.querySelectorAll('table[data-sortable]').forEach(table => {
    if (table.dataset.sortReady) return;
    table.dataset.sortReady = 'true';
    const wrapper = table.closest('.snapshot-table-wrap');
    const body = table.tBodies[0];
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
        button.querySelector('.sort-arrow').textContent = active ?
          (descending ? '↓' : '↑') : '↕';
      });
      wrapper.querySelector('.sort-status').textContent =
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
  initialize();
  document.addEventListener('taposaur:content', initialize);
})();
