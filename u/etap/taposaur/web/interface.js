(async () => {
  const debug = document.querySelector('#server-debug');
  if (debug) {
    let loaded = false, loading = false;
    debug.addEventListener('toggle', async () => {
      if (!debug.open || loaded || loading) return;
      loading = true;
      const target = document.querySelector('#server-debug-content');
      const url = new URL('/debug', location.origin);
      url.searchParams.set('view', 'panel');
      const uid = new URL(location.href).searchParams.get('uid');
      if (uid) url.searchParams.set('uid', uid);
      target.setAttribute('aria-busy', 'true');
      try {
        const response = await fetch(url, {cache:'no-store'});
        if (!response.ok) throw new Error('Could not load server details');
        const doc = new DOMParser().parseFromString(await response.text(), 'text/html');
        const content = doc.querySelector('[data-panel-content]');
        if (!content) throw new Error('Invalid server details response');
        target.replaceChildren(content);
        document.dispatchEvent(new Event('taposaur:content'));
        loaded = true;
      } catch (_) {
        const message = document.createElement('p');
        message.textContent = 'Server details could not be loaded. Close and reopen to retry.';
        target.prepend(message);
      } finally {
        loading = false;
        target.removeAttribute('aria-busy');
      }
    });
  }
  const abstracts = document.querySelector('#show-abstracts');
  if (abstracts) {
    const apply = () => document.querySelector('.results')?.classList.toggle(
      'ui-hide-abstracts', !abstracts.checked);
    try { abstracts.checked = localStorage.getItem('taposaur.abstracts') !== 'hide'; } catch (_) {}
    apply();
    abstracts.addEventListener('change', () => {
      apply();
      try { localStorage.setItem('taposaur.abstracts', abstracts.checked ? 'show' : 'hide'); } catch (_) {}
    });
  }
  const section = location.pathname.startsWith('/review') ? '/reviews'
    : location.pathname.startsWith('/job') ? '/jobs'
    : location.pathname.startsWith('/activity') ? '/activity'
    : location.pathname.startsWith('/audit') ? null : '/search';
  document.querySelectorAll('.site-nav a').forEach(link => {
    if (link.pathname === section) link.setAttribute('aria-current', 'page');
  });
  const renderMath = root => {
    root.querySelectorAll('[data-tex]').forEach(element => {
      if (!window.katex || element.dataset.rendered) return;
      const source = element.textContent;
      if (source.length > 20000) return;
      try {
        window.katex.render(source, element, {
          output: 'mathml', displayMode: element.dataset.display === 'true',
          trust: false, strict: 'error', throwOnError: true,
          maxExpand: 1000, maxSize: 20, macros: {}
        });
        element.dataset.rendered = 'true';
      } catch (_) {
        element.textContent = source;
        element.title = 'Formula could not be typeset. Original source shown.';
      }
    });
  };
  renderMath(document);
  document.addEventListener('click', event => {
    const link = event.target.closest('.tei-image-link');
    if (!link || event.button !== 0 || event.metaKey || event.ctrlKey ||
        event.shiftKey || event.altKey) return;
    event.preventDefault();
    if (link.nextElementSibling?.tagName === 'IMG') return;
    const img = document.createElement('img');
    img.alt = link.closest('figure')?.querySelector('figcaption')?.textContent || 'Figure';
    img.referrerPolicy = 'no-referrer';
    img.src = link.href;
    img.addEventListener('error', () => {
      img.remove();
      link.textContent = 'Image could not load. Open image in a new tab.';
    });
    link.after(img);
  });
  const input = document.querySelector('#journal-input');
  if (input) {
    const menu = document.querySelector('#journal-menu');
    const list = document.querySelector('#journal-options');
    const status = document.querySelector('#journal-status');
    const clear = document.querySelector('#journal-clear');
    let options = [], active = -1, serial = 0, timer, controller;
    const setActive = index => {
      active = index;
      [...list.children].forEach((row, i) => {
        row.setAttribute('aria-selected', String(i === index));
      });
      if (index < 0) input.removeAttribute('aria-activedescendant');
      else {
        input.setAttribute('aria-activedescendant', list.children[index].id);
        list.children[index].scrollIntoView({block: 'nearest'});
      }
    };
    const hide = () => {
      ++serial;
      clearTimeout(timer);
      controller?.abort();
      menu.hidden = true;
      input.setAttribute('aria-expanded', 'false');
      setActive(-1);
    };
    const choose = index => {
      if (!options[index]) return;
      input.value = options[index].name;
      hide();
      input.form.requestSubmit();
    };
    const show = () => {
      menu.hidden = false;
      input.setAttribute('aria-expanded', 'true');
    };
    const search = async token => {
      controller = new AbortController();
      try {
        const query = new URLSearchParams({q: input.value.trim()});
        const response = await fetch(`/journals?${query}`, {
          signal: controller.signal, headers: {Accept: 'application/json'}
        });
        if (!response.ok) throw new Error('Journal lookup failed');
        const rows = await response.json();
        if (token !== serial || document.activeElement !== input) return;
        options = input.value.trim() ? rows : [{name: '', papers: null}, ...rows];
        list.replaceChildren();
        options.forEach((journal, i) => {
          const row = document.createElement('li');
          row.id = `journal-option-${i}`;
          row.role = 'option';
          row.dataset.index = String(i);
          row.setAttribute('aria-selected', 'false');
          const name = document.createElement('span');
          name.textContent = journal.name || 'All journals';
          row.append(name);
          if (journal.papers !== null) {
            const count = document.createElement('small');
            count.textContent = `${journal.papers.toLocaleString()} ` +
              (journal.papers === 1 ? 'paper' : 'papers');
            row.append(count);
          }
          list.append(row);
        });
        status.textContent = rows.length ? 'Paper counts across this catalogue' :
          'No matching journals. Try a shorter name.';
        show();
      } catch (error) {
        if (error.name === 'AbortError' || token !== serial) return;
        status.textContent = 'Suggestions unavailable. Enter a full journal name.';
        show();
      }
    };
    const schedule = (delay = 150) => {
      hide();
      options = [];
      list.replaceChildren();
      clear.hidden = !input.value;
      status.textContent = 'Loading journals…';
      show();
      const token = serial;
      timer = setTimeout(() => search(token), delay);
    };
    clear.hidden = !input.value;
    input.addEventListener('focus', () => schedule(0));
    input.addEventListener('input', () => schedule());
    input.addEventListener('blur', hide);
    input.addEventListener('keydown', event => {
      if (event.isComposing) return;
      if (event.key === 'Escape') {
        event.preventDefault();
        hide();
      } else if (['ArrowDown', 'ArrowUp'].includes(event.key)) {
        event.preventDefault();
        if (menu.hidden) schedule(0);
        else if (options.length) setActive(event.key === 'ArrowDown' ?
          (active + 1) % options.length :
          (active < 0 ? options.length - 1 :
            (active - 1 + options.length) % options.length));
      } else if (event.key === 'Enter' && !menu.hidden && active >= 0) {
        event.preventDefault();
        choose(active);
      }
    });
    list.addEventListener('pointerdown', event => event.preventDefault());
    list.addEventListener('click', event => {
      const row = event.target.closest('[role="option"]');
      if (row) choose(Number(row.dataset.index));
    });
    clear.addEventListener('click', () => {
      input.value = '';
      hide();
      input.form.requestSubmit();
    });
  }

  const loading = document.querySelector('#paper-loading');
  if (loading) {
    const status = loading.querySelector('[role="status"]');
    status.textContent = 'Loading files and full paper details…';
    loading.setAttribute('aria-busy', 'true');
    try {
      const response = await fetch(loading.dataset.detailsUrl);
      if (!response.ok) throw new Error(`HTTP ${response.status}`);
      const html = new DOMParser().parseFromString(await response.text(), 'text/html');
      const main = html.querySelector('main');
      if (!main || !main.querySelector('.paper-detail')) {
        throw new Error('Missing paper details');
      }
      document.querySelector('main').replaceWith(main);
      document.dispatchEvent(new Event('taposaur:content'));
      document.title = html.title;
    } catch (_) {
      loading.setAttribute('aria-busy', 'false');
      status.textContent = 'Paper details could not be loaded. The saved catalogue ' +
        'metadata is shown above. Use the link below to try again.';
      return;
    }
  }

  const panel = document.querySelector('#paper-panel');
  if (!panel) return;
  const body = panel.querySelector('#panel-body');
  body.addEventListener('click', event => {
    const link = event.target.closest('a[href^="#tei-"]');
    if (!link || event.button !== 0 || event.metaKey || event.ctrlKey ||
        event.shiftKey || event.altKey) return;
    const target = body.querySelector(link.getAttribute('href'));
    if (!target) return;
    event.preventDefault();
    target.scrollIntoView({block: 'start'});
    target.setAttribute('tabindex', '-1');
    target.focus({preventScroll: true});
  });
  const closeButton = panel.querySelector('#panel-close');
  const openPage = panel.querySelector('#panel-open-page');
  const filename = panel.querySelector('#panel-filename');
  const layout = document.querySelector('.paper-layout');
  const cache = new Map();
  const tabs = [...panel.querySelectorAll('[role="tab"]')];
  let current, controller, serial = 0, opener, lastHash;
  const remember = () => {
    if (current && cache.has(current)) {
      cache.get(current).scroll = body.scrollTop;
    }
  };
  const markLinks = (kind, url) => {
    document.querySelectorAll('[data-panel]').forEach(link => {
      const selected = link.dataset.panel === kind &&
        (kind === 'history' || new URL(link.href).search === url.search);
      if (link.role === 'tab') {
        link.setAttribute('aria-selected', String(link.dataset.panel === kind));
        link.tabIndex = link.dataset.panel === kind ? 0 : -1;
      } else link.setAttribute('aria-expanded', String(selected));
    });
  };
  const show = async (kind, url, focus = false) => {
    if (!['/events', '/read'].includes(url.pathname) ||
        url.origin !== location.origin) return;
    const key = url.pathname + url.search;
    const token = ++serial;
    controller?.abort();
    remember();
    current = key;
    panel.hidden = false;
    layout.classList.add('panel-open');
    document.querySelector('main').classList.add('panel-open');
    openPage.href = key;
    filename.textContent = kind === 'tei' ? url.searchParams.get('file') : 'Event history';
    const tab = tabs.find(tab => tab.dataset.panel === kind);
    // Keep the chosen conversion when switching back from History.
    if (tab) tab.href = key;
    markLinks(kind, url);
    body.setAttribute('aria-labelledby', tab?.id || 'panel-heading');
    if (focus) (tab || closeButton).focus({preventScroll: true});
    if (cache.has(key)) {
      const saved = cache.get(key);
      body.replaceChildren(saved.node);
      body.scrollTop = saved.scroll;
      body.setAttribute('aria-busy', 'false');
      return;
    }
    body.replaceChildren();
    body.scrollTop = 0;
    const loading = document.createElement('p');
    loading.role = 'status';
    loading.textContent = kind === 'tei' ? 'Loading TEI…' : 'Loading history…';
    body.append(loading);
    body.setAttribute('aria-busy', 'true');
    controller = new AbortController();
    try {
      const request = new URL(url);
      request.searchParams.set('view', 'panel');
      const response = await fetch(request, {signal: controller.signal});
      if (!response.ok) throw new Error(`HTTP ${response.status}`);
      const html = await response.text();
      if (token !== serial) return;
      const document = new DOMParser().parseFromString(html, 'text/html');
      const content = document.querySelector('[data-panel-content]');
      if (!content) throw new Error('Missing panel content');
      cache.set(key, {node: content, scroll: 0});
      body.replaceChildren(content);
      renderMath(content);
    } catch (error) {
      if (error.name === 'AbortError' || token !== serial) return;
      const message = document.createElement('p');
      message.role = 'alert';
      message.textContent = 'Could not load this view. Retry or open the full page.';
      const retry = document.createElement('button');
      retry.type = 'button';
      retry.textContent = 'Retry';
      retry.addEventListener('click', () => show(kind, url));
      body.replaceChildren(message, retry);
    } finally {
      if (token === serial) body.setAttribute('aria-busy', 'false');
    }
  };
  const hide = (focus = false) => {
    ++serial;
    controller?.abort();
    remember();
    current = null;
    panel.hidden = true;
    layout.classList.remove('panel-open');
    document.querySelector('main').classList.remove('panel-open');
    document.querySelectorAll('[data-panel][aria-expanded]').forEach(link => {
      link.setAttribute('aria-expanded', 'false');
    });
    if (focus) (opener || document.querySelector('[data-panel]'))?.focus({preventScroll: true});
  };
  const navigate = (kind, url, focus = false) => {
    const hash = kind === 'history' ? '#history' :
      `#tei=${encodeURIComponent(url.searchParams.get('file'))}`;
    if (location.hash !== hash) history.pushState(null, '', hash);
    lastHash = hash;
    show(kind, url, focus);
  };
  document.addEventListener('click', event => {
    const link = event.target.closest('a[data-panel]');
    if (!link || event.button !== 0 || event.metaKey || event.ctrlKey ||
        event.shiftKey || event.altKey) return;
    event.preventDefault();
    if (!panel.contains(link)) opener = link;
    navigate(link.dataset.panel, new URL(link.href), true);
  });
  const close = () => {
    history.pushState(null, '', location.pathname + location.search);
    lastHash = '';
    hide(true);
  };
  closeButton.addEventListener('click', close);
  document.addEventListener('keydown', event => {
    if (event.key === 'Escape' && !panel.hidden) {
      event.preventDefault();
      close();
    }
  });
  tabs.forEach((tab, i) => tab.addEventListener('keydown', event => {
    if (!['ArrowLeft', 'ArrowRight', 'Home', 'End'].includes(event.key)) return;
    event.preventDefault();
    const next = event.key === 'Home' ? 0 : event.key === 'End' ? tabs.length - 1 :
      (i + (event.key === 'ArrowRight' ? 1 : -1) + tabs.length) % tabs.length;
    navigate(tabs[next].dataset.panel, new URL(tabs[next].href), true);
  }));
  const restore = () => {
    if (lastHash === location.hash) return;
    lastHash = location.hash;
    let link;
    if (location.hash === '#history') {
      link = document.querySelector('[data-panel="history"]');
    } else if (location.hash.startsWith('#tei=')) {
      try {
        const file = decodeURIComponent(location.hash.slice(5));
        link = [...document.querySelectorAll('[data-panel="tei"]')]
          .find(link => new URL(link.href).searchParams.get('file') === file);
      } catch (_) { /* An invalid fragment leaves the paper visible. */ }
    }
    if (link) show(link.dataset.panel, new URL(link.href));
    else hide();
  };
  addEventListener('popstate', restore);
  addEventListener('hashchange', restore);
  restore();
})();

(() => {
  const root = document.querySelector('#activity[data-activity-url]');
  if (!root) return;
  const tree = root.querySelector('#activity-jobs');
  const older = root.querySelector('#activity-older');
  const web = root.querySelector('#activity-web tbody');
  const state = root.querySelector('#activity-state');
  const updated = root.querySelector('#activity-updated');
  const metric = name => root.querySelector(`[data-metric="${name}"]`);
  const counter = name => root.querySelector(`[data-count-for="${name}"]`);
  const cursors = {web: 0, jobs: null, before: null};
  const jobs = new Map();
  let timer = null, webLines = 0, loading = false;
  const el = (tag, cls, text) => {
    const node = document.createElement(tag);
    if (cls) node.className = cls;
    if (text !== undefined) node.textContent = text;
    return node;
  };
  const linked = (text, href, cls) => {
    const a = el('a', cls, text); a.href = href; return a;
  };
  const clock = iso => iso.replace(/^.*T/, '').replace(/Z$/, '');
  const line = e => {
    const tr = el('tr', 'activity-web');
    tr.append(el('td', 'activity-time', clock(e.time)), el('td', '', e.event), el('td', '', e.detail));
    web.prepend(tr);
    while (web.children.length > 300) web.lastElementChild.remove();
    counter('web').textContent = ++webLines;
  };
  const known = ['queued', 'running', 'done', 'failed', 'cancelled'];
  const icon = state => {
    const id = known.includes(state) ? state : 'unknown';
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    svg.setAttribute('class', `state-icon state-${id}`);
    svg.setAttribute('role', 'img');
    svg.setAttribute('aria-label', state);
    const title = document.createElementNS('http://www.w3.org/2000/svg', 'title');
    title.textContent = state;
    const use = document.createElementNS('http://www.w3.org/2000/svg', 'use');
    use.setAttribute('href', `#state-${id}`);
    svg.append(title, use);
    return svg;
  };
  const chip = a => {
    const kind = a.kind || 'running';
    const c = el('span', `attempt-chip ${kind === 'succeeded' ? 'ok' : kind === 'failed' ? 'bad' : kind === 'running' ? 'live' : 'retry'}`);
    c.append(el('b', '', `#${a.attempt}`), ' ', kind.replace('_', ' '));
    if (a.seconds !== undefined) c.append(el('span', 'attempt-time', `${a.seconds} s`));
    if (a.detail) c.title = a.detail;
    if (a.started) c.title = `${c.title ? c.title + '\n' : ''}claimed ${clock(a.started)}${a.ended ? ' → ' + clock(a.ended) : ''}`;
    return c;
  };
  const describe = (row, job) => {
    row.dataset.state = job.state;
    const head = el('div', 'job-head');
    head.append(linked(job.queue, job.href, 'job-queue'), el('span', 'job-state-text', job.state),
      el('span', 'job-id', `job ${job.id}`));
    const chips = el('div', 'job-attempts');
    (job.attempts || []).forEach(a => chips.append(chip(a)));
    if (job.state === 'running' && !(job.attempts || []).some(a => !a.kind)) chips.append(el('span', 'attempt-chip live', 'running'));
    if (job.enqueued) row.dataset.enqueued = job.enqueued;
    const start = row.dataset.enqueued;
    const settled = /done|failed|cancelled/.test(job.state);
    const when = el('span', 'job-when');
    const stamp = (iso, text) => { const t = el('time', '', text || clock(iso)); t.dateTime = iso; return t; };
    if (start && settled) {
      const seconds = Math.max(0, Math.round((Date.parse(job.updated) - Date.parse(start)) / 1000));
      when.append(stamp(start), ' → ', stamp(job.updated), el('span', 'job-elapsed', `${seconds} s`));
      when.title = `queued ${start}, settled ${job.updated}`;
    } else if (start) {
      when.append(stamp(start, `queued ${clock(start)}`), ' → ', el('span', 'job-pending', job.state));
      when.title = `queued ${start}`;
    } else {
      when.append(stamp(job.updated));
      when.title = `last event ${job.updated}`;
    }
    row.replaceChildren(icon(job.state), head, chips, when);
  };
  const pipeline = job => {
    const card = el('li', 'pipeline');
    card.dataset.job = job.id;
    const title = el('div', 'pipeline-title');
    title.append(job.uid ? linked(job.uid, job.paper || job.href, 'pipeline-paper') : el('span', 'pipeline-paper', 'Unattributed work'));
    if (job.review) title.append(el('span', 'pipeline-review', ' · '), linked(job.review, job.review_href, 'pipeline-review'));
    card.append(title, el('ul', 'job-list'));
    return card;
  };
  // Live updates go on top; older pages go underneath.
  const place = (job, atEnd) => {
    let row = jobs.get(job.id);
    if (!row) {
      const item = el('li', 'job');
      row = el('div', 'job-row');
      item.append(row, el('ul', 'job-list'));
      const parent = job.parent && jobs.get(job.parent);
      if (parent) parent.parentElement.lastElementChild.append(item);
      else { const card = pipeline(job); card.lastElementChild.append(item); atEnd ? tree.append(card) : tree.prepend(card); }
      jobs.set(job.id, row);
    }
    describe(row, job);
  };
  const trim = () => {
    while (tree.children.length > 400) {
      const old = tree.lastElementChild;
      old.querySelectorAll('.job-row').forEach(r => { for (const [k, v] of jobs) if (v === r) jobs.delete(k); });
      old.remove();
    }
  };
  const summarise = feed => {
    const rows = [...tree.querySelectorAll('.job-row')];
    metric('queued').textContent = feed.counts.queued;
    metric('running').textContent = feed.counts.running;
    metric('failed').textContent = rows.filter(r => /failed|cancelled/.test(r.dataset.state)).length;
    metric('papers').textContent = tree.children.length;
    counter('jobs').textContent = rows.length;
  };
  const fetchFeed = async params => {
    const url = new URL(root.dataset.activityUrl, location.origin);
    for (const [k, v] of Object.entries(params)) url.searchParams.set(k, v);
    const response = await fetch(url, {cache: 'no-store'});
    if (!response.ok) throw new Error(`Server answered ${response.status}`);
    return response.json();
  };
  const apply = (feed, atEnd) => {
    if (tree) { feed.jobs.forEach(j => place(j, atEnd)); if (!atEnd) trim(); summarise(feed); }
    feed.web.forEach(line);
    cursors.web = feed.cursors.web;
    root.dataset.live = 'ok';
    state.textContent = 'Live';
    updated.textContent = `updated ${new Date().toLocaleTimeString()}`;
  };
  const poll = async () => {
    if (document.hidden || loading) return;
    loading = true;
    try {
      if (cursors.jobs === null) {
        const feed = await fetchFeed({snapshot: 1, web: cursors.web});
        cursors.jobs = feed.cursors.jobs;
        cursors.before = feed.cursors.before ?? null;
        apply(feed, false);
      } else {
        const feed = await fetchFeed({jobs: cursors.jobs, web: cursors.web});
        cursors.jobs = feed.cursors.jobs;
        apply(feed, false);
      }
    } catch (e) { state.textContent = e.message.startsWith('Server') ? e.message : 'Reconnecting'; root.dataset.live = 'error'; }
    finally { loading = false; }
  };
  const loadOlder = async () => {
    if (cursors.before === null || loading) return;
    loading = true;
    older.disabled = true;
    try {
      const feed = await fetchFeed({before: cursors.before, web: cursors.web});
      cursors.before = feed.cursors.before ?? null;
      apply(feed, true);
      if (cursors.before === null) older.textContent = 'No older events';
    } catch (_) { /* The button can be tried again. */ }
    finally { loading = false; older.disabled = cursors.before === null; }
  };
  if (older) older.addEventListener('click', loadOlder);
  const start = () => { if (!timer) { poll(); timer = setInterval(poll, 2000); } };
  const stop = () => { if (timer) { clearInterval(timer); timer = null; state.textContent = 'Paused'; root.dataset.live = 'paused'; } };
  document.addEventListener('visibilitychange', () => document.hidden ? stop() : start());
  start();
})();
