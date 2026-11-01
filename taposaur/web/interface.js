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
  document.querySelectorAll('.site-nav a').forEach(link => {
    if (location.pathname.startsWith('/review') && link.pathname === '/reviews' ||
        !location.pathname.startsWith('/review') && link.pathname === '/search')
      link.setAttribute('aria-current', 'page');
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
