(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** JavaScript strings for client-side interactivity. *)

let sidenotes_js = {|
// Sidenote positioning - keeps sidenotes aligned with their references.
// Each sidenote is placed at the Y position of its in-article ref,
// relative to the sidenotes-container. If it would overlap the previous
// sidenote, it is pushed down to sit just below it.
function positionSidenotes() {
  const container = document.getElementById('sidenotes-container');
  if (!container || window.innerWidth < 1024) return;

  // Collect all sidenote divs in DOM order (matches ref order in article)
  const sidenotes = container.querySelectorAll('.sidenote');
  if (!sidenotes.length) return;

  const containerRect = container.getBoundingClientRect();
  const containerPageTop = containerRect.top + window.scrollY;
  let lastBottom = 0;

  sidenotes.forEach(sidenote => {
    const id = sidenote.id.replace('sidenote-', '');
    const ref = document.querySelector('.sidenote-ref[data-sidenote="' + id + '"]');
    if (!ref) return;

    // Where the ref is on the page, relative to the container
    const refRect = ref.getBoundingClientRect();
    const refPageTop = refRect.top + window.scrollY;
    let targetTop = refPageTop - containerPageTop;

    // Clamp: never go above 0 (above the container)
    if (targetTop < 0) targetTop = 0;

    // Prevent overlap with previous sidenote
    if (targetTop < lastBottom + 8) {
      targetTop = lastBottom + 8;
    }

    sidenote.style.top = targetTop + 'px';

    // Use getBoundingClientRect for accurate height after positioning
    lastBottom = targetTop + sidenote.getBoundingClientRect().height;
  });

  // Reveal sidenotes now they are positioned
  sidenotes.forEach(sidenote => {
    sidenote.classList.remove('sidenote-hidden');
    sidenote.classList.add('sidenote-visible');
  });
}

// Floating thumbnail overlay that follows cursor
var thumbOverlay = null;
function getThumbOverlay() {
  if (!thumbOverlay) {
    thumbOverlay = document.createElement('div');
    thumbOverlay.className = 'fixed pointer-events-none z-50 rounded shadow-lg border border-border-color bg-bg p-0.5 transition-opacity duration-150 opacity-0 overflow-hidden';
    thumbOverlay.style.width = '56px';
    thumbOverlay.style.height = '56px';
    document.body.appendChild(thumbOverlay);
  }
  return thumbOverlay;
}

function showThumbOverlay(src, e) {
  const ov = getThumbOverlay();
  ov.innerHTML = '<img src="' + src + '" style="width:100%;height:100%;object-fit:cover;border-radius:3px;">';
  ov.style.left = (e.clientX + 12) + 'px';
  ov.style.top = (e.clientY + 12) + 'px';
  ov.classList.remove('opacity-0');
  ov.classList.add('opacity-100');
}

function moveThumbOverlay(e) {
  if (!thumbOverlay) return;
  thumbOverlay.style.left = (e.clientX + 12) + 'px';
  thumbOverlay.style.top = (e.clientY + 12) + 'px';
}

function hideThumbOverlay() {
  if (!thumbOverlay) return;
  thumbOverlay.classList.remove('opacity-100');
  thumbOverlay.classList.add('opacity-0');
}

// Sidenote hover interaction
function setupSidenoteHover() {
  document.querySelectorAll('.sidenote').forEach(sidenote => {
    const id = sidenote.id.replace('sidenote-', '');
    const refs = document.querySelectorAll('.sidenote-ref[data-sidenote="' + id + '"]');
    if (!refs.length) return;
    // Find thumbnail src stored as data attr (shown on hover)
    const thumbSrc = sidenote.dataset.thumb || '';

    function activate(e) {
      sidenote.classList.add('!border-accent', '!text-text');
      refs.forEach(ref => ref.classList.add('highlighted'));
      if (thumbSrc) showThumbOverlay(thumbSrc, e);
    }
    function deactivate() {
      sidenote.classList.remove('!border-accent', '!text-text');
      refs.forEach(ref => ref.classList.remove('highlighted'));
      hideThumbOverlay();
    }

    sidenote.addEventListener('mouseenter', activate);
    sidenote.addEventListener('mousemove', moveThumbOverlay);
    sidenote.addEventListener('mouseleave', deactivate);

    // Inline refs highlight sidenote but do NOT show thumbnail
    function activateNoThumb() {
      sidenote.classList.add('!border-accent', '!text-text');
      refs.forEach(ref => ref.classList.add('highlighted'));
    }
    refs.forEach(ref => {
      ref.addEventListener('mouseenter', activateNoThumb);
      ref.addEventListener('mouseleave', deactivate);
    });
  });
}

// Hide every inline note and clear the active marker on every ref.
function closeInlineNotes() {
  document.querySelectorAll('.sidenote-anchor').forEach(a =>
    a.classList.remove('sidenote-active'));
  document.querySelectorAll('.sidenote-inline').forEach(n =>
    n.classList.add('hidden'));
}

// Setup sidenote numbers and mobile toggles.
// Numbering is per slug: the first ref for a slug takes a new number and
// later refs reuse it. Inline notes are per ref, so a tap opens the note
// beside the ref that was tapped.
function setupSidenoteNumbers() {
  let noteNumber = 1;
  const seen = {};  // slug -> { number, count }
  document.querySelectorAll('.sidenote-ref').forEach(ref => {
    const id = ref.dataset.sidenote;
    const sidenote = document.getElementById('sidenote-' + id);
    if (!sidenote) return;

    // Determine the number: first occurrence gets a new one, duplicates reuse
    let currentNumber;
    let index;
    if (!seen[id]) {
      currentNumber = noteNumber++;
      index = 0;
      // Add number prefix to sidebar sidenote (only once)
      const numberSpan = document.createElement('span');
      numberSpan.className = 'sidenote-number font-semibold';
      numberSpan.textContent = currentNumber + '. ';
      sidenote.insertBefore(numberSpan, sidenote.firstChild);
      seen[id] = { number: currentNumber, count: 1 };
    } else {
      currentNumber = seen[id].number;
      index = seen[id].count++;
    }

    // Add toggle badge to every ref (shows the same number)
    const toggle = document.createElement('span');
    toggle.className = 'sidenote-toggle';
    toggle.textContent = currentNumber;
    const anchor = ref.closest('.sidenote-anchor');
    if (!anchor) return;
    // U+2060 WORD JOINER forbids a line break between the last letter of
    // the ref text and the marker. The .sidenote-toggle rule in theme.ml
    // keeps the marker display: inline so it is not an atomic inline,
    // which would reintroduce the break. Either half alone still lets
    // the marker wrap on to a line of its own.
    anchor.appendChild(document.createTextNode('\u2060'));
    anchor.appendChild(toggle);

    // One inline note per ref, keyed by slug and occurrence so ids stay
    // unique. A ref in no block has nowhere to put one and gets none.
    const block = ref.closest('p, blockquote, li, h1, h2, h3, h4, h5, ' +
      'h6, td, th, dd, dt, figcaption');
    if (!block) return;
    const inlineNote = document.createElement('div');
    inlineNote.className = 'hidden lg:!hidden sidenote-inline text-sm leading-relaxed text-text bg-surface border-l-2 border-accent px-3 py-2 my-2 rounded-r';
    inlineNote.id = 'sidenote-inline-' + id + '-' + index;
    inlineNote.innerHTML = '<span class="sidenote-number font-semibold">' + currentNumber + '.</span> ' + sidenote.innerHTML.replace(/<span class="sidenote-number.*?<\/span>/, '');
    block.insertAdjacentElement('afterend', inlineNote);

    anchor.addEventListener('click', (e) => {
      // Read the width at click time: the window can be resized after
      // setup, and above the breakpoint the link must navigate.
      if (window.innerWidth >= 1024) return;
      e.preventDefault();
      const wasActive = anchor.classList.contains('sidenote-active');
      closeInlineNotes();
      if (!wasActive) {
        anchor.classList.add('sidenote-active');
        inlineNote.classList.remove('hidden');
      }
    });
  });
  document.addEventListener('click', (e) => {
    if (!e.target.closest('.sidenote-anchor') &&
        !e.target.closest('.sidenote-inline')) {
      closeInlineNotes();
    }
  });
}

window.addEventListener('load', () => {
  // Setup numbers/toggles (modifies DOM)
  setupSidenoteNumbers();
  // Position after layout settles
  requestAnimationFrame(() => {
    positionSidenotes();
    setupSidenoteHover();
  });
  // Re-position again after images/fonts finish loading
  setTimeout(positionSidenotes, 500);
});
// Positions are computed in page coordinates, so scrolling never changes
// them by itself. What does move a ref is the article growing under it:
// its images carry no intrinsic size and most are lazy, so each one that
// decodes mid-scroll reflows everything below it. Watch the article and
// reposition when its height actually changes, which also covers fonts
// and late layout settling.
window.addEventListener('resize', positionSidenotes);
(function() {
  const article = document.querySelector('main');
  if (!article || !window.ResizeObserver) return;
  let pending = null;
  const observer = new ResizeObserver(() => {
    if (pending) cancelAnimationFrame(pending);
    pending = requestAnimationFrame(() => { pending = null; positionSidenotes(); });
  });
  observer.observe(article);
})();
|}

let toc_js = {|
// Table of Contents functionality (desktop only)
function setupTOC() {
  if (window.innerWidth < 1024) return;
  const tocBox = document.querySelector('.toc-box');
  const tocList = document.querySelector('#toc-box .toc-list');
  const tocLinks = document.querySelectorAll('.toc-link');
  if (!tocList) return;

  const sections = [];
  tocLinks.forEach(link => {
    const el = document.getElementById(link.getAttribute('href').slice(1));
    if (el) sections.push({ link, el, level: parseInt(link.dataset.level) || 2 });
  });
  if (!sections.length) return;

  // Pin the box flush under the header. Any gap lets the sidebar show
  // through as it scrolls behind, and the header height depends on how
  // the nav row wraps.
  function setTocTop() {
    const header = document.getElementById('header');
    if (!header) return;
    document.documentElement.style.setProperty('--toc-top', header.offsetHeight + 'px');
  }

  function updateTOC() {
    const scrollY = window.scrollY;
    const header = document.getElementById('header');
    const headerHeight = header ? header.offsetHeight : 0;

    // The fade below the box only makes sense once it has pinned.
    tocBox.classList.toggle('stuck',
      tocBox.getBoundingClientRect().top <= headerHeight + 1);

    const docHeight = document.documentElement.scrollHeight;
    const atBottom = (scrollY + window.innerHeight) >= docHeight - 5;
    const top = (el) => el.getBoundingClientRect().top + scrollY - headerHeight;
    // The reading position: the point a heading has to pass to count as
    // the section being read. At the foot of the page nothing further can
    // scroll, so read the last section as still in progress.
    const pos = atBottom ? docHeight - 1 : scrollY + 50;

    let activeLink = null;
    sections.forEach((section, i) => {
      // A section runs until the next heading at the same depth or
      // shallower, so an h2 stays active, and keeps filling, while its
      // subsections scroll past.
      let end = docHeight;
      for (let j = i + 1; j < sections.length; j++) {
        if (sections[j].level <= section.level) { end = top(sections[j].el); break; }
      }
      const start = top(section.el);
      const progress = Math.min(Math.max((pos - start) / Math.max(end - start, 1), 0), 1);
      const active = pos >= start && pos < end;
      section.link.classList.toggle('passed', pos >= end);
      section.link.classList.toggle('active', active);
      section.link.style.setProperty('--progress', (progress * 100) + '%');
      if (active) activeLink = section.link;
    });

    // A long contents list scrolls within its box, so keep the active
    // row in view without moving the page.
    if (activeLink && tocList.scrollHeight > tocList.clientHeight) {
      const listHeight = tocList.clientHeight;
      const scrollTarget = activeLink.offsetTop - (listHeight / 2)
        + (activeLink.offsetHeight / 2);
      tocList.scrollTo({ top: scrollTarget, behavior: 'smooth' });
    }
  }

  const scrollToSection = (targetId) => {
    const target = document.getElementById(targetId);
    if (target) {
      const header = document.getElementById('header');
      const headerHeight = header ? header.offsetHeight : 0;
      const targetPosition = target.getBoundingClientRect().top + window.scrollY - headerHeight - 20;
      window.scrollTo({ top: targetPosition, behavior: 'smooth' });
    }
  };

  tocLinks.forEach(link => {
    link.addEventListener('click', (e) => {
      e.preventDefault();
      scrollToSection(link.getAttribute('href').slice(1));
    });
  });

  const tocRoot = document.getElementById('toc-root');
  if (tocRoot) {
    tocRoot.addEventListener('click', (e) => {
      e.preventDefault();
      scrollToSection('intro');
    });
  }

  window.addEventListener('scroll', updateTOC, { passive: true });
  window.addEventListener('resize', setTocTop);
  setTocTop();
  updateTOC();
}

window.addEventListener('load', () => { setTimeout(setupTOC, 150); });
|}

let search_shortcut_js = {|
// Global search navigation: Cmd-K/Ctrl-K opens the search page, and
// [data-tag]/[data-kind] chips and #tag=/#kind= hashes route to it. The
// search page itself owns these on /search, so bail out there.
(function() {
  document.addEventListener('keydown', function(e) {
    if ((e.metaKey || e.ctrlKey) && e.key === 'k') {
      e.preventDefault();
      window.location.href = '/search';
    }
  });

  document.addEventListener('click', function(e) {
    if (document.getElementById('search-page-input')) return;
    var tagEl = e.target.closest('[data-tag]');
    // Tag-cloud chips filter in place; they are not a search-page link.
    if (tagEl && !tagEl.classList.contains('tag-cloud-btn')) {
      e.preventDefault();
      window.location.href = '/search?q=' + encodeURIComponent('#' + tagEl.getAttribute('data-tag'));
      return;
    }
    var kindEl = e.target.closest('[data-kind]');
    if (kindEl) {
      e.preventDefault();
      window.location.href = '/search?q=' + encodeURIComponent('kind:' + kindEl.getAttribute('data-kind'));
    }
  });

  if (!document.getElementById('search-page-input')) {
    var hash = location.hash;
    if (hash.indexOf('#tag=') === 0) {
      var tag = decodeURIComponent(hash.slice(5));
      location.replace('/search?q=' + encodeURIComponent('#' + tag));
    } else if (hash.indexOf('#kind=') === 0) {
      var kind = decodeURIComponent(hash.slice(6));
      location.replace('/search?q=' + encodeURIComponent('kind:' + kind));
    }
  }
})();
|}

let links_modal_js = {|
(function() {
  document.querySelectorAll('[data-modal-target]').forEach(function(btn) {
    var overlay = document.getElementById(btn.dataset.modalTarget);
    if (!overlay) return;
    var closeBtn = overlay.querySelector('.links-modal-close-btn');
    function open() {
      overlay.classList.add('active');
      document.body.style.overflow = 'hidden';
    }
    function close() {
      overlay.classList.remove('active');
      document.body.style.overflow = '';
    }
    btn.addEventListener('click', open);
    if (closeBtn) closeBtn.addEventListener('click', close);
    overlay.addEventListener('click', function(e) {
      if (e.target === overlay) close();
    });
  });
  document.addEventListener('keydown', function(e) {
    if (e.key === 'Escape') {
      document.querySelectorAll('.links-modal-overlay.active').forEach(function(o) {
        o.classList.remove('active');
      });
      document.body.style.overflow = '';
    }
  });
})();
|}

let hljs_init = {|
(function() {
  function updateHljsTheme() {
    var isDark = document.documentElement.classList.contains('dark');
    var light = document.getElementById('hljs-light');
    var dark = document.getElementById('hljs-dark');
    if (light && dark) {
      if (isDark) { light.disabled = true; dark.disabled = false; }
      else { light.disabled = false; dark.disabled = true; }
    }
  }
  updateHljsTheme();
  if (typeof hljs !== 'undefined') hljs.highlightAll();

  var copySvg = '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="9" y="9" width="13" height="13" rx="2"/><path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"/></svg>';
  var checkSvg = '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="20 6 9 17 4 12"/></svg>';

  document.querySelectorAll('pre > code').forEach(function(code) {
    var pre = code.parentElement;
    var rawText = code.textContent;
    var langMatch = code.className.match(/language-(\S+)/);
    var lang = langMatch ? langMatch[1] : '';

    var wrapper = document.createElement('div');
    wrapper.className = 'code-block';

    var toolbar = document.createElement('div');
    toolbar.className = 'code-toolbar';
    var copyBtn = document.createElement('button');
    copyBtn.className = 'code-copy';
    copyBtn.setAttribute('aria-label', 'Copy code');
    copyBtn.innerHTML = copySvg;
    copyBtn.addEventListener('click', function() {
      navigator.clipboard.writeText(rawText).then(function() {
        copyBtn.innerHTML = checkSvg;
        copyBtn.classList.add('copied');
        setTimeout(function() {
          copyBtn.innerHTML = copySvg;
          copyBtn.classList.remove('copied');
        }, 1500);
      });
    });
    toolbar.appendChild(copyBtn);

    pre.parentNode.insertBefore(wrapper, pre);
    wrapper.appendChild(toolbar);
    wrapper.appendChild(pre);
  });

  var observer = new MutationObserver(function(mutations) {
    mutations.forEach(function(m) {
      if (m.attributeName === 'class') updateHljsTheme();
    });
  });
  observer.observe(document.documentElement, { attributes: true });
})();
|}

let pagination_js = {|
// Pagination - lazy load more entries
(function() {
  const article = document.querySelector('[data-pagination="true"]');
  if (!article) return;

  const totalCount = parseInt(article.dataset.totalCount || '0');
  const collectionType = article.dataset.collectionType || 'entries';
  const types = article.dataset.types || '';
  let currentCount = parseInt(article.dataset.currentCount || '0');
  let loading = false;

  function loadMore() {
    if (loading || currentCount >= totalCount) return;
    loading = true;

    fetch('/api/entries?collection=' + collectionType + '&type=' + encodeURIComponent(types) + '&offset=' + currentCount + '&limit=25')
      .then(r => r.json())
      .then(data => {
        if (data.html) {
          const temp = document.createElement('div');
          temp.innerHTML = data.html;
          while (temp.firstChild) article.appendChild(temp.firstChild);
          currentCount += data.count || 0;
          article.dataset.currentCount = currentCount;
          document.dispatchEvent(new CustomEvent('pagination-loaded'));
          if (currentCount >= totalCount && sentinel) sentinel.remove();
        }
        loading = false;
      })
      .catch(() => { loading = false; });
  }

  if (currentCount < totalCount) {
    var sentinel = document.createElement('div');
    sentinel.style.height = '1px';
    article.after(sentinel);
    const observer = new IntersectionObserver((entries) => {
      if (entries[0].isIntersecting) loadMore();
    }, { rootMargin: '200px' });
    observer.observe(sentinel);
  }
})();
|}

let lightbox_js = {|
(function() {
  // Create lightbox overlay
  const overlay = document.createElement('div');
  overlay.id = 'lightbox-overlay';
  overlay.innerHTML = `
    <div class="lightbox-content">
      <img class="lightbox-img" />
      <div class="lightbox-below">
        <div class="lightbox-caption"></div>
        <div class="lightbox-downloads"></div>
      </div>
    </div>
    <button class="lightbox-close" aria-label="Close">&times;</button>
  `;
  document.body.appendChild(overlay);

  const img = overlay.querySelector('.lightbox-img');
  const caption = overlay.querySelector('.lightbox-caption');
  const downloads = overlay.querySelector('.lightbox-downloads');
  const closeBtn = overlay.querySelector('.lightbox-close');

  function open(trigger) {
    const src = trigger.dataset.lightbox;
    const cap = trigger.dataset.caption || '';
    let variants = [];
    try { variants = JSON.parse(trigger.dataset.variants || '[]'); } catch(e) {}

    img.src = src;
    img.alt = cap;
    caption.textContent = cap;
    caption.style.display = cap ? '' : 'none';

    // Build download links sorted by width descending
    variants.sort((a,b) => b.w - a.w);
    downloads.innerHTML = variants.map(v =>
      `<a href="${v.url}" download class="lightbox-dl">${v.w}&times;${v.h}</a>`
    ).join('');

    overlay.classList.add('active');
    document.body.style.overflow = 'hidden';
  }

  function close() {
    overlay.classList.remove('active');
    document.body.style.overflow = '';
    img.src = '';
  }

  // Attach to all lightbox triggers and expand buttons.
  // If a lightbox-trigger is inside an <a href>, let the link navigate instead.
  document.addEventListener('click', (e) => {
    const expand = e.target.closest('.lightbox-expand');
    if (expand) { e.preventDefault(); e.stopPropagation(); open(expand); return; }
    const trigger = e.target.closest('.lightbox-trigger');
    if (trigger) {
      const parentLink = trigger.closest('a[href]');
      if (parentLink && !parentLink.classList.contains('lightbox-trigger')) return;
      e.preventDefault(); open(trigger); return;
    }
    if (e.target === overlay || e.target === closeBtn) { close(); }
  });

  overlay.addEventListener('click', (e) => {
    if (e.target === overlay) close();
  });

  document.addEventListener('keydown', (e) => {
    if (e.key === 'Escape' && overlay.classList.contains('active')) close();
  });
})();
|}

let theme_toggle_js = {|
(function() {
  var btn = document.getElementById('theme-toggle-btn');
  if (!btn) return;

  var iconSystem = btn.querySelector('.theme-icon-system');
  var iconLight = btn.querySelector('.theme-icon-light');
  var iconDark = btn.querySelector('.theme-icon-dark');

  function getEffective(pref) {
    if (pref === 'light') return 'light';
    if (pref === 'dark') return 'dark';
    return matchMedia('(prefers-color-scheme:dark)').matches ? 'dark' : 'light';
  }

  function apply(pref) {
    var eff = getEffective(pref);
    var html = document.documentElement;
    if (eff === 'dark') html.classList.add('dark');
    else html.classList.remove('dark');

    // Update meta theme-color
    var meta = document.getElementById('meta-theme-color');
    if (meta) meta.content = eff === 'dark' ? '#0d1117' : '#fffffc';

    // Update icons
    if (iconSystem && iconLight && iconDark) {
      iconSystem.classList.add('hidden');
      iconLight.classList.add('hidden');
      iconDark.classList.add('hidden');
      if (pref === 'light') iconLight.classList.remove('hidden');
      else if (pref === 'dark') iconDark.classList.remove('hidden');
      else iconSystem.classList.remove('hidden');
    }
  }

  // Read current preference
  var current = localStorage.getItem('theme') || 'system';
  apply(current);

  // Cycle: system -> light -> dark -> system
  btn.addEventListener('click', function() {
    var next;
    if (current === 'system') next = 'light';
    else if (current === 'light') next = 'dark';
    else next = 'system';
    current = next;
    if (next === 'system') localStorage.removeItem('theme');
    else localStorage.setItem('theme', next);
    apply(next);
  });

  // Listen for OS preference changes (only matters in system mode)
  matchMedia('(prefers-color-scheme:dark)').addEventListener('change', function() {
    if (!localStorage.getItem('theme')) apply('system');
  });
})();
|}

let checkbox_filter_js = {|
// Checkbox filters. A checkbox carrying data-filter shows or hides every
// element whose data-filter-item equals its value. A container marked
// data-filter-section is hidden once none of its items are visible.
(function() {
  var checkboxes = document.querySelectorAll('input.filter-checkbox[data-filter]');
  if (!checkboxes.length) return;

  function apply() {
    checkboxes.forEach(function(cb) {
      var items = document.querySelectorAll('[data-filter-item="' + cb.dataset.filter + '"]');
      items.forEach(function(item) {
        item.style.display = cb.checked ? '' : 'none';
      });
    });
    document.querySelectorAll('[data-filter-section]').forEach(function(section) {
      var items = section.querySelectorAll('[data-filter-item]');
      if (!items.length) return;
      var visible = false;
      items.forEach(function(item) {
        if (item.style.display !== 'none') visible = true;
      });
      section.style.display = visible ? '' : 'none';
    });
  }

  checkboxes.forEach(function(cb) { cb.addEventListener('change', apply); });
  // Content loaded by pagination must respect the current checkbox state
  document.addEventListener('pagination-loaded', apply);
  // Some boxes start unchecked (expired ideas, untitled links)
  apply();
})();
|}

let calendar_js = {|
// Sidebar calendar — a heatmap strip plus a detail grid, kept in sync with
// the timeline scroll position. The container declares year mode by
// carrying data-calendar-years ({year: [months]}) or month mode by
// carrying data-calendar-months ({ym: [days]}). It also carries
// data-cal-track (selector for timeline elements bearing data-year-id or
// data-month-id), data-cal-noun (count word) and data-cal-empty (tooltip
// for an empty period).
(function() {
  var container = document.querySelector('[data-calendar-years], [data-calendar-months]');
  if (!container) return;

  var yearMode = 'calendarYears' in container.dataset;
  var raw = (yearMode ? container.dataset.calendarYears : container.dataset.calendarMonths) || '{}';
  var data;
  try { data = JSON.parse(raw); } catch(e) { return; }
  var allKeys = Object.keys(data).sort().reverse();
  if (!allKeys.length) return;
  var current = (yearMode ? container.dataset.currentYear : container.dataset.currentMonth) || allKeys[0];
  var currentDay = 0;
  var trackSel = container.dataset.calTrack;
  var keyAttr = yearMode ? 'year-id' : 'month-id';
  var noun = container.dataset.calNoun;
  var emptyTip = container.dataset.calEmpty;

  var heatmapEl = container.querySelector('.heatmap-strip');
  var headerEl = container.querySelector('.cal-header');
  var gridEl = container.querySelector('.cal-grid');

  var shortMonths = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'];

  var now = new Date();
  var todayKey = yearMode
    ? String(now.getFullYear())
    : now.getFullYear() + '-' + String(now.getMonth() + 1).padStart(2, '0');

  function ymAdd(ym, offset) {
    var parts = ym.split('-');
    var y = parseInt(parts[0]);
    var m = parseInt(parts[1]) - 1 + offset;
    var ny = y + Math.floor(m / 12);
    var nm = ((m % 12) + 12) % 12;
    return ny + '-' + String(nm + 1).padStart(2, '0');
  }

  function countFor(key) {
    return data[key] ? data[key].length : 0;
  }

  function keyLabel(key) {
    return yearMode ? "'" + key.slice(-2) : shortMonths[parseInt(key.split('-')[1]) - 1];
  }

  function keyTipName(key) {
    return yearMode ? key : shortMonths[parseInt(key.split('-')[1]) - 1];
  }

  function getHeatmapWindow() {
    var win = [];
    if (yearMode) {
      var cy = parseInt(current);
      for (var i = -4; i <= 5; i++) win.push(String(cy + i));
    } else {
      for (var i = -5; i <= 6; i++) win.push(ymAdd(current, i));
    }
    return win;
  }

  function scrollToKey(key) {
    var el = document.querySelector(trackSel + '[data-' + keyAttr + '="' + key + '"]');
    if (el) el.scrollIntoView({ behavior: 'smooth', block: 'start' });
  }

  function renderHeatmap() {
    heatmapEl.innerHTML = '';
    var windowKeys = getHeatmapWindow();
    var maxCount = 1;
    windowKeys.forEach(function(k) {
      var c = countFor(k);
      if (c > maxCount) maxCount = c;
    });

    var strip = document.createElement('div');
    strip.className = 'heatmap-grid';
    strip.style.gridTemplateColumns = 'repeat(' + windowKeys.length + ', 1fr)';

    windowKeys.forEach(function(k) {
      var count = countFor(k);
      var isFuture = k > todayKey;
      var cell = document.createElement('div');
      cell.className = 'heatmap-cell';
      if (k === current) cell.classList.add('heatmap-current');

      if (isFuture) {
        cell.dataset.state = 'future';
        cell.dataset.level = 0;
        cell.title = keyTipName(k) + ': upcoming';
      } else if (count === 0) {
        cell.dataset.state = 'empty';
        cell.dataset.level = 0;
        cell.title = keyTipName(k) + ': ' + emptyTip;
      } else {
        cell.dataset.state = 'active';
        var level = Math.min(4, Math.ceil(count / maxCount * 4));
        cell.dataset.level = level;
        cell.title = keyTipName(k) + ': ' + count + ' ' + noun + (count !== 1 ? 's' : '');
      }

      if (!isFuture) {
        (function(target) {
          cell.addEventListener('click', function() {
            current = target;
            currentDay = 0;
            renderDetail(current);
            renderHeatmap();
            scrollToKey(target);
          });
        })(k);
      }

      var label = document.createElement('span');
      label.className = 'heatmap-label';
      label.textContent = keyLabel(k);

      var circle = document.createElement('div');
      circle.className = 'heatmap-circle';
      if (isFuture) {
        circle.innerHTML = '<svg xmlns="http://www.w3.org/2000/svg" width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><path d="M3 12a9 9 0 1 0 18 0a9 9 0 0 0 -18 0"/><path d="M12 7v5l3 3"/></svg>';
      } else if (count === 0) {
        circle.innerHTML = '<svg xmlns="http://www.w3.org/2000/svg" width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="3" stroke-linecap="round"><path d="M5 12h14"/></svg>';
      }

      cell.appendChild(label);
      cell.appendChild(circle);
      strip.appendChild(cell);
    });
    heatmapEl.appendChild(strip);
  }

  function renderHeader(titleText) {
    headerEl.innerHTML = '';
    var prevBtn = document.createElement('button');
    prevBtn.className = 'cal-nav';
    prevBtn.textContent = '\u25C0';
    prevBtn.addEventListener('click', function() { navigate(-1); });
    var nextBtn = document.createElement('button');
    nextBtn.className = 'cal-nav';
    nextBtn.textContent = '\u25B6';
    nextBtn.addEventListener('click', function() { navigate(1); });
    var title = document.createElement('span');
    title.className = 'cal-title';
    title.textContent = titleText;
    headerEl.appendChild(prevBtn);
    headerEl.appendChild(title);
    headerEl.appendChild(nextBtn);
  }

  function daysInMonth(y, m) { return new Date(y, m, 0).getDate(); }
  function firstDayOfWeek(y, m) {
    var d = new Date(y, m - 1, 1).getDay();
    return d === 0 ? 6 : d - 1;
  }

  // Year mode: 12 month-of-year cells. Month mode: a weekday-aligned grid
  // of the month's days, padded to six rows so the box height is stable.
  function renderDetail(key) {
    if (yearMode) {
      renderHeader(key);
      var monthSet = new Set(data[key] || []);
      gridEl.innerHTML = '';
      gridEl.style.gridTemplateColumns = 'repeat(4, 1fr)';
      for (var m = 0; m < 12; m++) {
        var cell = document.createElement('span');
        cell.className = monthSet.has(m + 1) ? 'cal-day cal-day-active' : 'cal-day cal-day-empty';
        cell.textContent = shortMonths[m];
        gridEl.appendChild(cell);
      }
      return;
    }

    var parts = key.split('-');
    var year = parseInt(parts[0]);
    var month = parseInt(parts[1]);
    var daySet = new Set(data[key] || []);
    var total = daysInMonth(year, month);
    var offset = firstDayOfWeek(year, month);

    renderHeader(shortMonths[month - 1] + ' ' + year);

    gridEl.innerHTML = '';
    ['Mo','Tu','We','Th','Fr','Sa','Su'].forEach(function(wd) {
      var cell = document.createElement('span');
      cell.className = 'cal-weekday';
      cell.textContent = wd;
      gridEl.appendChild(cell);
    });
    for (var i = 0; i < offset; i++) {
      var empty = document.createElement('span');
      empty.className = 'cal-day cal-day-empty';
      gridEl.appendChild(empty);
    }
    for (var d = 1; d <= total; d++) {
      var cell = document.createElement('span');
      if (daySet.has(d)) {
        cell.className = 'cal-day cal-day-active';
        if (d === currentDay) cell.classList.add('cal-day-viewing');
      } else {
        cell.className = 'cal-day cal-day-empty';
      }
      cell.textContent = d;
      gridEl.appendChild(cell);
    }
    var totalCells = offset + total;
    while (totalCells < 42) {
      var pad = document.createElement('span');
      pad.className = 'cal-day cal-day-pad';
      gridEl.appendChild(pad);
      totalCells++;
    }
  }

  function navigate(dir) {
    var idx = allKeys.indexOf(current);
    var next = idx - dir;
    if (next >= 0 && next < allKeys.length) {
      current = allKeys[next];
      currentDay = 0;
      renderDetail(current);
      renderHeatmap();
    }
  }

  renderHeatmap();
  renderDetail(current);

  // Scroll tracking. Year mode reads section positions on scroll; month
  // mode observes timeline items, re-observing whatever pagination adds.
  if (yearMode) {
    if (document.querySelector(trackSel)) {
      // Query on each scroll so sections added by pagination are tracked
      var updateCurrent = function() {
        var best = null;
        document.querySelectorAll(trackSel).forEach(function(s) {
          if (s.getBoundingClientRect().top <= 120) best = s;
        });
        if (best) {
          var key = best.dataset.yearId;
          if (key && key !== current) {
            current = key;
            renderDetail(current);
            renderHeatmap();
          }
        }
      };
      window.addEventListener('scroll', updateCurrent, { passive: true });
      updateCurrent();
    }
  } else if ('IntersectionObserver' in window) {
    var observed = new WeakSet();
    var observer = new IntersectionObserver(function(entries) {
      entries.forEach(function(entry) {
        if (entry.isIntersecting) {
          var monthId = entry.target.dataset.monthId;
          var day = parseInt(entry.target.dataset.day || '0');
          var changed = false;
          if (monthId && monthId !== current) {
            current = monthId;
            currentDay = day;
            changed = true;
          } else if (day && day !== currentDay) {
            currentDay = day;
            changed = true;
          }
          if (changed) {
            renderDetail(current);
            renderHeatmap();
          }
        }
      });
    }, { rootMargin: '-80px 0px -60% 0px' });

    var observeItems = function() {
      document.querySelectorAll(trackSel + '[data-month-id]').forEach(function(s) {
        if (!observed.has(s)) {
          observed.add(s);
          observer.observe(s);
        }
      });
    };

    observeItems();
    document.addEventListener('pagination-loaded', function() {
      requestAnimationFrame(observeItems);
    });
    var mutObs = new MutationObserver(observeItems);
    var timeline = document.querySelector('[data-pagination="true"]');
    if (timeline) mutObs.observe(timeline, { childList: true, subtree: true });
  }
})();
|}

let tag_cloud_filter_js = {|
// Tag cloud filter for notes list
(function() {
  var buttons = document.querySelectorAll('.tag-cloud-btn');
  if (!buttons.length) return;

  var activeTags = new Set();

  function applyFilter() {
    var items = document.querySelectorAll('.note-item');
    if (activeTags.size === 0) {
      items.forEach(function(item) { item.style.display = ''; });
      document.querySelectorAll('[data-month-id]').forEach(function(s) { s.style.display = ''; });
      document.querySelectorAll('[data-year-id]').forEach(function(s) { s.style.display = ''; });
      return;
    }
    items.forEach(function(item) {
      var itemTags = (item.dataset.tags || '').split(',').filter(Boolean);
      var match = false;
      itemTags.forEach(function(t) {
        if (activeTags.has(t)) match = true;
      });
      item.style.display = match ? '' : 'none';
    });
    // Hide sections with no visible items
    document.querySelectorAll('[data-month-id]').forEach(function(section) {
      var visible = section.querySelectorAll('.note-item:not([style*="display: none"])');
      section.style.display = visible.length ? '' : 'none';
    });
    document.querySelectorAll('[data-year-id]').forEach(function(section) {
      var visible = section.querySelectorAll('.note-item:not([style*="display: none"])');
      section.style.display = visible.length ? '' : 'none';
    });
  }

  buttons.forEach(function(btn) {
    btn.addEventListener('click', function() {
      var tag = btn.dataset.tag;
      if (activeTags.has(tag)) {
        activeTags.delete(tag);
        btn.classList.remove('active');
      } else {
        activeTags.add(tag);
        btn.classList.add('active');
      }
      applyFilter();
    });
  });
})();
|}

let idea_filter_js = {|
// Ideas index filter. Every idea carries data-idea-item and data-level,
// whether it is an open card or a folded past line, so one pass filters
// both. Levels are OR against each other and AND against the keywords. The
// keyword text of an item includes the name of the project it sits under,
// which is why searching for a project name finds its ideas even though the
// name is only on the group head, and it includes the folded panel of a past
// idea, so a keyword reaches text the reader cannot see yet.
(function() {
  var root = document.querySelector('[data-idea-index]');
  if (!root) return;
  var box = document.getElementById('idea-search');
  var count = document.getElementById('idea-count');
  var empty = document.getElementById('idea-empty');
  var clear = document.getElementById('idea-clear');
  var totalOpen = parseInt(root.getAttribute('data-idea-open'), 10) || 0;

  var items = [];
  root.querySelectorAll('[data-idea-item]').forEach(function(el) {
    var group = el.closest('[data-idea-group]');
    items.push({
      el: el,
      past: el.tagName === 'DETAILS',
      level: el.getAttribute('data-level') || '',
      text: (el.textContent + ' ' +
             (group ? group.getAttribute('data-idea-group') : '')).toLowerCase()
    });
  });
  if (!items.length) return;

  var levels = new Set(), words = [];

  function matches(it) {
    if (levels.size && !levels.has(it.level)) return false;
    for (var i = 0; i < words.length; i++) {
      if (it.text.indexOf(words[i]) === -1) return false;
    }
    return true;
  }

  function syncLevels() {
    root.querySelectorAll('.idea-level').forEach(function(el) {
      el.classList.toggle('active', levels.has(el.getAttribute('data-level')));
    });
  }

  function syncExpand(group) {
    var btn = group.querySelector('[data-expand-all]');
    if (!btn) return;
    var shut = false;
    group.querySelectorAll('details.idea-past-card').forEach(function(d) {
      if (d.style.display !== 'none' && !d.open) shut = true;
    });
    btn.setAttribute('aria-expanded', shut ? 'false' : 'true');
  }

  function apply() {
    var nOpen = 0, nPast = 0;
    var searching = words.length > 0;
    items.forEach(function(it) {
      var ok = matches(it);
      it.el.style.display = ok ? '' : 'none';
      // A keyword can only match a past idea through text inside its folded
      // panel, so a hit has to open the panel or it looks like a false one.
      if (it.past) it.el.open = ok && searching;
      if (ok) { if (it.past) nPast++; else nOpen++; }
    });
    root.querySelectorAll('[data-idea-group]').forEach(function(g) {
      var visible = false;
      g.querySelectorAll('[data-idea-item]').forEach(function(el) {
        if (el.style.display !== 'none') visible = true;
      });
      g.style.display = visible ? '' : 'none';
      syncExpand(g);
    });
    var filtering = searching || levels.size > 0;
    if (count) {
      count.textContent = filtering
        ? nOpen + ' open, ' + nPast + ' previously offered'
        : totalOpen + ' open for takers';
    }
    if (clear) clear.hidden = !filtering;
    if (empty) empty.hidden = !(filtering && nOpen === 0 && nPast === 0);
  }

  root.addEventListener('click', function(e) {
    var lv = e.target.closest('.idea-level');
    if (lv) {
      e.preventDefault();
      var v = lv.getAttribute('data-level');
      if (levels.has(v)) levels.delete(v); else levels.add(v);
      syncLevels();
      apply();
      return;
    }
    var ex = e.target.closest('[data-expand-all]');
    if (ex) {
      e.preventDefault();
      var group = ex.closest('[data-idea-group]');
      var open = ex.getAttribute('aria-expanded') !== 'true';
      group.querySelectorAll('details.idea-past-card').forEach(function(d) {
        if (d.style.display !== 'none') d.open = open;
      });
      ex.setAttribute('aria-expanded', open ? 'true' : 'false');
    }
  });

  // The link on a folded line sits inside its <summary>, whose activation
  // behaviour would open the panel as well as follow the link. Stopping the
  // click here keeps it from reaching the summary at all.
  root.querySelectorAll('.idea-past-open').forEach(function(a) {
    a.addEventListener('click', function(e) { e.stopPropagation(); });
  });

  // A reader folding one line by hand leaves the group chevron claiming
  // everything is open, so the head follows what the lines actually do.
  root.addEventListener('toggle', function(e) {
    var d = e.target;
    if (!d.classList || !d.classList.contains('idea-past-card')) return;
    var group = d.closest('[data-idea-group]');
    if (group) syncExpand(group);
  }, true);

  if (box) {
    box.addEventListener('input', function() {
      words = box.value.toLowerCase().split(/\s+/).filter(Boolean);
      apply();
    });
  }

  if (clear) {
    clear.addEventListener('click', function() {
      levels.clear(); words = [];
      if (box) box.value = '';
      syncLevels();
      apply();
    });
  }

  apply();
})();
|}

let feed_dropdown_js = {|
(function() {
  var btn = document.getElementById('feed-dropdown-btn');
  var menu = document.getElementById('feed-dropdown');
  if (!btn || !menu) return;

  function positionMenu() {
    var r = btn.getBoundingClientRect();
    menu.style.top = (r.bottom + 4) + 'px';
    menu.style.left = Math.max(8, r.right - menu.offsetWidth) + 'px';
  }

  btn.addEventListener('click', function(e) {
    e.stopPropagation();
    var opening = !menu.classList.contains('open');
    menu.classList.toggle('open');
    if (opening) positionMenu();
  });

  document.addEventListener('click', function(e) {
    if (!menu.contains(e.target)) menu.classList.remove('open');
  });

  document.addEventListener('keydown', function(e) {
    if (e.key === 'Escape') menu.classList.remove('open');
  });
})();
|}

let mobile_menu_js = {|
(function() {
  var btn = document.getElementById('mobile-menu-btn');
  var menu = document.getElementById('mobile-menu');
  var close = document.getElementById('mobile-menu-close');
  var backdrop = menu && menu.querySelector('.mobile-menu-backdrop');
  if (!btn || !menu) return;

  function open() { menu.classList.add('open'); }
  function shut() { menu.classList.remove('open'); }

  btn.addEventListener('click', open);
  if (close) close.addEventListener('click', shut);
  if (backdrop) backdrop.addEventListener('click', shut);

  menu.querySelectorAll('.mobile-nav-link').forEach(function(a) {
    a.addEventListener('click', shut);
  });

  document.addEventListener('keydown', function(e) {
    if (e.key === 'Escape' && menu.classList.contains('open')) shut();
  });
})();
|}

let search_page_js = {|
// Search page: live results as you type, facets, keyboard selection
(function() {
  var input = document.getElementById('search-page-input');
  if (!input) return;
  var form = input.closest('form');
  var DEFAULTS = { limit: 20, link_limit: 12 };
  var limits = { limit: DEFAULTS.limit, link_limit: DEFAULTS.link_limit };
  var sort = new URLSearchParams(location.search).get('sort') === 'date'
    ? 'date' : 'relevance';
  var spinner = document.getElementById('search-spinner');
  var timer = null, sel = -1, controller = null, pending = 0;

  function results() { return document.getElementById('search-results'); }
  function hits() { return results().querySelectorAll('.sp-hit'); }

  function url(q) {
    return '/search?q=' + encodeURIComponent(q)
      + '&limit=' + limits.limit + '&link_limit=' + limits.link_limit
      + (sort === 'date' ? '&sort=date' : '');
  }

  // The spinner shows only while a request is in flight. Overlapping
  // fetches keep it up until the last one settles.
  function busy(on) {
    if (!spinner) return;
    pending += on ? 1 : -1;
    spinner.classList.toggle('busy', pending > 0);
  }

  function load(q) {
    if (controller) controller.abort();
    controller = new AbortController();
    busy(true);
    fetch(url(q) + '&fragment=1', { signal: controller.signal })
      .then(function(r) { return r.text(); })
      .then(function(html) {
        var box = results();
        box.outerHTML = html;
        sel = -1;
        var page = q ? '/search?q=' + encodeURIComponent(q)
          + (sort === 'date' ? '&sort=date' : '') : '/search';
        history.replaceState(null, '', page);
      })
      .catch(function() {})
      .finally(function() { busy(false); });
  }

  function search() {
    limits = { limit: DEFAULTS.limit, link_limit: DEFAULTS.link_limit };
    load(input.value.trim());
  }

  input.addEventListener('input', function() {
    clearTimeout(timer);
    timer = setTimeout(search, 120);
  });
  if (form) form.addEventListener('submit', function(e) { e.preventDefault(); search(); });

  function select(i) {
    var hs = hits();
    if (!hs.length) return;
    if (sel >= 0 && sel < hs.length) hs[sel].classList.remove('selected');
    sel = (i + hs.length) % hs.length;
    hs[sel].classList.add('selected');
    hs[sel].scrollIntoView({ block: 'nearest' });
  }

  input.addEventListener('keydown', function(e) {
    if (e.key === 'ArrowDown') { e.preventDefault(); select(sel + 1); }
    else if (e.key === 'ArrowUp') { e.preventDefault(); select(sel - 1); }
    else if (e.key === 'Enter' && sel >= 0) {
      e.preventDefault();
      var h = hits()[sel];
      if (h) {
        var to = h.getAttribute('href') || h.getAttribute('data-href');
        if (to) window.location.href = to;
      }
    }
    else if (e.key === 'Escape') { input.value = ''; search(); }
  });

  function toggleWord(word) {
    var words = input.value.trim().split(/\s+/).filter(Boolean);
    var i = words.indexOf(word);
    if (i >= 0) words.splice(i, 1); else words.push(word);
    input.value = words.join(' ');
    search();
    input.focus();
  }

  document.addEventListener('click', function(e) {
    var s = e.target.closest('[data-sort]');
    if (s) {
      e.preventDefault();
      sort = s.getAttribute('data-sort') === 'date' ? 'date' : 'relevance';
      load(input.value.trim());
      return;
    }
    // A link row is a div holding two anchors; a click on its background
    // follows the row's own destination.
    var row = e.target.closest('.sp-hit[data-href]');
    if (row && !e.target.closest('a')) {
      window.location.href = row.getAttribute('data-href');
      return;
    }
    var more = e.target.closest('[data-more]');
    if (more) {
      var p = more.getAttribute('data-more');
      limits[p] = Math.min(100, limits[p] * 2);
      load(input.value.trim());
      return;
    }
    var k = e.target.closest('[data-kind]');
    if (k) {
      e.preventDefault();
      toggleWord('kind:' + k.getAttribute('data-kind'));
      return;
    }
    // A row tag sits inside the row's own anchor, so stop the row
    // navigation and narrow the query instead.
    var t = e.target.closest('[data-tag]');
    if (t) { e.preventDefault(); toggleWord('#' + t.getAttribute('data-tag')); }
  });
})();
|}

(** [by_name] maps the file names served under [/js/] to their sources.
    [site.js] bundles the scripts every page includes. The bundle must
    execute after the highlight.js CDN script, which [hljs_init] uses. *)
let by_name = [
  "site.js", String.concat "\n" [
    sidenotes_js; search_shortcut_js; hljs_init; theme_toggle_js;
    feed_dropdown_js; mobile_menu_js ];
  "toc.js", toc_js;
  "pagination.js", pagination_js;
  "lightbox.js", lightbox_js;
  "links-modal.js", links_modal_js;
  "filter.js", checkbox_filter_js;
  "calendar.js", calendar_js;
  "tag-filter.js", tag_cloud_filter_js;
  "idea-filter.js", idea_filter_js;
  "search.js", search_page_js;
]
