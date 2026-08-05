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
    const ref = document.querySelector('.sidenote-ref[data-sidenote="' + id + '"]');
    if (!ref) return;
    // Find thumbnail src stored as data attr (shown on hover)
    const thumbSrc = sidenote.dataset.thumb || '';

    function activate(e) {
      sidenote.classList.add('!border-accent', '!text-text');
      ref.classList.add('highlighted');
      if (thumbSrc) showThumbOverlay(thumbSrc, e);
    }
    function deactivate() {
      sidenote.classList.remove('!border-accent', '!text-text');
      ref.classList.remove('highlighted');
      hideThumbOverlay();
    }

    sidenote.addEventListener('mouseenter', activate);
    sidenote.addEventListener('mousemove', moveThumbOverlay);
    sidenote.addEventListener('mouseleave', deactivate);

    // Inline refs highlight sidenote but do NOT show thumbnail
    function activateNoThumb() {
      sidenote.classList.add('!border-accent', '!text-text');
      ref.classList.add('highlighted');
    }
    ref.addEventListener('mouseenter', activateNoThumb);
    ref.addEventListener('mouseleave', deactivate);

    const toggle = document.querySelector('.sidenote-toggle[data-sidenote="' + id + '"]');
    if (toggle) {
      toggle.addEventListener('mouseenter', activateNoThumb);
      toggle.addEventListener('mouseleave', deactivate);
    }
  });
}

// Setup sidenote numbers and mobile toggles.
// Deduplicates: only the first ref for a given sidenote gets a number.
// Subsequent refs for the same slug reuse the same number.
function setupSidenoteNumbers() {
  let noteNumber = 1;
  const seen = {};  // slug -> { number, inlineNote }
  document.querySelectorAll('.sidenote-ref').forEach(ref => {
    const id = ref.dataset.sidenote;
    const sidenote = document.getElementById('sidenote-' + id);
    if (!sidenote) return;

    // Determine the number: first occurrence gets a new one, duplicates reuse
    let currentNumber;
    let inlineNote;
    const isFirst = !seen[id];
    if (isFirst) {
      currentNumber = noteNumber++;
      // Add number prefix to sidebar sidenote (only once)
      const numberSpan = document.createElement('span');
      numberSpan.className = 'sidenote-number font-semibold';
      numberSpan.textContent = currentNumber + '. ';
      sidenote.insertBefore(numberSpan, sidenote.firstChild);
      // Create mobile inline note (only once per sidenote)
      inlineNote = document.createElement('div');
      inlineNote.className = 'hidden lg:!hidden sidenote-inline text-sm leading-relaxed text-text bg-surface border-l-2 border-accent px-3 py-2 my-2 rounded-r';
      inlineNote.id = 'sidenote-inline-' + id;
      inlineNote.innerHTML = '<span class="sidenote-number font-semibold">' + currentNumber + '.</span> ' + sidenote.innerHTML.replace(/<span class="sidenote-number.*?<\/span>/, '');
      const paragraph = ref.closest('p, blockquote, li');
      if (paragraph && !document.getElementById('sidenote-inline-' + id)) {
        paragraph.insertAdjacentElement('afterend', inlineNote);
      }
      seen[id] = { number: currentNumber, inlineNote: inlineNote };
    } else {
      currentNumber = seen[id].number;
      inlineNote = seen[id].inlineNote;
    }

    // Add toggle badge to every ref (shows the same number)
    const toggle = document.createElement('span');
    toggle.className = 'sidenote-toggle';
    toggle.textContent = currentNumber;
    toggle.dataset.sidenote = id;
    const anchor = ref.closest('.sidenote-anchor');
    if (anchor) anchor.appendChild(toggle);
    toggle.addEventListener('click', (e) => {
      e.stopPropagation();
      const wasActive = toggle.classList.contains('!bg-accent');
      document.querySelectorAll('.sidenote-toggle').forEach(t => t.classList.remove('!bg-accent', '!border-accent', '!text-white'));
      document.querySelectorAll('.sidenote-inline').forEach(n => n.classList.add('hidden'));
      if (!wasActive) {
        toggle.classList.add('!bg-accent', '!border-accent', '!text-white');
        inlineNote.classList.remove('hidden');
      }
    });
  });
  document.addEventListener('click', (e) => {
    if (!e.target.closest('.sidenote-toggle') && !e.target.closest('.sidenote-inline')) {
      document.querySelectorAll('.sidenote-toggle').forEach(t => t.classList.remove('!bg-accent', '!border-accent', '!text-white'));
      document.querySelectorAll('.sidenote-inline').forEach(n => n.classList.add('hidden'));
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
window.addEventListener('resize', positionSidenotes);
window.addEventListener('scroll', positionSidenotes, { passive: true });
|}

let toc_js = {|
// Table of Contents functionality (desktop only)
function setupTOC() {
  if (window.innerWidth < 1024) return;
  const tocRow = document.getElementById('toc-row');
  const navNotes = document.getElementById('nav-notes');
  const tocLinks = document.querySelectorAll('.toc-link');
  if (!tocRow) return;

  const sectionIds = [];
  tocLinks.forEach(link => { sectionIds.push(link.getAttribute('href').slice(1)); });
  const sectionElements = sectionIds.map(id => document.getElementById(id)).filter(Boolean);

  let tocVisible = false;
  const showThreshold = 200;

  function updateTOC() {
    const scrollY = window.scrollY;
    const header = document.getElementById('header');
    const headerHeight = header ? header.offsetHeight : 0;

    if (scrollY > showThreshold && !tocVisible) {
      tocVisible = true;
      tocRow.classList.add('visible');
      if (navNotes) navNotes.classList.add('emphasized');
    } else if (scrollY <= showThreshold && tocVisible) {
      tocVisible = false;
      tocRow.classList.remove('visible');
      if (navNotes) navNotes.classList.remove('emphasized');
    }

    const atBottom = (scrollY + window.innerHeight) >= document.documentElement.scrollHeight - 5;
    let currentIndex = 0;
    let progressInSection = 0;
    if (atBottom) {
      currentIndex = sectionElements.length - 1;
      progressInSection = 100;
    } else {
      for (let i = sectionElements.length - 1; i >= 0; i--) {
        const el = sectionElements[i];
        if (el && el.getBoundingClientRect().top <= headerHeight + 50) {
          currentIndex = i;
          const sectionTop = el.getBoundingClientRect().top + scrollY - headerHeight;
          const nextEl = sectionElements[i + 1];
          const sectionBottom = nextEl
            ? nextEl.getBoundingClientRect().top + scrollY - headerHeight
            : document.documentElement.scrollHeight;
          const sectionHeight = sectionBottom - sectionTop;
          const scrollInSection = scrollY - sectionTop + headerHeight;
          progressInSection = Math.min(Math.max((scrollInSection / sectionHeight) * 100, 0), 100);
          break;
        }
      }
    }

    let activeLink = null;
    tocLinks.forEach((link, index) => {
      const linkIndex = parseInt(link.dataset.index);
      link.classList.remove('passed', 'active');
      if (linkIndex < currentIndex) {
        link.classList.add('passed');
        link.style.setProperty('--progress', '100%');
      } else if (linkIndex === currentIndex) {
        link.classList.add('active');
        link.style.setProperty('--progress', progressInSection + '%');
        activeLink = link;
      } else {
        link.style.setProperty('--progress', '0%');
      }
    });

    // Scroll active TOC link into view horizontally
    if (activeLink && tocRow.scrollWidth > tocRow.clientWidth) {
      const linkLeft = activeLink.offsetLeft;
      const linkWidth = activeLink.offsetWidth;
      const rowWidth = tocRow.clientWidth;
      const scrollTarget = linkLeft - (rowWidth / 2) + (linkWidth / 2);
      tocRow.scrollTo({ left: scrollTarget, behavior: 'smooth' });
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
  updateTOC();
}

window.addEventListener('load', () => { setTimeout(setupTOC, 150); });
|}

let search_js = {|
// Search — live FTS5 search with debounce, multi-kind filter, keyboard nav
(function() {
  var toggleBtn = document.getElementById('search-toggle-btn');
  var overlay = document.getElementById('search-modal-overlay');
  if (!toggleBtn || !overlay) return;

  var input = document.getElementById('search-input');
  var resultsEl = document.getElementById('search-results');
  var countEl = document.getElementById('search-count');
  var pills = overlay.querySelectorAll('.search-filter-pill');

  var activeKinds = new Set();
  var results = [];
  var selectedIndex = -1;
  var debounceTimer = null;

  // SVG icon paths per kind (Tabler Icons, 14px)
  var kindIcons = {
    paper: '<path d="M14 3v4a1 1 0 0 0 1 1h4"/><path d="M17 21h-10a2 2 0 0 1 -2 -2v-14a2 2 0 0 1 2 -2h7l5 5v11a2 2 0 0 1 -2 2z"/><path d="M9 9l1 0"/><path d="M9 13l6 0"/><path d="M9 17l6 0"/>',
    note: '<path d="M13 20l7 -7"/><path d="M13 20v-6a1 1 0 0 1 1 -1h6v-7a2 2 0 0 0 -2 -2h-12a2 2 0 0 0 -2 2v12a2 2 0 0 0 2 2h7"/>',
    project: '<path d="M5 4h4l3 3h7a2 2 0 0 1 2 2v8a2 2 0 0 1 -2 2h-14a2 2 0 0 1 -2 -2v-11a2 2 0 0 1 2 -2"/>',
    idea: '<path d="M3 12h1m8 -9v1m8 8h1m-15.4 -6.4l.7 .7m12.1 -.7l-.7 .7"/><path d="M9 16a5 5 0 1 1 6 0a3.5 3.5 0 0 0 -1 3a2 2 0 0 1 -4 0a3.5 3.5 0 0 0 -1 -3"/><path d="M9.7 17l4.6 0"/>',
    video: '<path d="M15 10l4.553 -2.276a1 1 0 0 1 1.447 .894v6.764a1 1 0 0 1 -1.447 .894l-4.553 -2.276v-4z"/><path d="M3 6m0 2a2 2 0 0 1 2 -2h8a2 2 0 0 1 2 2v8a2 2 0 0 1 -2 2h-8a2 2 0 0 1 -2 -2z"/>',
    link: '<path d="M9 15l6 -6"/><path d="M11 6l.463 -.536a5 5 0 0 1 7.071 7.072l-.534 .464"/><path d="M13 18l-.397 .534a5.068 5.068 0 0 1 -7.127 0a4.972 4.972 0 0 1 0 -7.071l.524 -.463"/>'
  };

  function kindSvg(kind, size) {
    var s = size || 14;
    var paths = kindIcons[kind] || kindIcons.link;
    return '<svg class="inline-block shrink-0 sr-icon" width="' + s + '" height="' + s + '" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">' + paths + '</svg>';
  }

  var emptyStateHtml = resultsEl.innerHTML;

  function openSearch() {
    overlay.classList.add('active');
    if (input) { input.value = ''; input.focus(); }
    results = [];
    selectedIndex = -1;
    resultsEl.innerHTML = emptyStateHtml;
    if (countEl) countEl.textContent = '';
    document.body.style.overflow = 'hidden';
  }

  function closeSearch() {
    overlay.classList.remove('active');
    document.body.style.overflow = '';
  }

  function renderEmpty(msg) {
    resultsEl.innerHTML = '<div class="search-no-results">' + msg + '</div>';
  }

  function escapeHtml(s) {
    var d = document.createElement('div');
    d.appendChild(document.createTextNode(s));
    return d.innerHTML;
  }

  function urlDomain(url) {
    try { return url.replace(/^https?:\/\//, '').split('/')[0]; } catch(e) { return url; }
  }

  function renderResults() {
    if (!results.length) {
      renderEmpty('No results found');
      if (countEl) countEl.textContent = '0 results';
      return;
    }
    if (countEl) countEl.textContent = results.length + ' result' + (results.length !== 1 ? 's' : '');
    var html = '';
    for (var i = 0; i < results.length; i++) {
      var r = results[i];
      var sel = i === selectedIndex ? ' selected' : '';

      html += '<div class="search-result' + sel + '" data-index="' + i + '">';
      html += '<div class="sr-row">';

      // Thumbnail (square, cropped)
      if (r.thumbnail) {
        html += '<div class="sr-thumb"><img src="' + escapeHtml(r.thumbnail) + '" loading="lazy" alt=""></div>';
      }

      html += '<div class="sr-body">';
      // Main result link
      html += '<a href="' + escapeHtml(r.url) + '" class="sr-main">';
      html += '<span class="sr-icon-wrap sr-icon-' + r.kind + '">' + kindSvg(r.kind) + '</span>';
      html += '<span class="sr-title">' + escapeHtml(r.title) + '</span>';
      html += '<span class="sr-date">' + r.date + '</span>';
      html += '</a>';

      // Snippet line
      if (r.snippet) {
        html += '<div class="sr-snippet">' + r.snippet + '</div>';
      }

      // For links: show the URL domain as a clickable link + parent entries
      // Tags
      if (r.tags && r.tags.length > 0) {
        html += '<div class="sr-tags">';
        for (var t = 0; t < r.tags.length; t++) {
          html += '<a class="sr-tag" data-tag="' + escapeHtml(r.tags[t]) + '">';
          html += '#' + escapeHtml(r.tags[t]);
          html += '</a>';
        }
        html += '</div>';
      }

      if (r.kind === 'link') {
        html += '<div class="sr-links">';
        html += '<a href="' + escapeHtml(r.url) + '" class="sr-url" target="_blank" rel="noopener">';
        html += '<svg class="inline-block shrink-0" width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M12 6h-6a2 2 0 0 0 -2 2v10a2 2 0 0 0 2 2h10a2 2 0 0 0 2 -2v-6"/><path d="M11 13l9 -9"/><path d="M15 4h5v5"/></svg>';
        html += ' ' + escapeHtml(urlDomain(r.url));
        html += '</a>';
        if (r.parents && r.parents.length > 0) {
          for (var j = 0; j < r.parents.length; j++) {
            var p = r.parents[j];
            html += '<a href="' + escapeHtml(p.url) + '" class="sr-parent">';
            html += kindSvg(p.kind, 12);
            html += '<span>' + escapeHtml(p.title) + '</span>';
            html += '</a>';
          }
        }
        html += '</div>';
      } else if (r.parents && r.parents.length > 0) {
        html += '<div class="sr-links">';
        for (var j = 0; j < r.parents.length; j++) {
          var p = r.parents[j];
          html += '<a href="' + escapeHtml(p.url) + '" class="sr-parent">';
          html += kindSvg(p.kind, 12);
          html += '<span>' + escapeHtml(p.title) + '</span>';
          html += '</a>';
        }
        html += '</div>';
      }

      html += '</div>'; // sr-body
      html += '</div>'; // sr-row
      html += '</div>'; // search-result
    }
    resultsEl.innerHTML = html;
  }

  function scrollToSelected() {
    var el = resultsEl.querySelector('.search-result.selected');
    if (el) el.scrollIntoView({ block: 'nearest' });
  }

  function updateSelection(newIdx) {
    // Update DOM classes directly instead of full re-render
    var items = resultsEl.querySelectorAll('.search-result');
    if (selectedIndex >= 0 && selectedIndex < items.length) {
      items[selectedIndex].classList.remove('selected');
    }
    selectedIndex = newIdx;
    if (selectedIndex >= 0 && selectedIndex < items.length) {
      items[selectedIndex].classList.add('selected');
    }
    scrollToSelected();
  }

  function buildQuery() {
    var q = (input ? input.value.trim() : '');
    var parts = [];
    activeKinds.forEach(function(k) { parts.push('kind:' + k); });
    if (q) parts.push(q);
    return parts.join(' ');
  }

  function doSearch() {
    var query = buildQuery();
    if (!query) {
      results = [];
      selectedIndex = -1;
      resultsEl.innerHTML = emptyStateHtml;
      if (countEl) countEl.textContent = '';
      return;
    }
    fetch('/api/search?limit=25&q=' + encodeURIComponent(query))
      .then(function(r) { return r.json(); })
      .then(function(data) {
        results = data.results || [];
        selectedIndex = results.length > 0 ? 0 : -1;
        renderResults();
      })
      .catch(function() {
        results = [];
        selectedIndex = -1;
        renderEmpty('Search failed');
      });
  }

  function debouncedSearch() {
    if (debounceTimer) clearTimeout(debounceTimer);
    debounceTimer = setTimeout(doSearch, 150);
  }

  if (input) {
    input.addEventListener('input', debouncedSearch);
  }

  // Kind filter pills — toggle individually (multi-select)
  pills.forEach(function(pill) {
    pill.addEventListener('click', function() {
      var kind = pill.dataset.kind;
      if (activeKinds.has(kind)) {
        activeKinds.delete(kind);
        pill.classList.remove('active');
      } else {
        activeKinds.add(kind);
        pill.classList.add('active');
      }
      doSearch();
    });
  });

  // Click on result tag — search for that tag
  resultsEl.addEventListener('click', function(e) {
    var tagEl = e.target.closest('.sr-tag');
    if (tagEl) {
      e.preventDefault();
      e.stopPropagation();
      var tag = tagEl.dataset.tag;
      if (input) {
        input.value = '#' + tag;
        input.focus();
        doSearch();
      }
      return;
    }
    var parent = e.target.closest('.sr-parent');
    if (parent) {
      closeSearch();
      return;
    }
    var link = e.target.closest('.sr-main');
    if (link) {
      closeSearch();
      return;
    }
    // Click on result row navigates to main URL
    var row = e.target.closest('.search-result');
    if (row && !e.target.closest('a')) {
      var mainLink = row.querySelector('.sr-main');
      if (mainLink) {
        window.location.href = mainLink.href;
        closeSearch();
      }
    }
  });

  // Hover to select
  resultsEl.addEventListener('mousemove', function(e) {
    var el = e.target.closest('.search-result');
    if (el) {
      var idx = parseInt(el.dataset.index);
      if (!isNaN(idx) && idx !== selectedIndex) {
        updateSelection(idx);
      }
    }
  });

  toggleBtn.addEventListener('click', openSearch);
  overlay.addEventListener('click', function(e) {
    if (e.target === overlay) closeSearch();
  });

  document.addEventListener('keydown', function(e) {
    if ((e.metaKey || e.ctrlKey) && e.key === 'k') {
      e.preventDefault();
      if (overlay.classList.contains('active')) closeSearch();
      else openSearch();
      return;
    }
    if (!overlay.classList.contains('active')) return;
    if (e.key === 'Escape') { closeSearch(); return; }
    if (e.key === 'ArrowDown') {
      e.preventDefault();
      if (results.length > 0) {
        updateSelection((selectedIndex + 1) % results.length);
      }
      return;
    }
    if (e.key === 'ArrowUp') {
      e.preventDefault();
      if (results.length > 0) {
        updateSelection((selectedIndex - 1 + results.length) % results.length);
      }
      return;
    }
    if (e.key === 'Enter') {
      e.preventDefault();
      if (selectedIndex >= 0 && selectedIndex < results.length) {
        window.location.href = results[selectedIndex].url;
        closeSearch();
      }
      return;
    }
  });

  // Global: clicking any [data-tag] element opens search with that tag
  document.addEventListener('click', function(e) {
    var tagEl = e.target.closest('[data-tag]');
    if (!tagEl) return;
    // Don't intercept tag-cloud-btn clicks (those do local filtering)
    if (tagEl.classList.contains('tag-cloud-btn')) return;
    e.preventDefault();
    var tag = tagEl.dataset.tag;
    overlay.classList.add('active');
    if (input) {
      input.value = '#' + tag;
      input.focus();
      input.dispatchEvent(new Event('input'));
    }
    document.body.style.overflow = 'hidden';
  });

  // Global: clicking any [data-kind] element opens search with that kind filter
  document.addEventListener('click', function(e) {
    var kindEl = e.target.closest('[data-kind]');
    if (!kindEl) return;
    e.preventDefault();
    var kind = kindEl.dataset.kind;
    // Activate the matching kind filter pill
    pills.forEach(function(pill) {
      if (pill.dataset.kind === kind) {
        activeKinds.add(kind);
        pill.classList.add('active');
      }
    });
    overlay.classList.add('active');
    if (input) {
      input.value = '';
      input.focus();
    }
    document.body.style.overflow = 'hidden';
    doSearch();
  });

  // On load: check for #tag=foo or #kind=foo in URL hash
  (function() {
    var hash = location.hash;
    if (hash && hash.indexOf('#tag=') === 0) {
      var tag = decodeURIComponent(hash.slice(5));
      setTimeout(function() {
        overlay.classList.add('active');
        if (input) {
          input.value = '#' + tag;
          input.focus();
          input.dispatchEvent(new Event('input'));
        }
        document.body.style.overflow = 'hidden';
      }, 100);
    } else if (hash && hash.indexOf('#kind=') === 0) {
      var kind = decodeURIComponent(hash.slice(6));
      setTimeout(function() {
        pills.forEach(function(pill) {
          if (pill.dataset.kind === kind) {
            activeKinds.add(kind);
            pill.classList.add('active');
          }
        });
        overlay.classList.add('active');
        if (input) {
          input.value = '';
          input.focus();
        }
        document.body.style.overflow = 'hidden';
        doSearch();
      }, 100);
    }
  })();
})();
|}

let links_modal_js = {|
(function() {
  document.querySelectorAll('.sidebar-meta-expand[data-modal-target]').forEach(function(btn) {
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

let status_filter_js = {|
// Idea status filter checkboxes
(function() {
  const checkboxes = document.querySelectorAll('.status-checkbox');
  if (!checkboxes.length) return;

  function applyFilter() {
    checkboxes.forEach(cb => {
      const status = cb.dataset.status;
      const items = document.querySelectorAll('.idea-item[data-status="' + status + '"]');
      items.forEach(item => {
        item.style.display = cb.checked ? '' : 'none';
      });
    });
    // Hide year sections with no visible items
    document.querySelectorAll('[data-year-id]').forEach(function(section) {
      var visible = section.querySelectorAll('.idea-item:not([style*="display: none"])');
      section.style.display = visible.length ? '' : 'none';
    });
  }

  checkboxes.forEach(cb => {
    cb.addEventListener('change', applyFilter);
  });

  // Apply initial filter state on page load (hides unchecked like Expired)
  applyFilter();
})();
|}

let classification_filter_js = {|
// Paper classification filter checkboxes
(function() {
  const checkboxes = document.querySelectorAll('.classification-checkbox');
  if (!checkboxes.length) return;

  checkboxes.forEach(cb => {
    cb.addEventListener('change', () => {
      const cls = cb.dataset.classification;
      const items = document.querySelectorAll('.paper-item[data-classification="' + cls + '"]');
      items.forEach(item => {
        item.style.display = cb.checked ? '' : 'none';
      });
      // Hide year sections with no visible papers
      document.querySelectorAll('[data-year-id]').forEach(function(section) {
        var visible = section.querySelectorAll('.paper-item:not([style*="display: none"])');
        section.style.display = visible.length ? '' : 'none';
      });
    });
  });
})();
|}

let papers_calendar_js = {|
// Papers calendar — year heatmap + month grid, syncs with scroll
(function() {
  var container = document.getElementById('papers-calendar');
  if (!container) return;

  var data;
  try { data = JSON.parse(container.dataset.calendarYears || '{}'); } catch(e) { return; }
  var currentYear = container.dataset.currentYear || '';
  var allYears = Object.keys(data).sort().reverse();
  if (!allYears.length) return;
  if (!currentYear) currentYear = allYears[0];

  var heatmapEl = container.querySelector('.heatmap-strip');
  var headerEl = container.querySelector('.cal-header');
  var gridEl = container.querySelector('.cal-grid');

  var shortMonths = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'];

  var now = new Date();
  var todayYear = String(now.getFullYear());

  function countForYear(y) {
    return data[y] ? data[y].length : 0;
  }

  // Build a 10-year window: 4 before + current + 5 after
  function getHeatmapWindow() {
    var cy = parseInt(currentYear);
    var win = [];
    for (var i = -4; i <= 5; i++) {
      win.push(String(cy + i));
    }
    return win;
  }

  function renderHeatmap() {
    heatmapEl.innerHTML = '';
    var windowYears = getHeatmapWindow();
    var maxCount = 1;
    windowYears.forEach(function(y) {
      var c = countForYear(y);
      if (c > maxCount) maxCount = c;
    });

    var strip = document.createElement('div');
    strip.className = 'heatmap-grid';
    strip.style.gridTemplateColumns = 'repeat(' + windowYears.length + ', 1fr)';

    windowYears.forEach(function(y) {
      var count = countForYear(y);
      var isFuture = y > todayYear;
      var cell = document.createElement('div');
      cell.className = 'heatmap-cell';
      if (y === currentYear) cell.classList.add('heatmap-current');

      if (isFuture) {
        cell.dataset.state = 'future';
        cell.dataset.level = 0;
        cell.title = y + ': upcoming';
      } else if (count === 0) {
        cell.dataset.state = 'empty';
        cell.dataset.level = 0;
        cell.title = y + ': no papers';
      } else {
        cell.dataset.state = 'active';
        var level = Math.min(4, Math.ceil(count / maxCount * 4));
        cell.dataset.level = level;
        cell.title = y + ': ' + count + ' paper' + (count !== 1 ? 's' : '');
      }

      if (!isFuture) {
        (function(targetY) {
          cell.addEventListener('click', function() {
            currentYear = targetY;
            renderMonth(currentYear);
            renderHeatmap();
            var section = document.getElementById('year-' + targetY);
            if (section) section.scrollIntoView({ behavior: 'smooth', block: 'start' });
          });
        })(y);
      }

      var label = document.createElement('span');
      label.className = 'heatmap-label';
      label.textContent = "'" + y.slice(-2);

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

  function renderMonth(y) {
    var months = data[y] || [];
    var monthSet = new Set(months);

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
    title.textContent = y;
    headerEl.appendChild(prevBtn);
    headerEl.appendChild(title);
    headerEl.appendChild(nextBtn);

    gridEl.innerHTML = '';
    gridEl.style.gridTemplateColumns = 'repeat(4, 1fr)';
    for (var m = 0; m < 12; m++) {
      var cell = document.createElement('span');
      if (monthSet.has(m + 1)) {
        cell.className = 'cal-day cal-day-active';
      } else {
        cell.className = 'cal-day cal-day-empty';
      }
      cell.textContent = shortMonths[m];
      gridEl.appendChild(cell);
    }
  }

  function navigate(dir) {
    var idx = allYears.indexOf(currentYear);
    var next = idx - dir;
    if (next >= 0 && next < allYears.length) {
      currentYear = allYears[next];
      renderMonth(currentYear);
      renderHeatmap();
    }
  }

  renderHeatmap();
  renderMonth(currentYear);

  // Scroll tracking — find the last section whose top has scrolled past the header
  var sections = document.querySelectorAll('[data-year-id]');
  if (sections.length) {
    function updateCurrentYear() {
      var best = null;
      sections.forEach(function(s) {
        var rect = s.getBoundingClientRect();
        if (rect.top <= 120) best = s;
      });
      if (best) {
        var yearId = best.dataset.yearId;
        if (yearId && yearId !== currentYear) {
          currentYear = yearId;
          renderMonth(currentYear);
          renderHeatmap();
        }
      }
    }
    window.addEventListener('scroll', updateCurrentYear, { passive: true });
    updateCurrentYear();
  }
})();
|}


let notes_calendar_js = {|
// Notes calendar — year heatmap + month grid, syncs with scroll
(function() {
  var container = document.getElementById('notes-calendar');
  if (!container) return;

  var data;
  try { data = JSON.parse(container.dataset.calendarMonths || '{}'); } catch(e) { return; }
  var currentMonth = container.dataset.currentMonth || '';
  var allMonths = Object.keys(data).sort().reverse();
  if (!allMonths.length) return;
  if (!currentMonth) currentMonth = allMonths[0];

  var heatmapEl = container.querySelector('.heatmap-strip');
  var headerEl = container.querySelector('.cal-header');
  var gridEl = container.querySelector('.cal-grid');

  var shortMonths = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'];

  // Determine today's YYYY-MM for future detection
  var now = new Date();
  var todayYM = now.getFullYear() + '-' + String(now.getMonth() + 1).padStart(2, '0');

  // Compute a 12-month YYYY-MM key centered on currentMonth
  function ymAdd(ym, offset) {
    var parts = ym.split('-');
    var y = parseInt(parts[0]);
    var m = parseInt(parts[1]) - 1 + offset;
    var ny = y + Math.floor(m / 12);
    var nm = ((m % 12) + 12) % 12;
    return ny + '-' + String(nm + 1).padStart(2, '0');
  }

  // Count posts for a YYYY-MM key from the full data object
  function countForMonth(ym) {
    return data[ym] ? data[ym].length : 0;
  }

  // Build the 12-month window: 5 months before currentMonth, currentMonth, 6 months after
  function getHeatmapWindow() {
    var window = [];
    for (var i = -5; i <= 6; i++) {
      window.push(ymAdd(currentMonth, i));
    }
    return window;
  }

  // Render year heatmap — 12 months around currentMonth
  function renderHeatmap() {
    heatmapEl.innerHTML = '';
    var windowMonths = getHeatmapWindow();
    var maxCount = 1;
    windowMonths.forEach(function(ym) {
      var c = countForMonth(ym);
      if (c > maxCount) maxCount = c;
    });

    var strip = document.createElement('div');
    strip.className = 'heatmap-grid';
    windowMonths.forEach(function(ym) {
      var count = countForMonth(ym);
      var parts = ym.split('-');
      var month = parseInt(parts[1]);
      var isFuture = ym > todayYM;
      var cell = document.createElement('div');
      cell.className = 'heatmap-cell';
      if (ym === currentMonth) cell.classList.add('heatmap-current');

      if (isFuture) {
        cell.dataset.state = 'future';
        cell.dataset.level = 0;
        cell.title = shortMonths[month - 1] + ': upcoming';
      } else if (count === 0) {
        cell.dataset.state = 'empty';
        cell.dataset.level = 0;
        cell.title = shortMonths[month - 1] + ': no notes';
      } else {
        cell.dataset.state = 'active';
        var level = Math.min(4, Math.ceil(count / maxCount * 4));
        cell.dataset.level = level;
        cell.title = shortMonths[month - 1] + ': ' + count + ' note' + (count !== 1 ? 's' : '');
      }

      if (!isFuture) {
        (function(targetYm) {
          cell.addEventListener('click', function() {
            currentMonth = targetYm;
            renderMonth(currentMonth);
            renderHeatmap();
            var section = document.getElementById('month-' + targetYm);
            if (section) section.scrollIntoView({ behavior: 'smooth', block: 'start' });
          });
        })(ym);
      }

      var label = document.createElement('span');
      label.className = 'heatmap-label';
      label.textContent = shortMonths[month - 1];

      // Single circle: colored background (heatmap) with count or icon inside
      var circle = document.createElement('div');
      circle.className = 'heatmap-circle';
      if (isFuture) {
        circle.innerHTML = '<svg xmlns="http://www.w3.org/2000/svg" width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><path d="M3 12a9 9 0 1 0 18 0a9 9 0 0 0 -18 0"/><path d="M12 7v5l3 3"/></svg>';
      } else if (count === 0) {
        circle.innerHTML = '<svg xmlns="http://www.w3.org/2000/svg" width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="3" stroke-linecap="round"><path d="M5 12h14"/></svg>';
      } else {
        circle.textContent = count;
      }

      cell.appendChild(label);
      cell.appendChild(circle);
      strip.appendChild(cell);
    });
    heatmapEl.appendChild(strip);
  }

  function daysInMonth(y, m) {
    return new Date(y, m, 0).getDate();
  }

  function firstDayOfWeek(y, m) {
    var d = new Date(y, m - 1, 1).getDay();
    return d === 0 ? 6 : d - 1;
  }

  function renderMonth(ym) {
    var parts = ym.split('-');
    var year = parseInt(parts[0]);
    var month = parseInt(parts[1]);
    var days = data[ym] || [];
    var daySet = new Set(days);
    var total = daysInMonth(year, month);
    var offset = firstDayOfWeek(year, month);

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
    title.textContent = shortMonths[month - 1] + ' ' + year;
    headerEl.appendChild(prevBtn);
    headerEl.appendChild(title);
    headerEl.appendChild(nextBtn);

    gridEl.innerHTML = '';
    var weekdays = ['Mo','Tu','We','Th','Fr','Sa','Su'];
    weekdays.forEach(function(wd) {
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
        cell.textContent = d;
        (function(day) {
          cell.addEventListener('click', function() {
            var id = 'note-' + ym + '-' + String(day).padStart(2, '0');
            var el = document.getElementById(id);
            if (el) el.scrollIntoView({ behavior: 'smooth', block: 'center' });
          });
        })(d);
      } else {
        cell.className = 'cal-day cal-day-empty';
        cell.textContent = d;
      }
      gridEl.appendChild(cell);
    }

    // Pad to 6 rows (42 cells) to keep constant height
    var totalCells = offset + total;
    while (totalCells < 42) {
      var pad = document.createElement('span');
      pad.className = 'cal-day cal-day-pad';
      gridEl.appendChild(pad);
      totalCells++;
    }
  }

  function navigate(dir) {
    var idx = allMonths.indexOf(currentMonth);
    var next = idx - dir;
    if (next >= 0 && next < allMonths.length) {
      currentMonth = allMonths[next];
      renderMonth(currentMonth);
      renderHeatmap();
    }
  }

  renderHeatmap();
  renderMonth(currentMonth);

  // Scroll tracking
  var sections = document.querySelectorAll('[data-month-id]');
  if (sections.length && 'IntersectionObserver' in window) {
    var observer = new IntersectionObserver(function(entries) {
      entries.forEach(function(entry) {
        if (entry.isIntersecting) {
          var monthId = entry.target.dataset.monthId;
          if (monthId && monthId !== currentMonth) {
            currentMonth = monthId;
            renderMonth(currentMonth);
            renderHeatmap();
          }
        }
      });
    }, { rootMargin: '-80px 0px -60% 0px' });
    sections.forEach(function(s) { observer.observe(s); });
  }
})();
|}

let links_calendar_js = {|
// Links calendar — year heatmap + month grid, syncs with scroll
(function() {
  var container = document.getElementById('links-calendar');
  if (!container) return;

  var data;
  try { data = JSON.parse(container.dataset.calendarMonths || '{}'); } catch(e) { return; }
  var currentMonth = container.dataset.currentMonth || '';
  var currentDay = 0;
  var allMonths = Object.keys(data).sort().reverse();
  if (!allMonths.length) return;
  if (!currentMonth) currentMonth = allMonths[0];

  var heatmapEl = container.querySelector('.heatmap-strip');
  var headerEl = container.querySelector('.cal-header');
  var gridEl = container.querySelector('.cal-grid');

  var shortMonths = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'];

  var now = new Date();
  var todayYM = now.getFullYear() + '-' + String(now.getMonth() + 1).padStart(2, '0');

  function ymAdd(ym, offset) {
    var parts = ym.split('-');
    var y = parseInt(parts[0]);
    var m = parseInt(parts[1]) - 1 + offset;
    var ny = y + Math.floor(m / 12);
    var nm = ((m % 12) + 12) % 12;
    return ny + '-' + String(nm + 1).padStart(2, '0');
  }

  function countForMonth(ym) {
    return data[ym] ? data[ym].length : 0;
  }

  function getHeatmapWindow() {
    var window = [];
    for (var i = -5; i <= 6; i++) {
      window.push(ymAdd(currentMonth, i));
    }
    return window;
  }

  function renderHeatmap() {
    heatmapEl.innerHTML = '';
    var windowMonths = getHeatmapWindow();
    var maxCount = 1;
    windowMonths.forEach(function(ym) {
      var c = countForMonth(ym);
      if (c > maxCount) maxCount = c;
    });

    var strip = document.createElement('div');
    strip.className = 'heatmap-grid';
    windowMonths.forEach(function(ym) {
      var count = countForMonth(ym);
      var parts = ym.split('-');
      var month = parseInt(parts[1]);
      var isFuture = ym > todayYM;
      var cell = document.createElement('div');
      cell.className = 'heatmap-cell';
      if (ym === currentMonth) cell.classList.add('heatmap-current');

      if (isFuture) {
        cell.dataset.state = 'future';
        cell.dataset.level = 0;
        cell.title = shortMonths[month - 1] + ': upcoming';
      } else if (count === 0) {
        cell.dataset.state = 'empty';
        cell.dataset.level = 0;
        cell.title = shortMonths[month - 1] + ': no links';
      } else {
        cell.dataset.state = 'active';
        var level = Math.min(4, Math.ceil(count / maxCount * 4));
        cell.dataset.level = level;
        cell.title = shortMonths[month - 1] + ': ' + count + ' link' + (count !== 1 ? 's' : '');
      }

      if (!isFuture) {
        (function(targetYm) {
          cell.addEventListener('click', function() {
            currentMonth = targetYm;
            renderMonth(currentMonth);
            renderHeatmap();
            // Find first link-group with this month-id
            var group = document.querySelector('.link-group[data-month-id="' + targetYm + '"]');
            if (group) group.scrollIntoView({ behavior: 'smooth', block: 'start' });
          });
        })(ym);
      }

      var label = document.createElement('span');
      label.className = 'heatmap-label';
      label.textContent = shortMonths[month - 1];

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

  function daysInMonth(y, m) {
    return new Date(y, m, 0).getDate();
  }

  function firstDayOfWeek(y, m) {
    var d = new Date(y, m - 1, 1).getDay();
    return d === 0 ? 6 : d - 1;
  }

  function renderMonth(ym) {
    var parts = ym.split('-');
    var year = parseInt(parts[0]);
    var month = parseInt(parts[1]);
    var days = data[ym] || [];
    var daySet = new Set(days);
    var total = daysInMonth(year, month);
    var offset = firstDayOfWeek(year, month);

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
    title.textContent = shortMonths[month - 1] + ' ' + year;
    headerEl.appendChild(prevBtn);
    headerEl.appendChild(title);
    headerEl.appendChild(nextBtn);

    gridEl.innerHTML = '';
    var weekdays = ['Mo','Tu','We','Th','Fr','Sa','Su'];
    weekdays.forEach(function(wd) {
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
        cell.textContent = d;
      } else {
        cell.className = 'cal-day cal-day-empty';
        cell.textContent = d;
      }
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
    var idx = allMonths.indexOf(currentMonth);
    var next = idx - dir;
    if (next >= 0 && next < allMonths.length) {
      currentMonth = allMonths[next];
      currentDay = 0;
      renderMonth(currentMonth);
      renderHeatmap();
    }
  }

  renderHeatmap();
  renderMonth(currentMonth);

  // Scroll tracking — observe link-group elements with data-month-id
  var observed = new WeakSet();
  var observer = null;
  if ('IntersectionObserver' in window) {
    observer = new IntersectionObserver(function(entries) {
      entries.forEach(function(entry) {
        if (entry.isIntersecting) {
          var monthId = entry.target.dataset.monthId;
          var day = parseInt(entry.target.dataset.day || '0');
          var changed = false;
          if (monthId && monthId !== currentMonth) {
            currentMonth = monthId;
            currentDay = day;
            changed = true;
          } else if (day && day !== currentDay) {
            currentDay = day;
            changed = true;
          }
          if (changed) {
            renderMonth(currentMonth);
            renderHeatmap();
          }
        }
      });
    }, { rootMargin: '-80px 0px -60% 0px' });
  }

  function observeGroups() {
    if (!observer) return;
    document.querySelectorAll('.link-group[data-month-id]').forEach(function(s) {
      if (!observed.has(s)) {
        observed.add(s);
        observer.observe(s);
      }
    });
  }

  observeGroups();
  document.addEventListener('pagination-loaded', function() {
    requestAnimationFrame(observeGroups);
  });

  // Also watch for DOM changes as a fallback
  var mutObs = new MutationObserver(function() {
    observeGroups();
  });
  var timeline = document.querySelector('[data-pagination="true"]');
  if (timeline) mutObs.observe(timeline, { childList: true, subtree: true });
})();
|}

let network_calendar_js = {|
// Network calendar — year heatmap + month grid, syncs with scroll (day-level)
(function() {
  var container = document.getElementById('network-calendar');
  if (!container) return;

  var data;
  try { data = JSON.parse(container.dataset.calendarMonths || '{}'); } catch(e) { return; }
  var currentMonth = container.dataset.currentMonth || '';
  var currentDay = 0;
  var allMonths = Object.keys(data).sort().reverse();
  if (!allMonths.length) return;
  if (!currentMonth) currentMonth = allMonths[0];

  var heatmapEl = container.querySelector('.heatmap-strip');
  var headerEl = container.querySelector('.cal-header');
  var gridEl = container.querySelector('.cal-grid');

  var shortMonths = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'];
  var now = new Date();
  var todayYM = now.getFullYear() + '-' + String(now.getMonth() + 1).padStart(2, '0');

  function ymAdd(ym, offset) {
    var parts = ym.split('-');
    var y = parseInt(parts[0]);
    var m = parseInt(parts[1]) - 1 + offset;
    var ny = y + Math.floor(m / 12);
    var nm = ((m % 12) + 12) % 12;
    return ny + '-' + String(nm + 1).padStart(2, '0');
  }

  function countForMonth(ym) {
    return data[ym] ? data[ym].length : 0;
  }

  function getHeatmapWindow() {
    var w = [];
    for (var i = -5; i <= 6; i++) w.push(ymAdd(currentMonth, i));
    return w;
  }

  function renderHeatmap() {
    heatmapEl.innerHTML = '';
    var windowMonths = getHeatmapWindow();
    var maxCount = 1;
    windowMonths.forEach(function(ym) {
      var c = countForMonth(ym);
      if (c > maxCount) maxCount = c;
    });
    var strip = document.createElement('div');
    strip.className = 'heatmap-grid';
    windowMonths.forEach(function(ym) {
      var count = countForMonth(ym);
      var parts = ym.split('-');
      var month = parseInt(parts[1]);
      var isFuture = ym > todayYM;
      var cell = document.createElement('div');
      cell.className = 'heatmap-cell';
      if (ym === currentMonth) cell.classList.add('heatmap-current');
      if (isFuture) {
        cell.dataset.state = 'future';
        cell.dataset.level = 0;
        cell.title = shortMonths[month - 1] + ': upcoming';
      } else if (count === 0) {
        cell.dataset.state = 'empty';
        cell.dataset.level = 0;
        cell.title = shortMonths[month - 1] + ': no posts';
      } else {
        cell.dataset.state = 'active';
        var level = Math.min(4, Math.ceil(count / maxCount * 4));
        cell.dataset.level = level;
        cell.title = shortMonths[month - 1] + ': ' + count + ' day' + (count !== 1 ? 's' : '');
      }
      if (!isFuture) {
        (function(targetYm) {
          cell.addEventListener('click', function() {
            currentMonth = targetYm;
            currentDay = 0;
            renderMonth(currentMonth);
            renderHeatmap();
            var el = document.querySelector('.network-feed-item[data-month-id="' + targetYm + '"]');
            if (el) el.scrollIntoView({ behavior: 'smooth', block: 'start' });
          });
        })(ym);
      }
      var label = document.createElement('span');
      label.className = 'heatmap-label';
      label.textContent = shortMonths[month - 1];
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

  function daysInMonth(y, m) { return new Date(y, m, 0).getDate(); }
  function firstDayOfWeek(y, m) {
    var d = new Date(y, m - 1, 1).getDay();
    return d === 0 ? 6 : d - 1;
  }

  function renderMonth(ym) {
    var parts = ym.split('-');
    var year = parseInt(parts[0]);
    var month = parseInt(parts[1]);
    var days = data[ym] || [];
    var daySet = new Set(days);
    var total = daysInMonth(year, month);
    var offset = firstDayOfWeek(year, month);

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
    title.textContent = shortMonths[month - 1] + ' ' + year;
    headerEl.appendChild(prevBtn);
    headerEl.appendChild(title);
    headerEl.appendChild(nextBtn);

    gridEl.innerHTML = '';
    var weekdays = ['Mo','Tu','We','Th','Fr','Sa','Su'];
    weekdays.forEach(function(wd) {
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
        cell.textContent = d;
      } else {
        cell.className = 'cal-day cal-day-empty';
        cell.textContent = d;
      }
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
    var idx = allMonths.indexOf(currentMonth);
    var next = idx - dir;
    if (next >= 0 && next < allMonths.length) {
      currentMonth = allMonths[next];
      currentDay = 0;
      renderMonth(currentMonth);
      renderHeatmap();
    }
  }

  renderHeatmap();
  renderMonth(currentMonth);

  // Scroll tracking — observe feed items with data-month-id and data-day
  var observed = new WeakSet();
  var observer = null;
  if ('IntersectionObserver' in window) {
    observer = new IntersectionObserver(function(entries) {
      entries.forEach(function(entry) {
        if (entry.isIntersecting) {
          var monthId = entry.target.dataset.monthId;
          var day = parseInt(entry.target.dataset.day || '0');
          var changed = false;
          if (monthId && monthId !== currentMonth) {
            currentMonth = monthId;
            currentDay = day;
            changed = true;
          } else if (day && day !== currentDay) {
            currentDay = day;
            changed = true;
          }
          if (changed) {
            renderMonth(currentMonth);
            renderHeatmap();
          }
        }
      });
    }, { rootMargin: '-80px 0px -60% 0px' });
  }

  function observeItems() {
    if (!observer) return;
    document.querySelectorAll('.network-feed-item[data-month-id]').forEach(function(s) {
      if (!observed.has(s)) {
        observed.add(s);
        observer.observe(s);
      }
    });
  }

  observeItems();
  document.addEventListener('pagination-loaded', function() {
    requestAnimationFrame(observeItems);
  });
  var mutObs = new MutationObserver(function() { observeItems(); });
  var timeline = document.querySelector('[data-collection-type="network"]');
  if (timeline) mutObs.observe(timeline, { childList: true, subtree: true });
})();
|}

let ideas_calendar_js = {|
// Ideas calendar — year heatmap with stacked status bars, syncs with scroll
(function() {
  var container = document.getElementById('ideas-calendar');
  if (!container) return;

  var data;
  try { data = JSON.parse(container.dataset.calendarYears || '{}'); } catch(e) { return; }
  var currentYear = container.dataset.currentYear || '';
  var allYears = Object.keys(data).sort().reverse();
  if (!allYears.length) return;
  if (!currentYear) currentYear = allYears[0];

  var heatmapEl = container.querySelector('.heatmap-strip');

  var now = new Date();
  var todayYear = String(now.getFullYear());

  function getHeatmapWindow() {
    var cy = parseInt(currentYear);
    var win = [];
    for (var i = -4; i <= 5; i++) {
      win.push(String(cy + i));
    }
    return win;
  }

  function totalForYear(y) {
    var d = data[y];
    if (!d) return 0;
    return d.a + d.d + d.o + d.c + d.e;
  }

  function renderHeatmap() {
    heatmapEl.innerHTML = '';
    var windowYears = getHeatmapWindow();

    // Find the maximum total across all years so bars scale proportionally
    var maxTotal = 0;
    allYears.forEach(function(y) {
      var t = totalForYear(y);
      if (t > maxTotal) maxTotal = t;
    });
    if (maxTotal === 0) maxTotal = 1;

    var strip = document.createElement('div');
    strip.className = 'heatmap-grid';
    strip.style.gridTemplateColumns = 'repeat(' + windowYears.length + ', 1fr)';

    windowYears.forEach(function(y) {
      var total = totalForYear(y);
      var isFuture = y > todayYear;
      var cell = document.createElement('div');
      cell.className = 'heatmap-cell';
      if (y === currentYear) cell.classList.add('heatmap-current');

      if (isFuture) {
        cell.dataset.state = 'future';
        cell.title = y;
      } else if (total === 0) {
        cell.dataset.state = 'empty';
        cell.title = y + ': no ideas';
      } else {
        cell.dataset.state = 'active';
        var d = data[y];
        var parts = [];
        if (d.a) parts.push(d.a + ' available');
        if (d.d) parts.push(d.d + ' discussion');
        if (d.o) parts.push(d.o + ' ongoing');
        if (d.c) parts.push(d.c + ' completed');
        if (d.e) parts.push(d.e + ' expired');
        cell.title = y + ': ' + parts.join(', ');
      }

      if (!isFuture) {
        (function(targetY) {
          cell.addEventListener('click', function() {
            currentYear = targetY;
            renderHeatmap();
            var section = document.getElementById('year-' + targetY);
            if (section) section.scrollIntoView({ behavior: 'smooth', block: 'start' });
          });
        })(y);
      }

      var label = document.createElement('span');
      label.className = 'heatmap-label';
      label.textContent = "'" + y.slice(-2);

      // Status stacked bar or fallback icon for empty/future
      var bar;
      if (isFuture) {
        bar = document.createElement('div');
        bar.className = 'heatmap-circle';
        bar.innerHTML = '<svg xmlns="http://www.w3.org/2000/svg" width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><path d="M3 12a9 9 0 1 0 18 0a9 9 0 0 0 -18 0"/><path d="M12 7v5l3 3"/></svg>';
      } else if (total === 0) {
        bar = document.createElement('div');
        bar.className = 'heatmap-circle';
        bar.innerHTML = '<svg xmlns="http://www.w3.org/2000/svg" width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="3" stroke-linecap="round"><path d="M5 12h14"/></svg>';
      } else {
        bar = document.createElement('div');
        bar.className = 'idea-status-bar';
        // Scale bar height proportionally to max year
        var scale = total / maxTotal;
        var minH = 0.25;
        var maxH = 1.15;
        var h = minH + (maxH - minH) * scale;
        bar.style.height = h.toFixed(2) + 'rem';
        var d = data[y];
        var segs = [
          { cls: 'bar-available', n: d.a },
          { cls: 'bar-discussion', n: d.d },
          { cls: 'bar-ongoing', n: d.o },
          { cls: 'bar-completed', n: d.c },
          { cls: 'bar-expired', n: d.e }
        ];
        segs.forEach(function(seg) {
          if (seg.n > 0) {
            var s = document.createElement('span');
            s.className = seg.cls;
            s.style.flex = seg.n;
            bar.appendChild(s);
          }
        });
      }

      cell.appendChild(label);
      cell.appendChild(bar);
      strip.appendChild(cell);
    });
    heatmapEl.appendChild(strip);
  }

  renderHeatmap();

  // Scroll tracking
  var sections = document.querySelectorAll('[data-year-id]');
  if (sections.length) {
    function updateCurrentYear() {
      var best = null;
      sections.forEach(function(s) {
        var rect = s.getBoundingClientRect();
        if (rect.top <= 120) best = s;
      });
      if (best) {
        var yearId = best.dataset.yearId;
        if (yearId && yearId !== currentYear) {
          currentYear = yearId;
          renderHeatmap();
        }
      }
    }
    window.addEventListener('scroll', updateCurrentYear, { passive: true });
    updateCurrentYear();
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

let masonry_js = {|
// No-op: both .vid-grid and .proj-grid now use CSS Grid which provides
// natural left-to-right flow. The masonry column-reorder is no longer needed.
|}

let link_filter_js = {|
// Link classification filter checkboxes
(function() {
  var checkboxes = document.querySelectorAll('.link-filter-checkbox');
  if (!checkboxes.length) return;

  function applyFilters() {
    checkboxes.forEach(function(cb) {
      var kind = cb.dataset.linkFilter;
      var rows = document.querySelectorAll('.link-row[data-link-filter="' + kind + '"]');
      rows.forEach(function(row) {
        row.style.display = cb.checked ? '' : 'none';
      });
    });
    // Hide link-group divs with no visible rows
    document.querySelectorAll('.link-group').forEach(function(group) {
      var visible = group.querySelectorAll('.link-row:not([style*="display: none"])');
      group.style.display = visible.length ? '' : 'none';
    });
  }

  checkboxes.forEach(function(cb) {
    cb.addEventListener('change', applyFilters);
  });

  // Re-apply filters when pagination loads new content
  document.addEventListener('pagination-loaded', function() {
    applyFilters();
  });

  // Apply initial filter state (hides unchecked kinds like untitled)
  applyFilters();
})();
|}
