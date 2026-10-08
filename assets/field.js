// Trial plan page (database/field.qmd).
//
// R draws every plot, block caption and entry template at the origin.
// This script lays them out for the current screen and handles pan/zoom,
// the plot cards and the filters.

(function () {
  'use strict';

  const plan = document.getElementById('plan');
  if (!plan) return;

  const SVG_NS = 'http://www.w3.org/2000/svg';

  // Keep these in step with the small-screen media queries in field.css.
  const phonePortrait = window.matchMedia('(max-width: 700px)');
  const shortScreen = window.matchMedia('(max-height: 520px)');
  const reducedMotion = window.matchMedia('(prefers-reduced-motion: reduce)');

  const stage = plan.querySelector('.plan-stage');
  const svg = document.getElementById('plan-svg');
  const world = document.getElementById('world');
  const ground = svg.querySelector('.ground');
  const blocks = Array.from(svg.querySelectorAll('.block'));
  const plots = Array.from(svg.querySelectorAll('.plot[data-id]'));

  // Sizes in plan units, set in scripts/trial_plan.R
  const PLOT_W = Number(svg.dataset.pw);
  const PLOT_H = Number(svg.dataset.ph);
  const COL_GAP = Number(svg.dataset.cg);
  const RANGE_GAP = Number(svg.dataset.rg);
  const BLOCK_GAP = Number(svg.dataset.bg);
  const PAD = Number(svg.dataset.pad);
  const CAPTION_SPACE = 3.4;
  const REFLOW_MS = 450;

  let layout = null;   // 'grid', 'stack' or 'row'
  let planWidth = 1;
  let planHeight = 1;

  function svgElement(name, attrs, text) {
    const el = document.createElementNS(SVG_NS, name);
    for (const [key, value] of Object.entries(attrs)) {
      el.setAttribute(key, value);
    }
    if (text !== undefined) el.textContent = text;
    return el;
  }


  // Layout ------------------------------------------------------------
  //
  // grid  - desktop and tablet. Blocks side by side, each roughly square,
  //         and the whole plan fits on screen.
  // stack - phone portrait. Blocks stacked, one plot per range. Fits the
  //         width; scroll down.
  // row   - phone landscape. Each block is a single range. Fits the
  //         height; scroll across.

  function layoutForScreen() {
    if (phonePortrait.matches) return 'stack';
    if (shortScreen.matches) return 'row';
    return 'grid';
  }

  function columnsFor(mode, plotCount) {
    if (mode === 'stack') return 1;
    if (mode === 'row') return plotCount;
    return Math.max(1, Math.ceil(Math.sqrt(plotCount * PLOT_H / PLOT_W)));
  }

  function wrapWords(text, maxChars) {
    const lines = [];
    let line = '';
    for (const word of text.split(/\s+/)) {
      if (line && (line + ' ' + word).length > maxChars) {
        lines.push(line);
        line = word;
      } else {
        line = line ? line + ' ' + word : word;
      }
    }
    if (line) lines.push(line);
    return lines;
  }

  // "Block A  Organisation" on one line, or the organisation wrapped onto
  // lines under the letter when the plan is stacked.
  function setCaption(caption, lines, x, y, wrapped) {
    caption.querySelectorAll('.block-name').forEach(t => t.remove());
    caption.style.transform = `translate(${x}px, ${y}px)`;
    lines.forEach((line, i) => {
      const attrs = wrapped
        ? { class: 'block-name', x: 0, dy: i === 0 ? 1.55 : 1.45 }
        : { class: 'block-name', dx: 1.6 };
      caption.appendChild(svgElement('tspan', attrs, line));
    });
  }

  function cellX(x, col) {
    return x + col * (PLOT_W + COL_GAP);
  }

  function cellY(y, row) {
    return y + row * (PLOT_H + RANGE_GAP);
  }

  // Places a block's plots on a grid of cells, first-fit in plot order.
  // An opened plot takes 2×2 cells and the rest pack around it.
  function packGrid(blockPlots, cols, opened) {
    const taken = [];
    const isFree = (r, c) => !(taken[r] && taken[r][c]);
    const take = (r, c) => { (taken[r] = taken[r] || [])[c] = true; };
    const placed = new Map();

    for (const plot of blockPlots) {
      const span = plot === opened ? 2 : 1;
      for (let r = 0; !placed.has(plot); r++) {
        for (let c = 0; c + span <= cols && !placed.has(plot); c++) {
          let fits = true;
          for (let dr = 0; dr < span; dr++) {
            for (let dc = 0; dc < span; dc++) {
              if (!isFree(r + dr, c + dc)) fits = false;
            }
          }
          if (!fits) continue;
          for (let dr = 0; dr < span; dr++) {
            for (let dc = 0; dc < span; dc++) take(r + dr, c + dc);
          }
          placed.set(plot, { row: r, col: c, span });
        }
      }
    }

    const rows = taken.length;
    const free = [];
    for (let r = 0; r < rows; r++) {
      for (let c = 0; c < cols; c++) {
        if (isFree(r, c)) free.push({ row: r, col: c });
      }
    }
    return { placed, rows, free };
  }

  function setPlotBox(plot, box) {
    plot.style.transform = `translate(${box.x}px, ${box.y}px)`;
    const fill = plot.querySelector('.plot-fill');
    fill.style.width = box.w + 'px';
    fill.style.height = box.h + 'px';
    Object.assign(plot.dataset, {
      px: box.x, py: box.y, pw: box.w, ph: box.h,
      cx: box.x + box.w / 2, cy: box.y + box.h / 2,
      row: box.row, col: box.col
    });
  }

  // Lays out one block at (x, y) and returns its size. `opened` is the
  // plot currently opened up, if it's in this block.
  function layoutBlock(block, mode, x, y, opened) {
    const blockPlots = Array.from(block.querySelectorAll('.plot[data-id]'));
    const extras = block.querySelector('.block-extras');
    extras.replaceChildren();
    const rangeLabel = (row, labelY) => extras.appendChild(svgElement('text', {
      class: 'axis-no', 'text-anchor': 'end', x: x - 1, y: labelY + 0.45
    }, row));

    // Phone portrait: one plot per range. An opened plot runs the full
    // width of the field and is about two and a half plots tall.
    if (mode === 'stack') {
      let nextY = y;
      blockPlots.forEach((plot, i) => {
        if (plot === opened) {
          const h = PLOT_H * 2.5 + RANGE_GAP * 1.5;
          setPlotBox(plot, { x: 0.5, y: nextY, w: PLOT_W + PAD * 2 - 1, h, row: i + 1, col: 1 });
          nextY += h + RANGE_GAP;
        } else {
          setPlotBox(plot, { x, y: nextY, w: PLOT_W, h: PLOT_H, row: i + 1, col: 1 });
          rangeLabel(i + 1, nextY + PLOT_H / 2);
          nextY += PLOT_H + RANGE_GAP;
        }
      });
      const height = nextY - RANGE_GAP - y;
      extras.appendChild(svgElement('text', {
        class: 'axis-no', 'text-anchor': 'middle', x: x + PLOT_W / 2, y: y + height + 2.2
      }, 1));
      return { width: PLOT_W, height };
    }

    // Grid and row layouts: cells, with the opened plot taking 2×2.
    let cols = columnsFor(mode, blockPlots.length);
    if (opened && blockPlots.includes(opened)) cols = Math.max(cols, 2);
    const { placed, rows, free } = packGrid(blockPlots, cols, opened);
    const width = cols * PLOT_W + (cols - 1) * COL_GAP;
    const height = rows * PLOT_H + (rows - 1) * RANGE_GAP;

    for (const [plot, cell] of placed) {
      setPlotBox(plot, {
        x: cellX(x, cell.col),
        y: cellY(y, cell.row),
        w: cell.span * PLOT_W + (cell.span - 1) * COL_GAP,
        h: cell.span * PLOT_H + (cell.span - 1) * RANGE_GAP,
        row: cell.row + 1,
        col: cell.col + 1
      });
    }

    for (const cell of free) {
      extras.appendChild(svgElement('rect', {
        class: 'plot is-buffer',
        x: cellX(x, cell.col), y: cellY(y, cell.row), width: PLOT_W, height: PLOT_H
      }));
    }
    for (let r = 0; r < rows; r++) rangeLabel(r + 1, cellY(y, r) + PLOT_H / 2);
    for (let c = 0; c < cols; c++) {
      extras.appendChild(svgElement('text', {
        class: 'axis-no', 'text-anchor': 'middle', x: cellX(x, c) + PLOT_W / 2, y: y + height + 2.2
      }, c + 1));
    }
    return { width, height };
  }

  // Lays out every block. With `animate`, plots glide to their new places
  // (used when a plot opens or closes); otherwise they jump.
  function layoutPlan(mode, opened = null, animate = false) {
    svg.classList.toggle('is-reflowing', animate && !reducedMotion.matches);
    clearTimeout(layoutPlan.timer);
    layoutPlan.timer = setTimeout(() => svg.classList.remove('is-reflowing'), REFLOW_MS);

    const stacked = mode === 'stack';
    let nextX = PAD;
    // Stacked, the plan is zoomed to the screen width, so the usual top
    // margin would turn into a big blank band.
    let nextY = stacked ? 1.5 : PAD;
    let right = 0;
    let bottom = 0;

    for (const block of blocks) {
      const caption = block.querySelector('.block-cap');
      const name = caption.dataset.name;
      const lines = stacked ? wrapWords(name, Math.floor(PLOT_W / 0.62)) : [name];
      const captionHeight = stacked ? 2.4 + lines.length * 1.5 : CAPTION_SPACE;

      const x = stacked ? PAD : nextX;
      const y = stacked ? nextY + captionHeight : PAD + CAPTION_SPACE;

      setCaption(caption, lines, x, y - captionHeight + 1.1, stacked);
      let captionWidth = caption.getComputedTextLength();
      if (stacked) captionWidth = Math.min(captionWidth, PLOT_W);

      const { width, height } = layoutBlock(block, mode, x, y, opened);

      right = Math.max(right, x + Math.max(width, captionWidth));
      bottom = Math.max(bottom, y + height + 3);
      if (stacked) {
        nextY = y + height + 3 + BLOCK_GAP * 0.6;
      } else {
        nextX += Math.max(width + BLOCK_GAP, captionWidth + 4);
      }
    }

    planWidth = right + PAD;
    planHeight = bottom + PAD;
    ground.setAttribute('width', planWidth);
    ground.setAttribute('height', planHeight);
  }


  // Pan and zoom ------------------------------------------------------

  const view = { scale: 1, x: 0, y: 0 };
  let animation = null;
  let animationDone = null;

  function drawView() {
    world.setAttribute('transform', `translate(${view.x} ${view.y}) scale(${view.scale})`);
    if (selected && entry.classList.contains('is-open')) placeCard(selected);
  }

  function setView(target) {
    view.scale = target.scale;
    view.x = target.x;
    view.y = target.y;
    drawView();
  }

  // grid fits the whole plan, stack fits the width and starts at the top,
  // row fits the height and starts at the left.
  function fittedView() {
    const w = svg.clientWidth;
    const h = svg.clientHeight;

    if (layout === 'stack') {
      const pad = 12;
      const scale = (w - pad * 2) / planWidth;
      const fitsDown = planHeight * scale <= h - pad * 2;
      return {
        scale,
        x: (w - planWidth * scale) / 2,
        y: fitsDown ? (h - planHeight * scale) / 2 : pad
      };
    }

    if (layout === 'row') {
      const pad = 8;
      const scale = (h - pad * 2) / planHeight;
      const fitsAcross = planWidth * scale <= w - pad * 2;
      return {
        scale,
        x: fitsAcross ? (w - planWidth * scale) / 2 : pad,
        y: (h - planHeight * scale) / 2
      };
    }

    const pad = 48;
    const scale = Math.min((w - pad * 2) / planWidth, (h - pad * 2) / planHeight);
    return {
      scale,
      x: (w - planWidth * scale) / 2,
      y: (h - planHeight * scale) / 2
    };
  }

  function clampScale(scale) {
    return Math.min(Math.max(scale, fittedView().scale * 0.6), 60);
  }

  // Stopping an animation early still runs its `done` callback, so
  // anything waiting on it (like a card unfolding) isn't left hanging.
  function stopAnimation() {
    if (animation) {
      cancelAnimationFrame(animation);
      animation = null;
    }
    const done = animationDone;
    animationDone = null;
    if (done) done();
  }

  function animateTo(target, duration, done) {
    stopAnimation();
    animationDone = done || null;
    const start = { ...view };
    const startTime = performance.now();

    function step(now) {
      const t = Math.min(1, (now - startTime) / duration);
      const eased = 1 - Math.pow(1 - t, 3);
      view.scale = start.scale + (target.scale - start.scale) * eased;
      view.x = start.x + (target.x - start.x) * eased;
      view.y = start.y + (target.y - start.y) * eased;
      drawView();
      if (t < 1) {
        animation = requestAnimationFrame(step);
      } else {
        animation = null;
        const finished = animationDone;
        animationDone = null;
        if (finished) finished();
      }
    }
    step(startTime);
  }

  function fitToScreen(animate, duration = 320) {
    if (animate) {
      animateTo(fittedView(), duration);
    } else {
      stopAnimation();
      setView(fittedView());
    }
  }

  // Zoom by `factor`, keeping the point (cx, cy) on screen where it is.
  // While a card is open, zoom around its corner instead, so the card
  // stays where it is and the field scales behind it.
  function zoomPivot(x, y) {
    if (selected && entry.classList.contains('is-open')) {
      const rect = plotRect(selected);
      return [rect.x, rect.y];
    }
    return [x, y];
  }

  function zoomAround(factor, x, y) {
    stopAnimation();
    const [cx, cy] = zoomPivot(x, y);
    const scale = clampScale(view.scale * factor);
    view.x = cx - (cx - view.x) * (scale / view.scale);
    view.y = cy - (cy - view.y) * (scale / view.scale);
    view.scale = scale;
    drawView();
  }

  function toLocal(clientX, clientY) {
    const box = svg.getBoundingClientRect();
    return [clientX - box.left, clientY - box.top];
  }

  svg.addEventListener('wheel', e => {
    e.preventDefault();
    const [x, y] = toLocal(e.clientX, e.clientY);
    zoomAround(Math.exp(-e.deltaY * 0.0015), x, y);
  }, { passive: false });

  // One pointer pans the map (or taps a plot). Two pointers pinch-zoom.
  const pointers = new Map();
  let drag = null;
  let pinch = null;

  function pinchMidpoint() {
    const [a, b] = Array.from(pointers.values());
    const [x, y] = toLocal((a.x + b.x) / 2, (a.y + b.y) / 2);
    return { x, y, distance: Math.hypot(b.x - a.x, b.y - a.y) };
  }

  svg.addEventListener('pointerdown', e => {
    if (e.pointerType === 'mouse' && e.button !== 0) return;
    stopAnimation();
    pointers.set(e.pointerId, { x: e.clientX, y: e.clientY });
    try {
      svg.setPointerCapture(e.pointerId);
    } catch (err) {
      // not every pointer can be captured (e.g. synthetic events)
    }

    if (pointers.size === 1) {
      // Note the plot now: once the pointer is captured, later events are
      // retargeted to the svg and the plot is no longer e.target.
      drag = {
        startX: e.clientX,
        startY: e.clientY,
        viewX: view.x,
        viewY: view.y,
        moved: false,
        plot: e.target.closest('.plot[data-id]')
      };
    } else if (pointers.size === 2) {
      // A second finger turns this into a pinch, never a tap
      drag = null;
      const mid = pinchMidpoint();
      const [pivotX, pivotY] = zoomPivot(mid.x, mid.y);
      pinch = {
        distance: mid.distance || 1,
        scale: view.scale,
        viewX: view.x,
        viewY: view.y,
        midX: pivotX,
        midY: pivotY,
        // with a card open, pinching only zooms; it doesn't pan
        fixed: pivotX !== mid.x || pivotY !== mid.y
      };
    }
  });

  svg.addEventListener('pointermove', e => {
    if (!pointers.has(e.pointerId)) return;
    pointers.set(e.pointerId, { x: e.clientX, y: e.clientY });

    if (pinch && pointers.size >= 2) {
      const mid = pinchMidpoint();
      const scale = clampScale(pinch.scale * mid.distance / pinch.distance);
      // keep what was under the fingers at the start under the fingers
      const anchorX = pinch.fixed ? pinch.midX : mid.x;
      const anchorY = pinch.fixed ? pinch.midY : mid.y;
      view.x = anchorX - (pinch.midX - pinch.viewX) * (scale / pinch.scale);
      view.y = anchorY - (pinch.midY - pinch.viewY) * (scale / pinch.scale);
      view.scale = scale;
      drawView();
      return;
    }

    if (!drag) return;
    const dx = e.clientX - drag.startX;
    const dy = e.clientY - drag.startY;
    if (Math.hypot(dx, dy) > 4) {
      drag.moved = true;
      svg.classList.add('is-dragging');
    }
    if (drag.moved) {
      view.x = drag.viewX + dx;
      view.y = drag.viewY + dy;
      drawView();
    }
  });

  function onPointerEnd(e) {
    if (!pointers.has(e.pointerId)) return;
    pointers.delete(e.pointerId);

    // Lifting fingers after a pinch isn't a tap
    if (pinch) {
      if (pointers.size < 2) pinch = null;
      return;
    }
    if (!drag || pointers.size > 0) return;

    const { moved, plot } = drag;
    drag = null;
    svg.classList.remove('is-dragging');
    if (moved || e.type === 'pointercancel') return;

    // Fall back to hit-testing the release point
    const target = plot || document.elementFromPoint(e.clientX, e.clientY)?.closest('.plot[data-id]');
    if (target && !target.classList.contains('is-out')) {
      openEntry(target);
    } else {
      foldCard();
    }
  }

  svg.addEventListener('pointerup', onPointerEnd);
  svg.addEventListener('pointercancel', onPointerEnd);

  function zoomFromCentre(factor) {
    zoomAround(factor, svg.clientWidth / 2, svg.clientHeight / 2);
  }

  document.getElementById('z-in').addEventListener('click', () => zoomFromCentre(1.5));
  document.getElementById('z-out').addEventListener('click', () => zoomFromCentre(1 / 1.5));
  document.getElementById('z-fit').addEventListener('click', () => {
    closeEntry(true);
    fitToScreen(true);
  });


  // Key ---------------------------------------------------------------
  // Always shown on large screens; folded behind a button on phones.

  const legend = document.getElementById('legend');
  const legendButton = legend.querySelector('.legend-toggle');

  function setLegend(open) {
    legend.classList.toggle('is-open', open);
    legendButton.setAttribute('aria-expanded', String(open));
  }

  legendButton.addEventListener('click', () => {
    setLegend(!legend.classList.contains('is-open'));
  });


  // Opening a plot ---------------------------------------------------
  //
  // Clicking a plot opens it up into a bigger plot (2×2 on large screens,
  // a tall full-width plot on phones). The rest of the block reflows
  // around it, then the entry fades in inside the bigger plot. The map
  // zooms just enough for the entry to be readable.

  const entry = document.getElementById('entry');
  const entryBody = document.getElementById('e-body');
  const closeButton = document.getElementById('e-close');
  const FADE_MS = 160;
  let selected = null;
  let openTimer = null;

  // Where a plot is on screen (relative to the map) for a given view.
  function plotRect(plot, v = view) {
    return {
      x: v.x + plot.dataset.px * v.scale,
      y: v.y + plot.dataset.py * v.scale,
      width: plot.dataset.pw * v.scale,
      height: plot.dataset.ph * v.scale
    };
  }

  // The view for an opened plot: big enough to read (about 520px wide on
  // large screens), never bigger than the map, and centred on screen.
  function framePlot(plot) {
    const margin = phonePortrait.matches ? 8 : 16;
    // the key and zoom buttons sit along the bottom of the map
    const bottomMargin = phonePortrait.matches || shortScreen.matches ? 12 : 56;
    const w = svg.clientWidth;
    const h = svg.clientHeight;
    const pw = Number(plot.dataset.pw);
    const ph = Number(plot.dataset.ph);

    const fits = Math.min((w - margin * 2) / pw, (h - margin - bottomMargin) / ph);
    const readable = phonePortrait.matches ? view.scale : Math.max(view.scale, 520 / pw);
    const scale = Math.min(readable, fits);

    // centre the plot in the part of the map not covered by the controls
    const centreX = w / 2;
    const centreY = margin + (h - margin - bottomMargin) / 2;
    return {
      scale,
      x: centreX - plot.dataset.cx * scale,
      y: centreY - plot.dataset.cy * scale
    };
  }

  // The entry sits exactly over the opened plot and follows it as the map
  // moves.
  function placeCard(plot) {
    const rect = plotRect(plot);
    entry.style.left = rect.x + 'px';
    entry.style.top = rect.y + 'px';
    entry.style.width = rect.width + 'px';
    entry.style.height = rect.height + 'px';

    // If the plot runs off the edge of the map (say after zooming in),
    // keep the close button inside the part you can see.
    const hiddenRight = rect.x + rect.width - svg.clientWidth;
    const hiddenTop = -rect.y;
    closeButton.style.right = Math.max(6, hiddenRight + 6) + 'px';
    closeButton.style.top = Math.max(6, hiddenTop + 6) + 'px';
  }

  function showPosition(plot) {
    const pos = entryBody.querySelector('.e-pos');
    if (pos) pos.textContent = `range ${plot.dataset.row}, column ${plot.dataset.col}`;
  }

  function hideCard() {
    entry.classList.remove('is-open');
    entry.setAttribute('aria-hidden', 'true');
  }

  function openEntry(plot) {
    clearTimeout(openTimer);
    if (selected && selected !== plot) {
      selected.classList.remove('is-selected', 'is-expanded');
    }
    hideCard();
    plots.forEach(p => p.classList.toggle('is-selected', p === plot));
    plot.classList.add('is-expanded');
    selected = plot;

    const template = document.querySelector(`template[data-for="${plot.dataset.id}"]`);
    entryBody.replaceChildren(template.content.cloneNode(true));
    entryBody.scrollTop = 0;
    setLegend(false);

    layoutPlan(layout, plot, true);
    showPosition(plot);
    animateTo(framePlot(plot), REFLOW_MS);

    // let the plots settle before the entry appears
    openTimer = setTimeout(() => {
      if (selected !== plot) return;
      placeCard(plot);
      entry.classList.add('is-open');
      entry.setAttribute('aria-hidden', 'false');
      closeButton.focus({ preventScroll: true });
    }, reducedMotion.matches ? 0 : REFLOW_MS);
  }

  // Shrinks the opened plot back and reflows the field. `animate` is false
  // when the plot has just been filtered out or the view is being reset.
  function closeEntry(animate = false) {
    if (!selected) return;
    clearTimeout(openTimer);
    const plot = selected;
    selected = null;
    plot.classList.remove('is-selected', 'is-expanded');
    hideCard();
    layoutPlan(layout, null, animate);
    return plot;
  }

  // Clicking empty ground: close, but leave the view where it is.
  function foldCard() {
    if (!selected) return;
    hideCard();
    setTimeout(() => closeEntry(true), reducedMotion.matches ? 0 : FADE_MS);
  }

  // × or Escape: close, then pull back out to the whole field.
  function dismissEntry() {
    if (!selected) return;
    hideCard();
    setTimeout(() => {
      const plot = closeEntry(true);
      if (!plot) return;
      fitToScreen(true, 650);
      if (plot.getAttribute('tabindex') === '0') plot.focus({ preventScroll: true });
    }, reducedMotion.matches ? 0 : FADE_MS);
  }

  closeButton.addEventListener('click', dismissEntry);

  document.addEventListener('keydown', e => {
    if (e.key === 'Escape') dismissEntry();

    const focused = document.activeElement;
    if ((e.key === 'Enter' || e.key === ' ') && focused?.classList.contains('plot')) {
      e.preventDefault();
      openEntry(focused);
    }
  });


  // Filters -----------------------------------------------------------
  //
  // Chips in the same group are OR'd (any selected keyword will do);
  // groups and the search box are AND'd. Each chip shows how many plots
  // you would see if you switched it on.

  const searchBox = document.getElementById('pl-search');
  const chips = Array.from(document.querySelectorAll('.facet-chip'));
  const clearButton = document.getElementById('pl-clear');
  const emptyMessage = document.getElementById('pl-empty');
  const matchCount = document.getElementById('n-match');
  const filterButton = document.getElementById('pl-filter-btn');
  const filterPanel = document.getElementById('pl-filters');
  const filterBadge = document.getElementById('pl-filter-n');
  const showButton = document.getElementById('pl-show');
  const activeList = document.getElementById('pl-active');

  const selectedFilters = { kw: new Set(), status: new Set(), cran: new Set() };

  // What each plot can be filtered on, read once from its data attributes
  const plotInfo = new Map(plots.map(plot => [plot, {
    search: plot.dataset.search,
    kw: (plot.dataset.keywords || '').split('|').filter(Boolean),
    status: [plot.dataset.status],
    cran: plot.dataset.cran === '1' ? ['1'] : []
  }]));

  function plural(n, word) {
    return `${n} ${word}${n === 1 ? '' : 's'}`;
  }

  // `ignoreGroup` lets a chip count what its own group would match.
  function plotMatches(plot, term, ignoreGroup) {
    const info = plotInfo.get(plot);
    if (term && !info.search.includes(term)) return false;

    for (const [group, values] of Object.entries(selectedFilters)) {
      if (group === ignoreGroup || values.size === 0) continue;
      if (!info[group].some(v => values.has(v))) return false;
    }
    return true;
  }

  function isOn(chip) {
    return chip.getAttribute('aria-pressed') === 'true';
  }

  function setChip(chip, on) {
    const values = selectedFilters[chip.dataset.group];
    if (on) {
      values.add(chip.dataset.key);
    } else {
      values.delete(chip.dataset.key);
    }
    chip.setAttribute('aria-pressed', String(on));
  }

  function updateChips(term) {
    for (const chip of chips) {
      const group = chip.dataset.group;
      const count = plots.filter(plot =>
        plotMatches(plot, term, group) && plotInfo.get(plot)[group].includes(chip.dataset.key)
      ).length;

      chip.querySelector('.chip-n').textContent = count;
      chip.disabled = count === 0 && !isOn(chip);
      chip.setAttribute('aria-label', `${chip.querySelector('.chip-label').textContent}, ${plural(count, 'plot')}`);
    }
  }

  function updateFilterButton(selectedCount, matching) {
    filterBadge.hidden = selectedCount === 0;
    filterBadge.textContent = selectedCount;
    filterButton.classList.toggle('is-active', selectedCount > 0);
    filterButton.setAttribute('aria-label', selectedCount ? `Filters, ${selectedCount} active` : 'Filters');

    showButton.textContent = matching === 0 ? 'No matches' : `Show ${plural(matching, 'plot')}`;
    showButton.disabled = matching === 0;
  }

  // Removable pills for the filters that are on, shown over the map so you
  // can see them with the panel closed.
  function showActiveFilters() {
    const on = chips.filter(isOn);
    activeList.replaceChildren();

    for (const chip of on) {
      const label = chip.querySelector('.chip-label').textContent;
      const pill = document.createElement('button');
      pill.type = 'button';
      pill.className = 'chip active-pill';
      pill.setAttribute('aria-label', `Remove filter: ${label}`);
      pill.append(label);

      const cross = document.createElement('span');
      cross.className = 'x';
      cross.setAttribute('aria-hidden', 'true');
      cross.textContent = '×';
      pill.append(cross);

      pill.addEventListener('click', () => {
        setChip(chip, false);
        applyFilters();
      });
      activeList.append(pill);
    }

    if (on.length > 1) {
      const clear = document.createElement('button');
      clear.type = 'button';
      clear.className = 'active-clear';
      clear.textContent = 'Clear all';
      clear.addEventListener('click', clearFilters);
      activeList.append(clear);
    }

    activeList.hidden = on.length === 0;
  }

  // Keep the filters in the URL so a filtered view can be shared. Not ?q=,
  // which Quarto's site search uses and strips.
  function updateUrl(term) {
    const url = new URL(window.location.href);
    ['search', 'k', 'status', 'cran'].forEach(key => url.searchParams.delete(key));
    if (term) url.searchParams.set('search', term);
    if (selectedFilters.kw.size) url.searchParams.set('k', Array.from(selectedFilters.kw).join(','));
    if (selectedFilters.status.size) url.searchParams.set('status', Array.from(selectedFilters.status).join(','));
    if (selectedFilters.cran.size) url.searchParams.set('cran', '1');
    window.history.replaceState(null, '', url);
  }

  function applyFilters() {
    const term = searchBox.value.trim().toLowerCase();

    let matching = 0;
    for (const plot of plots) {
      const ok = plotMatches(plot, term);
      plot.classList.toggle('is-out', !ok);
      plot.setAttribute('tabindex', ok ? '0' : '-1');
      if (ok) matching++;
    }
    matchCount.textContent = matching;

    const selectedCount = Object.values(selectedFilters).reduce((sum, set) => sum + set.size, 0);

    updateChips(term);
    updateFilterButton(selectedCount, matching);
    showActiveFilters();
    clearButton.hidden = !term && selectedCount === 0;
    emptyMessage.hidden = matching !== 0;

    if (selected && selected.classList.contains('is-out')) closeEntry(true);
    updateUrl(term);
  }

  function clearFilters() {
    chips.forEach(chip => setChip(chip, false));
    searchBox.value = '';
    applyFilters();
  }

  function setFilterPanel(open, returnFocus = true) {
    filterPanel.hidden = !open;
    filterButton.setAttribute('aria-expanded', String(open));

    if (open) {
      setLegend(false);
      const first = filterPanel.querySelector('.facet-chip:not(:disabled)') || filterPanel.querySelector('.fp-close');
      first.focus({ preventScroll: true });
    } else if (returnFocus) {
      filterButton.focus({ preventScroll: true });
    }
  }

  filterButton.addEventListener('click', () => setFilterPanel(filterPanel.hidden));
  document.getElementById('pl-filters-close').addEventListener('click', () => setFilterPanel(false));
  showButton.addEventListener('click', () => setFilterPanel(false));

  // Clicking anywhere else closes the panel, without pulling focus back.
  document.addEventListener('pointerdown', e => {
    if (filterPanel.hidden) return;
    if (filterPanel.contains(e.target) || filterButton.contains(e.target)) return;
    setFilterPanel(false, false);
  }, true);

  // Escape closes the panel before anything else on the page sees it.
  document.addEventListener('keydown', e => {
    if (e.key === 'Escape' && !filterPanel.hidden) {
      e.stopPropagation();
      setFilterPanel(false);
    }
  }, true);

  searchBox.addEventListener('input', applyFilters);
  searchBox.addEventListener('keydown', e => {
    if (e.key === 'Escape' && searchBox.value) {
      e.stopPropagation();
      searchBox.value = '';
      applyFilters();
    }
  });

  chips.forEach(chip => chip.addEventListener('click', () => {
    setChip(chip, !isOn(chip));
    applyFilters();
  }));

  clearButton.addEventListener('click', clearFilters);
  document.getElementById('pl-empty-clear').addEventListener('click', clearFilters);

  // Pick up filters from the URL. ?k= matches the database page's links.
  function restoreFromUrl() {
    const params = new URLSearchParams(window.location.search);
    searchBox.value = params.get('search') || params.get('q') || '';

    const wanted = { kw: params.get('k'), status: params.get('status'), cran: params.get('cran') };
    for (const chip of chips) {
      const keys = (wanted[chip.dataset.group] || '').toLowerCase().split(',').map(s => s.trim());
      if (keys.includes(chip.dataset.key)) setChip(chip, true);
    }
  }


  // Start up and resizing ----------------------------------------------

  function matchNavbarHeight() {
    const header = document.querySelector('#quarto-header') || document.querySelector('.navbar');
    const bottom = header ? header.getBoundingClientRect().bottom : 0;
    plan.style.setProperty('--nav-h', bottom + 'px');
  }

  // Only re-lay out when the screen crosses a breakpoint; otherwise just
  // keep the view sensible for the new size.
  function onResize() {
    matchNavbarHeight();

    const mode = layoutForScreen();
    if (mode !== layout) {
      layout = mode;
      layoutPlan(mode, selected);
    }

    if (selected) {
      showPosition(selected);
      stopAnimation();
      setView(framePlot(selected));
    } else {
      fitToScreen(false);
    }
  }

  restoreFromUrl();
  applyFilters();
  onResize();

  let resizeFrame = null;
  const observer = new ResizeObserver(() => {
    cancelAnimationFrame(resizeFrame);
    resizeFrame = requestAnimationFrame(onResize);
  });
  observer.observe(stage);
  const header = document.querySelector('#quarto-header');
  if (header) observer.observe(header);

  // Caption widths change once the web fonts arrive
  if (document.fonts) {
    document.fonts.ready.then(() => {
      layout = null;
      onResize();
    });
  }
})();
