(() => {
  let selected = new Set(), drag = null, suppressClick = false;
  const wells = () => [...document.querySelectorAll('#plate .well')];

  function paint(selection) {
    for (const well of wells()) {
      const active = selection.has(well.dataset.well);
      well.classList.toggle('selected', active);
      well.setAttribute('aria-pressed', String(active));
    }
    document.getElementById('selection_label').textContent = selection.size + ' selected';
  }

  function commit(selection) {
    selected = selection;
    paint(selected);
    Shiny.setInputValue('plate_selection', [...selected], { priority: 'event' });
  }

  function rectangle() {
    const start = drag.start.dataset, end = drag.end.dataset;
    const range = (value, a, b) => value >= Math.min(a, b) && value <= Math.max(a, b);
    const next = new Set(drag.append ? selected : []);
    for (const well of wells()) {
      if (range(+well.dataset.row, +start.row, +end.row) && range(+well.dataset.col, +start.col, +end.col)) {
        next.add(well.dataset.well);
      }
    }
    return next;
  }

  document.addEventListener('DOMContentLoaded', () => {
    const plate = document.getElementById('plate');
    const area = document.getElementById('plate_area');
    const menu = document.getElementById('plate_context');
    let anchor;
    function closeMenu(restoreFocus = false) {
      if (menu.hidden) return;
      menu.hidden = true;
      if (restoreFocus) anchor?.focus({ preventScroll: true });
    }
    function openMenu(well, point) {
      if (!selected.has(well.dataset.well)) return;
      anchor = well;
      const bounds = area.getBoundingClientRect();
      const target = well.getBoundingClientRect();
      document.getElementById('plate_context_count').textContent =
        selected.size + (selected.size === 1 ? ' well selected' : ' wells selected');
      menu.hidden = false;
      const left = Math.min((point?.clientX ?? target.right) - bounds.left + 5, area.clientWidth - menu.offsetWidth - 5);
      const top = Math.min((point?.clientY ?? target.top) - bounds.top + 3, area.clientHeight - menu.offsetHeight - 5);
      menu.style.left = area.scrollLeft + Math.max(5, left) + 'px';
      menu.style.top = Math.max(5, top) + 'px';
      menu.querySelector('button').focus({ preventScroll: true });
    }
    new MutationObserver(() => paint(selected)).observe(plate, { childList: true });
    Shiny.addCustomMessageHandler('workflow-next-enabled', enabled => {
      document.getElementById('workflow_next').disabled = !enabled;
    });
    Shiny.addCustomMessageHandler('plate-selection', value => {
      closeMenu();
      selected = new Set(Array.isArray(value) ? value : value ? [value] : []);
      paint(selected);
    });
    jQuery(document).on('shown.bs.tab', 'a[data-toggle="tab"],a[data-bs-toggle="tab"]', () => {
      closeMenu();
      window.scrollTo(0, 0);
    });

    plate.addEventListener('contextmenu', event => {
      const well = event.target.closest('.well');
      if (!well || !selected.has(well.dataset.well)) return;
      event.preventDefault();
      openMenu(well, event);
    });
    area.addEventListener('keydown', event => {
      if (event.key === 'Escape' && !menu.hidden) {
        event.preventDefault();
        closeMenu(true);
      }
      const well = event.target.closest('.well');
      if (!well || !selected.has(well.dataset.well)) return;
      if (event.key === 'ContextMenu' || (event.shiftKey && event.key === 'F10')) {
        event.preventDefault();
        openMenu(well);
      }
    });
    document.addEventListener('pointerdown', event => {
      if (!menu.contains(event.target)) closeMenu();
    });
    menu.addEventListener('click', event => {
      if (event.target.closest('button')) closeMenu(true);
    });

    document.addEventListener('click', event => {
      if (!event.target.closest('#confirm_group')) return;
      const name = document.getElementById('group_name').value;
      const group = document.getElementById('active_group').value;
      Shiny.setInputValue('confirm_group', { name, group }, { priority: 'event' });
    });
    area.addEventListener('click', event => {
      if (menu.contains(event.target) || (suppressClick && event.detail > 0)) return;
      const well = event.target.closest('.well');
      if (!well) {
        if (selected.size) commit(new Set());
        return;
      }
      const next = new Set(selected), id = well.dataset.well;
      if (next.has(id)) next.delete(id);
      else next.add(id);
      commit(next);
    });
    area.addEventListener('pointerdown', event => {
      const well = event.target.closest('.well');
      suppressClick = false;
      if (!well || event.button !== 0 || event.pointerType === 'touch') return;
      drag = { id: event.pointerId, start: well, end: well, append: event.shiftKey, moved: false };
    });
    window.addEventListener('pointermove', event => {
      if (!drag || drag.id !== event.pointerId) return;
      if (!(event.buttons & 1)) { cancelDrag(); return; }
      const hit = document.elementFromPoint(event.clientX, event.clientY)?.closest('#plate .well');
      if (!hit || hit === drag.end) return;
      drag.end = hit;
      drag.moved = true;
      paint(rectangle());
    });
    window.addEventListener('pointerup', event => {
      if (!drag || drag.id !== event.pointerId) return;
      if (drag.moved) {
        suppressClick = true;
        commit(rectangle());
      }
      drag = null;
    });
    function cancelDrag() {
      if (!drag) return;
      drag = null;
      paint(selected);
    }
    window.addEventListener('pointercancel', cancelDrag);
    window.addEventListener('blur', cancelDrag);
    document.getElementById('issue_details').addEventListener('toggle', event => {
      Shiny.setInputValue('show_import_issues', event.target.open);
    });
    paint(selected);
  });
})();
