(function () {
  let sequence = 0;
  function send(value, source) {
    if (!window.Shiny || !Number.isFinite(value) || value < 0 || value > 100) return;
    Shiny.setInputValue('threshold_change', {value, source, nonce: ++sequence}, {priority: 'event'});
  }
  // Native edits only: Shiny's numeric update uses jQuery.trigger('change'),
  // which must not send an earlier server value back as a fresh user edit.
  function numberEdited(event) {
    if (event.target.id === 'threshold_number' && event.target.value !== '') {
      send(Number(event.target.value), 'number');
    }
  }
  document.addEventListener('input', numberEdited);
  document.addEventListener('change', numberEdited);
})();
