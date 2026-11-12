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
  function bindSlider() {
    const slider = window.jQuery && jQuery('#threshold').data('ionRangeSlider');
    if (!slider || slider.repeatScopeThresholdBound) return;
    slider.repeatScopeThresholdBound = true;
    const originalFinish = slider.options.onFinish;
    slider.update({onFinish: function (data) {
      if (typeof originalFinish === 'function') originalFinish.call(this, data);
      send(Number(data.from), 'slider');
    }});
  }
  document.addEventListener('DOMContentLoaded', function () {
    if (window.jQuery) jQuery(document).on('shiny:bound shiny:connected', bindSlider);
    bindSlider();
  });
})();
