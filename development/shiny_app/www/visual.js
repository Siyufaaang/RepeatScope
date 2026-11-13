/* Navigation and decoration only; all data actions use existing Shiny bindings. */
document.addEventListener('DOMContentLoaded', () => {
  Shiny.addCustomMessageHandler('demo-load', message => document.getElementById('load_demo')?.click());
  document.addEventListener('click', event => {
    const trigger = event.target.closest('[data-trigger]');
    if (trigger) document.getElementById(trigger.dataset.trigger)?.click();
    const home = event.target.closest('[data-home-section]');
    if (home) {
      event.preventDefault();
      Shiny.setInputValue('home_section', {section:home.dataset.homeSection, nonce:Date.now()}, {priority:'event'});
    }
  });
  Shiny.addCustomMessageHandler('landing-scroll', id => {
    requestAnimationFrame(() => requestAnimationFrame(() => {
      const target=document.getElementById(id);
      if(target) target.scrollIntoView({behavior:matchMedia('(prefers-reduced-motion: reduce)').matches?'instant':'smooth',block:'start'});
    }));
  });
});
