/* Delegated events cover Shiny-rendered fields and modal dialogs. No input writes. */
(() => {
  function setup(){
    const tip=document.createElement('div');tip.id='rs-field-tooltip';tip.setAttribute('role','tooltip');tip.hidden=true;document.body.append(tip);
    let current=null,pinned=false,closeTimer=null,hovered=false;
    function cancel(){clearTimeout(closeTimer);closeTimer=null;}
    function close(){cancel();if(current)current.setAttribute('aria-expanded','false');tip.hidden=true;current=null;pinned=false;}
    function place(){
      if(!current || !current.isConnected || !current.getClientRects().length){close();return;}
      const b=current.getBoundingClientRect(),t=tip.getBoundingClientRect(),pad=12;
      const x=Math.max(pad,Math.min(b.right-t.width,innerWidth-t.width-pad));
      const y=b.bottom+9+t.height<innerHeight-pad ? b.bottom+9 : Math.max(pad,b.top-t.height-9);
      tip.style.left=x+'px';tip.style.top=y+'px';
    }
    function open(button){
      const source=document.getElementById(button.dataset.fieldHelp);if(!source)return;
      cancel();if(current && current!==button){current.setAttribute('aria-expanded','false');pinned=false;}
      current=button;tip.innerHTML=source.innerHTML;tip.hidden=false;button.setAttribute('aria-expanded','true');place();
    }
    function schedule(){cancel();closeTimer=setTimeout(()=>{if(!pinned&&!hovered&&document.activeElement!==current)close();},180);}
    document.addEventListener('pointerover',e=>{const b=e.target.closest('.field-help-trigger');if(b){hovered=true;open(b);}else if(tip.contains(e.target)){hovered=true;cancel();}});
    document.addEventListener('pointerout',e=>{if(e.target.closest('.field-help-trigger')||tip.contains(e.target)){hovered=false;schedule();}});
    document.addEventListener('focusin',e=>{const b=e.target.closest('.field-help-trigger');if(b)open(b);});
    document.addEventListener('focusout',e=>{if(e.target===current)schedule();});
    document.addEventListener('click',e=>{
      const b=e.target.closest('.field-help-trigger');
      if(b){e.preventDefault();e.stopPropagation();if(current===b&&pinned)close();else{open(b);pinned=true;}}
      else if(!tip.contains(e.target))close();
    });
    document.addEventListener('keydown',e=>{if(e.key==='Escape'&&current){close();e.stopPropagation();}});
    document.addEventListener('scroll',()=>{if(current)place();},true);
    window.addEventListener('resize',()=>{if(current)place();});
    document.addEventListener('visibilitychange',()=>{if(document.hidden)close();});
    new MutationObserver(()=>{if(current&&(!current.isConnected||!current.getClientRects().length))close();}).observe(document.body,{childList:true,subtree:true});
  }
  if(document.readyState==='loading')document.addEventListener('DOMContentLoaded',setup);else setup();
})();
