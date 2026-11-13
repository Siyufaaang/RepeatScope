/* A frozen synthetic trace is the only data source. No Shiny inputs or fitting. */
(function(root){
  'use strict';
  function learningFrames(t,method){
    const modal=t.peaks.find(p=>p.Size===t.modal).Peak_ID;
    const first={caption:'Anchor the target modal.',accepted:[modal],readout:'Modal '+t.modal.toFixed(2)+' bp · '+t.modalHeight+' RFU · relative step k = 0.'};
    const last=(which)=>({caption:'Apply the height filter, then calculate EI.',accepted:t[which+'EI'].ids.slice(),threshold:true,value:t[which+'EI'].ei,
      readout:t[which+'EI'].numerator+' / '+t[which+'EI'].denominator+' = '+t[which+'EI'].ei.toFixed(6)+' repeat steps.'});
    if(method==='ei')return [first,
      {caption:'Start with peaks assigned by the method.',accepted:t.adaptive.map(p=>p.Peak_ID),readout:'This EI example uses the Adaptive assignment. Bayesian uses the same formula.'},
      {caption:'Keep peaks at or above 3% of this modal.',accepted:t.adaptiveEI.ids.slice(),threshold:true,readout:'Threshold = 0.03 × '+t.modalHeight+' = '+(t.threshold*t.modalHeight)+' RFU; equality is included.'},
      {caption:'Weight each included repeat step by its height.',accepted:t.adaptiveEI.ids.slice(),threshold:true,weights:true,readout:'Σ Hₖ k = '+t.adaptiveEI.numerator+' RFU·steps; Σ Hₖ = '+t.adaptiveEI.denominator+' RFU.'},
      last('adaptive')];
    const frames=[first],maxK=Math.max(...t.adaptive.map(p=>p.Delta));
    if(method==='adaptive'){
      for(let k=1;k<=maxK;k++){
        const hit=t.adaptive.find(p=>p.Delta===k);
        if(!hit)continue; // this particular synthetic trace has no missing adaptive steps
        frames.push({delta:k,caption:'Step '+k+': search, select, update.',low:hit.Expected_size_bp-t.tolerance,high:hit.Expected_size_bp+t.tolerance,
          accepted:t.adaptive.filter(p=>p.Delta<=k).map(p=>p.Peak_ID),current:hit.Peak_ID,
          readout:'k = '+k+' · expected '+hit.Expected_size_bp.toFixed(3)+' bp · spacing used '+hit.Local_repeat_spacing_bp.toFixed(4)+' bp · window ±'+t.tolerance+' bp.'});
      }
    }else{
      const seeds=t.bayesian.seedAudit;
      for(const seed of seeds)frames.push({delta:seed.Delta,caption:'Calibrate early seed '+seed.Delta+'.',low:seed.Low,high:seed.High,status:seed.Status,current:seed.Chosen_ID,
        accepted:[modal,...seeds.filter(x=>x.Delta<=seed.Delta&&x.Status==='accepted_seed').map(x=>x.Chosen_ID)],
        readout:'Sequential seed window '+seed.Low.toFixed(3)+'–'+seed.High.toFixed(3)+' bp · '+seed.Status.replaceAll('_',' ')+'.'});
      frames.push({caption:'Freeze the seed posterior.',accepted:[modal,...t.bayesian.seeds],readout:t.bayesian.seeds.length+' accepted seeds · posterior spacing μ = '+t.bayesian.fit.mean.toFixed(4)+' bp. Later peaks do not refit it.'});
      for(const pred of t.bayesian.predictions.filter(p=>p.Delta>seeds.length&&p.Delta<=maxK))frames.push({delta:pred.Delta,caption:pred.Status==='unique_candidate'?'Step '+pred.Delta+': one candidate inside the interval.':'Step '+pred.Delta+': '+pred.Status.replaceAll('_',' ')+'.',
        low:pred.Low,high:pred.High,status:pred.Status,current:pred.Chosen_ID,
        accepted:t.bayesian.selected.filter(p=>p.Delta<=pred.Delta).map(p=>p.Peak_ID),
        readout:'95% predictive interval '+pred.Low.toFixed(3)+'–'+pred.High.toFixed(3)+' bp. '+(pred.Status==='no_candidate'?'No peak assigned at this step.':'Posterior remains frozen.')});
    }
    frames.push(last(method));return frames;
  }
  if(typeof module!=='undefined'&&module.exports)module.exports={learningFrames};
  if(!root.document)return;
  function setup(){
    const host=document.getElementById('methodology');if(!host)return;
    const q=s=>host.querySelector(s),tabs=[...host.querySelectorAll('[data-learn-tab]')];
    let trace,method='ei',frames=[],step=0,timer=null;
    const reduced=matchMedia('(prefers-reduced-motion: reduce)'),plots=[];
    function stop(){if(timer!==null)clearInterval(timer);timer=null;q('[data-learn-play]').textContent='Play';q('[data-learn-play]').setAttribute('aria-pressed','false');}
    const ns='http://www.w3.org/2000/svg';
    function svg(tag,attrs,parent,text){const e=document.createElementNS(ns,tag);for(const [k,v]of Object.entries(attrs||{}))e.setAttribute(k,v);if(text!==undefined)e.textContent=text;parent.append(e);return e;}
    function makePlot(node,limit,height,key){
      const left=50,right=660,top=18,bottom=height-35;
      const x=v=>left+(v-(trace.modal-2))/(trace.upper-trace.modal+4)*(right-left),y=v=>bottom-v/limit*(bottom-top);
      node.replaceChildren();svg('title',{},node,key==='main'?'Synthetic A04, full RFU scale':'Same peaks, 0–1000 RFU tail detail');
      const defs=svg('defs',{},node),clip=svg('clipPath',{id:'learning-'+key+'-clip'},defs);svg('rect',{x:left,y:top,width:right-left,height:bottom-top},clip);
      for(const v of [0,limit/2,limit]){svg('line',{x1:left,x2:right,y1:y(v),y2:y(v),class:'learning-grid'},node);svg('text',{x:left-8,y:y(v)+4,'text-anchor':'end',class:'learning-axis'},node,String(v));}
      for(const v of [210,225,240,255,265])svg('text',{x:x(v),y:bottom+18,'text-anchor':'middle',class:'learning-axis'},node,String(v));
      svg('text',{x:10,y:11,class:'learning-axis'},node,'RFU');svg('text',{x:right,y:height-2,'text-anchor':'end',class:'learning-axis'},node,'Size (bp)');
      const group=svg('g',{'clip-path':'url(#learning-'+key+'-clip)'},node);
      const window=svg('rect',{x:left,y:top,width:0,height:bottom-top,class:'learning-search'},group);
      const peakNodes=trace.peaks.map(p=>{
        const g=svg('g',{'data-teaching-peak':p.Peak_ID},group);
        svg('title',{},g,p.Size.toFixed(3)+' bp · '+p.Height+' RFU');
        const line=svg('line',{x1:x(p.Size),x2:x(p.Size),y1:bottom,y2:y(p.Height),class:'learning-peak'},g);
        const marker=svg(p.Size===trace.modal?'rect':'circle',p.Size===trace.modal?{x:x(p.Size)-3,y:y(p.Height)-3,width:6,height:6}:{cx:x(p.Size),cy:y(p.Height),r:3},g);
        marker.setAttribute('class','learning-peak-marker');return {p,line,marker};
      });
      const threshold=svg('line',{x1:left,x2:right,y1:y(trace.threshold*trace.modalHeight),y2:y(trace.threshold*trace.modalHeight),class:'learning-threshold'},group);
      const thresholdText=svg('text',{x:right-3,y:y(trace.threshold*trace.modalHeight)-6,'text-anchor':'end',class:'learning-axis'},node,'3% = 300 RFU');
      const deltaText=svg('text',{x:left+8,y:top+14,class:'learning-step-label'},node,'');
      return {x,window,peakNodes,threshold,thresholdText,deltaText,left,right};
    }
    function render(){
      const f=frames[step];if(!f)return;
      host.dataset.learnMethod=method;host.classList.toggle('reduce-learning-motion',reduced.matches);
      q('[data-learn-caption]').textContent=f.caption;q('[data-learn-readout]').textContent=f.readout;
      q('[data-learn-step]').textContent='Step '+(step+1)+' / '+frames.length;
      q('[data-learn-assignment]').textContent=(method==='ei'?'Adaptive-assigned peaks':'SYNTHETIC A04')+' · Extended · 3% threshold';
      q('[data-learn-prev]').disabled=step===0;q('[data-learn-next]').disabled=step===frames.length-1;
      q('[data-learn-play]').disabled=false;q('[data-learn-reset]').disabled=false;
      const progress=q('[role=progressbar]');progress.setAttribute('aria-valuemax',frames.length);progress.setAttribute('aria-valuenow',step+1);
      q('[data-learn-progress]').style.width=((step+1)/frames.length*100)+'%';
      const chosen=new Set(f.accepted);
      for(const plot of plots){
        const finite=Number.isFinite(f.low)&&Number.isFinite(f.high),start=finite?Math.max(plot.left,plot.x(f.low)):plot.left,end=finite?Math.min(plot.right,plot.x(f.high)):start;
        plot.window.style.x=start+'px';plot.window.style.width=Math.max(0,end-start)+'px';plot.window.style.opacity=finite?'1':'0';
        for(const {p,line,marker} of plot.peakNodes){
          const modal=p.Size===trace.modal,accepted=chosen.has(p.Peak_ID);
          line.classList.toggle('is-modal',modal);line.classList.toggle('is-accepted',accepted&&!modal);line.classList.toggle('is-current',p.Peak_ID===f.current);
          marker.classList.toggle('is-modal',modal);marker.classList.toggle('is-accepted',accepted&&!modal);marker.style.opacity=accepted?'1':'0';
        }
        plot.threshold.style.opacity=f.threshold?'1':'0';plot.thresholdText.style.opacity=f.threshold?'1':'0';plot.deltaText.textContent=f.delta?'k = '+f.delta:f.weights?'Each height weights its assigned step':'';
      }
    }
    function select(name,focus=false){stop();method=name;step=0;tabs.forEach(t=>{const active=t.dataset.learnTab===name;t.setAttribute('aria-selected',String(active));t.tabIndex=active?0:-1;document.getElementById(t.getAttribute('aria-controls')).hidden=!active;if(active&&focus)t.focus();});if(trace){frames=learningFrames(trace,method);render();}}
    function play(){if(!trace)return;if(timer!==null){stop();return;}if(step===frames.length-1)step=0;render();q('[data-learn-play]').textContent='Pause';q('[data-learn-play]').setAttribute('aria-pressed','true');timer=setInterval(()=>{step++;render();if(step>=frames.length-1)stop();},1600);}
    tabs.forEach((tab,i)=>{tab.addEventListener('click',()=>select(tab.dataset.learnTab));tab.addEventListener('keydown',e=>{let next;if(e.key==='ArrowRight')next=(i+1)%tabs.length;if(e.key==='ArrowLeft')next=(i+tabs.length-1)%tabs.length;if(e.key==='Home')next=0;if(e.key==='End')next=tabs.length-1;if(next!==undefined){e.preventDefault();select(tabs[next].dataset.learnTab,true);}});});
    q('[data-learn-play]').addEventListener('click',play);
    q('[data-learn-next]').addEventListener('click',()=>{stop();step=Math.min(step+1,frames.length-1);render();});
    q('[data-learn-prev]').addEventListener('click',()=>{stop();step=Math.max(step-1,0);render();});
    q('[data-learn-reset]').addEventListener('click',()=>{stop();step=0;render();play();});
    document.addEventListener('visibilitychange',()=>{if(document.hidden)stop();});
    new IntersectionObserver(entries=>{if(!entries[0].isIntersecting)stop();}).observe(q('.learning-player'));
    reduced.addEventListener('change',()=>{stop();if(trace)render();});
    fetch('method-trace.json').then(r=>{if(!r.ok)throw Error('trace');return r.json();}).then(t=>{
      trace=t;plots.push(makePlot(q('[data-learn-plot]'),10000,235,'main'),makePlot(q('[data-learn-zoom]'),1000,165,'zoom'));select('ei');
    }).catch(()=>{q('[data-learn-caption]').textContent='Animation unavailable.';q('[data-learn-readout]').textContent='The equations and parameter keys remain available. Reload to retry the synthetic walkthrough.';q('[data-learn-step]').textContent='';});
  }
  if(document.readyState==='loading')document.addEventListener('DOMContentLoaded',setup);else setup();
})(typeof window==='undefined'?globalThis:window);
