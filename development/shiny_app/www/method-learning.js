/* Frozen synthetic R trace only. Physical peaks never move during selection. */
(function(root){
  'use strict';
  // Display transform only. Threshold decisions and EI retain original RFU.
  function learningY(height,limit,top,bottom){return bottom-Math.sqrt(height/limit)*(bottom-top);}
  function learningFrames(t,method){
    const modal=t.peaks.find(p=>p.Size===t.modal).Peak_ID,floor=t.threshold*t.modalHeight;
    const low=t.peaks.filter(p=>p.Height<floor).length;
    const common={accepted:[modal],intervals:[],threshold:false,filtered:false,duration:3500};
    const intro=[
      {...common,phase:'inventory',caption:'Start with every measured peak.',duration:4000,readout:t.peaks.length+' physical peaks. Size and height are observed, before either method assigns them.'},
      {...common,phase:'zoom',caption:'See the modal and expansion tail together.',readout:'Square-root height scale reveals low peaks without clipping the modal. DNA sizes and RFU values stay unchanged.'},
      {...common,phase:'threshold',threshold:true,caption:'Draw the 3% modal-height threshold.',readout:'0.03 × '+t.modalHeight+' = '+floor+' RFU. Peaks exactly on the line are eligible.'},
      {...common,phase:'filter',threshold:true,filtered:true,caption:'Exclude each low peak; keep checking later peaks.',readout:low+' peak'+(low===1?'':'s')+' below '+floor+' RFU cannot influence seeds, spacing or ambiguity. Later eligible peaks remain candidates; the tail is not cut off.'}
    ];
    const next={...common,threshold:true,filtered:true};
    const last=which=>({...next,phase:'ei',caption:'Calculate EI from the selected peaks.',accepted:t[which+'EI'].ids.slice(),value:t[which+'EI'].ei,duration:4000,
      readout:t[which+'EI'].numerator+' / '+t[which+'EI'].denominator+' = '+t[which+'EI'].ei.toFixed(6)+' repeat steps. Modal included.'});
    if(method==='ei')return [...intro,
      {...next,phase:'select',caption:'Assign only eligible peaks.',accepted:t.adaptiveEI.ids.slice(),readout:'This example uses Adaptive assignments. Bayesian uses the same height-weighted EI formula.'},
      {...next,phase:'weight',caption:'Weight each repeat step by its peak height.',accepted:t.adaptiveEI.ids.slice(),readout:'Σ Hₖ k = '+t.adaptiveEI.numerator+'; Σ Hₖ = '+t.adaptiveEI.denominator+'.'},last('adaptive')];
    if(method==='adaptive')return [...intro,...t.adaptive.filter(p=>p.Delta>0).map(hit=>({...next,phase:'walk',delta:hit.Delta,duration:3000,
      caption:'Step '+hit.Delta+': select an eligible peak, update spacing.',
      intervals:[{delta:hit.Delta,low:hit.Expected_size_bp-t.tolerance,high:hit.Expected_size_bp+t.tolerance}],
      accepted:t.adaptive.filter(p=>p.Delta<=hit.Delta).map(p=>p.Peak_ID),current:hit.Peak_ID,
      readout:'Expected '+hit.Expected_size_bp.toFixed(3)+' bp · spacing '+hit.Local_repeat_spacing_bp.toFixed(4)+' bp · ±'+t.tolerance+' bp. Missing steps keep their original index.'})),last('adaptive')];
    const seedDeltas=new Set(t.bayesian.seedAudit.filter(s=>s.Status==='accepted_seed').map(s=>s.Delta));
    const intervals=t.bayesian.predictions.filter(p=>!seedDeltas.has(p.Delta)).map(p=>({delta:p.Delta,low:p.Low,high:p.High,status:p.Status}));
    return [...intro,
      {...next,phase:'calibrate',caption:'Calibrate from eligible early seeds.',accepted:[modal,...t.bayesian.seeds],
        intervals:t.bayesian.seedAudit.map(s=>({delta:s.Delta,low:s.Low,high:s.High,status:s.Status})),
        readout:t.bayesian.seeds.length+' accepted seeds · posterior spacing μ = '+t.bayesian.fit.mean.toFixed(4)+' bp. Seed RFU and dominance rules still apply.'},
      {...next,phase:'predict',caption:'Predict every tail interval together.',accepted:[modal,...t.bayesian.seeds],intervals,
        readout:'All 95% predictive position intervals use the same frozen posterior. Tail peaks do not update it.'},
      {...next,phase:'select',caption:'Color eligible, uniquely assigned peaks.',accepted:t.bayesianEI.ids.slice(),intervals,
        readout:'Only an eligible unique candidate in a supported, non-overlapping window is selected. Peaks outside the intervals remain gray.'},
      {...last('bayesian'),intervals}];
  }
  if(typeof module!=='undefined'&&module.exports)module.exports={learningFrames,learningY};
  if(!root.document)return;
  function setup(){
    const host=document.getElementById('methodology');if(!host)return;
    const q=s=>host.querySelector(s),tabs=[...host.querySelectorAll('[data-learn-tab]')];
    let trace,method='ei',frames=[],step=0,timer=null,visible=false,userPaused=false;
    const reduced=matchMedia('(prefers-reduced-motion: reduce)');let plot;
    function stop(){if(timer!==null)clearTimeout(timer);timer=null;q('[data-learn-play]').textContent=step===frames.length-1?'Play again':'Play';q('[data-learn-play]').setAttribute('aria-pressed','false');}
    const ns='http://www.w3.org/2000/svg';
    function svg(tag,attrs,parent,text){const e=document.createElementNS(ns,tag);for(const [k,v]of Object.entries(attrs||{}))e.setAttribute(k,v);if(text!==undefined)e.textContent=text;parent.append(e);return e;}
    function makePlot(node,limit,height){
      const left=52,right=659,top=34,bottom=height-38;
      const xmin=trace.modal-1.5,xmax=trace.upper+1.5;
      const x=v=>left+(v-xmin)/(xmax-xmin)*(right-left),y=v=>learningY(v,limit,top+8,bottom);
      node.replaceChildren();svg('title',{},node,'Modal and expansion tail: square-root height scale, ticks in thousands of original RFU');
      const defs=svg('defs',{},node),clip=svg('clipPath',{id:'learning-tail-clip'},defs);svg('rect',{x:left,y:top,width:right-left,height:bottom-top},clip);
      for(const v of [0,100,300,1000,2500,5000,10000]){svg('line',{x1:left,x2:right,y1:y(v),y2:y(v),class:'learning-grid'},node);svg('text',{x:left-8,y:y(v)+4,'text-anchor':'end',class:'learning-axis'},node,String(v/1000));}
      for(let v=Math.ceil(xmin/10)*10;v<xmax;v+=10)svg('text',{x:x(v),y:bottom+18,'text-anchor':'middle',class:'learning-axis'},node,String(v));
      svg('text',{x:left,y:15,class:'learning-axis'},node,'Height (×10³ RFU) · square-root scale');svg('text',{x:right,y:height-2,'text-anchor':'end',class:'learning-axis'},node,'DNA size (bp)');
      const group=svg('g',{'clip-path':'url(#learning-tail-clip)'},node),windows=svg('g',{},group);
      const peakNodes=trace.peaks.map(p=>{
        const g=svg('g',{'data-teaching-peak':p.Peak_ID},group);svg('title',{},g,p.Size.toFixed(3)+' bp · '+p.Height+' RFU');
        const line=svg('line',{x1:x(p.Size),x2:x(p.Size),y1:bottom,y2:y(p.Height),class:'learning-peak'},g);
        const marker=svg('circle',{cx:x(p.Size),cy:y(p.Height),r:3,class:'learning-peak-marker'},g);
        return {p,line,marker};
      });
      const threshold=svg('line',{x1:left,x2:right,y1:y(trace.threshold*trace.modalHeight),y2:y(trace.threshold*trace.modalHeight),class:'learning-threshold'},group);
      const thresholdText=svg('text',{x:right-3,y:y(trace.threshold*trace.modalHeight)-6,'text-anchor':'end',class:'learning-axis learning-threshold-label'},node,'3% = '+(trace.threshold*trace.modalHeight)+' RFU');
      svg('text',{x:x(trace.modal)+10,y:y(trace.modalHeight)+4,class:'learning-axis'},node,'Modal: '+trace.modalHeight.toLocaleString('en-US')+' RFU');
      return {x,windows,peakNodes,threshold,thresholdText,left,right,top,bottom};
    }
    function render(){
      const f=frames[step];if(!f)return;
      host.dataset.learnMethod=method;host.dataset.learnPhase=f.phase;host.classList.toggle('reduce-learning-motion',reduced.matches);
      q('[data-learn-caption]').textContent=f.caption;q('[data-learn-readout]').textContent=f.readout;
      q('[data-learn-step]').textContent=(step+1)+' / '+frames.length;
      q('[data-learn-assignment]').textContent='SYNTHETIC A04 · Extended · 3% before assignment';
      q('[data-learn-inventory]').hidden=f.phase!=='inventory';q('[data-learn-tail]').hidden=f.phase==='inventory';
      q('[data-learn-play]').disabled=false;q('[data-learn-reset]').disabled=false;
      q('[data-learn-prev]').disabled=step===0;q('[data-learn-next]').disabled=step===frames.length-1;
      const progress=q('[role=progressbar]');progress.setAttribute('aria-valuemax',frames.length);progress.setAttribute('aria-valuenow',step+1);
      q('[data-learn-progress]').style.width=((step+1)/frames.length*100)+'%';
      const chosen=new Set(f.accepted),floor=trace.threshold*trace.modalHeight;
      plot.windows.replaceChildren();
      for(const win of f.intervals){
        if(!Number.isFinite(win.low)||!Number.isFinite(win.high))continue;
        const start=Math.max(plot.left,plot.x(win.low)),end=Math.min(plot.right,plot.x(win.high));
        if(end>start)svg('rect',{x:start,y:plot.top,width:end-start,height:plot.bottom-plot.top,class:'learning-search'},plot.windows);
      }
      for(const {p,line,marker} of plot.peakNodes){
        const modal=p.Size===trace.modal,accepted=chosen.has(p.Peak_ID),excluded=f.filtered&&p.Height<floor;
        for(const node of [line,marker]){node.classList.toggle('is-modal',modal);node.classList.toggle('is-accepted',accepted&&!modal);node.classList.toggle('is-excluded',excluded);}
        line.classList.toggle('is-current',p.Peak_ID===f.current);marker.style.opacity=accepted?'1':'0';
      }
      plot.threshold.style.opacity=f.threshold?'1':'0';plot.thresholdText.style.opacity=f.threshold?'1':'0';
    }
    function schedule(){timer=setTimeout(()=>{timer=null;if(step<frames.length-1){step++;render();if(step<frames.length-1)schedule();else stop();}},frames[step].duration);}
    function start(){if(!trace)return;stop();if(step===frames.length-1)step=0;userPaused=false;render();q('[data-learn-play]').textContent='Pause';q('[data-learn-play]').setAttribute('aria-pressed','true');schedule();}
    function select(name,focus=false){stop();method=name;step=0;tabs.forEach(t=>{const active=t.dataset.learnTab===name;t.setAttribute('aria-selected',String(active));t.tabIndex=active?0:-1;document.getElementById(t.getAttribute('aria-controls')).hidden=!active;if(active&&focus)t.focus();});if(trace){frames=learningFrames(trace,method);if(reduced.matches)step=frames.length-1;render();if(visible&&!userPaused&&!reduced.matches&&!document.hidden)start();}}
    tabs.forEach((tab,i)=>{tab.addEventListener('click',()=>select(tab.dataset.learnTab));tab.addEventListener('keydown',e=>{let next;if(e.key==='ArrowRight')next=(i+1)%tabs.length;if(e.key==='ArrowLeft')next=(i+tabs.length-1)%tabs.length;if(e.key==='Home')next=0;if(e.key==='End')next=tabs.length-1;if(next!==undefined){e.preventDefault();select(tabs[next].dataset.learnTab,true);}});});
    q('[data-learn-play]').addEventListener('click',()=>{if(timer!==null){userPaused=true;stop();}else start();});
    q('[data-learn-reset]').addEventListener('click',()=>{step=0;start();});
    function moveStep(offset){if(!trace)return;userPaused=true;stop();step=Math.max(0,Math.min(frames.length-1,step+offset));render();stop();}
    q('[data-learn-prev]').addEventListener('click',()=>moveStep(-1));
    q('[data-learn-next]').addEventListener('click',()=>moveStep(1));
    document.addEventListener('visibilitychange',()=>{if(document.hidden)stop();else if(visible&&!userPaused&&!reduced.matches&&step<frames.length-1)start();});
    new IntersectionObserver(entries=>{visible=entries[0].isIntersecting;if(!visible)stop();else if(trace&&!userPaused&&!reduced.matches&&step<frames.length-1&&!document.hidden)start();},{threshold:.15}).observe(q('.learning-player'));
    reduced.addEventListener('change',()=>{stop();if(trace){if(reduced.matches)step=frames.length-1;render();}});
    fetch('method-trace.json').then(r=>{if(!r.ok)throw Error('trace');return r.json();}).then(t=>{
      trace=t;plot=makePlot(q('[data-learn-zoom]'),Math.max(...t.peaks.map(p=>p.Height)),340);
      const body=q('[data-learn-table]');for(const [i,p] of t.peaks.entries()){
        const row=document.createElement('tr');for(const v of [i+1,p.Size.toFixed(2),p.Height]){const cell=document.createElement('td');cell.textContent=String(v);row.append(cell);}body.append(row);
      }select('ei');
    }).catch(()=>{q('[data-learn-caption]').textContent='Animation unavailable.';q('[data-learn-readout]').textContent='Reload to retry the synthetic walkthrough.';q('[data-learn-step]').textContent='';});
  }
  if(document.readyState==='loading')document.addEventListener('DOMContentLoaded',setup);else setup();
})(typeof window==='undefined'?globalThis:window);
