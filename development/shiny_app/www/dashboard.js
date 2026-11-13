function dashboardTileSummary(w,method='a',stale=false) {
  if (!['sample','positive_control'].includes(w.role)) return {value:'',count:0};
  if (stale && w.acquired) return {value:'Rerun',count:0};
  const records=w.recordList || w.records || [];
  const nonNormal=records.filter(r=>r.kind!=='normal' && r[method+'_status']!=='not_applicable_normal');
  const short=s=>({target_ambiguous:'Review',target_conflict:'Review',target_overlap:'Review',target_review:'Review',
    target_missing:'No target',missing_target:'No target',not_fitted:'Not run',stale_results:'Rerun'})[s] || 'Unavailable';
  if(!nonNormal.length) return {value:records.length && records.every(r=>r[method+'_status']==='not_applicable_normal')?'Normal':w.acquired?'Review':'No peaks',count:0};
  const values=nonNormal.map(r=>Number.isFinite(r[method])?r[method].toFixed(3):short(r[method+'_status']));
  return {value:values.length<=2?values.join(' / '):values.length+' results',count:values.length};
}
function dashboardRecordIndex(w) {
  return Math.max(0,w.recordList.findIndex(r=>r.id===w.id));
}
Shiny.addCustomMessageHandler('dashboard-data', payload => {
  const container = document.getElementById('report-dashboard');
  const template = document.getElementById('dashboard-template');
  const reopen = Boolean(container.querySelector('dialog[open]'));
  container.replaceChildren(template.content.cloneNode(true));
  const root = document.getElementById('rs-plate-report-sketch');
  const find = s => root.querySelector(s);
  const wells = payload.wells.map(w => {
    w.records = w.records || [];
    const rec = w.records.find(r=>['expanded','intermediate','research'].includes(r.kind)) || w.records[0] || {};
    return Object.assign(w,rec,{recordList:w.records, status:rec.status || '', all:rec.all || [], chart:{peak:rec.all || [],modal:rec.modal}});
  });
  find('[data-gene]').textContent = payload.gene || 'Plate';
  find('[data-well-count]').textContent = wells.length;
  find('[data-role-count]').textContent = wells.filter(w=>w.role==='sample').length+' samples · '+wells.filter(w=>w.role==='ce_fill').length+' CE Fill';
  find('[data-threshold]').textContent = payload.threshold+'%';
  find('[data-window]').textContent = payload.window;
  if(payload.stale) find('[data-gene]').textContent += ' · FIT NEEDS UPDATING';
  const board = find('[data-board]');
  const detail = find('[data-detail]');
  const rows = 'ABCDEFGH'.split('');
  const cols = Array.from({length:12}, (_, i) => String(i + 1).padStart(2, '0'));
  const byWell = new Map(wells.map(w => [w.w, w]));
  const paired = w => Number.isFinite(w.a) && Number.isFinite(w.b);
  const fmt = x => x.toFixed(3);
  const different = w => (w.recordList || [w]).some(r => paired(r) && fmt(r.a) !== fmt(r.b));
  const records = wells.flatMap(w=>w.recordList.map(r=>Object.assign({},w,r,{chart:{peak:r.all || [],modal:r.modal}})));
  const pairs = records.filter(w => w.role === 'sample' && paired(w));
  const comparableWells = wells.filter(w => w.role === 'sample' && w.recordList.some(paired));
  const diffCount = comparableWells.filter(different).length;
  const note = find('[data-report-note]');
  if (payload.stale) {
    note.textContent = 'Results outdated — apply any edited filter, then run analysis. Imported peaks remain visible; old EI and old modal are hidden.';
    note.hidden = false;
  } else if (!wells.some(w => w.role === 'sample')) {
    note.textContent = 'No wells have the Sample role. Return to Arrange plate, select sample wells, and choose Role → Biological Sample.';
    note.hidden = false;
  } else if (!comparableWells.length) {
    note.textContent = 'No paired EI previews are available. Select a well to see its status and target.';
    note.hidden = false;
  }
  const canFold = w => w.role === 'ce_fill' && !w.acquired;
  const emptyRows = rows.filter(r => cols.every(c => canFold(byWell.get(r + c))));
  const emptyCols = cols.filter(c => rows.every(r => canFold(byWell.get(r + c))));
  const groups = [...new Set(wells.map(w=>w.group).filter(Boolean))];
  const groupClass = w => w.role === 'ce_fill' ? 'ce' : (w.group ? 'g'+(groups.indexOf(w.group)%3+1) : '');
  for(const name of groups){const item=el('span'); item.append(el('i','swatch '+groupClass({group:name})),document.createTextNode(name));find('[data-group-key]').append(item);}
  const ns = 'http://www.w3.org/2000/svg';
  let state = window.rsDashboardState || {view:'tail', diff:false, fold:false, well:null, expanded:[]};
  if(!['a','b'].includes(state.method))state.method='a';
  find('[data-comparisons]').open=Boolean(state.comparisons);

  function el(tag, cls, label) {
    const e = document.createElement(tag);
    if (cls) e.className = cls;
    if (label != null) e.textContent = label;
    return e;
  }
  function svg(tag, attrs, parent) {
    const e = document.createElementNS(ns, tag);
    Object.entries(attrs).forEach(([k,v]) => e.setAttribute(k,v));
    if (parent) parent.append(e);
    return e;
  }
  function line(parent,x1,y1,x2,y2,stroke,width=1) {
    return svg('line',{x1,y1,x2,y2,stroke,'stroke-width':width},parent);
  }
  function label(parent,x,y,content,attrs={}) {
    const t = svg('text',{x,y,fill:'var(--muted)','font-size':11,...attrs},parent);
    t.textContent = content;
  }
  function persist() {
    window.rsDashboardState = state;
  }
  function restore(saved) {
    const next = saved?.privateContent;
    if (next && ['all','tail'].includes(next.view) && Array.isArray(next.expanded)) state = next;
    update();
  }

  function displayPeaks(w) {
    if (state.view === 'all') {
      return w.all || [];
    }
    if (!Number.isFinite(w.modal)) return w.all || [];
    return w.chart.peak.filter(p => p[0] >= w.modal-1 && p[0] <= w.upper+1);
  }
  function plot(w, peaks, method = null) {
    const mini = method === null;
    const W = mini ? 65 : 310, H = mini ? 25 : 168;
    const margin = mini ? {l:1,r:1,t:1,b:2} : {l:43,r:9,t:22,b:34};
    const chart = svg('svg',{viewBox:'0 0 '+W+' '+H,preserveAspectRatio:mini?'none':'xMidYMid meet'});
    if (mini) chart.setAttribute('aria-hidden','true');
    else {
      chart.setAttribute('role','img');
      chart.setAttribute('aria-label',w.w+' '+method+' peak heights in RFU by fragment size in bp');
    }
    const lo = Math.min(...peaks.map(p => p[0]));
    const hi = Math.max(lo + 1,...peaks.map(p => p[0]));
    const top = Math.max(1,...peaks.map(p => p[1]));
    const bottom = H - margin.b;
    const x = v => margin.l + (v-lo)/(hi-lo)*(W-margin.r-margin.l);
    const y = v => bottom - v/top*(bottom-margin.t);
    line(chart,margin.l,bottom,W-margin.r,bottom,'var(--line)');
    if (!mini) {
      line(chart,margin.l,margin.t,margin.l,bottom,'var(--line)');
      for (const f of [0,.5,1]) {
        const xv = lo + f*(hi-lo);
        label(chart,x(xv),bottom+13,Math.round(xv),{'text-anchor':f===0?'start':f===1?'end':'middle'});
        label(chart,margin.l-4,y(f*top)+4,Math.round(f*top),{'text-anchor':'end'});
      }
      label(chart,(margin.l+W-margin.r)/2,H-3,'Size (bp)',{'text-anchor':'middle'});
      label(chart,0,11,'RFU');
    }
    for (const [bp,rfu,a,b] of peaks) {
      const A = a === 1, B = b === 1;
      const ink = bp===w.modal ? '--modal' : mini ? (A && B ? '--both' : A ? '--adaptive' : B ? '--bayes' : '--muted') :
        (method==='Adaptive' && A ? '--adaptive' : method==='Bayesian' && B ? '--bayes' : '--muted');
      const stick = line(chart,x(bp),bottom,x(bp),y(rfu),'var('+ink+')',ink==='--muted' ? .75 : 1.5);
      const tip=svg('title',{},stick);tip.textContent=bp.toFixed(2)+' bp / '+rfu+' RFU'+(bp===w.modal?' · modal':'');
      if (ink==='--muted' && paired(w)) stick.setAttribute('opacity','.45');
      if (mini && state.view==='tail' && A !== B) svg('circle',{cx:x(bp),cy:y(rfu),r:1.1,fill:'var('+ink+')'},chart);
    }
    return chart;
  }

  function showWell(w, recordIndex = dashboardRecordIndex(w)) {
    const select = find('[data-record-select]');
    select.replaceChildren();
    w.recordList.forEach((r,i)=>{const o=document.createElement('option');o.value=i;o.textContent=r.acquisition+' · '+r.allele;select.append(o);});
    select.hidden=w.recordList.length<2;
    select.value=recordIndex;
    select.onchange=()=>showWell(w,Number(select.value));
    const rec=w.recordList[recordIndex];
    if(rec) w=Object.assign({},w,rec,{chart:{peak:rec.all || [],modal:rec.modal}});
    state.well = w.w;
    state.record = rec?.id;
    if(rec) Shiny.setInputValue('dashboard_record',rec.id,{priority:'event'});
    board.querySelectorAll('.tile').forEach(b => b.classList.toggle('selected',b.dataset.well===w.w));
    find('#rs-plate-detail-heading').textContent = w.w+' · '+(w.name || 'CE Fill');
    find('[data-detail-group]').textContent = [w.group,w.role,w.acquisition,w.allele].filter(Boolean).join(' · ');
    find('[data-target-note]').textContent = w.target || 'No target yet.';
    find('[data-scope-note]').textContent = payload.filter+' · '+payload.window+' · '+payload.threshold+'% own modal'+(Number.isFinite(w.modal_rfu)?' = '+(w.modal_rfu*payload.threshold/100).toFixed(2)+' RFU':'')+(Number.isFinite(w.upper)?' · support through '+w.upper.toFixed(2)+' bp':'');
    find('[data-formal-note]').textContent = 'Formal eligibility: '+(w.formal || 'not reviewed');
    const candidates=find('[data-candidate-select]');candidates.replaceChildren();
    const placeholder=el('option','','Choose a candidate');placeholder.value='';candidates.append(placeholder);
    (w.candidates || []).filter(c=>c.id).forEach(c=>{const o=el('option','',c.size.toFixed(2)+' bp · '+c.rfu+' RFU');o.value=c.id;candidates.append(o);});
    candidates.value=w.selected_peak || '';
    find('[data-apply-target]').textContent='Review selected candidate';
    find('[data-apply-target]').onclick=()=>{if(candidates.value){Shiny.setInputValue('dashboard_target',{record:w.id,peak:candidates.value,nonce:Date.now()},{priority:'event'});detail.close();document.getElementById('manual_target_kind')?.scrollIntoView({block:'center'});}};
    const holder = find('[data-detail-plots]');
    holder.replaceChildren();
    const shown = displayPeaks(w);
    const peaks = shown.length || paired(w) ? shown : (w.all || []);
    if (peaks.length) {
      const methods = [['Adaptive',w.a,w.a_reason],['Bayesian',w.b,w.b_reason]];
      for (const [method,ei,reason] of methods) {
        const panel = el('section');
        panel.append(el('h3','',method+' · Preview EI '+(Number.isFinite(ei)?ei.toFixed(6):'unavailable')),plot(w,peaks,method));
        if (reason) panel.append(el('small','',reason));
        holder.append(panel);
      }
    } else holder.append(el('p','',paired(w)?'No expansion-tail peaks':'No sizing peaks'));
    if (!detail.open) detail.showModal();
    persist();
  }

  function tile(w) {
    const button = el('button',['tile',groupClass(w),different(w)?'different':'',state.well===w.w?'selected':''].join(' '));
    button.type='button';button.dataset.well=w.w;
    const name=w.role==='ce_fill'?'':(w.name || (w.role==='unlabelled'?'':w.role));
    const summary=dashboardTileSummary(w,state.method,payload.stale);
    const method=state.method==='a'?'Adaptive':'Bayesian';
    button.dataset.tooltip=[w.w,name,method+' EI '+summary.value].filter(Boolean).join(' · ');
    button.setAttribute('aria-label',button.dataset.tooltip);button.title=button.dataset.tooltip;
    const value=el('span','value',summary.value);
    value.classList.toggle('status-value',!/^\d/.test(summary.value));
    value.classList.toggle('multiple-values',summary.count>1);
    if(summary.count>1)value.title=summary.count+' separate target results; open the well for identities';
    button.append(el('span','well-id',w.w),el('span','name',name.replace(/^DNA_/,'')),value);
    button.addEventListener('click',()=>showWell(w));return button;
  }
  function band(key,cls) {
    const button = el('button','band '+cls,key.slice(3));
    button.type = 'button';
    button.setAttribute('aria-label','Expand '+(key.startsWith('col')?'column ':'row ')+key.slice(3));
    button.addEventListener('click',()=>{
      state.expanded.push(key);
      update();
      persist();
    });
    return button;
  }
  function drawBoard() {
    const folded = (key, list) => state.fold && list.includes(key.slice(3)) && !state.expanded.includes(key);
    const colFolded = c => folded('col'+c,emptyCols);
    board.replaceChildren();
    board.style.gridTemplateColumns = '16px '+cols.map(c=>colFolded(c)?'14px':'minmax(46px,1fr)').join(' ');
    board.style.setProperty('--board-min', (16+3*cols.length+cols.reduce((n,c)=>n+(colFolded(c)?14:46),0))+'px');
    board.style.setProperty('--board-rows', rows.length);
    board.classList.toggle('only-differences',state.diff);
    board.append(el('span'));
    cols.forEach((c,i)=>{
      board.append(el('span','axis',colFolded(c)?'':String(Number(c))));
      if (colFolded(c)) {
        const item = band('col'+c,'col-band');
        item.style.gridColumn = i+2;
        board.append(item);
      }
    });
    rows.forEach((r,i)=>{
      const row = i+2;
      const axis = el('span','axis',r);
      axis.style.gridRow = row;
      axis.style.gridColumn = 1;
      board.append(axis);
      if (folded('row'+r,emptyRows)) {
        const item = band('row'+r,'row-band');
        item.style.gridRow = row;
        board.append(item);
        return;
      }
      cols.forEach((c,j)=>{
        if (colFolded(c)) return;
        const item = tile(byWell.get(r+c));
        item.style.gridRow = row;
        item.style.gridColumn = j+2;
        board.append(item);
      });
    });
  }

  function drawScatter() {
    const W=151,H=154,left=30,right=144,top=8,bottom=116;
    const max = Math.max(1,Math.ceil(Math.max(0,...pairs.flatMap(w=>[w.a,w.b]))));
    const x = v => left+v/max*(right-left);
    const y = v => bottom-v/max*(bottom-top);
    const scatter = svg('svg',{viewBox:'0 0 '+W+' '+H,class:'scatter',role:'group','aria-label':'Adaptive vs Bayesian EI. Select a dot to open its well.'});
    for (const v of [0,max/2,max]) {
      line(scatter,left,y(v),right,y(v),'var(--line)');
      label(scatter,x(v),bottom+12,v,{'text-anchor':'middle'});
      label(scatter,left-5,y(v)+4,v,{'text-anchor':'end'});
    }
    line(scatter,left,bottom,right,top,'var(--muted)').setAttribute('stroke-dasharray','3 3');
    label(scatter,87,H-8,'Adaptive EI',{'text-anchor':'middle'});
    label(scatter,9,64,'Bayesian EI',{transform:'rotate(-90 9 64)','text-anchor':'middle'});
    for (const w of [...pairs].sort((a,b)=>Number(different(a))-Number(different(b)))) {
      const dot = svg('a',{href:'#rs-plate-detail-heading','aria-label':w.w+' '+w.acquisition+' '+w.allele+' Adaptive '+fmt(w.a)+', Bayesian '+fmt(w.b)},scatter);
      const mark = svg('circle',{cx:x(w.a),cy:y(w.b),r:different(w)?4:2.8,fill:different(w)?'var(--difference)':'var(--tone)'},dot);
      mark.dataset.tooltip = dot.getAttribute('aria-label');
      dot.addEventListener('click',e=>{e.preventDefault();showWell(w);});
    }
    find('[data-scatter]').append(scatter);
  }
  function drawGroups() {
    const summary = a => {
      const mean = a.reduce((s,n)=>s+n,0)/a.length;
      const sd = Math.sqrt(a.reduce((s,n)=>s+(n-mean)**2,0)/(a.length-1));
      return fmt(mean)+' ± '+fmt(sd);
    };
    for (const group of groups) {
      const set = pairs.filter(w=>w.group===group);
      const all = records.filter(w=>w.group===group && w.role==='sample');
      const section = el('div','group-row');
      const head = el('strong');
      head.append(el('i','swatch '+groupClass({group})),document.createTextNode(group+' · n='+set.length));
      section.append(head);
      if (set.length>1) section.append(el('p','','A '+summary(set.map(w=>w.a))),el('p','','B '+summary(set.map(w=>w.b))));
      else if(set.length===1)section.append(el('p','','A '+fmt(set[0].a)+' · B '+fmt(set[0].b)),el('small','','One paired record; SD unavailable.'));
      else section.append(el('small','','No comparable two-method results; mean unavailable (not zero).'));
      section.append(el('small','','Available: A '+all.filter(w=>Number.isFinite(w.a)).length+' / B '+all.filter(w=>Number.isFinite(w.b)).length));
      find('[data-groups]').append(section);
    }
  }
  function update() {
    find('[data-differences]').setAttribute('aria-pressed',state.diff);
    find('[data-fold]').setAttribute('aria-pressed',state.fold);
    root.querySelectorAll('[data-view]').forEach(b=>b.setAttribute('aria-pressed',b.dataset.view===state.view));
    root.querySelectorAll('[data-method]').forEach(b=>b.setAttribute('aria-pressed',b.dataset.method===state.method));
    drawBoard();
  }
  find('[data-comparisons]').addEventListener('toggle',()=>{state.comparisons=find('[data-comparisons]').open;persist();});
  root.querySelectorAll('[data-method]').forEach(b=>b.addEventListener('click',()=>{state.method=b.dataset.method;update();persist();}));
  find('[data-pair-count]').textContent = diffCount+' / '+comparableWells.length+' comparable';
  find('[data-differences]').addEventListener('click',()=>{state.diff=!state.diff;update();persist();});
  find('[data-fold]').addEventListener('click',()=>{state.fold=!state.fold;state.expanded=[];update();persist();});
  find('[data-close]').addEventListener('click',()=>detail.close());
  detail.addEventListener('click',e=>{if(e.target===detail) detail.close();});
  root.querySelectorAll('[data-view]').forEach(b=>b.addEventListener('click',()=>{state.view=b.dataset.view;update();persist();if(detail.open && byWell.has(state.well)){const w=byWell.get(state.well);showWell(w,Math.max(0,w.recordList.findIndex(r=>r.id===state.record)));}}));
  
  drawScatter();
  drawGroups();
  update();
  if(reopen && state.well && byWell.has(state.well)) {
    const w=byWell.get(state.well);const index=w.recordList.findIndex(r=>r.id===state.record);showWell(w,Math.max(0,index));
  }
});
