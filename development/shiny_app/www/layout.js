/* Shared, text-preserving Excel parser and staged plate editor. */
(function(root) {
  'use strict';
  const columns = ['Well','Code','Individual_ID','Group','Role','Genotype'];
  const headings = ['Well','Sample ID','Individual ID','Group','Role','Genotype'];
  const genotypes = {'Not set':'','Normal only':'normal_only','Normal + expanded':'normal_expanded','Expanded; other unresolved':'expanded_present','Two expanded expected':'two_expanded','Intermediate':'intermediate','Unclear':'unclear'};
  const roles = {'Pending':'unlabelled','Sample':'sample','Unused':'unused_empty','CE Fill':'ce_fill','PCR NTC':'ntc','Positive control':'positive_control','Normal control':'normal_control','Extraction blank':'extraction_blank','Other negative':'other_negative','Other':'other'};
  function parseTSV(text) {
    const rows=[]; let row=[],cell='',quoted=false,ended=false;
    text=String(text).replace(/^\uFEFF/,'').replace(/\r\n/g,'\n').replace(/\r/g,'\n');
    for(let i=0;i<text.length;i++) {
      const c=text[i]; ended=false;
      if(c==='"') {
        if(quoted && text[i+1]==='"'){cell+='"';i++;}
        else if(quoted)quoted=false;
        else if(cell==='')quoted=true;
        else cell+=c;
      } else if(!quoted && (c==='\t'||c==='\n')) {
        row.push(cell);cell='';if(c==='\n'){rows.push(row);row=[];ended=true;}
      } else cell+=c;
    }
    if(quoted)throw Error('Unclosed quote in pasted cells.');
    if(!ended){row.push(cell);rows.push(row);}
    return rows;
  }
  function valueFor(key,value) {
    value=value.trim();
    if(key==='Well') {
      const m=/^([A-H])0?([1-9]|1[0-2])$/i.exec(value);
      if(!m)throw Error('Invalid well: '+value+'. Use A01–H12.');
      return m[1].toUpperCase()+m[2].padStart(2,'0');
    }
    const choices=key==='Role'?roles:key==='Genotype'?genotypes:null;
    if(choices) {
      if(Object.hasOwn(choices,value))return choices[value];
      if(Object.values(choices).includes(value))return value;
      throw Error('Invalid '+key+': '+value+'. Use a listed option.');
    }
    return value;
  }
  function parseLayoutPaste(text) {
    const grid=parseTSV(text);if(grid.length<2)throw Error('Include a header row and at least one well.');
    const aliases={well:'Well',code:'Code',sampleid:'Code',individualid:'Individual_ID',group:'Group',role:'Role',genotype:'Genotype'};
    const keys=grid.shift().map(h=>aliases[h.toLowerCase().replace(/[ _-]/g,'')]);
    if(keys.some(k=>!k)||new Set(keys).size!==keys.length||!keys.includes('Well')||keys.length<2)throw Error('Headers must include Well and one or more of Sample ID, Individual ID, Group, Role, Genotype.');
    const seen=new Set();return grid.map(row=>{
      if(row.length!==keys.length)throw Error('Every row must have the same columns as the header; blank cells clear that field.');
      const record=Object.fromEntries(keys.map((k,i)=>[k,valueFor(k,row[i])]));
      if(seen.has(record.Well))throw Error('Duplicate well: '+record.Well);
      seen.add(record.Well);return record;
    });
  }
  function pasteRectangle(rows,index,column,text) {
    const grid=parseTSV(text),start=columns.indexOf(column);
    if(start<1||index+grid.length>rows.length||grid.some(r=>start+r.length>columns.length))throw Error('Paste extends outside the editable table.');
    return grid.map((r,i)=>Object.assign({Well:rows[index+i].Well},Object.fromEntries(r.map((v,j)=>[columns[start+j],valueFor(columns[start+j],v)]))));
  }
  function completeLayoutApply(state,scope) {
    return {draft:scope==='table'?new Map():state.draft,editorDirty:scope==='bulk'?false:state.editorDirty};
  }
  const api={parseTSV,parseLayoutPaste,pasteRectangle,completeLayoutApply};
  if(typeof module!=='undefined' && module.exports)module.exports=api;
  if(typeof document==='undefined')return;
  root.RepeatScopeLayout=api;
  document.addEventListener('DOMContentLoaded',()=>{
    let data=null,draft=new Map(),stale=false,editorDirty=false,inFlight=false;
    const $=id=>document.getElementById(id);
    const input=(name,value)=>Shiny.setInputValue(name,value,{priority:'event'});
    const message=(text,error=false)=>{$('layout_message').textContent=text;$('layout_message').classList.toggle('danger',error);};
    const fields=()=>{
      const out={};for(const [key,id] of [['Group','group'],['Individual_ID','individual'],['Role','role'],['Genotype','genotype']])if($('edit_'+id).checked)out[key]=$('layout_'+id).value;
      return out;
    };
    function pending(){
      const edit=fields();editorDirty=Object.keys(edit).length>0;
      const changed=[...draft.values()];
      input('layout_draft',{pending:changed.length>0||editorDirty,table_pending:changed.length>0,affects_fit:changed.some(r=>'Genotype'in r||'Role'in r)||'Genotype'in edit||'Role'in edit});
      $('layout_stage_count').textContent=changed.length?changed.length+' wells staged · not applied': 'No table edits';
      $('layout_table_apply').disabled=!changed.length||stale||inFlight;
    }
    function stage(patches) {
      if(!data)throw Error('Wait for the plate to load.');
      const next=new Map(draft);
      for(const patch of patches){if(!data.rows.some(r=>r.Well===patch.Well))throw Error('Unknown well: '+patch.Well);next.set(patch.Well,{...next.get(patch.Well),...patch});}
      draft=next;render();pending();message('Review the highlighted cells, then Apply table edits.');
    }
    function render() {
      if(!data)return;
      const table=$('layout_table');table.replaceChildren();
      const head=document.createElement('thead'),tr=document.createElement('tr');
      for(const h of headings){const th=document.createElement('th');th.textContent=h;tr.append(th);}head.append(tr);table.append(head);
      const body=document.createElement('tbody');
      data.rows.forEach((row,index)=>{
        const tr=document.createElement('tr');tr.dataset.well=row.Well;
        columns.forEach(key=>{
          const td=document.createElement('td');if(key==='Well'){td.textContent=row.Well;tr.append(td);return;}
          const choices=key==='Role'?roles:key==='Genotype'?genotypes:null;
          const el=document.createElement(choices?'select':'input');if(!choices){el.type='text';el.autocomplete='off';el.spellcheck=false;}
          if(choices)for(const [label,value] of Object.entries(choices)){const opt=document.createElement('option');opt.value=value;opt.textContent=label;el.append(opt);}
          el.setAttribute('aria-label',row.Well+' '+headings[columns.indexOf(key)]);el.dataset.column=key;
          el.value=Object.hasOwn(draft.get(row.Well)||{},key)?draft.get(row.Well)[key]:row[key];
          td.classList.toggle('staged',Object.hasOwn(draft.get(row.Well)||{},key));
          el.addEventListener('change',()=>{const patch={...draft.get(row.Well),Well:row.Well,[key]:el.value};draft.set(row.Well,patch);td.classList.add('staged');pending();});
          el.addEventListener('input',()=>{if(!choices){draft.set(row.Well,{...draft.get(row.Well),Well:row.Well,[key]:el.value});td.classList.add('staged');pending();}});
          el.addEventListener('paste',e=>{const text=e.clipboardData.getData('text/plain');if(!/[\t\r\n]/.test(text))return;e.preventDefault();try{stage(pasteRectangle(data.rows,index,key,text));}catch(err){message(err.message,true);}});
          td.append(el);tr.append(td);
        });body.append(tr);
      });table.append(body);
    }
    function resetEditor(){for(const name of ['group','individual','role','genotype']){$('edit_'+name).checked=false;jQuery($('edit_'+name)).trigger('change');}editorDirty=false;pending();}
    Shiny.addCustomMessageHandler('layout-data',next=>{
      if(draft.size && data && next.key!==data.key){stale=true;message('Plate information changed. Discard staged edits to reload before pasting again.',true);pending();return;}
      data=next;stale=false;render();pending();
    });
    Shiny.addCustomMessageHandler('layout-applied',next=>{
      const completed=completeLayoutApply({draft,editorDirty},next.scope);draft=completed.draft;editorDirty=completed.editorDirty;
      data=next;stale=false;inFlight=false;
      if(next.scope==='bulk')resetEditor();
      if(next.scope==='table')$('layout_paste').value='';
      render();pending();message('Changes applied. Undo is available.'+(editorDirty?' Selected-well edits are still pending.':''));
    });
    Shiny.addCustomMessageHandler('layout-rejected',reason=>{inFlight=false;message(reason,true);pending();});
    Shiny.addCustomMessageHandler('layout-reset',next=>{draft.clear();data=next;stale=false;inFlight=false;resetEditor();render();pending();message('Drafts discarded.');});
    $('layout_stage').addEventListener('click',()=>{try{stage(parseLayoutPaste($('layout_paste').value));}catch(err){message(err.message,true);}});
    $('layout_table_apply').addEventListener('click',()=>{if(stale||!draft.size||inFlight)return;inFlight=true;pending();input('layout_table_apply',{patch:[...draft.values()],key:data.key,nonce:Date.now()});});
    $('layout_apply').addEventListener('click',()=>{if(draft.size){message('Apply or discard the staged table edits first. Selected-well fields are kept.',true);return;}input('layout_apply',{fields:fields(),nonce:Date.now()});});
    for(const id of ['group','individual','role','genotype']){
      $('edit_'+id).addEventListener('change',pending);$('layout_'+id).addEventListener('input',pending);$('layout_'+id).addEventListener('change',pending);
    }
    for(const mode of ['group','individual'])$('select_'+mode).addEventListener('click',()=>input('layout_select',{mode,value:$('select_'+mode+'_value').value,nonce:Date.now()}));
    for(const mode of ['samples','missing','all','clear'])$('select_'+mode).addEventListener('click',()=>input('layout_select',{mode,nonce:Date.now()}));
  });
})(typeof window==='undefined'?globalThis:window);
