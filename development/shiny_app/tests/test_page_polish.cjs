const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const context = {Shiny:{addCustomMessageHandler(){}}};
vm.createContext(context);
vm.runInContext(fs.readFileSync('development/shiny_app/www/dashboard.js','utf8'),context);
assert.equal(typeof context.dashboardRoleSummary,'function','report exposes role counts by physical well');
const summary = wells => JSON.parse(JSON.stringify(context.dashboardRoleSummary(wells)));
assert.deepEqual(summary([]),[]);
assert.deepEqual(summary([
  {w:'A01',role:'sample',records:[{id:'injection1'},{id:'injection2'}]},
  {w:'A01',role:'sample'}, {w:'A02',role:'sample'}, {w:'B01',role:'ce_fill'}
]),[{role:'sample',label:'Sample',count:2},{role:'ce_fill',label:'CE Fill',count:1}],
'multiple records and repeated well entries never inflate physical-well counts');
const roles=['sample','ce_fill','ntc','positive_control','normal_control','extraction_blank','other_negative','other','unused_empty','unlabelled'];
const labels=['Sample','CE Fill','PCR NTC','Positive control','Normal control','Extraction blank','Other negative','Other','Unused','Pending'];
const mixed=summary(roles.map((role,i)=>({w:'A'+String(i+1).padStart(2,'0'),role})));
assert.deepEqual(mixed,roles.map((role,i)=>({role,label:labels[i],count:1})),
'every assigned category is named separately, including control types');
const plate=Array.from({length:96},(_,i)=>({w:'ABCDEFGH'[Math.floor(i/12)]+String(i%12+1).padStart(2,'0'),role:i<41?'sample':'ce_fill'}));
assert.deepEqual(summary(plate),[{role:'sample',label:'Sample',count:41},{role:'ce_fill',label:'CE Fill',count:55}]);
plate[0].role='ntc';
assert.deepEqual(summary(plate),[{role:'sample',label:'Sample',count:40},{role:'ce_fill',label:'CE Fill',count:55},{role:'ntc',label:'PCR NTC',count:1}],
'changing a role updates the category inventory without zero categories');
console.log('PHYSICAL WELL ROLE COUNTS PASS');

const listeners={},events=[];
const thresholdContext={document:{addEventListener:(name,fn)=>{listeners[name]=fn;}},window:{Shiny:{setInputValue:(name,value)=>events.push({name,...value})}}};
thresholdContext.Shiny=thresholdContext.window.Shiny;
vm.createContext(thresholdContext);
vm.runInContext(fs.readFileSync('development/shiny_app/www/threshold.js','utf8'),thresholdContext);
for(const value of ['','-1','101','not-a-number'])listeners.input({target:{id:'threshold_number',value}});
assert.equal(events.length,0,'invalid or empty editing must not replace the canonical threshold');
for(const value of ['0','3.5','100'])listeners.input({target:{id:'threshold_number',value}});
assert.deepEqual(events.map(e=>e.value),[0,3.5,100]);
assert.ok(events.every(e=>e.name==='threshold_change' && e.source==='number'));
assert.ok(events[2].nonce>events[1].nonce);
listeners.change({target:{id:'unrelated',value:'20'}});
assert.equal(events.length,3);
console.log('NUMERIC THRESHOLD EVENTS PASS');
