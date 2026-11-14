const assert=require('node:assert/strict'),fs=require('node:fs');
const {learningFrames,learningY}=require('../www/method-learning.js');
assert.equal(typeof learningY,'function','the chart needs a square-root height projection');
assert.equal(learningY(0,10000,20,220),220,'zero RFU remains on the baseline');
assert.equal(learningY(10000,10000,20,220),20,'the 10000 RFU modal fits in the chart');
assert.equal(learningY(2500,10000,20,220),120,'one quarter of modal RFU is half the displayed height');
assert.ok(Math.abs(learningY(300,10000,20,220)-185.35898384862244)<1e-10,
  'the 3% line uses the same nonlinear projection as the peaks');
const trace=JSON.parse(fs.readFileSync('development/shiny_app/www/method-trace.json','utf8'));
const before=JSON.stringify(trace);
const eligible=new Set(trace.peaks.filter(p=>p.Height>=trace.threshold*trace.modalHeight).map(p=>p.Peak_ID));
for(const method of ['adaptive','bayesian','ei']){
  const frames=learningFrames(trace,method);
  assert.ok(frames.every(f=>f.duration>=3000),'every animation page stays visible for at least three seconds');
  assert.deepEqual(frames.slice(0,4).map(f=>f.phase),['inventory','zoom','threshold','filter'],
    'physical inventory, zoom and 3% filter must precede any assignment');
  assert.equal(frames[2].threshold,true);assert.equal(frames[3].filtered,true);
  for(const f of frames.slice(4)){
    assert.equal(f.threshold,true);assert.equal(f.filtered,true);
    assert.ok(f.accepted.every(id=>eligible.has(id)),'low peaks never become accepted');
  }
  assert.deepEqual(frames.at(-1).accepted,trace[(method==='ei'?'adaptive':method)+'EI'].ids);
}
const a=learningFrames(trace,'adaptive'),b=learningFrames(trace,'bayesian');
for(const peak of trace.adaptive.filter(p=>p.Delta>0)){
  const f=a.find(f=>f.delta===peak.Delta);
  assert.equal(f.intervals[0].low,peak.Expected_size_bp-trace.tolerance);
  assert.equal(f.intervals[0].high,peak.Expected_size_bp+trace.tolerance);
}
const predictions=b.find(f=>f.phase==='predict'),selected=b.find(f=>f.phase==='select');
const tail=trace.bayesian.predictions.filter(p=>!trace.bayesian.seedAudit.some(s=>s.Status==='accepted_seed'&&s.Delta===p.Delta));
assert.deepEqual(predictions.intervals,tail.map(p=>({delta:p.Delta,low:p.Low,high:p.High,status:p.Status})),
  'all frozen Bayesian tail intervals appear in one frame');
assert.deepEqual(selected.intervals,predictions.intervals,'selection does not recalculate prediction intervals');
assert.deepEqual(selected.accepted,trace.bayesianEI.ids);
assert.equal(b.filter(f=>f.phase==='select').length,1,'Bayesian tail selection is simultaneous');
assert.equal(JSON.stringify(trace),before,'animation must not mutate the scientific trace');
assert.equal(trace.adaptiveEI.ei,88651/30843);assert.equal(trace.bayesianEI.ei,76076/30163);
console.log('THRESHOLD-FIRST ANIMATION ORDER, SIMULTANEOUS PREDICTIONS, EI AND IMMUTABILITY PASS');
