const assert=require('node:assert/strict'),fs=require('node:fs');
const {learningFrames}=require('../www/method-learning.js');
const trace=JSON.parse(fs.readFileSync('development/shiny_app/www/method-trace.json','utf8'));
const before=JSON.stringify(trace);
const a=learningFrames(trace,'adaptive'),b=learningFrames(trace,'bayesian'),ei=learningFrames(trace,'ei');
assert.equal(a[1].low,trace.adaptive[1].Expected_size_bp-trace.tolerance);
assert.equal(a[1].high,trace.adaptive[1].Expected_size_bp+trace.tolerance);
for(let k=1;k<=5;k++){assert.equal(b[k].low,trace.bayesian.seedAudit[k-1].Low);assert.equal(b[k].high,trace.bayesian.seedAudit[k-1].High);}
const pred=trace.bayesian.predictions.find(x=>x.Delta===18),f=b.find(x=>x.delta===18);
assert.equal(f.low,pred.Low);assert.equal(f.high,pred.High);assert.equal(f.status,pred.Status);
assert.deepEqual(a.at(-1).accepted,trace.adaptiveEI.ids);
assert.deepEqual(b.at(-1).accepted,trace.bayesianEI.ids);
assert.equal(ei.at(-1).value,trace.adaptiveEI.ei);
assert.equal(JSON.stringify(trace),before,'animation must not mutate the scientific trace');
assert.equal(trace.adaptiveEI.ei,88651/30843);assert.equal(trace.bayesianEI.ei,76076/30163);
console.log('SYNTHETIC ANIMATION WINDOWS, SEEDS, FINAL EI AND IMMUTABILITY PASS');
