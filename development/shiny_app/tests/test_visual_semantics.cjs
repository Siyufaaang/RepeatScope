const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const context = {Shiny:{addCustomMessageHandler(){}}};
vm.createContext(context);
vm.runInContext(fs.readFileSync('development/shiny_app/www/dashboard.js','utf8'),context);
assert.equal(typeof context.dashboardComparison,'function');
assert.equal(context.dashboardComparison({a:2.874266446,b:2.522162915}),'different');
assert.equal(context.dashboardComparison({a:0,b:0}),'equal');
assert.equal(context.dashboardComparison({a:1.00001,b:1.00002}),'equal','keep the existing displayed-precision rule');
for (const missing of [null,undefined,NaN,Infinity]) {
  assert.equal(context.dashboardComparison({a:0,b:missing}),'unavailable');
}
assert.equal(context.dashboardComparison({a:2,b:1},true),'unavailable','stale results cannot be compared');
assert.equal(context.dashboardPeakInk([208,10000,1,1],208,'Adaptive'),'--modal');
for (const method of [null,'Adaptive','Bayesian']) {
  assert.equal(context.dashboardPeakInk([210.92,6500,1,1],208,method),'--both');
  assert.equal(context.dashboardPeakInk([251.8,271,0,0],208,method),'--excluded');
}
assert.equal(context.dashboardPeakInk([261.01,345,1,0],208,'Adaptive'),'--adaptive');
assert.equal(context.dashboardPeakInk([261.01,345,1,0],208,'Bayesian'),'--excluded');
assert.equal(context.dashboardPeakInk([261.01,345,0,1],208,'Bayesian'),'--bayes');
assert.equal(context.dashboardPeakInk([261.01,345,0,1],208,'Adaptive'),'--excluded');
assert.equal(context.dashboardPeakInk([261.01,345,0,1],208,null),'--bayes');
const peak=[261.01,345,1,0], before=JSON.stringify(peak);
context.dashboardPeakInk(peak,208,'Adaptive');
assert.equal(JSON.stringify(peak),before,'presentation never mutates the original peak');
console.log('VISUAL COMPARISON AND PEAK SEMANTICS PASS');
const handlers={}, domCallbacks=[];
const notificationLayout={height:76,properties:{}};
const header={getBoundingClientRect:()=>({height:notificationLayout.height})};
const navigation={
  document:{addEventListener(type,handler){if(type==='DOMContentLoaded')domCallbacks.push(handler);},
    querySelector:selector=>selector==='.site-header'?header:null,
    documentElement:{style:{setProperty(name,value){notificationLayout.properties[name]=value;}}}},
  ResizeObserver:class {constructor(callback){notificationLayout.resize=callback;}observe(element){assert.equal(element,header);}},
  Shiny:{addCustomMessageHandler(type,handler){assert.equal(handler.length,1,'Shiny custom message handlers require exactly one argument');handlers[type]=handler;}}
};
vm.createContext(navigation);
vm.runInContext(fs.readFileSync('development/shiny_app/www/visual.js','utf8'),navigation);
domCallbacks.forEach(fn=>fn());
assert.equal(typeof handlers['demo-load'],'function');
assert.equal(typeof handlers['landing-scroll'],'function');
assert.equal(notificationLayout.properties['--site-header-height'],'76px','notifications clear the desktop header');
notificationLayout.height=157.3;
notificationLayout.resize();
assert.equal(notificationLayout.properties['--site-header-height'],'158px','notification position follows a wrapped responsive header');
console.log('SHINY NAVIGATION HANDLERS PASS');
