const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const context = {Shiny:{addCustomMessageHandler(){}}};
vm.createContext(context);
vm.runInContext(fs.readFileSync('development/shiny_app/www/dashboard.js','utf8'),context);
assert.equal(typeof context.dashboardRecordIndex,'function');
const records=[{id:'A01|injection-1|allele-1'},{id:'A01|injection-2|allele-2'}];
assert.equal(context.dashboardRecordIndex({id:records[1].id,recordList:records}),1,
  'scatter record 2 must open record 2, not record 1');
assert.equal(context.dashboardRecordIndex({id:records[0].id,recordList:records}),0);
assert.equal(context.dashboardRecordIndex({recordList:records}),0);
console.log('DASHBOARD RECORD IDENTITY PASS');
