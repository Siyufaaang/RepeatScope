const assert = require('node:assert/strict');
const fs = require('node:fs');
const file = 'development/shiny_app/www/layout.js';
assert.ok(fs.existsSync(file), 'Excel layout parser must exist');
const {parseTSV, parseLayoutPaste, pasteRectangle} = require('../www/layout.js');
assert.deepEqual(parseTSV('a\tb\r\nc\t\r\n'), [['a','b'],['c','']]);
assert.deepEqual(parseTSV('"line\n2"\t"a""b"'), [['line\n2','a"b']]);
assert.throws(()=>parseTSV('"open'), /quote/i);
assert.deepEqual(parseLayoutPaste('Well\tSample ID\tIndividual ID\tGenotype\nA1\t0004\t001\tNormal + expanded\nH06\t\t\tUnclear'),
  [{Well:'A01',Code:'0004',Individual_ID:'001',Genotype:'normal_expanded'},{Well:'H06',Code:'',Individual_ID:'',Genotype:'unclear'}]);
assert.deepEqual(parseLayoutPaste('Well\tRole\nE07\tCE Fill'), [{Well:'E07',Role:'ce_fill'}]);
for(const text of ['Well\tGenotype\nA01\tguess','Well\tGroup\nA01\tx\nA1\ty','Well\tGroup\nZ99\tx','Group\nx','Well\tOther\nA01\tx','Well\tGroup\nA01\tx\ty','Well\tRole\nE07\tWater']) assert.throws(()=>parseLayoutPaste(text));
const rows=[{Well:'A01',Code:'one',Individual_ID:'',Group:'',Role:'sample',Genotype:''},{Well:'A02',Code:'two',Individual_ID:'',Group:'',Role:'sample',Genotype:''}];
assert.deepEqual(pasteRectangle(rows,0,'Individual_ID','001\tG1\n002\tG2'),[{Well:'A01',Individual_ID:'001',Group:'G1'},{Well:'A02',Individual_ID:'002',Group:'G2'}]);
assert.throws(()=>pasteRectangle(rows,1,'Genotype','Unclear\nNormal only'), /outside/i);
assert.throws(()=>pasteRectangle(rows,0,'Genotype','Normal only\textra'), /outside/i);
console.log('LAYOUT EXCEL PASTE PASS');
