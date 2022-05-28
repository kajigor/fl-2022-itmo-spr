
const pug = require('pug');
const path = require('path');
const fs = require('fs');

const data = JSON.parse(fs.readFileSync(process.argv[2], 'utf-8'));

if (data.cyk) {
    for (const ch of data.cyk.checks) {
        for (let i = 0; i < ch.result.length; ++i) {
            for (let j = 0; j < ch.result[i].length; ++j) {
                ch.result[i][j] = ch.result[i][j].join(', ');
            }
        }
    }
}

function mapDict(dict) {
    for (const key in dict) {
        dict[key] = dict[key].split(' ')
            .map(s => { return { terminal : s.startsWith('"'), val : s } });
    }
}

mapDict(data.initial.dict);
for (const key in data.transform) {
    mapDict(data.transform[key].dict);
}

const pugfn = pug.compileFile(path.resolve(__dirname, 'page', 'page.pug'), { pretty : true });
const html = pugfn(data);

fs.writeFileSync(process.argv[3], html);
