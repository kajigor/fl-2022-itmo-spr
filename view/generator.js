
const pug = require('pug');
const path = require('path');
const fs = require('fs');

const data = JSON.parse(fs.readFileSync(process.argv[2], 'utf-8'));

function mapDict(dict) {
    for (const key in dict) {
        dict[key] = dict[key].split(' ')
            .map(s => { return { terminal : s.startsWith('"'), val : s } });
    }
}

mapDict(data.initial.dict);
mapDict(data.cnf.dict);
for (const key in data.transform) {
    mapDict(data.transform[key].dict);
}

const pugfn = pug.compileFile(path.resolve(__dirname, 'page', 'page.pug'), { pretty : true });
const html = pugfn(data);

fs.writeFileSync(process.argv[3], html);
