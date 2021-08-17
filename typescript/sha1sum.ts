#!/usr/bin/env node
import * as fs from 'fs';
import * as path from 'path';
import * as sha1 from 'js-sha1';

let fname: string = process.argv[2];

fs.readFile(fname, (err, data: Buffer): void => {
    let hash = sha1.create();
    hash.update(data);
    console.log(`${hash.hex()}  data`);
});
