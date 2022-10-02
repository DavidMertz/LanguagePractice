#!/usr/bin/env node
import * as fs from 'fs';
import * as path from 'path';
import * as Rusha from 'rusha';

let fname: string = process.argv[2];

fs.readFile(fname, (err, data: Buffer): void => {
    const hash = Rusha.createHash().update(data); 
    console.log(`${hash.digest('hex')}  ${fname}`);
});
