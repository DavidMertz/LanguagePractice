#!/usr/bin/env node
import * as fs from 'fs';
import * as path from 'path';
import * as sha1 from 'js-sha1';

/* Interface to store relevant info about a file */
interface Finfo {
    fname: string;
    size: number;
    hash: string;
}

/* Create global data structrue to map file sizes to Finfo objects */
interface SizeGroups { 
    [size: number]: Finfo[]; 
}
let by_size: SizeGroups = {};
/* Define mapping of hash digests to arrays of filenames */
interface HashGroup {
    [sha1: string]: Finfo[];
}
/* Callbacks and completion of each for a spin-wait */
let todo: number = 0;
let done: number = 0;

/*---------------------------------------------------------------------
 * Recursively and asynchronously walk directory
 * 
 * Callbacks for found `file` and `problem` encountered will be
 * provided with full paths to the relevant file/directory
 *
 * @param dir Folder name you want to recursively process
 * @param found Callback function, returns all files with full path.
 * @param problem Logging function to handle errors encounted
 ----------------------------------------------------------------------*/
const walkdir = (
    dir: string,
    found: (finfo: Finfo, problem: Function) => void,
    problem: (err: Error, msg: string) => void
) => { 
    fs.readdir(dir, (err: Error, list: string[]) => {
        if (err) { return problem(err, dir); }

        list.forEach((file: string) => {
            file = path.resolve(dir, file);
            fs.stat(file, (err2, stat) => {
                if (err2) { 
                    problem(err, file); }
                else if (stat && stat.isDirectory()) {
                    walkdir(file, found, problem); }
                else if (stat.isFile()) {
                    let finfo: Finfo = { 
                        fname: file, size: stat.size, hash: null }
                    todo += 1;
                    found(finfo, problem); 
                }
            });
        });
    });
}; 

/*---------------------------------------------------------------------
 * Populate the by_size data object size: Finfo
 *
 * Do not compute hashes until needed because of size dups
 *
 * @param finfo File information object following Finfo protocol
 * @param problem Logging function to handle errors encounted
 ----------------------------------------------------------------------*/
const accum_by_size = (
    finfo: Finfo,
    problem: (err: Error, msg: string) => void
) => {
    let size = finfo.size;
    // The first time we've seen this size
    if (! (size in by_size)) {
        by_size[size] = [finfo];
        done += 1;
    }
    // The second time we've seen this size
    // Need to go back and hash the first entry as well
    else if (by_size[size].length == 1) {
        let old_finfo = by_size[size][0];
        // Hash the existing entry (but don't push it again)
        fs.readFile(old_finfo.fname, (err, data: Buffer): void => {
            let hash = sha1.create();
            hash.update(data);
            if (err) { problem(err, old_finfo.fname); } 
            else {
                old_finfo.hash = hash.hex(data);
            }
        });
        // Now also hash the new entry and push it to array
        fs.readFile(finfo.fname, (err, data: Buffer) => {
            let hash = sha1.create();
            hash.update(data);
            if (err) { problem(err, finfo.fname); } 
            else {
                finfo.hash = hash.hex(data);
                by_size[size].push(finfo);
                done += 1;
            }
        });
    }
    // The third or later time, just hash new entry contents
    else {
        fs.readFile(finfo.fname, (err, data: Buffer) => {
            let hash = sha1.create();
            hash.update(data);
            if (err) { problem(err, finfo.fname); } 
            else {
                finfo.hash = hash.hex(data);
                by_size[size].push(finfo);
                done += 1;
            }
        });
    }
};

/*---------------------------------------------------------------------
 * Display report of files having identical content
 *
 * @param by_size Mapping of sizes to array of Finfo objects
 ----------------------------------------------------------------------*/
const show_dups = (
    by_size: SizeGroups
) => {
    // Loop through all the file sizes in reverse order
    for (let size of Object.keys(by_size).reverse()) {
        let finfos: Finfo[] = by_size[size];
        if ((+size > 0) && (finfos.length > 1)) {
            // New hash group
            let by_sha1: HashGroup = {};

            // Loop through all the files of this size
            for (let finfo of finfos) {
                let sha1: string = finfo.hash;
                if (! (sha1 in by_sha1)) {
                    // The first time we've seen this SHA1 digest
                    by_sha1[sha1] = [finfo]; }
                else {
                    // Add more dups of this SHA1 digest to array
                    by_sha1[sha1].push(finfo); }
            }
            // Loop through by_sha1 specific to this filesize
            for (let [sha1, finfos] of Object.entries(by_sha1)) {
                if (finfos.length > 1) {
                    console.log(`Size: ${size} | SHA1: ${sha1}"`);
                    for (let finfo of finfos) {
                        console.log(`  ${finfo.fname}`);
                    }
                }
            }
        }
    }
};

/* Utility function utilizing timeout Promise to sleep */    
function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

async function ShowDups(by_size: SizeGroups) {
    // Initial sleep to make sure some files are in TODO
    await sleep(100);
    // Sleep as needed for accum_by_size to catch up to walkdir
    while (todo != done) { await sleep(100); }
    // Now call the actual report function
    show_dups(by_size);
}

/*---------------------------------------------------------------------
 * The "main()" steps of the script
 ----------------------------------------------------------------------*/

let dir: string = process.argv[2];
walkdir(dir, accum_by_size, console.error);
ShowDups(by_size);
