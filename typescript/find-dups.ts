#!/usr/bin/env node
import * as fs from 'fs';
import * as path from 'path';
import * as sha1 from 'js-sha1';
import * as Rusha from 'rusha';
import { command, run, string, number, boolean, 
         positional, option, flag } from 'cmd-ts';

/* Interface to store relevant info about a file */
interface Finfo {
    fname: string;
    size: number;
    hash: string;
    inode: number;
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

/* A global cache of inodes to hashes */
interface InodeCache {
    [inode: number]: string;
}
let inodeCache: InodeCache = {};

/* Callbacks and completion of each for a spin-wait */
let todo: number = 0;
let done: number = 0;

/* Handle conditional flags with globals (probably not best apprach) */
let shaLib = "rusha";
let problem = console.error;

/* Put hashing in a function to cache and choose library */
const getDigest = (data: Buffer) => {
    if (shaLib == "rusha") {
        // rusha
        let hash = Rusha.createHash()
        hash.update(data);
        var digest = hash.digest('hex');
    }
    else if (shaLib == "js-sha1") {
        // js-sha1
        let hash = sha1.create();
        hash.update(data);
        var digest = hash.hex();
    }
    else {
        // the fallback?
        var digest = "NO SHA1 PERFORMED";
    }
    return digest;
};

/* Add hashes to all finfos in an array (use caching to save work) */
const hashFinfos = (finfos: Finfo[]) => {
    for (let finfo of finfos) {
        // The cheap case is using a cached inode
        if (finfo.inode in inodeCache) {
            finfo.hash = inodeCache[finfo.inode];
        }
        // More work is actually performing a hash
        else {
            let data = fs.readFileSync(finfo.fname);
            finfo.hash = getDigest(data);
            inodeCache[finfo.inode] = finfo.hash;

        }
     }
/*   
    // Hash the existing entry (but don't push it again)
    fs.readFile(old_finfo.fname, (err, data: Buffer): void => {
        if (err) { problem(err, old_finfo.fname); } 
        else {
            old_finfo.hash = getDigest(data);
        }
    });
    // Now also hash the new entry and push it to array
    fs.readFile(finfo.fname, (err, data: Buffer) => {
        if (err) { problem(err, finfo.fname); } 
        else {
            finfo.hash = getDigest(data);
            by_size[size].push(finfo);
            done += 1;
        }
    });
*/
}

/*---------------------------------------------------------------------
 * Recursively and asynchronously walk directory
 * 
 * Callbacks for found `file` encountered will be provided with full 
 * paths to the relevant file/directory
 *
 * @param dir Folder name you want to recursively process
 * @param found Callback function, returns all files with full path.
 ----------------------------------------------------------------------*/
const walkdir = (
    dir: string,
    found: (finfo: Finfo) => void
) => { 
    fs.readdir(dir, (err: Error, list: string[]) => {
        if (err) { return problem(err, dir); }

        list.forEach((file: string) => {
            file = path.resolve(dir, file);
            fs.lstat(file, (err2, stat) => {
                if (err2) { 
                    problem(err, file); }
                else if (stat && stat.isDirectory()) {
                    walkdir(file, found); }
                else if (stat.isSymbolicLink()) {
                    // Skip these
                    problem(null, `Skipping symlink ${file}`);
                }
                else if (stat.isFile()) {
                    let finfo: Finfo = { 
                            fname: file, 
                            size: stat.size, 
                            hash: null, 
                            inode: stat.ino }
                    todo += 1;
                    found(finfo); 
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
 ----------------------------------------------------------------------*/
const accum_by_size = (finfo: Finfo) => {
    let size = finfo.size;
    // The first time we've seen this size
    if (! (size in by_size)) {
        // Add a "pseudo-hash" which may suffice
        finfo.hash = `<INODE ${finfo.inode}>`;
        by_size[size] = [finfo];
        done += 1;
    }
    // This is a later time seeing this size 
    // May or may not need to hash prior entries
    else {
        let old_finfo = by_size[size][0];
        // No new inode, we can just borrow the pseudo-hash
        if (finfo.inode == old_finfo.inode) {
            finfo.hash = old_finfo.hash;
            by_size[size].push(finfo);
            done +=1;
        }
        // There are two inodes involved, need to do actual hashing
        else {
            // Add the new finfo first
            by_size[size].push(finfo);

            // Now ready to enforce actual hashes on all entries
            hashFinfos(by_size[size]);
            done += 1;
        }
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
                    console.log(`Size: ${size} | SHA1: ${sha1}`);
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

async function showDups(by_size: SizeGroups) {
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

const cmd = command({
    name: "find-dups",
    description: 'Find files with identical contents',
    version: '0.2',
    args: {
        rootdir: positional({
            type: string, 
            displayName: "rootdir"
        }),
        jsSha1: flag({
            long: "use-js-sha1",
            short: "j",
            type: boolean 
        }),
        rusha: flag({
            long: "use-rusha-sha",
            short: "r",
            type: boolean 
        }),
    },
    handler: args => {
        if (args.jsSha1) { shaLib = "js-sha1"; }
        if (args.rusha)  { shaLib = "rusha"; }
        console.error(args);
        walkdir(args.rootdir, accum_by_size);
        showDups(by_size);
   },
});
run(cmd, process.argv.slice(2));

