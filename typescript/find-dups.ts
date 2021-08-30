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
let ticker: number = 0;
let hashes_performed = 0;
let promises = [];

/* Handle conditional flags with globals (probably not best apprach) */
let shaLib = "js-sha1";
let problem = console.error;
let verbose = false;

/* Put hashing in a function to cache and choose library */
function getDigest(data: Buffer): string {
    let offset = 0;
    let chunksize = 100000;
    if (shaLib == "rusha") {
        // rusha
        let hash = Rusha.createHash()
        while (true) {
            let segment = data.slice(offset, offset+chunksize);
            hash.update(segment);
            offset += chunksize;
            if (segment.length < chunksize) { break }
        }
        var digest = hash.digest('hex');
    }
    else if (shaLib == "js-sha1") {
        // js-sha1
        let hash = sha1.create();
        while (true) {
            let segment = data.slice(offset, offset+chunksize);
            hash.update(segment);
            offset += chunksize;
            if (segment.length < chunksize) { break }
        }
        var digest = hash.hex();
    }
    else {
        // the fallback?
        var digest = "NO SHA1 PERFORMED";
    }
    hashes_performed += 1;
    return digest
}

/* Add hashes to all same-size finfos in an array (caching saves work) */
function hashFinfos(finfos: Finfo[]): Promise<number> {
    return new Promise((resolve, reject) => {
        let updatedFinfos = [];
        let size = finfos[0].size;
        for (var finfo of finfos) {
            // The cheap case is using a cached inode
            if (finfo.inode in inodeCache) {
                finfo.hash = inodeCache[finfo.inode];
                updatedFinfos.push(finfo);
            }
            // More work is actually performing a hash
            else {
                let data = fs.readFileSync(finfo.fname);
                let digestPromise = getDigest(data)
                promises.push(digestPromise);
                finfo.hash = digestPromise;
                inodeCache[finfo.inode] = finfo.hash;
                updatedFinfos.push(finfo);
            };
        };
        by_size[size] = updatedFinfos;
        if (true) {
            resolve(updatedFinfos.length);
        }
        else {
            // In concept this is where we could fail promise
            reject(666);
        }
    });
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
            ticker += 1;
            file = path.resolve(dir, file);
            fs.lstat(file, (err2, stat) => {
                if (err2) { 
                    problem(err, file); }
                else if (stat && stat.isDirectory()) {
                    walkdir(file, found); }
                else if (stat.isSymbolicLink()) {
                    // Skip these
                    //problem(null, `Skipping symlink ${file}`);
                }
                else if (stat.isFile()) {
                    let finfo: Finfo = { 
                            fname: file, 
                            size: stat.size, 
                            hash: null, 
                            inode: stat.ino }
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
    ticker += 1;
    let size = finfo.size;
    // The first time we've seen this size
    if (! (size in by_size)) {
        // Add a "pseudo-hash" which may suffice
        finfo.hash = `<INODE ${finfo.inode}>`;
        by_size[size] = [finfo];
    }
    // This is a later time seeing this size 
    // May or may not need to hash prior entries
    else {
        let old_finfo = by_size[size][0];
        // No new inode, we can just borrow the pseudo-hash
        if (finfo.inode == old_finfo.inode) {
            finfo.hash = old_finfo.hash;
            by_size[size].push(finfo);
        }
        // There multiple inodes involved, need to do actual hashing
        else {
            // Add the new finfo first
            by_size[size].push(finfo);

            // Now ready to enforce actual hashes on all entries
            ticker += 1
            promises.push(hashFinfos(by_size[size]));
        }
    }
};

/*---------------------------------------------------------------------
 * Display report of files having identical content
 *
 * @param by_size Mapping of sizes to array of Finfo objects
 ----------------------------------------------------------------------*/
const showDups = () => {
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

async function waitForShowDups() {
    // Initial sleep to make sure some files have been walked
    let progress = ticker;
    await sleep(30000);
    // Sleep as needed until promises all made (i.e. hashing queued)
    // Adjust the sleep slightly crudely, but dynamically
    while (progress < ticker) { 
        await sleep(4*promises.length); 
        progress = ticker;
    }
    // Now call the actual report function
    Promise.allSettled(promises).then((results) => {
        showDups();
        if (verbose) {
            console.error(`Ticker: ${ticker}`);
            console.error(`Promises: ${promises.length}`);
            console.error(`Hashes: ${hashes_performed}`);
        }
    });
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
        verbose: flag({
            long: "verbose",
            short: "v",
            type: boolean 
        })
    },
    handler: args => {
        if (args.jsSha1) { shaLib = "js-sha1"; }
        if (args.rusha)  { shaLib = "rusha"; }
        verbose = args.verbose;
        if (verbose) {
            console.error(`SHA1-lib: ${shaLib}`);
            for (let key of Object.keys(args)) {
                console.error(`${key}: ${args[key]}`);
            }
        }
        walkdir(args.rootdir, accum_by_size);
        waitForShowDups();
   },
});
run(cmd, process.argv.slice(2));

