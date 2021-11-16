#!/usr/bin/env node
import * as fs from 'fs';
import * as path from 'path';
import * as sha1 from 'js-sha1';
import * as Rusha from 'rusha';

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

/* Define mapping of inode to hash digest */
interface InodeHash {
    [inode: number]: string;
}

/* Callbacks and completion of each for a spin-wait */
let todo: number = 0;
let done: number = 0;
let hashes_performed: number = 0;

/* Specify the error handler/loggers */
interface Problem {
    (err: Error, msg: string): void;
}
let problem: Problem = console.error;

/* Put hashing in a function to cache and choose library */
const getDigest = (finfo: Finfo) => {
    let data = fs.readFileSync(finfo.fname);
    // js-sha1
    let hash = sha1.create();
    hash.update(data);
    let digest = hash.hex();
    finfo.hash = digest;
    return digest
    // rusha
    // const hash = Rusha.createHash().update(data);
    // return hash.digest('hex')
};

/* Function to hash an array of Finfo objects */
const hashAll = (finfos: Finfo[]) => {
    let hashCache: InodeHash = {};
    for (let finfo of finfos) {
        if (finfo.inode in hashCache) {
            finfo.hash = hashCache[finfo.inode];
        }
        else {
            getDigest(finfo);
            by_size[finfo.size].push(finfo);
            hashCache[finfo.inode] = finfo.hash;
            done += 1;
            hashes_performed += 1;
        };
    };
}

/*---------------------------------------------------------------------
 * Recursively walk directory
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
const accum_by_size = (
    finfo: Finfo,
) => {
    let size = finfo.size;
    // The first time we've seen this size
    if (! (size in by_size)) {
        finfo.hash = `<INODE ${finfo.inode}>`;
        by_size[size] = [finfo];
    }
    // Subsequent files of the same size...
    // (Maybe) need to go back and add hash to first entry too
    else {
        let thisSize = by_size[size];
        let first_finfo = thisSize[0];
        // Trick is whether we actually have multiple inodes already
        // If not, we can just keep adding the pseudo-hash
        if (first_finfo.inode==finfo.inode &&
            first_finfo.hash.startsWith("<INODE")) {
            finfo.hash = first_finfo.hash;
        }
        else { 
            hashAll(by_size[size]); 
        };
    }
    done += 1;
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


/* Usage: await sleep(ms) (within an async function only) */
function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

async function ShowDups(by_size: SizeGroups) {
    // Initial sleep to make sure some files are in TODO
    await sleep(100);
    // Sleep as needed for accum_by_size to catch up to walkdir
    while (todo != done) { 
        await sleep(10); 
    }
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

let dir: string = process.argv[2];
walkdir(dir, accum_by_size);
ShowDups(by_size);