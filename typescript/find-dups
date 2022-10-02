#!/usr/bin/env node
"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
    return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (_) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
exports.__esModule = true;
var fs = require("fs");
var path = require("path");
var sha1 = require("js-sha1");
var Rusha = require("rusha");
var cmd_ts_1 = require("cmd-ts");
var by_size = {};
var inodeCache = {};
/* Callbacks and completion of each for a spin-wait */
var hashes_performed = 0;
var promises = [];
/* Handle conditional flags with globals (probably not best apprach) */
var shaLib = "js-sha1";
var problem = console.error;
var verbose = false;
/* Put hashing in a function to cache and choose library */
function getDigest(data) {
    var offset = 0;
    var chunksize = 100000;
    if (shaLib == "rusha") {
        // rusha
        var hash = Rusha.createHash();
        while (true) {
            var segment = data.slice(offset, offset + chunksize);
            hash.update(segment);
            offset += chunksize;
            if (segment.length < chunksize) {
                break;
            }
        }
        var digest = hash.digest('hex');
    }
    else if (shaLib == "js-sha1") {
        // js-sha1
        var hash = sha1.create();
        while (true) {
            var segment = data.slice(offset, offset + chunksize);
            hash.update(segment);
            offset += chunksize;
            if (segment.length < chunksize) {
                break;
            }
        }
        var digest = hash.hex();
    }
    else {
        // the fallback?
        var digest = "NO SHA1 PERFORMED";
    }
    hashes_performed += 1;
    return digest;
}
/* Add hashes to all same-size finfos in an array (caching saves work) */
function hashFinfos(finfos) {
    return new Promise(function (resolve, reject) {
        var updatedFinfos = [];
        var size = finfos[0].size;
        for (var _i = 0, finfos_1 = finfos; _i < finfos_1.length; _i++) {
            var finfo = finfos_1[_i];
            // The cheap case is using a cached inode
            if (finfo.inode in inodeCache) {
                finfo.hash = inodeCache[finfo.inode];
                updatedFinfos.push(finfo);
            }
            // More work is actually performing a hash
            else {
                var data = fs.readFileSync(finfo.fname);
                var digestPromise = getDigest(data);
                promises.push(digestPromise);
                finfo.hash = digestPromise;
                inodeCache[finfo.inode] = finfo.hash;
                updatedFinfos.push(finfo);
            }
            ;
        }
        ;
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
 * Recursively walk directory
 *
 * Callbacks for found `fname` encountered will be provided with full
 * paths to the relevant file/directory
 *
 * @param dir Folder name you want to recursively process
 * @param found Callback function, returns all files with full path.
 ----------------------------------------------------------------------*/
var walkdir = function (dir, found) {
    var filenames = fs.readdirSync(dir);
    filenames.forEach(function (fname) {
        fname = path.resolve(dir, fname);
        var stat = fs.lstatSync(fname);
        if (stat && stat.isDirectory()) {
            walkdir(fname, found);
        }
        else if (stat.isSymbolicLink()) {
            // Skip these
            //problem(null, `Skipping symlink ${file}`);
        }
        else if (stat.isFile()) {
            var finfo = {
                fname: fname,
                size: stat.size,
                hash: null,
                inode: stat.ino
            };
            found(finfo);
        }
    });
};
/*---------------------------------------------------------------------
 * Populate the by_size data object size: Finfo
 *
 * Do not compute hashes until needed because of size dups
 *
 * @param finfo File information object following Finfo protocol
 ----------------------------------------------------------------------*/
var accum_by_size = function (finfo) {
    var size = finfo.size;
    // The first time we've seen this size
    if (!(size in by_size)) {
        // Add a "pseudo-hash" which may suffice
        finfo.hash = "<INODE " + finfo.inode + ">";
        by_size[size] = [finfo];
    }
    // This is a later time seeing this size 
    // May or may not need to hash prior entries
    else {
        var old_finfo = by_size[size][0];
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
            promises.push(hashFinfos(by_size[size]));
        }
    }
};
/*---------------------------------------------------------------------
 * Display report of files having identical content
 *
 * @param by_size Mapping of sizes to array of Finfo objects
 ----------------------------------------------------------------------*/
var showDups = function () {
    // Loop through all the file sizes in reverse order
    for (var _i = 0, _a = Object.keys(by_size).reverse(); _i < _a.length; _i++) {
        var size = _a[_i];
        var finfos = by_size[size];
        if ((+size > 0) && (finfos.length > 1)) {
            // New hash group
            var by_sha1 = {};
            // Loop through all the files of this size
            for (var _b = 0, finfos_2 = finfos; _b < finfos_2.length; _b++) {
                var finfo = finfos_2[_b];
                var sha1_1 = finfo.hash;
                if (!(sha1_1 in by_sha1)) {
                    // The first time we've seen this SHA1 digest
                    by_sha1[sha1_1] = [finfo];
                }
                else {
                    // Add more dups of this SHA1 digest to array
                    by_sha1[sha1_1].push(finfo);
                }
            }
            // Loop through by_sha1 specific to this filesize
            for (var _c = 0, _d = Object.entries(by_sha1); _c < _d.length; _c++) {
                var _e = _d[_c], sha1_2 = _e[0], finfos_4 = _e[1];
                if (finfos_4.length > 1) {
                    console.log("Size: " + size + " | SHA1: " + sha1_2);
                    for (var _f = 0, finfos_3 = finfos_4; _f < finfos_3.length; _f++) {
                        var finfo = finfos_3[_f];
                        console.log("  " + finfo.fname);
                    }
                }
            }
        }
    }
};
/* Usage: await sleep(ms) (within an async function only) */
function sleep(ms) {
    return new Promise(function (resolve) { return setTimeout(resolve, ms); });
}
/* Need async func to make sure all the promises are performed */
function waitForShowDups() {
    return __awaiter(this, void 0, void 0, function () {
        return __generator(this, function (_a) {
            // Call the actual report function once promises resolved
            Promise.allSettled(promises).then(function (results) {
                showDups();
                if (verbose) {
                    console.error("Promises: " + promises.length);
                    console.error("Hashes: " + hashes_performed);
                }
            });
            return [2 /*return*/];
        });
    });
}
/*---------------------------------------------------------------------
 * The "main()" steps of the script
 ----------------------------------------------------------------------*/
var cmd = cmd_ts_1.command({
    name: "find-dups",
    description: 'Find files with identical contents',
    version: '0.2',
    args: {
        rootdir: cmd_ts_1.positional({
            type: cmd_ts_1.string,
            displayName: "rootdir"
        }),
        jsSha1: cmd_ts_1.flag({
            long: "use-js-sha1",
            short: "j",
            type: cmd_ts_1.boolean
        }),
        rusha: cmd_ts_1.flag({
            long: "use-rusha-sha",
            short: "r",
            type: cmd_ts_1.boolean
        }),
        verbose: cmd_ts_1.flag({
            long: "verbose",
            short: "v",
            type: cmd_ts_1.boolean
        })
    },
    handler: function (args) {
        if (args.jsSha1) {
            shaLib = "js-sha1";
        }
        if (args.rusha) {
            shaLib = "rusha";
        }
        verbose = args.verbose;
        if (verbose) {
            console.error("SHA1-lib: " + shaLib);
            for (var _i = 0, _a = Object.keys(args); _i < _a.length; _i++) {
                var key = _a[_i];
                console.error(key + ": " + args[key]);
            }
        }
        walkdir(args.rootdir, accum_by_size);
        waitForShowDups();
    }
});
cmd_ts_1.run(cmd, process.argv.slice(2));
