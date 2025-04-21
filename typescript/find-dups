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
var by_size = {};
/* Callbacks and completion of each for a spin-wait */
var todo = 0;
var done = 0;
var hashes_performed = 0;
var problem = console.error;
/* Put hashing in a function to cache and choose library */
var getDigest = function (finfo) {
    var data = fs.readFileSync(finfo.fname);
    // js-sha1
    var hash = sha1.create();
    hash.update(data);
    var digest = hash.hex();
    finfo.hash = digest;
    return digest;
    // rusha
    // const hash = Rusha.createHash().update(data);
    // return hash.digest('hex')
};
/* Function to hash an array of Finfo objects */
var hashAll = function (finfos) {
    var hashCache = {};
    for (var _i = 0, finfos_1 = finfos; _i < finfos_1.length; _i++) {
        var finfo = finfos_1[_i];
        if (finfo.inode in hashCache) {
            finfo.hash = hashCache[finfo.inode];
        }
        else {
            getDigest(finfo);
            by_size[finfo.size].push(finfo);
            hashCache[finfo.inode] = finfo.hash;
            done += 1;
            hashes_performed += 1;
        }
        ;
    }
    ;
};
/*---------------------------------------------------------------------
 * Recursively walk directory
 *
 * Callbacks for found `file` encountered will be provided with full
 * paths to the relevant file/directory
 *
 * @param dir Folder name you want to recursively process
 * @param found Callback function, returns all files with full path.
 ----------------------------------------------------------------------*/
var walkdir = function (dir, found) {
    fs.readdir(dir, function (err, list) {
        if (err) {
            return problem(err, dir);
        }
        list.forEach(function (file) {
            file = path.resolve(dir, file);
            fs.lstat(file, function (err2, stat) {
                if (err2) {
                    problem(err, file);
                }
                else if (stat && stat.isDirectory()) {
                    walkdir(file, found);
                }
                else if (stat.isSymbolicLink()) {
                    // Skip these
                    problem(null, "Skipping symlink ".concat(file));
                }
                else if (stat.isFile()) {
                    var finfo = {
                        fname: file,
                        size: stat.size,
                        hash: null,
                        inode: stat.ino
                    };
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
var accum_by_size = function (finfo) {
    var size = finfo.size;
    // The first time we've seen this size
    if (!(size in by_size)) {
        finfo.hash = "<INODE ".concat(finfo.inode, ">");
        by_size[size] = [finfo];
    }
    // Subsequent files of the same size...
    // (Maybe) need to go back and add hash to first entry too
    else {
        var thisSize = by_size[size];
        var first_finfo = thisSize[0];
        // Trick is whether we actually have multiple inodes already
        // If not, we can just keep adding the pseudo-hash
        if (first_finfo.inode == finfo.inode &&
            first_finfo.hash.startsWith("<INODE")) {
            finfo.hash = first_finfo.hash;
        }
        else {
            hashAll(by_size[size]);
        }
        ;
    }
    done += 1;
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
                    console.log("Size: ".concat(size, " | SHA1: ").concat(sha1_2));
                    for (var _f = 0, finfos_3 = finfos_4; _f < finfos_3.length; _f++) {
                        var finfo = finfos_3[_f];
                        console.log("  ".concat(finfo.fname));
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
function ShowDups(by_size) {
    return __awaiter(this, void 0, void 0, function () {
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0: 
                // Initial sleep to make sure some files are in TODO
                return [4 /*yield*/, sleep(100)];
                case 1:
                    // Initial sleep to make sure some files are in TODO
                    _a.sent();
                    _a.label = 2;
                case 2:
                    if (!(todo != done)) return [3 /*break*/, 4];
                    return [4 /*yield*/, sleep(10)];
                case 3:
                    _a.sent();
                    return [3 /*break*/, 2];
                case 4:
                    // Now call the actual report function
                    show_dups(by_size);
                    return [2 /*return*/];
            }
        });
    });
}
/*---------------------------------------------------------------------
 * The "main()" steps of the script
 ----------------------------------------------------------------------*/
var cmd = command({
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
    handler: function (args) {
        if (args.jsSha1) {
            shaLib = "js-sha1";
        }
        if (args.rusha) {
            shaLib = "rusha";
        }
        verbose = args.verbose;
        if (verbose) {
            console.error("SHA1-lib: ".concat(shaLib));
            for (var _i = 0, _a = Object.keys(args); _i < _a.length; _i++) {
                var key = _a[_i];
                console.error("".concat(key, ": ").concat(args[key]));
            }
        }
        walkdir(args.rootdir, accum_by_size);
        waitForShowDups();
    }
});
run(cmd, process.argv.slice(2));
var dir = process.argv[2];
walkdir(dir, accum_by_size);
ShowDups(by_size);
