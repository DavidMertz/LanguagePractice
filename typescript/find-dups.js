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
var walkdir = function (dir, found, problem) {
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
                    walkdir(file, found, problem);
                }
                else if (stat.isSymbolicLink()) {
                    // Skip these
                    problem(null, "Skipping symlink " + file);
                }
                else if (stat.isFile()) {
                    var finfo = {
                        fname: file, size: stat.size, hash: null
                    };
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
var accum_by_size = function (finfo, problem) {
    var size = finfo.size;
    // The first time we've seen this size
    if (!(size in by_size)) {
        by_size[size] = [finfo];
        done += 1;
    }
    // The second time we've seen this size
    // Need to go back and hash the first entry as well
    else if (by_size[size].length == 1) {
        var old_finfo_1 = by_size[size][0];
        // Hash the existing entry (but don't push it again)
        fs.readFile(old_finfo_1.fname, function (err, data) {
            var hash = sha1.create();
            hash.update(data);
            if (err) {
                problem(err, old_finfo_1.fname);
            }
            else {
                old_finfo_1.hash = hash.hex(data);
            }
        });
        // Now also hash the new entry and push it to array
        fs.readFile(finfo.fname, function (err, data) {
            var hash = sha1.create();
            hash.update(data);
            if (err) {
                problem(err, finfo.fname);
            }
            else {
                finfo.hash = hash.hex(data);
                by_size[size].push(finfo);
                done += 1;
            }
        });
    }
    // The third or later time, just hash new entry contents
    else {
        fs.readFile(finfo.fname, function (err, data) {
            var hash = sha1.create();
            hash.update(data);
            if (err) {
                problem(err, finfo.fname);
            }
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
var show_dups = function (by_size) {
    // Loop through all the file sizes in reverse order
    for (var _i = 0, _a = Object.keys(by_size).reverse(); _i < _a.length; _i++) {
        var size = _a[_i];
        var finfos = by_size[size];
        if ((+size > 0) && (finfos.length > 1)) {
            // New hash group
            var by_sha1 = {};
            // Loop through all the files of this size
            for (var _b = 0, finfos_1 = finfos; _b < finfos_1.length; _b++) {
                var finfo = finfos_1[_b];
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
                var _e = _d[_c], sha1_2 = _e[0], finfos_3 = _e[1];
                if (finfos_3.length > 1) {
                    console.log("Size: " + size + " | SHA1: " + sha1_2 + "\"");
                    for (var _f = 0, finfos_2 = finfos_3; _f < finfos_2.length; _f++) {
                        var finfo = finfos_2[_f];
                        console.log("  " + finfo.fname);
                    }
                }
            }
        }
    }
};
/* Utility function utilizing timeout Promise to sleep */
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
                    return [4 /*yield*/, sleep(100)];
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
var dir = process.argv[2];
walkdir(dir, accum_by_size, console.error);
ShowDups(by_size);
