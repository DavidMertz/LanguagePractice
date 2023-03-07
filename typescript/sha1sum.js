#!/usr/bin/env node
"use strict";
exports.__esModule = true;
var fs = require("fs");
var Rusha = require("rusha");
var fname = process.argv[2];
fs.readFile(fname, function (err, data) {
    var hash = Rusha.createHash().update(data);
    console.log(hash.digest('hex') + "  " + fname);
});
