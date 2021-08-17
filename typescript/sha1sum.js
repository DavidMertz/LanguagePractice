#!/usr/bin/env node
"use strict";
exports.__esModule = true;
var fs = require("fs");
var sha1 = require("js-sha1");
var fname = process.argv[2];
fs.readFile(fname, function (err, data) {
    var hash = sha1.create();
    hash.update(data);
    console.log(hash.hex() + "  data");
});
