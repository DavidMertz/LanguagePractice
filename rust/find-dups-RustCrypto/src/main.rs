/* Created by David Mertz

Given a root directory, recurse in it and find all the duplicate files.
i.e.  files that have the same contents, but not necessarily the same
filename.

-----
This code is released as CC-0
http://creativecommons.org/publicdomain/zero/1.0/

The person who associated a work with this deed has dedicated the work
to the public domain by waiving all of his or her rights to the work
worldwide under copyright law, including all related and neighboring
rights, to the extent allowed by law.

You can copy, modify, distribute and perform the work, even for
commercial purposes, all without asking permission. See Other
Information below.
*/
extern crate walkdir;
extern crate clap;

use std::fs;
use std::collections::btree_map::BTreeMap;
use std::collections::HashMap;
use std::vec::Vec;
use walkdir::WalkDir;
use sha1::Digest;
use clap::{Arg, App};

fn show_dups(filesizes: BTreeMap::<u64, Vec::<String>>) {
    for (size, files) in (&filesizes).iter().rev() {
        if files.len() > 1 {
            let mut samesize = HashMap::<String, Vec::<String>>::new();

            for path in files {
                let content = match fs::read(path) {
                    Ok(content) => content,
                    Err(error) => {
                        eprintln!("UNAVAILABLE {} {:#?}", path, error);
                        continue } 
                };
                let digest = sha1::Sha1::digest(&content);
                let hexdigest = format!("{:x}", digest);

                let mut files = Vec::<String>::new();
                match samesize.get(&hexdigest) {
                    Some(found) => {
                        for fname in found { files.push(fname.to_string()); }
                    },
                    None => { /* Nothing special */ }
                };
                files.push(path.to_string());
                samesize.insert(hexdigest, files);
            };
            for (digest, files) in samesize {
                if files.len() > 1 {
                    println!("Size {} | SHA1: {}", size, digest);
                    for path in files { println!("  {}", path); }
                }
            }
        }
    }
}

fn main() {
	let args = App::new("Find Duplicate File Contents")
		.version("0.1")
		.author("David Mertz <mertz@kdm.training>")
		.about("Find Duplicate File Contents")
		.arg(Arg::with_name("minSize")
		   	.short("m")
		   	.long("min-size")
		   	.help("Ignore files smaller than min-size")
		   	.takes_value(true))
		.arg(Arg::with_name("maxSize")
		   	.short("M")
		   	.long("max-size")
		   	.help("Ignore files larger than max-size")
		   	.takes_value(true))
		.arg(Arg::with_name("sizeOnly")
		   	.long("size-only")
		   	.help("File match if same-size larger than size-only")
		   	.takes_value(true))
		.arg(Arg::with_name("DIR")
		   	.help("Root of directory tree to compare")
		   	.required(true)
		   	.index(1))
		.arg(Arg::with_name("verbose")
		   	.short("v")
			.long("verbose")
		    .multiple(false)
		    .help("Display extra information on STDERR"))
		.get_matches();

    let dir = args.value_of("DIR").unwrap();
    let min_size = match args.value_of("minSize") {
        None => 1,
        Some(s) => match s.parse::<u64>() {
            Ok(n) => n,
            Err(_) => { 
                eprintln!("An integer is required for min-size"); 0 },
        }
    };
    let max_size = match args.value_of("maxSize") {
        None => 10_000_000_000,
        Some(s) => match s.parse::<u64>() {
            Ok(n) => n,
            Err(_) => { 
                eprintln!("An integer is required for max-size"); 0 },
        }
    };
    let _size_only = match args.value_of("sizeOnly") {
        None => 1_000_000_000,
        Some(s) => match s.parse::<u64>() {
            Ok(n) => n,
            Err(_) => {
                eprintln!("An integer is required for size-only"); 0 },
        }
    };
    let verbose = args.is_present("verbose");
    if verbose {
        eprintln!("Arguments given are: {:#?}", args);
    }

    let mut filesizes = BTreeMap::<u64, Vec::<String>>::new();
    let tree = WalkDir::new(dir).into_iter();

    for e in tree.filter_map(|e| e.ok()) {
        let meta = e.metadata().unwrap();
        let fsize = meta.len();
        if meta.is_file() & (fsize >= min_size) & (fsize <= max_size) {
            let mut files = Vec::<String>::new();
            match filesizes.get(&meta.len()) {
                Some(found) => {
                    for fname in found { files.push(fname.to_string()); }
                },
                None => { /* Nothing special */ }
            };
            let path = match e.path().canonicalize() {
                Ok(path) => path.display().to_string(),
                Err(error) => {
                    eprintln!("Cannot canonicalize {} {:#?}", 
                              e.path().display(), error);
                    continue } 
            };
            files.push(path);
            filesizes.insert(meta.len(), files);
        }
    }
    show_dups(filesizes);
}
