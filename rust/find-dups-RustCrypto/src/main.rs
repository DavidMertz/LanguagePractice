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
extern crate crypto;

use std::env;
use std::fs;
use std::collections::btree_map::BTreeMap;
use std::collections::HashMap;
use std::vec::Vec;
use walkdir::WalkDir;
use self::crypto::digest::Digest;
use self::crypto::sha1::Sha1;

fn show_dups(filesizes: BTreeMap::<u64, Vec::<String>>) {
    for (size, files) in (&filesizes).iter().rev() {
        if files.len() > 1 {
            let mut samesize = HashMap::<String, Vec::<String>>::new();

            for path in files {
                let mut hash = Sha1::new();
                let content = match fs::read(path) {
                    Ok(content) => content,
                    Err(error) => {
                        eprintln!("UNAVAILABLE {} {:#?}", path, error);
                        continue } 
                };
                hash.input(&content);
                let mut files = Vec::<String>::new();
                match samesize.get(&hash.result_str()) {
                    Some(found) => {
                        for fname in found { files.push(fname.to_string()); }
                    },
                    None => { /* Nothing special */ }
                };
                files.push(path.to_string());
                samesize.insert(hash.result_str(), files);
            };
            for (hash, files) in samesize {
                if files.len() > 1 {
                    println!("Size {} | SHA1: {}", size, hash);
                    for path in files { println!("  {}", path); }
                }
            }
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let dir = &args[1];
    let mut filesizes = BTreeMap::<u64, Vec::<String>>::new();
    let tree = WalkDir::new(dir).into_iter();

    for e in tree.filter_map(|e| e.ok()) {
        let meta = e.metadata().unwrap();
        if meta.is_file() & (meta.len() > 0) {
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
