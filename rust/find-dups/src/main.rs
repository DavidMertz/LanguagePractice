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

use sha1::{Sha1, Digest};

use std::fs;
use std::os::unix::fs::MetadataExt;
use std::collections::btree_map::BTreeMap;
use std::collections::{HashMap, HashSet};
use std::vec::Vec;
use walkdir::WalkDir;
use clap::{Arg, App};
use num_format::{Locale, ToFormattedString};

fn get_hexdigest(path: &String, 
                 hash_cache: &mut HashMap<u64, String>) -> (String, bool) {
    // If we have cached this inode, no need to hash again
    let inode = get_inode(path.to_string());
    let digest = match hash_cache.get(&inode) {
        None => "NOT HASHED",
        Some(digest) => digest,
    };
    if digest != "NOT HASHED" {
        // Return false in tuple since no new read/hash performed
        return (digest.to_string(), false);
    }

    // So we need to read the file then hash it (using selected library)
    let content = match fs::read(path) {
        Ok(content) => content,
        Err(error) => {
            eprintln!("UNAVAILABLE {} {:#?}", path, error); vec![] } 
    };
    let mut hasher = Sha1::new();
    hasher.update(&content);
    let digest = hasher.finalize();        
    let hexdigest = format!("{:x}", digest);
    hash_cache.insert(inode, hexdigest.clone());
    return (hexdigest, true);
}

fn get_inode(path: String) -> u64 {
    match fs::metadata(path) {
        Ok(meta) => meta.ino(),
        Err(_) => 0,
    }
}

fn count_inodes(paths: Vec<String>) -> (bool, u64) {
    let mut inodes = HashSet::new();
    let mut inode_problem = 0;
    for path in paths {
        let mut inode = get_inode(path.to_string());
        if inode == 0 {
            // We assume this problem is rare, and no inodes are
            // coincidentally a very small number. Almost certainly
            // if getting the file meatadata fails, then reading 
            // the file for the hash will fail as well.
            eprintln!("Cannot obtain inode for {}!", path);
            inode_problem += 1;
            inode += inode_problem;
            // Distinct inodes will force hashing
        }
        inodes.insert(inode);
    }
    // Only one inode, return the actual inode value, else count
    if inodes.len() == 1 {
        let mut ret: u64 = 0;
        for inode in inodes.iter() {
            ret = *inode; break;
        }
        return (true, ret);
    } else {
        return (false, inodes.len() as u64);
    }
}

fn show_dups(filesizes: BTreeMap::<u64, Vec::<String>>) -> (u32, u32) {
    let mut hashcount: u32 = 0;
    let mut hashskip: u32 = 0;

    for (size, paths) in (&filesizes).iter().rev() {
        if paths.len() > 1 {
            let mut samesize = HashMap::<String, Vec::<String>>::new();

            // Might have many paths all linked naming identical inode
            let (unique, inode) = count_inodes(paths.to_vec());
            if unique {
                println!("Size {} | SHA1: <INODE {}>", size, inode);
                for path in paths { println!("  {}", path); };
                hashskip += paths.len() as u32;
                continue;
            }

            // The "regular" circumstance of needing to find hashes
            let mut hash_cache = HashMap::<u64, String>::new();
            for path in paths {
                let (hexdigest, newhash) = get_hexdigest(
                            &path.to_string(), &mut hash_cache);
                if newhash { hashcount += 1; }
                else { hashskip += 1; };

                let mut fnames = Vec::<String>::new();
                match samesize.get(&hexdigest) {
                    Some(found) => {
                        for fname in found { fnames.push(fname.to_string()); }
                    },
                    None => { /* Nothing special */ },
                };
                fnames.push(path.to_string());
                samesize.insert(hexdigest, fnames);
            };

            // Report results for size (unless same-inode shortcut taken)
            for (digest, fnames) in samesize {
                if fnames.len() > 1 {
                    println!("Size {} | SHA1: {}", size, digest);
                    for path in fnames { println!("  {}", path); }
                }
            }
        }
    }
    return (hashcount, hashskip);
}

fn main() {
	let args = App::new("Find Duplicate File Contents")
		.version("2025.04")
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
    let verbose = args.is_present("verbose");
    if verbose {
        eprintln!("verbose {}", verbose);
        eprintln!("min-size {}", min_size.to_formatted_string(&Locale::en));
        eprintln!("max-size {}", max_size.to_formatted_string(&Locale::en));
        eprintln!("rootdir {}", dir);
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
                None => { /* Nothing special */ },
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
    let (hashcount, hashskip) = show_dups(filesizes);

    if verbose {
        eprintln!("Hashes performed: {}", 
                  hashcount.to_formatted_string(&Locale::en));
        eprintln!("Hashes skipped: {}", 
                  hashskip.to_formatted_string(&Locale::en));
    }
}
