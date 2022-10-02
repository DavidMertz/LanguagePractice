extern crate crypto;

use std::fs;
use std::env;
use self::crypto::digest::Digest;
use self::crypto::sha1::Sha1;

fn main() {
    let args: Vec<String> = env::args().collect();
    let fname = &args[1];
    let content = match fs::read(fname) {
        Ok(content) => content,
        Err(error) => panic!("File error: {}", error),
    };
    let mut hash = Sha1::new();
    hash.input(&content);
    let hexdigest = hash.result_str();
    println!("{}  {}", hexdigest, fname);
}
