use std::fs;
use std::env;
use sha1::Digest;

fn main() {
    let args: Vec<String> = env::args().collect();
    let fname = &args[1];
    let content = match fs::read(fname) {
        Ok(content) => content,
        Err(error) => panic!("File error: {}", error),
    };
    let digest = sha1::Sha1::digest(&content);
    let hexdigest = format!("{:x}", digest);
    println!("{}  {}", hexdigest, fname);
}
