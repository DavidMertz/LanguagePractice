# Language Practice

Scratch pad to try out some different programming languages.

Python versions may be created as reference versions since I understand 
that language very well already.

# Languages I'd like to learn (or know better)

Checkmarks used to indicate at least a reasonable first version of specified tools have been created.

- [x] Python
- [x] Python (parallel)
- [x] Rust
- [x] Go
- [x] Julia
- [x] TypeScript
- [x] Haskell
- [x] Ruby
- [ ] Kotlin (JVM tooling is PITA; back-burner)
- [ ] Dart (I realize I don't care about UI; back-burner)
- [x] Bash (baseline)

# Tasks/scripts to create

## Find duplicate contents

A compliant implementation must do the following:

* Have an executable named `find-dups`.
* Take as a command-line argument the root path to examine for duplicate contents.
* Accept flags for `min-size` and `max-size` files to consider (any files outside the size range are simply ignored by the tool's report).
* A report should be produced on STDOUT. Optionally, informational messages may be written to STDERR.  The report should resemble:

```
% golang/find-dups/find-dups .
Size: 336 | SHA1: de54dc09d752c3708b92fc9af45eb54970348d6b
  /home/dmertz/git/LanguagePractice/.git/logs/HEAD
  /home/dmertz/git/LanguagePractice/.git/logs/refs/heads/main
Size: 45 | SHA1: b1713adaf16c3f6c66a004116b83cd15d88d2fd6
  /home/dmertz/git/LanguagePractice/rust/hello/hello.rs
  /home/dmertz/git/LanguagePractice/rust/hello2/src/main.rs
Size: 41 | SHA1: fc7c94eecb0631af36c2db841ad1b8c0d080a289
  /home/dmertz/git/LanguagePractice/.git/refs/heads/main
  /home/dmertz/git/LanguagePractice/.git/refs/remotes/origin/main
```

That is, a line should announce the size of files with identical contents
and the associated hash (SHA1 is used in example, and is probably a good
choice; another hash, or another technique altogether, is permitted). Sets
of duplicates must appear in descending order by size.

Two or more following lines will be indented by two spaces and contain the
full absolute path to the duplicate in question. Such duplicates appear in
unspecified order within their section.

### Notes on performance

As of commit f32658f, the performance of the various versions is approximately
as shown:

Each language reports file count and time elapsed for CONDA_PREFIX

| Language (options)   | Sanity chk  | Wall clock time
|----------------------|-------------|----------------
| Golang               | dups 457742 | 17 secs
| Rust (rust-crypto)   | dups 457742 | 19 secs
| Rust (RustCrypto)    | dups 457742 | 20 secs
| Ruby                 | dups 457742 | 20 secs
| Python               | dups 457742 | 22 secs
| Julia (-O3)          | dups 457742 | 27 secs
| Haskell              | dups 457742 | 79 secs
| TypeScript (js-sha1) | dups 457742 | 79 secs
| TypeScript (Rusha)   | dups 457742 | 94 secs

Two optimizations have been noticed and implemented in Python, Julia, Ruby,
Golang, Rust, and TypeScript. 

* For paths of the same size that are actually hard links to the same inode,
the file was initially hashed multiple times.  Simply borrowing the hash of
what is, after all, the identical on-disk location, is cheaper.  This fix makes
the Python version about 20% faster.

* A special case of this is where, in fact, **all** paths of a given size are
to the same inode.  As well as taking advantage of the optimization "don't hash
hard links *multiple times*", I extended this to "don't hash size-duplication
sets **at all** if they are all the same inode." 

I first realized the second case in implementing Ruby.  Given that I've made no
attempt (so far) at parallelism in Ruby, its near-top place surprises me
greatly now that several other languages have that same optimization.

Haskell uses the `-threaded` option, but discovers only a little bit of
parallelism.  However, working out the optimizations in the algorithm will
likely make much more difference than more parallelism.

Julia was relatively easy to parallelize, and in that case, parallelism
brought the performance from dreadful to mediocre.

The Rust version now has a switch  between the unmaintained `rust-crypto`
library which is substantially faster and the "official" `RustCrypto` one.
Also, the optimization level in Rust turns out to make A LOT of difference.

The Node.js JIT interpreter (TypeScript) manages to find a lot of parallism for
Promises. However, I simply could not coordinate the asynchronous
`fs.readdir()` for the range of tree sizes.  I had initially used hand-tuned
delays to do "enough" tree-walking before doing the parallel hashing
(otherwise, the script would end prematurely when nothing was "queued" in
Promises; but that was sensitive to the exact root directory.  The main problem
in the TypeScript implementation is that the underlying SHA1 implementation in
JavaScript is about 11x slower than the best ones, even with the JIT making a
relatively noble attempt (see `benchmark-justsha` and the corresponding
implementations of `sha1sum`). Analogous to Rust, one library that is faster
for a single operation is slower when parallelized.

### Notes on validation

Several languages now have a verbose mode that now reports, among other
things, the number of hashes actually performed and those skipped in the
optimizations.  For example, comparing Rust (the latest update) with Golang,
we see they are entirely consistent:

```
% rust/find-dups/target/release/find-dups -v $CONDA_PREFIX | wc -l
verbose true
min-size 1
max-size 10,000,000,000
RustCrypto false
rootdir /home/dmertz/miniconda3
Hashes performed: 210,171
Hashes skipped: 285,026
568829

% golang/find-dups/find-dups -v $CONDA_PREFIX | wc -l
max-size 10,000,000,000
min-size 1
verbose true
directory /home/dmertz/miniconda3
Hashes performed 210,171
Hashes short-cut 285,026
568829
```

## something-else ..
