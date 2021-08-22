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
- [ ] Ruby
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

As of commit 03eeed7..96f62bd, the performance of the various versions is
approximately as shown:

Each language reports file count and time elapsed for CONDA_PREFIX

| Language (options) | Sanity chk  | Wall clock time
|--------------------|-------------|----------------
| Golang             | dups 407839 | 27 secs
| Python             | dups 407839 | 28 secs
| Julia              | dups 407839 | 42 secs
| Haskell            | dups 407839 | 66 secs
| Rust (rust-crypto) | dups 407839 | 169 secs
| Rust (RustCrypto)  | dups 407839 | 196 secs
| TypeScript ->.js   | dups 407839 | 332 secs

To be fair, an optimization was noticed that has only been implemented in
Python and Julia so far (possibly in a slightly better way in Julia).
Specifically, for files of the same size that are actually hard links to the
same inode, the file will be hashed multiple times.  Simply borrowing the hash
of what is, after all, the identical on-disk location, is presumably cheaper.
This fix makes the Python version about 20% faster.

The Golang version that does not (yet) do this optimization is about tied for
speed with Python currently.  I expect less speedup from doing this in Golang
simply because the Goroutines already peg all my CPU cores.  Therefore, the
needless work is also (probably) parallel work, and wall clock time will
(probably) be little affected.  Haskell can probably be improved since the
`-threaded` option discovers only a little bit of parallelism.

Rust is the troubling outlier here, having a reputation as a "fast" language.
However, any parallelism that might be found has to be added explicitly, and a
bit laboriously. The current versions are strictly single-core! 

Even the Node.js JIT interpreter (TypeScript) manages to find a lot of
parallism for asynchronous callbacks.  The problem there is that the underlying
SHA1 implementation in JavaScript is about 11x slower than the best ones, even
with the JIT making a noble attempt (see `benchmark-justsha` and the
corresponding implementations of `sha1sum`).

## something-else ...
