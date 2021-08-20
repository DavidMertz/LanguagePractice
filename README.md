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
- [ ] Haskell
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
* Accept a flag `size-only` which indicates that above a certain size, hashing of a file can be omitted, and equality of size will simply be assumed to indicate equality of content.
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

### Notes on "duplicate"

There is a question here of what is a "file".  Specifically, there might be
symbolic links or hard links in the root examined.  Should these be
reported?

The current tools all ignore symlinks (in Python, with the `-i` option).
Taking a look at a snapshot of my $CONDA_PREFIX, currently, I see:

```bash
# All files
% find -L $CONDA_PREFIX -type f | wc -l
501153
# Exclude symlinks
% find $CONDA_PREFIX -type f | wc -l
449667
# Distinct inodes
% find $CONDA_PREFIX -type f -print0 | 
    xargs -0 ls -i | 
    cut -c-8 | 
    tr -d ' ' | 
    sort -u | 
    wc -l
204763
```

As of 2021-08-20 (rev 1da6f99..c002893), instrumenting the Haskell version
produces slightly confusing results.

* Haskell finds 204763 inodes (matching `find`)
* Haskell finds 501153 total files (matching `find`)
* Haskell finds 464553 "regular" files (excluding symlinks)

The last part is the confusion.  What are these 14,886 files that are not
symlinks?!

The number of duplicates Haskell identifies are greater by a similar number.
This more-or-less makes sense given the extra files being examined, whatever
they actually are.

In terms of the actual `find-dups` tools, all except Haskell version,
identify 407813 "duplicates."  Haskell finds 423918; this difference is
about 16,105 which is to say slightly more than the number of extra
non-symlinks.  It makes some sense that adding files makes new duplicates.
In at least some examples, there seem to be file names sharing an inode that
Haskell finds but other tools ignore for mysterious reasons.

## something-else ...
