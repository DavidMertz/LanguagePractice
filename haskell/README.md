# Notes on "duplicates"

There is a question here of what is a "file".  Specifically, there might be
symbolic links or hard links in the root examined.  Should these be reported?

The current tools all ignore symlinks (in Python, without the `-l` option).
Taking a look at a snapshot of my $CONDA_PREFIX, currently (2021-08-20), I see:

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

As of rev 1da6f99..c002893, instrumenting the Haskell version
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
Haskell finds but other tools ignore for unclear reasons.

## Update 2021-08-21

 My friend Brad Huntting had helped me work through the underlying conceptual
 problem.  Sometimes "regular" files actually live inside symlink directories.
 My first approach which failed to consider this is retained as the function
 `getAllFilesNoSymFiles`. 

The implementation was "fixed" using a suggestion by StackOverflow user ChrisB.
An initial alternate implemenation he suggested reduced, but did not eliminate,
the number non-excluded files inside symlink directories.  After noting this,
ChrisB provided a fixed implementation that also uses a much more easily
understood `do` notation, but does not utilize `pathWalkLazy` (being more
"hand-rolled").
