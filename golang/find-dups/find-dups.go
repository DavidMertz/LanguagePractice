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
package main

import (
    "fmt"
    "os"
    "syscall"
    "flag"
    "time"
    "sort"
    "bytes"
    "path/filepath"
    "crypto/sha1"
    "golang.org/x/text/language"
    "golang.org/x/text/message"
)

type Finfo struct {
    fsize int64
    abspath string
    hash [20]byte
    inode uint64
}

// Let's count the total hashes done and avoided
var hashes_performed int = 0
var hashes_skipped int = 0

func WalkDir(dir string, c chan Finfo) error {
    return filepath.Walk(dir,
            func(path string, info os.FileInfo, e error) error {
        if e != nil {
            return e
        }
        if info.Mode().IsRegular() {
            abspath, err := filepath.Abs(path)
            if err != nil {
                fmt.Fprintf(os.Stderr, "SKIPPING %s\n", path)
            } else {
                var stat syscall.Stat_t
                if e2 := syscall.Stat(abspath, &stat); err != nil { panic(e2) }
                c <- Finfo{info.Size(), abspath, [20]byte{}, stat.Ino}
            }
        }
    return nil
    })
}

func FillHash(finfo Finfo, hashCache map[uint64][20]byte) Finfo {
    content, err := os.ReadFile(finfo.abspath)
    if err != nil {
        fmt.Fprintf(os.Stderr,
                    "UNAVAILABLE %s\n", finfo.abspath)
    } else {
        // Maybe we've already hashed and cached this inode
        hash, found := hashCache[finfo.inode]
        if found { 
            finfo.hash = hash
            hashes_skipped += 1
        // Hash might be uncalculated, and we need to perform it
        } else if (finfo.hash == [20]byte{}) {
            finfo.hash = sha1.Sum(content)
            hashes_performed += 1
            hashCache[finfo.inode] = finfo.hash
        // Might have been filled in with an inode "pseudo-hash"
        } else {
            hashes_skipped += 1
        }
    }
    return finfo
}

func ShowDups(sizes map[int64][]Finfo, dupsizes []int64,
              minSize int, maxSize int) {
    // Sort from large to small filesize
    sort.Slice(dupsizes, func(i, j int) bool {
                return dupsizes[i] > dupsizes[j] })

    for _, size := range dupsizes {
        if (size > int64(maxSize)) { continue }
        if (size < int64(minSize)) { continue }

        // Look for multiple paths all with same inode
        firstInode := sizes[size][0].inode
        allSameInode := true
        for _, finfo := range sizes[size] {
            if (finfo.inode != firstInode) { 
                allSameInode = false
                break
            }
        }
        if (allSameInode) {
            for index, finfo := range sizes[size] {
                strInode := fmt.Sprintf("<INODE %08d>    ", finfo.inode)
                var byteInode [20]byte
                copy(byteInode[:], strInode)
                sizes[size][index].hash = byteInode
            }
        }
        // Multiple inodes, possibly with multiple fnames in each
        sameHash := make(map[[20]byte][]string)
        hashCache := make(map[uint64][20]byte)

        for _, finfo := range sizes[size] {
            finfo = FillHash(finfo, hashCache)
            thisHash := sameHash[finfo.hash]
            sameHash[finfo.hash] = append(thisHash, finfo.abspath) 
        }

        // Print out the accumulated report with (pseudo)digests
        for hash, fnames := range sameHash {
            if (len(fnames) > 1) {
                fmt.Fprintf(os.Stdout, "Size: %d | ", size)
                if bytes.HasPrefix(hash[:], []byte("<INODE ")) {
                    fmt.Fprintf(os.Stdout, "SHA1: %s\n", hash)
                } else {
                    fmt.Fprintf(os.Stdout, "SHA1: %x\n", hash)
                }
                for _, abspath := range fnames {
                    fmt.Fprintf(os.Stdout, "  %s\n", abspath)
                }
            }
        }
    }
}

func main() {
    var maxSize int
    flag.IntVar(&maxSize, "max-size", 1e10,
                "Ignore files larger than max-size")
    flag.IntVar(&maxSize, "M", 1e10, "Ignore larger than (short flag)")
    var minSize int
    flag.IntVar(&minSize, "min-size", 1,
                "Ignore files smaller than min-size")
    flag.IntVar(&minSize, "m", 1, "Ignore smaller than (short flag")
    var verbose bool
    flag.BoolVar(&verbose, "verbose", false, 
                 "Display progress information on STDERR")
    flag.BoolVar(&verbose, "v", false, "Display info on STDERR (short flag)")
    flag.Parse()
    if (len(flag.Args()) == 0) {
        fmt.Fprintf(os.Stderr, "Usage: %s [options] directory\n", os.Args[0])
        flag.PrintDefaults()
        os.Exit(1)  // Exit with error code 1 for failure to parse command-line flags
    }
    dir := flag.Args()[0]

    p := message.NewPrinter(language.English)

    if (verbose) {
        p.Fprintf(os.Stderr, "max-size %d\n", maxSize)
        p.Fprintf(os.Stderr, "min-size %d\n", minSize)
        p.Fprintf(os.Stderr, "verbose %t\n", verbose)
        p.Fprintf(os.Stderr, "directory %s\n", dir)
    }
    // Mapping from size to {abspath, sha1}  
    sizes := make(map[int64][]Finfo)

    // Find the files then later fill in SHA1 hashes
    c := make(chan Finfo, 100)
    go WalkDir(dir, c)
    for {
        select {
            case finfo := <-c:
                sizes[finfo.fsize] = append(sizes[finfo.fsize], finfo)
            case <-time.After(time.Second):
                var dupsizes []int64
                for size, finfo := range sizes {
                    if (len(finfo) > 1) {
                        dupsizes = append(dupsizes, size)
                    }
                }
                ShowDups(sizes, dupsizes, minSize, maxSize)
                if (verbose) {
                    p.Fprintf(os.Stderr, 
                        "Hashes performed %d\n", hashes_performed)
                    p.Fprintf(os.Stderr,
                        "Hashes short-cut %d\n", hashes_skipped)
                }
                return
        }
    }
}
