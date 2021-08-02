package main

import (
    "fmt"
    "os"
    "flag"
	"time"
    "sort"
    "path/filepath"
    "crypto/sha1"
)

type Finfo struct {
    fsize int64
    abspath string
    hash [20]byte
}

func WalkDir(dir string, c chan Finfo) error {
    return filepath.Walk(dir,
			func(path string, info os.FileInfo, e error) error {
        if e != nil {
            return e
        }
        // check if it is a regular file (not dir)
        if info.Mode().IsRegular() {
            abspath, err := filepath.Abs(path)
            if err != nil {
                fmt.Fprintf(os.Stderr, "SKIPPING %s\n", path)
            } else {
                c <- Finfo{info.Size(), abspath, [20]byte{}}
            }
        }
    return nil
    })
}

func FillHash(info Finfo, sizeOnly int) Finfo {
    content, err := os.ReadFile(info.abspath)
    if err != nil {
        fmt.Fprintf(os.Stderr,
                    "UNAVAILABLE %s\n", info.abspath)
    } else {
        // If over size-max, use nulls as hash value
        // i.e. perhaps skip work of large SHA calculation
        if (info.fsize <= int64(sizeOnly)) {
            info.hash = sha1.Sum(content)
        }
    }
    return info
}

func ShowDups(sizes map[int64][]Finfo, dupsizes []int64,
              minSize int, maxSize int) {
    // Sort from large to small filesize
    sort.Slice(dupsizes, func(i, j int) bool {
                return dupsizes[i] > dupsizes[j] })

    sameHash := make(map[[20]byte][]string)
    for _, size := range dupsizes {
        if (size > int64(maxSize)) { continue }
        if (size < int64(minSize)) { continue }
        fmt.Fprintf(os.Stdout, "Size: %d\n", size)
        for _, info := range sizes[size] {
            //sameHash = make(map[[20]byte][]string)
            sameHash[info.hash] = append(
                            sameHash[info.hash], info.abspath)  
        }
        fmt.Println(sameHash)
            //fmt.Fprintf(os.Stdout, "  %s\n", info.abspath)
            //fmt.Fprintf(os.Stdout, "  %x\n", info.hash)
    }
}

func main() {
    sizeOnly := flag.Int("size-only", 1e9,
        "Files match if same-size larger than size-only")
    maxSize := flag.Int("max-size", 1e10,
        "Ignore files larger than max-size")
    minSize := flag.Int("min-size", 1,
        "Ignore files smaller than min-size")
    verbose := flag.Bool("verbose", false,
        "Display progress information on STDERR")
    flag.Parse()
    dirs := flag.Args()

    if (*verbose) {
        fmt.Fprintf(os.Stderr, "size-only %d\n", *sizeOnly)
        fmt.Fprintf(os.Stderr, "max-size %d\n", *maxSize)
        fmt.Fprintf(os.Stderr, "min-size %d\n", *minSize)
        fmt.Fprintf(os.Stderr, "verbose %t\n", *verbose)
        fmt.Fprintf(os.Stderr, "%s\n", dirs)
    }
	// Mapping from size to {abspath, sha1}  
	sizes := make(map[int64][]Finfo)

    // Find the files then later fill in sha1 hashes
    for _, s := range dirs {
        c := make(chan Finfo, 100)
        go WalkDir(s, c)
		for {
			select {
				case info := <-c:
                    // TODO: We'd like asynchronous goroutine!
                    info = FillHash(info, *sizeOnly)
                    sizes[info.fsize] = append(sizes[info.fsize], info)
				case <-time.After(time.Second):
                    var dupsizes []int64
                    for size, info := range sizes {
                        if (len(info) > 1) {
                            dupsizes = append(dupsizes, size)
                        }
                    }
                    go ShowDups(sizes, dupsizes, *minSize, *maxSize)
					return
            }
        }
    }
}
