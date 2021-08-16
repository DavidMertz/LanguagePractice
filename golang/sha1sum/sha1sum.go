package main

import (
    "os"
    "fmt"
    "crypto/sha1"
)

func main () {
    fname := os.Args[1]
    content, err := os.ReadFile(fname)
    if err != nil {
        fmt.Fprintf(os.Stderr, "UNAVAILABLE %s\n", fname)
    } else {
        digest := sha1.Sum(content)
        fmt.Fprintf(os.Stdout, "%x  %s\n", digest, fname)
    }
}
