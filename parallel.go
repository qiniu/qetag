package main

import (
	"fmt"
	"io"
	"os"
	"bytes"
	"crypto/sha1"
	"encoding/base64"
	"runtime"
	"time"
)

const (
	BLOCK_SIZE int64 = 4194304
)

func main() {
	ts := time.Now()
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, `Usage: qetag <filename>`)
		return
	}

	etag, err := GetEtag(os.Args[1])
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return
	}
	fmt.Println(etag)
	duration := time.Since(ts)
	fmt.Println(duration.String())
}

func GetEtag(filename string) (etag string, err error) {

	f, err := os.Open(filename)
	if err != nil {
		return
	}

	fi, err := f.Stat()
	if err != nil {
		return
	}

	fsize := fi.Size()
	f.Close()

	return GetEtagMain(filename, fsize), nil
}

func GetEtagMain(filename string, fsize int64) (etag string) {
	file, _ := os.Open(filename)
	defer file.Close()

	blocks := BlockCount(fsize)
	sha1Buf := make([]byte, 0, 21)
	if blocks <= 1 {
		sha1Buf = SmallEtag(file, sha1Buf)
	} else {
		sha1Buf = BigEtag(file, sha1Buf, blocks)
	}
	etag = base64.URLEncoding.EncodeToString(sha1Buf)
	return
}

func SmallEtag(file io.Reader, sha1Buf []byte) ([]byte) {
	sha1Buf = append(sha1Buf, 0x16)
	sha1Buf = CalSha1(sha1Buf, file)
	return sha1Buf
}

func StartWorker(file io.ReaderAt, jobs <-chan int, resultChan chan <-map[int][]byte) {
	for j := range jobs {
		data := io.NewSectionReader(file, int64(j) * BLOCK_SIZE, BLOCK_SIZE)
		sha1Bytes := CalSha1(nil, data)
		resultChan <- map[int][]byte{
			j: sha1Bytes,
		}
	}
}

func BigEtag(file io.ReaderAt, sha1Buf []byte, blocks int64) []byte {
	cores := runtime.NumCPU()
	poolSize := cores

	resultChan := make(chan map[int][]byte, blocks)
	jobs := make(chan int, blocks)

	for w := 1; w <= poolSize; w++ {
		go StartWorker(file, jobs, resultChan)
	}

	for j := 0; j < int(blocks); j++ {
		jobs <- j
	}
	close(jobs)

	final := combiSha1(resultChan, blocks)
	return final
}

func combiSha1(resultChan chan map[int][]byte, blocks int64) []byte{
	Sha1Map := make(map[int][]byte, 0)

	for a := 0; a < int(blocks); a++ {
		eachChan := <-resultChan
		for k, v := range eachChan {
			Sha1Map[k] = v
		}

	}
	blockSha1 := make([]byte, 0, blocks * 20)
	for i := 0; int64(i) < blocks; i++ {
		blockSha1 = append(blockSha1, Sha1Map[i]...)
	}

	final := make([]byte, 0, 21)
	final = append(final, 0x96)
	final = CalSha1(final, bytes.NewReader(blockSha1))

	return final
}

func CalSha1(b []byte, r io.Reader) ([]byte) {

	h := sha1.New()
	io.Copy(h, r)
	return h.Sum(b)
}

func BlockCount(fsize int64) int64 {

	var blocks int64 = 1
	if fsize <= BLOCK_SIZE {
		return blocks
	} else {
		blocks := fsize / BLOCK_SIZE
		if fsize % BLOCK_SIZE == 0 {
			return blocks
		} else {
			return blocks + 1
		}
	}
}