package main

import (
	"fmt"
	"io"
	"os"
	"bytes"
	"crypto/sha1"
	"encoding/base64"
)

const (
	BLOCK_BITS = 22 // Indicate that the blocksize is 4M
	BLOCK_SIZE = 1 << BLOCK_BITS
)

func BlockCount(fsize int64) int {

	return int((fsize + (BLOCK_SIZE-1)) >> BLOCK_BITS)
}

func CalSha1(r io.Reader) (sha1Code []byte, err error) {

	h := sha1.New()
	_, err = io.Copy(h, r)
	if err != nil {
		return
	}
	sha1Code = h.Sum(nil)
	return 
}

func GetEtag(filename string) (etag string, err error) {

	f, err := os.Open(filename)
	if err != nil {
		return
	}
	defer f.Close()

	fi, err := f.Stat()
	if err != nil {
		return
	}
	fsize := fi.Size()
	blockCnt := BlockCount(fsize)
	sha1Buf := make([]byte, 0, 21)

	var sha1Code []byte
	if blockCnt <= 1 { // file size <= 4M
		sha1Buf = append(sha1Buf, 0x16)
		sha1Code, err = CalSha1(f) 
		if err != nil {
			return
		}
		sha1Buf = append(sha1Buf, sha1Code...)
	} else { // file size > 4M
		sha1Buf = append(sha1Buf, 0x96)
		sha1BlockBuf := make([]byte, 0, blockCnt * 20)
		for i := 0; i < blockCnt; i ++ {
			body := io.LimitReader(f, BLOCK_SIZE)
			sha1Code, err = CalSha1(body)
			if err != nil {
				return
			}
			sha1BlockBuf = append(sha1BlockBuf,sha1Code...)
		}
		tmpBuf := bytes.NewBuffer(sha1BlockBuf)
		sha1Final, _ := CalSha1(tmpBuf)
		sha1Buf = append(sha1Buf, sha1Final...)
	}
	etag = base64.URLEncoding.EncodeToString(sha1Buf)
	return
}

func main() {

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
}

