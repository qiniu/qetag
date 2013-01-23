package main

import (
	"bytes"
	"crypto/sha1"
	"encoding/base64"
	"fmt"
	"io"
	"log"
	"os"
)

const (
	BLOCK_BITS = 22 // Indicate that the blocksize is 4M
)

// To get how many blocks does the file has.
func BlockCount(fsize int64) int {
	blockMask := int64((1 << BLOCK_BITS) - 1)
	return int((fsize + blockMask) >> BLOCK_BITS)
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
			body := io.LimitReader(f, 1 << BLOCK_BITS)
			sha1Code, err = CalSha1(body)
			if err != nil {
				return
			}
			sha1BlockBuf = append(sha1BlockBuf,sha1Code...)
		}
		tmpBuf := bytes.NewBuffer(sha1BlockBuf)
		var sha1Final []byte
		sha1Final, err = CalSha1(tmpBuf)
		if err != nil {
			return
		}
		sha1Buf = append(sha1Buf, sha1Final...)
	}
	etag = base64.URLEncoding.EncodeToString(sha1Buf)
	return
}

func main() {

	usage := `usage : qetag <filename>`
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, usage)
		return
	}
	etag, err := GetEtag(os.Args[1])
	if err != nil {
		log.Println("Calculate etag failed : ", err)
		return
	}
	fmt.Println(etag)
}