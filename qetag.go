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

func CalSha1(r io.Reader) []byte {
	h := sha1.New()
	io.Copy(h, r)
	return h.Sum(nil)
}

func GetEtag(filename string) (etag string, err error) {
	f, err := os.Open(filename)
	defer f.Close()
	if err != nil {
		log.Println("Open file error : ", err)
		return
	}
	fi, err := f.Stat()
	if err != nil {
		log.Println(err)
		return
	}
	fsize := fi.Size()
	blockCnt := BlockCount(fsize)
	sha1Buf := make([]byte, 0, 21)

	if blockCnt <= 1 { // file size <= 4M
		sha1Buf = append(sha1Buf, 0x16)
		sha1Code := CalSha1(f)
		sha1Buf = append(sha1Buf, sha1Code...)
	} else { // file size > 4M
		sha1Buf = append(sha1Buf, 0x96)
		sha1BlockBuf := make([]byte, 0, blockCnt * 20)

		for i := 0; i < blockCnt; i ++ {
			body := io.LimitReader(f, 1 << BLOCK_BITS)
			sha1Code := CalSha1(body)
			sha1BlockBuf = append(sha1BlockBuf,sha1Code...)
		}
		tmpBuf := bytes.NewBuffer(sha1BlockBuf)
		sha1Final := CalSha1(tmpBuf)
		sha1Buf = append(sha1Buf, sha1Final...)
	}
	etag = base64.URLEncoding.EncodeToString(sha1Buf)
	return
}

func main() {

	usage := `usage : qetag <filename>`
	if len(os.Args) < 2 {
		fmt.Println(usage)
		return
	}
	etag, err := GetEtag(os.Args[1])
	if err != nil {
		log.Println("Calculate etag failed : ", err)
	}
	fmt.Println(etag)
}