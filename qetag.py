#!/bin/python

import base64
import hmac
from hashlib import sha1
import struct
import sys
import os.path
BLOCK_BITS = 22
BLOCK_SIZE = 1 << BLOCK_BITS

def block_count(fsize):
        return (fsize + (BLOCK_SIZE - 1)) >> BLOCK_BITS

def hash_sign(data):
        m = sha1()
        hex16 = struct.pack('B', 0x16)
        m.update(data)
        res = hex16 + m.digest()
        return base64.urlsafe_b64encode(res)

def cal_sha1(f):
        data = f.read(BLOCK_SIZE)
        sha1str = sha1(data)
        return sha1str.digest()

def get_etag(filename):
        if not os.path.isfile(filename):
                print 'Error, Can not open ' + filename + ' as a file.'
                exit(-1)
        blockcnt = block_count(os.path.getsize(filename))
        f = open(filename, 'rb')
        if blockcnt <= 1:
                sha1buf = struct.pack('B', 0x16)
                sha1code = cal_sha1(f)
                sha1buf = sha1buf + sha1code
        else:
                sha1buf = struct.pack('B', 0x96)
                sha1blockbuff = ''
                for i in range(0, blockcnt):
                        sha1code = cal_sha1(f)
                        sha1blockbuff = sha1blockbuff + sha1code
                tmpfilename = '/tmp/' + sha1(sha1blockbuff).hexdigest()
                tmpfile = open(tmpfilename, 'w+')
                tmpfile.write(sha1blockbuff)
                tmpfile.seek(0)
                sha1final = cal_sha1(tmpfile)
                sha1buf = sha1buf + sha1final
                tmpfile.close()
                os.remove(tmpfilename)
        return base64.urlsafe_b64encode(sha1buf)

if __name__ == "__main__":
    print get_etag(sys.argv[1:][0])
