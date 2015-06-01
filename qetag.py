#! /usr/bin/env python
# -*- coding: utf-8 -*-

# 最新版本 https://github.com/qiniu/python-sdk/blob/master/qiniu/utils.py#L129

import os
import sys
import base64
import hashlib
try:
    from cStringIO import StringIO as BytesIO  # py2
    bytes_chr = chr
except ImportError:
    from io import BytesIO  # py3
    bytes_chr = lambda c: bytes([c])


CHUNK_BITS = 22
CHUNK_SIZE = 1 << CHUNK_BITS  # == 2 ** 22 == 4 * 1024 * 1024 == 4MiB


def ensure_bytes(text, encoding='U8'):
    return text if isinstance(text, bytes) else text.encode(encoding)


def get_io_size(fio):
    """get file size from fio"""
    fio.seek(0, os.SEEK_END)
    fsize = fio.tell()
    fio.seek(0)
    return fsize


def get_io_qetag(fio):
    """Caculates qetag from file object

    Parameters:
        - fio: file-like object to the file

    Usage:
    >>> data = bytes_chr(0) * (CHUNK_SIZE + 42) * 42
    >>> fio = BytesIO(data)
    >>> print(get_io_qetag(fio))
    lnmoz9lrkr6HWgZyTqu2vD0XUj6R

    Returns qetag

    """
    size = get_io_size(fio)
    flag = CHUNK_BITS
    sha1 = hashlib.sha1
    buf = []
    while size > 0:
        size -= CHUNK_SIZE
        buf.append(sha1(fio.read(CHUNK_SIZE)).digest())
    buf = b''.join(buf)
    if len(buf) > 20:  # more than 1 chunk
        flag |= 0x80
        buf = sha1(buf).digest()
    fio.seek(0)
    return base64.urlsafe_b64encode(bytes_chr(flag) + buf).decode('ASCII')


def get_qetag(filename):
    """Caculates qetag

    Parameters:
        - filename: string, file name

    Returns qetag

    """
    with open(filename, 'rb') as fp:
        return get_io_qetag(fp)


if __name__ == '__main__':
    assert len(sys.argv) > 1
    print(get_qetag(sys.argv[1]))
