qetag
=====

qetag 是一个计算文件在七牛云存储上的 hash 值（也是文件下载时的 etag 值）的实用程序。

七牛的 hash/etag 算法是公开的。算法大体如下：

* 如果你能够确认文件 <= 4M，那么 hash = UrlsafeBase64([0x16, sha1(FileContent)])。也就是，文件的内容的sha1值（20个字节），前面加一个byte（值为0x16），构成 21 字节的二进制数据，然后对这 21 字节的数据做 urlsafe 的 base64 编码。
* 如果文件 > 4M，则 hash = UrlsafeBase64([0x96, sha1([sha1(Block1), sha1(Block2), ...])])，其中 Block 是把文件内容切分为 4M 为单位的一个个块，也就是 `BlockI = FileContent[I*4M:(I+1)*4M]`。

为何需要公开 hash/etag 算法？这个和 “消重” 问题有关，详细见：

* http://kb.qiniu.com/53tubk96
* http://segmentfault.com/q/1010000000315810

为何在 sha1 值前面加一个byte的标记位(0x16或0x96）？

* 0x16 = 22，而 2^22 = 4M。所以前面的 0x16 其实是文件按 4M 分块的意思。
* 0x96 = 0x80 | 0x16。其中的 0x80 表示这个文件是大文件（有多个分块），hash 值也经过了2重的 sha1 计算。
