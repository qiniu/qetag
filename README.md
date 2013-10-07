qetag
=====

qetag 是一个计算文件在七牛云存储上的 hash 值（也文件下载时的 etag 值）的实用程序。

七牛的 hash/etag 算法是公开的。算法大体如下：

* 如果你能够确认文件 <= 4M，那么 hash = UrlsafeBase64([0x16, sha1(FileContent)])
* 如果文件 > 4M，则 hash = UrlsafeBase64([0x96, sha1([sha1(Block1), sha1(Block2), ...])])，其中 Block 是把文件内容切分为 4M 为单位的一个个块，也就是 `BlockI = FileContent[I*4M:(I+1)*4M]`。

为何需要公开 hash/etag 算法？这个和 “消重” 问题有关，详细见：

* http://kb.qiniu.com/53tubk96
* http://segmentfault.com/q/1010000000315810

