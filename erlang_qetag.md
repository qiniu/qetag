# qetag
Qetag for Erlang

Maybe it's the fastest qetag program in Qiniu

里面有两种并行计算的写法，差别在于是每算一次sha1就合并，还是先算一个连续的大块再合并

使用的读文件方法都是read，这样可以利用erlang的async thread，如果使用raw，它使用scheduler线程调用read,而且非常依赖GC，内存占用高，还可能导致整个scheduler卡住。

Go读文件直接调用了syscall，和erlang中的raw模式是一致的

我关于read和raw的行为的理解，要感谢bhuztez带来的指点，thank you!

https://github.com/qiniu/qetag

核心数越多 跑的越快

在自己的2core 4线程的机器上跑1.7GB的qetag耗时2.3S左右

在虚拟机8core 上跑4GB的qetag耗时1.3S左右  8个核心均有负载 最高在40%

对比同事用go写的并行计算的程序(他随便写着玩的)

在我自己的笔记本上跑上面那个1.7GB的耗时2.1S左右

在8 core虚拟机上跑上面那个4GB的耗时4S左右  8个核心均有负载  最高在99%

USAGE:

make

qetag:etag_file(File_path).

qetag:qetag(File_path).

ps: 利用 erl +A 调整thread数量

