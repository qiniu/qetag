# qetag
Qetag for Erlang

使用的读文件方法是read，这样可以利用erlang的async thread，如果使用raw，它使用scheduler线程调用read,而且非常依赖GC，内存占用高，还可能导致整个scheduler卡住。

Go读文件直接调用了syscall，和erlang中的raw模式是一致的

https://github.com/qiniu/qetag

CPU核心数越多 跑的越快


USAGE:

make

qetag:etag_file(File_path).

ps: 利用 erl +A 调整thread数量

