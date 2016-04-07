# qetag
Qetag for Erlang

使用的读文件方法是read，这样可以利用erlang的async thread

如果只是作为一个计算单个qetag的脚本,使用raw的方式读文件可能更快

但如果使用raw，它使用scheduler线程调用read,而且非常依赖GC，内存占用高，还可能导致整个scheduler卡住

因此作为一个erlang程序是不推荐用raw的

Go读文件直接调用了syscall，和erlang中的raw模式是一致的

https://github.com/qiniu/qetag

CPU核心数越多 跑的越快


USAGE:

$erl

c(qetag).

qetag:run(File_path).

ps: 利用 erl +A 调整thread数量

