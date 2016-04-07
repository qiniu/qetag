-module(qetag).
-include_lib("kernel/include/file.hrl").

-compile(export_all).
-export([run/1]).

-define(BLOCK_SIZE, 4194304).

%% Thank you!
%% bhuztez
run(FileName) ->
    WorkerPoolSize = erlang:system_info(thread_pool_size),
    {ok, File} = file:open(FileName, [read, binary]),
    try
        qetag_file(File, WorkerPoolSize)
    after
        file:close(File)
    end.


%%%%%↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑%%%%%
%%%%%↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ YOU NEED CARE ABOUT ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑%%%%%
%%%%%                                                                                                    %%%%%
%%%%%                                                                                                    %%%%%
%%%%%↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Internal Function ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓%%%%%
%%%%%↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓%%%%%


qetag_file(File, WorkerPoolSize) ->
    qetag_file(File, WorkerPoolSize, ?BLOCK_SIZE).
qetag_file(File, WorkerPoolSize, BlockSize) ->
    {ok, FileSize} = file:position(File, eof),
    Blocks = (FileSize div BlockSize)
        + (case FileSize rem BlockSize of
               0 ->
                   0;
               _ ->
                   1
           end),
    ETag =
        case Blocks of
            1 ->
                spawn_link(?MODULE, start_worker, [self(), File, 0, 0, FileSize]),
                receive
                    {block_hash, 0, Digest} ->
                        <<22, Digest/binary>>
                end;
            _ ->
                run_workers(File, BlockSize, FileSize, WorkerPoolSize),
                Digest =
                    crypto:hash_final(
                        update_block_hash(
                            crypto:hash_init(sha),
                            0, Blocks)),
                <<150, Digest/binary>>
        end,
    urlsafe_base64_encode(ETag).


start_worker(Parent, File, Block, Offset, Size) ->
    {ok, Bin} = file:pread(File, Offset, Size),
    Parent ! {block_hash, Block, crypto:hash(sha, Bin)}.


run_workers(File, BlockSize, FileSize, WorkerPoolSize) ->
    spawn_link(
        ?MODULE,
        start_worker_pool,
        [self(), File, BlockSize, FileSize, WorkerPoolSize]).


start_worker_pool(Parent, File, BlockSize, FileSize, WorkerPoolSize) ->
    process_flag(trap_exit, true),
    worker_pool(
        #{ parent => Parent,
            file => File,
            block_size => BlockSize,
            file_size => FileSize,
            worker_pool_size => WorkerPoolSize,
            file_offset => 0,
            next_block => 0,
            workers => 0,
            worker_pids => sets:new()}).


update_block_hash(Context, Block, Blocks)
    when Block < Blocks ->
    receive
        {block_hash, Block, Digest} ->
            update_block_hash(
                crypto:hash_update(Context, Digest),
                Block + 1,
                Blocks)
    end;
update_block_hash(Context, Blocks, Blocks) ->
    Context.


run_worker(
    Offset, Size,
    State = #{
        parent := Parent,
        file := File,
        next_block := NextBlock,
        workers := Workers,
        worker_pids := Pids}) ->
    Pid = spawn_link(?MODULE, start_worker, [Parent, File, NextBlock, Offset, Size]),
    worker_pool(
        State#{
            next_block := NextBlock + 1,
            file_offset := Offset + Size,
            workers := Workers + 1,
            worker_pids := sets:add_element(Pid, Pids)}).


worker_pool(
    #{ file_offset := FileSize,
        file_size := FileSize,
        workers := 0}) ->
    ok;
worker_pool(
    State = #{
        file_offset := FileSize,
        file_size := FileSize}) ->
    receive
        {'EXIT', Pid, normal} ->
            worker_done(Pid, State)
    end;
worker_pool(
    State = #{
        workers := WorkerPoolSize,
        worker_pool_size := WorkerPoolSize}) ->
    receive
        {'EXIT', Pid, normal} ->
            worker_done(Pid, State)
    end;
worker_pool(
    State = #{
        file_offset := FileOffset,
        file_size := FileSize,
        block_size := BlockSize}
) when FileOffset + BlockSize < FileSize ->
    run_worker(FileOffset, BlockSize, State);
worker_pool(
    State = #{
        file_offset := FileOffset,
        file_size := FileSize}) ->
    run_worker(FileOffset, FileSize - FileOffset, State).


worker_done(Pid, State = #{workers := Workers, worker_pids := Pids}) ->
    case sets:is_element(Pid, Pids) of
        true ->
            worker_pool(
                State#{workers := Workers - 1,
                    worker_pids := sets:del_element(Pid, Pids)});
        false ->
            worker_pool(State)
    end.

-spec encode_mime(
    binary() | iolist()
) -> binary().

encode_mime(Bin) when is_binary(Bin) ->
    << << (urlencode_digit(D)) >> || <<D>> <= base64:encode(Bin) >>;
encode_mime(L) when is_list(L) ->
    encode_mime(iolist_to_binary(L)).


urlencode_digit($/) -> $_;
urlencode_digit($+) -> $-;
urlencode_digit(D)  -> D.


urlsafe_base64_encode(Data) ->
    binary:bin_to_list(encode_mime(Data)).
