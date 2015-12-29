-module(qetag).
-include_lib("kernel/include/file.hrl").

-compile(export_all).

-define(BLOCK_SIZE, 4194304).

etag_file(FileName) ->
    {ok, FInfo} = file:read_file_info(FileName),
    Fsize = FInfo#file_info.size,
    if
        Fsize > ?BLOCK_SIZE ->
            {ok, File} = file:open(FileName, [read, binary]),
            try
                etag_big(File, Fsize)
            after
                file:close(File)
            end;
        true -> etag_small_file(FileName)
    end.


%% etag_file2
%% bhuztez's work
%% Thank you
qetag(FileName) ->
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
%%%%%↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ SHIT HERE ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓%%%%%
%%%%%↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓%%%%%


etag_small_file(File_path) ->
    {ok, File_data} = file:read_file(File_path),
    etag_small_stream(File_data).


etag_small_stream(Input_stream) ->
    Digest = crypto:hash(sha, Input_stream),
    urlsafe_base64_encode(<<22, Digest/binary>>).


etag_big(File, Fsize) ->
    {Num_thread,  Num_blocks_in_rawblock, Num_blocks_in_lastsize, Start} = get_num_thread(Fsize),
    First_part = combine_sha1(<<>>, lists:sort(sha1_list(File, Num_thread, Num_blocks_in_rawblock))),
    if
        Num_blocks_in_lastsize == 0 ->
            First_part_sha1 =crypto:hash(sha, First_part),
            urlsafe_base64_encode(<<150, First_part_sha1/binary>>);
        true ->
            Second_part = combine_sha1(<<>>, lists:sort(sha1_list_last(File, Num_blocks_in_lastsize, Start))),
            SHA1_all = crypto:hash(sha, <<First_part/binary, Second_part/binary>>),
            urlsafe_base64_encode(<<150, SHA1_all/binary>>)
    end.


get_num_thread(Fsize) ->
    PoolSize = erlang:system_info(thread_pool_size),
    Num_blocks_in_rawblock = Fsize div ?BLOCK_SIZE div PoolSize,
    Onetime_size = PoolSize * ?BLOCK_SIZE * Num_blocks_in_rawblock,
    Last_size = Fsize - Onetime_size,
    Num_blocks_in_lastsize = Last_size div ?BLOCK_SIZE,
    if
        Fsize / ?BLOCK_SIZE == Fsize div ?BLOCK_SIZE ->
            {PoolSize - 1,  Num_blocks_in_rawblock - 1, Num_blocks_in_lastsize - 1, Num_blocks_in_rawblock * PoolSize};
        true ->
            {PoolSize - 1,  Num_blocks_in_rawblock - 1, Num_blocks_in_lastsize, Num_blocks_in_rawblock * PoolSize}
    end.



sha1_list(File, Num_thread, Num_blocks_in_rawblock) ->
    upmap(fun (Off) ->
        Read_start = Off * (Num_blocks_in_rawblock + 1),
        Read_off = Read_start + Num_blocks_in_rawblock,
        SHA1_list_rawblock = get_rawblock_sha1_list(File, lists:seq(Read_start, Read_off), <<>>),
        [{Off, SHA1_list_rawblock}]
          end, lists:seq(0, Num_thread)).


sha1_list_last(File, Num_thread, Start)->
    upmap(fun (Off)->
        {ok, Fd_bs} = file:pread(File, (Off + Start) * ?BLOCK_SIZE, ?BLOCK_SIZE),
        SHA1 = crypto:hash(sha, Fd_bs),
        [{Off, SHA1}]
          end, lists:seq(0, Num_thread)).


get_rawblock_sha1_list(_, [], Raw_BIN) ->
    Raw_BIN;
get_rawblock_sha1_list(File, [H|T], Raw_bin) ->
    {ok, Fd_bs} = file:pread(File,  H * ?BLOCK_SIZE, ?BLOCK_SIZE),
    Raw_BIN = erlang:iolist_to_binary([Raw_bin, crypto:hash(sha, Fd_bs)]),
    get_rawblock_sha1_list(File,  T, Raw_BIN).


upmap(F, L) ->
    Parent = self(),
    Ref = make_ref(),
    [receive {Ref, Result} ->
        Result ++ []
     end
        || _ <- [spawn(fun() -> Parent ! {Ref, F(X)} end) || X <- L]].


combine_sha1(SHA1_BIN, []) ->
    SHA1_BIN;
combine_sha1(SHA1_bin, [H|T]) ->
    [{_, RAW_SHA1}] = H,
    SHA1_BIN = erlang:iolist_to_binary([SHA1_bin, RAW_SHA1]),
    combine_sha1(SHA1_BIN, T).


-spec encode_mime(
    binary() | iolist()
) -> binary().

encode_mime(Bin) when is_binary(Bin) ->
    << << (urlencode_digit(D)) >> || <<D>> <= base64:encode(Bin) >>;
encode_mime(L) when is_list(L) ->
    encode_mime(iolist_to_binary(L)).


-spec decode(
    binary() | iolist()
) -> binary().

decode(Bin) when is_binary(Bin) ->
    Bin2 = case byte_size(Bin) rem 4 of
               % 1 -> << Bin/binary, "===" >>;
               2 -> << Bin/binary, "==" >>;
               3 -> << Bin/binary, "=" >>;
               _ -> Bin
           end,
    base64:decode(<< << (urldecode_digit(D)) >> || <<D>> <= Bin2 >>);
decode(L) when is_list(L) ->
    decode(iolist_to_binary(L)).


urlencode_digit($/) -> $_;
urlencode_digit($+) -> $-;
urlencode_digit(D)  -> D.


urldecode_digit($_) -> $/;
urldecode_digit($-) -> $+;
urldecode_digit(D)  -> D.


urlsafe_base64_encode(Data) ->
    binary:bin_to_list(encode_mime(Data)).


urlsafe_base64_decode(Data) ->
    binary:bin_to_list(decode(Data)).


%%% qetag2 %%%
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

worker_done(Pid, State = #{workers := Workers, worker_pids := Pids}) ->
    case sets:is_element(Pid, Pids) of
        true ->
            worker_pool(
                State#{workers := Workers - 1,
                    worker_pids := sets:del_element(Pid, Pids)});
        false ->
            worker_pool(State)
    end.

start_worker(Parent, File, Block, Offset, Size) ->
    {ok, Bin} = file:pread(File, Offset, Size),
    Parent ! {block_hash, Block, crypto:hash(sha, Bin)}.
