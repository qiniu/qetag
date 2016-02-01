-module(qetag).
-include_lib("kernel/include/file.hrl").

-export([etag_file/1]).

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



%%%%%↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑%%%%%
%%%%%↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ YOU NEED CARE ABOUT ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑%%%%%
%%%%%                                                                                                    %%%%%
%%%%%                                                                                                    %%%%%
%%%%%↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Internal Fuctions ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓%%%%%
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
        {Off, SHA1_list_rawblock}
          end, lists:seq(0, Num_thread)).


sha1_list_last(File, Num_thread, Start)->
    upmap(fun (Off)->
        {ok, Fd_bs} = file:pread(File, (Off + Start) * ?BLOCK_SIZE, ?BLOCK_SIZE),
        SHA1 = crypto:hash(sha, Fd_bs),
        {Off, SHA1}
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
        Result
     end
        || _ <- [spawn(fun() -> Parent ! {Ref, F(X)} end) || X <- L]].


combine_sha1(SHA1_BIN, []) ->
    SHA1_BIN;
combine_sha1(SHA1_bin, [H|T]) ->
    {_, RAW_SHA1} = H,
    SHA1_BIN = erlang:iolist_to_binary([SHA1_bin, RAW_SHA1]),
    combine_sha1(SHA1_BIN, T).


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
