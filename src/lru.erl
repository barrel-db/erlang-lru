%%% -*- erlang -*-
%%%
%%% This file is part of erlang-lru released under the BSD license.
%%%
%%% Copyright (c) 2015 Benoît Chesneau <benoitc@e-engura.org>
%%%
-module('lru').
-behaviour(gen_server).

-export([start/1, start/2, start/3,
         start_link/1, start_link/2, start_link/3,
         stop/1,
         get/2, get/3,
         contains/2,
         keys/1,
         peek/2, peek/3,
         add/3,
         contains_or_add/3,
         remove/2,
         remove_oldest/1,
         purge/1,
         size/1,
         info/1,
         set_size/2,
         resize/2]).

%% API exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).


-record(cache, {size,
                evict_list,
                items,
                evict_fun}).


-type lru_option() :: {evict_fun, fun()} |
                      {spawn_opt, list()}.

%%====================================================================
%% API functions
%%====================================================================

%% @doc creates an LRU of the given size
-spec start(non_neg_integer()) -> {ok, pid()} | {error, term()}.
start(Size) ->
    start(Size, []).

%% @doc creates an LRU of the given size
%% Options are:
%%  - `{evict_fun, Fun}' a function that will received the evicted key value
%%  "fun(Key, Value)".
%%  - `{spawn_opts, Opts}' the spawn options. see `erlang:spawn_opt/2' for
%%  more informations.
-spec start(Size::non_neg_integer(), Opts::[lru_option()]) ->
    {ok, pid()} | {error, term()}.
start(Size, Opts) ->
    SpawnOpts = proplists:get_value(spawn_opts, Opts, []),
    gen_server:start(?MODULE, [Size, Opts], [{spawn_opts, SpawnOpts}]).

%% @doc creates an LRU of the given size with a registered name
-spec start(Name::{local, Name::atom()} | {global, GlobalName::term()} | {via, ViaName::term()},
            Size::non_neg_integer(), Opts::[lru_option()]) ->
    {ok, pid()} | {error, term()}.
start(Name, Size, Opts) when is_integer(Size), Size > 0 ->
    SpawnOpts = proplists:get_value(spawn_opts, Opts, []),
    gen_server:start(Name, ?MODULE, [Size, Opts], [{spawn_opts, SpawnOpts}]).

%% @doc creates an LRU of the given size as part of a supervision tree
-spec start_link(non_neg_integer()) -> {ok, pid()} | {error, term()}.
start_link(Size) ->
    start_link(Size, []).

%% @doc creates an LRU of the given size as part of a supervision tree
-spec start_link(non_neg_integer(), [lru_option()]) -> {ok, pid()} | {error, term()}.
start_link(Size, Opts) ->
    SpawnOpts = proplists:get_value(spawn_opts, Opts, []),
    gen_server:start_link(?MODULE, [Size, Opts], [{spawn_opts, SpawnOpts}]).

%% @doc creates an LRU of the given size as part of a supervision tree with a
%% registered name.
-spec start_link(Name::{local, Name::atom()} | {global, GlobalName::term()} | {via, ViaName::term()},
                 Size::non_neg_integer(), Opts::[lru_option()]) ->
    {ok, pid()} | {error, term()}.
start_link(Name, Size, Opts) when is_integer(Size), Size > 0 ->
    SpawnOpts = proplists:get_value(spawn_opts, Opts, []),
    gen_server:start_link(Name, ?MODULE, [Size, Opts],[{spawn_opts, SpawnOpts}]).

%% @doc stop the LRU cache
-spec stop(Cache::pid()) -> ok.
stop(Cache) ->
    try
        gen_server:call(Cache, stop, infinity)
    catch
        exit:{noproc,_} -> ok;
        exit:noproc -> ok;
        %% Handle the case where the monitor triggers
        exit:{normal, _} -> ok
    end.

%% @doc adds a value to the cache.  Returns true if an eviction occured.
-spec add(Cache::pid(), Key::term(), Value::term()) -> true | false.
add(Cache, Key, Value) ->
    call(Cache, {add, Key, Value}).

%% @doc lookup a key's value from the cache. Return undefined if it's not
%% found.
-spec get(Cache::pid(), Key::term()) -> term() | undefined.
get(Cache, Key) ->
    lru:get(Cache, Key, undefined).


%% @doc lookup a key's value from the cache. Return the Default value if it's
%% not found.
-spec get(Cache::pid(), Key::term(), Default::term()) -> term().
get(Cache, Key, Default) ->
    call(Cache, {get, Key, Default}).

%% @doc Returns the key value (or undefined if not found) without updating the
%% "recently used"-ness of the key.
-spec peek(Cache::pid(), Key::term()) -> term() | undefined.
peek(Cache, Key) ->
    peek(Cache, Key, undefined).

%% @doc Returns the key value (or undefined if not found) without updating the
%% "recently used"-ness of the key.
-spec peek(Cache::pid(), Key::term(), Default::term()) -> term() | undefined.
peek(Cache, Key, Default) ->
    call(Cache, {peek, Key, Default}).

%% @doc check if the key is in the cache
-spec contains(Cache::pid(), Key::term()) -> true | false.
contains(Cache, Key) ->
    call(Cache, {contains, Key}).

%% @doc return all the keys from the cache
-spec keys(Cache::pid()) -> [term()].
keys(Cache) ->
    call(Cache, keys).

%% @doc  checks if a key is in the cache (without updating the recent-ness or
%% deleting it for being stale), if not, adds the value. Returns whether found and whether an eviction
%% occurred.
-spec contains_or_add(Cache::pid(), Key::term(), Value::term()) ->
    {Exists::boolean(), Evict::boolean()}.
contains_or_add(Cache, Key, Value) ->
    call(Cache, {contains_or_add, Key, Value}).

%% @doc remove a key from the cache
-spec remove(Cache::pid(), Key::term()) -> ok.
remove(Cache, Key) ->
    call(Cache, {remove, Key}).

%% @doc remove the oldest item from the cache
-spec remove_oldest(Cache::pid()) -> ok.
remove_oldest(Cache) ->
    call(Cache, remove_oldest).

%% @doc get the number of items in the cache
-spec size(Cache::pid()) -> non_neg_integer().
size(Cache) ->
    call(Cache, size).

%% @doc purge all items from the cache.
-spec purge(Cache::pid()) -> ok.
purge(Cache) ->
    call(Cache, purge).


%% @doc get cache info
-spec info(Cache::pid()) -> Info::list().
info(Cache) ->
    call(Cache, info).

%% @doc change the size of the cache
-spec set_size(Cache::pid(), Size::non_neg_integer()) -> ok.
set_size(Cache, Size) ->
    gen_server:cast(Cache, {set_size, Size}).

%% @doc resize of the cache
-spec resize(Cache::pid(), Size::non_neg_integer()) -> ok.
resize(Cache, Size) ->
    gen_server:cast(Cache, {resize, Size}).


%%====================================================================
%% Internal functions
%%====================================================================
%%

%% @private
init([Size, Opts]) ->
    EvictFun = proplists:get_value(evict_fun, Opts),

    {ok, #cache{size = Size,
                evict_list = [],
                items = #{},
                evict_fun = EvictFun}}.


%% @private
handle_call({add, Key, Value}, From, Cache) ->
    #cache{items=Items, evict_list=Evicted} = Cache,

    case maps:find(Key, Items) of
        error ->
            %% add new item
            Items2 = maps:put(Key, Value, Items),
            Evicted2 = push_front(Evicted, Key),
            Cache1 = Cache#cache{items=Items2, evict_list=Evicted2},
            %% check if the size is not exceeded
            if
                length(Evicted2) > Cache#cache.size ->
                    gen_server:reply(From, true),
                    {noreply, remove_oldest1(Cache1)};
                true ->
                    {reply, false, Cache1}
            end;
        {ok, _Value} ->
            gen_server:reply(From, false),
            %% add new value
            Items2 = maps:put(Key, Value, Items),
            %% move old entry to front
            Evicted2 = move_front(Evicted, Key),
            {noreply, Cache#cache{items=Items2, evict_list=Evicted2}}
    end;


handle_call({get, Key, Default}, _From, Cache) ->
    case maps:find(Key, Cache#cache.items) of
        {ok, Value} ->
            Evicted2 =  move_front(Cache#cache.evict_list, Key),
            {reply, Value, Cache#cache{evict_list=Evicted2}};
        error ->
            {reply, Default, Cache}
    end;

handle_call({peek, Key, Default}, _From, Cache) ->
    {reply, maps:get(Key, Cache#cache.items, Default), Cache};

handle_call({contains, Key}, _From, Cache) ->
    {reply, maps:is_key(Key, Cache#cache.items), Cache};

handle_call({contains_or_add, Key, Value}, From, Cache) ->
    case maps:is_key(Key, Cache#cache.items) of
        false ->
            #cache{items=Items, evict_list=Evicted} = Cache,
            Items2 = maps:put(Key, Value, Items),
            Evicted2 = push_front(Evicted, Key),
            Cache1 = Cache#cache{items=Items2, evict_list=Evicted2},
            if
                length(Evicted2) > Cache#cache.size ->
                    gen_server:reply(From, {false, true}),
                    {noreply, remove_oldest1(Cache1)};
                true ->
                    {reply, {false, false}, Cache1}
            end;
        true ->
            {reply, {true, false}, Cache}
    end;

handle_call(keys, _From, Cache) ->
    {reply, lists:reverse(Cache#cache.evict_list), Cache};

handle_call({remove, Key}, From, Cache) ->
    gen_server:reply(From, ok),
    {noreply, remove_element(Key, Cache)};

handle_call(remove_oldest, From, Cache) ->
    gen_server:reply(From, ok),
    {noreply, remove_oldest1(Cache)};

handle_call(size, _From, Cache) ->
    Sz = length(Cache#cache.evict_list),
    {reply, Sz, Cache};

handle_call(purge, _From, Cache) ->
    {reply, ok, Cache#cache{items=#{}, evict_list=[]}};

handle_call(info, _From, Cache) ->
    #cache{size=Max, evict_list=EvictList} = Cache,
    Info = [{max_size, Max},
            {size, length(EvictList)}],
    {reply, Info, Cache};

handle_call(stop, _From, Cache) ->
    {stop, normal, ok, Cache}.

%% @private
handle_cast({resize, Sz}, Cache) ->
    if
        Sz < Cache#cache.size ->
            Diff = Cache#cache.size - Sz,
            Cache2 = lists:foldl(fun(_, Cache1) ->
                                         remove_oldest1(Cache1)
                                 end, Cache, lists:seq(1, Diff)),
            {noreply, Cache2#cache{size=Sz}};
        true ->
            {noreply, Cache#cache{size=Sz}}
    end;

handle_cast({set_size, Sz}, Cache) ->
    {noreply, Cache#cache{size=Sz}};

handle_cast(_Msg, Cache) ->
    {noreply, Cache}.

%% @private
handle_info(_Info, Cache) ->
    {noreply, Cache}.

%% @private
code_change(_OldVsn, Cache, _Extra) ->
   {ok, Cache}.

%% @private
terminate(_Reason, _Cache) ->
    ok.


call(Cache, Msg) ->
    gen_server:call(Cache, Msg).


push_front(List, Key) ->
    [Key | List].


move_front(List, Key) ->
    [Key | lists:delete(Key, List)].


remove_oldest1(Cache) ->
    Last = lists:last(Cache#cache.evict_list),
    Cache1 = remove_item(Last, Cache),
    Cache1#cache{evict_list=lists:droplast(Cache#cache.evict_list)}.

remove_element(Key, Cache) ->
    Cache1 = remove_item(Key, Cache),
    Cache1#cache{evict_list=lists:delete(Key, Cache#cache.evict_list)}.

remove_item(Key, #cache{items=Items, evict_fun=EvictFun}=Cache) ->
    case EvictFun of
        undefined ->
            Cache#cache{items=maps:remove(Key, Items)};
        _ ->
            case maps:find(Key, Items) of
                {ok, Value} ->
                    EvictFun(Key, Value);
                error ->
                    ok
            end,
            Cache#cache{items=maps:remove(Key, Items)}
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

lru_test() ->

    _ = ets:new(lru_test, [named_table, ordered_set, public]),
    ets:insert_new(lru_test, {evict_count, 0}),
    EvictFun = fun(Key, Value) ->
                       if
                           Key /= Value ->
                               throw({not_equal, Key, Value});
                           true ->
                               ok
                       end,
                       _ = ets:update_counter(lru_test, evict_count, 1)
               end,
    {ok, Cache} = lru:start_link(128, [{evict_fun, EvictFun}]),
    ?assert(is_pid(Cache)),
    [lru:add(Cache, I, I) || I <- lists:seq(1, 256)],
    ?assertEqual(lru:size(Cache), 128),
    ?assert(ets:lookup(lru_test, evict_count) =:= [{evict_count, 128}]),

    lists:foldl(fun(Key, I) ->
                          Value = lru:get(Cache, Key),
                          ?assert(Value =:= Key),
                          ?assert(Value =/= (I+ 128)),
                          I+1
               end, 0, lru:keys(Cache)),

    lists:foreach(fun(I) ->
                          ?assert(lru:get(Cache, I) =:= undefined)
                  end, lists:seq(1, 128)),
    lists:foreach(fun(I) ->
                          ?assert(lru:get(Cache, I) =:= I)
                  end, lists:seq(129, 256)),



    lists:foreach(fun(I) ->
                          lru:remove(Cache, I),
                          ?assert(lru:get(Cache, I) =:= undefined)
                  end, lists:seq(129, 192)),


    ?assert(lru:get(Cache, 193) =:= 193),

    lists:foreach(fun(Key) ->
                          ?assert(lru:get(Cache, Key) > 192 )
                  end, lru:keys(Cache)),

    lru:purge(Cache),
    ?assert(lru:size(Cache) =:= 0),

    lists:foreach(fun(I) ->
                    ?assert(lru:get(Cache, I) =:= undefined)
            end, lists:seq(1, 200)),

    ets:delete(lru_test),
    lru:stop(Cache),
    ok.

lru_add_test() ->
    _ = ets:new(lru_test, [named_table, ordered_set, public]),
    ets:insert_new(lru_test, {evict_count, 0}),

    EvictFun = fun(_Key, _Value) ->
                       _ = ets:update_counter(lru_test, evict_count, 1)
               end,

    {ok, Cache} = lru:start_link(1, [{evict_fun, EvictFun}]),

    ?assert(lru:add(Cache, 1, 1) =:= false),
    ?assert(ets:lookup(lru_test, evict_count) =:= [{evict_count, 0}]),

    ?assert(lru:add(Cache, 2, 2) =:= true),
    ?assert(ets:lookup(lru_test, evict_count) =:= [{evict_count, 1}]),

    ets:delete(lru_test),
    lru:stop(Cache),
    ok.

lru_contains_test() ->
    {ok, Cache} = lru:start_link(2),
    lru:add(Cache, 1, 1),
    lru:add(Cache, 2, 2),
    ?assert(lru:contains(Cache, 1)),
    lru:add(Cache, 3, 3),
    ?assert(lru:contains(Cache, 1) =:= false),
    lru:stop(Cache),
    ok.

lru_contains_or_add_test() ->
    {ok, Cache} = lru:start_link(2),
    lru:add(Cache, 1, 1),
    lru:add(Cache, 2, 2),
    ?assertEqual(lru:contains_or_add(Cache, 1, 1), {true, false}),
    lru:add(Cache, 3, 3),
    ?assertEqual(lru:contains_or_add(Cache, 1, 1), {false, true}),
    ?assert(lru:contains(Cache, 1)),
    lru:stop(Cache),
    ok.

lru_peek_test() ->
    {ok, Cache} = lru:start_link(2),
    lru:add(Cache, 1, 1),
    lru:add(Cache, 2, 2),
    ?assertEqual(lru:peek(Cache, 1), 1),
    lru:add(Cache, 3, 3),
    ?assert(lru:contains(Cache, 1) =:= false),
    lru:stop(Cache),
    ok.


lru_info_test() ->
    {ok, Cache} = lru:start_link(2),
    lru:add(Cache, 1, 1),
    lru:add(Cache, 2, 2),
    ?assertEqual(lru:info(Cache), [{max_size, 2}, {size, 2}]),
    lru:stop(Cache),
    ok.

lru_set_size_test() ->
    {ok, Cache} = lru:start_link(2),
    lru:add(Cache, 1, 1),
    lru:add(Cache, 2, 2),
    ?assertEqual(lru:info(Cache), [{max_size, 2}, {size, 2}]),
    lru:set_size(Cache, 3),
    ?assertEqual(lru:info(Cache), [{max_size, 3}, {size, 2}]),
    lru:add(Cache, 3, 3),
    ?assertEqual(lru:keys(Cache), [1, 2, 3]),
    lru:resize(Cache, 1),
    ?assertEqual(lru:keys(Cache), [3]),
    lru:stop(Cache),
    ok.



-endif.
