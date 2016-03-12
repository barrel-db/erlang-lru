-module(lru_ets).

-export([new/3, add/3, get/4]).

-define(EVICT_POOL, 5).

-record(cache, {tab :: term(),
                size :: integer(),
                evict_fun :: fun() | 'undefined'}).


new(Name, Size, Options) ->
    EvictFun = proplists:get_value(evict_fun, Options),
    ets:new(Name, [named_table, public, ordered_set,
                   {write_concurrency, true},
                   {read_concurrency, true}]),
    #cache{tab=Name,
           size=Size,
           evict_fun=EvictFun}.


add(#cache{}=Cache, Key, Value) ->
    true = ets:insert(Cache#cache.tab, {Key, {ts(), Value}}),
    maybe_evict(Cache).

get(Cache, Key, Value, Default) ->
    case ets:lookup(Cache#cache.tab, Key) of
        [] ->
            Default;
        [{Key, {_TS, Value}}] ->
            _ = ets:insert(Cache#cache.tab, {Key, {ts(), Value}}),
            Value
    end.



maybe_evict(Cache) ->
    #cache{tab=Tab, size=Sz, evict_fun=EvictFun} = Cache,
    TSz = ets:info(Tab, size),

    if
        TSz =< Sz -> false;
        true ->
            Start = random_key(Tab, rand:uniform(TSz) -1),
            {Pool, _} = ets:select(Tab, [{{'$1', {'$2', '$3'}},
                                          [{'>=', '$1', Start}],
                                          ['$_']}],
                                   ?EVICT_POOL),
            case do_eviction(Pool) of
                false -> false;
                {K, {_Ts, V}} ->
                    _ = ets:delete(Tab, K),
                    case EvictFun of
                        undefined -> ok;
                        _ -> EvictFun(K, V)
                    end,
                    true
            end
    end.


do_eviction('$end_of_table') -> false;
do_eviction(Pool) ->
    [Last | _] = lists:sort(fun
                                 ({_, {A, _}}, {_, {B, _}}) when A =< B ->
                                     true;
                                 (_, _) ->
                                     false
                             end, Pool),
    Last.



ts() ->
    Time = erlang:monotonic_time(),
    UMI = erlang:unique_integer([monotonic]),
    {Time, UMI}.


random_key(Tab, Pos) when Pos < 0 ->
    random_key(Tab, 0);
random_key(Tab, Pos) ->
    [{K, _}] = ets:slot(Tab, Pos),
    K.
