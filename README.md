

# Erlang LRU: a fixed size LRU cache. #

Copyright (c) 2015 Benoît Chesneau.

__Version:__ 1.0.0.

## Erlang LRU

Erlang LRU implements a fixed size LRU cache.

The cache is maintained in a process that could be added to a supervision
treee.

Usage:
------

It's usage is very simple.

```erlang

Size = 128,
Cache = lru:start(Cache),

lru:add(Cache, 1, 1),
lru:add(Cache, 2, 2),
lru:remove(Cache, 2),
```

## Documentation

Full doc is available in the [`lru`](http://github.com/barrel-db/erlang-lru/blob/master/doc/lru.md) module.

## Build

$ rebar3 compile
