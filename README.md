erlang-lru
==========

Erlang LRU implements a dixed size LRU cache.

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

````

Build
-----

    $ rebar3 compile
