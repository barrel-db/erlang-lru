

# Module lru #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="types"></a>

## Data Types ##




### <a name="type-lru_option">lru_option()</a> ###


<pre><code>
lru_option() = {evict_fun, function()} | {spawn_opt, list()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add-3">add/3</a></td><td>adds a value to the cache.</td></tr><tr><td valign="top"><a href="#contains-2">contains/2</a></td><td>check if the key is in the cache.</td></tr><tr><td valign="top"><a href="#contains_or_add-3">contains_or_add/3</a></td><td> checks if a key is in the cache (without updating the recent-ness or
deleting it for being stale), if not, adds the value.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>lookup a key's value from the cache.</td></tr><tr><td valign="top"><a href="#get-3">get/3</a></td><td>lookup a key's value from the cache.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>get cache info.</td></tr><tr><td valign="top"><a href="#keys-1">keys/1</a></td><td>return all the keys from the cache.</td></tr><tr><td valign="top"><a href="#peek-2">peek/2</a></td><td>Returns the key value (or undefined if not found) without updating the
"recently used"-ness of the key.</td></tr><tr><td valign="top"><a href="#peek-3">peek/3</a></td><td>Returns the key value (or undefined if not found) without updating the
"recently used"-ness of the key.</td></tr><tr><td valign="top"><a href="#purge-1">purge/1</a></td><td>purge all items from the cache.</td></tr><tr><td valign="top"><a href="#remove-2">remove/2</a></td><td>remove a key from the cache.</td></tr><tr><td valign="top"><a href="#remove_oldest-1">remove_oldest/1</a></td><td>remove the oldest item from the cache.</td></tr><tr><td valign="top"><a href="#resize-2">resize/2</a></td><td>resize of the cache.</td></tr><tr><td valign="top"><a href="#set_size-2">set_size/2</a></td><td>change the size of the cache.</td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td>get the number of items in the cache.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>creates an LRU of the given size.</td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td>creates an LRU of the given size
Options are:
- <code>{evict_fun, Fun}</code> a function that will received the evicted key value
"fun(Key, Value)".</td></tr><tr><td valign="top"><a href="#start-3">start/3</a></td><td>creates an LRU of the given size with a registered name.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>creates an LRU of the given size as part of a supervision tree.</td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>creates an LRU of the given size as part of a supervision tree.</td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td>creates an LRU of the given size as part of a supervision tree with a
registered name.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>stop the LRU cache.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-3"></a>

### add/3 ###

<pre><code>
add(Cache::pid(), Key::term(), Value::term()) -&gt; true | false
</code></pre>
<br />

adds a value to the cache.  Returns true if an eviction occured.

<a name="contains-2"></a>

### contains/2 ###

<pre><code>
contains(Cache::pid(), Key::term()) -&gt; true | false
</code></pre>
<br />

check if the key is in the cache

<a name="contains_or_add-3"></a>

### contains_or_add/3 ###

<pre><code>
contains_or_add(Cache::pid(), Key::term(), Value::term()) -&gt; {Exists::boolean(), Evict::boolean()}
</code></pre>
<br />

checks if a key is in the cache (without updating the recent-ness or
deleting it for being stale), if not, adds the value. Returns whether found and whether an eviction
occurred.

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Cache::pid(), Key::term()) -&gt; term() | undefined
</code></pre>
<br />

lookup a key's value from the cache. Return undefined if it's not
found.

<a name="get-3"></a>

### get/3 ###

<pre><code>
get(Cache::pid(), Key::term(), Default::term()) -&gt; term()
</code></pre>
<br />

lookup a key's value from the cache. Return the Default value if it's
not found.

<a name="info-1"></a>

### info/1 ###

<pre><code>
info(Cache::pid()) -&gt; Info::list()
</code></pre>
<br />

get cache info

<a name="keys-1"></a>

### keys/1 ###

<pre><code>
keys(Cache::pid()) -&gt; [term()]
</code></pre>
<br />

return all the keys from the cache

<a name="peek-2"></a>

### peek/2 ###

<pre><code>
peek(Cache::pid(), Key::term()) -&gt; term() | undefined
</code></pre>
<br />

Returns the key value (or undefined if not found) without updating the
"recently used"-ness of the key.

<a name="peek-3"></a>

### peek/3 ###

<pre><code>
peek(Cache::pid(), Key::term(), Default::term()) -&gt; term() | undefined
</code></pre>
<br />

Returns the key value (or undefined if not found) without updating the
"recently used"-ness of the key.

<a name="purge-1"></a>

### purge/1 ###

<pre><code>
purge(Cache::pid()) -&gt; ok
</code></pre>
<br />

purge all items from the cache.

<a name="remove-2"></a>

### remove/2 ###

<pre><code>
remove(Cache::pid(), Key::term()) -&gt; ok
</code></pre>
<br />

remove a key from the cache

<a name="remove_oldest-1"></a>

### remove_oldest/1 ###

<pre><code>
remove_oldest(Cache::pid()) -&gt; ok
</code></pre>
<br />

remove the oldest item from the cache

<a name="resize-2"></a>

### resize/2 ###

<pre><code>
resize(Cache::pid(), Size::non_neg_integer()) -&gt; ok
</code></pre>
<br />

resize of the cache

<a name="set_size-2"></a>

### set_size/2 ###

<pre><code>
set_size(Cache::pid(), Size::non_neg_integer()) -&gt; ok
</code></pre>
<br />

change the size of the cache

<a name="size-1"></a>

### size/1 ###

<pre><code>
size(Cache::pid()) -&gt; non_neg_integer()
</code></pre>
<br />

get the number of items in the cache

<a name="start-1"></a>

### start/1 ###

<pre><code>
start(Size::non_neg_integer()) -&gt; {ok, pid()} | {error, term()}
</code></pre>
<br />

creates an LRU of the given size

<a name="start-2"></a>

### start/2 ###

<pre><code>
start(Size::non_neg_integer(), Opts::[<a href="#type-lru_option">lru_option()</a>]) -&gt; {ok, pid()} | {error, term()}
</code></pre>
<br />

creates an LRU of the given size
Options are:
- `{evict_fun, Fun}` a function that will received the evicted key value
"fun(Key, Value)".
- `{spawn_opts, Opts}` the spawn options. see `erlang:spawn_opt/2` for
more informations.

<a name="start-3"></a>

### start/3 ###

<pre><code>
start(Name::{local, Name::atom()} | {global, GlobalName::term()} | {via, ViaName::term()}, Size::non_neg_integer(), Opts::[<a href="#type-lru_option">lru_option()</a>]) -&gt; {ok, pid()} | {error, term()}
</code></pre>
<br />

creates an LRU of the given size with a registered name

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Size::non_neg_integer()) -&gt; {ok, pid()} | {error, term()}
</code></pre>
<br />

creates an LRU of the given size as part of a supervision tree

<a name="start_link-2"></a>

### start_link/2 ###

<pre><code>
start_link(Size::non_neg_integer(), Opts::[<a href="#type-lru_option">lru_option()</a>]) -&gt; {ok, pid()} | {error, term()}
</code></pre>
<br />

creates an LRU of the given size as part of a supervision tree

<a name="start_link-3"></a>

### start_link/3 ###

<pre><code>
start_link(Name::{local, Name::atom()} | {global, GlobalName::term()} | {via, ViaName::term()}, Size::non_neg_integer(), Opts::[<a href="#type-lru_option">lru_option()</a>]) -&gt; {ok, pid()} | {error, term()}
</code></pre>
<br />

creates an LRU of the given size as part of a supervision tree with a
registered name.

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(Cache::pid()) -&gt; ok
</code></pre>
<br />

stop the LRU cache

