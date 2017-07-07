

# Module foil #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-error">error()</a> ###


<pre><code>
error() = {error, atom()}
</code></pre>




### <a name="type-key">key()</a> ###


<pre><code>
key() = term()
</code></pre>




### <a name="type-namespace">namespace()</a> ###


<pre><code>
namespace() = atom()
</code></pre>




### <a name="type-value">value()</a> ###


<pre><code>
value() = term()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td></td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td></td></tr><tr><td valign="top"><a href="#insert-3">insert/3</a></td><td></td></tr><tr><td valign="top"><a href="#load-1">load/1</a></td><td></td></tr><tr><td valign="top"><a href="#lookup-2">lookup/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete-1"></a>

### delete/1 ###

<pre><code>
delete(Namespace::<a href="#type-namespace">namespace()</a>) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />

<a name="delete-2"></a>

### delete/2 ###

<pre><code>
delete(Namespace::<a href="#type-namespace">namespace()</a>, Key::<a href="#type-key">key()</a>) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />

<a name="insert-3"></a>

### insert/3 ###

<pre><code>
insert(Namespace::<a href="#type-namespace">namespace()</a>, Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />

<a name="load-1"></a>

### load/1 ###

<pre><code>
load(Namespace::<a href="#type-namespace">namespace()</a>) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />

<a name="lookup-2"></a>

### lookup/2 ###

<pre><code>
lookup(Namespace::<a href="#type-namespace">namespace()</a>, Key::<a href="#type-key">key()</a>) -&gt; {ok, <a href="#type-value">value()</a>} | <a href="#type-error">error()</a>
</code></pre>
<br />

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Namespace::<a href="#type-namespace">namespace()</a>) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />

