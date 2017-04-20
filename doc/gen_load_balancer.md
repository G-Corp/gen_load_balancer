

# Module gen_load_balancer #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

__This module defines the `gen_load_balancer` behaviour.__<br /> Required callback functions: `init/1`.

<a name="types"></a>

## Data Types ##




### <a name="type-lb_name">lb_name()</a> ###


<pre><code>
lb_name() = {local, Name::atom()} | {global, Name::atom()} | {via, Module::module(), Name::any()}
</code></pre>




### <a name="type-lb_ref">lb_ref()</a> ###


<pre><code>
lb_ref() = (Name::atom()) | {Name::atom(), Node::node()} | {global, Name::atom()} | {via, Module::module(), Name::any()} | pid()
</code></pre>




### <a name="type-startlink_err">startlink_err()</a> ###


<pre><code>
startlink_err() = {already_started, pid()} | {shutdown, term()} | term()
</code></pre>




### <a name="type-startlink_ret">startlink_ret()</a> ###


<pre><code>
startlink_ret() = {ok, pid()} | ignore | {error, <a href="#type-startlink_err">startlink_err()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#pid-1">pid/1</a></td><td>
Return the next available Pid.</td></tr><tr><td valign="top"><a href="#resize-2">resize/2</a></td><td>
Resize the list of processes supervised by the load balancer.</td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td>
Return the size of the list of processes supervised by the load balancer.</td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>
Create a load balancer supervisor process as part of the supervision tree.</td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td>
Create a load balancer supervisor process as part of the supervision tree.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>
Orders a generic load balancer to exit and waits for it to terminate.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="pid-1"></a>

### pid/1 ###

<pre><code>
pid(LoadBalancer::<a href="#type-lb_ref">lb_ref()</a>) -&gt; pid()
</code></pre>
<br />

Return the next available Pid.

<a name="resize-2"></a>

### resize/2 ###

<pre><code>
resize(LoadBalancer::<a href="#type-lb_ref">lb_ref()</a>, Size::non_neg_integer()) -&gt; {ok, non_neg_integer()} | {error, term()}
</code></pre>
<br />

Resize the list of processes supervised by the load balancer.

<a name="size-1"></a>

### size/1 ###

<pre><code>
size(LoadBalancer::<a href="#type-lb_ref">lb_ref()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

Return the size of the list of processes supervised by the load balancer.

<a name="start_link-2"></a>

### start_link/2 ###

<pre><code>
start_link(Mod::module(), Args::list()) -&gt; <a href="#type-startlink_ret">startlink_ret()</a>
</code></pre>
<br />

Create a load balancer supervisor process as part of the supervision tree.

<a name="start_link-3"></a>

### start_link/3 ###

<pre><code>
start_link(LbName::<a href="#type-lb_name">lb_name()</a>, Mod::module(), Args::list()) -&gt; <a href="#type-startlink_ret">startlink_ret()</a>
</code></pre>
<br />

Create a load balancer supervisor process as part of the supervision tree.

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(LoadBalancer::<a href="#type-lb_ref">lb_ref()</a>) -&gt; ok
</code></pre>
<br />

Orders a generic load balancer to exit and waits for it to terminate.

