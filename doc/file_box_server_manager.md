Module file_box_server_manager
==============================


<h1>Module file_box_server_manager</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2011, Hiroe Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Hiroe Shin ([`hiroe.orz@gmail.com`](mailto:hiroe.orz@gmail.com)).

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_size-2">add_size/2</a></td><td>
Add file total size.</td></tr><tr><td valign="top"><a href="#get_save_target_list-0">get_save_target_list/0</a></td><td>
Get File Save Target Cluster List.</td></tr><tr><td valign="top"><a href="#remove_size-2">remove_size/2</a></td><td>
Remove file total size.</td></tr><tr><td valign="top"><a href="#server_list-0">server_list/0</a></td><td>
Getting servers list for system management.</td></tr><tr><td valign="top"><a href="#set_server_status-3">set_server_status/3</a></td><td>
Set FileBoxServerManager Informations.</td></tr><tr><td valign="top"><a href="#set_server_status-4">set_server_status/4</a></td><td>
Set FileBoxServerManager Informations.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Starts the server.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="add_size-2"></a>

<h3>add_size/2</h3>





<pre>add_size(Id::integer(), Size::integer()) -> {ok, integer()}</pre>
<br></br>





Add file total size.
<a name="get_save_target_list-0"></a>

<h3>get_save_target_list/0</h3>





<pre>get_save_target_list() -> [#fb_server{id = undefined | integer(), node = undefined | atom(), pid = undefined | pid(), total_size = number()}]</pre>
<br></br>





Get File Save Target Cluster List.
<a name="remove_size-2"></a>

<h3>remove_size/2</h3>





<pre>remove_size(Id::integer(), Size::integer()) -> {ok, integer()}</pre>
<br></br>





Remove file total size.
<a name="server_list-0"></a>

<h3>server_list/0</h3>





<pre>server_list() -> list()</pre>
<br></br>





Getting servers list for system management.
<a name="set_server_status-3"></a>

<h3>set_server_status/3</h3>





<pre>set_server_status(Id::integer(), Node::atom(), Pid::pid()) -> ok</pre>
<br></br>





Set FileBoxServerManager Informations.
<a name="set_server_status-4"></a>

<h3>set_server_status/4</h3>





<pre>set_server_status(Id::integer(), Node::atom(), Pid::pid(), TotalSize::integer()) -> ok</pre>
<br></br>





Set FileBoxServerManager Informations.
<a name="start_link-0"></a>

<h3>start_link/0</h3>





<pre>start_link() -> {ok, Pid} | ignore | {error, Error}</pre>
<br></br>





Starts the server
