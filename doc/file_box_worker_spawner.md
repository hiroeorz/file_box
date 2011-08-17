Module file_box_worker_spawner
==============================


<h1>Module file_box_worker_spawner</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2011, Hiroe Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Hiroe Shin ([`hiroe.orz@gmail.com`](mailto:hiroe.orz@gmail.com)).

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#read_file-1">read_file/1</a></td><td>
Read data.</td></tr><tr><td valign="top"><a href="#save_file-2">save_file/2</a></td><td>
Save data.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Starts the server.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="read_file-1"></a>

<h3>read_file/1</h3>





<pre>read_file(FileName::string()) -> {ok, Data::binary()} | {error, not_found}</pre>
<br></br>





Read data.
<a name="save_file-2"></a>

<h3>save_file/2</h3>





<pre>save_file(FileName::string(), Data::binary()) -> {ok, Key::string()} | {error, Reason::atom()}</pre>
<br></br>





Save data.
<a name="start_link-0"></a>

<h3>start_link/0</h3>





<pre>start_link() -> {ok, Pid} | ignore | {error, Error}</pre>
<br></br>





Starts the server
