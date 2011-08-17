Module file_box_server
======================


<h1>Module file_box_server</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


FileBoxServer is Dir's File manager.



Copyright (c) 2011 HIROE Shin


__Authors:__ HIROE Shin ([`twitter: http://twitter.com/#!/hiroe_orz17`](mailto:twitter: http://twitter.com/#!/hiroe_orz17)).

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td>initialize server.</td></tr><tr><td valign="top"><a href="#read_file-2">read_file/2</a></td><td>read file from dir.</td></tr><tr><td valign="top"><a href="#save_file-3">save_file/3</a></td><td>save file to dir.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>starting server for added Id.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>starting server for added Id.</td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="code_change-3"></a>

<h3>code_change/3</h3>





`code_change(OldVsn, State, Extra) -> any()`

<a name="init-1"></a>

<h3>init/1</h3>





<pre>init(X1::integer()) -> {ok, #state{id = undefined | integer(), base_dir = undefined | string()}}</pre>
<br></br>




initialize server.
<a name="read_file-2"></a>

<h3>read_file/2</h3>





<pre>read_file(ServerId::integer(), FileName::string()) -> {ok, binary()} | {error, atom()}</pre>
<br></br>




read file from dir.
<a name="save_file-3"></a>

<h3>save_file/3</h3>





<pre>save_file(ServerId::integer(), FileName::string(), Data::binary()) -> ok | {error, atom()}</pre>
<br></br>




save file to dir.
<a name="start-1"></a>

<h3>start/1</h3>





<pre>start(ServerId::integer()) -> {ok, pid()}</pre>
<br></br>




starting server for added Id
<a name="start_link-1"></a>

<h3>start_link/1</h3>





<pre>start_link(ServerId::integer()) -> {ok, pid()}</pre>
<br></br>




starting server for added Id
<a name="terminate-2"></a>

<h3>terminate/2</h3>





`terminate(Reason, State) -> any()`

