Module file_box_db
==================


<h1>Module file_box_db</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


FileBoxDB is Database manager that has information for where is file data.



Copyright (c) 2011 HIROE Shin


__Authors:__ HIROE Shin ([`twitter: http://twitter.com/#!/hiroe_orz17`](mailto:twitter: http://twitter.com/#!/hiroe_orz17)).

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_file_key-1">get_file_key/1</a></td><td>save file key and server_id.</td></tr><tr><td valign="top"><a href="#get_server_id_list-1">get_server_id_list/1</a></td><td>get server_id from file key.</td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td>initialize server.</td></tr><tr><td valign="top"><a href="#set_server_list-2">set_server_list/2</a></td><td>get server_id from file key.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>starting server.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>starting server.</td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="code_change-3"></a>

<h3>code_change/3</h3>





`code_change(OldVsn, State, Extra) -> any()`

<a name="get_file_key-1"></a>

<h3>get_file_key/1</h3>





<pre>get_file_key(FileName::string()) -> {ok, string()}</pre>
<br></br>




save file key and server_id.
<a name="get_server_id_list-1"></a>

<h3>get_server_id_list/1</h3>





<pre>get_server_id_list(FileKey::string()) -> {ok, [integer()]} | {error, not_found}</pre>
<br></br>




get server_id from file key.
<a name="init-1"></a>

<h3>init/1</h3>





<pre>init(X1::[string()]) -> {ok, #state{}}</pre>
<br></br>




initialize server.
<a name="set_server_list-2"></a>

<h3>set_server_list/2</h3>





<pre>set_server_list(FileKey::string(), ServerIdList::[integer()]) -> ok | {error, not_found}</pre>
<br></br>




get server_id from file key.
<a name="start-0"></a>

<h3>start/0</h3>





<pre>start() -> {ok, pid()}</pre>
<br></br>




starting server.
<a name="start_link-0"></a>

<h3>start_link/0</h3>





<pre>start_link() -> {ok, pid()}</pre>
<br></br>




starting server.
<a name="terminate-2"></a>

<h3>terminate/2</h3>





`terminate(Reason, State) -> any()`

