Module file_box
===============


<h1>Module file_box</h1>

* [Function Index](#index)
* [Function Details](#functions)






<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#read_file-1">read_file/1</a></td><td>
Read data.</td></tr><tr><td valign="top"><a href="#save_file-2">save_file/2</a></td><td>
Save data.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>
Boot file_box system.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>
Shut down file_box system.</td></tr></table>




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
<a name="start-0"></a>

<h3>start/0</h3>





<pre>start() -> ok</pre>
<br></br>





Boot file_box system.
<a name="stop-0"></a>

<h3>stop/0</h3>





<pre>stop() -> ok</pre>
<br></br>





Shut down file_box system.
