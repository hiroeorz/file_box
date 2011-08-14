%% File : file_box_config.erl
%% Description : configuration manager for file_box

-module(file_box_config).
-include_lib("eunit/include/eunit.hrl").
-export([load/0, load/1, get/1]).

load() ->
    {ThisFile, _} = filename:find_src(file_box_config),
    SrcDir = filename:dirname(ThisFile),
    ConfFilePath = filename:absname_join(SrcDir, "../conf/file_box.conf"),
    load(ConfFilePath).

load(File) ->
    case file:open(File, read) of
	{ok, Dev} ->
	    {ok, {config, ConfigList}} = io:read(Dev, 0),
	    read_config(file_box, ConfigList),
	    file:close(Dev);
	Other ->
	    ?debugVal(Other),
	    Other
    end.

read_config(AppName, ConfigList) ->
    case ConfigList of
	[] -> ok;
	[Config | Tail] ->
	    {Key, Val} = Config,
	    application:set_env(AppName, Key, Val),
	    read_config(AppName, Tail)
    end.

get(Name) ->
    {ok, Value} = application:get_env(file_box, Name),
    Value.
