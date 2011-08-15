-module(file_box_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    file_box_config:load(),

    case node() of
	nonode@nohost -> ok;
	Node -> 
	    Cookie = file_box_config:get(cookie),
	    erlang:set_cookie(Node, Cookie)
    end,

    file_box_sup:start_link().

stop(_State) ->
    ok.
