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
	    erlang:set_cookie(Node, Cookie),
            NodeList = file_box_config:get(node_list),
            connect_to_nodes(NodeList)
    end,

    file_box_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Local functions
%% ===================================================================

connect_to_nodes(NodeList) ->
    case NodeList of
        [] ->
            receive
            after 2000 -> ok
            end,
            io:format("connected nodes:~p~n", [nodes()]),
            ok;
        [Node | Tail] ->
            net_kernel:connect(Node),
            connect_to_nodes(Tail)
    end.
