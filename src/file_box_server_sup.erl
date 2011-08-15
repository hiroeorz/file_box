%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc SuperVisor for FileBoxDBSerber.
%%%
%%% @end
%%% Created : 14 Aug 2011 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(file_box_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(list()) ->  {ok, {atom(), list()}} |
                       ignore |
                       {error, atom()}).
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    ChildList = file_box_server_childs(),

    {ok, {SupFlags, ChildList}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc create workers list.
%%
-spec(file_box_server_childs() -> list(tuple()) ).

file_box_server_childs() ->
    ServerIdList = file_box_config:get(file_server_id_list),
    BaseDir = file_box_config:get(data_dir),
    file_box_server_childs(ServerIdList, BaseDir, []).

file_box_server_childs(ServerIdList, BaseDir, ChildList) ->
    case ServerIdList of
        [] -> 
            ChildList;
        [ServerNo | Tail] ->
            WorkerName = 
                list_to_atom("file_box_server_" ++ integer_to_list(ServerNo)),

            AChild = {WorkerName,                                %% id 
                      {file_box_server, start_link, [ServerNo]}, %% child
                      permanent,                                 %% restart
                      2000,                                      %% shutdown
                      worker,                                    %% type
                      [file_box_server]},                        %% modules

            file_box_server_childs(Tail, BaseDir, [AChild | ChildList])
    end.
