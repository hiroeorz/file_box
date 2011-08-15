-module(file_box).

-export([start/0, stop/0, save_file/2, read_file/1]).

%%--------------------------------------------------------------------
%% @doc
%% Boot file_box system.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start() -> ok).

start() ->
  application:start(?MODULE).

%%--------------------------------------------------------------------
%% @doc
%% Shut down file_box system.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop() -> ok).

stop() ->
  application:stop(?MODULE).

%%--------------------------------------------------------------------
%% @doc
%% Save data.
%%
%% @end
%%--------------------------------------------------------------------
-spec(save_file(FileName::string(), Data::binary()) -> 
             {ok, Key::string()} | {error, Reason::atom()}).

save_file(FileName, Data) ->
    file_box_worker_spawner:save_file(FileName, Data).    

%%--------------------------------------------------------------------
%% @doc
%% Read data.
%%
%% @end
%%--------------------------------------------------------------------
-spec(read_file(FileName::string()) -> 
             {ok, Data::binary()} | {error, not_found}).

read_file(Key) ->
    file_box_worker_spawner:read_file(Key).

