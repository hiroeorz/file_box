
-module(file_box_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    FileBoxDb = {file_box_db,                                    %% id
                 {file_box_db, start_link, []},                  %% child
                 permanent,                                      %% restart
                 2000,                                           %% shutdown
                 worker,                                         %% type
                 [file_box_db]},                                 %% modules

    ServerSup = {file_box_server_sup,                            %% id
                 {file_box_server_sup, start_link, []},          %% child
                 permanent,                                      %% restart
                 2000,                                           %% shutdown
                 supervisor,                                     %% type
                 [file_box_server, file_box_server_sup]},        %% modules

    ServerManager = {file_box_server_manager,                    %% id
                     {file_box_server_manager, start_link, []},  %% child
                     permanent,                                  %% restart
                     2000,                                       %% shutdown
                     worker,                                     %% type
                     [file_box_server_manager]},                 %% modules

    WokerSpawner = {file_box_worker_spawner,                     %% id
                    {file_box_worker_spawner, start_link, []},   %% child
                    permanent,                                   %% restart
                    2000,                                        %% shutdown
                    worker,                                      %% type
                    [file_box_worker_spawner]},                  %% modules

    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    BootList = case file_box_config:get(type) of
                   master -> 
                       [ServerManager, FileBoxDb, ServerSup, WokerSpawner];
                   slave ->
                       [ServerSup, WokerSpawner]
               end,        

    {ok, { SupFlags, BootList } }.

