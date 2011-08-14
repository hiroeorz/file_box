%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 14 Aug 2011 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(file_box_server_manager).

-behaviour(gen_server).

%% HEADERS
-include_lib("file_box/include/fb_server.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([set_server_status/3, set_server_status/4, 
         get_save_target_list/0, add_size/2]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    DbFilePath = file_box_config:get(fb_server_manager_db_path),
    create_tables(DbFilePath),
    restore_table(),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @doc
%% Set FileBoxServerManager Informations.
%%
%% @end
%%--------------------------------------------------------------------
-spec(set_server_status(Id::integer(), Node::atom(), Pid::pid()) -> ok ).

set_server_status(Id, Node, Pid) ->
    gen_server:call(?SERVER, {set_server_status, Id, Node, Pid}).

%%--------------------------------------------------------------------
%% @doc
%% Set FileBoxServerManager Informations.
%%
%% @end
%%--------------------------------------------------------------------

-spec(set_server_status(Id::integer(), Node::atom(), 
                        Pid::pid(), TotalSize::integer()) -> ok ).

set_server_status(Id, Node, Pid, TotalSize) ->
    gen_server:call(?SERVER, {set_server_status, Id, Node, Pid, TotalSize}).

%%--------------------------------------------------------------------
%% @doc
%% Get File Save Target Cluster List.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_save_target_list() -> list(#fb_server{}) ).

get_save_target_list() ->
    gen_server:call(?SERVER, {get_save_target_list}).
    
%%--------------------------------------------------------------------
%% @doc
%% Get File Save Target Cluster List.
%%
%% @end
%%--------------------------------------------------------------------
-spec(add_size(Id::integer(), Size::integer()) -> {ok, integer()} ).

add_size(Id, Size) ->
    gen_server:call(?SERVER, {add_size, Id, Size}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({set_server_status, Id, Node, Pid}, _From, State) ->

    ServerInfo = case ets:lookup(fbServerManagerRam, Id) of
                     [] ->
                         #fb_server{id=Id, node=Node, pid=Pid};
                     [ServerStatus] ->
                         ServerStatus#fb_server{node=Node, pid=Pid}
                 end,

    ets:insert(fbServerManagerRam, ServerInfo),
    dets:insert(fbServerManagerDisk, ServerInfo),    
    {reply, ok, State};

handle_call({set_server_status, Id, Node, Pid, TotalSize}, _From, State) ->
    ServerInfo = #fb_server{id=Id, node=Node, pid=Pid, total_size=TotalSize},
    ets:insert(fbServerManagerRam, ServerInfo),
    dets:insert(fbServerManagerDisk, ServerInfo),    
    {reply, ok, State};

handle_call({get_save_target_list}, _From, State) ->
    List = case ets:first(fbServerManagerRam) of
               '$end_of_table' -> [];
               First ->
                   get_servers(First, ets:lookup(fbServerManagerRam, First))
           end,

    MirrorCount = file_box_config:get(mirror_count),
    NewList = lists:sublist(List, MirrorCount),

    {reply, NewList, State};

handle_call({add_size, Id, Size}, _From, State) ->
    Reply = case ets:lookup(fbServerManagerRam, Id) of
                [] -> {error, invalid_id};
                [ServerStatus] ->
                    NewTotalSize = ServerStatus#fb_server.total_size + Size,
                    NewServerStatus = 
                        ServerStatus#fb_server{total_size=NewTotalSize},
                    ets:insert(fbServerManagerRam, NewServerStatus),
                    dets:insert(fbServerManagerDisk, NewServerStatus),
                    {ok, NewTotalSize}
            end,

    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create ets and dets tables.
%% This function exec when system startup.
%%
%% @end
%%--------------------------------------------------------------------
-spec(create_tables(FilePath::string()) -> 
             {ok, server_manager_db}|{error, any()} ).

create_tables(FilePath) ->
    ets:new(fbServerManagerRam, 
            [named_table, ordered_set, {keypos, #fb_server.id}]),
    dets:open_file(fbServerManagerDisk, 
                   [{file, FilePath}, {keypos, #fb_server.id}]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load Data from dets table to ets table.
%% This function exec when system startup.
%%
%% @end
%%--------------------------------------------------------------------
-spec(restore_table() -> ok).

restore_table()->
    Insert = fun(ServerStatus)->
		     ets:insert(fbServerManagerRam, ServerStatus),
		     continue
	     end,
    dets:traverse(fbServerManagerDisk, Insert).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Return Server Cluster list sorted by total size.
%%
%% @end
%%--------------------------------------------------------------------

-spec(get_servers(Before::integer(), ServerList::list(#fb_server{})) ->
             list(#fb_server{})).

get_servers(Before, ServerList) ->
    case ets:next(fbServerManagerRam, Before) of
        '$end_of_table' ->
            lists:sort(fun(A, B) -> 
                               A#fb_server.total_size < B#fb_server.total_size
                       end,
                       ServerList);
        Next ->
            [Server] = ets:lookup(fbServerManagerRam, Next),
            get_servers(Next, [Server | ServerList])
    end.

