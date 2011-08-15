%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 15 Aug 2011 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(file_box_worker).

-behaviour(gen_server).

%% HEADERS
-include_lib("file_box/include/fb_server.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([save_file/3, read_file/2]).

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
    gen_server:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Save data.
%%
%% @end
%%--------------------------------------------------------------------
-spec(save_file(Pid::pid(), FileName::string(), Data::binary()) -> 
             {ok, Key::string()} | {error, Reason::atom()}).

save_file(Pid, FileName, Data) when is_pid(Pid) and is_list(FileName) and
                                    is_binary(Data) ->
    gen_server:call(Pid, {save_file, FileName, Data}).

%%--------------------------------------------------------------------
%% @doc
%% Read data from server.
%%
%% @end
%%--------------------------------------------------------------------
-spec(read_file(Pid::pid(), Key::string()) -> 
             {ok, Data::binary()}|{error, not_found}).

read_file(Pid, Key) ->
    gen_server:call(Pid, {read_file, Key}).


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
    {ok, #state{}}.

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
handle_call({save_file, FileName, Data}, _From, State) ->
    ServerList = file_box_server_manager:get_save_target_list(),
    {ok, FileKey} = file_box_db:get_file_key(FileName),
    MirrorCount = file_box_config:get(mirror_count),

    Reply = case save_to_servers(FileKey, Data, ServerList, MirrorCount, []) of
                {ok, SavedServerList} ->
                    Res = file_box_db:set_server_list(FileKey, SavedServerList),

                    case Res of
                        ok -> {ok, FileKey};
                        Other -> Other
                    end;

                {error, Reason} -> 
                    {error, Reason}
            end,

    {reply, Reply, State};

handle_call({read_file, Key}, _From, State) ->    
    Reply = case file_box_db:get_server_id_list(Key) of
                {ok, ServerIdList} ->
                    get_file_data(Key, ServerIdList);
                Other ->
                    Other
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
%% save data to servers and return saved server id list.
%%
%% @end
%%--------------------------------------------------------------------
-spec(save_to_servers(FileName::string(), Data::binary(), 
                      ServerList::list(integer()),
                      MirrorCount::integer(),
                      SavedServerList::list(integer())) -> 
             {ok, SavedServerList::list(integer())}|
             {errir, atom(), SavedServerList::list(integer())}).

save_to_servers(FileName, Data, ServerList, MirrorCount, SavedServerList) ->
    if length(SavedServerList) >= MirrorCount ->
            {ok, SavedServerList};
       true ->
            case ServerList of
                [] -> {error, too_few_servers, SavedServerList};
                [Server | Tail] ->
                    Res = file_box_server:save_file(Server#fb_server.id, 
                                                    FileName, Data),
                    
                    NewList = case Res of
                                  ok ->
                                      [Server#fb_server.id | SavedServerList];
                                  {error, _Reason} ->
                                      SavedServerList
                              end,

                    save_to_servers(FileName, Data, Tail, MirrorCount, NewList)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Getting file data from one of server.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_file_data(Key::string(), ServerIdList::list(integer())) -> 
             {ok, Data::binary()}|{error, Reason::atom()}).

get_file_data(Key, ServerIdList) ->
    case ServerIdList of
        [] -> {error, not_found};
        [ServerId | Tail] ->
            case file_box_server:read_file(ServerId, Key) of
                {ok, Data} -> {ok, Data};
                {error, _Reason} ->
                    get_file_data(Key, Tail)
            end
    end.
