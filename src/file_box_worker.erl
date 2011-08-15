%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u651149.xgsfmg23.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 15 Aug 2011 by Hiroe Shin <shin@u651149.xgsfmg23.imtp.tachikawa.mopera.net>
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

-export([save_file/3]).

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
-spec(start_link() -> {ok, UniqueKey::string()} | {error, Error::atom()}).

save_file(Pid, FileName, Data) when is_pid(Pid) and is_list(FileName) and
                                    is_binary(Data) ->
    gen_server:call(Pid, {save_file, FileName, Data}).

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

    Reply = case save_to_servers(FileKey, Data, ServerList, []) of
                {ok, SavedServerList} ->
                    Res = file_box_db:set_server_list(FileKey, SavedServerList),

                    case Res of
                        ok -> {ok, FileKey};
                        Other -> Other
                    end;

                {error, Reason} -> 
                    {error, Reason}
            end,

    {reply, Reply, State}.

save_to_servers(FileName, Data, ServerList, SavedServerList) ->
    case ServerList of
        [] -> {ok, SavedServerList};
        [Server | Tail] ->
            Res = file_box_server:save_file(Server#fb_server.id, 
                                            FileName, Data),

            case Res of
                ok ->
                    save_to_servers(FileName, Data, Tail, 
                                    [Server#fb_server.id | SavedServerList]);
                {error, _Reason} ->
                    save_to_servers(FileName, Data, Tail, SavedServerList)
            end
    end.

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
