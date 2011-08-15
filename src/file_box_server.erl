%%
%% @author HIROE Shin <twitter: http://twitter.com/#!/hiroe_orz17>
%% @doc FileBoxServer is Dir's File manager.
%% @copyright 2011 HIROE Shin
%%

-module(file_box_server).
-behavior(gen_server).
-include_lib("eunit/include/eunit.hrl").
-record(state, {id,       %% int()
                base_dir  %% string()
               }).

-export([init/1]).
-export([start/1, handle_call/3, handle_cast/2, handle_info/2, 
         code_change/3, terminate/2,
         start_link/1, save_file/3, read_file/2]).

%%
%% @doc starting server for added Id
%%
-spec(start(integer()) -> {ok, pid()} ).

start(ServerId) ->
    ServerName = server_name(ServerId),
    gen_server:start({global, ServerName}, ?MODULE, [ServerId], []).

%%
%% @doc starting server for added Id
%%
-spec(start_link (integer()) -> {ok, pid()} ).

start_link(ServerId) ->
    ServerName = server_name(ServerId),
    gen_server:start_link({global, ServerName}, ?MODULE, [ServerId], []).

%%
%% @doc initialize server.
%%
-spec(init(integer()) -> {ok, #state{}}).

init([ServerId]) ->
    BaseDir = file_box_config:get(data_dir),
    MyDataDir = BaseDir ++ integer_to_list(ServerId),
    ?debugVal(MyDataDir),

    case file:make_dir(MyDataDir) of
        ok -> ok;
        {error, eexist} -> ok;
        {error, enoent} -> 
            io:format("no such data directory: ~p~n", [BaseDir])
    end,

    State = #state{id=ServerId, base_dir=MyDataDir},
    ok = file_box_server_manager:set_server_status(ServerId, node(), self()),
    {ok, State}.

%%
%% @doc save file to dir.
%%
-spec(save_file(integer(), string(), binary()) -> ok | {error, atom()} ).

save_file(ServerId, FileName, Data) when is_integer(ServerId) and
                                         is_list(FileName) and 
                                         is_binary(Data) ->
    ServerName = server_name(ServerId),
    gen_server:call({global, ServerName}, {save_file, FileName, Data}).

%%
%% @doc read file from dir.
%%
-spec(read_file(integer(), string()) -> {ok, binary()} | {error, atom()} ).

read_file(ServerId, FileName) when is_integer(ServerId) and
                                   is_list(FileName) ->
    ServerName = server_name(ServerId),
    gen_server:call({global, ServerName}, {read_file, FileName}).    

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
-spec(handle_call(tuple(), pid(), #state{}) -> 
             {reply, any(), #state{}} ).

handle_call({save_file, FileName, Data}, _From, State) ->
    ServerId = State#state.id,
    Size = erlang:size(Data),
    {ok, _TotalSize} = file_box_server_manager:add_size(ServerId, Size),

    Path = file_path(State#state.base_dir, FileName),
    Result = file:write_file(Path, Data),

    case Result of
        ok -> ok;
        {error, _Reason} ->
            file_box_server_manager:remove_size(ServerId, Size)
    end,


    {reply, Result, State};

handle_call({read_file, FileName}, From, State) ->
    spawn_link(fun() ->
                       Path = file_path(State#state.base_dir, FileName),
                       Result = file:read_file(Path),
                       gen_server:reply(From, Result)
               end),

    {noreply, State}.

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
  
terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
      
%%
%% @doc create server name.
%%
-spec(server_name(integer()) -> atom() ).

server_name(ServerId) ->
    list_to_atom(atom_to_list(?MODULE) ++ integer_to_list(ServerId)).

%%
%% @doc create full path to file.
%%
-spec(file_path(string(), string()) -> string() ).

file_path(BaseDir, FileName) ->
    BaseDir ++ "/" ++ FileName.
