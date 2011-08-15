%%
%% @author HIROE Shin <twitter: http://twitter.com/#!/hiroe_orz17>
%% @doc FileBoxDB is Database manager that has information for where is file data.
%% @copyright 2011 HIROE Shin
%%

-module(file_box_db).
-behavior(gen_server).
-include_lib("eunit/include/eunit.hrl").

-record(state, {db_pid       %% pid()
               }).

-export([init/1]).
-export([start/0, start_link/0, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-export([get_file_key/1, get_server_id_list/1, set_server_list/2]).

%%
%% @doc starting server.
%%
-spec(start() -> {ok, pid()} ).

start() ->
    DbFileName = file_box_config:get(db_file_name),
    gen_server:start({global, ?MODULE}, ?MODULE, [DbFileName], []).

%%
%% @doc starting server.
%%
-spec(start_link () -> {ok, pid()} ).

start_link() ->
    DbFileName = file_box_config:get(db_file_name),
    gen_server:start_link({global, ?MODULE}, ?MODULE, [DbFileName], []).

%%
%% @doc initialize server.
%%
-spec(init([string()]) -> {ok, #state{}}).
 
init([DbFileName]) ->
    {ok, DBPid} = sqlite3:open(file_box_db_sqlite3, [{file, DbFileName}]),
    create_tables(DBPid),
    State = #state{db_pid=DBPid},
    {ok, State}.

%%
%% @doc save file key and server_id.
%%
-spec(get_file_key(string()) -> {ok, string()} ).

get_file_key(FileName) ->
    gen_server:call({global, ?MODULE}, {get_file_key, FileName}).

%%
%% @doc get server_id from file key.
%%
-spec(get_server_id_list(string()) -> 
             {ok, list(integer())}|{error, not_found} ).

get_server_id_list(FileKey) ->
    gen_server:call({global, ?MODULE}, {get_server_id_list, FileKey}).

%%
%% @doc get server_id from file key.
%%
-spec(set_server_list(string(), list(integer())) -> ok |{error, not_found} ).

set_server_list(FileKey, ServerIdList) ->
    gen_server:call({global, ?MODULE}, {set_server_list, FileKey, ServerIdList}).

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

handle_call({set_server_list, FileKey, ServerIdList}, _From, State) ->
    DBPid = State#state.db_pid,

    Result = 
        sqlite3:sql_exec(DBPid,
                         "update files_info
                                 set server_id_list = :server_id_list 
                                 where key = :key",
                         [{':server_id_list', list_to_binary(ServerIdList)},
                          {':key', FileKey}]),

    {reply, Result, State};

handle_call({get_file_key, FileName}, _From, State) ->
    UniqKey = create_file_key(FileName),

    DBPid = State#state.db_pid,
    Result = sqlite3:sql_exec(DBPid,
                              "insert into files_info (key) 
                                 values (:key)",
                              [{':key', UniqKey}]),
    case Result of
        {rowid, _Id} -> {reply, {ok, UniqKey}, State};
        Other -> {reply, Other, State}
    end;

handle_call({get_server_id_list, FileKey}, _From, State) ->
    DBPid = State#state.db_pid,
    SqlResult = sqlite3:sql_exec(DBPid,
                                 "select server_id_list from files_info
                                    where key = :key",
                                 [{':key', FileKey}]),
    Reply = case SqlResult of
                [] -> 
                    {error, not_found};
                [_, {rows,[{ServerIdListBin}]}] ->
                    {ok, binary_to_list(ServerIdListBin)}
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

terminate(_Reason, State) -> 
    DBPid = State#state.db_pid,
    sqlite3:close(DBPid),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%
%% @doc create table for file file position of where's pid.
%%
-spec(create_tables(pid()) -> ok).

create_tables(DBPid) ->
    case lists:member(files_info, sqlite3:list_tables(DBPid)) of
        true -> ok;
        false ->
            sqlite3:sql_exec(DBPid,
                            "create table files_info (
                               key VARCHAR(32) PRIMARY KEY,
                               server_id_list BLOB)")
    end.

%%
%% @doc create table for file position of where's pid.
%%
-spec(create_file_key(string()) -> string()).

create_file_key(Str) when is_list(Str) ->
    {Megaseconds, Seconds, Microseconds} = erlang:now(),
    TimeStr = integer_to_list(Megaseconds) ++
        integer_to_list(Seconds) ++
        integer_to_list(Microseconds),

    Context0 = crypto:sha_init(),
    Context1 = crypto:sha_update(Context0, TimeStr),
    Context2 = crypto:sha_update(Context1, atom_to_list(node())),
    Context3 = crypto:sha_update(Context2, Str),
    Digest = crypto:sha_final(Context3),

    lists:flatten(lists:map(fun(X) -> 
                                    io_lib:format("~.16X", [X, ""]) 
                            end, 
                            binary_to_list(Digest))).
    
