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
-export([start/0, start_link/0, handle_call/3, code_change/3, terminate/2,
         get_file_key/2, get_server_id/1]).

%%
%% @doc starting server.
%%
-spec(start() -> {ok, pid()} ).

start() ->
    DbFileName = file_box_config:get(db_file_name),
    gen_server:start({local, ?MODULE}, ?MODULE, [DbFileName], []).

%%
%% @doc starting server.
%%
-spec(start_link () -> {ok, pid()} ).

start_link() ->
    DbFileName = file_box_config:get(db_file_name),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [DbFileName], []).

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
-spec(get_file_key(integer(), string()) -> {ok, string()} ).

get_file_key(ServerId, FileName) ->
    gen_server:call(?MODULE, {get_file_key, ServerId, FileName}).

%%
%% @doc get server_id from file key.
%%
-spec(get_server_id(string()) -> {ok, integer()}|{error, not_found} ).

get_server_id(FileKey) ->
    gen_server:call(?MODULE, {get_server_id, FileKey}).

%%
%% @doc handle call for save_file
%%
-spec(handle_call(tuple(), pid(), #state{}) -> 
             {reply, any(), #state{}} ).

handle_call({get_file_key, ServerId, FileName}, _From, State) ->
    UniqKey = create_file_key(FileName),

    DBPid = State#state.db_pid,
    Result = sqlite3:sql_exec(DBPid,
                              "insert into files_info 
                                 values (:key, :server_id)",
                              [{':key', UniqKey}, {':server_id', ServerId}]),
    case Result of
        {rowid, _Id} -> {reply, {ok, UniqKey}, State};
        Other -> {reply, Other, State}
    end;

handle_call({get_server_id, FileKey}, _From, State) ->
    DBPid = State#state.db_pid,
    SqlResult = sqlite3:sql_exec(DBPid,
                                 "select server_id from files_info
                                    where key = :key",
                                 [{':key', FileKey}]),
    Reply = case SqlResult of
                [] -> 
                    {error, not_found};
                [_, {rows,[{ServerId}]}] ->
                    {ok, ServerId}
            end,

    {reply, Reply, State}.

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
                               server_id INTEGER NOT NULL)")
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
    
