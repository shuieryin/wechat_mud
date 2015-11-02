%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Nls gen_server supervisor. It initializes per csv file under
%%% priv/nls by nls_sup.erl which the number of initialized
%%% nls_server (server name is set to nls file name) equals to the
%%% number of nls files.
%%%
%%% @end
%%% Created : 14. Sep 2015 10:42 PM
%%%-------------------------------------------------------------------
-module(nls_sup).
-author("shuieryin").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-include("nls.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor by creating nls_server workers per nls csv
%% files under priv/nls.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
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
-spec init(Args :: term()) ->
    {ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}} | ignore.
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    ChildList = gen_child_list(),
    error_logger:info_msg("ChildList:~p~n", [ChildList]),
    {ok, {SupFlags, ChildList}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Actual function which creates workers from nls files. For details
%% see start_link/0 spec.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_child_list() -> ChildList when
    ChildList :: [{WorkerName, {nls_server, start_link, []}, Restart, Shutdown, Type, [nls_server]}],
    Restart :: supervisor:restart(),
    Shutdown :: supervisor:shutdown(),
    Type :: supervisor:worker(),
    WorkerName :: atom().
gen_child_list() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    {ok, FileNameList} = file:list_dir(?NLS_PATH),
    [{list_to_atom(filename:rootname(FileName)), {nls_server, start_link, [filename:join(?NLS_PATH, FileName)]}, Restart, Shutdown, Type, [nls_server]} || FileName <- FileNameList, filename:extension(FileName) == ?NLS_EXTENSION, FileName /= ?COMMON_NLS].