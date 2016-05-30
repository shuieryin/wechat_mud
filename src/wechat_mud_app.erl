%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Wechat mud application start.
%%%
%%% @end
%%% Created : 26. Aug 2015 11:04 AM
%%%-------------------------------------------------------------------
-module(wechat_mud_app).

-behaviour(application).

%% Application callbacks
-export([
    start/2,
    stop/1
]).

-record(state, {}).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Start application
%%
%% @end
%%--------------------------------------------------------------------
-spec start(StartType, StartArgs) -> Return when
    StartType :: application:start_type(),
    StartArgs :: term(), % generic term
    Return :: {ok, pid(), State :: #state{}}.
start(normal, _StartArgs) ->
    ok = start_redis(),
    ok = start_web(),
    {ok, Pid} = wechat_mud_sup:start_link(),
    ok = elib:show_errors(20),
    {ok, Pid, #state{}}.

%%--------------------------------------------------------------------
%% @doc
%% Stop application
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(State) -> ok when
    State :: #state{}.
stop(_State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Launch web service
%%
%% @end
%%--------------------------------------------------------------------
-spec start_redis() -> ok.
start_redis() ->
    spawn(
        fun() ->
            io:format("~p~n", [file:get_cwd()]),
            elib:cmd("redis-server")
        end),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Launch web service
%%
%% @end
%%--------------------------------------------------------------------
-spec start_web() -> ok.
start_web() ->
    io:format("a web test....~n"),
    Port = 13579,
    io:format("Load the page http://localhost:~p/ in your browser~n", [Port]),
    web_server_start:start_link(Port).