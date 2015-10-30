%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Web server start module. This module is only supports mapping "hapi"
%%% prefixed url as well as websocket connections. This is module is a
%%% simplification version from "https://github.com/joearms/ezwebframe".
%%%
%%% @end
%%% Created : 30. Oct 2015 10:22 PM
%%%-------------------------------------------------------------------
-module(web_server_start).
-author("shuieryin").

%% API
-export([start_link/1,
    init/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts up http server.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(Port) -> no_return() when
    Port :: port().
start_link(Port) ->
    io:format("Starting:~p~n", [file:get_cwd()]),
    %% server is the name of this module
    Env = #{},
    Dispatch = cowboy_router:compile([{'_', [{'_', ?MODULE, Env}]}]),
    NumberOfAcceptors = 100,
    Status = cowboy:start_http(ezwebframe, NumberOfAcceptors, [{port, Port}], [{env, [{dispatch, Dispatch}]}]),
    case Status of
        {error, _} ->
            io:format("websockets could not be started -- port ~p probably in use~n", [Port]),
            init:stop();
        {ok, _Pid} ->
            io:format("websockets started on port:~p~n", [Port])
    end.

%%--------------------------------------------------------------------
%% @doc
%% Handles a request. This function is spawned by cowboy_server
%% whenever a request comes in.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Req, Env) -> SocketInfo | HttpReply when
    SocketInfo :: {cowboy_websocket, Req, Pid},
    Req :: cowboy_req:req(),
    Pid :: pid(),
    HttpReply :: {ok, Resp, list()},
    Resp :: cowboy_req:req(),
    Env :: map().
init(Req, Env) ->
%%     error_logger:info_msg("Request raw:~p~n", [Req]),
    Resource = path(Req),
    case Resource of
        ["/", "websocket", ModStr] ->
            Self = self(),
            Mod = list_to_atom(ModStr),
            Pid = spawn_link(Mod, start, [Self]),
            {cowboy_websocket, Req, Pid};
        ["/", "hapi", ModStr] ->
            Mod = list_to_atom(ModStr),
            {ok, cowboy_req:reply(200, "", apply(Mod, start, [Req]), Req), Env};
        _ ->
            {ok, cowboy_req:reply(200, "", "", Req), Env}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Gets the request url path from request content and split it in
%% list with separator "/".
%%
%% @end
%%--------------------------------------------------------------------
path(Req) ->
    Path = cowboy_req:path(Req),
    filename:split(binary_to_list(Path)).