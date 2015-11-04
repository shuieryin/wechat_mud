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
    init/2,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3]).

-import(ezwebframe_mochijson2, [encode/1, decode/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts up http server.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(Port) -> ok when
    Port :: pos_integer().
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
    end,
    ok.

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

%%--------------------------------------------------------------------
%% @doc
%% Websockets handlings.
%%
%% @end
%%--------------------------------------------------------------------
-spec websocket_handle({text, Msg}, Req, Pid) -> {ok, Req, Pid} when
    Msg :: list(),
    Req :: cowboy_req:req(),
    Pid :: pid().
websocket_handle({text, Msg}, Req, Pid) ->
    %% This is a Json message from the browser
    case catch decode(Msg) of
        {'EXIT', _Why} ->
            Pid ! {invalidMessageNotJSON, Msg};
        {struct, _} = Z ->
            X1 = atomize(Z),
            Pid ! {self(), X1};
        Other ->
            Pid ! {invalidMessageNotStruct, Other}
    end,
    {ok, Req, Pid}.

%%--------------------------------------------------------------------
%% @doc
%% Websockets info.
%%
%% @end
%%--------------------------------------------------------------------
-spec websocket_info(Action, Req, Pid) -> Result when
    Action :: {send, string()} | [{cmd, _} | _] | term(),
    Req :: cowboy_req:req(),
    Pid :: pid(),
    Result :: {reply, {text, string() | binary()}, Req, Pid, hibernate} | {ok, Req, Pid, hibernate}.
websocket_info({send, Str}, Req, Pid) ->
    {reply, {text, Str}, Req, Pid, hibernate};
websocket_info([{cmd, _} | _] = L, Req, Pid) ->
    B = list_to_binary(encode([{struct, L}])),
    {reply, {text, B}, Req, Pid, hibernate};
websocket_info(Info, Req, Pid) ->
    io:format("Handle_info Info:~p Pid:~p~n", [Info, Pid]),
    {ok, Req, Pid, hibernate}.

%%--------------------------------------------------------------------
%% @doc
%% Terminate function for websockets.
%%
%% @end
%%--------------------------------------------------------------------
-spec websocket_terminate(_Reason, _Req, Pid) -> ok when
    _Reason :: any(),
    _Req :: cowboy_req:req(),
    Pid :: pid().
websocket_terminate(_Reason, _Req, Pid) ->
    io:format("websocket.erl terminate:~n"),
    exit(Pid, socketClosed),
    ok.

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
-spec path(Req) -> Paths when
    Req :: cowboy_req:req(),
    Paths :: [file:name_all()].
path(Req) ->
    Path = cowboy_req:path(Req),
    filename:split(binary_to_list(Path)).

%%--------------------------------------------------------------------
%% @doc
%% Special binary_to_atom
%%
%% @end
%%--------------------------------------------------------------------
-spec binary_to_atom(binary()) -> atom().
binary_to_atom(B) ->
    list_to_atom(binary_to_list(B)).

%%--------------------------------------------------------------------
%% @doc
%% Atomize turns all the keys in a struct to atoms
%%
%% @end
%%--------------------------------------------------------------------
-spec atomize(Source) -> Result when
    Source :: {struct, [{binary(), list() | term()}]} | list() | term(),
    Result :: {struct, [{atom(), list() | term()}]} | list() | term().
atomize({struct, L}) ->
    {struct, [{binary_to_atom(I), atomize(J)} || {I, J} <- L]};
atomize(L) when is_list(L) ->
    [atomize(I) || I <- L];
atomize(X) ->
    X.