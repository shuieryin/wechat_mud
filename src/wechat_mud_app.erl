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
    stop/1,
    init/2
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
start(normal, [AppNameStr] = StartArgs) ->
    Dispatch = cowboy_router:compile(
        [
            {
                '_',
                [
                    {
                        '_',
                        ?MODULE,
                        {priv_dir, list_to_atom(AppNameStr), "assets"}
                    }
                ]
            }
        ]
    ),

    AppNameBin = list_to_binary(AppNameStr),
    NumberOfAcceptors = 100,

    Port = 13579,
    case cowboy:start_http(binary_to_atom(<<AppNameBin/binary, "_listener">>, utf8), NumberOfAcceptors, [{port, Port}], [{env, [{dispatch, Dispatch}]}]) of
        {error, Reason} ->
            error_logger:error_msg("websockets could not be started -- port ~p probably in use~nReason:~p~n", [Port, Reason]),
            cm:q();
        {ok, _Pid} ->
            io:format("websockets started on port:~p~nLoad the page http://localhost:~p/ in your browser~n", [Port, Port])
    end,

    {ok, Pid} = wechat_mud_sup:start_link(StartArgs),
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
%% Handles a request. This function is spawned by cowboy_server
%% whenever a request comes in.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Req, Env) -> SocketInfo | HttpReply when
    SocketInfo :: {cowboy_websocket, Req, Pid},
    Req :: cowboy_req:req(),
    Pid :: pid(),
    HttpReply :: {ok, Resp, cowboy_middleware:env()},
    Resp :: cowboy_req:req(),
    Env :: {priv_dir, AppName :: atom(), StaticFolder :: list()}.
init(Req, {priv_dir, AppName, StaticFolder} = Env) ->
    % error_logger:info_msg("Request raw:~p~nEnv:~p~n", [Req, Env]),
    Resource = path(Req),
    case Resource of
        ["/", "websocket", ModStr] ->
            Self = self(),
            Mod = list_to_atom(ModStr),
            Pid = spawn_link(Mod, start, [Self]),
            {cowboy_websocket, Req, Pid};
        ["/", "hapi", ModStr] ->
            Mod = list_to_atom(ModStr),
            {Reply, UpdatedReq} = apply(Mod, start, [Req]),
            {ok, cowboy_req:reply(200, "", Reply, UpdatedReq), Env};
        ["/", "assets", Filename] ->
            StaticFolderPath = filename:join([code:priv_dir(AppName), StaticFolder, Filename]),
            ReturnContent =
                case file:read_file(StaticFolderPath) of
                    {ok, FileBin} ->
                        FileBin;
                    Error ->
                        list_to_binary(io_lib:format("~p", [Error]))
                end,
            {ok, cowboy_req:reply(200, "", ReturnContent, Req), Env};
        _Resource ->
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
-spec path(Req) -> Paths when
    Req :: cowboy_req:req(),
    Paths :: [file:name_all()].
path(Req) ->
    Path = cowboy_req:path(Req),
    filename:split(binary_to_list(Path)).