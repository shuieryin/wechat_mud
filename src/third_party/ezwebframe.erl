-module(ezwebframe).

-export([start_link/2,
    init/2
]).

-import(ezwebframe_mochijson2, [encode/1, decode/1]).

%% env has only one parameter - reserved for future expansion

-record(env, {dispatch}).

start_link(Dispatch, Port) ->
    io:format("Starting:~p~n", [file:get_cwd()]),
%%     ok = application:start(crypto),
%%     ok = application:start(ranch),
%%     ok = application:start(cowlib),
%%     ok = application:start(cowboy),
    ok = web_server_start(Port, Dispatch).

web_server_start(Port, Dispatcher) ->
    E0 = #env{dispatch = Dispatcher},
    Dispatch = cowboy_router:compile([{'_', [{'_', ?MODULE, E0}]}]),
    %% server is the name of this module
    NumberOfAcceptors = 100,
    Status =
        cowboy:start_http(ezwebframe,
            NumberOfAcceptors,
            [{port, Port}],
            [{env, [{dispatch, Dispatch}]}]),
    case Status of
        {error, _} ->
            io:format("websockets could not be started -- port ~p probably in use~n", [Port]),
            init:stop();
        {ok, _Pid} ->
            io:format("websockets started on port:~p~n", [Port])
    end.

init(Req, E0) ->
%%     error_logger:info_msg("Request raw:~p~n", [Req]),
    Resource = path(Req),
    case Resource of
        ["/", "websocket", ModStr] ->
            Self = self(),
            Mod = list_to_atom(ModStr),
            %% Spawn an erlang handler
            %% The return value will cause cowboy
            %% to call this module at the entry point
            %% websocket_handle
            Pid = spawn_link(Mod, start, [Self]),
            {cowboy_websocket, Req, Pid};
        ["/", "hapi", ModStr] ->
            Mod = list_to_atom(ModStr),
            {ok, cowboy_req:reply(200, "", apply(Mod, start, [Req]), Req), E0};
        _ ->
            {ok, cowboy_req:reply(200, "", "", Req), E0}
    end.

path(Req) ->
    Path = cowboy_req:path(Req),
    filename:split(binary_to_list(Path)).