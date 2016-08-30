-module(push2web_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    create_db_pools(),
    create_sender_pools(),
    Port = application:get_env(push2web, listen_port, 8001),
    Acceptors = application:get_env(push2web, acceptors, 10),
    Routes = [
        {'_', [
            {"/", cowboy_static, {priv_file, push2web, "index.html"}},
            {"/service-worker.js", cowboy_static, {priv_file, push2web, 
                "widgets/push2web/service-worker.js"}},
            {"/hyperpush", cowboy_static, {priv_file, push2web, 
                "widgets/push2web/main.js"}},
            {"/widgets/[...]", cowboy_static, {priv_dir, push2web, "widgets/", 
                    [{mimetypes, cow_mimetypes, all}]}},
            {'_', push2web_http_handler, []}
        ]}
    ],
    Dispatch = cowboy_router:compile(Routes),
    cowboy:start_http(http_listener, Acceptors, 
        [{port, Port}], 
        [{env, [
            {dispatch, Dispatch}
        ]}]),
    push2web_sup:start_link().

stop(_State) ->
    ok.

create_db_pools() -> 
    {ok, Pools} = application:get_env(push2web, db_pools),
    [begin
        {_, PoolSize, RestOpts} = lists:keytake(pool_size, 1, Opts),
        PoolArgs = [PoolSize, {worker, epgsql}],
        ok = octopus:start_pool(PoolName, PoolArgs, [RestOpts])
    end || {PoolName, Opts} <- Pools].

create_sender_pools() -> 
    {ok, PoolSize} = application:get_env(push2web, gcm_pool_size),
    PoolArgs = [{pool_size, PoolSize}, {worker, push2web_worker}],
    ok = octopus:start_pool(gcm_sender, PoolArgs, []).
