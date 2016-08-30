-module(push2web_http_handler).

%% cowboy_http_handler
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("push2web.hrl").

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Path, Req2} = cowboy_req:path(Req),
    {Method, Req3} = cowboy_req:method(Req2),
    {ok, Req5} = case {Method, Path} of
        {<<"POST">>, <<"/api/v1/web_push.json">>} ->
            {Hash, Req4} = cowboy_req:header(<<"authorization">>, Req3),
            Response = case auth_customer(Hash) of
                {true, CustomerId} ->
                    {ok, ReqBody, _} = cowboy_req:body(Req4),
                    PropList = push2web_protocol:decode(ReqBody),
                    Result = process_req(CustomerId, PropList),
                    _ = push2web_protocol:encode(Result);
                {false, _} ->
                    Error = {error, <<"Access denied, wrong id or password">>},
                    _ = push2web_protocol:encode(Error)
            end,    
            cowboy_req:reply(200, [], Response, Req4);
        {<<"GET">>, <<"/auth_widget">>} ->
            {PropList, Req4} = cowboy_req:qs_vals(Req3),
            Id = proplists:get_value(<<"widget_id">>, PropList),
            HostName = proplists:get_value(<<"hostname">>, PropList),
            ResultGet = push2web_db:query(get_widget_customer, [Id]),
            {Code, Response} = auth_widget(ResultGet, {Id, HostName}),
            cowboy_req:reply(Code, [], Response, Req4);
        {<<"GET">>, <<"/subscription">>} ->
            {PropList, Req4} = cowboy_req:qs_vals(Req3),
            _ = subscription(PropList),
            cowboy_req:reply(200, [], <<"ok">>, Req4);
        {<<"GET">>, <<"/message_log.last">>} ->
            {PropList, Req4} = cowboy_req:qs_vals(Req3),
            DeviceToken = proplists:get_value(<<"id">>, PropList),
            Response = get_last_message(DeviceToken),
            cowboy_req:reply(200, [], Response, Req4);
        {_, _} ->
            cowboy_req:reply(405, Req3)
    end,
    {ok, Req5, State}.

terminate(_Reason, _Req, _State) ->
    ok.

subscription(PropList) ->
    WidgetId = proplists:get_value(<<"widget_id">>, PropList),
    Token = proplists:get_value(<<"device_token">>, PropList),
    Type = proplists:get_value(<<"type">>, PropList),
    {ok, _, [{_, HostName}]} = push2web_db:query(get_widget_customer, [WidgetId]),
    Args = [WidgetId, Token, HostName, Type, 1],
    _ = push2web_db:query(put_subscription, Args).

auth_customer(Hash) ->
    Binary = base64:decode(Hash),
    [IdB, PasswdB|_] = binary:split(Binary, <<"@">>),
    Id = binary_to_integer(IdB),
    ResultGet = push2web_db:query(get_customer, [Id]),
    case ResultGet of 
        {ok, _, [{Id, PasswdB}]} -> {true, Id};
        _ -> {false, Id}
    end.

auth_widget({ok, _, [{Id, HostName}]}, {Id, HostName}) ->
    {200, <<"auth_widget=ok">>};
auth_widget(_, {Id, HostName}) ->
    {404, <<"Not found widget with Id:", Id/binary, " hostname:", HostName/binary>>}.

process_req(CustomerId, {ok, PropList}) ->
    MsgBody = proplists:get_value(<<"message_body">>, PropList),
    TargetUrl = proplists:get_value(<<"target_url">>, PropList, <<>>),
    Title = proplists:get_value(<<"title">>, PropList, ?DEFAULT_TITLE),
    Ttl = proplists:get_value(<<"ttl">>, PropList, ?DEFAULT_TTL),
    Icon = proplists:get_value(<<"icon">>, PropList, ?DEFAULT_ICON),
    ResGetUrl = push2web_db:query(get_target_url, [CustomerId, TargetUrl]),
    case {MsgBody, ResGetUrl} of
        {undefined, _} -> {error, <<"Undefined message_body">>};
        {_, undefined} -> {error, <<"Undefined target_url">>};
        {MsgBody, {ok, _, [{WCId, TargetUrl}]}} -> 
            ExpiredTime = get_expiredtime(Ttl),
            UID = list_to_binary(uuid:to_string(simple, uuid:uuid1())),
            Args = [UID, WCId, Title, TargetUrl, Icon, MsgBody, ExpiredTime],
            {Result, _} = push2web_db:query(put_message, Args),
            {Result, [{<<"message_id">>, UID}|PropList]};
        _ -> {error, <<"Undefined msg_body and target_url">>}
    end; 
process_req(_, Error) ->
    Error.

get_expiredtime(Ttl) ->
    LocalTime = calendar:local_time(),
    NowSec = calendar:datetime_to_gregorian_seconds(LocalTime),
    calendar:gregorian_seconds_to_datetime(NowSec + Ttl).
    
get_last_message(DeviceToken) ->
    {ok, _, [{WCId}|_]} = push2web_db:query(get_wc_id, [DeviceToken]),
    case push2web_db:query(get_last_message, [WCId]) of
        {ok, _, [{Title, Icon, Body}|_]} ->
            Args = [{<<"title">>, Title}, 
                    {<<"icon">>, Icon}, 
                    {<<"body">>, Body}],
            _ = push2web_protocol:encode({ok, Args});
        _ -> {error, <<"Undefined message for device">>}
    end.
