-module(push2web_protocol).
-export([decode/1, encode/1]).

decode(Req) ->
    try
        {ok, jsx:decode(Req)}
    catch
        _:_ ->
            {error, bad_packet}
    end.

encode({ok, Args}) ->
    Response = [{<<"status">>, <<"ok">>},
                {<<"args">>, Args}],
    jsx:encode(Response);
encode({error, Reason}) ->
    Response = [{<<"status">>, <<"error">>},
                {<<"description">>, Reason}],
    jsx:encode(Response).
