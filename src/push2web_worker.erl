-module(push2web_worker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("push2web.hrl").

-record(state, {header, gcm_url, timer, queue}).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, ApiKey} = application:get_env(push2web, gcm_api_key),
    {ok, GcmUrl} = application:get_env(push2web, gcm_url),
    Header = [{"authorization", string:join(["key", ApiKey], "=")}],
    {ok, TRef} = timer:send_after(1000, send),
    {ok, #state{gcm_url = GcmUrl, header = Header, timer = TRef}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(send, #state{timer = TRef, header = H, gcm_url = Url} = State) ->
    {ok, cancel} = timer:cancel(TRef),
    ResultGet = push2web_db:query(get_message, []),
    _ = send_messages(H, Url, ResultGet),
    {ok, NewTRef} = timer:send_after(1000, send),
    {noreply, State#state{timer = NewTRef}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
send_messages(Header, Url, {ok, _, Messages}) when Messages =/= [] ->
    AllTokens = lists:foldl(fun({Id, WCId}, Acc) -> 
        push2web_db:query(update_message_status, [1, Id]),
        case push2web_db:query(get_device_token, [WCId]) of
            {ok, _, Tokens} -> Acc ++ [T || {T} <- Tokens];
            _ -> Acc
        end
    end, [], Messages),
    _ = send_http(AllTokens, Header, Url);
send_messages(_, _, _) -> ok.

send_http(AllTokens, Header, Url) ->
    Json = jsx:encode([{<<"registration_ids">>, AllTokens}]),
    Request = {Url, Header, "application/json", Json},
    case httpc:request(post, Request, [{ssl, [{verify, 0}]}], 
                                        [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, ResponseJson}} ->
            Response = jsx:decode(ResponseJson),
            ?LOG_INFO("Notification sended: ~p; Tokens: ~p", [Response, AllTokens]);
        {ok, {{_, 401, Error}, _, R}} ->
            ?LOG_ERROR("Notification sended error: ~p; Reason: ~p", [Error, R]);
        {ok, {{_, Code, Error}, _, _}} ->
            ?LOG_ERROR("Request error: ~p; Code: ~p", [Error, Code])
    end.
