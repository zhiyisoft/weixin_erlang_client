-module(weixin_token).
-behaviour(gen_server).
-export([
  start_link/2,
  refresh/0,
  fetch/0
]).
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-include("weixin.hrl").
-define(SERVER, ?MODULE).
-define(DEFAULT_LEASE_TIME, 3600).  %% 3600 秒自动更新
-record(state, {app_id, app_secret, start_time}).


start_link(AppId, AppSecret) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [AppId, AppSecret], []).
refresh() ->
  gen_server:cast(?SERVER, refresh).
fetch() ->
  gen_server:call(?SERVER, fetch).


init([AppId, AppSecret]) ->
  ets:new(weixin_param, [public, named_table]),
  {ok, #state{app_id = AppId, app_secret = AppSecret, start_time = current_time()}, 0}.
handle_call(fetch, _From, State) ->
  #state{start_time = StartTime} = State,
  {reply,
    {ok, current_token()}, State#state{start_time = current_time()}, time_left(StartTime)}.
handle_cast(refresh, State) ->
  refresh_token(State),
  {noreply, State#state{start_time = current_time()}, time_left(current_time())}.
handle_info(timeout, State) ->
  refresh_token(State),
  {noreply, State#state{start_time = current_time()}, time_left(current_time())}.
terminate(_Reason, _State) ->
  ok.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


refresh_token(#state{app_id = AppId, app_secret = AppSecret}) ->
  case httpc:request(get, {?TOKEN_URL ++ "&appid=" ++ AppId ++ "&secret=" ++ AppSecret, []},
                          [{ssl, [{verify, verify_none}]}], []) of
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
      case lists:keyfind(<<"access_token">>, 1, jsx:decode(list_to_binary(Body))) of
        {<<"access_token">>, AccessToken} ->
          ets:insert(weixin_param, {access_token, AccessToken}),
          AccessToken
      end;
    _ -> {error, token_failed}
  end.


current_token() ->
  case ets:lookup(weixin_param, access_token) of
    [{access_token, AccessToken}] -> AccessToken;
    [] -> {error, no_token}
  end.


current_time() ->
  Now = calendar:local_time(),
  calendar:datetime_to_gregorian_seconds(Now).


time_left(StartTime) ->
  TimeElapsed = current_time() - StartTime,
  case ?DEFAULT_LEASE_TIME - TimeElapsed of
    Time when Time =<0 -> 0;
    Time               -> Time * 1000
  end.
