-module(weixin).
-behaviour(gen_server).
-export([
  start_link/0,
  token/0
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
-record(state, {}).


start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
token() ->
  gen_server:call(?SERVER, token).


init([]) ->
  {ok, #state{}}.
handle_call(token, _From, State) ->
  {reply, weixin_token:fetch(), State}.
handle_cast(_Action, State) ->
  {noreply, State}.
handle_info(timeout, State) ->
  {noreply, State}.
terminate(_Reason, _State) ->
  ok.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
