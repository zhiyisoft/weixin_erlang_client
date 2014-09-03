-module(weixin_sup).
-behavior(supervisor).

-export([start_link/1, start_child/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link({AppId, AppSecret}) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [AppId, AppSecret]).
start_child({To, Msg}) ->
  supervisor:start_child(?SERVER, [{To, Msg}]).

init([AppId, AppSecret]) ->
  Token = {token, {token, start_link, [AppId, AppSecret]},
           permanent, brutal_kill, worker, [token]},
  Children = [Token],
  RestartStrategy = {one_for_one, 10, 3},
  {ok, {RestartStrategy, Children}}.
