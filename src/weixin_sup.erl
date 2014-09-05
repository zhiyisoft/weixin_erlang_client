-module(weixin_sup).
-behavior(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link({AppId, AppSecret}) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [AppId, AppSecret]).

init([AppId, AppSecret]) ->
  Token  = {weixin_token, {weixin_token, start_link, [AppId, AppSecret]},
            permanent, brutal_kill, worker, [weixin_token]},
  Client = {weixin, {weixin, start_link, []},
            permanent, brutal_kill, worker, [weixin]},

  Children = [Token, Client],
  RestartStrategy = {one_for_one, 10, 3},
  {ok, {RestartStrategy, Children}}.
