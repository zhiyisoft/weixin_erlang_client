-module(weixin_app).
-behavior(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  {ok, AppId}     = application:get_env(weixin, app_id),
  {ok, AppSecret} = application:get_env(weixin, app_secret),
  weixin_sup:start_link({AppId, AppSecret}).

stop(_State) ->
    ok.
