-module(influx_udp_app).
-behaviour(application).
-include_lib("influx_udp/include/influx_udp_priv.hrl").
-include_lib("influx_udp/include/influx_udp.hrl").
-define(SERVER, ?MODULE).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  ?I(#{msg => "Starting application: influx_udp"}),
  influx_udp_sup:start_link().

stop(_State) ->
  ok.