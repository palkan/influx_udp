-module(influx_udp_sup).
-behaviour(supervisor).
-include_lib("influx_udp/include/influx_udp_priv.hrl").

%% API
-export([start_link/0, start_pool/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(
  CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}
).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_pool(
    Host::inet:ip_address()|inet:ip_hostname(),
    Port::inet:port_number()
  ) -> {ok, undefined | pid()} |
       {ok, undefined | pid(), any()} |
       {error, any()}.
start_pool(Name, Options) ->
  SizeArgs = [
    {size, maps:get(pool_size, Options)},
    {max_overflow, maps:get(max_overflow, Options)}
  ],

  PoolArgs = [
    {name, {local, Name}},
    {worker_module, influx_udp_worker}
  ] ++ SizeArgs,

  supervisor:start_child(
    ?MODULE,
    poolboy:child_spec(Name, PoolArgs, [{options, Options}])
  ).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Children = [
    ?CHILD(influx_udp, worker)
  ],
  {ok, { {one_for_one, 5, 10}, Children} }.
