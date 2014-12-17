-module(influx_udp_sup).
-behaviour(supervisor).
-include_lib("influx_udp/include/influx_udp_priv.hrl").

%% API
-export([start_link/0, start_pool/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_pool(Host::inet:ip_address()|inet:ip_hostname(), Port::inet:port_number()) -> ok.

start_pool(Host, Port) ->
  SizeArgs = [
    {size,?Config(pool_size,1)},
    {max_overflow,?Config(max_overflow,1)}
  ],
  PoolArgs = [
    {name, {local, ?POOL_NAME}},
    {worker_module, influx_udp_worker}
  ] ++ SizeArgs,

  PoolSpecs = [poolboy:child_spec(?POOL_NAME, PoolArgs, [Host, Port])],
  [supervisor:start_child(?MODULE, Spec) || Spec <- PoolSpecs],
  ok.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Children = [
    ?CHILD(influx_udp, worker)
  ], 

  {ok, { {one_for_one, 5, 10}, Children} }.