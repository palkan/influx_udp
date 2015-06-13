-module(influx_udp).
-include_lib("influx_udp/include/influx_udp_priv.hrl").
-include_lib("influx_udp/include/influx_udp.hrl").
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start/0, stop/0]).

-export([start_link/0, init_server/0]).

-export([
  start_pool/2,
  write/1,
  write/2,
  write/3,
  write/4
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
{
  defaults ::map() %% default pool configuration
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  proc_lib:start_link(?SERVER, init_server, []).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
  application:start(influx_udp).

stop() ->
  application:stop(influx_udp).

%% @doc
%% Start new pool with Name and Options (as map)
%% Options:
%% - host - InfluxDB hostname
%% - port - InflxuDB UDP port
%% - pool_size - Pool size (numer of workers)
%% - max_overflow - Pool max overflow size
%% @end
-spec start_pool(Name::atom(), Options::map()) -> ok.
start_pool(Name, Options) ->
  gen_server:call(?SERVER, {start_pool, Name, Options}).

%% @doc
%% Write binary data to influx using default pool.
%% Note: Assumed that data represents InfluxDB-valid input.
%% @end
-spec write(Bin::binary()) -> ok.
write(Bin) ->
  gen_server:call(?SERVER, {send_to_pool, default, {write, Bin}}).

%% @doc
%% Write binary data to influx using named pool.
%% @end
-spec write(Pool::atom(), Bin::binary()) -> ok
    ; (
        Series::atom()|string()|binary(),
        Points::list(influx_data_point())|influx_data_point()
      ) -> ok.
write(Pool, Bin) when is_binary(Bin) ->
  gen_server:call(?SERVER, {send_to_pool, Pool, {write, Bin}});

%% @doc
%% Write data points to Series using default pool.
%% @end
write(Series, Points) ->
  gen_server:call(
    ?SERVER,
    {send_to_pool, default, {write, Series, Points}}
  ).

-spec write(
    Pool::atom(),
    Series::atom()|string()|binary(),
    Points::list(influx_data_point())|influx_data_point()
  ) -> ok
; (
    Series::atom()|string()|binary(),
    Points::list(influx_data_point())|influx_data_point(),
    Tags::influx_data_point()
  ) -> ok.

%% @doc
%% Write data points to Series using named pool.
%% @end
write(Pool, Series, Points) when is_atom(Pool) ->
  gen_server:call(
    ?SERVER,
    {send_to_pool, Pool, {write, Series, Points}}
  );

%% @doc
%% Write data points with Tags to Series using default pool.
%% @end
write(Series, Points, Tags) ->
  gen_server:call(
    ?SERVER,
    {send_to_pool, default, {write, Series, Points, Tags}}
  ).

%% @doc
%% Write data points with Tags to Series using named pool.
%% @end
-spec write(
    Pool::atom(),
    Series::atom()|string()|binary(),
    Points::list(influx_data_point())|influx_data_point(),
    Tags::influx_data_point()
  ) -> ok.
write(Pool, Series, Points, Tags) ->
  gen_server:call(
    ?SERVER,
    {send_to_pool, Pool, {write, Series, Points, Tags}}
  ).

init_server() ->
  erlang:register(?SERVER, self()),
  proc_lib:init_ack({ok, self()}),

  %% Start default pool
  Defaults = #{
    port => ?Config(influx_port, 4444),
    host => ?Config(influx_host, '127.0.0.1'),
    pool_size => ?Config(pool_size, 3),
    max_overflow => ?Config(max_overflow, 1)
  },

  UseDefault = ?Config(default_pool, true),

  if UseDefault
    ->  start_pool_(default, Defaults);
    true -> ok
  end,

  gen_server:enter_loop(?SERVER, [], #state{defaults = Defaults}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
handle_call(
  {start_pool, Name, Options},
  _,
  #state{defaults = Defaults} = State
) ->
  {reply, start_pool_(Name, maps:merge(Defaults, Options)), State};

handle_call({send_to_pool, Pool, Msg}, _, State) ->
  poolboy:transaction(Pool,
    fun(Worker) ->
        gen_server:call(Worker, Msg)
    end
  ),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec start_pool_(
  Name::atom(),
  Options::map()
) -> {ok, Pid::pid()} | {error, Reason::any()}.
start_pool_(Name, #{ host := Hostname } = Options) ->
  case inet:getaddr(Hostname, inet) of
    {ok, Host} -> 
      influx_udp_sup:start_pool(
        Name,
        maps:update(host, Host, Options)
      );
    {error, Error} ->
      ?E({<<"Failed to resolve influxdb host">>, Hostname, Error}),
      {error, Error}
  end.
