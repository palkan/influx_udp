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
  write/4,
  write_to/2,
  write_to/3,
  write_to/4,
  write_to/5
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
%% Write binary data or map/proplist point(-s) to influx using default pool.
%% Note: Assumed that data represents InfluxDB-valid input.
%% @end
-spec write(Data::binary()|influx_data_points()) -> ok.
write(Data) ->
  write_to(default, Data).

%% @doc
%% Write data points to Series using default pool.
%% @end
- spec write(
  Series::atom()|string()|binary(),
  Points::list(influx_data_point())|influx_data_point()
) -> ok.
write(Series, Points) ->
  write_to(default, Series, Points).

%% @doc
%% Write data points with Tags to Series using default pool.
%% @end
-spec write(
    Series::atom()|string()|binary(),
    Points::list(influx_data_point())|influx_data_point(),
    Tags::influx_data_point()
  ) -> ok.
write(Series, Points, Tags) ->
  write_to(default, Series, Points, Tags).

%% @doc
%% Write data points with Tags and Time to Series using default pool.
%% @end
-spec write(
    Series::atom()|string()|binary(),
    Points::list(influx_data_point())|influx_data_point(),
    Tags::influx_data_point(),
    Time::non_neg_integer()|atom()
  ) -> ok.
write(Series, Points, Tags, Time) ->
  write_to(default, Series, Points, Tags, Time).


%% @doc
%% Write binary data or map/proplist point(-s) to influx using named pool.
%% @end
-spec write_to(Pool::atom(), Data::binary()|influx_data_points()) -> ok.
write_to(Pool, Data) ->
  gen_server:call(?SERVER, {send_to_pool, Pool, {write, Data}}).

%% @doc
%% Write data points to Series using named pool.
%% @end
-spec write_to(
    Pool::atom(),
    Series::atom()|string()|binary(),
    Points::list(influx_data_point())|influx_data_point()
  ) -> ok.
write_to(Pool, Series, Points) ->
  gen_server:call(
    ?SERVER,
    {send_to_pool, Pool, {write, Series, Points}}
  ).


%% @doc
%% Write data points with Tags to Series using named pool.
%% @end
-spec write_to(
    Pool::atom(),
    Series::atom()|string()|binary(),
    Points::list(influx_data_point())|influx_data_point(),
    Tags::influx_data_point()
  ) -> ok.
write_to(Pool, Series, Points, Tags) ->
  gen_server:call(
    ?SERVER,
    {send_to_pool, Pool, {write, Series, Points, Tags}}
  ).

%% @doc
%% Write data points with Tags and Time to Series using named pool.
%% @end
-spec write_to(
    Pool::atom(),
    Series::atom()|string()|binary(),
    Points::list(influx_data_point())|influx_data_point(),
    Tags::influx_data_point(),
    Time::non_neg_integer()|atom()
  ) -> ok.
write_to(Pool, Series, Points, Tags, Time) ->
  gen_server:call(
    ?SERVER,
    {send_to_pool, Pool, {write, Series, Points, Tags, Time}}
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
Addr =
  case inet:parse_address(Hostname) of
    {ok, _Addr} = Res -> Res;
    {error, einval} ->
      case inet:getaddr(Hostname, inet6) of
       {error, _} -> inet:getaddr(Hostname, inet);
       {ok, _Addr} = Res -> Res
      end
  end,

  case Addr of
    {ok, Host} ->
      influx_udp_sup:start_pool(
        Name,
        maps:update(host, Host, Options)
      );
    {error, Error} ->
      ?E({<<"Failed to resolve influxdb host">>, Hostname, Error}),
      {error, Error}
  end.
