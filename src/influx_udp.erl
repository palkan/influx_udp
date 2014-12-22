-module(influx_udp).
-behaviour(gen_server).
-behaviour(application).
-include_lib("influx_udp/include/influx_udp_priv.hrl").
-include_lib("influx_udp/include/influx_udp.hrl").
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start/0, start/2, stop/1]).

-export([start_link/0]).

-export([
  write/1,
  write/2
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
  start([],[]).

start(_StartType, _StartArgs) ->
  influx_udp_sup:start_link().

stop(_State) ->
    ok.

%% @doc
%% Write binary data to influx.
%% Note: Assumed that data represents Influx-valid JSON.
%% @end

-spec write(Bin::binary()) -> ok.

write(Bin) ->
  gen_server:call(?SERVER, {send_to_pool, {write, Bin}}).

%% @doc
%% Write data points to Series.
%% @end

-spec write(Series::atom()|string()|binary(), Points::list(influx_data_point())|influx_data_point()) -> ok.
write(Series, Points) ->
  gen_server:call(?SERVER, {send_to_pool, {write, Series, Points}}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  self() ! start_pool,
  {ok, []}.

handle_call({send_to_pool, Msg}, _, State) ->
  poolboy:transaction(?POOL_NAME, 
    fun(Worker) ->
        gen_server:call(Worker, Msg)
    end
  ),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(start_pool, State) ->
  Port = ?Config(influx_port, 4444),
  Hostname = ?Config(influx_host, '127.0.0.1'),
  
  case inet:getaddr(Hostname, inet) of
    {ok, Host} -> 
      influx_udp_sup:start_pool(Host, Port),
      {noreply, State};
    {error, Error} ->
      ?E({<<"Failed to resolve influxdb host">>, Error}),
      {stop, Error, State}
  end;

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

