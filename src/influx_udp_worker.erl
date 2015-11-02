-module(influx_udp_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).
-include_lib("influx_udp/include/influx_udp_priv.hrl").
-include_lib("influx_udp/include/influx_udp.hrl").
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  socket ::gen_udp:socket()|undefined,
  port ::inet:port_number()|undefined,
  host ::inet:ip_address()|inet:ip_hostname()|undefined,
  local_port ::inet:port_number()|undefined,
  debug = false ::boolean()
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([{options, #{ host := Host, port := Port} = _Options}]) ->
  AddrFamily = addr_family(Host),
  {ok, Socket} = gen_udp:open(0, [binary, {active, false}, AddrFamily]),
  LocalPort = inet:port(Socket),
  ?I({"Open UDP socket on port", LocalPort}),
  {
    ok, 
    #state{
      socket = Socket,
      host = Host,
      port = Port,
      local_port = LocalPort,
      debug = ?Config(debug, false)
    }
  }.

handle_call({write, Bin}, _From, State) when is_binary(Bin) ->
  {reply, send_data(State, Bin), State};

handle_call({write, Points}, _From, State) ->
  {reply, send_data(State, influx_line:encode(Points)), State};

handle_call({write, Series, Points}, _From, State) ->
  {reply, send_data(State, influx_line:encode(Series, Points)), State};

handle_call({write, Series, Points, Tags}, _From, State) ->
  {reply, send_data(State, influx_line:encode(Series, Points, Tags)), State};

handle_call({write, Series, Points, Tags, Time}, _From, State) ->
  {reply, send_data(State, influx_line:encode(Series, Points, Tags, Time)), State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{socket=Socket}) ->
  gen_udp:close(Socket),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

addr_family({_, _, _, _}) -> inet;
addr_family({_, _, _, _, _, _, _, _}) -> inet6.

send_data(_, {error, _Reason} = Error) ->
  ?E(Error),
   Error;

send_data(#state{socket=Socket, port=Port, host=Host, debug = Debug}, Data) ->
  debug_send(Data, Debug),
  gen_udp:send(Socket, Host, Port, Data).

debug_send(_, false) -> ok;

debug_send(Bin, _) ->
  ?D({send_binary_data, byte_size(Bin), Bin}).
