-module(test_udp_server).
-behaviour(gen_server).
-include_lib("influx_udp/include/influx_udp_priv.hrl").
-include_lib("influx_udp/include/influx_udp.hrl").
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,{
  sock,
  msg
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  {ok, Socket} = gen_udp:open(4444, [binary, {active, once}]),
  ?D({test_server_start_listening}),
  {ok, #state{sock=Socket}}.

handle_call(msg, _, #state{msg=Msg}=State) ->
  {reply, Msg, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({udp, Socket, _Host, _Port, Bin}, #state{sock=Sock}=State) ->
  ?D({message_received, Bin}),
  inet:setopts(Socket, [{active, once}]),
  {noreply, State#state{msg=Bin}};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{sock=Sock}) ->
  gen_udp:close(Sock),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

