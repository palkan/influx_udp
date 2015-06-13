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

-record(state,{
  socket ::gen_udp:socket()|undefined,
  port ::inet:port_number()|undefined,
  host ::inet:ip_address()|inet:ip_hostname()|undefined,
  local_port ::inet:port_number()|undefined
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
  {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
  LocalPort = inet:port(Socket),
  ?I({"Open UDP socket on port", LocalPort}),
  {ok, #state{socket = Socket, host = Host, port = Port, local_port = LocalPort}}.

handle_call({write, Bin}, _From, State) when is_binary(Bin) ->
  {reply, send_data(State,Bin), State};

handle_call({write, Series, Points}, _From, State) ->
  {reply, send_data(State, encode(Series, Points)), State};

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

send_data(_, false) -> {error, wrong_data};

send_data(#state{socket=Socket, port=Port, host=Host}, Data) ->
  gen_udp:send(Socket, Host, Port, Data).


%% @doc
%% Convert data to JSON (binary) of a type:
%% [
%%    {
%%        "name": "my_series",
%%        "columns": ["ip", "name"],
%%        "points": [
%%            ["127.0.0.1", "John"],
%%            ["192.168.0.1", "Paul"]
%%        ]
%%    }
%% ]
%% @end
-spec encode(Name::string()|atom()|binary(), Points::list(influx_data_point())|influx_data_point()) -> JSON::binary().

encode(Name, Points) when is_list(Name) ->
  encode(to_key(Name), Points);

encode(Name, Points) ->
  case encode_points(Points) of
    {[], _} -> false;
    {Columns, Values} -> jsx:encode([#{name => Name, columns => normalize_columns(Columns), points => Values}])
  end.

%% Handle proplist
encode_points([{_, _}|_] = Data) ->
  {_, Keys} = extract_columns(Data),
  {Keys, [encode_item(list, Keys, Data)]};

encode_points([H|_] = List) when is_list(List) -> 
  {Type, Keys} = extract_columns(H),
  {Keys, encode_list(Type, Keys, List)};

encode_points(Item) ->
  encode_points([Item]).

encode_list(Type, Keys, List) ->
  [encode_item(Type, Keys, Item) || Item <- List].  

encode_item(map, Keys, M) -> 
  [to_val(maps:get(K, M, null)) || K <- Keys];

encode_item(list, Keys, L) -> 
  [to_val(proplists:get_value(K, L, null)) || K <- Keys];

encode_item(_,_,_) -> false.

to_key(S) when is_list(S) ->
  list_to_binary(S);

to_key(N) when is_integer(N) ->
  integer_to_binary(N);

to_key(S) -> S.

to_val(S) when is_list(S) ->
  list_to_binary(S);

to_val(S) -> S.

extract_columns(M) when is_map(M) ->
  {map, maps:keys(M)};

extract_columns(Prop) when is_list(Prop) ->
  {list, proplists:get_keys(Prop)};

extract_columns(_) -> {false, []}.

normalize_columns(Columns) ->
  [to_key(C) || C <- Columns].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_series([#{<<"name">> := Series}]) -> Series.

get_columns([#{<<"columns">> := Columns}]) -> Columns.

get_points([#{<<"points">> := Points}]) -> Points.

decode_data(Data) -> jsx:decode(Data, [return_maps]).

encode_1_test() ->
  Data = decode_data(encode(test, [ [{1,1}], [{1,3}] ])),
  ?assertEqual(<<"test">>, get_series(Data)),
  ?assertEqual([<<"1">>], get_columns(Data)),
  ?assertEqual([[1],[3]], get_points(Data)).

encode_2_test() ->
  Data = decode_data(encode("test", [ [{<<"1">>,1}], [{<<"1">>,3}] ])),
  ?assertEqual(<<"test">>, get_series(Data)),
  ?assertEqual([<<"1">>], get_columns(Data)),
  ?assertEqual([[1],[3]], get_points(Data)).

encode_3_test() ->
  Data = decode_data(encode(<<"test">>, [#{'1' => 1}, #{'1' => 3}])),
  ?assertEqual(<<"test">>, get_series(Data)),
  ?assertEqual([<<"1">>], get_columns(Data)),
  ?assertEqual([[1],[3]], get_points(Data)).

encode_4_test() ->
  Data = decode_data(encode(<<"test">>, #{1 => 1})),
  ?assertEqual(<<"test">>, get_series(Data)),
  ?assertEqual([<<"1">>], get_columns(Data)),
  ?assertEqual([[1]], get_points(Data)).

encode_5_test() ->
  ?assertNot(encode(<<"test">>, 'bla-bla')).
-endif.