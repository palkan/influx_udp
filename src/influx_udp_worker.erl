-module(influx_udp_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).
-include_lib("influx_udp/include/influx_udp_priv.hrl").
-include_lib("influx_udp/include/influx_udp.hrl").
-define(SERVER, ?MODULE).
-define(Q, <<"\"">>).
-define(SPACE, <<" ">>).
-define(NEW_LINE, <<"\n">>).
-define(EMPTY, <<"">>).
-define(COMMA, <<",">>).
-define(EQ, <<"=">>).

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
  {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
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

handle_call({write, Series, Points}, _From, State) ->
  {reply, send_data(State, encode(Series, Points, [])), State};

handle_call({write, Series, Points, Tags}, _From, State) ->
  {reply, send_data(State, encode(Series, Points, Tags)), State};

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

send_data(#state{socket=Socket, port=Port, host=Host, debug = Debug}, Data) ->
  debug_send(Data, Debug),
  gen_udp:send(Socket, Host, Port, Data).


%% @doc
%% Convert data to line protocol:
%% my_series,host=eu-west,ip="127.0.0.1" value=43 10000000000
%% my_series,host=eu-west,ip="192.168.0.1" value=54 10000000000
%% @end
-spec encode(
  Name::string()|atom()|binary(),
  Points::list(influx_data_point())|influx_data_point(),
  Tags::proplists:proplist()|map()
) -> Line::binary().
encode(Name, Points, Tags) ->
  BTags = remove_trailing_comma(
    extract_tags(Tags)
  ),
  BName = to_key(Name),
  Prefix = <<BName/binary, BTags/binary>>,
  case encode_points(Points) of
    [] -> false;
    Values -> 
      lists:foldl(
        fun(V, Acc) ->
          <<Acc/binary, Prefix/binary, ?SPACE/binary, V/binary, ?NEW_LINE/binary>>
        end,
        ?EMPTY,
        Values   
      )
  end.


%% @doc
%% Return fields and time part of point as binary
%% @end

%% Handle proplist
encode_points([{_, _}|_] = Data) ->
  [encode_item(list, Data)];

encode_points(#{} = Map) ->
  [encode_item(map, Map)];

encode_points([#{}|_] = List)->
  [encode_item(map, Item) || Item <- List];

encode_points([[{_, _}|_]|_] = List)->
  [encode_item(list, Item) || Item <- List];

encode_points(_) -> [].

encode_item(map, M) ->
  {Data, Time} = extract_time(map, M),
  Fields = remove_trailing_comma(
    maps:fold(fun encode_map_fields/3, ?EMPTY, Data)
  ),
  <<Fields/binary, Time/binary>>;

encode_item(list, P) ->
  {Data, Time} = extract_time(list, P),
  Fields = remove_trailing_comma(
    lists:foldl(fun encode_list_fields/2, ?EMPTY, Data)
  ), 
  <<Fields/binary, Time/binary>>.

extract_tags([]) -> ?EMPTY;

extract_tags(M) when is_map(M) ->
  maps:fold(fun encode_map_tags/3, ?COMMA, M);

extract_tags(Prop) when is_list(Prop) ->
  lists:foldl(fun encode_list_tags/2, ?COMMA, Prop);

extract_tags(_) -> <<"">>.

encode_map_fields(K, V, Acc) ->
  BK = to_key(K),
  BV = to_val(V),
  <<Acc/binary, BK/binary, ?EQ/binary, BV/binary, ?COMMA/binary>>.

encode_list_fields({K, V}, Acc) ->
  encode_map_fields(K, V, Acc).

encode_map_tags(K, V, Acc) ->
  BK = to_key(K),
  BV = to_tag_val(V),
  <<Acc/binary, BK/binary, ?EQ/binary, BV/binary, ?COMMA/binary>>.

encode_list_tags({K, V}, Acc) ->
  encode_map_tags(K, V, Acc).

extract_time(map, #{ time := Time } = M) ->
  {maps:remove(time, M), to_time_val(Time)};

extract_time(map, #{ "time" := Time } = M) ->
  {maps:remove("time", M), to_time_val(Time)};

extract_time(map, #{ <<"time">> := Time } = M) ->
  {maps:remove(<<"time">>, M), to_time_val(Time)};

extract_time(list, List) ->
  NewList = proplists:substitute_aliases(
    [{"time", time}, {<<"time">>, time}], List
  ),
  case proplists:get_value(time, NewList) of
    undefined -> {List, ?EMPTY};
    Time -> {
      proplists:delete(time, NewList),
      to_time_val(Time)
    }
  end;

extract_time(_, Data) -> {Data, ?EMPTY}.  

to_key(A) when is_atom(A) ->
  to_key(atom_to_list(A));

to_key(S) when is_list(S) ->
  list_to_binary(S);

to_key(N) when is_integer(N) ->
  integer_to_binary(N);

to_key(S) -> S.

to_val(true) ->
  <<"true">>;

to_val(false) ->
  <<"false">>;

to_val(N) when is_integer(N) ->
  integer_to_binary(N);

to_val(N) when is_float(N) ->
  float_to_binary(N);

to_val(A) when is_atom(A) ->
  to_val(atom_to_list(A));

to_val(S) when is_list(S) ->
  to_val(list_to_binary(S));

to_val(S) when is_binary(S) -> 
  Escaped = escape_string(S),
  <<?Q/binary, Escaped/binary, ?Q/binary>>.

to_tag_val(V) when is_integer(V) ->
  to_val(V);

to_tag_val(V) when is_float(V) ->
  to_val(V);

to_tag_val(V) when is_atom(V) ->
  to_tag_val(atom_to_list(V));

to_tag_val(V) when is_list(V) ->
  to_tag_val(list_to_binary(V));

to_tag_val(V) when is_binary(V) ->
  escape_string(V).

to_time_val(V) ->
  B = to_val(V),
  <<?SPACE/binary, B/binary>>.

escape_string(Tag) ->
  binary:replace(Tag,
    [?COMMA, ?Q, ?SPACE, ?EQ],
    <<"\\">>,
    [global, {insert_replaced, 1}]
  ).

remove_trailing_comma(?EMPTY) -> ?EMPTY;

remove_trailing_comma(B) -> binary:part(B, {0, byte_size(B) - 1}).

debug_send(_, false) -> ok;

debug_send(Bin, _) ->
  ?D({send_binary_data, byte_size(Bin), Bin}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

to_key_test() ->
  ?assertEqual(<<"test">>, to_key(test)),
  ?assertEqual(<<"test">>, to_key("test")),
  ?assertEqual(<<"test">>, to_key(<<"test">>)),
  ?assertEqual(<<"123">>, to_key(123)).

to_val_test() ->
  ?assertEqual(<<"\"test\"">>, to_val(test)),
  ?assertEqual(<<"\"test\"">>, to_val("test")),
  ?assertEqual(<<"\"test\"">>, to_val(<<"test">>)),
  ?assertEqual(<<"true">>, to_val(true)),
  ?assertEqual(<<"false">>, to_val(false)),
  ?assertEqual(<<"\"te\\ st\"">>, to_val(<<"te st">>)),
  ?assertEqual(<<"\"te\\,st\"">>, to_val(<<"te,st">>)),
  ?assertEqual(<<"\"te\\\"st\"">>, to_val(<<"te\"st">>)),
  ?assertEqual(<<"\"t\\ e\\\"s\\,t\\\"\"">>, to_val(<<"t e\"s,t\"">>)),
  ?assertEqual(<<"123">>, to_val(123)).

to_tag_val_test() ->
  ?assertEqual(<<"test">>, to_tag_val(test)),
  ?assertEqual(<<"test">>, to_tag_val("test")),
  ?assertEqual(<<"test">>, to_tag_val(<<"test">>)),
  ?assertEqual(<<"true">>, to_tag_val(true)),
  ?assertEqual(<<"false">>, to_tag_val(false)),
  ?assertEqual(<<"te\\ st">>, to_tag_val(<<"te st">>)),
  ?assertEqual(<<"te\\,st">>, to_tag_val(<<"te,st">>)),
  ?assertEqual(<<"te\\\"st">>, to_tag_val(<<"te\"st">>)),
  ?assertEqual(<<"t\\ e\\\"s\\,t\\\"">>, to_tag_val(<<"t e\"s,t\"">>)),
  ?assertEqual(<<"123">>, to_tag_val(123)).


extract_time_test() ->
  ?assertMatch({#{}, <<" 123">>}, extract_time(map, #{time => 123})),
  ?assertMatch({#{}, <<" 123">>}, extract_time(map, #{"time" => 123})),
  ?assertMatch({#{}, <<" 123">>}, extract_time(map, #{<<"time">> => 123})),
  ?assertMatch({_, <<" 123">>}, extract_time(list, [{time, 123}])),
  ?assertMatch({_, <<" 123">>}, extract_time(list, [{"time", 123}])),
  ?assertMatch({_, <<" 123">>}, extract_time(list, [{<<"time">>, 123}])),
  ?assertMatch({_, <<"">>}, extract_time(list, [{<<"tume">>, 123}])).

encode_1_test() ->
  ?assertEqual(
    <<"test 1=1\ntest 1=3\n">>,
    encode(test, [ [{1, 1}], [{1, 3}] ], [])
  ).

encode_2_test() ->
  ?assertEqual(
    <<"test 1=1\ntest 1=3\n">>,
    encode("test", [ [{<<"1">>, 1}], [{<<"1">>, 3}] ], [])
  ).

encode_3_test() ->
  ?assertEqual(
    <<"test 1=1\ntest 1=3\n">>,
    encode(<<"test">>, [#{'1' => 1}, #{'1' => 3}], [])
  ).

encode_4_test() ->
  ?assertEqual(
    <<"test 1=1\n">>,
    encode(<<"test">>, #{1 => 1}, [])
  ).

encode_5_test() ->
  ?assertNot(encode(<<"test">>, 'bla-bla', [])).

encode_with_tags_1_test() ->
  ?assertEqual(
    <<"test,host=eu-west,ip=1.1.1.1 1=1\n">>,
    encode(<<"test">>, #{1 => 1}, [{host, "eu-west"}, {ip, '1.1.1.1'}])
  ).

encode_with_tags_2_test() ->
  ?assertEqual(
    <<"test,host=eu-west,ip=1.1.1.1 1=1\n">>,
    encode(<<"test">>, #{1 => 1}, #{host => 'eu-west', ip => <<"1.1.1.1">>})
  ).

encode_with_tags_3_test() ->
  ?assertEqual(
    <<"test,host=eu-west,num=111 memory=\"high\",cpu=20\ntest,host=eu-west,num=111 memory=\"low\",cpu=30\n">>,
    encode(
      <<"test">>,
      [
        [{memory, "high"}, {"cpu", 20}],
        [{<<"memory">>, "low"}, {cpu, 30}]
      ], 
      #{host => 'eu-west', num => 111}
    )
  ).

encode_with_time_1_test() ->
  ?assertEqual(
    <<"test,host=eu-west,ip=1.1.1.1 1=1 123123\n">>,
    encode(<<"test">>, #{1 => 1, time => 123123}, [{host, "eu-west"}, {ip, '1.1.1.1'}])
  ).

encode_with_time_2_test() ->
  ?assertEqual(
    <<"test,host=eu-west,ip=1.1.1.1 1=1,text=\"hello\" 123123\n">>,
    encode(<<"test">>,
      [[{"1", 1}, {text, <<"hello">>}, {time, 123123}]],
      [{host, "eu-west"}, {ip, '1.1.1.1'}]
    )
  ).

encode_escape_tags_test() ->
  ?assertEqual(
    <<"test,host=eu\\ we\\=st,ip=1\\,1\\,1\\,1 1=1 123123\n">>,
    encode(<<"test">>,
      [[{"1", 1}, {time, 123123}]],
      [{host, "eu we=st"}, {ip, '1,1,1,1'}]
    )
  ).

encode_escape_fields_test() ->
  ?assertEqual(
    <<"test,host=eu-west 1=1,text=\"hello\\ \\\"man\\=\\=human\\,\\ yo!\\\"\"\n">>,
    encode(<<"test">>,
      [[{"1", 1}, {text, <<"hello \"man==human, yo!\"">>}]],
      [{host, 'eu-west'}]
    )
  ).

-endif.