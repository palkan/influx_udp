%%% @doc
%%% InfluxDB Line protocol encoder.
%%% @end
-module(influx_line).
-include_lib("influx_udp/include/influx_udp_priv.hrl").
-include_lib("influx_udp/include/influx_udp.hrl").

-define(Q, <<"\"">>).
-define(SPACE, <<" ">>).
-define(NEW_LINE, <<"\n">>).
-define(EMPTY, <<"">>).
-define(COMMA, <<",">>).
-define(EQ, <<"=">>).

-export([
  encode/1,
  encode/2,
  encode/3,
  encode/4,
  encode_tags/1,
  encode_fields/1,
  timestamp/0
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc
%% Encode InfluxDB data point (or list of points) to line.
%% Point should contain 'measurement' and 'fields' keys (as atoms).
%% Other available keys: 'tags', 'time'.
%% Return binary line(-s) or `{error, invalid_data}' if any point contains
%% neither 'measurement' nor 'fields' key.
%%
%% Example:
%%
%% ```
%%   influx_line:encode(
%%     #{
%%       measurement => cpu,
%%       fields => #{ value => 43},
%%       tags => #{ host => 'eu-west', ip => "127.0.0.1" },
%%       time => 10000000000
%%     }
%%   ).
%%   <<"cpu,host=eu-west,ip="127.0.0.1" value=43 10000000000\n">>
%% '''
%% @end
-spec encode(Points::influx_data_points()) -> Line::binary()|{error, Reason::atom()}.
encode(#{} = Data) ->
  encode_with_time(Data, undefined);

encode([{_, _}|_] = Data) ->
  encode_with_time(Data, undefined);

encode(List) when is_list(List) ->
  ?D(encode_list),
  encode_points_list(undefined, List, timestamp(), []);

encode(_Other) ->
  ?D({unknown_data, _Other}),
  {error, invalid_data}.

%% @doc
%% Encode measurement and fields (or list of fields) to line.
%% @end
-spec encode(
  Name::string()|atom()|binary(),
  Fields::influx_data_points()
) -> Line::binary() | {error, Reason::atom()}.
encode(Name, Fields) ->
  encode(Name, Fields, [], undefined).

%% @doc
%% Encode measurement, fields (or list of fields) and tags to line.
%% @end
-spec encode(
  Name::string()|atom()|binary(),
  Fields::influx_data_points(),
  Tags::influx_data_point()|binary()
) -> Line::binary() | {error, Reason::atom()}.
encode(Name, Fields, Tags) ->
  encode(Name, Fields, Tags, undefined).

%% @doc
%% Encode measurement, fields (or list of fields), tags and time to line.
%% If Time is integer then it's used as point time.
%% If Time is `true' then `inlux_line:timestamp()' is used as point time.
%% If there are several points, then every point is set a uniq time
%% (by incrementing provided time).
%%
%% Note: if there are several points and no time specified then current time
%% is used as base time.
%%
%% Example:
%%
%% ```
%%   %% several points (uniq times)
%%   influx_line:encode(
%%     cpu,
%%     [
%%       #{ value => 43 },
%%       #{ value => 12 }
%%     ],
%%     #{ host => 'eu-west', ip => "127.0.0.1" },
%%     1000000000
%%   ).
%%   <<"cpu,host=eu-west,ip="127.0.0.1" value=43 1000000000\n
%%      cpu,host=eu-west,ip="127.0.0.1" value=12 1000000001\n">>
%%
%%   %% one point without time
%%   influx_line:encode(
%%     cpu,
%%     [
%%       #{ value => 43 },
%%       #{ value => 12 }
%%     ],
%%     #{ host => 'eu-west', ip => "127.0.0.1" }
%%   ).
%%   <<"cpu,host=eu-west,ip="127.0.0.1" value=43\n">>
%% '''
%% @end
-spec encode(
  Name::string()|atom()|binary(),
  Points::list(influx_data_point())|influx_data_point(),
  Tags::influx_data_point()|binary(),
  Time::non_neg_integer()|atom()
) -> Line::binary() | {error, Reason::atom()}.
encode(Name, #{} = Map, Tags, Time) ->
  encode_item(Name, Map, Tags, Time);

encode(Name, [{_, _}|_] = List, Tags, Time) ->
  encode_item(Name, List, Tags, Time);

encode(Name, [[{_, _}|_]|_] = Points, Tags, Time) ->
  encode_list(Name, Points, Tags, Time);

encode(Name, [#{}|_] = Points, Tags, Time) ->
  encode_list(Name, Points, Tags, Time);

encode(_, _, _, _) -> {error, invalid_data}.

%% @doc
%% Returns current UTC time in nanoseconds
%% @end
-spec timestamp() -> Time::non_neg_integer().
timestamp() ->
  ulitos:timestamp()*1000000.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
encode_item(Name, Item, Tags, Time) ->
  BTags = encode_tags(Tags),
  Measurement = to_key(Name),
  Prefix = <<Measurement/binary, BTags/binary>>,
  encode_item_with_prefix(Prefix, Item, Time).

encode_item_with_prefix(Prefix, Item, Time) ->
  BTime = encode_time(Time),
  Fields = encode_fields(Item),
  concat_line(Prefix, Fields, BTime).

encode_list(Name, [Item], Tags, Time) ->
  encode_item(Name, Item, Tags, Time);

encode_list(Name, List, Tags, Time) when is_integer(Time) ->
  BTags = encode_tags(Tags),
  Measurement = to_key(Name),
  Prefix = <<Measurement/binary, BTags/binary>>,
  encode_list_with_time(undefined, List, Prefix, Time, []);

encode_list(Name, List, Tags, _Time) ->
  encode_list(Name, List, Tags, influx_line:timestamp()).

encode_list_with_time({error, _} = Error, _, _, _, _) ->
  Error;

encode_list_with_time(Item, [], _, _, Acc) ->
  lists:foldl(fun concat_lines/2, ?EMPTY, [Item|Acc]);

encode_list_with_time(undefined, [H|Rest], Prefix, Time, Acc) ->
  encode_list_with_time(
    encode_item_with_prefix(Prefix, H, Time),
    Rest,
    Prefix,
    Time + 1,
    Acc
  );

encode_list_with_time(Item, [H|Rest], Prefix, Time, Acc) ->
  encode_list_with_time(
    encode_item_with_prefix(Prefix, H, Time),
    Rest,
    Prefix,
    Time + 1,
    [Item|Acc]
  ).

encode_points_list({error, _} = Error, _, _, _) ->
  Error;

encode_points_list(Item, [], _, Acc) ->
  lists:foldl(fun concat_lines/2, ?EMPTY, [Item|Acc]);

encode_points_list(undefined, [H|Rest], Time, Acc) ->
  encode_points_list(
    encode_with_time(H, Time),
    Rest,
    Time + 1,
    Acc
  );

encode_points_list(Item, [H|Rest], Time, Acc) ->
  encode_points_list(
    encode_with_time(H, Time),
    Rest,
    Time + 1,
    [Item|Acc]
  ).

encode_with_time([{_, _}|_] = List, BaseTime) ->
  M = proplists:get_value(measurement, List, undefined),
  F = proplists:get_value(fields, List, undefined),
  if M =:= undefined orelse F =:= undefined
    -> {error, invalid_data};
    true ->
      Time = encode_time(
        proplists:get_value(time, List, BaseTime)
      ),
      Tags = encode_tags(
        proplists:get_value(tags, List, [])
      ),
      Measurement = to_key(M),
      Fields = encode_fields(F),
      concat_line(Measurement, Tags, Fields, Time)
  end;

encode_with_time(#{measurement := M, fields := F} = Map, BaseTime) ->
  Time = encode_time(
    maps:get(time, Map, BaseTime)
  ),
  Tags = encode_tags(
    maps:get(tags, Map, [])
  ),
  Measurement = to_key(M),
  Fields = encode_fields(F),
  concat_line(Measurement, Tags, Fields, Time);

encode_with_time(_, _) -> {error, invalid_data}.

encode_time(undefined) ->
  ?EMPTY;

encode_time(true) ->
  encode_time(timestamp());

encode_time(N) ->
  to_time_val(N).

encode_tags(Tags) when is_binary(Tags) ->
  Tags;

encode_tags(#{} = Map) ->
  remove_trailing_comma(
    maps:fold(fun encode_map_tags/3, ?COMMA, Map)
  );

encode_tags(List) when is_list(List) ->
  remove_trailing_comma(
    lists:foldl(fun encode_list_tags/2, ?COMMA, List)
  );

encode_tags(_) -> ?SPACE.

encode_fields(#{} = Map) ->
  remove_trailing_comma(
    maps:fold(fun encode_map_fields/3, ?SPACE, Map)
  );

encode_fields(List) when is_list(List) ->
  remove_trailing_comma(
    lists:foldl(fun encode_list_fields/2, ?SPACE, List)
  );

encode_fields(_) -> {error, invalid_data}.

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
  B = to_tag_val(V),
  <<?SPACE/binary, B/binary>>.

escape_string(Tag) ->
  binary:replace(Tag,
    [?COMMA, ?Q, ?SPACE, ?EQ],
    <<"\\">>,
    [global, {insert_replaced, 1}]
  ).

concat_line(A, B, C) ->
  <<A/binary, B/binary, C/binary>>.

concat_line(A, B, C, D) ->
  <<A/binary, B/binary, C/binary, D/binary>>.

concat_lines(Acc, B) ->
  concat_line(Acc, ?NEW_LINE, B).

remove_trailing_comma(?EMPTY) -> ?EMPTY;

remove_trailing_comma(B) -> binary:part(B, {0, byte_size(B) - 1}).

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


encode_time_test() ->
  ?assertEqual(<<" 123">>, encode_time(123)),
  ?assertEqual(<<"">>, encode_time(undefined)).

encode_2_proplist_point_test() ->
  ?assertEqual(
    <<"test 1=1">>,
    encode(test, [{1, 1}])
  ).

encode_2_many_proplist_points_test() ->
  ?assertEqual(
    <<"test 1=1 1\ntest 1=3 2\n">>,
    encode("test", [ [{<<"1">>, 1}], [{<<"1">>, 3}] ], [], 1)
  ).

encode_2_map_point_test() ->
  ?assertEqual(
    <<"test 1=1">>,
    encode(<<"test">>, #{1 => 1})
  ).

encode_2_many_map_points_test() ->
  ?assertEqual(
    <<"test 1=1 1\ntest 1=3 2\n">>,
    encode(<<"test">>, [#{'1' => 1}, #{'1' => 3}], [], 1)
  ).

encode_3_invalid_test() ->
  ?assertEqual(
    {error, invalid_data},
    encode(<<"test">>, 'bla-bla', [])
  ).

encode_map_point_test() ->
  ?assertEqual(
    <<"test,host=eu-west,ip=1.1.1.1 val=10,text=\"hello\" 123">>,
    encode(
      #{
        measurement => <<"test">>,
        fields => [{"val", 10}, {text, <<"hello">>}],
        tags => [{host, "eu-west"}, {ip, '1.1.1.1'}],
        time => 123
      }
    )
  ).

encode_proplist_point_test() ->
  ?assertEqual(
    <<"test,host=eu-west,ip=1.1.1.1 val=10,text=\"hello\" 123">>,
    encode(
      [
        {measurement, <<"test">>},
        {fields, [{"val", 10}, {text, <<"hello">>}]},
        {tags, #{host => "eu-west", ip => '1.1.1.1'}},
        {time, 123}
      ]
    )
  ).

encode_map_points_test() ->
  ?assertEqual(
    <<"test val=10 1\ntest2 val=20 2\n">>,
    encode(
      [
        #{
          measurement => <<"test">>,
          fields => #{"val" => 10},
          time => "1"
        },
        #{
          measurement => <<"test2">>,
          fields => #{"val" => 20},
          time => <<"2">>
        }
      ]
    )
  ).

encode_proplist_points_test() ->
  ?assertEqual(
    <<"test val=10 1\ntest2 val=20 2\n">>,
    encode(
      [
        [
          {measurement, <<"test">>},
          {fields, #{"val" => 10}},
          {time, 1}
        ],
        [
          {measurement, <<"test2">>},
          {fields, #{"val" => 20}},
          {time, 2}
        ]
      ]
    )
  ).

encode_map_without_measurement_test() ->
  ?assertEqual(
    {error, invalid_data},
    encode(
      #{
        fields => #{ val => 10}
      }
    )
  ).

encode_map_without_fields_test() ->
  ?assertEqual(
    {error, invalid_data},
    encode(
      #{
        measurement => test,
        tags => #{ val => 10}
      }
    )
  ).

encode_proplist_without_measurement_test() ->
  ?assertEqual(
    {error, invalid_data},
    encode(
      [
        {fields,  [{val, 10}] }
      ]
    )
  ).

encode_proplist_without_fields_test() ->
  ?assertEqual(
    {error, invalid_data},
    encode(
      [
        {measurement, test},
        {tags, #{ val => 10}}
      ]
    )
  ).

encode_at_least_one_invalid_test() ->
  ?assertEqual(
    {error, invalid_data},
    encode(
      [
        [
          {measurement, test},
          {fields, #{ val => 10}}
        ],
        [
          {tags, #{ val => 10}}
        ]
      ]
    )
  ).

encode_3_with_proplist_tags_test() ->
  ?assertEqual(
    <<"test,host=eu-west,ip=1.1.1.1 1=1">>,
    encode(<<"test">>, #{1 => 1}, [{host, "eu-west"}, {ip, '1.1.1.1'}])
  ).

encode_3_with_map_tags_test() ->
  ?assertEqual(
    <<"test,host=eu-west,ip=1.1.1.1 1=1">>,
    encode(<<"test">>, #{1 => 1}, #{host => 'eu-west', ip => <<"1.1.1.1">>})
  ).

encode_3_with_binary_tags_test() ->
  ?assertEqual(
    <<"test,host=eu-west,ip=1.1.1.1 1=1">>,
    encode(<<"test">>, #{1 => 1}, <<",host=eu-west,ip=1.1.1.1">>)
  ).

encode_3_many_points_with_tags_test() ->
  ?assertEqual(
    <<"test,host=eu-west,num=111 memory=\"high\",cpu=20 100\ntest,host=eu-west,num=111 memory=\"low\",cpu=30 101\n">>,
    encode(
      <<"test">>,
      [
        [{memory, "high"}, {"cpu", 20}],
        [{<<"memory">>, "low"}, {cpu, 30}]
      ],
      #{host => 'eu-west', num => 111},
      100
    )
  ).

encode_4_test() ->
  ?assertEqual(
    <<"test,host=eu-west,ip=1.1.1.1 1=1 123123">>,
    encode(<<"test">>, #{1 => 1}, [{host, "eu-west"}, {ip, '1.1.1.1'}], 123123)
  ).

encode_4_many_points_test() ->
  ?assertEqual(
    <<"test,host=eu-west,ip=1.1.1.1 1=1,text=\"hello\" 123123">>,
    encode(<<"test">>,
      [[{"1", 1}, {text, <<"hello">>}]],
      [{host, "eu-west"}, {ip, '1.1.1.1'}],
      123123
    )
  ).

encode_escape_tags_test() ->
  ?assertEqual(
    <<"test,host=eu\\ we\\=st,ip=1\\,1\\,1\\,1 1=1">>,
    encode(<<"test">>,
      [[{"1", 1}]],
      [{host, "eu we=st"}, {ip, '1,1,1,1'}]
    )
  ).

encode_escape_fields_test() ->
  ?assertEqual(
    <<"test,host=eu-west 1=1,text=\"hello\\ \\\"man\\=\\=human\\,\\ yo!\\\"\"">>,
    encode(<<"test">>,
      [[{"1", 1}, {text, <<"hello \"man==human, yo!\"">>}]],
      [{host, 'eu-west'}]
    )
  ).

-endif.