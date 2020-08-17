-module(influx_udp_test).
-include_lib("influx_udp/include/influx_udp_priv.hrl").
-include_lib("influx_udp/include/influx_udp.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun setup_/0, fun cleanup_/1, F}).

%% The default influx_udp port
-define(PORT, 8089).
-define(PORT2, 44515).

setup_() ->
  {ok, UDP1} = test_udp_server:start(?PORT),
  {ok, UDP2} = test_udp_server:start(?PORT2),

  %% Set default configuration
  ulitos_app:set_var(?APP, influx_host, '127.0.0.1'),

  influx_udp:start(),
  {ok, _Pid} = influx_udp:start_pool(test, #{ pool_size => 1, port => ?PORT2}),
  {UDP1, UDP2}.

cleanup_({UDP1, UDP2}) ->
  influx_udp:stop(),
  test_udp_server:stop(UDP1),
  test_udp_server:stop(UDP2).

write_point_test_() ->
  [{"Write point",
    ?setup(
      fun(Config) ->
        {inorder,
          [
            write_point_t_(Config)
          ]
        }
      end
    )
  }].

write_point_pool_test_() ->
  [{"Write point to named pool",
    ?setup(
      fun(Config) ->
        {inorder,
          [
            write_point_to_pool_t_(Config)
          ]
        }
      end
    )
  }].

write_influx_point_test_() ->
  [{"Write point to named pool",
    ?setup(
      fun(Config) ->
        {inorder,
          [
            write_influx_point_t_(Config)
          ]
        }
      end
    )
  }].


write_point_t_({UDP1, _UDP2}) ->
  influx_udp:write(test, [{val, 1}], [{host, test}], 100),
  timer:sleep(1),
  [
    ?_assertEqual(<<"test,host=test val=1 100">>, gen_server:call(UDP1, msg))
  ].

write_point_to_pool_t_({_UDP1, UDP2}) ->
  influx_udp:write_to(test, <<"test_pool">>),
  timer:sleep(1),
  [
    ?_assertEqual(<<"test_pool">>, gen_server:call(UDP2, msg))
  ].

write_influx_point_t_({UDP1, _UDP2}) ->
  influx_udp:write([
    #{ measurement => <<"test">>, fields => #{ val => 1}, time => 1},
    #{ measurement => <<"test2">>, fields => #{ val => 2}, time => 2}
  ]),
  timer:sleep(1),
  [
    ?_assertEqual(<<"test val=1 1\ntest2 val=2 2\n">>, gen_server:call(UDP1, msg))
  ].
