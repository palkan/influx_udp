-module(influx_udp_test).
-include_lib("influx_udp/include/influx_udp_priv.hrl").
-include_lib("influx_udp/include/influx_udp.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun setup_/0, fun cleanup_/1, F}).

setup_() ->
  lager:start(),
  {ok, UDP1} = test_udp_server:start(4444),
  {ok, UDP2} = test_udp_server:start(4455),
  influx_udp:start(),
  {ok, Pid} = influx_udp:start_pool(test, #{ pool_size => 1, port => 4455}),
  {UDP1, UDP2}.

cleanup_({UDP1, UDP2}) ->
  application:stop(lager),
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


write_point_t_({UDP1, _UDP2}) ->
  influx_udp:write(<<"test">>),
  timer:sleep(1),
  [
    ?_assertEqual(<<"test">>, gen_server:call(UDP1, msg))
  ].

write_point_to_pool_t_({_UDP1, UDP2}) ->
  influx_udp:write_to(test, <<"test_pool">>),
  timer:sleep(1),
  [
    ?_assertEqual(<<"test_pool">>, gen_server:call(UDP2, msg))
  ].
