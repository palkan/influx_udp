-module(influx_udp_test).
-include_lib("influx_udp/include/influx_udp_priv.hrl").
-include_lib("influx_udp/include/influx_udp.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun setup_/0, fun cleanup_/1, F}).

setup_() ->
  lager:start(),
  test_udp_app:start([], []),
  influx_udp:start(),
  {ok, Pid} = influx_udp:start_pool(test, #{ pool_size => 1}).

cleanup_(_) ->
  application:stop(lager),
  influx_udp:stop(),
  test_udp_app:stop([]).

write_point_test_() ->
  [{"Write point",
    ?setup(
      fun(_) ->
        {inorder,
          [
            write_point_t_()
          ]
        }
      end
    )
  }].

write_point_pool_test_() ->
  [{"Write point to named pool",
    ?setup(
      fun(_) ->
        {inorder,
          [
            write_point_to_pool_t_()
          ]
        }
      end
    )
  }].


write_point_t_() ->
  influx_udp:write(<<"test">>),
  timer:sleep(1),
  [
    ?_assertEqual(<<"test">>, gen_server:call(test_udp_server, msg))
  ].

write_point_to_pool_t_() ->
  influx_udp:write(test, <<"test_pool">>),
  timer:sleep(1),
  [
    ?_assertEqual(<<"test_pool">>, gen_server:call(test_udp_server, msg))
  ].
