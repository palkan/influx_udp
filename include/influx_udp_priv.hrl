-define(APP, influx_udp).
-define(Config(X,Y), ulitos_app:get_var(?APP, X, Y)).


%% log functions

-ifdef(OLD_LOGGER).

-ifdef(TEST).

-define(D(X), error_logger:info_msg("[DEBUG] ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-define(I(X), error_logger:info_msg("[INFO] ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-define(E(X), error_logger:info_msg("[ERROR] ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-else.

-define(D(X), begin _ = X end).
-define(I(X), error_logger:info_msg("~p:~p ~p",[?MODULE, ?LINE, X])).
-define(E(X), error_logger:error_msg("~p:~p ~p",[?MODULE, ?LINE, X])).

-endif.

-else. % Use Erlang/OTP 21+ Logger.

-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).

-define(D(X), ?LOG_INFO("[DEBUG] ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-define(I(X), ?LOG_INFO("[INFO] ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-define(E(X), ?LOG_INFO("[ERROR] ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-else.

-define(D(X), ?LOG_DEBUG(X)).
-define(I(X), ?LOG_INFO(X)).
-define(E(X), ?LOG_ERROR(X)).

-endif.

-endif.
