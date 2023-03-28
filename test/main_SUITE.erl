-module(main_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [setup].


init_per_suite(Config) ->
   % application:start(main),
   % pong = main:ping(),
    Config.

end_per_suite(_Config) ->
 %   pong = main:ping(),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
setup(_Config)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ?assertMatch(ok,application:start(common)),
    ?assertMatch(pong,common:ping()),
    ?assertMatch(ok,application:start(sd)),
    ?assertMatch(pong,sd:ping()),
    ?assertMatch(ok,application:start(main)),
    ?assertMatch(pong,main:ping()),
    ok.
                                
