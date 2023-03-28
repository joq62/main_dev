%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 18 Jan 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(orchestrate).

-define(SleepInterval,60*1000).
%% API
-export([
	 start/2,
	 start/3
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start(ClusterSpec,LeaderPid)->
    start(ClusterSpec,LeaderPid,?SleepInterval).

start(ClusterSpec,LeaderPid,SleepInterval)->
  %  sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"start orchestrate  ",[node()]]),
    timer:sleep(SleepInterval),
    Result=case leader:am_i_leader(LeaderPid,node(),5000) of
	       false->
	%	   sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"am_i_leader",[false,node()]]),
		   [ok,ok,ok,ok];
	       true->
%		   sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"am_i_leader",[true,node()]]),
		   orchistrate(ClusterSpec,SleepInterval)
	   end,
%    sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"end orchestrate  ",[node()]]),
    rpc:cast(node(),control,orchestrate_result,Result).


orchistrate(_ClusterSpec,_SleepInterval)->
 %   sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"start orchestrate ",[node()]]),
    ResultStartParents=rpc:call(node(),lib_control,start_parents,[],15*1000),
 %   sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"ResultStartParents ",[ResultStartParents]]),

    ResultStartPods=rpc:call(node(),lib_control,start_pods,[],60*1000),
 %   sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"ResultStartPods ",[ResultStartPods]]),

    ResultStartUserAppls=rpc:call(node(),lib_control,start_appls,[],60*1000), 
 %   sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"ResultStartUserAppls ",[ResultStartUserAppls]]),

    ResultStartInfraAppls=ok, %% Shall be removed

    [ResultStartParents,ResultStartPods,ResultStartInfraAppls,ResultStartUserAppls].
    
   

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
