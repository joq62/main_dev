%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 18 Jan 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_control).
-define(LogDir,glurk).
-define(LogFileName,glurk).

%% API
-export([
	 is_wanted_state/0,
	 create_appl/1,
	 create_infra_appl/2,
	 create_pods_based_appl/1,
	 ensure_right_cookie/1,
	 start_parents/0,
	 start_pods/0,
	 start_appls/0,
	 start_infra_appls/1,
	 start_user_appls/0 
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_wanted_state()->
    ParentState=case rpc:call(node(),parent_server,stopped_nodes,[],10*1000) of
		    {ok,[]}->
			true;
		    _->
			false
		end,
    PodState=case rpc:call(node(),pod_server,stopped_nodes,[],25*1000)  of
		    {ok,[]}->
			true;
		    _->
			false
		end,
    ApplState=case rpc:call(node(),appl_server,stopped_appls,[],15*1000)  of
		 {ok,[]}->
		     true;
		 _->
		     false
		end,
    ParentState and PodState and ApplState. 
    

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_parents()->
    Result=case rpc:call(node(),parent_server,stopped_nodes,[],20*1000) of
	       {ok,[]}->
		   ok;
	       {ok,StoppedParents}->
		%   sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"StoppedParents ",[StoppedParents]]),
		   CreateResult=[{rpc:call(node(),parent_server,create_node,[Parent],25*1000),Parent}||Parent<-StoppedParents],
		   [sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to create node for Pod ",[Reason,Pod]])||
		       {{error,Reason},Pod}<-CreateResult],
		   [sd:cast(log,log,notice,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Succeeded to Creating pod ",[Pod]])||
		       {ok,Pod}<-CreateResult],
		   case rpc:call(node(),parent_server,active_nodes,[],20*1000) of
		       {ok,ActiveParents}->
			   _R1=[{net_adm:ping(Pod1),rpc:call(Pod1,net_adm,ping,[Pod2],5000)}||Pod1<-ActiveParents,
											      Pod2<-ActiveParents,
											      Pod1/=Pod2],
			   {ok,ActiveParents};
		       
		       Reason->
			    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get active parents, stoppedParents ",[StoppedParents,Reason]]),
			   {error,Reason}
		   end;
	       Reason->
		   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get  stopped Parents ",[Reason]]),
		   {error,Reason}
	       end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_pods()->
    Result=case rpc:call(node(),pod_server,stopped_nodes,[],25*1000) of
	       {ok,[]}->
		   ok;
	       {ok,Stopped}->
		%   sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Stopped Pods ",[Stopped]]),
		   CreateResult=[{rpc:call(node(),pod_server,create_node,[Pod],25*1000),Pod}||Pod<-Stopped],
		   [sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to create node for Pod ",[Reason,Pod]])||
		       {{error,Reason},Pod}<-CreateResult],
		   [sd:cast(log,log,notice,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Succeeded to Creating pod ",[Pod]])||
		       {ok,Pod}<-CreateResult],
		    
		   _CommonStart=[{rpc:call(node(),appl_server,create_appl,["common",Pod],25*1000),Pod}||{ok,Pod}<-CreateResult],
		   _SdStart=[{rpc:call(node(),appl_server,create_appl,["sd",Pod],25*1000),Pod}||{ok,Pod}<-CreateResult],
		   case rpc:call(node(),pod_server,active_nodes,[],15*1000) of
		       {ok,Active}->
			   _R1=[{net_adm:ping(Pod1),rpc:call(Pod1,net_adm,ping,[Pod2],5000)}||Pod1<-Active,
											      Pod2<-Active,
											      Pod1/=Pod2],
			   {ok,Active,Stopped};
		       Reason->
			   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get active parents, stopped ",[Stopped,Reason]]),
			   {error,Reason}
		   end;
	       Reason->
		   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get  stopped pods ",[Reason]]),
		   {error,Reason}
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_appls()->
    Result=case rpc:call(node(),appl_server,stopped_appls,[],60*1000) of
	       {ok,[]}->
		   ok;
	       {ok,StoppedApplInfoLists}->	
		 %  sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"StoppedApplInfoLists ",[StoppedApplInfoLists]]),		  
		   ApplCreateResult=create_appl(StoppedApplInfoLists,[]),
		   [ sd:cast(log,log,notice,[?MODULE,?FUNCTION_NAME,?LINE,node(),"application  start",[CreateResult]])||
		       CreateResult<-ApplCreateResult],
		   ApplCreateResult;
	       Reason->
		   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get  stopped pods ",[Reason]]),
		   {error,["appl_server,stopped_appls ",Reason,?MODULE,?LINE]}
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_infra_appls(ClusterSpec)->   
    sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Used but candidate to be removed ",[]]),
    Result=case rpc:call(node(),appl_server,stopped_appls,[],30*1000) of
	       {ok,[]}->
		  ok;
	      {ok,StoppedApplInfoLists}->
		  R_Nodelog=[{create_infra_appl({PodNode,ApplSpec,App},ClusterSpec),ApplSpec,PodNode}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
												       nodelog==App],
		  [sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ResultCreate nodelog :",ResultCreate,?MODULE,?LINE]])||
		      ResultCreate<- R_Nodelog],
		  
		  R_etcd=[{create_infra_appl({PodNode,ApplSpec,App},ClusterSpec),ApplSpec,PodNode}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
												       etcd==App],
		  [sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ResultCreate etcd :",ResultCreate,?MODULE,?LINE]])||
		      ResultCreate<-R_etcd],

		  R_control=[{create_infra_appl({PodNode,ApplSpec,App},ClusterSpec),ApplSpec,PodNode}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
													     control==App],
		  [sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ResultCreate control :",ResultCreate,?MODULE,?LINE]])||
		      ResultCreate<-R_control],
		  [{nodelog,R_Nodelog},{etcd,R_etcd},{control,R_control}];
	      Reason->
		  {error,[Reason,?MODULE,?LINE]}
	  end,
    Result.


		  

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_pods_based_appl(ApplSpec)->
    sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Used but candidate to be removed ",[]]),
    Result=case sd:call(etcd,db_pod_desired_state,get_all_id,[],60*1000) of
	       {badrpc,Reason}->
		   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["db_pod_desired_state,get_all_id : ",badrpc,Reason]]),
		   {error,[badrpc,Reason,?MODULE,?LINE]};
	       []->
		   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["db_pod_desired_state,get_all_id : ",no_pods]]),
		   {error,[no_pods,?MODULE,?LINE]};
	       Pods->
		   io:format("DBG:  Pods ~p~n",[{Pods,?MODULE,?LINE}]), 
		   AllPodsApplSpecsToStart=[{PodNode,sd:call(etcd,db_pod_desired_state,read,[appl_spec_list,PodNode],5*1000)}||PodNode<-Pods,
																  pang==net_adm:ping(PodNode)],
		   io:format("DBG:  AllPodsApplSpecsToStart ~p~n",[{AllPodsApplSpecsToStart,?MODULE,?LINE}]), 
		   PodsToStart=[PodNode||{PodNode,{ok,ApplSpecList}}<-AllPodsApplSpecsToStart,
					 lists:member(ApplSpec,ApplSpecList)],
		   io:format("DBG:  PodsToStart ~p~n",[{PodsToStart,?MODULE,?LINE}]), 
		   [{create_pod(PodNode),PodNode,ApplSpec}||PodNode<-PodsToStart]
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_pod(PodNode)->
    sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Used but candidate to be removed ",[]]),
    Result=case sd:call(etcd,db_pod_desired_state,read,[parent_node,PodNode],5000) of
	       {ok,ParentNode}->
		   case sd:call(etcd,db_pod_desired_state,read,[node_name,PodNode],5000) of
		       {ok,NodeName}->
			   case sd:call(etcd,db_pod_desired_state,read,[pod_dir,PodNode],5000) of
			       {ok,PodDir}->
				   case sd:call(etcd,db_pod_desired_state,read,[pa_args_list,PodNode],5000) of
				       {ok,PaArgsList}->
					   case sd:call(etcd,db_pod_desired_state,read,[env_args,PodNode],5000) of
					       {ok,EnvArgs}->
						   rpc:call(node(),pod_server,create_pod,[ParentNode,NodeName,PodDir,PaArgsList,EnvArgs],25*1000);
					       Reason ->
						   {error,[Reason,env_args,?MODULE,?LINE]}
					   end;
				       Reason ->
					   {error,[Reason,pa_args_list,?MODULE,?LINE]}
				   end;
			       Reason ->
				   {error,[Reason,pod_dir,?MODULE,?LINE]}
			   end;
		       Reason ->
			   {error,[Reason,node_name,?MODULE,?LINE]}
		   end;
	       Reason ->
		   {error,[Reason,parent_node,?MODULE,?LINE]}
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_infra_appl({PodNode,ApplSpec,nodelog},_ClusterSpec)->
    sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Used but candidate to be removed ",[]]),
    sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Used but candidate to be removed ",[]]),
    Result= case create_appl([{PodNode,ApplSpec,nodelog}]) of
		{error,Reason}->
		    {error,Reason};
		[{ok,PodNode,ApplSpec,nodelog}]->
		    case sd:call(etcd,db_pod_desired_state,read,[pod_dir,PodNode],5000) of
			{badrpc,Reason}->
			    {error,[badrpc,Reason]};
			{error,Reason}->
			    {error,Reason};
			{ok,PodDir}->
			    PathLogDir=filename:join(PodDir,?LogDir),
			    rpc:call(PodNode,file,del_dir_r,[PathLogDir],5000),
			    case rpc:call(PodNode,file,make_dir,[PathLogDir],5000) of
				{Error,Reason}->
				    {Error,Reason};
				ok->
				    PathLogFile=filename:join([PathLogDir,?LogFileName]),
				    case rpc:call(PodNode,nodelog,config,[PathLogFile],5000) of
					{Error,Reason}->
					    {Error,Reason};
					ok->	
					    ok
				    end
			    end
		    end
	    end,
    Result;
    
 
create_infra_appl({PodNode,ApplSpec,etcd},_ClusterSpec) ->
    sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Used but candidate to be removed ",[]]),
    Result= case create_appl([{PodNode,ApplSpec,etcd}]) of
		{error,Reason}->
		    {error,Reason};
		[{ok,PodNode,ApplSpec,etcd}]->
		    ok
	    end,
    Result;
create_infra_appl({PodNode,ApplSpec,control},ClusterSpec) ->
    sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Used but candidate to be removed ",[]]),
    rpc:call(PodNode,application,set_env,[[{control,[{cluster_spec,ClusterSpec}]}]],5000),
    Result= case create_appl([{PodNode,ApplSpec,control}]) of
		{error,Reason}->
		    {error,Reason};
		[{ok,PodNode,ApplSpec,control}]->
		    ok
	    end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_user_appls()->
    sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Used but candidate to be removed ",[]]),
    Result=case rpc:call(node(),appl_server,stopped_appls,[],15*1000) of
	       {ok,[]}->
		   ok;
	       {ok,StoppedApplInfoLists}->			  
		   StoppedUserApplications=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
								    common/=App,
								    sd/=App,
								    etcd/=App,
								    nodelog/=App,
								    control/=App],
		   ApplCreateResult=create_appl(StoppedUserApplications,[]),
		  % [sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["Create start_user_appls Result :", CreateResult,?MODULE,?LINE]])||
		  %     CreateResult<-ApplCreateResult];
		   ApplCreateResult;
	       Reason->
		   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error Create start_user_appls Result :", Reason,?MODULE,?LINE]]),
		   {error,["appl_server,stopped_appls ",Reason,?MODULE,?LINE]}
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
ensure_right_cookie(ClusterSpec)->
    Result=case sd:call(etcd,db_cluster_spec,read,[cookie,ClusterSpec],5000) of
	       {ok,Cookie}->
		   erlang:set_cookie(node(),list_to_atom(Cookie));
	       Reason->
		   {error,["etcd,db_cluster_spec,read,[cookie, ",Reason,?MODULE,?LINE]}
	   end,
    Result.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_appl(ApplSpecInfoList)->
    create_appl(ApplSpecInfoList,[]).
create_appl([],Acc)->
    Acc;
create_appl([{PodNode,ApplSpec,App}|T],Acc)->
    Result=rpc:call(node(),appl_server,create_appl,[ApplSpec,PodNode],25*1000),
%    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG PResult :", Result,?MODULE,?LINE]]),
    create_appl(T,[{Result,PodNode,ApplSpec,App}|Acc]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
%%%===================================================================
%%% Internal functions
%%%===================================================================
