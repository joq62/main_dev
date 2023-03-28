%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 18 Jan 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(local).


-define(LogDir,"log_dir").
-define(LogFileName,"file.logs").
-define(SleepInterval,60*1000).
%% API
-export([
	 start_local/1,
	 start_initial/1

	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_local(ClusterSpec)->
    ok=start_local_appls(ClusterSpec),
    ok=initiate_local_dbase(ClusterSpec),
    true=lib_infra_service:ensure_right_cookie(ClusterSpec),

    ok.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%-------------------------------------------------------------------
start_initial(ClusterSpec)->
    {ok,ActiveParents}=lib_infra_service:start_parents(),
     
    [{ok,NodelogPod,NodelogApplSpec}]=lib_infra_service:create_pods_based_appl("nodelog"),
    [{ok,_,_,_}]=lib_infra_service:create_appl([{NodelogPod,"common",common}]),
    [{ok,_,_,_}]=lib_infra_service:create_appl([{NodelogPod,"sd",sd}]),
    ok=lib_infra_service:create_infra_appl({NodelogPod,"nodelog",nodelog},ClusterSpec),
  
    [{ok,DbPod,DbApplSpec}]=lib_infra_service:create_pods_based_appl("db_etcd"),
    [{ok,_,_,_}]=lib_infra_service:create_appl([{DbPod,"common",common}]),
    [{ok,_,_,_}]=lib_infra_service:create_appl([{DbPod,"sd",sd}]),
    ok=lib_infra_service:create_infra_appl({DbPod,"db_etcd",db_etcd},ClusterSpec),
    application:stop(db_etcd),
    
    ok=parent_server:load_desired_state(ClusterSpec),
    ok=pod_server:load_desired_state(ClusterSpec),
    ok=appl_server:load_desired_state(ClusterSpec),

    [{ok,InfraPod,InfraApplSpec}]=lib_infra_service:create_pods_based_appl("infra_service"),
    [{ok,_,_,_}]=lib_infra_service:create_appl([{InfraPod,"common",common}]),
    [{ok,_,_,_}]=lib_infra_service:create_appl([{InfraPod,"sd",sd}]),
    ok=lib_infra_service:create_infra_appl({InfraPod,"infra_service",infra_service},ClusterSpec),
    true=rpc:cast(InfraPod,infra_service,start_orchistrate,[]),
    
     application:stop(infra_service),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_local_appls(ClusterSpec)->
    ok=application:start(common),
    ok=application:start(sd),
    ok=application:start(db_etcd),
    pong=db_etcd:ping(),
    ok=db_etcd:config(),
    
    ok=application:start(infra_service),
    pong=parent_server:ping(),
    pong=pod_server:ping(),
    pong=appl_server:ping(),
    

    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
initiate_local_dbase(ClusterSpec)->
    ok=parent_server:load_desired_state(ClusterSpec),
    ok=pod_server:load_desired_state(ClusterSpec),
    ok=appl_server:load_desired_state(ClusterSpec),	
    ok.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
init_servers(ClusterSpec)->
    Result=case rpc:call(node(),parent_server,load_desired_state,[ClusterSpec],30*10000) of
	       ok ->
		   case rpc:call(node(),pod_server,load_desired_state,[ClusterSpec],30*1000) of
		       ok ->
			   case rpc:call(node(),appl_server,load_desired_state,[ClusterSpec],30*1000) of
			       ok ->
				   ok;
			       Reason->
				   sd:cast(nodelog,nodelog,log,
					   [warning,?MODULE_STRING,?LINE,["appl_server:load_desired_state  : ",Reason,?MODULE,?LINE]]),
				   {error,Reason}
			   end;
		       Reason->
			   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,
							["pod_server:load_desired_state  : ",Reason,?MODULE,?LINE]]),
			   {error,Reason}
		   end;			       
	       Reason->
		   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,
						["parent_server:load_desired_state  : ",Reason,?MODULE,?LINE]]),
		   {error,Reason}
	   end,
    Result.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_pods_based_appl(ApplSpec)->
    AllPodsApplSpecsToStart=[{PodNode,db_pod_desired_state:read(appl_spec_list,PodNode)}||PodNode<-db_pod_desired_state:get_all_id(),
											  pang==net_adm:ping(PodNode)],
    PodsToStart=[PodNode||{PodNode,{ok,ApplSpecList}}<-AllPodsApplSpecsToStart,
			  lists:member(ApplSpec,ApplSpecList)],
 %   io:format("PodsToStart, ApplSpec ~p~n",[{PodsToStart,ApplSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
    [{create_pod(PodNode),PodNode,ApplSpec}||PodNode<-PodsToStart].

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_pod(PodNode)->
    Result=case sd:call(db_etcd,db_pod_desired_state,read,[parent_node,PodNode],5000) of
	       {ok,ParentNode}->
		   case sd:call(db_etcd,db_pod_desired_state,read,[node_name,PodNode],5000) of
		       {ok,NodeName}->
			   case sd:call(db_etcd,db_pod_desired_state,read,[pod_dir,PodNode],5000) of
			       {ok,PodDir}->
				   case sd:call(db_etcd,db_pod_desired_state,read,[pa_args_list,PodNode],5000) of
				       {ok,PaArgsList}->
					   case sd:call(db_etcd,db_pod_desired_state,read,[env_args,PodNode],5000) of
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
    Result= case create_appl([{PodNode,ApplSpec,nodelog}]) of
		{error,Reason}->
		    {error,Reason};
		[{ok,PodNode,ApplSpec,nodelog}]->
		    case sd:call(db_etcd,db_pod_desired_state,read,[pod_dir,PodNode],5000) of
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
    
 
create_infra_appl({PodNode,ApplSpec,db_etcd},_ClusterSpec) ->
    Result= case create_appl([{PodNode,ApplSpec,db_etcd}]) of
		{error,Reason}->
		    {error,Reason};
		[{ok,PodNode,ApplSpec,db_etcd}]->
		    case rpc:call(PodNode,db_etcd,config,[],5000) of
			{Error,Reason}->
			    {Error,Reason};
			ok->
			    ok
		    end
	    end,
    Result;
create_infra_appl({PodNode,ApplSpec,infra_service},ClusterSpec) ->
    
    Result= case create_appl([{PodNode,ApplSpec,infra_service}]) of
		{error,Reason}->
		    {error,Reason};
		[{ok,PodNode,ApplSpec,infra_service}]->
		    case rpc:call(PodNode,infra_service,config,[ClusterSpec],5*5000) of
			{Error,Reason}->
			    {Error,Reason};
			ok->
			    ok
		    end
	    end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_user_appls()->
    Result=case rpc:call(node(),appl_server,stopped_appls,[],5*1000) of
	       {ok,StoppedApplInfoLists}->			  
		   StoppedUserApplications=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
								    common/=App,
								    sd/=App,
								    db_etcd/=App,
								    nodelog/=App,
								    infra_service/=App],
		   ApplCreateResult=create_appl(StoppedUserApplications,[]),
		   [sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["Create Result :", CreateResult,?MODULE,?LINE]])||
		       CreateResult<-ApplCreateResult];
	       Reason->
		   {error,["appl_server,stopped_appls ",Reason,?MODULE,?LINE]}
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
ensure_right_cookie(ClusterSpec)->
    Result=case sd:call(db_etcd,db_cluster_spec,read,[cookie,ClusterSpec],5000) of
	       {ok,Cookie}->
		   erlang:set_cookie(node(),list_to_atom(Cookie));
	       Reason->
		   {error,["db_etcd,db_cluster_spec,read,[cookie, ",Reason,?MODULE,?LINE]}
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
