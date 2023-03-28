%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_appl).
 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports
-export([
	 create_appl/2,
	 active_appls/1,
	 stopped_appls/1

	]).


%% ====================================================================
%% External functions
%% ====================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
active_appls(ClusterSpec)->
    Result= case sd:call(etcd,db_pod_desired_state,pods,[ClusterSpec],5000) of
		{error,Reason}->
		    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get pods",[ClusterSpec,Reason]]),
		    {error,Reason};
		AllNodes->
		   A1=[{PodNode,sd:call(etcd,db_pod_desired_state,read,[appl_spec_list,PodNode],5000)}||PodNode<-AllNodes],
		   A2=[{PodNode,ApplList}||{PodNode,{ok,ApplList}}<-A1],
		   PodApplSpecAppList=lists:append([pod_app_list(PodApplSpecList,[])||PodApplSpecList<-A2]),
		   case stopped_appls(ClusterSpec) of
		       {error,Reason}->
			   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get stopped appls",[ClusterSpec,Reason]]),
			   {error,Reason};
		       {ok,StoppedAppls}->
			   ActiveAppls=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-PodApplSpecAppList,
								false==lists:member({PodNode,ApplSpec,App},StoppedAppls)],
			   {ok,ActiveAppls}
		   end
	   end,
    Result.
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stopped_appls(ClusterSpec)->
    Result=case pod_server:active_nodes() of
	       {ok,ActiveNodes}->
		   A1=[{PodNode,sd:call(etcd,db_pod_desired_state,read,[appl_spec_list,PodNode],5000)}||PodNode<-ActiveNodes],
		   A2=[{PodNode,ApplList}||{PodNode,{ok,ApplList}}<-A1],
		   PodApplSpecAppList=lists:append([pod_app_list(PodApplSpecList,[])||PodApplSpecList<-A2]),
		   StoppedAppls=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-PodApplSpecAppList,
					  false==is_app_running(App,PodNode)],
		   {ok,StoppedAppls}
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_app_running(App,PodNode)->
    Result=case rpc:call(PodNode,application,which_applications,[],5000) of
	       {badrpc,_}->
		   false;
	       Applications->
		   lists:keymember(App,1,Applications)
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
pod_app_list({_PodNode,[]},Acc)->
    Acc;
pod_app_list({PodNode,[ApplSpec|T]},Acc)->
   case sd:call(etcd,db_appl_spec,read,[app,ApplSpec],5000) of
       {ok,App}->
	   NewAcc=[{PodNode,ApplSpec,App}|Acc];
       Reason->
	   NewAcc=[{error,[Reason,PodNode,ApplSpec]}|Acc]
   end,
    pod_app_list({PodNode,T},NewAcc).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-define(TimeOut,20*1000).
create_appl(ApplSpec,PodNode)->
    create_appl(ApplSpec,PodNode,?TimeOut).

create_appl(ApplSpec,PodNode,TimeOut)->
    Result=case sd:call(etcd,db_pod_desired_state,read,[pod_dir,PodNode],5000) of
	       {error,Reason}->
		   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get pod dir at pod",[PodNode,Reason]]),
		   {error,Reason};
	       {ok,PodDir}->
		   ApplDir=filename:join(PodDir,ApplSpec),
		   rpc:call(PodNode,file,del_dir_r,[ApplDir],5000),
		   case rpc:call(PodNode,file,make_dir,[ApplDir],5000) of
		      {badrpc,Reason}->
			   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to make ApplDir  at pod",[ApplDir,PodNode,Reason]]),
			  {error,["Error during do_start : ",badrpc,Reason,ApplSpec,PodNode,?MODULE,?FUNCTION_NAME,?LINE]};
		      {error,Reason}->
			   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to make ApplDir  at pod",[ApplDir,PodNode,Reason]]),
			  {error,Reason};
		      ok->
			   case sd:call(etcd,db_pod_desired_state,read,[host_spec,PodNode],5000) of
			        {error,Reason}->
				   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get hostname  at pod",[PodNode,Reason]]),
				  {error,Reason};
			      {ok,HostSpec}->
				%  sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG ok HostSpec :",HostSpec,?MODULE,?LINE]]),
				  case sd:call(etcd,db_host_spec,read,[application_config,HostSpec],5000) of
				      {error,Reason}->
					  sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get application_config in host spec",[HostSpec,Reason]]),
					  {error,Reason};
				      {ok,ApplicationConfig}->
					  SetEnvResult=[rpc:call(PodNode,application,set_env,[[Config]],5000)||Config<-ApplicationConfig],
					  case sd:call(etcd,db_appl_spec,read,[gitpath,ApplSpec],5000) of 
					      {error,Reason}->
						  sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get gitpath in ApplSpec ",[ApplSpec,Reason]]),
						  {error,["Error during do_start : ",Reason,ApplSpec,PodNode,?MODULE,?FUNCTION_NAME,?LINE]};
					       {ok,PodApplGitPath}->
						  case do_load_start(ApplSpec,PodNode,PodApplGitPath,ApplDir,TimeOut) of
						      {error,Reason}->
							  sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to start  ",[ApplSpec,PodNode,PodApplGitPath,ApplDir,Reason]]),
							  {error,["Error during do_start : ",Reason,ApplSpec,PodNode,?MODULE,?FUNCTION_NAME,?LINE]};
						      ok->
							  ok
						  end
					  end
				  end
			  end
		  end
	  end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
do_load_start(ApplSpec,PodNode,PodApplGitPath,ApplDir,TimeOut)->
 %   sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG ApplSpec,PodNode,PodApplGitPath,ApplDir :", 
%							      ApplSpec,PodNode,PodApplGitPath,ApplDir,?MODULE,?LINE]]),
    Result= case appl:git_clone_to_dir(PodNode,PodApplGitPath,ApplDir) of
		{error,Reason}->
		    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to git_clone_to_dir ",[PodNode,PodApplGitPath,ApplDir,Reason]]),
		    {error,["Error when cloning : ", Reason,PodNode,PodApplGitPath,ApplDir,?MODULE,?FUNCTION_NAME,?LINE]};
		{ok,CloneDir}->
		    case sd:call(etcd,db_appl_spec,read,[app,ApplSpec],5000) of
			{error,Reason}->
			    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to app  ",[ApplSpec,Reason]]),
			    {error,["Error when cloning : ", Reason,?MODULE,?FUNCTION_NAME,?LINE]};
			{ok,PodApp}->
			    ApplEbin=filename:join([ApplDir,"ebin"]),
			    Paths=[ApplEbin],
			    case appl:load(PodNode,PodApp,Paths) of
				{error,Reason}->
				    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to load app  ",[PodNode,PodApp,Paths,Reason]]),
				    {error,["Error when loading application : ",Reason,PodNode,PodApp,Paths,?MODULE,?FUNCTION_NAME,?LINE]}; 
				ok->
				    case appl:start(PodNode,PodApp,TimeOut) of
					{error,Reason}->
					    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to start app  ",[PodNode,PodApp,Reason]]),
					    {error,Reason};
					ok->
					    ok;
					Error ->
					    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to start app  ",[PodNode,PodApp,Error]]),
					    {error,Error}
				    end
			    end

		    end
	    end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get_candidate_pods(SpecId,HostSpec,ClusterSpec)->
    {ok,ApplSpec}=sd:call(etcd,db_appl_deployment,read,[appl_spec,SpecId],5000),
    RightHost=[PodNode||PodNode<-sd:call(etcd,db_pod_desired_state,get_all_id,[],5000),
			{ok,HostSpec}==sd:call(etcd,db_pod_desired_state,read,[host_spec,PodNode],5000)],
    NodeApplSpecList=[{PodNode,sd:call(etcd,db_pod_desired_state,read,[appl_spec_list,PodNode],5000)}||PodNode<-RightHost],
    Candidates=[PodNode||{PodNode,{ok,ApplSpecList}}<-NodeApplSpecList,
			 false==lists:member(ApplSpec,ApplSpecList),
			 {ok,ClusterSpec}==sd:call(etcd,db_pod_desired_state,read,[cluster_spec,PodNode],5000)],
    prioritize(Candidates,[]).

prioritize([],Acc)->
    [PodNode||{_NumApplSpecs,PodNode}<-lists:keysort(1,Acc)];
prioritize([PodNode|T],Acc) ->
    {ok,ApplSpecList}=sd:call(etcd,db_pod_desired_state,read,[appl_spec_list,PodNode],5000),
    NumApplSpecs=list_length:start(ApplSpecList),
    prioritize(T,[{NumApplSpecs,PodNode}|Acc]).
 
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
load_desired_state(ClusterSpec)->
    {ok,PodsHostList}=sd:call(etcd,db_cluster_spec,read,[pods,ClusterSpec],5000),
    HostSpecList=[HostSpec||{_Num,HostSpec}<-PodsHostList],
    AllDeploymentId=sd:call(etcd,db_appl_deployment,get_all_id,[],5000),
    ApplDeploymentSpecInfoList=[sd:call(etcd,db_appl_deployment,read,[ApplDeploymentId],5000)||ApplDeploymentId<-AllDeploymentId,
			       {ok,ClusterSpec}==sd:call(etcd,db_appl_deployment,read,[cluster_spec,ApplDeploymentId],5000)],
    Result=main_load_desired_state(ApplDeploymentSpecInfoList,HostSpecList,[]),
    ok.

main_load_desired_state([],_HostSpecList,Acc)->
    Acc;
main_load_desired_state([ApplDeploymentSpec|T],HostSpecList,Acc) ->
    Result=load_desired_state(ApplDeploymentSpec,HostSpecList,[]),
    main_load_desired_state(T,HostSpecList,[Result|Acc]).

%%-- Affinity on each pod each_pod
load_desired_state({_SpecId,_ApplSpec,_Vsn,_ClusterSpec,_,each_pod},[],Acc)->
    Acc;
load_desired_state({SpecId,ApplSpec,_Vsn,ClusterSpec,1,each_pod},[HostSpec|T],Acc)->
    RightHost=[PodNode||PodNode<-sd:call(etcd,db_pod_desired_state,get_all_id,[],5000),
			{ok,HostSpec}==sd:call(etcd,db_pod_desired_state,read,[host_spec,PodNode],5000)],
    NodeApplSpecList=[{PodNode,sd:call(etcd,db_pod_desired_state,read,[appl_spec_list,PodNode],5000)}||PodNode<-RightHost],
    Candidates=[PodNode||{PodNode,{ok,ApplSpecList}}<-NodeApplSpecList,
			 false==lists:member(ApplSpec,ApplSpecList),
			 {ok,ClusterSpec}==sd:call(etcd,db_pod_desired_state,read,[cluster_spec,PodNode],5000)],
    AddResult=[{sd:call(etcd,db_pod_desired_state,add_appl_list,[ApplSpec,PodNode],5000),ApplSpec,PodNode,HostSpec}||PodNode<-Candidates],
    ErrorResult=[{error,["ERROR: Aborted ApplSpec,PodNode,HostSpec : ",Reason,ApplSpec,PodNode,HostSpec]}||
		    {{aborted,Reason},ApplSpec,PodNode,HostSpec}<-AddResult],
    OkResult=[{ok,ApplSpec,PodNode,HostSpec}||{{atomic,ok},ApplSpec,PodNode,HostSpec}<-AddResult],
    NewAcc=lists:append([OkResult,ErrorResult,Acc]),
    load_desired_state({SpecId,ApplSpec,_Vsn,ClusterSpec,1,each_pod},T,NewAcc);


%%-- Affinity any_host    
load_desired_state({_SpecId,_ApplSpec,_Vsn,_ClusterSpec,0,any_host},_HostList,Acc)->
    Acc;
load_desired_state({SpecId,ApplSpec,_Vsn,ClusterSpec,N,any_host},[HostSpec|T],Acc)->
    Result=case get_candidate_pods(SpecId,HostSpec,ClusterSpec) of
	       []->
		   {error,["ERROR: No candidates  ",ApplSpec,HostSpec]};
	       [PodNode|_]->
		   case sd:call(etcd,db_pod_desired_state,add_appl_list,[ApplSpec,PodNode],5000) of
		       {aborted,Reason}->
			   {error,["ERROR: Aborted ApplSpec,PodNode,HostSpec : ",Reason,ApplSpec,PodNode,HostSpec]};
		       {atomic,ok}->
			%   io:format("Ok  ~p~n",[{ApplSpec,PodNode,?MODULE,?FUNCTION_NAME}]),
			   ok
		   end
	   end,
    RotatedHostList=lists:append(T,[HostSpec]),
    load_desired_state({SpecId,ApplSpec,_Vsn,ClusterSpec,N-1,any_host},RotatedHostList,[Result|Acc]);
		       
load_desired_state({_SpecId,_ApplSpec,_Vsn,_ClusterSpec,0,_Affinity},_HostList,Acc)->
    Acc;
load_desired_state({SpecId,ApplSpec,_Vsn,ClusterSpec,N,[HostSpec|T]},HostList,Acc)->
    Result=case lists:member(HostSpec,HostList) of
	       false->
		   {error,["ERROR: Host not part of cluster hosts : ",HostSpec,HostList]};
	       true->
		   case get_candidate_pods(SpecId,HostSpec,ClusterSpec) of
		       []->
			   {error,["ERROR: No candidates  ",ApplSpec,HostSpec]};
		       [PodNode|_]->				   
			   case sd:call(etcd,db_pod_desired_state,add_appl_list,[ApplSpec,PodNode],5000) of
			       {aborted,Reason}->
				   {error,["ERROR: Aborted ApplSpec,PodNode,HostSpec : ",Reason,ApplSpec,PodNode,HostSpec]};
			       {atomic,ok}->
				   ok
			   end
		   end
	   end,
    RotatedWantedHostList=lists:append(T,[HostSpec]),
    load_desired_state({SpecId,ApplSpec,_Vsn,ClusterSpec,N-1,RotatedWantedHostList},HostList,[Result|Acc]).
