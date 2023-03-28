%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(appl_server).
 
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(HeartbeatTime,20*1000).
-define(TimeOut,10*1000).

%% External exports


-export([
	 create_appl/2,
	 load_desired_state/1,
	 desired_appls/0,
	 active_appls/0,
	 stopped_appls/0,
	 
	 ping/0
	]).

-export([
	 deploy_appls/1,
	 new/4,
	 new_on_pod/4,
	 load_start/5,
	 delete/3,
	 present_apps/1,
	 missing_apps/1,
	 
	 initiate/1,
	 start_monitoring/1,
	 heartbeat/0,
	 wanted_state/1,
	 restart_appl/2
	]).


-export([
	 start/1,
	 stop/0
	]).


%% gen_server callbacks



-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%-------------------------------------------------------------------
-record(state,{
	      cluster_spec,
	       present,
	       missing
	      }).


%% ====================================================================
%% External functions
%% ====================================================================

	    
%% call
start(ClusterSpec)-> gen_server:start_link({local, ?MODULE}, ?MODULE, [ClusterSpec], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).

%---------------------------------------------------------------------
load_desired_state(ClusterSpec)->
    gen_server:call(?MODULE,{load_desired_state,ClusterSpec},infinity).

create_appl(ApplSpec,PodNode)->
     gen_server:call(?MODULE, {create_appl,ApplSpec,PodNode},infinity).

desired_appls()->
    gen_server:call(?MODULE,{desired_appls},infinity).
active_appls()->
    gen_server:call(?MODULE,{active_appls},infinity).
stopped_appls()->
    gen_server:call(?MODULE,{stopped_appls},infinity).

%---------------------------------------------------------------------
new(ApplSpec,HostSpec,ClusterSpec,TimeOut)->
    gen_server:call(?MODULE, {new,ApplSpec,HostSpec,ClusterSpec,TimeOut},infinity).
new_on_pod(ApplSpec,PodNode,ClusterSpec,TimeOut)->
    gen_server:call(?MODULE, {new_on_pod,ApplSpec,PodNode,ClusterSpec,TimeOut},infinity).

load_start(ApplSpec,PodNode,PodApplGitPath,ApplDir,TimeOut)->
    gen_server:call(?MODULE, {load_start,ApplSpec,PodNode,PodApplGitPath,ApplDir,TimeOut},infinity).

delete(ApplSpec,PodNode,ClusterSpec)->
    gen_server:call(?MODULE, {delete,ApplSpec,PodNode,ClusterSpec},infinity).

deploy_appls(ClusterSpec)->
    gen_server:call(?MODULE, {deploy_appls,ClusterSpec},infinity).

present_apps(ClusterSpec) ->
    gen_server:call(?MODULE, {present_apps,ClusterSpec}).

missing_apps(ClusterSpec) ->
    gen_server:call(?MODULE, {missing_apps,ClusterSpec}).

ping() ->
    gen_server:call(?MODULE, {ping}).

%% cast
start_monitoring(ClusterSpec)->
    gen_server:cast(?MODULE, {start_monitoring,ClusterSpec}).

heartbeat()-> 
    gen_server:cast(?MODULE, {heartbeat}).
initiate(InstanceId)-> 
    gen_server:call(?MODULE, {initiate,InstanceId},infinity).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([ClusterSpec]) -> 
    io:format(" ~p~n",[{ClusterSpec,?MODULE,?LINE}]),
    sd:cast(log,log,notice,[?MODULE,?FUNCTION_NAME,?LINE,node(),"server start",[ClusterSpec]]),
    {ok, #state{
	    cluster_spec=ClusterSpec,
	    present=undefined,
	    missing=undefined}}.   
 

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

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
handle_call({active_appls},_From, State) ->
    Reply=case rpc:call(node(),lib_appl,active_appls,[State#state.cluster_spec],5000) of
	      {ok,Nodes}->
		  {ok,Nodes};
	      Reason->
		  sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get active appls ",[State#state.cluster_spec,Reason]]),
		  {error,Reason}
	  end,
    {reply, Reply, State};
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

handle_call({stopped_appls},_From, State) ->
    Reply=case rpc:call(node(),lib_appl,stopped_appls,[State#state.cluster_spec],20*5000) of
	      {ok,Nodes}->
		  {ok,Nodes};
	      Reason->
		  sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get stopped appls ",[State#state.cluster_spec,Reason]]),
		  {error,Reason}
	  end,
    {reply, Reply, State};
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
handle_call({create_appl,ApplSpec,PodNode},_From, State) ->
    Reply=case rpc:call(node(),lib_appl,create_appl,[ApplSpec,PodNode],20*1000) of
	      ok->
		  sd:cast(log,log,notice,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Succeeded to Create Application on pod node ",[ApplSpec,PodNode]]),
		  sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Succeeded to Create Application on pod node ",[ApplSpec,PodNode]]),
		  ok;
	      Reason->
		  sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to create appl  ",[ApplSpec,PodNode,Reason]]),
		  {error,Reason}
	  end,
    {reply, Reply, State};
    

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
handle_call({ping},_From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(Request, From, State) ->
    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Unmatched signal",[Request,From]]),
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Unmatched signal",[Msg]]),
    io:format("unmatched match cast ~p~n",[{Msg,?MODULE,?LINE}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({ssh_cm,_,{closed,0}}, State) ->
    {noreply, State};

handle_info(Info, State) ->
    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Unmatched signal",[Info]]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
appl_del(ApplSpec,PodNode,ClusterSpec)->
    Result=case sd:call(db_etcd,db_cluster_instance,read,[pod_dir,ClusterSpec,PodNode],5000) of
		    {error,Reason}->
			{error,Reason};
		    {ok,PodDir}->
		   case sd:call(db_etcd,db_appl_spec,read,[app,ApplSpec],5000) of
		       {error,Reason}->
			   {error,Reason};	       
		       {ok,PodApp}->
			   case appl:stop(PodNode,PodApp) of
			       {error,Reason}->
				   {error,Reason};
			       ok->
				   ApplDir=filename:join([PodDir,ApplSpec]),
				   case appl:unload(PodNode,PodApp,ApplDir) of
				       {error,Reason}->
					   {error,Reason};
				       ok->
					   case rd:rpc_call(db_etcd,db_appl_spec,read,[local_type,ApplSpec],5000) of
					       {error,Reason}->
						   {error,Reason}; 
					       {ok,LocalTypeList}->
						   [rpc:call(PodNode,rd,delete_local_resource,[LocalType,PodNode],5000)||LocalType<-LocalTypeList],
						   rpc:call(PodNode,rd,trade_resources,[],5000),
						   timer:sleep(2000),
						   case rd:rpc_call(db_etcd,db_appl_instance,delete,[ClusterSpec,ApplSpec,PodNode],5000) of
						       {atomic,ok}->
							   ok;
						       Reason ->
							   {error,Reason,?MODULE,?LINE}
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

do_new_on_pod(ApplSpec,PodNode,ClusterSpec,TimeOut)->
    {ok,PodDir}=sd:call(db_etcd,db_cluster_instance,read,[pod_dir,ClusterSpec,PodNode],5000),
    {ok,HostSpec}=sd:call(db_etcd,db_cluster_instance,read,[host_spec,ClusterSpec,PodNode],5000),
    {ok,PodApplGitPath}=sd:call(db_etcd,db_appl_spec,read,[gitpath,ApplSpec],5000),
    ApplDir=filename:join([PodDir,ApplSpec]),
    %% set application envs
    {ok,ApplicationConfig}=db_host_spec:read(application_config,HostSpec),  
    _SetEnvResult=[rpc:call(PodNode,application,set_env,[[Config]],5000)||Config<-ApplicationConfig],
    Result=case do_load_start(ApplSpec,PodNode,PodApplGitPath,ApplDir,TimeOut) of
	       {error,Reason}->
		   {error,Reason};
	       ok->
		   ok
	   end,
    Result.
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
appl_new(ApplSpec,HostSpec,ClusterSpec,TimeOut)->
    Result=case pod_server:get_pod(ApplSpec,HostSpec) of
	       {error,Reason}->
		   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get pod   ",[ApplSpec,HostSpec,Reason]]),
		   {error,Reason};
	       []->
		   {error,[no_pods_available,?MODULE,?LINE]};
	       {ok,PodNode}->
		   {ok,PodDir}=db_cluster_instance:read(pod_dir,ClusterSpec,PodNode),
		   {ok,PodApplGitPath}=db_appl_spec:read(gitpath,ApplSpec),
		   ApplDir=filename:join([PodDir,ApplSpec]),
		   %% set application envs
		   {ok,ApplicationConfig}=db_host_spec:read(application_config,HostSpec),  
		   _SetEnvResult=[rpc:call(PodNode,application,set_env,[[Config]],5000)||Config<-ApplicationConfig],
		   case do_load_start(ApplSpec,PodNode,PodApplGitPath,ApplDir,TimeOut) of
		       {error,Reason}->
			   {error,Reason};
		       ok->
			   {atomic,ok}=db_appl_instance:create(ClusterSpec,ApplSpec,PodNode,HostSpec,{date(),time()}),
			   {ok,PodNode}
		   end
	   end,
    Result.
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
do_load_start(ApplSpec,PodNode,PodApplGitPath,ApplDir,TimeOut)->
    Result= case rpc:call(PodNode,file,make_dir,[ApplDir],5000) of
		{error,Reason}->
		    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to create  ",[ApplSpec,PodNode,ApplDir,Reason]]),
		    {error,Reason};
		ok->
		    case appl:git_clone_to_dir(PodNode,PodApplGitPath,ApplDir) of
			{error,Reason}->
			    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to git clone  ",[ApplSpec,PodNode,PodApplGitPath,ApplDir,Reason]]),
			    {error,Reason};
			{ok,_}->
			    {ok,PodApp}=db_appl_spec:read(app,ApplSpec),
			    ApplEbin=filename:join([ApplDir,"ebin"]),
			    Paths=[ApplEbin],
			    case appl:load(PodNode,PodApp,Paths) of
				{error,Reason}->
				    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to load  ",[ApplSpec,PodNode,PodApp,Paths,Reason]]),
				    {error,Reason}; 
				ok->
				    case appl:start(PodNode,PodApp,TimeOut) of
					{error,Reason}->
					    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to start  ",[ApplSpec,PodNode,PodApp,Reason]]),
					    {error,Reason};
					ok->
					    ok
				    end
			    end
		    end
	    end,
    Result.


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
present(ClusterSpec)->
    AppPodList=[{db_appl_spec:read(app,AppSpec),PodNode,AppSpec}||{AppSpec,PodNode}<-db_appl_instance:get_pod_appl_specs(ClusterSpec)],
    [{AppSpec,PodNode,App}||{{ok,App},PodNode,AppSpec}<-AppPodList, 
			 true==rpc:call(node(),lists,keymember,[App,1,rpc:call(PodNode,application,which_applications,[],2000)])].
missing(ClusterSpec)->
    AppPodList=[{db_appl_spec:read(app,AppSpec),PodNode,AppSpec}||{AppSpec,PodNode}<-db_appl_instance:get_pod_appl_specs(ClusterSpec)],
    [{AppSpec,PodNode,App}||{{ok,App},PodNode,AppSpec}<-AppPodList, 
			true/=rpc:call(node(),lists,keymember,[App,1,rpc:call(PodNode,application,which_applications,[],2000)])].


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
hbeat(ClusterSpec)->
    timer:sleep(?HeartbeatTime),
    rpc:call(node(),?MODULE,wanted_state,[ClusterSpec],30*1000), 
    rpc:cast(node(),?MODULE,heartbeat,[]).
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
wanted_state(ClusterSpec)->
      % {AppSpec,PodNode}
    
    MissingPresentNodes=[{ApplSpec,PodNode}||{ApplSpec,PodNode,_App}<-missing(ClusterSpec),
					     pong==net_adm:ping(PodNode)],
    [rpc:call(node(),?MODULE,restart_appl,[ClusterSpec,{ApplSpec,PodNode}],10*1000)||{ApplSpec,PodNode}<-MissingPresentNodes],
%    io:format(" ~p~n",[{R,?MODULE,?FUNCTION_NAME}]),
    ok.
    
    
 %% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
restart_appl(ClusterSpec,{ApplSpec,PodNode})->
    {ok,HostSpec}=db_appl_instance:read(host_spec,ClusterSpec,PodNode),
    {ok,PodDir}=db_cluster_instance:read(pod_dir,ClusterSpec,PodNode),
    {ok,PodApplGitPath}=db_appl_spec:read(gitpath,ApplSpec),
    ApplDir=filename:join([PodDir,ApplSpec]),
		   %% set application envs
    {ok,ApplicationConfig}=db_host_spec:read(application_config,HostSpec),  
    _SetEnvResult=[rpc:call(PodNode,application,set_env,[[Config]],5000)||Config<-ApplicationConfig],
		   %io:format("DEBUG: SetEnvResult ~p~n",[SetEnvResult]),

    {ok,PodApp}=db_appl_spec:read(app,ApplSpec),
    appl:stop(PodNode,PodApp),
    appl:unload(PodNode,PodApp,ApplDir),
    Result=case rpc:call(PodNode,file,make_dir,[ApplDir],5000) of
	       {error,Reason}->
		   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to create pod dir  ",[ApplSpec,PodNode,ApplDir,Reason]]),
		   {error,Reason};
	       ok->
		   case appl:git_clone_to_dir(PodNode,PodApplGitPath,ApplDir) of
		       {error,Reason}->
			   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to git clone  ",[ApplSpec,PodNode,PodApplGitPath,ApplDir,Reason]]),
			   {error,Reason};
		       {ok,_}->
			   ApplEbin=filename:join([ApplDir,"ebin"]),
			   Paths=[ApplEbin],
			   case appl:load(PodNode,PodApp,Paths) of
			       {error,Reason}->
				   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to load appl ",[ApplSpec,PodNode,Reason]]),
				   {error,Reason};
			       ok->
				   case appl:start(PodNode,PodApp,?TimeOut) of
				       {error,Reason}->
					   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to start appl ",[ApplSpec,PodNode,Reason]]),
					   {error,Reason};
				       ok->
					   {ok,LocalTypeList}=db_appl_spec:read(local_type,ApplSpec),
					   [rpc:call(PodNode,rd,add_local_resource,[LocalType,PodNode],5000)||LocalType<-LocalTypeList],
					 %  [rd:add_target_resource_type(LocalType)||LocalType<-LocalTypeList],
					   {ok,TargetTypeList}=db_appl_spec:read(target_type,ApplSpec),
					   [rpc:call(PodNode,rd,add_target_resource_type,[TargetType],5000)||TargetType<-TargetTypeList],
					   rpc:call(PodNode,rd,trade_resources,[],5000),
					   timer:sleep(2000),
					   sd:cast(log,log,notice,[?MODULE,?FUNCTION_NAME,?LINE,node(),"appl re-started ",[ApplSpec,PodNode]]),
					   % tabort   sd:cast(log,log,notice,[?MODULE,?LINE,["application restarted  ",ApplSpec,PodNode]]),
					   {ok,PodNode}
				   end
			   end
		   end
	   end,
    Result.
    
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
deploy(ClusterSpec)->
    ApplDeploySpecList=db_appl_deployment:read_all(),
%    io:format(" ApplDeploySpecList,ClusterSpec ~p~n",[{ApplDeploySpecList,ClusterSpec,?MODULE,?LINE}]),
    start_appl(ApplDeploySpecList,ClusterSpec).

start_appl([],_CurrentClusterSpec)->
    ok;
          
start_appl([{_Id,ApplSpec,_Vsn,ClusterSpec,NumInstances,Affinity}|T],CurrentClusterSpec)->
    case (CurrentClusterSpec == ClusterSpec) of
	false->
	    false;
	true->
	    {ok,WorkerHostSpecs}=db_cluster_spec:read(worker_host_specs,ClusterSpec),
	    start_appl(ApplSpec,CurrentClusterSpec,NumInstances,Affinity,WorkerHostSpecs)
    end,
    start_appl(T,CurrentClusterSpec).

start_appl(_ApplSpec,_CurrentClusterSpec,0,_Affinity,_WorkerHostSpecs)->
    ok;
start_appl(ApplSpec,CurrentClusterSpec,N,any_host,[HostSpec|T])->
    _Result=case appl_new(ApplSpec,HostSpec,CurrentClusterSpec,60*1000) of
		{error,Reason}->
		    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Faield creating appl ",[ApplSpec,HostSpec,Reason]]),
		% tabort    sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,[error_creating_appl_at_host,ApplSpec,HostSpec,Reason]]),
		    {error,[error_creating_appl_at_host,ApplSpec,HostSpec,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
		{ok,PodNode}->
		    sd:cast(log,log,notice,[?MODULE,?FUNCTION_NAME,?LINE,node(),"appl started ",[ApplSpec,HostSpec,PodNode]]),
		% tabort sd:cast(log,log,notice,[?MODULE,?LINE,[appl_created_host,ApplSpec,HostSpec,PodNode]]),
		    {ok,PodNode}
	    end,
    RotatedHostSpecList=lists:append(T,[HostSpec]),
    start_appl(ApplSpec,CurrentClusterSpec,N-1,any_host,RotatedHostSpecList);

start_appl(ApplSpec,CurrentClusterSpec,N,[HostSpec|T],WorkerHostSpecs)->
    _Result=case appl_new(ApplSpec,HostSpec,CurrentClusterSpec,60*1000) of
		{error,Reason}->
		    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Faield creating appl ",[ApplSpec,HostSpec,Reason]]),
		% tabort    sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,[error_creating_appl_at_host,ApplSpec,HostSpec,Reason]]),
		    
		    {error,[error_creating_appl_at_host,ApplSpec,HostSpec,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
		{ok,PodNode}->
		    sd:cast(log,log,notice,[?MODULE,?FUNCTION_NAME,?LINE,node(),"appl started ",[ApplSpec,HostSpec,PodNode]]),
		% tabort    sd:cast(log,log,notice,[?MODULE,?LINE,[appl_created_host,ApplSpec,HostSpec,PodNode]]),
		    {ok,PodNode}
	    end,
    RotatedHostSpecList=lists:append(T,[HostSpec]),
    start_appl(ApplSpec,CurrentClusterSpec,N-1,RotatedHostSpecList,WorkerHostSpecs).

	    
