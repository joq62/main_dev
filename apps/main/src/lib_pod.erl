%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_pod).
 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports
-export([
	 create_node/5,
	 create_node/1,
	 active_nodes/1,
	 stopped_nodes/1

	]).


%% ====================================================================
%% External functions
%% ====================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
 %desired_nodes()->
 %   Result=case sd:call(etcd,db_pod_desired_state,get_all_id,[],5000) of
%	       {error,Reason}->
%		   {error,Reason};
%	       []->
%		   {error,["No desired parent nodes are declared: "]};
%	       Nodes->
%		   {ok,Nodes}
%	   end,
 %   Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
active_nodes(ClusterSpec)->
    Result= case sd:call(etcd,db_pod_desired_state,pods,[ClusterSpec],5000) of
		{error,Reason}->
		    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get pods",[ClusterSpec,Reason]]),
		    {error,Reason};
		AllNodes->
		    RunningNodesDir=[{Node,sd:call(etcd,db_pod_desired_state,read,[pod_dir,Node],5000)}||Node<-AllNodes,
													 pong==net_adm:ping(Node)],
		    ActiveNodes=[Node||{Node,{ok,PodDir}}<-RunningNodesDir,
				       rpc:call(Node,filelib,is_dir,[PodDir],5000)],
		    [rpc:call(Node,init,stop,[],3000)||{Node,{ok,_PodDir}}<-RunningNodesDir,
						       false==lists:member(Node,ActiveNodes)],
		    {ok,ActiveNodes}
	    end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stopped_nodes(ClusterSpec)->
    AllNodes= sd:call(etcd,db_pod_desired_state,pods,[ClusterSpec],5000),
    Result=case active_nodes(ClusterSpec) of
	       {ok,ActiveNodes}->		 
		   StoppedNodes=[Node||Node<-AllNodes,
				       false==lists:member(Node,ActiveNodes)],
		   {ok,StoppedNodes};
	       Reason->
		   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get active nodes",[ClusterSpec,Reason]]),
		   {error,Reason}
	   end,
    Result.
    
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_node(PodNode)->
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
						   create_node(ParentNode,NodeName,PodDir,PaArgsList,EnvArgs);
					       Reason->
						   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get env_args ",[PodNode,Reason]]),
						   {error,Reason}
					   end;
				       Reason->
					   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get pa_args ",[PodNode,Reason]]),
					   {error,Reason}
				   end;
			       Reason->
				   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get pod dir ",[PodNode,Reason]]),
				   {error,Reason}
			   end;
		       Reason->
			   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get node name ",[PodNode,Reason]]),
			   {error,Reason}
		   end;
	       Reason->
		   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get parent node ",[PodNode,Reason]]),
		   {error,Reason}
	   end,
    Result.

create_node(ParentNode,NodeName,PodDir,PaArgsList,EnvArgs)->
    Result=case rpc:call(ParentNode,net,gethostname,[],5000) of
	       {badrpc,Reason}->
		    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get hostname ",[ParentNode,NodeName,PodDir,Reason]]),
		   {badrpc,["Error  :",Reason,ParentNode]};
	       {ok,HostName}->
		   case rpc:call(ParentNode,erlang,get_cookie,[],5000) of 
		       {badrpc,Reason}->
			   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to getcookie ",[ParentNode,NodeName,PodDir,Reason]]),
			   {badrpc,["Error  :",Reason,ParentNode]};	
		       CookieAtom->
			   Cookie=atom_to_list(CookieAtom),
			   Args=" -setcookie "++Cookie++" "++EnvArgs,
			   case rpc:call(ParentNode,slave,start,[HostName,NodeName,Args],5000) of
			       {badrpc,Reason}->
				   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to slave ",[ParentNode,HostName,NodeName,Args,Reason]]),
				   {badrpc,Reason};
			       {error,Reason}->
				   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to slave ",[ParentNode,HostName,NodeName,Args,Reason]]),
				   {error,Reason};
			       {ok,SlaveNode}->
				   case net_kernel:connect_node(SlaveNode) of
				       false->
					   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to connect to node ",[SlaveNode]]),
					   {error,[failed_connect,SlaveNode]};
				       ignored->
					   {error,[ignored,SlaveNode]};
				       true->
					   rpc:call(SlaveNode,file,del_dir_r,[PodDir],5000),			  			  
					   case rpc:call(SlaveNode,file,make_dir,[PodDir],5000) of
					       {badrpc,Reason}->
						   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to make dir ",[PodDir]]),
						   rpc:call(SlaveNode,init,stop,[],1000),
						   {badrpc,Reason};
					       {error,Reason}->
						   rpc:call(SlaveNode,init,stop,[],1000),
						   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to make dir ",[PodDir]]),
						   {error,Reason};
					       ok->  
						   R=[{error,Path}||Path<-[PodDir|PaArgsList],
								     true/=rpc:call(SlaveNode,code,add_patha,[Path],5000)],
						   case R of
						       []->
							  ok;
						       _ ->
							   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed add paths ",[R]]),
							   {error,[R]}
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
