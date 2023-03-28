%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_parent).
 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports
-export([
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
active_nodes(ClusterSpec)->
    AllNodes= sd:call(etcd,db_parent_desired_state,pods,[ClusterSpec],5000),
    RunningNodes=[Node||Node<-AllNodes,
			pong==net_adm:ping(Node)],
    Result=case sd:call(etcd,db_cluster_spec,read,[root_dir,ClusterSpec],5000) of
	       {ok,RootDir}->	
		   ActiveNodes=[Node||Node<-RunningNodes,
				      rpc:call(Node,filelib,is_dir,[RootDir],5000)],
		   [rpc:call(Node,init,stop,[],3000)||Node<-RunningNodes,
						      false==lists:member(Node,ActiveNodes)],
		   {ok,ActiveNodes};
	       Reason->
		   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get root dir",[ClusterSpec,Reason]]),
		   {error,Reason}
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stopped_nodes(ClusterSpec)->
    AllNodes= sd:call(etcd,db_parent_desired_state,pods,[ClusterSpec],5000),
    Result=case active_nodes(ClusterSpec) of
	       {ok,ActiveNodes}->		 
		   StoppedNodes=[Node||Node<-AllNodes,
				       false==lists:member(Node,ActiveNodes)],
		   {ok,StoppedNodes};
	       Reason->
		    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get active nodes ",[ClusterSpec,Reason]]),
		   {error,Reason}
	   end,
    Result.
    
    
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_node(ParentNode)->
 %   sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG: create_node ParentNode : ",ParentNode,?MODULE,?LINE]]),
    Result=case sd:call(etcd,db_parent_desired_state,read,[host_spec,ParentNode],5000) of
	       {ok,HostSpec}->
		   case sd:call(etcd,db_parent_desired_state,read,[node_name,ParentNode],5000) of
		       {ok,NodeName}->
			   case sd:call(etcd,db_parent_desired_state,read,[cluster_spec,ParentNode],5000) of
			       {ok,ClusterSpec}-> 
				   case sd:call(etcd,db_cluster_spec,read,[cookie,ClusterSpec],5000) of
				       {ok,Cookie}->
					   EnvArgs=" -detached ",
					   PaArgs=" ",
					   TimeOut=10*1000,
					   case rpc:call(node(),ops_ssh,create,[HostSpec,NodeName,Cookie,PaArgs,EnvArgs,TimeOut],TimeOut+1000) of
					       {ok,ParentNode}->
						   case rpc:call(ParentNode,file,del_dir_r,[ClusterSpec],10*1000) of
						       ok->
							   case rpc:call(ParentNode,file,make_dir,[ClusterSpec],10*1000) of
							       ok->
								   ok;
							       Reason->
								   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to make dir  ",[ClusterSpec,ParentNode,Reason]]),
								   {error,Reason}
							   end;
						       Reason->
							   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to delete dir  ",[ClusterSpec,ParentNode,Reason]]),
							   {error,Reason}
						   end;
					       Reason->
						   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to create vm  ",[ParentNode,HostSpec,NodeName,Cookie,PaArgs,EnvArgs,TimeOut,Reason]]),
						   {error,Reason}
					   end;   
				       Reason->
					   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get cookie   ",[ParentNode,Reason]]),
					   {error,Reason}
				   end;
			       Reason->
				   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get cluster spec   ",[ParentNode,Reason]]),
				   {error,Reason}
			   end;
		       Reason->
			   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get node name   ",[ParentNode,Reason]]),
		   {error,Reason}
		   end;
	       Reason->
		   sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Failed to get host spec   ",[ParentNode,Reason]]),
		   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error: ,db_parent_desired_state,read,[host_spec,ParentNode: ",Reason,ParentNode,?MODULE,?LINE]]),
		   {error,Reason}
	   end,
    Result.    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

