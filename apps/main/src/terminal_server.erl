%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(terminal_server).
 
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(SERVER,?MODULE).
-define(ConsoleNodeName,"console").
%% External exports


-export([
	 print/1,
	 print/2
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
	       console_node
	      }).


%% ====================================================================
%% External functions
%% ====================================================================

start([ClusterSpec])-> gen_server:start_link({local, ?SERVER}, ?SERVER, [ClusterSpec], []).
stop()-> gen_server:call(?SERVER, {stop},infinity).


	    
%% call
print(Arg1)->
    gen_server:call(?SERVER, {print,Arg1},infinity).
print(Arg1,Arg2)->
    gen_server:call(?SERVER, {print,Arg1,Arg2},infinity).
	    
%% call


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
    
    ok=lib_install:start_local_appls(ClusterSpec),
    true=lib_control:ensure_right_cookie(ClusterSpec),
    application:stop(etcd),
    {ok,HostName}=net:gethostname(),
    ConsoleNode=list_to_atom(?ConsoleNodeName++"@"++HostName),
    pong=net_adm:ping(ConsoleNode),
    io:format("Started Server ~p~n",[{ClusterSpec,erlang:get_cookie(),?MODULE,?LINE}]), 
	{ok, #state{
		console_node=ConsoleNode,
		cluster_spec=ClusterSpec}}.   
 

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
handle_call({print,Arg1},_From, State) ->
    Reply=io:format(Arg1),	   
    {reply, Reply, State};

handle_call({print,Arg1,Arg2},_From, State) ->
    Reply=io:format(Arg1,Arg2),	   
    {reply, Reply, State};


handle_call({ping},_From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call({stop},_From, State) ->
    {stop, normal,stopped, State};

handle_call(Request, From, State) ->
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
    io:format("unmatched match cast ~p~n",[{Msg,?MODULE,?LINE}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({ssh_cm,_,_}, State) ->
    {noreply, State};

handle_info(Info, State) ->
    io:format("unmatched match~p~n",[{Info,?MODULE,?LINE}]), 
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

