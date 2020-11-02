%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(add_node). 
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%%---------------------------------------------------------------------
%% Records for test
%%
-define(InitFile,"./test_src/table_info.hrl").

%% --------------------------------------------------------------------

-export([start/0]).

%% ====================================================================
%% External functions
%% ====================================================================
% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
detect_lost_node(VmId)->
    ?assertEqual(ok,stop_node("db2")),
%    io:format("all ~p~n",[{?MODULE,?LINE,mnesia:system_info(all)}]),
    AllNodes=mnesia:system_info(db_nodes),
    io:format("AllNodes ~p~n",[{?MODULE,?LINE,AllNodes}]),
    Running=mnesia:system_info(running_db_nodes),
    io:format("Running ~p~n",[{?MODULE,?LINE,Running}]),

    ok.

% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
stop_node(VmId)->
    {ok,HostId}=inet:gethostname(),
    Vm=list_to_atom(VmId++"@"++HostId),
    rpc:call(Vm,init,stop,[]),
    timer:sleep(2000),
    ok.
% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start_node(VmId)->
    {ok,HostId}=inet:gethostname(),
    Vm=list_to_atom(VmId++"@"++HostId),
    []=os:cmd("erl -sname "++VmId++" -setcookie abc -detached"),
    R=check_started(500,Vm,10,{error,[Vm]}),    
    R.


% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
add(VmId)->
    {ok,HostId}=inet:gethostname(),
    Vm=list_to_atom(VmId++"@"++HostId),
    pong=net_adm:ping(Vm),
  %  dbase_lib:add_node(Vm),
    dbase:add_node(Vm),
%    dbase_lib:add_node(Vm),
    ok=check_db_values(VmId),
    io:format(" ~p~n",[{?MODULE,?LINE,mnesia:system_info()}]),
    
    ok.

% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start()->
    %initiate table 
    ?assertEqual(ok,stop_node("db1")),
    ?assertEqual(ok,start_node("db1")),
    ?assertEqual(ok,add("db1")),
    ?assertEqual(ok,stop_node("db2")),
    ?assertEqual(ok,start_node("db2")),
    ?assertEqual(ok,add("db2")),
    ?assertEqual(ok,detect_lost_node("db2")),
    

    ok.

% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
check_db_values(VmId)->
    {ok,HostId}=inet:gethostname(),
    Vm=list_to_atom(VmId++"@"++HostId),
    ?assertEqual(["wrong_port","asus","wrong_hostname",
		  "sthlm_1","wrong_ipaddr","wrong_passwd",
		  "wrong_userid"],
		 rpc:call(Vm,mnesia,dirty_all_keys,[computer])),
    ?assertEqual([],
		 rpc:call(Vm,mnesia,dirty_all_keys,[deployment])),
    ?assertEqual(["math"],
		 rpc:call(Vm,mnesia,dirty_all_keys,[deployment_spec])),
    ?assertEqual(["joq62"],
		 rpc:call(Vm,mnesia,dirty_all_keys,[passwd])),
    ?assertEqual([],
		 rpc:call(Vm,mnesia,dirty_all_keys,[sd])),
    ?assertEqual(["adder_service","divi_service","multi_service"],
		 rpc:call(Vm,mnesia,dirty_all_keys,[service_def])),
    ?assertEqual([{vm,'30000@asus',"asus","30000",worker,not_available}],
		 rpc:call(Vm,mnesia,dirty_read,[{vm,'30000@asus'}])),
    ok.

		   
check_started(_N,_Vm,_Timer,ok)->
    ok;
check_started(0,_Vm,_Timer,Result)->
    Result;
check_started(N,Vm,Timer,_Result)->
    NewResult=case net_adm:ping(Vm) of
		  pong->
		      ok;
		  Err->
		      timer:sleep(Timer),
		      {error,[Err,Vm]}
	      end,
    check_started(N-1,Vm,Timer,NewResult).



