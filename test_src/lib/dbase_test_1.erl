%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(dbase_test_1). 
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
%-include("log.hrl").
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
start()->
    ?debugMsg("start check intial dbase_service"),
    ?assertEqual(ok,init_dbase()),  
%    ?debugMsg("test1"),
%    ?assertEqual(ok,test1()), 

%    ?debugMsg("test2"),
%    ?assertEqual(ok,test2()), 
%    ?debugMsg("test3"),
%    ?assertEqual(ok,test3()), 
%    ?debugMsg("test4"),
%    ?assertEqual(ok,test4()), 

    ?debugMsg("Dbase test succeded "),    
    ok.

%%-----------------------------------------------------------------
init_dbase()->

    ok.

test1()->
    ?assertEqual(ok,dbase_service:insert(service_discovery,{"s1",node1})),
    timer:sleep(100),
    ?assertEqual([{service_discovery,"s1",node1}],dbase_service:read(service_discovery,"s1")),
    ?assertEqual([{service_discovery,"s1",node1}],rpc:call('node1@asus',dbase_service,read,[service_discovery,"s1"])),
    ok.
    
test2()->
    rpc:call('node1@asus',init,stop,[]),
    %% check 
    timer:sleep(1000),
    ?assertEqual([{service_discovery,"s1",node1}],dbase_service:read(service_discovery,"s1")),
    %% restart node
    os:cmd("erl -pa ebin -sname node1 -detached"), 
    timer:sleep(1000),
    ?assertMatch({badrpc,_},rpc:call('node1@asus',dbase_service,read,[service_discovery,"s1"])),
    ?assertEqual([ok],[rpc:call(Node,application,start,[dbase_service])||Node<-['node1@asus']]),
    ?assertEqual([ok],dbase_service:start_mnesia(['node1@asus'])),    
    ?assertEqual([{service_discovery,"s1",node1}],rpc:call('node1@asus',dbase_service,read,[service_discovery,"s1"])),
    
    
    rpc:call('node1@asus',init,stop,[]),
    %% check 
    timer:sleep(1000),
    ?assertEqual(ok,dbase_service:insert(service_discovery,{"s2",node2})),
    timer:sleep(100),    
    ?assertEqual([{service_discovery,"s2",node2}],dbase_service:read(service_discovery,"s2")),
    ?assertEqual({badrpc,nodedown},rpc:call('node1@asus',dbase_service,read,[service_discovery,"s2"])),

    os:cmd("erl -pa ebin -sname node1 -detached"), 
    timer:sleep(1000),
    ?assertMatch({badrpc,_},rpc:call('node1@asus',dbase_service,read,[service_discovery,"s2"])),
    ?assertEqual([ok],[rpc:call(Node,application,start,[dbase_service])||Node<-['node1@asus']]),
    ?assertEqual([ok],dbase_service:start_mnesia(['node1@asus'])),    
    ?assertEqual([{service_discovery,"s1",node1}],rpc:call('node1@asus',dbase_service,read,[service_discovery,"s1"])),
    ?assertEqual([{service_discovery,"s2",node2}],rpc:call('node1@asus',dbase_service,read,[service_discovery,"s2"])),


    ?assertMatch([{service_discovery,_,_},
		  {service_discovery,_,_}],dbase_service:read_table(service_discovery)),   
    ok.
    
test3()->
    ?assertEqual(ok,dbase_service:insert(catalog,{"s1",source1})),
    ?assertEqual([{catalog,"s1",source1}],dbase_service:read(catalog,"s1")),
    ?assertEqual([{catalog,"s1",source1}],rpc:call('node1@asus',dbase_service,read,[catalog,"s1"])),

    ?assertEqual(ok,dbase_service:insert(catalog,{"s1",source11})),
    ?assertEqual([{catalog,"s1",source11}],dbase_service:read(catalog,"s1")),
    ?assertEqual([{catalog,"s1",source11}],rpc:call('node1@asus',dbase_service,read,[catalog,"s1"])),    

    ok.

