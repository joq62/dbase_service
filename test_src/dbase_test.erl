%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(dbase_test). 
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

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
    ?debugMsg("test1"),
    ?assertEqual(ok,test1()), 

    ?debugMsg("Dbase test succeded "),    
    ok.

%%-----------------------------------------------------------------
init_dbase()->
    rpc:call('node1@asus',application,stop,[mnesia]),
    rpc:call('node1@asus',application,stop,[dbase_service]),

    MyNode=node(),
    ?assertEqual(ok,dbase_service:create_schema([MyNode,'node1@asus'])),
    ?assertEqual([{error,{already_started,dbase_service}},ok],[rpc:call(Node,application,start,[dbase_service])||Node<-[MyNode,'node1@asus']]),
    ?assertEqual([ok,ok],dbase_service:start_mnesia([MyNode,'node1@asus'])),
    ?assertEqual({atomic,ok},dbase_service:create_table()),
    ok.

test1()->
    ?assertEqual(ok,dbase_service:insert("s1",node1)),
    timer:sleep(100),
    ?assertEqual([{service_discovery,"s1",node1}],dbase_service:read(service_discovery,"s1")),
    ?assertEqual([{service_discovery,"s1",node1}],rpc:call('node1@asus',dbase_service,read,[service_discovery,"s1"])),
    ok.
    
