%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Create1d : 10 dec 2012
%%% -------------------------------------------------------------------
-module(dbase_tests). 
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").




%% --------------------------------------------------------------------
%% External exports
-export([start/0]).

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start()->
    ?debugMsg("Start setup"),
    setup(),
    ?debugMsg("Stop setup"),   
    %% Start application tests

    ?debugMsg("Start init_tables"),
    ?assertEqual(ok,init_tables:start()),
    ?debugMsg("Stop init_tables"),   

    ?debugMsg("Start add_node"),
    ?assertEqual(ok,add_node:start()),
    ?debugMsg("Stop add_node"), 
 %   ?debugMsg("computer_test"),    
 %   ?assertEqual(ok,computer_test:start()),
 %   ?debugMsg("init_test"),    
 %   ?assertEqual(ok,init_test:start()),

      %% End application tests
  
    cleanup(),
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
setup()->
    ?assertEqual(ok,application:start(dbase)), 
    ?assertMatch({pong,_,_},dbase:ping()),
  %  {ok,Bin}=file:read_file(?TEXTFILE),
  %  dbase_service:load_texfile("init_load",Bin),

    timer:sleep(500),
    ok.
cleanup()->
    MyNode=node(),
    [rpc:call(Node,init,stop,[])||Node<-[MyNode]],
  %  init:stop(),
    ok.
