%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Create1d : 10 dec 2012
%%% -------------------------------------------------------------------
-module(dbase_service_tests). 
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").




%% --------------------------------------------------------------------
%% External exports
-export([start/0]).


-define(TEXTFILE,"./test_src/dbase_init.hrl").



%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start()->
    spawn(fun()->eunit:test({timeout,1*60,dbase_service}) end).

cases_test()->
    ?debugMsg("Test system setup"),
    setup(),
    %% Start application tests

 %   ?debugMsg("computer_test"),    
 %   ?assertEqual(ok,computer_test:start()),
    ?debugMsg("init_test"),    
    ?assertEqual(ok,init_test:start()),

      %% End application tests
  
  %  cleanup(),
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
setup()->
    ?assertEqual(ok,application:start(dbase_service)), 
    ?assertMatch({pong,_,_},dbase_service:ping()),
  %  {ok,Bin}=file:read_file(?TEXTFILE),
  %  dbase_service:load_texfile("init_load",Bin),

    timer:sleep(500),
    ok.
cleanup()->
    MyNode=node(),
    [rpc:call(Node,init,stop,[])||Node<-[MyNode]],
  %  init:stop(),
    ok.
