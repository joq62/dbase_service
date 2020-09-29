%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(mnesia_test). 
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%---------------------------------------------------------------------
%% Records for test
%%
-record(computer,
	{
	  host_id,
	  ssh_uid,
	  ssh_passwd,
	  ip_addr,
	  port
	}).

-define(TABLE,computer).
-define(RECORD,computer).
-define(TEXTFILE,"test_src/dbase_init.hrl").

-define(MASTER_MNESIA,'mnesia@sthlm_1').

-define(MnesiaNodes,[?MASTER_MNESIA,'mnesia@asus']).
%% --------------------------------------------------------------------

-export([start/0]).

%% ====================================================================
%% External functions
%% ====================================================================
init_table()->
    rpc:call(?MASTER_MNESIA,mnesia,stop,[]),
    rpc:call(?MASTER_MNESIA,mnesia,create_schema,[?MnesiaNodes]),
    rpc:call(?MASTER_MNESIA,mnesia,start,[]),
    [rpc:call(Node,application,stop,[mnesia])||Node<-?MnesiaNodes],   
    [rpc:call(Node,application,start,[mnesia])||Node<-?MnesiaNodes],
    ?assertEqual({atomic,ok},mnesia:load_textfile(?TEXTFILE)),
    ok.

% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
read_tables()->
    Computers=rpc:call(?MASTER_MNESIA,mnesia,dirty_all_keys,[computer]),
    ?assertEqual([[{computer,"sthlm_1","pi","festum01","192.168.0.110",60110}],
		  [{computer,"asus","pi","festum01","192.168.0.100",60100}]],
		 [rpc:call(?MASTER_MNESIA,mnesia,dirty_read,[{computer,Computer}])||
		     Computer<-Computers]),

    ServiceDefs=rpc:call(?MASTER_MNESIA,mnesia,dirty_all_keys,[service_def]),
    ?assertEqual([[{service_def,"mail_service","1.0.0","https://github.com/joq62/"}],
		  [{service_def,"multi_service","1.0.0","https://github.com/joq62/"}],
		  [{service_def,"log_service","1.0.0","https://github.com/joq62/"}],
		  [{service_def,"adder_service","1.0.0","https://github.com/joq62/"}],
		  [{service_def,"vm_service","1.0.0","https://github.com/joq62/"}],
		  [{service_def,"divi_service","1.0.0","https://github.com/joq62/"}],
		  [{service_def,"orchistrate_service","1.0.0","https://github.com/joq62/"}],
		  [{service_def,"oam_service","1.0.0","https://github.com/joq62/"}]],
		 [rpc:call(?MASTER_MNESIA,mnesia,dirty_read,[{service_def,ServiceDef}])||
		     ServiceDef<-ServiceDefs]),

    
    ?assertEqual([{service_def,"mail_service","1.0.0","https://github.com/joq62/"}],
		 rpc:call(?MASTER_MNESIA,mnesia,dirty_read,[{service_def,"mail_service"}])),
    ?assertEqual([],
		 rpc:call(?MASTER_MNESIA,mnesia,dirty_read,[{service_def,"glurk"}])),
    
    ?assertEqual([{service_def,"mail_service","1.0.0","https://github.com/joq62/"}],
		 rpc:call(node(),mnesia,dirty_read,[{service_def,"mail_service"}])),
    ?assertEqual([],
		 rpc:call(node(),mnesia,dirty_read,[{service_def,"glurk"}])),

    % use host
    HostId=net_adm:localhost(),
    MnesiaVm=list_to_atom("mnesia@"++HostId),
    ?assertEqual([{service_def,"mail_service","1.0.0","https://github.com/joq62/"}],
		 rpc:call(MnesiaVm,mnesia,dirty_read,[{service_def,"mail_service"}])),
    ?assertEqual([],
		 rpc:call(MnesiaVm,mnesia,dirty_read,[{service_def,"glurk"}])),
    ok.

% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
sim_computer()->
    HostId=net_adm:localhost(),
    MnesiaVm=list_to_atom("mnesia@"++HostId),
    
    ?assertEqual([{"sthlm_1","pi","festum01","192.168.0.110",60110},
		  {"asus","pi","festum01","192.168.0.100",60100}],
		 rpc:call(MnesiaVm,db_computer,read_all,[])),

    ?assertEqual([{"mail_service","1.0.0","https://github.com/joq62/"},
		  {"multi_service","1.0.0","https://github.com/joq62/"},
		  {"log_service","1.0.0","https://github.com/joq62/"},
		  {"adder_service","1.0.0","https://github.com/joq62/"},
		  {"vm_service","1.0.0","https://github.com/joq62/"},
		  {"divi_service","1.0.0","https://github.com/joq62/"},
		  {"orchistrate_service","1.0.0","https://github.com/joq62/"},
		  {"oam_service","1.0.0","https://github.com/joq62/"}],
		 rpc:call(MnesiaVm,db_service_def,read_all,[])),

    
    
    ok.


% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start()->
    %initiate table 
    ?debugMsg("start mnesia_test"),  

    ?debugMsg("start init_table"),  
    ?assertEqual(ok,init_table()),
    ?debugMsg("start read_tables"), 
    ?assertEqual(ok,read_tables()),

    ?debugMsg("start sim_computer"), 
    ?assertEqual(ok,sim_computer()),
    ?debugMsg("test OK"), 
    ok.

create(Record) ->
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).


read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{HostId,SshUid,SshPassWd,IpAddr,Port}||{?RECORD,HostId,SshUid,SshPassWd,IpAddr,Port}<-Z].



read(HostId) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.host_id==HostId])),
    [{HId,SshUid,SshPassWd,IpAddr,Port}||{?RECORD,HId,SshUid,SshPassWd,IpAddr,Port}<-Z].


update(HostId,SshId,SshPwd,IpAddr,Port)->
    F = fun() ->
		Oid = {?TABLE, HostId},
		mnesia:delete(Oid),
		Record = #?RECORD{host_id=HostId,ssh_uid=SshId,ssh_passwd=SshPwd,
				  ip_addr=IpAddr,port=Port},
		mnesia:write(Record)
	end,
    mnesia:transaction(F).

delete(HostId) ->
    Oid = {?TABLE, HostId},
    F = fun() -> mnesia:delete(Oid) end,
  mnesia:transaction(F).


do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.


