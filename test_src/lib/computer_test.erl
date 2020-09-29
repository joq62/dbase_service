%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(computer_test). 
   
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


%% --------------------------------------------------------------------

-export([start/0]).

%% ====================================================================
%% External functions
%% ====================================================================
init_table()->
%   ?assertEqual({atomic,ok}, mnesia:create_table(?TABLE, [%{disc_copies,[node(),'mnesia@sthlm_1']},
%				 {attributes, record_info(fields, ?RECORD)}		
%				])),
    mnesia:create_table(?TABLE, [{disc_copies,[node(),'mnesia@sthlm_1']},
				 {attributes, record_info(fields, ?RECORD)}		
				]),
    mnesia:wait_for_tables(?TABLE, 20000),
    ok.

% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start()->
    %initiate table 
    ?assertEqual(ok,init_table()),

    create(#?RECORD{host_id="asus",ssh_uid="pi",ssh_passwd="festum01",
		    ip_addr="192.168.0.100",port=60100}),
    ?assertEqual([{"asus","pi","festum01","192.168.0.100",60100}],read_all()),
    
    create(#?RECORD{host_id="asus",ssh_uid="pi",ssh_passwd="festum01",
		    ip_addr="192.168.0.200",port=60200}),
    ?assertEqual([{"asus","pi","festum01","192.168.0.200",60200}],read_all()),
    
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


