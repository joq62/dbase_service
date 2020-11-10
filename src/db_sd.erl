-module(db_sd).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_sd.hrl").

-define(TABLE,sd).
-define(RECORD,sd).

%Start Special 

get(ServiceId)->
    X=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.service_id==ServiceId])),
    [XVm||{?RECORD,_XServiceId,_XVsn,_XHostId,_XVmId,XVm}<-X].

get(ServiceId,Vsn) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.service_id==ServiceId,
		     X#?RECORD.vsn==Vsn])),
    [XVm||{?RECORD,_XServiceId,_XVsn,_XHostId,_XVmId,XVm}<-Z].


% End Special

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				{type,bag}]),
    mnesia:wait_for_tables([?TABLE], 20000).
create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create({?MODULE,ServiceId,Vsn,HostId,VmId,Vm}) ->
    create(ServiceId,Vsn,HostId,VmId,Vm).
create(ServiceId,Vsn,HostId,VmId,Vm) ->
    Record=#?RECORD{service_id=ServiceId,
		    vsn=Vsn,
		    host_id=HostId,
		    vm_id=VmId,
		    vm=Vm 
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{XServiceId,XVsn,XHostId,XVmId,XVm}||{?RECORD,XServiceId,XVsn,XHostId,XVmId,XVm}<-Z].



read(ServiceId) ->
    X=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.service_id==ServiceId])),
    [{XServiceId,XVsn,XHostId,XVmId,XVm}||{?RECORD,XServiceId,XVsn,XHostId,XVmId,XVm}<-X].

read(ServiceId,Vsn) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.service_id==ServiceId,
		     X#?RECORD.vsn==Vsn])),
    [{XServiceId,XVsn,XHostId,XVmId,XVm}||{?RECORD,XServiceId,XVsn,XHostId,XVmId,XVm}<-Z].

delete(Id,Vsn,Vm) ->
    F = fun() -> 
		ServiceDiscovery=[X||X<-mnesia:read({?TABLE,Id}),
				     X#?RECORD.service_id==Id,
				     X#?RECORD.vsn==Vsn,
				     X#?RECORD.vm==Vm],
		case ServiceDiscovery of
		    []->
			mnesia:abort(?TABLE);
		    [S1]->
			mnesia:delete_object(S1) 
		end
	end,
    mnesia:transaction(F).


do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------
