-module(db_deployment).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_deployment.hrl").


-define(TABLE,deployment).
-define(RECORD,deployment).

% Start Special

status(Status)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.status==Status])),
    [{DeplId,SpecId,Vsn,Date,Time,HostId,VmId,SdList,XStatus}||{?RECORD,DeplId,SpecId,Vsn,Date,Time,HostId,VmId,_Vm,SdList,XStatus}<-Z].

update_status(DeplId,NewStatus)->
    F = fun() -> 
		Deployment=[X||X<-mnesia:read({?TABLE,DeplId}),
			       X#?RECORD.deployment_id==DeplId],
		case Deployment of
		    []->
			io:format("CurrentRecord = ~p~n",[{?MODULE,?LINE,[]}]),
			mnesia:abort(?TABLE);
		    [CurrentRecord]->
		%	io:format("CurrentRecord = ~p~n",[{?MODULE,?LINE,CurrentRecord}]),
			NewRecord=CurrentRecord#?RECORD{status=NewStatus},
		%	io:format("NewRecord = ~p~n",[{?MODULE,?LINE,NewRecord}]),
			mnesia:write(NewRecord)
		end
	end,
    io:format("F= ~p~n",[{?MODULE,?LINE,F}]),
    mnesia:transaction(F). 
    
% End Special 


create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
    mnesia:wait_for_tables([?TABLE], 20000).
create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create({?MODULE,DeplId,SpecId,Vsn,Date,Time,HostId,VmId,SdList,Status}) ->
    create(DeplId,SpecId,Vsn,Date,Time,HostId,VmId,SdList,Status).
create(DeplId,SpecId,Vsn,Date,Time,HostId,VmId,SdList,Status) ->
    Vm=list_to_atom(VmId++"@"++HostId),
    Record=#?RECORD{deployment_id=DeplId,
		    deployment_spec_id=SpecId,
		    deployment_spec_vsn=Vsn,
		    date=Date,
		    time=Time,
		    host_id=HostId,
		    vm_id=VmId,
		    vm=Vm,
		    sd_list=SdList,
		    status=Status
		   },

    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{DeplId,SpecId,Vsn,Date,Time,HostId,VmId,SdList,Status}||{?RECORD,DeplId,SpecId,Vsn,Date,Time,HostId,VmId,_Vm,SdList,Status}<-Z].


read(DeplId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.deployment_id==DeplId])),
    [Y]=[{XDeplId,SpecId,Vsn,Date,Time,HostId,VmId,SdList,Status}||{?RECORD,XDeplId,SpecId,Vsn,Date,Time,HostId,VmId,_Vm,SdList,Status}<-Z],
    Y.

delete(DeplId) ->
    F = fun() -> 
		Deployment=[X||X<-mnesia:read({?TABLE,DeplId}),
			       X#?RECORD.deployment_id==DeplId],
		case Deployment of
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
