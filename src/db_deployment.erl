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
    [{DeplId,SpecId,Vsn,Date,Time,SdList,XStatus}||{?RECORD,DeplId,SpecId,Vsn,Date,Time,SdList,XStatus}<-Z].


% End Special 


create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
    mnesia:wait_for_tables([?TABLE], 20000).
create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create({?MODULE,DeplId,SpecId,Vsn,Date,Time,SdList,Status}) ->
    create(DeplId,SpecId,Vsn,Date,Time,SdList,Status).
create(DeplId,SpecId,Vsn,Date,Time,SdList,Status) ->
    Record=#?RECORD{deployment_id=DeplId,
		    deployment_spec_id=SpecId,
		    deployment_spec_vsn=Vsn,
		    date=Date,
		    time=Time,
		    sd_list=SdList,
		    status=Status
		   },

    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{DeplId,SpecId,Vsn,Date,Time,SdList,Status}||{?RECORD,DeplId,SpecId,Vsn,Date,Time,SdList,Status}<-Z].


read(DeplId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.deployment_id==DeplId])),
    [{XDeplId,SpecId,Vsn,Date,Time,SdList,Status}||{?RECORD,XDeplId,SpecId,Vsn,Date,Time,SdList,Status}<-Z].

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
