%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(dbase_lib).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("db_computer.hrl").
-include("db_deployment.hrl").
-include("db_deployment_spec.hrl").
-include("db_passwd.hrl").
-include("db_sd.hrl").
-include("db_service_def.hrl").
-include("db_vm.hrl").

%% --------------------------------------------------------------------


%% External exports
-export([start/1,
	 db_init/1,
	 add_node/1,
	 add_extra_nodes/1
	]).

-define(WAIT_FOR_TABLES,5000).

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

start(Nodes)->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),
    db_init(lists:delete(node(), Nodes)).

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

add_node(Vm)->
    io:format(" ~p~n",[{?MODULE,?LINE,Vm}]),
    rpc:call(Vm,mnesia,stop,[]),
    mnesia:delete_schema([Vm]),
    ok=rpc:call(Vm,mnesia,start,[]),
    mnesia:change_config(extra_db_nodes, [Vm]),

    mnesia:add_table_copy(schema, Vm,ram_copies),
	    % Update with tables
    mnesia:add_table_copy(computer, Vm, ram_copies),
    mnesia:add_table_copy(deployment, Vm, ram_copies),
    mnesia:add_table_copy(deployment_spec, Vm, ram_copies),
    mnesia:add_table_copy(passwd, Vm, ram_copies),
    mnesia:add_table_copy(sd, Vm, ram_copies),
    mnesia:add_table_copy(service_def, Vm, ram_copies),
    mnesia:add_table_copy(vm, Vm, ram_copies),
    Tables=mnesia:system_info(tables),
    mnesia:wait_for_tables(Tables,?WAIT_FOR_TABLES),
    io:format(" ~p~n",[{?MODULE,?LINE,mnesia:system_info()}]),
    timer:sleep(5000),
  %  add_extra_nodes([Vm]),
  %  mnesia:change_config(extra_db_nodes, [Vm]) ,
  %  Tables=mnesia:system_info(tables),
  %  mnesia:wait_for_tables(Tables,?WAIT_FOR_TABLES),
  %  rpc:call(Vm,mnesia,change_config,[extra_db_nodes, [Vm]]) ,
  %  Tables=rpc:call(Vm,mnesia,system_info,[tables]),
  %  rpc:call(Vm,mnesia,wait_for_tables,[Tables,?WAIT_FOR_TABLES]),
    ok.
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

db_init([])->
    % Update with tables
    mnesia:create_table(computer,[{attributes, record_info(fields, computer)}]),
    mnesia:create_table(deployment,[{attributes, record_info(fields,deployment)}]),
    mnesia:create_table(deployment_spec,[{attributes, record_info(fields,deployment_spec)}]),
    mnesia:create_table(passwd,[{attributes, record_info(fields,passwd)}]),
    mnesia:create_table(sd,[{attributes, record_info(fields,sd)}]),
    mnesia:create_table(service_def,[{attributes, record_info(fields,service_def)}]),
    mnesia:create_table(vm,[{attributes, record_info(fields,vm)}]),    
    ok;

db_init(AllNodes)->
    io:format(" ~p~n",[{?MODULE,?LINE,AllNodes}]),
    add_extra_nodes(AllNodes).

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

add_extra_nodes([])->
    ok;
add_extra_nodes([Node|T])->
    case mnesia:change_config(extra_db_nodes, [Node]) of
	{ok,[Node]}->
	    io:format(" ~p~n",[{?MODULE,?LINE,node()}]),
	    mnesia:add_table_copy(schema, node(),ram_copies),
	    % Update with tables
	    mnesia:add_table_copy(computer, node(), ram_copies),
	    mnesia:add_table_copy(deployment, node(), ram_copies),
	    mnesia:add_table_copy(deployment_spec, node(), ram_copies),
	    mnesia:add_table_copy(passwd, node(), ram_copies),
	    mnesia:add_table_copy(sd, node(), ram_copies),
	    mnesia:add_table_copy(service_def, node(), ram_copies),
	    mnesia:add_table_copy(vm, node(), ram_copies),
	    Tables=mnesia:system_info(tables),
	    mnesia:wait_for_tables(Tables,?WAIT_FOR_TABLES);
	_ ->
	    io:format(" ~p~n",[{?MODULE,?LINE,node()}]),
	    add_extra_nodes(T)
    end.
