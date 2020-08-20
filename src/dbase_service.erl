%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc : represent a logical vm  
%%% 
%%% Supports the system with standard erlang vm functionality, load and start
%%% of an erlang application (downloaded from git hub) and "dns" support 
%%% 
%%% Make and start the board start SW.
%%%  boot_service initiates tcp_server and l0isten on port
%%%  Then it's standby and waits for controller to detect the board and start to load applications
%%% 
%%%     
%%% -------------------------------------------------------------------
-module(dbase_service). 

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("infra.hrl").
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{}).


	  
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================

-export([create_schema/1,
	 start_mnesia/1,
	 create_table/0,
	 insert/2,
	 read/2,
	 read_table/1
	]).

%% server interface
-export([ping/0
	]).




-export([start/0,
	 stop/0
	 ]).
%% internal 
%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================


%% Asynchrounus Signals
%boot_strap()->
 %   PortStr=atom_to_list(PortArg),
 %   Port=list_to_integer(PortStr),
   % application:set_env([{boot_service,{port,Port}}]),
%    application:start(boot_service).
	
%% Gen server function

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).


%%----------------------------------------------------------------------


ping()->
    gen_server:call(?MODULE,{ping},infinity).


start_mnesia(NodeList)->
    gen_server:call(?MODULE,{start_mnesia,NodeList},infinity).

create_schema(NodeList)->
    gen_server:call(?MODULE,{create_schema,NodeList},infinity).
create_table()->
    gen_server:call(?MODULE,{create_table},infinity).

insert(Table,Info)->
    gen_server:call(?MODULE,{insert,Table,Info},infinity).
read(Table,Key)->
    gen_server:call(?MODULE,{read,Table,Key},infinity).
read_table(Table)->
    gen_server:call(?MODULE,{read_table,Table},infinity).
%%___________________________________________________________________



%%-----------------------------------------------------------------------


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%
%% --------------------------------------------------------------------
init([]) ->
					   
    {ok, #state{}}.
    
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (aterminate/2 is called)
%% --------------------------------------------------------------------
handle_call({start_mnesia,NodeList}, _From, State) ->
    Reply=[rpc:call(Node,application,start,[mnesia])||Node<-NodeList],
    {reply, Reply, State};

handle_call({create_schema,NodeList}, _From, State) ->
    Reply=mnesia:create_schema(NodeList),
    {reply, Reply, State};


handle_call({create_table}, _From, State) ->
    mnesia:create_table(service_discovery,
			[{type,bag},
			 {attributes,record_info(fields,service_discovery)}]),
    
    mnesia:create_table(catalog,
			      [{attributes,record_info(fields,catalog)}]),
   
    mnesia:create_table(nodes,
			      [{attributes,record_info(fields,nodes)}]),

    mnesia:create_table(deployment,
			[{type,bag},
			 {attributes,record_info(fields,deployment)}]),

    mnesia:create_table(log,
			[{type,bag},
			 {attributes,record_info(fields,log)}]),
    
    Reply=ok,
    {reply, Reply, State};


handle_call({insert,Table,Info}, _From, State) ->
    Reply=case rpc:call(node(),dbase_lib,create_item,[Table,Info]) of
	      {badrpc,Err}->
		  {error,[?MODULE,?LINE,Err,Table,Info]};
	      InfoToStore->
		  Fun=fun()->
			      mnesia:write(InfoToStore)
		      end,
		  mnesia:transaction(Fun),
		  ok
	  end,
    {reply, Reply, State};

handle_call({read,Table,Key}, _From, State) ->
    Reply = case mnesia:dirty_read(Table,Key) of
		[]->
		    {error,[not_found,Table,Key]};
		R->
		    R
	    end,
    {reply, Reply, State};

handle_call({read_table,Table}, _From, State) ->
    CatchAll = [{'_',[],['$_']}],
    Reply=mnesia:dirty_select(Table, CatchAll),
    {reply, Reply, State};

handle_call({stop_service,ServiceId}, _From, State) ->
    Reply=rpc:call(node(),loader,stop,[ServiceId]),
    {reply, Reply, State};

handle_call({ping}, _From, State) ->
    Reply={pong,node(),?MODULE},
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,?LINE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast({glurk}, State) ->

    {noreply, State};

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)

handle_info(Info, State) ->
    io:format("unmatched match info ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
