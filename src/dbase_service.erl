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
%-include("infra.hrl").
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{}).

-define(Master,"asus").
-define(MnesiaNodes,['mnesia@sthlm_1','mnesia@asus']).
%-define(MnesiaNodes,['mnesia@asus']).
	  
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================


%% server interface
-export([create_schema/0,
	 delete_schema_file/0,
	 load_textfile/2,
	 load_textfile/1,
	 ping/0	 
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

%C="https://"++Uid++":"++Pwd++"@github.com/"++Uid++"/"++SId++".git".

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
create_schema()->
    gen_server:call(?MODULE,{create_schema},infinity).

delete_schema_file()->
    gen_server:call(?MODULE,{delete_schema_file},infinity).

load_textfile(Filename,Bin)->    
    gen_server:call(?MODULE,{load_textfile,Filename,Bin},infinity).
load_textfile(FileName)->    
    gen_server:call(?MODULE,{load_textfile,FileName},infinity).
    
ping()->
    gen_server:call(?MODULE,{ping},infinity).

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
    case net_adm:localhost() of
	?Master->
	    io:format("~p~n",[{?MODULE,?LINE,mnesia:create_schema(?MnesiaNodes)}]),
	    [rpc:call(Node,application,stop,[mnesia])||Node<-?MnesiaNodes],   
	    [rpc:call(Node,application,start,[mnesia])||Node<-?MnesiaNodes];    
	_ ->
	    ok
    end,
    
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

handle_call({ping}, _From, State) ->
    Reply={pong,node(),?MODULE},
    {reply, Reply, State};

handle_call({create_schema}, _From, State) ->
    Reply=mnesia:create_schema([?MnesiaNodes]),
    [rpc:call(Node,application,stop,[mnesia])||Node<-?MnesiaNodes],   
    [rpc:call(Node,application,start,[mnesia])||Node<-?MnesiaNodes],
    {reply, Reply, State};

handle_call({delete_schema_file}, _From, State) ->
    Reply=os:cmd("rm -rf Mne*"),
    {reply, Reply, State};

handle_call({load_textfile,FileName}, _From, State) ->
    Reply=mnesia:load_textfile(FileName),
 %   file:delete(Filename),
    {reply, Reply, State};

handle_call({load_textfile,Filename,Bin}, _From, State) ->
    file:delete(Filename),
    ok=file:write_file(Filename,Bin),
    Reply=mnesia:load_textfile(Filename),
 %   file:delete(Filename),
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
