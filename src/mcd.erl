%%% 
%%% Copyright (c) 2007, 2008, 2009 JackNyfe, Inc. <info@jacknyfe.com>
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%% 1. Redistributions of source code must retain the above copyright
%%%    notice, this list of conditions and the following disclaimer.
%%% 2. Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
%%% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
%%% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.
%%% 
%%% This module uses memcached protocol to interface memcached daemon:
%%% http://code.sixapart.com/svn/memcached/trunk/server/doc/protocol.txt
%%%
%%% EXPORTS:
%%%     mcd:start_link()
%%%     mcd:start_link([Address])
%%%     mcd:start_link([Address, Port])
%%%
%%%  Simple API:
%%% 	mcd:get(Key)
%%% 	mcd:get(ServerRef, Key)
%%% 	mcd:set(Key, Data)
%%% 	mcd:set(ServerRef, Key, Data)
%%%
%%%   mcd:incr(Key, Increment, DefaultValue)
%%%   mcd:incr(ServerRef, Key, Increment, DefaultValue)
%%%  Use Increment = 0 to get value or to set default if key does not exist.
%%%
%%%   mcd:decr(Key, Decrement)
%%%   mcd:decr(ServerRef, Key, Decrement)
%%%
%%%  Generic API:
%%% 	mcd:do(ServerRef, SimpleRequest)
%%% 	mcd:do(ServerRef, KeyRequest, Key)
%%% 	mcd:do(ServerRef, KeyDataRequest, Key, Data)
%%%	Type
%%%             ServerRef = as defined in gen_server(3)
%%%		SimpleRequest = version | flush_all | {flush_all, Expiration}
%%%		KeyRequest = get | delete | {delete, Time}
%%%		KeyDataRequest = Command | {Command, Flags, Expiration}
%%%		Command = set | add | replace
%%%   Increment = int()>=0
%%%   Decrement = int()>=0
%%%   DefaultValue = int()>=0
%%%
%%% Client may also use gen_server IPC primitives to request this module to
%%% perform storage and retrieval. Primitives are described in gen_server(3),
%%% that is, gen_server:call, gen_server:cast and others, using ServerRef
%%% returned by start_link(). Example: gen_server:cast(Server, Query).
%%%
%%% Recognized queries:
%%%   {Command, Key, Data}
%%%   {Command, Key, Data, Flags, Expiration}
%%%   {get, Key}
%%%   {delete, Key}
%%%   {delete, Key, Time}
%%%   {incr, Key, Value}	% not implemented yet
%%%   {decr, Key, Value}	% not implemented yet
%%%   {version}
%%%   {flush_all}
%%%   {flush_all, Expiration}
%%% Return values:
%%%   {ok, Data}
%%%   {error, Reason}
%%% Where:
%%%   Command: set | add | replace
%%%   Key: term()
%%%   Data: term()
%%%   Flags: int()>=0
%%%   Expiration: int()>=0
%%%   Value: int()>=0
%%%   Time: int()>=0
%%%   Reason: notfound | overload | noconn | flushed
%%% 
-module(mcd).
-behavior(gen_server).

-compile({no_auto_import,[monitor/3]}).

-export([start_link/0, start_link/1, start_link/2]).
-export([do/2, do/3, do/4]).

-export([ldo/1, ldo/2, ldo/3, ldo/5]).	%% do('localmcd', ...)
-export([get/1, get/2, set/2, set/3, set/5, async_set/3, async_set/5]).
-export([set_raw_integer/2, set_raw_integer/3, get_raw_integer/1, get_raw_integer/2]).
-export([incr/2, incr/3, decr/2, decr/3]).
-export([flush/0, flush/1]).
-export([monitor/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Start an anymous gen_server attached to a specified real memcached server.
%% Assumes localhost:11211 if no server address is given.
%%
start_link() -> start_link([]).
start_link([]) -> start_link(["127.0.0.1"]);
start_link([Address]) -> start_link([Address, 11211]);
start_link([Address, Port]) ->
	gen_server:start_link(?MODULE, [Address, Port], []).

%%
%% Start a named gen_server attached to a specified real memcached server.
%% Assumes localhost:11211 if no server address is given.
%%
start_link(Name, []) -> start_link(Name, ["127.0.0.1"]);
start_link(Name, [Address]) -> start_link(Name, [Address, 11211]);
start_link(Name, [Address, Port]) when is_atom(Name) ->
	gen_server:start_link({local, Name}, ?MODULE, [Address, Port], []).

%%
%% Call the specified memcached client gen_server with a request to ask
%% something from the associated real memcached process.
%%
%% The do/{2,3,4} is lighter than direct gen_server:call() to memcached
%% gen_server, since it spends some CPU in the requestor processes instead.
%%
%% See the file banner for possible requests.
%%
do(ServerRef, SimpleRequest) when is_atom(SimpleRequest) ->
	do_forwarder(call, ServerRef, {SimpleRequest});
do(ServerRef, SimpleRequest) when is_tuple(SimpleRequest) ->
	do_forwarder(call, ServerRef, SimpleRequest).

do(ServerRef, KeyRequest, Key) when is_atom(KeyRequest) ->
	do_forwarder(call, ServerRef, {KeyRequest, Key});
do(ServerRef, {KeyRequest}, Key) ->
	do_forwarder(call, ServerRef, {KeyRequest, Key}).

do(ServerRef, KeyDataReq, Key, Data) when is_atom(KeyDataReq) ->
	do_forwarder(call, ServerRef, {KeyDataReq, Key, Data});
do(ServerRef, {Cmd}, Key, Data) ->
	do_forwarder(call, ServerRef, {Cmd, Key, Data});
do(ServerRef, {Cmd, Flag, Expires}, Key, Data) ->
	do_forwarder(call, ServerRef, {Cmd, Key, Data, Flag, Expires}).

-define(LOCALMCDNAME, localmcd).
%%
%% The "ldo" is a "local do()". In our setup we assume that there is at least
%% one shared memcached running on the local host, named 'localmcd' (started by
%% an application supervisor process).
%% This call helps to avoid writing the mcd:do(localmcd, ...) code,
%% where using 'localmcd' string is prone to spelling errors.
%%
ldo(A) -> do(?LOCALMCDNAME, A).
ldo(A, B) -> do(?LOCALMCDNAME, A, B).
ldo(A, B, C) -> do(?LOCALMCDNAME, A, B, C).
ldo(set, Key, Data, Flag, Expires) ->
        do(?LOCALMCDNAME, {set, Flag, Expires}, Key, Data).

%% These helper functions provide more self-evident API.
get(Key) -> do(?LOCALMCDNAME, get, Key).
get(ServerRef, Key) -> do(ServerRef, get, Key).

set(Key, Data) -> do(?LOCALMCDNAME, set, Key, Data).
set(ServerRef, Key, Data) -> do(ServerRef, set, Key, Data).
set(ServerRef, Key, Data, Flags, Expiration) when is_integer(Flags), is_integer(Expiration), Flags >= 0, Flags < 65536, Expiration >= 0 -> do(ServerRef, {set, Flags, Expiration}, Key, Data).

set_raw_integer(Key, Value)->set_raw_integer(?LOCALMCDNAME, Key, Value).
set_raw_integer(ServerRef, Key, Value)when is_integer(Value), Value >= 0 ->
    do(ServerRef, {set_raw_integer, Key, Value}).

get_raw_integer(Key)->get_raw_integer(?LOCALMCDNAME, Key).
get_raw_integer(ServerRef, Key)->
    do(ServerRef, {incr, Key, 0}).

incr(Key, Value)->incr(?LOCALMCDNAME, Key, Value).
incr(ServerRef, Key, Value) when is_integer(Value), Value >=0 ->
    do(ServerRef, {incr, Key, Value}).

decr(Key, Value)->decr(?LOCALMCDNAME, Key, Value).
decr(ServerRef, Key, Value)->    
    do(ServerRef, {decr, Key, Value}).

flush()->flush(?LOCALMCDNAME).
flush(ServerRef)->
    do(ServerRef, {flush_all}).    

async_set(ServerRef, Key, Data) ->
	do_forwarder(cast, ServerRef, {set, Key, Data}),
	Data.
async_set(ServerRef, Key, Data, Flags, Expiration) when is_integer(Flags), is_integer(Expiration), Flags >= 0, Flags < 65536, Expiration >= 0 ->
	do_forwarder(cast, ServerRef, {set, Key, Data, Flags, Expiration}),
	Data.

%%
%% Enroll a specified monitoring process (MonitorPid) to receive
%% notifications about memcached state transitions and other anomalies.
%% This call sets or replaces the previous set of items to monitor for.
%%
%% @spec monitor(ServerRef, MonitorPid, MonitorItems)
%% Type MonitorPid = pid() | atom()
%%      MonitorItems = [MonitorItem]
%%      MonitorItem = state | overload
%%
monitor(ServerRef, MonitorPid, MonitorItems) when is_list(MonitorItems) ->
	gen_server:call(ServerRef, {set_monitor, MonitorPid, MonitorItems});
monitor(ServerRef, MonitorPid, MonitorItem) when is_atom(MonitorItem) ->
	?MODULE:monitor(ServerRef, MonitorPid, [MonitorItem]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, { address, port = 11211, socket = nosocket,
	receiver,		% data receiver process
	requests = 0,		% client requests received
	outstanding = 0,	% client requests outstanding
	anomalies = {0, 0, 0},	% {queue overloads, reconnects, unused}
	status = disabled,	% connection status:
				%   disabled | ready
				%   | {connecting, Since, {Pid,MRef}}
				%   | {testing, Since}	% testing protocol
				%   | {wait, Since}	% wait between connects
	monitored_by = []	% monitoring processes to receive anomalies
	}).


init([Address, Port]) ->
	{ ok, reconnect(#state{
			address = Address,
			port = Port,
			receiver = start_data_receiver(self())
		})
	}.

start_data_receiver(Parent) ->
	spawn_monitor(fun() ->
		ParentMon = erlang:monitor(process, Parent),
		data_receiver_loop(Parent, ParentMon, undefined)
	end).

handle_call(status, _From, State) ->
	#state{requests = QTotal, outstanding = QOut, anomalies = {QOV, REC, _},
		status = Status} = State,
	{reply,
		[{requests, QTotal}, {outstanding, QOut}, {overloads, QOV},
		{reconnects, REC}, {status, Status}],
	State};
handle_call({set_monitor, MonitorPid, Items}, _From, #state{monitored_by=CurMons} = State) ->
	MonRef = erlang:monitor(process, MonitorPid),
	NewMons = addMonitorPidItems(demonitorPid(CurMons, MonitorPid),
			MonitorPid, MonRef, Items),
	MonitoredItemsForPid = collectMonitoredItems(NewMons, MonitorPid),
	case MonitoredItemsForPid of
		[] -> erlang:demonitor(MonRef);
		_ -> ok
	end,
	{reply, MonitoredItemsForPid, State#state{monitored_by = NewMons}};

handle_call(Query, From, State) -> {noreply, scheduleQuery(State, Query, From)}.

handle_cast({connected, Pid, nosocket},
		#state{socket = nosocket,
			status = {connecting, _, {Pid,_}}} = State) ->
	{Since, ReconnectDelay} = compute_next_reconnect_delay(State),
	erlang:start_timer(ReconnectDelay, self(), { may, reconnect }),
	{noreply, State#state { status = {wait, Since} }};
handle_cast({connected, Pid, NewSocket},
		#state{socket = nosocket,
			receiver = {RcvrPid, _},
			status = {connecting, _, {Pid,_}}} = State) ->

	RcvrPid ! {switch_receiving_socket, self(), NewSocket},

	{Since, ReconnectDelay} = compute_next_reconnect_delay(State),

	ReqId = State#state.requests,

	% We ask for version information, which will set our status to ready
	{Socket, NewStatus} = case constructAndSendQuery(
				{self(), {connection_tested, NewSocket}},
				{version},
				NewSocket, State#state.receiver) of
		ok -> {NewSocket, {testing, Since}};
		{ error, _ } ->
			gen_tcp:close(NewSocket),
			erlang:start_timer(ReconnectDelay, self(),
				{ may, reconnect }),
			{nosocket, {wait, Since}}
	end,

	% Remember this socket in a new state.
	{noreply, State#state { socket = Socket,
		status = NewStatus,
		requests = ReqId + 1,
		outstanding = 1
		}};

handle_cast({connected, _, nosocket}, State) -> {noreply, State};
handle_cast({connected, _, Socket}, State) ->
	gen_tcp:close(Socket),
	{noreply, State};
handle_cast(Query, State) -> {noreply, scheduleQuery(State, Query, anon)}.

handle_info({request_served, Socket}, #state{socket=Socket, outstanding=QOut}=State) -> {noreply, State#state{outstanding=QOut - 1}};
handle_info({{connection_tested, Socket}, {ok, _Version}}, #state{socket = Socket, status = {testing, _}} = State) ->
	{noreply, State#state{status = ready}};
handle_info({timeout, _, {may, reconnect}}, State) -> {noreply, reconnect(State)};
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
	{noreply, reconnect(State#state{socket = nosocket})};
handle_info({'DOWN', MonRef, process, Pid, _Info}, #state{status={connecting,_,{Pid,MonRef}}}=State) ->
	error_logger:info_msg("Memcached connector died (~p),"
			" simulating nosock~n", [_Info]),
	handle_cast({connected, Pid, nosocket}, State);
handle_info({'DOWN', MonRef, process, Pid, _Info}, #state{receiver={Pid,MonRef}}=State) ->
	{stop, {receiver_down, _Info}, State};
handle_info({'DOWN', MonRef, process, Pid, _Info}, #state{monitored_by=Mons}=State) ->
	{noreply, State#state{
		monitored_by = removeMonitorPidAndMonRef(Mons, Pid, MonRef)
		} };
handle_info(_Info, State) ->
	error_logger:info_msg("Some info: ~p~n", [_Info]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Remove the specified pid from the lists of nodes monitoring this gen_server.
demonitorPid(Monitors, MonitorPid) ->
	[{Item, NewPids}
		|| {Item, PidRefs} <- Monitors,
		   NewPids <- [[PM || {P, MonRef} = PM <- PidRefs,
				erlang:demonitor(MonRef) == true,
				P /= MonitorPid]],
		   NewPids /= []
	].

removeMonitorPidAndMonRef(Monitors, Pid, MonRef) ->
	[{Item, NewPids}
		|| {Item, PidRefs} <- Monitors,
		   NewPids <- [[PM || {P, MR} = PM <- PidRefs,
				P /= Pid, MR /= MonRef]],
		   NewPids /= []
	].

% Add the specified pid to the lists of nodes monitoring this gen_server.
addMonitorPidItems(Monitors, MonitorPid, MonRef, Items) ->
	lists:foldl(fun(Item, M) ->
		addMonitorPidItem(M, MonitorPid, MonRef, Item)
	end, Monitors, Items).

addMonitorPidItem(Monitors, Pid, MonRef, I) when I == state; I == overload ->
	NewMons = [{Item, NewPids}
		|| {Item, Pids} <- Monitors,
		   NewPids <- [case Item of
				I -> [{Pid, MonRef} | Pids];
				_ -> Pids
				end]
	],
	case lists:keysearch(I, 1, NewMons) of
		false -> [{I, [{Pid, MonRef}]}|NewMons];
		{value, _} -> NewMons
	end;
addMonitorPidItem(Monitors, _Pid, _MonRef, _Item) -> Monitors.

reportEvent(#state{monitored_by = Mons} = State, Event, Info) ->
	[P ! {memcached, self(), Event, Info}
		|| {Item, Pids} <- Mons, Item == Event, {P, _} <- Pids],
	State.

% Figure out what items this pid monitors.
collectMonitoredItems(Monitors, MonitorPid) ->
	[Item || {Item, Pids} <- Monitors,
		lists:keysearch(MonitorPid, 1, Pids) /= false].

% @spec utime(now()) -> int()
utime({Mega, Secs, _}) -> 1000000 * Mega + Secs.

incrAnomaly({QOverloads, Reconnects, Unused}, overloads) ->
	{QOverloads + 1, Reconnects, Unused};
incrAnomaly({QOverloads, Reconnects, Unused}, reconnects) ->
	{QOverloads, Reconnects + 1, Unused};
incrAnomaly(Anomaly, FieldName) ->
	error_logger:error_msg("Anomaly ~p couldn't be increased in ~p~n",
		[FieldName, Anomaly]),
	Anomaly.

%% Destroy the existing connection and create a new one based on State params
%% @spec reconnect(record(state)) -> record(state)
reconnect(#state{status = {connecting, _, {_Pid,_MRef}}} = State) ->
	% Let the reconnect process continue.
	State;
reconnect(#state{address = Address, port = Port, socket = OldSock} = State) ->
	% Close the old socket, if available
	case OldSock of
		nosocket -> ok;
		_ -> gen_tcp:close(OldSock)
	end,

	Self = self(),
	{Pid,MRef} = spawn_monitor(fun() ->
			reconnector_process(Self, Address, Port) end),

	% We want to reconnect but we can't do it immediately, since
	% the tcp connection could be failing right after connection attempt.
	% So let it cook for a period of time before the next retry.
	{Since, _ReconnectDelay} = compute_next_reconnect_delay(State),

	NewAnomalies = case is_atom(State#state.status) of
		false -> State#state.anomalies;
		true -> 
			reportEvent(State, state, down),
			incrAnomaly(State#state.anomalies, reconnects)
	end,

	State#state { socket = nosocket,
		status = {connecting, Since, {Pid, MRef}},
		outstanding = 0,
		anomalies = NewAnomalies }.

compute_next_reconnect_delay(#state{status = Status}) ->
	ComputeReconnectDelay = fun(Since) ->
		% Wait increasingly longer,
		% but no longer than 5 minutes.
		case (utime(now()) - utime(Since)) of
			N when N > 300 -> 300 * 1000;
			N -> N * 1000
		end
	end,
	case Status of
		{connecting, Since, _} -> {Since, ComputeReconnectDelay(Since)};
		{testing, Since} -> {Since, ComputeReconnectDelay(Since)};
		{wait, Since} -> {Since, ComputeReconnectDelay(Since)};
		_ -> {now(), 1000}
	end.

reconnector_process(MCDServerPid, Address, Port) ->
	error_logger:info_msg("Creating interface ~p to memcached on ~p:~p~n",
          [MCDServerPid, Address,Port]),

	Socket = case gen_tcp:connect(Address, Port,
			[{packet, line}, binary, {active, false}], 5000) of
		{ ok, Sock } ->
			gen_tcp:controlling_process(Sock, MCDServerPid),
			Sock;
		{ error, _Reason } -> nosocket
	end,
	gen_server:cast(MCDServerPid, {connected, self(), Socket}).


%%
%% Send a query to the memcached server and add it to our local table
%% to capture corresponding server response.
%% This asynchronous process provides necessary pipelining for remote or
%% lagging memcached processes.
%%

scheduleQuery(#state{requests = QTotal, outstanding = QOut, receiver = Rcvr, socket = Socket, status = ready} = State, Query, From) when QOut < 1024 ->
	case constructAndSendQuery(From, Query, Socket, Rcvr) of
		ok -> State#state{requests = QTotal+1, outstanding = QOut+1};
		{error, _Reason} -> reconnect(State)
	end;
scheduleQuery(State, _Query, From) ->
	#state{outstanding = QOut, anomalies = An, status = Status} = State,
	if
		QOut >= 1024 ->
			replyBack(From, {error, overload}),
			reportEvent(State, overload, []),
			State#state{anomalies = incrAnomaly(An, overloads)};
		Status =/= ready ->
			replyBack(From, {error, noconn}),
			State
	end.

constructAndSendQuery(From, {'$constructed_query', _KeyMD5, {OTARequest, ReqType, ExpectationFlags}}, Socket, {RcvrPid, _}) ->
	RcvrPid ! {accept_response, From, ReqType, ExpectationFlags},
	gen_tcp:send(Socket, OTARequest);
constructAndSendQuery(From, Query, Socket, {RcvrPid, _}) ->
	{_MD5Key, OTARequest, ReqType} = constructMemcachedQuery(Query),
	RcvrPid ! {accept_response, From, ReqType, []},
	gen_tcp:send(Socket, OTARequest).

%%
%% Format the request and call the server synchronously
%% or cast a message asynchronously, without waiting for the result.
%%
do_forwarder(Method, ServerRef, Req) ->
	{KeyMD5, IOL, T} = constructMemcachedQuery(Req),
	Q = iolist_to_binary(IOL),
	case gen_server:Method(ServerRef,
			{'$constructed_query', KeyMD5, {Q, T, [raw_blob]}}) of

		% Return the actual Data piece which got stored on the
		% server. Since returning Data happens inside the single
		% process, this has no copying overhead and is nicer than
		% returning {ok, stored} to successful set/add/replace commands.
		{ok, stored} when T == rtCmd -> {ok, element(3, Req)};

		% Memcached returns a blob which needs to be converted
		% into to an Erlang term. It's better to do it in the requester
		% process space to avoid inter-process copying of potentially
		% complex data structures.
		{ok, {'$value_blob', B}} -> 
          try
               {ok, binary_to_term(B)}
          catch
               _E:_R ->
                   {error, "response is not erlang term"}
          end;
      Response -> Response
	end.

%% Convert arbitrary Erlang term into memcached key
%% @spec md5(term()) -> binary()
%% @spec b64(binary()) -> binary()
md5(Key) -> erlang:md5(term_to_binary(Key)).
b64(Key) -> base64:encode(Key).

%% Translate a query tuple into memcached protocol string and the
%% atom suggesting a procedure for parsing memcached server response.
%%
%% @spec constructMemcachedQuery(term()) -> {md5(), iolist(), ResponseKind}
%% Type ResponseKind = atom()
%%
constructMemcachedQuery({version}) -> {<<>>, [<<"version\r\n">>], rtVer};
constructMemcachedQuery({set, Key, Data}) ->
	constructMemcachedQueryCmd("set", Key, Data);
constructMemcachedQuery({set, Key, Data, Flags, Expiration}) ->
	constructMemcachedQueryCmd("set", Key, Data, Flags, Expiration);
constructMemcachedQuery({add, Key, Data}) ->
	constructMemcachedQueryCmd("add", Key, Data);
constructMemcachedQuery({add, Key, Data, Flags, Expiration}) ->
	constructMemcachedQueryCmd("add", Key, Data, Flags, Expiration);
constructMemcachedQuery({replace, Key, Data}) ->
	constructMemcachedQueryCmd("replace", Key, Data);
constructMemcachedQuery({replace, Key, Data, Flags, Expiration}) ->
	constructMemcachedQueryCmd("replace", Key, Data, Flags, Expiration);
constructMemcachedQuery({get, Key}) ->
	MD5Key = md5(Key),
	{MD5Key, ["get ", b64(MD5Key), "\r\n"], rtGet};
constructMemcachedQuery({delete, Key, Time}) when is_integer(Time), Time > 0 ->
	MD5Key = md5(Key),
	{MD5Key, ["delete ", b64(MD5Key), " ", integer_to_list(Time), "\r\n"], rtDel};
constructMemcachedQuery({delete, Key}) ->
	MD5Key = md5(Key),
	{MD5Key, ["delete ", b64(MD5Key), "\r\n"], rtDel};
constructMemcachedQuery({set_raw_integer, Key, Value})
    when is_integer(Value), Value >= 0 ->
  MD5Key = md5(Key),
  Raw = integer_to_list(Value),
  {MD5Key, ["set ", b64(MD5Key), " 0 0 ", integer_to_list(length(Raw)), "\r\n", 
            Raw , "\r\n"], rtCmd};
constructMemcachedQuery({incr, Key, Value})
		when is_integer(Value), Value >= 0 ->
	MD5Key = md5(Key),
	{MD5Key, ["incr ", b64(MD5Key), " ", integer_to_list(Value), "\r\n"], rtInt};
constructMemcachedQuery({decr, Key, Value})
		when is_integer(Value), Value >= 0 ->
	MD5Key = md5(Key),
	{MD5Key, ["decr ", b64(MD5Key), " ", integer_to_list(Value), "\r\n"], rtInt};
constructMemcachedQuery({flush_all, Expiration})
		when is_integer(Expiration), Expiration >= 0 ->
	{<<>>, ["flush_all ", integer_to_list(Expiration), "\r\n"], rtFlush};
constructMemcachedQuery({flush_all}) -> {<<>>, ["flush_all\r\n"], rtFlush}.

%% The "set", "add" and "replace" queries do get optional
%% "flag" and "expiration time" attributes. So these commands fall into
%% their own category of commands (say, ternary command). These commads'
%% construction is handled by this function.
%%
%% @spec constructMemcachedQuery(term()) -> {md5(), iolist(), ResponseKind}
%% Type ResponseKind = atom()
%%
constructMemcachedQueryCmd(Cmd, Key, Data) ->
	constructMemcachedQueryCmd(Cmd, Key, Data, 0, 0).
constructMemcachedQueryCmd(Cmd, Key, Data, Flags, Exptime)
	when is_list(Cmd), is_integer(Flags), is_integer(Exptime),
	Flags >= 0, Flags < 65536, Exptime >= 0 ->
	BinData = term_to_binary(Data),
	MD5Key = md5(Key),
	{MD5Key, [Cmd, " ", b64(MD5Key), " ", integer_to_list(Flags), " ",
		integer_to_list(Exptime), " ",
		integer_to_list(size(BinData)),
		"\r\n", BinData, "\r\n"], rtCmd}.

replyBack(anon, _) -> true;
replyBack(From, Result) -> gen_server:reply(From, Result).

data_receiver_loop(Parent, ParentMon, Socket) ->
	NewSocket = receive
	  {accept_response, RequestorFrom, Operation, Opts} when Socket /= undefined ->
		try data_receiver_accept_response(Operation, Opts, Socket) of
		  Value ->
			Parent ! {request_served, Socket},
			replyBack(RequestorFrom, Value),
			Socket
		catch
		  error:{badmatch,{error,_}} ->
			Parent ! {tcp_closed, Socket},
			replyBack(RequestorFrom, {error, noconn}),
			undefined
		end;
	  {accept_response, RequestorFrom, _, _} ->
		replyBack(RequestorFrom, {error, noconn}),
		Socket;
	  {switch_receiving_socket, Parent, ReplaceSocket} ->
		ReplaceSocket;
	  {'DOWN', ParentMon, process, Parent, _} -> exit(normal);
	  _Message -> Socket
	end,
	data_receiver_loop(Parent, ParentMon, NewSocket).

data_receiver_accept_response(rtVer, _, Socket) ->
	["VERSION", Value | _] = data_receive_string_tokens(Socket),
	{ok, Value};
data_receiver_accept_response(rtGet, ExpFlags, Socket) ->
	{ok, HeaderLine} = gen_tcp:recv(Socket, 0),
	case HeaderLine of
	  % Quick test before embarking on tokenizing
	  <<"END\r\n">> -> {error, notfound};
	  _ ->
		["VALUE", _Value, _Flag, DataSizeStr]
			= string:tokens(binary_to_list(HeaderLine), " \r\n"),
		ok = inet:setopts(Socket, [{packet, raw}]),
		Bin = data_receive_binary(Socket, list_to_integer(DataSizeStr)),
		<<"\r\nEND\r\n">> = data_receive_binary(Socket, 7),
		ok = inet:setopts(Socket, [{packet, line}]),
		case proplists:get_value(raw_blob, ExpFlags) of
			true -> {ok, {'$value_blob', Bin}};
			_ -> {ok, binary_to_term(Bin)}
		end
	end;
data_receiver_accept_response(rtInt, _, Socket) ->
	{ok, Response} = gen_tcp:recv(Socket, 0),
	case string:to_integer(binary_to_list(Response)) of
	  {Int, "\r\n"} when is_integer(Int) -> {ok, Int};
	  {error, _} when Response == <<"NOT_FOUND\r\n">> -> {error, notfound};
	  {error, _} -> data_receiver_error_reason(Response)
	end;
data_receiver_accept_response(rtCmd, _, Socket) ->
	data_receiver_accept_choice(Socket,
		[ {<<"STORED\r\n">>, {ok, stored}},
		  {<<"NOT_STORED\r\n">>, {error, notstored}} ]);
data_receiver_accept_response(rtDel, _, Socket) ->
	data_receiver_accept_choice(Socket,
		[ {<<"DELETED\r\n">>, {ok, deleted}},
		  {<<"NOT_FOUND\r\n">>, {error, notfound}} ]);
data_receiver_accept_response(rtFlush, _, Socket) ->
	data_receiver_accept_choice(Socket, [ {<<"OK\r\n">>, {ok, flushed}} ]).

data_receiver_accept_choice(Socket, Alternatives) ->
	{ok, Response} = gen_tcp:recv(Socket, 0),
	case lists:keysearch(Response, 1, Alternatives) of
		{value, {_, Answer}} -> Answer;
		false -> data_receiver_error_reason(Response)
	end.

data_receive_binary(Socket, DataSize) when is_integer(DataSize) ->
	{ok, Binary} = gen_tcp:recv(Socket, DataSize),
	Binary.

data_receive_string_tokens(Socket) ->
	{ok, Line} = gen_tcp:recv(Socket, 0),
	String = binary_to_list(Line),
	string:tokens(String, " \r\n").

data_receiver_error_reason(<<"SERVER_ERROR ", Reason/binary>>) ->
	data_receiver_error_reason(server_error, Reason);
data_receiver_error_reason(<<"CLIENT_ERROR ", Reason/binary>>) ->
	data_receiver_error_reason(client_error, Reason).

data_receiver_error_reason(Code, Reason) ->
	{error, {Code, [C || C <- binary_to_list(Reason), C >= $ ]}}.

