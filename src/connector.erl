-module(connector).

-export([create/2, connect/2, disconnect/1, discard/1]).
-export([get_connected/1, get_ResInst/1, get_type/1]).

-export([init/2, test/0]). % for internal use only. 

-spec create(pid(), pid() | atom()) -> pid().
create(ResInst_Pid, ConnectTyp_Pid) -> 
	spawn(?MODULE, init, [ResInst_Pid, ConnectTyp_Pid]).

-spec init(pid(), pid()) -> 'stopped'.
init(ResInst_Pid, ConnectTyp_Pid) -> 
	survivor:entry(connector_created), 
	loop(ResInst_Pid, disconnected, ConnectTyp_Pid).

-spec connect(pid(), pid()) -> {'connect', pid()}.
connect(Connector_Pid, C_Pid) ->
	Connector_Pid ! {connect, C_Pid}.

-spec disconnect(pid()) -> 'disconnect'.
disconnect(Connector_Pid) ->
	Connector_Pid ! disconnect.
 
-spec get_connected(pid()) -> {'ok',_} | {'error','timed_out',atom() | pid() | port() | {atom(),atom()},_,reference()}.
get_connected(Connector_Pid) ->
	msg:get(Connector_Pid, get_connected).

-spec get_ResInst(pid()) -> {'ok',_} | {'error','timed_out',atom() | pid() | port() | {atom(),atom()},_,reference()}.
get_ResInst(Connector_Pid) ->
	msg:get(Connector_Pid, get_ResInst).

-spec get_type(pid()) -> {'ok',_} | {'error','timed_out',atom() | pid() | port() | {atom(),atom()},_,reference()}.
get_type(Connector_Pid) ->
	msg:get(Connector_Pid, get_type ).

-spec discard(pid()) -> 'discard'.
discard(Connector_Pid) -> 
	Connector_Pid ! discard. 
	
% Connectors do not survive their ResInst, nor do they 
% move/change from one ResInst to another. 
-spec loop(pid(), pid()|disconnected, pid()) -> 'stopped'.
loop(ResInst_Pid, Connected_Pid, ConnectTyp_Pid) -> 
	receive
		{connect, C_Pid} -> 
			survivor:entry({connection_made, self(), C_Pid, for , ResInst_Pid}),
			loop(ResInst_Pid, C_Pid, ConnectTyp_Pid); 
		disconnect -> 
			loop(ResInst_Pid, disconnected, ConnectTyp_Pid);
		{get_connected, ReplyFn} -> 
			ReplyFn(Connected_Pid),
			loop(ResInst_Pid, Connected_Pid, ConnectTyp_Pid);
		{get_ResInst, ReplyFn} -> 
			ReplyFn(ResInst_Pid),
			loop(ResInst_Pid, Connected_Pid, ConnectTyp_Pid);
		{get_type, ReplyFn} -> 
			ReplyFn(ConnectTyp_Pid),
			loop(ResInst_Pid, Connected_Pid, ConnectTyp_Pid);	
		discard -> 
			survivor:entry(connector_discarded),
			stopped
	end. 

-spec test() -> {pid(), pid()}.
test() -> 
	C1_Pid = create(self(), dummy1_pid),
	C2_Pid = create(self(), dummy2_pid),
	connect(C1_Pid, C2_Pid),
	{C1_Pid, C2_Pid}.

	
		