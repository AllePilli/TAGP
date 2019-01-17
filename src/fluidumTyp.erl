-module(fluidumTyp).

-export([create/0, init/0, discover_circuit/1]).
-export([get_resource_circuit/2]).

-spec create() -> pid().
create() -> spawn(?MODULE, init, []).

-spec init() -> no_return().
init() -> 
	survivor:entry(fluidTyp_created), 
	loop().

-spec get_resource_circuit(atom() | pid() | port() | {atom(),atom()},_) -> {'ok',_} | {'error','timed_out',atom() | pid() | port() | {atom(),atom()},_,reference()}.
get_resource_circuit(TypePid, State) ->
	msg:get(TypePid, resource_circuit, State). 

-spec loop() -> no_return().
loop() -> 
	receive
		{initial_state, [ResInst_Pid, [Root_ConnectorPid, TypeOptions]], ReplyFn} -> 
			{ok, C} = discover_circuit(Root_ConnectorPid), 
			ReplyFn(#{resInst => ResInst_Pid, circuit => C, typeOptions => TypeOptions}), 
			loop();
		{connections_list, _State , ReplyFn} -> 
			ReplyFn([]), 
			loop();
		{locations_list, _State, ReplyFn} -> 
			ReplyFn([]),
			loop();
		{resource_circuit, State, ReplyFn} -> 
			#{circuit := C} = State, 
			{_RootC, CircuitMap} = C, 
			ReplyFn(extract(CircuitMap)),
			loop()
	end. 

-spec extract(map()) -> #{_=>'processed'}.
extract(C) -> extract(maps:next(maps:iterator(C)), #{}).

-spec extract('none' | {atom() | pid() | port() | {atom(),atom()},_,maps:iterator()},#{_=>'processed'}) -> #{_=>'processed'}.
extract({C, _ , Iter }, ResLoop) ->
		{ok, ResPid} = connector:get_ResInst(C),
		extract(maps:next(Iter), ResLoop#{ResPid => processed});

extract( none , ResLoop) -> ResLoop. 

-spec discover_circuit(_) -> {'ok',{_,#{_=>'processed'}}}.
discover_circuit(Root_Pid) -> 
	{ok,  Circuit} = discover_circuit([Root_Pid], #{  }),
	{ok, {Root_Pid, Circuit}}.

-spec discover_circuit([any()],#{_=>'processed'}) -> {'ok',#{_=>'processed'}}.
discover_circuit([ disconnected | Todo_List], Circuit) -> 
	discover_circuit(Todo_List, Circuit);

discover_circuit([C | Todo_List], Circuit) -> 
	{ok, Updated_Todo_list, Updated_Circuit} = 
		process_connection(C, maps:find(C, Circuit ), Todo_List, Circuit),
	discover_circuit(Updated_Todo_list, Updated_Circuit);

discover_circuit([], Circuit) ->
	{ ok, Circuit }.

-spec process_connection(_,'error' | {'ok',_},[any()],#{_=>'processed'}) -> {'ok',[any()],#{_=>'processed'}}.
process_connection(C, error, Todo_List, Circuit) -> 
	Updated_Circuit = Circuit#{ C => processed },
    {ok, CC} = connector:get_connected(C),
	Updated_Todo_list = [ CC | Todo_List],
	{ok, ResPid} = connector:get_ResInst(C),
	{ok, C_list} = resource_instance:list_connectors(ResPid),
	{ok, C_list ++  Updated_Todo_list, Updated_Circuit};

process_connection( _, _ , Todo_List, Circuit) -> 
	{ok, Todo_List, Circuit}.





