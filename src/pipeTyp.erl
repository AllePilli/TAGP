-module(pipeTyp).
-export([create/0, init/0, get_flow_influence/2]). % More to be added later. 

-spec create() -> {'ok',pid()}.
create() -> {ok, spawn(?MODULE, init, [])}.

-spec init() -> no_return().
init() -> 
	survivor:entry(pipeTyp_created),
	loop().

-spec get_flow_influence(atom() | pid() | port() | {atom(),atom()},_) -> {'ok',_} | {'error','timed_out',atom() | pid() | port() | {atom(),atom()},_,reference()}.
get_flow_influence(TypePid, State) -> 
	msg:get(TypePid, flow_influence, State).

-spec loop() -> no_return().
loop() -> 
	receive
		{initial_state, [ResInst_Pid, TypeOptions], ReplyFn} ->
			Location = location:create(ResInst_Pid, emptySpace),
			In = connector:create(ResInst_Pid, simplePipe),
			Out = connector:create(ResInst_Pid, simplePipe),
			ReplyFn(#{resInst => ResInst_Pid, chambers => [Location], 
					cList => [In, Out], typeOptions => TypeOptions}), 
			loop();
		{connections_list, State , ReplyFn} -> 
			#{cList := C_List} = State, ReplyFn(C_List), 
			loop();
		{locations_list, State, ReplyFn} -> 
			#{chambers := L_List} = State, ReplyFn(L_List),
			loop();
		{flow_influence, _State, ReplyFn} -> 
			FlowInfluenceFn = fun(Flow) -> flow(Flow) end, % placeholder only. 
			ReplyFn(FlowInfluenceFn), 
			loop()
	end. 

-spec flow(number()) -> float().
flow(N) -> - 0.01 * N. 
