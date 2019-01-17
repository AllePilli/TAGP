-module(heatExchangerTyp).
-export([create/0, init/0]).

-spec create() -> {'ok',pid()}.
create() -> {ok, spawn(?MODULE, init, [])}.

-spec init() -> no_return().
init() -> 
	survivor:entry(heatExchangerTyp_created),
	loop().

-spec loop() -> no_return().
loop() -> 
	receive
		{initial_state, [ResInst_Pid, PipeInst_Pid], ReplyFn} ->
			ReplyFn(#{resInst => ResInst_Pid, pipeInst => PipeInst_Pid}), 
			loop()
	end. 

 