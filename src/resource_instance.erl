-module(resource_instance).
-export([create/2]).
-export([list_connectors/1, list_locations/1]).
-export([get_type/1, get_ops/1, get_state/1]).
-export([get_flow_influence/1]).
%%% More to follow later. 

-spec create(atom() | tuple(),[any()]) -> any().
create(Selector, Environment) -> 
	apply(Selector, create, Environment).
	% returns {ok, ResInst_Pid}
	
-spec list_connectors(atom() | pid() | port() | {atom(),atom()}) -> {'ok',_} | {'error','timed_out',atom() | pid() | port() | {atom(),atom()},_,reference()}.
list_connectors(ResInst_Pid)	-> 
	msg:get(ResInst_Pid, get_connectors).	
	
-spec list_locations(atom() | pid() | port() | {atom(),atom()}) -> {'ok',_} | {'error','timed_out',atom() | pid() | port() | {atom(),atom()},_,reference()}.
list_locations(ResInst_Pid)	-> % ResInst is hosting
	msg:get(ResInst_Pid, get_locations).

-spec get_type(atom() | pid() | port() | {atom(),atom()}) -> {'ok',_} | {'error','timed_out',atom() | pid() | port() | {atom(),atom()},_,reference()}.
get_type(ResInst_Pid) -> % allows to retrieve state-agnostic information
	msg:get(ResInst_Pid, get_type).

-spec get_ops(atom() | pid() | port() | {atom(),atom()}) -> {'ok',_} | {'error','timed_out',atom() | pid() | port() | {atom(),atom()},_,reference()}.
get_ops(ResInst_Pid) -> % list of commands available in the current state
	% Does not lock the resource state; list may change at any time
	msg:get(ResInst_Pid, get_ops).

-spec get_state(atom() | pid() | port() | {atom(),atom()}) -> {'ok',_} | {'error','timed_out',atom() | pid() | port() | {atom(),atom()},_,reference()}.
get_state(ResInst_Pid) -> % current state understood by type (only)
	% Does not lock the resource state; may change at any time
	msg:get(ResInst_Pid, get_state).

-spec get_flow_influence(atom() | pid() | port() | {atom(),atom()}) -> {'ok',_} | {'error','timed_out',atom() | pid() | port() | {atom(),atom()},_,reference()}.
get_flow_influence(ResInst_Pid) -> 
	msg:get(ResInst_Pid, get_flow_influence).
