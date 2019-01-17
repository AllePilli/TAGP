-module(resource_type).
-export([create/2]).
-export([get_initial_state/3, get_connections_list/2, get_locations_list/2]).

-spec create(atom() | tuple(),[any()]) -> any().
create(Selector, ParameterList) -> 
	% e.g. called as follows: 
	%
	% 	resource_type:create(pipeTyp, []). 
	%
	% resource type creation must select a specific type
	% creation of an abstract (partially defined) type is
	% not possible. Returns {ok, ResTyp_Pid}. 
	apply(Selector, create, ParameterList).
	
-spec get_initial_state(atom() | pid() | port() | {atom(),atom()},_,_) -> {'ok',_} | {'error','timed_out',atom() | pid() | port() | {atom(),atom()},_,reference()}.
get_initial_state(ResTyp_Pid, ResInst_Pid, TypeOptions) -> 
	msg:get(ResTyp_Pid, initial_state, [ResInst_Pid, TypeOptions]). 
%	{ok, State} = resource_type:get_initial_state(ResTyp_Pid, self(), []),
	
-spec get_connections_list(atom() | pid() | port() | {atom(),atom()},_) -> {'ok',_} | {'error','timed_out',atom() | pid() | port() | {atom(),atom()},_,reference()}.
get_connections_list(ResTyp_Pid, State) -> 
	msg:get(ResTyp_Pid, connections_list, State).

-spec get_locations_list(atom() | pid() | port() | {atom(),atom()},_) -> {'ok',_} | {'error','timed_out',atom() | pid() | port() | {atom(),atom()},_,reference()}.
get_locations_list(ResTyp_Pid, State) -> 
	msg:get(ResTyp_Pid, locations_list, State). 
	
%%% More functions to follow later. 
