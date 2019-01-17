-module(heatExchangeLink).
-export([get_temp_influence/1]).

% dummy module - replace by realistic model
% e.g. the outgoing temperature provided by get_temp_influence
% will saturate when approching the inTemp on the other side
% of the link. 

-spec get_temp_influence(_) -> {'ok',fun((number(),number()) -> {_,_})}.
get_temp_influence(HE_link_spec) ->
	{ok, fun(Flow, InTemp) -> #{delta := Difference} = HE_link_spec, {ok, InTemp + (Difference/Flow)} end}.


