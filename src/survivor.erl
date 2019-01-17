-module(survivor).
-export([start/0, entry/1, init/0]). 

-spec start() -> true.
start() ->
	(whereis(survivor) =:= undefined) orelse unregister(survivor), 
	register(survivor, spawn(?MODULE, init, [])).

-spec entry(Data::_) -> true.
entry(Data)-> 
	ets:insert(logboek, {{now(), self()}, Data}). % Return true when eunit testing

-spec init() -> ok.
init() -> 
	(ets:info(logboek) =:= undefined) orelse ets:delete(logboek),
	ets:new(logboek, [ordered_set, named_table, public]), % Comment this line when eunit testing	
	loop().

-spec loop() -> ok.
loop() -> 
	receive
		stop -> ok
	end. 

