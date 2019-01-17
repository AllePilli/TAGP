-module(msg).
-define(TimeOut, 5000).
-export([get/2, get/3, set_ack/2, set_ack/3]).
-export([test/0]). 

-spec get(atom() | pid() | port() | {atom(),atom()},_) -> {'ok',_} | {'error','timed_out',atom() | pid() | port() | {atom(),atom()},_,reference()}.
get(Pid, Key) ->
	Pid ! {Key, replier(R = make_ref())},
	receive
		{R, Info} -> {ok, Info}
		after ?TimeOut -> {error, timed_out, Pid, Key, R}
	end.

-spec get(atom() | pid() | port() | {atom(),atom()},_,_) -> {'ok',_} | {'error','timed_out',atom() | pid() | port() | {atom(),atom()},_,reference()}.
get(Pid, Key, P_list) -> 
	Pid ! {Key, P_list, replier(R = make_ref())},
	receive
		{R, Info} -> {ok, Info}
		after ?TimeOut -> {error, timed_out, Pid, Key, R}
	end.
	
-spec set_ack(atom() | pid() | port() | {atom(),atom()},_) -> {'ok',_} | {'error','timed_out',atom() | pid() | port() | {atom(),atom()},_,reference()}.
set_ack(Pid, Key) -> % identical to get; for readability only. 
	Pid ! {Key, replier(R = make_ref())},
	receive
		{R, Info} -> {ok, Info}
		after ?TimeOut -> {error, timed_out, Pid, Key, R}
	end.

-spec set_ack(atom() | pid() | port() | {atom(),atom()},_,_) -> {'ok',_} | {'error','timed_out',atom() | pid() | port() | {atom(),atom()},_,reference()}.
set_ack(Pid, Key, P_list) -> % identical to get; for readability only. 
	Pid ! {Key, P_list, replier(R = make_ref())},
	receive
		{R, Info} -> {ok, Info}
		after ?TimeOut -> {error, timed_out, Pid, Key, R}
	end.

-spec replier(reference()) -> fun((_) -> {reference(),_}).
replier(Ref) ->  
    Sender = self(),
	fun(Msg) -> Sender ! {Ref, Msg} end.

-spec test() -> {'ok',_} | {'error','timed_out',atom() | pid() | port() | {atom(),atom()},_,reference()}.
test() -> 
	Pid = spawn(fun() -> receive {_Dummy, [P | _ ], F } -> F(2 * P) end, ok end),
	msg:set_ack(Pid, dummy, [10]). 

