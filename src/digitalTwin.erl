-module(digitalTwin).
-export([start/3, stop/0]).

-spec start(_,number(),number()) -> {'error',[1..255,...]} | {'ok',{pid(),[any(),...],nonempty_maybe_improper_list(),[any()],pid(),pid(),pid(),[any(),...],pid(),pid(),pid(),[any(),...]}}.
start(AmtPipes, AmtPumps, AmtHeatExchangers) when AmtPipes < AmtPumps + AmtHeatExchangers + 1 ->
    {error, "Configuration must consist of at least 3 pipes"}
;

start(AmtPipes, AmtPumps, AmtHeatExchangers) ->
    true = survivor:start(),
    create_configuration(AmtPipes, AmtPumps, AmtHeatExchangers)
.

%% creates a configuration of pipes connected in a circle
-spec create_configuration(number(),number(),number()) -> {'ok',{pid(),[any(),...],nonempty_maybe_improper_list(),[any()],pid(),pid(),pid(),[any(),...],pid(),pid(),pid(),[any(),...]}}.
create_configuration(AmtPipes, AmtPumps, AmtHeatExchangers) ->
    %% Create a circle of pipes
    {ok, PipeTypPid} = pipeTyp:create(),
    Pipes = createPipes(AmtPipes, PipeTypPid),

    createConnections(Pipes),
    Connectors = getConnectors(Pipes),

    Locations = getLocations(Pipes),

    %% Create fluidum and make it flow in the pipes
    [Root_In | _] = Connectors,
    FluidumTypPid = fluidumTyp:create(),
    {ok, FluidumInst} = fluidumInst:create(Root_In, FluidumTypPid),
    makeFluidumFlow(FluidumInst, Locations),

    %% Create Pumps
    {ok, PumpTypPid} = pumpTyp:create(),
    {Pumps, PipesWithoutPumps} = createPumps(AmtPumps, PumpTypPid, Pipes),

    %% Create Flowmeter first pipe without a pump
    RWCmdFnFlowMeter = fun() -> {ok, real_flow} end,
    [FirstPipeWithoutPump | PipesWithoutFlowMeter] = PipesWithoutPumps,
    {ok, FlowMeterTypPid} = flowMeterTyp:create(),
    {ok, FlowMeterInst} = flowMeterInst:create(self(), FlowMeterTypPid, FirstPipeWithoutPump, RWCmdFnFlowMeter),

    %% Create Heatexchanger on the rest of the pipes
    HE_link_spec = #{delta => 1},
    {ok, HeatExchangerTypPid} = heatExchangerTyp:create(),
    {HeatExchangers, _} = createHeatExchangers(AmtHeatExchangers, HeatExchangerTypPid, PipesWithoutFlowMeter, HE_link_spec),

    {ok, {PipeTypPid, Pipes, Connectors, Locations, FluidumTypPid, FluidumInst, PumpTypPid, Pumps, FlowMeterTypPid, FlowMeterInst, HeatExchangerTypPid, HeatExchangers}}
.

-spec stop() -> 'stopped'.
stop() ->
    survivor ! stop,
    stopped
.

-spec createPipes(number(),pid()) -> [pid(),...].
createPipes(AmtPipes, PipeTypPid) ->
    createPipes(AmtPipes, PipeTypPid, [])
.

-spec createPipes(number(),pid(),[pid()]) -> [pid(),...].
createPipes(AmtPipes, PipeTypPid, Pipes) when AmtPipes == 1 ->
    {ok, PipeInst} = pipeInst:create(self(), PipeTypPid),
    TotalPipes = Pipes ++ [PipeInst],
    TotalPipes
;

createPipes(AmtPipes, PipeTypPid, Pipes) ->
    {ok, PipeInst} = pipeInst:create(self(), PipeTypPid),
    createPipes(AmtPipes - 1, PipeTypPid, Pipes ++ [PipeInst])
.

-spec createConnections([pid(),...]) -> {'connect',_}.
createConnections([Root | T]) ->
    createConnections(Root, Root, T)
.

-spec createConnections(pid(),pid(),[pid()]) -> {'connect',_}.
createConnections(Root, Pipe1, [Pipe2 | T]) ->
    {ok, [_, Pipe1_Out]} = msg:get(Pipe1, get_connectors),
    {ok, [Pipe2_In, _]} = msg:get(Pipe2, get_connectors),

    connector:connect(Pipe1_Out, Pipe2_In),
    createConnections(Root, Pipe2, T)
;

createConnections(Root, LastPipe, []) ->
    {ok, [_, LastPipe_Out]} = msg:get(LastPipe, get_connectors),
    {ok, [Root_In, _]} = msg:get(Root, get_connectors),
    connector:connect(LastPipe_Out, Root_In)
.

-spec getConnectors([pid(),...]) -> any().
getConnectors(Pipes) ->
    getConnectors(Pipes, [])
.

-spec getConnectors([pid()],_) -> any().
getConnectors([Pipe | T], Connectors) ->
    {ok, Cons} = msg:get(Pipe, get_connectors),
    getConnectors(T, lists:append(Connectors, Cons))
;

getConnectors([], Connectors) ->
    Connectors
.

-spec getLocations([pid(),...]) -> any().
getLocations(Pipes) ->
    getLocations(Pipes, [])
.

-spec getLocations([pid()],_) -> any().
getLocations([Pipe | T], Locations) ->
    {ok, Location} = msg:get(Pipe, get_locations),
    getLocations(T, Locations ++ Location)
;

getLocations([], Locations) ->
    Locations
.

-spec makeFluidumFlow(pid(),[atom() | pid() | port() | {atom(),atom()}]) -> 'ok'.
makeFluidumFlow(FluidumInst, [Location | T]) ->
    location:arrival(Location, FluidumInst),
    makeFluidumFlow(FluidumInst, T)
;

makeFluidumFlow(_, []) ->
    ok
.

-spec createPumps(number(),pid(),[pid(),...]) -> {[pid(),...],[pid()]}.
createPumps(AmtPumps, PumpTypPid, Pipes) ->
    createPumps(AmtPumps, PumpTypPid, Pipes, [])
.

-spec createPumps(number(),pid(),[pid(),...],[pid()]) -> {[pid(),...],[pid()]}.
createPumps(AmtPumps, PumpTypPid, Pipes, Pumps) when AmtPumps > 1 ->
    RWCmdFnPump = fun(on) ->
        {ok, on};
    (off) ->
        {ok, off}
    end,

    [Pipe | T] = Pipes,
    {ok, PumpInst} = pumpInst:create(self(), PumpTypPid, Pipe, RWCmdFnPump),
    
    createPumps(AmtPumps - 1, PumpTypPid, T, Pumps ++ [PumpInst])
;

createPumps(AmtPumps, PumpTypPid, Pipes, Pumps) when AmtPumps == 1 ->
    RWCmdFnPump = fun(on) ->
        {ok, on};
    (off) ->
        {ok, off}
    end,

    [LastPipe | T] = Pipes,
    {ok, PumpInst} = pumpInst:create(self(), PumpTypPid, LastPipe, RWCmdFnPump),
    {Pumps ++ [PumpInst], T}
.

-spec createHeatExchangers(number(),pid(),[pid(),...],#{'delta':=1}) -> {[pid(),...],[pid()]}.
createHeatExchangers(AmtHeatExchangers, HeatExchangerTypPid, Pipes, HE_link_spec) ->
    createHeatExchangers(AmtHeatExchangers, HeatExchangerTypPid, Pipes, [], HE_link_spec)
.

-spec createHeatExchangers(number(),pid(),[pid(),...],[pid()],#{'delta':=1}) -> {[pid(),...],[pid()]}.
createHeatExchangers(AmtHeatExchangers, HeatExchangerTypPid, Pipes, HeatExchangers, HE_link_spec) when AmtHeatExchangers > 1 ->
    [Pipe | T] = Pipes,
    {ok, HeatExchangerInst} = heatExchangerInst:create(self(), HeatExchangerTypPid, Pipe, HE_link_spec),

    createHeatExchangers(AmtHeatExchangers - 1, HeatExchangerTypPid, T, HeatExchangers ++ [HeatExchangerInst], HE_link_spec)
;

createHeatExchangers(AmtHeatExchangers, HeatExchangerTypPid, Pipes, HeatExchangers, HE_link_spec) when AmtHeatExchangers == 1 ->
    [Pipe | RegularPipes] = Pipes,
    {ok, HeatExchangerInst} = heatExchangerInst:create(self(), HeatExchangerTypPid, Pipe, HE_link_spec),

    {HeatExchangers ++ [HeatExchangerInst], RegularPipes}
.
