-module(test_digitalTwin).
-include_lib("eunit/include/eunit.hrl").
-export([]).

digitalTwin_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun testDigitalTwin/1,
        fun testPipes/1,
        fun testFlowMeter/1,
        fun testPumps/1,
        fun testHeatexchangers/1
    ]}
.

setup() ->
    AmtPipes = 9,
    AmtPumps = 2,
    AmtHeatEchangers = 4,
    {ok, {PipeTypPid, Pipes, Connectors, Locations, FluidumTypPid, FluidumInst, PumpTypPid, Pumps, FlowMeterTypPid, FlowMeterInst, HeatExchangerTypPid, HeatExchangers}} = digitalTwin:start(AmtPipes, AmtPumps, AmtHeatEchangers),
    {PipeTypPid, Pipes, Connectors, Locations, FluidumTypPid, FluidumInst, PumpTypPid, Pumps, FlowMeterTypPid, FlowMeterInst, HeatExchangerTypPid, HeatExchangers, AmtPipes, AmtPumps, AmtHeatEchangers}
.

cleanup(_) ->
    digitalTwin:stop()
.

%% Tests all Type Pid's, individual instances and the amount of Pipes, Connectors, Locations, Pumps, HeatExchangers
testDigitalTwin({PipeTypPid, Pipes, Connectors, Locations, FluidumTypPid, FluidumInst, PumpTypPid, Pumps, FlowMeterTypPid, FlowMeterInst, HeatExchangerTypPid, HeatExchangers, AmtPipes, AmtPumps, AmtHeatEchangers}) ->
    [
        %% Test Type Pid's
        ?_assert(alive(PipeTypPid)),
        ?_assert(alive(FluidumTypPid)),
        ?_assert(alive(PumpTypPid)),
        ?_assert(alive(FlowMeterTypPid)),
        ?_assert(alive(HeatExchangerTypPid)),
        %% Test Instances
        ?_assert(alive(FluidumInst)),
        ?_assert(alive(FlowMeterInst)),
        %% Test Lengths
        ?_assertEqual(AmtPipes, length(Pipes)),
        ?_assertEqual(2 * AmtPipes, length(Connectors)),
        ?_assertEqual(AmtPipes, length(Locations)),
        ?_assertEqual(AmtPumps, length(Pumps)),
        ?_assertEqual(AmtHeatEchangers, length(HeatExchangers))
    ]
.

testPipes({_, Pipes, Connectors, Locations, _, FluidumInst, _, _, _, _, _, _, AmtPipes, _, _}) ->
    testPipes(Pipes, Connectors, Locations, AmtPipes, [], FluidumInst)
.

testPipes(Pipes, Connectors, Locations, AmtPipes, Tests, FluidumInst) when AmtPipes > 0 ->
    [Pipe | OtherPipes] = Pipes,
    [ConIn, ConOut | OtherConnectors] = Connectors,
    [Location | OtherLocations] = Locations,

    Flow = 5,
    {ok, FlowInfluenceFn} = pipeInst:get_flow_influence(Pipe),
    
    {ok, ConInInst} = connector:get_ResInst(ConIn),
    {ok, ConOutInst} = connector:get_ResInst(ConOut),
    {ok, LocationInst} = location:get_ResInst(Location),
    {ok, ConInTyp} = connector:get_type(ConIn),
    {ok, ConOutTyp} = connector:get_type(ConOut),
    {ok, Visitor} = location:get_Visitor(Location),

    Test = [
        %% Test if Pipe, Connectors and Location are alive
        ?_assert(alive(Pipe)),
        ?_assert(alive(ConIn)),
        ?_assert(alive(ConOut)),
        ?_assert(alive(Location)),
        %% Test if Flow influence is correctly calculated
        ?_assertEqual(-0.01 * Flow, FlowInfluenceFn(Flow)),
        %% Test if instances and types are correct
        ?_assertEqual(ConInInst, Pipe),
        ?_assertEqual(ConOutInst, Pipe),
        ?_assertEqual(LocationInst, Pipe),
        ?_assertEqual(ConInTyp, simplePipe),
        ?_assertEqual(ConOutTyp, simplePipe),
        %% Test if Visitor is correct
        ?_assertEqual(Visitor, FluidumInst)
    ],

    testPipes(OtherPipes, OtherConnectors, OtherLocations, AmtPipes - 1, Tests ++ [Test], FluidumInst)
;

testPipes([], [], [], 0, Tests, _) ->
    Tests
.

testFlowMeter({_, _, _, _, _, _, _, _, _, FlowMeterInst, _, _, _, _, _}) ->
    Test = [
        %% Test instance pid's
        ?_assert(alive(FlowMeterInst))
    ],

    {ok, Measurement} = flowMeterInst:measure_flow(FlowMeterInst),
    Tests = Test ++ [
        ?_assertEqual(Measurement, {ok, real_flow})
    ],

    Tests
.

testPumps({_, _, _, _, _, _, _, Pumps, _, _, _, _, _, AmtPumps, _}) ->
    testPumps(Pumps, AmtPumps, [])
.

testPumps([Pump | OtherPumps], AmtPumps, Tests) when AmtPumps > 0 ->
    Test1 = Tests ++ [
        ?_assert(alive(Pump)),
        ?_assertEqual({ok, off}, pumpInst:is_on(Pump))
    ],

    %% Test if switch on and off functions work
    switchOn = pumpInst:switch_on(Pump),
    IsOn1 = pumpInst:is_on(Pump),
    Test2 = Test1 ++ [?_assertEqual({ok, on}, IsOn1)],

    switchOn = pumpInst:switch_on(Pump),
    IsOn2 = pumpInst:is_on(Pump),
    Test3 = Test2 ++ [?_assertEqual({ok, on}, IsOn2)],
    {ok, FlowInfluenceOn} = pumpInst:flow_influence(Pump),

    switchOff = pumpInst:switch_off(Pump),
    IsOn3 = pumpInst:is_on(Pump),
    Test4 = Test3 ++ [?_assertEqual({ok, off}, IsOn3)],

    switchOff = pumpInst:switch_off(Pump),
    IsOn4 = pumpInst:is_on(Pump),
    Test5 = Test4 ++ [?_assertEqual({ok, off}, IsOn4)],

    %% Test flow influence
    {ok, FlowInfluenceOff} = pumpInst:flow_influence(Pump),
    Test6 = Test5 ++ [
        ?_assertEqual(FlowInfluenceOff(0), 0),
        ?_assertEqual(FlowInfluenceOff(1), 0),
        ?_assertEqual(FlowInfluenceOff(10), 0),
        ?_assertEqual(FlowInfluenceOn(0), 250),
        ?_assertEqual(FlowInfluenceOn(1), 243),
        ?_assertEqual(FlowInfluenceOn(10), 0)
    ],

    testPumps(OtherPumps, AmtPumps - 1, Tests ++ [Test6])
;

testPumps([], 0, Tests) ->
    Tests
.

testHeatexchangers({_, _, _, _, _, _, _, _, _, _, _, HeatExchangers, _, _, AmtHeatEchangers}) ->
    testHeatexchangers(HeatExchangers, AmtHeatEchangers, [])
.

testHeatexchangers([HeatExchanger | OtherHeatExchangers], AmtHeatEchangers, Tests) when AmtHeatEchangers > 0 ->
    {ok, {ok, TempInfl}} = heatExchangerInst:temp_influence(HeatExchanger),
    {ok, Influence} = TempInfl(5, 25), %% (Flow, Temperature)
    ExpectedInfluence = 25 + (1 / 5), %% Temperature + (Difference / Flow)

    Test = [
        ?_assert(alive(HeatExchanger)),
        ?_assertEqual(ExpectedInfluence, Influence)
    ],

    testHeatexchangers(OtherHeatExchangers, AmtHeatEchangers - 1, Tests ++ Test)
;

testHeatexchangers([], 0, Tests) ->
    Tests
.

alive(Pid) ->
    erlang:is_process_alive(Pid)
.
