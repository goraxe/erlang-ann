-module(neuron).

% build me
-export([new/1, new/2]).

% getters
-export([name/1, loop/1]).

% input/output managment
-export([connect/3, connect/2, activate/1 ]).

-include("network.hrl").


%% output type
-record (output, { pid, weight}).

%% private data
-record (state, { outputs, threshold}).

new(Name) ->
    new(Name,0.5).

new(Name, Threshold) ->
    Pid =  spawn(?MODULE, loop, [{0, [], 0}]), % Threshold, [], 0
    #neuron { name = Name, pid = Pid }.

name(Neuron) ->
    Neuron#neuron.name.


connect(Neuron1, Pid) ->
    Output = #output { pid = Pid, weight = 1},
    io:format("is this a neuron ~p~n", [Neuron1]),
    Neuron1#neuron.pid ! { connect, Output },
    { ok }.

connect(Neuron1, Neuron2, Weight) ->
    Output = #output { pid = Neuron2#neuron.pid, weight = Weight},
    Neuron1#neuron.pid ! { connect, Output },
    { ok }.

fire([]) ->
    {ok};
fire([Head|Tail]) ->
    io:format("Sending ~p on synapsis ~p\n", [ Head#output.weight, Head#output.pid ]),
    Head#output.pid ! { self(), { fire, Head#output.weight } },
    fire(Tail).

activate(Neuron) ->
    Pid = Neuron#neuron.pid,
    Neuron#neuron.pid ! { self(), { fire, 1 } },
    receive 
        { Pid , ok } ->
            { ok }
    end.


loop(State) ->
    receive
        { From, { fire, Weight } } ->
            From ! {self(), ok},
            { Threshold, Outputs, Accum } = State,
            io:format("got fire message Accum = ~p Weight=~p~n", [Accum, Weight]), 
            NewAccum = Accum + Weight,
            case NewAccum >= Threshold of
                true ->
                    fire(Outputs),
                    loop({ Threshold, Outputs, NewAccum });
                false ->
                    loop({ Threshold, Outputs, NewAccum})
            end;
        { connect,  Output } ->
                { _Threshold, Outputs, _Accum } = State,
                loop({_Threshold, [ Output | Outputs], _Accum} );
        { Pid , ok } ->
            loop(State); 
        terminate ->
            ok;
        Foo -> 
            io:format("got weird message >>>>~p<<<<<~n", [Foo]),
            loop(State)
    end.


