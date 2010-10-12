-module(neuron).

% build me
-export([new/1, new/2]).

% getters
-export([name/1, loop/1]).

% input/output managment
-export([connect/3, connect/2, activate/2, reset/1 ]).

-include("network.hrl").


%% output type
-record (output, { pid, weight}).

%% private data
-record (state, { outputs = [], threshold = 0.0 , accum = 0.0, inputs = 0.0, waiting_for = 0.0, fired = false, activate = fun sigmoid/2}).

new(Name) ->
    new(Name,0.5).

new(Name, Threshold) ->
    State = #state { threshold = Threshold },
    Pid =  spawn(?MODULE, loop, [State]),
    #neuron { name = Name, pid = Pid }.

name(Neuron) ->
    Neuron#neuron.name.


connect(Neuron1, Pid) ->
    Output = #output { pid = Pid, weight = 1},
    % io:format("is this a neuron ~p~n", [Neuron1]),
    Neuron1#neuron.pid ! { connect, { output, Output } },
    { ok }.

connect(Neuron1, Neuron2, Weight) ->
    Output = #output { pid = Neuron2#neuron.pid, weight = Weight},
    Neuron1#neuron.pid ! { connect, { output, Output } },
    Neuron2#neuron.pid ! { connect, { input, 1 } },
    { ok }.

fire(State, [], _Value) ->
    % io:format("\t~p done sending fire messages\n", [self()]),
    State#state{fired = true };
fire(State, [Head|Tail], Value) ->
    % io:format("\t\t ~p Sending ~p on synapsis(~p) ~p\n", [ self(), Value * Head#output.weight, Head#output.weight, Head#output.pid ]),
    Head#output.pid ! { self(), { fire, Value * Head#output.weight } },
    fire(State, Tail, Value).

activate(Neuron, Value) ->
    Pid = Neuron#neuron.pid,
    Neuron#neuron.pid ! { self(), { fire, Value } },
    receive 
        { Pid , ok } ->
            { ok }
    end.

reset(Neuron) ->
    Neuron#neuron.pid ! { reset }.

sigmoid(State, Weight) ->
    NewState = State#state { accum = State#state.accum + Weight },
    % io:format("~p .... accum ~p weight ~p\n", [self(), NewState#state.accum , Weight]),
    { NewState,  NewState#state.accum > NewState#state.threshold }.


recieve_fire_message(State, Weight) ->
    NewState = State#state{  waiting_for = (State#state.waiting_for - 1 ) },
    Fun = NewState#state.activate,
    Fun(NewState, Weight).

display_fire_message(State, Weight) -> 
     io:format("\t ~p got fire message  Accum = ~p Weight=~p Threshold = ~p, Waiting_for ~p~n", [self(), State#state.accum, Weight, State#state.threshold, State#state.waiting_for]).

loop(State) ->
    receive
        { From, { fire, Weight } } ->
            From ! {self(), ok},
            display_fire_message(State, Weight),
            { NewState, Fired } = recieve_fire_message(State, Weight),
            % should we broad cast our result
            case NewState#state.fired of 
                true ->
                    % io:format("\t ~p already fired~n", [self()]),
                    loop(NewState);
                false ->
                    case NewState#state.waiting_for =< 0.0 of
                        % if we have activated fire FIXME should do this only once
                        true ->
                            case Fired  of 
                                true ->
                                    loop(fire(NewState, NewState#state.outputs, 1 ));
                                false ->
                                    loop(fire(NewState, NewState#state.outputs, 0 ))
                            end;
                        false ->
                            % have recieved all our messages
                            loop(NewState)
                    end
            end;
        { connect, { output, Output } } ->
            NewState = State#state{ outputs = [ Output | State#state.outputs ] },
            loop(NewState);
        { connect, { input, 1 } } ->
            NewState = State#state{ inputs = State#state.inputs + 1,  waiting_for = State#state.waiting_for + 1 },
            % io:format("\t ~p got input connect message, incrementing num inputs, and num to wait for currently ~p\n", [self(), NewState#state.waiting_for]),
            loop(NewState);
        { reset } ->
            NewState = State#state{accum = 0, waiting_for = State#state.inputs, fired = false },
            loop(NewState);
        { Pid , ok } ->
            loop(State); 
        terminate ->
            ok;
        Foo -> 
            io:format("\t ~pgot weird message >>>>~p<<<<<~n", [self(), Foo]),
            loop(State)
    end.


