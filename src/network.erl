-module(network).

% get me a network
-export([new/1, load_from_file/1]).

% fiddle my network
-export([add_neuron/2, train/1]).

% show me the money 
-export([display/1]).

-include("network.hrl").

new(Name) ->
    #network { name = Name, neurons = orddict:new(), patterns = orddict:new(), inputs = orddict:new()}.


display(Network) ->
    io:format("Network ~p~n", [ Network ]).

add_neuron(Network = #network{ neurons = Neurons },  Neuron) ->
    Network#network{neurons= orddict:store( neuron:name(Neuron), Neuron , Neurons ) }.

add_input(Network = #network{ inputs = Inputs }, InputName, NeuronName) ->
    { ok , Neuron } = get_neuron(Network, NeuronName ),
    Input = #input { name =InputName, output = Neuron },
    Network#network{inputs = orddict:store( InputName, Input, Inputs ) }.

add_pattern(Network = #network{ patterns = Patterns }, PatternName, Pattern) ->
    Network#network{patterns = orddict:store( PatternName, Pattern, Patterns ) }.

add_connection(Network, NeuronName1, NeuronName2, Weight) ->
    { ok, Neuron1 } = get_neuron(Network, NeuronName1),
    { ok, Neuron2 } = get_neuron(Network, NeuronName2),
    { NewWeight , [] } = string:to_integer(Weight),
    { ok } = neuron:connect(Neuron1, Neuron2, NewWeight),
    Network.

add_output(Network, OutputName, NeuronName) -> 
    { ok, Neuron } = get_neuron(Network, NeuronName),
    { ok } = neuron:connect(Neuron, self()),
    Network.

get_neuron(Network, NeuronName) ->
    Neuron  = orddict:fetch(NeuronName, Network#network.neurons),
    { ok, Neuron }.

get_input(Network, InputName) ->
    Input = orddict:fetch(InputName, Network#network.inputs),
    { ok, Input }.


send_input_value(Network, InputName, Value) when is_number(Value) -> 
    { ok, Input }  = get_input(Network, InputName),
    io:format("attempting to send  ~p sig to ~p~n", [Value, Input]),
    Neuron = Input#input.output,
    neuron:activate(Neuron, Value),
    { ok };
send_input_value(Network, InputName, Value) -> 
    { NewValue, [] } = string:to_integer(Value),
    send_input_value(Network, InputName, NewValue).

train(Network) ->
    orddict:fold(fun run_train_pattern/3,Network, Network#network.patterns).

run_train_pattern(PatternName, Pattern, Network ) -> 
    io:format("running pattern ~s~n", [PatternName]),
    NewNetwork = run_pattern_part (Pattern, Network),
    orddict:map(fun (_Key, Neuron) -> neuron:reset(Neuron) end, NewNetwork#network.neurons),
    NewNetwork.

run_pattern_part ([ Type, Ref, Value | Rest ], Network) ->
    run_pattern_part({ Type, Ref, Value }, Rest, Network);
run_pattern_part ([], Network) ->
    Network.


run_pattern_part({"input", Ref, Value}, Pattern, Network) ->
    { ok } = send_input_value(Network, Ref, Value),
    run_pattern_part(Pattern, Network);
run_pattern_part({"output", Ref, Value}, Pattern, Network) ->
    { ExpectedValue, [] } = string:to_integer(Value),
    receive 
        { From, {fire, ExpectedValue}} ->
                io:format("pattern ~p matched ~p output\n", [ Ref, ExpectedValue] );
        Foo -> 
            io:format("got output message ~p~n", [Foo])
    end,
    run_pattern_part(Pattern, Network).

load_from_file(File) ->
%    io:format("attempting to load from file ~s~n", File),
    {ok, Device} = file:open(File, [read]),
    Net = new(File),
    process_line(Device,Net).


process_line(Device,Net) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Net;
        Line -> 
%            io:format("recieved line ~p~n", [Line]),
            NewNet = parse(Line, Net),
            process_line(Device,NewNet)
    end.


% ignore new lines
parse("\n", Net) ->
    Net;
% tokenise the line
parse(Line, Net)  ->
     [NewLine] = string:tokens(Line, "\n"),
     [Cmd|List] = string:tokens(NewLine, " "),
     parse(Cmd, List, Net).
% parse neurons
parse("neuron:", [Name, Threshold], Net) ->
    io:format("creating neuron ~s", [Name]),
    { NewThreshold, [] } = string:to_float(Threshold),
    Neuron = neuron:new(Name, NewThreshold ),
    io:format(" ~p~n", [Neuron#neuron.pid]),
    add_neuron(Net, Neuron);
% parse inputs
parse("input:", [Name, Neuron], Net) ->
    add_input(Net, Name, Neuron);
parse("connect:", [Neuron1, Neuron2, Weight], Net) ->
    add_connection(Net, Neuron1, Neuron2, Weight);
parse("output:", [Output, Neuron], Net)->
    add_output(Net, Output, Neuron);
parse("pattern:", [ PatternName | Pattern], Net) ->
    add_pattern(Net, PatternName, Pattern);
% blow up on unknown
parse(Token, _, Net) -> false.
