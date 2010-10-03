-module(network).

-export([new/1,display/1, add_neuron/2]).

new(Name) ->
    { network, Name, neurons, [] }.


display(Network) ->
    io:format("Network ~p!\n", [ Network ]).

add_neuron({network,  Name, neurons, Neurons},  Neuron) ->
    {network, Name, neurons, [  Neuron | Neurons ] }.

load_from_file(File) ->
    {ok, Device} = file:open(File, [read]),
    process_line(Device,Net).

process_line(Device,Net) ->
    case io:get_line(Device) of
        eof  -> file:close(Device);
        Line -> NewNet = parse(Line, Net),
            process_line(Device,NewNet)
    end,
    {ok, Net}.

parse(Line, Net) ->
     parse(string:tokens(Line), Net).
parse('{',neuron, Name, '}', Net) ->
    Neuron = neuron:new(Name),
    add_neuron(Net, Neuron).
