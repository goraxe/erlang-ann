-module(network).

% get me a network
-export([new/1, load_from_file/1]).

% fiddle my network
-export([add_neuron/2]).

% show me the money 
-export([display/1]).

new(Name) ->
    { network, Name, neurons, [] }.


display(Network) ->
    io:format("Network ~p~n", [ Network ]).

add_neuron({network,  Name, neurons, Neurons},  Neuron) ->
    {network, Name, neurons, [  Neuron | Neurons ] }.

load_from_file(File) ->
    io:format("attempting to load from file ~s~n", File),
    {ok, Device} = file:open(File, [read]),
    Net = new(File),
    process_line(Device,Net).

process_line(Device,Net) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device);
        Line -> 
            io:format("recieved line ~p~n", [Line]),
            NewNet = parse(Line, Net),
%            NewNet = Net,
            process_line(Device,NewNet)
    end,
    {ok, Net}.
parse("\n", Net) ->
    Net;
parse(Line, Net) ->
     io:format("parsing line ~p~n", [Line]),
     parse_tokens(string:tokens(Line, " "), Net).
parse_tokens(["{","neuron:", Name, "}\n"], Net) ->
    Neuron = neuron:new(Name),
    add_neuron(Net, Neuron).
