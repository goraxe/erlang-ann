-module(neuron).

-export([new/1]).

new(Name) ->
    { neuron, Name }.
