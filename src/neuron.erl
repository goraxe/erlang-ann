-module(neruon).

-export([new/1]).

new(Name) ->
    { neuron, Name }
