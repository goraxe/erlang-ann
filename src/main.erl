-module(main).
-export([main/1]).

main([]) ->
    main('main');
main(Arg) ->
    _Net = network:new(Arg),
    Net = network:add_neuron(_Net,neuron),
    network:display(Net).
