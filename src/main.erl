-module(main).
-export([main/1]).

main([]) ->
    io:format("a network file is needed~n"),
    erlang:halt();
main(Arg) ->
    Net = network:load_from_file(Arg),
    network:train(Net),
    network:display(Net).
