#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pz ebin deps/*/ebin -config app.config +A 50

main([Node, Cookie| Rest]) ->
    Options = parse_options(lists:append(string:tokens(Rest, " "))),
    Paths = ["/../ebin/", "/../deps/cecho/ebin/", "/../deps/lager/ebin/", "/../deps/goldrush/ebin/"],
    [true = code:add_pathz(filename:dirname(escript:script_name()) ++ Path) || Path <- Paths],
    case lists:member(hd("."), Node) of
        true ->
           net_kernel:start(['erl_prism@localhost.local', longnames]);
        false ->
           net_kernel:start(['erl_prism@localhost', shortnames])
    end,
    erl_prism:start(list_to_atom(Node), list_to_atom(Cookie), Options);

main(_) ->
    usage().

usage() ->
    io:format("usage:\n\terl_prism host cookie [Options]\n", []),
    io:format("options:\n", []),
    io:format("\t-a [Integer]: auto-capture every n seconds\n", []),
    halt(1).

parse_options([]) ->
    [];
parse_options(["-a", NumberString | Rest]) ->
    {Number, []} = string:to_integer(NumberString),
    [{auto_capture, Number * 1000} | parse_options(Rest)];
parse_options(_) ->
    usage().

