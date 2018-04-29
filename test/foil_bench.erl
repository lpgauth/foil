-module(foil_bench).

-export([
    run/0
]).

-define(B, 1000). % batch size
-define(C, 2048). % concurrency
-define(N, 2048). % iterations
-define(T, [
    {atom, test},
    {binary, <<0:1024>>},
    {complex, {test, [<<"test">>, "test"]}},
    {list, "test"},
    {tuple, {test, test2}}
]).

%% public
-spec run() ->
    ok.

run() ->
    error_logger:tty(false),
    foil_app:start(),
    io:format("~23s ~6s ~6s ~6s~n", [name, mean, p99, p999]),
    run(?T, ?C, ?N),
    foil_app:stop().

%% private
lookup(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        false -> undefined;
        {_, Value} -> Value
    end.

name(Name, Type) ->
    list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(Type)).

run([], _C, _N) ->
    ok;
run([{Type, Value} | T], C, N) ->
    run_ets(Type, Value, C, N),
    run_foil_indirect(Type, Value, C, N),
    run_foil_direct(Type, Value, C, N),
    run(T, C, N).

run(Name, Fun, C, N) ->
    [Fun() || _ <- lists:seq(1, 10000)],
    Results = timing_hdr:run(Fun, [
        {name, Name},
        {concurrency, C},
        {iterations, N},
        {output, "output/" ++ atom_to_list(Name)}
    ]),
    Mean = lookup(mean, Results) / ?B,
    P99 = lookup(p99, Results) / ?B,
    P999 = lookup(p999, Results)  / ?B,

    io:format("~23s ~6.3f ~6.3f ~6.3f~n", [Name, Mean, P99, P999]).

run_ets(Type, Value, C, N) ->
    ets:new(?MODULE, [named_table, public, {read_concurrency, true}]),
    [ets:insert(?MODULE, {X, <<"foo">>}) || X <- lists:seq(0, 100)],
    ets:insert(?MODULE, {test, Value}),
    Fun = fun() ->
        [Value = ets:lookup_element(?MODULE, test, 2) || _ <- lists:seq(0, ?B)],
        ok
    end,
    run(name(ets, Type), Fun, C, N),
    ets:delete(?MODULE).

run_foil_direct(Type, Value, C, N) ->
    foil:new(?MODULE),
    [foil:insert(?MODULE, X, <<"foo">>) || X <- lists:seq(0, 100)],
    foil:insert(?MODULE, test, Value),
    foil:load(?MODULE),
    timer:sleep(500),
    Fun = fun() ->
        [{ok, Value} = foil_bench_foil:lookup(test) || _ <- lists:seq(0, ?B)],
        ok
    end,
    run(name(foil_direct, Type), Fun, C, N),
    foil:delete(?MODULE).

run_foil_indirect(Type, Value, C, N) ->
    foil:new(?MODULE),
    [foil:insert(?MODULE, X, <<"foo">>) || X <- lists:seq(0, 100)],
    foil:insert(?MODULE, test, Value),
    foil:load(?MODULE),
    timer:sleep(500),
    Fun = fun() ->
        [{ok, Value} = foil:lookup(?MODULE, test) || _ <- lists:seq(0, ?B)],
        ok
    end,
    run(name(foil_indirect, Type), Fun, C, N),
    foil:delete(?MODULE).
