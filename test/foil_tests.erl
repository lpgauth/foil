-module(foil_tests).
-include("foil.hrl").
-include_lib("eunit/include/eunit.hrl").

foil_test() ->
    error_logger:tty(false),

    {error, foil_not_started} = foil:new(test),
    {error, foil_not_started} = foil:insert(test, key, value),
    {error, foil_not_started} = foil:load(test),
    {error, foil_not_started} = foil:lookup(test, key),
    {error, foil_not_started} = foil:all(test),
    {error, foil_not_started} = foil:delete(test, key),
    {error, foil_not_started} = foil:delete(test),

    foil_app:start(),

    ok = foil:new(test),
    {error, module_exists} = foil:new(test),

    ok = foil:insert(test, key, value),
    ok = foil:insert(test, key2, [<<"foo">>, <<"bar">>]),
    ok = foil:insert(test, key3, {1, 1.234}),
    ok = foil:insert(test, key4, "test"),
    ok = foil:insert(test, key5, #{"hello" => 123}),

    Ref = erlang:make_ref(),
    ok = foil:insert(test, key6, Ref),

    {error, module_not_found} = foil:insert(test2, key2, value),

    ok = foil:delete(test, key4),
    {error, module_not_found} = foil:delete(tes2, key),

    ok = foil:load(test),
    {error, module_not_found} = foil:load(test2),

    {ok, value} = test_foil:lookup(key),
    {ok, [<<"foo">>, <<"bar">>]} = foil:lookup(test, key2),
    {ok, {1, 1.234}} = foil:lookup(test, key3),
    {ok, #{
        key := value,
        key2 := [<<"foo">>, <<"bar">>],
        key3 := {1, 1.234},
        key5 := #{"hello" := 123},
        key6 := Ref
    }} = foil:all(test),
    {error, module_not_found} = foil:lookup(test2, key),
    {error, module_not_found} = foil:all(test2),
    {error, key_not_found} = foil:lookup(test, key4),

    ok = foil:delete(test),
    {error, module_not_found} = foil:delete(test),

    foil_app:stop().
