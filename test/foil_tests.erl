-module(foil_tests).
-include("foil.hrl").
-include_lib("eunit/include/eunit.hrl").

foil_test() ->
    error_logger:tty(false),

    {error, foil_not_started} = foil:new(test),
    {error, foil_not_started} = foil:insert(test, key, value),
    {error, foil_not_started} = foil:load(test),
    {error, foil_not_started} = foil:lookup(tets, key),
    {error, foil_not_started} = foil:delete(test, key),
    {error, foil_not_started} = foil:delete(test),

    foil_app:start(),

    ok = foil:new(test),
    {error, module_exists} = foil:new(test),

    ok = foil:insert(test, key, value),
    ok = foil:insert(test, key2, [<<"foo">>, <<"bar">>]),
    ok = foil:insert(test, key3, {1, 1.234}),
    ok = foil:insert(test, key4, "test"),
    ok = foil:insert(test, "key5", 5.5),
    ok = foil:insert(test, 1.234, 2),
    {error, module_not_found} = foil:insert(test2, key2, value),

    ok = foil:delete(test, key4),
    {error, module_not_found} = foil:delete(tes2, key),

    ok = foil:load(test),
    {error, module_not_found} = foil:load(test2),

    {ok, value} = test_foil:lookup(key),
    {ok, [<<"foo">>, <<"bar">>]} = foil:lookup(test, key2),
    {ok, {1, 1.234}} = foil:lookup(test, key3),
    {ok, 5.5} = foil:lookup(test, "key5"),
    {ok, 2} = foil:lookup(test, 1.234),
    {error, module_not_found} = foil:lookup(test2, key),
    {error, key_not_found} = foil:lookup(test, key4),

    ok = foil:delete(test),
    {error, module_not_found} = foil:delete(test),

    foil_app:stop().


foil_any_term_test()->
    error_logger:tty(false),

    Table = any_term_test,

    foil_app:start(),

    ok = foil:new(Table),
    TestPid = spawn(timer, sleep, [10000]),
    TestRef1 = make_ref(),
    TestRef2 = make_ref(),
    RefList = [TestRef1, TestRef2],

    ComplexListTerm =
        [make_ref(), make_ref(), {1, 2, moo, {make_ref(), TestPid}}, "Hello"],
    ComplexTupleTerm1 = {1, {ref1, make_ref()}, [TestPid]},
    ComplexTupleTerm2 = {moo, boo, make_ref(), {1, 2, 3}, [5, 6, 7]},
    PropList = [{ref1, make_ref()}, {ref2, make_ref()}, {ref3, make_ref()}],

    ok = foil:insert(Table, ref1, TestRef1),
    ok = foil:insert(Table, ref2, TestRef2),
    ok = foil:insert(Table, ref_list, RefList),
    ok = foil:insert(Table, pid, TestPid),
    ok = foil:insert(Table, list, ComplexListTerm),
    ok = foil:insert(Table, 1, ComplexTupleTerm1),
    ok = foil:insert(Table, 2, ComplexTupleTerm2),
    ok = foil:insert(Table, "prop_list", PropList),

    ok = foil:load(Table),

    ct:print("MODULE CODE: ~p~n", [code:load_file(Table)]),

    {ok, TestRef1} = any_term_test_foil:lookup(ref1),
    {ok, TestRef1} = foil:lookup(Table, ref1),

    {ok, TestRef2} = any_term_test_foil:lookup(ref2),
    {ok, TestRef2} = foil:lookup(Table, ref2),

    {ok, RefList} = any_term_test_foil:lookup(ref_list),
    {ok, RefList} = foil:lookup(Table, ref_list),

    {ok, TestPid} = any_term_test_foil:lookup(pid),
    {ok, TestPid} = foil:lookup(Table, pid),
    {current_function, {timer, sleep, 1}} = process_info(TestPid, current_function),

    {ok, ComplexListTerm} = any_term_test_foil:lookup(list),
    {ok, ComplexListTerm} = foil:lookup(Table, list),

    {ok, ComplexTupleTerm1} = any_term_test_foil:lookup(1),
    {ok, ComplexTupleTerm1} = foil:lookup(Table, 1),

    {ok, ComplexTupleTerm2} = any_term_test_foil:lookup(2),
    {ok, ComplexTupleTerm2} = foil:lookup(Table, 2),

    {ok, PropList} = any_term_test_foil:lookup("prop_list"),
    {ok, PropList} = foil:lookup(Table, "prop_list"),

    ok = foil:delete(Table),
    {error, module_not_found} = foil:delete(Table),

    foil_app:stop().
