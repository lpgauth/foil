-module(foil_compiler).
-include("foil.hrl").

-export([
    load/2
]).

%% public
-spec load(namespace(), [{key(), value()}]) ->
    ok.

load(Module, KVs) ->
    Forms = forms(Module, KVs),
    {ok, Module, Bin} = compile:forms(Forms, [debug_info]),
    code:soft_purge(Module),
    Filename = atom_to_list(Module) ++ ".erl",
    {module, Module} = code:load_binary(Module, Filename, Bin),
    ok.

%% private
forms(Module, KVs) ->
    Mod = erl_syntax:attribute(erl_syntax:atom(module),
        [erl_syntax:atom(Module)]),
    ExportList = [
        erl_syntax:arity_qualifier(erl_syntax:atom(lookup),
        erl_syntax:integer(1)),
        erl_syntax:arity_qualifier(erl_syntax:atom(all),
        erl_syntax:integer(0))],
    Export = erl_syntax:attribute(erl_syntax:atom(export),
        [erl_syntax:list(ExportList)]),
    Lookup = erl_syntax:function(erl_syntax:atom(lookup),
        lookup_clauses(KVs)),
    All = erl_syntax:function(erl_syntax:atom(all),
        all(lists:sort(KVs))),
    [erl_syntax:revert(X) || X <- [Mod, Export, Lookup, All]].

all(KVs) ->
    Pairs = [erl_syntax:map_field_assoc(to_syntax(K), to_syntax(V))
        || {K, V} <- KVs],
    Body = erl_syntax:tuple([erl_syntax:atom(ok), erl_syntax:map_expr(Pairs)]),
    [erl_syntax:clause([], [Body])].

lookup_clause(Key, Value) ->
    Var = to_syntax(Key),
    Body = erl_syntax:tuple([erl_syntax:atom(ok),
        to_syntax(Value)]),
    erl_syntax:clause([Var], [], [Body]).

lookup_clause_anon() ->
    Var = erl_syntax:variable("_"),
    Body = erl_syntax:tuple([erl_syntax:atom(error),
        erl_syntax:atom(key_not_found)]),
    erl_syntax:clause([Var], [], [Body]).

lookup_clauses(KVs) ->
    lookup_clauses(KVs, []).

lookup_clauses([], Acc) ->
    lists:reverse(lists:flatten([lookup_clause_anon() | Acc]));
lookup_clauses([{Key, Value} | T], Acc) ->
    lookup_clauses(T, [lookup_clause(Key, Value) | Acc]).

to_syntax(Atom) when is_atom(Atom) ->
    erl_syntax:atom(Atom);
to_syntax(Binary) when is_binary(Binary) ->
    String = erl_syntax:string(binary_to_list(Binary)),
    erl_syntax:binary([erl_syntax:binary_field(String)]);
to_syntax(Float) when is_float(Float) ->
    erl_syntax:float(Float);
to_syntax(Integer) when is_integer(Integer) ->
    erl_syntax:integer(Integer);
to_syntax(List) when is_list(List) ->
    erl_syntax:list([to_syntax(X) || X <- List]);
to_syntax(Tuple) when is_tuple(Tuple) ->
    erl_syntax:tuple([to_syntax(X) || X <- tuple_to_list(Tuple)]).
