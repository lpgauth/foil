-module(foil).
-include("foil.hrl").

-compile(inline).
-compile({inline_size, 512}).

-ignore_xref([
    {foil_modules, all, 0},
    {foil_modules, lookup, 1}
]).

-export([
    all/1,
    delete/1,
    delete/2,
    insert/3,
    load/1,
    lookup/2,
    namespaces/0,
    new/1
]).

%% public

-spec all(namespace()) ->
    {ok, #{key() := value()}} | error().

all(Namespace) ->
    ?WITH_MODULE(Namespace, Module, Module:all()).

-spec delete(namespace()) ->
    ok | error().

delete(Namespace) ->
    ?WITH_MODULE(Namespace, Module, begin
        ets:delete(Module),
        ets:delete(?FOIL_TABLE, Namespace),
        KVs = ets:tab2list(?FOIL_TABLE),
        foil_compiler:load(foil_modules, KVs),
        ok
    end).

-spec delete(namespace(), key()) ->
    ok | error().

delete(Namespace, Key) ->
    ?WITH_MODULE(Namespace, Module, begin
        ets:delete(Module, Key),
        ok
    end).

-spec insert(namespace(), key(), value()) ->
    ok | error().

insert(Namespace, Key, Value) ->
    ?WITH_MODULE(Namespace, Module, begin
        ets:insert(Module, {Key, Value}),
        ok
    end).

-spec load(namespace()) ->
    ok | error().

load(Namespace) ->
    ?WITH_MODULE(Namespace, Module, begin
        KVs = ets:tab2list(Module),
        foil_compiler:load(Module, KVs)
    end).

-spec lookup(namespace(), key()) ->
    {ok, value()} | error().

lookup(Namespace, Key) ->
    ?WITH_MODULE(Namespace, Module, Module:lookup(Key)).

-spec namespaces() ->
    {ok, [namespace()]} | {error, foil_not_started}.

namespaces() ->
    try foil_modules:all() of
        {ok, Map} ->
            {ok, maps:keys(Map)}
    catch
        error:undef ->
            {error, foil_not_started}
    end.

-spec new(namespace()) ->
    ok | error().

new(Namespace) ->
    try foil_modules:lookup(Namespace) of
        {ok, _Module} ->
            {error, module_exists};
        {error, key_not_found} ->
            Module = module(Namespace),
            ets:new(Module, [named_table, public]),
            Server = whereis(foil_server),
            ets:give_away(Module, Server, undefined),
            ets:insert(?FOIL_TABLE, {Namespace, Module}),
            KVs = ets:tab2list(?FOIL_TABLE),
            foil_compiler:load(foil_modules, KVs)
    catch
        error:undef ->
            {error, foil_not_started}
    end.

%% private

module(Namespace) ->
    list_to_atom(atom_to_list(Namespace) ++ "_foil").
