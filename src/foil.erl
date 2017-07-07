-module(foil).
-include("foil.hrl").

-compile(inline).
-compile({inline_size, 512}).

-ignore_xref([
    {foil_modules, lookup, 1}
]).


-export([
    delete/1,
    delete/2,
    insert/3,
    load/1,
    lookup/2,
    new/1
]).

%% public
-spec delete(namespace()) ->
    ok | error().

delete(Namespace) ->
    case foil_modules:lookup(Namespace) of
        {ok, Module} ->
            ets:delete(Module),
            ets:delete(?FOIL_TABLE, Namespace),
            KVs = ets:tab2list(?FOIL_TABLE),
            foil_compiler:load(foil_modules, KVs),
            ok;
        {error, key_not_found} ->
            {error, module_not_found}
    end.

-spec delete(namespace(), key()) ->
    ok | error().

delete(Namespace, Key) ->
    case foil_modules:lookup(Namespace) of
        {ok, Module} ->
            ets:delete(Module, Key),
            ok;
        {error, key_not_found} ->
            {error, module_not_found}
    end.

-spec insert(namespace(), key(), value()) ->
    ok | error().

insert(Namespace, Key, Value) ->
    case foil_modules:lookup(Namespace) of
        {ok, Module} ->
            ets:insert(Module, {Key, Value}),
            ok;
        {error, key_not_found} ->
            {error, module_not_found}
    end.

-spec load(namespace()) ->
    ok | error().

load(Namespace) ->
    case foil_modules:lookup(Namespace) of
        {ok, Module} ->
            KVs = ets:tab2list(Module),
            foil_compiler:load(Module, KVs);
        {error, key_not_found} ->
            {error, module_not_found}
    end.

-spec lookup(namespace(), key()) ->
    {ok, value()} | error().

lookup(Namespace, Key) ->
    case foil_modules:lookup(Namespace) of
        {ok, Module} ->
            Module:lookup(Key);
        {error, key_not_found} ->
            {error, module_not_found}
    end.

-spec new(namespace()) ->
    ok | error().

new(Namespace) ->
    case foil_modules:lookup(Namespace) of
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
    end.

%% private
module(Namespace) ->
    list_to_atom(atom_to_list(Namespace) ++ "_foil").
