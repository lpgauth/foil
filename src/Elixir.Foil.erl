%% Elixir shim for foil. On the BEAM an Elixir module `Foo` is the atom
%% `'Elixir.Foo'`, so naming this module `'Elixir.Foil'` makes the foil
%% API callable from Elixir as `Foil.lookup/2`, `Foil.insert/3`, etc.
%% with no Mix/Elixir build step. Each function delegates straight to the
%% Erlang `foil` module, keeping the two APIs in lockstep.
-module('Elixir.Foil').
-include("foil.hrl").

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
    foil:all(Namespace).

-spec delete(namespace()) ->
    ok | error().

delete(Namespace) ->
    foil:delete(Namespace).

-spec delete(namespace(), key()) ->
    ok | error().

delete(Namespace, Key) ->
    foil:delete(Namespace, Key).

-spec insert(namespace(), key(), value()) ->
    ok | error().

insert(Namespace, Key, Value) ->
    foil:insert(Namespace, Key, Value).

-spec load(namespace()) ->
    ok | error().

load(Namespace) ->
    foil:load(Namespace).

-spec lookup(namespace(), key()) ->
    {ok, value()} | error().

lookup(Namespace, Key) ->
    foil:lookup(Namespace, Key).

-spec namespaces() ->
    {ok, [namespace()]} | {error, foil_not_started}.

namespaces() ->
    foil:namespaces().

-spec new(namespace()) ->
    ok | error().

new(Namespace) ->
    foil:new(Namespace).
