-module(foil_server).
-include("foil.hrl").

-export([
    start_link/0
]).

-behaviour(metal).
-export([
    init/3,
    handle_msg/2,
    terminate/2
]).

%% public
-spec start_link() ->
    {ok, pid()}.

start_link() ->
    metal:start_link(?SERVER, ?SERVER, undefined).

%% metal callbacks
-spec init(atom(), pid(), term()) ->
    {ok, term()}.

init(_Name, _Parent, undefined) ->
    ets:new(?FOIL_TABLE, [public, named_table]),
    KVs = ets:tab2list(?FOIL_TABLE),
    ok = foil_compiler:load(foil_modules, KVs),
    {ok, undefined}.

-spec handle_msg(term(), term()) ->
    {ok, term()}.

handle_msg({'ETS-TRANSFER', _, _, _}, State) ->
    {ok, State}.

-spec terminate(term(), term()) ->
    ok.

terminate(_Reason, _State) ->
    ok.
