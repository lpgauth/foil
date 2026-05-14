%% macros
-define(APP, foil).
-define(CHILD(Mod), {Mod, {Mod, start_link, []}, permanent, 5000, worker, [Mod]}).
-define(SERVER, foil_server).

%% tables
-define(FOIL_TABLE, foil_internal).

%% Look up Namespace in the runtime-compiled foil_modules table, bind the
%% compiled module's name to Module, and evaluate Expr in that scope.
%% Errors are standardized to {error, module_not_found} (namespace not
%% registered) and {error, foil_not_started} (app not running).
%% Macro rather than function so the cache hot path stays branch-only.
-define(WITH_MODULE(Namespace, Module, Expr),
    try foil_modules:lookup(Namespace) of
        {ok, Module} ->
            Expr;
        {error, key_not_found} ->
            {error, module_not_found}
    catch
        error:undef ->
            {error, foil_not_started}
    end).

%% types
-type error_reason() :: foil_not_started
                      | key_not_found
                      | module_exists
                      | module_not_found.
-type error()        :: {error, error_reason()}.
-type key()          :: term().
-type namespace()    :: atom().
-type value()        :: term().
