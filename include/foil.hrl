%% macros
-define(APP, foil).
-define(CHILD(Mod), {Mod, {Mod, start_link, []}, permanent, 5000, worker, [Mod]}).
-define(SERVER, foil_server).

%% tables
-define(FOIL_TABLE, foil_internal).

%% types
-type error()     :: {error, atom()}.
-type key()       :: term().
-type namespace() :: atom().
-type value()     :: term().
