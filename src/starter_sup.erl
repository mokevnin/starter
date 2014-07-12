-module(starter_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Pools} = application:get_env(starter, pools),
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
                                  PoolArgs = [{name, {local, Name}},
                                              {worker_module, starter_worker}] ++ SizeArgs,
                                  poolboy:child_spec(Name, PoolArgs, WorkerArgs)
                          end, Pools),
    Workers = [?CHILD(files_cleaner, worker) | PoolSpecs],

    {ok, {{one_for_one, 10, 10}, Workers}}.
