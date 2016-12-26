-module(super_sup_worker).

-behaviour(supervisor).

%% %% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

%% %% -------------------------------------------------------------------
%% %% API functions
%% %% -------------------------------------------------------------------

start_link(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, []).

%% -------------------------------------------------------------------
%% Supervisor callbacks
%% -------------------------------------------------------------------

init([]) ->
    GenServer = ?CHILD(gen_state, worker),
    {ok, { {simple_one_for_one, 5, 10}, [GenServer]}}.



