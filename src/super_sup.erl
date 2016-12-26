-module(super_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
    start_super_one/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% -------------------------------------------------------------------
%% Supervisor callbacks
%% -------------------------------------------------------------------

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.




atom_suffix(Prefix, No) ->
    L = atom_to_list(Prefix) ++ "_" ++ integer_to_list(No),
    list_to_atom(L).

%% 获取super的spec.
super_spec(Id) ->
    SupName = atom_suffix(super_sup_worker, Id),
    {SupName,
        {super_sup_worker, start_link, [SupName]},
        transient,
        infinity,
        supervisor,
        []
    }.

%% 获取super的spec.
worker_spec(Id) ->
    SupName = atom_suffix(super_sup_worker, Id),
    WorkerName = atom_suffix(super_worker, Id),
    {WorkerName,
        {super_worker, start_link, [SupName, WorkerName]},
        transient,
        infinity,
        worker,
        []
    }.

start_super_one(Id) ->
    Ret1 = supervisor:start_child(super_sup, super_spec(Id)),
    lager:info("start_super_one id ~p (~p)~n", [Id, Ret1]),
    Ret2= supervisor:start_child(super_sup, worker_spec(Id)),
    lager:info("start_super_one(~p)~n", [ Ret2]),
    ok.
