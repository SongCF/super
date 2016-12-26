-module(super_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("super.hrl").

%% -------------------------------------------------------------------
%% Application callbacks
%% -------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    Ret = super_sup:start_link(),

    %% 初始化表
    %case cluster:node_role(node()) of
    case cluster:get_my_role() of
        leader ->
            super = cluster:init_table(super, record_info(fields, super), bag),
            Fun1 = fun(X) ->
                           mnesia:dirty_delete(super, X),
                           lager:info("super nodedown ~p", [X])
                   end,
            notify:sub(nodedown, Fun1);
        X  ->
            lager:info("my role is ~p", [X]),
            ignore
    end,

    %% 初始化进程
    Count = application:get_env(super, count, 10),
    [super_sup:start_super_one(Id) || Id <- lists:seq(1, Count)],

    Ret.

stop(_State) ->
    ok.


